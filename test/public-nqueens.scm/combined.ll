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
%mainenv57043 = call %struct.ScmObj* @const_init_null()
%mainargs57044 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv57043, %struct.ScmObj* %mainargs57044)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv57041,%struct.ScmObj* %mainargs57042) {
%stackaddr$makeclosure57045 = alloca %struct.ScmObj*, align 8
%fptrToInt57046 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47573 to i64
%ae47573 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57046)
store volatile %struct.ScmObj* %ae47573, %struct.ScmObj** %stackaddr$makeclosure57045, align 8
%ae47574 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57047 = alloca %struct.ScmObj*, align 8
%fptrToInt57048 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47575 to i64
%ae47575 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57048)
store volatile %struct.ScmObj* %ae47575, %struct.ScmObj** %stackaddr$makeclosure57047, align 8
%argslist57040$ae475730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57049 = alloca %struct.ScmObj*, align 8
%argslist57040$ae475731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47575, %struct.ScmObj* %argslist57040$ae475730)
store volatile %struct.ScmObj* %argslist57040$ae475731, %struct.ScmObj** %stackaddr$prim57049, align 8
%stackaddr$prim57050 = alloca %struct.ScmObj*, align 8
%argslist57040$ae475732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47574, %struct.ScmObj* %argslist57040$ae475731)
store volatile %struct.ScmObj* %argslist57040$ae475732, %struct.ScmObj** %stackaddr$prim57050, align 8
%clofunc57051 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47573)
musttail call tailcc void %clofunc57051(%struct.ScmObj* %ae47573, %struct.ScmObj* %argslist57040$ae475732)
ret void
}

define tailcc void @proc_clo$ae47573(%struct.ScmObj* %env$ae47573,%struct.ScmObj* %current_45args56398) {
%stackaddr$prim57052 = alloca %struct.ScmObj*, align 8
%_95k47381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56398)
store volatile %struct.ScmObj* %_95k47381, %struct.ScmObj** %stackaddr$prim57052, align 8
%stackaddr$prim57053 = alloca %struct.ScmObj*, align 8
%current_45args56399 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56398)
store volatile %struct.ScmObj* %current_45args56399, %struct.ScmObj** %stackaddr$prim57053, align 8
%stackaddr$prim57054 = alloca %struct.ScmObj*, align 8
%anf_45bind47215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56399)
store volatile %struct.ScmObj* %anf_45bind47215, %struct.ScmObj** %stackaddr$prim57054, align 8
%stackaddr$makeclosure57055 = alloca %struct.ScmObj*, align 8
%fptrToInt57056 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47588 to i64
%ae47588 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57056)
store volatile %struct.ScmObj* %ae47588, %struct.ScmObj** %stackaddr$makeclosure57055, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47588, %struct.ScmObj* %anf_45bind47215, i64 0)
%ae47589 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57057 = alloca %struct.ScmObj*, align 8
%fptrToInt57058 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47590 to i64
%ae47590 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57058)
store volatile %struct.ScmObj* %ae47590, %struct.ScmObj** %stackaddr$makeclosure57057, align 8
%argslist57035$ae475880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57059 = alloca %struct.ScmObj*, align 8
%argslist57035$ae475881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47590, %struct.ScmObj* %argslist57035$ae475880)
store volatile %struct.ScmObj* %argslist57035$ae475881, %struct.ScmObj** %stackaddr$prim57059, align 8
%stackaddr$prim57060 = alloca %struct.ScmObj*, align 8
%argslist57035$ae475882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47589, %struct.ScmObj* %argslist57035$ae475881)
store volatile %struct.ScmObj* %argslist57035$ae475882, %struct.ScmObj** %stackaddr$prim57060, align 8
%clofunc57061 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47588)
musttail call tailcc void %clofunc57061(%struct.ScmObj* %ae47588, %struct.ScmObj* %argslist57035$ae475882)
ret void
}

define tailcc void @proc_clo$ae47588(%struct.ScmObj* %env$ae47588,%struct.ScmObj* %current_45args56401) {
%stackaddr$env-ref57062 = alloca %struct.ScmObj*, align 8
%anf_45bind47215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47588, i64 0)
store %struct.ScmObj* %anf_45bind47215, %struct.ScmObj** %stackaddr$env-ref57062
%stackaddr$prim57063 = alloca %struct.ScmObj*, align 8
%_95k47382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56401)
store volatile %struct.ScmObj* %_95k47382, %struct.ScmObj** %stackaddr$prim57063, align 8
%stackaddr$prim57064 = alloca %struct.ScmObj*, align 8
%current_45args56402 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56401)
store volatile %struct.ScmObj* %current_45args56402, %struct.ScmObj** %stackaddr$prim57064, align 8
%stackaddr$prim57065 = alloca %struct.ScmObj*, align 8
%anf_45bind47219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56402)
store volatile %struct.ScmObj* %anf_45bind47219, %struct.ScmObj** %stackaddr$prim57065, align 8
%stackaddr$makeclosure57066 = alloca %struct.ScmObj*, align 8
%fptrToInt57067 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47703 to i64
%ae47703 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57067)
store volatile %struct.ScmObj* %ae47703, %struct.ScmObj** %stackaddr$makeclosure57066, align 8
%argslist57014$anf_45bind472150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57068 = alloca %struct.ScmObj*, align 8
%argslist57014$anf_45bind472151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47219, %struct.ScmObj* %argslist57014$anf_45bind472150)
store volatile %struct.ScmObj* %argslist57014$anf_45bind472151, %struct.ScmObj** %stackaddr$prim57068, align 8
%stackaddr$prim57069 = alloca %struct.ScmObj*, align 8
%argslist57014$anf_45bind472152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47703, %struct.ScmObj* %argslist57014$anf_45bind472151)
store volatile %struct.ScmObj* %argslist57014$anf_45bind472152, %struct.ScmObj** %stackaddr$prim57069, align 8
%clofunc57070 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47215)
musttail call tailcc void %clofunc57070(%struct.ScmObj* %anf_45bind47215, %struct.ScmObj* %argslist57014$anf_45bind472152)
ret void
}

define tailcc void @proc_clo$ae47703(%struct.ScmObj* %env$ae47703,%struct.ScmObj* %current_45args56404) {
%stackaddr$prim57071 = alloca %struct.ScmObj*, align 8
%_95k47383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56404)
store volatile %struct.ScmObj* %_95k47383, %struct.ScmObj** %stackaddr$prim57071, align 8
%stackaddr$prim57072 = alloca %struct.ScmObj*, align 8
%current_45args56405 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56404)
store volatile %struct.ScmObj* %current_45args56405, %struct.ScmObj** %stackaddr$prim57072, align 8
%stackaddr$prim57073 = alloca %struct.ScmObj*, align 8
%Ycmb47073 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56405)
store volatile %struct.ScmObj* %Ycmb47073, %struct.ScmObj** %stackaddr$prim57073, align 8
%stackaddr$makeclosure57074 = alloca %struct.ScmObj*, align 8
%fptrToInt57075 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47705 to i64
%ae47705 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57075)
store volatile %struct.ScmObj* %ae47705, %struct.ScmObj** %stackaddr$makeclosure57074, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47705, %struct.ScmObj* %Ycmb47073, i64 0)
%ae47706 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57076 = alloca %struct.ScmObj*, align 8
%fptrToInt57077 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47707 to i64
%ae47707 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57077)
store volatile %struct.ScmObj* %ae47707, %struct.ScmObj** %stackaddr$makeclosure57076, align 8
%argslist57013$ae477050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57078 = alloca %struct.ScmObj*, align 8
%argslist57013$ae477051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47707, %struct.ScmObj* %argslist57013$ae477050)
store volatile %struct.ScmObj* %argslist57013$ae477051, %struct.ScmObj** %stackaddr$prim57078, align 8
%stackaddr$prim57079 = alloca %struct.ScmObj*, align 8
%argslist57013$ae477052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47706, %struct.ScmObj* %argslist57013$ae477051)
store volatile %struct.ScmObj* %argslist57013$ae477052, %struct.ScmObj** %stackaddr$prim57079, align 8
%clofunc57080 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47705)
musttail call tailcc void %clofunc57080(%struct.ScmObj* %ae47705, %struct.ScmObj* %argslist57013$ae477052)
ret void
}

define tailcc void @proc_clo$ae47705(%struct.ScmObj* %env$ae47705,%struct.ScmObj* %current_45args56407) {
%stackaddr$env-ref57081 = alloca %struct.ScmObj*, align 8
%Ycmb47073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47705, i64 0)
store %struct.ScmObj* %Ycmb47073, %struct.ScmObj** %stackaddr$env-ref57081
%stackaddr$prim57082 = alloca %struct.ScmObj*, align 8
%_95k47384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56407)
store volatile %struct.ScmObj* %_95k47384, %struct.ScmObj** %stackaddr$prim57082, align 8
%stackaddr$prim57083 = alloca %struct.ScmObj*, align 8
%current_45args56408 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56407)
store volatile %struct.ScmObj* %current_45args56408, %struct.ScmObj** %stackaddr$prim57083, align 8
%stackaddr$prim57084 = alloca %struct.ScmObj*, align 8
%anf_45bind47224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56408)
store volatile %struct.ScmObj* %anf_45bind47224, %struct.ScmObj** %stackaddr$prim57084, align 8
%stackaddr$makeclosure57085 = alloca %struct.ScmObj*, align 8
%fptrToInt57086 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47783 to i64
%ae47783 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57086)
store volatile %struct.ScmObj* %ae47783, %struct.ScmObj** %stackaddr$makeclosure57085, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47783, %struct.ScmObj* %Ycmb47073, i64 0)
%argslist56997$Ycmb470730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57087 = alloca %struct.ScmObj*, align 8
%argslist56997$Ycmb470731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47224, %struct.ScmObj* %argslist56997$Ycmb470730)
store volatile %struct.ScmObj* %argslist56997$Ycmb470731, %struct.ScmObj** %stackaddr$prim57087, align 8
%stackaddr$prim57088 = alloca %struct.ScmObj*, align 8
%argslist56997$Ycmb470732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47783, %struct.ScmObj* %argslist56997$Ycmb470731)
store volatile %struct.ScmObj* %argslist56997$Ycmb470732, %struct.ScmObj** %stackaddr$prim57088, align 8
%clofunc57089 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47073)
musttail call tailcc void %clofunc57089(%struct.ScmObj* %Ycmb47073, %struct.ScmObj* %argslist56997$Ycmb470732)
ret void
}

define tailcc void @proc_clo$ae47783(%struct.ScmObj* %env$ae47783,%struct.ScmObj* %current_45args56410) {
%stackaddr$env-ref57090 = alloca %struct.ScmObj*, align 8
%Ycmb47073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47783, i64 0)
store %struct.ScmObj* %Ycmb47073, %struct.ScmObj** %stackaddr$env-ref57090
%stackaddr$prim57091 = alloca %struct.ScmObj*, align 8
%_95k47385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56410)
store volatile %struct.ScmObj* %_95k47385, %struct.ScmObj** %stackaddr$prim57091, align 8
%stackaddr$prim57092 = alloca %struct.ScmObj*, align 8
%current_45args56411 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56410)
store volatile %struct.ScmObj* %current_45args56411, %struct.ScmObj** %stackaddr$prim57092, align 8
%stackaddr$prim57093 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56411)
store volatile %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$prim57093, align 8
%stackaddr$makeclosure57094 = alloca %struct.ScmObj*, align 8
%fptrToInt57095 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47785 to i64
%ae47785 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57095)
store volatile %struct.ScmObj* %ae47785, %struct.ScmObj** %stackaddr$makeclosure57094, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47785, %struct.ScmObj* %_37foldr147094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47785, %struct.ScmObj* %Ycmb47073, i64 1)
%ae47786 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57096 = alloca %struct.ScmObj*, align 8
%fptrToInt57097 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47787 to i64
%ae47787 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57097)
store volatile %struct.ScmObj* %ae47787, %struct.ScmObj** %stackaddr$makeclosure57096, align 8
%argslist56996$ae477850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57098 = alloca %struct.ScmObj*, align 8
%argslist56996$ae477851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47787, %struct.ScmObj* %argslist56996$ae477850)
store volatile %struct.ScmObj* %argslist56996$ae477851, %struct.ScmObj** %stackaddr$prim57098, align 8
%stackaddr$prim57099 = alloca %struct.ScmObj*, align 8
%argslist56996$ae477852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47786, %struct.ScmObj* %argslist56996$ae477851)
store volatile %struct.ScmObj* %argslist56996$ae477852, %struct.ScmObj** %stackaddr$prim57099, align 8
%clofunc57100 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47785)
musttail call tailcc void %clofunc57100(%struct.ScmObj* %ae47785, %struct.ScmObj* %argslist56996$ae477852)
ret void
}

define tailcc void @proc_clo$ae47785(%struct.ScmObj* %env$ae47785,%struct.ScmObj* %current_45args56413) {
%stackaddr$env-ref57101 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47785, i64 0)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref57101
%stackaddr$env-ref57102 = alloca %struct.ScmObj*, align 8
%Ycmb47073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47785, i64 1)
store %struct.ScmObj* %Ycmb47073, %struct.ScmObj** %stackaddr$env-ref57102
%stackaddr$prim57103 = alloca %struct.ScmObj*, align 8
%_95k47386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56413)
store volatile %struct.ScmObj* %_95k47386, %struct.ScmObj** %stackaddr$prim57103, align 8
%stackaddr$prim57104 = alloca %struct.ScmObj*, align 8
%current_45args56414 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56413)
store volatile %struct.ScmObj* %current_45args56414, %struct.ScmObj** %stackaddr$prim57104, align 8
%stackaddr$prim57105 = alloca %struct.ScmObj*, align 8
%anf_45bind47230 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56414)
store volatile %struct.ScmObj* %anf_45bind47230, %struct.ScmObj** %stackaddr$prim57105, align 8
%stackaddr$makeclosure57106 = alloca %struct.ScmObj*, align 8
%fptrToInt57107 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47880 to i64
%ae47880 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57107)
store volatile %struct.ScmObj* %ae47880, %struct.ScmObj** %stackaddr$makeclosure57106, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47880, %struct.ScmObj* %_37foldr147094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47880, %struct.ScmObj* %Ycmb47073, i64 1)
%argslist56977$Ycmb470730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57108 = alloca %struct.ScmObj*, align 8
%argslist56977$Ycmb470731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47230, %struct.ScmObj* %argslist56977$Ycmb470730)
store volatile %struct.ScmObj* %argslist56977$Ycmb470731, %struct.ScmObj** %stackaddr$prim57108, align 8
%stackaddr$prim57109 = alloca %struct.ScmObj*, align 8
%argslist56977$Ycmb470732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47880, %struct.ScmObj* %argslist56977$Ycmb470731)
store volatile %struct.ScmObj* %argslist56977$Ycmb470732, %struct.ScmObj** %stackaddr$prim57109, align 8
%clofunc57110 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47073)
musttail call tailcc void %clofunc57110(%struct.ScmObj* %Ycmb47073, %struct.ScmObj* %argslist56977$Ycmb470732)
ret void
}

define tailcc void @proc_clo$ae47880(%struct.ScmObj* %env$ae47880,%struct.ScmObj* %current_45args56416) {
%stackaddr$env-ref57111 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47880, i64 0)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref57111
%stackaddr$env-ref57112 = alloca %struct.ScmObj*, align 8
%Ycmb47073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47880, i64 1)
store %struct.ScmObj* %Ycmb47073, %struct.ScmObj** %stackaddr$env-ref57112
%stackaddr$prim57113 = alloca %struct.ScmObj*, align 8
%_95k47387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56416)
store volatile %struct.ScmObj* %_95k47387, %struct.ScmObj** %stackaddr$prim57113, align 8
%stackaddr$prim57114 = alloca %struct.ScmObj*, align 8
%current_45args56417 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56416)
store volatile %struct.ScmObj* %current_45args56417, %struct.ScmObj** %stackaddr$prim57114, align 8
%stackaddr$prim57115 = alloca %struct.ScmObj*, align 8
%_37map147090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56417)
store volatile %struct.ScmObj* %_37map147090, %struct.ScmObj** %stackaddr$prim57115, align 8
%stackaddr$makeclosure57116 = alloca %struct.ScmObj*, align 8
%fptrToInt57117 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47882 to i64
%ae47882 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57117)
store volatile %struct.ScmObj* %ae47882, %struct.ScmObj** %stackaddr$makeclosure57116, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47882, %struct.ScmObj* %_37foldr147094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47882, %struct.ScmObj* %_37map147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47882, %struct.ScmObj* %Ycmb47073, i64 2)
%ae47883 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57118 = alloca %struct.ScmObj*, align 8
%fptrToInt57119 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47884 to i64
%ae47884 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57119)
store volatile %struct.ScmObj* %ae47884, %struct.ScmObj** %stackaddr$makeclosure57118, align 8
%argslist56976$ae478820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57120 = alloca %struct.ScmObj*, align 8
%argslist56976$ae478821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47884, %struct.ScmObj* %argslist56976$ae478820)
store volatile %struct.ScmObj* %argslist56976$ae478821, %struct.ScmObj** %stackaddr$prim57120, align 8
%stackaddr$prim57121 = alloca %struct.ScmObj*, align 8
%argslist56976$ae478822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47883, %struct.ScmObj* %argslist56976$ae478821)
store volatile %struct.ScmObj* %argslist56976$ae478822, %struct.ScmObj** %stackaddr$prim57121, align 8
%clofunc57122 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47882)
musttail call tailcc void %clofunc57122(%struct.ScmObj* %ae47882, %struct.ScmObj* %argslist56976$ae478822)
ret void
}

define tailcc void @proc_clo$ae47882(%struct.ScmObj* %env$ae47882,%struct.ScmObj* %current_45args56419) {
%stackaddr$env-ref57123 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47882, i64 0)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref57123
%stackaddr$env-ref57124 = alloca %struct.ScmObj*, align 8
%_37map147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47882, i64 1)
store %struct.ScmObj* %_37map147090, %struct.ScmObj** %stackaddr$env-ref57124
%stackaddr$env-ref57125 = alloca %struct.ScmObj*, align 8
%Ycmb47073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47882, i64 2)
store %struct.ScmObj* %Ycmb47073, %struct.ScmObj** %stackaddr$env-ref57125
%stackaddr$prim57126 = alloca %struct.ScmObj*, align 8
%_95k47388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56419)
store volatile %struct.ScmObj* %_95k47388, %struct.ScmObj** %stackaddr$prim57126, align 8
%stackaddr$prim57127 = alloca %struct.ScmObj*, align 8
%current_45args56420 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56419)
store volatile %struct.ScmObj* %current_45args56420, %struct.ScmObj** %stackaddr$prim57127, align 8
%stackaddr$prim57128 = alloca %struct.ScmObj*, align 8
%anf_45bind47237 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56420)
store volatile %struct.ScmObj* %anf_45bind47237, %struct.ScmObj** %stackaddr$prim57128, align 8
%stackaddr$makeclosure57129 = alloca %struct.ScmObj*, align 8
%fptrToInt57130 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48030 to i64
%ae48030 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57130)
store volatile %struct.ScmObj* %ae48030, %struct.ScmObj** %stackaddr$makeclosure57129, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48030, %struct.ScmObj* %_37foldr147094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48030, %struct.ScmObj* %_37map147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48030, %struct.ScmObj* %Ycmb47073, i64 2)
%argslist56960$Ycmb470730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57131 = alloca %struct.ScmObj*, align 8
%argslist56960$Ycmb470731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47237, %struct.ScmObj* %argslist56960$Ycmb470730)
store volatile %struct.ScmObj* %argslist56960$Ycmb470731, %struct.ScmObj** %stackaddr$prim57131, align 8
%stackaddr$prim57132 = alloca %struct.ScmObj*, align 8
%argslist56960$Ycmb470732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48030, %struct.ScmObj* %argslist56960$Ycmb470731)
store volatile %struct.ScmObj* %argslist56960$Ycmb470732, %struct.ScmObj** %stackaddr$prim57132, align 8
%clofunc57133 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47073)
musttail call tailcc void %clofunc57133(%struct.ScmObj* %Ycmb47073, %struct.ScmObj* %argslist56960$Ycmb470732)
ret void
}

define tailcc void @proc_clo$ae48030(%struct.ScmObj* %env$ae48030,%struct.ScmObj* %current_45args56422) {
%stackaddr$env-ref57134 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48030, i64 0)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref57134
%stackaddr$env-ref57135 = alloca %struct.ScmObj*, align 8
%_37map147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48030, i64 1)
store %struct.ScmObj* %_37map147090, %struct.ScmObj** %stackaddr$env-ref57135
%stackaddr$env-ref57136 = alloca %struct.ScmObj*, align 8
%Ycmb47073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48030, i64 2)
store %struct.ScmObj* %Ycmb47073, %struct.ScmObj** %stackaddr$env-ref57136
%stackaddr$prim57137 = alloca %struct.ScmObj*, align 8
%_95k47389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56422)
store volatile %struct.ScmObj* %_95k47389, %struct.ScmObj** %stackaddr$prim57137, align 8
%stackaddr$prim57138 = alloca %struct.ScmObj*, align 8
%current_45args56423 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56422)
store volatile %struct.ScmObj* %current_45args56423, %struct.ScmObj** %stackaddr$prim57138, align 8
%stackaddr$prim57139 = alloca %struct.ScmObj*, align 8
%_37take47086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56423)
store volatile %struct.ScmObj* %_37take47086, %struct.ScmObj** %stackaddr$prim57139, align 8
%stackaddr$makeclosure57140 = alloca %struct.ScmObj*, align 8
%fptrToInt57141 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48032 to i64
%ae48032 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57141)
store volatile %struct.ScmObj* %ae48032, %struct.ScmObj** %stackaddr$makeclosure57140, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48032, %struct.ScmObj* %_37foldr147094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48032, %struct.ScmObj* %_37map147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48032, %struct.ScmObj* %Ycmb47073, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48032, %struct.ScmObj* %_37take47086, i64 3)
%ae48033 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57142 = alloca %struct.ScmObj*, align 8
%fptrToInt57143 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48034 to i64
%ae48034 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57143)
store volatile %struct.ScmObj* %ae48034, %struct.ScmObj** %stackaddr$makeclosure57142, align 8
%argslist56959$ae480320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57144 = alloca %struct.ScmObj*, align 8
%argslist56959$ae480321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48034, %struct.ScmObj* %argslist56959$ae480320)
store volatile %struct.ScmObj* %argslist56959$ae480321, %struct.ScmObj** %stackaddr$prim57144, align 8
%stackaddr$prim57145 = alloca %struct.ScmObj*, align 8
%argslist56959$ae480322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48033, %struct.ScmObj* %argslist56959$ae480321)
store volatile %struct.ScmObj* %argslist56959$ae480322, %struct.ScmObj** %stackaddr$prim57145, align 8
%clofunc57146 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48032)
musttail call tailcc void %clofunc57146(%struct.ScmObj* %ae48032, %struct.ScmObj* %argslist56959$ae480322)
ret void
}

define tailcc void @proc_clo$ae48032(%struct.ScmObj* %env$ae48032,%struct.ScmObj* %current_45args56425) {
%stackaddr$env-ref57147 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48032, i64 0)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref57147
%stackaddr$env-ref57148 = alloca %struct.ScmObj*, align 8
%_37map147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48032, i64 1)
store %struct.ScmObj* %_37map147090, %struct.ScmObj** %stackaddr$env-ref57148
%stackaddr$env-ref57149 = alloca %struct.ScmObj*, align 8
%Ycmb47073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48032, i64 2)
store %struct.ScmObj* %Ycmb47073, %struct.ScmObj** %stackaddr$env-ref57149
%stackaddr$env-ref57150 = alloca %struct.ScmObj*, align 8
%_37take47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48032, i64 3)
store %struct.ScmObj* %_37take47086, %struct.ScmObj** %stackaddr$env-ref57150
%stackaddr$prim57151 = alloca %struct.ScmObj*, align 8
%_95k47390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56425)
store volatile %struct.ScmObj* %_95k47390, %struct.ScmObj** %stackaddr$prim57151, align 8
%stackaddr$prim57152 = alloca %struct.ScmObj*, align 8
%current_45args56426 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56425)
store volatile %struct.ScmObj* %current_45args56426, %struct.ScmObj** %stackaddr$prim57152, align 8
%stackaddr$prim57153 = alloca %struct.ScmObj*, align 8
%anf_45bind47241 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56426)
store volatile %struct.ScmObj* %anf_45bind47241, %struct.ScmObj** %stackaddr$prim57153, align 8
%stackaddr$makeclosure57154 = alloca %struct.ScmObj*, align 8
%fptrToInt57155 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48113 to i64
%ae48113 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57155)
store volatile %struct.ScmObj* %ae48113, %struct.ScmObj** %stackaddr$makeclosure57154, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48113, %struct.ScmObj* %_37foldr147094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48113, %struct.ScmObj* %_37map147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48113, %struct.ScmObj* %Ycmb47073, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48113, %struct.ScmObj* %_37take47086, i64 3)
%argslist56945$Ycmb470730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57156 = alloca %struct.ScmObj*, align 8
%argslist56945$Ycmb470731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47241, %struct.ScmObj* %argslist56945$Ycmb470730)
store volatile %struct.ScmObj* %argslist56945$Ycmb470731, %struct.ScmObj** %stackaddr$prim57156, align 8
%stackaddr$prim57157 = alloca %struct.ScmObj*, align 8
%argslist56945$Ycmb470732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48113, %struct.ScmObj* %argslist56945$Ycmb470731)
store volatile %struct.ScmObj* %argslist56945$Ycmb470732, %struct.ScmObj** %stackaddr$prim57157, align 8
%clofunc57158 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47073)
musttail call tailcc void %clofunc57158(%struct.ScmObj* %Ycmb47073, %struct.ScmObj* %argslist56945$Ycmb470732)
ret void
}

define tailcc void @proc_clo$ae48113(%struct.ScmObj* %env$ae48113,%struct.ScmObj* %current_45args56428) {
%stackaddr$env-ref57159 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48113, i64 0)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref57159
%stackaddr$env-ref57160 = alloca %struct.ScmObj*, align 8
%_37map147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48113, i64 1)
store %struct.ScmObj* %_37map147090, %struct.ScmObj** %stackaddr$env-ref57160
%stackaddr$env-ref57161 = alloca %struct.ScmObj*, align 8
%Ycmb47073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48113, i64 2)
store %struct.ScmObj* %Ycmb47073, %struct.ScmObj** %stackaddr$env-ref57161
%stackaddr$env-ref57162 = alloca %struct.ScmObj*, align 8
%_37take47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48113, i64 3)
store %struct.ScmObj* %_37take47086, %struct.ScmObj** %stackaddr$env-ref57162
%stackaddr$prim57163 = alloca %struct.ScmObj*, align 8
%_95k47391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56428)
store volatile %struct.ScmObj* %_95k47391, %struct.ScmObj** %stackaddr$prim57163, align 8
%stackaddr$prim57164 = alloca %struct.ScmObj*, align 8
%current_45args56429 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56428)
store volatile %struct.ScmObj* %current_45args56429, %struct.ScmObj** %stackaddr$prim57164, align 8
%stackaddr$prim57165 = alloca %struct.ScmObj*, align 8
%_37length47083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56429)
store volatile %struct.ScmObj* %_37length47083, %struct.ScmObj** %stackaddr$prim57165, align 8
%stackaddr$makeclosure57166 = alloca %struct.ScmObj*, align 8
%fptrToInt57167 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48115 to i64
%ae48115 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57167)
store volatile %struct.ScmObj* %ae48115, %struct.ScmObj** %stackaddr$makeclosure57166, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48115, %struct.ScmObj* %_37foldr147094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48115, %struct.ScmObj* %_37map147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48115, %struct.ScmObj* %Ycmb47073, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48115, %struct.ScmObj* %_37take47086, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48115, %struct.ScmObj* %_37length47083, i64 4)
%ae48116 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57168 = alloca %struct.ScmObj*, align 8
%fptrToInt57169 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48117 to i64
%ae48117 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57169)
store volatile %struct.ScmObj* %ae48117, %struct.ScmObj** %stackaddr$makeclosure57168, align 8
%argslist56944$ae481150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57170 = alloca %struct.ScmObj*, align 8
%argslist56944$ae481151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48117, %struct.ScmObj* %argslist56944$ae481150)
store volatile %struct.ScmObj* %argslist56944$ae481151, %struct.ScmObj** %stackaddr$prim57170, align 8
%stackaddr$prim57171 = alloca %struct.ScmObj*, align 8
%argslist56944$ae481152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48116, %struct.ScmObj* %argslist56944$ae481151)
store volatile %struct.ScmObj* %argslist56944$ae481152, %struct.ScmObj** %stackaddr$prim57171, align 8
%clofunc57172 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48115)
musttail call tailcc void %clofunc57172(%struct.ScmObj* %ae48115, %struct.ScmObj* %argslist56944$ae481152)
ret void
}

define tailcc void @proc_clo$ae48115(%struct.ScmObj* %env$ae48115,%struct.ScmObj* %current_45args56431) {
%stackaddr$env-ref57173 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48115, i64 0)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref57173
%stackaddr$env-ref57174 = alloca %struct.ScmObj*, align 8
%_37map147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48115, i64 1)
store %struct.ScmObj* %_37map147090, %struct.ScmObj** %stackaddr$env-ref57174
%stackaddr$env-ref57175 = alloca %struct.ScmObj*, align 8
%Ycmb47073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48115, i64 2)
store %struct.ScmObj* %Ycmb47073, %struct.ScmObj** %stackaddr$env-ref57175
%stackaddr$env-ref57176 = alloca %struct.ScmObj*, align 8
%_37take47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48115, i64 3)
store %struct.ScmObj* %_37take47086, %struct.ScmObj** %stackaddr$env-ref57176
%stackaddr$env-ref57177 = alloca %struct.ScmObj*, align 8
%_37length47083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48115, i64 4)
store %struct.ScmObj* %_37length47083, %struct.ScmObj** %stackaddr$env-ref57177
%stackaddr$prim57178 = alloca %struct.ScmObj*, align 8
%_95k47392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56431)
store volatile %struct.ScmObj* %_95k47392, %struct.ScmObj** %stackaddr$prim57178, align 8
%stackaddr$prim57179 = alloca %struct.ScmObj*, align 8
%current_45args56432 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56431)
store volatile %struct.ScmObj* %current_45args56432, %struct.ScmObj** %stackaddr$prim57179, align 8
%stackaddr$prim57180 = alloca %struct.ScmObj*, align 8
%anf_45bind47246 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56432)
store volatile %struct.ScmObj* %anf_45bind47246, %struct.ScmObj** %stackaddr$prim57180, align 8
%stackaddr$makeclosure57181 = alloca %struct.ScmObj*, align 8
%fptrToInt57182 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48192 to i64
%ae48192 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57182)
store volatile %struct.ScmObj* %ae48192, %struct.ScmObj** %stackaddr$makeclosure57181, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48192, %struct.ScmObj* %_37foldr147094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48192, %struct.ScmObj* %_37map147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48192, %struct.ScmObj* %Ycmb47073, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48192, %struct.ScmObj* %_37take47086, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48192, %struct.ScmObj* %_37length47083, i64 4)
%argslist56928$Ycmb470730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57183 = alloca %struct.ScmObj*, align 8
%argslist56928$Ycmb470731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47246, %struct.ScmObj* %argslist56928$Ycmb470730)
store volatile %struct.ScmObj* %argslist56928$Ycmb470731, %struct.ScmObj** %stackaddr$prim57183, align 8
%stackaddr$prim57184 = alloca %struct.ScmObj*, align 8
%argslist56928$Ycmb470732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48192, %struct.ScmObj* %argslist56928$Ycmb470731)
store volatile %struct.ScmObj* %argslist56928$Ycmb470732, %struct.ScmObj** %stackaddr$prim57184, align 8
%clofunc57185 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47073)
musttail call tailcc void %clofunc57185(%struct.ScmObj* %Ycmb47073, %struct.ScmObj* %argslist56928$Ycmb470732)
ret void
}

define tailcc void @proc_clo$ae48192(%struct.ScmObj* %env$ae48192,%struct.ScmObj* %current_45args56434) {
%stackaddr$env-ref57186 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48192, i64 0)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref57186
%stackaddr$env-ref57187 = alloca %struct.ScmObj*, align 8
%_37map147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48192, i64 1)
store %struct.ScmObj* %_37map147090, %struct.ScmObj** %stackaddr$env-ref57187
%stackaddr$env-ref57188 = alloca %struct.ScmObj*, align 8
%Ycmb47073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48192, i64 2)
store %struct.ScmObj* %Ycmb47073, %struct.ScmObj** %stackaddr$env-ref57188
%stackaddr$env-ref57189 = alloca %struct.ScmObj*, align 8
%_37take47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48192, i64 3)
store %struct.ScmObj* %_37take47086, %struct.ScmObj** %stackaddr$env-ref57189
%stackaddr$env-ref57190 = alloca %struct.ScmObj*, align 8
%_37length47083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48192, i64 4)
store %struct.ScmObj* %_37length47083, %struct.ScmObj** %stackaddr$env-ref57190
%stackaddr$prim57191 = alloca %struct.ScmObj*, align 8
%_95k47393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56434)
store volatile %struct.ScmObj* %_95k47393, %struct.ScmObj** %stackaddr$prim57191, align 8
%stackaddr$prim57192 = alloca %struct.ScmObj*, align 8
%current_45args56435 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56434)
store volatile %struct.ScmObj* %current_45args56435, %struct.ScmObj** %stackaddr$prim57192, align 8
%stackaddr$prim57193 = alloca %struct.ScmObj*, align 8
%_37foldl147078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56435)
store volatile %struct.ScmObj* %_37foldl147078, %struct.ScmObj** %stackaddr$prim57193, align 8
%stackaddr$makeclosure57194 = alloca %struct.ScmObj*, align 8
%fptrToInt57195 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48194 to i64
%ae48194 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt57195)
store volatile %struct.ScmObj* %ae48194, %struct.ScmObj** %stackaddr$makeclosure57194, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48194, %struct.ScmObj* %_37foldr147094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48194, %struct.ScmObj* %_37foldl147078, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48194, %struct.ScmObj* %_37map147090, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48194, %struct.ScmObj* %Ycmb47073, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48194, %struct.ScmObj* %_37take47086, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48194, %struct.ScmObj* %_37length47083, i64 5)
%ae48195 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57196 = alloca %struct.ScmObj*, align 8
%fptrToInt57197 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48196 to i64
%ae48196 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57197)
store volatile %struct.ScmObj* %ae48196, %struct.ScmObj** %stackaddr$makeclosure57196, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48196, %struct.ScmObj* %_37foldl147078, i64 0)
%argslist56927$ae481940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57198 = alloca %struct.ScmObj*, align 8
%argslist56927$ae481941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48196, %struct.ScmObj* %argslist56927$ae481940)
store volatile %struct.ScmObj* %argslist56927$ae481941, %struct.ScmObj** %stackaddr$prim57198, align 8
%stackaddr$prim57199 = alloca %struct.ScmObj*, align 8
%argslist56927$ae481942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48195, %struct.ScmObj* %argslist56927$ae481941)
store volatile %struct.ScmObj* %argslist56927$ae481942, %struct.ScmObj** %stackaddr$prim57199, align 8
%clofunc57200 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48194)
musttail call tailcc void %clofunc57200(%struct.ScmObj* %ae48194, %struct.ScmObj* %argslist56927$ae481942)
ret void
}

define tailcc void @proc_clo$ae48194(%struct.ScmObj* %env$ae48194,%struct.ScmObj* %current_45args56437) {
%stackaddr$env-ref57201 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48194, i64 0)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref57201
%stackaddr$env-ref57202 = alloca %struct.ScmObj*, align 8
%_37foldl147078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48194, i64 1)
store %struct.ScmObj* %_37foldl147078, %struct.ScmObj** %stackaddr$env-ref57202
%stackaddr$env-ref57203 = alloca %struct.ScmObj*, align 8
%_37map147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48194, i64 2)
store %struct.ScmObj* %_37map147090, %struct.ScmObj** %stackaddr$env-ref57203
%stackaddr$env-ref57204 = alloca %struct.ScmObj*, align 8
%Ycmb47073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48194, i64 3)
store %struct.ScmObj* %Ycmb47073, %struct.ScmObj** %stackaddr$env-ref57204
%stackaddr$env-ref57205 = alloca %struct.ScmObj*, align 8
%_37take47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48194, i64 4)
store %struct.ScmObj* %_37take47086, %struct.ScmObj** %stackaddr$env-ref57205
%stackaddr$env-ref57206 = alloca %struct.ScmObj*, align 8
%_37length47083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48194, i64 5)
store %struct.ScmObj* %_37length47083, %struct.ScmObj** %stackaddr$env-ref57206
%stackaddr$prim57207 = alloca %struct.ScmObj*, align 8
%_95k47394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56437)
store volatile %struct.ScmObj* %_95k47394, %struct.ScmObj** %stackaddr$prim57207, align 8
%stackaddr$prim57208 = alloca %struct.ScmObj*, align 8
%current_45args56438 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56437)
store volatile %struct.ScmObj* %current_45args56438, %struct.ScmObj** %stackaddr$prim57208, align 8
%stackaddr$prim57209 = alloca %struct.ScmObj*, align 8
%_37last47116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56438)
store volatile %struct.ScmObj* %_37last47116, %struct.ScmObj** %stackaddr$prim57209, align 8
%stackaddr$makeclosure57210 = alloca %struct.ScmObj*, align 8
%fptrToInt57211 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48248 to i64
%ae48248 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57211)
store volatile %struct.ScmObj* %ae48248, %struct.ScmObj** %stackaddr$makeclosure57210, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48248, %struct.ScmObj* %_37foldr147094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48248, %struct.ScmObj* %_37foldl147078, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48248, %struct.ScmObj* %_37map147090, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48248, %struct.ScmObj* %Ycmb47073, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48248, %struct.ScmObj* %_37last47116, i64 4)
%ae48249 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57212 = alloca %struct.ScmObj*, align 8
%fptrToInt57213 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48250 to i64
%ae48250 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57213)
store volatile %struct.ScmObj* %ae48250, %struct.ScmObj** %stackaddr$makeclosure57212, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48250, %struct.ScmObj* %_37take47086, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48250, %struct.ScmObj* %_37length47083, i64 1)
%argslist56913$ae482480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57214 = alloca %struct.ScmObj*, align 8
%argslist56913$ae482481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48250, %struct.ScmObj* %argslist56913$ae482480)
store volatile %struct.ScmObj* %argslist56913$ae482481, %struct.ScmObj** %stackaddr$prim57214, align 8
%stackaddr$prim57215 = alloca %struct.ScmObj*, align 8
%argslist56913$ae482482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48249, %struct.ScmObj* %argslist56913$ae482481)
store volatile %struct.ScmObj* %argslist56913$ae482482, %struct.ScmObj** %stackaddr$prim57215, align 8
%clofunc57216 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48248)
musttail call tailcc void %clofunc57216(%struct.ScmObj* %ae48248, %struct.ScmObj* %argslist56913$ae482482)
ret void
}

define tailcc void @proc_clo$ae48248(%struct.ScmObj* %env$ae48248,%struct.ScmObj* %current_45args56440) {
%stackaddr$env-ref57217 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48248, i64 0)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref57217
%stackaddr$env-ref57218 = alloca %struct.ScmObj*, align 8
%_37foldl147078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48248, i64 1)
store %struct.ScmObj* %_37foldl147078, %struct.ScmObj** %stackaddr$env-ref57218
%stackaddr$env-ref57219 = alloca %struct.ScmObj*, align 8
%_37map147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48248, i64 2)
store %struct.ScmObj* %_37map147090, %struct.ScmObj** %stackaddr$env-ref57219
%stackaddr$env-ref57220 = alloca %struct.ScmObj*, align 8
%Ycmb47073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48248, i64 3)
store %struct.ScmObj* %Ycmb47073, %struct.ScmObj** %stackaddr$env-ref57220
%stackaddr$env-ref57221 = alloca %struct.ScmObj*, align 8
%_37last47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48248, i64 4)
store %struct.ScmObj* %_37last47116, %struct.ScmObj** %stackaddr$env-ref57221
%stackaddr$prim57222 = alloca %struct.ScmObj*, align 8
%_95k47395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56440)
store volatile %struct.ScmObj* %_95k47395, %struct.ScmObj** %stackaddr$prim57222, align 8
%stackaddr$prim57223 = alloca %struct.ScmObj*, align 8
%current_45args56441 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56440)
store volatile %struct.ScmObj* %current_45args56441, %struct.ScmObj** %stackaddr$prim57223, align 8
%stackaddr$prim57224 = alloca %struct.ScmObj*, align 8
%_37drop_45right47113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56441)
store volatile %struct.ScmObj* %_37drop_45right47113, %struct.ScmObj** %stackaddr$prim57224, align 8
%stackaddr$makeclosure57225 = alloca %struct.ScmObj*, align 8
%fptrToInt57226 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48278 to i64
%ae48278 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57226)
store volatile %struct.ScmObj* %ae48278, %struct.ScmObj** %stackaddr$makeclosure57225, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48278, %struct.ScmObj* %_37foldr147094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48278, %struct.ScmObj* %_37foldl147078, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48278, %struct.ScmObj* %Ycmb47073, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48278, %struct.ScmObj* %_37last47116, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48278, %struct.ScmObj* %_37drop_45right47113, i64 4)
%ae48279 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57227 = alloca %struct.ScmObj*, align 8
%fptrToInt57228 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48280 to i64
%ae48280 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57228)
store volatile %struct.ScmObj* %ae48280, %struct.ScmObj** %stackaddr$makeclosure57227, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48280, %struct.ScmObj* %_37foldr147094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48280, %struct.ScmObj* %_37map147090, i64 1)
%argslist56903$ae482780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57229 = alloca %struct.ScmObj*, align 8
%argslist56903$ae482781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48280, %struct.ScmObj* %argslist56903$ae482780)
store volatile %struct.ScmObj* %argslist56903$ae482781, %struct.ScmObj** %stackaddr$prim57229, align 8
%stackaddr$prim57230 = alloca %struct.ScmObj*, align 8
%argslist56903$ae482782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48279, %struct.ScmObj* %argslist56903$ae482781)
store volatile %struct.ScmObj* %argslist56903$ae482782, %struct.ScmObj** %stackaddr$prim57230, align 8
%clofunc57231 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48278)
musttail call tailcc void %clofunc57231(%struct.ScmObj* %ae48278, %struct.ScmObj* %argslist56903$ae482782)
ret void
}

define tailcc void @proc_clo$ae48278(%struct.ScmObj* %env$ae48278,%struct.ScmObj* %current_45args56443) {
%stackaddr$env-ref57232 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48278, i64 0)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref57232
%stackaddr$env-ref57233 = alloca %struct.ScmObj*, align 8
%_37foldl147078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48278, i64 1)
store %struct.ScmObj* %_37foldl147078, %struct.ScmObj** %stackaddr$env-ref57233
%stackaddr$env-ref57234 = alloca %struct.ScmObj*, align 8
%Ycmb47073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48278, i64 2)
store %struct.ScmObj* %Ycmb47073, %struct.ScmObj** %stackaddr$env-ref57234
%stackaddr$env-ref57235 = alloca %struct.ScmObj*, align 8
%_37last47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48278, i64 3)
store %struct.ScmObj* %_37last47116, %struct.ScmObj** %stackaddr$env-ref57235
%stackaddr$env-ref57236 = alloca %struct.ScmObj*, align 8
%_37drop_45right47113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48278, i64 4)
store %struct.ScmObj* %_37drop_45right47113, %struct.ScmObj** %stackaddr$env-ref57236
%stackaddr$prim57237 = alloca %struct.ScmObj*, align 8
%_95k47396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56443)
store volatile %struct.ScmObj* %_95k47396, %struct.ScmObj** %stackaddr$prim57237, align 8
%stackaddr$prim57238 = alloca %struct.ScmObj*, align 8
%current_45args56444 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56443)
store volatile %struct.ScmObj* %current_45args56444, %struct.ScmObj** %stackaddr$prim57238, align 8
%stackaddr$prim57239 = alloca %struct.ScmObj*, align 8
%anf_45bind47262 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56444)
store volatile %struct.ScmObj* %anf_45bind47262, %struct.ScmObj** %stackaddr$prim57239, align 8
%stackaddr$makeclosure57240 = alloca %struct.ScmObj*, align 8
%fptrToInt57241 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48662 to i64
%ae48662 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57241)
store volatile %struct.ScmObj* %ae48662, %struct.ScmObj** %stackaddr$makeclosure57240, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48662, %struct.ScmObj* %_37foldr147094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48662, %struct.ScmObj* %_37foldl147078, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48662, %struct.ScmObj* %Ycmb47073, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48662, %struct.ScmObj* %_37last47116, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48662, %struct.ScmObj* %_37drop_45right47113, i64 4)
%argslist56843$Ycmb470730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57242 = alloca %struct.ScmObj*, align 8
%argslist56843$Ycmb470731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47262, %struct.ScmObj* %argslist56843$Ycmb470730)
store volatile %struct.ScmObj* %argslist56843$Ycmb470731, %struct.ScmObj** %stackaddr$prim57242, align 8
%stackaddr$prim57243 = alloca %struct.ScmObj*, align 8
%argslist56843$Ycmb470732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48662, %struct.ScmObj* %argslist56843$Ycmb470731)
store volatile %struct.ScmObj* %argslist56843$Ycmb470732, %struct.ScmObj** %stackaddr$prim57243, align 8
%clofunc57244 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47073)
musttail call tailcc void %clofunc57244(%struct.ScmObj* %Ycmb47073, %struct.ScmObj* %argslist56843$Ycmb470732)
ret void
}

define tailcc void @proc_clo$ae48662(%struct.ScmObj* %env$ae48662,%struct.ScmObj* %current_45args56446) {
%stackaddr$env-ref57245 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48662, i64 0)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref57245
%stackaddr$env-ref57246 = alloca %struct.ScmObj*, align 8
%_37foldl147078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48662, i64 1)
store %struct.ScmObj* %_37foldl147078, %struct.ScmObj** %stackaddr$env-ref57246
%stackaddr$env-ref57247 = alloca %struct.ScmObj*, align 8
%Ycmb47073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48662, i64 2)
store %struct.ScmObj* %Ycmb47073, %struct.ScmObj** %stackaddr$env-ref57247
%stackaddr$env-ref57248 = alloca %struct.ScmObj*, align 8
%_37last47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48662, i64 3)
store %struct.ScmObj* %_37last47116, %struct.ScmObj** %stackaddr$env-ref57248
%stackaddr$env-ref57249 = alloca %struct.ScmObj*, align 8
%_37drop_45right47113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48662, i64 4)
store %struct.ScmObj* %_37drop_45right47113, %struct.ScmObj** %stackaddr$env-ref57249
%stackaddr$prim57250 = alloca %struct.ScmObj*, align 8
%_95k47397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56446)
store volatile %struct.ScmObj* %_95k47397, %struct.ScmObj** %stackaddr$prim57250, align 8
%stackaddr$prim57251 = alloca %struct.ScmObj*, align 8
%current_45args56447 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56446)
store volatile %struct.ScmObj* %current_45args56447, %struct.ScmObj** %stackaddr$prim57251, align 8
%stackaddr$prim57252 = alloca %struct.ScmObj*, align 8
%_37foldr47099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56447)
store volatile %struct.ScmObj* %_37foldr47099, %struct.ScmObj** %stackaddr$prim57252, align 8
%stackaddr$makeclosure57253 = alloca %struct.ScmObj*, align 8
%fptrToInt57254 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48664 to i64
%ae48664 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt57254)
store volatile %struct.ScmObj* %ae48664, %struct.ScmObj** %stackaddr$makeclosure57253, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48664, %struct.ScmObj* %_37foldr147094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48664, %struct.ScmObj* %_37foldl147078, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48664, %struct.ScmObj* %Ycmb47073, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48664, %struct.ScmObj* %_37last47116, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48664, %struct.ScmObj* %_37foldr47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48664, %struct.ScmObj* %_37drop_45right47113, i64 5)
%ae48665 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57255 = alloca %struct.ScmObj*, align 8
%fptrToInt57256 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48666 to i64
%ae48666 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57256)
store volatile %struct.ScmObj* %ae48666, %struct.ScmObj** %stackaddr$makeclosure57255, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48666, %struct.ScmObj* %_37foldr147094, i64 0)
%argslist56842$ae486640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57257 = alloca %struct.ScmObj*, align 8
%argslist56842$ae486641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48666, %struct.ScmObj* %argslist56842$ae486640)
store volatile %struct.ScmObj* %argslist56842$ae486641, %struct.ScmObj** %stackaddr$prim57257, align 8
%stackaddr$prim57258 = alloca %struct.ScmObj*, align 8
%argslist56842$ae486642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48665, %struct.ScmObj* %argslist56842$ae486641)
store volatile %struct.ScmObj* %argslist56842$ae486642, %struct.ScmObj** %stackaddr$prim57258, align 8
%clofunc57259 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48664)
musttail call tailcc void %clofunc57259(%struct.ScmObj* %ae48664, %struct.ScmObj* %argslist56842$ae486642)
ret void
}

define tailcc void @proc_clo$ae48664(%struct.ScmObj* %env$ae48664,%struct.ScmObj* %current_45args56449) {
%stackaddr$env-ref57260 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48664, i64 0)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref57260
%stackaddr$env-ref57261 = alloca %struct.ScmObj*, align 8
%_37foldl147078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48664, i64 1)
store %struct.ScmObj* %_37foldl147078, %struct.ScmObj** %stackaddr$env-ref57261
%stackaddr$env-ref57262 = alloca %struct.ScmObj*, align 8
%Ycmb47073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48664, i64 2)
store %struct.ScmObj* %Ycmb47073, %struct.ScmObj** %stackaddr$env-ref57262
%stackaddr$env-ref57263 = alloca %struct.ScmObj*, align 8
%_37last47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48664, i64 3)
store %struct.ScmObj* %_37last47116, %struct.ScmObj** %stackaddr$env-ref57263
%stackaddr$env-ref57264 = alloca %struct.ScmObj*, align 8
%_37foldr47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48664, i64 4)
store %struct.ScmObj* %_37foldr47099, %struct.ScmObj** %stackaddr$env-ref57264
%stackaddr$env-ref57265 = alloca %struct.ScmObj*, align 8
%_37drop_45right47113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48664, i64 5)
store %struct.ScmObj* %_37drop_45right47113, %struct.ScmObj** %stackaddr$env-ref57265
%stackaddr$prim57266 = alloca %struct.ScmObj*, align 8
%_95k47398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56449)
store volatile %struct.ScmObj* %_95k47398, %struct.ScmObj** %stackaddr$prim57266, align 8
%stackaddr$prim57267 = alloca %struct.ScmObj*, align 8
%current_45args56450 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56449)
store volatile %struct.ScmObj* %current_45args56450, %struct.ScmObj** %stackaddr$prim57267, align 8
%stackaddr$prim57268 = alloca %struct.ScmObj*, align 8
%_37map147125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56450)
store volatile %struct.ScmObj* %_37map147125, %struct.ScmObj** %stackaddr$prim57268, align 8
%stackaddr$makeclosure57269 = alloca %struct.ScmObj*, align 8
%fptrToInt57270 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48741 to i64
%ae48741 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57270)
store volatile %struct.ScmObj* %ae48741, %struct.ScmObj** %stackaddr$makeclosure57269, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48741, %struct.ScmObj* %_37foldr147094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48741, %struct.ScmObj* %_37foldl147078, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48741, %struct.ScmObj* %_37map147125, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48741, %struct.ScmObj* %Ycmb47073, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48741, %struct.ScmObj* %_37foldr47099, i64 4)
%ae48742 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57271 = alloca %struct.ScmObj*, align 8
%fptrToInt57272 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48743 to i64
%ae48743 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57272)
store volatile %struct.ScmObj* %ae48743, %struct.ScmObj** %stackaddr$makeclosure57271, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48743, %struct.ScmObj* %_37last47116, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48743, %struct.ScmObj* %_37foldr47099, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48743, %struct.ScmObj* %_37drop_45right47113, i64 2)
%argslist56823$ae487410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57273 = alloca %struct.ScmObj*, align 8
%argslist56823$ae487411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48743, %struct.ScmObj* %argslist56823$ae487410)
store volatile %struct.ScmObj* %argslist56823$ae487411, %struct.ScmObj** %stackaddr$prim57273, align 8
%stackaddr$prim57274 = alloca %struct.ScmObj*, align 8
%argslist56823$ae487412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48742, %struct.ScmObj* %argslist56823$ae487411)
store volatile %struct.ScmObj* %argslist56823$ae487412, %struct.ScmObj** %stackaddr$prim57274, align 8
%clofunc57275 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48741)
musttail call tailcc void %clofunc57275(%struct.ScmObj* %ae48741, %struct.ScmObj* %argslist56823$ae487412)
ret void
}

define tailcc void @proc_clo$ae48741(%struct.ScmObj* %env$ae48741,%struct.ScmObj* %current_45args56452) {
%stackaddr$env-ref57276 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48741, i64 0)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref57276
%stackaddr$env-ref57277 = alloca %struct.ScmObj*, align 8
%_37foldl147078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48741, i64 1)
store %struct.ScmObj* %_37foldl147078, %struct.ScmObj** %stackaddr$env-ref57277
%stackaddr$env-ref57278 = alloca %struct.ScmObj*, align 8
%_37map147125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48741, i64 2)
store %struct.ScmObj* %_37map147125, %struct.ScmObj** %stackaddr$env-ref57278
%stackaddr$env-ref57279 = alloca %struct.ScmObj*, align 8
%Ycmb47073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48741, i64 3)
store %struct.ScmObj* %Ycmb47073, %struct.ScmObj** %stackaddr$env-ref57279
%stackaddr$env-ref57280 = alloca %struct.ScmObj*, align 8
%_37foldr47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48741, i64 4)
store %struct.ScmObj* %_37foldr47099, %struct.ScmObj** %stackaddr$env-ref57280
%stackaddr$prim57281 = alloca %struct.ScmObj*, align 8
%_95k47399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56452)
store volatile %struct.ScmObj* %_95k47399, %struct.ScmObj** %stackaddr$prim57281, align 8
%stackaddr$prim57282 = alloca %struct.ScmObj*, align 8
%current_45args56453 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56452)
store volatile %struct.ScmObj* %current_45args56453, %struct.ScmObj** %stackaddr$prim57282, align 8
%stackaddr$prim57283 = alloca %struct.ScmObj*, align 8
%_37map47120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56453)
store volatile %struct.ScmObj* %_37map47120, %struct.ScmObj** %stackaddr$prim57283, align 8
%stackaddr$makeclosure57284 = alloca %struct.ScmObj*, align 8
%fptrToInt57285 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48887 to i64
%ae48887 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57285)
store volatile %struct.ScmObj* %ae48887, %struct.ScmObj** %stackaddr$makeclosure57284, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48887, %struct.ScmObj* %_37foldl147078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48887, %struct.ScmObj* %Ycmb47073, i64 1)
%ae48888 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57286 = alloca %struct.ScmObj*, align 8
%fptrToInt57287 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48889 to i64
%ae48889 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57287)
store volatile %struct.ScmObj* %ae48889, %struct.ScmObj** %stackaddr$makeclosure57286, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48889, %struct.ScmObj* %_37foldr147094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48889, %struct.ScmObj* %_37map147125, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48889, %struct.ScmObj* %_37foldr47099, i64 2)
%argslist56806$ae488870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57288 = alloca %struct.ScmObj*, align 8
%argslist56806$ae488871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48889, %struct.ScmObj* %argslist56806$ae488870)
store volatile %struct.ScmObj* %argslist56806$ae488871, %struct.ScmObj** %stackaddr$prim57288, align 8
%stackaddr$prim57289 = alloca %struct.ScmObj*, align 8
%argslist56806$ae488872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48888, %struct.ScmObj* %argslist56806$ae488871)
store volatile %struct.ScmObj* %argslist56806$ae488872, %struct.ScmObj** %stackaddr$prim57289, align 8
%clofunc57290 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48887)
musttail call tailcc void %clofunc57290(%struct.ScmObj* %ae48887, %struct.ScmObj* %argslist56806$ae488872)
ret void
}

define tailcc void @proc_clo$ae48887(%struct.ScmObj* %env$ae48887,%struct.ScmObj* %current_45args56455) {
%stackaddr$env-ref57291 = alloca %struct.ScmObj*, align 8
%_37foldl147078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48887, i64 0)
store %struct.ScmObj* %_37foldl147078, %struct.ScmObj** %stackaddr$env-ref57291
%stackaddr$env-ref57292 = alloca %struct.ScmObj*, align 8
%Ycmb47073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48887, i64 1)
store %struct.ScmObj* %Ycmb47073, %struct.ScmObj** %stackaddr$env-ref57292
%stackaddr$prim57293 = alloca %struct.ScmObj*, align 8
%_95k47400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56455)
store volatile %struct.ScmObj* %_95k47400, %struct.ScmObj** %stackaddr$prim57293, align 8
%stackaddr$prim57294 = alloca %struct.ScmObj*, align 8
%current_45args56456 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56455)
store volatile %struct.ScmObj* %current_45args56456, %struct.ScmObj** %stackaddr$prim57294, align 8
%stackaddr$prim57295 = alloca %struct.ScmObj*, align 8
%anf_45bind47282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56456)
store volatile %struct.ScmObj* %anf_45bind47282, %struct.ScmObj** %stackaddr$prim57295, align 8
%stackaddr$makeclosure57296 = alloca %struct.ScmObj*, align 8
%fptrToInt57297 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49279 to i64
%ae49279 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57297)
store volatile %struct.ScmObj* %ae49279, %struct.ScmObj** %stackaddr$makeclosure57296, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49279, %struct.ScmObj* %_37foldl147078, i64 0)
%argslist56746$Ycmb470730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57298 = alloca %struct.ScmObj*, align 8
%argslist56746$Ycmb470731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47282, %struct.ScmObj* %argslist56746$Ycmb470730)
store volatile %struct.ScmObj* %argslist56746$Ycmb470731, %struct.ScmObj** %stackaddr$prim57298, align 8
%stackaddr$prim57299 = alloca %struct.ScmObj*, align 8
%argslist56746$Ycmb470732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49279, %struct.ScmObj* %argslist56746$Ycmb470731)
store volatile %struct.ScmObj* %argslist56746$Ycmb470732, %struct.ScmObj** %stackaddr$prim57299, align 8
%clofunc57300 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47073)
musttail call tailcc void %clofunc57300(%struct.ScmObj* %Ycmb47073, %struct.ScmObj* %argslist56746$Ycmb470732)
ret void
}

define tailcc void @proc_clo$ae49279(%struct.ScmObj* %env$ae49279,%struct.ScmObj* %current_45args56458) {
%stackaddr$env-ref57301 = alloca %struct.ScmObj*, align 8
%_37foldl147078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49279, i64 0)
store %struct.ScmObj* %_37foldl147078, %struct.ScmObj** %stackaddr$env-ref57301
%stackaddr$prim57302 = alloca %struct.ScmObj*, align 8
%_95k47401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56458)
store volatile %struct.ScmObj* %_95k47401, %struct.ScmObj** %stackaddr$prim57302, align 8
%stackaddr$prim57303 = alloca %struct.ScmObj*, align 8
%current_45args56459 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56458)
store volatile %struct.ScmObj* %current_45args56459, %struct.ScmObj** %stackaddr$prim57303, align 8
%stackaddr$prim57304 = alloca %struct.ScmObj*, align 8
%_37foldl47176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56459)
store volatile %struct.ScmObj* %_37foldl47176, %struct.ScmObj** %stackaddr$prim57304, align 8
%stackaddr$makeclosure57305 = alloca %struct.ScmObj*, align 8
%fptrToInt57306 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49281 to i64
%ae49281 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57306)
store volatile %struct.ScmObj* %ae49281, %struct.ScmObj** %stackaddr$makeclosure57305, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49281, %struct.ScmObj* %_37foldl147078, i64 0)
%ae49282 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57307 = alloca %struct.ScmObj*, align 8
%fptrToInt57308 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49283 to i64
%ae49283 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57308)
store volatile %struct.ScmObj* %ae49283, %struct.ScmObj** %stackaddr$makeclosure57307, align 8
%argslist56745$ae492810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57309 = alloca %struct.ScmObj*, align 8
%argslist56745$ae492811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49283, %struct.ScmObj* %argslist56745$ae492810)
store volatile %struct.ScmObj* %argslist56745$ae492811, %struct.ScmObj** %stackaddr$prim57309, align 8
%stackaddr$prim57310 = alloca %struct.ScmObj*, align 8
%argslist56745$ae492812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49282, %struct.ScmObj* %argslist56745$ae492811)
store volatile %struct.ScmObj* %argslist56745$ae492812, %struct.ScmObj** %stackaddr$prim57310, align 8
%clofunc57311 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49281)
musttail call tailcc void %clofunc57311(%struct.ScmObj* %ae49281, %struct.ScmObj* %argslist56745$ae492812)
ret void
}

define tailcc void @proc_clo$ae49281(%struct.ScmObj* %env$ae49281,%struct.ScmObj* %current_45args56461) {
%stackaddr$env-ref57312 = alloca %struct.ScmObj*, align 8
%_37foldl147078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49281, i64 0)
store %struct.ScmObj* %_37foldl147078, %struct.ScmObj** %stackaddr$env-ref57312
%stackaddr$prim57313 = alloca %struct.ScmObj*, align 8
%_95k47402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56461)
store volatile %struct.ScmObj* %_95k47402, %struct.ScmObj** %stackaddr$prim57313, align 8
%stackaddr$prim57314 = alloca %struct.ScmObj*, align 8
%current_45args56462 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56461)
store volatile %struct.ScmObj* %current_45args56462, %struct.ScmObj** %stackaddr$prim57314, align 8
%stackaddr$prim57315 = alloca %struct.ScmObj*, align 8
%_37_6247173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56462)
store volatile %struct.ScmObj* %_37_6247173, %struct.ScmObj** %stackaddr$prim57315, align 8
%stackaddr$makeclosure57316 = alloca %struct.ScmObj*, align 8
%fptrToInt57317 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49305 to i64
%ae49305 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57317)
store volatile %struct.ScmObj* %ae49305, %struct.ScmObj** %stackaddr$makeclosure57316, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49305, %struct.ScmObj* %_37foldl147078, i64 0)
%ae49306 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57318 = alloca %struct.ScmObj*, align 8
%fptrToInt57319 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49307 to i64
%ae49307 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57319)
store volatile %struct.ScmObj* %ae49307, %struct.ScmObj** %stackaddr$makeclosure57318, align 8
%argslist56739$ae493050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57320 = alloca %struct.ScmObj*, align 8
%argslist56739$ae493051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49307, %struct.ScmObj* %argslist56739$ae493050)
store volatile %struct.ScmObj* %argslist56739$ae493051, %struct.ScmObj** %stackaddr$prim57320, align 8
%stackaddr$prim57321 = alloca %struct.ScmObj*, align 8
%argslist56739$ae493052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49306, %struct.ScmObj* %argslist56739$ae493051)
store volatile %struct.ScmObj* %argslist56739$ae493052, %struct.ScmObj** %stackaddr$prim57321, align 8
%clofunc57322 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49305)
musttail call tailcc void %clofunc57322(%struct.ScmObj* %ae49305, %struct.ScmObj* %argslist56739$ae493052)
ret void
}

define tailcc void @proc_clo$ae49305(%struct.ScmObj* %env$ae49305,%struct.ScmObj* %current_45args56464) {
%stackaddr$env-ref57323 = alloca %struct.ScmObj*, align 8
%_37foldl147078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49305, i64 0)
store %struct.ScmObj* %_37foldl147078, %struct.ScmObj** %stackaddr$env-ref57323
%stackaddr$prim57324 = alloca %struct.ScmObj*, align 8
%_95k47403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56464)
store volatile %struct.ScmObj* %_95k47403, %struct.ScmObj** %stackaddr$prim57324, align 8
%stackaddr$prim57325 = alloca %struct.ScmObj*, align 8
%current_45args56465 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56464)
store volatile %struct.ScmObj* %current_45args56465, %struct.ScmObj** %stackaddr$prim57325, align 8
%stackaddr$prim57326 = alloca %struct.ScmObj*, align 8
%_37_62_6147170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56465)
store volatile %struct.ScmObj* %_37_62_6147170, %struct.ScmObj** %stackaddr$prim57326, align 8
%ae49329 = call %struct.ScmObj* @const_init_int(i64 1)
%ae49330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57327 = alloca %struct.ScmObj*, align 8
%_37append47166 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49329, %struct.ScmObj* %ae49330)
store volatile %struct.ScmObj* %_37append47166, %struct.ScmObj** %stackaddr$prim57327, align 8
%stackaddr$makeclosure57328 = alloca %struct.ScmObj*, align 8
%fptrToInt57329 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49331 to i64
%ae49331 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57329)
store volatile %struct.ScmObj* %ae49331, %struct.ScmObj** %stackaddr$makeclosure57328, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49331, %struct.ScmObj* %_37foldl147078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49331, %struct.ScmObj* %_37append47166, i64 1)
%ae49332 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57330 = alloca %struct.ScmObj*, align 8
%fptrToInt57331 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49333 to i64
%ae49333 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57331)
store volatile %struct.ScmObj* %ae49333, %struct.ScmObj** %stackaddr$makeclosure57330, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49333, %struct.ScmObj* %_37append47166, i64 0)
%argslist56733$ae493310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57332 = alloca %struct.ScmObj*, align 8
%argslist56733$ae493311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49333, %struct.ScmObj* %argslist56733$ae493310)
store volatile %struct.ScmObj* %argslist56733$ae493311, %struct.ScmObj** %stackaddr$prim57332, align 8
%stackaddr$prim57333 = alloca %struct.ScmObj*, align 8
%argslist56733$ae493312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49332, %struct.ScmObj* %argslist56733$ae493311)
store volatile %struct.ScmObj* %argslist56733$ae493312, %struct.ScmObj** %stackaddr$prim57333, align 8
%clofunc57334 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49331)
musttail call tailcc void %clofunc57334(%struct.ScmObj* %ae49331, %struct.ScmObj* %argslist56733$ae493312)
ret void
}

define tailcc void @proc_clo$ae49331(%struct.ScmObj* %env$ae49331,%struct.ScmObj* %current_45args56467) {
%stackaddr$env-ref57335 = alloca %struct.ScmObj*, align 8
%_37foldl147078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49331, i64 0)
store %struct.ScmObj* %_37foldl147078, %struct.ScmObj** %stackaddr$env-ref57335
%stackaddr$env-ref57336 = alloca %struct.ScmObj*, align 8
%_37append47166 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49331, i64 1)
store %struct.ScmObj* %_37append47166, %struct.ScmObj** %stackaddr$env-ref57336
%stackaddr$prim57337 = alloca %struct.ScmObj*, align 8
%_95k47404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56467)
store volatile %struct.ScmObj* %_95k47404, %struct.ScmObj** %stackaddr$prim57337, align 8
%stackaddr$prim57338 = alloca %struct.ScmObj*, align 8
%current_45args56468 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56467)
store volatile %struct.ScmObj* %current_45args56468, %struct.ScmObj** %stackaddr$prim57338, align 8
%stackaddr$prim57339 = alloca %struct.ScmObj*, align 8
%anf_45bind47290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56468)
store volatile %struct.ScmObj* %anf_45bind47290, %struct.ScmObj** %stackaddr$prim57339, align 8
%ae49399 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57340 = alloca %struct.ScmObj*, align 8
%_95047167 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append47166, %struct.ScmObj* %ae49399, %struct.ScmObj* %anf_45bind47290)
store volatile %struct.ScmObj* %_95047167, %struct.ScmObj** %stackaddr$prim57340, align 8
%ae49402 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57341 = alloca %struct.ScmObj*, align 8
%_37append47165 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47166, %struct.ScmObj* %ae49402)
store volatile %struct.ScmObj* %_37append47165, %struct.ScmObj** %stackaddr$prim57341, align 8
%stackaddr$makeclosure57342 = alloca %struct.ScmObj*, align 8
%fptrToInt57343 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49403 to i64
%ae49403 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57343)
store volatile %struct.ScmObj* %ae49403, %struct.ScmObj** %stackaddr$makeclosure57342, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49403, %struct.ScmObj* %_37foldl147078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49403, %struct.ScmObj* %_37append47165, i64 1)
%ae49404 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57344 = alloca %struct.ScmObj*, align 8
%fptrToInt57345 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49405 to i64
%ae49405 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57345)
store volatile %struct.ScmObj* %ae49405, %struct.ScmObj** %stackaddr$makeclosure57344, align 8
%argslist56722$ae494030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57346 = alloca %struct.ScmObj*, align 8
%argslist56722$ae494031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49405, %struct.ScmObj* %argslist56722$ae494030)
store volatile %struct.ScmObj* %argslist56722$ae494031, %struct.ScmObj** %stackaddr$prim57346, align 8
%stackaddr$prim57347 = alloca %struct.ScmObj*, align 8
%argslist56722$ae494032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49404, %struct.ScmObj* %argslist56722$ae494031)
store volatile %struct.ScmObj* %argslist56722$ae494032, %struct.ScmObj** %stackaddr$prim57347, align 8
%clofunc57348 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49403)
musttail call tailcc void %clofunc57348(%struct.ScmObj* %ae49403, %struct.ScmObj* %argslist56722$ae494032)
ret void
}

define tailcc void @proc_clo$ae49403(%struct.ScmObj* %env$ae49403,%struct.ScmObj* %current_45args56470) {
%stackaddr$env-ref57349 = alloca %struct.ScmObj*, align 8
%_37foldl147078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49403, i64 0)
store %struct.ScmObj* %_37foldl147078, %struct.ScmObj** %stackaddr$env-ref57349
%stackaddr$env-ref57350 = alloca %struct.ScmObj*, align 8
%_37append47165 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49403, i64 1)
store %struct.ScmObj* %_37append47165, %struct.ScmObj** %stackaddr$env-ref57350
%stackaddr$prim57351 = alloca %struct.ScmObj*, align 8
%_95k47405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56470)
store volatile %struct.ScmObj* %_95k47405, %struct.ScmObj** %stackaddr$prim57351, align 8
%stackaddr$prim57352 = alloca %struct.ScmObj*, align 8
%current_45args56471 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56470)
store volatile %struct.ScmObj* %current_45args56471, %struct.ScmObj** %stackaddr$prim57352, align 8
%stackaddr$prim57353 = alloca %struct.ScmObj*, align 8
%_37list_6347158 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56471)
store volatile %struct.ScmObj* %_37list_6347158, %struct.ScmObj** %stackaddr$prim57353, align 8
%stackaddr$makeclosure57354 = alloca %struct.ScmObj*, align 8
%fptrToInt57355 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49819 to i64
%ae49819 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57355)
store volatile %struct.ScmObj* %ae49819, %struct.ScmObj** %stackaddr$makeclosure57354, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49819, %struct.ScmObj* %_37foldl147078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49819, %struct.ScmObj* %_37append47165, i64 1)
%ae49820 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57356 = alloca %struct.ScmObj*, align 8
%fptrToInt57357 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49821 to i64
%ae49821 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57357)
store volatile %struct.ScmObj* %ae49821, %struct.ScmObj** %stackaddr$makeclosure57356, align 8
%argslist56697$ae498190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57358 = alloca %struct.ScmObj*, align 8
%argslist56697$ae498191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49821, %struct.ScmObj* %argslist56697$ae498190)
store volatile %struct.ScmObj* %argslist56697$ae498191, %struct.ScmObj** %stackaddr$prim57358, align 8
%stackaddr$prim57359 = alloca %struct.ScmObj*, align 8
%argslist56697$ae498192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49820, %struct.ScmObj* %argslist56697$ae498191)
store volatile %struct.ScmObj* %argslist56697$ae498192, %struct.ScmObj** %stackaddr$prim57359, align 8
%clofunc57360 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49819)
musttail call tailcc void %clofunc57360(%struct.ScmObj* %ae49819, %struct.ScmObj* %argslist56697$ae498192)
ret void
}

define tailcc void @proc_clo$ae49819(%struct.ScmObj* %env$ae49819,%struct.ScmObj* %current_45args56473) {
%stackaddr$env-ref57361 = alloca %struct.ScmObj*, align 8
%_37foldl147078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49819, i64 0)
store %struct.ScmObj* %_37foldl147078, %struct.ScmObj** %stackaddr$env-ref57361
%stackaddr$env-ref57362 = alloca %struct.ScmObj*, align 8
%_37append47165 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49819, i64 1)
store %struct.ScmObj* %_37append47165, %struct.ScmObj** %stackaddr$env-ref57362
%stackaddr$prim57363 = alloca %struct.ScmObj*, align 8
%_95k47406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56473)
store volatile %struct.ScmObj* %_95k47406, %struct.ScmObj** %stackaddr$prim57363, align 8
%stackaddr$prim57364 = alloca %struct.ScmObj*, align 8
%current_45args56474 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56473)
store volatile %struct.ScmObj* %current_45args56474, %struct.ScmObj** %stackaddr$prim57364, align 8
%stackaddr$prim57365 = alloca %struct.ScmObj*, align 8
%_37drop47149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56474)
store volatile %struct.ScmObj* %_37drop47149, %struct.ScmObj** %stackaddr$prim57365, align 8
%stackaddr$makeclosure57366 = alloca %struct.ScmObj*, align 8
%fptrToInt57367 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50355 to i64
%ae50355 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57367)
store volatile %struct.ScmObj* %ae50355, %struct.ScmObj** %stackaddr$makeclosure57366, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50355, %struct.ScmObj* %_37foldl147078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50355, %struct.ScmObj* %_37append47165, i64 1)
%ae50356 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57368 = alloca %struct.ScmObj*, align 8
%fptrToInt57369 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50357 to i64
%ae50357 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57369)
store volatile %struct.ScmObj* %ae50357, %struct.ScmObj** %stackaddr$makeclosure57368, align 8
%argslist56673$ae503550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57370 = alloca %struct.ScmObj*, align 8
%argslist56673$ae503551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50357, %struct.ScmObj* %argslist56673$ae503550)
store volatile %struct.ScmObj* %argslist56673$ae503551, %struct.ScmObj** %stackaddr$prim57370, align 8
%stackaddr$prim57371 = alloca %struct.ScmObj*, align 8
%argslist56673$ae503552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50356, %struct.ScmObj* %argslist56673$ae503551)
store volatile %struct.ScmObj* %argslist56673$ae503552, %struct.ScmObj** %stackaddr$prim57371, align 8
%clofunc57372 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50355)
musttail call tailcc void %clofunc57372(%struct.ScmObj* %ae50355, %struct.ScmObj* %argslist56673$ae503552)
ret void
}

define tailcc void @proc_clo$ae50355(%struct.ScmObj* %env$ae50355,%struct.ScmObj* %current_45args56476) {
%stackaddr$env-ref57373 = alloca %struct.ScmObj*, align 8
%_37foldl147078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50355, i64 0)
store %struct.ScmObj* %_37foldl147078, %struct.ScmObj** %stackaddr$env-ref57373
%stackaddr$env-ref57374 = alloca %struct.ScmObj*, align 8
%_37append47165 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50355, i64 1)
store %struct.ScmObj* %_37append47165, %struct.ScmObj** %stackaddr$env-ref57374
%stackaddr$prim57375 = alloca %struct.ScmObj*, align 8
%_95k47407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56476)
store volatile %struct.ScmObj* %_95k47407, %struct.ScmObj** %stackaddr$prim57375, align 8
%stackaddr$prim57376 = alloca %struct.ScmObj*, align 8
%current_45args56477 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56476)
store volatile %struct.ScmObj* %current_45args56477, %struct.ScmObj** %stackaddr$prim57376, align 8
%stackaddr$prim57377 = alloca %struct.ScmObj*, align 8
%_37memv47142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56477)
store volatile %struct.ScmObj* %_37memv47142, %struct.ScmObj** %stackaddr$prim57377, align 8
%stackaddr$makeclosure57378 = alloca %struct.ScmObj*, align 8
%fptrToInt57379 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50759 to i64
%ae50759 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57379)
store volatile %struct.ScmObj* %ae50759, %struct.ScmObj** %stackaddr$makeclosure57378, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50759, %struct.ScmObj* %_37append47165, i64 0)
%ae50760 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57380 = alloca %struct.ScmObj*, align 8
%fptrToInt57381 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50761 to i64
%ae50761 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57381)
store volatile %struct.ScmObj* %ae50761, %struct.ScmObj** %stackaddr$makeclosure57380, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50761, %struct.ScmObj* %_37foldl147078, i64 0)
%argslist56647$ae507590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57382 = alloca %struct.ScmObj*, align 8
%argslist56647$ae507591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50761, %struct.ScmObj* %argslist56647$ae507590)
store volatile %struct.ScmObj* %argslist56647$ae507591, %struct.ScmObj** %stackaddr$prim57382, align 8
%stackaddr$prim57383 = alloca %struct.ScmObj*, align 8
%argslist56647$ae507592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50760, %struct.ScmObj* %argslist56647$ae507591)
store volatile %struct.ScmObj* %argslist56647$ae507592, %struct.ScmObj** %stackaddr$prim57383, align 8
%clofunc57384 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50759)
musttail call tailcc void %clofunc57384(%struct.ScmObj* %ae50759, %struct.ScmObj* %argslist56647$ae507592)
ret void
}

define tailcc void @proc_clo$ae50759(%struct.ScmObj* %env$ae50759,%struct.ScmObj* %current_45args56479) {
%stackaddr$env-ref57385 = alloca %struct.ScmObj*, align 8
%_37append47165 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50759, i64 0)
store %struct.ScmObj* %_37append47165, %struct.ScmObj** %stackaddr$env-ref57385
%stackaddr$prim57386 = alloca %struct.ScmObj*, align 8
%_95k47408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56479)
store volatile %struct.ScmObj* %_95k47408, %struct.ScmObj** %stackaddr$prim57386, align 8
%stackaddr$prim57387 = alloca %struct.ScmObj*, align 8
%current_45args56480 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56479)
store volatile %struct.ScmObj* %current_45args56480, %struct.ScmObj** %stackaddr$prim57387, align 8
%stackaddr$prim57388 = alloca %struct.ScmObj*, align 8
%_37_4747138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56480)
store volatile %struct.ScmObj* %_37_4747138, %struct.ScmObj** %stackaddr$prim57388, align 8
%stackaddr$makeclosure57389 = alloca %struct.ScmObj*, align 8
%fptrToInt57390 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50857 to i64
%ae50857 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57390)
store volatile %struct.ScmObj* %ae50857, %struct.ScmObj** %stackaddr$makeclosure57389, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50857, %struct.ScmObj* %_37append47165, i64 0)
%ae50858 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57391 = alloca %struct.ScmObj*, align 8
%fptrToInt57392 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50859 to i64
%ae50859 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57392)
store volatile %struct.ScmObj* %ae50859, %struct.ScmObj** %stackaddr$makeclosure57391, align 8
%argslist56634$ae508570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57393 = alloca %struct.ScmObj*, align 8
%argslist56634$ae508571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50859, %struct.ScmObj* %argslist56634$ae508570)
store volatile %struct.ScmObj* %argslist56634$ae508571, %struct.ScmObj** %stackaddr$prim57393, align 8
%stackaddr$prim57394 = alloca %struct.ScmObj*, align 8
%argslist56634$ae508572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50858, %struct.ScmObj* %argslist56634$ae508571)
store volatile %struct.ScmObj* %argslist56634$ae508572, %struct.ScmObj** %stackaddr$prim57394, align 8
%clofunc57395 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50857)
musttail call tailcc void %clofunc57395(%struct.ScmObj* %ae50857, %struct.ScmObj* %argslist56634$ae508572)
ret void
}

define tailcc void @proc_clo$ae50857(%struct.ScmObj* %env$ae50857,%struct.ScmObj* %current_45args56482) {
%stackaddr$env-ref57396 = alloca %struct.ScmObj*, align 8
%_37append47165 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50857, i64 0)
store %struct.ScmObj* %_37append47165, %struct.ScmObj** %stackaddr$env-ref57396
%stackaddr$prim57397 = alloca %struct.ScmObj*, align 8
%_95k47409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56482)
store volatile %struct.ScmObj* %_95k47409, %struct.ScmObj** %stackaddr$prim57397, align 8
%stackaddr$prim57398 = alloca %struct.ScmObj*, align 8
%current_45args56483 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56482)
store volatile %struct.ScmObj* %current_45args56483, %struct.ScmObj** %stackaddr$prim57398, align 8
%stackaddr$prim57399 = alloca %struct.ScmObj*, align 8
%_37first47136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56483)
store volatile %struct.ScmObj* %_37first47136, %struct.ScmObj** %stackaddr$prim57399, align 8
%stackaddr$makeclosure57400 = alloca %struct.ScmObj*, align 8
%fptrToInt57401 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50877 to i64
%ae50877 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57401)
store volatile %struct.ScmObj* %ae50877, %struct.ScmObj** %stackaddr$makeclosure57400, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50877, %struct.ScmObj* %_37append47165, i64 0)
%ae50878 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57402 = alloca %struct.ScmObj*, align 8
%fptrToInt57403 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50879 to i64
%ae50879 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57403)
store volatile %struct.ScmObj* %ae50879, %struct.ScmObj** %stackaddr$makeclosure57402, align 8
%argslist56629$ae508770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57404 = alloca %struct.ScmObj*, align 8
%argslist56629$ae508771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50879, %struct.ScmObj* %argslist56629$ae508770)
store volatile %struct.ScmObj* %argslist56629$ae508771, %struct.ScmObj** %stackaddr$prim57404, align 8
%stackaddr$prim57405 = alloca %struct.ScmObj*, align 8
%argslist56629$ae508772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50878, %struct.ScmObj* %argslist56629$ae508771)
store volatile %struct.ScmObj* %argslist56629$ae508772, %struct.ScmObj** %stackaddr$prim57405, align 8
%clofunc57406 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50877)
musttail call tailcc void %clofunc57406(%struct.ScmObj* %ae50877, %struct.ScmObj* %argslist56629$ae508772)
ret void
}

define tailcc void @proc_clo$ae50877(%struct.ScmObj* %env$ae50877,%struct.ScmObj* %current_45args56485) {
%stackaddr$env-ref57407 = alloca %struct.ScmObj*, align 8
%_37append47165 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50877, i64 0)
store %struct.ScmObj* %_37append47165, %struct.ScmObj** %stackaddr$env-ref57407
%stackaddr$prim57408 = alloca %struct.ScmObj*, align 8
%_95k47410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56485)
store volatile %struct.ScmObj* %_95k47410, %struct.ScmObj** %stackaddr$prim57408, align 8
%stackaddr$prim57409 = alloca %struct.ScmObj*, align 8
%current_45args56486 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56485)
store volatile %struct.ScmObj* %current_45args56486, %struct.ScmObj** %stackaddr$prim57409, align 8
%stackaddr$prim57410 = alloca %struct.ScmObj*, align 8
%_37second47134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56486)
store volatile %struct.ScmObj* %_37second47134, %struct.ScmObj** %stackaddr$prim57410, align 8
%stackaddr$makeclosure57411 = alloca %struct.ScmObj*, align 8
%fptrToInt57412 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50899 to i64
%ae50899 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57412)
store volatile %struct.ScmObj* %ae50899, %struct.ScmObj** %stackaddr$makeclosure57411, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50899, %struct.ScmObj* %_37append47165, i64 0)
%ae50900 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57413 = alloca %struct.ScmObj*, align 8
%fptrToInt57414 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50901 to i64
%ae50901 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57414)
store volatile %struct.ScmObj* %ae50901, %struct.ScmObj** %stackaddr$makeclosure57413, align 8
%argslist56624$ae508990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57415 = alloca %struct.ScmObj*, align 8
%argslist56624$ae508991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50901, %struct.ScmObj* %argslist56624$ae508990)
store volatile %struct.ScmObj* %argslist56624$ae508991, %struct.ScmObj** %stackaddr$prim57415, align 8
%stackaddr$prim57416 = alloca %struct.ScmObj*, align 8
%argslist56624$ae508992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50900, %struct.ScmObj* %argslist56624$ae508991)
store volatile %struct.ScmObj* %argslist56624$ae508992, %struct.ScmObj** %stackaddr$prim57416, align 8
%clofunc57417 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50899)
musttail call tailcc void %clofunc57417(%struct.ScmObj* %ae50899, %struct.ScmObj* %argslist56624$ae508992)
ret void
}

define tailcc void @proc_clo$ae50899(%struct.ScmObj* %env$ae50899,%struct.ScmObj* %current_45args56488) {
%stackaddr$env-ref57418 = alloca %struct.ScmObj*, align 8
%_37append47165 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50899, i64 0)
store %struct.ScmObj* %_37append47165, %struct.ScmObj** %stackaddr$env-ref57418
%stackaddr$prim57419 = alloca %struct.ScmObj*, align 8
%_95k47411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56488)
store volatile %struct.ScmObj* %_95k47411, %struct.ScmObj** %stackaddr$prim57419, align 8
%stackaddr$prim57420 = alloca %struct.ScmObj*, align 8
%current_45args56489 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56488)
store volatile %struct.ScmObj* %current_45args56489, %struct.ScmObj** %stackaddr$prim57420, align 8
%stackaddr$prim57421 = alloca %struct.ScmObj*, align 8
%_37third47132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56489)
store volatile %struct.ScmObj* %_37third47132, %struct.ScmObj** %stackaddr$prim57421, align 8
%stackaddr$makeclosure57422 = alloca %struct.ScmObj*, align 8
%fptrToInt57423 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50923 to i64
%ae50923 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57423)
store volatile %struct.ScmObj* %ae50923, %struct.ScmObj** %stackaddr$makeclosure57422, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50923, %struct.ScmObj* %_37append47165, i64 0)
%ae50924 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57424 = alloca %struct.ScmObj*, align 8
%fptrToInt57425 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50925 to i64
%ae50925 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57425)
store volatile %struct.ScmObj* %ae50925, %struct.ScmObj** %stackaddr$makeclosure57424, align 8
%argslist56619$ae509230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57426 = alloca %struct.ScmObj*, align 8
%argslist56619$ae509231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50925, %struct.ScmObj* %argslist56619$ae509230)
store volatile %struct.ScmObj* %argslist56619$ae509231, %struct.ScmObj** %stackaddr$prim57426, align 8
%stackaddr$prim57427 = alloca %struct.ScmObj*, align 8
%argslist56619$ae509232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50924, %struct.ScmObj* %argslist56619$ae509231)
store volatile %struct.ScmObj* %argslist56619$ae509232, %struct.ScmObj** %stackaddr$prim57427, align 8
%clofunc57428 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50923)
musttail call tailcc void %clofunc57428(%struct.ScmObj* %ae50923, %struct.ScmObj* %argslist56619$ae509232)
ret void
}

define tailcc void @proc_clo$ae50923(%struct.ScmObj* %env$ae50923,%struct.ScmObj* %current_45args56491) {
%stackaddr$env-ref57429 = alloca %struct.ScmObj*, align 8
%_37append47165 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50923, i64 0)
store %struct.ScmObj* %_37append47165, %struct.ScmObj** %stackaddr$env-ref57429
%stackaddr$prim57430 = alloca %struct.ScmObj*, align 8
%_95k47412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56491)
store volatile %struct.ScmObj* %_95k47412, %struct.ScmObj** %stackaddr$prim57430, align 8
%stackaddr$prim57431 = alloca %struct.ScmObj*, align 8
%current_45args56492 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56491)
store volatile %struct.ScmObj* %current_45args56492, %struct.ScmObj** %stackaddr$prim57431, align 8
%stackaddr$prim57432 = alloca %struct.ScmObj*, align 8
%_37fourth47130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56492)
store volatile %struct.ScmObj* %_37fourth47130, %struct.ScmObj** %stackaddr$prim57432, align 8
%stackaddr$prim57433 = alloca %struct.ScmObj*, align 8
%anf_45bind47326 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47326, %struct.ScmObj** %stackaddr$prim57433, align 8
%ae50949 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57434 = alloca %struct.ScmObj*, align 8
%nqueens47191 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50949, %struct.ScmObj* %anf_45bind47326)
store volatile %struct.ScmObj* %nqueens47191, %struct.ScmObj** %stackaddr$prim57434, align 8
%stackaddr$makeclosure57435 = alloca %struct.ScmObj*, align 8
%fptrToInt57436 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50951 to i64
%ae50951 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57436)
store volatile %struct.ScmObj* %ae50951, %struct.ScmObj** %stackaddr$makeclosure57435, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50951, %struct.ScmObj* %nqueens47191, i64 0)
%ae50952 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57437 = alloca %struct.ScmObj*, align 8
%fptrToInt57438 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50953 to i64
%ae50953 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57438)
store volatile %struct.ScmObj* %ae50953, %struct.ScmObj** %stackaddr$makeclosure57437, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50953, %struct.ScmObj* %_37append47165, i64 0)
%argslist56614$ae509510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57439 = alloca %struct.ScmObj*, align 8
%argslist56614$ae509511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50953, %struct.ScmObj* %argslist56614$ae509510)
store volatile %struct.ScmObj* %argslist56614$ae509511, %struct.ScmObj** %stackaddr$prim57439, align 8
%stackaddr$prim57440 = alloca %struct.ScmObj*, align 8
%argslist56614$ae509512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50952, %struct.ScmObj* %argslist56614$ae509511)
store volatile %struct.ScmObj* %argslist56614$ae509512, %struct.ScmObj** %stackaddr$prim57440, align 8
%clofunc57441 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50951)
musttail call tailcc void %clofunc57441(%struct.ScmObj* %ae50951, %struct.ScmObj* %argslist56614$ae509512)
ret void
}

define tailcc void @proc_clo$ae50951(%struct.ScmObj* %env$ae50951,%struct.ScmObj* %current_45args56494) {
%stackaddr$env-ref57442 = alloca %struct.ScmObj*, align 8
%nqueens47191 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50951, i64 0)
store %struct.ScmObj* %nqueens47191, %struct.ScmObj** %stackaddr$env-ref57442
%stackaddr$prim57443 = alloca %struct.ScmObj*, align 8
%_95k47413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56494)
store volatile %struct.ScmObj* %_95k47413, %struct.ScmObj** %stackaddr$prim57443, align 8
%stackaddr$prim57444 = alloca %struct.ScmObj*, align 8
%current_45args56495 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56494)
store volatile %struct.ScmObj* %current_45args56495, %struct.ScmObj** %stackaddr$prim57444, align 8
%stackaddr$prim57445 = alloca %struct.ScmObj*, align 8
%anf_45bind47379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56495)
store volatile %struct.ScmObj* %anf_45bind47379, %struct.ScmObj** %stackaddr$prim57445, align 8
%ae53160 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57446 = alloca %struct.ScmObj*, align 8
%t4706847192 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %nqueens47191, %struct.ScmObj* %ae53160, %struct.ScmObj* %anf_45bind47379)
store volatile %struct.ScmObj* %t4706847192, %struct.ScmObj** %stackaddr$prim57446, align 8
%ae53163 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57447 = alloca %struct.ScmObj*, align 8
%anf_45bind47380 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %nqueens47191, %struct.ScmObj* %ae53163)
store volatile %struct.ScmObj* %anf_45bind47380, %struct.ScmObj** %stackaddr$prim57447, align 8
%stackaddr$makeclosure57448 = alloca %struct.ScmObj*, align 8
%fptrToInt57449 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53165 to i64
%ae53165 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57449)
store volatile %struct.ScmObj* %ae53165, %struct.ScmObj** %stackaddr$makeclosure57448, align 8
%ae53166 = call %struct.ScmObj* @const_init_int(i64 7)
%argslist56501$anf_45bind473800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57450 = alloca %struct.ScmObj*, align 8
%argslist56501$anf_45bind473801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53166, %struct.ScmObj* %argslist56501$anf_45bind473800)
store volatile %struct.ScmObj* %argslist56501$anf_45bind473801, %struct.ScmObj** %stackaddr$prim57450, align 8
%stackaddr$prim57451 = alloca %struct.ScmObj*, align 8
%argslist56501$anf_45bind473802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53165, %struct.ScmObj* %argslist56501$anf_45bind473801)
store volatile %struct.ScmObj* %argslist56501$anf_45bind473802, %struct.ScmObj** %stackaddr$prim57451, align 8
%clofunc57452 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47380)
musttail call tailcc void %clofunc57452(%struct.ScmObj* %anf_45bind47380, %struct.ScmObj* %argslist56501$anf_45bind473802)
ret void
}

define tailcc void @proc_clo$ae53165(%struct.ScmObj* %env$ae53165,%struct.ScmObj* %current_45args56497) {
%stackaddr$prim57453 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56497)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim57453, align 8
%stackaddr$prim57454 = alloca %struct.ScmObj*, align 8
%current_45args56498 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56497)
store volatile %struct.ScmObj* %current_45args56498, %struct.ScmObj** %stackaddr$prim57454, align 8
%stackaddr$prim57455 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56498)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim57455, align 8
%stackaddr$prim57456 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim57456, align 8
%argslist56500$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57457 = alloca %struct.ScmObj*, align 8
%argslist56500$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist56500$k0)
store volatile %struct.ScmObj* %argslist56500$k1, %struct.ScmObj** %stackaddr$prim57457, align 8
%clofunc57458 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc57458(%struct.ScmObj* %k, %struct.ScmObj* %argslist56500$k1)
ret void
}

define tailcc void @proc_clo$ae50953(%struct.ScmObj* %env$ae50953,%struct.ScmObj* %current_45args56502) {
%stackaddr$env-ref57459 = alloca %struct.ScmObj*, align 8
%_37append47165 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50953, i64 0)
store %struct.ScmObj* %_37append47165, %struct.ScmObj** %stackaddr$env-ref57459
%stackaddr$prim57460 = alloca %struct.ScmObj*, align 8
%k47414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56502)
store volatile %struct.ScmObj* %k47414, %struct.ScmObj** %stackaddr$prim57460, align 8
%stackaddr$prim57461 = alloca %struct.ScmObj*, align 8
%current_45args56503 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56502)
store volatile %struct.ScmObj* %current_45args56503, %struct.ScmObj** %stackaddr$prim57461, align 8
%stackaddr$prim57462 = alloca %struct.ScmObj*, align 8
%n47193 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56503)
store volatile %struct.ScmObj* %n47193, %struct.ScmObj** %stackaddr$prim57462, align 8
%stackaddr$prim57463 = alloca %struct.ScmObj*, align 8
%anf_45bind47327 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47327, %struct.ScmObj** %stackaddr$prim57463, align 8
%ae50954 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57464 = alloca %struct.ScmObj*, align 8
%one_45to47196 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50954, %struct.ScmObj* %anf_45bind47327)
store volatile %struct.ScmObj* %one_45to47196, %struct.ScmObj** %stackaddr$prim57464, align 8
%stackaddr$prim57465 = alloca %struct.ScmObj*, align 8
%anf_45bind47328 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47328, %struct.ScmObj** %stackaddr$prim57465, align 8
%ae50956 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57466 = alloca %struct.ScmObj*, align 8
%my_45try47195 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50956, %struct.ScmObj* %anf_45bind47328)
store volatile %struct.ScmObj* %my_45try47195, %struct.ScmObj** %stackaddr$prim57466, align 8
%stackaddr$prim57467 = alloca %struct.ScmObj*, align 8
%anf_45bind47329 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47329, %struct.ScmObj** %stackaddr$prim57467, align 8
%ae50958 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57468 = alloca %struct.ScmObj*, align 8
%ok_6347194 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50958, %struct.ScmObj* %anf_45bind47329)
store volatile %struct.ScmObj* %ok_6347194, %struct.ScmObj** %stackaddr$prim57468, align 8
%stackaddr$makeclosure57469 = alloca %struct.ScmObj*, align 8
%fptrToInt57470 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50960 to i64
%ae50960 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt57470)
store volatile %struct.ScmObj* %ae50960, %struct.ScmObj** %stackaddr$makeclosure57469, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50960, %struct.ScmObj* %k47414, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50960, %struct.ScmObj* %_37append47165, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50960, %struct.ScmObj* %one_45to47196, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50960, %struct.ScmObj* %my_45try47195, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae50960, %struct.ScmObj* %ok_6347194, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae50960, %struct.ScmObj* %n47193, i64 5)
%ae50961 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57471 = alloca %struct.ScmObj*, align 8
%fptrToInt57472 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50962 to i64
%ae50962 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57472)
store volatile %struct.ScmObj* %ae50962, %struct.ScmObj** %stackaddr$makeclosure57471, align 8
%argslist56613$ae509600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57473 = alloca %struct.ScmObj*, align 8
%argslist56613$ae509601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50962, %struct.ScmObj* %argslist56613$ae509600)
store volatile %struct.ScmObj* %argslist56613$ae509601, %struct.ScmObj** %stackaddr$prim57473, align 8
%stackaddr$prim57474 = alloca %struct.ScmObj*, align 8
%argslist56613$ae509602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50961, %struct.ScmObj* %argslist56613$ae509601)
store volatile %struct.ScmObj* %argslist56613$ae509602, %struct.ScmObj** %stackaddr$prim57474, align 8
%clofunc57475 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50960)
musttail call tailcc void %clofunc57475(%struct.ScmObj* %ae50960, %struct.ScmObj* %argslist56613$ae509602)
ret void
}

define tailcc void @proc_clo$ae50960(%struct.ScmObj* %env$ae50960,%struct.ScmObj* %current_45args56505) {
%stackaddr$env-ref57476 = alloca %struct.ScmObj*, align 8
%k47414 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50960, i64 0)
store %struct.ScmObj* %k47414, %struct.ScmObj** %stackaddr$env-ref57476
%stackaddr$env-ref57477 = alloca %struct.ScmObj*, align 8
%_37append47165 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50960, i64 1)
store %struct.ScmObj* %_37append47165, %struct.ScmObj** %stackaddr$env-ref57477
%stackaddr$env-ref57478 = alloca %struct.ScmObj*, align 8
%one_45to47196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50960, i64 2)
store %struct.ScmObj* %one_45to47196, %struct.ScmObj** %stackaddr$env-ref57478
%stackaddr$env-ref57479 = alloca %struct.ScmObj*, align 8
%my_45try47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50960, i64 3)
store %struct.ScmObj* %my_45try47195, %struct.ScmObj** %stackaddr$env-ref57479
%stackaddr$env-ref57480 = alloca %struct.ScmObj*, align 8
%ok_6347194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50960, i64 4)
store %struct.ScmObj* %ok_6347194, %struct.ScmObj** %stackaddr$env-ref57480
%stackaddr$env-ref57481 = alloca %struct.ScmObj*, align 8
%n47193 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50960, i64 5)
store %struct.ScmObj* %n47193, %struct.ScmObj** %stackaddr$env-ref57481
%stackaddr$prim57482 = alloca %struct.ScmObj*, align 8
%_95k47415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56505)
store volatile %struct.ScmObj* %_95k47415, %struct.ScmObj** %stackaddr$prim57482, align 8
%stackaddr$prim57483 = alloca %struct.ScmObj*, align 8
%current_45args56506 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56505)
store volatile %struct.ScmObj* %current_45args56506, %struct.ScmObj** %stackaddr$prim57483, align 8
%stackaddr$prim57484 = alloca %struct.ScmObj*, align 8
%anf_45bind47339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56506)
store volatile %struct.ScmObj* %anf_45bind47339, %struct.ScmObj** %stackaddr$prim57484, align 8
%ae51227 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57485 = alloca %struct.ScmObj*, align 8
%t4707147206 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %one_45to47196, %struct.ScmObj* %ae51227, %struct.ScmObj* %anf_45bind47339)
store volatile %struct.ScmObj* %t4707147206, %struct.ScmObj** %stackaddr$prim57485, align 8
%stackaddr$makeclosure57486 = alloca %struct.ScmObj*, align 8
%fptrToInt57487 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51229 to i64
%ae51229 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57487)
store volatile %struct.ScmObj* %ae51229, %struct.ScmObj** %stackaddr$makeclosure57486, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51229, %struct.ScmObj* %k47414, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51229, %struct.ScmObj* %one_45to47196, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51229, %struct.ScmObj* %my_45try47195, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51229, %struct.ScmObj* %ok_6347194, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51229, %struct.ScmObj* %n47193, i64 4)
%ae51230 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57488 = alloca %struct.ScmObj*, align 8
%fptrToInt57489 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51231 to i64
%ae51231 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57489)
store volatile %struct.ScmObj* %ae51231, %struct.ScmObj** %stackaddr$makeclosure57488, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51231, %struct.ScmObj* %_37append47165, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51231, %struct.ScmObj* %my_45try47195, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51231, %struct.ScmObj* %ok_6347194, i64 2)
%argslist56589$ae512290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57490 = alloca %struct.ScmObj*, align 8
%argslist56589$ae512291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51231, %struct.ScmObj* %argslist56589$ae512290)
store volatile %struct.ScmObj* %argslist56589$ae512291, %struct.ScmObj** %stackaddr$prim57490, align 8
%stackaddr$prim57491 = alloca %struct.ScmObj*, align 8
%argslist56589$ae512292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51230, %struct.ScmObj* %argslist56589$ae512291)
store volatile %struct.ScmObj* %argslist56589$ae512292, %struct.ScmObj** %stackaddr$prim57491, align 8
%clofunc57492 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51229)
musttail call tailcc void %clofunc57492(%struct.ScmObj* %ae51229, %struct.ScmObj* %argslist56589$ae512292)
ret void
}

define tailcc void @proc_clo$ae51229(%struct.ScmObj* %env$ae51229,%struct.ScmObj* %current_45args56508) {
%stackaddr$env-ref57493 = alloca %struct.ScmObj*, align 8
%k47414 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51229, i64 0)
store %struct.ScmObj* %k47414, %struct.ScmObj** %stackaddr$env-ref57493
%stackaddr$env-ref57494 = alloca %struct.ScmObj*, align 8
%one_45to47196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51229, i64 1)
store %struct.ScmObj* %one_45to47196, %struct.ScmObj** %stackaddr$env-ref57494
%stackaddr$env-ref57495 = alloca %struct.ScmObj*, align 8
%my_45try47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51229, i64 2)
store %struct.ScmObj* %my_45try47195, %struct.ScmObj** %stackaddr$env-ref57495
%stackaddr$env-ref57496 = alloca %struct.ScmObj*, align 8
%ok_6347194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51229, i64 3)
store %struct.ScmObj* %ok_6347194, %struct.ScmObj** %stackaddr$env-ref57496
%stackaddr$env-ref57497 = alloca %struct.ScmObj*, align 8
%n47193 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51229, i64 4)
store %struct.ScmObj* %n47193, %struct.ScmObj** %stackaddr$env-ref57497
%stackaddr$prim57498 = alloca %struct.ScmObj*, align 8
%_95k47416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56508)
store volatile %struct.ScmObj* %_95k47416, %struct.ScmObj** %stackaddr$prim57498, align 8
%stackaddr$prim57499 = alloca %struct.ScmObj*, align 8
%current_45args56509 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56508)
store volatile %struct.ScmObj* %current_45args56509, %struct.ScmObj** %stackaddr$prim57499, align 8
%stackaddr$prim57500 = alloca %struct.ScmObj*, align 8
%anf_45bind47358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56509)
store volatile %struct.ScmObj* %anf_45bind47358, %struct.ScmObj** %stackaddr$prim57500, align 8
%ae51619 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57501 = alloca %struct.ScmObj*, align 8
%t4707047201 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %my_45try47195, %struct.ScmObj* %ae51619, %struct.ScmObj* %anf_45bind47358)
store volatile %struct.ScmObj* %t4707047201, %struct.ScmObj** %stackaddr$prim57501, align 8
%stackaddr$makeclosure57502 = alloca %struct.ScmObj*, align 8
%fptrToInt57503 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51621 to i64
%ae51621 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57503)
store volatile %struct.ScmObj* %ae51621, %struct.ScmObj** %stackaddr$makeclosure57502, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51621, %struct.ScmObj* %k47414, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51621, %struct.ScmObj* %one_45to47196, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51621, %struct.ScmObj* %my_45try47195, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51621, %struct.ScmObj* %ok_6347194, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51621, %struct.ScmObj* %n47193, i64 4)
%ae51622 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57504 = alloca %struct.ScmObj*, align 8
%fptrToInt57505 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51623 to i64
%ae51623 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57505)
store volatile %struct.ScmObj* %ae51623, %struct.ScmObj** %stackaddr$makeclosure57504, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51623, %struct.ScmObj* %ok_6347194, i64 0)
%argslist56546$ae516210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57506 = alloca %struct.ScmObj*, align 8
%argslist56546$ae516211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51623, %struct.ScmObj* %argslist56546$ae516210)
store volatile %struct.ScmObj* %argslist56546$ae516211, %struct.ScmObj** %stackaddr$prim57506, align 8
%stackaddr$prim57507 = alloca %struct.ScmObj*, align 8
%argslist56546$ae516212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51622, %struct.ScmObj* %argslist56546$ae516211)
store volatile %struct.ScmObj* %argslist56546$ae516212, %struct.ScmObj** %stackaddr$prim57507, align 8
%clofunc57508 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51621)
musttail call tailcc void %clofunc57508(%struct.ScmObj* %ae51621, %struct.ScmObj* %argslist56546$ae516212)
ret void
}

define tailcc void @proc_clo$ae51621(%struct.ScmObj* %env$ae51621,%struct.ScmObj* %current_45args56511) {
%stackaddr$env-ref57509 = alloca %struct.ScmObj*, align 8
%k47414 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51621, i64 0)
store %struct.ScmObj* %k47414, %struct.ScmObj** %stackaddr$env-ref57509
%stackaddr$env-ref57510 = alloca %struct.ScmObj*, align 8
%one_45to47196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51621, i64 1)
store %struct.ScmObj* %one_45to47196, %struct.ScmObj** %stackaddr$env-ref57510
%stackaddr$env-ref57511 = alloca %struct.ScmObj*, align 8
%my_45try47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51621, i64 2)
store %struct.ScmObj* %my_45try47195, %struct.ScmObj** %stackaddr$env-ref57511
%stackaddr$env-ref57512 = alloca %struct.ScmObj*, align 8
%ok_6347194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51621, i64 3)
store %struct.ScmObj* %ok_6347194, %struct.ScmObj** %stackaddr$env-ref57512
%stackaddr$env-ref57513 = alloca %struct.ScmObj*, align 8
%n47193 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51621, i64 4)
store %struct.ScmObj* %n47193, %struct.ScmObj** %stackaddr$env-ref57513
%stackaddr$prim57514 = alloca %struct.ScmObj*, align 8
%_95k47417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56511)
store volatile %struct.ScmObj* %_95k47417, %struct.ScmObj** %stackaddr$prim57514, align 8
%stackaddr$prim57515 = alloca %struct.ScmObj*, align 8
%current_45args56512 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56511)
store volatile %struct.ScmObj* %current_45args56512, %struct.ScmObj** %stackaddr$prim57515, align 8
%stackaddr$prim57516 = alloca %struct.ScmObj*, align 8
%anf_45bind47371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56512)
store volatile %struct.ScmObj* %anf_45bind47371, %struct.ScmObj** %stackaddr$prim57516, align 8
%ae51737 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57517 = alloca %struct.ScmObj*, align 8
%t4706947197 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %ok_6347194, %struct.ScmObj* %ae51737, %struct.ScmObj* %anf_45bind47371)
store volatile %struct.ScmObj* %t4706947197, %struct.ScmObj** %stackaddr$prim57517, align 8
%ae51740 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57518 = alloca %struct.ScmObj*, align 8
%anf_45bind47372 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %my_45try47195, %struct.ScmObj* %ae51740)
store volatile %struct.ScmObj* %anf_45bind47372, %struct.ScmObj** %stackaddr$prim57518, align 8
%ae51742 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57519 = alloca %struct.ScmObj*, align 8
%anf_45bind47373 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %one_45to47196, %struct.ScmObj* %ae51742)
store volatile %struct.ScmObj* %anf_45bind47373, %struct.ScmObj** %stackaddr$prim57519, align 8
%stackaddr$makeclosure57520 = alloca %struct.ScmObj*, align 8
%fptrToInt57521 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51744 to i64
%ae51744 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57521)
store volatile %struct.ScmObj* %ae51744, %struct.ScmObj** %stackaddr$makeclosure57520, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51744, %struct.ScmObj* %k47414, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51744, %struct.ScmObj* %anf_45bind47372, i64 1)
%argslist56536$anf_45bind473730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57522 = alloca %struct.ScmObj*, align 8
%argslist56536$anf_45bind473731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %n47193, %struct.ScmObj* %argslist56536$anf_45bind473730)
store volatile %struct.ScmObj* %argslist56536$anf_45bind473731, %struct.ScmObj** %stackaddr$prim57522, align 8
%stackaddr$prim57523 = alloca %struct.ScmObj*, align 8
%argslist56536$anf_45bind473732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51744, %struct.ScmObj* %argslist56536$anf_45bind473731)
store volatile %struct.ScmObj* %argslist56536$anf_45bind473732, %struct.ScmObj** %stackaddr$prim57523, align 8
%clofunc57524 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47373)
musttail call tailcc void %clofunc57524(%struct.ScmObj* %anf_45bind47373, %struct.ScmObj* %argslist56536$anf_45bind473732)
ret void
}

define tailcc void @proc_clo$ae51744(%struct.ScmObj* %env$ae51744,%struct.ScmObj* %current_45args56514) {
%stackaddr$env-ref57525 = alloca %struct.ScmObj*, align 8
%k47414 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51744, i64 0)
store %struct.ScmObj* %k47414, %struct.ScmObj** %stackaddr$env-ref57525
%stackaddr$env-ref57526 = alloca %struct.ScmObj*, align 8
%anf_45bind47372 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51744, i64 1)
store %struct.ScmObj* %anf_45bind47372, %struct.ScmObj** %stackaddr$env-ref57526
%stackaddr$prim57527 = alloca %struct.ScmObj*, align 8
%_95k47418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56514)
store volatile %struct.ScmObj* %_95k47418, %struct.ScmObj** %stackaddr$prim57527, align 8
%stackaddr$prim57528 = alloca %struct.ScmObj*, align 8
%current_45args56515 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56514)
store volatile %struct.ScmObj* %current_45args56515, %struct.ScmObj** %stackaddr$prim57528, align 8
%stackaddr$prim57529 = alloca %struct.ScmObj*, align 8
%anf_45bind47374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56515)
store volatile %struct.ScmObj* %anf_45bind47374, %struct.ScmObj** %stackaddr$prim57529, align 8
%stackaddr$makeclosure57530 = alloca %struct.ScmObj*, align 8
%fptrToInt57531 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51746 to i64
%ae51746 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57531)
store volatile %struct.ScmObj* %ae51746, %struct.ScmObj** %stackaddr$makeclosure57530, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51746, %struct.ScmObj* %k47414, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51746, %struct.ScmObj* %anf_45bind47374, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51746, %struct.ScmObj* %anf_45bind47372, i64 2)
%ae51747 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57532 = alloca %struct.ScmObj*, align 8
%fptrToInt57533 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51748 to i64
%ae51748 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57533)
store volatile %struct.ScmObj* %ae51748, %struct.ScmObj** %stackaddr$makeclosure57532, align 8
%argslist56535$ae517460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57534 = alloca %struct.ScmObj*, align 8
%argslist56535$ae517461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51748, %struct.ScmObj* %argslist56535$ae517460)
store volatile %struct.ScmObj* %argslist56535$ae517461, %struct.ScmObj** %stackaddr$prim57534, align 8
%stackaddr$prim57535 = alloca %struct.ScmObj*, align 8
%argslist56535$ae517462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51747, %struct.ScmObj* %argslist56535$ae517461)
store volatile %struct.ScmObj* %argslist56535$ae517462, %struct.ScmObj** %stackaddr$prim57535, align 8
%clofunc57536 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51746)
musttail call tailcc void %clofunc57536(%struct.ScmObj* %ae51746, %struct.ScmObj* %argslist56535$ae517462)
ret void
}

define tailcc void @proc_clo$ae51746(%struct.ScmObj* %env$ae51746,%struct.ScmObj* %current_45args56517) {
%stackaddr$env-ref57537 = alloca %struct.ScmObj*, align 8
%k47414 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51746, i64 0)
store %struct.ScmObj* %k47414, %struct.ScmObj** %stackaddr$env-ref57537
%stackaddr$env-ref57538 = alloca %struct.ScmObj*, align 8
%anf_45bind47374 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51746, i64 1)
store %struct.ScmObj* %anf_45bind47374, %struct.ScmObj** %stackaddr$env-ref57538
%stackaddr$env-ref57539 = alloca %struct.ScmObj*, align 8
%anf_45bind47372 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51746, i64 2)
store %struct.ScmObj* %anf_45bind47372, %struct.ScmObj** %stackaddr$env-ref57539
%stackaddr$prim57540 = alloca %struct.ScmObj*, align 8
%_95k47419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56517)
store volatile %struct.ScmObj* %_95k47419, %struct.ScmObj** %stackaddr$prim57540, align 8
%stackaddr$prim57541 = alloca %struct.ScmObj*, align 8
%current_45args56518 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56517)
store volatile %struct.ScmObj* %current_45args56518, %struct.ScmObj** %stackaddr$prim57541, align 8
%stackaddr$prim57542 = alloca %struct.ScmObj*, align 8
%anf_45bind47375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56518)
store volatile %struct.ScmObj* %anf_45bind47375, %struct.ScmObj** %stackaddr$prim57542, align 8
%stackaddr$makeclosure57543 = alloca %struct.ScmObj*, align 8
%fptrToInt57544 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51769 to i64
%ae51769 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57544)
store volatile %struct.ScmObj* %ae51769, %struct.ScmObj** %stackaddr$makeclosure57543, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51769, %struct.ScmObj* %k47414, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51769, %struct.ScmObj* %anf_45bind47374, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51769, %struct.ScmObj* %anf_45bind47372, i64 2)
%argslist56533$anf_45bind473750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57545 = alloca %struct.ScmObj*, align 8
%argslist56533$anf_45bind473751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51769, %struct.ScmObj* %argslist56533$anf_45bind473750)
store volatile %struct.ScmObj* %argslist56533$anf_45bind473751, %struct.ScmObj** %stackaddr$prim57545, align 8
%clofunc57546 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47375)
musttail call tailcc void %clofunc57546(%struct.ScmObj* %anf_45bind47375, %struct.ScmObj* %argslist56533$anf_45bind473751)
ret void
}

define tailcc void @proc_clo$ae51769(%struct.ScmObj* %env$ae51769,%struct.ScmObj* %current_45args56520) {
%stackaddr$env-ref57547 = alloca %struct.ScmObj*, align 8
%k47414 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51769, i64 0)
store %struct.ScmObj* %k47414, %struct.ScmObj** %stackaddr$env-ref57547
%stackaddr$env-ref57548 = alloca %struct.ScmObj*, align 8
%anf_45bind47374 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51769, i64 1)
store %struct.ScmObj* %anf_45bind47374, %struct.ScmObj** %stackaddr$env-ref57548
%stackaddr$env-ref57549 = alloca %struct.ScmObj*, align 8
%anf_45bind47372 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51769, i64 2)
store %struct.ScmObj* %anf_45bind47372, %struct.ScmObj** %stackaddr$env-ref57549
%stackaddr$prim57550 = alloca %struct.ScmObj*, align 8
%_95k47420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56520)
store volatile %struct.ScmObj* %_95k47420, %struct.ScmObj** %stackaddr$prim57550, align 8
%stackaddr$prim57551 = alloca %struct.ScmObj*, align 8
%current_45args56521 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56520)
store volatile %struct.ScmObj* %current_45args56521, %struct.ScmObj** %stackaddr$prim57551, align 8
%stackaddr$prim57552 = alloca %struct.ScmObj*, align 8
%anf_45bind47376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56521)
store volatile %struct.ScmObj* %anf_45bind47376, %struct.ScmObj** %stackaddr$prim57552, align 8
%stackaddr$makeclosure57553 = alloca %struct.ScmObj*, align 8
%fptrToInt57554 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51770 to i64
%ae51770 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57554)
store volatile %struct.ScmObj* %ae51770, %struct.ScmObj** %stackaddr$makeclosure57553, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51770, %struct.ScmObj* %k47414, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51770, %struct.ScmObj* %anf_45bind47376, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51770, %struct.ScmObj* %anf_45bind47374, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51770, %struct.ScmObj* %anf_45bind47372, i64 3)
%ae51771 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57555 = alloca %struct.ScmObj*, align 8
%fptrToInt57556 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51772 to i64
%ae51772 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57556)
store volatile %struct.ScmObj* %ae51772, %struct.ScmObj** %stackaddr$makeclosure57555, align 8
%argslist56532$ae517700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57557 = alloca %struct.ScmObj*, align 8
%argslist56532$ae517701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51772, %struct.ScmObj* %argslist56532$ae517700)
store volatile %struct.ScmObj* %argslist56532$ae517701, %struct.ScmObj** %stackaddr$prim57557, align 8
%stackaddr$prim57558 = alloca %struct.ScmObj*, align 8
%argslist56532$ae517702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51771, %struct.ScmObj* %argslist56532$ae517701)
store volatile %struct.ScmObj* %argslist56532$ae517702, %struct.ScmObj** %stackaddr$prim57558, align 8
%clofunc57559 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51770)
musttail call tailcc void %clofunc57559(%struct.ScmObj* %ae51770, %struct.ScmObj* %argslist56532$ae517702)
ret void
}

define tailcc void @proc_clo$ae51770(%struct.ScmObj* %env$ae51770,%struct.ScmObj* %current_45args56523) {
%stackaddr$env-ref57560 = alloca %struct.ScmObj*, align 8
%k47414 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51770, i64 0)
store %struct.ScmObj* %k47414, %struct.ScmObj** %stackaddr$env-ref57560
%stackaddr$env-ref57561 = alloca %struct.ScmObj*, align 8
%anf_45bind47376 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51770, i64 1)
store %struct.ScmObj* %anf_45bind47376, %struct.ScmObj** %stackaddr$env-ref57561
%stackaddr$env-ref57562 = alloca %struct.ScmObj*, align 8
%anf_45bind47374 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51770, i64 2)
store %struct.ScmObj* %anf_45bind47374, %struct.ScmObj** %stackaddr$env-ref57562
%stackaddr$env-ref57563 = alloca %struct.ScmObj*, align 8
%anf_45bind47372 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51770, i64 3)
store %struct.ScmObj* %anf_45bind47372, %struct.ScmObj** %stackaddr$env-ref57563
%stackaddr$prim57564 = alloca %struct.ScmObj*, align 8
%_95k47421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56523)
store volatile %struct.ScmObj* %_95k47421, %struct.ScmObj** %stackaddr$prim57564, align 8
%stackaddr$prim57565 = alloca %struct.ScmObj*, align 8
%current_45args56524 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56523)
store volatile %struct.ScmObj* %current_45args56524, %struct.ScmObj** %stackaddr$prim57565, align 8
%stackaddr$prim57566 = alloca %struct.ScmObj*, align 8
%anf_45bind47377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56524)
store volatile %struct.ScmObj* %anf_45bind47377, %struct.ScmObj** %stackaddr$prim57566, align 8
%stackaddr$makeclosure57567 = alloca %struct.ScmObj*, align 8
%fptrToInt57568 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51793 to i64
%ae51793 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57568)
store volatile %struct.ScmObj* %ae51793, %struct.ScmObj** %stackaddr$makeclosure57567, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51793, %struct.ScmObj* %k47414, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51793, %struct.ScmObj* %anf_45bind47376, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51793, %struct.ScmObj* %anf_45bind47374, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51793, %struct.ScmObj* %anf_45bind47372, i64 3)
%argslist56530$anf_45bind473770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57569 = alloca %struct.ScmObj*, align 8
%argslist56530$anf_45bind473771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51793, %struct.ScmObj* %argslist56530$anf_45bind473770)
store volatile %struct.ScmObj* %argslist56530$anf_45bind473771, %struct.ScmObj** %stackaddr$prim57569, align 8
%clofunc57570 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47377)
musttail call tailcc void %clofunc57570(%struct.ScmObj* %anf_45bind47377, %struct.ScmObj* %argslist56530$anf_45bind473771)
ret void
}

define tailcc void @proc_clo$ae51793(%struct.ScmObj* %env$ae51793,%struct.ScmObj* %current_45args56526) {
%stackaddr$env-ref57571 = alloca %struct.ScmObj*, align 8
%k47414 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51793, i64 0)
store %struct.ScmObj* %k47414, %struct.ScmObj** %stackaddr$env-ref57571
%stackaddr$env-ref57572 = alloca %struct.ScmObj*, align 8
%anf_45bind47376 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51793, i64 1)
store %struct.ScmObj* %anf_45bind47376, %struct.ScmObj** %stackaddr$env-ref57572
%stackaddr$env-ref57573 = alloca %struct.ScmObj*, align 8
%anf_45bind47374 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51793, i64 2)
store %struct.ScmObj* %anf_45bind47374, %struct.ScmObj** %stackaddr$env-ref57573
%stackaddr$env-ref57574 = alloca %struct.ScmObj*, align 8
%anf_45bind47372 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51793, i64 3)
store %struct.ScmObj* %anf_45bind47372, %struct.ScmObj** %stackaddr$env-ref57574
%stackaddr$prim57575 = alloca %struct.ScmObj*, align 8
%_95k47422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56526)
store volatile %struct.ScmObj* %_95k47422, %struct.ScmObj** %stackaddr$prim57575, align 8
%stackaddr$prim57576 = alloca %struct.ScmObj*, align 8
%current_45args56527 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56526)
store volatile %struct.ScmObj* %current_45args56527, %struct.ScmObj** %stackaddr$prim57576, align 8
%stackaddr$prim57577 = alloca %struct.ScmObj*, align 8
%anf_45bind47378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56527)
store volatile %struct.ScmObj* %anf_45bind47378, %struct.ScmObj** %stackaddr$prim57577, align 8
%argslist56529$anf_45bind473720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57578 = alloca %struct.ScmObj*, align 8
%argslist56529$anf_45bind473721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47378, %struct.ScmObj* %argslist56529$anf_45bind473720)
store volatile %struct.ScmObj* %argslist56529$anf_45bind473721, %struct.ScmObj** %stackaddr$prim57578, align 8
%stackaddr$prim57579 = alloca %struct.ScmObj*, align 8
%argslist56529$anf_45bind473722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47376, %struct.ScmObj* %argslist56529$anf_45bind473721)
store volatile %struct.ScmObj* %argslist56529$anf_45bind473722, %struct.ScmObj** %stackaddr$prim57579, align 8
%stackaddr$prim57580 = alloca %struct.ScmObj*, align 8
%argslist56529$anf_45bind473723 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47374, %struct.ScmObj* %argslist56529$anf_45bind473722)
store volatile %struct.ScmObj* %argslist56529$anf_45bind473723, %struct.ScmObj** %stackaddr$prim57580, align 8
%stackaddr$prim57581 = alloca %struct.ScmObj*, align 8
%argslist56529$anf_45bind473724 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47414, %struct.ScmObj* %argslist56529$anf_45bind473723)
store volatile %struct.ScmObj* %argslist56529$anf_45bind473724, %struct.ScmObj** %stackaddr$prim57581, align 8
%clofunc57582 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47372)
musttail call tailcc void %clofunc57582(%struct.ScmObj* %anf_45bind47372, %struct.ScmObj* %argslist56529$anf_45bind473724)
ret void
}

define tailcc void @proc_clo$ae51772(%struct.ScmObj* %env$ae51772,%struct.ScmObj* %lst4721447423) {
%stackaddr$prim57583 = alloca %struct.ScmObj*, align 8
%k47424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4721447423)
store volatile %struct.ScmObj* %k47424, %struct.ScmObj** %stackaddr$prim57583, align 8
%stackaddr$prim57584 = alloca %struct.ScmObj*, align 8
%lst47214 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4721447423)
store volatile %struct.ScmObj* %lst47214, %struct.ScmObj** %stackaddr$prim57584, align 8
%ae51776 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56531$k474240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57585 = alloca %struct.ScmObj*, align 8
%argslist56531$k474241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47214, %struct.ScmObj* %argslist56531$k474240)
store volatile %struct.ScmObj* %argslist56531$k474241, %struct.ScmObj** %stackaddr$prim57585, align 8
%stackaddr$prim57586 = alloca %struct.ScmObj*, align 8
%argslist56531$k474242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51776, %struct.ScmObj* %argslist56531$k474241)
store volatile %struct.ScmObj* %argslist56531$k474242, %struct.ScmObj** %stackaddr$prim57586, align 8
%clofunc57587 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47424)
musttail call tailcc void %clofunc57587(%struct.ScmObj* %k47424, %struct.ScmObj* %argslist56531$k474242)
ret void
}

define tailcc void @proc_clo$ae51748(%struct.ScmObj* %env$ae51748,%struct.ScmObj* %lst4721347425) {
%stackaddr$prim57588 = alloca %struct.ScmObj*, align 8
%k47426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4721347425)
store volatile %struct.ScmObj* %k47426, %struct.ScmObj** %stackaddr$prim57588, align 8
%stackaddr$prim57589 = alloca %struct.ScmObj*, align 8
%lst47213 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4721347425)
store volatile %struct.ScmObj* %lst47213, %struct.ScmObj** %stackaddr$prim57589, align 8
%ae51752 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56534$k474260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57590 = alloca %struct.ScmObj*, align 8
%argslist56534$k474261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47213, %struct.ScmObj* %argslist56534$k474260)
store volatile %struct.ScmObj* %argslist56534$k474261, %struct.ScmObj** %stackaddr$prim57590, align 8
%stackaddr$prim57591 = alloca %struct.ScmObj*, align 8
%argslist56534$k474262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51752, %struct.ScmObj* %argslist56534$k474261)
store volatile %struct.ScmObj* %argslist56534$k474262, %struct.ScmObj** %stackaddr$prim57591, align 8
%clofunc57592 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47426)
musttail call tailcc void %clofunc57592(%struct.ScmObj* %k47426, %struct.ScmObj* %argslist56534$k474262)
ret void
}

define tailcc void @proc_clo$ae51623(%struct.ScmObj* %env$ae51623,%struct.ScmObj* %current_45args56537) {
%stackaddr$env-ref57593 = alloca %struct.ScmObj*, align 8
%ok_6347194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51623, i64 0)
store %struct.ScmObj* %ok_6347194, %struct.ScmObj** %stackaddr$env-ref57593
%stackaddr$prim57594 = alloca %struct.ScmObj*, align 8
%k47427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56537)
store volatile %struct.ScmObj* %k47427, %struct.ScmObj** %stackaddr$prim57594, align 8
%stackaddr$prim57595 = alloca %struct.ScmObj*, align 8
%current_45args56538 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56537)
store volatile %struct.ScmObj* %current_45args56538, %struct.ScmObj** %stackaddr$prim57595, align 8
%stackaddr$prim57596 = alloca %struct.ScmObj*, align 8
%row47200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56538)
store volatile %struct.ScmObj* %row47200, %struct.ScmObj** %stackaddr$prim57596, align 8
%stackaddr$prim57597 = alloca %struct.ScmObj*, align 8
%current_45args56539 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56538)
store volatile %struct.ScmObj* %current_45args56539, %struct.ScmObj** %stackaddr$prim57597, align 8
%stackaddr$prim57598 = alloca %struct.ScmObj*, align 8
%dist47199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56539)
store volatile %struct.ScmObj* %dist47199, %struct.ScmObj** %stackaddr$prim57598, align 8
%stackaddr$prim57599 = alloca %struct.ScmObj*, align 8
%current_45args56540 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56539)
store volatile %struct.ScmObj* %current_45args56540, %struct.ScmObj** %stackaddr$prim57599, align 8
%stackaddr$prim57600 = alloca %struct.ScmObj*, align 8
%placed47198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56540)
store volatile %struct.ScmObj* %placed47198, %struct.ScmObj** %stackaddr$prim57600, align 8
%stackaddr$prim57601 = alloca %struct.ScmObj*, align 8
%anf_45bind47359 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %placed47198)
store volatile %struct.ScmObj* %anf_45bind47359, %struct.ScmObj** %stackaddr$prim57601, align 8
%truthy$cmp57602 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47359)
%cmp$cmp57602 = icmp eq i64 %truthy$cmp57602, 1
br i1 %cmp$cmp57602, label %truebranch$cmp57602, label %falsebranch$cmp57602
truebranch$cmp57602:
%ae51627 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51628 = call %struct.ScmObj* @const_init_true()
%argslist56542$k474270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57603 = alloca %struct.ScmObj*, align 8
%argslist56542$k474271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51628, %struct.ScmObj* %argslist56542$k474270)
store volatile %struct.ScmObj* %argslist56542$k474271, %struct.ScmObj** %stackaddr$prim57603, align 8
%stackaddr$prim57604 = alloca %struct.ScmObj*, align 8
%argslist56542$k474272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51627, %struct.ScmObj* %argslist56542$k474271)
store volatile %struct.ScmObj* %argslist56542$k474272, %struct.ScmObj** %stackaddr$prim57604, align 8
%clofunc57605 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47427)
musttail call tailcc void %clofunc57605(%struct.ScmObj* %k47427, %struct.ScmObj* %argslist56542$k474272)
ret void
falsebranch$cmp57602:
%stackaddr$prim57606 = alloca %struct.ScmObj*, align 8
%anf_45bind47360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %placed47198)
store volatile %struct.ScmObj* %anf_45bind47360, %struct.ScmObj** %stackaddr$prim57606, align 8
%stackaddr$prim57607 = alloca %struct.ScmObj*, align 8
%anf_45bind47361 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %row47200, %struct.ScmObj* %dist47199)
store volatile %struct.ScmObj* %anf_45bind47361, %struct.ScmObj** %stackaddr$prim57607, align 8
%stackaddr$prim57608 = alloca %struct.ScmObj*, align 8
%anf_45bind47362 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %anf_45bind47360, %struct.ScmObj* %anf_45bind47361)
store volatile %struct.ScmObj* %anf_45bind47362, %struct.ScmObj** %stackaddr$prim57608, align 8
%stackaddr$prim57609 = alloca %struct.ScmObj*, align 8
%anf_45bind47363 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47362)
store volatile %struct.ScmObj* %anf_45bind47363, %struct.ScmObj** %stackaddr$prim57609, align 8
%truthy$cmp57610 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47363)
%cmp$cmp57610 = icmp eq i64 %truthy$cmp57610, 1
br i1 %cmp$cmp57610, label %truebranch$cmp57610, label %falsebranch$cmp57610
truebranch$cmp57610:
%stackaddr$prim57611 = alloca %struct.ScmObj*, align 8
%anf_45bind47364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %placed47198)
store volatile %struct.ScmObj* %anf_45bind47364, %struct.ScmObj** %stackaddr$prim57611, align 8
%stackaddr$prim57612 = alloca %struct.ScmObj*, align 8
%anf_45bind47365 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %row47200, %struct.ScmObj* %dist47199)
store volatile %struct.ScmObj* %anf_45bind47365, %struct.ScmObj** %stackaddr$prim57612, align 8
%stackaddr$prim57613 = alloca %struct.ScmObj*, align 8
%anf_45bind47366 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %anf_45bind47364, %struct.ScmObj* %anf_45bind47365)
store volatile %struct.ScmObj* %anf_45bind47366, %struct.ScmObj** %stackaddr$prim57613, align 8
%stackaddr$prim57614 = alloca %struct.ScmObj*, align 8
%anf_45bind47367 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47366)
store volatile %struct.ScmObj* %anf_45bind47367, %struct.ScmObj** %stackaddr$prim57614, align 8
%truthy$cmp57615 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47367)
%cmp$cmp57615 = icmp eq i64 %truthy$cmp57615, 1
br i1 %cmp$cmp57615, label %truebranch$cmp57615, label %falsebranch$cmp57615
truebranch$cmp57615:
%ae51650 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57616 = alloca %struct.ScmObj*, align 8
%anf_45bind47368 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %ok_6347194, %struct.ScmObj* %ae51650)
store volatile %struct.ScmObj* %anf_45bind47368, %struct.ScmObj** %stackaddr$prim57616, align 8
%ae51652 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57617 = alloca %struct.ScmObj*, align 8
%anf_45bind47369 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %dist47199, %struct.ScmObj* %ae51652)
store volatile %struct.ScmObj* %anf_45bind47369, %struct.ScmObj** %stackaddr$prim57617, align 8
%stackaddr$prim57618 = alloca %struct.ScmObj*, align 8
%anf_45bind47370 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %placed47198)
store volatile %struct.ScmObj* %anf_45bind47370, %struct.ScmObj** %stackaddr$prim57618, align 8
%argslist56543$anf_45bind473680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57619 = alloca %struct.ScmObj*, align 8
%argslist56543$anf_45bind473681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47370, %struct.ScmObj* %argslist56543$anf_45bind473680)
store volatile %struct.ScmObj* %argslist56543$anf_45bind473681, %struct.ScmObj** %stackaddr$prim57619, align 8
%stackaddr$prim57620 = alloca %struct.ScmObj*, align 8
%argslist56543$anf_45bind473682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47369, %struct.ScmObj* %argslist56543$anf_45bind473681)
store volatile %struct.ScmObj* %argslist56543$anf_45bind473682, %struct.ScmObj** %stackaddr$prim57620, align 8
%stackaddr$prim57621 = alloca %struct.ScmObj*, align 8
%argslist56543$anf_45bind473683 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %row47200, %struct.ScmObj* %argslist56543$anf_45bind473682)
store volatile %struct.ScmObj* %argslist56543$anf_45bind473683, %struct.ScmObj** %stackaddr$prim57621, align 8
%stackaddr$prim57622 = alloca %struct.ScmObj*, align 8
%argslist56543$anf_45bind473684 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47427, %struct.ScmObj* %argslist56543$anf_45bind473683)
store volatile %struct.ScmObj* %argslist56543$anf_45bind473684, %struct.ScmObj** %stackaddr$prim57622, align 8
%clofunc57623 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47368)
musttail call tailcc void %clofunc57623(%struct.ScmObj* %anf_45bind47368, %struct.ScmObj* %argslist56543$anf_45bind473684)
ret void
falsebranch$cmp57615:
%ae51678 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51679 = call %struct.ScmObj* @const_init_false()
%argslist56544$k474270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57624 = alloca %struct.ScmObj*, align 8
%argslist56544$k474271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51679, %struct.ScmObj* %argslist56544$k474270)
store volatile %struct.ScmObj* %argslist56544$k474271, %struct.ScmObj** %stackaddr$prim57624, align 8
%stackaddr$prim57625 = alloca %struct.ScmObj*, align 8
%argslist56544$k474272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51678, %struct.ScmObj* %argslist56544$k474271)
store volatile %struct.ScmObj* %argslist56544$k474272, %struct.ScmObj** %stackaddr$prim57625, align 8
%clofunc57626 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47427)
musttail call tailcc void %clofunc57626(%struct.ScmObj* %k47427, %struct.ScmObj* %argslist56544$k474272)
ret void
falsebranch$cmp57610:
%ae51687 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51688 = call %struct.ScmObj* @const_init_false()
%argslist56545$k474270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57627 = alloca %struct.ScmObj*, align 8
%argslist56545$k474271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51688, %struct.ScmObj* %argslist56545$k474270)
store volatile %struct.ScmObj* %argslist56545$k474271, %struct.ScmObj** %stackaddr$prim57627, align 8
%stackaddr$prim57628 = alloca %struct.ScmObj*, align 8
%argslist56545$k474272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51687, %struct.ScmObj* %argslist56545$k474271)
store volatile %struct.ScmObj* %argslist56545$k474272, %struct.ScmObj** %stackaddr$prim57628, align 8
%clofunc57629 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47427)
musttail call tailcc void %clofunc57629(%struct.ScmObj* %k47427, %struct.ScmObj* %argslist56545$k474272)
ret void
}

define tailcc void @proc_clo$ae51231(%struct.ScmObj* %env$ae51231,%struct.ScmObj* %current_45args56547) {
%stackaddr$env-ref57630 = alloca %struct.ScmObj*, align 8
%_37append47165 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51231, i64 0)
store %struct.ScmObj* %_37append47165, %struct.ScmObj** %stackaddr$env-ref57630
%stackaddr$env-ref57631 = alloca %struct.ScmObj*, align 8
%my_45try47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51231, i64 1)
store %struct.ScmObj* %my_45try47195, %struct.ScmObj** %stackaddr$env-ref57631
%stackaddr$env-ref57632 = alloca %struct.ScmObj*, align 8
%ok_6347194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51231, i64 2)
store %struct.ScmObj* %ok_6347194, %struct.ScmObj** %stackaddr$env-ref57632
%stackaddr$prim57633 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56547)
store volatile %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$prim57633, align 8
%stackaddr$prim57634 = alloca %struct.ScmObj*, align 8
%current_45args56548 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56547)
store volatile %struct.ScmObj* %current_45args56548, %struct.ScmObj** %stackaddr$prim57634, align 8
%stackaddr$prim57635 = alloca %struct.ScmObj*, align 8
%x47204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56548)
store volatile %struct.ScmObj* %x47204, %struct.ScmObj** %stackaddr$prim57635, align 8
%stackaddr$prim57636 = alloca %struct.ScmObj*, align 8
%current_45args56549 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56548)
store volatile %struct.ScmObj* %current_45args56549, %struct.ScmObj** %stackaddr$prim57636, align 8
%stackaddr$prim57637 = alloca %struct.ScmObj*, align 8
%y47203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56549)
store volatile %struct.ScmObj* %y47203, %struct.ScmObj** %stackaddr$prim57637, align 8
%stackaddr$prim57638 = alloca %struct.ScmObj*, align 8
%current_45args56550 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56549)
store volatile %struct.ScmObj* %current_45args56550, %struct.ScmObj** %stackaddr$prim57638, align 8
%stackaddr$prim57639 = alloca %struct.ScmObj*, align 8
%z47202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56550)
store volatile %struct.ScmObj* %z47202, %struct.ScmObj** %stackaddr$prim57639, align 8
%stackaddr$prim57640 = alloca %struct.ScmObj*, align 8
%anf_45bind47340 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %x47204)
store volatile %struct.ScmObj* %anf_45bind47340, %struct.ScmObj** %stackaddr$prim57640, align 8
%truthy$cmp57641 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47340)
%cmp$cmp57641 = icmp eq i64 %truthy$cmp57641, 1
br i1 %cmp$cmp57641, label %truebranch$cmp57641, label %falsebranch$cmp57641
truebranch$cmp57641:
%stackaddr$prim57642 = alloca %struct.ScmObj*, align 8
%anf_45bind47341 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %y47203)
store volatile %struct.ScmObj* %anf_45bind47341, %struct.ScmObj** %stackaddr$prim57642, align 8
%truthy$cmp57643 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47341)
%cmp$cmp57643 = icmp eq i64 %truthy$cmp57643, 1
br i1 %cmp$cmp57643, label %truebranch$cmp57643, label %falsebranch$cmp57643
truebranch$cmp57643:
%ae51237 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51238 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist56552$k474280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57644 = alloca %struct.ScmObj*, align 8
%argslist56552$k474281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51238, %struct.ScmObj* %argslist56552$k474280)
store volatile %struct.ScmObj* %argslist56552$k474281, %struct.ScmObj** %stackaddr$prim57644, align 8
%stackaddr$prim57645 = alloca %struct.ScmObj*, align 8
%argslist56552$k474282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51237, %struct.ScmObj* %argslist56552$k474281)
store volatile %struct.ScmObj* %argslist56552$k474282, %struct.ScmObj** %stackaddr$prim57645, align 8
%clofunc57646 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47428)
musttail call tailcc void %clofunc57646(%struct.ScmObj* %k47428, %struct.ScmObj* %argslist56552$k474282)
ret void
falsebranch$cmp57643:
%ae51246 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51247 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56553$k474280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57647 = alloca %struct.ScmObj*, align 8
%argslist56553$k474281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51247, %struct.ScmObj* %argslist56553$k474280)
store volatile %struct.ScmObj* %argslist56553$k474281, %struct.ScmObj** %stackaddr$prim57647, align 8
%stackaddr$prim57648 = alloca %struct.ScmObj*, align 8
%argslist56553$k474282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51246, %struct.ScmObj* %argslist56553$k474281)
store volatile %struct.ScmObj* %argslist56553$k474282, %struct.ScmObj** %stackaddr$prim57648, align 8
%clofunc57649 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47428)
musttail call tailcc void %clofunc57649(%struct.ScmObj* %k47428, %struct.ScmObj* %argslist56553$k474282)
ret void
falsebranch$cmp57641:
%ae51255 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57650 = alloca %struct.ScmObj*, align 8
%anf_45bind47342 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %ok_6347194, %struct.ScmObj* %ae51255)
store volatile %struct.ScmObj* %anf_45bind47342, %struct.ScmObj** %stackaddr$prim57650, align 8
%stackaddr$prim57651 = alloca %struct.ScmObj*, align 8
%anf_45bind47343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47204)
store volatile %struct.ScmObj* %anf_45bind47343, %struct.ScmObj** %stackaddr$prim57651, align 8
%stackaddr$makeclosure57652 = alloca %struct.ScmObj*, align 8
%fptrToInt57653 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51258 to i64
%ae51258 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt57653)
store volatile %struct.ScmObj* %ae51258, %struct.ScmObj** %stackaddr$makeclosure57652, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51258, %struct.ScmObj* %x47204, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51258, %struct.ScmObj* %k47428, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51258, %struct.ScmObj* %y47203, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51258, %struct.ScmObj* %z47202, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51258, %struct.ScmObj* %_37append47165, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51258, %struct.ScmObj* %my_45try47195, i64 5)
%ae51260 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist56588$anf_45bind473420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57654 = alloca %struct.ScmObj*, align 8
%argslist56588$anf_45bind473421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %z47202, %struct.ScmObj* %argslist56588$anf_45bind473420)
store volatile %struct.ScmObj* %argslist56588$anf_45bind473421, %struct.ScmObj** %stackaddr$prim57654, align 8
%stackaddr$prim57655 = alloca %struct.ScmObj*, align 8
%argslist56588$anf_45bind473422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51260, %struct.ScmObj* %argslist56588$anf_45bind473421)
store volatile %struct.ScmObj* %argslist56588$anf_45bind473422, %struct.ScmObj** %stackaddr$prim57655, align 8
%stackaddr$prim57656 = alloca %struct.ScmObj*, align 8
%argslist56588$anf_45bind473423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47343, %struct.ScmObj* %argslist56588$anf_45bind473422)
store volatile %struct.ScmObj* %argslist56588$anf_45bind473423, %struct.ScmObj** %stackaddr$prim57656, align 8
%stackaddr$prim57657 = alloca %struct.ScmObj*, align 8
%argslist56588$anf_45bind473424 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51258, %struct.ScmObj* %argslist56588$anf_45bind473423)
store volatile %struct.ScmObj* %argslist56588$anf_45bind473424, %struct.ScmObj** %stackaddr$prim57657, align 8
%clofunc57658 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47342)
musttail call tailcc void %clofunc57658(%struct.ScmObj* %anf_45bind47342, %struct.ScmObj* %argslist56588$anf_45bind473424)
ret void
}

define tailcc void @proc_clo$ae51258(%struct.ScmObj* %env$ae51258,%struct.ScmObj* %current_45args56554) {
%stackaddr$env-ref57659 = alloca %struct.ScmObj*, align 8
%x47204 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51258, i64 0)
store %struct.ScmObj* %x47204, %struct.ScmObj** %stackaddr$env-ref57659
%stackaddr$env-ref57660 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51258, i64 1)
store %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$env-ref57660
%stackaddr$env-ref57661 = alloca %struct.ScmObj*, align 8
%y47203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51258, i64 2)
store %struct.ScmObj* %y47203, %struct.ScmObj** %stackaddr$env-ref57661
%stackaddr$env-ref57662 = alloca %struct.ScmObj*, align 8
%z47202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51258, i64 3)
store %struct.ScmObj* %z47202, %struct.ScmObj** %stackaddr$env-ref57662
%stackaddr$env-ref57663 = alloca %struct.ScmObj*, align 8
%_37append47165 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51258, i64 4)
store %struct.ScmObj* %_37append47165, %struct.ScmObj** %stackaddr$env-ref57663
%stackaddr$env-ref57664 = alloca %struct.ScmObj*, align 8
%my_45try47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51258, i64 5)
store %struct.ScmObj* %my_45try47195, %struct.ScmObj** %stackaddr$env-ref57664
%stackaddr$prim57665 = alloca %struct.ScmObj*, align 8
%_95k47429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56554)
store volatile %struct.ScmObj* %_95k47429, %struct.ScmObj** %stackaddr$prim57665, align 8
%stackaddr$prim57666 = alloca %struct.ScmObj*, align 8
%current_45args56555 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56554)
store volatile %struct.ScmObj* %current_45args56555, %struct.ScmObj** %stackaddr$prim57666, align 8
%stackaddr$prim57667 = alloca %struct.ScmObj*, align 8
%anf_45bind47344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56555)
store volatile %struct.ScmObj* %anf_45bind47344, %struct.ScmObj** %stackaddr$prim57667, align 8
%truthy$cmp57668 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47344)
%cmp$cmp57668 = icmp eq i64 %truthy$cmp57668, 1
br i1 %cmp$cmp57668, label %truebranch$cmp57668, label %falsebranch$cmp57668
truebranch$cmp57668:
%ae51269 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57669 = alloca %struct.ScmObj*, align 8
%anf_45bind47345 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %my_45try47195, %struct.ScmObj* %ae51269)
store volatile %struct.ScmObj* %anf_45bind47345, %struct.ScmObj** %stackaddr$prim57669, align 8
%stackaddr$prim57670 = alloca %struct.ScmObj*, align 8
%anf_45bind47346 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47204)
store volatile %struct.ScmObj* %anf_45bind47346, %struct.ScmObj** %stackaddr$prim57670, align 8
%stackaddr$makeclosure57671 = alloca %struct.ScmObj*, align 8
%fptrToInt57672 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51272 to i64
%ae51272 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt57672)
store volatile %struct.ScmObj* %ae51272, %struct.ScmObj** %stackaddr$makeclosure57671, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51272, %struct.ScmObj* %x47204, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51272, %struct.ScmObj* %k47428, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51272, %struct.ScmObj* %y47203, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51272, %struct.ScmObj* %z47202, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51272, %struct.ScmObj* %anf_45bind47345, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51272, %struct.ScmObj* %my_45try47195, i64 5)
%argslist56578$_37append471650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57673 = alloca %struct.ScmObj*, align 8
%argslist56578$_37append471651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y47203, %struct.ScmObj* %argslist56578$_37append471650)
store volatile %struct.ScmObj* %argslist56578$_37append471651, %struct.ScmObj** %stackaddr$prim57673, align 8
%stackaddr$prim57674 = alloca %struct.ScmObj*, align 8
%argslist56578$_37append471652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47346, %struct.ScmObj* %argslist56578$_37append471651)
store volatile %struct.ScmObj* %argslist56578$_37append471652, %struct.ScmObj** %stackaddr$prim57674, align 8
%stackaddr$prim57675 = alloca %struct.ScmObj*, align 8
%argslist56578$_37append471653 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51272, %struct.ScmObj* %argslist56578$_37append471652)
store volatile %struct.ScmObj* %argslist56578$_37append471653, %struct.ScmObj** %stackaddr$prim57675, align 8
%clofunc57676 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37append47165)
musttail call tailcc void %clofunc57676(%struct.ScmObj* %_37append47165, %struct.ScmObj* %argslist56578$_37append471653)
ret void
falsebranch$cmp57668:
%stackaddr$makeclosure57677 = alloca %struct.ScmObj*, align 8
%fptrToInt57678 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51407 to i64
%ae51407 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57678)
store volatile %struct.ScmObj* %ae51407, %struct.ScmObj** %stackaddr$makeclosure57677, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51407, %struct.ScmObj* %x47204, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51407, %struct.ScmObj* %k47428, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51407, %struct.ScmObj* %y47203, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51407, %struct.ScmObj* %z47202, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51407, %struct.ScmObj* %my_45try47195, i64 4)
%ae51408 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51409 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56587$ae514070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57679 = alloca %struct.ScmObj*, align 8
%argslist56587$ae514071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51409, %struct.ScmObj* %argslist56587$ae514070)
store volatile %struct.ScmObj* %argslist56587$ae514071, %struct.ScmObj** %stackaddr$prim57679, align 8
%stackaddr$prim57680 = alloca %struct.ScmObj*, align 8
%argslist56587$ae514072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51408, %struct.ScmObj* %argslist56587$ae514071)
store volatile %struct.ScmObj* %argslist56587$ae514072, %struct.ScmObj** %stackaddr$prim57680, align 8
%clofunc57681 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51407)
musttail call tailcc void %clofunc57681(%struct.ScmObj* %ae51407, %struct.ScmObj* %argslist56587$ae514072)
ret void
}

define tailcc void @proc_clo$ae51272(%struct.ScmObj* %env$ae51272,%struct.ScmObj* %current_45args56557) {
%stackaddr$env-ref57682 = alloca %struct.ScmObj*, align 8
%x47204 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51272, i64 0)
store %struct.ScmObj* %x47204, %struct.ScmObj** %stackaddr$env-ref57682
%stackaddr$env-ref57683 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51272, i64 1)
store %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$env-ref57683
%stackaddr$env-ref57684 = alloca %struct.ScmObj*, align 8
%y47203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51272, i64 2)
store %struct.ScmObj* %y47203, %struct.ScmObj** %stackaddr$env-ref57684
%stackaddr$env-ref57685 = alloca %struct.ScmObj*, align 8
%z47202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51272, i64 3)
store %struct.ScmObj* %z47202, %struct.ScmObj** %stackaddr$env-ref57685
%stackaddr$env-ref57686 = alloca %struct.ScmObj*, align 8
%anf_45bind47345 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51272, i64 4)
store %struct.ScmObj* %anf_45bind47345, %struct.ScmObj** %stackaddr$env-ref57686
%stackaddr$env-ref57687 = alloca %struct.ScmObj*, align 8
%my_45try47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51272, i64 5)
store %struct.ScmObj* %my_45try47195, %struct.ScmObj** %stackaddr$env-ref57687
%stackaddr$prim57688 = alloca %struct.ScmObj*, align 8
%_95k47433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56557)
store volatile %struct.ScmObj* %_95k47433, %struct.ScmObj** %stackaddr$prim57688, align 8
%stackaddr$prim57689 = alloca %struct.ScmObj*, align 8
%current_45args56558 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56557)
store volatile %struct.ScmObj* %current_45args56558, %struct.ScmObj** %stackaddr$prim57689, align 8
%stackaddr$prim57690 = alloca %struct.ScmObj*, align 8
%anf_45bind47347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56558)
store volatile %struct.ScmObj* %anf_45bind47347, %struct.ScmObj** %stackaddr$prim57690, align 8
%stackaddr$makeclosure57691 = alloca %struct.ScmObj*, align 8
%fptrToInt57692 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51275 to i64
%ae51275 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57692)
store volatile %struct.ScmObj* %ae51275, %struct.ScmObj** %stackaddr$makeclosure57691, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51275, %struct.ScmObj* %x47204, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51275, %struct.ScmObj* %k47428, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51275, %struct.ScmObj* %y47203, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51275, %struct.ScmObj* %anf_45bind47347, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51275, %struct.ScmObj* %z47202, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51275, %struct.ScmObj* %anf_45bind47345, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51275, %struct.ScmObj* %my_45try47195, i64 6)
%ae51276 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57693 = alloca %struct.ScmObj*, align 8
%fptrToInt57694 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51277 to i64
%ae51277 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57694)
store volatile %struct.ScmObj* %ae51277, %struct.ScmObj** %stackaddr$makeclosure57693, align 8
%argslist56577$ae512750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57695 = alloca %struct.ScmObj*, align 8
%argslist56577$ae512751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51277, %struct.ScmObj* %argslist56577$ae512750)
store volatile %struct.ScmObj* %argslist56577$ae512751, %struct.ScmObj** %stackaddr$prim57695, align 8
%stackaddr$prim57696 = alloca %struct.ScmObj*, align 8
%argslist56577$ae512752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51276, %struct.ScmObj* %argslist56577$ae512751)
store volatile %struct.ScmObj* %argslist56577$ae512752, %struct.ScmObj** %stackaddr$prim57696, align 8
%clofunc57697 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51275)
musttail call tailcc void %clofunc57697(%struct.ScmObj* %ae51275, %struct.ScmObj* %argslist56577$ae512752)
ret void
}

define tailcc void @proc_clo$ae51275(%struct.ScmObj* %env$ae51275,%struct.ScmObj* %current_45args56560) {
%stackaddr$env-ref57698 = alloca %struct.ScmObj*, align 8
%x47204 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51275, i64 0)
store %struct.ScmObj* %x47204, %struct.ScmObj** %stackaddr$env-ref57698
%stackaddr$env-ref57699 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51275, i64 1)
store %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$env-ref57699
%stackaddr$env-ref57700 = alloca %struct.ScmObj*, align 8
%y47203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51275, i64 2)
store %struct.ScmObj* %y47203, %struct.ScmObj** %stackaddr$env-ref57700
%stackaddr$env-ref57701 = alloca %struct.ScmObj*, align 8
%anf_45bind47347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51275, i64 3)
store %struct.ScmObj* %anf_45bind47347, %struct.ScmObj** %stackaddr$env-ref57701
%stackaddr$env-ref57702 = alloca %struct.ScmObj*, align 8
%z47202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51275, i64 4)
store %struct.ScmObj* %z47202, %struct.ScmObj** %stackaddr$env-ref57702
%stackaddr$env-ref57703 = alloca %struct.ScmObj*, align 8
%anf_45bind47345 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51275, i64 5)
store %struct.ScmObj* %anf_45bind47345, %struct.ScmObj** %stackaddr$env-ref57703
%stackaddr$env-ref57704 = alloca %struct.ScmObj*, align 8
%my_45try47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51275, i64 6)
store %struct.ScmObj* %my_45try47195, %struct.ScmObj** %stackaddr$env-ref57704
%stackaddr$prim57705 = alloca %struct.ScmObj*, align 8
%_95k47434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56560)
store volatile %struct.ScmObj* %_95k47434, %struct.ScmObj** %stackaddr$prim57705, align 8
%stackaddr$prim57706 = alloca %struct.ScmObj*, align 8
%current_45args56561 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56560)
store volatile %struct.ScmObj* %current_45args56561, %struct.ScmObj** %stackaddr$prim57706, align 8
%stackaddr$prim57707 = alloca %struct.ScmObj*, align 8
%anf_45bind47348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56561)
store volatile %struct.ScmObj* %anf_45bind47348, %struct.ScmObj** %stackaddr$prim57707, align 8
%stackaddr$makeclosure57708 = alloca %struct.ScmObj*, align 8
%fptrToInt57709 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51298 to i64
%ae51298 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57709)
store volatile %struct.ScmObj* %ae51298, %struct.ScmObj** %stackaddr$makeclosure57708, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51298, %struct.ScmObj* %x47204, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51298, %struct.ScmObj* %k47428, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51298, %struct.ScmObj* %y47203, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51298, %struct.ScmObj* %anf_45bind47347, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51298, %struct.ScmObj* %z47202, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51298, %struct.ScmObj* %anf_45bind47345, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51298, %struct.ScmObj* %my_45try47195, i64 6)
%argslist56575$anf_45bind473480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57710 = alloca %struct.ScmObj*, align 8
%argslist56575$anf_45bind473481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51298, %struct.ScmObj* %argslist56575$anf_45bind473480)
store volatile %struct.ScmObj* %argslist56575$anf_45bind473481, %struct.ScmObj** %stackaddr$prim57710, align 8
%clofunc57711 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47348)
musttail call tailcc void %clofunc57711(%struct.ScmObj* %anf_45bind47348, %struct.ScmObj* %argslist56575$anf_45bind473481)
ret void
}

define tailcc void @proc_clo$ae51298(%struct.ScmObj* %env$ae51298,%struct.ScmObj* %current_45args56563) {
%stackaddr$env-ref57712 = alloca %struct.ScmObj*, align 8
%x47204 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51298, i64 0)
store %struct.ScmObj* %x47204, %struct.ScmObj** %stackaddr$env-ref57712
%stackaddr$env-ref57713 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51298, i64 1)
store %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$env-ref57713
%stackaddr$env-ref57714 = alloca %struct.ScmObj*, align 8
%y47203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51298, i64 2)
store %struct.ScmObj* %y47203, %struct.ScmObj** %stackaddr$env-ref57714
%stackaddr$env-ref57715 = alloca %struct.ScmObj*, align 8
%anf_45bind47347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51298, i64 3)
store %struct.ScmObj* %anf_45bind47347, %struct.ScmObj** %stackaddr$env-ref57715
%stackaddr$env-ref57716 = alloca %struct.ScmObj*, align 8
%z47202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51298, i64 4)
store %struct.ScmObj* %z47202, %struct.ScmObj** %stackaddr$env-ref57716
%stackaddr$env-ref57717 = alloca %struct.ScmObj*, align 8
%anf_45bind47345 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51298, i64 5)
store %struct.ScmObj* %anf_45bind47345, %struct.ScmObj** %stackaddr$env-ref57717
%stackaddr$env-ref57718 = alloca %struct.ScmObj*, align 8
%my_45try47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51298, i64 6)
store %struct.ScmObj* %my_45try47195, %struct.ScmObj** %stackaddr$env-ref57718
%stackaddr$prim57719 = alloca %struct.ScmObj*, align 8
%_95k47435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56563)
store volatile %struct.ScmObj* %_95k47435, %struct.ScmObj** %stackaddr$prim57719, align 8
%stackaddr$prim57720 = alloca %struct.ScmObj*, align 8
%current_45args56564 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56563)
store volatile %struct.ScmObj* %current_45args56564, %struct.ScmObj** %stackaddr$prim57720, align 8
%stackaddr$prim57721 = alloca %struct.ScmObj*, align 8
%anf_45bind47349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56564)
store volatile %struct.ScmObj* %anf_45bind47349, %struct.ScmObj** %stackaddr$prim57721, align 8
%stackaddr$prim57722 = alloca %struct.ScmObj*, align 8
%anf_45bind47350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47204)
store volatile %struct.ScmObj* %anf_45bind47350, %struct.ScmObj** %stackaddr$prim57722, align 8
%stackaddr$prim57723 = alloca %struct.ScmObj*, align 8
%anf_45bind47351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47350, %struct.ScmObj* %z47202)
store volatile %struct.ScmObj* %anf_45bind47351, %struct.ScmObj** %stackaddr$prim57723, align 8
%stackaddr$makeclosure57724 = alloca %struct.ScmObj*, align 8
%fptrToInt57725 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51303 to i64
%ae51303 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57725)
store volatile %struct.ScmObj* %ae51303, %struct.ScmObj** %stackaddr$makeclosure57724, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51303, %struct.ScmObj* %x47204, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51303, %struct.ScmObj* %k47428, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51303, %struct.ScmObj* %y47203, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51303, %struct.ScmObj* %z47202, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51303, %struct.ScmObj* %my_45try47195, i64 4)
%argslist56574$anf_45bind473450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57726 = alloca %struct.ScmObj*, align 8
%argslist56574$anf_45bind473451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47351, %struct.ScmObj* %argslist56574$anf_45bind473450)
store volatile %struct.ScmObj* %argslist56574$anf_45bind473451, %struct.ScmObj** %stackaddr$prim57726, align 8
%stackaddr$prim57727 = alloca %struct.ScmObj*, align 8
%argslist56574$anf_45bind473452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47349, %struct.ScmObj* %argslist56574$anf_45bind473451)
store volatile %struct.ScmObj* %argslist56574$anf_45bind473452, %struct.ScmObj** %stackaddr$prim57727, align 8
%stackaddr$prim57728 = alloca %struct.ScmObj*, align 8
%argslist56574$anf_45bind473453 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47347, %struct.ScmObj* %argslist56574$anf_45bind473452)
store volatile %struct.ScmObj* %argslist56574$anf_45bind473453, %struct.ScmObj** %stackaddr$prim57728, align 8
%stackaddr$prim57729 = alloca %struct.ScmObj*, align 8
%argslist56574$anf_45bind473454 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51303, %struct.ScmObj* %argslist56574$anf_45bind473453)
store volatile %struct.ScmObj* %argslist56574$anf_45bind473454, %struct.ScmObj** %stackaddr$prim57729, align 8
%clofunc57730 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47345)
musttail call tailcc void %clofunc57730(%struct.ScmObj* %anf_45bind47345, %struct.ScmObj* %argslist56574$anf_45bind473454)
ret void
}

define tailcc void @proc_clo$ae51303(%struct.ScmObj* %env$ae51303,%struct.ScmObj* %current_45args56566) {
%stackaddr$env-ref57731 = alloca %struct.ScmObj*, align 8
%x47204 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51303, i64 0)
store %struct.ScmObj* %x47204, %struct.ScmObj** %stackaddr$env-ref57731
%stackaddr$env-ref57732 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51303, i64 1)
store %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$env-ref57732
%stackaddr$env-ref57733 = alloca %struct.ScmObj*, align 8
%y47203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51303, i64 2)
store %struct.ScmObj* %y47203, %struct.ScmObj** %stackaddr$env-ref57733
%stackaddr$env-ref57734 = alloca %struct.ScmObj*, align 8
%z47202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51303, i64 3)
store %struct.ScmObj* %z47202, %struct.ScmObj** %stackaddr$env-ref57734
%stackaddr$env-ref57735 = alloca %struct.ScmObj*, align 8
%my_45try47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51303, i64 4)
store %struct.ScmObj* %my_45try47195, %struct.ScmObj** %stackaddr$env-ref57735
%stackaddr$prim57736 = alloca %struct.ScmObj*, align 8
%_95k47430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56566)
store volatile %struct.ScmObj* %_95k47430, %struct.ScmObj** %stackaddr$prim57736, align 8
%stackaddr$prim57737 = alloca %struct.ScmObj*, align 8
%current_45args56567 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56566)
store volatile %struct.ScmObj* %current_45args56567, %struct.ScmObj** %stackaddr$prim57737, align 8
%stackaddr$prim57738 = alloca %struct.ScmObj*, align 8
%anf_45bind47352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56567)
store volatile %struct.ScmObj* %anf_45bind47352, %struct.ScmObj** %stackaddr$prim57738, align 8
%ae51308 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57739 = alloca %struct.ScmObj*, align 8
%anf_45bind47353 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %my_45try47195, %struct.ScmObj* %ae51308)
store volatile %struct.ScmObj* %anf_45bind47353, %struct.ScmObj** %stackaddr$prim57739, align 8
%stackaddr$prim57740 = alloca %struct.ScmObj*, align 8
%anf_45bind47354 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47204)
store volatile %struct.ScmObj* %anf_45bind47354, %struct.ScmObj** %stackaddr$prim57740, align 8
%stackaddr$prim57741 = alloca %struct.ScmObj*, align 8
%anf_45bind47355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47204)
store volatile %struct.ScmObj* %anf_45bind47355, %struct.ScmObj** %stackaddr$prim57741, align 8
%stackaddr$prim57742 = alloca %struct.ScmObj*, align 8
%anf_45bind47356 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47355, %struct.ScmObj* %y47203)
store volatile %struct.ScmObj* %anf_45bind47356, %struct.ScmObj** %stackaddr$prim57742, align 8
%stackaddr$makeclosure57743 = alloca %struct.ScmObj*, align 8
%fptrToInt57744 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51314 to i64
%ae51314 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57744)
store volatile %struct.ScmObj* %ae51314, %struct.ScmObj** %stackaddr$makeclosure57743, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51314, %struct.ScmObj* %k47428, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51314, %struct.ScmObj* %anf_45bind47352, i64 1)
%argslist56573$anf_45bind473530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57745 = alloca %struct.ScmObj*, align 8
%argslist56573$anf_45bind473531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %z47202, %struct.ScmObj* %argslist56573$anf_45bind473530)
store volatile %struct.ScmObj* %argslist56573$anf_45bind473531, %struct.ScmObj** %stackaddr$prim57745, align 8
%stackaddr$prim57746 = alloca %struct.ScmObj*, align 8
%argslist56573$anf_45bind473532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47356, %struct.ScmObj* %argslist56573$anf_45bind473531)
store volatile %struct.ScmObj* %argslist56573$anf_45bind473532, %struct.ScmObj** %stackaddr$prim57746, align 8
%stackaddr$prim57747 = alloca %struct.ScmObj*, align 8
%argslist56573$anf_45bind473533 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47354, %struct.ScmObj* %argslist56573$anf_45bind473532)
store volatile %struct.ScmObj* %argslist56573$anf_45bind473533, %struct.ScmObj** %stackaddr$prim57747, align 8
%stackaddr$prim57748 = alloca %struct.ScmObj*, align 8
%argslist56573$anf_45bind473534 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51314, %struct.ScmObj* %argslist56573$anf_45bind473533)
store volatile %struct.ScmObj* %argslist56573$anf_45bind473534, %struct.ScmObj** %stackaddr$prim57748, align 8
%clofunc57749 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47353)
musttail call tailcc void %clofunc57749(%struct.ScmObj* %anf_45bind47353, %struct.ScmObj* %argslist56573$anf_45bind473534)
ret void
}

define tailcc void @proc_clo$ae51314(%struct.ScmObj* %env$ae51314,%struct.ScmObj* %current_45args56569) {
%stackaddr$env-ref57750 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51314, i64 0)
store %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$env-ref57750
%stackaddr$env-ref57751 = alloca %struct.ScmObj*, align 8
%anf_45bind47352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51314, i64 1)
store %struct.ScmObj* %anf_45bind47352, %struct.ScmObj** %stackaddr$env-ref57751
%stackaddr$prim57752 = alloca %struct.ScmObj*, align 8
%_95k47431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56569)
store volatile %struct.ScmObj* %_95k47431, %struct.ScmObj** %stackaddr$prim57752, align 8
%stackaddr$prim57753 = alloca %struct.ScmObj*, align 8
%current_45args56570 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56569)
store volatile %struct.ScmObj* %current_45args56570, %struct.ScmObj** %stackaddr$prim57753, align 8
%stackaddr$prim57754 = alloca %struct.ScmObj*, align 8
%anf_45bind47357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56570)
store volatile %struct.ScmObj* %anf_45bind47357, %struct.ScmObj** %stackaddr$prim57754, align 8
%stackaddr$prim57755 = alloca %struct.ScmObj*, align 8
%cpsprim47432 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %anf_45bind47352, %struct.ScmObj* %anf_45bind47357)
store volatile %struct.ScmObj* %cpsprim47432, %struct.ScmObj** %stackaddr$prim57755, align 8
%ae51321 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56572$k474280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57756 = alloca %struct.ScmObj*, align 8
%argslist56572$k474281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47432, %struct.ScmObj* %argslist56572$k474280)
store volatile %struct.ScmObj* %argslist56572$k474281, %struct.ScmObj** %stackaddr$prim57756, align 8
%stackaddr$prim57757 = alloca %struct.ScmObj*, align 8
%argslist56572$k474282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51321, %struct.ScmObj* %argslist56572$k474281)
store volatile %struct.ScmObj* %argslist56572$k474282, %struct.ScmObj** %stackaddr$prim57757, align 8
%clofunc57758 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47428)
musttail call tailcc void %clofunc57758(%struct.ScmObj* %k47428, %struct.ScmObj* %argslist56572$k474282)
ret void
}

define tailcc void @proc_clo$ae51277(%struct.ScmObj* %env$ae51277,%struct.ScmObj* %lst4720547436) {
%stackaddr$prim57759 = alloca %struct.ScmObj*, align 8
%k47437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4720547436)
store volatile %struct.ScmObj* %k47437, %struct.ScmObj** %stackaddr$prim57759, align 8
%stackaddr$prim57760 = alloca %struct.ScmObj*, align 8
%lst47205 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4720547436)
store volatile %struct.ScmObj* %lst47205, %struct.ScmObj** %stackaddr$prim57760, align 8
%ae51281 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56576$k474370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57761 = alloca %struct.ScmObj*, align 8
%argslist56576$k474371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47205, %struct.ScmObj* %argslist56576$k474370)
store volatile %struct.ScmObj* %argslist56576$k474371, %struct.ScmObj** %stackaddr$prim57761, align 8
%stackaddr$prim57762 = alloca %struct.ScmObj*, align 8
%argslist56576$k474372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51281, %struct.ScmObj* %argslist56576$k474371)
store volatile %struct.ScmObj* %argslist56576$k474372, %struct.ScmObj** %stackaddr$prim57762, align 8
%clofunc57763 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47437)
musttail call tailcc void %clofunc57763(%struct.ScmObj* %k47437, %struct.ScmObj* %argslist56576$k474372)
ret void
}

define tailcc void @proc_clo$ae51407(%struct.ScmObj* %env$ae51407,%struct.ScmObj* %current_45args56579) {
%stackaddr$env-ref57764 = alloca %struct.ScmObj*, align 8
%x47204 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51407, i64 0)
store %struct.ScmObj* %x47204, %struct.ScmObj** %stackaddr$env-ref57764
%stackaddr$env-ref57765 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51407, i64 1)
store %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$env-ref57765
%stackaddr$env-ref57766 = alloca %struct.ScmObj*, align 8
%y47203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51407, i64 2)
store %struct.ScmObj* %y47203, %struct.ScmObj** %stackaddr$env-ref57766
%stackaddr$env-ref57767 = alloca %struct.ScmObj*, align 8
%z47202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51407, i64 3)
store %struct.ScmObj* %z47202, %struct.ScmObj** %stackaddr$env-ref57767
%stackaddr$env-ref57768 = alloca %struct.ScmObj*, align 8
%my_45try47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51407, i64 4)
store %struct.ScmObj* %my_45try47195, %struct.ScmObj** %stackaddr$env-ref57768
%stackaddr$prim57769 = alloca %struct.ScmObj*, align 8
%_95k47430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56579)
store volatile %struct.ScmObj* %_95k47430, %struct.ScmObj** %stackaddr$prim57769, align 8
%stackaddr$prim57770 = alloca %struct.ScmObj*, align 8
%current_45args56580 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56579)
store volatile %struct.ScmObj* %current_45args56580, %struct.ScmObj** %stackaddr$prim57770, align 8
%stackaddr$prim57771 = alloca %struct.ScmObj*, align 8
%anf_45bind47352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56580)
store volatile %struct.ScmObj* %anf_45bind47352, %struct.ScmObj** %stackaddr$prim57771, align 8
%ae51417 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57772 = alloca %struct.ScmObj*, align 8
%anf_45bind47353 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %my_45try47195, %struct.ScmObj* %ae51417)
store volatile %struct.ScmObj* %anf_45bind47353, %struct.ScmObj** %stackaddr$prim57772, align 8
%stackaddr$prim57773 = alloca %struct.ScmObj*, align 8
%anf_45bind47354 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47204)
store volatile %struct.ScmObj* %anf_45bind47354, %struct.ScmObj** %stackaddr$prim57773, align 8
%stackaddr$prim57774 = alloca %struct.ScmObj*, align 8
%anf_45bind47355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47204)
store volatile %struct.ScmObj* %anf_45bind47355, %struct.ScmObj** %stackaddr$prim57774, align 8
%stackaddr$prim57775 = alloca %struct.ScmObj*, align 8
%anf_45bind47356 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47355, %struct.ScmObj* %y47203)
store volatile %struct.ScmObj* %anf_45bind47356, %struct.ScmObj** %stackaddr$prim57775, align 8
%stackaddr$makeclosure57776 = alloca %struct.ScmObj*, align 8
%fptrToInt57777 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51423 to i64
%ae51423 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57777)
store volatile %struct.ScmObj* %ae51423, %struct.ScmObj** %stackaddr$makeclosure57776, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51423, %struct.ScmObj* %k47428, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51423, %struct.ScmObj* %anf_45bind47352, i64 1)
%argslist56586$anf_45bind473530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57778 = alloca %struct.ScmObj*, align 8
%argslist56586$anf_45bind473531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %z47202, %struct.ScmObj* %argslist56586$anf_45bind473530)
store volatile %struct.ScmObj* %argslist56586$anf_45bind473531, %struct.ScmObj** %stackaddr$prim57778, align 8
%stackaddr$prim57779 = alloca %struct.ScmObj*, align 8
%argslist56586$anf_45bind473532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47356, %struct.ScmObj* %argslist56586$anf_45bind473531)
store volatile %struct.ScmObj* %argslist56586$anf_45bind473532, %struct.ScmObj** %stackaddr$prim57779, align 8
%stackaddr$prim57780 = alloca %struct.ScmObj*, align 8
%argslist56586$anf_45bind473533 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47354, %struct.ScmObj* %argslist56586$anf_45bind473532)
store volatile %struct.ScmObj* %argslist56586$anf_45bind473533, %struct.ScmObj** %stackaddr$prim57780, align 8
%stackaddr$prim57781 = alloca %struct.ScmObj*, align 8
%argslist56586$anf_45bind473534 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51423, %struct.ScmObj* %argslist56586$anf_45bind473533)
store volatile %struct.ScmObj* %argslist56586$anf_45bind473534, %struct.ScmObj** %stackaddr$prim57781, align 8
%clofunc57782 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47353)
musttail call tailcc void %clofunc57782(%struct.ScmObj* %anf_45bind47353, %struct.ScmObj* %argslist56586$anf_45bind473534)
ret void
}

define tailcc void @proc_clo$ae51423(%struct.ScmObj* %env$ae51423,%struct.ScmObj* %current_45args56582) {
%stackaddr$env-ref57783 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51423, i64 0)
store %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$env-ref57783
%stackaddr$env-ref57784 = alloca %struct.ScmObj*, align 8
%anf_45bind47352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51423, i64 1)
store %struct.ScmObj* %anf_45bind47352, %struct.ScmObj** %stackaddr$env-ref57784
%stackaddr$prim57785 = alloca %struct.ScmObj*, align 8
%_95k47431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56582)
store volatile %struct.ScmObj* %_95k47431, %struct.ScmObj** %stackaddr$prim57785, align 8
%stackaddr$prim57786 = alloca %struct.ScmObj*, align 8
%current_45args56583 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56582)
store volatile %struct.ScmObj* %current_45args56583, %struct.ScmObj** %stackaddr$prim57786, align 8
%stackaddr$prim57787 = alloca %struct.ScmObj*, align 8
%anf_45bind47357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56583)
store volatile %struct.ScmObj* %anf_45bind47357, %struct.ScmObj** %stackaddr$prim57787, align 8
%stackaddr$prim57788 = alloca %struct.ScmObj*, align 8
%cpsprim47432 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %anf_45bind47352, %struct.ScmObj* %anf_45bind47357)
store volatile %struct.ScmObj* %cpsprim47432, %struct.ScmObj** %stackaddr$prim57788, align 8
%ae51430 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56585$k474280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57789 = alloca %struct.ScmObj*, align 8
%argslist56585$k474281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47432, %struct.ScmObj* %argslist56585$k474280)
store volatile %struct.ScmObj* %argslist56585$k474281, %struct.ScmObj** %stackaddr$prim57789, align 8
%stackaddr$prim57790 = alloca %struct.ScmObj*, align 8
%argslist56585$k474282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51430, %struct.ScmObj* %argslist56585$k474281)
store volatile %struct.ScmObj* %argslist56585$k474282, %struct.ScmObj** %stackaddr$prim57790, align 8
%clofunc57791 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47428)
musttail call tailcc void %clofunc57791(%struct.ScmObj* %k47428, %struct.ScmObj* %argslist56585$k474282)
ret void
}

define tailcc void @proc_clo$ae50962(%struct.ScmObj* %env$ae50962,%struct.ScmObj* %current_45args56590) {
%stackaddr$prim57792 = alloca %struct.ScmObj*, align 8
%k47438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56590)
store volatile %struct.ScmObj* %k47438, %struct.ScmObj** %stackaddr$prim57792, align 8
%stackaddr$prim57793 = alloca %struct.ScmObj*, align 8
%current_45args56591 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56590)
store volatile %struct.ScmObj* %current_45args56591, %struct.ScmObj** %stackaddr$prim57793, align 8
%stackaddr$prim57794 = alloca %struct.ScmObj*, align 8
%n47207 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56591)
store volatile %struct.ScmObj* %n47207, %struct.ScmObj** %stackaddr$prim57794, align 8
%stackaddr$prim57795 = alloca %struct.ScmObj*, align 8
%anf_45bind47330 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47330, %struct.ScmObj** %stackaddr$prim57795, align 8
%ae50963 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57796 = alloca %struct.ScmObj*, align 8
%loop47208 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50963, %struct.ScmObj* %anf_45bind47330)
store volatile %struct.ScmObj* %loop47208, %struct.ScmObj** %stackaddr$prim57796, align 8
%stackaddr$makeclosure57797 = alloca %struct.ScmObj*, align 8
%fptrToInt57798 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50965 to i64
%ae50965 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57798)
store volatile %struct.ScmObj* %ae50965, %struct.ScmObj** %stackaddr$makeclosure57797, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50965, %struct.ScmObj* %k47438, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50965, %struct.ScmObj* %loop47208, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50965, %struct.ScmObj* %n47207, i64 2)
%ae50966 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57799 = alloca %struct.ScmObj*, align 8
%fptrToInt57800 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50967 to i64
%ae50967 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57800)
store volatile %struct.ScmObj* %ae50967, %struct.ScmObj** %stackaddr$makeclosure57799, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50967, %struct.ScmObj* %loop47208, i64 0)
%argslist56612$ae509650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57801 = alloca %struct.ScmObj*, align 8
%argslist56612$ae509651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50967, %struct.ScmObj* %argslist56612$ae509650)
store volatile %struct.ScmObj* %argslist56612$ae509651, %struct.ScmObj** %stackaddr$prim57801, align 8
%stackaddr$prim57802 = alloca %struct.ScmObj*, align 8
%argslist56612$ae509652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50966, %struct.ScmObj* %argslist56612$ae509651)
store volatile %struct.ScmObj* %argslist56612$ae509652, %struct.ScmObj** %stackaddr$prim57802, align 8
%clofunc57803 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50965)
musttail call tailcc void %clofunc57803(%struct.ScmObj* %ae50965, %struct.ScmObj* %argslist56612$ae509652)
ret void
}

define tailcc void @proc_clo$ae50965(%struct.ScmObj* %env$ae50965,%struct.ScmObj* %current_45args56593) {
%stackaddr$env-ref57804 = alloca %struct.ScmObj*, align 8
%k47438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50965, i64 0)
store %struct.ScmObj* %k47438, %struct.ScmObj** %stackaddr$env-ref57804
%stackaddr$env-ref57805 = alloca %struct.ScmObj*, align 8
%loop47208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50965, i64 1)
store %struct.ScmObj* %loop47208, %struct.ScmObj** %stackaddr$env-ref57805
%stackaddr$env-ref57806 = alloca %struct.ScmObj*, align 8
%n47207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50965, i64 2)
store %struct.ScmObj* %n47207, %struct.ScmObj** %stackaddr$env-ref57806
%stackaddr$prim57807 = alloca %struct.ScmObj*, align 8
%_95k47439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56593)
store volatile %struct.ScmObj* %_95k47439, %struct.ScmObj** %stackaddr$prim57807, align 8
%stackaddr$prim57808 = alloca %struct.ScmObj*, align 8
%current_45args56594 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56593)
store volatile %struct.ScmObj* %current_45args56594, %struct.ScmObj** %stackaddr$prim57808, align 8
%stackaddr$prim57809 = alloca %struct.ScmObj*, align 8
%anf_45bind47335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56594)
store volatile %struct.ScmObj* %anf_45bind47335, %struct.ScmObj** %stackaddr$prim57809, align 8
%ae51044 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57810 = alloca %struct.ScmObj*, align 8
%t4707247209 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %loop47208, %struct.ScmObj* %ae51044, %struct.ScmObj* %anf_45bind47335)
store volatile %struct.ScmObj* %t4707247209, %struct.ScmObj** %stackaddr$prim57810, align 8
%ae51047 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57811 = alloca %struct.ScmObj*, align 8
%anf_45bind47336 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %loop47208, %struct.ScmObj* %ae51047)
store volatile %struct.ScmObj* %anf_45bind47336, %struct.ScmObj** %stackaddr$prim57811, align 8
%stackaddr$makeclosure57812 = alloca %struct.ScmObj*, align 8
%fptrToInt57813 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51048 to i64
%ae51048 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57813)
store volatile %struct.ScmObj* %ae51048, %struct.ScmObj** %stackaddr$makeclosure57812, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51048, %struct.ScmObj* %k47438, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51048, %struct.ScmObj* %anf_45bind47336, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51048, %struct.ScmObj* %n47207, i64 2)
%ae51049 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57814 = alloca %struct.ScmObj*, align 8
%fptrToInt57815 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51050 to i64
%ae51050 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57815)
store volatile %struct.ScmObj* %ae51050, %struct.ScmObj** %stackaddr$makeclosure57814, align 8
%argslist56605$ae510480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57816 = alloca %struct.ScmObj*, align 8
%argslist56605$ae510481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51050, %struct.ScmObj* %argslist56605$ae510480)
store volatile %struct.ScmObj* %argslist56605$ae510481, %struct.ScmObj** %stackaddr$prim57816, align 8
%stackaddr$prim57817 = alloca %struct.ScmObj*, align 8
%argslist56605$ae510482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51049, %struct.ScmObj* %argslist56605$ae510481)
store volatile %struct.ScmObj* %argslist56605$ae510482, %struct.ScmObj** %stackaddr$prim57817, align 8
%clofunc57818 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51048)
musttail call tailcc void %clofunc57818(%struct.ScmObj* %ae51048, %struct.ScmObj* %argslist56605$ae510482)
ret void
}

define tailcc void @proc_clo$ae51048(%struct.ScmObj* %env$ae51048,%struct.ScmObj* %current_45args56596) {
%stackaddr$env-ref57819 = alloca %struct.ScmObj*, align 8
%k47438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51048, i64 0)
store %struct.ScmObj* %k47438, %struct.ScmObj** %stackaddr$env-ref57819
%stackaddr$env-ref57820 = alloca %struct.ScmObj*, align 8
%anf_45bind47336 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51048, i64 1)
store %struct.ScmObj* %anf_45bind47336, %struct.ScmObj** %stackaddr$env-ref57820
%stackaddr$env-ref57821 = alloca %struct.ScmObj*, align 8
%n47207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51048, i64 2)
store %struct.ScmObj* %n47207, %struct.ScmObj** %stackaddr$env-ref57821
%stackaddr$prim57822 = alloca %struct.ScmObj*, align 8
%_95k47440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56596)
store volatile %struct.ScmObj* %_95k47440, %struct.ScmObj** %stackaddr$prim57822, align 8
%stackaddr$prim57823 = alloca %struct.ScmObj*, align 8
%current_45args56597 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56596)
store volatile %struct.ScmObj* %current_45args56597, %struct.ScmObj** %stackaddr$prim57823, align 8
%stackaddr$prim57824 = alloca %struct.ScmObj*, align 8
%anf_45bind47337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56597)
store volatile %struct.ScmObj* %anf_45bind47337, %struct.ScmObj** %stackaddr$prim57824, align 8
%stackaddr$makeclosure57825 = alloca %struct.ScmObj*, align 8
%fptrToInt57826 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51071 to i64
%ae51071 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57826)
store volatile %struct.ScmObj* %ae51071, %struct.ScmObj** %stackaddr$makeclosure57825, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51071, %struct.ScmObj* %k47438, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51071, %struct.ScmObj* %anf_45bind47336, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51071, %struct.ScmObj* %n47207, i64 2)
%argslist56603$anf_45bind473370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57827 = alloca %struct.ScmObj*, align 8
%argslist56603$anf_45bind473371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51071, %struct.ScmObj* %argslist56603$anf_45bind473370)
store volatile %struct.ScmObj* %argslist56603$anf_45bind473371, %struct.ScmObj** %stackaddr$prim57827, align 8
%clofunc57828 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47337)
musttail call tailcc void %clofunc57828(%struct.ScmObj* %anf_45bind47337, %struct.ScmObj* %argslist56603$anf_45bind473371)
ret void
}

define tailcc void @proc_clo$ae51071(%struct.ScmObj* %env$ae51071,%struct.ScmObj* %current_45args56599) {
%stackaddr$env-ref57829 = alloca %struct.ScmObj*, align 8
%k47438 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51071, i64 0)
store %struct.ScmObj* %k47438, %struct.ScmObj** %stackaddr$env-ref57829
%stackaddr$env-ref57830 = alloca %struct.ScmObj*, align 8
%anf_45bind47336 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51071, i64 1)
store %struct.ScmObj* %anf_45bind47336, %struct.ScmObj** %stackaddr$env-ref57830
%stackaddr$env-ref57831 = alloca %struct.ScmObj*, align 8
%n47207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51071, i64 2)
store %struct.ScmObj* %n47207, %struct.ScmObj** %stackaddr$env-ref57831
%stackaddr$prim57832 = alloca %struct.ScmObj*, align 8
%_95k47441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56599)
store volatile %struct.ScmObj* %_95k47441, %struct.ScmObj** %stackaddr$prim57832, align 8
%stackaddr$prim57833 = alloca %struct.ScmObj*, align 8
%current_45args56600 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56599)
store volatile %struct.ScmObj* %current_45args56600, %struct.ScmObj** %stackaddr$prim57833, align 8
%stackaddr$prim57834 = alloca %struct.ScmObj*, align 8
%anf_45bind47338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56600)
store volatile %struct.ScmObj* %anf_45bind47338, %struct.ScmObj** %stackaddr$prim57834, align 8
%argslist56602$anf_45bind473360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57835 = alloca %struct.ScmObj*, align 8
%argslist56602$anf_45bind473361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47338, %struct.ScmObj* %argslist56602$anf_45bind473360)
store volatile %struct.ScmObj* %argslist56602$anf_45bind473361, %struct.ScmObj** %stackaddr$prim57835, align 8
%stackaddr$prim57836 = alloca %struct.ScmObj*, align 8
%argslist56602$anf_45bind473362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %n47207, %struct.ScmObj* %argslist56602$anf_45bind473361)
store volatile %struct.ScmObj* %argslist56602$anf_45bind473362, %struct.ScmObj** %stackaddr$prim57836, align 8
%stackaddr$prim57837 = alloca %struct.ScmObj*, align 8
%argslist56602$anf_45bind473363 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47438, %struct.ScmObj* %argslist56602$anf_45bind473362)
store volatile %struct.ScmObj* %argslist56602$anf_45bind473363, %struct.ScmObj** %stackaddr$prim57837, align 8
%clofunc57838 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47336)
musttail call tailcc void %clofunc57838(%struct.ScmObj* %anf_45bind47336, %struct.ScmObj* %argslist56602$anf_45bind473363)
ret void
}

define tailcc void @proc_clo$ae51050(%struct.ScmObj* %env$ae51050,%struct.ScmObj* %lst4721247442) {
%stackaddr$prim57839 = alloca %struct.ScmObj*, align 8
%k47443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4721247442)
store volatile %struct.ScmObj* %k47443, %struct.ScmObj** %stackaddr$prim57839, align 8
%stackaddr$prim57840 = alloca %struct.ScmObj*, align 8
%lst47212 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4721247442)
store volatile %struct.ScmObj* %lst47212, %struct.ScmObj** %stackaddr$prim57840, align 8
%ae51054 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56604$k474430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57841 = alloca %struct.ScmObj*, align 8
%argslist56604$k474431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47212, %struct.ScmObj* %argslist56604$k474430)
store volatile %struct.ScmObj* %argslist56604$k474431, %struct.ScmObj** %stackaddr$prim57841, align 8
%stackaddr$prim57842 = alloca %struct.ScmObj*, align 8
%argslist56604$k474432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51054, %struct.ScmObj* %argslist56604$k474431)
store volatile %struct.ScmObj* %argslist56604$k474432, %struct.ScmObj** %stackaddr$prim57842, align 8
%clofunc57843 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47443)
musttail call tailcc void %clofunc57843(%struct.ScmObj* %k47443, %struct.ScmObj* %argslist56604$k474432)
ret void
}

define tailcc void @proc_clo$ae50967(%struct.ScmObj* %env$ae50967,%struct.ScmObj* %current_45args56606) {
%stackaddr$env-ref57844 = alloca %struct.ScmObj*, align 8
%loop47208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50967, i64 0)
store %struct.ScmObj* %loop47208, %struct.ScmObj** %stackaddr$env-ref57844
%stackaddr$prim57845 = alloca %struct.ScmObj*, align 8
%k47444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56606)
store volatile %struct.ScmObj* %k47444, %struct.ScmObj** %stackaddr$prim57845, align 8
%stackaddr$prim57846 = alloca %struct.ScmObj*, align 8
%current_45args56607 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56606)
store volatile %struct.ScmObj* %current_45args56607, %struct.ScmObj** %stackaddr$prim57846, align 8
%stackaddr$prim57847 = alloca %struct.ScmObj*, align 8
%i47211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56607)
store volatile %struct.ScmObj* %i47211, %struct.ScmObj** %stackaddr$prim57847, align 8
%stackaddr$prim57848 = alloca %struct.ScmObj*, align 8
%current_45args56608 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56607)
store volatile %struct.ScmObj* %current_45args56608, %struct.ScmObj** %stackaddr$prim57848, align 8
%stackaddr$prim57849 = alloca %struct.ScmObj*, align 8
%l47210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56608)
store volatile %struct.ScmObj* %l47210, %struct.ScmObj** %stackaddr$prim57849, align 8
%ae50969 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57850 = alloca %struct.ScmObj*, align 8
%anf_45bind47331 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %i47211, %struct.ScmObj* %ae50969)
store volatile %struct.ScmObj* %anf_45bind47331, %struct.ScmObj** %stackaddr$prim57850, align 8
%truthy$cmp57851 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47331)
%cmp$cmp57851 = icmp eq i64 %truthy$cmp57851, 1
br i1 %cmp$cmp57851, label %truebranch$cmp57851, label %falsebranch$cmp57851
truebranch$cmp57851:
%ae50972 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56610$k474440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57852 = alloca %struct.ScmObj*, align 8
%argslist56610$k474441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %l47210, %struct.ScmObj* %argslist56610$k474440)
store volatile %struct.ScmObj* %argslist56610$k474441, %struct.ScmObj** %stackaddr$prim57852, align 8
%stackaddr$prim57853 = alloca %struct.ScmObj*, align 8
%argslist56610$k474442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50972, %struct.ScmObj* %argslist56610$k474441)
store volatile %struct.ScmObj* %argslist56610$k474442, %struct.ScmObj** %stackaddr$prim57853, align 8
%clofunc57854 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47444)
musttail call tailcc void %clofunc57854(%struct.ScmObj* %k47444, %struct.ScmObj* %argslist56610$k474442)
ret void
falsebranch$cmp57851:
%ae50978 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57855 = alloca %struct.ScmObj*, align 8
%anf_45bind47332 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %loop47208, %struct.ScmObj* %ae50978)
store volatile %struct.ScmObj* %anf_45bind47332, %struct.ScmObj** %stackaddr$prim57855, align 8
%ae50980 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57856 = alloca %struct.ScmObj*, align 8
%anf_45bind47333 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %i47211, %struct.ScmObj* %ae50980)
store volatile %struct.ScmObj* %anf_45bind47333, %struct.ScmObj** %stackaddr$prim57856, align 8
%stackaddr$prim57857 = alloca %struct.ScmObj*, align 8
%anf_45bind47334 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %i47211, %struct.ScmObj* %l47210)
store volatile %struct.ScmObj* %anf_45bind47334, %struct.ScmObj** %stackaddr$prim57857, align 8
%argslist56611$anf_45bind473320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57858 = alloca %struct.ScmObj*, align 8
%argslist56611$anf_45bind473321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47334, %struct.ScmObj* %argslist56611$anf_45bind473320)
store volatile %struct.ScmObj* %argslist56611$anf_45bind473321, %struct.ScmObj** %stackaddr$prim57858, align 8
%stackaddr$prim57859 = alloca %struct.ScmObj*, align 8
%argslist56611$anf_45bind473322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47333, %struct.ScmObj* %argslist56611$anf_45bind473321)
store volatile %struct.ScmObj* %argslist56611$anf_45bind473322, %struct.ScmObj** %stackaddr$prim57859, align 8
%stackaddr$prim57860 = alloca %struct.ScmObj*, align 8
%argslist56611$anf_45bind473323 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47444, %struct.ScmObj* %argslist56611$anf_45bind473322)
store volatile %struct.ScmObj* %argslist56611$anf_45bind473323, %struct.ScmObj** %stackaddr$prim57860, align 8
%clofunc57861 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47332)
musttail call tailcc void %clofunc57861(%struct.ScmObj* %anf_45bind47332, %struct.ScmObj* %argslist56611$anf_45bind473323)
ret void
}

define tailcc void @proc_clo$ae50925(%struct.ScmObj* %env$ae50925,%struct.ScmObj* %current_45args56615) {
%stackaddr$prim57862 = alloca %struct.ScmObj*, align 8
%k47445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56615)
store volatile %struct.ScmObj* %k47445, %struct.ScmObj** %stackaddr$prim57862, align 8
%stackaddr$prim57863 = alloca %struct.ScmObj*, align 8
%current_45args56616 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56615)
store volatile %struct.ScmObj* %current_45args56616, %struct.ScmObj** %stackaddr$prim57863, align 8
%stackaddr$prim57864 = alloca %struct.ScmObj*, align 8
%x47131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56616)
store volatile %struct.ScmObj* %x47131, %struct.ScmObj** %stackaddr$prim57864, align 8
%stackaddr$prim57865 = alloca %struct.ScmObj*, align 8
%anf_45bind47323 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47131)
store volatile %struct.ScmObj* %anf_45bind47323, %struct.ScmObj** %stackaddr$prim57865, align 8
%stackaddr$prim57866 = alloca %struct.ScmObj*, align 8
%anf_45bind47324 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47323)
store volatile %struct.ScmObj* %anf_45bind47324, %struct.ScmObj** %stackaddr$prim57866, align 8
%stackaddr$prim57867 = alloca %struct.ScmObj*, align 8
%anf_45bind47325 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47324)
store volatile %struct.ScmObj* %anf_45bind47325, %struct.ScmObj** %stackaddr$prim57867, align 8
%stackaddr$prim57868 = alloca %struct.ScmObj*, align 8
%cpsprim47446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47325)
store volatile %struct.ScmObj* %cpsprim47446, %struct.ScmObj** %stackaddr$prim57868, align 8
%ae50931 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56618$k474450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57869 = alloca %struct.ScmObj*, align 8
%argslist56618$k474451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47446, %struct.ScmObj* %argslist56618$k474450)
store volatile %struct.ScmObj* %argslist56618$k474451, %struct.ScmObj** %stackaddr$prim57869, align 8
%stackaddr$prim57870 = alloca %struct.ScmObj*, align 8
%argslist56618$k474452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50931, %struct.ScmObj* %argslist56618$k474451)
store volatile %struct.ScmObj* %argslist56618$k474452, %struct.ScmObj** %stackaddr$prim57870, align 8
%clofunc57871 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47445)
musttail call tailcc void %clofunc57871(%struct.ScmObj* %k47445, %struct.ScmObj* %argslist56618$k474452)
ret void
}

define tailcc void @proc_clo$ae50901(%struct.ScmObj* %env$ae50901,%struct.ScmObj* %current_45args56620) {
%stackaddr$prim57872 = alloca %struct.ScmObj*, align 8
%k47447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56620)
store volatile %struct.ScmObj* %k47447, %struct.ScmObj** %stackaddr$prim57872, align 8
%stackaddr$prim57873 = alloca %struct.ScmObj*, align 8
%current_45args56621 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56620)
store volatile %struct.ScmObj* %current_45args56621, %struct.ScmObj** %stackaddr$prim57873, align 8
%stackaddr$prim57874 = alloca %struct.ScmObj*, align 8
%x47133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56621)
store volatile %struct.ScmObj* %x47133, %struct.ScmObj** %stackaddr$prim57874, align 8
%stackaddr$prim57875 = alloca %struct.ScmObj*, align 8
%anf_45bind47321 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47133)
store volatile %struct.ScmObj* %anf_45bind47321, %struct.ScmObj** %stackaddr$prim57875, align 8
%stackaddr$prim57876 = alloca %struct.ScmObj*, align 8
%anf_45bind47322 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47321)
store volatile %struct.ScmObj* %anf_45bind47322, %struct.ScmObj** %stackaddr$prim57876, align 8
%stackaddr$prim57877 = alloca %struct.ScmObj*, align 8
%cpsprim47448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47322)
store volatile %struct.ScmObj* %cpsprim47448, %struct.ScmObj** %stackaddr$prim57877, align 8
%ae50906 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56623$k474470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57878 = alloca %struct.ScmObj*, align 8
%argslist56623$k474471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47448, %struct.ScmObj* %argslist56623$k474470)
store volatile %struct.ScmObj* %argslist56623$k474471, %struct.ScmObj** %stackaddr$prim57878, align 8
%stackaddr$prim57879 = alloca %struct.ScmObj*, align 8
%argslist56623$k474472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50906, %struct.ScmObj* %argslist56623$k474471)
store volatile %struct.ScmObj* %argslist56623$k474472, %struct.ScmObj** %stackaddr$prim57879, align 8
%clofunc57880 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47447)
musttail call tailcc void %clofunc57880(%struct.ScmObj* %k47447, %struct.ScmObj* %argslist56623$k474472)
ret void
}

define tailcc void @proc_clo$ae50879(%struct.ScmObj* %env$ae50879,%struct.ScmObj* %current_45args56625) {
%stackaddr$prim57881 = alloca %struct.ScmObj*, align 8
%k47449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56625)
store volatile %struct.ScmObj* %k47449, %struct.ScmObj** %stackaddr$prim57881, align 8
%stackaddr$prim57882 = alloca %struct.ScmObj*, align 8
%current_45args56626 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56625)
store volatile %struct.ScmObj* %current_45args56626, %struct.ScmObj** %stackaddr$prim57882, align 8
%stackaddr$prim57883 = alloca %struct.ScmObj*, align 8
%x47135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56626)
store volatile %struct.ScmObj* %x47135, %struct.ScmObj** %stackaddr$prim57883, align 8
%stackaddr$prim57884 = alloca %struct.ScmObj*, align 8
%anf_45bind47320 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47135)
store volatile %struct.ScmObj* %anf_45bind47320, %struct.ScmObj** %stackaddr$prim57884, align 8
%stackaddr$prim57885 = alloca %struct.ScmObj*, align 8
%cpsprim47450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47320)
store volatile %struct.ScmObj* %cpsprim47450, %struct.ScmObj** %stackaddr$prim57885, align 8
%ae50883 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56628$k474490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57886 = alloca %struct.ScmObj*, align 8
%argslist56628$k474491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47450, %struct.ScmObj* %argslist56628$k474490)
store volatile %struct.ScmObj* %argslist56628$k474491, %struct.ScmObj** %stackaddr$prim57886, align 8
%stackaddr$prim57887 = alloca %struct.ScmObj*, align 8
%argslist56628$k474492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50883, %struct.ScmObj* %argslist56628$k474491)
store volatile %struct.ScmObj* %argslist56628$k474492, %struct.ScmObj** %stackaddr$prim57887, align 8
%clofunc57888 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47449)
musttail call tailcc void %clofunc57888(%struct.ScmObj* %k47449, %struct.ScmObj* %argslist56628$k474492)
ret void
}

define tailcc void @proc_clo$ae50859(%struct.ScmObj* %env$ae50859,%struct.ScmObj* %current_45args56630) {
%stackaddr$prim57889 = alloca %struct.ScmObj*, align 8
%k47451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56630)
store volatile %struct.ScmObj* %k47451, %struct.ScmObj** %stackaddr$prim57889, align 8
%stackaddr$prim57890 = alloca %struct.ScmObj*, align 8
%current_45args56631 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56630)
store volatile %struct.ScmObj* %current_45args56631, %struct.ScmObj** %stackaddr$prim57890, align 8
%stackaddr$prim57891 = alloca %struct.ScmObj*, align 8
%x47137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56631)
store volatile %struct.ScmObj* %x47137, %struct.ScmObj** %stackaddr$prim57891, align 8
%stackaddr$prim57892 = alloca %struct.ScmObj*, align 8
%cpsprim47452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47137)
store volatile %struct.ScmObj* %cpsprim47452, %struct.ScmObj** %stackaddr$prim57892, align 8
%ae50862 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56633$k474510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57893 = alloca %struct.ScmObj*, align 8
%argslist56633$k474511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47452, %struct.ScmObj* %argslist56633$k474510)
store volatile %struct.ScmObj* %argslist56633$k474511, %struct.ScmObj** %stackaddr$prim57893, align 8
%stackaddr$prim57894 = alloca %struct.ScmObj*, align 8
%argslist56633$k474512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50862, %struct.ScmObj* %argslist56633$k474511)
store volatile %struct.ScmObj* %argslist56633$k474512, %struct.ScmObj** %stackaddr$prim57894, align 8
%clofunc57895 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47451)
musttail call tailcc void %clofunc57895(%struct.ScmObj* %k47451, %struct.ScmObj* %argslist56633$k474512)
ret void
}

define tailcc void @proc_clo$ae50761(%struct.ScmObj* %env$ae50761,%struct.ScmObj* %args4713947453) {
%stackaddr$env-ref57896 = alloca %struct.ScmObj*, align 8
%_37foldl147078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50761, i64 0)
store %struct.ScmObj* %_37foldl147078, %struct.ScmObj** %stackaddr$env-ref57896
%stackaddr$prim57897 = alloca %struct.ScmObj*, align 8
%k47454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4713947453)
store volatile %struct.ScmObj* %k47454, %struct.ScmObj** %stackaddr$prim57897, align 8
%stackaddr$prim57898 = alloca %struct.ScmObj*, align 8
%args47139 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4713947453)
store volatile %struct.ScmObj* %args47139, %struct.ScmObj** %stackaddr$prim57898, align 8
%stackaddr$prim57899 = alloca %struct.ScmObj*, align 8
%anf_45bind47314 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args47139)
store volatile %struct.ScmObj* %anf_45bind47314, %struct.ScmObj** %stackaddr$prim57899, align 8
%truthy$cmp57900 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47314)
%cmp$cmp57900 = icmp eq i64 %truthy$cmp57900, 1
br i1 %cmp$cmp57900, label %truebranch$cmp57900, label %falsebranch$cmp57900
truebranch$cmp57900:
%ae50767 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50768 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist56635$k474540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57901 = alloca %struct.ScmObj*, align 8
%argslist56635$k474541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50768, %struct.ScmObj* %argslist56635$k474540)
store volatile %struct.ScmObj* %argslist56635$k474541, %struct.ScmObj** %stackaddr$prim57901, align 8
%stackaddr$prim57902 = alloca %struct.ScmObj*, align 8
%argslist56635$k474542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50767, %struct.ScmObj* %argslist56635$k474541)
store volatile %struct.ScmObj* %argslist56635$k474542, %struct.ScmObj** %stackaddr$prim57902, align 8
%clofunc57903 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47454)
musttail call tailcc void %clofunc57903(%struct.ScmObj* %k47454, %struct.ScmObj* %argslist56635$k474542)
ret void
falsebranch$cmp57900:
%stackaddr$prim57904 = alloca %struct.ScmObj*, align 8
%anf_45bind47315 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47139)
store volatile %struct.ScmObj* %anf_45bind47315, %struct.ScmObj** %stackaddr$prim57904, align 8
%stackaddr$prim57905 = alloca %struct.ScmObj*, align 8
%anf_45bind47316 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47315)
store volatile %struct.ScmObj* %anf_45bind47316, %struct.ScmObj** %stackaddr$prim57905, align 8
%truthy$cmp57906 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47316)
%cmp$cmp57906 = icmp eq i64 %truthy$cmp57906, 1
br i1 %cmp$cmp57906, label %truebranch$cmp57906, label %falsebranch$cmp57906
truebranch$cmp57906:
%stackaddr$prim57907 = alloca %struct.ScmObj*, align 8
%cpsprim47455 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47139)
store volatile %struct.ScmObj* %cpsprim47455, %struct.ScmObj** %stackaddr$prim57907, align 8
%ae50780 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56636$k474540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57908 = alloca %struct.ScmObj*, align 8
%argslist56636$k474541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47455, %struct.ScmObj* %argslist56636$k474540)
store volatile %struct.ScmObj* %argslist56636$k474541, %struct.ScmObj** %stackaddr$prim57908, align 8
%stackaddr$prim57909 = alloca %struct.ScmObj*, align 8
%argslist56636$k474542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50780, %struct.ScmObj* %argslist56636$k474541)
store volatile %struct.ScmObj* %argslist56636$k474542, %struct.ScmObj** %stackaddr$prim57909, align 8
%clofunc57910 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47454)
musttail call tailcc void %clofunc57910(%struct.ScmObj* %k47454, %struct.ScmObj* %argslist56636$k474542)
ret void
falsebranch$cmp57906:
%stackaddr$makeclosure57911 = alloca %struct.ScmObj*, align 8
%fptrToInt57912 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50785 to i64
%ae50785 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57912)
store volatile %struct.ScmObj* %ae50785, %struct.ScmObj** %stackaddr$makeclosure57911, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50785, %struct.ScmObj* %_37foldl147078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50785, %struct.ScmObj* %args47139, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50785, %struct.ScmObj* %k47454, i64 2)
%ae50786 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57913 = alloca %struct.ScmObj*, align 8
%fptrToInt57914 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50787 to i64
%ae50787 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57914)
store volatile %struct.ScmObj* %ae50787, %struct.ScmObj** %stackaddr$makeclosure57913, align 8
%argslist56646$ae507850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57915 = alloca %struct.ScmObj*, align 8
%argslist56646$ae507851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50787, %struct.ScmObj* %argslist56646$ae507850)
store volatile %struct.ScmObj* %argslist56646$ae507851, %struct.ScmObj** %stackaddr$prim57915, align 8
%stackaddr$prim57916 = alloca %struct.ScmObj*, align 8
%argslist56646$ae507852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50786, %struct.ScmObj* %argslist56646$ae507851)
store volatile %struct.ScmObj* %argslist56646$ae507852, %struct.ScmObj** %stackaddr$prim57916, align 8
%clofunc57917 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50785)
musttail call tailcc void %clofunc57917(%struct.ScmObj* %ae50785, %struct.ScmObj* %argslist56646$ae507852)
ret void
}

define tailcc void @proc_clo$ae50785(%struct.ScmObj* %env$ae50785,%struct.ScmObj* %current_45args56637) {
%stackaddr$env-ref57918 = alloca %struct.ScmObj*, align 8
%_37foldl147078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50785, i64 0)
store %struct.ScmObj* %_37foldl147078, %struct.ScmObj** %stackaddr$env-ref57918
%stackaddr$env-ref57919 = alloca %struct.ScmObj*, align 8
%args47139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50785, i64 1)
store %struct.ScmObj* %args47139, %struct.ScmObj** %stackaddr$env-ref57919
%stackaddr$env-ref57920 = alloca %struct.ScmObj*, align 8
%k47454 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50785, i64 2)
store %struct.ScmObj* %k47454, %struct.ScmObj** %stackaddr$env-ref57920
%stackaddr$prim57921 = alloca %struct.ScmObj*, align 8
%_95k47456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56637)
store volatile %struct.ScmObj* %_95k47456, %struct.ScmObj** %stackaddr$prim57921, align 8
%stackaddr$prim57922 = alloca %struct.ScmObj*, align 8
%current_45args56638 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56637)
store volatile %struct.ScmObj* %current_45args56638, %struct.ScmObj** %stackaddr$prim57922, align 8
%stackaddr$prim57923 = alloca %struct.ScmObj*, align 8
%anf_45bind47317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56638)
store volatile %struct.ScmObj* %anf_45bind47317, %struct.ScmObj** %stackaddr$prim57923, align 8
%stackaddr$prim57924 = alloca %struct.ScmObj*, align 8
%anf_45bind47318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47139)
store volatile %struct.ScmObj* %anf_45bind47318, %struct.ScmObj** %stackaddr$prim57924, align 8
%stackaddr$prim57925 = alloca %struct.ScmObj*, align 8
%anf_45bind47319 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47139)
store volatile %struct.ScmObj* %anf_45bind47319, %struct.ScmObj** %stackaddr$prim57925, align 8
%argslist56640$_37foldl1470780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57926 = alloca %struct.ScmObj*, align 8
%argslist56640$_37foldl1470781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47319, %struct.ScmObj* %argslist56640$_37foldl1470780)
store volatile %struct.ScmObj* %argslist56640$_37foldl1470781, %struct.ScmObj** %stackaddr$prim57926, align 8
%stackaddr$prim57927 = alloca %struct.ScmObj*, align 8
%argslist56640$_37foldl1470782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47318, %struct.ScmObj* %argslist56640$_37foldl1470781)
store volatile %struct.ScmObj* %argslist56640$_37foldl1470782, %struct.ScmObj** %stackaddr$prim57927, align 8
%stackaddr$prim57928 = alloca %struct.ScmObj*, align 8
%argslist56640$_37foldl1470783 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47317, %struct.ScmObj* %argslist56640$_37foldl1470782)
store volatile %struct.ScmObj* %argslist56640$_37foldl1470783, %struct.ScmObj** %stackaddr$prim57928, align 8
%stackaddr$prim57929 = alloca %struct.ScmObj*, align 8
%argslist56640$_37foldl1470784 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47454, %struct.ScmObj* %argslist56640$_37foldl1470783)
store volatile %struct.ScmObj* %argslist56640$_37foldl1470784, %struct.ScmObj** %stackaddr$prim57929, align 8
%clofunc57930 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147078)
musttail call tailcc void %clofunc57930(%struct.ScmObj* %_37foldl147078, %struct.ScmObj* %argslist56640$_37foldl1470784)
ret void
}

define tailcc void @proc_clo$ae50787(%struct.ScmObj* %env$ae50787,%struct.ScmObj* %current_45args56641) {
%stackaddr$prim57931 = alloca %struct.ScmObj*, align 8
%k47457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56641)
store volatile %struct.ScmObj* %k47457, %struct.ScmObj** %stackaddr$prim57931, align 8
%stackaddr$prim57932 = alloca %struct.ScmObj*, align 8
%current_45args56642 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56641)
store volatile %struct.ScmObj* %current_45args56642, %struct.ScmObj** %stackaddr$prim57932, align 8
%stackaddr$prim57933 = alloca %struct.ScmObj*, align 8
%n47141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56642)
store volatile %struct.ScmObj* %n47141, %struct.ScmObj** %stackaddr$prim57933, align 8
%stackaddr$prim57934 = alloca %struct.ScmObj*, align 8
%current_45args56643 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56642)
store volatile %struct.ScmObj* %current_45args56643, %struct.ScmObj** %stackaddr$prim57934, align 8
%stackaddr$prim57935 = alloca %struct.ScmObj*, align 8
%v47140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56643)
store volatile %struct.ScmObj* %v47140, %struct.ScmObj** %stackaddr$prim57935, align 8
%stackaddr$prim57936 = alloca %struct.ScmObj*, align 8
%cpsprim47458 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v47140, %struct.ScmObj* %n47141)
store volatile %struct.ScmObj* %cpsprim47458, %struct.ScmObj** %stackaddr$prim57936, align 8
%ae50791 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56645$k474570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57937 = alloca %struct.ScmObj*, align 8
%argslist56645$k474571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47458, %struct.ScmObj* %argslist56645$k474570)
store volatile %struct.ScmObj* %argslist56645$k474571, %struct.ScmObj** %stackaddr$prim57937, align 8
%stackaddr$prim57938 = alloca %struct.ScmObj*, align 8
%argslist56645$k474572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50791, %struct.ScmObj* %argslist56645$k474571)
store volatile %struct.ScmObj* %argslist56645$k474572, %struct.ScmObj** %stackaddr$prim57938, align 8
%clofunc57939 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47457)
musttail call tailcc void %clofunc57939(%struct.ScmObj* %k47457, %struct.ScmObj* %argslist56645$k474572)
ret void
}

define tailcc void @proc_clo$ae50357(%struct.ScmObj* %env$ae50357,%struct.ScmObj* %current_45args56648) {
%stackaddr$prim57940 = alloca %struct.ScmObj*, align 8
%k47459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56648)
store volatile %struct.ScmObj* %k47459, %struct.ScmObj** %stackaddr$prim57940, align 8
%stackaddr$prim57941 = alloca %struct.ScmObj*, align 8
%current_45args56649 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56648)
store volatile %struct.ScmObj* %current_45args56649, %struct.ScmObj** %stackaddr$prim57941, align 8
%stackaddr$prim57942 = alloca %struct.ScmObj*, align 8
%v47144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56649)
store volatile %struct.ScmObj* %v47144, %struct.ScmObj** %stackaddr$prim57942, align 8
%stackaddr$prim57943 = alloca %struct.ScmObj*, align 8
%current_45args56650 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56649)
store volatile %struct.ScmObj* %current_45args56650, %struct.ScmObj** %stackaddr$prim57943, align 8
%stackaddr$prim57944 = alloca %struct.ScmObj*, align 8
%lst47143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56650)
store volatile %struct.ScmObj* %lst47143, %struct.ScmObj** %stackaddr$prim57944, align 8
%ae50358 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57945 = alloca %struct.ScmObj*, align 8
%lst47145 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50358, %struct.ScmObj* %lst47143)
store volatile %struct.ScmObj* %lst47145, %struct.ScmObj** %stackaddr$prim57945, align 8
%stackaddr$makeclosure57946 = alloca %struct.ScmObj*, align 8
%fptrToInt57947 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50360 to i64
%ae50360 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57947)
store volatile %struct.ScmObj* %ae50360, %struct.ScmObj** %stackaddr$makeclosure57946, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50360, %struct.ScmObj* %k47459, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50360, %struct.ScmObj* %lst47145, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50360, %struct.ScmObj* %v47144, i64 2)
%ae50361 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57948 = alloca %struct.ScmObj*, align 8
%fptrToInt57949 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50362 to i64
%ae50362 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57949)
store volatile %struct.ScmObj* %ae50362, %struct.ScmObj** %stackaddr$makeclosure57948, align 8
%argslist56672$ae503600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57950 = alloca %struct.ScmObj*, align 8
%argslist56672$ae503601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50362, %struct.ScmObj* %argslist56672$ae503600)
store volatile %struct.ScmObj* %argslist56672$ae503601, %struct.ScmObj** %stackaddr$prim57950, align 8
%stackaddr$prim57951 = alloca %struct.ScmObj*, align 8
%argslist56672$ae503602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50361, %struct.ScmObj* %argslist56672$ae503601)
store volatile %struct.ScmObj* %argslist56672$ae503602, %struct.ScmObj** %stackaddr$prim57951, align 8
%clofunc57952 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50360)
musttail call tailcc void %clofunc57952(%struct.ScmObj* %ae50360, %struct.ScmObj* %argslist56672$ae503602)
ret void
}

define tailcc void @proc_clo$ae50360(%struct.ScmObj* %env$ae50360,%struct.ScmObj* %current_45args56652) {
%stackaddr$env-ref57953 = alloca %struct.ScmObj*, align 8
%k47459 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50360, i64 0)
store %struct.ScmObj* %k47459, %struct.ScmObj** %stackaddr$env-ref57953
%stackaddr$env-ref57954 = alloca %struct.ScmObj*, align 8
%lst47145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50360, i64 1)
store %struct.ScmObj* %lst47145, %struct.ScmObj** %stackaddr$env-ref57954
%stackaddr$env-ref57955 = alloca %struct.ScmObj*, align 8
%v47144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50360, i64 2)
store %struct.ScmObj* %v47144, %struct.ScmObj** %stackaddr$env-ref57955
%stackaddr$prim57956 = alloca %struct.ScmObj*, align 8
%_95k47460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56652)
store volatile %struct.ScmObj* %_95k47460, %struct.ScmObj** %stackaddr$prim57956, align 8
%stackaddr$prim57957 = alloca %struct.ScmObj*, align 8
%current_45args56653 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56652)
store volatile %struct.ScmObj* %current_45args56653, %struct.ScmObj** %stackaddr$prim57957, align 8
%stackaddr$prim57958 = alloca %struct.ScmObj*, align 8
%anf_45bind47306 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56653)
store volatile %struct.ScmObj* %anf_45bind47306, %struct.ScmObj** %stackaddr$prim57958, align 8
%stackaddr$makeclosure57959 = alloca %struct.ScmObj*, align 8
%fptrToInt57960 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50376 to i64
%ae50376 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57960)
store volatile %struct.ScmObj* %ae50376, %struct.ScmObj** %stackaddr$makeclosure57959, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50376, %struct.ScmObj* %k47459, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50376, %struct.ScmObj* %lst47145, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50376, %struct.ScmObj* %v47144, i64 2)
%stackaddr$makeclosure57961 = alloca %struct.ScmObj*, align 8
%fptrToInt57962 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50377 to i64
%ae50377 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57962)
store volatile %struct.ScmObj* %ae50377, %struct.ScmObj** %stackaddr$makeclosure57961, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50377, %struct.ScmObj* %k47459, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50377, %struct.ScmObj* %lst47145, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50377, %struct.ScmObj* %v47144, i64 2)
%argslist56667$anf_45bind473060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57963 = alloca %struct.ScmObj*, align 8
%argslist56667$anf_45bind473061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50377, %struct.ScmObj* %argslist56667$anf_45bind473060)
store volatile %struct.ScmObj* %argslist56667$anf_45bind473061, %struct.ScmObj** %stackaddr$prim57963, align 8
%stackaddr$prim57964 = alloca %struct.ScmObj*, align 8
%argslist56667$anf_45bind473062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50376, %struct.ScmObj* %argslist56667$anf_45bind473061)
store volatile %struct.ScmObj* %argslist56667$anf_45bind473062, %struct.ScmObj** %stackaddr$prim57964, align 8
%clofunc57965 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47306)
musttail call tailcc void %clofunc57965(%struct.ScmObj* %anf_45bind47306, %struct.ScmObj* %argslist56667$anf_45bind473062)
ret void
}

define tailcc void @proc_clo$ae50376(%struct.ScmObj* %env$ae50376,%struct.ScmObj* %current_45args56655) {
%stackaddr$env-ref57966 = alloca %struct.ScmObj*, align 8
%k47459 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50376, i64 0)
store %struct.ScmObj* %k47459, %struct.ScmObj** %stackaddr$env-ref57966
%stackaddr$env-ref57967 = alloca %struct.ScmObj*, align 8
%lst47145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50376, i64 1)
store %struct.ScmObj* %lst47145, %struct.ScmObj** %stackaddr$env-ref57967
%stackaddr$env-ref57968 = alloca %struct.ScmObj*, align 8
%v47144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50376, i64 2)
store %struct.ScmObj* %v47144, %struct.ScmObj** %stackaddr$env-ref57968
%stackaddr$prim57969 = alloca %struct.ScmObj*, align 8
%_95k47461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56655)
store volatile %struct.ScmObj* %_95k47461, %struct.ScmObj** %stackaddr$prim57969, align 8
%stackaddr$prim57970 = alloca %struct.ScmObj*, align 8
%current_45args56656 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56655)
store volatile %struct.ScmObj* %current_45args56656, %struct.ScmObj** %stackaddr$prim57970, align 8
%stackaddr$prim57971 = alloca %struct.ScmObj*, align 8
%cc47146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56656)
store volatile %struct.ScmObj* %cc47146, %struct.ScmObj** %stackaddr$prim57971, align 8
%ae50485 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57972 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47145, %struct.ScmObj* %ae50485)
store volatile %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$prim57972, align 8
%stackaddr$prim57973 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47307)
store volatile %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$prim57973, align 8
%truthy$cmp57974 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47308)
%cmp$cmp57974 = icmp eq i64 %truthy$cmp57974, 1
br i1 %cmp$cmp57974, label %truebranch$cmp57974, label %falsebranch$cmp57974
truebranch$cmp57974:
%ae50489 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50490 = call %struct.ScmObj* @const_init_false()
%argslist56658$k474590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57975 = alloca %struct.ScmObj*, align 8
%argslist56658$k474591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50490, %struct.ScmObj* %argslist56658$k474590)
store volatile %struct.ScmObj* %argslist56658$k474591, %struct.ScmObj** %stackaddr$prim57975, align 8
%stackaddr$prim57976 = alloca %struct.ScmObj*, align 8
%argslist56658$k474592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50489, %struct.ScmObj* %argslist56658$k474591)
store volatile %struct.ScmObj* %argslist56658$k474592, %struct.ScmObj** %stackaddr$prim57976, align 8
%clofunc57977 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47459)
musttail call tailcc void %clofunc57977(%struct.ScmObj* %k47459, %struct.ScmObj* %argslist56658$k474592)
ret void
falsebranch$cmp57974:
%ae50498 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57978 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47145, %struct.ScmObj* %ae50498)
store volatile %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$prim57978, align 8
%stackaddr$prim57979 = alloca %struct.ScmObj*, align 8
%anf_45bind47310 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47309)
store volatile %struct.ScmObj* %anf_45bind47310, %struct.ScmObj** %stackaddr$prim57979, align 8
%stackaddr$prim57980 = alloca %struct.ScmObj*, align 8
%anf_45bind47311 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47310, %struct.ScmObj* %v47144)
store volatile %struct.ScmObj* %anf_45bind47311, %struct.ScmObj** %stackaddr$prim57980, align 8
%truthy$cmp57981 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47311)
%cmp$cmp57981 = icmp eq i64 %truthy$cmp57981, 1
br i1 %cmp$cmp57981, label %truebranch$cmp57981, label %falsebranch$cmp57981
truebranch$cmp57981:
%ae50504 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57982 = alloca %struct.ScmObj*, align 8
%cpsprim47462 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47145, %struct.ScmObj* %ae50504)
store volatile %struct.ScmObj* %cpsprim47462, %struct.ScmObj** %stackaddr$prim57982, align 8
%ae50506 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56659$k474590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57983 = alloca %struct.ScmObj*, align 8
%argslist56659$k474591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47462, %struct.ScmObj* %argslist56659$k474590)
store volatile %struct.ScmObj* %argslist56659$k474591, %struct.ScmObj** %stackaddr$prim57983, align 8
%stackaddr$prim57984 = alloca %struct.ScmObj*, align 8
%argslist56659$k474592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50506, %struct.ScmObj* %argslist56659$k474591)
store volatile %struct.ScmObj* %argslist56659$k474592, %struct.ScmObj** %stackaddr$prim57984, align 8
%clofunc57985 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47459)
musttail call tailcc void %clofunc57985(%struct.ScmObj* %k47459, %struct.ScmObj* %argslist56659$k474592)
ret void
falsebranch$cmp57981:
%ae50517 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57986 = alloca %struct.ScmObj*, align 8
%anf_45bind47312 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47145, %struct.ScmObj* %ae50517)
store volatile %struct.ScmObj* %anf_45bind47312, %struct.ScmObj** %stackaddr$prim57986, align 8
%stackaddr$prim57987 = alloca %struct.ScmObj*, align 8
%anf_45bind47313 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47312)
store volatile %struct.ScmObj* %anf_45bind47313, %struct.ScmObj** %stackaddr$prim57987, align 8
%ae50520 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57988 = alloca %struct.ScmObj*, align 8
%_95047148 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47145, %struct.ScmObj* %ae50520, %struct.ScmObj* %anf_45bind47313)
store volatile %struct.ScmObj* %_95047148, %struct.ScmObj** %stackaddr$prim57988, align 8
%argslist56660$cc471460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57989 = alloca %struct.ScmObj*, align 8
%argslist56660$cc471461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47146, %struct.ScmObj* %argslist56660$cc471460)
store volatile %struct.ScmObj* %argslist56660$cc471461, %struct.ScmObj** %stackaddr$prim57989, align 8
%stackaddr$prim57990 = alloca %struct.ScmObj*, align 8
%argslist56660$cc471462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47459, %struct.ScmObj* %argslist56660$cc471461)
store volatile %struct.ScmObj* %argslist56660$cc471462, %struct.ScmObj** %stackaddr$prim57990, align 8
%clofunc57991 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47146)
musttail call tailcc void %clofunc57991(%struct.ScmObj* %cc47146, %struct.ScmObj* %argslist56660$cc471462)
ret void
}

define tailcc void @proc_clo$ae50377(%struct.ScmObj* %env$ae50377,%struct.ScmObj* %current_45args56661) {
%stackaddr$env-ref57992 = alloca %struct.ScmObj*, align 8
%k47459 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50377, i64 0)
store %struct.ScmObj* %k47459, %struct.ScmObj** %stackaddr$env-ref57992
%stackaddr$env-ref57993 = alloca %struct.ScmObj*, align 8
%lst47145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50377, i64 1)
store %struct.ScmObj* %lst47145, %struct.ScmObj** %stackaddr$env-ref57993
%stackaddr$env-ref57994 = alloca %struct.ScmObj*, align 8
%v47144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50377, i64 2)
store %struct.ScmObj* %v47144, %struct.ScmObj** %stackaddr$env-ref57994
%stackaddr$prim57995 = alloca %struct.ScmObj*, align 8
%_95k47461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56661)
store volatile %struct.ScmObj* %_95k47461, %struct.ScmObj** %stackaddr$prim57995, align 8
%stackaddr$prim57996 = alloca %struct.ScmObj*, align 8
%current_45args56662 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56661)
store volatile %struct.ScmObj* %current_45args56662, %struct.ScmObj** %stackaddr$prim57996, align 8
%stackaddr$prim57997 = alloca %struct.ScmObj*, align 8
%cc47146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56662)
store volatile %struct.ScmObj* %cc47146, %struct.ScmObj** %stackaddr$prim57997, align 8
%ae50379 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57998 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47145, %struct.ScmObj* %ae50379)
store volatile %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$prim57998, align 8
%stackaddr$prim57999 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47307)
store volatile %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$prim57999, align 8
%truthy$cmp58000 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47308)
%cmp$cmp58000 = icmp eq i64 %truthy$cmp58000, 1
br i1 %cmp$cmp58000, label %truebranch$cmp58000, label %falsebranch$cmp58000
truebranch$cmp58000:
%ae50383 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50384 = call %struct.ScmObj* @const_init_false()
%argslist56664$k474590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58001 = alloca %struct.ScmObj*, align 8
%argslist56664$k474591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50384, %struct.ScmObj* %argslist56664$k474590)
store volatile %struct.ScmObj* %argslist56664$k474591, %struct.ScmObj** %stackaddr$prim58001, align 8
%stackaddr$prim58002 = alloca %struct.ScmObj*, align 8
%argslist56664$k474592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50383, %struct.ScmObj* %argslist56664$k474591)
store volatile %struct.ScmObj* %argslist56664$k474592, %struct.ScmObj** %stackaddr$prim58002, align 8
%clofunc58003 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47459)
musttail call tailcc void %clofunc58003(%struct.ScmObj* %k47459, %struct.ScmObj* %argslist56664$k474592)
ret void
falsebranch$cmp58000:
%ae50392 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58004 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47145, %struct.ScmObj* %ae50392)
store volatile %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$prim58004, align 8
%stackaddr$prim58005 = alloca %struct.ScmObj*, align 8
%anf_45bind47310 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47309)
store volatile %struct.ScmObj* %anf_45bind47310, %struct.ScmObj** %stackaddr$prim58005, align 8
%stackaddr$prim58006 = alloca %struct.ScmObj*, align 8
%anf_45bind47311 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47310, %struct.ScmObj* %v47144)
store volatile %struct.ScmObj* %anf_45bind47311, %struct.ScmObj** %stackaddr$prim58006, align 8
%truthy$cmp58007 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47311)
%cmp$cmp58007 = icmp eq i64 %truthy$cmp58007, 1
br i1 %cmp$cmp58007, label %truebranch$cmp58007, label %falsebranch$cmp58007
truebranch$cmp58007:
%ae50398 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58008 = alloca %struct.ScmObj*, align 8
%cpsprim47462 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47145, %struct.ScmObj* %ae50398)
store volatile %struct.ScmObj* %cpsprim47462, %struct.ScmObj** %stackaddr$prim58008, align 8
%ae50400 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56665$k474590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58009 = alloca %struct.ScmObj*, align 8
%argslist56665$k474591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47462, %struct.ScmObj* %argslist56665$k474590)
store volatile %struct.ScmObj* %argslist56665$k474591, %struct.ScmObj** %stackaddr$prim58009, align 8
%stackaddr$prim58010 = alloca %struct.ScmObj*, align 8
%argslist56665$k474592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50400, %struct.ScmObj* %argslist56665$k474591)
store volatile %struct.ScmObj* %argslist56665$k474592, %struct.ScmObj** %stackaddr$prim58010, align 8
%clofunc58011 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47459)
musttail call tailcc void %clofunc58011(%struct.ScmObj* %k47459, %struct.ScmObj* %argslist56665$k474592)
ret void
falsebranch$cmp58007:
%ae50411 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58012 = alloca %struct.ScmObj*, align 8
%anf_45bind47312 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47145, %struct.ScmObj* %ae50411)
store volatile %struct.ScmObj* %anf_45bind47312, %struct.ScmObj** %stackaddr$prim58012, align 8
%stackaddr$prim58013 = alloca %struct.ScmObj*, align 8
%anf_45bind47313 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47312)
store volatile %struct.ScmObj* %anf_45bind47313, %struct.ScmObj** %stackaddr$prim58013, align 8
%ae50414 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58014 = alloca %struct.ScmObj*, align 8
%_95047148 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47145, %struct.ScmObj* %ae50414, %struct.ScmObj* %anf_45bind47313)
store volatile %struct.ScmObj* %_95047148, %struct.ScmObj** %stackaddr$prim58014, align 8
%argslist56666$cc471460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58015 = alloca %struct.ScmObj*, align 8
%argslist56666$cc471461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47146, %struct.ScmObj* %argslist56666$cc471460)
store volatile %struct.ScmObj* %argslist56666$cc471461, %struct.ScmObj** %stackaddr$prim58015, align 8
%stackaddr$prim58016 = alloca %struct.ScmObj*, align 8
%argslist56666$cc471462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47459, %struct.ScmObj* %argslist56666$cc471461)
store volatile %struct.ScmObj* %argslist56666$cc471462, %struct.ScmObj** %stackaddr$prim58016, align 8
%clofunc58017 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47146)
musttail call tailcc void %clofunc58017(%struct.ScmObj* %cc47146, %struct.ScmObj* %argslist56666$cc471462)
ret void
}

define tailcc void @proc_clo$ae50362(%struct.ScmObj* %env$ae50362,%struct.ScmObj* %current_45args56668) {
%stackaddr$prim58018 = alloca %struct.ScmObj*, align 8
%k47463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56668)
store volatile %struct.ScmObj* %k47463, %struct.ScmObj** %stackaddr$prim58018, align 8
%stackaddr$prim58019 = alloca %struct.ScmObj*, align 8
%current_45args56669 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56668)
store volatile %struct.ScmObj* %current_45args56669, %struct.ScmObj** %stackaddr$prim58019, align 8
%stackaddr$prim58020 = alloca %struct.ScmObj*, align 8
%u47147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56669)
store volatile %struct.ScmObj* %u47147, %struct.ScmObj** %stackaddr$prim58020, align 8
%argslist56671$u471470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58021 = alloca %struct.ScmObj*, align 8
%argslist56671$u471471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47147, %struct.ScmObj* %argslist56671$u471470)
store volatile %struct.ScmObj* %argslist56671$u471471, %struct.ScmObj** %stackaddr$prim58021, align 8
%stackaddr$prim58022 = alloca %struct.ScmObj*, align 8
%argslist56671$u471472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47463, %struct.ScmObj* %argslist56671$u471471)
store volatile %struct.ScmObj* %argslist56671$u471472, %struct.ScmObj** %stackaddr$prim58022, align 8
%clofunc58023 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47147)
musttail call tailcc void %clofunc58023(%struct.ScmObj* %u47147, %struct.ScmObj* %argslist56671$u471472)
ret void
}

define tailcc void @proc_clo$ae49821(%struct.ScmObj* %env$ae49821,%struct.ScmObj* %current_45args56674) {
%stackaddr$prim58024 = alloca %struct.ScmObj*, align 8
%k47464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56674)
store volatile %struct.ScmObj* %k47464, %struct.ScmObj** %stackaddr$prim58024, align 8
%stackaddr$prim58025 = alloca %struct.ScmObj*, align 8
%current_45args56675 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56674)
store volatile %struct.ScmObj* %current_45args56675, %struct.ScmObj** %stackaddr$prim58025, align 8
%stackaddr$prim58026 = alloca %struct.ScmObj*, align 8
%lst47151 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56675)
store volatile %struct.ScmObj* %lst47151, %struct.ScmObj** %stackaddr$prim58026, align 8
%stackaddr$prim58027 = alloca %struct.ScmObj*, align 8
%current_45args56676 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56675)
store volatile %struct.ScmObj* %current_45args56676, %struct.ScmObj** %stackaddr$prim58027, align 8
%stackaddr$prim58028 = alloca %struct.ScmObj*, align 8
%n47150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56676)
store volatile %struct.ScmObj* %n47150, %struct.ScmObj** %stackaddr$prim58028, align 8
%ae49822 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58029 = alloca %struct.ScmObj*, align 8
%n47153 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49822, %struct.ScmObj* %n47150)
store volatile %struct.ScmObj* %n47153, %struct.ScmObj** %stackaddr$prim58029, align 8
%ae49824 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58030 = alloca %struct.ScmObj*, align 8
%lst47152 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49824, %struct.ScmObj* %lst47151)
store volatile %struct.ScmObj* %lst47152, %struct.ScmObj** %stackaddr$prim58030, align 8
%stackaddr$makeclosure58031 = alloca %struct.ScmObj*, align 8
%fptrToInt58032 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49826 to i64
%ae49826 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58032)
store volatile %struct.ScmObj* %ae49826, %struct.ScmObj** %stackaddr$makeclosure58031, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49826, %struct.ScmObj* %n47153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49826, %struct.ScmObj* %lst47152, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49826, %struct.ScmObj* %k47464, i64 2)
%ae49827 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58033 = alloca %struct.ScmObj*, align 8
%fptrToInt58034 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49828 to i64
%ae49828 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58034)
store volatile %struct.ScmObj* %ae49828, %struct.ScmObj** %stackaddr$makeclosure58033, align 8
%argslist56696$ae498260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58035 = alloca %struct.ScmObj*, align 8
%argslist56696$ae498261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49828, %struct.ScmObj* %argslist56696$ae498260)
store volatile %struct.ScmObj* %argslist56696$ae498261, %struct.ScmObj** %stackaddr$prim58035, align 8
%stackaddr$prim58036 = alloca %struct.ScmObj*, align 8
%argslist56696$ae498262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49827, %struct.ScmObj* %argslist56696$ae498261)
store volatile %struct.ScmObj* %argslist56696$ae498262, %struct.ScmObj** %stackaddr$prim58036, align 8
%clofunc58037 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49826)
musttail call tailcc void %clofunc58037(%struct.ScmObj* %ae49826, %struct.ScmObj* %argslist56696$ae498262)
ret void
}

define tailcc void @proc_clo$ae49826(%struct.ScmObj* %env$ae49826,%struct.ScmObj* %current_45args56678) {
%stackaddr$env-ref58038 = alloca %struct.ScmObj*, align 8
%n47153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49826, i64 0)
store %struct.ScmObj* %n47153, %struct.ScmObj** %stackaddr$env-ref58038
%stackaddr$env-ref58039 = alloca %struct.ScmObj*, align 8
%lst47152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49826, i64 1)
store %struct.ScmObj* %lst47152, %struct.ScmObj** %stackaddr$env-ref58039
%stackaddr$env-ref58040 = alloca %struct.ScmObj*, align 8
%k47464 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49826, i64 2)
store %struct.ScmObj* %k47464, %struct.ScmObj** %stackaddr$env-ref58040
%stackaddr$prim58041 = alloca %struct.ScmObj*, align 8
%_95k47465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56678)
store volatile %struct.ScmObj* %_95k47465, %struct.ScmObj** %stackaddr$prim58041, align 8
%stackaddr$prim58042 = alloca %struct.ScmObj*, align 8
%current_45args56679 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56678)
store volatile %struct.ScmObj* %current_45args56679, %struct.ScmObj** %stackaddr$prim58042, align 8
%stackaddr$prim58043 = alloca %struct.ScmObj*, align 8
%anf_45bind47299 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56679)
store volatile %struct.ScmObj* %anf_45bind47299, %struct.ScmObj** %stackaddr$prim58043, align 8
%stackaddr$makeclosure58044 = alloca %struct.ScmObj*, align 8
%fptrToInt58045 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49842 to i64
%ae49842 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58045)
store volatile %struct.ScmObj* %ae49842, %struct.ScmObj** %stackaddr$makeclosure58044, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49842, %struct.ScmObj* %n47153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49842, %struct.ScmObj* %lst47152, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49842, %struct.ScmObj* %k47464, i64 2)
%stackaddr$makeclosure58046 = alloca %struct.ScmObj*, align 8
%fptrToInt58047 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49843 to i64
%ae49843 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58047)
store volatile %struct.ScmObj* %ae49843, %struct.ScmObj** %stackaddr$makeclosure58046, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49843, %struct.ScmObj* %n47153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49843, %struct.ScmObj* %lst47152, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49843, %struct.ScmObj* %k47464, i64 2)
%argslist56691$anf_45bind472990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58048 = alloca %struct.ScmObj*, align 8
%argslist56691$anf_45bind472991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49843, %struct.ScmObj* %argslist56691$anf_45bind472990)
store volatile %struct.ScmObj* %argslist56691$anf_45bind472991, %struct.ScmObj** %stackaddr$prim58048, align 8
%stackaddr$prim58049 = alloca %struct.ScmObj*, align 8
%argslist56691$anf_45bind472992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49842, %struct.ScmObj* %argslist56691$anf_45bind472991)
store volatile %struct.ScmObj* %argslist56691$anf_45bind472992, %struct.ScmObj** %stackaddr$prim58049, align 8
%clofunc58050 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47299)
musttail call tailcc void %clofunc58050(%struct.ScmObj* %anf_45bind47299, %struct.ScmObj* %argslist56691$anf_45bind472992)
ret void
}

define tailcc void @proc_clo$ae49842(%struct.ScmObj* %env$ae49842,%struct.ScmObj* %current_45args56681) {
%stackaddr$env-ref58051 = alloca %struct.ScmObj*, align 8
%n47153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49842, i64 0)
store %struct.ScmObj* %n47153, %struct.ScmObj** %stackaddr$env-ref58051
%stackaddr$env-ref58052 = alloca %struct.ScmObj*, align 8
%lst47152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49842, i64 1)
store %struct.ScmObj* %lst47152, %struct.ScmObj** %stackaddr$env-ref58052
%stackaddr$env-ref58053 = alloca %struct.ScmObj*, align 8
%k47464 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49842, i64 2)
store %struct.ScmObj* %k47464, %struct.ScmObj** %stackaddr$env-ref58053
%stackaddr$prim58054 = alloca %struct.ScmObj*, align 8
%_95k47466 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56681)
store volatile %struct.ScmObj* %_95k47466, %struct.ScmObj** %stackaddr$prim58054, align 8
%stackaddr$prim58055 = alloca %struct.ScmObj*, align 8
%current_45args56682 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56681)
store volatile %struct.ScmObj* %current_45args56682, %struct.ScmObj** %stackaddr$prim58055, align 8
%stackaddr$prim58056 = alloca %struct.ScmObj*, align 8
%cc47154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56682)
store volatile %struct.ScmObj* %cc47154, %struct.ScmObj** %stackaddr$prim58056, align 8
%ae49985 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58057 = alloca %struct.ScmObj*, align 8
%anf_45bind47300 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47153, %struct.ScmObj* %ae49985)
store volatile %struct.ScmObj* %anf_45bind47300, %struct.ScmObj** %stackaddr$prim58057, align 8
%ae49986 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58058 = alloca %struct.ScmObj*, align 8
%anf_45bind47301 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49986, %struct.ScmObj* %anf_45bind47300)
store volatile %struct.ScmObj* %anf_45bind47301, %struct.ScmObj** %stackaddr$prim58058, align 8
%truthy$cmp58059 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47301)
%cmp$cmp58059 = icmp eq i64 %truthy$cmp58059, 1
br i1 %cmp$cmp58059, label %truebranch$cmp58059, label %falsebranch$cmp58059
truebranch$cmp58059:
%ae49990 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58060 = alloca %struct.ScmObj*, align 8
%cpsprim47467 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47152, %struct.ScmObj* %ae49990)
store volatile %struct.ScmObj* %cpsprim47467, %struct.ScmObj** %stackaddr$prim58060, align 8
%ae49992 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56684$k474640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58061 = alloca %struct.ScmObj*, align 8
%argslist56684$k474641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47467, %struct.ScmObj* %argslist56684$k474640)
store volatile %struct.ScmObj* %argslist56684$k474641, %struct.ScmObj** %stackaddr$prim58061, align 8
%stackaddr$prim58062 = alloca %struct.ScmObj*, align 8
%argslist56684$k474642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49992, %struct.ScmObj* %argslist56684$k474641)
store volatile %struct.ScmObj* %argslist56684$k474642, %struct.ScmObj** %stackaddr$prim58062, align 8
%clofunc58063 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47464)
musttail call tailcc void %clofunc58063(%struct.ScmObj* %k47464, %struct.ScmObj* %argslist56684$k474642)
ret void
falsebranch$cmp58059:
%ae50003 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58064 = alloca %struct.ScmObj*, align 8
%anf_45bind47302 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47152, %struct.ScmObj* %ae50003)
store volatile %struct.ScmObj* %anf_45bind47302, %struct.ScmObj** %stackaddr$prim58064, align 8
%stackaddr$prim58065 = alloca %struct.ScmObj*, align 8
%anf_45bind47303 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47302)
store volatile %struct.ScmObj* %anf_45bind47303, %struct.ScmObj** %stackaddr$prim58065, align 8
%ae50006 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58066 = alloca %struct.ScmObj*, align 8
%_95047157 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47152, %struct.ScmObj* %ae50006, %struct.ScmObj* %anf_45bind47303)
store volatile %struct.ScmObj* %_95047157, %struct.ScmObj** %stackaddr$prim58066, align 8
%ae50009 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58067 = alloca %struct.ScmObj*, align 8
%anf_45bind47304 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47153, %struct.ScmObj* %ae50009)
store volatile %struct.ScmObj* %anf_45bind47304, %struct.ScmObj** %stackaddr$prim58067, align 8
%ae50011 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58068 = alloca %struct.ScmObj*, align 8
%anf_45bind47305 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47304, %struct.ScmObj* %ae50011)
store volatile %struct.ScmObj* %anf_45bind47305, %struct.ScmObj** %stackaddr$prim58068, align 8
%ae50013 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58069 = alloca %struct.ScmObj*, align 8
%_95147156 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47153, %struct.ScmObj* %ae50013, %struct.ScmObj* %anf_45bind47305)
store volatile %struct.ScmObj* %_95147156, %struct.ScmObj** %stackaddr$prim58069, align 8
%argslist56685$cc471540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58070 = alloca %struct.ScmObj*, align 8
%argslist56685$cc471541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47154, %struct.ScmObj* %argslist56685$cc471540)
store volatile %struct.ScmObj* %argslist56685$cc471541, %struct.ScmObj** %stackaddr$prim58070, align 8
%stackaddr$prim58071 = alloca %struct.ScmObj*, align 8
%argslist56685$cc471542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47464, %struct.ScmObj* %argslist56685$cc471541)
store volatile %struct.ScmObj* %argslist56685$cc471542, %struct.ScmObj** %stackaddr$prim58071, align 8
%clofunc58072 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47154)
musttail call tailcc void %clofunc58072(%struct.ScmObj* %cc47154, %struct.ScmObj* %argslist56685$cc471542)
ret void
}

define tailcc void @proc_clo$ae49843(%struct.ScmObj* %env$ae49843,%struct.ScmObj* %current_45args56686) {
%stackaddr$env-ref58073 = alloca %struct.ScmObj*, align 8
%n47153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49843, i64 0)
store %struct.ScmObj* %n47153, %struct.ScmObj** %stackaddr$env-ref58073
%stackaddr$env-ref58074 = alloca %struct.ScmObj*, align 8
%lst47152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49843, i64 1)
store %struct.ScmObj* %lst47152, %struct.ScmObj** %stackaddr$env-ref58074
%stackaddr$env-ref58075 = alloca %struct.ScmObj*, align 8
%k47464 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49843, i64 2)
store %struct.ScmObj* %k47464, %struct.ScmObj** %stackaddr$env-ref58075
%stackaddr$prim58076 = alloca %struct.ScmObj*, align 8
%_95k47466 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56686)
store volatile %struct.ScmObj* %_95k47466, %struct.ScmObj** %stackaddr$prim58076, align 8
%stackaddr$prim58077 = alloca %struct.ScmObj*, align 8
%current_45args56687 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56686)
store volatile %struct.ScmObj* %current_45args56687, %struct.ScmObj** %stackaddr$prim58077, align 8
%stackaddr$prim58078 = alloca %struct.ScmObj*, align 8
%cc47154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56687)
store volatile %struct.ScmObj* %cc47154, %struct.ScmObj** %stackaddr$prim58078, align 8
%ae49845 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58079 = alloca %struct.ScmObj*, align 8
%anf_45bind47300 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47153, %struct.ScmObj* %ae49845)
store volatile %struct.ScmObj* %anf_45bind47300, %struct.ScmObj** %stackaddr$prim58079, align 8
%ae49846 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58080 = alloca %struct.ScmObj*, align 8
%anf_45bind47301 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49846, %struct.ScmObj* %anf_45bind47300)
store volatile %struct.ScmObj* %anf_45bind47301, %struct.ScmObj** %stackaddr$prim58080, align 8
%truthy$cmp58081 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47301)
%cmp$cmp58081 = icmp eq i64 %truthy$cmp58081, 1
br i1 %cmp$cmp58081, label %truebranch$cmp58081, label %falsebranch$cmp58081
truebranch$cmp58081:
%ae49850 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58082 = alloca %struct.ScmObj*, align 8
%cpsprim47467 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47152, %struct.ScmObj* %ae49850)
store volatile %struct.ScmObj* %cpsprim47467, %struct.ScmObj** %stackaddr$prim58082, align 8
%ae49852 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56689$k474640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58083 = alloca %struct.ScmObj*, align 8
%argslist56689$k474641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47467, %struct.ScmObj* %argslist56689$k474640)
store volatile %struct.ScmObj* %argslist56689$k474641, %struct.ScmObj** %stackaddr$prim58083, align 8
%stackaddr$prim58084 = alloca %struct.ScmObj*, align 8
%argslist56689$k474642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49852, %struct.ScmObj* %argslist56689$k474641)
store volatile %struct.ScmObj* %argslist56689$k474642, %struct.ScmObj** %stackaddr$prim58084, align 8
%clofunc58085 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47464)
musttail call tailcc void %clofunc58085(%struct.ScmObj* %k47464, %struct.ScmObj* %argslist56689$k474642)
ret void
falsebranch$cmp58081:
%ae49863 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58086 = alloca %struct.ScmObj*, align 8
%anf_45bind47302 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47152, %struct.ScmObj* %ae49863)
store volatile %struct.ScmObj* %anf_45bind47302, %struct.ScmObj** %stackaddr$prim58086, align 8
%stackaddr$prim58087 = alloca %struct.ScmObj*, align 8
%anf_45bind47303 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47302)
store volatile %struct.ScmObj* %anf_45bind47303, %struct.ScmObj** %stackaddr$prim58087, align 8
%ae49866 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58088 = alloca %struct.ScmObj*, align 8
%_95047157 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47152, %struct.ScmObj* %ae49866, %struct.ScmObj* %anf_45bind47303)
store volatile %struct.ScmObj* %_95047157, %struct.ScmObj** %stackaddr$prim58088, align 8
%ae49869 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58089 = alloca %struct.ScmObj*, align 8
%anf_45bind47304 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47153, %struct.ScmObj* %ae49869)
store volatile %struct.ScmObj* %anf_45bind47304, %struct.ScmObj** %stackaddr$prim58089, align 8
%ae49871 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58090 = alloca %struct.ScmObj*, align 8
%anf_45bind47305 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47304, %struct.ScmObj* %ae49871)
store volatile %struct.ScmObj* %anf_45bind47305, %struct.ScmObj** %stackaddr$prim58090, align 8
%ae49873 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58091 = alloca %struct.ScmObj*, align 8
%_95147156 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47153, %struct.ScmObj* %ae49873, %struct.ScmObj* %anf_45bind47305)
store volatile %struct.ScmObj* %_95147156, %struct.ScmObj** %stackaddr$prim58091, align 8
%argslist56690$cc471540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58092 = alloca %struct.ScmObj*, align 8
%argslist56690$cc471541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47154, %struct.ScmObj* %argslist56690$cc471540)
store volatile %struct.ScmObj* %argslist56690$cc471541, %struct.ScmObj** %stackaddr$prim58092, align 8
%stackaddr$prim58093 = alloca %struct.ScmObj*, align 8
%argslist56690$cc471542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47464, %struct.ScmObj* %argslist56690$cc471541)
store volatile %struct.ScmObj* %argslist56690$cc471542, %struct.ScmObj** %stackaddr$prim58093, align 8
%clofunc58094 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47154)
musttail call tailcc void %clofunc58094(%struct.ScmObj* %cc47154, %struct.ScmObj* %argslist56690$cc471542)
ret void
}

define tailcc void @proc_clo$ae49828(%struct.ScmObj* %env$ae49828,%struct.ScmObj* %current_45args56692) {
%stackaddr$prim58095 = alloca %struct.ScmObj*, align 8
%k47468 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56692)
store volatile %struct.ScmObj* %k47468, %struct.ScmObj** %stackaddr$prim58095, align 8
%stackaddr$prim58096 = alloca %struct.ScmObj*, align 8
%current_45args56693 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56692)
store volatile %struct.ScmObj* %current_45args56693, %struct.ScmObj** %stackaddr$prim58096, align 8
%stackaddr$prim58097 = alloca %struct.ScmObj*, align 8
%u47155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56693)
store volatile %struct.ScmObj* %u47155, %struct.ScmObj** %stackaddr$prim58097, align 8
%argslist56695$u471550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58098 = alloca %struct.ScmObj*, align 8
%argslist56695$u471551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47155, %struct.ScmObj* %argslist56695$u471550)
store volatile %struct.ScmObj* %argslist56695$u471551, %struct.ScmObj** %stackaddr$prim58098, align 8
%stackaddr$prim58099 = alloca %struct.ScmObj*, align 8
%argslist56695$u471552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47468, %struct.ScmObj* %argslist56695$u471551)
store volatile %struct.ScmObj* %argslist56695$u471552, %struct.ScmObj** %stackaddr$prim58099, align 8
%clofunc58100 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47155)
musttail call tailcc void %clofunc58100(%struct.ScmObj* %u47155, %struct.ScmObj* %argslist56695$u471552)
ret void
}

define tailcc void @proc_clo$ae49405(%struct.ScmObj* %env$ae49405,%struct.ScmObj* %current_45args56698) {
%stackaddr$prim58101 = alloca %struct.ScmObj*, align 8
%k47469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56698)
store volatile %struct.ScmObj* %k47469, %struct.ScmObj** %stackaddr$prim58101, align 8
%stackaddr$prim58102 = alloca %struct.ScmObj*, align 8
%current_45args56699 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56698)
store volatile %struct.ScmObj* %current_45args56699, %struct.ScmObj** %stackaddr$prim58102, align 8
%stackaddr$prim58103 = alloca %struct.ScmObj*, align 8
%a47159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56699)
store volatile %struct.ScmObj* %a47159, %struct.ScmObj** %stackaddr$prim58103, align 8
%ae49406 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58104 = alloca %struct.ScmObj*, align 8
%a47160 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49406, %struct.ScmObj* %a47159)
store volatile %struct.ScmObj* %a47160, %struct.ScmObj** %stackaddr$prim58104, align 8
%stackaddr$makeclosure58105 = alloca %struct.ScmObj*, align 8
%fptrToInt58106 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49408 to i64
%ae49408 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58106)
store volatile %struct.ScmObj* %ae49408, %struct.ScmObj** %stackaddr$makeclosure58105, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49408, %struct.ScmObj* %k47469, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49408, %struct.ScmObj* %a47160, i64 1)
%ae49409 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58107 = alloca %struct.ScmObj*, align 8
%fptrToInt58108 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49410 to i64
%ae49410 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58108)
store volatile %struct.ScmObj* %ae49410, %struct.ScmObj** %stackaddr$makeclosure58107, align 8
%argslist56721$ae494080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58109 = alloca %struct.ScmObj*, align 8
%argslist56721$ae494081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49410, %struct.ScmObj* %argslist56721$ae494080)
store volatile %struct.ScmObj* %argslist56721$ae494081, %struct.ScmObj** %stackaddr$prim58109, align 8
%stackaddr$prim58110 = alloca %struct.ScmObj*, align 8
%argslist56721$ae494082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49409, %struct.ScmObj* %argslist56721$ae494081)
store volatile %struct.ScmObj* %argslist56721$ae494082, %struct.ScmObj** %stackaddr$prim58110, align 8
%clofunc58111 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49408)
musttail call tailcc void %clofunc58111(%struct.ScmObj* %ae49408, %struct.ScmObj* %argslist56721$ae494082)
ret void
}

define tailcc void @proc_clo$ae49408(%struct.ScmObj* %env$ae49408,%struct.ScmObj* %current_45args56701) {
%stackaddr$env-ref58112 = alloca %struct.ScmObj*, align 8
%k47469 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49408, i64 0)
store %struct.ScmObj* %k47469, %struct.ScmObj** %stackaddr$env-ref58112
%stackaddr$env-ref58113 = alloca %struct.ScmObj*, align 8
%a47160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49408, i64 1)
store %struct.ScmObj* %a47160, %struct.ScmObj** %stackaddr$env-ref58113
%stackaddr$prim58114 = alloca %struct.ScmObj*, align 8
%_95k47470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56701)
store volatile %struct.ScmObj* %_95k47470, %struct.ScmObj** %stackaddr$prim58114, align 8
%stackaddr$prim58115 = alloca %struct.ScmObj*, align 8
%current_45args56702 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56701)
store volatile %struct.ScmObj* %current_45args56702, %struct.ScmObj** %stackaddr$prim58115, align 8
%stackaddr$prim58116 = alloca %struct.ScmObj*, align 8
%anf_45bind47291 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56702)
store volatile %struct.ScmObj* %anf_45bind47291, %struct.ScmObj** %stackaddr$prim58116, align 8
%stackaddr$makeclosure58117 = alloca %struct.ScmObj*, align 8
%fptrToInt58118 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49427 to i64
%ae49427 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58118)
store volatile %struct.ScmObj* %ae49427, %struct.ScmObj** %stackaddr$makeclosure58117, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49427, %struct.ScmObj* %k47469, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49427, %struct.ScmObj* %a47160, i64 1)
%stackaddr$makeclosure58119 = alloca %struct.ScmObj*, align 8
%fptrToInt58120 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49428 to i64
%ae49428 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58120)
store volatile %struct.ScmObj* %ae49428, %struct.ScmObj** %stackaddr$makeclosure58119, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49428, %struct.ScmObj* %k47469, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49428, %struct.ScmObj* %a47160, i64 1)
%argslist56716$anf_45bind472910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58121 = alloca %struct.ScmObj*, align 8
%argslist56716$anf_45bind472911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49428, %struct.ScmObj* %argslist56716$anf_45bind472910)
store volatile %struct.ScmObj* %argslist56716$anf_45bind472911, %struct.ScmObj** %stackaddr$prim58121, align 8
%stackaddr$prim58122 = alloca %struct.ScmObj*, align 8
%argslist56716$anf_45bind472912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49427, %struct.ScmObj* %argslist56716$anf_45bind472911)
store volatile %struct.ScmObj* %argslist56716$anf_45bind472912, %struct.ScmObj** %stackaddr$prim58122, align 8
%clofunc58123 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47291)
musttail call tailcc void %clofunc58123(%struct.ScmObj* %anf_45bind47291, %struct.ScmObj* %argslist56716$anf_45bind472912)
ret void
}

define tailcc void @proc_clo$ae49427(%struct.ScmObj* %env$ae49427,%struct.ScmObj* %current_45args56704) {
%stackaddr$env-ref58124 = alloca %struct.ScmObj*, align 8
%k47469 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49427, i64 0)
store %struct.ScmObj* %k47469, %struct.ScmObj** %stackaddr$env-ref58124
%stackaddr$env-ref58125 = alloca %struct.ScmObj*, align 8
%a47160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49427, i64 1)
store %struct.ScmObj* %a47160, %struct.ScmObj** %stackaddr$env-ref58125
%stackaddr$prim58126 = alloca %struct.ScmObj*, align 8
%_95k47471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56704)
store volatile %struct.ScmObj* %_95k47471, %struct.ScmObj** %stackaddr$prim58126, align 8
%stackaddr$prim58127 = alloca %struct.ScmObj*, align 8
%current_45args56705 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56704)
store volatile %struct.ScmObj* %current_45args56705, %struct.ScmObj** %stackaddr$prim58127, align 8
%stackaddr$prim58128 = alloca %struct.ScmObj*, align 8
%cc47161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56705)
store volatile %struct.ScmObj* %cc47161, %struct.ScmObj** %stackaddr$prim58128, align 8
%ae49543 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58129 = alloca %struct.ScmObj*, align 8
%anf_45bind47292 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47160, %struct.ScmObj* %ae49543)
store volatile %struct.ScmObj* %anf_45bind47292, %struct.ScmObj** %stackaddr$prim58129, align 8
%stackaddr$prim58130 = alloca %struct.ScmObj*, align 8
%anf_45bind47293 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47292)
store volatile %struct.ScmObj* %anf_45bind47293, %struct.ScmObj** %stackaddr$prim58130, align 8
%truthy$cmp58131 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47293)
%cmp$cmp58131 = icmp eq i64 %truthy$cmp58131, 1
br i1 %cmp$cmp58131, label %truebranch$cmp58131, label %falsebranch$cmp58131
truebranch$cmp58131:
%ae49547 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49548 = call %struct.ScmObj* @const_init_true()
%argslist56707$k474690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58132 = alloca %struct.ScmObj*, align 8
%argslist56707$k474691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49548, %struct.ScmObj* %argslist56707$k474690)
store volatile %struct.ScmObj* %argslist56707$k474691, %struct.ScmObj** %stackaddr$prim58132, align 8
%stackaddr$prim58133 = alloca %struct.ScmObj*, align 8
%argslist56707$k474692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49547, %struct.ScmObj* %argslist56707$k474691)
store volatile %struct.ScmObj* %argslist56707$k474692, %struct.ScmObj** %stackaddr$prim58133, align 8
%clofunc58134 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47469)
musttail call tailcc void %clofunc58134(%struct.ScmObj* %k47469, %struct.ScmObj* %argslist56707$k474692)
ret void
falsebranch$cmp58131:
%ae49556 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58135 = alloca %struct.ScmObj*, align 8
%anf_45bind47294 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47160, %struct.ScmObj* %ae49556)
store volatile %struct.ScmObj* %anf_45bind47294, %struct.ScmObj** %stackaddr$prim58135, align 8
%stackaddr$prim58136 = alloca %struct.ScmObj*, align 8
%anf_45bind47295 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47294)
store volatile %struct.ScmObj* %anf_45bind47295, %struct.ScmObj** %stackaddr$prim58136, align 8
%truthy$cmp58137 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47295)
%cmp$cmp58137 = icmp eq i64 %truthy$cmp58137, 1
br i1 %cmp$cmp58137, label %truebranch$cmp58137, label %falsebranch$cmp58137
truebranch$cmp58137:
%ae49560 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58138 = alloca %struct.ScmObj*, align 8
%anf_45bind47296 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47160, %struct.ScmObj* %ae49560)
store volatile %struct.ScmObj* %anf_45bind47296, %struct.ScmObj** %stackaddr$prim58138, align 8
%stackaddr$prim58139 = alloca %struct.ScmObj*, align 8
%b47163 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47296)
store volatile %struct.ScmObj* %b47163, %struct.ScmObj** %stackaddr$prim58139, align 8
%ae49563 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58140 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47160, %struct.ScmObj* %ae49563)
store volatile %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$prim58140, align 8
%stackaddr$prim58141 = alloca %struct.ScmObj*, align 8
%anf_45bind47298 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47297)
store volatile %struct.ScmObj* %anf_45bind47298, %struct.ScmObj** %stackaddr$prim58141, align 8
%ae49566 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58142 = alloca %struct.ScmObj*, align 8
%_95047164 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47160, %struct.ScmObj* %ae49566, %struct.ScmObj* %anf_45bind47298)
store volatile %struct.ScmObj* %_95047164, %struct.ScmObj** %stackaddr$prim58142, align 8
%argslist56708$cc471610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58143 = alloca %struct.ScmObj*, align 8
%argslist56708$cc471611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47161, %struct.ScmObj* %argslist56708$cc471610)
store volatile %struct.ScmObj* %argslist56708$cc471611, %struct.ScmObj** %stackaddr$prim58143, align 8
%stackaddr$prim58144 = alloca %struct.ScmObj*, align 8
%argslist56708$cc471612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47469, %struct.ScmObj* %argslist56708$cc471611)
store volatile %struct.ScmObj* %argslist56708$cc471612, %struct.ScmObj** %stackaddr$prim58144, align 8
%clofunc58145 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47161)
musttail call tailcc void %clofunc58145(%struct.ScmObj* %cc47161, %struct.ScmObj* %argslist56708$cc471612)
ret void
falsebranch$cmp58137:
%ae49599 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49600 = call %struct.ScmObj* @const_init_false()
%argslist56709$k474690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58146 = alloca %struct.ScmObj*, align 8
%argslist56709$k474691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49600, %struct.ScmObj* %argslist56709$k474690)
store volatile %struct.ScmObj* %argslist56709$k474691, %struct.ScmObj** %stackaddr$prim58146, align 8
%stackaddr$prim58147 = alloca %struct.ScmObj*, align 8
%argslist56709$k474692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49599, %struct.ScmObj* %argslist56709$k474691)
store volatile %struct.ScmObj* %argslist56709$k474692, %struct.ScmObj** %stackaddr$prim58147, align 8
%clofunc58148 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47469)
musttail call tailcc void %clofunc58148(%struct.ScmObj* %k47469, %struct.ScmObj* %argslist56709$k474692)
ret void
}

define tailcc void @proc_clo$ae49428(%struct.ScmObj* %env$ae49428,%struct.ScmObj* %current_45args56710) {
%stackaddr$env-ref58149 = alloca %struct.ScmObj*, align 8
%k47469 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49428, i64 0)
store %struct.ScmObj* %k47469, %struct.ScmObj** %stackaddr$env-ref58149
%stackaddr$env-ref58150 = alloca %struct.ScmObj*, align 8
%a47160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49428, i64 1)
store %struct.ScmObj* %a47160, %struct.ScmObj** %stackaddr$env-ref58150
%stackaddr$prim58151 = alloca %struct.ScmObj*, align 8
%_95k47471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56710)
store volatile %struct.ScmObj* %_95k47471, %struct.ScmObj** %stackaddr$prim58151, align 8
%stackaddr$prim58152 = alloca %struct.ScmObj*, align 8
%current_45args56711 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56710)
store volatile %struct.ScmObj* %current_45args56711, %struct.ScmObj** %stackaddr$prim58152, align 8
%stackaddr$prim58153 = alloca %struct.ScmObj*, align 8
%cc47161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56711)
store volatile %struct.ScmObj* %cc47161, %struct.ScmObj** %stackaddr$prim58153, align 8
%ae49430 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58154 = alloca %struct.ScmObj*, align 8
%anf_45bind47292 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47160, %struct.ScmObj* %ae49430)
store volatile %struct.ScmObj* %anf_45bind47292, %struct.ScmObj** %stackaddr$prim58154, align 8
%stackaddr$prim58155 = alloca %struct.ScmObj*, align 8
%anf_45bind47293 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47292)
store volatile %struct.ScmObj* %anf_45bind47293, %struct.ScmObj** %stackaddr$prim58155, align 8
%truthy$cmp58156 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47293)
%cmp$cmp58156 = icmp eq i64 %truthy$cmp58156, 1
br i1 %cmp$cmp58156, label %truebranch$cmp58156, label %falsebranch$cmp58156
truebranch$cmp58156:
%ae49434 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49435 = call %struct.ScmObj* @const_init_true()
%argslist56713$k474690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58157 = alloca %struct.ScmObj*, align 8
%argslist56713$k474691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49435, %struct.ScmObj* %argslist56713$k474690)
store volatile %struct.ScmObj* %argslist56713$k474691, %struct.ScmObj** %stackaddr$prim58157, align 8
%stackaddr$prim58158 = alloca %struct.ScmObj*, align 8
%argslist56713$k474692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49434, %struct.ScmObj* %argslist56713$k474691)
store volatile %struct.ScmObj* %argslist56713$k474692, %struct.ScmObj** %stackaddr$prim58158, align 8
%clofunc58159 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47469)
musttail call tailcc void %clofunc58159(%struct.ScmObj* %k47469, %struct.ScmObj* %argslist56713$k474692)
ret void
falsebranch$cmp58156:
%ae49443 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58160 = alloca %struct.ScmObj*, align 8
%anf_45bind47294 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47160, %struct.ScmObj* %ae49443)
store volatile %struct.ScmObj* %anf_45bind47294, %struct.ScmObj** %stackaddr$prim58160, align 8
%stackaddr$prim58161 = alloca %struct.ScmObj*, align 8
%anf_45bind47295 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47294)
store volatile %struct.ScmObj* %anf_45bind47295, %struct.ScmObj** %stackaddr$prim58161, align 8
%truthy$cmp58162 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47295)
%cmp$cmp58162 = icmp eq i64 %truthy$cmp58162, 1
br i1 %cmp$cmp58162, label %truebranch$cmp58162, label %falsebranch$cmp58162
truebranch$cmp58162:
%ae49447 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58163 = alloca %struct.ScmObj*, align 8
%anf_45bind47296 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47160, %struct.ScmObj* %ae49447)
store volatile %struct.ScmObj* %anf_45bind47296, %struct.ScmObj** %stackaddr$prim58163, align 8
%stackaddr$prim58164 = alloca %struct.ScmObj*, align 8
%b47163 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47296)
store volatile %struct.ScmObj* %b47163, %struct.ScmObj** %stackaddr$prim58164, align 8
%ae49450 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58165 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47160, %struct.ScmObj* %ae49450)
store volatile %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$prim58165, align 8
%stackaddr$prim58166 = alloca %struct.ScmObj*, align 8
%anf_45bind47298 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47297)
store volatile %struct.ScmObj* %anf_45bind47298, %struct.ScmObj** %stackaddr$prim58166, align 8
%ae49453 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58167 = alloca %struct.ScmObj*, align 8
%_95047164 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47160, %struct.ScmObj* %ae49453, %struct.ScmObj* %anf_45bind47298)
store volatile %struct.ScmObj* %_95047164, %struct.ScmObj** %stackaddr$prim58167, align 8
%argslist56714$cc471610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58168 = alloca %struct.ScmObj*, align 8
%argslist56714$cc471611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47161, %struct.ScmObj* %argslist56714$cc471610)
store volatile %struct.ScmObj* %argslist56714$cc471611, %struct.ScmObj** %stackaddr$prim58168, align 8
%stackaddr$prim58169 = alloca %struct.ScmObj*, align 8
%argslist56714$cc471612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47469, %struct.ScmObj* %argslist56714$cc471611)
store volatile %struct.ScmObj* %argslist56714$cc471612, %struct.ScmObj** %stackaddr$prim58169, align 8
%clofunc58170 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47161)
musttail call tailcc void %clofunc58170(%struct.ScmObj* %cc47161, %struct.ScmObj* %argslist56714$cc471612)
ret void
falsebranch$cmp58162:
%ae49486 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49487 = call %struct.ScmObj* @const_init_false()
%argslist56715$k474690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58171 = alloca %struct.ScmObj*, align 8
%argslist56715$k474691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49487, %struct.ScmObj* %argslist56715$k474690)
store volatile %struct.ScmObj* %argslist56715$k474691, %struct.ScmObj** %stackaddr$prim58171, align 8
%stackaddr$prim58172 = alloca %struct.ScmObj*, align 8
%argslist56715$k474692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49486, %struct.ScmObj* %argslist56715$k474691)
store volatile %struct.ScmObj* %argslist56715$k474692, %struct.ScmObj** %stackaddr$prim58172, align 8
%clofunc58173 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47469)
musttail call tailcc void %clofunc58173(%struct.ScmObj* %k47469, %struct.ScmObj* %argslist56715$k474692)
ret void
}

define tailcc void @proc_clo$ae49410(%struct.ScmObj* %env$ae49410,%struct.ScmObj* %current_45args56717) {
%stackaddr$prim58174 = alloca %struct.ScmObj*, align 8
%k47472 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56717)
store volatile %struct.ScmObj* %k47472, %struct.ScmObj** %stackaddr$prim58174, align 8
%stackaddr$prim58175 = alloca %struct.ScmObj*, align 8
%current_45args56718 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56717)
store volatile %struct.ScmObj* %current_45args56718, %struct.ScmObj** %stackaddr$prim58175, align 8
%stackaddr$prim58176 = alloca %struct.ScmObj*, align 8
%k47162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56718)
store volatile %struct.ScmObj* %k47162, %struct.ScmObj** %stackaddr$prim58176, align 8
%ae49412 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56720$k474720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58177 = alloca %struct.ScmObj*, align 8
%argslist56720$k474721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47162, %struct.ScmObj* %argslist56720$k474720)
store volatile %struct.ScmObj* %argslist56720$k474721, %struct.ScmObj** %stackaddr$prim58177, align 8
%stackaddr$prim58178 = alloca %struct.ScmObj*, align 8
%argslist56720$k474722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49412, %struct.ScmObj* %argslist56720$k474721)
store volatile %struct.ScmObj* %argslist56720$k474722, %struct.ScmObj** %stackaddr$prim58178, align 8
%clofunc58179 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47472)
musttail call tailcc void %clofunc58179(%struct.ScmObj* %k47472, %struct.ScmObj* %argslist56720$k474722)
ret void
}

define tailcc void @proc_clo$ae49333(%struct.ScmObj* %env$ae49333,%struct.ScmObj* %current_45args56723) {
%stackaddr$env-ref58180 = alloca %struct.ScmObj*, align 8
%_37append47166 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49333, i64 0)
store %struct.ScmObj* %_37append47166, %struct.ScmObj** %stackaddr$env-ref58180
%stackaddr$prim58181 = alloca %struct.ScmObj*, align 8
%k47473 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56723)
store volatile %struct.ScmObj* %k47473, %struct.ScmObj** %stackaddr$prim58181, align 8
%stackaddr$prim58182 = alloca %struct.ScmObj*, align 8
%current_45args56724 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56723)
store volatile %struct.ScmObj* %current_45args56724, %struct.ScmObj** %stackaddr$prim58182, align 8
%stackaddr$prim58183 = alloca %struct.ScmObj*, align 8
%ls047169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56724)
store volatile %struct.ScmObj* %ls047169, %struct.ScmObj** %stackaddr$prim58183, align 8
%stackaddr$prim58184 = alloca %struct.ScmObj*, align 8
%current_45args56725 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56724)
store volatile %struct.ScmObj* %current_45args56725, %struct.ScmObj** %stackaddr$prim58184, align 8
%stackaddr$prim58185 = alloca %struct.ScmObj*, align 8
%ls147168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56725)
store volatile %struct.ScmObj* %ls147168, %struct.ScmObj** %stackaddr$prim58185, align 8
%stackaddr$prim58186 = alloca %struct.ScmObj*, align 8
%anf_45bind47285 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls047169)
store volatile %struct.ScmObj* %anf_45bind47285, %struct.ScmObj** %stackaddr$prim58186, align 8
%truthy$cmp58187 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47285)
%cmp$cmp58187 = icmp eq i64 %truthy$cmp58187, 1
br i1 %cmp$cmp58187, label %truebranch$cmp58187, label %falsebranch$cmp58187
truebranch$cmp58187:
%ae49337 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56727$k474730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58188 = alloca %struct.ScmObj*, align 8
%argslist56727$k474731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147168, %struct.ScmObj* %argslist56727$k474730)
store volatile %struct.ScmObj* %argslist56727$k474731, %struct.ScmObj** %stackaddr$prim58188, align 8
%stackaddr$prim58189 = alloca %struct.ScmObj*, align 8
%argslist56727$k474732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49337, %struct.ScmObj* %argslist56727$k474731)
store volatile %struct.ScmObj* %argslist56727$k474732, %struct.ScmObj** %stackaddr$prim58189, align 8
%clofunc58190 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47473)
musttail call tailcc void %clofunc58190(%struct.ScmObj* %k47473, %struct.ScmObj* %argslist56727$k474732)
ret void
falsebranch$cmp58187:
%stackaddr$prim58191 = alloca %struct.ScmObj*, align 8
%anf_45bind47286 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls047169)
store volatile %struct.ScmObj* %anf_45bind47286, %struct.ScmObj** %stackaddr$prim58191, align 8
%ae49344 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58192 = alloca %struct.ScmObj*, align 8
%anf_45bind47287 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47166, %struct.ScmObj* %ae49344)
store volatile %struct.ScmObj* %anf_45bind47287, %struct.ScmObj** %stackaddr$prim58192, align 8
%stackaddr$prim58193 = alloca %struct.ScmObj*, align 8
%anf_45bind47288 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls047169)
store volatile %struct.ScmObj* %anf_45bind47288, %struct.ScmObj** %stackaddr$prim58193, align 8
%stackaddr$makeclosure58194 = alloca %struct.ScmObj*, align 8
%fptrToInt58195 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49347 to i64
%ae49347 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58195)
store volatile %struct.ScmObj* %ae49347, %struct.ScmObj** %stackaddr$makeclosure58194, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49347, %struct.ScmObj* %anf_45bind47286, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49347, %struct.ScmObj* %k47473, i64 1)
%argslist56732$anf_45bind472870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58196 = alloca %struct.ScmObj*, align 8
%argslist56732$anf_45bind472871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147168, %struct.ScmObj* %argslist56732$anf_45bind472870)
store volatile %struct.ScmObj* %argslist56732$anf_45bind472871, %struct.ScmObj** %stackaddr$prim58196, align 8
%stackaddr$prim58197 = alloca %struct.ScmObj*, align 8
%argslist56732$anf_45bind472872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47288, %struct.ScmObj* %argslist56732$anf_45bind472871)
store volatile %struct.ScmObj* %argslist56732$anf_45bind472872, %struct.ScmObj** %stackaddr$prim58197, align 8
%stackaddr$prim58198 = alloca %struct.ScmObj*, align 8
%argslist56732$anf_45bind472873 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49347, %struct.ScmObj* %argslist56732$anf_45bind472872)
store volatile %struct.ScmObj* %argslist56732$anf_45bind472873, %struct.ScmObj** %stackaddr$prim58198, align 8
%clofunc58199 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47287)
musttail call tailcc void %clofunc58199(%struct.ScmObj* %anf_45bind47287, %struct.ScmObj* %argslist56732$anf_45bind472873)
ret void
}

define tailcc void @proc_clo$ae49347(%struct.ScmObj* %env$ae49347,%struct.ScmObj* %current_45args56728) {
%stackaddr$env-ref58200 = alloca %struct.ScmObj*, align 8
%anf_45bind47286 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49347, i64 0)
store %struct.ScmObj* %anf_45bind47286, %struct.ScmObj** %stackaddr$env-ref58200
%stackaddr$env-ref58201 = alloca %struct.ScmObj*, align 8
%k47473 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49347, i64 1)
store %struct.ScmObj* %k47473, %struct.ScmObj** %stackaddr$env-ref58201
%stackaddr$prim58202 = alloca %struct.ScmObj*, align 8
%_95k47474 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56728)
store volatile %struct.ScmObj* %_95k47474, %struct.ScmObj** %stackaddr$prim58202, align 8
%stackaddr$prim58203 = alloca %struct.ScmObj*, align 8
%current_45args56729 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56728)
store volatile %struct.ScmObj* %current_45args56729, %struct.ScmObj** %stackaddr$prim58203, align 8
%stackaddr$prim58204 = alloca %struct.ScmObj*, align 8
%anf_45bind47289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56729)
store volatile %struct.ScmObj* %anf_45bind47289, %struct.ScmObj** %stackaddr$prim58204, align 8
%stackaddr$prim58205 = alloca %struct.ScmObj*, align 8
%cpsprim47475 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47286, %struct.ScmObj* %anf_45bind47289)
store volatile %struct.ScmObj* %cpsprim47475, %struct.ScmObj** %stackaddr$prim58205, align 8
%ae49353 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56731$k474730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58206 = alloca %struct.ScmObj*, align 8
%argslist56731$k474731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47475, %struct.ScmObj* %argslist56731$k474730)
store volatile %struct.ScmObj* %argslist56731$k474731, %struct.ScmObj** %stackaddr$prim58206, align 8
%stackaddr$prim58207 = alloca %struct.ScmObj*, align 8
%argslist56731$k474732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49353, %struct.ScmObj* %argslist56731$k474731)
store volatile %struct.ScmObj* %argslist56731$k474732, %struct.ScmObj** %stackaddr$prim58207, align 8
%clofunc58208 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47473)
musttail call tailcc void %clofunc58208(%struct.ScmObj* %k47473, %struct.ScmObj* %argslist56731$k474732)
ret void
}

define tailcc void @proc_clo$ae49307(%struct.ScmObj* %env$ae49307,%struct.ScmObj* %current_45args56734) {
%stackaddr$prim58209 = alloca %struct.ScmObj*, align 8
%k47476 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56734)
store volatile %struct.ScmObj* %k47476, %struct.ScmObj** %stackaddr$prim58209, align 8
%stackaddr$prim58210 = alloca %struct.ScmObj*, align 8
%current_45args56735 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56734)
store volatile %struct.ScmObj* %current_45args56735, %struct.ScmObj** %stackaddr$prim58210, align 8
%stackaddr$prim58211 = alloca %struct.ScmObj*, align 8
%a47172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56735)
store volatile %struct.ScmObj* %a47172, %struct.ScmObj** %stackaddr$prim58211, align 8
%stackaddr$prim58212 = alloca %struct.ScmObj*, align 8
%current_45args56736 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56735)
store volatile %struct.ScmObj* %current_45args56736, %struct.ScmObj** %stackaddr$prim58212, align 8
%stackaddr$prim58213 = alloca %struct.ScmObj*, align 8
%b47171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56736)
store volatile %struct.ScmObj* %b47171, %struct.ScmObj** %stackaddr$prim58213, align 8
%stackaddr$prim58214 = alloca %struct.ScmObj*, align 8
%anf_45bind47284 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a47172, %struct.ScmObj* %b47171)
store volatile %struct.ScmObj* %anf_45bind47284, %struct.ScmObj** %stackaddr$prim58214, align 8
%stackaddr$prim58215 = alloca %struct.ScmObj*, align 8
%cpsprim47477 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47284)
store volatile %struct.ScmObj* %cpsprim47477, %struct.ScmObj** %stackaddr$prim58215, align 8
%ae49312 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56738$k474760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58216 = alloca %struct.ScmObj*, align 8
%argslist56738$k474761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47477, %struct.ScmObj* %argslist56738$k474760)
store volatile %struct.ScmObj* %argslist56738$k474761, %struct.ScmObj** %stackaddr$prim58216, align 8
%stackaddr$prim58217 = alloca %struct.ScmObj*, align 8
%argslist56738$k474762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49312, %struct.ScmObj* %argslist56738$k474761)
store volatile %struct.ScmObj* %argslist56738$k474762, %struct.ScmObj** %stackaddr$prim58217, align 8
%clofunc58218 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47476)
musttail call tailcc void %clofunc58218(%struct.ScmObj* %k47476, %struct.ScmObj* %argslist56738$k474762)
ret void
}

define tailcc void @proc_clo$ae49283(%struct.ScmObj* %env$ae49283,%struct.ScmObj* %current_45args56740) {
%stackaddr$prim58219 = alloca %struct.ScmObj*, align 8
%k47478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56740)
store volatile %struct.ScmObj* %k47478, %struct.ScmObj** %stackaddr$prim58219, align 8
%stackaddr$prim58220 = alloca %struct.ScmObj*, align 8
%current_45args56741 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56740)
store volatile %struct.ScmObj* %current_45args56741, %struct.ScmObj** %stackaddr$prim58220, align 8
%stackaddr$prim58221 = alloca %struct.ScmObj*, align 8
%a47175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56741)
store volatile %struct.ScmObj* %a47175, %struct.ScmObj** %stackaddr$prim58221, align 8
%stackaddr$prim58222 = alloca %struct.ScmObj*, align 8
%current_45args56742 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56741)
store volatile %struct.ScmObj* %current_45args56742, %struct.ScmObj** %stackaddr$prim58222, align 8
%stackaddr$prim58223 = alloca %struct.ScmObj*, align 8
%b47174 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56742)
store volatile %struct.ScmObj* %b47174, %struct.ScmObj** %stackaddr$prim58223, align 8
%stackaddr$prim58224 = alloca %struct.ScmObj*, align 8
%anf_45bind47283 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a47175, %struct.ScmObj* %b47174)
store volatile %struct.ScmObj* %anf_45bind47283, %struct.ScmObj** %stackaddr$prim58224, align 8
%stackaddr$prim58225 = alloca %struct.ScmObj*, align 8
%cpsprim47479 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47283)
store volatile %struct.ScmObj* %cpsprim47479, %struct.ScmObj** %stackaddr$prim58225, align 8
%ae49288 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56744$k474780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58226 = alloca %struct.ScmObj*, align 8
%argslist56744$k474781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47479, %struct.ScmObj* %argslist56744$k474780)
store volatile %struct.ScmObj* %argslist56744$k474781, %struct.ScmObj** %stackaddr$prim58226, align 8
%stackaddr$prim58227 = alloca %struct.ScmObj*, align 8
%argslist56744$k474782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49288, %struct.ScmObj* %argslist56744$k474781)
store volatile %struct.ScmObj* %argslist56744$k474782, %struct.ScmObj** %stackaddr$prim58227, align 8
%clofunc58228 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47478)
musttail call tailcc void %clofunc58228(%struct.ScmObj* %k47478, %struct.ScmObj* %argslist56744$k474782)
ret void
}

define tailcc void @proc_clo$ae48889(%struct.ScmObj* %env$ae48889,%struct.ScmObj* %current_45args56747) {
%stackaddr$env-ref58229 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48889, i64 0)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref58229
%stackaddr$env-ref58230 = alloca %struct.ScmObj*, align 8
%_37map147125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48889, i64 1)
store %struct.ScmObj* %_37map147125, %struct.ScmObj** %stackaddr$env-ref58230
%stackaddr$env-ref58231 = alloca %struct.ScmObj*, align 8
%_37foldr47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48889, i64 2)
store %struct.ScmObj* %_37foldr47099, %struct.ScmObj** %stackaddr$env-ref58231
%stackaddr$prim58232 = alloca %struct.ScmObj*, align 8
%k47480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56747)
store volatile %struct.ScmObj* %k47480, %struct.ScmObj** %stackaddr$prim58232, align 8
%stackaddr$prim58233 = alloca %struct.ScmObj*, align 8
%current_45args56748 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56747)
store volatile %struct.ScmObj* %current_45args56748, %struct.ScmObj** %stackaddr$prim58233, align 8
%stackaddr$prim58234 = alloca %struct.ScmObj*, align 8
%_37foldl47177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56748)
store volatile %struct.ScmObj* %_37foldl47177, %struct.ScmObj** %stackaddr$prim58234, align 8
%ae48891 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58235 = alloca %struct.ScmObj*, align 8
%fptrToInt58236 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48892 to i64
%ae48892 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58236)
store volatile %struct.ScmObj* %ae48892, %struct.ScmObj** %stackaddr$makeclosure58235, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48892, %struct.ScmObj* %_37foldr147094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48892, %struct.ScmObj* %_37map147125, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48892, %struct.ScmObj* %_37foldr47099, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48892, %struct.ScmObj* %_37foldl47177, i64 3)
%argslist56805$k474800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58237 = alloca %struct.ScmObj*, align 8
%argslist56805$k474801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48892, %struct.ScmObj* %argslist56805$k474800)
store volatile %struct.ScmObj* %argslist56805$k474801, %struct.ScmObj** %stackaddr$prim58237, align 8
%stackaddr$prim58238 = alloca %struct.ScmObj*, align 8
%argslist56805$k474802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48891, %struct.ScmObj* %argslist56805$k474801)
store volatile %struct.ScmObj* %argslist56805$k474802, %struct.ScmObj** %stackaddr$prim58238, align 8
%clofunc58239 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47480)
musttail call tailcc void %clofunc58239(%struct.ScmObj* %k47480, %struct.ScmObj* %argslist56805$k474802)
ret void
}

define tailcc void @proc_clo$ae48892(%struct.ScmObj* %env$ae48892,%struct.ScmObj* %args4717847481) {
%stackaddr$env-ref58240 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48892, i64 0)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref58240
%stackaddr$env-ref58241 = alloca %struct.ScmObj*, align 8
%_37map147125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48892, i64 1)
store %struct.ScmObj* %_37map147125, %struct.ScmObj** %stackaddr$env-ref58241
%stackaddr$env-ref58242 = alloca %struct.ScmObj*, align 8
%_37foldr47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48892, i64 2)
store %struct.ScmObj* %_37foldr47099, %struct.ScmObj** %stackaddr$env-ref58242
%stackaddr$env-ref58243 = alloca %struct.ScmObj*, align 8
%_37foldl47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48892, i64 3)
store %struct.ScmObj* %_37foldl47177, %struct.ScmObj** %stackaddr$env-ref58243
%stackaddr$prim58244 = alloca %struct.ScmObj*, align 8
%k47482 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4717847481)
store volatile %struct.ScmObj* %k47482, %struct.ScmObj** %stackaddr$prim58244, align 8
%stackaddr$prim58245 = alloca %struct.ScmObj*, align 8
%args47178 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4717847481)
store volatile %struct.ScmObj* %args47178, %struct.ScmObj** %stackaddr$prim58245, align 8
%stackaddr$prim58246 = alloca %struct.ScmObj*, align 8
%f47181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47178)
store volatile %struct.ScmObj* %f47181, %struct.ScmObj** %stackaddr$prim58246, align 8
%stackaddr$prim58247 = alloca %struct.ScmObj*, align 8
%anf_45bind47271 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47178)
store volatile %struct.ScmObj* %anf_45bind47271, %struct.ScmObj** %stackaddr$prim58247, align 8
%stackaddr$prim58248 = alloca %struct.ScmObj*, align 8
%acc47180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47271)
store volatile %struct.ScmObj* %acc47180, %struct.ScmObj** %stackaddr$prim58248, align 8
%stackaddr$prim58249 = alloca %struct.ScmObj*, align 8
%anf_45bind47272 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47178)
store volatile %struct.ScmObj* %anf_45bind47272, %struct.ScmObj** %stackaddr$prim58249, align 8
%stackaddr$prim58250 = alloca %struct.ScmObj*, align 8
%lsts47179 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47272)
store volatile %struct.ScmObj* %lsts47179, %struct.ScmObj** %stackaddr$prim58250, align 8
%stackaddr$makeclosure58251 = alloca %struct.ScmObj*, align 8
%fptrToInt58252 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48900 to i64
%ae48900 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt58252)
store volatile %struct.ScmObj* %ae48900, %struct.ScmObj** %stackaddr$makeclosure58251, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48900, %struct.ScmObj* %lsts47179, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48900, %struct.ScmObj* %_37foldr47099, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48900, %struct.ScmObj* %_37foldr147094, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48900, %struct.ScmObj* %_37map147125, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48900, %struct.ScmObj* %f47181, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48900, %struct.ScmObj* %acc47180, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48900, %struct.ScmObj* %k47482, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48900, %struct.ScmObj* %_37foldl47177, i64 7)
%ae48901 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58253 = alloca %struct.ScmObj*, align 8
%fptrToInt58254 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48902 to i64
%ae48902 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58254)
store volatile %struct.ScmObj* %ae48902, %struct.ScmObj** %stackaddr$makeclosure58253, align 8
%argslist56804$ae489000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58255 = alloca %struct.ScmObj*, align 8
%argslist56804$ae489001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48902, %struct.ScmObj* %argslist56804$ae489000)
store volatile %struct.ScmObj* %argslist56804$ae489001, %struct.ScmObj** %stackaddr$prim58255, align 8
%stackaddr$prim58256 = alloca %struct.ScmObj*, align 8
%argslist56804$ae489002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48901, %struct.ScmObj* %argslist56804$ae489001)
store volatile %struct.ScmObj* %argslist56804$ae489002, %struct.ScmObj** %stackaddr$prim58256, align 8
%clofunc58257 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48900)
musttail call tailcc void %clofunc58257(%struct.ScmObj* %ae48900, %struct.ScmObj* %argslist56804$ae489002)
ret void
}

define tailcc void @proc_clo$ae48900(%struct.ScmObj* %env$ae48900,%struct.ScmObj* %current_45args56750) {
%stackaddr$env-ref58258 = alloca %struct.ScmObj*, align 8
%lsts47179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48900, i64 0)
store %struct.ScmObj* %lsts47179, %struct.ScmObj** %stackaddr$env-ref58258
%stackaddr$env-ref58259 = alloca %struct.ScmObj*, align 8
%_37foldr47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48900, i64 1)
store %struct.ScmObj* %_37foldr47099, %struct.ScmObj** %stackaddr$env-ref58259
%stackaddr$env-ref58260 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48900, i64 2)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref58260
%stackaddr$env-ref58261 = alloca %struct.ScmObj*, align 8
%_37map147125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48900, i64 3)
store %struct.ScmObj* %_37map147125, %struct.ScmObj** %stackaddr$env-ref58261
%stackaddr$env-ref58262 = alloca %struct.ScmObj*, align 8
%f47181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48900, i64 4)
store %struct.ScmObj* %f47181, %struct.ScmObj** %stackaddr$env-ref58262
%stackaddr$env-ref58263 = alloca %struct.ScmObj*, align 8
%acc47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48900, i64 5)
store %struct.ScmObj* %acc47180, %struct.ScmObj** %stackaddr$env-ref58263
%stackaddr$env-ref58264 = alloca %struct.ScmObj*, align 8
%k47482 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48900, i64 6)
store %struct.ScmObj* %k47482, %struct.ScmObj** %stackaddr$env-ref58264
%stackaddr$env-ref58265 = alloca %struct.ScmObj*, align 8
%_37foldl47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48900, i64 7)
store %struct.ScmObj* %_37foldl47177, %struct.ScmObj** %stackaddr$env-ref58265
%stackaddr$prim58266 = alloca %struct.ScmObj*, align 8
%_95k47483 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56750)
store volatile %struct.ScmObj* %_95k47483, %struct.ScmObj** %stackaddr$prim58266, align 8
%stackaddr$prim58267 = alloca %struct.ScmObj*, align 8
%current_45args56751 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56750)
store volatile %struct.ScmObj* %current_45args56751, %struct.ScmObj** %stackaddr$prim58267, align 8
%stackaddr$prim58268 = alloca %struct.ScmObj*, align 8
%anf_45bind47273 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56751)
store volatile %struct.ScmObj* %anf_45bind47273, %struct.ScmObj** %stackaddr$prim58268, align 8
%stackaddr$makeclosure58269 = alloca %struct.ScmObj*, align 8
%fptrToInt58270 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48932 to i64
%ae48932 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58270)
store volatile %struct.ScmObj* %ae48932, %struct.ScmObj** %stackaddr$makeclosure58269, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48932, %struct.ScmObj* %lsts47179, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48932, %struct.ScmObj* %_37foldr47099, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48932, %struct.ScmObj* %_37map147125, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48932, %struct.ScmObj* %f47181, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48932, %struct.ScmObj* %acc47180, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48932, %struct.ScmObj* %k47482, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48932, %struct.ScmObj* %_37foldl47177, i64 6)
%ae48934 = call %struct.ScmObj* @const_init_false()
%argslist56797$_37foldr1470940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58271 = alloca %struct.ScmObj*, align 8
%argslist56797$_37foldr1470941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47179, %struct.ScmObj* %argslist56797$_37foldr1470940)
store volatile %struct.ScmObj* %argslist56797$_37foldr1470941, %struct.ScmObj** %stackaddr$prim58271, align 8
%stackaddr$prim58272 = alloca %struct.ScmObj*, align 8
%argslist56797$_37foldr1470942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48934, %struct.ScmObj* %argslist56797$_37foldr1470941)
store volatile %struct.ScmObj* %argslist56797$_37foldr1470942, %struct.ScmObj** %stackaddr$prim58272, align 8
%stackaddr$prim58273 = alloca %struct.ScmObj*, align 8
%argslist56797$_37foldr1470943 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47273, %struct.ScmObj* %argslist56797$_37foldr1470942)
store volatile %struct.ScmObj* %argslist56797$_37foldr1470943, %struct.ScmObj** %stackaddr$prim58273, align 8
%stackaddr$prim58274 = alloca %struct.ScmObj*, align 8
%argslist56797$_37foldr1470944 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48932, %struct.ScmObj* %argslist56797$_37foldr1470943)
store volatile %struct.ScmObj* %argslist56797$_37foldr1470944, %struct.ScmObj** %stackaddr$prim58274, align 8
%clofunc58275 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147094)
musttail call tailcc void %clofunc58275(%struct.ScmObj* %_37foldr147094, %struct.ScmObj* %argslist56797$_37foldr1470944)
ret void
}

define tailcc void @proc_clo$ae48932(%struct.ScmObj* %env$ae48932,%struct.ScmObj* %current_45args56753) {
%stackaddr$env-ref58276 = alloca %struct.ScmObj*, align 8
%lsts47179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48932, i64 0)
store %struct.ScmObj* %lsts47179, %struct.ScmObj** %stackaddr$env-ref58276
%stackaddr$env-ref58277 = alloca %struct.ScmObj*, align 8
%_37foldr47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48932, i64 1)
store %struct.ScmObj* %_37foldr47099, %struct.ScmObj** %stackaddr$env-ref58277
%stackaddr$env-ref58278 = alloca %struct.ScmObj*, align 8
%_37map147125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48932, i64 2)
store %struct.ScmObj* %_37map147125, %struct.ScmObj** %stackaddr$env-ref58278
%stackaddr$env-ref58279 = alloca %struct.ScmObj*, align 8
%f47181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48932, i64 3)
store %struct.ScmObj* %f47181, %struct.ScmObj** %stackaddr$env-ref58279
%stackaddr$env-ref58280 = alloca %struct.ScmObj*, align 8
%acc47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48932, i64 4)
store %struct.ScmObj* %acc47180, %struct.ScmObj** %stackaddr$env-ref58280
%stackaddr$env-ref58281 = alloca %struct.ScmObj*, align 8
%k47482 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48932, i64 5)
store %struct.ScmObj* %k47482, %struct.ScmObj** %stackaddr$env-ref58281
%stackaddr$env-ref58282 = alloca %struct.ScmObj*, align 8
%_37foldl47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48932, i64 6)
store %struct.ScmObj* %_37foldl47177, %struct.ScmObj** %stackaddr$env-ref58282
%stackaddr$prim58283 = alloca %struct.ScmObj*, align 8
%_95k47484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56753)
store volatile %struct.ScmObj* %_95k47484, %struct.ScmObj** %stackaddr$prim58283, align 8
%stackaddr$prim58284 = alloca %struct.ScmObj*, align 8
%current_45args56754 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56753)
store volatile %struct.ScmObj* %current_45args56754, %struct.ScmObj** %stackaddr$prim58284, align 8
%stackaddr$prim58285 = alloca %struct.ScmObj*, align 8
%anf_45bind47274 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56754)
store volatile %struct.ScmObj* %anf_45bind47274, %struct.ScmObj** %stackaddr$prim58285, align 8
%truthy$cmp58286 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47274)
%cmp$cmp58286 = icmp eq i64 %truthy$cmp58286, 1
br i1 %cmp$cmp58286, label %truebranch$cmp58286, label %falsebranch$cmp58286
truebranch$cmp58286:
%ae48943 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56756$k474820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58287 = alloca %struct.ScmObj*, align 8
%argslist56756$k474821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47180, %struct.ScmObj* %argslist56756$k474820)
store volatile %struct.ScmObj* %argslist56756$k474821, %struct.ScmObj** %stackaddr$prim58287, align 8
%stackaddr$prim58288 = alloca %struct.ScmObj*, align 8
%argslist56756$k474822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48943, %struct.ScmObj* %argslist56756$k474821)
store volatile %struct.ScmObj* %argslist56756$k474822, %struct.ScmObj** %stackaddr$prim58288, align 8
%clofunc58289 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47482)
musttail call tailcc void %clofunc58289(%struct.ScmObj* %k47482, %struct.ScmObj* %argslist56756$k474822)
ret void
falsebranch$cmp58286:
%stackaddr$makeclosure58290 = alloca %struct.ScmObj*, align 8
%fptrToInt58291 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48948 to i64
%ae48948 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58291)
store volatile %struct.ScmObj* %ae48948, %struct.ScmObj** %stackaddr$makeclosure58290, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48948, %struct.ScmObj* %lsts47179, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48948, %struct.ScmObj* %_37foldr47099, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48948, %struct.ScmObj* %_37map147125, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48948, %struct.ScmObj* %f47181, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48948, %struct.ScmObj* %acc47180, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48948, %struct.ScmObj* %k47482, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48948, %struct.ScmObj* %_37foldl47177, i64 6)
%ae48949 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58292 = alloca %struct.ScmObj*, align 8
%fptrToInt58293 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48950 to i64
%ae48950 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58293)
store volatile %struct.ScmObj* %ae48950, %struct.ScmObj** %stackaddr$makeclosure58292, align 8
%argslist56796$ae489480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58294 = alloca %struct.ScmObj*, align 8
%argslist56796$ae489481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48950, %struct.ScmObj* %argslist56796$ae489480)
store volatile %struct.ScmObj* %argslist56796$ae489481, %struct.ScmObj** %stackaddr$prim58294, align 8
%stackaddr$prim58295 = alloca %struct.ScmObj*, align 8
%argslist56796$ae489482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48949, %struct.ScmObj* %argslist56796$ae489481)
store volatile %struct.ScmObj* %argslist56796$ae489482, %struct.ScmObj** %stackaddr$prim58295, align 8
%clofunc58296 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48948)
musttail call tailcc void %clofunc58296(%struct.ScmObj* %ae48948, %struct.ScmObj* %argslist56796$ae489482)
ret void
}

define tailcc void @proc_clo$ae48948(%struct.ScmObj* %env$ae48948,%struct.ScmObj* %current_45args56757) {
%stackaddr$env-ref58297 = alloca %struct.ScmObj*, align 8
%lsts47179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48948, i64 0)
store %struct.ScmObj* %lsts47179, %struct.ScmObj** %stackaddr$env-ref58297
%stackaddr$env-ref58298 = alloca %struct.ScmObj*, align 8
%_37foldr47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48948, i64 1)
store %struct.ScmObj* %_37foldr47099, %struct.ScmObj** %stackaddr$env-ref58298
%stackaddr$env-ref58299 = alloca %struct.ScmObj*, align 8
%_37map147125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48948, i64 2)
store %struct.ScmObj* %_37map147125, %struct.ScmObj** %stackaddr$env-ref58299
%stackaddr$env-ref58300 = alloca %struct.ScmObj*, align 8
%f47181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48948, i64 3)
store %struct.ScmObj* %f47181, %struct.ScmObj** %stackaddr$env-ref58300
%stackaddr$env-ref58301 = alloca %struct.ScmObj*, align 8
%acc47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48948, i64 4)
store %struct.ScmObj* %acc47180, %struct.ScmObj** %stackaddr$env-ref58301
%stackaddr$env-ref58302 = alloca %struct.ScmObj*, align 8
%k47482 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48948, i64 5)
store %struct.ScmObj* %k47482, %struct.ScmObj** %stackaddr$env-ref58302
%stackaddr$env-ref58303 = alloca %struct.ScmObj*, align 8
%_37foldl47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48948, i64 6)
store %struct.ScmObj* %_37foldl47177, %struct.ScmObj** %stackaddr$env-ref58303
%stackaddr$prim58304 = alloca %struct.ScmObj*, align 8
%_95k47485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56757)
store volatile %struct.ScmObj* %_95k47485, %struct.ScmObj** %stackaddr$prim58304, align 8
%stackaddr$prim58305 = alloca %struct.ScmObj*, align 8
%current_45args56758 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56757)
store volatile %struct.ScmObj* %current_45args56758, %struct.ScmObj** %stackaddr$prim58305, align 8
%stackaddr$prim58306 = alloca %struct.ScmObj*, align 8
%anf_45bind47275 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56758)
store volatile %struct.ScmObj* %anf_45bind47275, %struct.ScmObj** %stackaddr$prim58306, align 8
%stackaddr$makeclosure58307 = alloca %struct.ScmObj*, align 8
%fptrToInt58308 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48969 to i64
%ae48969 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58308)
store volatile %struct.ScmObj* %ae48969, %struct.ScmObj** %stackaddr$makeclosure58307, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48969, %struct.ScmObj* %lsts47179, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48969, %struct.ScmObj* %_37foldr47099, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48969, %struct.ScmObj* %_37map147125, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48969, %struct.ScmObj* %f47181, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48969, %struct.ScmObj* %acc47180, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48969, %struct.ScmObj* %k47482, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48969, %struct.ScmObj* %_37foldl47177, i64 6)
%argslist56791$_37map1471250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58309 = alloca %struct.ScmObj*, align 8
%argslist56791$_37map1471251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47179, %struct.ScmObj* %argslist56791$_37map1471250)
store volatile %struct.ScmObj* %argslist56791$_37map1471251, %struct.ScmObj** %stackaddr$prim58309, align 8
%stackaddr$prim58310 = alloca %struct.ScmObj*, align 8
%argslist56791$_37map1471252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47275, %struct.ScmObj* %argslist56791$_37map1471251)
store volatile %struct.ScmObj* %argslist56791$_37map1471252, %struct.ScmObj** %stackaddr$prim58310, align 8
%stackaddr$prim58311 = alloca %struct.ScmObj*, align 8
%argslist56791$_37map1471253 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48969, %struct.ScmObj* %argslist56791$_37map1471252)
store volatile %struct.ScmObj* %argslist56791$_37map1471253, %struct.ScmObj** %stackaddr$prim58311, align 8
%clofunc58312 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147125)
musttail call tailcc void %clofunc58312(%struct.ScmObj* %_37map147125, %struct.ScmObj* %argslist56791$_37map1471253)
ret void
}

define tailcc void @proc_clo$ae48969(%struct.ScmObj* %env$ae48969,%struct.ScmObj* %current_45args56760) {
%stackaddr$env-ref58313 = alloca %struct.ScmObj*, align 8
%lsts47179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48969, i64 0)
store %struct.ScmObj* %lsts47179, %struct.ScmObj** %stackaddr$env-ref58313
%stackaddr$env-ref58314 = alloca %struct.ScmObj*, align 8
%_37foldr47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48969, i64 1)
store %struct.ScmObj* %_37foldr47099, %struct.ScmObj** %stackaddr$env-ref58314
%stackaddr$env-ref58315 = alloca %struct.ScmObj*, align 8
%_37map147125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48969, i64 2)
store %struct.ScmObj* %_37map147125, %struct.ScmObj** %stackaddr$env-ref58315
%stackaddr$env-ref58316 = alloca %struct.ScmObj*, align 8
%f47181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48969, i64 3)
store %struct.ScmObj* %f47181, %struct.ScmObj** %stackaddr$env-ref58316
%stackaddr$env-ref58317 = alloca %struct.ScmObj*, align 8
%acc47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48969, i64 4)
store %struct.ScmObj* %acc47180, %struct.ScmObj** %stackaddr$env-ref58317
%stackaddr$env-ref58318 = alloca %struct.ScmObj*, align 8
%k47482 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48969, i64 5)
store %struct.ScmObj* %k47482, %struct.ScmObj** %stackaddr$env-ref58318
%stackaddr$env-ref58319 = alloca %struct.ScmObj*, align 8
%_37foldl47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48969, i64 6)
store %struct.ScmObj* %_37foldl47177, %struct.ScmObj** %stackaddr$env-ref58319
%stackaddr$prim58320 = alloca %struct.ScmObj*, align 8
%_95k47486 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56760)
store volatile %struct.ScmObj* %_95k47486, %struct.ScmObj** %stackaddr$prim58320, align 8
%stackaddr$prim58321 = alloca %struct.ScmObj*, align 8
%current_45args56761 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56760)
store volatile %struct.ScmObj* %current_45args56761, %struct.ScmObj** %stackaddr$prim58321, align 8
%stackaddr$prim58322 = alloca %struct.ScmObj*, align 8
%lsts_4347186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56761)
store volatile %struct.ScmObj* %lsts_4347186, %struct.ScmObj** %stackaddr$prim58322, align 8
%stackaddr$makeclosure58323 = alloca %struct.ScmObj*, align 8
%fptrToInt58324 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48972 to i64
%ae48972 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt58324)
store volatile %struct.ScmObj* %ae48972, %struct.ScmObj** %stackaddr$makeclosure58323, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48972, %struct.ScmObj* %lsts47179, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48972, %struct.ScmObj* %_37foldr47099, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48972, %struct.ScmObj* %_37map147125, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48972, %struct.ScmObj* %lsts_4347186, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48972, %struct.ScmObj* %f47181, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48972, %struct.ScmObj* %acc47180, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48972, %struct.ScmObj* %k47482, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48972, %struct.ScmObj* %_37foldl47177, i64 7)
%ae48973 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58325 = alloca %struct.ScmObj*, align 8
%fptrToInt58326 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48974 to i64
%ae48974 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58326)
store volatile %struct.ScmObj* %ae48974, %struct.ScmObj** %stackaddr$makeclosure58325, align 8
%argslist56790$ae489720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58327 = alloca %struct.ScmObj*, align 8
%argslist56790$ae489721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48974, %struct.ScmObj* %argslist56790$ae489720)
store volatile %struct.ScmObj* %argslist56790$ae489721, %struct.ScmObj** %stackaddr$prim58327, align 8
%stackaddr$prim58328 = alloca %struct.ScmObj*, align 8
%argslist56790$ae489722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48973, %struct.ScmObj* %argslist56790$ae489721)
store volatile %struct.ScmObj* %argslist56790$ae489722, %struct.ScmObj** %stackaddr$prim58328, align 8
%clofunc58329 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48972)
musttail call tailcc void %clofunc58329(%struct.ScmObj* %ae48972, %struct.ScmObj* %argslist56790$ae489722)
ret void
}

define tailcc void @proc_clo$ae48972(%struct.ScmObj* %env$ae48972,%struct.ScmObj* %current_45args56763) {
%stackaddr$env-ref58330 = alloca %struct.ScmObj*, align 8
%lsts47179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48972, i64 0)
store %struct.ScmObj* %lsts47179, %struct.ScmObj** %stackaddr$env-ref58330
%stackaddr$env-ref58331 = alloca %struct.ScmObj*, align 8
%_37foldr47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48972, i64 1)
store %struct.ScmObj* %_37foldr47099, %struct.ScmObj** %stackaddr$env-ref58331
%stackaddr$env-ref58332 = alloca %struct.ScmObj*, align 8
%_37map147125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48972, i64 2)
store %struct.ScmObj* %_37map147125, %struct.ScmObj** %stackaddr$env-ref58332
%stackaddr$env-ref58333 = alloca %struct.ScmObj*, align 8
%lsts_4347186 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48972, i64 3)
store %struct.ScmObj* %lsts_4347186, %struct.ScmObj** %stackaddr$env-ref58333
%stackaddr$env-ref58334 = alloca %struct.ScmObj*, align 8
%f47181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48972, i64 4)
store %struct.ScmObj* %f47181, %struct.ScmObj** %stackaddr$env-ref58334
%stackaddr$env-ref58335 = alloca %struct.ScmObj*, align 8
%acc47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48972, i64 5)
store %struct.ScmObj* %acc47180, %struct.ScmObj** %stackaddr$env-ref58335
%stackaddr$env-ref58336 = alloca %struct.ScmObj*, align 8
%k47482 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48972, i64 6)
store %struct.ScmObj* %k47482, %struct.ScmObj** %stackaddr$env-ref58336
%stackaddr$env-ref58337 = alloca %struct.ScmObj*, align 8
%_37foldl47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48972, i64 7)
store %struct.ScmObj* %_37foldl47177, %struct.ScmObj** %stackaddr$env-ref58337
%stackaddr$prim58338 = alloca %struct.ScmObj*, align 8
%_95k47487 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56763)
store volatile %struct.ScmObj* %_95k47487, %struct.ScmObj** %stackaddr$prim58338, align 8
%stackaddr$prim58339 = alloca %struct.ScmObj*, align 8
%current_45args56764 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56763)
store volatile %struct.ScmObj* %current_45args56764, %struct.ScmObj** %stackaddr$prim58339, align 8
%stackaddr$prim58340 = alloca %struct.ScmObj*, align 8
%anf_45bind47276 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56764)
store volatile %struct.ScmObj* %anf_45bind47276, %struct.ScmObj** %stackaddr$prim58340, align 8
%stackaddr$makeclosure58341 = alloca %struct.ScmObj*, align 8
%fptrToInt58342 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48993 to i64
%ae48993 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt58342)
store volatile %struct.ScmObj* %ae48993, %struct.ScmObj** %stackaddr$makeclosure58341, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48993, %struct.ScmObj* %lsts_4347186, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48993, %struct.ScmObj* %f47181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48993, %struct.ScmObj* %acc47180, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48993, %struct.ScmObj* %_37foldr47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48993, %struct.ScmObj* %k47482, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48993, %struct.ScmObj* %_37foldl47177, i64 5)
%argslist56785$_37map1471250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58343 = alloca %struct.ScmObj*, align 8
%argslist56785$_37map1471251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47179, %struct.ScmObj* %argslist56785$_37map1471250)
store volatile %struct.ScmObj* %argslist56785$_37map1471251, %struct.ScmObj** %stackaddr$prim58343, align 8
%stackaddr$prim58344 = alloca %struct.ScmObj*, align 8
%argslist56785$_37map1471252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47276, %struct.ScmObj* %argslist56785$_37map1471251)
store volatile %struct.ScmObj* %argslist56785$_37map1471252, %struct.ScmObj** %stackaddr$prim58344, align 8
%stackaddr$prim58345 = alloca %struct.ScmObj*, align 8
%argslist56785$_37map1471253 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48993, %struct.ScmObj* %argslist56785$_37map1471252)
store volatile %struct.ScmObj* %argslist56785$_37map1471253, %struct.ScmObj** %stackaddr$prim58345, align 8
%clofunc58346 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147125)
musttail call tailcc void %clofunc58346(%struct.ScmObj* %_37map147125, %struct.ScmObj* %argslist56785$_37map1471253)
ret void
}

define tailcc void @proc_clo$ae48993(%struct.ScmObj* %env$ae48993,%struct.ScmObj* %current_45args56766) {
%stackaddr$env-ref58347 = alloca %struct.ScmObj*, align 8
%lsts_4347186 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48993, i64 0)
store %struct.ScmObj* %lsts_4347186, %struct.ScmObj** %stackaddr$env-ref58347
%stackaddr$env-ref58348 = alloca %struct.ScmObj*, align 8
%f47181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48993, i64 1)
store %struct.ScmObj* %f47181, %struct.ScmObj** %stackaddr$env-ref58348
%stackaddr$env-ref58349 = alloca %struct.ScmObj*, align 8
%acc47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48993, i64 2)
store %struct.ScmObj* %acc47180, %struct.ScmObj** %stackaddr$env-ref58349
%stackaddr$env-ref58350 = alloca %struct.ScmObj*, align 8
%_37foldr47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48993, i64 3)
store %struct.ScmObj* %_37foldr47099, %struct.ScmObj** %stackaddr$env-ref58350
%stackaddr$env-ref58351 = alloca %struct.ScmObj*, align 8
%k47482 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48993, i64 4)
store %struct.ScmObj* %k47482, %struct.ScmObj** %stackaddr$env-ref58351
%stackaddr$env-ref58352 = alloca %struct.ScmObj*, align 8
%_37foldl47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48993, i64 5)
store %struct.ScmObj* %_37foldl47177, %struct.ScmObj** %stackaddr$env-ref58352
%stackaddr$prim58353 = alloca %struct.ScmObj*, align 8
%_95k47488 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56766)
store volatile %struct.ScmObj* %_95k47488, %struct.ScmObj** %stackaddr$prim58353, align 8
%stackaddr$prim58354 = alloca %struct.ScmObj*, align 8
%current_45args56767 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56766)
store volatile %struct.ScmObj* %current_45args56767, %struct.ScmObj** %stackaddr$prim58354, align 8
%stackaddr$prim58355 = alloca %struct.ScmObj*, align 8
%vs47184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56767)
store volatile %struct.ScmObj* %vs47184, %struct.ScmObj** %stackaddr$prim58355, align 8
%stackaddr$makeclosure58356 = alloca %struct.ScmObj*, align 8
%fptrToInt58357 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48996 to i64
%ae48996 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58357)
store volatile %struct.ScmObj* %ae48996, %struct.ScmObj** %stackaddr$makeclosure58356, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48996, %struct.ScmObj* %lsts_4347186, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48996, %struct.ScmObj* %vs47184, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48996, %struct.ScmObj* %f47181, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48996, %struct.ScmObj* %acc47180, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48996, %struct.ScmObj* %_37foldr47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48996, %struct.ScmObj* %k47482, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48996, %struct.ScmObj* %_37foldl47177, i64 6)
%ae48997 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58358 = alloca %struct.ScmObj*, align 8
%fptrToInt58359 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48998 to i64
%ae48998 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58359)
store volatile %struct.ScmObj* %ae48998, %struct.ScmObj** %stackaddr$makeclosure58358, align 8
%argslist56784$ae489960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58360 = alloca %struct.ScmObj*, align 8
%argslist56784$ae489961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48998, %struct.ScmObj* %argslist56784$ae489960)
store volatile %struct.ScmObj* %argslist56784$ae489961, %struct.ScmObj** %stackaddr$prim58360, align 8
%stackaddr$prim58361 = alloca %struct.ScmObj*, align 8
%argslist56784$ae489962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48997, %struct.ScmObj* %argslist56784$ae489961)
store volatile %struct.ScmObj* %argslist56784$ae489962, %struct.ScmObj** %stackaddr$prim58361, align 8
%clofunc58362 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48996)
musttail call tailcc void %clofunc58362(%struct.ScmObj* %ae48996, %struct.ScmObj* %argslist56784$ae489962)
ret void
}

define tailcc void @proc_clo$ae48996(%struct.ScmObj* %env$ae48996,%struct.ScmObj* %current_45args56769) {
%stackaddr$env-ref58363 = alloca %struct.ScmObj*, align 8
%lsts_4347186 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48996, i64 0)
store %struct.ScmObj* %lsts_4347186, %struct.ScmObj** %stackaddr$env-ref58363
%stackaddr$env-ref58364 = alloca %struct.ScmObj*, align 8
%vs47184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48996, i64 1)
store %struct.ScmObj* %vs47184, %struct.ScmObj** %stackaddr$env-ref58364
%stackaddr$env-ref58365 = alloca %struct.ScmObj*, align 8
%f47181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48996, i64 2)
store %struct.ScmObj* %f47181, %struct.ScmObj** %stackaddr$env-ref58365
%stackaddr$env-ref58366 = alloca %struct.ScmObj*, align 8
%acc47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48996, i64 3)
store %struct.ScmObj* %acc47180, %struct.ScmObj** %stackaddr$env-ref58366
%stackaddr$env-ref58367 = alloca %struct.ScmObj*, align 8
%_37foldr47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48996, i64 4)
store %struct.ScmObj* %_37foldr47099, %struct.ScmObj** %stackaddr$env-ref58367
%stackaddr$env-ref58368 = alloca %struct.ScmObj*, align 8
%k47482 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48996, i64 5)
store %struct.ScmObj* %k47482, %struct.ScmObj** %stackaddr$env-ref58368
%stackaddr$env-ref58369 = alloca %struct.ScmObj*, align 8
%_37foldl47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48996, i64 6)
store %struct.ScmObj* %_37foldl47177, %struct.ScmObj** %stackaddr$env-ref58369
%stackaddr$prim58370 = alloca %struct.ScmObj*, align 8
%_95k47489 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56769)
store volatile %struct.ScmObj* %_95k47489, %struct.ScmObj** %stackaddr$prim58370, align 8
%stackaddr$prim58371 = alloca %struct.ScmObj*, align 8
%current_45args56770 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56769)
store volatile %struct.ScmObj* %current_45args56770, %struct.ScmObj** %stackaddr$prim58371, align 8
%stackaddr$prim58372 = alloca %struct.ScmObj*, align 8
%anf_45bind47277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56770)
store volatile %struct.ScmObj* %anf_45bind47277, %struct.ScmObj** %stackaddr$prim58372, align 8
%ae49019 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58373 = alloca %struct.ScmObj*, align 8
%anf_45bind47278 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47180, %struct.ScmObj* %ae49019)
store volatile %struct.ScmObj* %anf_45bind47278, %struct.ScmObj** %stackaddr$prim58373, align 8
%stackaddr$makeclosure58374 = alloca %struct.ScmObj*, align 8
%fptrToInt58375 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49021 to i64
%ae49021 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58375)
store volatile %struct.ScmObj* %ae49021, %struct.ScmObj** %stackaddr$makeclosure58374, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49021, %struct.ScmObj* %lsts_4347186, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49021, %struct.ScmObj* %f47181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49021, %struct.ScmObj* %k47482, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49021, %struct.ScmObj* %_37foldl47177, i64 3)
%argslist56778$_37foldr470990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58376 = alloca %struct.ScmObj*, align 8
%argslist56778$_37foldr470991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47184, %struct.ScmObj* %argslist56778$_37foldr470990)
store volatile %struct.ScmObj* %argslist56778$_37foldr470991, %struct.ScmObj** %stackaddr$prim58376, align 8
%stackaddr$prim58377 = alloca %struct.ScmObj*, align 8
%argslist56778$_37foldr470992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47278, %struct.ScmObj* %argslist56778$_37foldr470991)
store volatile %struct.ScmObj* %argslist56778$_37foldr470992, %struct.ScmObj** %stackaddr$prim58377, align 8
%stackaddr$prim58378 = alloca %struct.ScmObj*, align 8
%argslist56778$_37foldr470993 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47277, %struct.ScmObj* %argslist56778$_37foldr470992)
store volatile %struct.ScmObj* %argslist56778$_37foldr470993, %struct.ScmObj** %stackaddr$prim58378, align 8
%stackaddr$prim58379 = alloca %struct.ScmObj*, align 8
%argslist56778$_37foldr470994 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49021, %struct.ScmObj* %argslist56778$_37foldr470993)
store volatile %struct.ScmObj* %argslist56778$_37foldr470994, %struct.ScmObj** %stackaddr$prim58379, align 8
%clofunc58380 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47099)
musttail call tailcc void %clofunc58380(%struct.ScmObj* %_37foldr47099, %struct.ScmObj* %argslist56778$_37foldr470994)
ret void
}

define tailcc void @proc_clo$ae49021(%struct.ScmObj* %env$ae49021,%struct.ScmObj* %current_45args56772) {
%stackaddr$env-ref58381 = alloca %struct.ScmObj*, align 8
%lsts_4347186 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49021, i64 0)
store %struct.ScmObj* %lsts_4347186, %struct.ScmObj** %stackaddr$env-ref58381
%stackaddr$env-ref58382 = alloca %struct.ScmObj*, align 8
%f47181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49021, i64 1)
store %struct.ScmObj* %f47181, %struct.ScmObj** %stackaddr$env-ref58382
%stackaddr$env-ref58383 = alloca %struct.ScmObj*, align 8
%k47482 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49021, i64 2)
store %struct.ScmObj* %k47482, %struct.ScmObj** %stackaddr$env-ref58383
%stackaddr$env-ref58384 = alloca %struct.ScmObj*, align 8
%_37foldl47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49021, i64 3)
store %struct.ScmObj* %_37foldl47177, %struct.ScmObj** %stackaddr$env-ref58384
%stackaddr$prim58385 = alloca %struct.ScmObj*, align 8
%_95k47490 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56772)
store volatile %struct.ScmObj* %_95k47490, %struct.ScmObj** %stackaddr$prim58385, align 8
%stackaddr$prim58386 = alloca %struct.ScmObj*, align 8
%current_45args56773 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56772)
store volatile %struct.ScmObj* %current_45args56773, %struct.ScmObj** %stackaddr$prim58386, align 8
%stackaddr$prim58387 = alloca %struct.ScmObj*, align 8
%anf_45bind47279 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56773)
store volatile %struct.ScmObj* %anf_45bind47279, %struct.ScmObj** %stackaddr$prim58387, align 8
%stackaddr$makeclosure58388 = alloca %struct.ScmObj*, align 8
%fptrToInt58389 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49025 to i64
%ae49025 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58389)
store volatile %struct.ScmObj* %ae49025, %struct.ScmObj** %stackaddr$makeclosure58388, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49025, %struct.ScmObj* %lsts_4347186, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49025, %struct.ScmObj* %f47181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49025, %struct.ScmObj* %k47482, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49025, %struct.ScmObj* %_37foldl47177, i64 3)
%stackaddr$prim58390 = alloca %struct.ScmObj*, align 8
%cpsargs47493 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49025, %struct.ScmObj* %anf_45bind47279)
store volatile %struct.ScmObj* %cpsargs47493, %struct.ScmObj** %stackaddr$prim58390, align 8
%clofunc58391 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47181)
musttail call tailcc void %clofunc58391(%struct.ScmObj* %f47181, %struct.ScmObj* %cpsargs47493)
ret void
}

define tailcc void @proc_clo$ae49025(%struct.ScmObj* %env$ae49025,%struct.ScmObj* %current_45args56775) {
%stackaddr$env-ref58392 = alloca %struct.ScmObj*, align 8
%lsts_4347186 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49025, i64 0)
store %struct.ScmObj* %lsts_4347186, %struct.ScmObj** %stackaddr$env-ref58392
%stackaddr$env-ref58393 = alloca %struct.ScmObj*, align 8
%f47181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49025, i64 1)
store %struct.ScmObj* %f47181, %struct.ScmObj** %stackaddr$env-ref58393
%stackaddr$env-ref58394 = alloca %struct.ScmObj*, align 8
%k47482 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49025, i64 2)
store %struct.ScmObj* %k47482, %struct.ScmObj** %stackaddr$env-ref58394
%stackaddr$env-ref58395 = alloca %struct.ScmObj*, align 8
%_37foldl47177 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49025, i64 3)
store %struct.ScmObj* %_37foldl47177, %struct.ScmObj** %stackaddr$env-ref58395
%stackaddr$prim58396 = alloca %struct.ScmObj*, align 8
%_95k47491 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56775)
store volatile %struct.ScmObj* %_95k47491, %struct.ScmObj** %stackaddr$prim58396, align 8
%stackaddr$prim58397 = alloca %struct.ScmObj*, align 8
%current_45args56776 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56775)
store volatile %struct.ScmObj* %current_45args56776, %struct.ScmObj** %stackaddr$prim58397, align 8
%stackaddr$prim58398 = alloca %struct.ScmObj*, align 8
%acc_4347188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56776)
store volatile %struct.ScmObj* %acc_4347188, %struct.ScmObj** %stackaddr$prim58398, align 8
%stackaddr$prim58399 = alloca %struct.ScmObj*, align 8
%anf_45bind47280 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4347188, %struct.ScmObj* %lsts_4347186)
store volatile %struct.ScmObj* %anf_45bind47280, %struct.ScmObj** %stackaddr$prim58399, align 8
%stackaddr$prim58400 = alloca %struct.ScmObj*, align 8
%anf_45bind47281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47181, %struct.ScmObj* %anf_45bind47280)
store volatile %struct.ScmObj* %anf_45bind47281, %struct.ScmObj** %stackaddr$prim58400, align 8
%stackaddr$prim58401 = alloca %struct.ScmObj*, align 8
%cpsargs47492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47482, %struct.ScmObj* %anf_45bind47281)
store volatile %struct.ScmObj* %cpsargs47492, %struct.ScmObj** %stackaddr$prim58401, align 8
%clofunc58402 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl47177)
musttail call tailcc void %clofunc58402(%struct.ScmObj* %_37foldl47177, %struct.ScmObj* %cpsargs47492)
ret void
}

define tailcc void @proc_clo$ae48998(%struct.ScmObj* %env$ae48998,%struct.ScmObj* %current_45args56779) {
%stackaddr$prim58403 = alloca %struct.ScmObj*, align 8
%k47494 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56779)
store volatile %struct.ScmObj* %k47494, %struct.ScmObj** %stackaddr$prim58403, align 8
%stackaddr$prim58404 = alloca %struct.ScmObj*, align 8
%current_45args56780 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56779)
store volatile %struct.ScmObj* %current_45args56780, %struct.ScmObj** %stackaddr$prim58404, align 8
%stackaddr$prim58405 = alloca %struct.ScmObj*, align 8
%a47190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56780)
store volatile %struct.ScmObj* %a47190, %struct.ScmObj** %stackaddr$prim58405, align 8
%stackaddr$prim58406 = alloca %struct.ScmObj*, align 8
%current_45args56781 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56780)
store volatile %struct.ScmObj* %current_45args56781, %struct.ScmObj** %stackaddr$prim58406, align 8
%stackaddr$prim58407 = alloca %struct.ScmObj*, align 8
%b47189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56781)
store volatile %struct.ScmObj* %b47189, %struct.ScmObj** %stackaddr$prim58407, align 8
%stackaddr$prim58408 = alloca %struct.ScmObj*, align 8
%cpsprim47495 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47190, %struct.ScmObj* %b47189)
store volatile %struct.ScmObj* %cpsprim47495, %struct.ScmObj** %stackaddr$prim58408, align 8
%ae49002 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56783$k474940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58409 = alloca %struct.ScmObj*, align 8
%argslist56783$k474941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47495, %struct.ScmObj* %argslist56783$k474940)
store volatile %struct.ScmObj* %argslist56783$k474941, %struct.ScmObj** %stackaddr$prim58409, align 8
%stackaddr$prim58410 = alloca %struct.ScmObj*, align 8
%argslist56783$k474942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49002, %struct.ScmObj* %argslist56783$k474941)
store volatile %struct.ScmObj* %argslist56783$k474942, %struct.ScmObj** %stackaddr$prim58410, align 8
%clofunc58411 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47494)
musttail call tailcc void %clofunc58411(%struct.ScmObj* %k47494, %struct.ScmObj* %argslist56783$k474942)
ret void
}

define tailcc void @proc_clo$ae48974(%struct.ScmObj* %env$ae48974,%struct.ScmObj* %current_45args56786) {
%stackaddr$prim58412 = alloca %struct.ScmObj*, align 8
%k47496 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56786)
store volatile %struct.ScmObj* %k47496, %struct.ScmObj** %stackaddr$prim58412, align 8
%stackaddr$prim58413 = alloca %struct.ScmObj*, align 8
%current_45args56787 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56786)
store volatile %struct.ScmObj* %current_45args56787, %struct.ScmObj** %stackaddr$prim58413, align 8
%stackaddr$prim58414 = alloca %struct.ScmObj*, align 8
%x47185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56787)
store volatile %struct.ScmObj* %x47185, %struct.ScmObj** %stackaddr$prim58414, align 8
%stackaddr$prim58415 = alloca %struct.ScmObj*, align 8
%cpsprim47497 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47185)
store volatile %struct.ScmObj* %cpsprim47497, %struct.ScmObj** %stackaddr$prim58415, align 8
%ae48977 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56789$k474960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58416 = alloca %struct.ScmObj*, align 8
%argslist56789$k474961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47497, %struct.ScmObj* %argslist56789$k474960)
store volatile %struct.ScmObj* %argslist56789$k474961, %struct.ScmObj** %stackaddr$prim58416, align 8
%stackaddr$prim58417 = alloca %struct.ScmObj*, align 8
%argslist56789$k474962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48977, %struct.ScmObj* %argslist56789$k474961)
store volatile %struct.ScmObj* %argslist56789$k474962, %struct.ScmObj** %stackaddr$prim58417, align 8
%clofunc58418 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47496)
musttail call tailcc void %clofunc58418(%struct.ScmObj* %k47496, %struct.ScmObj* %argslist56789$k474962)
ret void
}

define tailcc void @proc_clo$ae48950(%struct.ScmObj* %env$ae48950,%struct.ScmObj* %current_45args56792) {
%stackaddr$prim58419 = alloca %struct.ScmObj*, align 8
%k47498 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56792)
store volatile %struct.ScmObj* %k47498, %struct.ScmObj** %stackaddr$prim58419, align 8
%stackaddr$prim58420 = alloca %struct.ScmObj*, align 8
%current_45args56793 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56792)
store volatile %struct.ScmObj* %current_45args56793, %struct.ScmObj** %stackaddr$prim58420, align 8
%stackaddr$prim58421 = alloca %struct.ScmObj*, align 8
%x47187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56793)
store volatile %struct.ScmObj* %x47187, %struct.ScmObj** %stackaddr$prim58421, align 8
%stackaddr$prim58422 = alloca %struct.ScmObj*, align 8
%cpsprim47499 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47187)
store volatile %struct.ScmObj* %cpsprim47499, %struct.ScmObj** %stackaddr$prim58422, align 8
%ae48953 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56795$k474980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58423 = alloca %struct.ScmObj*, align 8
%argslist56795$k474981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47499, %struct.ScmObj* %argslist56795$k474980)
store volatile %struct.ScmObj* %argslist56795$k474981, %struct.ScmObj** %stackaddr$prim58423, align 8
%stackaddr$prim58424 = alloca %struct.ScmObj*, align 8
%argslist56795$k474982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48953, %struct.ScmObj* %argslist56795$k474981)
store volatile %struct.ScmObj* %argslist56795$k474982, %struct.ScmObj** %stackaddr$prim58424, align 8
%clofunc58425 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47498)
musttail call tailcc void %clofunc58425(%struct.ScmObj* %k47498, %struct.ScmObj* %argslist56795$k474982)
ret void
}

define tailcc void @proc_clo$ae48902(%struct.ScmObj* %env$ae48902,%struct.ScmObj* %current_45args56798) {
%stackaddr$prim58426 = alloca %struct.ScmObj*, align 8
%k47500 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56798)
store volatile %struct.ScmObj* %k47500, %struct.ScmObj** %stackaddr$prim58426, align 8
%stackaddr$prim58427 = alloca %struct.ScmObj*, align 8
%current_45args56799 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56798)
store volatile %struct.ScmObj* %current_45args56799, %struct.ScmObj** %stackaddr$prim58427, align 8
%stackaddr$prim58428 = alloca %struct.ScmObj*, align 8
%lst47183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56799)
store volatile %struct.ScmObj* %lst47183, %struct.ScmObj** %stackaddr$prim58428, align 8
%stackaddr$prim58429 = alloca %struct.ScmObj*, align 8
%current_45args56800 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56799)
store volatile %struct.ScmObj* %current_45args56800, %struct.ScmObj** %stackaddr$prim58429, align 8
%stackaddr$prim58430 = alloca %struct.ScmObj*, align 8
%b47182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56800)
store volatile %struct.ScmObj* %b47182, %struct.ScmObj** %stackaddr$prim58430, align 8
%truthy$cmp58431 = call i64 @is_truthy_value(%struct.ScmObj* %b47182)
%cmp$cmp58431 = icmp eq i64 %truthy$cmp58431, 1
br i1 %cmp$cmp58431, label %truebranch$cmp58431, label %falsebranch$cmp58431
truebranch$cmp58431:
%ae48905 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56802$k475000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58432 = alloca %struct.ScmObj*, align 8
%argslist56802$k475001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47182, %struct.ScmObj* %argslist56802$k475000)
store volatile %struct.ScmObj* %argslist56802$k475001, %struct.ScmObj** %stackaddr$prim58432, align 8
%stackaddr$prim58433 = alloca %struct.ScmObj*, align 8
%argslist56802$k475002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48905, %struct.ScmObj* %argslist56802$k475001)
store volatile %struct.ScmObj* %argslist56802$k475002, %struct.ScmObj** %stackaddr$prim58433, align 8
%clofunc58434 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47500)
musttail call tailcc void %clofunc58434(%struct.ScmObj* %k47500, %struct.ScmObj* %argslist56802$k475002)
ret void
falsebranch$cmp58431:
%stackaddr$prim58435 = alloca %struct.ScmObj*, align 8
%cpsprim47501 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47183)
store volatile %struct.ScmObj* %cpsprim47501, %struct.ScmObj** %stackaddr$prim58435, align 8
%ae48912 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56803$k475000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58436 = alloca %struct.ScmObj*, align 8
%argslist56803$k475001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47501, %struct.ScmObj* %argslist56803$k475000)
store volatile %struct.ScmObj* %argslist56803$k475001, %struct.ScmObj** %stackaddr$prim58436, align 8
%stackaddr$prim58437 = alloca %struct.ScmObj*, align 8
%argslist56803$k475002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48912, %struct.ScmObj* %argslist56803$k475001)
store volatile %struct.ScmObj* %argslist56803$k475002, %struct.ScmObj** %stackaddr$prim58437, align 8
%clofunc58438 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47500)
musttail call tailcc void %clofunc58438(%struct.ScmObj* %k47500, %struct.ScmObj* %argslist56803$k475002)
ret void
}

define tailcc void @proc_clo$ae48743(%struct.ScmObj* %env$ae48743,%struct.ScmObj* %args4712147502) {
%stackaddr$env-ref58439 = alloca %struct.ScmObj*, align 8
%_37last47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48743, i64 0)
store %struct.ScmObj* %_37last47116, %struct.ScmObj** %stackaddr$env-ref58439
%stackaddr$env-ref58440 = alloca %struct.ScmObj*, align 8
%_37foldr47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48743, i64 1)
store %struct.ScmObj* %_37foldr47099, %struct.ScmObj** %stackaddr$env-ref58440
%stackaddr$env-ref58441 = alloca %struct.ScmObj*, align 8
%_37drop_45right47113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48743, i64 2)
store %struct.ScmObj* %_37drop_45right47113, %struct.ScmObj** %stackaddr$env-ref58441
%stackaddr$prim58442 = alloca %struct.ScmObj*, align 8
%k47503 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4712147502)
store volatile %struct.ScmObj* %k47503, %struct.ScmObj** %stackaddr$prim58442, align 8
%stackaddr$prim58443 = alloca %struct.ScmObj*, align 8
%args47121 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4712147502)
store volatile %struct.ScmObj* %args47121, %struct.ScmObj** %stackaddr$prim58443, align 8
%stackaddr$prim58444 = alloca %struct.ScmObj*, align 8
%f47123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47121)
store volatile %struct.ScmObj* %f47123, %struct.ScmObj** %stackaddr$prim58444, align 8
%stackaddr$prim58445 = alloca %struct.ScmObj*, align 8
%lsts47122 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47121)
store volatile %struct.ScmObj* %lsts47122, %struct.ScmObj** %stackaddr$prim58445, align 8
%stackaddr$makeclosure58446 = alloca %struct.ScmObj*, align 8
%fptrToInt58447 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48748 to i64
%ae48748 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58447)
store volatile %struct.ScmObj* %ae48748, %struct.ScmObj** %stackaddr$makeclosure58446, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48748, %struct.ScmObj* %lsts47122, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48748, %struct.ScmObj* %k47503, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48748, %struct.ScmObj* %_37foldr47099, i64 2)
%ae48749 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58448 = alloca %struct.ScmObj*, align 8
%fptrToInt58449 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48750 to i64
%ae48750 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58449)
store volatile %struct.ScmObj* %ae48750, %struct.ScmObj** %stackaddr$makeclosure58448, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48750, %struct.ScmObj* %f47123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48750, %struct.ScmObj* %_37last47116, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48750, %struct.ScmObj* %_37drop_45right47113, i64 2)
%argslist56822$ae487480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58450 = alloca %struct.ScmObj*, align 8
%argslist56822$ae487481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48750, %struct.ScmObj* %argslist56822$ae487480)
store volatile %struct.ScmObj* %argslist56822$ae487481, %struct.ScmObj** %stackaddr$prim58450, align 8
%stackaddr$prim58451 = alloca %struct.ScmObj*, align 8
%argslist56822$ae487482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48749, %struct.ScmObj* %argslist56822$ae487481)
store volatile %struct.ScmObj* %argslist56822$ae487482, %struct.ScmObj** %stackaddr$prim58451, align 8
%clofunc58452 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48748)
musttail call tailcc void %clofunc58452(%struct.ScmObj* %ae48748, %struct.ScmObj* %argslist56822$ae487482)
ret void
}

define tailcc void @proc_clo$ae48748(%struct.ScmObj* %env$ae48748,%struct.ScmObj* %current_45args56807) {
%stackaddr$env-ref58453 = alloca %struct.ScmObj*, align 8
%lsts47122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48748, i64 0)
store %struct.ScmObj* %lsts47122, %struct.ScmObj** %stackaddr$env-ref58453
%stackaddr$env-ref58454 = alloca %struct.ScmObj*, align 8
%k47503 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48748, i64 1)
store %struct.ScmObj* %k47503, %struct.ScmObj** %stackaddr$env-ref58454
%stackaddr$env-ref58455 = alloca %struct.ScmObj*, align 8
%_37foldr47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48748, i64 2)
store %struct.ScmObj* %_37foldr47099, %struct.ScmObj** %stackaddr$env-ref58455
%stackaddr$prim58456 = alloca %struct.ScmObj*, align 8
%_95k47504 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56807)
store volatile %struct.ScmObj* %_95k47504, %struct.ScmObj** %stackaddr$prim58456, align 8
%stackaddr$prim58457 = alloca %struct.ScmObj*, align 8
%current_45args56808 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56807)
store volatile %struct.ScmObj* %current_45args56808, %struct.ScmObj** %stackaddr$prim58457, align 8
%stackaddr$prim58458 = alloca %struct.ScmObj*, align 8
%anf_45bind47268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56808)
store volatile %struct.ScmObj* %anf_45bind47268, %struct.ScmObj** %stackaddr$prim58458, align 8
%ae48811 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58459 = alloca %struct.ScmObj*, align 8
%anf_45bind47269 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48811, %struct.ScmObj* %lsts47122)
store volatile %struct.ScmObj* %anf_45bind47269, %struct.ScmObj** %stackaddr$prim58459, align 8
%stackaddr$prim58460 = alloca %struct.ScmObj*, align 8
%anf_45bind47270 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47268, %struct.ScmObj* %anf_45bind47269)
store volatile %struct.ScmObj* %anf_45bind47270, %struct.ScmObj** %stackaddr$prim58460, align 8
%stackaddr$prim58461 = alloca %struct.ScmObj*, align 8
%cpsargs47505 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47503, %struct.ScmObj* %anf_45bind47270)
store volatile %struct.ScmObj* %cpsargs47505, %struct.ScmObj** %stackaddr$prim58461, align 8
%clofunc58462 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47099)
musttail call tailcc void %clofunc58462(%struct.ScmObj* %_37foldr47099, %struct.ScmObj* %cpsargs47505)
ret void
}

define tailcc void @proc_clo$ae48750(%struct.ScmObj* %env$ae48750,%struct.ScmObj* %fargs4712447506) {
%stackaddr$env-ref58463 = alloca %struct.ScmObj*, align 8
%f47123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48750, i64 0)
store %struct.ScmObj* %f47123, %struct.ScmObj** %stackaddr$env-ref58463
%stackaddr$env-ref58464 = alloca %struct.ScmObj*, align 8
%_37last47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48750, i64 1)
store %struct.ScmObj* %_37last47116, %struct.ScmObj** %stackaddr$env-ref58464
%stackaddr$env-ref58465 = alloca %struct.ScmObj*, align 8
%_37drop_45right47113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48750, i64 2)
store %struct.ScmObj* %_37drop_45right47113, %struct.ScmObj** %stackaddr$env-ref58465
%stackaddr$prim58466 = alloca %struct.ScmObj*, align 8
%k47507 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4712447506)
store volatile %struct.ScmObj* %k47507, %struct.ScmObj** %stackaddr$prim58466, align 8
%stackaddr$prim58467 = alloca %struct.ScmObj*, align 8
%fargs47124 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4712447506)
store volatile %struct.ScmObj* %fargs47124, %struct.ScmObj** %stackaddr$prim58467, align 8
%stackaddr$makeclosure58468 = alloca %struct.ScmObj*, align 8
%fptrToInt58469 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48754 to i64
%ae48754 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58469)
store volatile %struct.ScmObj* %ae48754, %struct.ScmObj** %stackaddr$makeclosure58468, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48754, %struct.ScmObj* %f47123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48754, %struct.ScmObj* %k47507, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48754, %struct.ScmObj* %fargs47124, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48754, %struct.ScmObj* %_37last47116, i64 3)
%ae48756 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist56821$_37drop_45right471130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58470 = alloca %struct.ScmObj*, align 8
%argslist56821$_37drop_45right471131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48756, %struct.ScmObj* %argslist56821$_37drop_45right471130)
store volatile %struct.ScmObj* %argslist56821$_37drop_45right471131, %struct.ScmObj** %stackaddr$prim58470, align 8
%stackaddr$prim58471 = alloca %struct.ScmObj*, align 8
%argslist56821$_37drop_45right471132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47124, %struct.ScmObj* %argslist56821$_37drop_45right471131)
store volatile %struct.ScmObj* %argslist56821$_37drop_45right471132, %struct.ScmObj** %stackaddr$prim58471, align 8
%stackaddr$prim58472 = alloca %struct.ScmObj*, align 8
%argslist56821$_37drop_45right471133 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48754, %struct.ScmObj* %argslist56821$_37drop_45right471132)
store volatile %struct.ScmObj* %argslist56821$_37drop_45right471133, %struct.ScmObj** %stackaddr$prim58472, align 8
%clofunc58473 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right47113)
musttail call tailcc void %clofunc58473(%struct.ScmObj* %_37drop_45right47113, %struct.ScmObj* %argslist56821$_37drop_45right471133)
ret void
}

define tailcc void @proc_clo$ae48754(%struct.ScmObj* %env$ae48754,%struct.ScmObj* %current_45args56810) {
%stackaddr$env-ref58474 = alloca %struct.ScmObj*, align 8
%f47123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48754, i64 0)
store %struct.ScmObj* %f47123, %struct.ScmObj** %stackaddr$env-ref58474
%stackaddr$env-ref58475 = alloca %struct.ScmObj*, align 8
%k47507 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48754, i64 1)
store %struct.ScmObj* %k47507, %struct.ScmObj** %stackaddr$env-ref58475
%stackaddr$env-ref58476 = alloca %struct.ScmObj*, align 8
%fargs47124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48754, i64 2)
store %struct.ScmObj* %fargs47124, %struct.ScmObj** %stackaddr$env-ref58476
%stackaddr$env-ref58477 = alloca %struct.ScmObj*, align 8
%_37last47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48754, i64 3)
store %struct.ScmObj* %_37last47116, %struct.ScmObj** %stackaddr$env-ref58477
%stackaddr$prim58478 = alloca %struct.ScmObj*, align 8
%_95k47508 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56810)
store volatile %struct.ScmObj* %_95k47508, %struct.ScmObj** %stackaddr$prim58478, align 8
%stackaddr$prim58479 = alloca %struct.ScmObj*, align 8
%current_45args56811 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56810)
store volatile %struct.ScmObj* %current_45args56811, %struct.ScmObj** %stackaddr$prim58479, align 8
%stackaddr$prim58480 = alloca %struct.ScmObj*, align 8
%anf_45bind47265 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56811)
store volatile %struct.ScmObj* %anf_45bind47265, %struct.ScmObj** %stackaddr$prim58480, align 8
%stackaddr$makeclosure58481 = alloca %struct.ScmObj*, align 8
%fptrToInt58482 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48761 to i64
%ae48761 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58482)
store volatile %struct.ScmObj* %ae48761, %struct.ScmObj** %stackaddr$makeclosure58481, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48761, %struct.ScmObj* %fargs47124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48761, %struct.ScmObj* %k47507, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48761, %struct.ScmObj* %_37last47116, i64 2)
%stackaddr$prim58483 = alloca %struct.ScmObj*, align 8
%cpsargs47512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48761, %struct.ScmObj* %anf_45bind47265)
store volatile %struct.ScmObj* %cpsargs47512, %struct.ScmObj** %stackaddr$prim58483, align 8
%clofunc58484 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47123)
musttail call tailcc void %clofunc58484(%struct.ScmObj* %f47123, %struct.ScmObj* %cpsargs47512)
ret void
}

define tailcc void @proc_clo$ae48761(%struct.ScmObj* %env$ae48761,%struct.ScmObj* %current_45args56813) {
%stackaddr$env-ref58485 = alloca %struct.ScmObj*, align 8
%fargs47124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48761, i64 0)
store %struct.ScmObj* %fargs47124, %struct.ScmObj** %stackaddr$env-ref58485
%stackaddr$env-ref58486 = alloca %struct.ScmObj*, align 8
%k47507 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48761, i64 1)
store %struct.ScmObj* %k47507, %struct.ScmObj** %stackaddr$env-ref58486
%stackaddr$env-ref58487 = alloca %struct.ScmObj*, align 8
%_37last47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48761, i64 2)
store %struct.ScmObj* %_37last47116, %struct.ScmObj** %stackaddr$env-ref58487
%stackaddr$prim58488 = alloca %struct.ScmObj*, align 8
%_95k47509 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56813)
store volatile %struct.ScmObj* %_95k47509, %struct.ScmObj** %stackaddr$prim58488, align 8
%stackaddr$prim58489 = alloca %struct.ScmObj*, align 8
%current_45args56814 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56813)
store volatile %struct.ScmObj* %current_45args56814, %struct.ScmObj** %stackaddr$prim58489, align 8
%stackaddr$prim58490 = alloca %struct.ScmObj*, align 8
%anf_45bind47266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56814)
store volatile %struct.ScmObj* %anf_45bind47266, %struct.ScmObj** %stackaddr$prim58490, align 8
%stackaddr$makeclosure58491 = alloca %struct.ScmObj*, align 8
%fptrToInt58492 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48766 to i64
%ae48766 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58492)
store volatile %struct.ScmObj* %ae48766, %struct.ScmObj** %stackaddr$makeclosure58491, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48766, %struct.ScmObj* %k47507, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48766, %struct.ScmObj* %anf_45bind47266, i64 1)
%argslist56820$_37last471160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58493 = alloca %struct.ScmObj*, align 8
%argslist56820$_37last471161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47124, %struct.ScmObj* %argslist56820$_37last471160)
store volatile %struct.ScmObj* %argslist56820$_37last471161, %struct.ScmObj** %stackaddr$prim58493, align 8
%stackaddr$prim58494 = alloca %struct.ScmObj*, align 8
%argslist56820$_37last471162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48766, %struct.ScmObj* %argslist56820$_37last471161)
store volatile %struct.ScmObj* %argslist56820$_37last471162, %struct.ScmObj** %stackaddr$prim58494, align 8
%clofunc58495 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last47116)
musttail call tailcc void %clofunc58495(%struct.ScmObj* %_37last47116, %struct.ScmObj* %argslist56820$_37last471162)
ret void
}

define tailcc void @proc_clo$ae48766(%struct.ScmObj* %env$ae48766,%struct.ScmObj* %current_45args56816) {
%stackaddr$env-ref58496 = alloca %struct.ScmObj*, align 8
%k47507 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48766, i64 0)
store %struct.ScmObj* %k47507, %struct.ScmObj** %stackaddr$env-ref58496
%stackaddr$env-ref58497 = alloca %struct.ScmObj*, align 8
%anf_45bind47266 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48766, i64 1)
store %struct.ScmObj* %anf_45bind47266, %struct.ScmObj** %stackaddr$env-ref58497
%stackaddr$prim58498 = alloca %struct.ScmObj*, align 8
%_95k47510 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56816)
store volatile %struct.ScmObj* %_95k47510, %struct.ScmObj** %stackaddr$prim58498, align 8
%stackaddr$prim58499 = alloca %struct.ScmObj*, align 8
%current_45args56817 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56816)
store volatile %struct.ScmObj* %current_45args56817, %struct.ScmObj** %stackaddr$prim58499, align 8
%stackaddr$prim58500 = alloca %struct.ScmObj*, align 8
%anf_45bind47267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56817)
store volatile %struct.ScmObj* %anf_45bind47267, %struct.ScmObj** %stackaddr$prim58500, align 8
%stackaddr$prim58501 = alloca %struct.ScmObj*, align 8
%cpsprim47511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47266, %struct.ScmObj* %anf_45bind47267)
store volatile %struct.ScmObj* %cpsprim47511, %struct.ScmObj** %stackaddr$prim58501, align 8
%ae48771 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56819$k475070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58502 = alloca %struct.ScmObj*, align 8
%argslist56819$k475071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47511, %struct.ScmObj* %argslist56819$k475070)
store volatile %struct.ScmObj* %argslist56819$k475071, %struct.ScmObj** %stackaddr$prim58502, align 8
%stackaddr$prim58503 = alloca %struct.ScmObj*, align 8
%argslist56819$k475072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48771, %struct.ScmObj* %argslist56819$k475071)
store volatile %struct.ScmObj* %argslist56819$k475072, %struct.ScmObj** %stackaddr$prim58503, align 8
%clofunc58504 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47507)
musttail call tailcc void %clofunc58504(%struct.ScmObj* %k47507, %struct.ScmObj* %argslist56819$k475072)
ret void
}

define tailcc void @proc_clo$ae48666(%struct.ScmObj* %env$ae48666,%struct.ScmObj* %current_45args56824) {
%stackaddr$env-ref58505 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48666, i64 0)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref58505
%stackaddr$prim58506 = alloca %struct.ScmObj*, align 8
%k47513 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56824)
store volatile %struct.ScmObj* %k47513, %struct.ScmObj** %stackaddr$prim58506, align 8
%stackaddr$prim58507 = alloca %struct.ScmObj*, align 8
%current_45args56825 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56824)
store volatile %struct.ScmObj* %current_45args56825, %struct.ScmObj** %stackaddr$prim58507, align 8
%stackaddr$prim58508 = alloca %struct.ScmObj*, align 8
%f47127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56825)
store volatile %struct.ScmObj* %f47127, %struct.ScmObj** %stackaddr$prim58508, align 8
%stackaddr$prim58509 = alloca %struct.ScmObj*, align 8
%current_45args56826 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56825)
store volatile %struct.ScmObj* %current_45args56826, %struct.ScmObj** %stackaddr$prim58509, align 8
%stackaddr$prim58510 = alloca %struct.ScmObj*, align 8
%lst47126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56826)
store volatile %struct.ScmObj* %lst47126, %struct.ScmObj** %stackaddr$prim58510, align 8
%stackaddr$makeclosure58511 = alloca %struct.ScmObj*, align 8
%fptrToInt58512 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48667 to i64
%ae48667 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58512)
store volatile %struct.ScmObj* %ae48667, %struct.ScmObj** %stackaddr$makeclosure58511, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48667, %struct.ScmObj* %lst47126, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48667, %struct.ScmObj* %_37foldr147094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48667, %struct.ScmObj* %k47513, i64 2)
%ae48668 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58513 = alloca %struct.ScmObj*, align 8
%fptrToInt58514 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48669 to i64
%ae48669 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58514)
store volatile %struct.ScmObj* %ae48669, %struct.ScmObj** %stackaddr$makeclosure58513, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48669, %struct.ScmObj* %f47127, i64 0)
%argslist56841$ae486670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58515 = alloca %struct.ScmObj*, align 8
%argslist56841$ae486671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48669, %struct.ScmObj* %argslist56841$ae486670)
store volatile %struct.ScmObj* %argslist56841$ae486671, %struct.ScmObj** %stackaddr$prim58515, align 8
%stackaddr$prim58516 = alloca %struct.ScmObj*, align 8
%argslist56841$ae486672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48668, %struct.ScmObj* %argslist56841$ae486671)
store volatile %struct.ScmObj* %argslist56841$ae486672, %struct.ScmObj** %stackaddr$prim58516, align 8
%clofunc58517 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48667)
musttail call tailcc void %clofunc58517(%struct.ScmObj* %ae48667, %struct.ScmObj* %argslist56841$ae486672)
ret void
}

define tailcc void @proc_clo$ae48667(%struct.ScmObj* %env$ae48667,%struct.ScmObj* %current_45args56828) {
%stackaddr$env-ref58518 = alloca %struct.ScmObj*, align 8
%lst47126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48667, i64 0)
store %struct.ScmObj* %lst47126, %struct.ScmObj** %stackaddr$env-ref58518
%stackaddr$env-ref58519 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48667, i64 1)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref58519
%stackaddr$env-ref58520 = alloca %struct.ScmObj*, align 8
%k47513 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48667, i64 2)
store %struct.ScmObj* %k47513, %struct.ScmObj** %stackaddr$env-ref58520
%stackaddr$prim58521 = alloca %struct.ScmObj*, align 8
%_95k47514 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56828)
store volatile %struct.ScmObj* %_95k47514, %struct.ScmObj** %stackaddr$prim58521, align 8
%stackaddr$prim58522 = alloca %struct.ScmObj*, align 8
%current_45args56829 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56828)
store volatile %struct.ScmObj* %current_45args56829, %struct.ScmObj** %stackaddr$prim58522, align 8
%stackaddr$prim58523 = alloca %struct.ScmObj*, align 8
%anf_45bind47264 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56829)
store volatile %struct.ScmObj* %anf_45bind47264, %struct.ScmObj** %stackaddr$prim58523, align 8
%ae48701 = call %struct.ScmObj* @const_init_null()
%argslist56831$_37foldr1470940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58524 = alloca %struct.ScmObj*, align 8
%argslist56831$_37foldr1470941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47126, %struct.ScmObj* %argslist56831$_37foldr1470940)
store volatile %struct.ScmObj* %argslist56831$_37foldr1470941, %struct.ScmObj** %stackaddr$prim58524, align 8
%stackaddr$prim58525 = alloca %struct.ScmObj*, align 8
%argslist56831$_37foldr1470942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48701, %struct.ScmObj* %argslist56831$_37foldr1470941)
store volatile %struct.ScmObj* %argslist56831$_37foldr1470942, %struct.ScmObj** %stackaddr$prim58525, align 8
%stackaddr$prim58526 = alloca %struct.ScmObj*, align 8
%argslist56831$_37foldr1470943 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47264, %struct.ScmObj* %argslist56831$_37foldr1470942)
store volatile %struct.ScmObj* %argslist56831$_37foldr1470943, %struct.ScmObj** %stackaddr$prim58526, align 8
%stackaddr$prim58527 = alloca %struct.ScmObj*, align 8
%argslist56831$_37foldr1470944 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47513, %struct.ScmObj* %argslist56831$_37foldr1470943)
store volatile %struct.ScmObj* %argslist56831$_37foldr1470944, %struct.ScmObj** %stackaddr$prim58527, align 8
%clofunc58528 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147094)
musttail call tailcc void %clofunc58528(%struct.ScmObj* %_37foldr147094, %struct.ScmObj* %argslist56831$_37foldr1470944)
ret void
}

define tailcc void @proc_clo$ae48669(%struct.ScmObj* %env$ae48669,%struct.ScmObj* %current_45args56832) {
%stackaddr$env-ref58529 = alloca %struct.ScmObj*, align 8
%f47127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48669, i64 0)
store %struct.ScmObj* %f47127, %struct.ScmObj** %stackaddr$env-ref58529
%stackaddr$prim58530 = alloca %struct.ScmObj*, align 8
%k47515 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56832)
store volatile %struct.ScmObj* %k47515, %struct.ScmObj** %stackaddr$prim58530, align 8
%stackaddr$prim58531 = alloca %struct.ScmObj*, align 8
%current_45args56833 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56832)
store volatile %struct.ScmObj* %current_45args56833, %struct.ScmObj** %stackaddr$prim58531, align 8
%stackaddr$prim58532 = alloca %struct.ScmObj*, align 8
%v47129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56833)
store volatile %struct.ScmObj* %v47129, %struct.ScmObj** %stackaddr$prim58532, align 8
%stackaddr$prim58533 = alloca %struct.ScmObj*, align 8
%current_45args56834 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56833)
store volatile %struct.ScmObj* %current_45args56834, %struct.ScmObj** %stackaddr$prim58533, align 8
%stackaddr$prim58534 = alloca %struct.ScmObj*, align 8
%r47128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56834)
store volatile %struct.ScmObj* %r47128, %struct.ScmObj** %stackaddr$prim58534, align 8
%stackaddr$makeclosure58535 = alloca %struct.ScmObj*, align 8
%fptrToInt58536 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48671 to i64
%ae48671 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58536)
store volatile %struct.ScmObj* %ae48671, %struct.ScmObj** %stackaddr$makeclosure58535, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48671, %struct.ScmObj* %k47515, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48671, %struct.ScmObj* %r47128, i64 1)
%argslist56840$f471270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58537 = alloca %struct.ScmObj*, align 8
%argslist56840$f471271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v47129, %struct.ScmObj* %argslist56840$f471270)
store volatile %struct.ScmObj* %argslist56840$f471271, %struct.ScmObj** %stackaddr$prim58537, align 8
%stackaddr$prim58538 = alloca %struct.ScmObj*, align 8
%argslist56840$f471272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48671, %struct.ScmObj* %argslist56840$f471271)
store volatile %struct.ScmObj* %argslist56840$f471272, %struct.ScmObj** %stackaddr$prim58538, align 8
%clofunc58539 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47127)
musttail call tailcc void %clofunc58539(%struct.ScmObj* %f47127, %struct.ScmObj* %argslist56840$f471272)
ret void
}

define tailcc void @proc_clo$ae48671(%struct.ScmObj* %env$ae48671,%struct.ScmObj* %current_45args56836) {
%stackaddr$env-ref58540 = alloca %struct.ScmObj*, align 8
%k47515 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48671, i64 0)
store %struct.ScmObj* %k47515, %struct.ScmObj** %stackaddr$env-ref58540
%stackaddr$env-ref58541 = alloca %struct.ScmObj*, align 8
%r47128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48671, i64 1)
store %struct.ScmObj* %r47128, %struct.ScmObj** %stackaddr$env-ref58541
%stackaddr$prim58542 = alloca %struct.ScmObj*, align 8
%_95k47516 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56836)
store volatile %struct.ScmObj* %_95k47516, %struct.ScmObj** %stackaddr$prim58542, align 8
%stackaddr$prim58543 = alloca %struct.ScmObj*, align 8
%current_45args56837 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56836)
store volatile %struct.ScmObj* %current_45args56837, %struct.ScmObj** %stackaddr$prim58543, align 8
%stackaddr$prim58544 = alloca %struct.ScmObj*, align 8
%anf_45bind47263 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56837)
store volatile %struct.ScmObj* %anf_45bind47263, %struct.ScmObj** %stackaddr$prim58544, align 8
%stackaddr$prim58545 = alloca %struct.ScmObj*, align 8
%cpsprim47517 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47263, %struct.ScmObj* %r47128)
store volatile %struct.ScmObj* %cpsprim47517, %struct.ScmObj** %stackaddr$prim58545, align 8
%ae48676 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56839$k475150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58546 = alloca %struct.ScmObj*, align 8
%argslist56839$k475151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47517, %struct.ScmObj* %argslist56839$k475150)
store volatile %struct.ScmObj* %argslist56839$k475151, %struct.ScmObj** %stackaddr$prim58546, align 8
%stackaddr$prim58547 = alloca %struct.ScmObj*, align 8
%argslist56839$k475152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48676, %struct.ScmObj* %argslist56839$k475151)
store volatile %struct.ScmObj* %argslist56839$k475152, %struct.ScmObj** %stackaddr$prim58547, align 8
%clofunc58548 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47515)
musttail call tailcc void %clofunc58548(%struct.ScmObj* %k47515, %struct.ScmObj* %argslist56839$k475152)
ret void
}

define tailcc void @proc_clo$ae48280(%struct.ScmObj* %env$ae48280,%struct.ScmObj* %current_45args56844) {
%stackaddr$env-ref58549 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48280, i64 0)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref58549
%stackaddr$env-ref58550 = alloca %struct.ScmObj*, align 8
%_37map147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48280, i64 1)
store %struct.ScmObj* %_37map147090, %struct.ScmObj** %stackaddr$env-ref58550
%stackaddr$prim58551 = alloca %struct.ScmObj*, align 8
%k47518 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56844)
store volatile %struct.ScmObj* %k47518, %struct.ScmObj** %stackaddr$prim58551, align 8
%stackaddr$prim58552 = alloca %struct.ScmObj*, align 8
%current_45args56845 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56844)
store volatile %struct.ScmObj* %current_45args56845, %struct.ScmObj** %stackaddr$prim58552, align 8
%stackaddr$prim58553 = alloca %struct.ScmObj*, align 8
%_37foldr47100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56845)
store volatile %struct.ScmObj* %_37foldr47100, %struct.ScmObj** %stackaddr$prim58553, align 8
%ae48282 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58554 = alloca %struct.ScmObj*, align 8
%fptrToInt58555 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48283 to i64
%ae48283 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58555)
store volatile %struct.ScmObj* %ae48283, %struct.ScmObj** %stackaddr$makeclosure58554, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48283, %struct.ScmObj* %_37foldr147094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48283, %struct.ScmObj* %_37map147090, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48283, %struct.ScmObj* %_37foldr47100, i64 2)
%argslist56902$k475180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58556 = alloca %struct.ScmObj*, align 8
%argslist56902$k475181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48283, %struct.ScmObj* %argslist56902$k475180)
store volatile %struct.ScmObj* %argslist56902$k475181, %struct.ScmObj** %stackaddr$prim58556, align 8
%stackaddr$prim58557 = alloca %struct.ScmObj*, align 8
%argslist56902$k475182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48282, %struct.ScmObj* %argslist56902$k475181)
store volatile %struct.ScmObj* %argslist56902$k475182, %struct.ScmObj** %stackaddr$prim58557, align 8
%clofunc58558 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47518)
musttail call tailcc void %clofunc58558(%struct.ScmObj* %k47518, %struct.ScmObj* %argslist56902$k475182)
ret void
}

define tailcc void @proc_clo$ae48283(%struct.ScmObj* %env$ae48283,%struct.ScmObj* %args4710147519) {
%stackaddr$env-ref58559 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48283, i64 0)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref58559
%stackaddr$env-ref58560 = alloca %struct.ScmObj*, align 8
%_37map147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48283, i64 1)
store %struct.ScmObj* %_37map147090, %struct.ScmObj** %stackaddr$env-ref58560
%stackaddr$env-ref58561 = alloca %struct.ScmObj*, align 8
%_37foldr47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48283, i64 2)
store %struct.ScmObj* %_37foldr47100, %struct.ScmObj** %stackaddr$env-ref58561
%stackaddr$prim58562 = alloca %struct.ScmObj*, align 8
%k47520 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4710147519)
store volatile %struct.ScmObj* %k47520, %struct.ScmObj** %stackaddr$prim58562, align 8
%stackaddr$prim58563 = alloca %struct.ScmObj*, align 8
%args47101 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4710147519)
store volatile %struct.ScmObj* %args47101, %struct.ScmObj** %stackaddr$prim58563, align 8
%stackaddr$prim58564 = alloca %struct.ScmObj*, align 8
%f47104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47101)
store volatile %struct.ScmObj* %f47104, %struct.ScmObj** %stackaddr$prim58564, align 8
%stackaddr$prim58565 = alloca %struct.ScmObj*, align 8
%anf_45bind47250 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47101)
store volatile %struct.ScmObj* %anf_45bind47250, %struct.ScmObj** %stackaddr$prim58565, align 8
%stackaddr$prim58566 = alloca %struct.ScmObj*, align 8
%acc47103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47250)
store volatile %struct.ScmObj* %acc47103, %struct.ScmObj** %stackaddr$prim58566, align 8
%stackaddr$prim58567 = alloca %struct.ScmObj*, align 8
%anf_45bind47251 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47101)
store volatile %struct.ScmObj* %anf_45bind47251, %struct.ScmObj** %stackaddr$prim58567, align 8
%stackaddr$prim58568 = alloca %struct.ScmObj*, align 8
%lsts47102 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47251)
store volatile %struct.ScmObj* %lsts47102, %struct.ScmObj** %stackaddr$prim58568, align 8
%stackaddr$makeclosure58569 = alloca %struct.ScmObj*, align 8
%fptrToInt58570 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48291 to i64
%ae48291 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58570)
store volatile %struct.ScmObj* %ae48291, %struct.ScmObj** %stackaddr$makeclosure58569, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48291, %struct.ScmObj* %f47104, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48291, %struct.ScmObj* %k47520, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48291, %struct.ScmObj* %_37foldr147094, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48291, %struct.ScmObj* %_37map147090, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48291, %struct.ScmObj* %acc47103, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48291, %struct.ScmObj* %lsts47102, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48291, %struct.ScmObj* %_37foldr47100, i64 6)
%ae48292 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58571 = alloca %struct.ScmObj*, align 8
%fptrToInt58572 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48293 to i64
%ae48293 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58572)
store volatile %struct.ScmObj* %ae48293, %struct.ScmObj** %stackaddr$makeclosure58571, align 8
%argslist56901$ae482910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58573 = alloca %struct.ScmObj*, align 8
%argslist56901$ae482911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48293, %struct.ScmObj* %argslist56901$ae482910)
store volatile %struct.ScmObj* %argslist56901$ae482911, %struct.ScmObj** %stackaddr$prim58573, align 8
%stackaddr$prim58574 = alloca %struct.ScmObj*, align 8
%argslist56901$ae482912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48292, %struct.ScmObj* %argslist56901$ae482911)
store volatile %struct.ScmObj* %argslist56901$ae482912, %struct.ScmObj** %stackaddr$prim58574, align 8
%clofunc58575 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48291)
musttail call tailcc void %clofunc58575(%struct.ScmObj* %ae48291, %struct.ScmObj* %argslist56901$ae482912)
ret void
}

define tailcc void @proc_clo$ae48291(%struct.ScmObj* %env$ae48291,%struct.ScmObj* %current_45args56847) {
%stackaddr$env-ref58576 = alloca %struct.ScmObj*, align 8
%f47104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48291, i64 0)
store %struct.ScmObj* %f47104, %struct.ScmObj** %stackaddr$env-ref58576
%stackaddr$env-ref58577 = alloca %struct.ScmObj*, align 8
%k47520 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48291, i64 1)
store %struct.ScmObj* %k47520, %struct.ScmObj** %stackaddr$env-ref58577
%stackaddr$env-ref58578 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48291, i64 2)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref58578
%stackaddr$env-ref58579 = alloca %struct.ScmObj*, align 8
%_37map147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48291, i64 3)
store %struct.ScmObj* %_37map147090, %struct.ScmObj** %stackaddr$env-ref58579
%stackaddr$env-ref58580 = alloca %struct.ScmObj*, align 8
%acc47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48291, i64 4)
store %struct.ScmObj* %acc47103, %struct.ScmObj** %stackaddr$env-ref58580
%stackaddr$env-ref58581 = alloca %struct.ScmObj*, align 8
%lsts47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48291, i64 5)
store %struct.ScmObj* %lsts47102, %struct.ScmObj** %stackaddr$env-ref58581
%stackaddr$env-ref58582 = alloca %struct.ScmObj*, align 8
%_37foldr47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48291, i64 6)
store %struct.ScmObj* %_37foldr47100, %struct.ScmObj** %stackaddr$env-ref58582
%stackaddr$prim58583 = alloca %struct.ScmObj*, align 8
%_95k47521 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56847)
store volatile %struct.ScmObj* %_95k47521, %struct.ScmObj** %stackaddr$prim58583, align 8
%stackaddr$prim58584 = alloca %struct.ScmObj*, align 8
%current_45args56848 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56847)
store volatile %struct.ScmObj* %current_45args56848, %struct.ScmObj** %stackaddr$prim58584, align 8
%stackaddr$prim58585 = alloca %struct.ScmObj*, align 8
%anf_45bind47252 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56848)
store volatile %struct.ScmObj* %anf_45bind47252, %struct.ScmObj** %stackaddr$prim58585, align 8
%stackaddr$makeclosure58586 = alloca %struct.ScmObj*, align 8
%fptrToInt58587 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48323 to i64
%ae48323 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58587)
store volatile %struct.ScmObj* %ae48323, %struct.ScmObj** %stackaddr$makeclosure58586, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48323, %struct.ScmObj* %f47104, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48323, %struct.ScmObj* %k47520, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48323, %struct.ScmObj* %_37foldr147094, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48323, %struct.ScmObj* %_37map147090, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48323, %struct.ScmObj* %acc47103, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48323, %struct.ScmObj* %lsts47102, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48323, %struct.ScmObj* %_37foldr47100, i64 6)
%ae48325 = call %struct.ScmObj* @const_init_false()
%argslist56894$_37foldr1470940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58588 = alloca %struct.ScmObj*, align 8
%argslist56894$_37foldr1470941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47102, %struct.ScmObj* %argslist56894$_37foldr1470940)
store volatile %struct.ScmObj* %argslist56894$_37foldr1470941, %struct.ScmObj** %stackaddr$prim58588, align 8
%stackaddr$prim58589 = alloca %struct.ScmObj*, align 8
%argslist56894$_37foldr1470942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48325, %struct.ScmObj* %argslist56894$_37foldr1470941)
store volatile %struct.ScmObj* %argslist56894$_37foldr1470942, %struct.ScmObj** %stackaddr$prim58589, align 8
%stackaddr$prim58590 = alloca %struct.ScmObj*, align 8
%argslist56894$_37foldr1470943 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47252, %struct.ScmObj* %argslist56894$_37foldr1470942)
store volatile %struct.ScmObj* %argslist56894$_37foldr1470943, %struct.ScmObj** %stackaddr$prim58590, align 8
%stackaddr$prim58591 = alloca %struct.ScmObj*, align 8
%argslist56894$_37foldr1470944 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48323, %struct.ScmObj* %argslist56894$_37foldr1470943)
store volatile %struct.ScmObj* %argslist56894$_37foldr1470944, %struct.ScmObj** %stackaddr$prim58591, align 8
%clofunc58592 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147094)
musttail call tailcc void %clofunc58592(%struct.ScmObj* %_37foldr147094, %struct.ScmObj* %argslist56894$_37foldr1470944)
ret void
}

define tailcc void @proc_clo$ae48323(%struct.ScmObj* %env$ae48323,%struct.ScmObj* %current_45args56850) {
%stackaddr$env-ref58593 = alloca %struct.ScmObj*, align 8
%f47104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48323, i64 0)
store %struct.ScmObj* %f47104, %struct.ScmObj** %stackaddr$env-ref58593
%stackaddr$env-ref58594 = alloca %struct.ScmObj*, align 8
%k47520 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48323, i64 1)
store %struct.ScmObj* %k47520, %struct.ScmObj** %stackaddr$env-ref58594
%stackaddr$env-ref58595 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48323, i64 2)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref58595
%stackaddr$env-ref58596 = alloca %struct.ScmObj*, align 8
%_37map147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48323, i64 3)
store %struct.ScmObj* %_37map147090, %struct.ScmObj** %stackaddr$env-ref58596
%stackaddr$env-ref58597 = alloca %struct.ScmObj*, align 8
%acc47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48323, i64 4)
store %struct.ScmObj* %acc47103, %struct.ScmObj** %stackaddr$env-ref58597
%stackaddr$env-ref58598 = alloca %struct.ScmObj*, align 8
%lsts47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48323, i64 5)
store %struct.ScmObj* %lsts47102, %struct.ScmObj** %stackaddr$env-ref58598
%stackaddr$env-ref58599 = alloca %struct.ScmObj*, align 8
%_37foldr47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48323, i64 6)
store %struct.ScmObj* %_37foldr47100, %struct.ScmObj** %stackaddr$env-ref58599
%stackaddr$prim58600 = alloca %struct.ScmObj*, align 8
%_95k47522 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56850)
store volatile %struct.ScmObj* %_95k47522, %struct.ScmObj** %stackaddr$prim58600, align 8
%stackaddr$prim58601 = alloca %struct.ScmObj*, align 8
%current_45args56851 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56850)
store volatile %struct.ScmObj* %current_45args56851, %struct.ScmObj** %stackaddr$prim58601, align 8
%stackaddr$prim58602 = alloca %struct.ScmObj*, align 8
%anf_45bind47253 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56851)
store volatile %struct.ScmObj* %anf_45bind47253, %struct.ScmObj** %stackaddr$prim58602, align 8
%truthy$cmp58603 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47253)
%cmp$cmp58603 = icmp eq i64 %truthy$cmp58603, 1
br i1 %cmp$cmp58603, label %truebranch$cmp58603, label %falsebranch$cmp58603
truebranch$cmp58603:
%ae48334 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56853$k475200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58604 = alloca %struct.ScmObj*, align 8
%argslist56853$k475201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47103, %struct.ScmObj* %argslist56853$k475200)
store volatile %struct.ScmObj* %argslist56853$k475201, %struct.ScmObj** %stackaddr$prim58604, align 8
%stackaddr$prim58605 = alloca %struct.ScmObj*, align 8
%argslist56853$k475202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48334, %struct.ScmObj* %argslist56853$k475201)
store volatile %struct.ScmObj* %argslist56853$k475202, %struct.ScmObj** %stackaddr$prim58605, align 8
%clofunc58606 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47520)
musttail call tailcc void %clofunc58606(%struct.ScmObj* %k47520, %struct.ScmObj* %argslist56853$k475202)
ret void
falsebranch$cmp58603:
%stackaddr$makeclosure58607 = alloca %struct.ScmObj*, align 8
%fptrToInt58608 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48339 to i64
%ae48339 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58608)
store volatile %struct.ScmObj* %ae48339, %struct.ScmObj** %stackaddr$makeclosure58607, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48339, %struct.ScmObj* %f47104, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48339, %struct.ScmObj* %k47520, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48339, %struct.ScmObj* %_37foldr147094, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48339, %struct.ScmObj* %_37map147090, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48339, %struct.ScmObj* %acc47103, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48339, %struct.ScmObj* %lsts47102, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48339, %struct.ScmObj* %_37foldr47100, i64 6)
%ae48340 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58609 = alloca %struct.ScmObj*, align 8
%fptrToInt58610 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48341 to i64
%ae48341 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58610)
store volatile %struct.ScmObj* %ae48341, %struct.ScmObj** %stackaddr$makeclosure58609, align 8
%argslist56893$ae483390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58611 = alloca %struct.ScmObj*, align 8
%argslist56893$ae483391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48341, %struct.ScmObj* %argslist56893$ae483390)
store volatile %struct.ScmObj* %argslist56893$ae483391, %struct.ScmObj** %stackaddr$prim58611, align 8
%stackaddr$prim58612 = alloca %struct.ScmObj*, align 8
%argslist56893$ae483392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48340, %struct.ScmObj* %argslist56893$ae483391)
store volatile %struct.ScmObj* %argslist56893$ae483392, %struct.ScmObj** %stackaddr$prim58612, align 8
%clofunc58613 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48339)
musttail call tailcc void %clofunc58613(%struct.ScmObj* %ae48339, %struct.ScmObj* %argslist56893$ae483392)
ret void
}

define tailcc void @proc_clo$ae48339(%struct.ScmObj* %env$ae48339,%struct.ScmObj* %current_45args56854) {
%stackaddr$env-ref58614 = alloca %struct.ScmObj*, align 8
%f47104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48339, i64 0)
store %struct.ScmObj* %f47104, %struct.ScmObj** %stackaddr$env-ref58614
%stackaddr$env-ref58615 = alloca %struct.ScmObj*, align 8
%k47520 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48339, i64 1)
store %struct.ScmObj* %k47520, %struct.ScmObj** %stackaddr$env-ref58615
%stackaddr$env-ref58616 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48339, i64 2)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref58616
%stackaddr$env-ref58617 = alloca %struct.ScmObj*, align 8
%_37map147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48339, i64 3)
store %struct.ScmObj* %_37map147090, %struct.ScmObj** %stackaddr$env-ref58617
%stackaddr$env-ref58618 = alloca %struct.ScmObj*, align 8
%acc47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48339, i64 4)
store %struct.ScmObj* %acc47103, %struct.ScmObj** %stackaddr$env-ref58618
%stackaddr$env-ref58619 = alloca %struct.ScmObj*, align 8
%lsts47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48339, i64 5)
store %struct.ScmObj* %lsts47102, %struct.ScmObj** %stackaddr$env-ref58619
%stackaddr$env-ref58620 = alloca %struct.ScmObj*, align 8
%_37foldr47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48339, i64 6)
store %struct.ScmObj* %_37foldr47100, %struct.ScmObj** %stackaddr$env-ref58620
%stackaddr$prim58621 = alloca %struct.ScmObj*, align 8
%_95k47523 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56854)
store volatile %struct.ScmObj* %_95k47523, %struct.ScmObj** %stackaddr$prim58621, align 8
%stackaddr$prim58622 = alloca %struct.ScmObj*, align 8
%current_45args56855 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56854)
store volatile %struct.ScmObj* %current_45args56855, %struct.ScmObj** %stackaddr$prim58622, align 8
%stackaddr$prim58623 = alloca %struct.ScmObj*, align 8
%anf_45bind47254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56855)
store volatile %struct.ScmObj* %anf_45bind47254, %struct.ScmObj** %stackaddr$prim58623, align 8
%stackaddr$makeclosure58624 = alloca %struct.ScmObj*, align 8
%fptrToInt58625 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48360 to i64
%ae48360 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58625)
store volatile %struct.ScmObj* %ae48360, %struct.ScmObj** %stackaddr$makeclosure58624, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48360, %struct.ScmObj* %f47104, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48360, %struct.ScmObj* %k47520, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48360, %struct.ScmObj* %_37foldr147094, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48360, %struct.ScmObj* %_37map147090, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48360, %struct.ScmObj* %acc47103, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48360, %struct.ScmObj* %lsts47102, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48360, %struct.ScmObj* %_37foldr47100, i64 6)
%argslist56888$_37map1470900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58626 = alloca %struct.ScmObj*, align 8
%argslist56888$_37map1470901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47102, %struct.ScmObj* %argslist56888$_37map1470900)
store volatile %struct.ScmObj* %argslist56888$_37map1470901, %struct.ScmObj** %stackaddr$prim58626, align 8
%stackaddr$prim58627 = alloca %struct.ScmObj*, align 8
%argslist56888$_37map1470902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47254, %struct.ScmObj* %argslist56888$_37map1470901)
store volatile %struct.ScmObj* %argslist56888$_37map1470902, %struct.ScmObj** %stackaddr$prim58627, align 8
%stackaddr$prim58628 = alloca %struct.ScmObj*, align 8
%argslist56888$_37map1470903 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48360, %struct.ScmObj* %argslist56888$_37map1470902)
store volatile %struct.ScmObj* %argslist56888$_37map1470903, %struct.ScmObj** %stackaddr$prim58628, align 8
%clofunc58629 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147090)
musttail call tailcc void %clofunc58629(%struct.ScmObj* %_37map147090, %struct.ScmObj* %argslist56888$_37map1470903)
ret void
}

define tailcc void @proc_clo$ae48360(%struct.ScmObj* %env$ae48360,%struct.ScmObj* %current_45args56857) {
%stackaddr$env-ref58630 = alloca %struct.ScmObj*, align 8
%f47104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48360, i64 0)
store %struct.ScmObj* %f47104, %struct.ScmObj** %stackaddr$env-ref58630
%stackaddr$env-ref58631 = alloca %struct.ScmObj*, align 8
%k47520 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48360, i64 1)
store %struct.ScmObj* %k47520, %struct.ScmObj** %stackaddr$env-ref58631
%stackaddr$env-ref58632 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48360, i64 2)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref58632
%stackaddr$env-ref58633 = alloca %struct.ScmObj*, align 8
%_37map147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48360, i64 3)
store %struct.ScmObj* %_37map147090, %struct.ScmObj** %stackaddr$env-ref58633
%stackaddr$env-ref58634 = alloca %struct.ScmObj*, align 8
%acc47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48360, i64 4)
store %struct.ScmObj* %acc47103, %struct.ScmObj** %stackaddr$env-ref58634
%stackaddr$env-ref58635 = alloca %struct.ScmObj*, align 8
%lsts47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48360, i64 5)
store %struct.ScmObj* %lsts47102, %struct.ScmObj** %stackaddr$env-ref58635
%stackaddr$env-ref58636 = alloca %struct.ScmObj*, align 8
%_37foldr47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48360, i64 6)
store %struct.ScmObj* %_37foldr47100, %struct.ScmObj** %stackaddr$env-ref58636
%stackaddr$prim58637 = alloca %struct.ScmObj*, align 8
%_95k47524 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56857)
store volatile %struct.ScmObj* %_95k47524, %struct.ScmObj** %stackaddr$prim58637, align 8
%stackaddr$prim58638 = alloca %struct.ScmObj*, align 8
%current_45args56858 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56857)
store volatile %struct.ScmObj* %current_45args56858, %struct.ScmObj** %stackaddr$prim58638, align 8
%stackaddr$prim58639 = alloca %struct.ScmObj*, align 8
%lsts_4347109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56858)
store volatile %struct.ScmObj* %lsts_4347109, %struct.ScmObj** %stackaddr$prim58639, align 8
%stackaddr$makeclosure58640 = alloca %struct.ScmObj*, align 8
%fptrToInt58641 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48363 to i64
%ae48363 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt58641)
store volatile %struct.ScmObj* %ae48363, %struct.ScmObj** %stackaddr$makeclosure58640, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48363, %struct.ScmObj* %f47104, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48363, %struct.ScmObj* %k47520, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48363, %struct.ScmObj* %_37foldr147094, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48363, %struct.ScmObj* %lsts_4347109, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48363, %struct.ScmObj* %_37map147090, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48363, %struct.ScmObj* %acc47103, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48363, %struct.ScmObj* %lsts47102, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48363, %struct.ScmObj* %_37foldr47100, i64 7)
%ae48364 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58642 = alloca %struct.ScmObj*, align 8
%fptrToInt58643 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48365 to i64
%ae48365 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58643)
store volatile %struct.ScmObj* %ae48365, %struct.ScmObj** %stackaddr$makeclosure58642, align 8
%argslist56887$ae483630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58644 = alloca %struct.ScmObj*, align 8
%argslist56887$ae483631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48365, %struct.ScmObj* %argslist56887$ae483630)
store volatile %struct.ScmObj* %argslist56887$ae483631, %struct.ScmObj** %stackaddr$prim58644, align 8
%stackaddr$prim58645 = alloca %struct.ScmObj*, align 8
%argslist56887$ae483632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48364, %struct.ScmObj* %argslist56887$ae483631)
store volatile %struct.ScmObj* %argslist56887$ae483632, %struct.ScmObj** %stackaddr$prim58645, align 8
%clofunc58646 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48363)
musttail call tailcc void %clofunc58646(%struct.ScmObj* %ae48363, %struct.ScmObj* %argslist56887$ae483632)
ret void
}

define tailcc void @proc_clo$ae48363(%struct.ScmObj* %env$ae48363,%struct.ScmObj* %current_45args56860) {
%stackaddr$env-ref58647 = alloca %struct.ScmObj*, align 8
%f47104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48363, i64 0)
store %struct.ScmObj* %f47104, %struct.ScmObj** %stackaddr$env-ref58647
%stackaddr$env-ref58648 = alloca %struct.ScmObj*, align 8
%k47520 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48363, i64 1)
store %struct.ScmObj* %k47520, %struct.ScmObj** %stackaddr$env-ref58648
%stackaddr$env-ref58649 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48363, i64 2)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref58649
%stackaddr$env-ref58650 = alloca %struct.ScmObj*, align 8
%lsts_4347109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48363, i64 3)
store %struct.ScmObj* %lsts_4347109, %struct.ScmObj** %stackaddr$env-ref58650
%stackaddr$env-ref58651 = alloca %struct.ScmObj*, align 8
%_37map147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48363, i64 4)
store %struct.ScmObj* %_37map147090, %struct.ScmObj** %stackaddr$env-ref58651
%stackaddr$env-ref58652 = alloca %struct.ScmObj*, align 8
%acc47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48363, i64 5)
store %struct.ScmObj* %acc47103, %struct.ScmObj** %stackaddr$env-ref58652
%stackaddr$env-ref58653 = alloca %struct.ScmObj*, align 8
%lsts47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48363, i64 6)
store %struct.ScmObj* %lsts47102, %struct.ScmObj** %stackaddr$env-ref58653
%stackaddr$env-ref58654 = alloca %struct.ScmObj*, align 8
%_37foldr47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48363, i64 7)
store %struct.ScmObj* %_37foldr47100, %struct.ScmObj** %stackaddr$env-ref58654
%stackaddr$prim58655 = alloca %struct.ScmObj*, align 8
%_95k47525 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56860)
store volatile %struct.ScmObj* %_95k47525, %struct.ScmObj** %stackaddr$prim58655, align 8
%stackaddr$prim58656 = alloca %struct.ScmObj*, align 8
%current_45args56861 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56860)
store volatile %struct.ScmObj* %current_45args56861, %struct.ScmObj** %stackaddr$prim58656, align 8
%stackaddr$prim58657 = alloca %struct.ScmObj*, align 8
%anf_45bind47255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56861)
store volatile %struct.ScmObj* %anf_45bind47255, %struct.ScmObj** %stackaddr$prim58657, align 8
%stackaddr$makeclosure58658 = alloca %struct.ScmObj*, align 8
%fptrToInt58659 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48384 to i64
%ae48384 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt58659)
store volatile %struct.ScmObj* %ae48384, %struct.ScmObj** %stackaddr$makeclosure58658, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48384, %struct.ScmObj* %f47104, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48384, %struct.ScmObj* %k47520, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48384, %struct.ScmObj* %_37foldr147094, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48384, %struct.ScmObj* %lsts_4347109, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48384, %struct.ScmObj* %acc47103, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48384, %struct.ScmObj* %_37foldr47100, i64 5)
%argslist56882$_37map1470900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58660 = alloca %struct.ScmObj*, align 8
%argslist56882$_37map1470901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47102, %struct.ScmObj* %argslist56882$_37map1470900)
store volatile %struct.ScmObj* %argslist56882$_37map1470901, %struct.ScmObj** %stackaddr$prim58660, align 8
%stackaddr$prim58661 = alloca %struct.ScmObj*, align 8
%argslist56882$_37map1470902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47255, %struct.ScmObj* %argslist56882$_37map1470901)
store volatile %struct.ScmObj* %argslist56882$_37map1470902, %struct.ScmObj** %stackaddr$prim58661, align 8
%stackaddr$prim58662 = alloca %struct.ScmObj*, align 8
%argslist56882$_37map1470903 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48384, %struct.ScmObj* %argslist56882$_37map1470902)
store volatile %struct.ScmObj* %argslist56882$_37map1470903, %struct.ScmObj** %stackaddr$prim58662, align 8
%clofunc58663 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147090)
musttail call tailcc void %clofunc58663(%struct.ScmObj* %_37map147090, %struct.ScmObj* %argslist56882$_37map1470903)
ret void
}

define tailcc void @proc_clo$ae48384(%struct.ScmObj* %env$ae48384,%struct.ScmObj* %current_45args56863) {
%stackaddr$env-ref58664 = alloca %struct.ScmObj*, align 8
%f47104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48384, i64 0)
store %struct.ScmObj* %f47104, %struct.ScmObj** %stackaddr$env-ref58664
%stackaddr$env-ref58665 = alloca %struct.ScmObj*, align 8
%k47520 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48384, i64 1)
store %struct.ScmObj* %k47520, %struct.ScmObj** %stackaddr$env-ref58665
%stackaddr$env-ref58666 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48384, i64 2)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref58666
%stackaddr$env-ref58667 = alloca %struct.ScmObj*, align 8
%lsts_4347109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48384, i64 3)
store %struct.ScmObj* %lsts_4347109, %struct.ScmObj** %stackaddr$env-ref58667
%stackaddr$env-ref58668 = alloca %struct.ScmObj*, align 8
%acc47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48384, i64 4)
store %struct.ScmObj* %acc47103, %struct.ScmObj** %stackaddr$env-ref58668
%stackaddr$env-ref58669 = alloca %struct.ScmObj*, align 8
%_37foldr47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48384, i64 5)
store %struct.ScmObj* %_37foldr47100, %struct.ScmObj** %stackaddr$env-ref58669
%stackaddr$prim58670 = alloca %struct.ScmObj*, align 8
%_95k47526 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56863)
store volatile %struct.ScmObj* %_95k47526, %struct.ScmObj** %stackaddr$prim58670, align 8
%stackaddr$prim58671 = alloca %struct.ScmObj*, align 8
%current_45args56864 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56863)
store volatile %struct.ScmObj* %current_45args56864, %struct.ScmObj** %stackaddr$prim58671, align 8
%stackaddr$prim58672 = alloca %struct.ScmObj*, align 8
%vs47107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56864)
store volatile %struct.ScmObj* %vs47107, %struct.ScmObj** %stackaddr$prim58672, align 8
%stackaddr$makeclosure58673 = alloca %struct.ScmObj*, align 8
%fptrToInt58674 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48387 to i64
%ae48387 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58674)
store volatile %struct.ScmObj* %ae48387, %struct.ScmObj** %stackaddr$makeclosure58673, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48387, %struct.ScmObj* %f47104, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48387, %struct.ScmObj* %k47520, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48387, %struct.ScmObj* %_37foldr147094, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48387, %struct.ScmObj* %lsts_4347109, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48387, %struct.ScmObj* %vs47107, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48387, %struct.ScmObj* %acc47103, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48387, %struct.ScmObj* %_37foldr47100, i64 6)
%ae48388 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58675 = alloca %struct.ScmObj*, align 8
%fptrToInt58676 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48389 to i64
%ae48389 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58676)
store volatile %struct.ScmObj* %ae48389, %struct.ScmObj** %stackaddr$makeclosure58675, align 8
%argslist56881$ae483870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58677 = alloca %struct.ScmObj*, align 8
%argslist56881$ae483871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48389, %struct.ScmObj* %argslist56881$ae483870)
store volatile %struct.ScmObj* %argslist56881$ae483871, %struct.ScmObj** %stackaddr$prim58677, align 8
%stackaddr$prim58678 = alloca %struct.ScmObj*, align 8
%argslist56881$ae483872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48388, %struct.ScmObj* %argslist56881$ae483871)
store volatile %struct.ScmObj* %argslist56881$ae483872, %struct.ScmObj** %stackaddr$prim58678, align 8
%clofunc58679 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48387)
musttail call tailcc void %clofunc58679(%struct.ScmObj* %ae48387, %struct.ScmObj* %argslist56881$ae483872)
ret void
}

define tailcc void @proc_clo$ae48387(%struct.ScmObj* %env$ae48387,%struct.ScmObj* %current_45args56866) {
%stackaddr$env-ref58680 = alloca %struct.ScmObj*, align 8
%f47104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48387, i64 0)
store %struct.ScmObj* %f47104, %struct.ScmObj** %stackaddr$env-ref58680
%stackaddr$env-ref58681 = alloca %struct.ScmObj*, align 8
%k47520 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48387, i64 1)
store %struct.ScmObj* %k47520, %struct.ScmObj** %stackaddr$env-ref58681
%stackaddr$env-ref58682 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48387, i64 2)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref58682
%stackaddr$env-ref58683 = alloca %struct.ScmObj*, align 8
%lsts_4347109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48387, i64 3)
store %struct.ScmObj* %lsts_4347109, %struct.ScmObj** %stackaddr$env-ref58683
%stackaddr$env-ref58684 = alloca %struct.ScmObj*, align 8
%vs47107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48387, i64 4)
store %struct.ScmObj* %vs47107, %struct.ScmObj** %stackaddr$env-ref58684
%stackaddr$env-ref58685 = alloca %struct.ScmObj*, align 8
%acc47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48387, i64 5)
store %struct.ScmObj* %acc47103, %struct.ScmObj** %stackaddr$env-ref58685
%stackaddr$env-ref58686 = alloca %struct.ScmObj*, align 8
%_37foldr47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48387, i64 6)
store %struct.ScmObj* %_37foldr47100, %struct.ScmObj** %stackaddr$env-ref58686
%stackaddr$prim58687 = alloca %struct.ScmObj*, align 8
%_95k47527 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56866)
store volatile %struct.ScmObj* %_95k47527, %struct.ScmObj** %stackaddr$prim58687, align 8
%stackaddr$prim58688 = alloca %struct.ScmObj*, align 8
%current_45args56867 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56866)
store volatile %struct.ScmObj* %current_45args56867, %struct.ScmObj** %stackaddr$prim58688, align 8
%stackaddr$prim58689 = alloca %struct.ScmObj*, align 8
%anf_45bind47256 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56867)
store volatile %struct.ScmObj* %anf_45bind47256, %struct.ScmObj** %stackaddr$prim58689, align 8
%stackaddr$prim58690 = alloca %struct.ScmObj*, align 8
%anf_45bind47257 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47103, %struct.ScmObj* %lsts_4347109)
store volatile %struct.ScmObj* %anf_45bind47257, %struct.ScmObj** %stackaddr$prim58690, align 8
%stackaddr$prim58691 = alloca %struct.ScmObj*, align 8
%anf_45bind47258 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47104, %struct.ScmObj* %anf_45bind47257)
store volatile %struct.ScmObj* %anf_45bind47258, %struct.ScmObj** %stackaddr$prim58691, align 8
%stackaddr$makeclosure58692 = alloca %struct.ScmObj*, align 8
%fptrToInt58693 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48413 to i64
%ae48413 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt58693)
store volatile %struct.ScmObj* %ae48413, %struct.ScmObj** %stackaddr$makeclosure58692, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48413, %struct.ScmObj* %f47104, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48413, %struct.ScmObj* %k47520, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48413, %struct.ScmObj* %_37foldr147094, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48413, %struct.ScmObj* %vs47107, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48413, %struct.ScmObj* %anf_45bind47256, i64 4)
%stackaddr$prim58694 = alloca %struct.ScmObj*, align 8
%cpsargs47531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48413, %struct.ScmObj* %anf_45bind47258)
store volatile %struct.ScmObj* %cpsargs47531, %struct.ScmObj** %stackaddr$prim58694, align 8
%clofunc58695 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47100)
musttail call tailcc void %clofunc58695(%struct.ScmObj* %_37foldr47100, %struct.ScmObj* %cpsargs47531)
ret void
}

define tailcc void @proc_clo$ae48413(%struct.ScmObj* %env$ae48413,%struct.ScmObj* %current_45args56869) {
%stackaddr$env-ref58696 = alloca %struct.ScmObj*, align 8
%f47104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48413, i64 0)
store %struct.ScmObj* %f47104, %struct.ScmObj** %stackaddr$env-ref58696
%stackaddr$env-ref58697 = alloca %struct.ScmObj*, align 8
%k47520 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48413, i64 1)
store %struct.ScmObj* %k47520, %struct.ScmObj** %stackaddr$env-ref58697
%stackaddr$env-ref58698 = alloca %struct.ScmObj*, align 8
%_37foldr147094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48413, i64 2)
store %struct.ScmObj* %_37foldr147094, %struct.ScmObj** %stackaddr$env-ref58698
%stackaddr$env-ref58699 = alloca %struct.ScmObj*, align 8
%vs47107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48413, i64 3)
store %struct.ScmObj* %vs47107, %struct.ScmObj** %stackaddr$env-ref58699
%stackaddr$env-ref58700 = alloca %struct.ScmObj*, align 8
%anf_45bind47256 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48413, i64 4)
store %struct.ScmObj* %anf_45bind47256, %struct.ScmObj** %stackaddr$env-ref58700
%stackaddr$prim58701 = alloca %struct.ScmObj*, align 8
%_95k47528 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56869)
store volatile %struct.ScmObj* %_95k47528, %struct.ScmObj** %stackaddr$prim58701, align 8
%stackaddr$prim58702 = alloca %struct.ScmObj*, align 8
%current_45args56870 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56869)
store volatile %struct.ScmObj* %current_45args56870, %struct.ScmObj** %stackaddr$prim58702, align 8
%stackaddr$prim58703 = alloca %struct.ScmObj*, align 8
%anf_45bind47259 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56870)
store volatile %struct.ScmObj* %anf_45bind47259, %struct.ScmObj** %stackaddr$prim58703, align 8
%ae48418 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58704 = alloca %struct.ScmObj*, align 8
%anf_45bind47260 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47259, %struct.ScmObj* %ae48418)
store volatile %struct.ScmObj* %anf_45bind47260, %struct.ScmObj** %stackaddr$prim58704, align 8
%stackaddr$makeclosure58705 = alloca %struct.ScmObj*, align 8
%fptrToInt58706 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48420 to i64
%ae48420 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58706)
store volatile %struct.ScmObj* %ae48420, %struct.ScmObj** %stackaddr$makeclosure58705, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48420, %struct.ScmObj* %f47104, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48420, %struct.ScmObj* %k47520, i64 1)
%argslist56875$_37foldr1470940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58707 = alloca %struct.ScmObj*, align 8
%argslist56875$_37foldr1470941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47107, %struct.ScmObj* %argslist56875$_37foldr1470940)
store volatile %struct.ScmObj* %argslist56875$_37foldr1470941, %struct.ScmObj** %stackaddr$prim58707, align 8
%stackaddr$prim58708 = alloca %struct.ScmObj*, align 8
%argslist56875$_37foldr1470942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47260, %struct.ScmObj* %argslist56875$_37foldr1470941)
store volatile %struct.ScmObj* %argslist56875$_37foldr1470942, %struct.ScmObj** %stackaddr$prim58708, align 8
%stackaddr$prim58709 = alloca %struct.ScmObj*, align 8
%argslist56875$_37foldr1470943 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47256, %struct.ScmObj* %argslist56875$_37foldr1470942)
store volatile %struct.ScmObj* %argslist56875$_37foldr1470943, %struct.ScmObj** %stackaddr$prim58709, align 8
%stackaddr$prim58710 = alloca %struct.ScmObj*, align 8
%argslist56875$_37foldr1470944 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48420, %struct.ScmObj* %argslist56875$_37foldr1470943)
store volatile %struct.ScmObj* %argslist56875$_37foldr1470944, %struct.ScmObj** %stackaddr$prim58710, align 8
%clofunc58711 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147094)
musttail call tailcc void %clofunc58711(%struct.ScmObj* %_37foldr147094, %struct.ScmObj* %argslist56875$_37foldr1470944)
ret void
}

define tailcc void @proc_clo$ae48420(%struct.ScmObj* %env$ae48420,%struct.ScmObj* %current_45args56872) {
%stackaddr$env-ref58712 = alloca %struct.ScmObj*, align 8
%f47104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48420, i64 0)
store %struct.ScmObj* %f47104, %struct.ScmObj** %stackaddr$env-ref58712
%stackaddr$env-ref58713 = alloca %struct.ScmObj*, align 8
%k47520 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48420, i64 1)
store %struct.ScmObj* %k47520, %struct.ScmObj** %stackaddr$env-ref58713
%stackaddr$prim58714 = alloca %struct.ScmObj*, align 8
%_95k47529 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56872)
store volatile %struct.ScmObj* %_95k47529, %struct.ScmObj** %stackaddr$prim58714, align 8
%stackaddr$prim58715 = alloca %struct.ScmObj*, align 8
%current_45args56873 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56872)
store volatile %struct.ScmObj* %current_45args56873, %struct.ScmObj** %stackaddr$prim58715, align 8
%stackaddr$prim58716 = alloca %struct.ScmObj*, align 8
%anf_45bind47261 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56873)
store volatile %struct.ScmObj* %anf_45bind47261, %struct.ScmObj** %stackaddr$prim58716, align 8
%stackaddr$prim58717 = alloca %struct.ScmObj*, align 8
%cpsargs47530 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47520, %struct.ScmObj* %anf_45bind47261)
store volatile %struct.ScmObj* %cpsargs47530, %struct.ScmObj** %stackaddr$prim58717, align 8
%clofunc58718 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47104)
musttail call tailcc void %clofunc58718(%struct.ScmObj* %f47104, %struct.ScmObj* %cpsargs47530)
ret void
}

define tailcc void @proc_clo$ae48389(%struct.ScmObj* %env$ae48389,%struct.ScmObj* %current_45args56876) {
%stackaddr$prim58719 = alloca %struct.ScmObj*, align 8
%k47532 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56876)
store volatile %struct.ScmObj* %k47532, %struct.ScmObj** %stackaddr$prim58719, align 8
%stackaddr$prim58720 = alloca %struct.ScmObj*, align 8
%current_45args56877 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56876)
store volatile %struct.ScmObj* %current_45args56877, %struct.ScmObj** %stackaddr$prim58720, align 8
%stackaddr$prim58721 = alloca %struct.ScmObj*, align 8
%a47112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56877)
store volatile %struct.ScmObj* %a47112, %struct.ScmObj** %stackaddr$prim58721, align 8
%stackaddr$prim58722 = alloca %struct.ScmObj*, align 8
%current_45args56878 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56877)
store volatile %struct.ScmObj* %current_45args56878, %struct.ScmObj** %stackaddr$prim58722, align 8
%stackaddr$prim58723 = alloca %struct.ScmObj*, align 8
%b47111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56878)
store volatile %struct.ScmObj* %b47111, %struct.ScmObj** %stackaddr$prim58723, align 8
%stackaddr$prim58724 = alloca %struct.ScmObj*, align 8
%cpsprim47533 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47112, %struct.ScmObj* %b47111)
store volatile %struct.ScmObj* %cpsprim47533, %struct.ScmObj** %stackaddr$prim58724, align 8
%ae48393 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56880$k475320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58725 = alloca %struct.ScmObj*, align 8
%argslist56880$k475321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47533, %struct.ScmObj* %argslist56880$k475320)
store volatile %struct.ScmObj* %argslist56880$k475321, %struct.ScmObj** %stackaddr$prim58725, align 8
%stackaddr$prim58726 = alloca %struct.ScmObj*, align 8
%argslist56880$k475322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48393, %struct.ScmObj* %argslist56880$k475321)
store volatile %struct.ScmObj* %argslist56880$k475322, %struct.ScmObj** %stackaddr$prim58726, align 8
%clofunc58727 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47532)
musttail call tailcc void %clofunc58727(%struct.ScmObj* %k47532, %struct.ScmObj* %argslist56880$k475322)
ret void
}

define tailcc void @proc_clo$ae48365(%struct.ScmObj* %env$ae48365,%struct.ScmObj* %current_45args56883) {
%stackaddr$prim58728 = alloca %struct.ScmObj*, align 8
%k47534 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56883)
store volatile %struct.ScmObj* %k47534, %struct.ScmObj** %stackaddr$prim58728, align 8
%stackaddr$prim58729 = alloca %struct.ScmObj*, align 8
%current_45args56884 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56883)
store volatile %struct.ScmObj* %current_45args56884, %struct.ScmObj** %stackaddr$prim58729, align 8
%stackaddr$prim58730 = alloca %struct.ScmObj*, align 8
%x47108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56884)
store volatile %struct.ScmObj* %x47108, %struct.ScmObj** %stackaddr$prim58730, align 8
%stackaddr$prim58731 = alloca %struct.ScmObj*, align 8
%cpsprim47535 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47108)
store volatile %struct.ScmObj* %cpsprim47535, %struct.ScmObj** %stackaddr$prim58731, align 8
%ae48368 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56886$k475340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58732 = alloca %struct.ScmObj*, align 8
%argslist56886$k475341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47535, %struct.ScmObj* %argslist56886$k475340)
store volatile %struct.ScmObj* %argslist56886$k475341, %struct.ScmObj** %stackaddr$prim58732, align 8
%stackaddr$prim58733 = alloca %struct.ScmObj*, align 8
%argslist56886$k475342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48368, %struct.ScmObj* %argslist56886$k475341)
store volatile %struct.ScmObj* %argslist56886$k475342, %struct.ScmObj** %stackaddr$prim58733, align 8
%clofunc58734 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47534)
musttail call tailcc void %clofunc58734(%struct.ScmObj* %k47534, %struct.ScmObj* %argslist56886$k475342)
ret void
}

define tailcc void @proc_clo$ae48341(%struct.ScmObj* %env$ae48341,%struct.ScmObj* %current_45args56889) {
%stackaddr$prim58735 = alloca %struct.ScmObj*, align 8
%k47536 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56889)
store volatile %struct.ScmObj* %k47536, %struct.ScmObj** %stackaddr$prim58735, align 8
%stackaddr$prim58736 = alloca %struct.ScmObj*, align 8
%current_45args56890 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56889)
store volatile %struct.ScmObj* %current_45args56890, %struct.ScmObj** %stackaddr$prim58736, align 8
%stackaddr$prim58737 = alloca %struct.ScmObj*, align 8
%x47110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56890)
store volatile %struct.ScmObj* %x47110, %struct.ScmObj** %stackaddr$prim58737, align 8
%stackaddr$prim58738 = alloca %struct.ScmObj*, align 8
%cpsprim47537 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47110)
store volatile %struct.ScmObj* %cpsprim47537, %struct.ScmObj** %stackaddr$prim58738, align 8
%ae48344 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56892$k475360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58739 = alloca %struct.ScmObj*, align 8
%argslist56892$k475361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47537, %struct.ScmObj* %argslist56892$k475360)
store volatile %struct.ScmObj* %argslist56892$k475361, %struct.ScmObj** %stackaddr$prim58739, align 8
%stackaddr$prim58740 = alloca %struct.ScmObj*, align 8
%argslist56892$k475362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48344, %struct.ScmObj* %argslist56892$k475361)
store volatile %struct.ScmObj* %argslist56892$k475362, %struct.ScmObj** %stackaddr$prim58740, align 8
%clofunc58741 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47536)
musttail call tailcc void %clofunc58741(%struct.ScmObj* %k47536, %struct.ScmObj* %argslist56892$k475362)
ret void
}

define tailcc void @proc_clo$ae48293(%struct.ScmObj* %env$ae48293,%struct.ScmObj* %current_45args56895) {
%stackaddr$prim58742 = alloca %struct.ScmObj*, align 8
%k47538 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56895)
store volatile %struct.ScmObj* %k47538, %struct.ScmObj** %stackaddr$prim58742, align 8
%stackaddr$prim58743 = alloca %struct.ScmObj*, align 8
%current_45args56896 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56895)
store volatile %struct.ScmObj* %current_45args56896, %struct.ScmObj** %stackaddr$prim58743, align 8
%stackaddr$prim58744 = alloca %struct.ScmObj*, align 8
%lst47106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56896)
store volatile %struct.ScmObj* %lst47106, %struct.ScmObj** %stackaddr$prim58744, align 8
%stackaddr$prim58745 = alloca %struct.ScmObj*, align 8
%current_45args56897 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56896)
store volatile %struct.ScmObj* %current_45args56897, %struct.ScmObj** %stackaddr$prim58745, align 8
%stackaddr$prim58746 = alloca %struct.ScmObj*, align 8
%b47105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56897)
store volatile %struct.ScmObj* %b47105, %struct.ScmObj** %stackaddr$prim58746, align 8
%truthy$cmp58747 = call i64 @is_truthy_value(%struct.ScmObj* %b47105)
%cmp$cmp58747 = icmp eq i64 %truthy$cmp58747, 1
br i1 %cmp$cmp58747, label %truebranch$cmp58747, label %falsebranch$cmp58747
truebranch$cmp58747:
%ae48296 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56899$k475380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58748 = alloca %struct.ScmObj*, align 8
%argslist56899$k475381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47105, %struct.ScmObj* %argslist56899$k475380)
store volatile %struct.ScmObj* %argslist56899$k475381, %struct.ScmObj** %stackaddr$prim58748, align 8
%stackaddr$prim58749 = alloca %struct.ScmObj*, align 8
%argslist56899$k475382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48296, %struct.ScmObj* %argslist56899$k475381)
store volatile %struct.ScmObj* %argslist56899$k475382, %struct.ScmObj** %stackaddr$prim58749, align 8
%clofunc58750 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47538)
musttail call tailcc void %clofunc58750(%struct.ScmObj* %k47538, %struct.ScmObj* %argslist56899$k475382)
ret void
falsebranch$cmp58747:
%stackaddr$prim58751 = alloca %struct.ScmObj*, align 8
%cpsprim47539 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47106)
store volatile %struct.ScmObj* %cpsprim47539, %struct.ScmObj** %stackaddr$prim58751, align 8
%ae48303 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56900$k475380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58752 = alloca %struct.ScmObj*, align 8
%argslist56900$k475381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47539, %struct.ScmObj* %argslist56900$k475380)
store volatile %struct.ScmObj* %argslist56900$k475381, %struct.ScmObj** %stackaddr$prim58752, align 8
%stackaddr$prim58753 = alloca %struct.ScmObj*, align 8
%argslist56900$k475382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48303, %struct.ScmObj* %argslist56900$k475381)
store volatile %struct.ScmObj* %argslist56900$k475382, %struct.ScmObj** %stackaddr$prim58753, align 8
%clofunc58754 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47538)
musttail call tailcc void %clofunc58754(%struct.ScmObj* %k47538, %struct.ScmObj* %argslist56900$k475382)
ret void
}

define tailcc void @proc_clo$ae48250(%struct.ScmObj* %env$ae48250,%struct.ScmObj* %current_45args56904) {
%stackaddr$env-ref58755 = alloca %struct.ScmObj*, align 8
%_37take47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48250, i64 0)
store %struct.ScmObj* %_37take47086, %struct.ScmObj** %stackaddr$env-ref58755
%stackaddr$env-ref58756 = alloca %struct.ScmObj*, align 8
%_37length47083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48250, i64 1)
store %struct.ScmObj* %_37length47083, %struct.ScmObj** %stackaddr$env-ref58756
%stackaddr$prim58757 = alloca %struct.ScmObj*, align 8
%k47540 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56904)
store volatile %struct.ScmObj* %k47540, %struct.ScmObj** %stackaddr$prim58757, align 8
%stackaddr$prim58758 = alloca %struct.ScmObj*, align 8
%current_45args56905 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56904)
store volatile %struct.ScmObj* %current_45args56905, %struct.ScmObj** %stackaddr$prim58758, align 8
%stackaddr$prim58759 = alloca %struct.ScmObj*, align 8
%lst47115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56905)
store volatile %struct.ScmObj* %lst47115, %struct.ScmObj** %stackaddr$prim58759, align 8
%stackaddr$prim58760 = alloca %struct.ScmObj*, align 8
%current_45args56906 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56905)
store volatile %struct.ScmObj* %current_45args56906, %struct.ScmObj** %stackaddr$prim58760, align 8
%stackaddr$prim58761 = alloca %struct.ScmObj*, align 8
%n47114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56906)
store volatile %struct.ScmObj* %n47114, %struct.ScmObj** %stackaddr$prim58761, align 8
%stackaddr$makeclosure58762 = alloca %struct.ScmObj*, align 8
%fptrToInt58763 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48252 to i64
%ae48252 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58763)
store volatile %struct.ScmObj* %ae48252, %struct.ScmObj** %stackaddr$makeclosure58762, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48252, %struct.ScmObj* %k47540, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48252, %struct.ScmObj* %_37take47086, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48252, %struct.ScmObj* %lst47115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48252, %struct.ScmObj* %n47114, i64 3)
%argslist56912$_37length470830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58764 = alloca %struct.ScmObj*, align 8
%argslist56912$_37length470831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47115, %struct.ScmObj* %argslist56912$_37length470830)
store volatile %struct.ScmObj* %argslist56912$_37length470831, %struct.ScmObj** %stackaddr$prim58764, align 8
%stackaddr$prim58765 = alloca %struct.ScmObj*, align 8
%argslist56912$_37length470832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48252, %struct.ScmObj* %argslist56912$_37length470831)
store volatile %struct.ScmObj* %argslist56912$_37length470832, %struct.ScmObj** %stackaddr$prim58765, align 8
%clofunc58766 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47083)
musttail call tailcc void %clofunc58766(%struct.ScmObj* %_37length47083, %struct.ScmObj* %argslist56912$_37length470832)
ret void
}

define tailcc void @proc_clo$ae48252(%struct.ScmObj* %env$ae48252,%struct.ScmObj* %current_45args56908) {
%stackaddr$env-ref58767 = alloca %struct.ScmObj*, align 8
%k47540 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48252, i64 0)
store %struct.ScmObj* %k47540, %struct.ScmObj** %stackaddr$env-ref58767
%stackaddr$env-ref58768 = alloca %struct.ScmObj*, align 8
%_37take47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48252, i64 1)
store %struct.ScmObj* %_37take47086, %struct.ScmObj** %stackaddr$env-ref58768
%stackaddr$env-ref58769 = alloca %struct.ScmObj*, align 8
%lst47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48252, i64 2)
store %struct.ScmObj* %lst47115, %struct.ScmObj** %stackaddr$env-ref58769
%stackaddr$env-ref58770 = alloca %struct.ScmObj*, align 8
%n47114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48252, i64 3)
store %struct.ScmObj* %n47114, %struct.ScmObj** %stackaddr$env-ref58770
%stackaddr$prim58771 = alloca %struct.ScmObj*, align 8
%_95k47541 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56908)
store volatile %struct.ScmObj* %_95k47541, %struct.ScmObj** %stackaddr$prim58771, align 8
%stackaddr$prim58772 = alloca %struct.ScmObj*, align 8
%current_45args56909 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56908)
store volatile %struct.ScmObj* %current_45args56909, %struct.ScmObj** %stackaddr$prim58772, align 8
%stackaddr$prim58773 = alloca %struct.ScmObj*, align 8
%anf_45bind47248 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56909)
store volatile %struct.ScmObj* %anf_45bind47248, %struct.ScmObj** %stackaddr$prim58773, align 8
%stackaddr$prim58774 = alloca %struct.ScmObj*, align 8
%anf_45bind47249 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47248, %struct.ScmObj* %n47114)
store volatile %struct.ScmObj* %anf_45bind47249, %struct.ScmObj** %stackaddr$prim58774, align 8
%argslist56911$_37take470860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58775 = alloca %struct.ScmObj*, align 8
%argslist56911$_37take470861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47249, %struct.ScmObj* %argslist56911$_37take470860)
store volatile %struct.ScmObj* %argslist56911$_37take470861, %struct.ScmObj** %stackaddr$prim58775, align 8
%stackaddr$prim58776 = alloca %struct.ScmObj*, align 8
%argslist56911$_37take470862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47115, %struct.ScmObj* %argslist56911$_37take470861)
store volatile %struct.ScmObj* %argslist56911$_37take470862, %struct.ScmObj** %stackaddr$prim58776, align 8
%stackaddr$prim58777 = alloca %struct.ScmObj*, align 8
%argslist56911$_37take470863 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47540, %struct.ScmObj* %argslist56911$_37take470862)
store volatile %struct.ScmObj* %argslist56911$_37take470863, %struct.ScmObj** %stackaddr$prim58777, align 8
%clofunc58778 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47086)
musttail call tailcc void %clofunc58778(%struct.ScmObj* %_37take47086, %struct.ScmObj* %argslist56911$_37take470863)
ret void
}

define tailcc void @proc_clo$ae48196(%struct.ScmObj* %env$ae48196,%struct.ScmObj* %current_45args56914) {
%stackaddr$env-ref58779 = alloca %struct.ScmObj*, align 8
%_37foldl147078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48196, i64 0)
store %struct.ScmObj* %_37foldl147078, %struct.ScmObj** %stackaddr$env-ref58779
%stackaddr$prim58780 = alloca %struct.ScmObj*, align 8
%k47542 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56914)
store volatile %struct.ScmObj* %k47542, %struct.ScmObj** %stackaddr$prim58780, align 8
%stackaddr$prim58781 = alloca %struct.ScmObj*, align 8
%current_45args56915 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56914)
store volatile %struct.ScmObj* %current_45args56915, %struct.ScmObj** %stackaddr$prim58781, align 8
%stackaddr$prim58782 = alloca %struct.ScmObj*, align 8
%lst47117 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56915)
store volatile %struct.ScmObj* %lst47117, %struct.ScmObj** %stackaddr$prim58782, align 8
%stackaddr$makeclosure58783 = alloca %struct.ScmObj*, align 8
%fptrToInt58784 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48197 to i64
%ae48197 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58784)
store volatile %struct.ScmObj* %ae48197, %struct.ScmObj** %stackaddr$makeclosure58783, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48197, %struct.ScmObj* %_37foldl147078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48197, %struct.ScmObj* %k47542, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48197, %struct.ScmObj* %lst47117, i64 2)
%ae48198 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58785 = alloca %struct.ScmObj*, align 8
%fptrToInt58786 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48199 to i64
%ae48199 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58786)
store volatile %struct.ScmObj* %ae48199, %struct.ScmObj** %stackaddr$makeclosure58785, align 8
%argslist56926$ae481970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58787 = alloca %struct.ScmObj*, align 8
%argslist56926$ae481971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48199, %struct.ScmObj* %argslist56926$ae481970)
store volatile %struct.ScmObj* %argslist56926$ae481971, %struct.ScmObj** %stackaddr$prim58787, align 8
%stackaddr$prim58788 = alloca %struct.ScmObj*, align 8
%argslist56926$ae481972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48198, %struct.ScmObj* %argslist56926$ae481971)
store volatile %struct.ScmObj* %argslist56926$ae481972, %struct.ScmObj** %stackaddr$prim58788, align 8
%clofunc58789 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48197)
musttail call tailcc void %clofunc58789(%struct.ScmObj* %ae48197, %struct.ScmObj* %argslist56926$ae481972)
ret void
}

define tailcc void @proc_clo$ae48197(%struct.ScmObj* %env$ae48197,%struct.ScmObj* %current_45args56917) {
%stackaddr$env-ref58790 = alloca %struct.ScmObj*, align 8
%_37foldl147078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48197, i64 0)
store %struct.ScmObj* %_37foldl147078, %struct.ScmObj** %stackaddr$env-ref58790
%stackaddr$env-ref58791 = alloca %struct.ScmObj*, align 8
%k47542 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48197, i64 1)
store %struct.ScmObj* %k47542, %struct.ScmObj** %stackaddr$env-ref58791
%stackaddr$env-ref58792 = alloca %struct.ScmObj*, align 8
%lst47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48197, i64 2)
store %struct.ScmObj* %lst47117, %struct.ScmObj** %stackaddr$env-ref58792
%stackaddr$prim58793 = alloca %struct.ScmObj*, align 8
%_95k47543 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56917)
store volatile %struct.ScmObj* %_95k47543, %struct.ScmObj** %stackaddr$prim58793, align 8
%stackaddr$prim58794 = alloca %struct.ScmObj*, align 8
%current_45args56918 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56917)
store volatile %struct.ScmObj* %current_45args56918, %struct.ScmObj** %stackaddr$prim58794, align 8
%stackaddr$prim58795 = alloca %struct.ScmObj*, align 8
%anf_45bind47247 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56918)
store volatile %struct.ScmObj* %anf_45bind47247, %struct.ScmObj** %stackaddr$prim58795, align 8
%ae48218 = call %struct.ScmObj* @const_init_null()
%argslist56920$_37foldl1470780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58796 = alloca %struct.ScmObj*, align 8
%argslist56920$_37foldl1470781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47117, %struct.ScmObj* %argslist56920$_37foldl1470780)
store volatile %struct.ScmObj* %argslist56920$_37foldl1470781, %struct.ScmObj** %stackaddr$prim58796, align 8
%stackaddr$prim58797 = alloca %struct.ScmObj*, align 8
%argslist56920$_37foldl1470782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48218, %struct.ScmObj* %argslist56920$_37foldl1470781)
store volatile %struct.ScmObj* %argslist56920$_37foldl1470782, %struct.ScmObj** %stackaddr$prim58797, align 8
%stackaddr$prim58798 = alloca %struct.ScmObj*, align 8
%argslist56920$_37foldl1470783 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47247, %struct.ScmObj* %argslist56920$_37foldl1470782)
store volatile %struct.ScmObj* %argslist56920$_37foldl1470783, %struct.ScmObj** %stackaddr$prim58798, align 8
%stackaddr$prim58799 = alloca %struct.ScmObj*, align 8
%argslist56920$_37foldl1470784 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47542, %struct.ScmObj* %argslist56920$_37foldl1470783)
store volatile %struct.ScmObj* %argslist56920$_37foldl1470784, %struct.ScmObj** %stackaddr$prim58799, align 8
%clofunc58800 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147078)
musttail call tailcc void %clofunc58800(%struct.ScmObj* %_37foldl147078, %struct.ScmObj* %argslist56920$_37foldl1470784)
ret void
}

define tailcc void @proc_clo$ae48199(%struct.ScmObj* %env$ae48199,%struct.ScmObj* %current_45args56921) {
%stackaddr$prim58801 = alloca %struct.ScmObj*, align 8
%k47544 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56921)
store volatile %struct.ScmObj* %k47544, %struct.ScmObj** %stackaddr$prim58801, align 8
%stackaddr$prim58802 = alloca %struct.ScmObj*, align 8
%current_45args56922 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56921)
store volatile %struct.ScmObj* %current_45args56922, %struct.ScmObj** %stackaddr$prim58802, align 8
%stackaddr$prim58803 = alloca %struct.ScmObj*, align 8
%x47119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56922)
store volatile %struct.ScmObj* %x47119, %struct.ScmObj** %stackaddr$prim58803, align 8
%stackaddr$prim58804 = alloca %struct.ScmObj*, align 8
%current_45args56923 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56922)
store volatile %struct.ScmObj* %current_45args56923, %struct.ScmObj** %stackaddr$prim58804, align 8
%stackaddr$prim58805 = alloca %struct.ScmObj*, align 8
%y47118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56923)
store volatile %struct.ScmObj* %y47118, %struct.ScmObj** %stackaddr$prim58805, align 8
%ae48201 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56925$k475440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58806 = alloca %struct.ScmObj*, align 8
%argslist56925$k475441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x47119, %struct.ScmObj* %argslist56925$k475440)
store volatile %struct.ScmObj* %argslist56925$k475441, %struct.ScmObj** %stackaddr$prim58806, align 8
%stackaddr$prim58807 = alloca %struct.ScmObj*, align 8
%argslist56925$k475442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48201, %struct.ScmObj* %argslist56925$k475441)
store volatile %struct.ScmObj* %argslist56925$k475442, %struct.ScmObj** %stackaddr$prim58807, align 8
%clofunc58808 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47544)
musttail call tailcc void %clofunc58808(%struct.ScmObj* %k47544, %struct.ScmObj* %argslist56925$k475442)
ret void
}

define tailcc void @proc_clo$ae48117(%struct.ScmObj* %env$ae48117,%struct.ScmObj* %current_45args56929) {
%stackaddr$prim58809 = alloca %struct.ScmObj*, align 8
%k47545 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56929)
store volatile %struct.ScmObj* %k47545, %struct.ScmObj** %stackaddr$prim58809, align 8
%stackaddr$prim58810 = alloca %struct.ScmObj*, align 8
%current_45args56930 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56929)
store volatile %struct.ScmObj* %current_45args56930, %struct.ScmObj** %stackaddr$prim58810, align 8
%stackaddr$prim58811 = alloca %struct.ScmObj*, align 8
%_37foldl147079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56930)
store volatile %struct.ScmObj* %_37foldl147079, %struct.ScmObj** %stackaddr$prim58811, align 8
%ae48119 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58812 = alloca %struct.ScmObj*, align 8
%fptrToInt58813 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48120 to i64
%ae48120 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58813)
store volatile %struct.ScmObj* %ae48120, %struct.ScmObj** %stackaddr$makeclosure58812, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48120, %struct.ScmObj* %_37foldl147079, i64 0)
%argslist56943$k475450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58814 = alloca %struct.ScmObj*, align 8
%argslist56943$k475451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48120, %struct.ScmObj* %argslist56943$k475450)
store volatile %struct.ScmObj* %argslist56943$k475451, %struct.ScmObj** %stackaddr$prim58814, align 8
%stackaddr$prim58815 = alloca %struct.ScmObj*, align 8
%argslist56943$k475452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48119, %struct.ScmObj* %argslist56943$k475451)
store volatile %struct.ScmObj* %argslist56943$k475452, %struct.ScmObj** %stackaddr$prim58815, align 8
%clofunc58816 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47545)
musttail call tailcc void %clofunc58816(%struct.ScmObj* %k47545, %struct.ScmObj* %argslist56943$k475452)
ret void
}

define tailcc void @proc_clo$ae48120(%struct.ScmObj* %env$ae48120,%struct.ScmObj* %current_45args56932) {
%stackaddr$env-ref58817 = alloca %struct.ScmObj*, align 8
%_37foldl147079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48120, i64 0)
store %struct.ScmObj* %_37foldl147079, %struct.ScmObj** %stackaddr$env-ref58817
%stackaddr$prim58818 = alloca %struct.ScmObj*, align 8
%k47546 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56932)
store volatile %struct.ScmObj* %k47546, %struct.ScmObj** %stackaddr$prim58818, align 8
%stackaddr$prim58819 = alloca %struct.ScmObj*, align 8
%current_45args56933 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56932)
store volatile %struct.ScmObj* %current_45args56933, %struct.ScmObj** %stackaddr$prim58819, align 8
%stackaddr$prim58820 = alloca %struct.ScmObj*, align 8
%f47082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56933)
store volatile %struct.ScmObj* %f47082, %struct.ScmObj** %stackaddr$prim58820, align 8
%stackaddr$prim58821 = alloca %struct.ScmObj*, align 8
%current_45args56934 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56933)
store volatile %struct.ScmObj* %current_45args56934, %struct.ScmObj** %stackaddr$prim58821, align 8
%stackaddr$prim58822 = alloca %struct.ScmObj*, align 8
%acc47081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56934)
store volatile %struct.ScmObj* %acc47081, %struct.ScmObj** %stackaddr$prim58822, align 8
%stackaddr$prim58823 = alloca %struct.ScmObj*, align 8
%current_45args56935 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56934)
store volatile %struct.ScmObj* %current_45args56935, %struct.ScmObj** %stackaddr$prim58823, align 8
%stackaddr$prim58824 = alloca %struct.ScmObj*, align 8
%lst47080 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56935)
store volatile %struct.ScmObj* %lst47080, %struct.ScmObj** %stackaddr$prim58824, align 8
%stackaddr$prim58825 = alloca %struct.ScmObj*, align 8
%anf_45bind47242 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47080)
store volatile %struct.ScmObj* %anf_45bind47242, %struct.ScmObj** %stackaddr$prim58825, align 8
%truthy$cmp58826 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47242)
%cmp$cmp58826 = icmp eq i64 %truthy$cmp58826, 1
br i1 %cmp$cmp58826, label %truebranch$cmp58826, label %falsebranch$cmp58826
truebranch$cmp58826:
%ae48124 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56937$k475460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58827 = alloca %struct.ScmObj*, align 8
%argslist56937$k475461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47081, %struct.ScmObj* %argslist56937$k475460)
store volatile %struct.ScmObj* %argslist56937$k475461, %struct.ScmObj** %stackaddr$prim58827, align 8
%stackaddr$prim58828 = alloca %struct.ScmObj*, align 8
%argslist56937$k475462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48124, %struct.ScmObj* %argslist56937$k475461)
store volatile %struct.ScmObj* %argslist56937$k475462, %struct.ScmObj** %stackaddr$prim58828, align 8
%clofunc58829 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47546)
musttail call tailcc void %clofunc58829(%struct.ScmObj* %k47546, %struct.ScmObj* %argslist56937$k475462)
ret void
falsebranch$cmp58826:
%stackaddr$prim58830 = alloca %struct.ScmObj*, align 8
%anf_45bind47243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47080)
store volatile %struct.ScmObj* %anf_45bind47243, %struct.ScmObj** %stackaddr$prim58830, align 8
%stackaddr$makeclosure58831 = alloca %struct.ScmObj*, align 8
%fptrToInt58832 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48131 to i64
%ae48131 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58832)
store volatile %struct.ScmObj* %ae48131, %struct.ScmObj** %stackaddr$makeclosure58831, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48131, %struct.ScmObj* %f47082, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48131, %struct.ScmObj* %k47546, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48131, %struct.ScmObj* %lst47080, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48131, %struct.ScmObj* %_37foldl147079, i64 3)
%argslist56942$f470820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58833 = alloca %struct.ScmObj*, align 8
%argslist56942$f470821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47081, %struct.ScmObj* %argslist56942$f470820)
store volatile %struct.ScmObj* %argslist56942$f470821, %struct.ScmObj** %stackaddr$prim58833, align 8
%stackaddr$prim58834 = alloca %struct.ScmObj*, align 8
%argslist56942$f470822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47243, %struct.ScmObj* %argslist56942$f470821)
store volatile %struct.ScmObj* %argslist56942$f470822, %struct.ScmObj** %stackaddr$prim58834, align 8
%stackaddr$prim58835 = alloca %struct.ScmObj*, align 8
%argslist56942$f470823 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48131, %struct.ScmObj* %argslist56942$f470822)
store volatile %struct.ScmObj* %argslist56942$f470823, %struct.ScmObj** %stackaddr$prim58835, align 8
%clofunc58836 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47082)
musttail call tailcc void %clofunc58836(%struct.ScmObj* %f47082, %struct.ScmObj* %argslist56942$f470823)
ret void
}

define tailcc void @proc_clo$ae48131(%struct.ScmObj* %env$ae48131,%struct.ScmObj* %current_45args56938) {
%stackaddr$env-ref58837 = alloca %struct.ScmObj*, align 8
%f47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48131, i64 0)
store %struct.ScmObj* %f47082, %struct.ScmObj** %stackaddr$env-ref58837
%stackaddr$env-ref58838 = alloca %struct.ScmObj*, align 8
%k47546 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48131, i64 1)
store %struct.ScmObj* %k47546, %struct.ScmObj** %stackaddr$env-ref58838
%stackaddr$env-ref58839 = alloca %struct.ScmObj*, align 8
%lst47080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48131, i64 2)
store %struct.ScmObj* %lst47080, %struct.ScmObj** %stackaddr$env-ref58839
%stackaddr$env-ref58840 = alloca %struct.ScmObj*, align 8
%_37foldl147079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48131, i64 3)
store %struct.ScmObj* %_37foldl147079, %struct.ScmObj** %stackaddr$env-ref58840
%stackaddr$prim58841 = alloca %struct.ScmObj*, align 8
%_95k47547 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56938)
store volatile %struct.ScmObj* %_95k47547, %struct.ScmObj** %stackaddr$prim58841, align 8
%stackaddr$prim58842 = alloca %struct.ScmObj*, align 8
%current_45args56939 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56938)
store volatile %struct.ScmObj* %current_45args56939, %struct.ScmObj** %stackaddr$prim58842, align 8
%stackaddr$prim58843 = alloca %struct.ScmObj*, align 8
%anf_45bind47244 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56939)
store volatile %struct.ScmObj* %anf_45bind47244, %struct.ScmObj** %stackaddr$prim58843, align 8
%stackaddr$prim58844 = alloca %struct.ScmObj*, align 8
%anf_45bind47245 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47080)
store volatile %struct.ScmObj* %anf_45bind47245, %struct.ScmObj** %stackaddr$prim58844, align 8
%argslist56941$_37foldl1470790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58845 = alloca %struct.ScmObj*, align 8
%argslist56941$_37foldl1470791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47245, %struct.ScmObj* %argslist56941$_37foldl1470790)
store volatile %struct.ScmObj* %argslist56941$_37foldl1470791, %struct.ScmObj** %stackaddr$prim58845, align 8
%stackaddr$prim58846 = alloca %struct.ScmObj*, align 8
%argslist56941$_37foldl1470792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47244, %struct.ScmObj* %argslist56941$_37foldl1470791)
store volatile %struct.ScmObj* %argslist56941$_37foldl1470792, %struct.ScmObj** %stackaddr$prim58846, align 8
%stackaddr$prim58847 = alloca %struct.ScmObj*, align 8
%argslist56941$_37foldl1470793 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47082, %struct.ScmObj* %argslist56941$_37foldl1470792)
store volatile %struct.ScmObj* %argslist56941$_37foldl1470793, %struct.ScmObj** %stackaddr$prim58847, align 8
%stackaddr$prim58848 = alloca %struct.ScmObj*, align 8
%argslist56941$_37foldl1470794 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47546, %struct.ScmObj* %argslist56941$_37foldl1470793)
store volatile %struct.ScmObj* %argslist56941$_37foldl1470794, %struct.ScmObj** %stackaddr$prim58848, align 8
%clofunc58849 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147079)
musttail call tailcc void %clofunc58849(%struct.ScmObj* %_37foldl147079, %struct.ScmObj* %argslist56941$_37foldl1470794)
ret void
}

define tailcc void @proc_clo$ae48034(%struct.ScmObj* %env$ae48034,%struct.ScmObj* %current_45args56946) {
%stackaddr$prim58850 = alloca %struct.ScmObj*, align 8
%k47548 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56946)
store volatile %struct.ScmObj* %k47548, %struct.ScmObj** %stackaddr$prim58850, align 8
%stackaddr$prim58851 = alloca %struct.ScmObj*, align 8
%current_45args56947 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56946)
store volatile %struct.ScmObj* %current_45args56947, %struct.ScmObj** %stackaddr$prim58851, align 8
%stackaddr$prim58852 = alloca %struct.ScmObj*, align 8
%_37length47084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56947)
store volatile %struct.ScmObj* %_37length47084, %struct.ScmObj** %stackaddr$prim58852, align 8
%ae48036 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58853 = alloca %struct.ScmObj*, align 8
%fptrToInt58854 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48037 to i64
%ae48037 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58854)
store volatile %struct.ScmObj* %ae48037, %struct.ScmObj** %stackaddr$makeclosure58853, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48037, %struct.ScmObj* %_37length47084, i64 0)
%argslist56958$k475480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58855 = alloca %struct.ScmObj*, align 8
%argslist56958$k475481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48037, %struct.ScmObj* %argslist56958$k475480)
store volatile %struct.ScmObj* %argslist56958$k475481, %struct.ScmObj** %stackaddr$prim58855, align 8
%stackaddr$prim58856 = alloca %struct.ScmObj*, align 8
%argslist56958$k475482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48036, %struct.ScmObj* %argslist56958$k475481)
store volatile %struct.ScmObj* %argslist56958$k475482, %struct.ScmObj** %stackaddr$prim58856, align 8
%clofunc58857 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47548)
musttail call tailcc void %clofunc58857(%struct.ScmObj* %k47548, %struct.ScmObj* %argslist56958$k475482)
ret void
}

define tailcc void @proc_clo$ae48037(%struct.ScmObj* %env$ae48037,%struct.ScmObj* %current_45args56949) {
%stackaddr$env-ref58858 = alloca %struct.ScmObj*, align 8
%_37length47084 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48037, i64 0)
store %struct.ScmObj* %_37length47084, %struct.ScmObj** %stackaddr$env-ref58858
%stackaddr$prim58859 = alloca %struct.ScmObj*, align 8
%k47549 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56949)
store volatile %struct.ScmObj* %k47549, %struct.ScmObj** %stackaddr$prim58859, align 8
%stackaddr$prim58860 = alloca %struct.ScmObj*, align 8
%current_45args56950 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56949)
store volatile %struct.ScmObj* %current_45args56950, %struct.ScmObj** %stackaddr$prim58860, align 8
%stackaddr$prim58861 = alloca %struct.ScmObj*, align 8
%lst47085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56950)
store volatile %struct.ScmObj* %lst47085, %struct.ScmObj** %stackaddr$prim58861, align 8
%stackaddr$prim58862 = alloca %struct.ScmObj*, align 8
%anf_45bind47238 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47085)
store volatile %struct.ScmObj* %anf_45bind47238, %struct.ScmObj** %stackaddr$prim58862, align 8
%truthy$cmp58863 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47238)
%cmp$cmp58863 = icmp eq i64 %truthy$cmp58863, 1
br i1 %cmp$cmp58863, label %truebranch$cmp58863, label %falsebranch$cmp58863
truebranch$cmp58863:
%ae48041 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48042 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56952$k475490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58864 = alloca %struct.ScmObj*, align 8
%argslist56952$k475491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48042, %struct.ScmObj* %argslist56952$k475490)
store volatile %struct.ScmObj* %argslist56952$k475491, %struct.ScmObj** %stackaddr$prim58864, align 8
%stackaddr$prim58865 = alloca %struct.ScmObj*, align 8
%argslist56952$k475492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48041, %struct.ScmObj* %argslist56952$k475491)
store volatile %struct.ScmObj* %argslist56952$k475492, %struct.ScmObj** %stackaddr$prim58865, align 8
%clofunc58866 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47549)
musttail call tailcc void %clofunc58866(%struct.ScmObj* %k47549, %struct.ScmObj* %argslist56952$k475492)
ret void
falsebranch$cmp58863:
%stackaddr$prim58867 = alloca %struct.ScmObj*, align 8
%anf_45bind47239 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47085)
store volatile %struct.ScmObj* %anf_45bind47239, %struct.ScmObj** %stackaddr$prim58867, align 8
%stackaddr$makeclosure58868 = alloca %struct.ScmObj*, align 8
%fptrToInt58869 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48051 to i64
%ae48051 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58869)
store volatile %struct.ScmObj* %ae48051, %struct.ScmObj** %stackaddr$makeclosure58868, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48051, %struct.ScmObj* %k47549, i64 0)
%argslist56957$_37length470840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58870 = alloca %struct.ScmObj*, align 8
%argslist56957$_37length470841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47239, %struct.ScmObj* %argslist56957$_37length470840)
store volatile %struct.ScmObj* %argslist56957$_37length470841, %struct.ScmObj** %stackaddr$prim58870, align 8
%stackaddr$prim58871 = alloca %struct.ScmObj*, align 8
%argslist56957$_37length470842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48051, %struct.ScmObj* %argslist56957$_37length470841)
store volatile %struct.ScmObj* %argslist56957$_37length470842, %struct.ScmObj** %stackaddr$prim58871, align 8
%clofunc58872 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47084)
musttail call tailcc void %clofunc58872(%struct.ScmObj* %_37length47084, %struct.ScmObj* %argslist56957$_37length470842)
ret void
}

define tailcc void @proc_clo$ae48051(%struct.ScmObj* %env$ae48051,%struct.ScmObj* %current_45args56953) {
%stackaddr$env-ref58873 = alloca %struct.ScmObj*, align 8
%k47549 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48051, i64 0)
store %struct.ScmObj* %k47549, %struct.ScmObj** %stackaddr$env-ref58873
%stackaddr$prim58874 = alloca %struct.ScmObj*, align 8
%_95k47550 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56953)
store volatile %struct.ScmObj* %_95k47550, %struct.ScmObj** %stackaddr$prim58874, align 8
%stackaddr$prim58875 = alloca %struct.ScmObj*, align 8
%current_45args56954 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56953)
store volatile %struct.ScmObj* %current_45args56954, %struct.ScmObj** %stackaddr$prim58875, align 8
%stackaddr$prim58876 = alloca %struct.ScmObj*, align 8
%anf_45bind47240 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56954)
store volatile %struct.ScmObj* %anf_45bind47240, %struct.ScmObj** %stackaddr$prim58876, align 8
%ae48053 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58877 = alloca %struct.ScmObj*, align 8
%cpsprim47551 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae48053, %struct.ScmObj* %anf_45bind47240)
store volatile %struct.ScmObj* %cpsprim47551, %struct.ScmObj** %stackaddr$prim58877, align 8
%ae48056 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56956$k475490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58878 = alloca %struct.ScmObj*, align 8
%argslist56956$k475491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47551, %struct.ScmObj* %argslist56956$k475490)
store volatile %struct.ScmObj* %argslist56956$k475491, %struct.ScmObj** %stackaddr$prim58878, align 8
%stackaddr$prim58879 = alloca %struct.ScmObj*, align 8
%argslist56956$k475492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48056, %struct.ScmObj* %argslist56956$k475491)
store volatile %struct.ScmObj* %argslist56956$k475492, %struct.ScmObj** %stackaddr$prim58879, align 8
%clofunc58880 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47549)
musttail call tailcc void %clofunc58880(%struct.ScmObj* %k47549, %struct.ScmObj* %argslist56956$k475492)
ret void
}

define tailcc void @proc_clo$ae47884(%struct.ScmObj* %env$ae47884,%struct.ScmObj* %current_45args56961) {
%stackaddr$prim58881 = alloca %struct.ScmObj*, align 8
%k47552 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56961)
store volatile %struct.ScmObj* %k47552, %struct.ScmObj** %stackaddr$prim58881, align 8
%stackaddr$prim58882 = alloca %struct.ScmObj*, align 8
%current_45args56962 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56961)
store volatile %struct.ScmObj* %current_45args56962, %struct.ScmObj** %stackaddr$prim58882, align 8
%stackaddr$prim58883 = alloca %struct.ScmObj*, align 8
%_37take47087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56962)
store volatile %struct.ScmObj* %_37take47087, %struct.ScmObj** %stackaddr$prim58883, align 8
%ae47886 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58884 = alloca %struct.ScmObj*, align 8
%fptrToInt58885 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47887 to i64
%ae47887 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58885)
store volatile %struct.ScmObj* %ae47887, %struct.ScmObj** %stackaddr$makeclosure58884, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47887, %struct.ScmObj* %_37take47087, i64 0)
%argslist56975$k475520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58886 = alloca %struct.ScmObj*, align 8
%argslist56975$k475521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47887, %struct.ScmObj* %argslist56975$k475520)
store volatile %struct.ScmObj* %argslist56975$k475521, %struct.ScmObj** %stackaddr$prim58886, align 8
%stackaddr$prim58887 = alloca %struct.ScmObj*, align 8
%argslist56975$k475522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47886, %struct.ScmObj* %argslist56975$k475521)
store volatile %struct.ScmObj* %argslist56975$k475522, %struct.ScmObj** %stackaddr$prim58887, align 8
%clofunc58888 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47552)
musttail call tailcc void %clofunc58888(%struct.ScmObj* %k47552, %struct.ScmObj* %argslist56975$k475522)
ret void
}

define tailcc void @proc_clo$ae47887(%struct.ScmObj* %env$ae47887,%struct.ScmObj* %current_45args56964) {
%stackaddr$env-ref58889 = alloca %struct.ScmObj*, align 8
%_37take47087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47887, i64 0)
store %struct.ScmObj* %_37take47087, %struct.ScmObj** %stackaddr$env-ref58889
%stackaddr$prim58890 = alloca %struct.ScmObj*, align 8
%k47553 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56964)
store volatile %struct.ScmObj* %k47553, %struct.ScmObj** %stackaddr$prim58890, align 8
%stackaddr$prim58891 = alloca %struct.ScmObj*, align 8
%current_45args56965 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56964)
store volatile %struct.ScmObj* %current_45args56965, %struct.ScmObj** %stackaddr$prim58891, align 8
%stackaddr$prim58892 = alloca %struct.ScmObj*, align 8
%lst47089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56965)
store volatile %struct.ScmObj* %lst47089, %struct.ScmObj** %stackaddr$prim58892, align 8
%stackaddr$prim58893 = alloca %struct.ScmObj*, align 8
%current_45args56966 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56965)
store volatile %struct.ScmObj* %current_45args56966, %struct.ScmObj** %stackaddr$prim58893, align 8
%stackaddr$prim58894 = alloca %struct.ScmObj*, align 8
%n47088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56966)
store volatile %struct.ScmObj* %n47088, %struct.ScmObj** %stackaddr$prim58894, align 8
%ae47889 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58895 = alloca %struct.ScmObj*, align 8
%anf_45bind47231 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n47088, %struct.ScmObj* %ae47889)
store volatile %struct.ScmObj* %anf_45bind47231, %struct.ScmObj** %stackaddr$prim58895, align 8
%truthy$cmp58896 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47231)
%cmp$cmp58896 = icmp eq i64 %truthy$cmp58896, 1
br i1 %cmp$cmp58896, label %truebranch$cmp58896, label %falsebranch$cmp58896
truebranch$cmp58896:
%ae47892 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47893 = call %struct.ScmObj* @const_init_null()
%argslist56968$k475530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58897 = alloca %struct.ScmObj*, align 8
%argslist56968$k475531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47893, %struct.ScmObj* %argslist56968$k475530)
store volatile %struct.ScmObj* %argslist56968$k475531, %struct.ScmObj** %stackaddr$prim58897, align 8
%stackaddr$prim58898 = alloca %struct.ScmObj*, align 8
%argslist56968$k475532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47892, %struct.ScmObj* %argslist56968$k475531)
store volatile %struct.ScmObj* %argslist56968$k475532, %struct.ScmObj** %stackaddr$prim58898, align 8
%clofunc58899 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47553)
musttail call tailcc void %clofunc58899(%struct.ScmObj* %k47553, %struct.ScmObj* %argslist56968$k475532)
ret void
falsebranch$cmp58896:
%stackaddr$prim58900 = alloca %struct.ScmObj*, align 8
%anf_45bind47232 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47089)
store volatile %struct.ScmObj* %anf_45bind47232, %struct.ScmObj** %stackaddr$prim58900, align 8
%truthy$cmp58901 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47232)
%cmp$cmp58901 = icmp eq i64 %truthy$cmp58901, 1
br i1 %cmp$cmp58901, label %truebranch$cmp58901, label %falsebranch$cmp58901
truebranch$cmp58901:
%ae47903 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47904 = call %struct.ScmObj* @const_init_null()
%argslist56969$k475530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58902 = alloca %struct.ScmObj*, align 8
%argslist56969$k475531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47904, %struct.ScmObj* %argslist56969$k475530)
store volatile %struct.ScmObj* %argslist56969$k475531, %struct.ScmObj** %stackaddr$prim58902, align 8
%stackaddr$prim58903 = alloca %struct.ScmObj*, align 8
%argslist56969$k475532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47903, %struct.ScmObj* %argslist56969$k475531)
store volatile %struct.ScmObj* %argslist56969$k475532, %struct.ScmObj** %stackaddr$prim58903, align 8
%clofunc58904 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47553)
musttail call tailcc void %clofunc58904(%struct.ScmObj* %k47553, %struct.ScmObj* %argslist56969$k475532)
ret void
falsebranch$cmp58901:
%stackaddr$prim58905 = alloca %struct.ScmObj*, align 8
%anf_45bind47233 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47089)
store volatile %struct.ScmObj* %anf_45bind47233, %struct.ScmObj** %stackaddr$prim58905, align 8
%stackaddr$prim58906 = alloca %struct.ScmObj*, align 8
%anf_45bind47234 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47089)
store volatile %struct.ScmObj* %anf_45bind47234, %struct.ScmObj** %stackaddr$prim58906, align 8
%ae47914 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58907 = alloca %struct.ScmObj*, align 8
%anf_45bind47235 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n47088, %struct.ScmObj* %ae47914)
store volatile %struct.ScmObj* %anf_45bind47235, %struct.ScmObj** %stackaddr$prim58907, align 8
%stackaddr$makeclosure58908 = alloca %struct.ScmObj*, align 8
%fptrToInt58909 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47916 to i64
%ae47916 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58909)
store volatile %struct.ScmObj* %ae47916, %struct.ScmObj** %stackaddr$makeclosure58908, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47916, %struct.ScmObj* %anf_45bind47233, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47916, %struct.ScmObj* %k47553, i64 1)
%argslist56974$_37take470870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58910 = alloca %struct.ScmObj*, align 8
%argslist56974$_37take470871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47235, %struct.ScmObj* %argslist56974$_37take470870)
store volatile %struct.ScmObj* %argslist56974$_37take470871, %struct.ScmObj** %stackaddr$prim58910, align 8
%stackaddr$prim58911 = alloca %struct.ScmObj*, align 8
%argslist56974$_37take470872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47234, %struct.ScmObj* %argslist56974$_37take470871)
store volatile %struct.ScmObj* %argslist56974$_37take470872, %struct.ScmObj** %stackaddr$prim58911, align 8
%stackaddr$prim58912 = alloca %struct.ScmObj*, align 8
%argslist56974$_37take470873 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47916, %struct.ScmObj* %argslist56974$_37take470872)
store volatile %struct.ScmObj* %argslist56974$_37take470873, %struct.ScmObj** %stackaddr$prim58912, align 8
%clofunc58913 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47087)
musttail call tailcc void %clofunc58913(%struct.ScmObj* %_37take47087, %struct.ScmObj* %argslist56974$_37take470873)
ret void
}

define tailcc void @proc_clo$ae47916(%struct.ScmObj* %env$ae47916,%struct.ScmObj* %current_45args56970) {
%stackaddr$env-ref58914 = alloca %struct.ScmObj*, align 8
%anf_45bind47233 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47916, i64 0)
store %struct.ScmObj* %anf_45bind47233, %struct.ScmObj** %stackaddr$env-ref58914
%stackaddr$env-ref58915 = alloca %struct.ScmObj*, align 8
%k47553 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47916, i64 1)
store %struct.ScmObj* %k47553, %struct.ScmObj** %stackaddr$env-ref58915
%stackaddr$prim58916 = alloca %struct.ScmObj*, align 8
%_95k47554 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56970)
store volatile %struct.ScmObj* %_95k47554, %struct.ScmObj** %stackaddr$prim58916, align 8
%stackaddr$prim58917 = alloca %struct.ScmObj*, align 8
%current_45args56971 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56970)
store volatile %struct.ScmObj* %current_45args56971, %struct.ScmObj** %stackaddr$prim58917, align 8
%stackaddr$prim58918 = alloca %struct.ScmObj*, align 8
%anf_45bind47236 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56971)
store volatile %struct.ScmObj* %anf_45bind47236, %struct.ScmObj** %stackaddr$prim58918, align 8
%stackaddr$prim58919 = alloca %struct.ScmObj*, align 8
%cpsprim47555 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47233, %struct.ScmObj* %anf_45bind47236)
store volatile %struct.ScmObj* %cpsprim47555, %struct.ScmObj** %stackaddr$prim58919, align 8
%ae47922 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56973$k475530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58920 = alloca %struct.ScmObj*, align 8
%argslist56973$k475531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47555, %struct.ScmObj* %argslist56973$k475530)
store volatile %struct.ScmObj* %argslist56973$k475531, %struct.ScmObj** %stackaddr$prim58920, align 8
%stackaddr$prim58921 = alloca %struct.ScmObj*, align 8
%argslist56973$k475532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47922, %struct.ScmObj* %argslist56973$k475531)
store volatile %struct.ScmObj* %argslist56973$k475532, %struct.ScmObj** %stackaddr$prim58921, align 8
%clofunc58922 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47553)
musttail call tailcc void %clofunc58922(%struct.ScmObj* %k47553, %struct.ScmObj* %argslist56973$k475532)
ret void
}

define tailcc void @proc_clo$ae47787(%struct.ScmObj* %env$ae47787,%struct.ScmObj* %current_45args56978) {
%stackaddr$prim58923 = alloca %struct.ScmObj*, align 8
%k47556 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56978)
store volatile %struct.ScmObj* %k47556, %struct.ScmObj** %stackaddr$prim58923, align 8
%stackaddr$prim58924 = alloca %struct.ScmObj*, align 8
%current_45args56979 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56978)
store volatile %struct.ScmObj* %current_45args56979, %struct.ScmObj** %stackaddr$prim58924, align 8
%stackaddr$prim58925 = alloca %struct.ScmObj*, align 8
%_37map47091 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56979)
store volatile %struct.ScmObj* %_37map47091, %struct.ScmObj** %stackaddr$prim58925, align 8
%ae47789 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58926 = alloca %struct.ScmObj*, align 8
%fptrToInt58927 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47790 to i64
%ae47790 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58927)
store volatile %struct.ScmObj* %ae47790, %struct.ScmObj** %stackaddr$makeclosure58926, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47790, %struct.ScmObj* %_37map47091, i64 0)
%argslist56995$k475560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58928 = alloca %struct.ScmObj*, align 8
%argslist56995$k475561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47790, %struct.ScmObj* %argslist56995$k475560)
store volatile %struct.ScmObj* %argslist56995$k475561, %struct.ScmObj** %stackaddr$prim58928, align 8
%stackaddr$prim58929 = alloca %struct.ScmObj*, align 8
%argslist56995$k475562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47789, %struct.ScmObj* %argslist56995$k475561)
store volatile %struct.ScmObj* %argslist56995$k475562, %struct.ScmObj** %stackaddr$prim58929, align 8
%clofunc58930 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47556)
musttail call tailcc void %clofunc58930(%struct.ScmObj* %k47556, %struct.ScmObj* %argslist56995$k475562)
ret void
}

define tailcc void @proc_clo$ae47790(%struct.ScmObj* %env$ae47790,%struct.ScmObj* %current_45args56981) {
%stackaddr$env-ref58931 = alloca %struct.ScmObj*, align 8
%_37map47091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47790, i64 0)
store %struct.ScmObj* %_37map47091, %struct.ScmObj** %stackaddr$env-ref58931
%stackaddr$prim58932 = alloca %struct.ScmObj*, align 8
%k47557 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56981)
store volatile %struct.ScmObj* %k47557, %struct.ScmObj** %stackaddr$prim58932, align 8
%stackaddr$prim58933 = alloca %struct.ScmObj*, align 8
%current_45args56982 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56981)
store volatile %struct.ScmObj* %current_45args56982, %struct.ScmObj** %stackaddr$prim58933, align 8
%stackaddr$prim58934 = alloca %struct.ScmObj*, align 8
%f47093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56982)
store volatile %struct.ScmObj* %f47093, %struct.ScmObj** %stackaddr$prim58934, align 8
%stackaddr$prim58935 = alloca %struct.ScmObj*, align 8
%current_45args56983 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56982)
store volatile %struct.ScmObj* %current_45args56983, %struct.ScmObj** %stackaddr$prim58935, align 8
%stackaddr$prim58936 = alloca %struct.ScmObj*, align 8
%lst47092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56983)
store volatile %struct.ScmObj* %lst47092, %struct.ScmObj** %stackaddr$prim58936, align 8
%stackaddr$prim58937 = alloca %struct.ScmObj*, align 8
%anf_45bind47225 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47092)
store volatile %struct.ScmObj* %anf_45bind47225, %struct.ScmObj** %stackaddr$prim58937, align 8
%truthy$cmp58938 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47225)
%cmp$cmp58938 = icmp eq i64 %truthy$cmp58938, 1
br i1 %cmp$cmp58938, label %truebranch$cmp58938, label %falsebranch$cmp58938
truebranch$cmp58938:
%ae47794 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47795 = call %struct.ScmObj* @const_init_null()
%argslist56985$k475570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58939 = alloca %struct.ScmObj*, align 8
%argslist56985$k475571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47795, %struct.ScmObj* %argslist56985$k475570)
store volatile %struct.ScmObj* %argslist56985$k475571, %struct.ScmObj** %stackaddr$prim58939, align 8
%stackaddr$prim58940 = alloca %struct.ScmObj*, align 8
%argslist56985$k475572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47794, %struct.ScmObj* %argslist56985$k475571)
store volatile %struct.ScmObj* %argslist56985$k475572, %struct.ScmObj** %stackaddr$prim58940, align 8
%clofunc58941 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47557)
musttail call tailcc void %clofunc58941(%struct.ScmObj* %k47557, %struct.ScmObj* %argslist56985$k475572)
ret void
falsebranch$cmp58938:
%stackaddr$prim58942 = alloca %struct.ScmObj*, align 8
%anf_45bind47226 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47092)
store volatile %struct.ScmObj* %anf_45bind47226, %struct.ScmObj** %stackaddr$prim58942, align 8
%stackaddr$makeclosure58943 = alloca %struct.ScmObj*, align 8
%fptrToInt58944 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47804 to i64
%ae47804 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58944)
store volatile %struct.ScmObj* %ae47804, %struct.ScmObj** %stackaddr$makeclosure58943, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47804, %struct.ScmObj* %f47093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47804, %struct.ScmObj* %k47557, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47804, %struct.ScmObj* %lst47092, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47804, %struct.ScmObj* %_37map47091, i64 3)
%argslist56994$f470930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58945 = alloca %struct.ScmObj*, align 8
%argslist56994$f470931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47226, %struct.ScmObj* %argslist56994$f470930)
store volatile %struct.ScmObj* %argslist56994$f470931, %struct.ScmObj** %stackaddr$prim58945, align 8
%stackaddr$prim58946 = alloca %struct.ScmObj*, align 8
%argslist56994$f470932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47804, %struct.ScmObj* %argslist56994$f470931)
store volatile %struct.ScmObj* %argslist56994$f470932, %struct.ScmObj** %stackaddr$prim58946, align 8
%clofunc58947 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47093)
musttail call tailcc void %clofunc58947(%struct.ScmObj* %f47093, %struct.ScmObj* %argslist56994$f470932)
ret void
}

define tailcc void @proc_clo$ae47804(%struct.ScmObj* %env$ae47804,%struct.ScmObj* %current_45args56986) {
%stackaddr$env-ref58948 = alloca %struct.ScmObj*, align 8
%f47093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47804, i64 0)
store %struct.ScmObj* %f47093, %struct.ScmObj** %stackaddr$env-ref58948
%stackaddr$env-ref58949 = alloca %struct.ScmObj*, align 8
%k47557 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47804, i64 1)
store %struct.ScmObj* %k47557, %struct.ScmObj** %stackaddr$env-ref58949
%stackaddr$env-ref58950 = alloca %struct.ScmObj*, align 8
%lst47092 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47804, i64 2)
store %struct.ScmObj* %lst47092, %struct.ScmObj** %stackaddr$env-ref58950
%stackaddr$env-ref58951 = alloca %struct.ScmObj*, align 8
%_37map47091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47804, i64 3)
store %struct.ScmObj* %_37map47091, %struct.ScmObj** %stackaddr$env-ref58951
%stackaddr$prim58952 = alloca %struct.ScmObj*, align 8
%_95k47558 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56986)
store volatile %struct.ScmObj* %_95k47558, %struct.ScmObj** %stackaddr$prim58952, align 8
%stackaddr$prim58953 = alloca %struct.ScmObj*, align 8
%current_45args56987 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56986)
store volatile %struct.ScmObj* %current_45args56987, %struct.ScmObj** %stackaddr$prim58953, align 8
%stackaddr$prim58954 = alloca %struct.ScmObj*, align 8
%anf_45bind47227 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56987)
store volatile %struct.ScmObj* %anf_45bind47227, %struct.ScmObj** %stackaddr$prim58954, align 8
%stackaddr$prim58955 = alloca %struct.ScmObj*, align 8
%anf_45bind47228 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47092)
store volatile %struct.ScmObj* %anf_45bind47228, %struct.ScmObj** %stackaddr$prim58955, align 8
%stackaddr$makeclosure58956 = alloca %struct.ScmObj*, align 8
%fptrToInt58957 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47808 to i64
%ae47808 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58957)
store volatile %struct.ScmObj* %ae47808, %struct.ScmObj** %stackaddr$makeclosure58956, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47808, %struct.ScmObj* %k47557, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47808, %struct.ScmObj* %anf_45bind47227, i64 1)
%argslist56993$_37map470910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58958 = alloca %struct.ScmObj*, align 8
%argslist56993$_37map470911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47228, %struct.ScmObj* %argslist56993$_37map470910)
store volatile %struct.ScmObj* %argslist56993$_37map470911, %struct.ScmObj** %stackaddr$prim58958, align 8
%stackaddr$prim58959 = alloca %struct.ScmObj*, align 8
%argslist56993$_37map470912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47093, %struct.ScmObj* %argslist56993$_37map470911)
store volatile %struct.ScmObj* %argslist56993$_37map470912, %struct.ScmObj** %stackaddr$prim58959, align 8
%stackaddr$prim58960 = alloca %struct.ScmObj*, align 8
%argslist56993$_37map470913 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47808, %struct.ScmObj* %argslist56993$_37map470912)
store volatile %struct.ScmObj* %argslist56993$_37map470913, %struct.ScmObj** %stackaddr$prim58960, align 8
%clofunc58961 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map47091)
musttail call tailcc void %clofunc58961(%struct.ScmObj* %_37map47091, %struct.ScmObj* %argslist56993$_37map470913)
ret void
}

define tailcc void @proc_clo$ae47808(%struct.ScmObj* %env$ae47808,%struct.ScmObj* %current_45args56989) {
%stackaddr$env-ref58962 = alloca %struct.ScmObj*, align 8
%k47557 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47808, i64 0)
store %struct.ScmObj* %k47557, %struct.ScmObj** %stackaddr$env-ref58962
%stackaddr$env-ref58963 = alloca %struct.ScmObj*, align 8
%anf_45bind47227 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47808, i64 1)
store %struct.ScmObj* %anf_45bind47227, %struct.ScmObj** %stackaddr$env-ref58963
%stackaddr$prim58964 = alloca %struct.ScmObj*, align 8
%_95k47559 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56989)
store volatile %struct.ScmObj* %_95k47559, %struct.ScmObj** %stackaddr$prim58964, align 8
%stackaddr$prim58965 = alloca %struct.ScmObj*, align 8
%current_45args56990 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56989)
store volatile %struct.ScmObj* %current_45args56990, %struct.ScmObj** %stackaddr$prim58965, align 8
%stackaddr$prim58966 = alloca %struct.ScmObj*, align 8
%anf_45bind47229 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56990)
store volatile %struct.ScmObj* %anf_45bind47229, %struct.ScmObj** %stackaddr$prim58966, align 8
%stackaddr$prim58967 = alloca %struct.ScmObj*, align 8
%cpsprim47560 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47227, %struct.ScmObj* %anf_45bind47229)
store volatile %struct.ScmObj* %cpsprim47560, %struct.ScmObj** %stackaddr$prim58967, align 8
%ae47814 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56992$k475570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58968 = alloca %struct.ScmObj*, align 8
%argslist56992$k475571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47560, %struct.ScmObj* %argslist56992$k475570)
store volatile %struct.ScmObj* %argslist56992$k475571, %struct.ScmObj** %stackaddr$prim58968, align 8
%stackaddr$prim58969 = alloca %struct.ScmObj*, align 8
%argslist56992$k475572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47814, %struct.ScmObj* %argslist56992$k475571)
store volatile %struct.ScmObj* %argslist56992$k475572, %struct.ScmObj** %stackaddr$prim58969, align 8
%clofunc58970 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47557)
musttail call tailcc void %clofunc58970(%struct.ScmObj* %k47557, %struct.ScmObj* %argslist56992$k475572)
ret void
}

define tailcc void @proc_clo$ae47707(%struct.ScmObj* %env$ae47707,%struct.ScmObj* %current_45args56998) {
%stackaddr$prim58971 = alloca %struct.ScmObj*, align 8
%k47561 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56998)
store volatile %struct.ScmObj* %k47561, %struct.ScmObj** %stackaddr$prim58971, align 8
%stackaddr$prim58972 = alloca %struct.ScmObj*, align 8
%current_45args56999 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56998)
store volatile %struct.ScmObj* %current_45args56999, %struct.ScmObj** %stackaddr$prim58972, align 8
%stackaddr$prim58973 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56999)
store volatile %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$prim58973, align 8
%ae47709 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58974 = alloca %struct.ScmObj*, align 8
%fptrToInt58975 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47710 to i64
%ae47710 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58975)
store volatile %struct.ScmObj* %ae47710, %struct.ScmObj** %stackaddr$makeclosure58974, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47710, %struct.ScmObj* %_37foldr147095, i64 0)
%argslist57012$k475610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58976 = alloca %struct.ScmObj*, align 8
%argslist57012$k475611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47710, %struct.ScmObj* %argslist57012$k475610)
store volatile %struct.ScmObj* %argslist57012$k475611, %struct.ScmObj** %stackaddr$prim58976, align 8
%stackaddr$prim58977 = alloca %struct.ScmObj*, align 8
%argslist57012$k475612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47709, %struct.ScmObj* %argslist57012$k475611)
store volatile %struct.ScmObj* %argslist57012$k475612, %struct.ScmObj** %stackaddr$prim58977, align 8
%clofunc58978 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47561)
musttail call tailcc void %clofunc58978(%struct.ScmObj* %k47561, %struct.ScmObj* %argslist57012$k475612)
ret void
}

define tailcc void @proc_clo$ae47710(%struct.ScmObj* %env$ae47710,%struct.ScmObj* %current_45args57001) {
%stackaddr$env-ref58979 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47710, i64 0)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref58979
%stackaddr$prim58980 = alloca %struct.ScmObj*, align 8
%k47562 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57001)
store volatile %struct.ScmObj* %k47562, %struct.ScmObj** %stackaddr$prim58980, align 8
%stackaddr$prim58981 = alloca %struct.ScmObj*, align 8
%current_45args57002 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57001)
store volatile %struct.ScmObj* %current_45args57002, %struct.ScmObj** %stackaddr$prim58981, align 8
%stackaddr$prim58982 = alloca %struct.ScmObj*, align 8
%f47098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57002)
store volatile %struct.ScmObj* %f47098, %struct.ScmObj** %stackaddr$prim58982, align 8
%stackaddr$prim58983 = alloca %struct.ScmObj*, align 8
%current_45args57003 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57002)
store volatile %struct.ScmObj* %current_45args57003, %struct.ScmObj** %stackaddr$prim58983, align 8
%stackaddr$prim58984 = alloca %struct.ScmObj*, align 8
%acc47097 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57003)
store volatile %struct.ScmObj* %acc47097, %struct.ScmObj** %stackaddr$prim58984, align 8
%stackaddr$prim58985 = alloca %struct.ScmObj*, align 8
%current_45args57004 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57003)
store volatile %struct.ScmObj* %current_45args57004, %struct.ScmObj** %stackaddr$prim58985, align 8
%stackaddr$prim58986 = alloca %struct.ScmObj*, align 8
%lst47096 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57004)
store volatile %struct.ScmObj* %lst47096, %struct.ScmObj** %stackaddr$prim58986, align 8
%stackaddr$prim58987 = alloca %struct.ScmObj*, align 8
%anf_45bind47220 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47096)
store volatile %struct.ScmObj* %anf_45bind47220, %struct.ScmObj** %stackaddr$prim58987, align 8
%truthy$cmp58988 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47220)
%cmp$cmp58988 = icmp eq i64 %truthy$cmp58988, 1
br i1 %cmp$cmp58988, label %truebranch$cmp58988, label %falsebranch$cmp58988
truebranch$cmp58988:
%ae47714 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57006$k475620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58989 = alloca %struct.ScmObj*, align 8
%argslist57006$k475621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47097, %struct.ScmObj* %argslist57006$k475620)
store volatile %struct.ScmObj* %argslist57006$k475621, %struct.ScmObj** %stackaddr$prim58989, align 8
%stackaddr$prim58990 = alloca %struct.ScmObj*, align 8
%argslist57006$k475622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47714, %struct.ScmObj* %argslist57006$k475621)
store volatile %struct.ScmObj* %argslist57006$k475622, %struct.ScmObj** %stackaddr$prim58990, align 8
%clofunc58991 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47562)
musttail call tailcc void %clofunc58991(%struct.ScmObj* %k47562, %struct.ScmObj* %argslist57006$k475622)
ret void
falsebranch$cmp58988:
%stackaddr$prim58992 = alloca %struct.ScmObj*, align 8
%anf_45bind47221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47096)
store volatile %struct.ScmObj* %anf_45bind47221, %struct.ScmObj** %stackaddr$prim58992, align 8
%stackaddr$prim58993 = alloca %struct.ScmObj*, align 8
%anf_45bind47222 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47096)
store volatile %struct.ScmObj* %anf_45bind47222, %struct.ScmObj** %stackaddr$prim58993, align 8
%stackaddr$makeclosure58994 = alloca %struct.ScmObj*, align 8
%fptrToInt58995 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47722 to i64
%ae47722 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58995)
store volatile %struct.ScmObj* %ae47722, %struct.ScmObj** %stackaddr$makeclosure58994, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47722, %struct.ScmObj* %f47098, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47722, %struct.ScmObj* %k47562, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47722, %struct.ScmObj* %anf_45bind47221, i64 2)
%argslist57011$_37foldr1470950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58996 = alloca %struct.ScmObj*, align 8
%argslist57011$_37foldr1470951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47222, %struct.ScmObj* %argslist57011$_37foldr1470950)
store volatile %struct.ScmObj* %argslist57011$_37foldr1470951, %struct.ScmObj** %stackaddr$prim58996, align 8
%stackaddr$prim58997 = alloca %struct.ScmObj*, align 8
%argslist57011$_37foldr1470952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47097, %struct.ScmObj* %argslist57011$_37foldr1470951)
store volatile %struct.ScmObj* %argslist57011$_37foldr1470952, %struct.ScmObj** %stackaddr$prim58997, align 8
%stackaddr$prim58998 = alloca %struct.ScmObj*, align 8
%argslist57011$_37foldr1470953 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47098, %struct.ScmObj* %argslist57011$_37foldr1470952)
store volatile %struct.ScmObj* %argslist57011$_37foldr1470953, %struct.ScmObj** %stackaddr$prim58998, align 8
%stackaddr$prim58999 = alloca %struct.ScmObj*, align 8
%argslist57011$_37foldr1470954 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47722, %struct.ScmObj* %argslist57011$_37foldr1470953)
store volatile %struct.ScmObj* %argslist57011$_37foldr1470954, %struct.ScmObj** %stackaddr$prim58999, align 8
%clofunc59000 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147095)
musttail call tailcc void %clofunc59000(%struct.ScmObj* %_37foldr147095, %struct.ScmObj* %argslist57011$_37foldr1470954)
ret void
}

define tailcc void @proc_clo$ae47722(%struct.ScmObj* %env$ae47722,%struct.ScmObj* %current_45args57007) {
%stackaddr$env-ref59001 = alloca %struct.ScmObj*, align 8
%f47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47722, i64 0)
store %struct.ScmObj* %f47098, %struct.ScmObj** %stackaddr$env-ref59001
%stackaddr$env-ref59002 = alloca %struct.ScmObj*, align 8
%k47562 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47722, i64 1)
store %struct.ScmObj* %k47562, %struct.ScmObj** %stackaddr$env-ref59002
%stackaddr$env-ref59003 = alloca %struct.ScmObj*, align 8
%anf_45bind47221 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47722, i64 2)
store %struct.ScmObj* %anf_45bind47221, %struct.ScmObj** %stackaddr$env-ref59003
%stackaddr$prim59004 = alloca %struct.ScmObj*, align 8
%_95k47563 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57007)
store volatile %struct.ScmObj* %_95k47563, %struct.ScmObj** %stackaddr$prim59004, align 8
%stackaddr$prim59005 = alloca %struct.ScmObj*, align 8
%current_45args57008 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57007)
store volatile %struct.ScmObj* %current_45args57008, %struct.ScmObj** %stackaddr$prim59005, align 8
%stackaddr$prim59006 = alloca %struct.ScmObj*, align 8
%anf_45bind47223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57008)
store volatile %struct.ScmObj* %anf_45bind47223, %struct.ScmObj** %stackaddr$prim59006, align 8
%argslist57010$f470980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59007 = alloca %struct.ScmObj*, align 8
%argslist57010$f470981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47223, %struct.ScmObj* %argslist57010$f470980)
store volatile %struct.ScmObj* %argslist57010$f470981, %struct.ScmObj** %stackaddr$prim59007, align 8
%stackaddr$prim59008 = alloca %struct.ScmObj*, align 8
%argslist57010$f470982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47221, %struct.ScmObj* %argslist57010$f470981)
store volatile %struct.ScmObj* %argslist57010$f470982, %struct.ScmObj** %stackaddr$prim59008, align 8
%stackaddr$prim59009 = alloca %struct.ScmObj*, align 8
%argslist57010$f470983 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47562, %struct.ScmObj* %argslist57010$f470982)
store volatile %struct.ScmObj* %argslist57010$f470983, %struct.ScmObj** %stackaddr$prim59009, align 8
%clofunc59010 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47098)
musttail call tailcc void %clofunc59010(%struct.ScmObj* %f47098, %struct.ScmObj* %argslist57010$f470983)
ret void
}

define tailcc void @proc_clo$ae47590(%struct.ScmObj* %env$ae47590,%struct.ScmObj* %current_45args57015) {
%stackaddr$prim59011 = alloca %struct.ScmObj*, align 8
%k47564 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57015)
store volatile %struct.ScmObj* %k47564, %struct.ScmObj** %stackaddr$prim59011, align 8
%stackaddr$prim59012 = alloca %struct.ScmObj*, align 8
%current_45args57016 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57015)
store volatile %struct.ScmObj* %current_45args57016, %struct.ScmObj** %stackaddr$prim59012, align 8
%stackaddr$prim59013 = alloca %struct.ScmObj*, align 8
%y47075 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57016)
store volatile %struct.ScmObj* %y47075, %struct.ScmObj** %stackaddr$prim59013, align 8
%ae47592 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59014 = alloca %struct.ScmObj*, align 8
%fptrToInt59015 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47593 to i64
%ae47593 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt59015)
store volatile %struct.ScmObj* %ae47593, %struct.ScmObj** %stackaddr$makeclosure59014, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47593, %struct.ScmObj* %y47075, i64 0)
%argslist57034$k475640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59016 = alloca %struct.ScmObj*, align 8
%argslist57034$k475641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47593, %struct.ScmObj* %argslist57034$k475640)
store volatile %struct.ScmObj* %argslist57034$k475641, %struct.ScmObj** %stackaddr$prim59016, align 8
%stackaddr$prim59017 = alloca %struct.ScmObj*, align 8
%argslist57034$k475642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47592, %struct.ScmObj* %argslist57034$k475641)
store volatile %struct.ScmObj* %argslist57034$k475642, %struct.ScmObj** %stackaddr$prim59017, align 8
%clofunc59018 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47564)
musttail call tailcc void %clofunc59018(%struct.ScmObj* %k47564, %struct.ScmObj* %argslist57034$k475642)
ret void
}

define tailcc void @proc_clo$ae47593(%struct.ScmObj* %env$ae47593,%struct.ScmObj* %current_45args57018) {
%stackaddr$env-ref59019 = alloca %struct.ScmObj*, align 8
%y47075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47593, i64 0)
store %struct.ScmObj* %y47075, %struct.ScmObj** %stackaddr$env-ref59019
%stackaddr$prim59020 = alloca %struct.ScmObj*, align 8
%k47565 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57018)
store volatile %struct.ScmObj* %k47565, %struct.ScmObj** %stackaddr$prim59020, align 8
%stackaddr$prim59021 = alloca %struct.ScmObj*, align 8
%current_45args57019 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57018)
store volatile %struct.ScmObj* %current_45args57019, %struct.ScmObj** %stackaddr$prim59021, align 8
%stackaddr$prim59022 = alloca %struct.ScmObj*, align 8
%f47076 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57019)
store volatile %struct.ScmObj* %f47076, %struct.ScmObj** %stackaddr$prim59022, align 8
%stackaddr$makeclosure59023 = alloca %struct.ScmObj*, align 8
%fptrToInt59024 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47594 to i64
%ae47594 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59024)
store volatile %struct.ScmObj* %ae47594, %struct.ScmObj** %stackaddr$makeclosure59023, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47594, %struct.ScmObj* %f47076, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47594, %struct.ScmObj* %k47565, i64 1)
%ae47595 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59025 = alloca %struct.ScmObj*, align 8
%fptrToInt59026 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47596 to i64
%ae47596 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59026)
store volatile %struct.ScmObj* %ae47596, %struct.ScmObj** %stackaddr$makeclosure59025, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47596, %struct.ScmObj* %f47076, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47596, %struct.ScmObj* %y47075, i64 1)
%argslist57033$ae475940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59027 = alloca %struct.ScmObj*, align 8
%argslist57033$ae475941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47596, %struct.ScmObj* %argslist57033$ae475940)
store volatile %struct.ScmObj* %argslist57033$ae475941, %struct.ScmObj** %stackaddr$prim59027, align 8
%stackaddr$prim59028 = alloca %struct.ScmObj*, align 8
%argslist57033$ae475942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47595, %struct.ScmObj* %argslist57033$ae475941)
store volatile %struct.ScmObj* %argslist57033$ae475942, %struct.ScmObj** %stackaddr$prim59028, align 8
%clofunc59029 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47594)
musttail call tailcc void %clofunc59029(%struct.ScmObj* %ae47594, %struct.ScmObj* %argslist57033$ae475942)
ret void
}

define tailcc void @proc_clo$ae47594(%struct.ScmObj* %env$ae47594,%struct.ScmObj* %current_45args57021) {
%stackaddr$env-ref59030 = alloca %struct.ScmObj*, align 8
%f47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47594, i64 0)
store %struct.ScmObj* %f47076, %struct.ScmObj** %stackaddr$env-ref59030
%stackaddr$env-ref59031 = alloca %struct.ScmObj*, align 8
%k47565 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47594, i64 1)
store %struct.ScmObj* %k47565, %struct.ScmObj** %stackaddr$env-ref59031
%stackaddr$prim59032 = alloca %struct.ScmObj*, align 8
%_95k47566 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57021)
store volatile %struct.ScmObj* %_95k47566, %struct.ScmObj** %stackaddr$prim59032, align 8
%stackaddr$prim59033 = alloca %struct.ScmObj*, align 8
%current_45args57022 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57021)
store volatile %struct.ScmObj* %current_45args57022, %struct.ScmObj** %stackaddr$prim59033, align 8
%stackaddr$prim59034 = alloca %struct.ScmObj*, align 8
%anf_45bind47218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57022)
store volatile %struct.ScmObj* %anf_45bind47218, %struct.ScmObj** %stackaddr$prim59034, align 8
%argslist57024$f470760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59035 = alloca %struct.ScmObj*, align 8
%argslist57024$f470761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47218, %struct.ScmObj* %argslist57024$f470760)
store volatile %struct.ScmObj* %argslist57024$f470761, %struct.ScmObj** %stackaddr$prim59035, align 8
%stackaddr$prim59036 = alloca %struct.ScmObj*, align 8
%argslist57024$f470762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47565, %struct.ScmObj* %argslist57024$f470761)
store volatile %struct.ScmObj* %argslist57024$f470762, %struct.ScmObj** %stackaddr$prim59036, align 8
%clofunc59037 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47076)
musttail call tailcc void %clofunc59037(%struct.ScmObj* %f47076, %struct.ScmObj* %argslist57024$f470762)
ret void
}

define tailcc void @proc_clo$ae47596(%struct.ScmObj* %env$ae47596,%struct.ScmObj* %args4707747567) {
%stackaddr$env-ref59038 = alloca %struct.ScmObj*, align 8
%f47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47596, i64 0)
store %struct.ScmObj* %f47076, %struct.ScmObj** %stackaddr$env-ref59038
%stackaddr$env-ref59039 = alloca %struct.ScmObj*, align 8
%y47075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47596, i64 1)
store %struct.ScmObj* %y47075, %struct.ScmObj** %stackaddr$env-ref59039
%stackaddr$prim59040 = alloca %struct.ScmObj*, align 8
%k47568 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4707747567)
store volatile %struct.ScmObj* %k47568, %struct.ScmObj** %stackaddr$prim59040, align 8
%stackaddr$prim59041 = alloca %struct.ScmObj*, align 8
%args47077 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4707747567)
store volatile %struct.ScmObj* %args47077, %struct.ScmObj** %stackaddr$prim59041, align 8
%stackaddr$makeclosure59042 = alloca %struct.ScmObj*, align 8
%fptrToInt59043 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47600 to i64
%ae47600 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt59043)
store volatile %struct.ScmObj* %ae47600, %struct.ScmObj** %stackaddr$makeclosure59042, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47600, %struct.ScmObj* %args47077, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47600, %struct.ScmObj* %f47076, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47600, %struct.ScmObj* %k47568, i64 2)
%argslist57032$y470750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59044 = alloca %struct.ScmObj*, align 8
%argslist57032$y470751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y47075, %struct.ScmObj* %argslist57032$y470750)
store volatile %struct.ScmObj* %argslist57032$y470751, %struct.ScmObj** %stackaddr$prim59044, align 8
%stackaddr$prim59045 = alloca %struct.ScmObj*, align 8
%argslist57032$y470752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47600, %struct.ScmObj* %argslist57032$y470751)
store volatile %struct.ScmObj* %argslist57032$y470752, %struct.ScmObj** %stackaddr$prim59045, align 8
%clofunc59046 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y47075)
musttail call tailcc void %clofunc59046(%struct.ScmObj* %y47075, %struct.ScmObj* %argslist57032$y470752)
ret void
}

define tailcc void @proc_clo$ae47600(%struct.ScmObj* %env$ae47600,%struct.ScmObj* %current_45args57025) {
%stackaddr$env-ref59047 = alloca %struct.ScmObj*, align 8
%args47077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47600, i64 0)
store %struct.ScmObj* %args47077, %struct.ScmObj** %stackaddr$env-ref59047
%stackaddr$env-ref59048 = alloca %struct.ScmObj*, align 8
%f47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47600, i64 1)
store %struct.ScmObj* %f47076, %struct.ScmObj** %stackaddr$env-ref59048
%stackaddr$env-ref59049 = alloca %struct.ScmObj*, align 8
%k47568 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47600, i64 2)
store %struct.ScmObj* %k47568, %struct.ScmObj** %stackaddr$env-ref59049
%stackaddr$prim59050 = alloca %struct.ScmObj*, align 8
%_95k47569 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57025)
store volatile %struct.ScmObj* %_95k47569, %struct.ScmObj** %stackaddr$prim59050, align 8
%stackaddr$prim59051 = alloca %struct.ScmObj*, align 8
%current_45args57026 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57025)
store volatile %struct.ScmObj* %current_45args57026, %struct.ScmObj** %stackaddr$prim59051, align 8
%stackaddr$prim59052 = alloca %struct.ScmObj*, align 8
%anf_45bind47216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57026)
store volatile %struct.ScmObj* %anf_45bind47216, %struct.ScmObj** %stackaddr$prim59052, align 8
%stackaddr$makeclosure59053 = alloca %struct.ScmObj*, align 8
%fptrToInt59054 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47603 to i64
%ae47603 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59054)
store volatile %struct.ScmObj* %ae47603, %struct.ScmObj** %stackaddr$makeclosure59053, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47603, %struct.ScmObj* %args47077, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47603, %struct.ScmObj* %k47568, i64 1)
%argslist57031$anf_45bind472160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59055 = alloca %struct.ScmObj*, align 8
%argslist57031$anf_45bind472161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47076, %struct.ScmObj* %argslist57031$anf_45bind472160)
store volatile %struct.ScmObj* %argslist57031$anf_45bind472161, %struct.ScmObj** %stackaddr$prim59055, align 8
%stackaddr$prim59056 = alloca %struct.ScmObj*, align 8
%argslist57031$anf_45bind472162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47603, %struct.ScmObj* %argslist57031$anf_45bind472161)
store volatile %struct.ScmObj* %argslist57031$anf_45bind472162, %struct.ScmObj** %stackaddr$prim59056, align 8
%clofunc59057 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47216)
musttail call tailcc void %clofunc59057(%struct.ScmObj* %anf_45bind47216, %struct.ScmObj* %argslist57031$anf_45bind472162)
ret void
}

define tailcc void @proc_clo$ae47603(%struct.ScmObj* %env$ae47603,%struct.ScmObj* %current_45args57028) {
%stackaddr$env-ref59058 = alloca %struct.ScmObj*, align 8
%args47077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47603, i64 0)
store %struct.ScmObj* %args47077, %struct.ScmObj** %stackaddr$env-ref59058
%stackaddr$env-ref59059 = alloca %struct.ScmObj*, align 8
%k47568 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47603, i64 1)
store %struct.ScmObj* %k47568, %struct.ScmObj** %stackaddr$env-ref59059
%stackaddr$prim59060 = alloca %struct.ScmObj*, align 8
%_95k47570 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57028)
store volatile %struct.ScmObj* %_95k47570, %struct.ScmObj** %stackaddr$prim59060, align 8
%stackaddr$prim59061 = alloca %struct.ScmObj*, align 8
%current_45args57029 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57028)
store volatile %struct.ScmObj* %current_45args57029, %struct.ScmObj** %stackaddr$prim59061, align 8
%stackaddr$prim59062 = alloca %struct.ScmObj*, align 8
%anf_45bind47217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57029)
store volatile %struct.ScmObj* %anf_45bind47217, %struct.ScmObj** %stackaddr$prim59062, align 8
%stackaddr$prim59063 = alloca %struct.ScmObj*, align 8
%cpsargs47571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47568, %struct.ScmObj* %args47077)
store volatile %struct.ScmObj* %cpsargs47571, %struct.ScmObj** %stackaddr$prim59063, align 8
%clofunc59064 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47217)
musttail call tailcc void %clofunc59064(%struct.ScmObj* %anf_45bind47217, %struct.ScmObj* %cpsargs47571)
ret void
}

define tailcc void @proc_clo$ae47575(%struct.ScmObj* %env$ae47575,%struct.ScmObj* %current_45args57036) {
%stackaddr$prim59065 = alloca %struct.ScmObj*, align 8
%k47572 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57036)
store volatile %struct.ScmObj* %k47572, %struct.ScmObj** %stackaddr$prim59065, align 8
%stackaddr$prim59066 = alloca %struct.ScmObj*, align 8
%current_45args57037 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57036)
store volatile %struct.ScmObj* %current_45args57037, %struct.ScmObj** %stackaddr$prim59066, align 8
%stackaddr$prim59067 = alloca %struct.ScmObj*, align 8
%yu47074 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57037)
store volatile %struct.ScmObj* %yu47074, %struct.ScmObj** %stackaddr$prim59067, align 8
%argslist57039$yu470740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59068 = alloca %struct.ScmObj*, align 8
%argslist57039$yu470741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu47074, %struct.ScmObj* %argslist57039$yu470740)
store volatile %struct.ScmObj* %argslist57039$yu470741, %struct.ScmObj** %stackaddr$prim59068, align 8
%stackaddr$prim59069 = alloca %struct.ScmObj*, align 8
%argslist57039$yu470742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47572, %struct.ScmObj* %argslist57039$yu470741)
store volatile %struct.ScmObj* %argslist57039$yu470742, %struct.ScmObj** %stackaddr$prim59069, align 8
%clofunc59070 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu47074)
musttail call tailcc void %clofunc59070(%struct.ScmObj* %yu47074, %struct.ScmObj* %argslist57039$yu470742)
ret void
}