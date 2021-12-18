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

@global$sym$ae4400451132 = private unnamed_addr constant [8 x i8] c"promise\00", align 8

define ccc i32 @main() {
%mainenv50293 = call %struct.ScmObj* @const_init_null()
%mainargs50294 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv50293, %struct.ScmObj* %mainargs50294)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv50291,%struct.ScmObj* %mainargs50292) {
%stackaddr$makeclosure50295 = alloca %struct.ScmObj*, align 8
%fptrToInt50296 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40616 to i64
%ae40616 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50296)
store volatile %struct.ScmObj* %ae40616, %struct.ScmObj** %stackaddr$makeclosure50295, align 8
%ae40617 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50297 = alloca %struct.ScmObj*, align 8
%fptrToInt50298 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40618 to i64
%ae40618 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50298)
store volatile %struct.ScmObj* %ae40618, %struct.ScmObj** %stackaddr$makeclosure50297, align 8
%args50290$ae40616$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50299 = alloca %struct.ScmObj*, align 8
%args50290$ae40616$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40618, %struct.ScmObj* %args50290$ae40616$0)
store volatile %struct.ScmObj* %args50290$ae40616$1, %struct.ScmObj** %stackaddr$prim50299, align 8
%stackaddr$prim50300 = alloca %struct.ScmObj*, align 8
%args50290$ae40616$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40617, %struct.ScmObj* %args50290$ae40616$1)
store volatile %struct.ScmObj* %args50290$ae40616$2, %struct.ScmObj** %stackaddr$prim50300, align 8
%clofunc50301 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40616)
musttail call tailcc void %clofunc50301(%struct.ScmObj* %ae40616, %struct.ScmObj* %args50290$ae40616$2)
ret void
}

define tailcc void @proc_clo$ae40616(%struct.ScmObj* %env$ae40616,%struct.ScmObj* %current_45args49638) {
%stackaddr$prim50302 = alloca %struct.ScmObj*, align 8
%_95k40421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49638)
store volatile %struct.ScmObj* %_95k40421, %struct.ScmObj** %stackaddr$prim50302, align 8
%stackaddr$prim50303 = alloca %struct.ScmObj*, align 8
%current_45args49639 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49638)
store volatile %struct.ScmObj* %current_45args49639, %struct.ScmObj** %stackaddr$prim50303, align 8
%stackaddr$prim50304 = alloca %struct.ScmObj*, align 8
%anf_45bind40251 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49639)
store volatile %struct.ScmObj* %anf_45bind40251, %struct.ScmObj** %stackaddr$prim50304, align 8
%stackaddr$makeclosure50305 = alloca %struct.ScmObj*, align 8
%fptrToInt50306 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40631 to i64
%ae40631 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50306)
store volatile %struct.ScmObj* %ae40631, %struct.ScmObj** %stackaddr$makeclosure50305, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40631, %struct.ScmObj* %anf_45bind40251, i64 0)
%ae40632 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50307 = alloca %struct.ScmObj*, align 8
%fptrToInt50308 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40633 to i64
%ae40633 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50308)
store volatile %struct.ScmObj* %ae40633, %struct.ScmObj** %stackaddr$makeclosure50307, align 8
%args50285$ae40631$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50309 = alloca %struct.ScmObj*, align 8
%args50285$ae40631$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40633, %struct.ScmObj* %args50285$ae40631$0)
store volatile %struct.ScmObj* %args50285$ae40631$1, %struct.ScmObj** %stackaddr$prim50309, align 8
%stackaddr$prim50310 = alloca %struct.ScmObj*, align 8
%args50285$ae40631$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40632, %struct.ScmObj* %args50285$ae40631$1)
store volatile %struct.ScmObj* %args50285$ae40631$2, %struct.ScmObj** %stackaddr$prim50310, align 8
%clofunc50311 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40631)
musttail call tailcc void %clofunc50311(%struct.ScmObj* %ae40631, %struct.ScmObj* %args50285$ae40631$2)
ret void
}

define tailcc void @proc_clo$ae40631(%struct.ScmObj* %env$ae40631,%struct.ScmObj* %current_45args49641) {
%stackaddr$env-ref50312 = alloca %struct.ScmObj*, align 8
%anf_45bind40251 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40631, i64 0)
store %struct.ScmObj* %anf_45bind40251, %struct.ScmObj** %stackaddr$env-ref50312
%stackaddr$prim50313 = alloca %struct.ScmObj*, align 8
%_95k40422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49641)
store volatile %struct.ScmObj* %_95k40422, %struct.ScmObj** %stackaddr$prim50313, align 8
%stackaddr$prim50314 = alloca %struct.ScmObj*, align 8
%current_45args49642 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49641)
store volatile %struct.ScmObj* %current_45args49642, %struct.ScmObj** %stackaddr$prim50314, align 8
%stackaddr$prim50315 = alloca %struct.ScmObj*, align 8
%anf_45bind40255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49642)
store volatile %struct.ScmObj* %anf_45bind40255, %struct.ScmObj** %stackaddr$prim50315, align 8
%stackaddr$makeclosure50316 = alloca %struct.ScmObj*, align 8
%fptrToInt50317 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40746 to i64
%ae40746 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50317)
store volatile %struct.ScmObj* %ae40746, %struct.ScmObj** %stackaddr$makeclosure50316, align 8
%args50264$anf_45bind40251$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50318 = alloca %struct.ScmObj*, align 8
%args50264$anf_45bind40251$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40255, %struct.ScmObj* %args50264$anf_45bind40251$0)
store volatile %struct.ScmObj* %args50264$anf_45bind40251$1, %struct.ScmObj** %stackaddr$prim50318, align 8
%stackaddr$prim50319 = alloca %struct.ScmObj*, align 8
%args50264$anf_45bind40251$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40746, %struct.ScmObj* %args50264$anf_45bind40251$1)
store volatile %struct.ScmObj* %args50264$anf_45bind40251$2, %struct.ScmObj** %stackaddr$prim50319, align 8
%clofunc50320 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40251)
musttail call tailcc void %clofunc50320(%struct.ScmObj* %anf_45bind40251, %struct.ScmObj* %args50264$anf_45bind40251$2)
ret void
}

define tailcc void @proc_clo$ae40746(%struct.ScmObj* %env$ae40746,%struct.ScmObj* %current_45args49644) {
%stackaddr$prim50321 = alloca %struct.ScmObj*, align 8
%_95k40423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49644)
store volatile %struct.ScmObj* %_95k40423, %struct.ScmObj** %stackaddr$prim50321, align 8
%stackaddr$prim50322 = alloca %struct.ScmObj*, align 8
%current_45args49645 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49644)
store volatile %struct.ScmObj* %current_45args49645, %struct.ScmObj** %stackaddr$prim50322, align 8
%stackaddr$prim50323 = alloca %struct.ScmObj*, align 8
%Ycmb40107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49645)
store volatile %struct.ScmObj* %Ycmb40107, %struct.ScmObj** %stackaddr$prim50323, align 8
%stackaddr$makeclosure50324 = alloca %struct.ScmObj*, align 8
%fptrToInt50325 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40748 to i64
%ae40748 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50325)
store volatile %struct.ScmObj* %ae40748, %struct.ScmObj** %stackaddr$makeclosure50324, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40748, %struct.ScmObj* %Ycmb40107, i64 0)
%ae40749 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50326 = alloca %struct.ScmObj*, align 8
%fptrToInt50327 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40750 to i64
%ae40750 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50327)
store volatile %struct.ScmObj* %ae40750, %struct.ScmObj** %stackaddr$makeclosure50326, align 8
%args50263$ae40748$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50328 = alloca %struct.ScmObj*, align 8
%args50263$ae40748$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40750, %struct.ScmObj* %args50263$ae40748$0)
store volatile %struct.ScmObj* %args50263$ae40748$1, %struct.ScmObj** %stackaddr$prim50328, align 8
%stackaddr$prim50329 = alloca %struct.ScmObj*, align 8
%args50263$ae40748$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40749, %struct.ScmObj* %args50263$ae40748$1)
store volatile %struct.ScmObj* %args50263$ae40748$2, %struct.ScmObj** %stackaddr$prim50329, align 8
%clofunc50330 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40748)
musttail call tailcc void %clofunc50330(%struct.ScmObj* %ae40748, %struct.ScmObj* %args50263$ae40748$2)
ret void
}

define tailcc void @proc_clo$ae40748(%struct.ScmObj* %env$ae40748,%struct.ScmObj* %current_45args49647) {
%stackaddr$env-ref50331 = alloca %struct.ScmObj*, align 8
%Ycmb40107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40748, i64 0)
store %struct.ScmObj* %Ycmb40107, %struct.ScmObj** %stackaddr$env-ref50331
%stackaddr$prim50332 = alloca %struct.ScmObj*, align 8
%_95k40424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49647)
store volatile %struct.ScmObj* %_95k40424, %struct.ScmObj** %stackaddr$prim50332, align 8
%stackaddr$prim50333 = alloca %struct.ScmObj*, align 8
%current_45args49648 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49647)
store volatile %struct.ScmObj* %current_45args49648, %struct.ScmObj** %stackaddr$prim50333, align 8
%stackaddr$prim50334 = alloca %struct.ScmObj*, align 8
%anf_45bind40260 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49648)
store volatile %struct.ScmObj* %anf_45bind40260, %struct.ScmObj** %stackaddr$prim50334, align 8
%stackaddr$makeclosure50335 = alloca %struct.ScmObj*, align 8
%fptrToInt50336 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40826 to i64
%ae40826 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50336)
store volatile %struct.ScmObj* %ae40826, %struct.ScmObj** %stackaddr$makeclosure50335, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40826, %struct.ScmObj* %Ycmb40107, i64 0)
%args50247$Ycmb40107$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50337 = alloca %struct.ScmObj*, align 8
%args50247$Ycmb40107$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40260, %struct.ScmObj* %args50247$Ycmb40107$0)
store volatile %struct.ScmObj* %args50247$Ycmb40107$1, %struct.ScmObj** %stackaddr$prim50337, align 8
%stackaddr$prim50338 = alloca %struct.ScmObj*, align 8
%args50247$Ycmb40107$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40826, %struct.ScmObj* %args50247$Ycmb40107$1)
store volatile %struct.ScmObj* %args50247$Ycmb40107$2, %struct.ScmObj** %stackaddr$prim50338, align 8
%clofunc50339 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40107)
musttail call tailcc void %clofunc50339(%struct.ScmObj* %Ycmb40107, %struct.ScmObj* %args50247$Ycmb40107$2)
ret void
}

define tailcc void @proc_clo$ae40826(%struct.ScmObj* %env$ae40826,%struct.ScmObj* %current_45args49650) {
%stackaddr$env-ref50340 = alloca %struct.ScmObj*, align 8
%Ycmb40107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40826, i64 0)
store %struct.ScmObj* %Ycmb40107, %struct.ScmObj** %stackaddr$env-ref50340
%stackaddr$prim50341 = alloca %struct.ScmObj*, align 8
%_95k40425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49650)
store volatile %struct.ScmObj* %_95k40425, %struct.ScmObj** %stackaddr$prim50341, align 8
%stackaddr$prim50342 = alloca %struct.ScmObj*, align 8
%current_45args49651 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49650)
store volatile %struct.ScmObj* %current_45args49651, %struct.ScmObj** %stackaddr$prim50342, align 8
%stackaddr$prim50343 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49651)
store volatile %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$prim50343, align 8
%stackaddr$makeclosure50344 = alloca %struct.ScmObj*, align 8
%fptrToInt50345 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40828 to i64
%ae40828 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50345)
store volatile %struct.ScmObj* %ae40828, %struct.ScmObj** %stackaddr$makeclosure50344, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40828, %struct.ScmObj* %_37foldr140128, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40828, %struct.ScmObj* %Ycmb40107, i64 1)
%ae40829 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50346 = alloca %struct.ScmObj*, align 8
%fptrToInt50347 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40830 to i64
%ae40830 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50347)
store volatile %struct.ScmObj* %ae40830, %struct.ScmObj** %stackaddr$makeclosure50346, align 8
%args50246$ae40828$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50348 = alloca %struct.ScmObj*, align 8
%args50246$ae40828$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40830, %struct.ScmObj* %args50246$ae40828$0)
store volatile %struct.ScmObj* %args50246$ae40828$1, %struct.ScmObj** %stackaddr$prim50348, align 8
%stackaddr$prim50349 = alloca %struct.ScmObj*, align 8
%args50246$ae40828$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40829, %struct.ScmObj* %args50246$ae40828$1)
store volatile %struct.ScmObj* %args50246$ae40828$2, %struct.ScmObj** %stackaddr$prim50349, align 8
%clofunc50350 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40828)
musttail call tailcc void %clofunc50350(%struct.ScmObj* %ae40828, %struct.ScmObj* %args50246$ae40828$2)
ret void
}

define tailcc void @proc_clo$ae40828(%struct.ScmObj* %env$ae40828,%struct.ScmObj* %current_45args49653) {
%stackaddr$env-ref50351 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40828, i64 0)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref50351
%stackaddr$env-ref50352 = alloca %struct.ScmObj*, align 8
%Ycmb40107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40828, i64 1)
store %struct.ScmObj* %Ycmb40107, %struct.ScmObj** %stackaddr$env-ref50352
%stackaddr$prim50353 = alloca %struct.ScmObj*, align 8
%_95k40426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49653)
store volatile %struct.ScmObj* %_95k40426, %struct.ScmObj** %stackaddr$prim50353, align 8
%stackaddr$prim50354 = alloca %struct.ScmObj*, align 8
%current_45args49654 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49653)
store volatile %struct.ScmObj* %current_45args49654, %struct.ScmObj** %stackaddr$prim50354, align 8
%stackaddr$prim50355 = alloca %struct.ScmObj*, align 8
%anf_45bind40266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49654)
store volatile %struct.ScmObj* %anf_45bind40266, %struct.ScmObj** %stackaddr$prim50355, align 8
%stackaddr$makeclosure50356 = alloca %struct.ScmObj*, align 8
%fptrToInt50357 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40923 to i64
%ae40923 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50357)
store volatile %struct.ScmObj* %ae40923, %struct.ScmObj** %stackaddr$makeclosure50356, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40923, %struct.ScmObj* %_37foldr140128, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40923, %struct.ScmObj* %Ycmb40107, i64 1)
%args50227$Ycmb40107$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50358 = alloca %struct.ScmObj*, align 8
%args50227$Ycmb40107$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40266, %struct.ScmObj* %args50227$Ycmb40107$0)
store volatile %struct.ScmObj* %args50227$Ycmb40107$1, %struct.ScmObj** %stackaddr$prim50358, align 8
%stackaddr$prim50359 = alloca %struct.ScmObj*, align 8
%args50227$Ycmb40107$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40923, %struct.ScmObj* %args50227$Ycmb40107$1)
store volatile %struct.ScmObj* %args50227$Ycmb40107$2, %struct.ScmObj** %stackaddr$prim50359, align 8
%clofunc50360 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40107)
musttail call tailcc void %clofunc50360(%struct.ScmObj* %Ycmb40107, %struct.ScmObj* %args50227$Ycmb40107$2)
ret void
}

define tailcc void @proc_clo$ae40923(%struct.ScmObj* %env$ae40923,%struct.ScmObj* %current_45args49656) {
%stackaddr$env-ref50361 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40923, i64 0)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref50361
%stackaddr$env-ref50362 = alloca %struct.ScmObj*, align 8
%Ycmb40107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40923, i64 1)
store %struct.ScmObj* %Ycmb40107, %struct.ScmObj** %stackaddr$env-ref50362
%stackaddr$prim50363 = alloca %struct.ScmObj*, align 8
%_95k40427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49656)
store volatile %struct.ScmObj* %_95k40427, %struct.ScmObj** %stackaddr$prim50363, align 8
%stackaddr$prim50364 = alloca %struct.ScmObj*, align 8
%current_45args49657 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49656)
store volatile %struct.ScmObj* %current_45args49657, %struct.ScmObj** %stackaddr$prim50364, align 8
%stackaddr$prim50365 = alloca %struct.ScmObj*, align 8
%_37map140124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49657)
store volatile %struct.ScmObj* %_37map140124, %struct.ScmObj** %stackaddr$prim50365, align 8
%stackaddr$makeclosure50366 = alloca %struct.ScmObj*, align 8
%fptrToInt50367 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40925 to i64
%ae40925 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50367)
store volatile %struct.ScmObj* %ae40925, %struct.ScmObj** %stackaddr$makeclosure50366, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40925, %struct.ScmObj* %_37foldr140128, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40925, %struct.ScmObj* %_37map140124, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40925, %struct.ScmObj* %Ycmb40107, i64 2)
%ae40926 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50368 = alloca %struct.ScmObj*, align 8
%fptrToInt50369 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40927 to i64
%ae40927 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50369)
store volatile %struct.ScmObj* %ae40927, %struct.ScmObj** %stackaddr$makeclosure50368, align 8
%args50226$ae40925$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50370 = alloca %struct.ScmObj*, align 8
%args50226$ae40925$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40927, %struct.ScmObj* %args50226$ae40925$0)
store volatile %struct.ScmObj* %args50226$ae40925$1, %struct.ScmObj** %stackaddr$prim50370, align 8
%stackaddr$prim50371 = alloca %struct.ScmObj*, align 8
%args50226$ae40925$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40926, %struct.ScmObj* %args50226$ae40925$1)
store volatile %struct.ScmObj* %args50226$ae40925$2, %struct.ScmObj** %stackaddr$prim50371, align 8
%clofunc50372 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40925)
musttail call tailcc void %clofunc50372(%struct.ScmObj* %ae40925, %struct.ScmObj* %args50226$ae40925$2)
ret void
}

define tailcc void @proc_clo$ae40925(%struct.ScmObj* %env$ae40925,%struct.ScmObj* %current_45args49659) {
%stackaddr$env-ref50373 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40925, i64 0)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref50373
%stackaddr$env-ref50374 = alloca %struct.ScmObj*, align 8
%_37map140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40925, i64 1)
store %struct.ScmObj* %_37map140124, %struct.ScmObj** %stackaddr$env-ref50374
%stackaddr$env-ref50375 = alloca %struct.ScmObj*, align 8
%Ycmb40107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40925, i64 2)
store %struct.ScmObj* %Ycmb40107, %struct.ScmObj** %stackaddr$env-ref50375
%stackaddr$prim50376 = alloca %struct.ScmObj*, align 8
%_95k40428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49659)
store volatile %struct.ScmObj* %_95k40428, %struct.ScmObj** %stackaddr$prim50376, align 8
%stackaddr$prim50377 = alloca %struct.ScmObj*, align 8
%current_45args49660 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49659)
store volatile %struct.ScmObj* %current_45args49660, %struct.ScmObj** %stackaddr$prim50377, align 8
%stackaddr$prim50378 = alloca %struct.ScmObj*, align 8
%anf_45bind40273 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49660)
store volatile %struct.ScmObj* %anf_45bind40273, %struct.ScmObj** %stackaddr$prim50378, align 8
%stackaddr$makeclosure50379 = alloca %struct.ScmObj*, align 8
%fptrToInt50380 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41073 to i64
%ae41073 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50380)
store volatile %struct.ScmObj* %ae41073, %struct.ScmObj** %stackaddr$makeclosure50379, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41073, %struct.ScmObj* %_37foldr140128, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41073, %struct.ScmObj* %_37map140124, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41073, %struct.ScmObj* %Ycmb40107, i64 2)
%args50210$Ycmb40107$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50381 = alloca %struct.ScmObj*, align 8
%args50210$Ycmb40107$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40273, %struct.ScmObj* %args50210$Ycmb40107$0)
store volatile %struct.ScmObj* %args50210$Ycmb40107$1, %struct.ScmObj** %stackaddr$prim50381, align 8
%stackaddr$prim50382 = alloca %struct.ScmObj*, align 8
%args50210$Ycmb40107$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41073, %struct.ScmObj* %args50210$Ycmb40107$1)
store volatile %struct.ScmObj* %args50210$Ycmb40107$2, %struct.ScmObj** %stackaddr$prim50382, align 8
%clofunc50383 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40107)
musttail call tailcc void %clofunc50383(%struct.ScmObj* %Ycmb40107, %struct.ScmObj* %args50210$Ycmb40107$2)
ret void
}

define tailcc void @proc_clo$ae41073(%struct.ScmObj* %env$ae41073,%struct.ScmObj* %current_45args49662) {
%stackaddr$env-ref50384 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41073, i64 0)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref50384
%stackaddr$env-ref50385 = alloca %struct.ScmObj*, align 8
%_37map140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41073, i64 1)
store %struct.ScmObj* %_37map140124, %struct.ScmObj** %stackaddr$env-ref50385
%stackaddr$env-ref50386 = alloca %struct.ScmObj*, align 8
%Ycmb40107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41073, i64 2)
store %struct.ScmObj* %Ycmb40107, %struct.ScmObj** %stackaddr$env-ref50386
%stackaddr$prim50387 = alloca %struct.ScmObj*, align 8
%_95k40429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49662)
store volatile %struct.ScmObj* %_95k40429, %struct.ScmObj** %stackaddr$prim50387, align 8
%stackaddr$prim50388 = alloca %struct.ScmObj*, align 8
%current_45args49663 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49662)
store volatile %struct.ScmObj* %current_45args49663, %struct.ScmObj** %stackaddr$prim50388, align 8
%stackaddr$prim50389 = alloca %struct.ScmObj*, align 8
%_37take40120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49663)
store volatile %struct.ScmObj* %_37take40120, %struct.ScmObj** %stackaddr$prim50389, align 8
%stackaddr$makeclosure50390 = alloca %struct.ScmObj*, align 8
%fptrToInt50391 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41075 to i64
%ae41075 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50391)
store volatile %struct.ScmObj* %ae41075, %struct.ScmObj** %stackaddr$makeclosure50390, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41075, %struct.ScmObj* %_37take40120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41075, %struct.ScmObj* %_37foldr140128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41075, %struct.ScmObj* %_37map140124, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41075, %struct.ScmObj* %Ycmb40107, i64 3)
%ae41076 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50392 = alloca %struct.ScmObj*, align 8
%fptrToInt50393 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41077 to i64
%ae41077 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50393)
store volatile %struct.ScmObj* %ae41077, %struct.ScmObj** %stackaddr$makeclosure50392, align 8
%args50209$ae41075$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50394 = alloca %struct.ScmObj*, align 8
%args50209$ae41075$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41077, %struct.ScmObj* %args50209$ae41075$0)
store volatile %struct.ScmObj* %args50209$ae41075$1, %struct.ScmObj** %stackaddr$prim50394, align 8
%stackaddr$prim50395 = alloca %struct.ScmObj*, align 8
%args50209$ae41075$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41076, %struct.ScmObj* %args50209$ae41075$1)
store volatile %struct.ScmObj* %args50209$ae41075$2, %struct.ScmObj** %stackaddr$prim50395, align 8
%clofunc50396 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41075)
musttail call tailcc void %clofunc50396(%struct.ScmObj* %ae41075, %struct.ScmObj* %args50209$ae41075$2)
ret void
}

define tailcc void @proc_clo$ae41075(%struct.ScmObj* %env$ae41075,%struct.ScmObj* %current_45args49665) {
%stackaddr$env-ref50397 = alloca %struct.ScmObj*, align 8
%_37take40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41075, i64 0)
store %struct.ScmObj* %_37take40120, %struct.ScmObj** %stackaddr$env-ref50397
%stackaddr$env-ref50398 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41075, i64 1)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref50398
%stackaddr$env-ref50399 = alloca %struct.ScmObj*, align 8
%_37map140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41075, i64 2)
store %struct.ScmObj* %_37map140124, %struct.ScmObj** %stackaddr$env-ref50399
%stackaddr$env-ref50400 = alloca %struct.ScmObj*, align 8
%Ycmb40107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41075, i64 3)
store %struct.ScmObj* %Ycmb40107, %struct.ScmObj** %stackaddr$env-ref50400
%stackaddr$prim50401 = alloca %struct.ScmObj*, align 8
%_95k40430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49665)
store volatile %struct.ScmObj* %_95k40430, %struct.ScmObj** %stackaddr$prim50401, align 8
%stackaddr$prim50402 = alloca %struct.ScmObj*, align 8
%current_45args49666 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49665)
store volatile %struct.ScmObj* %current_45args49666, %struct.ScmObj** %stackaddr$prim50402, align 8
%stackaddr$prim50403 = alloca %struct.ScmObj*, align 8
%anf_45bind40277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49666)
store volatile %struct.ScmObj* %anf_45bind40277, %struct.ScmObj** %stackaddr$prim50403, align 8
%stackaddr$makeclosure50404 = alloca %struct.ScmObj*, align 8
%fptrToInt50405 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41156 to i64
%ae41156 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50405)
store volatile %struct.ScmObj* %ae41156, %struct.ScmObj** %stackaddr$makeclosure50404, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41156, %struct.ScmObj* %_37take40120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41156, %struct.ScmObj* %_37foldr140128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41156, %struct.ScmObj* %_37map140124, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41156, %struct.ScmObj* %Ycmb40107, i64 3)
%args50195$Ycmb40107$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50406 = alloca %struct.ScmObj*, align 8
%args50195$Ycmb40107$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40277, %struct.ScmObj* %args50195$Ycmb40107$0)
store volatile %struct.ScmObj* %args50195$Ycmb40107$1, %struct.ScmObj** %stackaddr$prim50406, align 8
%stackaddr$prim50407 = alloca %struct.ScmObj*, align 8
%args50195$Ycmb40107$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41156, %struct.ScmObj* %args50195$Ycmb40107$1)
store volatile %struct.ScmObj* %args50195$Ycmb40107$2, %struct.ScmObj** %stackaddr$prim50407, align 8
%clofunc50408 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40107)
musttail call tailcc void %clofunc50408(%struct.ScmObj* %Ycmb40107, %struct.ScmObj* %args50195$Ycmb40107$2)
ret void
}

define tailcc void @proc_clo$ae41156(%struct.ScmObj* %env$ae41156,%struct.ScmObj* %current_45args49668) {
%stackaddr$env-ref50409 = alloca %struct.ScmObj*, align 8
%_37take40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41156, i64 0)
store %struct.ScmObj* %_37take40120, %struct.ScmObj** %stackaddr$env-ref50409
%stackaddr$env-ref50410 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41156, i64 1)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref50410
%stackaddr$env-ref50411 = alloca %struct.ScmObj*, align 8
%_37map140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41156, i64 2)
store %struct.ScmObj* %_37map140124, %struct.ScmObj** %stackaddr$env-ref50411
%stackaddr$env-ref50412 = alloca %struct.ScmObj*, align 8
%Ycmb40107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41156, i64 3)
store %struct.ScmObj* %Ycmb40107, %struct.ScmObj** %stackaddr$env-ref50412
%stackaddr$prim50413 = alloca %struct.ScmObj*, align 8
%_95k40431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49668)
store volatile %struct.ScmObj* %_95k40431, %struct.ScmObj** %stackaddr$prim50413, align 8
%stackaddr$prim50414 = alloca %struct.ScmObj*, align 8
%current_45args49669 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49668)
store volatile %struct.ScmObj* %current_45args49669, %struct.ScmObj** %stackaddr$prim50414, align 8
%stackaddr$prim50415 = alloca %struct.ScmObj*, align 8
%_37length40117 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49669)
store volatile %struct.ScmObj* %_37length40117, %struct.ScmObj** %stackaddr$prim50415, align 8
%stackaddr$makeclosure50416 = alloca %struct.ScmObj*, align 8
%fptrToInt50417 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41158 to i64
%ae41158 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt50417)
store volatile %struct.ScmObj* %ae41158, %struct.ScmObj** %stackaddr$makeclosure50416, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41158, %struct.ScmObj* %_37take40120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41158, %struct.ScmObj* %_37length40117, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41158, %struct.ScmObj* %_37foldr140128, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41158, %struct.ScmObj* %_37map140124, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41158, %struct.ScmObj* %Ycmb40107, i64 4)
%ae41159 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50418 = alloca %struct.ScmObj*, align 8
%fptrToInt50419 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41160 to i64
%ae41160 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50419)
store volatile %struct.ScmObj* %ae41160, %struct.ScmObj** %stackaddr$makeclosure50418, align 8
%args50194$ae41158$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50420 = alloca %struct.ScmObj*, align 8
%args50194$ae41158$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41160, %struct.ScmObj* %args50194$ae41158$0)
store volatile %struct.ScmObj* %args50194$ae41158$1, %struct.ScmObj** %stackaddr$prim50420, align 8
%stackaddr$prim50421 = alloca %struct.ScmObj*, align 8
%args50194$ae41158$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41159, %struct.ScmObj* %args50194$ae41158$1)
store volatile %struct.ScmObj* %args50194$ae41158$2, %struct.ScmObj** %stackaddr$prim50421, align 8
%clofunc50422 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41158)
musttail call tailcc void %clofunc50422(%struct.ScmObj* %ae41158, %struct.ScmObj* %args50194$ae41158$2)
ret void
}

define tailcc void @proc_clo$ae41158(%struct.ScmObj* %env$ae41158,%struct.ScmObj* %current_45args49671) {
%stackaddr$env-ref50423 = alloca %struct.ScmObj*, align 8
%_37take40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41158, i64 0)
store %struct.ScmObj* %_37take40120, %struct.ScmObj** %stackaddr$env-ref50423
%stackaddr$env-ref50424 = alloca %struct.ScmObj*, align 8
%_37length40117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41158, i64 1)
store %struct.ScmObj* %_37length40117, %struct.ScmObj** %stackaddr$env-ref50424
%stackaddr$env-ref50425 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41158, i64 2)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref50425
%stackaddr$env-ref50426 = alloca %struct.ScmObj*, align 8
%_37map140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41158, i64 3)
store %struct.ScmObj* %_37map140124, %struct.ScmObj** %stackaddr$env-ref50426
%stackaddr$env-ref50427 = alloca %struct.ScmObj*, align 8
%Ycmb40107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41158, i64 4)
store %struct.ScmObj* %Ycmb40107, %struct.ScmObj** %stackaddr$env-ref50427
%stackaddr$prim50428 = alloca %struct.ScmObj*, align 8
%_95k40432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49671)
store volatile %struct.ScmObj* %_95k40432, %struct.ScmObj** %stackaddr$prim50428, align 8
%stackaddr$prim50429 = alloca %struct.ScmObj*, align 8
%current_45args49672 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49671)
store volatile %struct.ScmObj* %current_45args49672, %struct.ScmObj** %stackaddr$prim50429, align 8
%stackaddr$prim50430 = alloca %struct.ScmObj*, align 8
%anf_45bind40282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49672)
store volatile %struct.ScmObj* %anf_45bind40282, %struct.ScmObj** %stackaddr$prim50430, align 8
%stackaddr$makeclosure50431 = alloca %struct.ScmObj*, align 8
%fptrToInt50432 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41235 to i64
%ae41235 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt50432)
store volatile %struct.ScmObj* %ae41235, %struct.ScmObj** %stackaddr$makeclosure50431, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41235, %struct.ScmObj* %_37take40120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41235, %struct.ScmObj* %_37length40117, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41235, %struct.ScmObj* %_37foldr140128, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41235, %struct.ScmObj* %_37map140124, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41235, %struct.ScmObj* %Ycmb40107, i64 4)
%args50178$Ycmb40107$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50433 = alloca %struct.ScmObj*, align 8
%args50178$Ycmb40107$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40282, %struct.ScmObj* %args50178$Ycmb40107$0)
store volatile %struct.ScmObj* %args50178$Ycmb40107$1, %struct.ScmObj** %stackaddr$prim50433, align 8
%stackaddr$prim50434 = alloca %struct.ScmObj*, align 8
%args50178$Ycmb40107$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41235, %struct.ScmObj* %args50178$Ycmb40107$1)
store volatile %struct.ScmObj* %args50178$Ycmb40107$2, %struct.ScmObj** %stackaddr$prim50434, align 8
%clofunc50435 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40107)
musttail call tailcc void %clofunc50435(%struct.ScmObj* %Ycmb40107, %struct.ScmObj* %args50178$Ycmb40107$2)
ret void
}

define tailcc void @proc_clo$ae41235(%struct.ScmObj* %env$ae41235,%struct.ScmObj* %current_45args49674) {
%stackaddr$env-ref50436 = alloca %struct.ScmObj*, align 8
%_37take40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41235, i64 0)
store %struct.ScmObj* %_37take40120, %struct.ScmObj** %stackaddr$env-ref50436
%stackaddr$env-ref50437 = alloca %struct.ScmObj*, align 8
%_37length40117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41235, i64 1)
store %struct.ScmObj* %_37length40117, %struct.ScmObj** %stackaddr$env-ref50437
%stackaddr$env-ref50438 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41235, i64 2)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref50438
%stackaddr$env-ref50439 = alloca %struct.ScmObj*, align 8
%_37map140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41235, i64 3)
store %struct.ScmObj* %_37map140124, %struct.ScmObj** %stackaddr$env-ref50439
%stackaddr$env-ref50440 = alloca %struct.ScmObj*, align 8
%Ycmb40107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41235, i64 4)
store %struct.ScmObj* %Ycmb40107, %struct.ScmObj** %stackaddr$env-ref50440
%stackaddr$prim50441 = alloca %struct.ScmObj*, align 8
%_95k40433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49674)
store volatile %struct.ScmObj* %_95k40433, %struct.ScmObj** %stackaddr$prim50441, align 8
%stackaddr$prim50442 = alloca %struct.ScmObj*, align 8
%current_45args49675 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49674)
store volatile %struct.ScmObj* %current_45args49675, %struct.ScmObj** %stackaddr$prim50442, align 8
%stackaddr$prim50443 = alloca %struct.ScmObj*, align 8
%_37foldl140112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49675)
store volatile %struct.ScmObj* %_37foldl140112, %struct.ScmObj** %stackaddr$prim50443, align 8
%stackaddr$makeclosure50444 = alloca %struct.ScmObj*, align 8
%fptrToInt50445 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41237 to i64
%ae41237 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt50445)
store volatile %struct.ScmObj* %ae41237, %struct.ScmObj** %stackaddr$makeclosure50444, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41237, %struct.ScmObj* %_37foldr140128, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41237, %struct.ScmObj* %_37foldl140112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41237, %struct.ScmObj* %_37take40120, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41237, %struct.ScmObj* %_37length40117, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41237, %struct.ScmObj* %_37map140124, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41237, %struct.ScmObj* %Ycmb40107, i64 5)
%ae41238 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50446 = alloca %struct.ScmObj*, align 8
%fptrToInt50447 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41239 to i64
%ae41239 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50447)
store volatile %struct.ScmObj* %ae41239, %struct.ScmObj** %stackaddr$makeclosure50446, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41239, %struct.ScmObj* %_37foldl140112, i64 0)
%args50177$ae41237$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50448 = alloca %struct.ScmObj*, align 8
%args50177$ae41237$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41239, %struct.ScmObj* %args50177$ae41237$0)
store volatile %struct.ScmObj* %args50177$ae41237$1, %struct.ScmObj** %stackaddr$prim50448, align 8
%stackaddr$prim50449 = alloca %struct.ScmObj*, align 8
%args50177$ae41237$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41238, %struct.ScmObj* %args50177$ae41237$1)
store volatile %struct.ScmObj* %args50177$ae41237$2, %struct.ScmObj** %stackaddr$prim50449, align 8
%clofunc50450 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41237)
musttail call tailcc void %clofunc50450(%struct.ScmObj* %ae41237, %struct.ScmObj* %args50177$ae41237$2)
ret void
}

define tailcc void @proc_clo$ae41237(%struct.ScmObj* %env$ae41237,%struct.ScmObj* %current_45args49677) {
%stackaddr$env-ref50451 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41237, i64 0)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref50451
%stackaddr$env-ref50452 = alloca %struct.ScmObj*, align 8
%_37foldl140112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41237, i64 1)
store %struct.ScmObj* %_37foldl140112, %struct.ScmObj** %stackaddr$env-ref50452
%stackaddr$env-ref50453 = alloca %struct.ScmObj*, align 8
%_37take40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41237, i64 2)
store %struct.ScmObj* %_37take40120, %struct.ScmObj** %stackaddr$env-ref50453
%stackaddr$env-ref50454 = alloca %struct.ScmObj*, align 8
%_37length40117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41237, i64 3)
store %struct.ScmObj* %_37length40117, %struct.ScmObj** %stackaddr$env-ref50454
%stackaddr$env-ref50455 = alloca %struct.ScmObj*, align 8
%_37map140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41237, i64 4)
store %struct.ScmObj* %_37map140124, %struct.ScmObj** %stackaddr$env-ref50455
%stackaddr$env-ref50456 = alloca %struct.ScmObj*, align 8
%Ycmb40107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41237, i64 5)
store %struct.ScmObj* %Ycmb40107, %struct.ScmObj** %stackaddr$env-ref50456
%stackaddr$prim50457 = alloca %struct.ScmObj*, align 8
%_95k40434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49677)
store volatile %struct.ScmObj* %_95k40434, %struct.ScmObj** %stackaddr$prim50457, align 8
%stackaddr$prim50458 = alloca %struct.ScmObj*, align 8
%current_45args49678 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49677)
store volatile %struct.ScmObj* %current_45args49678, %struct.ScmObj** %stackaddr$prim50458, align 8
%stackaddr$prim50459 = alloca %struct.ScmObj*, align 8
%_37last40150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49678)
store volatile %struct.ScmObj* %_37last40150, %struct.ScmObj** %stackaddr$prim50459, align 8
%stackaddr$makeclosure50460 = alloca %struct.ScmObj*, align 8
%fptrToInt50461 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41291 to i64
%ae41291 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt50461)
store volatile %struct.ScmObj* %ae41291, %struct.ScmObj** %stackaddr$makeclosure50460, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41291, %struct.ScmObj* %_37foldr140128, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41291, %struct.ScmObj* %_37foldl140112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41291, %struct.ScmObj* %_37last40150, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41291, %struct.ScmObj* %_37map140124, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41291, %struct.ScmObj* %Ycmb40107, i64 4)
%ae41292 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50462 = alloca %struct.ScmObj*, align 8
%fptrToInt50463 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41293 to i64
%ae41293 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50463)
store volatile %struct.ScmObj* %ae41293, %struct.ScmObj** %stackaddr$makeclosure50462, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41293, %struct.ScmObj* %_37take40120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41293, %struct.ScmObj* %_37length40117, i64 1)
%args50163$ae41291$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50464 = alloca %struct.ScmObj*, align 8
%args50163$ae41291$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41293, %struct.ScmObj* %args50163$ae41291$0)
store volatile %struct.ScmObj* %args50163$ae41291$1, %struct.ScmObj** %stackaddr$prim50464, align 8
%stackaddr$prim50465 = alloca %struct.ScmObj*, align 8
%args50163$ae41291$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41292, %struct.ScmObj* %args50163$ae41291$1)
store volatile %struct.ScmObj* %args50163$ae41291$2, %struct.ScmObj** %stackaddr$prim50465, align 8
%clofunc50466 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41291)
musttail call tailcc void %clofunc50466(%struct.ScmObj* %ae41291, %struct.ScmObj* %args50163$ae41291$2)
ret void
}

define tailcc void @proc_clo$ae41291(%struct.ScmObj* %env$ae41291,%struct.ScmObj* %current_45args49680) {
%stackaddr$env-ref50467 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41291, i64 0)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref50467
%stackaddr$env-ref50468 = alloca %struct.ScmObj*, align 8
%_37foldl140112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41291, i64 1)
store %struct.ScmObj* %_37foldl140112, %struct.ScmObj** %stackaddr$env-ref50468
%stackaddr$env-ref50469 = alloca %struct.ScmObj*, align 8
%_37last40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41291, i64 2)
store %struct.ScmObj* %_37last40150, %struct.ScmObj** %stackaddr$env-ref50469
%stackaddr$env-ref50470 = alloca %struct.ScmObj*, align 8
%_37map140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41291, i64 3)
store %struct.ScmObj* %_37map140124, %struct.ScmObj** %stackaddr$env-ref50470
%stackaddr$env-ref50471 = alloca %struct.ScmObj*, align 8
%Ycmb40107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41291, i64 4)
store %struct.ScmObj* %Ycmb40107, %struct.ScmObj** %stackaddr$env-ref50471
%stackaddr$prim50472 = alloca %struct.ScmObj*, align 8
%_95k40435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49680)
store volatile %struct.ScmObj* %_95k40435, %struct.ScmObj** %stackaddr$prim50472, align 8
%stackaddr$prim50473 = alloca %struct.ScmObj*, align 8
%current_45args49681 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49680)
store volatile %struct.ScmObj* %current_45args49681, %struct.ScmObj** %stackaddr$prim50473, align 8
%stackaddr$prim50474 = alloca %struct.ScmObj*, align 8
%_37drop_45right40147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49681)
store volatile %struct.ScmObj* %_37drop_45right40147, %struct.ScmObj** %stackaddr$prim50474, align 8
%stackaddr$makeclosure50475 = alloca %struct.ScmObj*, align 8
%fptrToInt50476 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41321 to i64
%ae41321 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt50476)
store volatile %struct.ScmObj* %ae41321, %struct.ScmObj** %stackaddr$makeclosure50475, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41321, %struct.ScmObj* %_37foldr140128, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41321, %struct.ScmObj* %_37foldl140112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41321, %struct.ScmObj* %_37last40150, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41321, %struct.ScmObj* %_37drop_45right40147, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41321, %struct.ScmObj* %Ycmb40107, i64 4)
%ae41322 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50477 = alloca %struct.ScmObj*, align 8
%fptrToInt50478 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41323 to i64
%ae41323 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50478)
store volatile %struct.ScmObj* %ae41323, %struct.ScmObj** %stackaddr$makeclosure50477, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41323, %struct.ScmObj* %_37foldr140128, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41323, %struct.ScmObj* %_37map140124, i64 1)
%args50153$ae41321$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50479 = alloca %struct.ScmObj*, align 8
%args50153$ae41321$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41323, %struct.ScmObj* %args50153$ae41321$0)
store volatile %struct.ScmObj* %args50153$ae41321$1, %struct.ScmObj** %stackaddr$prim50479, align 8
%stackaddr$prim50480 = alloca %struct.ScmObj*, align 8
%args50153$ae41321$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41322, %struct.ScmObj* %args50153$ae41321$1)
store volatile %struct.ScmObj* %args50153$ae41321$2, %struct.ScmObj** %stackaddr$prim50480, align 8
%clofunc50481 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41321)
musttail call tailcc void %clofunc50481(%struct.ScmObj* %ae41321, %struct.ScmObj* %args50153$ae41321$2)
ret void
}

define tailcc void @proc_clo$ae41321(%struct.ScmObj* %env$ae41321,%struct.ScmObj* %current_45args49683) {
%stackaddr$env-ref50482 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41321, i64 0)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref50482
%stackaddr$env-ref50483 = alloca %struct.ScmObj*, align 8
%_37foldl140112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41321, i64 1)
store %struct.ScmObj* %_37foldl140112, %struct.ScmObj** %stackaddr$env-ref50483
%stackaddr$env-ref50484 = alloca %struct.ScmObj*, align 8
%_37last40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41321, i64 2)
store %struct.ScmObj* %_37last40150, %struct.ScmObj** %stackaddr$env-ref50484
%stackaddr$env-ref50485 = alloca %struct.ScmObj*, align 8
%_37drop_45right40147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41321, i64 3)
store %struct.ScmObj* %_37drop_45right40147, %struct.ScmObj** %stackaddr$env-ref50485
%stackaddr$env-ref50486 = alloca %struct.ScmObj*, align 8
%Ycmb40107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41321, i64 4)
store %struct.ScmObj* %Ycmb40107, %struct.ScmObj** %stackaddr$env-ref50486
%stackaddr$prim50487 = alloca %struct.ScmObj*, align 8
%_95k40436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49683)
store volatile %struct.ScmObj* %_95k40436, %struct.ScmObj** %stackaddr$prim50487, align 8
%stackaddr$prim50488 = alloca %struct.ScmObj*, align 8
%current_45args49684 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49683)
store volatile %struct.ScmObj* %current_45args49684, %struct.ScmObj** %stackaddr$prim50488, align 8
%stackaddr$prim50489 = alloca %struct.ScmObj*, align 8
%anf_45bind40298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49684)
store volatile %struct.ScmObj* %anf_45bind40298, %struct.ScmObj** %stackaddr$prim50489, align 8
%stackaddr$makeclosure50490 = alloca %struct.ScmObj*, align 8
%fptrToInt50491 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41705 to i64
%ae41705 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt50491)
store volatile %struct.ScmObj* %ae41705, %struct.ScmObj** %stackaddr$makeclosure50490, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41705, %struct.ScmObj* %_37foldr140128, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41705, %struct.ScmObj* %_37foldl140112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41705, %struct.ScmObj* %_37last40150, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41705, %struct.ScmObj* %_37drop_45right40147, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41705, %struct.ScmObj* %Ycmb40107, i64 4)
%args50093$Ycmb40107$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50492 = alloca %struct.ScmObj*, align 8
%args50093$Ycmb40107$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40298, %struct.ScmObj* %args50093$Ycmb40107$0)
store volatile %struct.ScmObj* %args50093$Ycmb40107$1, %struct.ScmObj** %stackaddr$prim50492, align 8
%stackaddr$prim50493 = alloca %struct.ScmObj*, align 8
%args50093$Ycmb40107$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41705, %struct.ScmObj* %args50093$Ycmb40107$1)
store volatile %struct.ScmObj* %args50093$Ycmb40107$2, %struct.ScmObj** %stackaddr$prim50493, align 8
%clofunc50494 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40107)
musttail call tailcc void %clofunc50494(%struct.ScmObj* %Ycmb40107, %struct.ScmObj* %args50093$Ycmb40107$2)
ret void
}

define tailcc void @proc_clo$ae41705(%struct.ScmObj* %env$ae41705,%struct.ScmObj* %current_45args49686) {
%stackaddr$env-ref50495 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41705, i64 0)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref50495
%stackaddr$env-ref50496 = alloca %struct.ScmObj*, align 8
%_37foldl140112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41705, i64 1)
store %struct.ScmObj* %_37foldl140112, %struct.ScmObj** %stackaddr$env-ref50496
%stackaddr$env-ref50497 = alloca %struct.ScmObj*, align 8
%_37last40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41705, i64 2)
store %struct.ScmObj* %_37last40150, %struct.ScmObj** %stackaddr$env-ref50497
%stackaddr$env-ref50498 = alloca %struct.ScmObj*, align 8
%_37drop_45right40147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41705, i64 3)
store %struct.ScmObj* %_37drop_45right40147, %struct.ScmObj** %stackaddr$env-ref50498
%stackaddr$env-ref50499 = alloca %struct.ScmObj*, align 8
%Ycmb40107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41705, i64 4)
store %struct.ScmObj* %Ycmb40107, %struct.ScmObj** %stackaddr$env-ref50499
%stackaddr$prim50500 = alloca %struct.ScmObj*, align 8
%_95k40437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49686)
store volatile %struct.ScmObj* %_95k40437, %struct.ScmObj** %stackaddr$prim50500, align 8
%stackaddr$prim50501 = alloca %struct.ScmObj*, align 8
%current_45args49687 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49686)
store volatile %struct.ScmObj* %current_45args49687, %struct.ScmObj** %stackaddr$prim50501, align 8
%stackaddr$prim50502 = alloca %struct.ScmObj*, align 8
%_37foldr40133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49687)
store volatile %struct.ScmObj* %_37foldr40133, %struct.ScmObj** %stackaddr$prim50502, align 8
%stackaddr$makeclosure50503 = alloca %struct.ScmObj*, align 8
%fptrToInt50504 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41707 to i64
%ae41707 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt50504)
store volatile %struct.ScmObj* %ae41707, %struct.ScmObj** %stackaddr$makeclosure50503, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41707, %struct.ScmObj* %_37foldr140128, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41707, %struct.ScmObj* %_37foldl140112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41707, %struct.ScmObj* %_37last40150, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41707, %struct.ScmObj* %_37foldr40133, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41707, %struct.ScmObj* %_37drop_45right40147, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41707, %struct.ScmObj* %Ycmb40107, i64 5)
%ae41708 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50505 = alloca %struct.ScmObj*, align 8
%fptrToInt50506 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41709 to i64
%ae41709 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50506)
store volatile %struct.ScmObj* %ae41709, %struct.ScmObj** %stackaddr$makeclosure50505, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41709, %struct.ScmObj* %_37foldr140128, i64 0)
%args50092$ae41707$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50507 = alloca %struct.ScmObj*, align 8
%args50092$ae41707$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41709, %struct.ScmObj* %args50092$ae41707$0)
store volatile %struct.ScmObj* %args50092$ae41707$1, %struct.ScmObj** %stackaddr$prim50507, align 8
%stackaddr$prim50508 = alloca %struct.ScmObj*, align 8
%args50092$ae41707$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41708, %struct.ScmObj* %args50092$ae41707$1)
store volatile %struct.ScmObj* %args50092$ae41707$2, %struct.ScmObj** %stackaddr$prim50508, align 8
%clofunc50509 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41707)
musttail call tailcc void %clofunc50509(%struct.ScmObj* %ae41707, %struct.ScmObj* %args50092$ae41707$2)
ret void
}

define tailcc void @proc_clo$ae41707(%struct.ScmObj* %env$ae41707,%struct.ScmObj* %current_45args49689) {
%stackaddr$env-ref50510 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41707, i64 0)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref50510
%stackaddr$env-ref50511 = alloca %struct.ScmObj*, align 8
%_37foldl140112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41707, i64 1)
store %struct.ScmObj* %_37foldl140112, %struct.ScmObj** %stackaddr$env-ref50511
%stackaddr$env-ref50512 = alloca %struct.ScmObj*, align 8
%_37last40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41707, i64 2)
store %struct.ScmObj* %_37last40150, %struct.ScmObj** %stackaddr$env-ref50512
%stackaddr$env-ref50513 = alloca %struct.ScmObj*, align 8
%_37foldr40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41707, i64 3)
store %struct.ScmObj* %_37foldr40133, %struct.ScmObj** %stackaddr$env-ref50513
%stackaddr$env-ref50514 = alloca %struct.ScmObj*, align 8
%_37drop_45right40147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41707, i64 4)
store %struct.ScmObj* %_37drop_45right40147, %struct.ScmObj** %stackaddr$env-ref50514
%stackaddr$env-ref50515 = alloca %struct.ScmObj*, align 8
%Ycmb40107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41707, i64 5)
store %struct.ScmObj* %Ycmb40107, %struct.ScmObj** %stackaddr$env-ref50515
%stackaddr$prim50516 = alloca %struct.ScmObj*, align 8
%_95k40438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49689)
store volatile %struct.ScmObj* %_95k40438, %struct.ScmObj** %stackaddr$prim50516, align 8
%stackaddr$prim50517 = alloca %struct.ScmObj*, align 8
%current_45args49690 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49689)
store volatile %struct.ScmObj* %current_45args49690, %struct.ScmObj** %stackaddr$prim50517, align 8
%stackaddr$prim50518 = alloca %struct.ScmObj*, align 8
%_37map140159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49690)
store volatile %struct.ScmObj* %_37map140159, %struct.ScmObj** %stackaddr$prim50518, align 8
%stackaddr$makeclosure50519 = alloca %struct.ScmObj*, align 8
%fptrToInt50520 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41784 to i64
%ae41784 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt50520)
store volatile %struct.ScmObj* %ae41784, %struct.ScmObj** %stackaddr$makeclosure50519, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41784, %struct.ScmObj* %_37foldr140128, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41784, %struct.ScmObj* %_37foldl140112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41784, %struct.ScmObj* %_37foldr40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41784, %struct.ScmObj* %_37map140159, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41784, %struct.ScmObj* %Ycmb40107, i64 4)
%ae41785 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50521 = alloca %struct.ScmObj*, align 8
%fptrToInt50522 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41786 to i64
%ae41786 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50522)
store volatile %struct.ScmObj* %ae41786, %struct.ScmObj** %stackaddr$makeclosure50521, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41786, %struct.ScmObj* %_37last40150, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41786, %struct.ScmObj* %_37foldr40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41786, %struct.ScmObj* %_37drop_45right40147, i64 2)
%args50073$ae41784$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50523 = alloca %struct.ScmObj*, align 8
%args50073$ae41784$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41786, %struct.ScmObj* %args50073$ae41784$0)
store volatile %struct.ScmObj* %args50073$ae41784$1, %struct.ScmObj** %stackaddr$prim50523, align 8
%stackaddr$prim50524 = alloca %struct.ScmObj*, align 8
%args50073$ae41784$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41785, %struct.ScmObj* %args50073$ae41784$1)
store volatile %struct.ScmObj* %args50073$ae41784$2, %struct.ScmObj** %stackaddr$prim50524, align 8
%clofunc50525 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41784)
musttail call tailcc void %clofunc50525(%struct.ScmObj* %ae41784, %struct.ScmObj* %args50073$ae41784$2)
ret void
}

define tailcc void @proc_clo$ae41784(%struct.ScmObj* %env$ae41784,%struct.ScmObj* %current_45args49692) {
%stackaddr$env-ref50526 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41784, i64 0)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref50526
%stackaddr$env-ref50527 = alloca %struct.ScmObj*, align 8
%_37foldl140112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41784, i64 1)
store %struct.ScmObj* %_37foldl140112, %struct.ScmObj** %stackaddr$env-ref50527
%stackaddr$env-ref50528 = alloca %struct.ScmObj*, align 8
%_37foldr40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41784, i64 2)
store %struct.ScmObj* %_37foldr40133, %struct.ScmObj** %stackaddr$env-ref50528
%stackaddr$env-ref50529 = alloca %struct.ScmObj*, align 8
%_37map140159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41784, i64 3)
store %struct.ScmObj* %_37map140159, %struct.ScmObj** %stackaddr$env-ref50529
%stackaddr$env-ref50530 = alloca %struct.ScmObj*, align 8
%Ycmb40107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41784, i64 4)
store %struct.ScmObj* %Ycmb40107, %struct.ScmObj** %stackaddr$env-ref50530
%stackaddr$prim50531 = alloca %struct.ScmObj*, align 8
%_95k40439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49692)
store volatile %struct.ScmObj* %_95k40439, %struct.ScmObj** %stackaddr$prim50531, align 8
%stackaddr$prim50532 = alloca %struct.ScmObj*, align 8
%current_45args49693 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49692)
store volatile %struct.ScmObj* %current_45args49693, %struct.ScmObj** %stackaddr$prim50532, align 8
%stackaddr$prim50533 = alloca %struct.ScmObj*, align 8
%_37map40154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49693)
store volatile %struct.ScmObj* %_37map40154, %struct.ScmObj** %stackaddr$prim50533, align 8
%stackaddr$makeclosure50534 = alloca %struct.ScmObj*, align 8
%fptrToInt50535 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41930 to i64
%ae41930 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50535)
store volatile %struct.ScmObj* %ae41930, %struct.ScmObj** %stackaddr$makeclosure50534, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41930, %struct.ScmObj* %_37foldl140112, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41930, %struct.ScmObj* %Ycmb40107, i64 1)
%ae41931 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50536 = alloca %struct.ScmObj*, align 8
%fptrToInt50537 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41932 to i64
%ae41932 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50537)
store volatile %struct.ScmObj* %ae41932, %struct.ScmObj** %stackaddr$makeclosure50536, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41932, %struct.ScmObj* %_37foldr40133, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41932, %struct.ScmObj* %_37foldr140128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41932, %struct.ScmObj* %_37map140159, i64 2)
%args50056$ae41930$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50538 = alloca %struct.ScmObj*, align 8
%args50056$ae41930$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41932, %struct.ScmObj* %args50056$ae41930$0)
store volatile %struct.ScmObj* %args50056$ae41930$1, %struct.ScmObj** %stackaddr$prim50538, align 8
%stackaddr$prim50539 = alloca %struct.ScmObj*, align 8
%args50056$ae41930$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41931, %struct.ScmObj* %args50056$ae41930$1)
store volatile %struct.ScmObj* %args50056$ae41930$2, %struct.ScmObj** %stackaddr$prim50539, align 8
%clofunc50540 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41930)
musttail call tailcc void %clofunc50540(%struct.ScmObj* %ae41930, %struct.ScmObj* %args50056$ae41930$2)
ret void
}

define tailcc void @proc_clo$ae41930(%struct.ScmObj* %env$ae41930,%struct.ScmObj* %current_45args49695) {
%stackaddr$env-ref50541 = alloca %struct.ScmObj*, align 8
%_37foldl140112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41930, i64 0)
store %struct.ScmObj* %_37foldl140112, %struct.ScmObj** %stackaddr$env-ref50541
%stackaddr$env-ref50542 = alloca %struct.ScmObj*, align 8
%Ycmb40107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41930, i64 1)
store %struct.ScmObj* %Ycmb40107, %struct.ScmObj** %stackaddr$env-ref50542
%stackaddr$prim50543 = alloca %struct.ScmObj*, align 8
%_95k40440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49695)
store volatile %struct.ScmObj* %_95k40440, %struct.ScmObj** %stackaddr$prim50543, align 8
%stackaddr$prim50544 = alloca %struct.ScmObj*, align 8
%current_45args49696 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49695)
store volatile %struct.ScmObj* %current_45args49696, %struct.ScmObj** %stackaddr$prim50544, align 8
%stackaddr$prim50545 = alloca %struct.ScmObj*, align 8
%anf_45bind40318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49696)
store volatile %struct.ScmObj* %anf_45bind40318, %struct.ScmObj** %stackaddr$prim50545, align 8
%stackaddr$makeclosure50546 = alloca %struct.ScmObj*, align 8
%fptrToInt50547 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42322 to i64
%ae42322 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50547)
store volatile %struct.ScmObj* %ae42322, %struct.ScmObj** %stackaddr$makeclosure50546, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42322, %struct.ScmObj* %_37foldl140112, i64 0)
%args49996$Ycmb40107$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50548 = alloca %struct.ScmObj*, align 8
%args49996$Ycmb40107$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40318, %struct.ScmObj* %args49996$Ycmb40107$0)
store volatile %struct.ScmObj* %args49996$Ycmb40107$1, %struct.ScmObj** %stackaddr$prim50548, align 8
%stackaddr$prim50549 = alloca %struct.ScmObj*, align 8
%args49996$Ycmb40107$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42322, %struct.ScmObj* %args49996$Ycmb40107$1)
store volatile %struct.ScmObj* %args49996$Ycmb40107$2, %struct.ScmObj** %stackaddr$prim50549, align 8
%clofunc50550 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40107)
musttail call tailcc void %clofunc50550(%struct.ScmObj* %Ycmb40107, %struct.ScmObj* %args49996$Ycmb40107$2)
ret void
}

define tailcc void @proc_clo$ae42322(%struct.ScmObj* %env$ae42322,%struct.ScmObj* %current_45args49698) {
%stackaddr$env-ref50551 = alloca %struct.ScmObj*, align 8
%_37foldl140112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42322, i64 0)
store %struct.ScmObj* %_37foldl140112, %struct.ScmObj** %stackaddr$env-ref50551
%stackaddr$prim50552 = alloca %struct.ScmObj*, align 8
%_95k40441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49698)
store volatile %struct.ScmObj* %_95k40441, %struct.ScmObj** %stackaddr$prim50552, align 8
%stackaddr$prim50553 = alloca %struct.ScmObj*, align 8
%current_45args49699 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49698)
store volatile %struct.ScmObj* %current_45args49699, %struct.ScmObj** %stackaddr$prim50553, align 8
%stackaddr$prim50554 = alloca %struct.ScmObj*, align 8
%_37foldl40210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49699)
store volatile %struct.ScmObj* %_37foldl40210, %struct.ScmObj** %stackaddr$prim50554, align 8
%stackaddr$makeclosure50555 = alloca %struct.ScmObj*, align 8
%fptrToInt50556 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42324 to i64
%ae42324 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50556)
store volatile %struct.ScmObj* %ae42324, %struct.ScmObj** %stackaddr$makeclosure50555, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42324, %struct.ScmObj* %_37foldl140112, i64 0)
%ae42325 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50557 = alloca %struct.ScmObj*, align 8
%fptrToInt50558 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42326 to i64
%ae42326 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50558)
store volatile %struct.ScmObj* %ae42326, %struct.ScmObj** %stackaddr$makeclosure50557, align 8
%args49995$ae42324$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50559 = alloca %struct.ScmObj*, align 8
%args49995$ae42324$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42326, %struct.ScmObj* %args49995$ae42324$0)
store volatile %struct.ScmObj* %args49995$ae42324$1, %struct.ScmObj** %stackaddr$prim50559, align 8
%stackaddr$prim50560 = alloca %struct.ScmObj*, align 8
%args49995$ae42324$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42325, %struct.ScmObj* %args49995$ae42324$1)
store volatile %struct.ScmObj* %args49995$ae42324$2, %struct.ScmObj** %stackaddr$prim50560, align 8
%clofunc50561 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42324)
musttail call tailcc void %clofunc50561(%struct.ScmObj* %ae42324, %struct.ScmObj* %args49995$ae42324$2)
ret void
}

define tailcc void @proc_clo$ae42324(%struct.ScmObj* %env$ae42324,%struct.ScmObj* %current_45args49701) {
%stackaddr$env-ref50562 = alloca %struct.ScmObj*, align 8
%_37foldl140112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42324, i64 0)
store %struct.ScmObj* %_37foldl140112, %struct.ScmObj** %stackaddr$env-ref50562
%stackaddr$prim50563 = alloca %struct.ScmObj*, align 8
%_95k40442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49701)
store volatile %struct.ScmObj* %_95k40442, %struct.ScmObj** %stackaddr$prim50563, align 8
%stackaddr$prim50564 = alloca %struct.ScmObj*, align 8
%current_45args49702 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49701)
store volatile %struct.ScmObj* %current_45args49702, %struct.ScmObj** %stackaddr$prim50564, align 8
%stackaddr$prim50565 = alloca %struct.ScmObj*, align 8
%_37_6240207 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49702)
store volatile %struct.ScmObj* %_37_6240207, %struct.ScmObj** %stackaddr$prim50565, align 8
%stackaddr$makeclosure50566 = alloca %struct.ScmObj*, align 8
%fptrToInt50567 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42348 to i64
%ae42348 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50567)
store volatile %struct.ScmObj* %ae42348, %struct.ScmObj** %stackaddr$makeclosure50566, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42348, %struct.ScmObj* %_37foldl140112, i64 0)
%ae42349 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50568 = alloca %struct.ScmObj*, align 8
%fptrToInt50569 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42350 to i64
%ae42350 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50569)
store volatile %struct.ScmObj* %ae42350, %struct.ScmObj** %stackaddr$makeclosure50568, align 8
%args49989$ae42348$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50570 = alloca %struct.ScmObj*, align 8
%args49989$ae42348$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42350, %struct.ScmObj* %args49989$ae42348$0)
store volatile %struct.ScmObj* %args49989$ae42348$1, %struct.ScmObj** %stackaddr$prim50570, align 8
%stackaddr$prim50571 = alloca %struct.ScmObj*, align 8
%args49989$ae42348$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42349, %struct.ScmObj* %args49989$ae42348$1)
store volatile %struct.ScmObj* %args49989$ae42348$2, %struct.ScmObj** %stackaddr$prim50571, align 8
%clofunc50572 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42348)
musttail call tailcc void %clofunc50572(%struct.ScmObj* %ae42348, %struct.ScmObj* %args49989$ae42348$2)
ret void
}

define tailcc void @proc_clo$ae42348(%struct.ScmObj* %env$ae42348,%struct.ScmObj* %current_45args49704) {
%stackaddr$env-ref50573 = alloca %struct.ScmObj*, align 8
%_37foldl140112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42348, i64 0)
store %struct.ScmObj* %_37foldl140112, %struct.ScmObj** %stackaddr$env-ref50573
%stackaddr$prim50574 = alloca %struct.ScmObj*, align 8
%_95k40443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49704)
store volatile %struct.ScmObj* %_95k40443, %struct.ScmObj** %stackaddr$prim50574, align 8
%stackaddr$prim50575 = alloca %struct.ScmObj*, align 8
%current_45args49705 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49704)
store volatile %struct.ScmObj* %current_45args49705, %struct.ScmObj** %stackaddr$prim50575, align 8
%stackaddr$prim50576 = alloca %struct.ScmObj*, align 8
%_37_62_6140204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49705)
store volatile %struct.ScmObj* %_37_62_6140204, %struct.ScmObj** %stackaddr$prim50576, align 8
%ae42372 = call %struct.ScmObj* @const_init_int(i64 1)
%ae42373 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50577 = alloca %struct.ScmObj*, align 8
%_37append40200 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42372, %struct.ScmObj* %ae42373)
store volatile %struct.ScmObj* %_37append40200, %struct.ScmObj** %stackaddr$prim50577, align 8
%stackaddr$makeclosure50578 = alloca %struct.ScmObj*, align 8
%fptrToInt50579 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42374 to i64
%ae42374 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50579)
store volatile %struct.ScmObj* %ae42374, %struct.ScmObj** %stackaddr$makeclosure50578, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42374, %struct.ScmObj* %_37append40200, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42374, %struct.ScmObj* %_37foldl140112, i64 1)
%ae42375 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50580 = alloca %struct.ScmObj*, align 8
%fptrToInt50581 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42376 to i64
%ae42376 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50581)
store volatile %struct.ScmObj* %ae42376, %struct.ScmObj** %stackaddr$makeclosure50580, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42376, %struct.ScmObj* %_37append40200, i64 0)
%args49983$ae42374$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50582 = alloca %struct.ScmObj*, align 8
%args49983$ae42374$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42376, %struct.ScmObj* %args49983$ae42374$0)
store volatile %struct.ScmObj* %args49983$ae42374$1, %struct.ScmObj** %stackaddr$prim50582, align 8
%stackaddr$prim50583 = alloca %struct.ScmObj*, align 8
%args49983$ae42374$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42375, %struct.ScmObj* %args49983$ae42374$1)
store volatile %struct.ScmObj* %args49983$ae42374$2, %struct.ScmObj** %stackaddr$prim50583, align 8
%clofunc50584 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42374)
musttail call tailcc void %clofunc50584(%struct.ScmObj* %ae42374, %struct.ScmObj* %args49983$ae42374$2)
ret void
}

define tailcc void @proc_clo$ae42374(%struct.ScmObj* %env$ae42374,%struct.ScmObj* %current_45args49707) {
%stackaddr$env-ref50585 = alloca %struct.ScmObj*, align 8
%_37append40200 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42374, i64 0)
store %struct.ScmObj* %_37append40200, %struct.ScmObj** %stackaddr$env-ref50585
%stackaddr$env-ref50586 = alloca %struct.ScmObj*, align 8
%_37foldl140112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42374, i64 1)
store %struct.ScmObj* %_37foldl140112, %struct.ScmObj** %stackaddr$env-ref50586
%stackaddr$prim50587 = alloca %struct.ScmObj*, align 8
%_95k40444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49707)
store volatile %struct.ScmObj* %_95k40444, %struct.ScmObj** %stackaddr$prim50587, align 8
%stackaddr$prim50588 = alloca %struct.ScmObj*, align 8
%current_45args49708 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49707)
store volatile %struct.ScmObj* %current_45args49708, %struct.ScmObj** %stackaddr$prim50588, align 8
%stackaddr$prim50589 = alloca %struct.ScmObj*, align 8
%anf_45bind40326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49708)
store volatile %struct.ScmObj* %anf_45bind40326, %struct.ScmObj** %stackaddr$prim50589, align 8
%ae42442 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50590 = alloca %struct.ScmObj*, align 8
%_95040201 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append40200, %struct.ScmObj* %ae42442, %struct.ScmObj* %anf_45bind40326)
store volatile %struct.ScmObj* %_95040201, %struct.ScmObj** %stackaddr$prim50590, align 8
%ae42445 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50591 = alloca %struct.ScmObj*, align 8
%_37append40199 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40200, %struct.ScmObj* %ae42445)
store volatile %struct.ScmObj* %_37append40199, %struct.ScmObj** %stackaddr$prim50591, align 8
%stackaddr$makeclosure50592 = alloca %struct.ScmObj*, align 8
%fptrToInt50593 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42446 to i64
%ae42446 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50593)
store volatile %struct.ScmObj* %ae42446, %struct.ScmObj** %stackaddr$makeclosure50592, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42446, %struct.ScmObj* %_37append40199, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42446, %struct.ScmObj* %_37foldl140112, i64 1)
%ae42447 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50594 = alloca %struct.ScmObj*, align 8
%fptrToInt50595 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42448 to i64
%ae42448 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50595)
store volatile %struct.ScmObj* %ae42448, %struct.ScmObj** %stackaddr$makeclosure50594, align 8
%args49972$ae42446$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50596 = alloca %struct.ScmObj*, align 8
%args49972$ae42446$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42448, %struct.ScmObj* %args49972$ae42446$0)
store volatile %struct.ScmObj* %args49972$ae42446$1, %struct.ScmObj** %stackaddr$prim50596, align 8
%stackaddr$prim50597 = alloca %struct.ScmObj*, align 8
%args49972$ae42446$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42447, %struct.ScmObj* %args49972$ae42446$1)
store volatile %struct.ScmObj* %args49972$ae42446$2, %struct.ScmObj** %stackaddr$prim50597, align 8
%clofunc50598 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42446)
musttail call tailcc void %clofunc50598(%struct.ScmObj* %ae42446, %struct.ScmObj* %args49972$ae42446$2)
ret void
}

define tailcc void @proc_clo$ae42446(%struct.ScmObj* %env$ae42446,%struct.ScmObj* %current_45args49710) {
%stackaddr$env-ref50599 = alloca %struct.ScmObj*, align 8
%_37append40199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42446, i64 0)
store %struct.ScmObj* %_37append40199, %struct.ScmObj** %stackaddr$env-ref50599
%stackaddr$env-ref50600 = alloca %struct.ScmObj*, align 8
%_37foldl140112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42446, i64 1)
store %struct.ScmObj* %_37foldl140112, %struct.ScmObj** %stackaddr$env-ref50600
%stackaddr$prim50601 = alloca %struct.ScmObj*, align 8
%_95k40445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49710)
store volatile %struct.ScmObj* %_95k40445, %struct.ScmObj** %stackaddr$prim50601, align 8
%stackaddr$prim50602 = alloca %struct.ScmObj*, align 8
%current_45args49711 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49710)
store volatile %struct.ScmObj* %current_45args49711, %struct.ScmObj** %stackaddr$prim50602, align 8
%stackaddr$prim50603 = alloca %struct.ScmObj*, align 8
%_37list_6340192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49711)
store volatile %struct.ScmObj* %_37list_6340192, %struct.ScmObj** %stackaddr$prim50603, align 8
%stackaddr$makeclosure50604 = alloca %struct.ScmObj*, align 8
%fptrToInt50605 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42862 to i64
%ae42862 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50605)
store volatile %struct.ScmObj* %ae42862, %struct.ScmObj** %stackaddr$makeclosure50604, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42862, %struct.ScmObj* %_37append40199, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42862, %struct.ScmObj* %_37foldl140112, i64 1)
%ae42863 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50606 = alloca %struct.ScmObj*, align 8
%fptrToInt50607 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42864 to i64
%ae42864 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50607)
store volatile %struct.ScmObj* %ae42864, %struct.ScmObj** %stackaddr$makeclosure50606, align 8
%args49947$ae42862$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50608 = alloca %struct.ScmObj*, align 8
%args49947$ae42862$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42864, %struct.ScmObj* %args49947$ae42862$0)
store volatile %struct.ScmObj* %args49947$ae42862$1, %struct.ScmObj** %stackaddr$prim50608, align 8
%stackaddr$prim50609 = alloca %struct.ScmObj*, align 8
%args49947$ae42862$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42863, %struct.ScmObj* %args49947$ae42862$1)
store volatile %struct.ScmObj* %args49947$ae42862$2, %struct.ScmObj** %stackaddr$prim50609, align 8
%clofunc50610 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42862)
musttail call tailcc void %clofunc50610(%struct.ScmObj* %ae42862, %struct.ScmObj* %args49947$ae42862$2)
ret void
}

define tailcc void @proc_clo$ae42862(%struct.ScmObj* %env$ae42862,%struct.ScmObj* %current_45args49713) {
%stackaddr$env-ref50611 = alloca %struct.ScmObj*, align 8
%_37append40199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42862, i64 0)
store %struct.ScmObj* %_37append40199, %struct.ScmObj** %stackaddr$env-ref50611
%stackaddr$env-ref50612 = alloca %struct.ScmObj*, align 8
%_37foldl140112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42862, i64 1)
store %struct.ScmObj* %_37foldl140112, %struct.ScmObj** %stackaddr$env-ref50612
%stackaddr$prim50613 = alloca %struct.ScmObj*, align 8
%_95k40446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49713)
store volatile %struct.ScmObj* %_95k40446, %struct.ScmObj** %stackaddr$prim50613, align 8
%stackaddr$prim50614 = alloca %struct.ScmObj*, align 8
%current_45args49714 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49713)
store volatile %struct.ScmObj* %current_45args49714, %struct.ScmObj** %stackaddr$prim50614, align 8
%stackaddr$prim50615 = alloca %struct.ScmObj*, align 8
%_37drop40183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49714)
store volatile %struct.ScmObj* %_37drop40183, %struct.ScmObj** %stackaddr$prim50615, align 8
%stackaddr$makeclosure50616 = alloca %struct.ScmObj*, align 8
%fptrToInt50617 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43398 to i64
%ae43398 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50617)
store volatile %struct.ScmObj* %ae43398, %struct.ScmObj** %stackaddr$makeclosure50616, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43398, %struct.ScmObj* %_37append40199, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43398, %struct.ScmObj* %_37foldl140112, i64 1)
%ae43399 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50618 = alloca %struct.ScmObj*, align 8
%fptrToInt50619 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43400 to i64
%ae43400 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50619)
store volatile %struct.ScmObj* %ae43400, %struct.ScmObj** %stackaddr$makeclosure50618, align 8
%args49923$ae43398$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50620 = alloca %struct.ScmObj*, align 8
%args49923$ae43398$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43400, %struct.ScmObj* %args49923$ae43398$0)
store volatile %struct.ScmObj* %args49923$ae43398$1, %struct.ScmObj** %stackaddr$prim50620, align 8
%stackaddr$prim50621 = alloca %struct.ScmObj*, align 8
%args49923$ae43398$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43399, %struct.ScmObj* %args49923$ae43398$1)
store volatile %struct.ScmObj* %args49923$ae43398$2, %struct.ScmObj** %stackaddr$prim50621, align 8
%clofunc50622 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43398)
musttail call tailcc void %clofunc50622(%struct.ScmObj* %ae43398, %struct.ScmObj* %args49923$ae43398$2)
ret void
}

define tailcc void @proc_clo$ae43398(%struct.ScmObj* %env$ae43398,%struct.ScmObj* %current_45args49716) {
%stackaddr$env-ref50623 = alloca %struct.ScmObj*, align 8
%_37append40199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43398, i64 0)
store %struct.ScmObj* %_37append40199, %struct.ScmObj** %stackaddr$env-ref50623
%stackaddr$env-ref50624 = alloca %struct.ScmObj*, align 8
%_37foldl140112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43398, i64 1)
store %struct.ScmObj* %_37foldl140112, %struct.ScmObj** %stackaddr$env-ref50624
%stackaddr$prim50625 = alloca %struct.ScmObj*, align 8
%_95k40447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49716)
store volatile %struct.ScmObj* %_95k40447, %struct.ScmObj** %stackaddr$prim50625, align 8
%stackaddr$prim50626 = alloca %struct.ScmObj*, align 8
%current_45args49717 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49716)
store volatile %struct.ScmObj* %current_45args49717, %struct.ScmObj** %stackaddr$prim50626, align 8
%stackaddr$prim50627 = alloca %struct.ScmObj*, align 8
%_37memv40176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49717)
store volatile %struct.ScmObj* %_37memv40176, %struct.ScmObj** %stackaddr$prim50627, align 8
%stackaddr$makeclosure50628 = alloca %struct.ScmObj*, align 8
%fptrToInt50629 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43802 to i64
%ae43802 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50629)
store volatile %struct.ScmObj* %ae43802, %struct.ScmObj** %stackaddr$makeclosure50628, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43802, %struct.ScmObj* %_37append40199, i64 0)
%ae43803 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50630 = alloca %struct.ScmObj*, align 8
%fptrToInt50631 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43804 to i64
%ae43804 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50631)
store volatile %struct.ScmObj* %ae43804, %struct.ScmObj** %stackaddr$makeclosure50630, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43804, %struct.ScmObj* %_37foldl140112, i64 0)
%args49897$ae43802$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50632 = alloca %struct.ScmObj*, align 8
%args49897$ae43802$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43804, %struct.ScmObj* %args49897$ae43802$0)
store volatile %struct.ScmObj* %args49897$ae43802$1, %struct.ScmObj** %stackaddr$prim50632, align 8
%stackaddr$prim50633 = alloca %struct.ScmObj*, align 8
%args49897$ae43802$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43803, %struct.ScmObj* %args49897$ae43802$1)
store volatile %struct.ScmObj* %args49897$ae43802$2, %struct.ScmObj** %stackaddr$prim50633, align 8
%clofunc50634 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43802)
musttail call tailcc void %clofunc50634(%struct.ScmObj* %ae43802, %struct.ScmObj* %args49897$ae43802$2)
ret void
}

define tailcc void @proc_clo$ae43802(%struct.ScmObj* %env$ae43802,%struct.ScmObj* %current_45args49719) {
%stackaddr$env-ref50635 = alloca %struct.ScmObj*, align 8
%_37append40199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43802, i64 0)
store %struct.ScmObj* %_37append40199, %struct.ScmObj** %stackaddr$env-ref50635
%stackaddr$prim50636 = alloca %struct.ScmObj*, align 8
%_95k40448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49719)
store volatile %struct.ScmObj* %_95k40448, %struct.ScmObj** %stackaddr$prim50636, align 8
%stackaddr$prim50637 = alloca %struct.ScmObj*, align 8
%current_45args49720 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49719)
store volatile %struct.ScmObj* %current_45args49720, %struct.ScmObj** %stackaddr$prim50637, align 8
%stackaddr$prim50638 = alloca %struct.ScmObj*, align 8
%_37_4740172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49720)
store volatile %struct.ScmObj* %_37_4740172, %struct.ScmObj** %stackaddr$prim50638, align 8
%stackaddr$makeclosure50639 = alloca %struct.ScmObj*, align 8
%fptrToInt50640 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43900 to i64
%ae43900 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50640)
store volatile %struct.ScmObj* %ae43900, %struct.ScmObj** %stackaddr$makeclosure50639, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43900, %struct.ScmObj* %_37append40199, i64 0)
%ae43901 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50641 = alloca %struct.ScmObj*, align 8
%fptrToInt50642 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43902 to i64
%ae43902 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50642)
store volatile %struct.ScmObj* %ae43902, %struct.ScmObj** %stackaddr$makeclosure50641, align 8
%args49884$ae43900$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50643 = alloca %struct.ScmObj*, align 8
%args49884$ae43900$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43902, %struct.ScmObj* %args49884$ae43900$0)
store volatile %struct.ScmObj* %args49884$ae43900$1, %struct.ScmObj** %stackaddr$prim50643, align 8
%stackaddr$prim50644 = alloca %struct.ScmObj*, align 8
%args49884$ae43900$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43901, %struct.ScmObj* %args49884$ae43900$1)
store volatile %struct.ScmObj* %args49884$ae43900$2, %struct.ScmObj** %stackaddr$prim50644, align 8
%clofunc50645 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43900)
musttail call tailcc void %clofunc50645(%struct.ScmObj* %ae43900, %struct.ScmObj* %args49884$ae43900$2)
ret void
}

define tailcc void @proc_clo$ae43900(%struct.ScmObj* %env$ae43900,%struct.ScmObj* %current_45args49722) {
%stackaddr$env-ref50646 = alloca %struct.ScmObj*, align 8
%_37append40199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43900, i64 0)
store %struct.ScmObj* %_37append40199, %struct.ScmObj** %stackaddr$env-ref50646
%stackaddr$prim50647 = alloca %struct.ScmObj*, align 8
%_95k40449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49722)
store volatile %struct.ScmObj* %_95k40449, %struct.ScmObj** %stackaddr$prim50647, align 8
%stackaddr$prim50648 = alloca %struct.ScmObj*, align 8
%current_45args49723 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49722)
store volatile %struct.ScmObj* %current_45args49723, %struct.ScmObj** %stackaddr$prim50648, align 8
%stackaddr$prim50649 = alloca %struct.ScmObj*, align 8
%_37first40170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49723)
store volatile %struct.ScmObj* %_37first40170, %struct.ScmObj** %stackaddr$prim50649, align 8
%stackaddr$makeclosure50650 = alloca %struct.ScmObj*, align 8
%fptrToInt50651 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43920 to i64
%ae43920 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50651)
store volatile %struct.ScmObj* %ae43920, %struct.ScmObj** %stackaddr$makeclosure50650, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43920, %struct.ScmObj* %_37append40199, i64 0)
%ae43921 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50652 = alloca %struct.ScmObj*, align 8
%fptrToInt50653 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43922 to i64
%ae43922 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50653)
store volatile %struct.ScmObj* %ae43922, %struct.ScmObj** %stackaddr$makeclosure50652, align 8
%args49879$ae43920$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50654 = alloca %struct.ScmObj*, align 8
%args49879$ae43920$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43922, %struct.ScmObj* %args49879$ae43920$0)
store volatile %struct.ScmObj* %args49879$ae43920$1, %struct.ScmObj** %stackaddr$prim50654, align 8
%stackaddr$prim50655 = alloca %struct.ScmObj*, align 8
%args49879$ae43920$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43921, %struct.ScmObj* %args49879$ae43920$1)
store volatile %struct.ScmObj* %args49879$ae43920$2, %struct.ScmObj** %stackaddr$prim50655, align 8
%clofunc50656 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43920)
musttail call tailcc void %clofunc50656(%struct.ScmObj* %ae43920, %struct.ScmObj* %args49879$ae43920$2)
ret void
}

define tailcc void @proc_clo$ae43920(%struct.ScmObj* %env$ae43920,%struct.ScmObj* %current_45args49725) {
%stackaddr$env-ref50657 = alloca %struct.ScmObj*, align 8
%_37append40199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43920, i64 0)
store %struct.ScmObj* %_37append40199, %struct.ScmObj** %stackaddr$env-ref50657
%stackaddr$prim50658 = alloca %struct.ScmObj*, align 8
%_95k40450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49725)
store volatile %struct.ScmObj* %_95k40450, %struct.ScmObj** %stackaddr$prim50658, align 8
%stackaddr$prim50659 = alloca %struct.ScmObj*, align 8
%current_45args49726 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49725)
store volatile %struct.ScmObj* %current_45args49726, %struct.ScmObj** %stackaddr$prim50659, align 8
%stackaddr$prim50660 = alloca %struct.ScmObj*, align 8
%_37second40168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49726)
store volatile %struct.ScmObj* %_37second40168, %struct.ScmObj** %stackaddr$prim50660, align 8
%stackaddr$makeclosure50661 = alloca %struct.ScmObj*, align 8
%fptrToInt50662 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43942 to i64
%ae43942 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50662)
store volatile %struct.ScmObj* %ae43942, %struct.ScmObj** %stackaddr$makeclosure50661, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43942, %struct.ScmObj* %_37append40199, i64 0)
%ae43943 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50663 = alloca %struct.ScmObj*, align 8
%fptrToInt50664 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43944 to i64
%ae43944 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50664)
store volatile %struct.ScmObj* %ae43944, %struct.ScmObj** %stackaddr$makeclosure50663, align 8
%args49874$ae43942$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50665 = alloca %struct.ScmObj*, align 8
%args49874$ae43942$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43944, %struct.ScmObj* %args49874$ae43942$0)
store volatile %struct.ScmObj* %args49874$ae43942$1, %struct.ScmObj** %stackaddr$prim50665, align 8
%stackaddr$prim50666 = alloca %struct.ScmObj*, align 8
%args49874$ae43942$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43943, %struct.ScmObj* %args49874$ae43942$1)
store volatile %struct.ScmObj* %args49874$ae43942$2, %struct.ScmObj** %stackaddr$prim50666, align 8
%clofunc50667 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43942)
musttail call tailcc void %clofunc50667(%struct.ScmObj* %ae43942, %struct.ScmObj* %args49874$ae43942$2)
ret void
}

define tailcc void @proc_clo$ae43942(%struct.ScmObj* %env$ae43942,%struct.ScmObj* %current_45args49728) {
%stackaddr$env-ref50668 = alloca %struct.ScmObj*, align 8
%_37append40199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43942, i64 0)
store %struct.ScmObj* %_37append40199, %struct.ScmObj** %stackaddr$env-ref50668
%stackaddr$prim50669 = alloca %struct.ScmObj*, align 8
%_95k40451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49728)
store volatile %struct.ScmObj* %_95k40451, %struct.ScmObj** %stackaddr$prim50669, align 8
%stackaddr$prim50670 = alloca %struct.ScmObj*, align 8
%current_45args49729 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49728)
store volatile %struct.ScmObj* %current_45args49729, %struct.ScmObj** %stackaddr$prim50670, align 8
%stackaddr$prim50671 = alloca %struct.ScmObj*, align 8
%_37third40166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49729)
store volatile %struct.ScmObj* %_37third40166, %struct.ScmObj** %stackaddr$prim50671, align 8
%stackaddr$makeclosure50672 = alloca %struct.ScmObj*, align 8
%fptrToInt50673 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43966 to i64
%ae43966 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50673)
store volatile %struct.ScmObj* %ae43966, %struct.ScmObj** %stackaddr$makeclosure50672, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43966, %struct.ScmObj* %_37append40199, i64 0)
%ae43967 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50674 = alloca %struct.ScmObj*, align 8
%fptrToInt50675 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43968 to i64
%ae43968 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50675)
store volatile %struct.ScmObj* %ae43968, %struct.ScmObj** %stackaddr$makeclosure50674, align 8
%args49869$ae43966$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50676 = alloca %struct.ScmObj*, align 8
%args49869$ae43966$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43968, %struct.ScmObj* %args49869$ae43966$0)
store volatile %struct.ScmObj* %args49869$ae43966$1, %struct.ScmObj** %stackaddr$prim50676, align 8
%stackaddr$prim50677 = alloca %struct.ScmObj*, align 8
%args49869$ae43966$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43967, %struct.ScmObj* %args49869$ae43966$1)
store volatile %struct.ScmObj* %args49869$ae43966$2, %struct.ScmObj** %stackaddr$prim50677, align 8
%clofunc50678 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43966)
musttail call tailcc void %clofunc50678(%struct.ScmObj* %ae43966, %struct.ScmObj* %args49869$ae43966$2)
ret void
}

define tailcc void @proc_clo$ae43966(%struct.ScmObj* %env$ae43966,%struct.ScmObj* %current_45args49731) {
%stackaddr$env-ref50679 = alloca %struct.ScmObj*, align 8
%_37append40199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43966, i64 0)
store %struct.ScmObj* %_37append40199, %struct.ScmObj** %stackaddr$env-ref50679
%stackaddr$prim50680 = alloca %struct.ScmObj*, align 8
%_95k40452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49731)
store volatile %struct.ScmObj* %_95k40452, %struct.ScmObj** %stackaddr$prim50680, align 8
%stackaddr$prim50681 = alloca %struct.ScmObj*, align 8
%current_45args49732 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49731)
store volatile %struct.ScmObj* %current_45args49732, %struct.ScmObj** %stackaddr$prim50681, align 8
%stackaddr$prim50682 = alloca %struct.ScmObj*, align 8
%_37fourth40164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49732)
store volatile %struct.ScmObj* %_37fourth40164, %struct.ScmObj** %stackaddr$prim50682, align 8
%stackaddr$makeclosure50683 = alloca %struct.ScmObj*, align 8
%fptrToInt50684 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43992 to i64
%ae43992 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50684)
store volatile %struct.ScmObj* %ae43992, %struct.ScmObj** %stackaddr$makeclosure50683, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43992, %struct.ScmObj* %_37append40199, i64 0)
%ae43993 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50685 = alloca %struct.ScmObj*, align 8
%fptrToInt50686 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43994 to i64
%ae43994 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50686)
store volatile %struct.ScmObj* %ae43994, %struct.ScmObj** %stackaddr$makeclosure50685, align 8
%args49864$ae43992$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50687 = alloca %struct.ScmObj*, align 8
%args49864$ae43992$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43994, %struct.ScmObj* %args49864$ae43992$0)
store volatile %struct.ScmObj* %args49864$ae43992$1, %struct.ScmObj** %stackaddr$prim50687, align 8
%stackaddr$prim50688 = alloca %struct.ScmObj*, align 8
%args49864$ae43992$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43993, %struct.ScmObj* %args49864$ae43992$1)
store volatile %struct.ScmObj* %args49864$ae43992$2, %struct.ScmObj** %stackaddr$prim50688, align 8
%clofunc50689 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43992)
musttail call tailcc void %clofunc50689(%struct.ScmObj* %ae43992, %struct.ScmObj* %args49864$ae43992$2)
ret void
}

define tailcc void @proc_clo$ae43992(%struct.ScmObj* %env$ae43992,%struct.ScmObj* %current_45args49734) {
%stackaddr$env-ref50690 = alloca %struct.ScmObj*, align 8
%_37append40199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43992, i64 0)
store %struct.ScmObj* %_37append40199, %struct.ScmObj** %stackaddr$env-ref50690
%stackaddr$prim50691 = alloca %struct.ScmObj*, align 8
%_95k40453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49734)
store volatile %struct.ScmObj* %_95k40453, %struct.ScmObj** %stackaddr$prim50691, align 8
%stackaddr$prim50692 = alloca %struct.ScmObj*, align 8
%current_45args49735 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49734)
store volatile %struct.ScmObj* %current_45args49735, %struct.ScmObj** %stackaddr$prim50692, align 8
%stackaddr$prim50693 = alloca %struct.ScmObj*, align 8
%promise_6340225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49735)
store volatile %struct.ScmObj* %promise_6340225, %struct.ScmObj** %stackaddr$prim50693, align 8
%stackaddr$prim50694 = alloca %struct.ScmObj*, align 8
%anf_45bind40366 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40366, %struct.ScmObj** %stackaddr$prim50694, align 8
%ae44079 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50695 = alloca %struct.ScmObj*, align 8
%nqueens40227 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44079, %struct.ScmObj* %anf_45bind40366)
store volatile %struct.ScmObj* %nqueens40227, %struct.ScmObj** %stackaddr$prim50695, align 8
%stackaddr$makeclosure50696 = alloca %struct.ScmObj*, align 8
%fptrToInt50697 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44081 to i64
%ae44081 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50697)
store volatile %struct.ScmObj* %ae44081, %struct.ScmObj** %stackaddr$makeclosure50696, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44081, %struct.ScmObj* %nqueens40227, i64 0)
%ae44082 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50698 = alloca %struct.ScmObj*, align 8
%fptrToInt50699 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44083 to i64
%ae44083 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50699)
store volatile %struct.ScmObj* %ae44083, %struct.ScmObj** %stackaddr$makeclosure50698, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44083, %struct.ScmObj* %_37append40199, i64 0)
%args49857$ae44081$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50700 = alloca %struct.ScmObj*, align 8
%args49857$ae44081$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44083, %struct.ScmObj* %args49857$ae44081$0)
store volatile %struct.ScmObj* %args49857$ae44081$1, %struct.ScmObj** %stackaddr$prim50700, align 8
%stackaddr$prim50701 = alloca %struct.ScmObj*, align 8
%args49857$ae44081$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44082, %struct.ScmObj* %args49857$ae44081$1)
store volatile %struct.ScmObj* %args49857$ae44081$2, %struct.ScmObj** %stackaddr$prim50701, align 8
%clofunc50702 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44081)
musttail call tailcc void %clofunc50702(%struct.ScmObj* %ae44081, %struct.ScmObj* %args49857$ae44081$2)
ret void
}

define tailcc void @proc_clo$ae44081(%struct.ScmObj* %env$ae44081,%struct.ScmObj* %current_45args49737) {
%stackaddr$env-ref50703 = alloca %struct.ScmObj*, align 8
%nqueens40227 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44081, i64 0)
store %struct.ScmObj* %nqueens40227, %struct.ScmObj** %stackaddr$env-ref50703
%stackaddr$prim50704 = alloca %struct.ScmObj*, align 8
%_95k40454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49737)
store volatile %struct.ScmObj* %_95k40454, %struct.ScmObj** %stackaddr$prim50704, align 8
%stackaddr$prim50705 = alloca %struct.ScmObj*, align 8
%current_45args49738 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49737)
store volatile %struct.ScmObj* %current_45args49738, %struct.ScmObj** %stackaddr$prim50705, align 8
%stackaddr$prim50706 = alloca %struct.ScmObj*, align 8
%anf_45bind40419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49738)
store volatile %struct.ScmObj* %anf_45bind40419, %struct.ScmObj** %stackaddr$prim50706, align 8
%ae46290 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50707 = alloca %struct.ScmObj*, align 8
%t4010240228 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %nqueens40227, %struct.ScmObj* %ae46290, %struct.ScmObj* %anf_45bind40419)
store volatile %struct.ScmObj* %t4010240228, %struct.ScmObj** %stackaddr$prim50707, align 8
%ae46293 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50708 = alloca %struct.ScmObj*, align 8
%anf_45bind40420 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %nqueens40227, %struct.ScmObj* %ae46293)
store volatile %struct.ScmObj* %anf_45bind40420, %struct.ScmObj** %stackaddr$prim50708, align 8
%stackaddr$makeclosure50709 = alloca %struct.ScmObj*, align 8
%fptrToInt50710 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae46295 to i64
%ae46295 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50710)
store volatile %struct.ScmObj* %ae46295, %struct.ScmObj** %stackaddr$makeclosure50709, align 8
%ae46296 = call %struct.ScmObj* @const_init_int(i64 7)
%args49744$anf_45bind40420$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50711 = alloca %struct.ScmObj*, align 8
%args49744$anf_45bind40420$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46296, %struct.ScmObj* %args49744$anf_45bind40420$0)
store volatile %struct.ScmObj* %args49744$anf_45bind40420$1, %struct.ScmObj** %stackaddr$prim50711, align 8
%stackaddr$prim50712 = alloca %struct.ScmObj*, align 8
%args49744$anf_45bind40420$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae46295, %struct.ScmObj* %args49744$anf_45bind40420$1)
store volatile %struct.ScmObj* %args49744$anf_45bind40420$2, %struct.ScmObj** %stackaddr$prim50712, align 8
%clofunc50713 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40420)
musttail call tailcc void %clofunc50713(%struct.ScmObj* %anf_45bind40420, %struct.ScmObj* %args49744$anf_45bind40420$2)
ret void
}

define tailcc void @proc_clo$ae46295(%struct.ScmObj* %env$ae46295,%struct.ScmObj* %current_45args49740) {
%stackaddr$prim50714 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49740)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim50714, align 8
%stackaddr$prim50715 = alloca %struct.ScmObj*, align 8
%current_45args49741 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49740)
store volatile %struct.ScmObj* %current_45args49741, %struct.ScmObj** %stackaddr$prim50715, align 8
%stackaddr$prim50716 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49741)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim50716, align 8
%stackaddr$prim50717 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim50717, align 8
%args49743$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50718 = alloca %struct.ScmObj*, align 8
%args49743$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args49743$k$0)
store volatile %struct.ScmObj* %args49743$k$1, %struct.ScmObj** %stackaddr$prim50718, align 8
%clofunc50719 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc50719(%struct.ScmObj* %k, %struct.ScmObj* %args49743$k$1)
ret void
}

define tailcc void @proc_clo$ae44083(%struct.ScmObj* %env$ae44083,%struct.ScmObj* %current_45args49745) {
%stackaddr$env-ref50720 = alloca %struct.ScmObj*, align 8
%_37append40199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44083, i64 0)
store %struct.ScmObj* %_37append40199, %struct.ScmObj** %stackaddr$env-ref50720
%stackaddr$prim50721 = alloca %struct.ScmObj*, align 8
%k40455 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49745)
store volatile %struct.ScmObj* %k40455, %struct.ScmObj** %stackaddr$prim50721, align 8
%stackaddr$prim50722 = alloca %struct.ScmObj*, align 8
%current_45args49746 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49745)
store volatile %struct.ScmObj* %current_45args49746, %struct.ScmObj** %stackaddr$prim50722, align 8
%stackaddr$prim50723 = alloca %struct.ScmObj*, align 8
%n40229 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49746)
store volatile %struct.ScmObj* %n40229, %struct.ScmObj** %stackaddr$prim50723, align 8
%stackaddr$prim50724 = alloca %struct.ScmObj*, align 8
%anf_45bind40367 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40367, %struct.ScmObj** %stackaddr$prim50724, align 8
%ae44084 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50725 = alloca %struct.ScmObj*, align 8
%one_45to40232 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44084, %struct.ScmObj* %anf_45bind40367)
store volatile %struct.ScmObj* %one_45to40232, %struct.ScmObj** %stackaddr$prim50725, align 8
%stackaddr$prim50726 = alloca %struct.ScmObj*, align 8
%anf_45bind40368 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40368, %struct.ScmObj** %stackaddr$prim50726, align 8
%ae44086 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50727 = alloca %struct.ScmObj*, align 8
%my_45try40231 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44086, %struct.ScmObj* %anf_45bind40368)
store volatile %struct.ScmObj* %my_45try40231, %struct.ScmObj** %stackaddr$prim50727, align 8
%stackaddr$prim50728 = alloca %struct.ScmObj*, align 8
%anf_45bind40369 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40369, %struct.ScmObj** %stackaddr$prim50728, align 8
%ae44088 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50729 = alloca %struct.ScmObj*, align 8
%ok_6340230 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44088, %struct.ScmObj* %anf_45bind40369)
store volatile %struct.ScmObj* %ok_6340230, %struct.ScmObj** %stackaddr$prim50729, align 8
%stackaddr$makeclosure50730 = alloca %struct.ScmObj*, align 8
%fptrToInt50731 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44090 to i64
%ae44090 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt50731)
store volatile %struct.ScmObj* %ae44090, %struct.ScmObj** %stackaddr$makeclosure50730, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44090, %struct.ScmObj* %k40455, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44090, %struct.ScmObj* %_37append40199, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44090, %struct.ScmObj* %my_45try40231, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44090, %struct.ScmObj* %one_45to40232, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44090, %struct.ScmObj* %ok_6340230, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44090, %struct.ScmObj* %n40229, i64 5)
%ae44091 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50732 = alloca %struct.ScmObj*, align 8
%fptrToInt50733 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44092 to i64
%ae44092 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50733)
store volatile %struct.ScmObj* %ae44092, %struct.ScmObj** %stackaddr$makeclosure50732, align 8
%args49856$ae44090$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50734 = alloca %struct.ScmObj*, align 8
%args49856$ae44090$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44092, %struct.ScmObj* %args49856$ae44090$0)
store volatile %struct.ScmObj* %args49856$ae44090$1, %struct.ScmObj** %stackaddr$prim50734, align 8
%stackaddr$prim50735 = alloca %struct.ScmObj*, align 8
%args49856$ae44090$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44091, %struct.ScmObj* %args49856$ae44090$1)
store volatile %struct.ScmObj* %args49856$ae44090$2, %struct.ScmObj** %stackaddr$prim50735, align 8
%clofunc50736 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44090)
musttail call tailcc void %clofunc50736(%struct.ScmObj* %ae44090, %struct.ScmObj* %args49856$ae44090$2)
ret void
}

define tailcc void @proc_clo$ae44090(%struct.ScmObj* %env$ae44090,%struct.ScmObj* %current_45args49748) {
%stackaddr$env-ref50737 = alloca %struct.ScmObj*, align 8
%k40455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44090, i64 0)
store %struct.ScmObj* %k40455, %struct.ScmObj** %stackaddr$env-ref50737
%stackaddr$env-ref50738 = alloca %struct.ScmObj*, align 8
%_37append40199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44090, i64 1)
store %struct.ScmObj* %_37append40199, %struct.ScmObj** %stackaddr$env-ref50738
%stackaddr$env-ref50739 = alloca %struct.ScmObj*, align 8
%my_45try40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44090, i64 2)
store %struct.ScmObj* %my_45try40231, %struct.ScmObj** %stackaddr$env-ref50739
%stackaddr$env-ref50740 = alloca %struct.ScmObj*, align 8
%one_45to40232 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44090, i64 3)
store %struct.ScmObj* %one_45to40232, %struct.ScmObj** %stackaddr$env-ref50740
%stackaddr$env-ref50741 = alloca %struct.ScmObj*, align 8
%ok_6340230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44090, i64 4)
store %struct.ScmObj* %ok_6340230, %struct.ScmObj** %stackaddr$env-ref50741
%stackaddr$env-ref50742 = alloca %struct.ScmObj*, align 8
%n40229 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44090, i64 5)
store %struct.ScmObj* %n40229, %struct.ScmObj** %stackaddr$env-ref50742
%stackaddr$prim50743 = alloca %struct.ScmObj*, align 8
%_95k40456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49748)
store volatile %struct.ScmObj* %_95k40456, %struct.ScmObj** %stackaddr$prim50743, align 8
%stackaddr$prim50744 = alloca %struct.ScmObj*, align 8
%current_45args49749 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49748)
store volatile %struct.ScmObj* %current_45args49749, %struct.ScmObj** %stackaddr$prim50744, align 8
%stackaddr$prim50745 = alloca %struct.ScmObj*, align 8
%anf_45bind40379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49749)
store volatile %struct.ScmObj* %anf_45bind40379, %struct.ScmObj** %stackaddr$prim50745, align 8
%ae44357 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50746 = alloca %struct.ScmObj*, align 8
%t4010540242 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %one_45to40232, %struct.ScmObj* %ae44357, %struct.ScmObj* %anf_45bind40379)
store volatile %struct.ScmObj* %t4010540242, %struct.ScmObj** %stackaddr$prim50746, align 8
%stackaddr$makeclosure50747 = alloca %struct.ScmObj*, align 8
%fptrToInt50748 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44359 to i64
%ae44359 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt50748)
store volatile %struct.ScmObj* %ae44359, %struct.ScmObj** %stackaddr$makeclosure50747, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44359, %struct.ScmObj* %my_45try40231, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44359, %struct.ScmObj* %k40455, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44359, %struct.ScmObj* %one_45to40232, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44359, %struct.ScmObj* %ok_6340230, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44359, %struct.ScmObj* %n40229, i64 4)
%ae44360 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50749 = alloca %struct.ScmObj*, align 8
%fptrToInt50750 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44361 to i64
%ae44361 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50750)
store volatile %struct.ScmObj* %ae44361, %struct.ScmObj** %stackaddr$makeclosure50749, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44361, %struct.ScmObj* %my_45try40231, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44361, %struct.ScmObj* %_37append40199, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44361, %struct.ScmObj* %ok_6340230, i64 2)
%args49832$ae44359$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50751 = alloca %struct.ScmObj*, align 8
%args49832$ae44359$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44361, %struct.ScmObj* %args49832$ae44359$0)
store volatile %struct.ScmObj* %args49832$ae44359$1, %struct.ScmObj** %stackaddr$prim50751, align 8
%stackaddr$prim50752 = alloca %struct.ScmObj*, align 8
%args49832$ae44359$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44360, %struct.ScmObj* %args49832$ae44359$1)
store volatile %struct.ScmObj* %args49832$ae44359$2, %struct.ScmObj** %stackaddr$prim50752, align 8
%clofunc50753 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44359)
musttail call tailcc void %clofunc50753(%struct.ScmObj* %ae44359, %struct.ScmObj* %args49832$ae44359$2)
ret void
}

define tailcc void @proc_clo$ae44359(%struct.ScmObj* %env$ae44359,%struct.ScmObj* %current_45args49751) {
%stackaddr$env-ref50754 = alloca %struct.ScmObj*, align 8
%my_45try40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44359, i64 0)
store %struct.ScmObj* %my_45try40231, %struct.ScmObj** %stackaddr$env-ref50754
%stackaddr$env-ref50755 = alloca %struct.ScmObj*, align 8
%k40455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44359, i64 1)
store %struct.ScmObj* %k40455, %struct.ScmObj** %stackaddr$env-ref50755
%stackaddr$env-ref50756 = alloca %struct.ScmObj*, align 8
%one_45to40232 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44359, i64 2)
store %struct.ScmObj* %one_45to40232, %struct.ScmObj** %stackaddr$env-ref50756
%stackaddr$env-ref50757 = alloca %struct.ScmObj*, align 8
%ok_6340230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44359, i64 3)
store %struct.ScmObj* %ok_6340230, %struct.ScmObj** %stackaddr$env-ref50757
%stackaddr$env-ref50758 = alloca %struct.ScmObj*, align 8
%n40229 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44359, i64 4)
store %struct.ScmObj* %n40229, %struct.ScmObj** %stackaddr$env-ref50758
%stackaddr$prim50759 = alloca %struct.ScmObj*, align 8
%_95k40457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49751)
store volatile %struct.ScmObj* %_95k40457, %struct.ScmObj** %stackaddr$prim50759, align 8
%stackaddr$prim50760 = alloca %struct.ScmObj*, align 8
%current_45args49752 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49751)
store volatile %struct.ScmObj* %current_45args49752, %struct.ScmObj** %stackaddr$prim50760, align 8
%stackaddr$prim50761 = alloca %struct.ScmObj*, align 8
%anf_45bind40398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49752)
store volatile %struct.ScmObj* %anf_45bind40398, %struct.ScmObj** %stackaddr$prim50761, align 8
%ae44749 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50762 = alloca %struct.ScmObj*, align 8
%t4010440237 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %my_45try40231, %struct.ScmObj* %ae44749, %struct.ScmObj* %anf_45bind40398)
store volatile %struct.ScmObj* %t4010440237, %struct.ScmObj** %stackaddr$prim50762, align 8
%stackaddr$makeclosure50763 = alloca %struct.ScmObj*, align 8
%fptrToInt50764 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44751 to i64
%ae44751 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt50764)
store volatile %struct.ScmObj* %ae44751, %struct.ScmObj** %stackaddr$makeclosure50763, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44751, %struct.ScmObj* %my_45try40231, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44751, %struct.ScmObj* %k40455, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44751, %struct.ScmObj* %one_45to40232, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44751, %struct.ScmObj* %ok_6340230, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44751, %struct.ScmObj* %n40229, i64 4)
%ae44752 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50765 = alloca %struct.ScmObj*, align 8
%fptrToInt50766 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44753 to i64
%ae44753 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50766)
store volatile %struct.ScmObj* %ae44753, %struct.ScmObj** %stackaddr$makeclosure50765, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44753, %struct.ScmObj* %ok_6340230, i64 0)
%args49789$ae44751$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50767 = alloca %struct.ScmObj*, align 8
%args49789$ae44751$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44753, %struct.ScmObj* %args49789$ae44751$0)
store volatile %struct.ScmObj* %args49789$ae44751$1, %struct.ScmObj** %stackaddr$prim50767, align 8
%stackaddr$prim50768 = alloca %struct.ScmObj*, align 8
%args49789$ae44751$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44752, %struct.ScmObj* %args49789$ae44751$1)
store volatile %struct.ScmObj* %args49789$ae44751$2, %struct.ScmObj** %stackaddr$prim50768, align 8
%clofunc50769 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44751)
musttail call tailcc void %clofunc50769(%struct.ScmObj* %ae44751, %struct.ScmObj* %args49789$ae44751$2)
ret void
}

define tailcc void @proc_clo$ae44751(%struct.ScmObj* %env$ae44751,%struct.ScmObj* %current_45args49754) {
%stackaddr$env-ref50770 = alloca %struct.ScmObj*, align 8
%my_45try40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44751, i64 0)
store %struct.ScmObj* %my_45try40231, %struct.ScmObj** %stackaddr$env-ref50770
%stackaddr$env-ref50771 = alloca %struct.ScmObj*, align 8
%k40455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44751, i64 1)
store %struct.ScmObj* %k40455, %struct.ScmObj** %stackaddr$env-ref50771
%stackaddr$env-ref50772 = alloca %struct.ScmObj*, align 8
%one_45to40232 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44751, i64 2)
store %struct.ScmObj* %one_45to40232, %struct.ScmObj** %stackaddr$env-ref50772
%stackaddr$env-ref50773 = alloca %struct.ScmObj*, align 8
%ok_6340230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44751, i64 3)
store %struct.ScmObj* %ok_6340230, %struct.ScmObj** %stackaddr$env-ref50773
%stackaddr$env-ref50774 = alloca %struct.ScmObj*, align 8
%n40229 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44751, i64 4)
store %struct.ScmObj* %n40229, %struct.ScmObj** %stackaddr$env-ref50774
%stackaddr$prim50775 = alloca %struct.ScmObj*, align 8
%_95k40458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49754)
store volatile %struct.ScmObj* %_95k40458, %struct.ScmObj** %stackaddr$prim50775, align 8
%stackaddr$prim50776 = alloca %struct.ScmObj*, align 8
%current_45args49755 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49754)
store volatile %struct.ScmObj* %current_45args49755, %struct.ScmObj** %stackaddr$prim50776, align 8
%stackaddr$prim50777 = alloca %struct.ScmObj*, align 8
%anf_45bind40411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49755)
store volatile %struct.ScmObj* %anf_45bind40411, %struct.ScmObj** %stackaddr$prim50777, align 8
%ae44867 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50778 = alloca %struct.ScmObj*, align 8
%t4010340233 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %ok_6340230, %struct.ScmObj* %ae44867, %struct.ScmObj* %anf_45bind40411)
store volatile %struct.ScmObj* %t4010340233, %struct.ScmObj** %stackaddr$prim50778, align 8
%ae44870 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50779 = alloca %struct.ScmObj*, align 8
%anf_45bind40412 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %my_45try40231, %struct.ScmObj* %ae44870)
store volatile %struct.ScmObj* %anf_45bind40412, %struct.ScmObj** %stackaddr$prim50779, align 8
%ae44872 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50780 = alloca %struct.ScmObj*, align 8
%anf_45bind40413 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %one_45to40232, %struct.ScmObj* %ae44872)
store volatile %struct.ScmObj* %anf_45bind40413, %struct.ScmObj** %stackaddr$prim50780, align 8
%stackaddr$makeclosure50781 = alloca %struct.ScmObj*, align 8
%fptrToInt50782 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44874 to i64
%ae44874 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50782)
store volatile %struct.ScmObj* %ae44874, %struct.ScmObj** %stackaddr$makeclosure50781, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44874, %struct.ScmObj* %k40455, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44874, %struct.ScmObj* %anf_45bind40412, i64 1)
%args49779$anf_45bind40413$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50783 = alloca %struct.ScmObj*, align 8
%args49779$anf_45bind40413$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %n40229, %struct.ScmObj* %args49779$anf_45bind40413$0)
store volatile %struct.ScmObj* %args49779$anf_45bind40413$1, %struct.ScmObj** %stackaddr$prim50783, align 8
%stackaddr$prim50784 = alloca %struct.ScmObj*, align 8
%args49779$anf_45bind40413$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44874, %struct.ScmObj* %args49779$anf_45bind40413$1)
store volatile %struct.ScmObj* %args49779$anf_45bind40413$2, %struct.ScmObj** %stackaddr$prim50784, align 8
%clofunc50785 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40413)
musttail call tailcc void %clofunc50785(%struct.ScmObj* %anf_45bind40413, %struct.ScmObj* %args49779$anf_45bind40413$2)
ret void
}

define tailcc void @proc_clo$ae44874(%struct.ScmObj* %env$ae44874,%struct.ScmObj* %current_45args49757) {
%stackaddr$env-ref50786 = alloca %struct.ScmObj*, align 8
%k40455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44874, i64 0)
store %struct.ScmObj* %k40455, %struct.ScmObj** %stackaddr$env-ref50786
%stackaddr$env-ref50787 = alloca %struct.ScmObj*, align 8
%anf_45bind40412 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44874, i64 1)
store %struct.ScmObj* %anf_45bind40412, %struct.ScmObj** %stackaddr$env-ref50787
%stackaddr$prim50788 = alloca %struct.ScmObj*, align 8
%_95k40459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49757)
store volatile %struct.ScmObj* %_95k40459, %struct.ScmObj** %stackaddr$prim50788, align 8
%stackaddr$prim50789 = alloca %struct.ScmObj*, align 8
%current_45args49758 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49757)
store volatile %struct.ScmObj* %current_45args49758, %struct.ScmObj** %stackaddr$prim50789, align 8
%stackaddr$prim50790 = alloca %struct.ScmObj*, align 8
%anf_45bind40414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49758)
store volatile %struct.ScmObj* %anf_45bind40414, %struct.ScmObj** %stackaddr$prim50790, align 8
%stackaddr$makeclosure50791 = alloca %struct.ScmObj*, align 8
%fptrToInt50792 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44876 to i64
%ae44876 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50792)
store volatile %struct.ScmObj* %ae44876, %struct.ScmObj** %stackaddr$makeclosure50791, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44876, %struct.ScmObj* %k40455, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44876, %struct.ScmObj* %anf_45bind40414, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44876, %struct.ScmObj* %anf_45bind40412, i64 2)
%ae44877 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50793 = alloca %struct.ScmObj*, align 8
%fptrToInt50794 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44878 to i64
%ae44878 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50794)
store volatile %struct.ScmObj* %ae44878, %struct.ScmObj** %stackaddr$makeclosure50793, align 8
%args49778$ae44876$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50795 = alloca %struct.ScmObj*, align 8
%args49778$ae44876$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44878, %struct.ScmObj* %args49778$ae44876$0)
store volatile %struct.ScmObj* %args49778$ae44876$1, %struct.ScmObj** %stackaddr$prim50795, align 8
%stackaddr$prim50796 = alloca %struct.ScmObj*, align 8
%args49778$ae44876$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44877, %struct.ScmObj* %args49778$ae44876$1)
store volatile %struct.ScmObj* %args49778$ae44876$2, %struct.ScmObj** %stackaddr$prim50796, align 8
%clofunc50797 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44876)
musttail call tailcc void %clofunc50797(%struct.ScmObj* %ae44876, %struct.ScmObj* %args49778$ae44876$2)
ret void
}

define tailcc void @proc_clo$ae44876(%struct.ScmObj* %env$ae44876,%struct.ScmObj* %current_45args49760) {
%stackaddr$env-ref50798 = alloca %struct.ScmObj*, align 8
%k40455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44876, i64 0)
store %struct.ScmObj* %k40455, %struct.ScmObj** %stackaddr$env-ref50798
%stackaddr$env-ref50799 = alloca %struct.ScmObj*, align 8
%anf_45bind40414 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44876, i64 1)
store %struct.ScmObj* %anf_45bind40414, %struct.ScmObj** %stackaddr$env-ref50799
%stackaddr$env-ref50800 = alloca %struct.ScmObj*, align 8
%anf_45bind40412 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44876, i64 2)
store %struct.ScmObj* %anf_45bind40412, %struct.ScmObj** %stackaddr$env-ref50800
%stackaddr$prim50801 = alloca %struct.ScmObj*, align 8
%_95k40460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49760)
store volatile %struct.ScmObj* %_95k40460, %struct.ScmObj** %stackaddr$prim50801, align 8
%stackaddr$prim50802 = alloca %struct.ScmObj*, align 8
%current_45args49761 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49760)
store volatile %struct.ScmObj* %current_45args49761, %struct.ScmObj** %stackaddr$prim50802, align 8
%stackaddr$prim50803 = alloca %struct.ScmObj*, align 8
%anf_45bind40415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49761)
store volatile %struct.ScmObj* %anf_45bind40415, %struct.ScmObj** %stackaddr$prim50803, align 8
%stackaddr$makeclosure50804 = alloca %struct.ScmObj*, align 8
%fptrToInt50805 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44899 to i64
%ae44899 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50805)
store volatile %struct.ScmObj* %ae44899, %struct.ScmObj** %stackaddr$makeclosure50804, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44899, %struct.ScmObj* %k40455, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44899, %struct.ScmObj* %anf_45bind40414, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44899, %struct.ScmObj* %anf_45bind40412, i64 2)
%args49776$anf_45bind40415$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50806 = alloca %struct.ScmObj*, align 8
%args49776$anf_45bind40415$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44899, %struct.ScmObj* %args49776$anf_45bind40415$0)
store volatile %struct.ScmObj* %args49776$anf_45bind40415$1, %struct.ScmObj** %stackaddr$prim50806, align 8
%clofunc50807 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40415)
musttail call tailcc void %clofunc50807(%struct.ScmObj* %anf_45bind40415, %struct.ScmObj* %args49776$anf_45bind40415$1)
ret void
}

define tailcc void @proc_clo$ae44899(%struct.ScmObj* %env$ae44899,%struct.ScmObj* %current_45args49763) {
%stackaddr$env-ref50808 = alloca %struct.ScmObj*, align 8
%k40455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44899, i64 0)
store %struct.ScmObj* %k40455, %struct.ScmObj** %stackaddr$env-ref50808
%stackaddr$env-ref50809 = alloca %struct.ScmObj*, align 8
%anf_45bind40414 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44899, i64 1)
store %struct.ScmObj* %anf_45bind40414, %struct.ScmObj** %stackaddr$env-ref50809
%stackaddr$env-ref50810 = alloca %struct.ScmObj*, align 8
%anf_45bind40412 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44899, i64 2)
store %struct.ScmObj* %anf_45bind40412, %struct.ScmObj** %stackaddr$env-ref50810
%stackaddr$prim50811 = alloca %struct.ScmObj*, align 8
%_95k40461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49763)
store volatile %struct.ScmObj* %_95k40461, %struct.ScmObj** %stackaddr$prim50811, align 8
%stackaddr$prim50812 = alloca %struct.ScmObj*, align 8
%current_45args49764 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49763)
store volatile %struct.ScmObj* %current_45args49764, %struct.ScmObj** %stackaddr$prim50812, align 8
%stackaddr$prim50813 = alloca %struct.ScmObj*, align 8
%anf_45bind40416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49764)
store volatile %struct.ScmObj* %anf_45bind40416, %struct.ScmObj** %stackaddr$prim50813, align 8
%stackaddr$makeclosure50814 = alloca %struct.ScmObj*, align 8
%fptrToInt50815 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44900 to i64
%ae44900 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50815)
store volatile %struct.ScmObj* %ae44900, %struct.ScmObj** %stackaddr$makeclosure50814, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44900, %struct.ScmObj* %k40455, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44900, %struct.ScmObj* %anf_45bind40416, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44900, %struct.ScmObj* %anf_45bind40414, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44900, %struct.ScmObj* %anf_45bind40412, i64 3)
%ae44901 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50816 = alloca %struct.ScmObj*, align 8
%fptrToInt50817 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44902 to i64
%ae44902 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50817)
store volatile %struct.ScmObj* %ae44902, %struct.ScmObj** %stackaddr$makeclosure50816, align 8
%args49775$ae44900$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50818 = alloca %struct.ScmObj*, align 8
%args49775$ae44900$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44902, %struct.ScmObj* %args49775$ae44900$0)
store volatile %struct.ScmObj* %args49775$ae44900$1, %struct.ScmObj** %stackaddr$prim50818, align 8
%stackaddr$prim50819 = alloca %struct.ScmObj*, align 8
%args49775$ae44900$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44901, %struct.ScmObj* %args49775$ae44900$1)
store volatile %struct.ScmObj* %args49775$ae44900$2, %struct.ScmObj** %stackaddr$prim50819, align 8
%clofunc50820 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44900)
musttail call tailcc void %clofunc50820(%struct.ScmObj* %ae44900, %struct.ScmObj* %args49775$ae44900$2)
ret void
}

define tailcc void @proc_clo$ae44900(%struct.ScmObj* %env$ae44900,%struct.ScmObj* %current_45args49766) {
%stackaddr$env-ref50821 = alloca %struct.ScmObj*, align 8
%k40455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44900, i64 0)
store %struct.ScmObj* %k40455, %struct.ScmObj** %stackaddr$env-ref50821
%stackaddr$env-ref50822 = alloca %struct.ScmObj*, align 8
%anf_45bind40416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44900, i64 1)
store %struct.ScmObj* %anf_45bind40416, %struct.ScmObj** %stackaddr$env-ref50822
%stackaddr$env-ref50823 = alloca %struct.ScmObj*, align 8
%anf_45bind40414 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44900, i64 2)
store %struct.ScmObj* %anf_45bind40414, %struct.ScmObj** %stackaddr$env-ref50823
%stackaddr$env-ref50824 = alloca %struct.ScmObj*, align 8
%anf_45bind40412 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44900, i64 3)
store %struct.ScmObj* %anf_45bind40412, %struct.ScmObj** %stackaddr$env-ref50824
%stackaddr$prim50825 = alloca %struct.ScmObj*, align 8
%_95k40462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49766)
store volatile %struct.ScmObj* %_95k40462, %struct.ScmObj** %stackaddr$prim50825, align 8
%stackaddr$prim50826 = alloca %struct.ScmObj*, align 8
%current_45args49767 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49766)
store volatile %struct.ScmObj* %current_45args49767, %struct.ScmObj** %stackaddr$prim50826, align 8
%stackaddr$prim50827 = alloca %struct.ScmObj*, align 8
%anf_45bind40417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49767)
store volatile %struct.ScmObj* %anf_45bind40417, %struct.ScmObj** %stackaddr$prim50827, align 8
%stackaddr$makeclosure50828 = alloca %struct.ScmObj*, align 8
%fptrToInt50829 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44923 to i64
%ae44923 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50829)
store volatile %struct.ScmObj* %ae44923, %struct.ScmObj** %stackaddr$makeclosure50828, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44923, %struct.ScmObj* %k40455, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44923, %struct.ScmObj* %anf_45bind40416, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44923, %struct.ScmObj* %anf_45bind40414, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44923, %struct.ScmObj* %anf_45bind40412, i64 3)
%args49773$anf_45bind40417$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50830 = alloca %struct.ScmObj*, align 8
%args49773$anf_45bind40417$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44923, %struct.ScmObj* %args49773$anf_45bind40417$0)
store volatile %struct.ScmObj* %args49773$anf_45bind40417$1, %struct.ScmObj** %stackaddr$prim50830, align 8
%clofunc50831 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40417)
musttail call tailcc void %clofunc50831(%struct.ScmObj* %anf_45bind40417, %struct.ScmObj* %args49773$anf_45bind40417$1)
ret void
}

define tailcc void @proc_clo$ae44923(%struct.ScmObj* %env$ae44923,%struct.ScmObj* %current_45args49769) {
%stackaddr$env-ref50832 = alloca %struct.ScmObj*, align 8
%k40455 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44923, i64 0)
store %struct.ScmObj* %k40455, %struct.ScmObj** %stackaddr$env-ref50832
%stackaddr$env-ref50833 = alloca %struct.ScmObj*, align 8
%anf_45bind40416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44923, i64 1)
store %struct.ScmObj* %anf_45bind40416, %struct.ScmObj** %stackaddr$env-ref50833
%stackaddr$env-ref50834 = alloca %struct.ScmObj*, align 8
%anf_45bind40414 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44923, i64 2)
store %struct.ScmObj* %anf_45bind40414, %struct.ScmObj** %stackaddr$env-ref50834
%stackaddr$env-ref50835 = alloca %struct.ScmObj*, align 8
%anf_45bind40412 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44923, i64 3)
store %struct.ScmObj* %anf_45bind40412, %struct.ScmObj** %stackaddr$env-ref50835
%stackaddr$prim50836 = alloca %struct.ScmObj*, align 8
%_95k40463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49769)
store volatile %struct.ScmObj* %_95k40463, %struct.ScmObj** %stackaddr$prim50836, align 8
%stackaddr$prim50837 = alloca %struct.ScmObj*, align 8
%current_45args49770 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49769)
store volatile %struct.ScmObj* %current_45args49770, %struct.ScmObj** %stackaddr$prim50837, align 8
%stackaddr$prim50838 = alloca %struct.ScmObj*, align 8
%anf_45bind40418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49770)
store volatile %struct.ScmObj* %anf_45bind40418, %struct.ScmObj** %stackaddr$prim50838, align 8
%args49772$anf_45bind40412$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50839 = alloca %struct.ScmObj*, align 8
%args49772$anf_45bind40412$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40418, %struct.ScmObj* %args49772$anf_45bind40412$0)
store volatile %struct.ScmObj* %args49772$anf_45bind40412$1, %struct.ScmObj** %stackaddr$prim50839, align 8
%stackaddr$prim50840 = alloca %struct.ScmObj*, align 8
%args49772$anf_45bind40412$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40416, %struct.ScmObj* %args49772$anf_45bind40412$1)
store volatile %struct.ScmObj* %args49772$anf_45bind40412$2, %struct.ScmObj** %stackaddr$prim50840, align 8
%stackaddr$prim50841 = alloca %struct.ScmObj*, align 8
%args49772$anf_45bind40412$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40414, %struct.ScmObj* %args49772$anf_45bind40412$2)
store volatile %struct.ScmObj* %args49772$anf_45bind40412$3, %struct.ScmObj** %stackaddr$prim50841, align 8
%stackaddr$prim50842 = alloca %struct.ScmObj*, align 8
%args49772$anf_45bind40412$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40455, %struct.ScmObj* %args49772$anf_45bind40412$3)
store volatile %struct.ScmObj* %args49772$anf_45bind40412$4, %struct.ScmObj** %stackaddr$prim50842, align 8
%clofunc50843 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40412)
musttail call tailcc void %clofunc50843(%struct.ScmObj* %anf_45bind40412, %struct.ScmObj* %args49772$anf_45bind40412$4)
ret void
}

define tailcc void @proc_clo$ae44902(%struct.ScmObj* %env$ae44902,%struct.ScmObj* %lst4025040464) {
%stackaddr$prim50844 = alloca %struct.ScmObj*, align 8
%k40465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4025040464)
store volatile %struct.ScmObj* %k40465, %struct.ScmObj** %stackaddr$prim50844, align 8
%stackaddr$prim50845 = alloca %struct.ScmObj*, align 8
%lst40250 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4025040464)
store volatile %struct.ScmObj* %lst40250, %struct.ScmObj** %stackaddr$prim50845, align 8
%ae44906 = call %struct.ScmObj* @const_init_int(i64 0)
%args49774$k40465$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50846 = alloca %struct.ScmObj*, align 8
%args49774$k40465$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40250, %struct.ScmObj* %args49774$k40465$0)
store volatile %struct.ScmObj* %args49774$k40465$1, %struct.ScmObj** %stackaddr$prim50846, align 8
%stackaddr$prim50847 = alloca %struct.ScmObj*, align 8
%args49774$k40465$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44906, %struct.ScmObj* %args49774$k40465$1)
store volatile %struct.ScmObj* %args49774$k40465$2, %struct.ScmObj** %stackaddr$prim50847, align 8
%clofunc50848 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40465)
musttail call tailcc void %clofunc50848(%struct.ScmObj* %k40465, %struct.ScmObj* %args49774$k40465$2)
ret void
}

define tailcc void @proc_clo$ae44878(%struct.ScmObj* %env$ae44878,%struct.ScmObj* %lst4024940466) {
%stackaddr$prim50849 = alloca %struct.ScmObj*, align 8
%k40467 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4024940466)
store volatile %struct.ScmObj* %k40467, %struct.ScmObj** %stackaddr$prim50849, align 8
%stackaddr$prim50850 = alloca %struct.ScmObj*, align 8
%lst40249 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4024940466)
store volatile %struct.ScmObj* %lst40249, %struct.ScmObj** %stackaddr$prim50850, align 8
%ae44882 = call %struct.ScmObj* @const_init_int(i64 0)
%args49777$k40467$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50851 = alloca %struct.ScmObj*, align 8
%args49777$k40467$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40249, %struct.ScmObj* %args49777$k40467$0)
store volatile %struct.ScmObj* %args49777$k40467$1, %struct.ScmObj** %stackaddr$prim50851, align 8
%stackaddr$prim50852 = alloca %struct.ScmObj*, align 8
%args49777$k40467$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44882, %struct.ScmObj* %args49777$k40467$1)
store volatile %struct.ScmObj* %args49777$k40467$2, %struct.ScmObj** %stackaddr$prim50852, align 8
%clofunc50853 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40467)
musttail call tailcc void %clofunc50853(%struct.ScmObj* %k40467, %struct.ScmObj* %args49777$k40467$2)
ret void
}

define tailcc void @proc_clo$ae44753(%struct.ScmObj* %env$ae44753,%struct.ScmObj* %current_45args49780) {
%stackaddr$env-ref50854 = alloca %struct.ScmObj*, align 8
%ok_6340230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44753, i64 0)
store %struct.ScmObj* %ok_6340230, %struct.ScmObj** %stackaddr$env-ref50854
%stackaddr$prim50855 = alloca %struct.ScmObj*, align 8
%k40468 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49780)
store volatile %struct.ScmObj* %k40468, %struct.ScmObj** %stackaddr$prim50855, align 8
%stackaddr$prim50856 = alloca %struct.ScmObj*, align 8
%current_45args49781 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49780)
store volatile %struct.ScmObj* %current_45args49781, %struct.ScmObj** %stackaddr$prim50856, align 8
%stackaddr$prim50857 = alloca %struct.ScmObj*, align 8
%row40236 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49781)
store volatile %struct.ScmObj* %row40236, %struct.ScmObj** %stackaddr$prim50857, align 8
%stackaddr$prim50858 = alloca %struct.ScmObj*, align 8
%current_45args49782 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49781)
store volatile %struct.ScmObj* %current_45args49782, %struct.ScmObj** %stackaddr$prim50858, align 8
%stackaddr$prim50859 = alloca %struct.ScmObj*, align 8
%dist40235 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49782)
store volatile %struct.ScmObj* %dist40235, %struct.ScmObj** %stackaddr$prim50859, align 8
%stackaddr$prim50860 = alloca %struct.ScmObj*, align 8
%current_45args49783 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49782)
store volatile %struct.ScmObj* %current_45args49783, %struct.ScmObj** %stackaddr$prim50860, align 8
%stackaddr$prim50861 = alloca %struct.ScmObj*, align 8
%placed40234 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49783)
store volatile %struct.ScmObj* %placed40234, %struct.ScmObj** %stackaddr$prim50861, align 8
%stackaddr$prim50862 = alloca %struct.ScmObj*, align 8
%anf_45bind40399 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %placed40234)
store volatile %struct.ScmObj* %anf_45bind40399, %struct.ScmObj** %stackaddr$prim50862, align 8
%truthy$cmp50863 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40399)
%cmp$cmp50863 = icmp eq i64 %truthy$cmp50863, 1
br i1 %cmp$cmp50863, label %truebranch$cmp50863, label %falsebranch$cmp50863
truebranch$cmp50863:
%ae44757 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44758 = call %struct.ScmObj* @const_init_true()
%args49785$k40468$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50864 = alloca %struct.ScmObj*, align 8
%args49785$k40468$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44758, %struct.ScmObj* %args49785$k40468$0)
store volatile %struct.ScmObj* %args49785$k40468$1, %struct.ScmObj** %stackaddr$prim50864, align 8
%stackaddr$prim50865 = alloca %struct.ScmObj*, align 8
%args49785$k40468$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44757, %struct.ScmObj* %args49785$k40468$1)
store volatile %struct.ScmObj* %args49785$k40468$2, %struct.ScmObj** %stackaddr$prim50865, align 8
%clofunc50866 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40468)
musttail call tailcc void %clofunc50866(%struct.ScmObj* %k40468, %struct.ScmObj* %args49785$k40468$2)
ret void
falsebranch$cmp50863:
%stackaddr$prim50867 = alloca %struct.ScmObj*, align 8
%anf_45bind40400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %placed40234)
store volatile %struct.ScmObj* %anf_45bind40400, %struct.ScmObj** %stackaddr$prim50867, align 8
%stackaddr$prim50868 = alloca %struct.ScmObj*, align 8
%anf_45bind40401 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %row40236, %struct.ScmObj* %dist40235)
store volatile %struct.ScmObj* %anf_45bind40401, %struct.ScmObj** %stackaddr$prim50868, align 8
%stackaddr$prim50869 = alloca %struct.ScmObj*, align 8
%anf_45bind40402 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %anf_45bind40400, %struct.ScmObj* %anf_45bind40401)
store volatile %struct.ScmObj* %anf_45bind40402, %struct.ScmObj** %stackaddr$prim50869, align 8
%stackaddr$prim50870 = alloca %struct.ScmObj*, align 8
%anf_45bind40403 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40402)
store volatile %struct.ScmObj* %anf_45bind40403, %struct.ScmObj** %stackaddr$prim50870, align 8
%truthy$cmp50871 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40403)
%cmp$cmp50871 = icmp eq i64 %truthy$cmp50871, 1
br i1 %cmp$cmp50871, label %truebranch$cmp50871, label %falsebranch$cmp50871
truebranch$cmp50871:
%stackaddr$prim50872 = alloca %struct.ScmObj*, align 8
%anf_45bind40404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %placed40234)
store volatile %struct.ScmObj* %anf_45bind40404, %struct.ScmObj** %stackaddr$prim50872, align 8
%stackaddr$prim50873 = alloca %struct.ScmObj*, align 8
%anf_45bind40405 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %row40236, %struct.ScmObj* %dist40235)
store volatile %struct.ScmObj* %anf_45bind40405, %struct.ScmObj** %stackaddr$prim50873, align 8
%stackaddr$prim50874 = alloca %struct.ScmObj*, align 8
%anf_45bind40406 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %anf_45bind40404, %struct.ScmObj* %anf_45bind40405)
store volatile %struct.ScmObj* %anf_45bind40406, %struct.ScmObj** %stackaddr$prim50874, align 8
%stackaddr$prim50875 = alloca %struct.ScmObj*, align 8
%anf_45bind40407 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40406)
store volatile %struct.ScmObj* %anf_45bind40407, %struct.ScmObj** %stackaddr$prim50875, align 8
%truthy$cmp50876 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40407)
%cmp$cmp50876 = icmp eq i64 %truthy$cmp50876, 1
br i1 %cmp$cmp50876, label %truebranch$cmp50876, label %falsebranch$cmp50876
truebranch$cmp50876:
%ae44780 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50877 = alloca %struct.ScmObj*, align 8
%anf_45bind40408 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %ok_6340230, %struct.ScmObj* %ae44780)
store volatile %struct.ScmObj* %anf_45bind40408, %struct.ScmObj** %stackaddr$prim50877, align 8
%ae44782 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50878 = alloca %struct.ScmObj*, align 8
%anf_45bind40409 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %dist40235, %struct.ScmObj* %ae44782)
store volatile %struct.ScmObj* %anf_45bind40409, %struct.ScmObj** %stackaddr$prim50878, align 8
%stackaddr$prim50879 = alloca %struct.ScmObj*, align 8
%anf_45bind40410 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %placed40234)
store volatile %struct.ScmObj* %anf_45bind40410, %struct.ScmObj** %stackaddr$prim50879, align 8
%args49786$anf_45bind40408$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50880 = alloca %struct.ScmObj*, align 8
%args49786$anf_45bind40408$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40410, %struct.ScmObj* %args49786$anf_45bind40408$0)
store volatile %struct.ScmObj* %args49786$anf_45bind40408$1, %struct.ScmObj** %stackaddr$prim50880, align 8
%stackaddr$prim50881 = alloca %struct.ScmObj*, align 8
%args49786$anf_45bind40408$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40409, %struct.ScmObj* %args49786$anf_45bind40408$1)
store volatile %struct.ScmObj* %args49786$anf_45bind40408$2, %struct.ScmObj** %stackaddr$prim50881, align 8
%stackaddr$prim50882 = alloca %struct.ScmObj*, align 8
%args49786$anf_45bind40408$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %row40236, %struct.ScmObj* %args49786$anf_45bind40408$2)
store volatile %struct.ScmObj* %args49786$anf_45bind40408$3, %struct.ScmObj** %stackaddr$prim50882, align 8
%stackaddr$prim50883 = alloca %struct.ScmObj*, align 8
%args49786$anf_45bind40408$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40468, %struct.ScmObj* %args49786$anf_45bind40408$3)
store volatile %struct.ScmObj* %args49786$anf_45bind40408$4, %struct.ScmObj** %stackaddr$prim50883, align 8
%clofunc50884 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40408)
musttail call tailcc void %clofunc50884(%struct.ScmObj* %anf_45bind40408, %struct.ScmObj* %args49786$anf_45bind40408$4)
ret void
falsebranch$cmp50876:
%ae44808 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44809 = call %struct.ScmObj* @const_init_false()
%args49787$k40468$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50885 = alloca %struct.ScmObj*, align 8
%args49787$k40468$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44809, %struct.ScmObj* %args49787$k40468$0)
store volatile %struct.ScmObj* %args49787$k40468$1, %struct.ScmObj** %stackaddr$prim50885, align 8
%stackaddr$prim50886 = alloca %struct.ScmObj*, align 8
%args49787$k40468$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44808, %struct.ScmObj* %args49787$k40468$1)
store volatile %struct.ScmObj* %args49787$k40468$2, %struct.ScmObj** %stackaddr$prim50886, align 8
%clofunc50887 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40468)
musttail call tailcc void %clofunc50887(%struct.ScmObj* %k40468, %struct.ScmObj* %args49787$k40468$2)
ret void
falsebranch$cmp50871:
%ae44817 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44818 = call %struct.ScmObj* @const_init_false()
%args49788$k40468$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50888 = alloca %struct.ScmObj*, align 8
%args49788$k40468$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44818, %struct.ScmObj* %args49788$k40468$0)
store volatile %struct.ScmObj* %args49788$k40468$1, %struct.ScmObj** %stackaddr$prim50888, align 8
%stackaddr$prim50889 = alloca %struct.ScmObj*, align 8
%args49788$k40468$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44817, %struct.ScmObj* %args49788$k40468$1)
store volatile %struct.ScmObj* %args49788$k40468$2, %struct.ScmObj** %stackaddr$prim50889, align 8
%clofunc50890 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40468)
musttail call tailcc void %clofunc50890(%struct.ScmObj* %k40468, %struct.ScmObj* %args49788$k40468$2)
ret void
}

define tailcc void @proc_clo$ae44361(%struct.ScmObj* %env$ae44361,%struct.ScmObj* %current_45args49790) {
%stackaddr$env-ref50891 = alloca %struct.ScmObj*, align 8
%my_45try40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44361, i64 0)
store %struct.ScmObj* %my_45try40231, %struct.ScmObj** %stackaddr$env-ref50891
%stackaddr$env-ref50892 = alloca %struct.ScmObj*, align 8
%_37append40199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44361, i64 1)
store %struct.ScmObj* %_37append40199, %struct.ScmObj** %stackaddr$env-ref50892
%stackaddr$env-ref50893 = alloca %struct.ScmObj*, align 8
%ok_6340230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44361, i64 2)
store %struct.ScmObj* %ok_6340230, %struct.ScmObj** %stackaddr$env-ref50893
%stackaddr$prim50894 = alloca %struct.ScmObj*, align 8
%k40469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49790)
store volatile %struct.ScmObj* %k40469, %struct.ScmObj** %stackaddr$prim50894, align 8
%stackaddr$prim50895 = alloca %struct.ScmObj*, align 8
%current_45args49791 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49790)
store volatile %struct.ScmObj* %current_45args49791, %struct.ScmObj** %stackaddr$prim50895, align 8
%stackaddr$prim50896 = alloca %struct.ScmObj*, align 8
%x40240 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49791)
store volatile %struct.ScmObj* %x40240, %struct.ScmObj** %stackaddr$prim50896, align 8
%stackaddr$prim50897 = alloca %struct.ScmObj*, align 8
%current_45args49792 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49791)
store volatile %struct.ScmObj* %current_45args49792, %struct.ScmObj** %stackaddr$prim50897, align 8
%stackaddr$prim50898 = alloca %struct.ScmObj*, align 8
%y40239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49792)
store volatile %struct.ScmObj* %y40239, %struct.ScmObj** %stackaddr$prim50898, align 8
%stackaddr$prim50899 = alloca %struct.ScmObj*, align 8
%current_45args49793 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49792)
store volatile %struct.ScmObj* %current_45args49793, %struct.ScmObj** %stackaddr$prim50899, align 8
%stackaddr$prim50900 = alloca %struct.ScmObj*, align 8
%z40238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49793)
store volatile %struct.ScmObj* %z40238, %struct.ScmObj** %stackaddr$prim50900, align 8
%stackaddr$prim50901 = alloca %struct.ScmObj*, align 8
%anf_45bind40380 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %x40240)
store volatile %struct.ScmObj* %anf_45bind40380, %struct.ScmObj** %stackaddr$prim50901, align 8
%truthy$cmp50902 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40380)
%cmp$cmp50902 = icmp eq i64 %truthy$cmp50902, 1
br i1 %cmp$cmp50902, label %truebranch$cmp50902, label %falsebranch$cmp50902
truebranch$cmp50902:
%stackaddr$prim50903 = alloca %struct.ScmObj*, align 8
%anf_45bind40381 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %y40239)
store volatile %struct.ScmObj* %anf_45bind40381, %struct.ScmObj** %stackaddr$prim50903, align 8
%truthy$cmp50904 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40381)
%cmp$cmp50904 = icmp eq i64 %truthy$cmp50904, 1
br i1 %cmp$cmp50904, label %truebranch$cmp50904, label %falsebranch$cmp50904
truebranch$cmp50904:
%ae44367 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44368 = call %struct.ScmObj* @const_init_int(i64 1)
%args49795$k40469$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50905 = alloca %struct.ScmObj*, align 8
%args49795$k40469$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44368, %struct.ScmObj* %args49795$k40469$0)
store volatile %struct.ScmObj* %args49795$k40469$1, %struct.ScmObj** %stackaddr$prim50905, align 8
%stackaddr$prim50906 = alloca %struct.ScmObj*, align 8
%args49795$k40469$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44367, %struct.ScmObj* %args49795$k40469$1)
store volatile %struct.ScmObj* %args49795$k40469$2, %struct.ScmObj** %stackaddr$prim50906, align 8
%clofunc50907 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40469)
musttail call tailcc void %clofunc50907(%struct.ScmObj* %k40469, %struct.ScmObj* %args49795$k40469$2)
ret void
falsebranch$cmp50904:
%ae44376 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44377 = call %struct.ScmObj* @const_init_int(i64 0)
%args49796$k40469$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50908 = alloca %struct.ScmObj*, align 8
%args49796$k40469$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44377, %struct.ScmObj* %args49796$k40469$0)
store volatile %struct.ScmObj* %args49796$k40469$1, %struct.ScmObj** %stackaddr$prim50908, align 8
%stackaddr$prim50909 = alloca %struct.ScmObj*, align 8
%args49796$k40469$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44376, %struct.ScmObj* %args49796$k40469$1)
store volatile %struct.ScmObj* %args49796$k40469$2, %struct.ScmObj** %stackaddr$prim50909, align 8
%clofunc50910 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40469)
musttail call tailcc void %clofunc50910(%struct.ScmObj* %k40469, %struct.ScmObj* %args49796$k40469$2)
ret void
falsebranch$cmp50902:
%ae44385 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50911 = alloca %struct.ScmObj*, align 8
%anf_45bind40382 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %ok_6340230, %struct.ScmObj* %ae44385)
store volatile %struct.ScmObj* %anf_45bind40382, %struct.ScmObj** %stackaddr$prim50911, align 8
%stackaddr$prim50912 = alloca %struct.ScmObj*, align 8
%anf_45bind40383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40240)
store volatile %struct.ScmObj* %anf_45bind40383, %struct.ScmObj** %stackaddr$prim50912, align 8
%stackaddr$makeclosure50913 = alloca %struct.ScmObj*, align 8
%fptrToInt50914 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44388 to i64
%ae44388 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt50914)
store volatile %struct.ScmObj* %ae44388, %struct.ScmObj** %stackaddr$makeclosure50913, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44388, %struct.ScmObj* %my_45try40231, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44388, %struct.ScmObj* %_37append40199, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44388, %struct.ScmObj* %k40469, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44388, %struct.ScmObj* %x40240, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44388, %struct.ScmObj* %y40239, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44388, %struct.ScmObj* %z40238, i64 5)
%ae44390 = call %struct.ScmObj* @const_init_int(i64 1)
%args49831$anf_45bind40382$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50915 = alloca %struct.ScmObj*, align 8
%args49831$anf_45bind40382$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %z40238, %struct.ScmObj* %args49831$anf_45bind40382$0)
store volatile %struct.ScmObj* %args49831$anf_45bind40382$1, %struct.ScmObj** %stackaddr$prim50915, align 8
%stackaddr$prim50916 = alloca %struct.ScmObj*, align 8
%args49831$anf_45bind40382$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44390, %struct.ScmObj* %args49831$anf_45bind40382$1)
store volatile %struct.ScmObj* %args49831$anf_45bind40382$2, %struct.ScmObj** %stackaddr$prim50916, align 8
%stackaddr$prim50917 = alloca %struct.ScmObj*, align 8
%args49831$anf_45bind40382$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40383, %struct.ScmObj* %args49831$anf_45bind40382$2)
store volatile %struct.ScmObj* %args49831$anf_45bind40382$3, %struct.ScmObj** %stackaddr$prim50917, align 8
%stackaddr$prim50918 = alloca %struct.ScmObj*, align 8
%args49831$anf_45bind40382$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44388, %struct.ScmObj* %args49831$anf_45bind40382$3)
store volatile %struct.ScmObj* %args49831$anf_45bind40382$4, %struct.ScmObj** %stackaddr$prim50918, align 8
%clofunc50919 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40382)
musttail call tailcc void %clofunc50919(%struct.ScmObj* %anf_45bind40382, %struct.ScmObj* %args49831$anf_45bind40382$4)
ret void
}

define tailcc void @proc_clo$ae44388(%struct.ScmObj* %env$ae44388,%struct.ScmObj* %current_45args49797) {
%stackaddr$env-ref50920 = alloca %struct.ScmObj*, align 8
%my_45try40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44388, i64 0)
store %struct.ScmObj* %my_45try40231, %struct.ScmObj** %stackaddr$env-ref50920
%stackaddr$env-ref50921 = alloca %struct.ScmObj*, align 8
%_37append40199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44388, i64 1)
store %struct.ScmObj* %_37append40199, %struct.ScmObj** %stackaddr$env-ref50921
%stackaddr$env-ref50922 = alloca %struct.ScmObj*, align 8
%k40469 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44388, i64 2)
store %struct.ScmObj* %k40469, %struct.ScmObj** %stackaddr$env-ref50922
%stackaddr$env-ref50923 = alloca %struct.ScmObj*, align 8
%x40240 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44388, i64 3)
store %struct.ScmObj* %x40240, %struct.ScmObj** %stackaddr$env-ref50923
%stackaddr$env-ref50924 = alloca %struct.ScmObj*, align 8
%y40239 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44388, i64 4)
store %struct.ScmObj* %y40239, %struct.ScmObj** %stackaddr$env-ref50924
%stackaddr$env-ref50925 = alloca %struct.ScmObj*, align 8
%z40238 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44388, i64 5)
store %struct.ScmObj* %z40238, %struct.ScmObj** %stackaddr$env-ref50925
%stackaddr$prim50926 = alloca %struct.ScmObj*, align 8
%_95k40470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49797)
store volatile %struct.ScmObj* %_95k40470, %struct.ScmObj** %stackaddr$prim50926, align 8
%stackaddr$prim50927 = alloca %struct.ScmObj*, align 8
%current_45args49798 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49797)
store volatile %struct.ScmObj* %current_45args49798, %struct.ScmObj** %stackaddr$prim50927, align 8
%stackaddr$prim50928 = alloca %struct.ScmObj*, align 8
%anf_45bind40384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49798)
store volatile %struct.ScmObj* %anf_45bind40384, %struct.ScmObj** %stackaddr$prim50928, align 8
%truthy$cmp50929 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40384)
%cmp$cmp50929 = icmp eq i64 %truthy$cmp50929, 1
br i1 %cmp$cmp50929, label %truebranch$cmp50929, label %falsebranch$cmp50929
truebranch$cmp50929:
%ae44399 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50930 = alloca %struct.ScmObj*, align 8
%anf_45bind40385 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %my_45try40231, %struct.ScmObj* %ae44399)
store volatile %struct.ScmObj* %anf_45bind40385, %struct.ScmObj** %stackaddr$prim50930, align 8
%stackaddr$prim50931 = alloca %struct.ScmObj*, align 8
%anf_45bind40386 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40240)
store volatile %struct.ScmObj* %anf_45bind40386, %struct.ScmObj** %stackaddr$prim50931, align 8
%stackaddr$makeclosure50932 = alloca %struct.ScmObj*, align 8
%fptrToInt50933 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44402 to i64
%ae44402 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt50933)
store volatile %struct.ScmObj* %ae44402, %struct.ScmObj** %stackaddr$makeclosure50932, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44402, %struct.ScmObj* %my_45try40231, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44402, %struct.ScmObj* %k40469, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44402, %struct.ScmObj* %anf_45bind40385, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44402, %struct.ScmObj* %x40240, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44402, %struct.ScmObj* %y40239, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44402, %struct.ScmObj* %z40238, i64 5)
%args49821$_37append40199$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50934 = alloca %struct.ScmObj*, align 8
%args49821$_37append40199$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y40239, %struct.ScmObj* %args49821$_37append40199$0)
store volatile %struct.ScmObj* %args49821$_37append40199$1, %struct.ScmObj** %stackaddr$prim50934, align 8
%stackaddr$prim50935 = alloca %struct.ScmObj*, align 8
%args49821$_37append40199$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40386, %struct.ScmObj* %args49821$_37append40199$1)
store volatile %struct.ScmObj* %args49821$_37append40199$2, %struct.ScmObj** %stackaddr$prim50935, align 8
%stackaddr$prim50936 = alloca %struct.ScmObj*, align 8
%args49821$_37append40199$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44402, %struct.ScmObj* %args49821$_37append40199$2)
store volatile %struct.ScmObj* %args49821$_37append40199$3, %struct.ScmObj** %stackaddr$prim50936, align 8
%clofunc50937 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37append40199)
musttail call tailcc void %clofunc50937(%struct.ScmObj* %_37append40199, %struct.ScmObj* %args49821$_37append40199$3)
ret void
falsebranch$cmp50929:
%stackaddr$makeclosure50938 = alloca %struct.ScmObj*, align 8
%fptrToInt50939 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44537 to i64
%ae44537 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt50939)
store volatile %struct.ScmObj* %ae44537, %struct.ScmObj** %stackaddr$makeclosure50938, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44537, %struct.ScmObj* %my_45try40231, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44537, %struct.ScmObj* %k40469, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44537, %struct.ScmObj* %x40240, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44537, %struct.ScmObj* %y40239, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44537, %struct.ScmObj* %z40238, i64 4)
%ae44538 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44539 = call %struct.ScmObj* @const_init_int(i64 0)
%args49830$ae44537$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50940 = alloca %struct.ScmObj*, align 8
%args49830$ae44537$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44539, %struct.ScmObj* %args49830$ae44537$0)
store volatile %struct.ScmObj* %args49830$ae44537$1, %struct.ScmObj** %stackaddr$prim50940, align 8
%stackaddr$prim50941 = alloca %struct.ScmObj*, align 8
%args49830$ae44537$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44538, %struct.ScmObj* %args49830$ae44537$1)
store volatile %struct.ScmObj* %args49830$ae44537$2, %struct.ScmObj** %stackaddr$prim50941, align 8
%clofunc50942 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44537)
musttail call tailcc void %clofunc50942(%struct.ScmObj* %ae44537, %struct.ScmObj* %args49830$ae44537$2)
ret void
}

define tailcc void @proc_clo$ae44402(%struct.ScmObj* %env$ae44402,%struct.ScmObj* %current_45args49800) {
%stackaddr$env-ref50943 = alloca %struct.ScmObj*, align 8
%my_45try40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44402, i64 0)
store %struct.ScmObj* %my_45try40231, %struct.ScmObj** %stackaddr$env-ref50943
%stackaddr$env-ref50944 = alloca %struct.ScmObj*, align 8
%k40469 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44402, i64 1)
store %struct.ScmObj* %k40469, %struct.ScmObj** %stackaddr$env-ref50944
%stackaddr$env-ref50945 = alloca %struct.ScmObj*, align 8
%anf_45bind40385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44402, i64 2)
store %struct.ScmObj* %anf_45bind40385, %struct.ScmObj** %stackaddr$env-ref50945
%stackaddr$env-ref50946 = alloca %struct.ScmObj*, align 8
%x40240 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44402, i64 3)
store %struct.ScmObj* %x40240, %struct.ScmObj** %stackaddr$env-ref50946
%stackaddr$env-ref50947 = alloca %struct.ScmObj*, align 8
%y40239 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44402, i64 4)
store %struct.ScmObj* %y40239, %struct.ScmObj** %stackaddr$env-ref50947
%stackaddr$env-ref50948 = alloca %struct.ScmObj*, align 8
%z40238 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44402, i64 5)
store %struct.ScmObj* %z40238, %struct.ScmObj** %stackaddr$env-ref50948
%stackaddr$prim50949 = alloca %struct.ScmObj*, align 8
%_95k40474 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49800)
store volatile %struct.ScmObj* %_95k40474, %struct.ScmObj** %stackaddr$prim50949, align 8
%stackaddr$prim50950 = alloca %struct.ScmObj*, align 8
%current_45args49801 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49800)
store volatile %struct.ScmObj* %current_45args49801, %struct.ScmObj** %stackaddr$prim50950, align 8
%stackaddr$prim50951 = alloca %struct.ScmObj*, align 8
%anf_45bind40387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49801)
store volatile %struct.ScmObj* %anf_45bind40387, %struct.ScmObj** %stackaddr$prim50951, align 8
%stackaddr$makeclosure50952 = alloca %struct.ScmObj*, align 8
%fptrToInt50953 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44405 to i64
%ae44405 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50953)
store volatile %struct.ScmObj* %ae44405, %struct.ScmObj** %stackaddr$makeclosure50952, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44405, %struct.ScmObj* %my_45try40231, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44405, %struct.ScmObj* %k40469, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44405, %struct.ScmObj* %anf_45bind40387, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44405, %struct.ScmObj* %anf_45bind40385, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44405, %struct.ScmObj* %x40240, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44405, %struct.ScmObj* %y40239, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44405, %struct.ScmObj* %z40238, i64 6)
%ae44406 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50954 = alloca %struct.ScmObj*, align 8
%fptrToInt50955 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44407 to i64
%ae44407 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50955)
store volatile %struct.ScmObj* %ae44407, %struct.ScmObj** %stackaddr$makeclosure50954, align 8
%args49820$ae44405$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50956 = alloca %struct.ScmObj*, align 8
%args49820$ae44405$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44407, %struct.ScmObj* %args49820$ae44405$0)
store volatile %struct.ScmObj* %args49820$ae44405$1, %struct.ScmObj** %stackaddr$prim50956, align 8
%stackaddr$prim50957 = alloca %struct.ScmObj*, align 8
%args49820$ae44405$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44406, %struct.ScmObj* %args49820$ae44405$1)
store volatile %struct.ScmObj* %args49820$ae44405$2, %struct.ScmObj** %stackaddr$prim50957, align 8
%clofunc50958 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44405)
musttail call tailcc void %clofunc50958(%struct.ScmObj* %ae44405, %struct.ScmObj* %args49820$ae44405$2)
ret void
}

define tailcc void @proc_clo$ae44405(%struct.ScmObj* %env$ae44405,%struct.ScmObj* %current_45args49803) {
%stackaddr$env-ref50959 = alloca %struct.ScmObj*, align 8
%my_45try40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44405, i64 0)
store %struct.ScmObj* %my_45try40231, %struct.ScmObj** %stackaddr$env-ref50959
%stackaddr$env-ref50960 = alloca %struct.ScmObj*, align 8
%k40469 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44405, i64 1)
store %struct.ScmObj* %k40469, %struct.ScmObj** %stackaddr$env-ref50960
%stackaddr$env-ref50961 = alloca %struct.ScmObj*, align 8
%anf_45bind40387 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44405, i64 2)
store %struct.ScmObj* %anf_45bind40387, %struct.ScmObj** %stackaddr$env-ref50961
%stackaddr$env-ref50962 = alloca %struct.ScmObj*, align 8
%anf_45bind40385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44405, i64 3)
store %struct.ScmObj* %anf_45bind40385, %struct.ScmObj** %stackaddr$env-ref50962
%stackaddr$env-ref50963 = alloca %struct.ScmObj*, align 8
%x40240 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44405, i64 4)
store %struct.ScmObj* %x40240, %struct.ScmObj** %stackaddr$env-ref50963
%stackaddr$env-ref50964 = alloca %struct.ScmObj*, align 8
%y40239 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44405, i64 5)
store %struct.ScmObj* %y40239, %struct.ScmObj** %stackaddr$env-ref50964
%stackaddr$env-ref50965 = alloca %struct.ScmObj*, align 8
%z40238 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44405, i64 6)
store %struct.ScmObj* %z40238, %struct.ScmObj** %stackaddr$env-ref50965
%stackaddr$prim50966 = alloca %struct.ScmObj*, align 8
%_95k40475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49803)
store volatile %struct.ScmObj* %_95k40475, %struct.ScmObj** %stackaddr$prim50966, align 8
%stackaddr$prim50967 = alloca %struct.ScmObj*, align 8
%current_45args49804 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49803)
store volatile %struct.ScmObj* %current_45args49804, %struct.ScmObj** %stackaddr$prim50967, align 8
%stackaddr$prim50968 = alloca %struct.ScmObj*, align 8
%anf_45bind40388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49804)
store volatile %struct.ScmObj* %anf_45bind40388, %struct.ScmObj** %stackaddr$prim50968, align 8
%stackaddr$makeclosure50969 = alloca %struct.ScmObj*, align 8
%fptrToInt50970 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44428 to i64
%ae44428 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50970)
store volatile %struct.ScmObj* %ae44428, %struct.ScmObj** %stackaddr$makeclosure50969, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44428, %struct.ScmObj* %my_45try40231, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44428, %struct.ScmObj* %k40469, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44428, %struct.ScmObj* %anf_45bind40387, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44428, %struct.ScmObj* %anf_45bind40385, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44428, %struct.ScmObj* %x40240, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44428, %struct.ScmObj* %y40239, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44428, %struct.ScmObj* %z40238, i64 6)
%args49818$anf_45bind40388$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50971 = alloca %struct.ScmObj*, align 8
%args49818$anf_45bind40388$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44428, %struct.ScmObj* %args49818$anf_45bind40388$0)
store volatile %struct.ScmObj* %args49818$anf_45bind40388$1, %struct.ScmObj** %stackaddr$prim50971, align 8
%clofunc50972 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40388)
musttail call tailcc void %clofunc50972(%struct.ScmObj* %anf_45bind40388, %struct.ScmObj* %args49818$anf_45bind40388$1)
ret void
}

define tailcc void @proc_clo$ae44428(%struct.ScmObj* %env$ae44428,%struct.ScmObj* %current_45args49806) {
%stackaddr$env-ref50973 = alloca %struct.ScmObj*, align 8
%my_45try40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44428, i64 0)
store %struct.ScmObj* %my_45try40231, %struct.ScmObj** %stackaddr$env-ref50973
%stackaddr$env-ref50974 = alloca %struct.ScmObj*, align 8
%k40469 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44428, i64 1)
store %struct.ScmObj* %k40469, %struct.ScmObj** %stackaddr$env-ref50974
%stackaddr$env-ref50975 = alloca %struct.ScmObj*, align 8
%anf_45bind40387 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44428, i64 2)
store %struct.ScmObj* %anf_45bind40387, %struct.ScmObj** %stackaddr$env-ref50975
%stackaddr$env-ref50976 = alloca %struct.ScmObj*, align 8
%anf_45bind40385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44428, i64 3)
store %struct.ScmObj* %anf_45bind40385, %struct.ScmObj** %stackaddr$env-ref50976
%stackaddr$env-ref50977 = alloca %struct.ScmObj*, align 8
%x40240 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44428, i64 4)
store %struct.ScmObj* %x40240, %struct.ScmObj** %stackaddr$env-ref50977
%stackaddr$env-ref50978 = alloca %struct.ScmObj*, align 8
%y40239 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44428, i64 5)
store %struct.ScmObj* %y40239, %struct.ScmObj** %stackaddr$env-ref50978
%stackaddr$env-ref50979 = alloca %struct.ScmObj*, align 8
%z40238 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44428, i64 6)
store %struct.ScmObj* %z40238, %struct.ScmObj** %stackaddr$env-ref50979
%stackaddr$prim50980 = alloca %struct.ScmObj*, align 8
%_95k40476 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49806)
store volatile %struct.ScmObj* %_95k40476, %struct.ScmObj** %stackaddr$prim50980, align 8
%stackaddr$prim50981 = alloca %struct.ScmObj*, align 8
%current_45args49807 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49806)
store volatile %struct.ScmObj* %current_45args49807, %struct.ScmObj** %stackaddr$prim50981, align 8
%stackaddr$prim50982 = alloca %struct.ScmObj*, align 8
%anf_45bind40389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49807)
store volatile %struct.ScmObj* %anf_45bind40389, %struct.ScmObj** %stackaddr$prim50982, align 8
%stackaddr$prim50983 = alloca %struct.ScmObj*, align 8
%anf_45bind40390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40240)
store volatile %struct.ScmObj* %anf_45bind40390, %struct.ScmObj** %stackaddr$prim50983, align 8
%stackaddr$prim50984 = alloca %struct.ScmObj*, align 8
%anf_45bind40391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40390, %struct.ScmObj* %z40238)
store volatile %struct.ScmObj* %anf_45bind40391, %struct.ScmObj** %stackaddr$prim50984, align 8
%stackaddr$makeclosure50985 = alloca %struct.ScmObj*, align 8
%fptrToInt50986 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44433 to i64
%ae44433 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt50986)
store volatile %struct.ScmObj* %ae44433, %struct.ScmObj** %stackaddr$makeclosure50985, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44433, %struct.ScmObj* %my_45try40231, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44433, %struct.ScmObj* %k40469, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44433, %struct.ScmObj* %x40240, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44433, %struct.ScmObj* %y40239, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44433, %struct.ScmObj* %z40238, i64 4)
%args49817$anf_45bind40385$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50987 = alloca %struct.ScmObj*, align 8
%args49817$anf_45bind40385$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40391, %struct.ScmObj* %args49817$anf_45bind40385$0)
store volatile %struct.ScmObj* %args49817$anf_45bind40385$1, %struct.ScmObj** %stackaddr$prim50987, align 8
%stackaddr$prim50988 = alloca %struct.ScmObj*, align 8
%args49817$anf_45bind40385$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40389, %struct.ScmObj* %args49817$anf_45bind40385$1)
store volatile %struct.ScmObj* %args49817$anf_45bind40385$2, %struct.ScmObj** %stackaddr$prim50988, align 8
%stackaddr$prim50989 = alloca %struct.ScmObj*, align 8
%args49817$anf_45bind40385$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40387, %struct.ScmObj* %args49817$anf_45bind40385$2)
store volatile %struct.ScmObj* %args49817$anf_45bind40385$3, %struct.ScmObj** %stackaddr$prim50989, align 8
%stackaddr$prim50990 = alloca %struct.ScmObj*, align 8
%args49817$anf_45bind40385$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44433, %struct.ScmObj* %args49817$anf_45bind40385$3)
store volatile %struct.ScmObj* %args49817$anf_45bind40385$4, %struct.ScmObj** %stackaddr$prim50990, align 8
%clofunc50991 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40385)
musttail call tailcc void %clofunc50991(%struct.ScmObj* %anf_45bind40385, %struct.ScmObj* %args49817$anf_45bind40385$4)
ret void
}

define tailcc void @proc_clo$ae44433(%struct.ScmObj* %env$ae44433,%struct.ScmObj* %current_45args49809) {
%stackaddr$env-ref50992 = alloca %struct.ScmObj*, align 8
%my_45try40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44433, i64 0)
store %struct.ScmObj* %my_45try40231, %struct.ScmObj** %stackaddr$env-ref50992
%stackaddr$env-ref50993 = alloca %struct.ScmObj*, align 8
%k40469 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44433, i64 1)
store %struct.ScmObj* %k40469, %struct.ScmObj** %stackaddr$env-ref50993
%stackaddr$env-ref50994 = alloca %struct.ScmObj*, align 8
%x40240 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44433, i64 2)
store %struct.ScmObj* %x40240, %struct.ScmObj** %stackaddr$env-ref50994
%stackaddr$env-ref50995 = alloca %struct.ScmObj*, align 8
%y40239 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44433, i64 3)
store %struct.ScmObj* %y40239, %struct.ScmObj** %stackaddr$env-ref50995
%stackaddr$env-ref50996 = alloca %struct.ScmObj*, align 8
%z40238 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44433, i64 4)
store %struct.ScmObj* %z40238, %struct.ScmObj** %stackaddr$env-ref50996
%stackaddr$prim50997 = alloca %struct.ScmObj*, align 8
%_95k40471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49809)
store volatile %struct.ScmObj* %_95k40471, %struct.ScmObj** %stackaddr$prim50997, align 8
%stackaddr$prim50998 = alloca %struct.ScmObj*, align 8
%current_45args49810 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49809)
store volatile %struct.ScmObj* %current_45args49810, %struct.ScmObj** %stackaddr$prim50998, align 8
%stackaddr$prim50999 = alloca %struct.ScmObj*, align 8
%anf_45bind40392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49810)
store volatile %struct.ScmObj* %anf_45bind40392, %struct.ScmObj** %stackaddr$prim50999, align 8
%ae44438 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51000 = alloca %struct.ScmObj*, align 8
%anf_45bind40393 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %my_45try40231, %struct.ScmObj* %ae44438)
store volatile %struct.ScmObj* %anf_45bind40393, %struct.ScmObj** %stackaddr$prim51000, align 8
%stackaddr$prim51001 = alloca %struct.ScmObj*, align 8
%anf_45bind40394 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40240)
store volatile %struct.ScmObj* %anf_45bind40394, %struct.ScmObj** %stackaddr$prim51001, align 8
%stackaddr$prim51002 = alloca %struct.ScmObj*, align 8
%anf_45bind40395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40240)
store volatile %struct.ScmObj* %anf_45bind40395, %struct.ScmObj** %stackaddr$prim51002, align 8
%stackaddr$prim51003 = alloca %struct.ScmObj*, align 8
%anf_45bind40396 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40395, %struct.ScmObj* %y40239)
store volatile %struct.ScmObj* %anf_45bind40396, %struct.ScmObj** %stackaddr$prim51003, align 8
%stackaddr$makeclosure51004 = alloca %struct.ScmObj*, align 8
%fptrToInt51005 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44444 to i64
%ae44444 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt51005)
store volatile %struct.ScmObj* %ae44444, %struct.ScmObj** %stackaddr$makeclosure51004, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44444, %struct.ScmObj* %anf_45bind40392, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44444, %struct.ScmObj* %k40469, i64 1)
%args49816$anf_45bind40393$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51006 = alloca %struct.ScmObj*, align 8
%args49816$anf_45bind40393$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %z40238, %struct.ScmObj* %args49816$anf_45bind40393$0)
store volatile %struct.ScmObj* %args49816$anf_45bind40393$1, %struct.ScmObj** %stackaddr$prim51006, align 8
%stackaddr$prim51007 = alloca %struct.ScmObj*, align 8
%args49816$anf_45bind40393$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40396, %struct.ScmObj* %args49816$anf_45bind40393$1)
store volatile %struct.ScmObj* %args49816$anf_45bind40393$2, %struct.ScmObj** %stackaddr$prim51007, align 8
%stackaddr$prim51008 = alloca %struct.ScmObj*, align 8
%args49816$anf_45bind40393$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40394, %struct.ScmObj* %args49816$anf_45bind40393$2)
store volatile %struct.ScmObj* %args49816$anf_45bind40393$3, %struct.ScmObj** %stackaddr$prim51008, align 8
%stackaddr$prim51009 = alloca %struct.ScmObj*, align 8
%args49816$anf_45bind40393$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44444, %struct.ScmObj* %args49816$anf_45bind40393$3)
store volatile %struct.ScmObj* %args49816$anf_45bind40393$4, %struct.ScmObj** %stackaddr$prim51009, align 8
%clofunc51010 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40393)
musttail call tailcc void %clofunc51010(%struct.ScmObj* %anf_45bind40393, %struct.ScmObj* %args49816$anf_45bind40393$4)
ret void
}

define tailcc void @proc_clo$ae44444(%struct.ScmObj* %env$ae44444,%struct.ScmObj* %current_45args49812) {
%stackaddr$env-ref51011 = alloca %struct.ScmObj*, align 8
%anf_45bind40392 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44444, i64 0)
store %struct.ScmObj* %anf_45bind40392, %struct.ScmObj** %stackaddr$env-ref51011
%stackaddr$env-ref51012 = alloca %struct.ScmObj*, align 8
%k40469 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44444, i64 1)
store %struct.ScmObj* %k40469, %struct.ScmObj** %stackaddr$env-ref51012
%stackaddr$prim51013 = alloca %struct.ScmObj*, align 8
%_95k40472 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49812)
store volatile %struct.ScmObj* %_95k40472, %struct.ScmObj** %stackaddr$prim51013, align 8
%stackaddr$prim51014 = alloca %struct.ScmObj*, align 8
%current_45args49813 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49812)
store volatile %struct.ScmObj* %current_45args49813, %struct.ScmObj** %stackaddr$prim51014, align 8
%stackaddr$prim51015 = alloca %struct.ScmObj*, align 8
%anf_45bind40397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49813)
store volatile %struct.ScmObj* %anf_45bind40397, %struct.ScmObj** %stackaddr$prim51015, align 8
%stackaddr$prim51016 = alloca %struct.ScmObj*, align 8
%cpsprim40473 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %anf_45bind40392, %struct.ScmObj* %anf_45bind40397)
store volatile %struct.ScmObj* %cpsprim40473, %struct.ScmObj** %stackaddr$prim51016, align 8
%ae44451 = call %struct.ScmObj* @const_init_int(i64 0)
%args49815$k40469$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51017 = alloca %struct.ScmObj*, align 8
%args49815$k40469$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40473, %struct.ScmObj* %args49815$k40469$0)
store volatile %struct.ScmObj* %args49815$k40469$1, %struct.ScmObj** %stackaddr$prim51017, align 8
%stackaddr$prim51018 = alloca %struct.ScmObj*, align 8
%args49815$k40469$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44451, %struct.ScmObj* %args49815$k40469$1)
store volatile %struct.ScmObj* %args49815$k40469$2, %struct.ScmObj** %stackaddr$prim51018, align 8
%clofunc51019 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40469)
musttail call tailcc void %clofunc51019(%struct.ScmObj* %k40469, %struct.ScmObj* %args49815$k40469$2)
ret void
}

define tailcc void @proc_clo$ae44407(%struct.ScmObj* %env$ae44407,%struct.ScmObj* %lst4024140477) {
%stackaddr$prim51020 = alloca %struct.ScmObj*, align 8
%k40478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4024140477)
store volatile %struct.ScmObj* %k40478, %struct.ScmObj** %stackaddr$prim51020, align 8
%stackaddr$prim51021 = alloca %struct.ScmObj*, align 8
%lst40241 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4024140477)
store volatile %struct.ScmObj* %lst40241, %struct.ScmObj** %stackaddr$prim51021, align 8
%ae44411 = call %struct.ScmObj* @const_init_int(i64 0)
%args49819$k40478$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51022 = alloca %struct.ScmObj*, align 8
%args49819$k40478$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40241, %struct.ScmObj* %args49819$k40478$0)
store volatile %struct.ScmObj* %args49819$k40478$1, %struct.ScmObj** %stackaddr$prim51022, align 8
%stackaddr$prim51023 = alloca %struct.ScmObj*, align 8
%args49819$k40478$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44411, %struct.ScmObj* %args49819$k40478$1)
store volatile %struct.ScmObj* %args49819$k40478$2, %struct.ScmObj** %stackaddr$prim51023, align 8
%clofunc51024 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40478)
musttail call tailcc void %clofunc51024(%struct.ScmObj* %k40478, %struct.ScmObj* %args49819$k40478$2)
ret void
}

define tailcc void @proc_clo$ae44537(%struct.ScmObj* %env$ae44537,%struct.ScmObj* %current_45args49822) {
%stackaddr$env-ref51025 = alloca %struct.ScmObj*, align 8
%my_45try40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44537, i64 0)
store %struct.ScmObj* %my_45try40231, %struct.ScmObj** %stackaddr$env-ref51025
%stackaddr$env-ref51026 = alloca %struct.ScmObj*, align 8
%k40469 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44537, i64 1)
store %struct.ScmObj* %k40469, %struct.ScmObj** %stackaddr$env-ref51026
%stackaddr$env-ref51027 = alloca %struct.ScmObj*, align 8
%x40240 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44537, i64 2)
store %struct.ScmObj* %x40240, %struct.ScmObj** %stackaddr$env-ref51027
%stackaddr$env-ref51028 = alloca %struct.ScmObj*, align 8
%y40239 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44537, i64 3)
store %struct.ScmObj* %y40239, %struct.ScmObj** %stackaddr$env-ref51028
%stackaddr$env-ref51029 = alloca %struct.ScmObj*, align 8
%z40238 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44537, i64 4)
store %struct.ScmObj* %z40238, %struct.ScmObj** %stackaddr$env-ref51029
%stackaddr$prim51030 = alloca %struct.ScmObj*, align 8
%_95k40471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49822)
store volatile %struct.ScmObj* %_95k40471, %struct.ScmObj** %stackaddr$prim51030, align 8
%stackaddr$prim51031 = alloca %struct.ScmObj*, align 8
%current_45args49823 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49822)
store volatile %struct.ScmObj* %current_45args49823, %struct.ScmObj** %stackaddr$prim51031, align 8
%stackaddr$prim51032 = alloca %struct.ScmObj*, align 8
%anf_45bind40392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49823)
store volatile %struct.ScmObj* %anf_45bind40392, %struct.ScmObj** %stackaddr$prim51032, align 8
%ae44547 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51033 = alloca %struct.ScmObj*, align 8
%anf_45bind40393 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %my_45try40231, %struct.ScmObj* %ae44547)
store volatile %struct.ScmObj* %anf_45bind40393, %struct.ScmObj** %stackaddr$prim51033, align 8
%stackaddr$prim51034 = alloca %struct.ScmObj*, align 8
%anf_45bind40394 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40240)
store volatile %struct.ScmObj* %anf_45bind40394, %struct.ScmObj** %stackaddr$prim51034, align 8
%stackaddr$prim51035 = alloca %struct.ScmObj*, align 8
%anf_45bind40395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40240)
store volatile %struct.ScmObj* %anf_45bind40395, %struct.ScmObj** %stackaddr$prim51035, align 8
%stackaddr$prim51036 = alloca %struct.ScmObj*, align 8
%anf_45bind40396 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40395, %struct.ScmObj* %y40239)
store volatile %struct.ScmObj* %anf_45bind40396, %struct.ScmObj** %stackaddr$prim51036, align 8
%stackaddr$makeclosure51037 = alloca %struct.ScmObj*, align 8
%fptrToInt51038 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44553 to i64
%ae44553 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt51038)
store volatile %struct.ScmObj* %ae44553, %struct.ScmObj** %stackaddr$makeclosure51037, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44553, %struct.ScmObj* %anf_45bind40392, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44553, %struct.ScmObj* %k40469, i64 1)
%args49829$anf_45bind40393$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51039 = alloca %struct.ScmObj*, align 8
%args49829$anf_45bind40393$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %z40238, %struct.ScmObj* %args49829$anf_45bind40393$0)
store volatile %struct.ScmObj* %args49829$anf_45bind40393$1, %struct.ScmObj** %stackaddr$prim51039, align 8
%stackaddr$prim51040 = alloca %struct.ScmObj*, align 8
%args49829$anf_45bind40393$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40396, %struct.ScmObj* %args49829$anf_45bind40393$1)
store volatile %struct.ScmObj* %args49829$anf_45bind40393$2, %struct.ScmObj** %stackaddr$prim51040, align 8
%stackaddr$prim51041 = alloca %struct.ScmObj*, align 8
%args49829$anf_45bind40393$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40394, %struct.ScmObj* %args49829$anf_45bind40393$2)
store volatile %struct.ScmObj* %args49829$anf_45bind40393$3, %struct.ScmObj** %stackaddr$prim51041, align 8
%stackaddr$prim51042 = alloca %struct.ScmObj*, align 8
%args49829$anf_45bind40393$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44553, %struct.ScmObj* %args49829$anf_45bind40393$3)
store volatile %struct.ScmObj* %args49829$anf_45bind40393$4, %struct.ScmObj** %stackaddr$prim51042, align 8
%clofunc51043 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40393)
musttail call tailcc void %clofunc51043(%struct.ScmObj* %anf_45bind40393, %struct.ScmObj* %args49829$anf_45bind40393$4)
ret void
}

define tailcc void @proc_clo$ae44553(%struct.ScmObj* %env$ae44553,%struct.ScmObj* %current_45args49825) {
%stackaddr$env-ref51044 = alloca %struct.ScmObj*, align 8
%anf_45bind40392 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44553, i64 0)
store %struct.ScmObj* %anf_45bind40392, %struct.ScmObj** %stackaddr$env-ref51044
%stackaddr$env-ref51045 = alloca %struct.ScmObj*, align 8
%k40469 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44553, i64 1)
store %struct.ScmObj* %k40469, %struct.ScmObj** %stackaddr$env-ref51045
%stackaddr$prim51046 = alloca %struct.ScmObj*, align 8
%_95k40472 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49825)
store volatile %struct.ScmObj* %_95k40472, %struct.ScmObj** %stackaddr$prim51046, align 8
%stackaddr$prim51047 = alloca %struct.ScmObj*, align 8
%current_45args49826 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49825)
store volatile %struct.ScmObj* %current_45args49826, %struct.ScmObj** %stackaddr$prim51047, align 8
%stackaddr$prim51048 = alloca %struct.ScmObj*, align 8
%anf_45bind40397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49826)
store volatile %struct.ScmObj* %anf_45bind40397, %struct.ScmObj** %stackaddr$prim51048, align 8
%stackaddr$prim51049 = alloca %struct.ScmObj*, align 8
%cpsprim40473 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %anf_45bind40392, %struct.ScmObj* %anf_45bind40397)
store volatile %struct.ScmObj* %cpsprim40473, %struct.ScmObj** %stackaddr$prim51049, align 8
%ae44560 = call %struct.ScmObj* @const_init_int(i64 0)
%args49828$k40469$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51050 = alloca %struct.ScmObj*, align 8
%args49828$k40469$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40473, %struct.ScmObj* %args49828$k40469$0)
store volatile %struct.ScmObj* %args49828$k40469$1, %struct.ScmObj** %stackaddr$prim51050, align 8
%stackaddr$prim51051 = alloca %struct.ScmObj*, align 8
%args49828$k40469$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44560, %struct.ScmObj* %args49828$k40469$1)
store volatile %struct.ScmObj* %args49828$k40469$2, %struct.ScmObj** %stackaddr$prim51051, align 8
%clofunc51052 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40469)
musttail call tailcc void %clofunc51052(%struct.ScmObj* %k40469, %struct.ScmObj* %args49828$k40469$2)
ret void
}

define tailcc void @proc_clo$ae44092(%struct.ScmObj* %env$ae44092,%struct.ScmObj* %current_45args49833) {
%stackaddr$prim51053 = alloca %struct.ScmObj*, align 8
%k40479 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49833)
store volatile %struct.ScmObj* %k40479, %struct.ScmObj** %stackaddr$prim51053, align 8
%stackaddr$prim51054 = alloca %struct.ScmObj*, align 8
%current_45args49834 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49833)
store volatile %struct.ScmObj* %current_45args49834, %struct.ScmObj** %stackaddr$prim51054, align 8
%stackaddr$prim51055 = alloca %struct.ScmObj*, align 8
%n40243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49834)
store volatile %struct.ScmObj* %n40243, %struct.ScmObj** %stackaddr$prim51055, align 8
%stackaddr$prim51056 = alloca %struct.ScmObj*, align 8
%anf_45bind40370 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40370, %struct.ScmObj** %stackaddr$prim51056, align 8
%ae44093 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim51057 = alloca %struct.ScmObj*, align 8
%loop40244 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44093, %struct.ScmObj* %anf_45bind40370)
store volatile %struct.ScmObj* %loop40244, %struct.ScmObj** %stackaddr$prim51057, align 8
%stackaddr$makeclosure51058 = alloca %struct.ScmObj*, align 8
%fptrToInt51059 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44095 to i64
%ae44095 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt51059)
store volatile %struct.ScmObj* %ae44095, %struct.ScmObj** %stackaddr$makeclosure51058, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44095, %struct.ScmObj* %loop40244, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44095, %struct.ScmObj* %n40243, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44095, %struct.ScmObj* %k40479, i64 2)
%ae44096 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51060 = alloca %struct.ScmObj*, align 8
%fptrToInt51061 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44097 to i64
%ae44097 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt51061)
store volatile %struct.ScmObj* %ae44097, %struct.ScmObj** %stackaddr$makeclosure51060, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44097, %struct.ScmObj* %loop40244, i64 0)
%args49855$ae44095$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51062 = alloca %struct.ScmObj*, align 8
%args49855$ae44095$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44097, %struct.ScmObj* %args49855$ae44095$0)
store volatile %struct.ScmObj* %args49855$ae44095$1, %struct.ScmObj** %stackaddr$prim51062, align 8
%stackaddr$prim51063 = alloca %struct.ScmObj*, align 8
%args49855$ae44095$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44096, %struct.ScmObj* %args49855$ae44095$1)
store volatile %struct.ScmObj* %args49855$ae44095$2, %struct.ScmObj** %stackaddr$prim51063, align 8
%clofunc51064 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44095)
musttail call tailcc void %clofunc51064(%struct.ScmObj* %ae44095, %struct.ScmObj* %args49855$ae44095$2)
ret void
}

define tailcc void @proc_clo$ae44095(%struct.ScmObj* %env$ae44095,%struct.ScmObj* %current_45args49836) {
%stackaddr$env-ref51065 = alloca %struct.ScmObj*, align 8
%loop40244 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44095, i64 0)
store %struct.ScmObj* %loop40244, %struct.ScmObj** %stackaddr$env-ref51065
%stackaddr$env-ref51066 = alloca %struct.ScmObj*, align 8
%n40243 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44095, i64 1)
store %struct.ScmObj* %n40243, %struct.ScmObj** %stackaddr$env-ref51066
%stackaddr$env-ref51067 = alloca %struct.ScmObj*, align 8
%k40479 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44095, i64 2)
store %struct.ScmObj* %k40479, %struct.ScmObj** %stackaddr$env-ref51067
%stackaddr$prim51068 = alloca %struct.ScmObj*, align 8
%_95k40480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49836)
store volatile %struct.ScmObj* %_95k40480, %struct.ScmObj** %stackaddr$prim51068, align 8
%stackaddr$prim51069 = alloca %struct.ScmObj*, align 8
%current_45args49837 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49836)
store volatile %struct.ScmObj* %current_45args49837, %struct.ScmObj** %stackaddr$prim51069, align 8
%stackaddr$prim51070 = alloca %struct.ScmObj*, align 8
%anf_45bind40375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49837)
store volatile %struct.ScmObj* %anf_45bind40375, %struct.ScmObj** %stackaddr$prim51070, align 8
%ae44174 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51071 = alloca %struct.ScmObj*, align 8
%t4010640245 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %loop40244, %struct.ScmObj* %ae44174, %struct.ScmObj* %anf_45bind40375)
store volatile %struct.ScmObj* %t4010640245, %struct.ScmObj** %stackaddr$prim51071, align 8
%ae44177 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51072 = alloca %struct.ScmObj*, align 8
%anf_45bind40376 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %loop40244, %struct.ScmObj* %ae44177)
store volatile %struct.ScmObj* %anf_45bind40376, %struct.ScmObj** %stackaddr$prim51072, align 8
%stackaddr$makeclosure51073 = alloca %struct.ScmObj*, align 8
%fptrToInt51074 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44178 to i64
%ae44178 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt51074)
store volatile %struct.ScmObj* %ae44178, %struct.ScmObj** %stackaddr$makeclosure51073, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44178, %struct.ScmObj* %anf_45bind40376, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44178, %struct.ScmObj* %n40243, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44178, %struct.ScmObj* %k40479, i64 2)
%ae44179 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51075 = alloca %struct.ScmObj*, align 8
%fptrToInt51076 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44180 to i64
%ae44180 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt51076)
store volatile %struct.ScmObj* %ae44180, %struct.ScmObj** %stackaddr$makeclosure51075, align 8
%args49848$ae44178$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51077 = alloca %struct.ScmObj*, align 8
%args49848$ae44178$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44180, %struct.ScmObj* %args49848$ae44178$0)
store volatile %struct.ScmObj* %args49848$ae44178$1, %struct.ScmObj** %stackaddr$prim51077, align 8
%stackaddr$prim51078 = alloca %struct.ScmObj*, align 8
%args49848$ae44178$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44179, %struct.ScmObj* %args49848$ae44178$1)
store volatile %struct.ScmObj* %args49848$ae44178$2, %struct.ScmObj** %stackaddr$prim51078, align 8
%clofunc51079 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44178)
musttail call tailcc void %clofunc51079(%struct.ScmObj* %ae44178, %struct.ScmObj* %args49848$ae44178$2)
ret void
}

define tailcc void @proc_clo$ae44178(%struct.ScmObj* %env$ae44178,%struct.ScmObj* %current_45args49839) {
%stackaddr$env-ref51080 = alloca %struct.ScmObj*, align 8
%anf_45bind40376 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44178, i64 0)
store %struct.ScmObj* %anf_45bind40376, %struct.ScmObj** %stackaddr$env-ref51080
%stackaddr$env-ref51081 = alloca %struct.ScmObj*, align 8
%n40243 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44178, i64 1)
store %struct.ScmObj* %n40243, %struct.ScmObj** %stackaddr$env-ref51081
%stackaddr$env-ref51082 = alloca %struct.ScmObj*, align 8
%k40479 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44178, i64 2)
store %struct.ScmObj* %k40479, %struct.ScmObj** %stackaddr$env-ref51082
%stackaddr$prim51083 = alloca %struct.ScmObj*, align 8
%_95k40481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49839)
store volatile %struct.ScmObj* %_95k40481, %struct.ScmObj** %stackaddr$prim51083, align 8
%stackaddr$prim51084 = alloca %struct.ScmObj*, align 8
%current_45args49840 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49839)
store volatile %struct.ScmObj* %current_45args49840, %struct.ScmObj** %stackaddr$prim51084, align 8
%stackaddr$prim51085 = alloca %struct.ScmObj*, align 8
%anf_45bind40377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49840)
store volatile %struct.ScmObj* %anf_45bind40377, %struct.ScmObj** %stackaddr$prim51085, align 8
%stackaddr$makeclosure51086 = alloca %struct.ScmObj*, align 8
%fptrToInt51087 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44201 to i64
%ae44201 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt51087)
store volatile %struct.ScmObj* %ae44201, %struct.ScmObj** %stackaddr$makeclosure51086, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44201, %struct.ScmObj* %anf_45bind40376, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44201, %struct.ScmObj* %n40243, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44201, %struct.ScmObj* %k40479, i64 2)
%args49846$anf_45bind40377$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51088 = alloca %struct.ScmObj*, align 8
%args49846$anf_45bind40377$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44201, %struct.ScmObj* %args49846$anf_45bind40377$0)
store volatile %struct.ScmObj* %args49846$anf_45bind40377$1, %struct.ScmObj** %stackaddr$prim51088, align 8
%clofunc51089 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40377)
musttail call tailcc void %clofunc51089(%struct.ScmObj* %anf_45bind40377, %struct.ScmObj* %args49846$anf_45bind40377$1)
ret void
}

define tailcc void @proc_clo$ae44201(%struct.ScmObj* %env$ae44201,%struct.ScmObj* %current_45args49842) {
%stackaddr$env-ref51090 = alloca %struct.ScmObj*, align 8
%anf_45bind40376 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44201, i64 0)
store %struct.ScmObj* %anf_45bind40376, %struct.ScmObj** %stackaddr$env-ref51090
%stackaddr$env-ref51091 = alloca %struct.ScmObj*, align 8
%n40243 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44201, i64 1)
store %struct.ScmObj* %n40243, %struct.ScmObj** %stackaddr$env-ref51091
%stackaddr$env-ref51092 = alloca %struct.ScmObj*, align 8
%k40479 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44201, i64 2)
store %struct.ScmObj* %k40479, %struct.ScmObj** %stackaddr$env-ref51092
%stackaddr$prim51093 = alloca %struct.ScmObj*, align 8
%_95k40482 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49842)
store volatile %struct.ScmObj* %_95k40482, %struct.ScmObj** %stackaddr$prim51093, align 8
%stackaddr$prim51094 = alloca %struct.ScmObj*, align 8
%current_45args49843 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49842)
store volatile %struct.ScmObj* %current_45args49843, %struct.ScmObj** %stackaddr$prim51094, align 8
%stackaddr$prim51095 = alloca %struct.ScmObj*, align 8
%anf_45bind40378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49843)
store volatile %struct.ScmObj* %anf_45bind40378, %struct.ScmObj** %stackaddr$prim51095, align 8
%args49845$anf_45bind40376$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51096 = alloca %struct.ScmObj*, align 8
%args49845$anf_45bind40376$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40378, %struct.ScmObj* %args49845$anf_45bind40376$0)
store volatile %struct.ScmObj* %args49845$anf_45bind40376$1, %struct.ScmObj** %stackaddr$prim51096, align 8
%stackaddr$prim51097 = alloca %struct.ScmObj*, align 8
%args49845$anf_45bind40376$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %n40243, %struct.ScmObj* %args49845$anf_45bind40376$1)
store volatile %struct.ScmObj* %args49845$anf_45bind40376$2, %struct.ScmObj** %stackaddr$prim51097, align 8
%stackaddr$prim51098 = alloca %struct.ScmObj*, align 8
%args49845$anf_45bind40376$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40479, %struct.ScmObj* %args49845$anf_45bind40376$2)
store volatile %struct.ScmObj* %args49845$anf_45bind40376$3, %struct.ScmObj** %stackaddr$prim51098, align 8
%clofunc51099 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40376)
musttail call tailcc void %clofunc51099(%struct.ScmObj* %anf_45bind40376, %struct.ScmObj* %args49845$anf_45bind40376$3)
ret void
}

define tailcc void @proc_clo$ae44180(%struct.ScmObj* %env$ae44180,%struct.ScmObj* %lst4024840483) {
%stackaddr$prim51100 = alloca %struct.ScmObj*, align 8
%k40484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4024840483)
store volatile %struct.ScmObj* %k40484, %struct.ScmObj** %stackaddr$prim51100, align 8
%stackaddr$prim51101 = alloca %struct.ScmObj*, align 8
%lst40248 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4024840483)
store volatile %struct.ScmObj* %lst40248, %struct.ScmObj** %stackaddr$prim51101, align 8
%ae44184 = call %struct.ScmObj* @const_init_int(i64 0)
%args49847$k40484$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51102 = alloca %struct.ScmObj*, align 8
%args49847$k40484$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40248, %struct.ScmObj* %args49847$k40484$0)
store volatile %struct.ScmObj* %args49847$k40484$1, %struct.ScmObj** %stackaddr$prim51102, align 8
%stackaddr$prim51103 = alloca %struct.ScmObj*, align 8
%args49847$k40484$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44184, %struct.ScmObj* %args49847$k40484$1)
store volatile %struct.ScmObj* %args49847$k40484$2, %struct.ScmObj** %stackaddr$prim51103, align 8
%clofunc51104 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40484)
musttail call tailcc void %clofunc51104(%struct.ScmObj* %k40484, %struct.ScmObj* %args49847$k40484$2)
ret void
}

define tailcc void @proc_clo$ae44097(%struct.ScmObj* %env$ae44097,%struct.ScmObj* %current_45args49849) {
%stackaddr$env-ref51105 = alloca %struct.ScmObj*, align 8
%loop40244 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44097, i64 0)
store %struct.ScmObj* %loop40244, %struct.ScmObj** %stackaddr$env-ref51105
%stackaddr$prim51106 = alloca %struct.ScmObj*, align 8
%k40485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49849)
store volatile %struct.ScmObj* %k40485, %struct.ScmObj** %stackaddr$prim51106, align 8
%stackaddr$prim51107 = alloca %struct.ScmObj*, align 8
%current_45args49850 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49849)
store volatile %struct.ScmObj* %current_45args49850, %struct.ScmObj** %stackaddr$prim51107, align 8
%stackaddr$prim51108 = alloca %struct.ScmObj*, align 8
%i40247 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49850)
store volatile %struct.ScmObj* %i40247, %struct.ScmObj** %stackaddr$prim51108, align 8
%stackaddr$prim51109 = alloca %struct.ScmObj*, align 8
%current_45args49851 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49850)
store volatile %struct.ScmObj* %current_45args49851, %struct.ScmObj** %stackaddr$prim51109, align 8
%stackaddr$prim51110 = alloca %struct.ScmObj*, align 8
%l40246 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49851)
store volatile %struct.ScmObj* %l40246, %struct.ScmObj** %stackaddr$prim51110, align 8
%ae44099 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51111 = alloca %struct.ScmObj*, align 8
%anf_45bind40371 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %i40247, %struct.ScmObj* %ae44099)
store volatile %struct.ScmObj* %anf_45bind40371, %struct.ScmObj** %stackaddr$prim51111, align 8
%truthy$cmp51112 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40371)
%cmp$cmp51112 = icmp eq i64 %truthy$cmp51112, 1
br i1 %cmp$cmp51112, label %truebranch$cmp51112, label %falsebranch$cmp51112
truebranch$cmp51112:
%ae44102 = call %struct.ScmObj* @const_init_int(i64 0)
%args49853$k40485$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51113 = alloca %struct.ScmObj*, align 8
%args49853$k40485$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %l40246, %struct.ScmObj* %args49853$k40485$0)
store volatile %struct.ScmObj* %args49853$k40485$1, %struct.ScmObj** %stackaddr$prim51113, align 8
%stackaddr$prim51114 = alloca %struct.ScmObj*, align 8
%args49853$k40485$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44102, %struct.ScmObj* %args49853$k40485$1)
store volatile %struct.ScmObj* %args49853$k40485$2, %struct.ScmObj** %stackaddr$prim51114, align 8
%clofunc51115 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40485)
musttail call tailcc void %clofunc51115(%struct.ScmObj* %k40485, %struct.ScmObj* %args49853$k40485$2)
ret void
falsebranch$cmp51112:
%ae44108 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51116 = alloca %struct.ScmObj*, align 8
%anf_45bind40372 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %loop40244, %struct.ScmObj* %ae44108)
store volatile %struct.ScmObj* %anf_45bind40372, %struct.ScmObj** %stackaddr$prim51116, align 8
%ae44110 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim51117 = alloca %struct.ScmObj*, align 8
%anf_45bind40373 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %i40247, %struct.ScmObj* %ae44110)
store volatile %struct.ScmObj* %anf_45bind40373, %struct.ScmObj** %stackaddr$prim51117, align 8
%stackaddr$prim51118 = alloca %struct.ScmObj*, align 8
%anf_45bind40374 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %i40247, %struct.ScmObj* %l40246)
store volatile %struct.ScmObj* %anf_45bind40374, %struct.ScmObj** %stackaddr$prim51118, align 8
%args49854$anf_45bind40372$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51119 = alloca %struct.ScmObj*, align 8
%args49854$anf_45bind40372$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40374, %struct.ScmObj* %args49854$anf_45bind40372$0)
store volatile %struct.ScmObj* %args49854$anf_45bind40372$1, %struct.ScmObj** %stackaddr$prim51119, align 8
%stackaddr$prim51120 = alloca %struct.ScmObj*, align 8
%args49854$anf_45bind40372$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40373, %struct.ScmObj* %args49854$anf_45bind40372$1)
store volatile %struct.ScmObj* %args49854$anf_45bind40372$2, %struct.ScmObj** %stackaddr$prim51120, align 8
%stackaddr$prim51121 = alloca %struct.ScmObj*, align 8
%args49854$anf_45bind40372$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40485, %struct.ScmObj* %args49854$anf_45bind40372$2)
store volatile %struct.ScmObj* %args49854$anf_45bind40372$3, %struct.ScmObj** %stackaddr$prim51121, align 8
%clofunc51122 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40372)
musttail call tailcc void %clofunc51122(%struct.ScmObj* %anf_45bind40372, %struct.ScmObj* %args49854$anf_45bind40372$3)
ret void
}

define tailcc void @proc_clo$ae43994(%struct.ScmObj* %env$ae43994,%struct.ScmObj* %current_45args49858) {
%stackaddr$prim51123 = alloca %struct.ScmObj*, align 8
%k40486 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49858)
store volatile %struct.ScmObj* %k40486, %struct.ScmObj** %stackaddr$prim51123, align 8
%stackaddr$prim51124 = alloca %struct.ScmObj*, align 8
%current_45args49859 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49858)
store volatile %struct.ScmObj* %current_45args49859, %struct.ScmObj** %stackaddr$prim51124, align 8
%stackaddr$prim51125 = alloca %struct.ScmObj*, align 8
%thunk40226 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49859)
store volatile %struct.ScmObj* %thunk40226, %struct.ScmObj** %stackaddr$prim51125, align 8
%stackaddr$prim51126 = alloca %struct.ScmObj*, align 8
%anf_45bind40362 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk40226)
store volatile %struct.ScmObj* %anf_45bind40362, %struct.ScmObj** %stackaddr$prim51126, align 8
%truthy$cmp51127 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40362)
%cmp$cmp51127 = icmp eq i64 %truthy$cmp51127, 1
br i1 %cmp$cmp51127, label %truebranch$cmp51127, label %falsebranch$cmp51127
truebranch$cmp51127:
%stackaddr$prim51128 = alloca %struct.ScmObj*, align 8
%anf_45bind40363 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk40226)
store volatile %struct.ScmObj* %anf_45bind40363, %struct.ScmObj** %stackaddr$prim51128, align 8
%ae43999 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim51129 = alloca %struct.ScmObj*, align 8
%anf_45bind40364 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40363, %struct.ScmObj* %ae43999)
store volatile %struct.ScmObj* %anf_45bind40364, %struct.ScmObj** %stackaddr$prim51129, align 8
%truthy$cmp51130 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40364)
%cmp$cmp51130 = icmp eq i64 %truthy$cmp51130, 1
br i1 %cmp$cmp51130, label %truebranch$cmp51130, label %falsebranch$cmp51130
truebranch$cmp51130:
%ae44002 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51131 = alloca %struct.ScmObj*, align 8
%anf_45bind40365 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk40226, %struct.ScmObj* %ae44002)
store volatile %struct.ScmObj* %anf_45bind40365, %struct.ScmObj** %stackaddr$prim51131, align 8
%ae44004 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae4400451132, i32 0, i32 0))
%stackaddr$prim51133 = alloca %struct.ScmObj*, align 8
%cpsprim40487 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40365, %struct.ScmObj* %ae44004)
store volatile %struct.ScmObj* %cpsprim40487, %struct.ScmObj** %stackaddr$prim51133, align 8
%ae44006 = call %struct.ScmObj* @const_init_int(i64 0)
%args49861$k40486$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51134 = alloca %struct.ScmObj*, align 8
%args49861$k40486$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40487, %struct.ScmObj* %args49861$k40486$0)
store volatile %struct.ScmObj* %args49861$k40486$1, %struct.ScmObj** %stackaddr$prim51134, align 8
%stackaddr$prim51135 = alloca %struct.ScmObj*, align 8
%args49861$k40486$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44006, %struct.ScmObj* %args49861$k40486$1)
store volatile %struct.ScmObj* %args49861$k40486$2, %struct.ScmObj** %stackaddr$prim51135, align 8
%clofunc51136 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40486)
musttail call tailcc void %clofunc51136(%struct.ScmObj* %k40486, %struct.ScmObj* %args49861$k40486$2)
ret void
falsebranch$cmp51130:
%ae44024 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44025 = call %struct.ScmObj* @const_init_false()
%args49862$k40486$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51137 = alloca %struct.ScmObj*, align 8
%args49862$k40486$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44025, %struct.ScmObj* %args49862$k40486$0)
store volatile %struct.ScmObj* %args49862$k40486$1, %struct.ScmObj** %stackaddr$prim51137, align 8
%stackaddr$prim51138 = alloca %struct.ScmObj*, align 8
%args49862$k40486$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44024, %struct.ScmObj* %args49862$k40486$1)
store volatile %struct.ScmObj* %args49862$k40486$2, %struct.ScmObj** %stackaddr$prim51138, align 8
%clofunc51139 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40486)
musttail call tailcc void %clofunc51139(%struct.ScmObj* %k40486, %struct.ScmObj* %args49862$k40486$2)
ret void
falsebranch$cmp51127:
%ae44046 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44047 = call %struct.ScmObj* @const_init_false()
%args49863$k40486$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51140 = alloca %struct.ScmObj*, align 8
%args49863$k40486$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44047, %struct.ScmObj* %args49863$k40486$0)
store volatile %struct.ScmObj* %args49863$k40486$1, %struct.ScmObj** %stackaddr$prim51140, align 8
%stackaddr$prim51141 = alloca %struct.ScmObj*, align 8
%args49863$k40486$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44046, %struct.ScmObj* %args49863$k40486$1)
store volatile %struct.ScmObj* %args49863$k40486$2, %struct.ScmObj** %stackaddr$prim51141, align 8
%clofunc51142 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40486)
musttail call tailcc void %clofunc51142(%struct.ScmObj* %k40486, %struct.ScmObj* %args49863$k40486$2)
ret void
}

define tailcc void @proc_clo$ae43968(%struct.ScmObj* %env$ae43968,%struct.ScmObj* %current_45args49865) {
%stackaddr$prim51143 = alloca %struct.ScmObj*, align 8
%k40488 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49865)
store volatile %struct.ScmObj* %k40488, %struct.ScmObj** %stackaddr$prim51143, align 8
%stackaddr$prim51144 = alloca %struct.ScmObj*, align 8
%current_45args49866 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49865)
store volatile %struct.ScmObj* %current_45args49866, %struct.ScmObj** %stackaddr$prim51144, align 8
%stackaddr$prim51145 = alloca %struct.ScmObj*, align 8
%x40165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49866)
store volatile %struct.ScmObj* %x40165, %struct.ScmObj** %stackaddr$prim51145, align 8
%stackaddr$prim51146 = alloca %struct.ScmObj*, align 8
%anf_45bind40359 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40165)
store volatile %struct.ScmObj* %anf_45bind40359, %struct.ScmObj** %stackaddr$prim51146, align 8
%stackaddr$prim51147 = alloca %struct.ScmObj*, align 8
%anf_45bind40360 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40359)
store volatile %struct.ScmObj* %anf_45bind40360, %struct.ScmObj** %stackaddr$prim51147, align 8
%stackaddr$prim51148 = alloca %struct.ScmObj*, align 8
%anf_45bind40361 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40360)
store volatile %struct.ScmObj* %anf_45bind40361, %struct.ScmObj** %stackaddr$prim51148, align 8
%stackaddr$prim51149 = alloca %struct.ScmObj*, align 8
%cpsprim40489 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40361)
store volatile %struct.ScmObj* %cpsprim40489, %struct.ScmObj** %stackaddr$prim51149, align 8
%ae43974 = call %struct.ScmObj* @const_init_int(i64 0)
%args49868$k40488$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51150 = alloca %struct.ScmObj*, align 8
%args49868$k40488$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40489, %struct.ScmObj* %args49868$k40488$0)
store volatile %struct.ScmObj* %args49868$k40488$1, %struct.ScmObj** %stackaddr$prim51150, align 8
%stackaddr$prim51151 = alloca %struct.ScmObj*, align 8
%args49868$k40488$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43974, %struct.ScmObj* %args49868$k40488$1)
store volatile %struct.ScmObj* %args49868$k40488$2, %struct.ScmObj** %stackaddr$prim51151, align 8
%clofunc51152 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40488)
musttail call tailcc void %clofunc51152(%struct.ScmObj* %k40488, %struct.ScmObj* %args49868$k40488$2)
ret void
}

define tailcc void @proc_clo$ae43944(%struct.ScmObj* %env$ae43944,%struct.ScmObj* %current_45args49870) {
%stackaddr$prim51153 = alloca %struct.ScmObj*, align 8
%k40490 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49870)
store volatile %struct.ScmObj* %k40490, %struct.ScmObj** %stackaddr$prim51153, align 8
%stackaddr$prim51154 = alloca %struct.ScmObj*, align 8
%current_45args49871 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49870)
store volatile %struct.ScmObj* %current_45args49871, %struct.ScmObj** %stackaddr$prim51154, align 8
%stackaddr$prim51155 = alloca %struct.ScmObj*, align 8
%x40167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49871)
store volatile %struct.ScmObj* %x40167, %struct.ScmObj** %stackaddr$prim51155, align 8
%stackaddr$prim51156 = alloca %struct.ScmObj*, align 8
%anf_45bind40357 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40167)
store volatile %struct.ScmObj* %anf_45bind40357, %struct.ScmObj** %stackaddr$prim51156, align 8
%stackaddr$prim51157 = alloca %struct.ScmObj*, align 8
%anf_45bind40358 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40357)
store volatile %struct.ScmObj* %anf_45bind40358, %struct.ScmObj** %stackaddr$prim51157, align 8
%stackaddr$prim51158 = alloca %struct.ScmObj*, align 8
%cpsprim40491 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40358)
store volatile %struct.ScmObj* %cpsprim40491, %struct.ScmObj** %stackaddr$prim51158, align 8
%ae43949 = call %struct.ScmObj* @const_init_int(i64 0)
%args49873$k40490$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51159 = alloca %struct.ScmObj*, align 8
%args49873$k40490$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40491, %struct.ScmObj* %args49873$k40490$0)
store volatile %struct.ScmObj* %args49873$k40490$1, %struct.ScmObj** %stackaddr$prim51159, align 8
%stackaddr$prim51160 = alloca %struct.ScmObj*, align 8
%args49873$k40490$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43949, %struct.ScmObj* %args49873$k40490$1)
store volatile %struct.ScmObj* %args49873$k40490$2, %struct.ScmObj** %stackaddr$prim51160, align 8
%clofunc51161 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40490)
musttail call tailcc void %clofunc51161(%struct.ScmObj* %k40490, %struct.ScmObj* %args49873$k40490$2)
ret void
}

define tailcc void @proc_clo$ae43922(%struct.ScmObj* %env$ae43922,%struct.ScmObj* %current_45args49875) {
%stackaddr$prim51162 = alloca %struct.ScmObj*, align 8
%k40492 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49875)
store volatile %struct.ScmObj* %k40492, %struct.ScmObj** %stackaddr$prim51162, align 8
%stackaddr$prim51163 = alloca %struct.ScmObj*, align 8
%current_45args49876 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49875)
store volatile %struct.ScmObj* %current_45args49876, %struct.ScmObj** %stackaddr$prim51163, align 8
%stackaddr$prim51164 = alloca %struct.ScmObj*, align 8
%x40169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49876)
store volatile %struct.ScmObj* %x40169, %struct.ScmObj** %stackaddr$prim51164, align 8
%stackaddr$prim51165 = alloca %struct.ScmObj*, align 8
%anf_45bind40356 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40169)
store volatile %struct.ScmObj* %anf_45bind40356, %struct.ScmObj** %stackaddr$prim51165, align 8
%stackaddr$prim51166 = alloca %struct.ScmObj*, align 8
%cpsprim40493 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40356)
store volatile %struct.ScmObj* %cpsprim40493, %struct.ScmObj** %stackaddr$prim51166, align 8
%ae43926 = call %struct.ScmObj* @const_init_int(i64 0)
%args49878$k40492$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51167 = alloca %struct.ScmObj*, align 8
%args49878$k40492$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40493, %struct.ScmObj* %args49878$k40492$0)
store volatile %struct.ScmObj* %args49878$k40492$1, %struct.ScmObj** %stackaddr$prim51167, align 8
%stackaddr$prim51168 = alloca %struct.ScmObj*, align 8
%args49878$k40492$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43926, %struct.ScmObj* %args49878$k40492$1)
store volatile %struct.ScmObj* %args49878$k40492$2, %struct.ScmObj** %stackaddr$prim51168, align 8
%clofunc51169 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40492)
musttail call tailcc void %clofunc51169(%struct.ScmObj* %k40492, %struct.ScmObj* %args49878$k40492$2)
ret void
}

define tailcc void @proc_clo$ae43902(%struct.ScmObj* %env$ae43902,%struct.ScmObj* %current_45args49880) {
%stackaddr$prim51170 = alloca %struct.ScmObj*, align 8
%k40494 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49880)
store volatile %struct.ScmObj* %k40494, %struct.ScmObj** %stackaddr$prim51170, align 8
%stackaddr$prim51171 = alloca %struct.ScmObj*, align 8
%current_45args49881 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49880)
store volatile %struct.ScmObj* %current_45args49881, %struct.ScmObj** %stackaddr$prim51171, align 8
%stackaddr$prim51172 = alloca %struct.ScmObj*, align 8
%x40171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49881)
store volatile %struct.ScmObj* %x40171, %struct.ScmObj** %stackaddr$prim51172, align 8
%stackaddr$prim51173 = alloca %struct.ScmObj*, align 8
%cpsprim40495 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40171)
store volatile %struct.ScmObj* %cpsprim40495, %struct.ScmObj** %stackaddr$prim51173, align 8
%ae43905 = call %struct.ScmObj* @const_init_int(i64 0)
%args49883$k40494$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51174 = alloca %struct.ScmObj*, align 8
%args49883$k40494$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40495, %struct.ScmObj* %args49883$k40494$0)
store volatile %struct.ScmObj* %args49883$k40494$1, %struct.ScmObj** %stackaddr$prim51174, align 8
%stackaddr$prim51175 = alloca %struct.ScmObj*, align 8
%args49883$k40494$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43905, %struct.ScmObj* %args49883$k40494$1)
store volatile %struct.ScmObj* %args49883$k40494$2, %struct.ScmObj** %stackaddr$prim51175, align 8
%clofunc51176 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40494)
musttail call tailcc void %clofunc51176(%struct.ScmObj* %k40494, %struct.ScmObj* %args49883$k40494$2)
ret void
}

define tailcc void @proc_clo$ae43804(%struct.ScmObj* %env$ae43804,%struct.ScmObj* %args4017340496) {
%stackaddr$env-ref51177 = alloca %struct.ScmObj*, align 8
%_37foldl140112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43804, i64 0)
store %struct.ScmObj* %_37foldl140112, %struct.ScmObj** %stackaddr$env-ref51177
%stackaddr$prim51178 = alloca %struct.ScmObj*, align 8
%k40497 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4017340496)
store volatile %struct.ScmObj* %k40497, %struct.ScmObj** %stackaddr$prim51178, align 8
%stackaddr$prim51179 = alloca %struct.ScmObj*, align 8
%args40173 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4017340496)
store volatile %struct.ScmObj* %args40173, %struct.ScmObj** %stackaddr$prim51179, align 8
%stackaddr$prim51180 = alloca %struct.ScmObj*, align 8
%anf_45bind40350 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args40173)
store volatile %struct.ScmObj* %anf_45bind40350, %struct.ScmObj** %stackaddr$prim51180, align 8
%truthy$cmp51181 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40350)
%cmp$cmp51181 = icmp eq i64 %truthy$cmp51181, 1
br i1 %cmp$cmp51181, label %truebranch$cmp51181, label %falsebranch$cmp51181
truebranch$cmp51181:
%ae43810 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43811 = call %struct.ScmObj* @const_init_int(i64 1)
%args49885$k40497$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51182 = alloca %struct.ScmObj*, align 8
%args49885$k40497$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43811, %struct.ScmObj* %args49885$k40497$0)
store volatile %struct.ScmObj* %args49885$k40497$1, %struct.ScmObj** %stackaddr$prim51182, align 8
%stackaddr$prim51183 = alloca %struct.ScmObj*, align 8
%args49885$k40497$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43810, %struct.ScmObj* %args49885$k40497$1)
store volatile %struct.ScmObj* %args49885$k40497$2, %struct.ScmObj** %stackaddr$prim51183, align 8
%clofunc51184 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40497)
musttail call tailcc void %clofunc51184(%struct.ScmObj* %k40497, %struct.ScmObj* %args49885$k40497$2)
ret void
falsebranch$cmp51181:
%stackaddr$prim51185 = alloca %struct.ScmObj*, align 8
%anf_45bind40351 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40173)
store volatile %struct.ScmObj* %anf_45bind40351, %struct.ScmObj** %stackaddr$prim51185, align 8
%stackaddr$prim51186 = alloca %struct.ScmObj*, align 8
%anf_45bind40352 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40351)
store volatile %struct.ScmObj* %anf_45bind40352, %struct.ScmObj** %stackaddr$prim51186, align 8
%truthy$cmp51187 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40352)
%cmp$cmp51187 = icmp eq i64 %truthy$cmp51187, 1
br i1 %cmp$cmp51187, label %truebranch$cmp51187, label %falsebranch$cmp51187
truebranch$cmp51187:
%stackaddr$prim51188 = alloca %struct.ScmObj*, align 8
%cpsprim40498 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40173)
store volatile %struct.ScmObj* %cpsprim40498, %struct.ScmObj** %stackaddr$prim51188, align 8
%ae43823 = call %struct.ScmObj* @const_init_int(i64 0)
%args49886$k40497$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51189 = alloca %struct.ScmObj*, align 8
%args49886$k40497$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40498, %struct.ScmObj* %args49886$k40497$0)
store volatile %struct.ScmObj* %args49886$k40497$1, %struct.ScmObj** %stackaddr$prim51189, align 8
%stackaddr$prim51190 = alloca %struct.ScmObj*, align 8
%args49886$k40497$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43823, %struct.ScmObj* %args49886$k40497$1)
store volatile %struct.ScmObj* %args49886$k40497$2, %struct.ScmObj** %stackaddr$prim51190, align 8
%clofunc51191 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40497)
musttail call tailcc void %clofunc51191(%struct.ScmObj* %k40497, %struct.ScmObj* %args49886$k40497$2)
ret void
falsebranch$cmp51187:
%stackaddr$makeclosure51192 = alloca %struct.ScmObj*, align 8
%fptrToInt51193 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43828 to i64
%ae43828 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt51193)
store volatile %struct.ScmObj* %ae43828, %struct.ScmObj** %stackaddr$makeclosure51192, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43828, %struct.ScmObj* %k40497, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43828, %struct.ScmObj* %_37foldl140112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43828, %struct.ScmObj* %args40173, i64 2)
%ae43829 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51194 = alloca %struct.ScmObj*, align 8
%fptrToInt51195 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43830 to i64
%ae43830 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt51195)
store volatile %struct.ScmObj* %ae43830, %struct.ScmObj** %stackaddr$makeclosure51194, align 8
%args49896$ae43828$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51196 = alloca %struct.ScmObj*, align 8
%args49896$ae43828$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43830, %struct.ScmObj* %args49896$ae43828$0)
store volatile %struct.ScmObj* %args49896$ae43828$1, %struct.ScmObj** %stackaddr$prim51196, align 8
%stackaddr$prim51197 = alloca %struct.ScmObj*, align 8
%args49896$ae43828$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43829, %struct.ScmObj* %args49896$ae43828$1)
store volatile %struct.ScmObj* %args49896$ae43828$2, %struct.ScmObj** %stackaddr$prim51197, align 8
%clofunc51198 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43828)
musttail call tailcc void %clofunc51198(%struct.ScmObj* %ae43828, %struct.ScmObj* %args49896$ae43828$2)
ret void
}

define tailcc void @proc_clo$ae43828(%struct.ScmObj* %env$ae43828,%struct.ScmObj* %current_45args49887) {
%stackaddr$env-ref51199 = alloca %struct.ScmObj*, align 8
%k40497 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43828, i64 0)
store %struct.ScmObj* %k40497, %struct.ScmObj** %stackaddr$env-ref51199
%stackaddr$env-ref51200 = alloca %struct.ScmObj*, align 8
%_37foldl140112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43828, i64 1)
store %struct.ScmObj* %_37foldl140112, %struct.ScmObj** %stackaddr$env-ref51200
%stackaddr$env-ref51201 = alloca %struct.ScmObj*, align 8
%args40173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43828, i64 2)
store %struct.ScmObj* %args40173, %struct.ScmObj** %stackaddr$env-ref51201
%stackaddr$prim51202 = alloca %struct.ScmObj*, align 8
%_95k40499 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49887)
store volatile %struct.ScmObj* %_95k40499, %struct.ScmObj** %stackaddr$prim51202, align 8
%stackaddr$prim51203 = alloca %struct.ScmObj*, align 8
%current_45args49888 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49887)
store volatile %struct.ScmObj* %current_45args49888, %struct.ScmObj** %stackaddr$prim51203, align 8
%stackaddr$prim51204 = alloca %struct.ScmObj*, align 8
%anf_45bind40353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49888)
store volatile %struct.ScmObj* %anf_45bind40353, %struct.ScmObj** %stackaddr$prim51204, align 8
%stackaddr$prim51205 = alloca %struct.ScmObj*, align 8
%anf_45bind40354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40173)
store volatile %struct.ScmObj* %anf_45bind40354, %struct.ScmObj** %stackaddr$prim51205, align 8
%stackaddr$prim51206 = alloca %struct.ScmObj*, align 8
%anf_45bind40355 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40173)
store volatile %struct.ScmObj* %anf_45bind40355, %struct.ScmObj** %stackaddr$prim51206, align 8
%args49890$_37foldl140112$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51207 = alloca %struct.ScmObj*, align 8
%args49890$_37foldl140112$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40355, %struct.ScmObj* %args49890$_37foldl140112$0)
store volatile %struct.ScmObj* %args49890$_37foldl140112$1, %struct.ScmObj** %stackaddr$prim51207, align 8
%stackaddr$prim51208 = alloca %struct.ScmObj*, align 8
%args49890$_37foldl140112$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40354, %struct.ScmObj* %args49890$_37foldl140112$1)
store volatile %struct.ScmObj* %args49890$_37foldl140112$2, %struct.ScmObj** %stackaddr$prim51208, align 8
%stackaddr$prim51209 = alloca %struct.ScmObj*, align 8
%args49890$_37foldl140112$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40353, %struct.ScmObj* %args49890$_37foldl140112$2)
store volatile %struct.ScmObj* %args49890$_37foldl140112$3, %struct.ScmObj** %stackaddr$prim51209, align 8
%stackaddr$prim51210 = alloca %struct.ScmObj*, align 8
%args49890$_37foldl140112$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40497, %struct.ScmObj* %args49890$_37foldl140112$3)
store volatile %struct.ScmObj* %args49890$_37foldl140112$4, %struct.ScmObj** %stackaddr$prim51210, align 8
%clofunc51211 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140112)
musttail call tailcc void %clofunc51211(%struct.ScmObj* %_37foldl140112, %struct.ScmObj* %args49890$_37foldl140112$4)
ret void
}

define tailcc void @proc_clo$ae43830(%struct.ScmObj* %env$ae43830,%struct.ScmObj* %current_45args49891) {
%stackaddr$prim51212 = alloca %struct.ScmObj*, align 8
%k40500 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49891)
store volatile %struct.ScmObj* %k40500, %struct.ScmObj** %stackaddr$prim51212, align 8
%stackaddr$prim51213 = alloca %struct.ScmObj*, align 8
%current_45args49892 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49891)
store volatile %struct.ScmObj* %current_45args49892, %struct.ScmObj** %stackaddr$prim51213, align 8
%stackaddr$prim51214 = alloca %struct.ScmObj*, align 8
%n40175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49892)
store volatile %struct.ScmObj* %n40175, %struct.ScmObj** %stackaddr$prim51214, align 8
%stackaddr$prim51215 = alloca %struct.ScmObj*, align 8
%current_45args49893 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49892)
store volatile %struct.ScmObj* %current_45args49893, %struct.ScmObj** %stackaddr$prim51215, align 8
%stackaddr$prim51216 = alloca %struct.ScmObj*, align 8
%v40174 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49893)
store volatile %struct.ScmObj* %v40174, %struct.ScmObj** %stackaddr$prim51216, align 8
%stackaddr$prim51217 = alloca %struct.ScmObj*, align 8
%cpsprim40501 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v40174, %struct.ScmObj* %n40175)
store volatile %struct.ScmObj* %cpsprim40501, %struct.ScmObj** %stackaddr$prim51217, align 8
%ae43834 = call %struct.ScmObj* @const_init_int(i64 0)
%args49895$k40500$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51218 = alloca %struct.ScmObj*, align 8
%args49895$k40500$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40501, %struct.ScmObj* %args49895$k40500$0)
store volatile %struct.ScmObj* %args49895$k40500$1, %struct.ScmObj** %stackaddr$prim51218, align 8
%stackaddr$prim51219 = alloca %struct.ScmObj*, align 8
%args49895$k40500$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43834, %struct.ScmObj* %args49895$k40500$1)
store volatile %struct.ScmObj* %args49895$k40500$2, %struct.ScmObj** %stackaddr$prim51219, align 8
%clofunc51220 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40500)
musttail call tailcc void %clofunc51220(%struct.ScmObj* %k40500, %struct.ScmObj* %args49895$k40500$2)
ret void
}

define tailcc void @proc_clo$ae43400(%struct.ScmObj* %env$ae43400,%struct.ScmObj* %current_45args49898) {
%stackaddr$prim51221 = alloca %struct.ScmObj*, align 8
%k40502 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49898)
store volatile %struct.ScmObj* %k40502, %struct.ScmObj** %stackaddr$prim51221, align 8
%stackaddr$prim51222 = alloca %struct.ScmObj*, align 8
%current_45args49899 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49898)
store volatile %struct.ScmObj* %current_45args49899, %struct.ScmObj** %stackaddr$prim51222, align 8
%stackaddr$prim51223 = alloca %struct.ScmObj*, align 8
%v40178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49899)
store volatile %struct.ScmObj* %v40178, %struct.ScmObj** %stackaddr$prim51223, align 8
%stackaddr$prim51224 = alloca %struct.ScmObj*, align 8
%current_45args49900 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49899)
store volatile %struct.ScmObj* %current_45args49900, %struct.ScmObj** %stackaddr$prim51224, align 8
%stackaddr$prim51225 = alloca %struct.ScmObj*, align 8
%lst40177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49900)
store volatile %struct.ScmObj* %lst40177, %struct.ScmObj** %stackaddr$prim51225, align 8
%ae43401 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim51226 = alloca %struct.ScmObj*, align 8
%lst40179 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae43401, %struct.ScmObj* %lst40177)
store volatile %struct.ScmObj* %lst40179, %struct.ScmObj** %stackaddr$prim51226, align 8
%stackaddr$makeclosure51227 = alloca %struct.ScmObj*, align 8
%fptrToInt51228 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43403 to i64
%ae43403 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt51228)
store volatile %struct.ScmObj* %ae43403, %struct.ScmObj** %stackaddr$makeclosure51227, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43403, %struct.ScmObj* %k40502, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43403, %struct.ScmObj* %lst40179, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43403, %struct.ScmObj* %v40178, i64 2)
%ae43404 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51229 = alloca %struct.ScmObj*, align 8
%fptrToInt51230 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43405 to i64
%ae43405 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt51230)
store volatile %struct.ScmObj* %ae43405, %struct.ScmObj** %stackaddr$makeclosure51229, align 8
%args49922$ae43403$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51231 = alloca %struct.ScmObj*, align 8
%args49922$ae43403$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43405, %struct.ScmObj* %args49922$ae43403$0)
store volatile %struct.ScmObj* %args49922$ae43403$1, %struct.ScmObj** %stackaddr$prim51231, align 8
%stackaddr$prim51232 = alloca %struct.ScmObj*, align 8
%args49922$ae43403$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43404, %struct.ScmObj* %args49922$ae43403$1)
store volatile %struct.ScmObj* %args49922$ae43403$2, %struct.ScmObj** %stackaddr$prim51232, align 8
%clofunc51233 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43403)
musttail call tailcc void %clofunc51233(%struct.ScmObj* %ae43403, %struct.ScmObj* %args49922$ae43403$2)
ret void
}

define tailcc void @proc_clo$ae43403(%struct.ScmObj* %env$ae43403,%struct.ScmObj* %current_45args49902) {
%stackaddr$env-ref51234 = alloca %struct.ScmObj*, align 8
%k40502 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43403, i64 0)
store %struct.ScmObj* %k40502, %struct.ScmObj** %stackaddr$env-ref51234
%stackaddr$env-ref51235 = alloca %struct.ScmObj*, align 8
%lst40179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43403, i64 1)
store %struct.ScmObj* %lst40179, %struct.ScmObj** %stackaddr$env-ref51235
%stackaddr$env-ref51236 = alloca %struct.ScmObj*, align 8
%v40178 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43403, i64 2)
store %struct.ScmObj* %v40178, %struct.ScmObj** %stackaddr$env-ref51236
%stackaddr$prim51237 = alloca %struct.ScmObj*, align 8
%_95k40503 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49902)
store volatile %struct.ScmObj* %_95k40503, %struct.ScmObj** %stackaddr$prim51237, align 8
%stackaddr$prim51238 = alloca %struct.ScmObj*, align 8
%current_45args49903 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49902)
store volatile %struct.ScmObj* %current_45args49903, %struct.ScmObj** %stackaddr$prim51238, align 8
%stackaddr$prim51239 = alloca %struct.ScmObj*, align 8
%anf_45bind40342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49903)
store volatile %struct.ScmObj* %anf_45bind40342, %struct.ScmObj** %stackaddr$prim51239, align 8
%stackaddr$makeclosure51240 = alloca %struct.ScmObj*, align 8
%fptrToInt51241 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43419 to i64
%ae43419 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt51241)
store volatile %struct.ScmObj* %ae43419, %struct.ScmObj** %stackaddr$makeclosure51240, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43419, %struct.ScmObj* %k40502, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43419, %struct.ScmObj* %lst40179, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43419, %struct.ScmObj* %v40178, i64 2)
%stackaddr$makeclosure51242 = alloca %struct.ScmObj*, align 8
%fptrToInt51243 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43420 to i64
%ae43420 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt51243)
store volatile %struct.ScmObj* %ae43420, %struct.ScmObj** %stackaddr$makeclosure51242, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43420, %struct.ScmObj* %k40502, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43420, %struct.ScmObj* %lst40179, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43420, %struct.ScmObj* %v40178, i64 2)
%args49917$anf_45bind40342$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51244 = alloca %struct.ScmObj*, align 8
%args49917$anf_45bind40342$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43420, %struct.ScmObj* %args49917$anf_45bind40342$0)
store volatile %struct.ScmObj* %args49917$anf_45bind40342$1, %struct.ScmObj** %stackaddr$prim51244, align 8
%stackaddr$prim51245 = alloca %struct.ScmObj*, align 8
%args49917$anf_45bind40342$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43419, %struct.ScmObj* %args49917$anf_45bind40342$1)
store volatile %struct.ScmObj* %args49917$anf_45bind40342$2, %struct.ScmObj** %stackaddr$prim51245, align 8
%clofunc51246 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40342)
musttail call tailcc void %clofunc51246(%struct.ScmObj* %anf_45bind40342, %struct.ScmObj* %args49917$anf_45bind40342$2)
ret void
}

define tailcc void @proc_clo$ae43419(%struct.ScmObj* %env$ae43419,%struct.ScmObj* %current_45args49905) {
%stackaddr$env-ref51247 = alloca %struct.ScmObj*, align 8
%k40502 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43419, i64 0)
store %struct.ScmObj* %k40502, %struct.ScmObj** %stackaddr$env-ref51247
%stackaddr$env-ref51248 = alloca %struct.ScmObj*, align 8
%lst40179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43419, i64 1)
store %struct.ScmObj* %lst40179, %struct.ScmObj** %stackaddr$env-ref51248
%stackaddr$env-ref51249 = alloca %struct.ScmObj*, align 8
%v40178 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43419, i64 2)
store %struct.ScmObj* %v40178, %struct.ScmObj** %stackaddr$env-ref51249
%stackaddr$prim51250 = alloca %struct.ScmObj*, align 8
%_95k40504 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49905)
store volatile %struct.ScmObj* %_95k40504, %struct.ScmObj** %stackaddr$prim51250, align 8
%stackaddr$prim51251 = alloca %struct.ScmObj*, align 8
%current_45args49906 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49905)
store volatile %struct.ScmObj* %current_45args49906, %struct.ScmObj** %stackaddr$prim51251, align 8
%stackaddr$prim51252 = alloca %struct.ScmObj*, align 8
%cc40180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49906)
store volatile %struct.ScmObj* %cc40180, %struct.ScmObj** %stackaddr$prim51252, align 8
%ae43528 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51253 = alloca %struct.ScmObj*, align 8
%anf_45bind40343 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40179, %struct.ScmObj* %ae43528)
store volatile %struct.ScmObj* %anf_45bind40343, %struct.ScmObj** %stackaddr$prim51253, align 8
%stackaddr$prim51254 = alloca %struct.ScmObj*, align 8
%anf_45bind40344 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40343)
store volatile %struct.ScmObj* %anf_45bind40344, %struct.ScmObj** %stackaddr$prim51254, align 8
%truthy$cmp51255 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40344)
%cmp$cmp51255 = icmp eq i64 %truthy$cmp51255, 1
br i1 %cmp$cmp51255, label %truebranch$cmp51255, label %falsebranch$cmp51255
truebranch$cmp51255:
%ae43532 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43533 = call %struct.ScmObj* @const_init_false()
%args49908$k40502$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51256 = alloca %struct.ScmObj*, align 8
%args49908$k40502$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43533, %struct.ScmObj* %args49908$k40502$0)
store volatile %struct.ScmObj* %args49908$k40502$1, %struct.ScmObj** %stackaddr$prim51256, align 8
%stackaddr$prim51257 = alloca %struct.ScmObj*, align 8
%args49908$k40502$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43532, %struct.ScmObj* %args49908$k40502$1)
store volatile %struct.ScmObj* %args49908$k40502$2, %struct.ScmObj** %stackaddr$prim51257, align 8
%clofunc51258 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40502)
musttail call tailcc void %clofunc51258(%struct.ScmObj* %k40502, %struct.ScmObj* %args49908$k40502$2)
ret void
falsebranch$cmp51255:
%ae43541 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51259 = alloca %struct.ScmObj*, align 8
%anf_45bind40345 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40179, %struct.ScmObj* %ae43541)
store volatile %struct.ScmObj* %anf_45bind40345, %struct.ScmObj** %stackaddr$prim51259, align 8
%stackaddr$prim51260 = alloca %struct.ScmObj*, align 8
%anf_45bind40346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40345)
store volatile %struct.ScmObj* %anf_45bind40346, %struct.ScmObj** %stackaddr$prim51260, align 8
%stackaddr$prim51261 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40346, %struct.ScmObj* %v40178)
store volatile %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$prim51261, align 8
%truthy$cmp51262 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40347)
%cmp$cmp51262 = icmp eq i64 %truthy$cmp51262, 1
br i1 %cmp$cmp51262, label %truebranch$cmp51262, label %falsebranch$cmp51262
truebranch$cmp51262:
%ae43547 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51263 = alloca %struct.ScmObj*, align 8
%cpsprim40505 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40179, %struct.ScmObj* %ae43547)
store volatile %struct.ScmObj* %cpsprim40505, %struct.ScmObj** %stackaddr$prim51263, align 8
%ae43549 = call %struct.ScmObj* @const_init_int(i64 0)
%args49909$k40502$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51264 = alloca %struct.ScmObj*, align 8
%args49909$k40502$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40505, %struct.ScmObj* %args49909$k40502$0)
store volatile %struct.ScmObj* %args49909$k40502$1, %struct.ScmObj** %stackaddr$prim51264, align 8
%stackaddr$prim51265 = alloca %struct.ScmObj*, align 8
%args49909$k40502$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43549, %struct.ScmObj* %args49909$k40502$1)
store volatile %struct.ScmObj* %args49909$k40502$2, %struct.ScmObj** %stackaddr$prim51265, align 8
%clofunc51266 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40502)
musttail call tailcc void %clofunc51266(%struct.ScmObj* %k40502, %struct.ScmObj* %args49909$k40502$2)
ret void
falsebranch$cmp51262:
%ae43560 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51267 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40179, %struct.ScmObj* %ae43560)
store volatile %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$prim51267, align 8
%stackaddr$prim51268 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40348)
store volatile %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$prim51268, align 8
%ae43563 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51269 = alloca %struct.ScmObj*, align 8
%_95040182 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40179, %struct.ScmObj* %ae43563, %struct.ScmObj* %anf_45bind40349)
store volatile %struct.ScmObj* %_95040182, %struct.ScmObj** %stackaddr$prim51269, align 8
%args49910$cc40180$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51270 = alloca %struct.ScmObj*, align 8
%args49910$cc40180$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40180, %struct.ScmObj* %args49910$cc40180$0)
store volatile %struct.ScmObj* %args49910$cc40180$1, %struct.ScmObj** %stackaddr$prim51270, align 8
%stackaddr$prim51271 = alloca %struct.ScmObj*, align 8
%args49910$cc40180$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40502, %struct.ScmObj* %args49910$cc40180$1)
store volatile %struct.ScmObj* %args49910$cc40180$2, %struct.ScmObj** %stackaddr$prim51271, align 8
%clofunc51272 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40180)
musttail call tailcc void %clofunc51272(%struct.ScmObj* %cc40180, %struct.ScmObj* %args49910$cc40180$2)
ret void
}

define tailcc void @proc_clo$ae43420(%struct.ScmObj* %env$ae43420,%struct.ScmObj* %current_45args49911) {
%stackaddr$env-ref51273 = alloca %struct.ScmObj*, align 8
%k40502 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43420, i64 0)
store %struct.ScmObj* %k40502, %struct.ScmObj** %stackaddr$env-ref51273
%stackaddr$env-ref51274 = alloca %struct.ScmObj*, align 8
%lst40179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43420, i64 1)
store %struct.ScmObj* %lst40179, %struct.ScmObj** %stackaddr$env-ref51274
%stackaddr$env-ref51275 = alloca %struct.ScmObj*, align 8
%v40178 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43420, i64 2)
store %struct.ScmObj* %v40178, %struct.ScmObj** %stackaddr$env-ref51275
%stackaddr$prim51276 = alloca %struct.ScmObj*, align 8
%_95k40504 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49911)
store volatile %struct.ScmObj* %_95k40504, %struct.ScmObj** %stackaddr$prim51276, align 8
%stackaddr$prim51277 = alloca %struct.ScmObj*, align 8
%current_45args49912 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49911)
store volatile %struct.ScmObj* %current_45args49912, %struct.ScmObj** %stackaddr$prim51277, align 8
%stackaddr$prim51278 = alloca %struct.ScmObj*, align 8
%cc40180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49912)
store volatile %struct.ScmObj* %cc40180, %struct.ScmObj** %stackaddr$prim51278, align 8
%ae43422 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51279 = alloca %struct.ScmObj*, align 8
%anf_45bind40343 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40179, %struct.ScmObj* %ae43422)
store volatile %struct.ScmObj* %anf_45bind40343, %struct.ScmObj** %stackaddr$prim51279, align 8
%stackaddr$prim51280 = alloca %struct.ScmObj*, align 8
%anf_45bind40344 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40343)
store volatile %struct.ScmObj* %anf_45bind40344, %struct.ScmObj** %stackaddr$prim51280, align 8
%truthy$cmp51281 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40344)
%cmp$cmp51281 = icmp eq i64 %truthy$cmp51281, 1
br i1 %cmp$cmp51281, label %truebranch$cmp51281, label %falsebranch$cmp51281
truebranch$cmp51281:
%ae43426 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43427 = call %struct.ScmObj* @const_init_false()
%args49914$k40502$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51282 = alloca %struct.ScmObj*, align 8
%args49914$k40502$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43427, %struct.ScmObj* %args49914$k40502$0)
store volatile %struct.ScmObj* %args49914$k40502$1, %struct.ScmObj** %stackaddr$prim51282, align 8
%stackaddr$prim51283 = alloca %struct.ScmObj*, align 8
%args49914$k40502$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43426, %struct.ScmObj* %args49914$k40502$1)
store volatile %struct.ScmObj* %args49914$k40502$2, %struct.ScmObj** %stackaddr$prim51283, align 8
%clofunc51284 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40502)
musttail call tailcc void %clofunc51284(%struct.ScmObj* %k40502, %struct.ScmObj* %args49914$k40502$2)
ret void
falsebranch$cmp51281:
%ae43435 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51285 = alloca %struct.ScmObj*, align 8
%anf_45bind40345 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40179, %struct.ScmObj* %ae43435)
store volatile %struct.ScmObj* %anf_45bind40345, %struct.ScmObj** %stackaddr$prim51285, align 8
%stackaddr$prim51286 = alloca %struct.ScmObj*, align 8
%anf_45bind40346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40345)
store volatile %struct.ScmObj* %anf_45bind40346, %struct.ScmObj** %stackaddr$prim51286, align 8
%stackaddr$prim51287 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40346, %struct.ScmObj* %v40178)
store volatile %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$prim51287, align 8
%truthy$cmp51288 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40347)
%cmp$cmp51288 = icmp eq i64 %truthy$cmp51288, 1
br i1 %cmp$cmp51288, label %truebranch$cmp51288, label %falsebranch$cmp51288
truebranch$cmp51288:
%ae43441 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51289 = alloca %struct.ScmObj*, align 8
%cpsprim40505 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40179, %struct.ScmObj* %ae43441)
store volatile %struct.ScmObj* %cpsprim40505, %struct.ScmObj** %stackaddr$prim51289, align 8
%ae43443 = call %struct.ScmObj* @const_init_int(i64 0)
%args49915$k40502$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51290 = alloca %struct.ScmObj*, align 8
%args49915$k40502$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40505, %struct.ScmObj* %args49915$k40502$0)
store volatile %struct.ScmObj* %args49915$k40502$1, %struct.ScmObj** %stackaddr$prim51290, align 8
%stackaddr$prim51291 = alloca %struct.ScmObj*, align 8
%args49915$k40502$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43443, %struct.ScmObj* %args49915$k40502$1)
store volatile %struct.ScmObj* %args49915$k40502$2, %struct.ScmObj** %stackaddr$prim51291, align 8
%clofunc51292 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40502)
musttail call tailcc void %clofunc51292(%struct.ScmObj* %k40502, %struct.ScmObj* %args49915$k40502$2)
ret void
falsebranch$cmp51288:
%ae43454 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51293 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40179, %struct.ScmObj* %ae43454)
store volatile %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$prim51293, align 8
%stackaddr$prim51294 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40348)
store volatile %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$prim51294, align 8
%ae43457 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51295 = alloca %struct.ScmObj*, align 8
%_95040182 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40179, %struct.ScmObj* %ae43457, %struct.ScmObj* %anf_45bind40349)
store volatile %struct.ScmObj* %_95040182, %struct.ScmObj** %stackaddr$prim51295, align 8
%args49916$cc40180$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51296 = alloca %struct.ScmObj*, align 8
%args49916$cc40180$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40180, %struct.ScmObj* %args49916$cc40180$0)
store volatile %struct.ScmObj* %args49916$cc40180$1, %struct.ScmObj** %stackaddr$prim51296, align 8
%stackaddr$prim51297 = alloca %struct.ScmObj*, align 8
%args49916$cc40180$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40502, %struct.ScmObj* %args49916$cc40180$1)
store volatile %struct.ScmObj* %args49916$cc40180$2, %struct.ScmObj** %stackaddr$prim51297, align 8
%clofunc51298 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40180)
musttail call tailcc void %clofunc51298(%struct.ScmObj* %cc40180, %struct.ScmObj* %args49916$cc40180$2)
ret void
}

define tailcc void @proc_clo$ae43405(%struct.ScmObj* %env$ae43405,%struct.ScmObj* %current_45args49918) {
%stackaddr$prim51299 = alloca %struct.ScmObj*, align 8
%k40506 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49918)
store volatile %struct.ScmObj* %k40506, %struct.ScmObj** %stackaddr$prim51299, align 8
%stackaddr$prim51300 = alloca %struct.ScmObj*, align 8
%current_45args49919 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49918)
store volatile %struct.ScmObj* %current_45args49919, %struct.ScmObj** %stackaddr$prim51300, align 8
%stackaddr$prim51301 = alloca %struct.ScmObj*, align 8
%u40181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49919)
store volatile %struct.ScmObj* %u40181, %struct.ScmObj** %stackaddr$prim51301, align 8
%args49921$u40181$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51302 = alloca %struct.ScmObj*, align 8
%args49921$u40181$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40181, %struct.ScmObj* %args49921$u40181$0)
store volatile %struct.ScmObj* %args49921$u40181$1, %struct.ScmObj** %stackaddr$prim51302, align 8
%stackaddr$prim51303 = alloca %struct.ScmObj*, align 8
%args49921$u40181$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40506, %struct.ScmObj* %args49921$u40181$1)
store volatile %struct.ScmObj* %args49921$u40181$2, %struct.ScmObj** %stackaddr$prim51303, align 8
%clofunc51304 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40181)
musttail call tailcc void %clofunc51304(%struct.ScmObj* %u40181, %struct.ScmObj* %args49921$u40181$2)
ret void
}

define tailcc void @proc_clo$ae42864(%struct.ScmObj* %env$ae42864,%struct.ScmObj* %current_45args49924) {
%stackaddr$prim51305 = alloca %struct.ScmObj*, align 8
%k40507 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49924)
store volatile %struct.ScmObj* %k40507, %struct.ScmObj** %stackaddr$prim51305, align 8
%stackaddr$prim51306 = alloca %struct.ScmObj*, align 8
%current_45args49925 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49924)
store volatile %struct.ScmObj* %current_45args49925, %struct.ScmObj** %stackaddr$prim51306, align 8
%stackaddr$prim51307 = alloca %struct.ScmObj*, align 8
%lst40185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49925)
store volatile %struct.ScmObj* %lst40185, %struct.ScmObj** %stackaddr$prim51307, align 8
%stackaddr$prim51308 = alloca %struct.ScmObj*, align 8
%current_45args49926 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49925)
store volatile %struct.ScmObj* %current_45args49926, %struct.ScmObj** %stackaddr$prim51308, align 8
%stackaddr$prim51309 = alloca %struct.ScmObj*, align 8
%n40184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49926)
store volatile %struct.ScmObj* %n40184, %struct.ScmObj** %stackaddr$prim51309, align 8
%ae42865 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim51310 = alloca %struct.ScmObj*, align 8
%n40187 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42865, %struct.ScmObj* %n40184)
store volatile %struct.ScmObj* %n40187, %struct.ScmObj** %stackaddr$prim51310, align 8
%ae42867 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim51311 = alloca %struct.ScmObj*, align 8
%lst40186 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42867, %struct.ScmObj* %lst40185)
store volatile %struct.ScmObj* %lst40186, %struct.ScmObj** %stackaddr$prim51311, align 8
%stackaddr$makeclosure51312 = alloca %struct.ScmObj*, align 8
%fptrToInt51313 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42869 to i64
%ae42869 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt51313)
store volatile %struct.ScmObj* %ae42869, %struct.ScmObj** %stackaddr$makeclosure51312, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42869, %struct.ScmObj* %k40507, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42869, %struct.ScmObj* %n40187, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42869, %struct.ScmObj* %lst40186, i64 2)
%ae42870 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51314 = alloca %struct.ScmObj*, align 8
%fptrToInt51315 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42871 to i64
%ae42871 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt51315)
store volatile %struct.ScmObj* %ae42871, %struct.ScmObj** %stackaddr$makeclosure51314, align 8
%args49946$ae42869$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51316 = alloca %struct.ScmObj*, align 8
%args49946$ae42869$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42871, %struct.ScmObj* %args49946$ae42869$0)
store volatile %struct.ScmObj* %args49946$ae42869$1, %struct.ScmObj** %stackaddr$prim51316, align 8
%stackaddr$prim51317 = alloca %struct.ScmObj*, align 8
%args49946$ae42869$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42870, %struct.ScmObj* %args49946$ae42869$1)
store volatile %struct.ScmObj* %args49946$ae42869$2, %struct.ScmObj** %stackaddr$prim51317, align 8
%clofunc51318 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42869)
musttail call tailcc void %clofunc51318(%struct.ScmObj* %ae42869, %struct.ScmObj* %args49946$ae42869$2)
ret void
}

define tailcc void @proc_clo$ae42869(%struct.ScmObj* %env$ae42869,%struct.ScmObj* %current_45args49928) {
%stackaddr$env-ref51319 = alloca %struct.ScmObj*, align 8
%k40507 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42869, i64 0)
store %struct.ScmObj* %k40507, %struct.ScmObj** %stackaddr$env-ref51319
%stackaddr$env-ref51320 = alloca %struct.ScmObj*, align 8
%n40187 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42869, i64 1)
store %struct.ScmObj* %n40187, %struct.ScmObj** %stackaddr$env-ref51320
%stackaddr$env-ref51321 = alloca %struct.ScmObj*, align 8
%lst40186 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42869, i64 2)
store %struct.ScmObj* %lst40186, %struct.ScmObj** %stackaddr$env-ref51321
%stackaddr$prim51322 = alloca %struct.ScmObj*, align 8
%_95k40508 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49928)
store volatile %struct.ScmObj* %_95k40508, %struct.ScmObj** %stackaddr$prim51322, align 8
%stackaddr$prim51323 = alloca %struct.ScmObj*, align 8
%current_45args49929 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49928)
store volatile %struct.ScmObj* %current_45args49929, %struct.ScmObj** %stackaddr$prim51323, align 8
%stackaddr$prim51324 = alloca %struct.ScmObj*, align 8
%anf_45bind40335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49929)
store volatile %struct.ScmObj* %anf_45bind40335, %struct.ScmObj** %stackaddr$prim51324, align 8
%stackaddr$makeclosure51325 = alloca %struct.ScmObj*, align 8
%fptrToInt51326 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42885 to i64
%ae42885 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt51326)
store volatile %struct.ScmObj* %ae42885, %struct.ScmObj** %stackaddr$makeclosure51325, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42885, %struct.ScmObj* %k40507, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42885, %struct.ScmObj* %n40187, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42885, %struct.ScmObj* %lst40186, i64 2)
%stackaddr$makeclosure51327 = alloca %struct.ScmObj*, align 8
%fptrToInt51328 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42886 to i64
%ae42886 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt51328)
store volatile %struct.ScmObj* %ae42886, %struct.ScmObj** %stackaddr$makeclosure51327, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42886, %struct.ScmObj* %k40507, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42886, %struct.ScmObj* %n40187, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42886, %struct.ScmObj* %lst40186, i64 2)
%args49941$anf_45bind40335$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51329 = alloca %struct.ScmObj*, align 8
%args49941$anf_45bind40335$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42886, %struct.ScmObj* %args49941$anf_45bind40335$0)
store volatile %struct.ScmObj* %args49941$anf_45bind40335$1, %struct.ScmObj** %stackaddr$prim51329, align 8
%stackaddr$prim51330 = alloca %struct.ScmObj*, align 8
%args49941$anf_45bind40335$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42885, %struct.ScmObj* %args49941$anf_45bind40335$1)
store volatile %struct.ScmObj* %args49941$anf_45bind40335$2, %struct.ScmObj** %stackaddr$prim51330, align 8
%clofunc51331 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40335)
musttail call tailcc void %clofunc51331(%struct.ScmObj* %anf_45bind40335, %struct.ScmObj* %args49941$anf_45bind40335$2)
ret void
}

define tailcc void @proc_clo$ae42885(%struct.ScmObj* %env$ae42885,%struct.ScmObj* %current_45args49931) {
%stackaddr$env-ref51332 = alloca %struct.ScmObj*, align 8
%k40507 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42885, i64 0)
store %struct.ScmObj* %k40507, %struct.ScmObj** %stackaddr$env-ref51332
%stackaddr$env-ref51333 = alloca %struct.ScmObj*, align 8
%n40187 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42885, i64 1)
store %struct.ScmObj* %n40187, %struct.ScmObj** %stackaddr$env-ref51333
%stackaddr$env-ref51334 = alloca %struct.ScmObj*, align 8
%lst40186 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42885, i64 2)
store %struct.ScmObj* %lst40186, %struct.ScmObj** %stackaddr$env-ref51334
%stackaddr$prim51335 = alloca %struct.ScmObj*, align 8
%_95k40509 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49931)
store volatile %struct.ScmObj* %_95k40509, %struct.ScmObj** %stackaddr$prim51335, align 8
%stackaddr$prim51336 = alloca %struct.ScmObj*, align 8
%current_45args49932 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49931)
store volatile %struct.ScmObj* %current_45args49932, %struct.ScmObj** %stackaddr$prim51336, align 8
%stackaddr$prim51337 = alloca %struct.ScmObj*, align 8
%cc40188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49932)
store volatile %struct.ScmObj* %cc40188, %struct.ScmObj** %stackaddr$prim51337, align 8
%ae43028 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51338 = alloca %struct.ScmObj*, align 8
%anf_45bind40336 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40187, %struct.ScmObj* %ae43028)
store volatile %struct.ScmObj* %anf_45bind40336, %struct.ScmObj** %stackaddr$prim51338, align 8
%ae43029 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51339 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae43029, %struct.ScmObj* %anf_45bind40336)
store volatile %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$prim51339, align 8
%truthy$cmp51340 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40337)
%cmp$cmp51340 = icmp eq i64 %truthy$cmp51340, 1
br i1 %cmp$cmp51340, label %truebranch$cmp51340, label %falsebranch$cmp51340
truebranch$cmp51340:
%ae43033 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51341 = alloca %struct.ScmObj*, align 8
%cpsprim40510 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40186, %struct.ScmObj* %ae43033)
store volatile %struct.ScmObj* %cpsprim40510, %struct.ScmObj** %stackaddr$prim51341, align 8
%ae43035 = call %struct.ScmObj* @const_init_int(i64 0)
%args49934$k40507$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51342 = alloca %struct.ScmObj*, align 8
%args49934$k40507$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40510, %struct.ScmObj* %args49934$k40507$0)
store volatile %struct.ScmObj* %args49934$k40507$1, %struct.ScmObj** %stackaddr$prim51342, align 8
%stackaddr$prim51343 = alloca %struct.ScmObj*, align 8
%args49934$k40507$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43035, %struct.ScmObj* %args49934$k40507$1)
store volatile %struct.ScmObj* %args49934$k40507$2, %struct.ScmObj** %stackaddr$prim51343, align 8
%clofunc51344 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40507)
musttail call tailcc void %clofunc51344(%struct.ScmObj* %k40507, %struct.ScmObj* %args49934$k40507$2)
ret void
falsebranch$cmp51340:
%ae43046 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51345 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40186, %struct.ScmObj* %ae43046)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim51345, align 8
%stackaddr$prim51346 = alloca %struct.ScmObj*, align 8
%anf_45bind40339 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40338)
store volatile %struct.ScmObj* %anf_45bind40339, %struct.ScmObj** %stackaddr$prim51346, align 8
%ae43049 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51347 = alloca %struct.ScmObj*, align 8
%_95040191 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40186, %struct.ScmObj* %ae43049, %struct.ScmObj* %anf_45bind40339)
store volatile %struct.ScmObj* %_95040191, %struct.ScmObj** %stackaddr$prim51347, align 8
%ae43052 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51348 = alloca %struct.ScmObj*, align 8
%anf_45bind40340 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40187, %struct.ScmObj* %ae43052)
store volatile %struct.ScmObj* %anf_45bind40340, %struct.ScmObj** %stackaddr$prim51348, align 8
%ae43054 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim51349 = alloca %struct.ScmObj*, align 8
%anf_45bind40341 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40340, %struct.ScmObj* %ae43054)
store volatile %struct.ScmObj* %anf_45bind40341, %struct.ScmObj** %stackaddr$prim51349, align 8
%ae43056 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51350 = alloca %struct.ScmObj*, align 8
%_95140190 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40187, %struct.ScmObj* %ae43056, %struct.ScmObj* %anf_45bind40341)
store volatile %struct.ScmObj* %_95140190, %struct.ScmObj** %stackaddr$prim51350, align 8
%args49935$cc40188$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51351 = alloca %struct.ScmObj*, align 8
%args49935$cc40188$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40188, %struct.ScmObj* %args49935$cc40188$0)
store volatile %struct.ScmObj* %args49935$cc40188$1, %struct.ScmObj** %stackaddr$prim51351, align 8
%stackaddr$prim51352 = alloca %struct.ScmObj*, align 8
%args49935$cc40188$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40507, %struct.ScmObj* %args49935$cc40188$1)
store volatile %struct.ScmObj* %args49935$cc40188$2, %struct.ScmObj** %stackaddr$prim51352, align 8
%clofunc51353 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40188)
musttail call tailcc void %clofunc51353(%struct.ScmObj* %cc40188, %struct.ScmObj* %args49935$cc40188$2)
ret void
}

define tailcc void @proc_clo$ae42886(%struct.ScmObj* %env$ae42886,%struct.ScmObj* %current_45args49936) {
%stackaddr$env-ref51354 = alloca %struct.ScmObj*, align 8
%k40507 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42886, i64 0)
store %struct.ScmObj* %k40507, %struct.ScmObj** %stackaddr$env-ref51354
%stackaddr$env-ref51355 = alloca %struct.ScmObj*, align 8
%n40187 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42886, i64 1)
store %struct.ScmObj* %n40187, %struct.ScmObj** %stackaddr$env-ref51355
%stackaddr$env-ref51356 = alloca %struct.ScmObj*, align 8
%lst40186 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42886, i64 2)
store %struct.ScmObj* %lst40186, %struct.ScmObj** %stackaddr$env-ref51356
%stackaddr$prim51357 = alloca %struct.ScmObj*, align 8
%_95k40509 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49936)
store volatile %struct.ScmObj* %_95k40509, %struct.ScmObj** %stackaddr$prim51357, align 8
%stackaddr$prim51358 = alloca %struct.ScmObj*, align 8
%current_45args49937 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49936)
store volatile %struct.ScmObj* %current_45args49937, %struct.ScmObj** %stackaddr$prim51358, align 8
%stackaddr$prim51359 = alloca %struct.ScmObj*, align 8
%cc40188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49937)
store volatile %struct.ScmObj* %cc40188, %struct.ScmObj** %stackaddr$prim51359, align 8
%ae42888 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51360 = alloca %struct.ScmObj*, align 8
%anf_45bind40336 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40187, %struct.ScmObj* %ae42888)
store volatile %struct.ScmObj* %anf_45bind40336, %struct.ScmObj** %stackaddr$prim51360, align 8
%ae42889 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51361 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42889, %struct.ScmObj* %anf_45bind40336)
store volatile %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$prim51361, align 8
%truthy$cmp51362 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40337)
%cmp$cmp51362 = icmp eq i64 %truthy$cmp51362, 1
br i1 %cmp$cmp51362, label %truebranch$cmp51362, label %falsebranch$cmp51362
truebranch$cmp51362:
%ae42893 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51363 = alloca %struct.ScmObj*, align 8
%cpsprim40510 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40186, %struct.ScmObj* %ae42893)
store volatile %struct.ScmObj* %cpsprim40510, %struct.ScmObj** %stackaddr$prim51363, align 8
%ae42895 = call %struct.ScmObj* @const_init_int(i64 0)
%args49939$k40507$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51364 = alloca %struct.ScmObj*, align 8
%args49939$k40507$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40510, %struct.ScmObj* %args49939$k40507$0)
store volatile %struct.ScmObj* %args49939$k40507$1, %struct.ScmObj** %stackaddr$prim51364, align 8
%stackaddr$prim51365 = alloca %struct.ScmObj*, align 8
%args49939$k40507$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42895, %struct.ScmObj* %args49939$k40507$1)
store volatile %struct.ScmObj* %args49939$k40507$2, %struct.ScmObj** %stackaddr$prim51365, align 8
%clofunc51366 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40507)
musttail call tailcc void %clofunc51366(%struct.ScmObj* %k40507, %struct.ScmObj* %args49939$k40507$2)
ret void
falsebranch$cmp51362:
%ae42906 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51367 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40186, %struct.ScmObj* %ae42906)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim51367, align 8
%stackaddr$prim51368 = alloca %struct.ScmObj*, align 8
%anf_45bind40339 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40338)
store volatile %struct.ScmObj* %anf_45bind40339, %struct.ScmObj** %stackaddr$prim51368, align 8
%ae42909 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51369 = alloca %struct.ScmObj*, align 8
%_95040191 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40186, %struct.ScmObj* %ae42909, %struct.ScmObj* %anf_45bind40339)
store volatile %struct.ScmObj* %_95040191, %struct.ScmObj** %stackaddr$prim51369, align 8
%ae42912 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51370 = alloca %struct.ScmObj*, align 8
%anf_45bind40340 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40187, %struct.ScmObj* %ae42912)
store volatile %struct.ScmObj* %anf_45bind40340, %struct.ScmObj** %stackaddr$prim51370, align 8
%ae42914 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim51371 = alloca %struct.ScmObj*, align 8
%anf_45bind40341 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40340, %struct.ScmObj* %ae42914)
store volatile %struct.ScmObj* %anf_45bind40341, %struct.ScmObj** %stackaddr$prim51371, align 8
%ae42916 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51372 = alloca %struct.ScmObj*, align 8
%_95140190 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40187, %struct.ScmObj* %ae42916, %struct.ScmObj* %anf_45bind40341)
store volatile %struct.ScmObj* %_95140190, %struct.ScmObj** %stackaddr$prim51372, align 8
%args49940$cc40188$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51373 = alloca %struct.ScmObj*, align 8
%args49940$cc40188$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40188, %struct.ScmObj* %args49940$cc40188$0)
store volatile %struct.ScmObj* %args49940$cc40188$1, %struct.ScmObj** %stackaddr$prim51373, align 8
%stackaddr$prim51374 = alloca %struct.ScmObj*, align 8
%args49940$cc40188$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40507, %struct.ScmObj* %args49940$cc40188$1)
store volatile %struct.ScmObj* %args49940$cc40188$2, %struct.ScmObj** %stackaddr$prim51374, align 8
%clofunc51375 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40188)
musttail call tailcc void %clofunc51375(%struct.ScmObj* %cc40188, %struct.ScmObj* %args49940$cc40188$2)
ret void
}

define tailcc void @proc_clo$ae42871(%struct.ScmObj* %env$ae42871,%struct.ScmObj* %current_45args49942) {
%stackaddr$prim51376 = alloca %struct.ScmObj*, align 8
%k40511 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49942)
store volatile %struct.ScmObj* %k40511, %struct.ScmObj** %stackaddr$prim51376, align 8
%stackaddr$prim51377 = alloca %struct.ScmObj*, align 8
%current_45args49943 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49942)
store volatile %struct.ScmObj* %current_45args49943, %struct.ScmObj** %stackaddr$prim51377, align 8
%stackaddr$prim51378 = alloca %struct.ScmObj*, align 8
%u40189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49943)
store volatile %struct.ScmObj* %u40189, %struct.ScmObj** %stackaddr$prim51378, align 8
%args49945$u40189$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51379 = alloca %struct.ScmObj*, align 8
%args49945$u40189$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40189, %struct.ScmObj* %args49945$u40189$0)
store volatile %struct.ScmObj* %args49945$u40189$1, %struct.ScmObj** %stackaddr$prim51379, align 8
%stackaddr$prim51380 = alloca %struct.ScmObj*, align 8
%args49945$u40189$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40511, %struct.ScmObj* %args49945$u40189$1)
store volatile %struct.ScmObj* %args49945$u40189$2, %struct.ScmObj** %stackaddr$prim51380, align 8
%clofunc51381 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40189)
musttail call tailcc void %clofunc51381(%struct.ScmObj* %u40189, %struct.ScmObj* %args49945$u40189$2)
ret void
}

define tailcc void @proc_clo$ae42448(%struct.ScmObj* %env$ae42448,%struct.ScmObj* %current_45args49948) {
%stackaddr$prim51382 = alloca %struct.ScmObj*, align 8
%k40512 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49948)
store volatile %struct.ScmObj* %k40512, %struct.ScmObj** %stackaddr$prim51382, align 8
%stackaddr$prim51383 = alloca %struct.ScmObj*, align 8
%current_45args49949 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49948)
store volatile %struct.ScmObj* %current_45args49949, %struct.ScmObj** %stackaddr$prim51383, align 8
%stackaddr$prim51384 = alloca %struct.ScmObj*, align 8
%a40193 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49949)
store volatile %struct.ScmObj* %a40193, %struct.ScmObj** %stackaddr$prim51384, align 8
%ae42449 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim51385 = alloca %struct.ScmObj*, align 8
%a40194 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42449, %struct.ScmObj* %a40193)
store volatile %struct.ScmObj* %a40194, %struct.ScmObj** %stackaddr$prim51385, align 8
%stackaddr$makeclosure51386 = alloca %struct.ScmObj*, align 8
%fptrToInt51387 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42451 to i64
%ae42451 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt51387)
store volatile %struct.ScmObj* %ae42451, %struct.ScmObj** %stackaddr$makeclosure51386, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42451, %struct.ScmObj* %a40194, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42451, %struct.ScmObj* %k40512, i64 1)
%ae42452 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51388 = alloca %struct.ScmObj*, align 8
%fptrToInt51389 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42453 to i64
%ae42453 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt51389)
store volatile %struct.ScmObj* %ae42453, %struct.ScmObj** %stackaddr$makeclosure51388, align 8
%args49971$ae42451$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51390 = alloca %struct.ScmObj*, align 8
%args49971$ae42451$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42453, %struct.ScmObj* %args49971$ae42451$0)
store volatile %struct.ScmObj* %args49971$ae42451$1, %struct.ScmObj** %stackaddr$prim51390, align 8
%stackaddr$prim51391 = alloca %struct.ScmObj*, align 8
%args49971$ae42451$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42452, %struct.ScmObj* %args49971$ae42451$1)
store volatile %struct.ScmObj* %args49971$ae42451$2, %struct.ScmObj** %stackaddr$prim51391, align 8
%clofunc51392 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42451)
musttail call tailcc void %clofunc51392(%struct.ScmObj* %ae42451, %struct.ScmObj* %args49971$ae42451$2)
ret void
}

define tailcc void @proc_clo$ae42451(%struct.ScmObj* %env$ae42451,%struct.ScmObj* %current_45args49951) {
%stackaddr$env-ref51393 = alloca %struct.ScmObj*, align 8
%a40194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42451, i64 0)
store %struct.ScmObj* %a40194, %struct.ScmObj** %stackaddr$env-ref51393
%stackaddr$env-ref51394 = alloca %struct.ScmObj*, align 8
%k40512 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42451, i64 1)
store %struct.ScmObj* %k40512, %struct.ScmObj** %stackaddr$env-ref51394
%stackaddr$prim51395 = alloca %struct.ScmObj*, align 8
%_95k40513 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49951)
store volatile %struct.ScmObj* %_95k40513, %struct.ScmObj** %stackaddr$prim51395, align 8
%stackaddr$prim51396 = alloca %struct.ScmObj*, align 8
%current_45args49952 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49951)
store volatile %struct.ScmObj* %current_45args49952, %struct.ScmObj** %stackaddr$prim51396, align 8
%stackaddr$prim51397 = alloca %struct.ScmObj*, align 8
%anf_45bind40327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49952)
store volatile %struct.ScmObj* %anf_45bind40327, %struct.ScmObj** %stackaddr$prim51397, align 8
%stackaddr$makeclosure51398 = alloca %struct.ScmObj*, align 8
%fptrToInt51399 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42470 to i64
%ae42470 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt51399)
store volatile %struct.ScmObj* %ae42470, %struct.ScmObj** %stackaddr$makeclosure51398, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42470, %struct.ScmObj* %a40194, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42470, %struct.ScmObj* %k40512, i64 1)
%stackaddr$makeclosure51400 = alloca %struct.ScmObj*, align 8
%fptrToInt51401 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42471 to i64
%ae42471 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt51401)
store volatile %struct.ScmObj* %ae42471, %struct.ScmObj** %stackaddr$makeclosure51400, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42471, %struct.ScmObj* %a40194, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42471, %struct.ScmObj* %k40512, i64 1)
%args49966$anf_45bind40327$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51402 = alloca %struct.ScmObj*, align 8
%args49966$anf_45bind40327$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42471, %struct.ScmObj* %args49966$anf_45bind40327$0)
store volatile %struct.ScmObj* %args49966$anf_45bind40327$1, %struct.ScmObj** %stackaddr$prim51402, align 8
%stackaddr$prim51403 = alloca %struct.ScmObj*, align 8
%args49966$anf_45bind40327$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42470, %struct.ScmObj* %args49966$anf_45bind40327$1)
store volatile %struct.ScmObj* %args49966$anf_45bind40327$2, %struct.ScmObj** %stackaddr$prim51403, align 8
%clofunc51404 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40327)
musttail call tailcc void %clofunc51404(%struct.ScmObj* %anf_45bind40327, %struct.ScmObj* %args49966$anf_45bind40327$2)
ret void
}

define tailcc void @proc_clo$ae42470(%struct.ScmObj* %env$ae42470,%struct.ScmObj* %current_45args49954) {
%stackaddr$env-ref51405 = alloca %struct.ScmObj*, align 8
%a40194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42470, i64 0)
store %struct.ScmObj* %a40194, %struct.ScmObj** %stackaddr$env-ref51405
%stackaddr$env-ref51406 = alloca %struct.ScmObj*, align 8
%k40512 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42470, i64 1)
store %struct.ScmObj* %k40512, %struct.ScmObj** %stackaddr$env-ref51406
%stackaddr$prim51407 = alloca %struct.ScmObj*, align 8
%_95k40514 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49954)
store volatile %struct.ScmObj* %_95k40514, %struct.ScmObj** %stackaddr$prim51407, align 8
%stackaddr$prim51408 = alloca %struct.ScmObj*, align 8
%current_45args49955 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49954)
store volatile %struct.ScmObj* %current_45args49955, %struct.ScmObj** %stackaddr$prim51408, align 8
%stackaddr$prim51409 = alloca %struct.ScmObj*, align 8
%cc40195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49955)
store volatile %struct.ScmObj* %cc40195, %struct.ScmObj** %stackaddr$prim51409, align 8
%ae42586 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51410 = alloca %struct.ScmObj*, align 8
%anf_45bind40328 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40194, %struct.ScmObj* %ae42586)
store volatile %struct.ScmObj* %anf_45bind40328, %struct.ScmObj** %stackaddr$prim51410, align 8
%stackaddr$prim51411 = alloca %struct.ScmObj*, align 8
%anf_45bind40329 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40328)
store volatile %struct.ScmObj* %anf_45bind40329, %struct.ScmObj** %stackaddr$prim51411, align 8
%truthy$cmp51412 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40329)
%cmp$cmp51412 = icmp eq i64 %truthy$cmp51412, 1
br i1 %cmp$cmp51412, label %truebranch$cmp51412, label %falsebranch$cmp51412
truebranch$cmp51412:
%ae42590 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42591 = call %struct.ScmObj* @const_init_true()
%args49957$k40512$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51413 = alloca %struct.ScmObj*, align 8
%args49957$k40512$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42591, %struct.ScmObj* %args49957$k40512$0)
store volatile %struct.ScmObj* %args49957$k40512$1, %struct.ScmObj** %stackaddr$prim51413, align 8
%stackaddr$prim51414 = alloca %struct.ScmObj*, align 8
%args49957$k40512$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42590, %struct.ScmObj* %args49957$k40512$1)
store volatile %struct.ScmObj* %args49957$k40512$2, %struct.ScmObj** %stackaddr$prim51414, align 8
%clofunc51415 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40512)
musttail call tailcc void %clofunc51415(%struct.ScmObj* %k40512, %struct.ScmObj* %args49957$k40512$2)
ret void
falsebranch$cmp51412:
%ae42599 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51416 = alloca %struct.ScmObj*, align 8
%anf_45bind40330 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40194, %struct.ScmObj* %ae42599)
store volatile %struct.ScmObj* %anf_45bind40330, %struct.ScmObj** %stackaddr$prim51416, align 8
%stackaddr$prim51417 = alloca %struct.ScmObj*, align 8
%anf_45bind40331 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40330)
store volatile %struct.ScmObj* %anf_45bind40331, %struct.ScmObj** %stackaddr$prim51417, align 8
%truthy$cmp51418 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40331)
%cmp$cmp51418 = icmp eq i64 %truthy$cmp51418, 1
br i1 %cmp$cmp51418, label %truebranch$cmp51418, label %falsebranch$cmp51418
truebranch$cmp51418:
%ae42603 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51419 = alloca %struct.ScmObj*, align 8
%anf_45bind40332 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40194, %struct.ScmObj* %ae42603)
store volatile %struct.ScmObj* %anf_45bind40332, %struct.ScmObj** %stackaddr$prim51419, align 8
%stackaddr$prim51420 = alloca %struct.ScmObj*, align 8
%b40197 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40332)
store volatile %struct.ScmObj* %b40197, %struct.ScmObj** %stackaddr$prim51420, align 8
%ae42606 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51421 = alloca %struct.ScmObj*, align 8
%anf_45bind40333 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40194, %struct.ScmObj* %ae42606)
store volatile %struct.ScmObj* %anf_45bind40333, %struct.ScmObj** %stackaddr$prim51421, align 8
%stackaddr$prim51422 = alloca %struct.ScmObj*, align 8
%anf_45bind40334 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40333)
store volatile %struct.ScmObj* %anf_45bind40334, %struct.ScmObj** %stackaddr$prim51422, align 8
%ae42609 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51423 = alloca %struct.ScmObj*, align 8
%_95040198 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40194, %struct.ScmObj* %ae42609, %struct.ScmObj* %anf_45bind40334)
store volatile %struct.ScmObj* %_95040198, %struct.ScmObj** %stackaddr$prim51423, align 8
%args49958$cc40195$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51424 = alloca %struct.ScmObj*, align 8
%args49958$cc40195$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40195, %struct.ScmObj* %args49958$cc40195$0)
store volatile %struct.ScmObj* %args49958$cc40195$1, %struct.ScmObj** %stackaddr$prim51424, align 8
%stackaddr$prim51425 = alloca %struct.ScmObj*, align 8
%args49958$cc40195$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40512, %struct.ScmObj* %args49958$cc40195$1)
store volatile %struct.ScmObj* %args49958$cc40195$2, %struct.ScmObj** %stackaddr$prim51425, align 8
%clofunc51426 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40195)
musttail call tailcc void %clofunc51426(%struct.ScmObj* %cc40195, %struct.ScmObj* %args49958$cc40195$2)
ret void
falsebranch$cmp51418:
%ae42642 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42643 = call %struct.ScmObj* @const_init_false()
%args49959$k40512$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51427 = alloca %struct.ScmObj*, align 8
%args49959$k40512$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42643, %struct.ScmObj* %args49959$k40512$0)
store volatile %struct.ScmObj* %args49959$k40512$1, %struct.ScmObj** %stackaddr$prim51427, align 8
%stackaddr$prim51428 = alloca %struct.ScmObj*, align 8
%args49959$k40512$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42642, %struct.ScmObj* %args49959$k40512$1)
store volatile %struct.ScmObj* %args49959$k40512$2, %struct.ScmObj** %stackaddr$prim51428, align 8
%clofunc51429 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40512)
musttail call tailcc void %clofunc51429(%struct.ScmObj* %k40512, %struct.ScmObj* %args49959$k40512$2)
ret void
}

define tailcc void @proc_clo$ae42471(%struct.ScmObj* %env$ae42471,%struct.ScmObj* %current_45args49960) {
%stackaddr$env-ref51430 = alloca %struct.ScmObj*, align 8
%a40194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42471, i64 0)
store %struct.ScmObj* %a40194, %struct.ScmObj** %stackaddr$env-ref51430
%stackaddr$env-ref51431 = alloca %struct.ScmObj*, align 8
%k40512 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42471, i64 1)
store %struct.ScmObj* %k40512, %struct.ScmObj** %stackaddr$env-ref51431
%stackaddr$prim51432 = alloca %struct.ScmObj*, align 8
%_95k40514 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49960)
store volatile %struct.ScmObj* %_95k40514, %struct.ScmObj** %stackaddr$prim51432, align 8
%stackaddr$prim51433 = alloca %struct.ScmObj*, align 8
%current_45args49961 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49960)
store volatile %struct.ScmObj* %current_45args49961, %struct.ScmObj** %stackaddr$prim51433, align 8
%stackaddr$prim51434 = alloca %struct.ScmObj*, align 8
%cc40195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49961)
store volatile %struct.ScmObj* %cc40195, %struct.ScmObj** %stackaddr$prim51434, align 8
%ae42473 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51435 = alloca %struct.ScmObj*, align 8
%anf_45bind40328 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40194, %struct.ScmObj* %ae42473)
store volatile %struct.ScmObj* %anf_45bind40328, %struct.ScmObj** %stackaddr$prim51435, align 8
%stackaddr$prim51436 = alloca %struct.ScmObj*, align 8
%anf_45bind40329 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40328)
store volatile %struct.ScmObj* %anf_45bind40329, %struct.ScmObj** %stackaddr$prim51436, align 8
%truthy$cmp51437 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40329)
%cmp$cmp51437 = icmp eq i64 %truthy$cmp51437, 1
br i1 %cmp$cmp51437, label %truebranch$cmp51437, label %falsebranch$cmp51437
truebranch$cmp51437:
%ae42477 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42478 = call %struct.ScmObj* @const_init_true()
%args49963$k40512$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51438 = alloca %struct.ScmObj*, align 8
%args49963$k40512$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42478, %struct.ScmObj* %args49963$k40512$0)
store volatile %struct.ScmObj* %args49963$k40512$1, %struct.ScmObj** %stackaddr$prim51438, align 8
%stackaddr$prim51439 = alloca %struct.ScmObj*, align 8
%args49963$k40512$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42477, %struct.ScmObj* %args49963$k40512$1)
store volatile %struct.ScmObj* %args49963$k40512$2, %struct.ScmObj** %stackaddr$prim51439, align 8
%clofunc51440 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40512)
musttail call tailcc void %clofunc51440(%struct.ScmObj* %k40512, %struct.ScmObj* %args49963$k40512$2)
ret void
falsebranch$cmp51437:
%ae42486 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51441 = alloca %struct.ScmObj*, align 8
%anf_45bind40330 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40194, %struct.ScmObj* %ae42486)
store volatile %struct.ScmObj* %anf_45bind40330, %struct.ScmObj** %stackaddr$prim51441, align 8
%stackaddr$prim51442 = alloca %struct.ScmObj*, align 8
%anf_45bind40331 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40330)
store volatile %struct.ScmObj* %anf_45bind40331, %struct.ScmObj** %stackaddr$prim51442, align 8
%truthy$cmp51443 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40331)
%cmp$cmp51443 = icmp eq i64 %truthy$cmp51443, 1
br i1 %cmp$cmp51443, label %truebranch$cmp51443, label %falsebranch$cmp51443
truebranch$cmp51443:
%ae42490 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51444 = alloca %struct.ScmObj*, align 8
%anf_45bind40332 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40194, %struct.ScmObj* %ae42490)
store volatile %struct.ScmObj* %anf_45bind40332, %struct.ScmObj** %stackaddr$prim51444, align 8
%stackaddr$prim51445 = alloca %struct.ScmObj*, align 8
%b40197 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40332)
store volatile %struct.ScmObj* %b40197, %struct.ScmObj** %stackaddr$prim51445, align 8
%ae42493 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51446 = alloca %struct.ScmObj*, align 8
%anf_45bind40333 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40194, %struct.ScmObj* %ae42493)
store volatile %struct.ScmObj* %anf_45bind40333, %struct.ScmObj** %stackaddr$prim51446, align 8
%stackaddr$prim51447 = alloca %struct.ScmObj*, align 8
%anf_45bind40334 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40333)
store volatile %struct.ScmObj* %anf_45bind40334, %struct.ScmObj** %stackaddr$prim51447, align 8
%ae42496 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51448 = alloca %struct.ScmObj*, align 8
%_95040198 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40194, %struct.ScmObj* %ae42496, %struct.ScmObj* %anf_45bind40334)
store volatile %struct.ScmObj* %_95040198, %struct.ScmObj** %stackaddr$prim51448, align 8
%args49964$cc40195$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51449 = alloca %struct.ScmObj*, align 8
%args49964$cc40195$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40195, %struct.ScmObj* %args49964$cc40195$0)
store volatile %struct.ScmObj* %args49964$cc40195$1, %struct.ScmObj** %stackaddr$prim51449, align 8
%stackaddr$prim51450 = alloca %struct.ScmObj*, align 8
%args49964$cc40195$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40512, %struct.ScmObj* %args49964$cc40195$1)
store volatile %struct.ScmObj* %args49964$cc40195$2, %struct.ScmObj** %stackaddr$prim51450, align 8
%clofunc51451 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40195)
musttail call tailcc void %clofunc51451(%struct.ScmObj* %cc40195, %struct.ScmObj* %args49964$cc40195$2)
ret void
falsebranch$cmp51443:
%ae42529 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42530 = call %struct.ScmObj* @const_init_false()
%args49965$k40512$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51452 = alloca %struct.ScmObj*, align 8
%args49965$k40512$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42530, %struct.ScmObj* %args49965$k40512$0)
store volatile %struct.ScmObj* %args49965$k40512$1, %struct.ScmObj** %stackaddr$prim51452, align 8
%stackaddr$prim51453 = alloca %struct.ScmObj*, align 8
%args49965$k40512$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42529, %struct.ScmObj* %args49965$k40512$1)
store volatile %struct.ScmObj* %args49965$k40512$2, %struct.ScmObj** %stackaddr$prim51453, align 8
%clofunc51454 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40512)
musttail call tailcc void %clofunc51454(%struct.ScmObj* %k40512, %struct.ScmObj* %args49965$k40512$2)
ret void
}

define tailcc void @proc_clo$ae42453(%struct.ScmObj* %env$ae42453,%struct.ScmObj* %current_45args49967) {
%stackaddr$prim51455 = alloca %struct.ScmObj*, align 8
%k40515 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49967)
store volatile %struct.ScmObj* %k40515, %struct.ScmObj** %stackaddr$prim51455, align 8
%stackaddr$prim51456 = alloca %struct.ScmObj*, align 8
%current_45args49968 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49967)
store volatile %struct.ScmObj* %current_45args49968, %struct.ScmObj** %stackaddr$prim51456, align 8
%stackaddr$prim51457 = alloca %struct.ScmObj*, align 8
%k40196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49968)
store volatile %struct.ScmObj* %k40196, %struct.ScmObj** %stackaddr$prim51457, align 8
%ae42455 = call %struct.ScmObj* @const_init_int(i64 0)
%args49970$k40515$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51458 = alloca %struct.ScmObj*, align 8
%args49970$k40515$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40196, %struct.ScmObj* %args49970$k40515$0)
store volatile %struct.ScmObj* %args49970$k40515$1, %struct.ScmObj** %stackaddr$prim51458, align 8
%stackaddr$prim51459 = alloca %struct.ScmObj*, align 8
%args49970$k40515$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42455, %struct.ScmObj* %args49970$k40515$1)
store volatile %struct.ScmObj* %args49970$k40515$2, %struct.ScmObj** %stackaddr$prim51459, align 8
%clofunc51460 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40515)
musttail call tailcc void %clofunc51460(%struct.ScmObj* %k40515, %struct.ScmObj* %args49970$k40515$2)
ret void
}

define tailcc void @proc_clo$ae42376(%struct.ScmObj* %env$ae42376,%struct.ScmObj* %current_45args49973) {
%stackaddr$env-ref51461 = alloca %struct.ScmObj*, align 8
%_37append40200 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42376, i64 0)
store %struct.ScmObj* %_37append40200, %struct.ScmObj** %stackaddr$env-ref51461
%stackaddr$prim51462 = alloca %struct.ScmObj*, align 8
%k40516 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49973)
store volatile %struct.ScmObj* %k40516, %struct.ScmObj** %stackaddr$prim51462, align 8
%stackaddr$prim51463 = alloca %struct.ScmObj*, align 8
%current_45args49974 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49973)
store volatile %struct.ScmObj* %current_45args49974, %struct.ScmObj** %stackaddr$prim51463, align 8
%stackaddr$prim51464 = alloca %struct.ScmObj*, align 8
%ls040203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49974)
store volatile %struct.ScmObj* %ls040203, %struct.ScmObj** %stackaddr$prim51464, align 8
%stackaddr$prim51465 = alloca %struct.ScmObj*, align 8
%current_45args49975 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49974)
store volatile %struct.ScmObj* %current_45args49975, %struct.ScmObj** %stackaddr$prim51465, align 8
%stackaddr$prim51466 = alloca %struct.ScmObj*, align 8
%ls140202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49975)
store volatile %struct.ScmObj* %ls140202, %struct.ScmObj** %stackaddr$prim51466, align 8
%stackaddr$prim51467 = alloca %struct.ScmObj*, align 8
%anf_45bind40321 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls040203)
store volatile %struct.ScmObj* %anf_45bind40321, %struct.ScmObj** %stackaddr$prim51467, align 8
%truthy$cmp51468 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40321)
%cmp$cmp51468 = icmp eq i64 %truthy$cmp51468, 1
br i1 %cmp$cmp51468, label %truebranch$cmp51468, label %falsebranch$cmp51468
truebranch$cmp51468:
%ae42380 = call %struct.ScmObj* @const_init_int(i64 0)
%args49977$k40516$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51469 = alloca %struct.ScmObj*, align 8
%args49977$k40516$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140202, %struct.ScmObj* %args49977$k40516$0)
store volatile %struct.ScmObj* %args49977$k40516$1, %struct.ScmObj** %stackaddr$prim51469, align 8
%stackaddr$prim51470 = alloca %struct.ScmObj*, align 8
%args49977$k40516$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42380, %struct.ScmObj* %args49977$k40516$1)
store volatile %struct.ScmObj* %args49977$k40516$2, %struct.ScmObj** %stackaddr$prim51470, align 8
%clofunc51471 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40516)
musttail call tailcc void %clofunc51471(%struct.ScmObj* %k40516, %struct.ScmObj* %args49977$k40516$2)
ret void
falsebranch$cmp51468:
%stackaddr$prim51472 = alloca %struct.ScmObj*, align 8
%anf_45bind40322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls040203)
store volatile %struct.ScmObj* %anf_45bind40322, %struct.ScmObj** %stackaddr$prim51472, align 8
%ae42387 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim51473 = alloca %struct.ScmObj*, align 8
%anf_45bind40323 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40200, %struct.ScmObj* %ae42387)
store volatile %struct.ScmObj* %anf_45bind40323, %struct.ScmObj** %stackaddr$prim51473, align 8
%stackaddr$prim51474 = alloca %struct.ScmObj*, align 8
%anf_45bind40324 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls040203)
store volatile %struct.ScmObj* %anf_45bind40324, %struct.ScmObj** %stackaddr$prim51474, align 8
%stackaddr$makeclosure51475 = alloca %struct.ScmObj*, align 8
%fptrToInt51476 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42390 to i64
%ae42390 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt51476)
store volatile %struct.ScmObj* %ae42390, %struct.ScmObj** %stackaddr$makeclosure51475, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42390, %struct.ScmObj* %k40516, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42390, %struct.ScmObj* %anf_45bind40322, i64 1)
%args49982$anf_45bind40323$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51477 = alloca %struct.ScmObj*, align 8
%args49982$anf_45bind40323$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140202, %struct.ScmObj* %args49982$anf_45bind40323$0)
store volatile %struct.ScmObj* %args49982$anf_45bind40323$1, %struct.ScmObj** %stackaddr$prim51477, align 8
%stackaddr$prim51478 = alloca %struct.ScmObj*, align 8
%args49982$anf_45bind40323$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40324, %struct.ScmObj* %args49982$anf_45bind40323$1)
store volatile %struct.ScmObj* %args49982$anf_45bind40323$2, %struct.ScmObj** %stackaddr$prim51478, align 8
%stackaddr$prim51479 = alloca %struct.ScmObj*, align 8
%args49982$anf_45bind40323$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42390, %struct.ScmObj* %args49982$anf_45bind40323$2)
store volatile %struct.ScmObj* %args49982$anf_45bind40323$3, %struct.ScmObj** %stackaddr$prim51479, align 8
%clofunc51480 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40323)
musttail call tailcc void %clofunc51480(%struct.ScmObj* %anf_45bind40323, %struct.ScmObj* %args49982$anf_45bind40323$3)
ret void
}

define tailcc void @proc_clo$ae42390(%struct.ScmObj* %env$ae42390,%struct.ScmObj* %current_45args49978) {
%stackaddr$env-ref51481 = alloca %struct.ScmObj*, align 8
%k40516 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42390, i64 0)
store %struct.ScmObj* %k40516, %struct.ScmObj** %stackaddr$env-ref51481
%stackaddr$env-ref51482 = alloca %struct.ScmObj*, align 8
%anf_45bind40322 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42390, i64 1)
store %struct.ScmObj* %anf_45bind40322, %struct.ScmObj** %stackaddr$env-ref51482
%stackaddr$prim51483 = alloca %struct.ScmObj*, align 8
%_95k40517 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49978)
store volatile %struct.ScmObj* %_95k40517, %struct.ScmObj** %stackaddr$prim51483, align 8
%stackaddr$prim51484 = alloca %struct.ScmObj*, align 8
%current_45args49979 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49978)
store volatile %struct.ScmObj* %current_45args49979, %struct.ScmObj** %stackaddr$prim51484, align 8
%stackaddr$prim51485 = alloca %struct.ScmObj*, align 8
%anf_45bind40325 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49979)
store volatile %struct.ScmObj* %anf_45bind40325, %struct.ScmObj** %stackaddr$prim51485, align 8
%stackaddr$prim51486 = alloca %struct.ScmObj*, align 8
%cpsprim40518 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40322, %struct.ScmObj* %anf_45bind40325)
store volatile %struct.ScmObj* %cpsprim40518, %struct.ScmObj** %stackaddr$prim51486, align 8
%ae42396 = call %struct.ScmObj* @const_init_int(i64 0)
%args49981$k40516$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51487 = alloca %struct.ScmObj*, align 8
%args49981$k40516$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40518, %struct.ScmObj* %args49981$k40516$0)
store volatile %struct.ScmObj* %args49981$k40516$1, %struct.ScmObj** %stackaddr$prim51487, align 8
%stackaddr$prim51488 = alloca %struct.ScmObj*, align 8
%args49981$k40516$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42396, %struct.ScmObj* %args49981$k40516$1)
store volatile %struct.ScmObj* %args49981$k40516$2, %struct.ScmObj** %stackaddr$prim51488, align 8
%clofunc51489 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40516)
musttail call tailcc void %clofunc51489(%struct.ScmObj* %k40516, %struct.ScmObj* %args49981$k40516$2)
ret void
}

define tailcc void @proc_clo$ae42350(%struct.ScmObj* %env$ae42350,%struct.ScmObj* %current_45args49984) {
%stackaddr$prim51490 = alloca %struct.ScmObj*, align 8
%k40519 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49984)
store volatile %struct.ScmObj* %k40519, %struct.ScmObj** %stackaddr$prim51490, align 8
%stackaddr$prim51491 = alloca %struct.ScmObj*, align 8
%current_45args49985 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49984)
store volatile %struct.ScmObj* %current_45args49985, %struct.ScmObj** %stackaddr$prim51491, align 8
%stackaddr$prim51492 = alloca %struct.ScmObj*, align 8
%a40206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49985)
store volatile %struct.ScmObj* %a40206, %struct.ScmObj** %stackaddr$prim51492, align 8
%stackaddr$prim51493 = alloca %struct.ScmObj*, align 8
%current_45args49986 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49985)
store volatile %struct.ScmObj* %current_45args49986, %struct.ScmObj** %stackaddr$prim51493, align 8
%stackaddr$prim51494 = alloca %struct.ScmObj*, align 8
%b40205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49986)
store volatile %struct.ScmObj* %b40205, %struct.ScmObj** %stackaddr$prim51494, align 8
%stackaddr$prim51495 = alloca %struct.ScmObj*, align 8
%anf_45bind40320 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a40206, %struct.ScmObj* %b40205)
store volatile %struct.ScmObj* %anf_45bind40320, %struct.ScmObj** %stackaddr$prim51495, align 8
%stackaddr$prim51496 = alloca %struct.ScmObj*, align 8
%cpsprim40520 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40320)
store volatile %struct.ScmObj* %cpsprim40520, %struct.ScmObj** %stackaddr$prim51496, align 8
%ae42355 = call %struct.ScmObj* @const_init_int(i64 0)
%args49988$k40519$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51497 = alloca %struct.ScmObj*, align 8
%args49988$k40519$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40520, %struct.ScmObj* %args49988$k40519$0)
store volatile %struct.ScmObj* %args49988$k40519$1, %struct.ScmObj** %stackaddr$prim51497, align 8
%stackaddr$prim51498 = alloca %struct.ScmObj*, align 8
%args49988$k40519$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42355, %struct.ScmObj* %args49988$k40519$1)
store volatile %struct.ScmObj* %args49988$k40519$2, %struct.ScmObj** %stackaddr$prim51498, align 8
%clofunc51499 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40519)
musttail call tailcc void %clofunc51499(%struct.ScmObj* %k40519, %struct.ScmObj* %args49988$k40519$2)
ret void
}

define tailcc void @proc_clo$ae42326(%struct.ScmObj* %env$ae42326,%struct.ScmObj* %current_45args49990) {
%stackaddr$prim51500 = alloca %struct.ScmObj*, align 8
%k40521 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49990)
store volatile %struct.ScmObj* %k40521, %struct.ScmObj** %stackaddr$prim51500, align 8
%stackaddr$prim51501 = alloca %struct.ScmObj*, align 8
%current_45args49991 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49990)
store volatile %struct.ScmObj* %current_45args49991, %struct.ScmObj** %stackaddr$prim51501, align 8
%stackaddr$prim51502 = alloca %struct.ScmObj*, align 8
%a40209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49991)
store volatile %struct.ScmObj* %a40209, %struct.ScmObj** %stackaddr$prim51502, align 8
%stackaddr$prim51503 = alloca %struct.ScmObj*, align 8
%current_45args49992 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49991)
store volatile %struct.ScmObj* %current_45args49992, %struct.ScmObj** %stackaddr$prim51503, align 8
%stackaddr$prim51504 = alloca %struct.ScmObj*, align 8
%b40208 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49992)
store volatile %struct.ScmObj* %b40208, %struct.ScmObj** %stackaddr$prim51504, align 8
%stackaddr$prim51505 = alloca %struct.ScmObj*, align 8
%anf_45bind40319 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a40209, %struct.ScmObj* %b40208)
store volatile %struct.ScmObj* %anf_45bind40319, %struct.ScmObj** %stackaddr$prim51505, align 8
%stackaddr$prim51506 = alloca %struct.ScmObj*, align 8
%cpsprim40522 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40319)
store volatile %struct.ScmObj* %cpsprim40522, %struct.ScmObj** %stackaddr$prim51506, align 8
%ae42331 = call %struct.ScmObj* @const_init_int(i64 0)
%args49994$k40521$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51507 = alloca %struct.ScmObj*, align 8
%args49994$k40521$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40522, %struct.ScmObj* %args49994$k40521$0)
store volatile %struct.ScmObj* %args49994$k40521$1, %struct.ScmObj** %stackaddr$prim51507, align 8
%stackaddr$prim51508 = alloca %struct.ScmObj*, align 8
%args49994$k40521$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42331, %struct.ScmObj* %args49994$k40521$1)
store volatile %struct.ScmObj* %args49994$k40521$2, %struct.ScmObj** %stackaddr$prim51508, align 8
%clofunc51509 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40521)
musttail call tailcc void %clofunc51509(%struct.ScmObj* %k40521, %struct.ScmObj* %args49994$k40521$2)
ret void
}

define tailcc void @proc_clo$ae41932(%struct.ScmObj* %env$ae41932,%struct.ScmObj* %current_45args49997) {
%stackaddr$env-ref51510 = alloca %struct.ScmObj*, align 8
%_37foldr40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41932, i64 0)
store %struct.ScmObj* %_37foldr40133, %struct.ScmObj** %stackaddr$env-ref51510
%stackaddr$env-ref51511 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41932, i64 1)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref51511
%stackaddr$env-ref51512 = alloca %struct.ScmObj*, align 8
%_37map140159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41932, i64 2)
store %struct.ScmObj* %_37map140159, %struct.ScmObj** %stackaddr$env-ref51512
%stackaddr$prim51513 = alloca %struct.ScmObj*, align 8
%k40523 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49997)
store volatile %struct.ScmObj* %k40523, %struct.ScmObj** %stackaddr$prim51513, align 8
%stackaddr$prim51514 = alloca %struct.ScmObj*, align 8
%current_45args49998 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49997)
store volatile %struct.ScmObj* %current_45args49998, %struct.ScmObj** %stackaddr$prim51514, align 8
%stackaddr$prim51515 = alloca %struct.ScmObj*, align 8
%_37foldl40211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49998)
store volatile %struct.ScmObj* %_37foldl40211, %struct.ScmObj** %stackaddr$prim51515, align 8
%ae41934 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51516 = alloca %struct.ScmObj*, align 8
%fptrToInt51517 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41935 to i64
%ae41935 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt51517)
store volatile %struct.ScmObj* %ae41935, %struct.ScmObj** %stackaddr$makeclosure51516, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41935, %struct.ScmObj* %_37foldr40133, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41935, %struct.ScmObj* %_37foldl40211, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41935, %struct.ScmObj* %_37foldr140128, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41935, %struct.ScmObj* %_37map140159, i64 3)
%args50055$k40523$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51518 = alloca %struct.ScmObj*, align 8
%args50055$k40523$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41935, %struct.ScmObj* %args50055$k40523$0)
store volatile %struct.ScmObj* %args50055$k40523$1, %struct.ScmObj** %stackaddr$prim51518, align 8
%stackaddr$prim51519 = alloca %struct.ScmObj*, align 8
%args50055$k40523$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41934, %struct.ScmObj* %args50055$k40523$1)
store volatile %struct.ScmObj* %args50055$k40523$2, %struct.ScmObj** %stackaddr$prim51519, align 8
%clofunc51520 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40523)
musttail call tailcc void %clofunc51520(%struct.ScmObj* %k40523, %struct.ScmObj* %args50055$k40523$2)
ret void
}

define tailcc void @proc_clo$ae41935(%struct.ScmObj* %env$ae41935,%struct.ScmObj* %args4021240524) {
%stackaddr$env-ref51521 = alloca %struct.ScmObj*, align 8
%_37foldr40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41935, i64 0)
store %struct.ScmObj* %_37foldr40133, %struct.ScmObj** %stackaddr$env-ref51521
%stackaddr$env-ref51522 = alloca %struct.ScmObj*, align 8
%_37foldl40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41935, i64 1)
store %struct.ScmObj* %_37foldl40211, %struct.ScmObj** %stackaddr$env-ref51522
%stackaddr$env-ref51523 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41935, i64 2)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref51523
%stackaddr$env-ref51524 = alloca %struct.ScmObj*, align 8
%_37map140159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41935, i64 3)
store %struct.ScmObj* %_37map140159, %struct.ScmObj** %stackaddr$env-ref51524
%stackaddr$prim51525 = alloca %struct.ScmObj*, align 8
%k40525 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4021240524)
store volatile %struct.ScmObj* %k40525, %struct.ScmObj** %stackaddr$prim51525, align 8
%stackaddr$prim51526 = alloca %struct.ScmObj*, align 8
%args40212 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4021240524)
store volatile %struct.ScmObj* %args40212, %struct.ScmObj** %stackaddr$prim51526, align 8
%stackaddr$prim51527 = alloca %struct.ScmObj*, align 8
%f40215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40212)
store volatile %struct.ScmObj* %f40215, %struct.ScmObj** %stackaddr$prim51527, align 8
%stackaddr$prim51528 = alloca %struct.ScmObj*, align 8
%anf_45bind40307 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40212)
store volatile %struct.ScmObj* %anf_45bind40307, %struct.ScmObj** %stackaddr$prim51528, align 8
%stackaddr$prim51529 = alloca %struct.ScmObj*, align 8
%acc40214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40307)
store volatile %struct.ScmObj* %acc40214, %struct.ScmObj** %stackaddr$prim51529, align 8
%stackaddr$prim51530 = alloca %struct.ScmObj*, align 8
%anf_45bind40308 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40212)
store volatile %struct.ScmObj* %anf_45bind40308, %struct.ScmObj** %stackaddr$prim51530, align 8
%stackaddr$prim51531 = alloca %struct.ScmObj*, align 8
%lsts40213 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40308)
store volatile %struct.ScmObj* %lsts40213, %struct.ScmObj** %stackaddr$prim51531, align 8
%stackaddr$makeclosure51532 = alloca %struct.ScmObj*, align 8
%fptrToInt51533 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41943 to i64
%ae41943 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt51533)
store volatile %struct.ScmObj* %ae41943, %struct.ScmObj** %stackaddr$makeclosure51532, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41943, %struct.ScmObj* %lsts40213, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41943, %struct.ScmObj* %_37foldr40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41943, %struct.ScmObj* %f40215, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41943, %struct.ScmObj* %acc40214, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41943, %struct.ScmObj* %_37foldl40211, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41943, %struct.ScmObj* %_37foldr140128, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41943, %struct.ScmObj* %_37map140159, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41943, %struct.ScmObj* %k40525, i64 7)
%ae41944 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51534 = alloca %struct.ScmObj*, align 8
%fptrToInt51535 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41945 to i64
%ae41945 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt51535)
store volatile %struct.ScmObj* %ae41945, %struct.ScmObj** %stackaddr$makeclosure51534, align 8
%args50054$ae41943$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51536 = alloca %struct.ScmObj*, align 8
%args50054$ae41943$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41945, %struct.ScmObj* %args50054$ae41943$0)
store volatile %struct.ScmObj* %args50054$ae41943$1, %struct.ScmObj** %stackaddr$prim51536, align 8
%stackaddr$prim51537 = alloca %struct.ScmObj*, align 8
%args50054$ae41943$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41944, %struct.ScmObj* %args50054$ae41943$1)
store volatile %struct.ScmObj* %args50054$ae41943$2, %struct.ScmObj** %stackaddr$prim51537, align 8
%clofunc51538 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41943)
musttail call tailcc void %clofunc51538(%struct.ScmObj* %ae41943, %struct.ScmObj* %args50054$ae41943$2)
ret void
}

define tailcc void @proc_clo$ae41943(%struct.ScmObj* %env$ae41943,%struct.ScmObj* %current_45args50000) {
%stackaddr$env-ref51539 = alloca %struct.ScmObj*, align 8
%lsts40213 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41943, i64 0)
store %struct.ScmObj* %lsts40213, %struct.ScmObj** %stackaddr$env-ref51539
%stackaddr$env-ref51540 = alloca %struct.ScmObj*, align 8
%_37foldr40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41943, i64 1)
store %struct.ScmObj* %_37foldr40133, %struct.ScmObj** %stackaddr$env-ref51540
%stackaddr$env-ref51541 = alloca %struct.ScmObj*, align 8
%f40215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41943, i64 2)
store %struct.ScmObj* %f40215, %struct.ScmObj** %stackaddr$env-ref51541
%stackaddr$env-ref51542 = alloca %struct.ScmObj*, align 8
%acc40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41943, i64 3)
store %struct.ScmObj* %acc40214, %struct.ScmObj** %stackaddr$env-ref51542
%stackaddr$env-ref51543 = alloca %struct.ScmObj*, align 8
%_37foldl40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41943, i64 4)
store %struct.ScmObj* %_37foldl40211, %struct.ScmObj** %stackaddr$env-ref51543
%stackaddr$env-ref51544 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41943, i64 5)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref51544
%stackaddr$env-ref51545 = alloca %struct.ScmObj*, align 8
%_37map140159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41943, i64 6)
store %struct.ScmObj* %_37map140159, %struct.ScmObj** %stackaddr$env-ref51545
%stackaddr$env-ref51546 = alloca %struct.ScmObj*, align 8
%k40525 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41943, i64 7)
store %struct.ScmObj* %k40525, %struct.ScmObj** %stackaddr$env-ref51546
%stackaddr$prim51547 = alloca %struct.ScmObj*, align 8
%_95k40526 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50000)
store volatile %struct.ScmObj* %_95k40526, %struct.ScmObj** %stackaddr$prim51547, align 8
%stackaddr$prim51548 = alloca %struct.ScmObj*, align 8
%current_45args50001 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50000)
store volatile %struct.ScmObj* %current_45args50001, %struct.ScmObj** %stackaddr$prim51548, align 8
%stackaddr$prim51549 = alloca %struct.ScmObj*, align 8
%anf_45bind40309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50001)
store volatile %struct.ScmObj* %anf_45bind40309, %struct.ScmObj** %stackaddr$prim51549, align 8
%stackaddr$makeclosure51550 = alloca %struct.ScmObj*, align 8
%fptrToInt51551 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41975 to i64
%ae41975 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt51551)
store volatile %struct.ScmObj* %ae41975, %struct.ScmObj** %stackaddr$makeclosure51550, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41975, %struct.ScmObj* %lsts40213, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41975, %struct.ScmObj* %_37foldr40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41975, %struct.ScmObj* %f40215, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41975, %struct.ScmObj* %acc40214, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41975, %struct.ScmObj* %_37foldl40211, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41975, %struct.ScmObj* %_37map140159, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41975, %struct.ScmObj* %k40525, i64 6)
%ae41977 = call %struct.ScmObj* @const_init_false()
%args50047$_37foldr140128$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51552 = alloca %struct.ScmObj*, align 8
%args50047$_37foldr140128$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40213, %struct.ScmObj* %args50047$_37foldr140128$0)
store volatile %struct.ScmObj* %args50047$_37foldr140128$1, %struct.ScmObj** %stackaddr$prim51552, align 8
%stackaddr$prim51553 = alloca %struct.ScmObj*, align 8
%args50047$_37foldr140128$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41977, %struct.ScmObj* %args50047$_37foldr140128$1)
store volatile %struct.ScmObj* %args50047$_37foldr140128$2, %struct.ScmObj** %stackaddr$prim51553, align 8
%stackaddr$prim51554 = alloca %struct.ScmObj*, align 8
%args50047$_37foldr140128$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40309, %struct.ScmObj* %args50047$_37foldr140128$2)
store volatile %struct.ScmObj* %args50047$_37foldr140128$3, %struct.ScmObj** %stackaddr$prim51554, align 8
%stackaddr$prim51555 = alloca %struct.ScmObj*, align 8
%args50047$_37foldr140128$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41975, %struct.ScmObj* %args50047$_37foldr140128$3)
store volatile %struct.ScmObj* %args50047$_37foldr140128$4, %struct.ScmObj** %stackaddr$prim51555, align 8
%clofunc51556 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140128)
musttail call tailcc void %clofunc51556(%struct.ScmObj* %_37foldr140128, %struct.ScmObj* %args50047$_37foldr140128$4)
ret void
}

define tailcc void @proc_clo$ae41975(%struct.ScmObj* %env$ae41975,%struct.ScmObj* %current_45args50003) {
%stackaddr$env-ref51557 = alloca %struct.ScmObj*, align 8
%lsts40213 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41975, i64 0)
store %struct.ScmObj* %lsts40213, %struct.ScmObj** %stackaddr$env-ref51557
%stackaddr$env-ref51558 = alloca %struct.ScmObj*, align 8
%_37foldr40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41975, i64 1)
store %struct.ScmObj* %_37foldr40133, %struct.ScmObj** %stackaddr$env-ref51558
%stackaddr$env-ref51559 = alloca %struct.ScmObj*, align 8
%f40215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41975, i64 2)
store %struct.ScmObj* %f40215, %struct.ScmObj** %stackaddr$env-ref51559
%stackaddr$env-ref51560 = alloca %struct.ScmObj*, align 8
%acc40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41975, i64 3)
store %struct.ScmObj* %acc40214, %struct.ScmObj** %stackaddr$env-ref51560
%stackaddr$env-ref51561 = alloca %struct.ScmObj*, align 8
%_37foldl40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41975, i64 4)
store %struct.ScmObj* %_37foldl40211, %struct.ScmObj** %stackaddr$env-ref51561
%stackaddr$env-ref51562 = alloca %struct.ScmObj*, align 8
%_37map140159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41975, i64 5)
store %struct.ScmObj* %_37map140159, %struct.ScmObj** %stackaddr$env-ref51562
%stackaddr$env-ref51563 = alloca %struct.ScmObj*, align 8
%k40525 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41975, i64 6)
store %struct.ScmObj* %k40525, %struct.ScmObj** %stackaddr$env-ref51563
%stackaddr$prim51564 = alloca %struct.ScmObj*, align 8
%_95k40527 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50003)
store volatile %struct.ScmObj* %_95k40527, %struct.ScmObj** %stackaddr$prim51564, align 8
%stackaddr$prim51565 = alloca %struct.ScmObj*, align 8
%current_45args50004 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50003)
store volatile %struct.ScmObj* %current_45args50004, %struct.ScmObj** %stackaddr$prim51565, align 8
%stackaddr$prim51566 = alloca %struct.ScmObj*, align 8
%anf_45bind40310 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50004)
store volatile %struct.ScmObj* %anf_45bind40310, %struct.ScmObj** %stackaddr$prim51566, align 8
%truthy$cmp51567 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40310)
%cmp$cmp51567 = icmp eq i64 %truthy$cmp51567, 1
br i1 %cmp$cmp51567, label %truebranch$cmp51567, label %falsebranch$cmp51567
truebranch$cmp51567:
%ae41986 = call %struct.ScmObj* @const_init_int(i64 0)
%args50006$k40525$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51568 = alloca %struct.ScmObj*, align 8
%args50006$k40525$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40214, %struct.ScmObj* %args50006$k40525$0)
store volatile %struct.ScmObj* %args50006$k40525$1, %struct.ScmObj** %stackaddr$prim51568, align 8
%stackaddr$prim51569 = alloca %struct.ScmObj*, align 8
%args50006$k40525$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41986, %struct.ScmObj* %args50006$k40525$1)
store volatile %struct.ScmObj* %args50006$k40525$2, %struct.ScmObj** %stackaddr$prim51569, align 8
%clofunc51570 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40525)
musttail call tailcc void %clofunc51570(%struct.ScmObj* %k40525, %struct.ScmObj* %args50006$k40525$2)
ret void
falsebranch$cmp51567:
%stackaddr$makeclosure51571 = alloca %struct.ScmObj*, align 8
%fptrToInt51572 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41991 to i64
%ae41991 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt51572)
store volatile %struct.ScmObj* %ae41991, %struct.ScmObj** %stackaddr$makeclosure51571, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41991, %struct.ScmObj* %lsts40213, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41991, %struct.ScmObj* %_37foldr40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41991, %struct.ScmObj* %f40215, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41991, %struct.ScmObj* %acc40214, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41991, %struct.ScmObj* %_37foldl40211, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41991, %struct.ScmObj* %_37map140159, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41991, %struct.ScmObj* %k40525, i64 6)
%ae41992 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51573 = alloca %struct.ScmObj*, align 8
%fptrToInt51574 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41993 to i64
%ae41993 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt51574)
store volatile %struct.ScmObj* %ae41993, %struct.ScmObj** %stackaddr$makeclosure51573, align 8
%args50046$ae41991$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51575 = alloca %struct.ScmObj*, align 8
%args50046$ae41991$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41993, %struct.ScmObj* %args50046$ae41991$0)
store volatile %struct.ScmObj* %args50046$ae41991$1, %struct.ScmObj** %stackaddr$prim51575, align 8
%stackaddr$prim51576 = alloca %struct.ScmObj*, align 8
%args50046$ae41991$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41992, %struct.ScmObj* %args50046$ae41991$1)
store volatile %struct.ScmObj* %args50046$ae41991$2, %struct.ScmObj** %stackaddr$prim51576, align 8
%clofunc51577 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41991)
musttail call tailcc void %clofunc51577(%struct.ScmObj* %ae41991, %struct.ScmObj* %args50046$ae41991$2)
ret void
}

define tailcc void @proc_clo$ae41991(%struct.ScmObj* %env$ae41991,%struct.ScmObj* %current_45args50007) {
%stackaddr$env-ref51578 = alloca %struct.ScmObj*, align 8
%lsts40213 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41991, i64 0)
store %struct.ScmObj* %lsts40213, %struct.ScmObj** %stackaddr$env-ref51578
%stackaddr$env-ref51579 = alloca %struct.ScmObj*, align 8
%_37foldr40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41991, i64 1)
store %struct.ScmObj* %_37foldr40133, %struct.ScmObj** %stackaddr$env-ref51579
%stackaddr$env-ref51580 = alloca %struct.ScmObj*, align 8
%f40215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41991, i64 2)
store %struct.ScmObj* %f40215, %struct.ScmObj** %stackaddr$env-ref51580
%stackaddr$env-ref51581 = alloca %struct.ScmObj*, align 8
%acc40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41991, i64 3)
store %struct.ScmObj* %acc40214, %struct.ScmObj** %stackaddr$env-ref51581
%stackaddr$env-ref51582 = alloca %struct.ScmObj*, align 8
%_37foldl40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41991, i64 4)
store %struct.ScmObj* %_37foldl40211, %struct.ScmObj** %stackaddr$env-ref51582
%stackaddr$env-ref51583 = alloca %struct.ScmObj*, align 8
%_37map140159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41991, i64 5)
store %struct.ScmObj* %_37map140159, %struct.ScmObj** %stackaddr$env-ref51583
%stackaddr$env-ref51584 = alloca %struct.ScmObj*, align 8
%k40525 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41991, i64 6)
store %struct.ScmObj* %k40525, %struct.ScmObj** %stackaddr$env-ref51584
%stackaddr$prim51585 = alloca %struct.ScmObj*, align 8
%_95k40528 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50007)
store volatile %struct.ScmObj* %_95k40528, %struct.ScmObj** %stackaddr$prim51585, align 8
%stackaddr$prim51586 = alloca %struct.ScmObj*, align 8
%current_45args50008 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50007)
store volatile %struct.ScmObj* %current_45args50008, %struct.ScmObj** %stackaddr$prim51586, align 8
%stackaddr$prim51587 = alloca %struct.ScmObj*, align 8
%anf_45bind40311 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50008)
store volatile %struct.ScmObj* %anf_45bind40311, %struct.ScmObj** %stackaddr$prim51587, align 8
%stackaddr$makeclosure51588 = alloca %struct.ScmObj*, align 8
%fptrToInt51589 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42012 to i64
%ae42012 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt51589)
store volatile %struct.ScmObj* %ae42012, %struct.ScmObj** %stackaddr$makeclosure51588, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42012, %struct.ScmObj* %lsts40213, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42012, %struct.ScmObj* %_37foldr40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42012, %struct.ScmObj* %f40215, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae42012, %struct.ScmObj* %acc40214, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae42012, %struct.ScmObj* %_37foldl40211, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae42012, %struct.ScmObj* %_37map140159, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae42012, %struct.ScmObj* %k40525, i64 6)
%args50041$_37map140159$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51590 = alloca %struct.ScmObj*, align 8
%args50041$_37map140159$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40213, %struct.ScmObj* %args50041$_37map140159$0)
store volatile %struct.ScmObj* %args50041$_37map140159$1, %struct.ScmObj** %stackaddr$prim51590, align 8
%stackaddr$prim51591 = alloca %struct.ScmObj*, align 8
%args50041$_37map140159$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40311, %struct.ScmObj* %args50041$_37map140159$1)
store volatile %struct.ScmObj* %args50041$_37map140159$2, %struct.ScmObj** %stackaddr$prim51591, align 8
%stackaddr$prim51592 = alloca %struct.ScmObj*, align 8
%args50041$_37map140159$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42012, %struct.ScmObj* %args50041$_37map140159$2)
store volatile %struct.ScmObj* %args50041$_37map140159$3, %struct.ScmObj** %stackaddr$prim51592, align 8
%clofunc51593 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140159)
musttail call tailcc void %clofunc51593(%struct.ScmObj* %_37map140159, %struct.ScmObj* %args50041$_37map140159$3)
ret void
}

define tailcc void @proc_clo$ae42012(%struct.ScmObj* %env$ae42012,%struct.ScmObj* %current_45args50010) {
%stackaddr$env-ref51594 = alloca %struct.ScmObj*, align 8
%lsts40213 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42012, i64 0)
store %struct.ScmObj* %lsts40213, %struct.ScmObj** %stackaddr$env-ref51594
%stackaddr$env-ref51595 = alloca %struct.ScmObj*, align 8
%_37foldr40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42012, i64 1)
store %struct.ScmObj* %_37foldr40133, %struct.ScmObj** %stackaddr$env-ref51595
%stackaddr$env-ref51596 = alloca %struct.ScmObj*, align 8
%f40215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42012, i64 2)
store %struct.ScmObj* %f40215, %struct.ScmObj** %stackaddr$env-ref51596
%stackaddr$env-ref51597 = alloca %struct.ScmObj*, align 8
%acc40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42012, i64 3)
store %struct.ScmObj* %acc40214, %struct.ScmObj** %stackaddr$env-ref51597
%stackaddr$env-ref51598 = alloca %struct.ScmObj*, align 8
%_37foldl40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42012, i64 4)
store %struct.ScmObj* %_37foldl40211, %struct.ScmObj** %stackaddr$env-ref51598
%stackaddr$env-ref51599 = alloca %struct.ScmObj*, align 8
%_37map140159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42012, i64 5)
store %struct.ScmObj* %_37map140159, %struct.ScmObj** %stackaddr$env-ref51599
%stackaddr$env-ref51600 = alloca %struct.ScmObj*, align 8
%k40525 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42012, i64 6)
store %struct.ScmObj* %k40525, %struct.ScmObj** %stackaddr$env-ref51600
%stackaddr$prim51601 = alloca %struct.ScmObj*, align 8
%_95k40529 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50010)
store volatile %struct.ScmObj* %_95k40529, %struct.ScmObj** %stackaddr$prim51601, align 8
%stackaddr$prim51602 = alloca %struct.ScmObj*, align 8
%current_45args50011 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50010)
store volatile %struct.ScmObj* %current_45args50011, %struct.ScmObj** %stackaddr$prim51602, align 8
%stackaddr$prim51603 = alloca %struct.ScmObj*, align 8
%lsts_4340220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50011)
store volatile %struct.ScmObj* %lsts_4340220, %struct.ScmObj** %stackaddr$prim51603, align 8
%stackaddr$makeclosure51604 = alloca %struct.ScmObj*, align 8
%fptrToInt51605 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42015 to i64
%ae42015 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt51605)
store volatile %struct.ScmObj* %ae42015, %struct.ScmObj** %stackaddr$makeclosure51604, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42015, %struct.ScmObj* %lsts40213, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42015, %struct.ScmObj* %_37foldr40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42015, %struct.ScmObj* %f40215, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae42015, %struct.ScmObj* %acc40214, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae42015, %struct.ScmObj* %_37foldl40211, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae42015, %struct.ScmObj* %_37map140159, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae42015, %struct.ScmObj* %k40525, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae42015, %struct.ScmObj* %lsts_4340220, i64 7)
%ae42016 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51606 = alloca %struct.ScmObj*, align 8
%fptrToInt51607 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42017 to i64
%ae42017 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt51607)
store volatile %struct.ScmObj* %ae42017, %struct.ScmObj** %stackaddr$makeclosure51606, align 8
%args50040$ae42015$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51608 = alloca %struct.ScmObj*, align 8
%args50040$ae42015$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42017, %struct.ScmObj* %args50040$ae42015$0)
store volatile %struct.ScmObj* %args50040$ae42015$1, %struct.ScmObj** %stackaddr$prim51608, align 8
%stackaddr$prim51609 = alloca %struct.ScmObj*, align 8
%args50040$ae42015$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42016, %struct.ScmObj* %args50040$ae42015$1)
store volatile %struct.ScmObj* %args50040$ae42015$2, %struct.ScmObj** %stackaddr$prim51609, align 8
%clofunc51610 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42015)
musttail call tailcc void %clofunc51610(%struct.ScmObj* %ae42015, %struct.ScmObj* %args50040$ae42015$2)
ret void
}

define tailcc void @proc_clo$ae42015(%struct.ScmObj* %env$ae42015,%struct.ScmObj* %current_45args50013) {
%stackaddr$env-ref51611 = alloca %struct.ScmObj*, align 8
%lsts40213 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42015, i64 0)
store %struct.ScmObj* %lsts40213, %struct.ScmObj** %stackaddr$env-ref51611
%stackaddr$env-ref51612 = alloca %struct.ScmObj*, align 8
%_37foldr40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42015, i64 1)
store %struct.ScmObj* %_37foldr40133, %struct.ScmObj** %stackaddr$env-ref51612
%stackaddr$env-ref51613 = alloca %struct.ScmObj*, align 8
%f40215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42015, i64 2)
store %struct.ScmObj* %f40215, %struct.ScmObj** %stackaddr$env-ref51613
%stackaddr$env-ref51614 = alloca %struct.ScmObj*, align 8
%acc40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42015, i64 3)
store %struct.ScmObj* %acc40214, %struct.ScmObj** %stackaddr$env-ref51614
%stackaddr$env-ref51615 = alloca %struct.ScmObj*, align 8
%_37foldl40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42015, i64 4)
store %struct.ScmObj* %_37foldl40211, %struct.ScmObj** %stackaddr$env-ref51615
%stackaddr$env-ref51616 = alloca %struct.ScmObj*, align 8
%_37map140159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42015, i64 5)
store %struct.ScmObj* %_37map140159, %struct.ScmObj** %stackaddr$env-ref51616
%stackaddr$env-ref51617 = alloca %struct.ScmObj*, align 8
%k40525 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42015, i64 6)
store %struct.ScmObj* %k40525, %struct.ScmObj** %stackaddr$env-ref51617
%stackaddr$env-ref51618 = alloca %struct.ScmObj*, align 8
%lsts_4340220 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42015, i64 7)
store %struct.ScmObj* %lsts_4340220, %struct.ScmObj** %stackaddr$env-ref51618
%stackaddr$prim51619 = alloca %struct.ScmObj*, align 8
%_95k40530 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50013)
store volatile %struct.ScmObj* %_95k40530, %struct.ScmObj** %stackaddr$prim51619, align 8
%stackaddr$prim51620 = alloca %struct.ScmObj*, align 8
%current_45args50014 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50013)
store volatile %struct.ScmObj* %current_45args50014, %struct.ScmObj** %stackaddr$prim51620, align 8
%stackaddr$prim51621 = alloca %struct.ScmObj*, align 8
%anf_45bind40312 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50014)
store volatile %struct.ScmObj* %anf_45bind40312, %struct.ScmObj** %stackaddr$prim51621, align 8
%stackaddr$makeclosure51622 = alloca %struct.ScmObj*, align 8
%fptrToInt51623 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42036 to i64
%ae42036 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt51623)
store volatile %struct.ScmObj* %ae42036, %struct.ScmObj** %stackaddr$makeclosure51622, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42036, %struct.ScmObj* %f40215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42036, %struct.ScmObj* %acc40214, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42036, %struct.ScmObj* %_37foldr40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae42036, %struct.ScmObj* %_37foldl40211, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae42036, %struct.ScmObj* %k40525, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae42036, %struct.ScmObj* %lsts_4340220, i64 5)
%args50035$_37map140159$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51624 = alloca %struct.ScmObj*, align 8
%args50035$_37map140159$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40213, %struct.ScmObj* %args50035$_37map140159$0)
store volatile %struct.ScmObj* %args50035$_37map140159$1, %struct.ScmObj** %stackaddr$prim51624, align 8
%stackaddr$prim51625 = alloca %struct.ScmObj*, align 8
%args50035$_37map140159$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40312, %struct.ScmObj* %args50035$_37map140159$1)
store volatile %struct.ScmObj* %args50035$_37map140159$2, %struct.ScmObj** %stackaddr$prim51625, align 8
%stackaddr$prim51626 = alloca %struct.ScmObj*, align 8
%args50035$_37map140159$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42036, %struct.ScmObj* %args50035$_37map140159$2)
store volatile %struct.ScmObj* %args50035$_37map140159$3, %struct.ScmObj** %stackaddr$prim51626, align 8
%clofunc51627 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140159)
musttail call tailcc void %clofunc51627(%struct.ScmObj* %_37map140159, %struct.ScmObj* %args50035$_37map140159$3)
ret void
}

define tailcc void @proc_clo$ae42036(%struct.ScmObj* %env$ae42036,%struct.ScmObj* %current_45args50016) {
%stackaddr$env-ref51628 = alloca %struct.ScmObj*, align 8
%f40215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42036, i64 0)
store %struct.ScmObj* %f40215, %struct.ScmObj** %stackaddr$env-ref51628
%stackaddr$env-ref51629 = alloca %struct.ScmObj*, align 8
%acc40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42036, i64 1)
store %struct.ScmObj* %acc40214, %struct.ScmObj** %stackaddr$env-ref51629
%stackaddr$env-ref51630 = alloca %struct.ScmObj*, align 8
%_37foldr40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42036, i64 2)
store %struct.ScmObj* %_37foldr40133, %struct.ScmObj** %stackaddr$env-ref51630
%stackaddr$env-ref51631 = alloca %struct.ScmObj*, align 8
%_37foldl40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42036, i64 3)
store %struct.ScmObj* %_37foldl40211, %struct.ScmObj** %stackaddr$env-ref51631
%stackaddr$env-ref51632 = alloca %struct.ScmObj*, align 8
%k40525 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42036, i64 4)
store %struct.ScmObj* %k40525, %struct.ScmObj** %stackaddr$env-ref51632
%stackaddr$env-ref51633 = alloca %struct.ScmObj*, align 8
%lsts_4340220 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42036, i64 5)
store %struct.ScmObj* %lsts_4340220, %struct.ScmObj** %stackaddr$env-ref51633
%stackaddr$prim51634 = alloca %struct.ScmObj*, align 8
%_95k40531 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50016)
store volatile %struct.ScmObj* %_95k40531, %struct.ScmObj** %stackaddr$prim51634, align 8
%stackaddr$prim51635 = alloca %struct.ScmObj*, align 8
%current_45args50017 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50016)
store volatile %struct.ScmObj* %current_45args50017, %struct.ScmObj** %stackaddr$prim51635, align 8
%stackaddr$prim51636 = alloca %struct.ScmObj*, align 8
%vs40218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50017)
store volatile %struct.ScmObj* %vs40218, %struct.ScmObj** %stackaddr$prim51636, align 8
%stackaddr$makeclosure51637 = alloca %struct.ScmObj*, align 8
%fptrToInt51638 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42039 to i64
%ae42039 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt51638)
store volatile %struct.ScmObj* %ae42039, %struct.ScmObj** %stackaddr$makeclosure51637, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42039, %struct.ScmObj* %f40215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42039, %struct.ScmObj* %acc40214, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42039, %struct.ScmObj* %_37foldr40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae42039, %struct.ScmObj* %_37foldl40211, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae42039, %struct.ScmObj* %k40525, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae42039, %struct.ScmObj* %lsts_4340220, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae42039, %struct.ScmObj* %vs40218, i64 6)
%ae42040 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51639 = alloca %struct.ScmObj*, align 8
%fptrToInt51640 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42041 to i64
%ae42041 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt51640)
store volatile %struct.ScmObj* %ae42041, %struct.ScmObj** %stackaddr$makeclosure51639, align 8
%args50034$ae42039$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51641 = alloca %struct.ScmObj*, align 8
%args50034$ae42039$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42041, %struct.ScmObj* %args50034$ae42039$0)
store volatile %struct.ScmObj* %args50034$ae42039$1, %struct.ScmObj** %stackaddr$prim51641, align 8
%stackaddr$prim51642 = alloca %struct.ScmObj*, align 8
%args50034$ae42039$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42040, %struct.ScmObj* %args50034$ae42039$1)
store volatile %struct.ScmObj* %args50034$ae42039$2, %struct.ScmObj** %stackaddr$prim51642, align 8
%clofunc51643 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42039)
musttail call tailcc void %clofunc51643(%struct.ScmObj* %ae42039, %struct.ScmObj* %args50034$ae42039$2)
ret void
}

define tailcc void @proc_clo$ae42039(%struct.ScmObj* %env$ae42039,%struct.ScmObj* %current_45args50019) {
%stackaddr$env-ref51644 = alloca %struct.ScmObj*, align 8
%f40215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42039, i64 0)
store %struct.ScmObj* %f40215, %struct.ScmObj** %stackaddr$env-ref51644
%stackaddr$env-ref51645 = alloca %struct.ScmObj*, align 8
%acc40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42039, i64 1)
store %struct.ScmObj* %acc40214, %struct.ScmObj** %stackaddr$env-ref51645
%stackaddr$env-ref51646 = alloca %struct.ScmObj*, align 8
%_37foldr40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42039, i64 2)
store %struct.ScmObj* %_37foldr40133, %struct.ScmObj** %stackaddr$env-ref51646
%stackaddr$env-ref51647 = alloca %struct.ScmObj*, align 8
%_37foldl40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42039, i64 3)
store %struct.ScmObj* %_37foldl40211, %struct.ScmObj** %stackaddr$env-ref51647
%stackaddr$env-ref51648 = alloca %struct.ScmObj*, align 8
%k40525 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42039, i64 4)
store %struct.ScmObj* %k40525, %struct.ScmObj** %stackaddr$env-ref51648
%stackaddr$env-ref51649 = alloca %struct.ScmObj*, align 8
%lsts_4340220 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42039, i64 5)
store %struct.ScmObj* %lsts_4340220, %struct.ScmObj** %stackaddr$env-ref51649
%stackaddr$env-ref51650 = alloca %struct.ScmObj*, align 8
%vs40218 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42039, i64 6)
store %struct.ScmObj* %vs40218, %struct.ScmObj** %stackaddr$env-ref51650
%stackaddr$prim51651 = alloca %struct.ScmObj*, align 8
%_95k40532 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50019)
store volatile %struct.ScmObj* %_95k40532, %struct.ScmObj** %stackaddr$prim51651, align 8
%stackaddr$prim51652 = alloca %struct.ScmObj*, align 8
%current_45args50020 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50019)
store volatile %struct.ScmObj* %current_45args50020, %struct.ScmObj** %stackaddr$prim51652, align 8
%stackaddr$prim51653 = alloca %struct.ScmObj*, align 8
%anf_45bind40313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50020)
store volatile %struct.ScmObj* %anf_45bind40313, %struct.ScmObj** %stackaddr$prim51653, align 8
%ae42062 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51654 = alloca %struct.ScmObj*, align 8
%anf_45bind40314 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40214, %struct.ScmObj* %ae42062)
store volatile %struct.ScmObj* %anf_45bind40314, %struct.ScmObj** %stackaddr$prim51654, align 8
%stackaddr$makeclosure51655 = alloca %struct.ScmObj*, align 8
%fptrToInt51656 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42064 to i64
%ae42064 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt51656)
store volatile %struct.ScmObj* %ae42064, %struct.ScmObj** %stackaddr$makeclosure51655, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42064, %struct.ScmObj* %f40215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42064, %struct.ScmObj* %_37foldl40211, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42064, %struct.ScmObj* %k40525, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae42064, %struct.ScmObj* %lsts_4340220, i64 3)
%args50028$_37foldr40133$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51657 = alloca %struct.ScmObj*, align 8
%args50028$_37foldr40133$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40218, %struct.ScmObj* %args50028$_37foldr40133$0)
store volatile %struct.ScmObj* %args50028$_37foldr40133$1, %struct.ScmObj** %stackaddr$prim51657, align 8
%stackaddr$prim51658 = alloca %struct.ScmObj*, align 8
%args50028$_37foldr40133$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40314, %struct.ScmObj* %args50028$_37foldr40133$1)
store volatile %struct.ScmObj* %args50028$_37foldr40133$2, %struct.ScmObj** %stackaddr$prim51658, align 8
%stackaddr$prim51659 = alloca %struct.ScmObj*, align 8
%args50028$_37foldr40133$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40313, %struct.ScmObj* %args50028$_37foldr40133$2)
store volatile %struct.ScmObj* %args50028$_37foldr40133$3, %struct.ScmObj** %stackaddr$prim51659, align 8
%stackaddr$prim51660 = alloca %struct.ScmObj*, align 8
%args50028$_37foldr40133$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42064, %struct.ScmObj* %args50028$_37foldr40133$3)
store volatile %struct.ScmObj* %args50028$_37foldr40133$4, %struct.ScmObj** %stackaddr$prim51660, align 8
%clofunc51661 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40133)
musttail call tailcc void %clofunc51661(%struct.ScmObj* %_37foldr40133, %struct.ScmObj* %args50028$_37foldr40133$4)
ret void
}

define tailcc void @proc_clo$ae42064(%struct.ScmObj* %env$ae42064,%struct.ScmObj* %current_45args50022) {
%stackaddr$env-ref51662 = alloca %struct.ScmObj*, align 8
%f40215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42064, i64 0)
store %struct.ScmObj* %f40215, %struct.ScmObj** %stackaddr$env-ref51662
%stackaddr$env-ref51663 = alloca %struct.ScmObj*, align 8
%_37foldl40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42064, i64 1)
store %struct.ScmObj* %_37foldl40211, %struct.ScmObj** %stackaddr$env-ref51663
%stackaddr$env-ref51664 = alloca %struct.ScmObj*, align 8
%k40525 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42064, i64 2)
store %struct.ScmObj* %k40525, %struct.ScmObj** %stackaddr$env-ref51664
%stackaddr$env-ref51665 = alloca %struct.ScmObj*, align 8
%lsts_4340220 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42064, i64 3)
store %struct.ScmObj* %lsts_4340220, %struct.ScmObj** %stackaddr$env-ref51665
%stackaddr$prim51666 = alloca %struct.ScmObj*, align 8
%_95k40533 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50022)
store volatile %struct.ScmObj* %_95k40533, %struct.ScmObj** %stackaddr$prim51666, align 8
%stackaddr$prim51667 = alloca %struct.ScmObj*, align 8
%current_45args50023 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50022)
store volatile %struct.ScmObj* %current_45args50023, %struct.ScmObj** %stackaddr$prim51667, align 8
%stackaddr$prim51668 = alloca %struct.ScmObj*, align 8
%anf_45bind40315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50023)
store volatile %struct.ScmObj* %anf_45bind40315, %struct.ScmObj** %stackaddr$prim51668, align 8
%stackaddr$makeclosure51669 = alloca %struct.ScmObj*, align 8
%fptrToInt51670 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42068 to i64
%ae42068 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt51670)
store volatile %struct.ScmObj* %ae42068, %struct.ScmObj** %stackaddr$makeclosure51669, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42068, %struct.ScmObj* %f40215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42068, %struct.ScmObj* %_37foldl40211, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42068, %struct.ScmObj* %k40525, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae42068, %struct.ScmObj* %lsts_4340220, i64 3)
%stackaddr$prim51671 = alloca %struct.ScmObj*, align 8
%cpsargs40536 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42068, %struct.ScmObj* %anf_45bind40315)
store volatile %struct.ScmObj* %cpsargs40536, %struct.ScmObj** %stackaddr$prim51671, align 8
%clofunc51672 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40215)
musttail call tailcc void %clofunc51672(%struct.ScmObj* %f40215, %struct.ScmObj* %cpsargs40536)
ret void
}

define tailcc void @proc_clo$ae42068(%struct.ScmObj* %env$ae42068,%struct.ScmObj* %current_45args50025) {
%stackaddr$env-ref51673 = alloca %struct.ScmObj*, align 8
%f40215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42068, i64 0)
store %struct.ScmObj* %f40215, %struct.ScmObj** %stackaddr$env-ref51673
%stackaddr$env-ref51674 = alloca %struct.ScmObj*, align 8
%_37foldl40211 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42068, i64 1)
store %struct.ScmObj* %_37foldl40211, %struct.ScmObj** %stackaddr$env-ref51674
%stackaddr$env-ref51675 = alloca %struct.ScmObj*, align 8
%k40525 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42068, i64 2)
store %struct.ScmObj* %k40525, %struct.ScmObj** %stackaddr$env-ref51675
%stackaddr$env-ref51676 = alloca %struct.ScmObj*, align 8
%lsts_4340220 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42068, i64 3)
store %struct.ScmObj* %lsts_4340220, %struct.ScmObj** %stackaddr$env-ref51676
%stackaddr$prim51677 = alloca %struct.ScmObj*, align 8
%_95k40534 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50025)
store volatile %struct.ScmObj* %_95k40534, %struct.ScmObj** %stackaddr$prim51677, align 8
%stackaddr$prim51678 = alloca %struct.ScmObj*, align 8
%current_45args50026 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50025)
store volatile %struct.ScmObj* %current_45args50026, %struct.ScmObj** %stackaddr$prim51678, align 8
%stackaddr$prim51679 = alloca %struct.ScmObj*, align 8
%acc_4340222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50026)
store volatile %struct.ScmObj* %acc_4340222, %struct.ScmObj** %stackaddr$prim51679, align 8
%stackaddr$prim51680 = alloca %struct.ScmObj*, align 8
%anf_45bind40316 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4340222, %struct.ScmObj* %lsts_4340220)
store volatile %struct.ScmObj* %anf_45bind40316, %struct.ScmObj** %stackaddr$prim51680, align 8
%stackaddr$prim51681 = alloca %struct.ScmObj*, align 8
%anf_45bind40317 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40215, %struct.ScmObj* %anf_45bind40316)
store volatile %struct.ScmObj* %anf_45bind40317, %struct.ScmObj** %stackaddr$prim51681, align 8
%stackaddr$prim51682 = alloca %struct.ScmObj*, align 8
%cpsargs40535 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40525, %struct.ScmObj* %anf_45bind40317)
store volatile %struct.ScmObj* %cpsargs40535, %struct.ScmObj** %stackaddr$prim51682, align 8
%clofunc51683 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl40211)
musttail call tailcc void %clofunc51683(%struct.ScmObj* %_37foldl40211, %struct.ScmObj* %cpsargs40535)
ret void
}

define tailcc void @proc_clo$ae42041(%struct.ScmObj* %env$ae42041,%struct.ScmObj* %current_45args50029) {
%stackaddr$prim51684 = alloca %struct.ScmObj*, align 8
%k40537 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50029)
store volatile %struct.ScmObj* %k40537, %struct.ScmObj** %stackaddr$prim51684, align 8
%stackaddr$prim51685 = alloca %struct.ScmObj*, align 8
%current_45args50030 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50029)
store volatile %struct.ScmObj* %current_45args50030, %struct.ScmObj** %stackaddr$prim51685, align 8
%stackaddr$prim51686 = alloca %struct.ScmObj*, align 8
%a40224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50030)
store volatile %struct.ScmObj* %a40224, %struct.ScmObj** %stackaddr$prim51686, align 8
%stackaddr$prim51687 = alloca %struct.ScmObj*, align 8
%current_45args50031 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50030)
store volatile %struct.ScmObj* %current_45args50031, %struct.ScmObj** %stackaddr$prim51687, align 8
%stackaddr$prim51688 = alloca %struct.ScmObj*, align 8
%b40223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50031)
store volatile %struct.ScmObj* %b40223, %struct.ScmObj** %stackaddr$prim51688, align 8
%stackaddr$prim51689 = alloca %struct.ScmObj*, align 8
%cpsprim40538 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40224, %struct.ScmObj* %b40223)
store volatile %struct.ScmObj* %cpsprim40538, %struct.ScmObj** %stackaddr$prim51689, align 8
%ae42045 = call %struct.ScmObj* @const_init_int(i64 0)
%args50033$k40537$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51690 = alloca %struct.ScmObj*, align 8
%args50033$k40537$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40538, %struct.ScmObj* %args50033$k40537$0)
store volatile %struct.ScmObj* %args50033$k40537$1, %struct.ScmObj** %stackaddr$prim51690, align 8
%stackaddr$prim51691 = alloca %struct.ScmObj*, align 8
%args50033$k40537$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42045, %struct.ScmObj* %args50033$k40537$1)
store volatile %struct.ScmObj* %args50033$k40537$2, %struct.ScmObj** %stackaddr$prim51691, align 8
%clofunc51692 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40537)
musttail call tailcc void %clofunc51692(%struct.ScmObj* %k40537, %struct.ScmObj* %args50033$k40537$2)
ret void
}

define tailcc void @proc_clo$ae42017(%struct.ScmObj* %env$ae42017,%struct.ScmObj* %current_45args50036) {
%stackaddr$prim51693 = alloca %struct.ScmObj*, align 8
%k40539 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50036)
store volatile %struct.ScmObj* %k40539, %struct.ScmObj** %stackaddr$prim51693, align 8
%stackaddr$prim51694 = alloca %struct.ScmObj*, align 8
%current_45args50037 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50036)
store volatile %struct.ScmObj* %current_45args50037, %struct.ScmObj** %stackaddr$prim51694, align 8
%stackaddr$prim51695 = alloca %struct.ScmObj*, align 8
%x40219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50037)
store volatile %struct.ScmObj* %x40219, %struct.ScmObj** %stackaddr$prim51695, align 8
%stackaddr$prim51696 = alloca %struct.ScmObj*, align 8
%cpsprim40540 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40219)
store volatile %struct.ScmObj* %cpsprim40540, %struct.ScmObj** %stackaddr$prim51696, align 8
%ae42020 = call %struct.ScmObj* @const_init_int(i64 0)
%args50039$k40539$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51697 = alloca %struct.ScmObj*, align 8
%args50039$k40539$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40540, %struct.ScmObj* %args50039$k40539$0)
store volatile %struct.ScmObj* %args50039$k40539$1, %struct.ScmObj** %stackaddr$prim51697, align 8
%stackaddr$prim51698 = alloca %struct.ScmObj*, align 8
%args50039$k40539$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42020, %struct.ScmObj* %args50039$k40539$1)
store volatile %struct.ScmObj* %args50039$k40539$2, %struct.ScmObj** %stackaddr$prim51698, align 8
%clofunc51699 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40539)
musttail call tailcc void %clofunc51699(%struct.ScmObj* %k40539, %struct.ScmObj* %args50039$k40539$2)
ret void
}

define tailcc void @proc_clo$ae41993(%struct.ScmObj* %env$ae41993,%struct.ScmObj* %current_45args50042) {
%stackaddr$prim51700 = alloca %struct.ScmObj*, align 8
%k40541 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50042)
store volatile %struct.ScmObj* %k40541, %struct.ScmObj** %stackaddr$prim51700, align 8
%stackaddr$prim51701 = alloca %struct.ScmObj*, align 8
%current_45args50043 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50042)
store volatile %struct.ScmObj* %current_45args50043, %struct.ScmObj** %stackaddr$prim51701, align 8
%stackaddr$prim51702 = alloca %struct.ScmObj*, align 8
%x40221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50043)
store volatile %struct.ScmObj* %x40221, %struct.ScmObj** %stackaddr$prim51702, align 8
%stackaddr$prim51703 = alloca %struct.ScmObj*, align 8
%cpsprim40542 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40221)
store volatile %struct.ScmObj* %cpsprim40542, %struct.ScmObj** %stackaddr$prim51703, align 8
%ae41996 = call %struct.ScmObj* @const_init_int(i64 0)
%args50045$k40541$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51704 = alloca %struct.ScmObj*, align 8
%args50045$k40541$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40542, %struct.ScmObj* %args50045$k40541$0)
store volatile %struct.ScmObj* %args50045$k40541$1, %struct.ScmObj** %stackaddr$prim51704, align 8
%stackaddr$prim51705 = alloca %struct.ScmObj*, align 8
%args50045$k40541$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41996, %struct.ScmObj* %args50045$k40541$1)
store volatile %struct.ScmObj* %args50045$k40541$2, %struct.ScmObj** %stackaddr$prim51705, align 8
%clofunc51706 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40541)
musttail call tailcc void %clofunc51706(%struct.ScmObj* %k40541, %struct.ScmObj* %args50045$k40541$2)
ret void
}

define tailcc void @proc_clo$ae41945(%struct.ScmObj* %env$ae41945,%struct.ScmObj* %current_45args50048) {
%stackaddr$prim51707 = alloca %struct.ScmObj*, align 8
%k40543 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50048)
store volatile %struct.ScmObj* %k40543, %struct.ScmObj** %stackaddr$prim51707, align 8
%stackaddr$prim51708 = alloca %struct.ScmObj*, align 8
%current_45args50049 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50048)
store volatile %struct.ScmObj* %current_45args50049, %struct.ScmObj** %stackaddr$prim51708, align 8
%stackaddr$prim51709 = alloca %struct.ScmObj*, align 8
%lst40217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50049)
store volatile %struct.ScmObj* %lst40217, %struct.ScmObj** %stackaddr$prim51709, align 8
%stackaddr$prim51710 = alloca %struct.ScmObj*, align 8
%current_45args50050 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50049)
store volatile %struct.ScmObj* %current_45args50050, %struct.ScmObj** %stackaddr$prim51710, align 8
%stackaddr$prim51711 = alloca %struct.ScmObj*, align 8
%b40216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50050)
store volatile %struct.ScmObj* %b40216, %struct.ScmObj** %stackaddr$prim51711, align 8
%truthy$cmp51712 = call i64 @is_truthy_value(%struct.ScmObj* %b40216)
%cmp$cmp51712 = icmp eq i64 %truthy$cmp51712, 1
br i1 %cmp$cmp51712, label %truebranch$cmp51712, label %falsebranch$cmp51712
truebranch$cmp51712:
%ae41948 = call %struct.ScmObj* @const_init_int(i64 0)
%args50052$k40543$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51713 = alloca %struct.ScmObj*, align 8
%args50052$k40543$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40216, %struct.ScmObj* %args50052$k40543$0)
store volatile %struct.ScmObj* %args50052$k40543$1, %struct.ScmObj** %stackaddr$prim51713, align 8
%stackaddr$prim51714 = alloca %struct.ScmObj*, align 8
%args50052$k40543$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41948, %struct.ScmObj* %args50052$k40543$1)
store volatile %struct.ScmObj* %args50052$k40543$2, %struct.ScmObj** %stackaddr$prim51714, align 8
%clofunc51715 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40543)
musttail call tailcc void %clofunc51715(%struct.ScmObj* %k40543, %struct.ScmObj* %args50052$k40543$2)
ret void
falsebranch$cmp51712:
%stackaddr$prim51716 = alloca %struct.ScmObj*, align 8
%cpsprim40544 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40217)
store volatile %struct.ScmObj* %cpsprim40544, %struct.ScmObj** %stackaddr$prim51716, align 8
%ae41955 = call %struct.ScmObj* @const_init_int(i64 0)
%args50053$k40543$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51717 = alloca %struct.ScmObj*, align 8
%args50053$k40543$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40544, %struct.ScmObj* %args50053$k40543$0)
store volatile %struct.ScmObj* %args50053$k40543$1, %struct.ScmObj** %stackaddr$prim51717, align 8
%stackaddr$prim51718 = alloca %struct.ScmObj*, align 8
%args50053$k40543$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41955, %struct.ScmObj* %args50053$k40543$1)
store volatile %struct.ScmObj* %args50053$k40543$2, %struct.ScmObj** %stackaddr$prim51718, align 8
%clofunc51719 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40543)
musttail call tailcc void %clofunc51719(%struct.ScmObj* %k40543, %struct.ScmObj* %args50053$k40543$2)
ret void
}

define tailcc void @proc_clo$ae41786(%struct.ScmObj* %env$ae41786,%struct.ScmObj* %args4015540545) {
%stackaddr$env-ref51720 = alloca %struct.ScmObj*, align 8
%_37last40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41786, i64 0)
store %struct.ScmObj* %_37last40150, %struct.ScmObj** %stackaddr$env-ref51720
%stackaddr$env-ref51721 = alloca %struct.ScmObj*, align 8
%_37foldr40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41786, i64 1)
store %struct.ScmObj* %_37foldr40133, %struct.ScmObj** %stackaddr$env-ref51721
%stackaddr$env-ref51722 = alloca %struct.ScmObj*, align 8
%_37drop_45right40147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41786, i64 2)
store %struct.ScmObj* %_37drop_45right40147, %struct.ScmObj** %stackaddr$env-ref51722
%stackaddr$prim51723 = alloca %struct.ScmObj*, align 8
%k40546 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4015540545)
store volatile %struct.ScmObj* %k40546, %struct.ScmObj** %stackaddr$prim51723, align 8
%stackaddr$prim51724 = alloca %struct.ScmObj*, align 8
%args40155 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4015540545)
store volatile %struct.ScmObj* %args40155, %struct.ScmObj** %stackaddr$prim51724, align 8
%stackaddr$prim51725 = alloca %struct.ScmObj*, align 8
%f40157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40155)
store volatile %struct.ScmObj* %f40157, %struct.ScmObj** %stackaddr$prim51725, align 8
%stackaddr$prim51726 = alloca %struct.ScmObj*, align 8
%lsts40156 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40155)
store volatile %struct.ScmObj* %lsts40156, %struct.ScmObj** %stackaddr$prim51726, align 8
%stackaddr$makeclosure51727 = alloca %struct.ScmObj*, align 8
%fptrToInt51728 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41791 to i64
%ae41791 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt51728)
store volatile %struct.ScmObj* %ae41791, %struct.ScmObj** %stackaddr$makeclosure51727, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41791, %struct.ScmObj* %_37foldr40133, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41791, %struct.ScmObj* %k40546, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41791, %struct.ScmObj* %lsts40156, i64 2)
%ae41792 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51729 = alloca %struct.ScmObj*, align 8
%fptrToInt51730 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41793 to i64
%ae41793 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt51730)
store volatile %struct.ScmObj* %ae41793, %struct.ScmObj** %stackaddr$makeclosure51729, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41793, %struct.ScmObj* %_37last40150, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41793, %struct.ScmObj* %_37drop_45right40147, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41793, %struct.ScmObj* %f40157, i64 2)
%args50072$ae41791$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51731 = alloca %struct.ScmObj*, align 8
%args50072$ae41791$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41793, %struct.ScmObj* %args50072$ae41791$0)
store volatile %struct.ScmObj* %args50072$ae41791$1, %struct.ScmObj** %stackaddr$prim51731, align 8
%stackaddr$prim51732 = alloca %struct.ScmObj*, align 8
%args50072$ae41791$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41792, %struct.ScmObj* %args50072$ae41791$1)
store volatile %struct.ScmObj* %args50072$ae41791$2, %struct.ScmObj** %stackaddr$prim51732, align 8
%clofunc51733 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41791)
musttail call tailcc void %clofunc51733(%struct.ScmObj* %ae41791, %struct.ScmObj* %args50072$ae41791$2)
ret void
}

define tailcc void @proc_clo$ae41791(%struct.ScmObj* %env$ae41791,%struct.ScmObj* %current_45args50057) {
%stackaddr$env-ref51734 = alloca %struct.ScmObj*, align 8
%_37foldr40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41791, i64 0)
store %struct.ScmObj* %_37foldr40133, %struct.ScmObj** %stackaddr$env-ref51734
%stackaddr$env-ref51735 = alloca %struct.ScmObj*, align 8
%k40546 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41791, i64 1)
store %struct.ScmObj* %k40546, %struct.ScmObj** %stackaddr$env-ref51735
%stackaddr$env-ref51736 = alloca %struct.ScmObj*, align 8
%lsts40156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41791, i64 2)
store %struct.ScmObj* %lsts40156, %struct.ScmObj** %stackaddr$env-ref51736
%stackaddr$prim51737 = alloca %struct.ScmObj*, align 8
%_95k40547 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50057)
store volatile %struct.ScmObj* %_95k40547, %struct.ScmObj** %stackaddr$prim51737, align 8
%stackaddr$prim51738 = alloca %struct.ScmObj*, align 8
%current_45args50058 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50057)
store volatile %struct.ScmObj* %current_45args50058, %struct.ScmObj** %stackaddr$prim51738, align 8
%stackaddr$prim51739 = alloca %struct.ScmObj*, align 8
%anf_45bind40304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50058)
store volatile %struct.ScmObj* %anf_45bind40304, %struct.ScmObj** %stackaddr$prim51739, align 8
%ae41854 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51740 = alloca %struct.ScmObj*, align 8
%anf_45bind40305 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41854, %struct.ScmObj* %lsts40156)
store volatile %struct.ScmObj* %anf_45bind40305, %struct.ScmObj** %stackaddr$prim51740, align 8
%stackaddr$prim51741 = alloca %struct.ScmObj*, align 8
%anf_45bind40306 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40304, %struct.ScmObj* %anf_45bind40305)
store volatile %struct.ScmObj* %anf_45bind40306, %struct.ScmObj** %stackaddr$prim51741, align 8
%stackaddr$prim51742 = alloca %struct.ScmObj*, align 8
%cpsargs40548 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40546, %struct.ScmObj* %anf_45bind40306)
store volatile %struct.ScmObj* %cpsargs40548, %struct.ScmObj** %stackaddr$prim51742, align 8
%clofunc51743 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40133)
musttail call tailcc void %clofunc51743(%struct.ScmObj* %_37foldr40133, %struct.ScmObj* %cpsargs40548)
ret void
}

define tailcc void @proc_clo$ae41793(%struct.ScmObj* %env$ae41793,%struct.ScmObj* %fargs4015840549) {
%stackaddr$env-ref51744 = alloca %struct.ScmObj*, align 8
%_37last40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41793, i64 0)
store %struct.ScmObj* %_37last40150, %struct.ScmObj** %stackaddr$env-ref51744
%stackaddr$env-ref51745 = alloca %struct.ScmObj*, align 8
%_37drop_45right40147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41793, i64 1)
store %struct.ScmObj* %_37drop_45right40147, %struct.ScmObj** %stackaddr$env-ref51745
%stackaddr$env-ref51746 = alloca %struct.ScmObj*, align 8
%f40157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41793, i64 2)
store %struct.ScmObj* %f40157, %struct.ScmObj** %stackaddr$env-ref51746
%stackaddr$prim51747 = alloca %struct.ScmObj*, align 8
%k40550 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4015840549)
store volatile %struct.ScmObj* %k40550, %struct.ScmObj** %stackaddr$prim51747, align 8
%stackaddr$prim51748 = alloca %struct.ScmObj*, align 8
%fargs40158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4015840549)
store volatile %struct.ScmObj* %fargs40158, %struct.ScmObj** %stackaddr$prim51748, align 8
%stackaddr$makeclosure51749 = alloca %struct.ScmObj*, align 8
%fptrToInt51750 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41797 to i64
%ae41797 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt51750)
store volatile %struct.ScmObj* %ae41797, %struct.ScmObj** %stackaddr$makeclosure51749, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41797, %struct.ScmObj* %k40550, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41797, %struct.ScmObj* %_37last40150, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41797, %struct.ScmObj* %fargs40158, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41797, %struct.ScmObj* %f40157, i64 3)
%ae41799 = call %struct.ScmObj* @const_init_int(i64 1)
%args50071$_37drop_45right40147$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51751 = alloca %struct.ScmObj*, align 8
%args50071$_37drop_45right40147$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41799, %struct.ScmObj* %args50071$_37drop_45right40147$0)
store volatile %struct.ScmObj* %args50071$_37drop_45right40147$1, %struct.ScmObj** %stackaddr$prim51751, align 8
%stackaddr$prim51752 = alloca %struct.ScmObj*, align 8
%args50071$_37drop_45right40147$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40158, %struct.ScmObj* %args50071$_37drop_45right40147$1)
store volatile %struct.ScmObj* %args50071$_37drop_45right40147$2, %struct.ScmObj** %stackaddr$prim51752, align 8
%stackaddr$prim51753 = alloca %struct.ScmObj*, align 8
%args50071$_37drop_45right40147$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41797, %struct.ScmObj* %args50071$_37drop_45right40147$2)
store volatile %struct.ScmObj* %args50071$_37drop_45right40147$3, %struct.ScmObj** %stackaddr$prim51753, align 8
%clofunc51754 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right40147)
musttail call tailcc void %clofunc51754(%struct.ScmObj* %_37drop_45right40147, %struct.ScmObj* %args50071$_37drop_45right40147$3)
ret void
}

define tailcc void @proc_clo$ae41797(%struct.ScmObj* %env$ae41797,%struct.ScmObj* %current_45args50060) {
%stackaddr$env-ref51755 = alloca %struct.ScmObj*, align 8
%k40550 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41797, i64 0)
store %struct.ScmObj* %k40550, %struct.ScmObj** %stackaddr$env-ref51755
%stackaddr$env-ref51756 = alloca %struct.ScmObj*, align 8
%_37last40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41797, i64 1)
store %struct.ScmObj* %_37last40150, %struct.ScmObj** %stackaddr$env-ref51756
%stackaddr$env-ref51757 = alloca %struct.ScmObj*, align 8
%fargs40158 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41797, i64 2)
store %struct.ScmObj* %fargs40158, %struct.ScmObj** %stackaddr$env-ref51757
%stackaddr$env-ref51758 = alloca %struct.ScmObj*, align 8
%f40157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41797, i64 3)
store %struct.ScmObj* %f40157, %struct.ScmObj** %stackaddr$env-ref51758
%stackaddr$prim51759 = alloca %struct.ScmObj*, align 8
%_95k40551 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50060)
store volatile %struct.ScmObj* %_95k40551, %struct.ScmObj** %stackaddr$prim51759, align 8
%stackaddr$prim51760 = alloca %struct.ScmObj*, align 8
%current_45args50061 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50060)
store volatile %struct.ScmObj* %current_45args50061, %struct.ScmObj** %stackaddr$prim51760, align 8
%stackaddr$prim51761 = alloca %struct.ScmObj*, align 8
%anf_45bind40301 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50061)
store volatile %struct.ScmObj* %anf_45bind40301, %struct.ScmObj** %stackaddr$prim51761, align 8
%stackaddr$makeclosure51762 = alloca %struct.ScmObj*, align 8
%fptrToInt51763 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41804 to i64
%ae41804 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt51763)
store volatile %struct.ScmObj* %ae41804, %struct.ScmObj** %stackaddr$makeclosure51762, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41804, %struct.ScmObj* %k40550, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41804, %struct.ScmObj* %_37last40150, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41804, %struct.ScmObj* %fargs40158, i64 2)
%stackaddr$prim51764 = alloca %struct.ScmObj*, align 8
%cpsargs40555 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41804, %struct.ScmObj* %anf_45bind40301)
store volatile %struct.ScmObj* %cpsargs40555, %struct.ScmObj** %stackaddr$prim51764, align 8
%clofunc51765 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40157)
musttail call tailcc void %clofunc51765(%struct.ScmObj* %f40157, %struct.ScmObj* %cpsargs40555)
ret void
}

define tailcc void @proc_clo$ae41804(%struct.ScmObj* %env$ae41804,%struct.ScmObj* %current_45args50063) {
%stackaddr$env-ref51766 = alloca %struct.ScmObj*, align 8
%k40550 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41804, i64 0)
store %struct.ScmObj* %k40550, %struct.ScmObj** %stackaddr$env-ref51766
%stackaddr$env-ref51767 = alloca %struct.ScmObj*, align 8
%_37last40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41804, i64 1)
store %struct.ScmObj* %_37last40150, %struct.ScmObj** %stackaddr$env-ref51767
%stackaddr$env-ref51768 = alloca %struct.ScmObj*, align 8
%fargs40158 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41804, i64 2)
store %struct.ScmObj* %fargs40158, %struct.ScmObj** %stackaddr$env-ref51768
%stackaddr$prim51769 = alloca %struct.ScmObj*, align 8
%_95k40552 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50063)
store volatile %struct.ScmObj* %_95k40552, %struct.ScmObj** %stackaddr$prim51769, align 8
%stackaddr$prim51770 = alloca %struct.ScmObj*, align 8
%current_45args50064 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50063)
store volatile %struct.ScmObj* %current_45args50064, %struct.ScmObj** %stackaddr$prim51770, align 8
%stackaddr$prim51771 = alloca %struct.ScmObj*, align 8
%anf_45bind40302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50064)
store volatile %struct.ScmObj* %anf_45bind40302, %struct.ScmObj** %stackaddr$prim51771, align 8
%stackaddr$makeclosure51772 = alloca %struct.ScmObj*, align 8
%fptrToInt51773 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41809 to i64
%ae41809 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt51773)
store volatile %struct.ScmObj* %ae41809, %struct.ScmObj** %stackaddr$makeclosure51772, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41809, %struct.ScmObj* %k40550, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41809, %struct.ScmObj* %anf_45bind40302, i64 1)
%args50070$_37last40150$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51774 = alloca %struct.ScmObj*, align 8
%args50070$_37last40150$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40158, %struct.ScmObj* %args50070$_37last40150$0)
store volatile %struct.ScmObj* %args50070$_37last40150$1, %struct.ScmObj** %stackaddr$prim51774, align 8
%stackaddr$prim51775 = alloca %struct.ScmObj*, align 8
%args50070$_37last40150$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41809, %struct.ScmObj* %args50070$_37last40150$1)
store volatile %struct.ScmObj* %args50070$_37last40150$2, %struct.ScmObj** %stackaddr$prim51775, align 8
%clofunc51776 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last40150)
musttail call tailcc void %clofunc51776(%struct.ScmObj* %_37last40150, %struct.ScmObj* %args50070$_37last40150$2)
ret void
}

define tailcc void @proc_clo$ae41809(%struct.ScmObj* %env$ae41809,%struct.ScmObj* %current_45args50066) {
%stackaddr$env-ref51777 = alloca %struct.ScmObj*, align 8
%k40550 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41809, i64 0)
store %struct.ScmObj* %k40550, %struct.ScmObj** %stackaddr$env-ref51777
%stackaddr$env-ref51778 = alloca %struct.ScmObj*, align 8
%anf_45bind40302 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41809, i64 1)
store %struct.ScmObj* %anf_45bind40302, %struct.ScmObj** %stackaddr$env-ref51778
%stackaddr$prim51779 = alloca %struct.ScmObj*, align 8
%_95k40553 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50066)
store volatile %struct.ScmObj* %_95k40553, %struct.ScmObj** %stackaddr$prim51779, align 8
%stackaddr$prim51780 = alloca %struct.ScmObj*, align 8
%current_45args50067 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50066)
store volatile %struct.ScmObj* %current_45args50067, %struct.ScmObj** %stackaddr$prim51780, align 8
%stackaddr$prim51781 = alloca %struct.ScmObj*, align 8
%anf_45bind40303 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50067)
store volatile %struct.ScmObj* %anf_45bind40303, %struct.ScmObj** %stackaddr$prim51781, align 8
%stackaddr$prim51782 = alloca %struct.ScmObj*, align 8
%cpsprim40554 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40302, %struct.ScmObj* %anf_45bind40303)
store volatile %struct.ScmObj* %cpsprim40554, %struct.ScmObj** %stackaddr$prim51782, align 8
%ae41814 = call %struct.ScmObj* @const_init_int(i64 0)
%args50069$k40550$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51783 = alloca %struct.ScmObj*, align 8
%args50069$k40550$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40554, %struct.ScmObj* %args50069$k40550$0)
store volatile %struct.ScmObj* %args50069$k40550$1, %struct.ScmObj** %stackaddr$prim51783, align 8
%stackaddr$prim51784 = alloca %struct.ScmObj*, align 8
%args50069$k40550$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41814, %struct.ScmObj* %args50069$k40550$1)
store volatile %struct.ScmObj* %args50069$k40550$2, %struct.ScmObj** %stackaddr$prim51784, align 8
%clofunc51785 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40550)
musttail call tailcc void %clofunc51785(%struct.ScmObj* %k40550, %struct.ScmObj* %args50069$k40550$2)
ret void
}

define tailcc void @proc_clo$ae41709(%struct.ScmObj* %env$ae41709,%struct.ScmObj* %current_45args50074) {
%stackaddr$env-ref51786 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41709, i64 0)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref51786
%stackaddr$prim51787 = alloca %struct.ScmObj*, align 8
%k40556 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50074)
store volatile %struct.ScmObj* %k40556, %struct.ScmObj** %stackaddr$prim51787, align 8
%stackaddr$prim51788 = alloca %struct.ScmObj*, align 8
%current_45args50075 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50074)
store volatile %struct.ScmObj* %current_45args50075, %struct.ScmObj** %stackaddr$prim51788, align 8
%stackaddr$prim51789 = alloca %struct.ScmObj*, align 8
%f40161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50075)
store volatile %struct.ScmObj* %f40161, %struct.ScmObj** %stackaddr$prim51789, align 8
%stackaddr$prim51790 = alloca %struct.ScmObj*, align 8
%current_45args50076 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50075)
store volatile %struct.ScmObj* %current_45args50076, %struct.ScmObj** %stackaddr$prim51790, align 8
%stackaddr$prim51791 = alloca %struct.ScmObj*, align 8
%lst40160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50076)
store volatile %struct.ScmObj* %lst40160, %struct.ScmObj** %stackaddr$prim51791, align 8
%stackaddr$makeclosure51792 = alloca %struct.ScmObj*, align 8
%fptrToInt51793 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41710 to i64
%ae41710 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt51793)
store volatile %struct.ScmObj* %ae41710, %struct.ScmObj** %stackaddr$makeclosure51792, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41710, %struct.ScmObj* %lst40160, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41710, %struct.ScmObj* %_37foldr140128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41710, %struct.ScmObj* %k40556, i64 2)
%ae41711 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51794 = alloca %struct.ScmObj*, align 8
%fptrToInt51795 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41712 to i64
%ae41712 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt51795)
store volatile %struct.ScmObj* %ae41712, %struct.ScmObj** %stackaddr$makeclosure51794, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41712, %struct.ScmObj* %f40161, i64 0)
%args50091$ae41710$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51796 = alloca %struct.ScmObj*, align 8
%args50091$ae41710$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41712, %struct.ScmObj* %args50091$ae41710$0)
store volatile %struct.ScmObj* %args50091$ae41710$1, %struct.ScmObj** %stackaddr$prim51796, align 8
%stackaddr$prim51797 = alloca %struct.ScmObj*, align 8
%args50091$ae41710$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41711, %struct.ScmObj* %args50091$ae41710$1)
store volatile %struct.ScmObj* %args50091$ae41710$2, %struct.ScmObj** %stackaddr$prim51797, align 8
%clofunc51798 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41710)
musttail call tailcc void %clofunc51798(%struct.ScmObj* %ae41710, %struct.ScmObj* %args50091$ae41710$2)
ret void
}

define tailcc void @proc_clo$ae41710(%struct.ScmObj* %env$ae41710,%struct.ScmObj* %current_45args50078) {
%stackaddr$env-ref51799 = alloca %struct.ScmObj*, align 8
%lst40160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41710, i64 0)
store %struct.ScmObj* %lst40160, %struct.ScmObj** %stackaddr$env-ref51799
%stackaddr$env-ref51800 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41710, i64 1)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref51800
%stackaddr$env-ref51801 = alloca %struct.ScmObj*, align 8
%k40556 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41710, i64 2)
store %struct.ScmObj* %k40556, %struct.ScmObj** %stackaddr$env-ref51801
%stackaddr$prim51802 = alloca %struct.ScmObj*, align 8
%_95k40557 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50078)
store volatile %struct.ScmObj* %_95k40557, %struct.ScmObj** %stackaddr$prim51802, align 8
%stackaddr$prim51803 = alloca %struct.ScmObj*, align 8
%current_45args50079 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50078)
store volatile %struct.ScmObj* %current_45args50079, %struct.ScmObj** %stackaddr$prim51803, align 8
%stackaddr$prim51804 = alloca %struct.ScmObj*, align 8
%anf_45bind40300 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50079)
store volatile %struct.ScmObj* %anf_45bind40300, %struct.ScmObj** %stackaddr$prim51804, align 8
%ae41744 = call %struct.ScmObj* @const_init_null()
%args50081$_37foldr140128$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51805 = alloca %struct.ScmObj*, align 8
%args50081$_37foldr140128$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40160, %struct.ScmObj* %args50081$_37foldr140128$0)
store volatile %struct.ScmObj* %args50081$_37foldr140128$1, %struct.ScmObj** %stackaddr$prim51805, align 8
%stackaddr$prim51806 = alloca %struct.ScmObj*, align 8
%args50081$_37foldr140128$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41744, %struct.ScmObj* %args50081$_37foldr140128$1)
store volatile %struct.ScmObj* %args50081$_37foldr140128$2, %struct.ScmObj** %stackaddr$prim51806, align 8
%stackaddr$prim51807 = alloca %struct.ScmObj*, align 8
%args50081$_37foldr140128$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40300, %struct.ScmObj* %args50081$_37foldr140128$2)
store volatile %struct.ScmObj* %args50081$_37foldr140128$3, %struct.ScmObj** %stackaddr$prim51807, align 8
%stackaddr$prim51808 = alloca %struct.ScmObj*, align 8
%args50081$_37foldr140128$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40556, %struct.ScmObj* %args50081$_37foldr140128$3)
store volatile %struct.ScmObj* %args50081$_37foldr140128$4, %struct.ScmObj** %stackaddr$prim51808, align 8
%clofunc51809 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140128)
musttail call tailcc void %clofunc51809(%struct.ScmObj* %_37foldr140128, %struct.ScmObj* %args50081$_37foldr140128$4)
ret void
}

define tailcc void @proc_clo$ae41712(%struct.ScmObj* %env$ae41712,%struct.ScmObj* %current_45args50082) {
%stackaddr$env-ref51810 = alloca %struct.ScmObj*, align 8
%f40161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41712, i64 0)
store %struct.ScmObj* %f40161, %struct.ScmObj** %stackaddr$env-ref51810
%stackaddr$prim51811 = alloca %struct.ScmObj*, align 8
%k40558 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50082)
store volatile %struct.ScmObj* %k40558, %struct.ScmObj** %stackaddr$prim51811, align 8
%stackaddr$prim51812 = alloca %struct.ScmObj*, align 8
%current_45args50083 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50082)
store volatile %struct.ScmObj* %current_45args50083, %struct.ScmObj** %stackaddr$prim51812, align 8
%stackaddr$prim51813 = alloca %struct.ScmObj*, align 8
%v40163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50083)
store volatile %struct.ScmObj* %v40163, %struct.ScmObj** %stackaddr$prim51813, align 8
%stackaddr$prim51814 = alloca %struct.ScmObj*, align 8
%current_45args50084 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50083)
store volatile %struct.ScmObj* %current_45args50084, %struct.ScmObj** %stackaddr$prim51814, align 8
%stackaddr$prim51815 = alloca %struct.ScmObj*, align 8
%r40162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50084)
store volatile %struct.ScmObj* %r40162, %struct.ScmObj** %stackaddr$prim51815, align 8
%stackaddr$makeclosure51816 = alloca %struct.ScmObj*, align 8
%fptrToInt51817 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41714 to i64
%ae41714 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt51817)
store volatile %struct.ScmObj* %ae41714, %struct.ScmObj** %stackaddr$makeclosure51816, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41714, %struct.ScmObj* %r40162, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41714, %struct.ScmObj* %k40558, i64 1)
%args50090$f40161$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51818 = alloca %struct.ScmObj*, align 8
%args50090$f40161$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v40163, %struct.ScmObj* %args50090$f40161$0)
store volatile %struct.ScmObj* %args50090$f40161$1, %struct.ScmObj** %stackaddr$prim51818, align 8
%stackaddr$prim51819 = alloca %struct.ScmObj*, align 8
%args50090$f40161$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41714, %struct.ScmObj* %args50090$f40161$1)
store volatile %struct.ScmObj* %args50090$f40161$2, %struct.ScmObj** %stackaddr$prim51819, align 8
%clofunc51820 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40161)
musttail call tailcc void %clofunc51820(%struct.ScmObj* %f40161, %struct.ScmObj* %args50090$f40161$2)
ret void
}

define tailcc void @proc_clo$ae41714(%struct.ScmObj* %env$ae41714,%struct.ScmObj* %current_45args50086) {
%stackaddr$env-ref51821 = alloca %struct.ScmObj*, align 8
%r40162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41714, i64 0)
store %struct.ScmObj* %r40162, %struct.ScmObj** %stackaddr$env-ref51821
%stackaddr$env-ref51822 = alloca %struct.ScmObj*, align 8
%k40558 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41714, i64 1)
store %struct.ScmObj* %k40558, %struct.ScmObj** %stackaddr$env-ref51822
%stackaddr$prim51823 = alloca %struct.ScmObj*, align 8
%_95k40559 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50086)
store volatile %struct.ScmObj* %_95k40559, %struct.ScmObj** %stackaddr$prim51823, align 8
%stackaddr$prim51824 = alloca %struct.ScmObj*, align 8
%current_45args50087 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50086)
store volatile %struct.ScmObj* %current_45args50087, %struct.ScmObj** %stackaddr$prim51824, align 8
%stackaddr$prim51825 = alloca %struct.ScmObj*, align 8
%anf_45bind40299 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50087)
store volatile %struct.ScmObj* %anf_45bind40299, %struct.ScmObj** %stackaddr$prim51825, align 8
%stackaddr$prim51826 = alloca %struct.ScmObj*, align 8
%cpsprim40560 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40299, %struct.ScmObj* %r40162)
store volatile %struct.ScmObj* %cpsprim40560, %struct.ScmObj** %stackaddr$prim51826, align 8
%ae41719 = call %struct.ScmObj* @const_init_int(i64 0)
%args50089$k40558$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51827 = alloca %struct.ScmObj*, align 8
%args50089$k40558$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40560, %struct.ScmObj* %args50089$k40558$0)
store volatile %struct.ScmObj* %args50089$k40558$1, %struct.ScmObj** %stackaddr$prim51827, align 8
%stackaddr$prim51828 = alloca %struct.ScmObj*, align 8
%args50089$k40558$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41719, %struct.ScmObj* %args50089$k40558$1)
store volatile %struct.ScmObj* %args50089$k40558$2, %struct.ScmObj** %stackaddr$prim51828, align 8
%clofunc51829 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40558)
musttail call tailcc void %clofunc51829(%struct.ScmObj* %k40558, %struct.ScmObj* %args50089$k40558$2)
ret void
}

define tailcc void @proc_clo$ae41323(%struct.ScmObj* %env$ae41323,%struct.ScmObj* %current_45args50094) {
%stackaddr$env-ref51830 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41323, i64 0)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref51830
%stackaddr$env-ref51831 = alloca %struct.ScmObj*, align 8
%_37map140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41323, i64 1)
store %struct.ScmObj* %_37map140124, %struct.ScmObj** %stackaddr$env-ref51831
%stackaddr$prim51832 = alloca %struct.ScmObj*, align 8
%k40561 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50094)
store volatile %struct.ScmObj* %k40561, %struct.ScmObj** %stackaddr$prim51832, align 8
%stackaddr$prim51833 = alloca %struct.ScmObj*, align 8
%current_45args50095 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50094)
store volatile %struct.ScmObj* %current_45args50095, %struct.ScmObj** %stackaddr$prim51833, align 8
%stackaddr$prim51834 = alloca %struct.ScmObj*, align 8
%_37foldr40134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50095)
store volatile %struct.ScmObj* %_37foldr40134, %struct.ScmObj** %stackaddr$prim51834, align 8
%ae41325 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51835 = alloca %struct.ScmObj*, align 8
%fptrToInt51836 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41326 to i64
%ae41326 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt51836)
store volatile %struct.ScmObj* %ae41326, %struct.ScmObj** %stackaddr$makeclosure51835, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41326, %struct.ScmObj* %_37foldr40134, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41326, %struct.ScmObj* %_37foldr140128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41326, %struct.ScmObj* %_37map140124, i64 2)
%args50152$k40561$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51837 = alloca %struct.ScmObj*, align 8
%args50152$k40561$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41326, %struct.ScmObj* %args50152$k40561$0)
store volatile %struct.ScmObj* %args50152$k40561$1, %struct.ScmObj** %stackaddr$prim51837, align 8
%stackaddr$prim51838 = alloca %struct.ScmObj*, align 8
%args50152$k40561$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41325, %struct.ScmObj* %args50152$k40561$1)
store volatile %struct.ScmObj* %args50152$k40561$2, %struct.ScmObj** %stackaddr$prim51838, align 8
%clofunc51839 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40561)
musttail call tailcc void %clofunc51839(%struct.ScmObj* %k40561, %struct.ScmObj* %args50152$k40561$2)
ret void
}

define tailcc void @proc_clo$ae41326(%struct.ScmObj* %env$ae41326,%struct.ScmObj* %args4013540562) {
%stackaddr$env-ref51840 = alloca %struct.ScmObj*, align 8
%_37foldr40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41326, i64 0)
store %struct.ScmObj* %_37foldr40134, %struct.ScmObj** %stackaddr$env-ref51840
%stackaddr$env-ref51841 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41326, i64 1)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref51841
%stackaddr$env-ref51842 = alloca %struct.ScmObj*, align 8
%_37map140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41326, i64 2)
store %struct.ScmObj* %_37map140124, %struct.ScmObj** %stackaddr$env-ref51842
%stackaddr$prim51843 = alloca %struct.ScmObj*, align 8
%k40563 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4013540562)
store volatile %struct.ScmObj* %k40563, %struct.ScmObj** %stackaddr$prim51843, align 8
%stackaddr$prim51844 = alloca %struct.ScmObj*, align 8
%args40135 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4013540562)
store volatile %struct.ScmObj* %args40135, %struct.ScmObj** %stackaddr$prim51844, align 8
%stackaddr$prim51845 = alloca %struct.ScmObj*, align 8
%f40138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40135)
store volatile %struct.ScmObj* %f40138, %struct.ScmObj** %stackaddr$prim51845, align 8
%stackaddr$prim51846 = alloca %struct.ScmObj*, align 8
%anf_45bind40286 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40135)
store volatile %struct.ScmObj* %anf_45bind40286, %struct.ScmObj** %stackaddr$prim51846, align 8
%stackaddr$prim51847 = alloca %struct.ScmObj*, align 8
%acc40137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40286)
store volatile %struct.ScmObj* %acc40137, %struct.ScmObj** %stackaddr$prim51847, align 8
%stackaddr$prim51848 = alloca %struct.ScmObj*, align 8
%anf_45bind40287 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40135)
store volatile %struct.ScmObj* %anf_45bind40287, %struct.ScmObj** %stackaddr$prim51848, align 8
%stackaddr$prim51849 = alloca %struct.ScmObj*, align 8
%lsts40136 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40287)
store volatile %struct.ScmObj* %lsts40136, %struct.ScmObj** %stackaddr$prim51849, align 8
%stackaddr$makeclosure51850 = alloca %struct.ScmObj*, align 8
%fptrToInt51851 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41334 to i64
%ae41334 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt51851)
store volatile %struct.ScmObj* %ae41334, %struct.ScmObj** %stackaddr$makeclosure51850, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41334, %struct.ScmObj* %acc40137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41334, %struct.ScmObj* %lsts40136, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41334, %struct.ScmObj* %_37foldr40134, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41334, %struct.ScmObj* %k40563, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41334, %struct.ScmObj* %_37foldr140128, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41334, %struct.ScmObj* %_37map140124, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41334, %struct.ScmObj* %f40138, i64 6)
%ae41335 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51852 = alloca %struct.ScmObj*, align 8
%fptrToInt51853 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41336 to i64
%ae41336 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt51853)
store volatile %struct.ScmObj* %ae41336, %struct.ScmObj** %stackaddr$makeclosure51852, align 8
%args50151$ae41334$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51854 = alloca %struct.ScmObj*, align 8
%args50151$ae41334$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41336, %struct.ScmObj* %args50151$ae41334$0)
store volatile %struct.ScmObj* %args50151$ae41334$1, %struct.ScmObj** %stackaddr$prim51854, align 8
%stackaddr$prim51855 = alloca %struct.ScmObj*, align 8
%args50151$ae41334$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41335, %struct.ScmObj* %args50151$ae41334$1)
store volatile %struct.ScmObj* %args50151$ae41334$2, %struct.ScmObj** %stackaddr$prim51855, align 8
%clofunc51856 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41334)
musttail call tailcc void %clofunc51856(%struct.ScmObj* %ae41334, %struct.ScmObj* %args50151$ae41334$2)
ret void
}

define tailcc void @proc_clo$ae41334(%struct.ScmObj* %env$ae41334,%struct.ScmObj* %current_45args50097) {
%stackaddr$env-ref51857 = alloca %struct.ScmObj*, align 8
%acc40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41334, i64 0)
store %struct.ScmObj* %acc40137, %struct.ScmObj** %stackaddr$env-ref51857
%stackaddr$env-ref51858 = alloca %struct.ScmObj*, align 8
%lsts40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41334, i64 1)
store %struct.ScmObj* %lsts40136, %struct.ScmObj** %stackaddr$env-ref51858
%stackaddr$env-ref51859 = alloca %struct.ScmObj*, align 8
%_37foldr40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41334, i64 2)
store %struct.ScmObj* %_37foldr40134, %struct.ScmObj** %stackaddr$env-ref51859
%stackaddr$env-ref51860 = alloca %struct.ScmObj*, align 8
%k40563 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41334, i64 3)
store %struct.ScmObj* %k40563, %struct.ScmObj** %stackaddr$env-ref51860
%stackaddr$env-ref51861 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41334, i64 4)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref51861
%stackaddr$env-ref51862 = alloca %struct.ScmObj*, align 8
%_37map140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41334, i64 5)
store %struct.ScmObj* %_37map140124, %struct.ScmObj** %stackaddr$env-ref51862
%stackaddr$env-ref51863 = alloca %struct.ScmObj*, align 8
%f40138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41334, i64 6)
store %struct.ScmObj* %f40138, %struct.ScmObj** %stackaddr$env-ref51863
%stackaddr$prim51864 = alloca %struct.ScmObj*, align 8
%_95k40564 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50097)
store volatile %struct.ScmObj* %_95k40564, %struct.ScmObj** %stackaddr$prim51864, align 8
%stackaddr$prim51865 = alloca %struct.ScmObj*, align 8
%current_45args50098 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50097)
store volatile %struct.ScmObj* %current_45args50098, %struct.ScmObj** %stackaddr$prim51865, align 8
%stackaddr$prim51866 = alloca %struct.ScmObj*, align 8
%anf_45bind40288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50098)
store volatile %struct.ScmObj* %anf_45bind40288, %struct.ScmObj** %stackaddr$prim51866, align 8
%stackaddr$makeclosure51867 = alloca %struct.ScmObj*, align 8
%fptrToInt51868 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41366 to i64
%ae41366 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt51868)
store volatile %struct.ScmObj* %ae41366, %struct.ScmObj** %stackaddr$makeclosure51867, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41366, %struct.ScmObj* %acc40137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41366, %struct.ScmObj* %lsts40136, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41366, %struct.ScmObj* %_37foldr40134, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41366, %struct.ScmObj* %k40563, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41366, %struct.ScmObj* %_37foldr140128, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41366, %struct.ScmObj* %_37map140124, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41366, %struct.ScmObj* %f40138, i64 6)
%ae41368 = call %struct.ScmObj* @const_init_false()
%args50144$_37foldr140128$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51869 = alloca %struct.ScmObj*, align 8
%args50144$_37foldr140128$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40136, %struct.ScmObj* %args50144$_37foldr140128$0)
store volatile %struct.ScmObj* %args50144$_37foldr140128$1, %struct.ScmObj** %stackaddr$prim51869, align 8
%stackaddr$prim51870 = alloca %struct.ScmObj*, align 8
%args50144$_37foldr140128$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41368, %struct.ScmObj* %args50144$_37foldr140128$1)
store volatile %struct.ScmObj* %args50144$_37foldr140128$2, %struct.ScmObj** %stackaddr$prim51870, align 8
%stackaddr$prim51871 = alloca %struct.ScmObj*, align 8
%args50144$_37foldr140128$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40288, %struct.ScmObj* %args50144$_37foldr140128$2)
store volatile %struct.ScmObj* %args50144$_37foldr140128$3, %struct.ScmObj** %stackaddr$prim51871, align 8
%stackaddr$prim51872 = alloca %struct.ScmObj*, align 8
%args50144$_37foldr140128$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41366, %struct.ScmObj* %args50144$_37foldr140128$3)
store volatile %struct.ScmObj* %args50144$_37foldr140128$4, %struct.ScmObj** %stackaddr$prim51872, align 8
%clofunc51873 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140128)
musttail call tailcc void %clofunc51873(%struct.ScmObj* %_37foldr140128, %struct.ScmObj* %args50144$_37foldr140128$4)
ret void
}

define tailcc void @proc_clo$ae41366(%struct.ScmObj* %env$ae41366,%struct.ScmObj* %current_45args50100) {
%stackaddr$env-ref51874 = alloca %struct.ScmObj*, align 8
%acc40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41366, i64 0)
store %struct.ScmObj* %acc40137, %struct.ScmObj** %stackaddr$env-ref51874
%stackaddr$env-ref51875 = alloca %struct.ScmObj*, align 8
%lsts40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41366, i64 1)
store %struct.ScmObj* %lsts40136, %struct.ScmObj** %stackaddr$env-ref51875
%stackaddr$env-ref51876 = alloca %struct.ScmObj*, align 8
%_37foldr40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41366, i64 2)
store %struct.ScmObj* %_37foldr40134, %struct.ScmObj** %stackaddr$env-ref51876
%stackaddr$env-ref51877 = alloca %struct.ScmObj*, align 8
%k40563 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41366, i64 3)
store %struct.ScmObj* %k40563, %struct.ScmObj** %stackaddr$env-ref51877
%stackaddr$env-ref51878 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41366, i64 4)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref51878
%stackaddr$env-ref51879 = alloca %struct.ScmObj*, align 8
%_37map140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41366, i64 5)
store %struct.ScmObj* %_37map140124, %struct.ScmObj** %stackaddr$env-ref51879
%stackaddr$env-ref51880 = alloca %struct.ScmObj*, align 8
%f40138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41366, i64 6)
store %struct.ScmObj* %f40138, %struct.ScmObj** %stackaddr$env-ref51880
%stackaddr$prim51881 = alloca %struct.ScmObj*, align 8
%_95k40565 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50100)
store volatile %struct.ScmObj* %_95k40565, %struct.ScmObj** %stackaddr$prim51881, align 8
%stackaddr$prim51882 = alloca %struct.ScmObj*, align 8
%current_45args50101 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50100)
store volatile %struct.ScmObj* %current_45args50101, %struct.ScmObj** %stackaddr$prim51882, align 8
%stackaddr$prim51883 = alloca %struct.ScmObj*, align 8
%anf_45bind40289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50101)
store volatile %struct.ScmObj* %anf_45bind40289, %struct.ScmObj** %stackaddr$prim51883, align 8
%truthy$cmp51884 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40289)
%cmp$cmp51884 = icmp eq i64 %truthy$cmp51884, 1
br i1 %cmp$cmp51884, label %truebranch$cmp51884, label %falsebranch$cmp51884
truebranch$cmp51884:
%ae41377 = call %struct.ScmObj* @const_init_int(i64 0)
%args50103$k40563$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51885 = alloca %struct.ScmObj*, align 8
%args50103$k40563$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40137, %struct.ScmObj* %args50103$k40563$0)
store volatile %struct.ScmObj* %args50103$k40563$1, %struct.ScmObj** %stackaddr$prim51885, align 8
%stackaddr$prim51886 = alloca %struct.ScmObj*, align 8
%args50103$k40563$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41377, %struct.ScmObj* %args50103$k40563$1)
store volatile %struct.ScmObj* %args50103$k40563$2, %struct.ScmObj** %stackaddr$prim51886, align 8
%clofunc51887 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40563)
musttail call tailcc void %clofunc51887(%struct.ScmObj* %k40563, %struct.ScmObj* %args50103$k40563$2)
ret void
falsebranch$cmp51884:
%stackaddr$makeclosure51888 = alloca %struct.ScmObj*, align 8
%fptrToInt51889 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41382 to i64
%ae41382 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt51889)
store volatile %struct.ScmObj* %ae41382, %struct.ScmObj** %stackaddr$makeclosure51888, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41382, %struct.ScmObj* %acc40137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41382, %struct.ScmObj* %lsts40136, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41382, %struct.ScmObj* %_37foldr40134, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41382, %struct.ScmObj* %k40563, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41382, %struct.ScmObj* %_37foldr140128, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41382, %struct.ScmObj* %_37map140124, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41382, %struct.ScmObj* %f40138, i64 6)
%ae41383 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51890 = alloca %struct.ScmObj*, align 8
%fptrToInt51891 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41384 to i64
%ae41384 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt51891)
store volatile %struct.ScmObj* %ae41384, %struct.ScmObj** %stackaddr$makeclosure51890, align 8
%args50143$ae41382$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51892 = alloca %struct.ScmObj*, align 8
%args50143$ae41382$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41384, %struct.ScmObj* %args50143$ae41382$0)
store volatile %struct.ScmObj* %args50143$ae41382$1, %struct.ScmObj** %stackaddr$prim51892, align 8
%stackaddr$prim51893 = alloca %struct.ScmObj*, align 8
%args50143$ae41382$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41383, %struct.ScmObj* %args50143$ae41382$1)
store volatile %struct.ScmObj* %args50143$ae41382$2, %struct.ScmObj** %stackaddr$prim51893, align 8
%clofunc51894 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41382)
musttail call tailcc void %clofunc51894(%struct.ScmObj* %ae41382, %struct.ScmObj* %args50143$ae41382$2)
ret void
}

define tailcc void @proc_clo$ae41382(%struct.ScmObj* %env$ae41382,%struct.ScmObj* %current_45args50104) {
%stackaddr$env-ref51895 = alloca %struct.ScmObj*, align 8
%acc40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41382, i64 0)
store %struct.ScmObj* %acc40137, %struct.ScmObj** %stackaddr$env-ref51895
%stackaddr$env-ref51896 = alloca %struct.ScmObj*, align 8
%lsts40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41382, i64 1)
store %struct.ScmObj* %lsts40136, %struct.ScmObj** %stackaddr$env-ref51896
%stackaddr$env-ref51897 = alloca %struct.ScmObj*, align 8
%_37foldr40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41382, i64 2)
store %struct.ScmObj* %_37foldr40134, %struct.ScmObj** %stackaddr$env-ref51897
%stackaddr$env-ref51898 = alloca %struct.ScmObj*, align 8
%k40563 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41382, i64 3)
store %struct.ScmObj* %k40563, %struct.ScmObj** %stackaddr$env-ref51898
%stackaddr$env-ref51899 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41382, i64 4)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref51899
%stackaddr$env-ref51900 = alloca %struct.ScmObj*, align 8
%_37map140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41382, i64 5)
store %struct.ScmObj* %_37map140124, %struct.ScmObj** %stackaddr$env-ref51900
%stackaddr$env-ref51901 = alloca %struct.ScmObj*, align 8
%f40138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41382, i64 6)
store %struct.ScmObj* %f40138, %struct.ScmObj** %stackaddr$env-ref51901
%stackaddr$prim51902 = alloca %struct.ScmObj*, align 8
%_95k40566 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50104)
store volatile %struct.ScmObj* %_95k40566, %struct.ScmObj** %stackaddr$prim51902, align 8
%stackaddr$prim51903 = alloca %struct.ScmObj*, align 8
%current_45args50105 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50104)
store volatile %struct.ScmObj* %current_45args50105, %struct.ScmObj** %stackaddr$prim51903, align 8
%stackaddr$prim51904 = alloca %struct.ScmObj*, align 8
%anf_45bind40290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50105)
store volatile %struct.ScmObj* %anf_45bind40290, %struct.ScmObj** %stackaddr$prim51904, align 8
%stackaddr$makeclosure51905 = alloca %struct.ScmObj*, align 8
%fptrToInt51906 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41403 to i64
%ae41403 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt51906)
store volatile %struct.ScmObj* %ae41403, %struct.ScmObj** %stackaddr$makeclosure51905, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41403, %struct.ScmObj* %acc40137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41403, %struct.ScmObj* %lsts40136, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41403, %struct.ScmObj* %_37foldr40134, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41403, %struct.ScmObj* %k40563, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41403, %struct.ScmObj* %_37foldr140128, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41403, %struct.ScmObj* %_37map140124, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41403, %struct.ScmObj* %f40138, i64 6)
%args50138$_37map140124$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51907 = alloca %struct.ScmObj*, align 8
%args50138$_37map140124$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40136, %struct.ScmObj* %args50138$_37map140124$0)
store volatile %struct.ScmObj* %args50138$_37map140124$1, %struct.ScmObj** %stackaddr$prim51907, align 8
%stackaddr$prim51908 = alloca %struct.ScmObj*, align 8
%args50138$_37map140124$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40290, %struct.ScmObj* %args50138$_37map140124$1)
store volatile %struct.ScmObj* %args50138$_37map140124$2, %struct.ScmObj** %stackaddr$prim51908, align 8
%stackaddr$prim51909 = alloca %struct.ScmObj*, align 8
%args50138$_37map140124$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41403, %struct.ScmObj* %args50138$_37map140124$2)
store volatile %struct.ScmObj* %args50138$_37map140124$3, %struct.ScmObj** %stackaddr$prim51909, align 8
%clofunc51910 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140124)
musttail call tailcc void %clofunc51910(%struct.ScmObj* %_37map140124, %struct.ScmObj* %args50138$_37map140124$3)
ret void
}

define tailcc void @proc_clo$ae41403(%struct.ScmObj* %env$ae41403,%struct.ScmObj* %current_45args50107) {
%stackaddr$env-ref51911 = alloca %struct.ScmObj*, align 8
%acc40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41403, i64 0)
store %struct.ScmObj* %acc40137, %struct.ScmObj** %stackaddr$env-ref51911
%stackaddr$env-ref51912 = alloca %struct.ScmObj*, align 8
%lsts40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41403, i64 1)
store %struct.ScmObj* %lsts40136, %struct.ScmObj** %stackaddr$env-ref51912
%stackaddr$env-ref51913 = alloca %struct.ScmObj*, align 8
%_37foldr40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41403, i64 2)
store %struct.ScmObj* %_37foldr40134, %struct.ScmObj** %stackaddr$env-ref51913
%stackaddr$env-ref51914 = alloca %struct.ScmObj*, align 8
%k40563 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41403, i64 3)
store %struct.ScmObj* %k40563, %struct.ScmObj** %stackaddr$env-ref51914
%stackaddr$env-ref51915 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41403, i64 4)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref51915
%stackaddr$env-ref51916 = alloca %struct.ScmObj*, align 8
%_37map140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41403, i64 5)
store %struct.ScmObj* %_37map140124, %struct.ScmObj** %stackaddr$env-ref51916
%stackaddr$env-ref51917 = alloca %struct.ScmObj*, align 8
%f40138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41403, i64 6)
store %struct.ScmObj* %f40138, %struct.ScmObj** %stackaddr$env-ref51917
%stackaddr$prim51918 = alloca %struct.ScmObj*, align 8
%_95k40567 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50107)
store volatile %struct.ScmObj* %_95k40567, %struct.ScmObj** %stackaddr$prim51918, align 8
%stackaddr$prim51919 = alloca %struct.ScmObj*, align 8
%current_45args50108 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50107)
store volatile %struct.ScmObj* %current_45args50108, %struct.ScmObj** %stackaddr$prim51919, align 8
%stackaddr$prim51920 = alloca %struct.ScmObj*, align 8
%lsts_4340143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50108)
store volatile %struct.ScmObj* %lsts_4340143, %struct.ScmObj** %stackaddr$prim51920, align 8
%stackaddr$makeclosure51921 = alloca %struct.ScmObj*, align 8
%fptrToInt51922 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41406 to i64
%ae41406 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt51922)
store volatile %struct.ScmObj* %ae41406, %struct.ScmObj** %stackaddr$makeclosure51921, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41406, %struct.ScmObj* %acc40137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41406, %struct.ScmObj* %lsts40136, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41406, %struct.ScmObj* %_37foldr40134, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41406, %struct.ScmObj* %k40563, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41406, %struct.ScmObj* %_37foldr140128, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41406, %struct.ScmObj* %lsts_4340143, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41406, %struct.ScmObj* %_37map140124, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41406, %struct.ScmObj* %f40138, i64 7)
%ae41407 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51923 = alloca %struct.ScmObj*, align 8
%fptrToInt51924 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41408 to i64
%ae41408 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt51924)
store volatile %struct.ScmObj* %ae41408, %struct.ScmObj** %stackaddr$makeclosure51923, align 8
%args50137$ae41406$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51925 = alloca %struct.ScmObj*, align 8
%args50137$ae41406$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41408, %struct.ScmObj* %args50137$ae41406$0)
store volatile %struct.ScmObj* %args50137$ae41406$1, %struct.ScmObj** %stackaddr$prim51925, align 8
%stackaddr$prim51926 = alloca %struct.ScmObj*, align 8
%args50137$ae41406$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41407, %struct.ScmObj* %args50137$ae41406$1)
store volatile %struct.ScmObj* %args50137$ae41406$2, %struct.ScmObj** %stackaddr$prim51926, align 8
%clofunc51927 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41406)
musttail call tailcc void %clofunc51927(%struct.ScmObj* %ae41406, %struct.ScmObj* %args50137$ae41406$2)
ret void
}

define tailcc void @proc_clo$ae41406(%struct.ScmObj* %env$ae41406,%struct.ScmObj* %current_45args50110) {
%stackaddr$env-ref51928 = alloca %struct.ScmObj*, align 8
%acc40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41406, i64 0)
store %struct.ScmObj* %acc40137, %struct.ScmObj** %stackaddr$env-ref51928
%stackaddr$env-ref51929 = alloca %struct.ScmObj*, align 8
%lsts40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41406, i64 1)
store %struct.ScmObj* %lsts40136, %struct.ScmObj** %stackaddr$env-ref51929
%stackaddr$env-ref51930 = alloca %struct.ScmObj*, align 8
%_37foldr40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41406, i64 2)
store %struct.ScmObj* %_37foldr40134, %struct.ScmObj** %stackaddr$env-ref51930
%stackaddr$env-ref51931 = alloca %struct.ScmObj*, align 8
%k40563 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41406, i64 3)
store %struct.ScmObj* %k40563, %struct.ScmObj** %stackaddr$env-ref51931
%stackaddr$env-ref51932 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41406, i64 4)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref51932
%stackaddr$env-ref51933 = alloca %struct.ScmObj*, align 8
%lsts_4340143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41406, i64 5)
store %struct.ScmObj* %lsts_4340143, %struct.ScmObj** %stackaddr$env-ref51933
%stackaddr$env-ref51934 = alloca %struct.ScmObj*, align 8
%_37map140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41406, i64 6)
store %struct.ScmObj* %_37map140124, %struct.ScmObj** %stackaddr$env-ref51934
%stackaddr$env-ref51935 = alloca %struct.ScmObj*, align 8
%f40138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41406, i64 7)
store %struct.ScmObj* %f40138, %struct.ScmObj** %stackaddr$env-ref51935
%stackaddr$prim51936 = alloca %struct.ScmObj*, align 8
%_95k40568 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50110)
store volatile %struct.ScmObj* %_95k40568, %struct.ScmObj** %stackaddr$prim51936, align 8
%stackaddr$prim51937 = alloca %struct.ScmObj*, align 8
%current_45args50111 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50110)
store volatile %struct.ScmObj* %current_45args50111, %struct.ScmObj** %stackaddr$prim51937, align 8
%stackaddr$prim51938 = alloca %struct.ScmObj*, align 8
%anf_45bind40291 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50111)
store volatile %struct.ScmObj* %anf_45bind40291, %struct.ScmObj** %stackaddr$prim51938, align 8
%stackaddr$makeclosure51939 = alloca %struct.ScmObj*, align 8
%fptrToInt51940 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41427 to i64
%ae41427 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt51940)
store volatile %struct.ScmObj* %ae41427, %struct.ScmObj** %stackaddr$makeclosure51939, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41427, %struct.ScmObj* %acc40137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41427, %struct.ScmObj* %_37foldr40134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41427, %struct.ScmObj* %k40563, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41427, %struct.ScmObj* %_37foldr140128, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41427, %struct.ScmObj* %lsts_4340143, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41427, %struct.ScmObj* %f40138, i64 5)
%args50132$_37map140124$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51941 = alloca %struct.ScmObj*, align 8
%args50132$_37map140124$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40136, %struct.ScmObj* %args50132$_37map140124$0)
store volatile %struct.ScmObj* %args50132$_37map140124$1, %struct.ScmObj** %stackaddr$prim51941, align 8
%stackaddr$prim51942 = alloca %struct.ScmObj*, align 8
%args50132$_37map140124$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40291, %struct.ScmObj* %args50132$_37map140124$1)
store volatile %struct.ScmObj* %args50132$_37map140124$2, %struct.ScmObj** %stackaddr$prim51942, align 8
%stackaddr$prim51943 = alloca %struct.ScmObj*, align 8
%args50132$_37map140124$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41427, %struct.ScmObj* %args50132$_37map140124$2)
store volatile %struct.ScmObj* %args50132$_37map140124$3, %struct.ScmObj** %stackaddr$prim51943, align 8
%clofunc51944 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140124)
musttail call tailcc void %clofunc51944(%struct.ScmObj* %_37map140124, %struct.ScmObj* %args50132$_37map140124$3)
ret void
}

define tailcc void @proc_clo$ae41427(%struct.ScmObj* %env$ae41427,%struct.ScmObj* %current_45args50113) {
%stackaddr$env-ref51945 = alloca %struct.ScmObj*, align 8
%acc40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41427, i64 0)
store %struct.ScmObj* %acc40137, %struct.ScmObj** %stackaddr$env-ref51945
%stackaddr$env-ref51946 = alloca %struct.ScmObj*, align 8
%_37foldr40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41427, i64 1)
store %struct.ScmObj* %_37foldr40134, %struct.ScmObj** %stackaddr$env-ref51946
%stackaddr$env-ref51947 = alloca %struct.ScmObj*, align 8
%k40563 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41427, i64 2)
store %struct.ScmObj* %k40563, %struct.ScmObj** %stackaddr$env-ref51947
%stackaddr$env-ref51948 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41427, i64 3)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref51948
%stackaddr$env-ref51949 = alloca %struct.ScmObj*, align 8
%lsts_4340143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41427, i64 4)
store %struct.ScmObj* %lsts_4340143, %struct.ScmObj** %stackaddr$env-ref51949
%stackaddr$env-ref51950 = alloca %struct.ScmObj*, align 8
%f40138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41427, i64 5)
store %struct.ScmObj* %f40138, %struct.ScmObj** %stackaddr$env-ref51950
%stackaddr$prim51951 = alloca %struct.ScmObj*, align 8
%_95k40569 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50113)
store volatile %struct.ScmObj* %_95k40569, %struct.ScmObj** %stackaddr$prim51951, align 8
%stackaddr$prim51952 = alloca %struct.ScmObj*, align 8
%current_45args50114 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50113)
store volatile %struct.ScmObj* %current_45args50114, %struct.ScmObj** %stackaddr$prim51952, align 8
%stackaddr$prim51953 = alloca %struct.ScmObj*, align 8
%vs40141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50114)
store volatile %struct.ScmObj* %vs40141, %struct.ScmObj** %stackaddr$prim51953, align 8
%stackaddr$makeclosure51954 = alloca %struct.ScmObj*, align 8
%fptrToInt51955 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41430 to i64
%ae41430 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt51955)
store volatile %struct.ScmObj* %ae41430, %struct.ScmObj** %stackaddr$makeclosure51954, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41430, %struct.ScmObj* %acc40137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41430, %struct.ScmObj* %_37foldr40134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41430, %struct.ScmObj* %k40563, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41430, %struct.ScmObj* %_37foldr140128, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41430, %struct.ScmObj* %lsts_4340143, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41430, %struct.ScmObj* %vs40141, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41430, %struct.ScmObj* %f40138, i64 6)
%ae41431 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51956 = alloca %struct.ScmObj*, align 8
%fptrToInt51957 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41432 to i64
%ae41432 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt51957)
store volatile %struct.ScmObj* %ae41432, %struct.ScmObj** %stackaddr$makeclosure51956, align 8
%args50131$ae41430$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51958 = alloca %struct.ScmObj*, align 8
%args50131$ae41430$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41432, %struct.ScmObj* %args50131$ae41430$0)
store volatile %struct.ScmObj* %args50131$ae41430$1, %struct.ScmObj** %stackaddr$prim51958, align 8
%stackaddr$prim51959 = alloca %struct.ScmObj*, align 8
%args50131$ae41430$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41431, %struct.ScmObj* %args50131$ae41430$1)
store volatile %struct.ScmObj* %args50131$ae41430$2, %struct.ScmObj** %stackaddr$prim51959, align 8
%clofunc51960 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41430)
musttail call tailcc void %clofunc51960(%struct.ScmObj* %ae41430, %struct.ScmObj* %args50131$ae41430$2)
ret void
}

define tailcc void @proc_clo$ae41430(%struct.ScmObj* %env$ae41430,%struct.ScmObj* %current_45args50116) {
%stackaddr$env-ref51961 = alloca %struct.ScmObj*, align 8
%acc40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41430, i64 0)
store %struct.ScmObj* %acc40137, %struct.ScmObj** %stackaddr$env-ref51961
%stackaddr$env-ref51962 = alloca %struct.ScmObj*, align 8
%_37foldr40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41430, i64 1)
store %struct.ScmObj* %_37foldr40134, %struct.ScmObj** %stackaddr$env-ref51962
%stackaddr$env-ref51963 = alloca %struct.ScmObj*, align 8
%k40563 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41430, i64 2)
store %struct.ScmObj* %k40563, %struct.ScmObj** %stackaddr$env-ref51963
%stackaddr$env-ref51964 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41430, i64 3)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref51964
%stackaddr$env-ref51965 = alloca %struct.ScmObj*, align 8
%lsts_4340143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41430, i64 4)
store %struct.ScmObj* %lsts_4340143, %struct.ScmObj** %stackaddr$env-ref51965
%stackaddr$env-ref51966 = alloca %struct.ScmObj*, align 8
%vs40141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41430, i64 5)
store %struct.ScmObj* %vs40141, %struct.ScmObj** %stackaddr$env-ref51966
%stackaddr$env-ref51967 = alloca %struct.ScmObj*, align 8
%f40138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41430, i64 6)
store %struct.ScmObj* %f40138, %struct.ScmObj** %stackaddr$env-ref51967
%stackaddr$prim51968 = alloca %struct.ScmObj*, align 8
%_95k40570 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50116)
store volatile %struct.ScmObj* %_95k40570, %struct.ScmObj** %stackaddr$prim51968, align 8
%stackaddr$prim51969 = alloca %struct.ScmObj*, align 8
%current_45args50117 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50116)
store volatile %struct.ScmObj* %current_45args50117, %struct.ScmObj** %stackaddr$prim51969, align 8
%stackaddr$prim51970 = alloca %struct.ScmObj*, align 8
%anf_45bind40292 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50117)
store volatile %struct.ScmObj* %anf_45bind40292, %struct.ScmObj** %stackaddr$prim51970, align 8
%stackaddr$prim51971 = alloca %struct.ScmObj*, align 8
%anf_45bind40293 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40137, %struct.ScmObj* %lsts_4340143)
store volatile %struct.ScmObj* %anf_45bind40293, %struct.ScmObj** %stackaddr$prim51971, align 8
%stackaddr$prim51972 = alloca %struct.ScmObj*, align 8
%anf_45bind40294 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40138, %struct.ScmObj* %anf_45bind40293)
store volatile %struct.ScmObj* %anf_45bind40294, %struct.ScmObj** %stackaddr$prim51972, align 8
%stackaddr$makeclosure51973 = alloca %struct.ScmObj*, align 8
%fptrToInt51974 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41456 to i64
%ae41456 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt51974)
store volatile %struct.ScmObj* %ae41456, %struct.ScmObj** %stackaddr$makeclosure51973, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41456, %struct.ScmObj* %anf_45bind40292, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41456, %struct.ScmObj* %k40563, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41456, %struct.ScmObj* %_37foldr140128, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41456, %struct.ScmObj* %vs40141, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41456, %struct.ScmObj* %f40138, i64 4)
%stackaddr$prim51975 = alloca %struct.ScmObj*, align 8
%cpsargs40574 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41456, %struct.ScmObj* %anf_45bind40294)
store volatile %struct.ScmObj* %cpsargs40574, %struct.ScmObj** %stackaddr$prim51975, align 8
%clofunc51976 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40134)
musttail call tailcc void %clofunc51976(%struct.ScmObj* %_37foldr40134, %struct.ScmObj* %cpsargs40574)
ret void
}

define tailcc void @proc_clo$ae41456(%struct.ScmObj* %env$ae41456,%struct.ScmObj* %current_45args50119) {
%stackaddr$env-ref51977 = alloca %struct.ScmObj*, align 8
%anf_45bind40292 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41456, i64 0)
store %struct.ScmObj* %anf_45bind40292, %struct.ScmObj** %stackaddr$env-ref51977
%stackaddr$env-ref51978 = alloca %struct.ScmObj*, align 8
%k40563 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41456, i64 1)
store %struct.ScmObj* %k40563, %struct.ScmObj** %stackaddr$env-ref51978
%stackaddr$env-ref51979 = alloca %struct.ScmObj*, align 8
%_37foldr140128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41456, i64 2)
store %struct.ScmObj* %_37foldr140128, %struct.ScmObj** %stackaddr$env-ref51979
%stackaddr$env-ref51980 = alloca %struct.ScmObj*, align 8
%vs40141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41456, i64 3)
store %struct.ScmObj* %vs40141, %struct.ScmObj** %stackaddr$env-ref51980
%stackaddr$env-ref51981 = alloca %struct.ScmObj*, align 8
%f40138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41456, i64 4)
store %struct.ScmObj* %f40138, %struct.ScmObj** %stackaddr$env-ref51981
%stackaddr$prim51982 = alloca %struct.ScmObj*, align 8
%_95k40571 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50119)
store volatile %struct.ScmObj* %_95k40571, %struct.ScmObj** %stackaddr$prim51982, align 8
%stackaddr$prim51983 = alloca %struct.ScmObj*, align 8
%current_45args50120 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50119)
store volatile %struct.ScmObj* %current_45args50120, %struct.ScmObj** %stackaddr$prim51983, align 8
%stackaddr$prim51984 = alloca %struct.ScmObj*, align 8
%anf_45bind40295 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50120)
store volatile %struct.ScmObj* %anf_45bind40295, %struct.ScmObj** %stackaddr$prim51984, align 8
%ae41461 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51985 = alloca %struct.ScmObj*, align 8
%anf_45bind40296 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40295, %struct.ScmObj* %ae41461)
store volatile %struct.ScmObj* %anf_45bind40296, %struct.ScmObj** %stackaddr$prim51985, align 8
%stackaddr$makeclosure51986 = alloca %struct.ScmObj*, align 8
%fptrToInt51987 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41463 to i64
%ae41463 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt51987)
store volatile %struct.ScmObj* %ae41463, %struct.ScmObj** %stackaddr$makeclosure51986, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41463, %struct.ScmObj* %k40563, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41463, %struct.ScmObj* %f40138, i64 1)
%args50125$_37foldr140128$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51988 = alloca %struct.ScmObj*, align 8
%args50125$_37foldr140128$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40141, %struct.ScmObj* %args50125$_37foldr140128$0)
store volatile %struct.ScmObj* %args50125$_37foldr140128$1, %struct.ScmObj** %stackaddr$prim51988, align 8
%stackaddr$prim51989 = alloca %struct.ScmObj*, align 8
%args50125$_37foldr140128$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40296, %struct.ScmObj* %args50125$_37foldr140128$1)
store volatile %struct.ScmObj* %args50125$_37foldr140128$2, %struct.ScmObj** %stackaddr$prim51989, align 8
%stackaddr$prim51990 = alloca %struct.ScmObj*, align 8
%args50125$_37foldr140128$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40292, %struct.ScmObj* %args50125$_37foldr140128$2)
store volatile %struct.ScmObj* %args50125$_37foldr140128$3, %struct.ScmObj** %stackaddr$prim51990, align 8
%stackaddr$prim51991 = alloca %struct.ScmObj*, align 8
%args50125$_37foldr140128$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41463, %struct.ScmObj* %args50125$_37foldr140128$3)
store volatile %struct.ScmObj* %args50125$_37foldr140128$4, %struct.ScmObj** %stackaddr$prim51991, align 8
%clofunc51992 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140128)
musttail call tailcc void %clofunc51992(%struct.ScmObj* %_37foldr140128, %struct.ScmObj* %args50125$_37foldr140128$4)
ret void
}

define tailcc void @proc_clo$ae41463(%struct.ScmObj* %env$ae41463,%struct.ScmObj* %current_45args50122) {
%stackaddr$env-ref51993 = alloca %struct.ScmObj*, align 8
%k40563 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41463, i64 0)
store %struct.ScmObj* %k40563, %struct.ScmObj** %stackaddr$env-ref51993
%stackaddr$env-ref51994 = alloca %struct.ScmObj*, align 8
%f40138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41463, i64 1)
store %struct.ScmObj* %f40138, %struct.ScmObj** %stackaddr$env-ref51994
%stackaddr$prim51995 = alloca %struct.ScmObj*, align 8
%_95k40572 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50122)
store volatile %struct.ScmObj* %_95k40572, %struct.ScmObj** %stackaddr$prim51995, align 8
%stackaddr$prim51996 = alloca %struct.ScmObj*, align 8
%current_45args50123 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50122)
store volatile %struct.ScmObj* %current_45args50123, %struct.ScmObj** %stackaddr$prim51996, align 8
%stackaddr$prim51997 = alloca %struct.ScmObj*, align 8
%anf_45bind40297 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50123)
store volatile %struct.ScmObj* %anf_45bind40297, %struct.ScmObj** %stackaddr$prim51997, align 8
%stackaddr$prim51998 = alloca %struct.ScmObj*, align 8
%cpsargs40573 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40563, %struct.ScmObj* %anf_45bind40297)
store volatile %struct.ScmObj* %cpsargs40573, %struct.ScmObj** %stackaddr$prim51998, align 8
%clofunc51999 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40138)
musttail call tailcc void %clofunc51999(%struct.ScmObj* %f40138, %struct.ScmObj* %cpsargs40573)
ret void
}

define tailcc void @proc_clo$ae41432(%struct.ScmObj* %env$ae41432,%struct.ScmObj* %current_45args50126) {
%stackaddr$prim52000 = alloca %struct.ScmObj*, align 8
%k40575 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50126)
store volatile %struct.ScmObj* %k40575, %struct.ScmObj** %stackaddr$prim52000, align 8
%stackaddr$prim52001 = alloca %struct.ScmObj*, align 8
%current_45args50127 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50126)
store volatile %struct.ScmObj* %current_45args50127, %struct.ScmObj** %stackaddr$prim52001, align 8
%stackaddr$prim52002 = alloca %struct.ScmObj*, align 8
%a40146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50127)
store volatile %struct.ScmObj* %a40146, %struct.ScmObj** %stackaddr$prim52002, align 8
%stackaddr$prim52003 = alloca %struct.ScmObj*, align 8
%current_45args50128 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50127)
store volatile %struct.ScmObj* %current_45args50128, %struct.ScmObj** %stackaddr$prim52003, align 8
%stackaddr$prim52004 = alloca %struct.ScmObj*, align 8
%b40145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50128)
store volatile %struct.ScmObj* %b40145, %struct.ScmObj** %stackaddr$prim52004, align 8
%stackaddr$prim52005 = alloca %struct.ScmObj*, align 8
%cpsprim40576 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40146, %struct.ScmObj* %b40145)
store volatile %struct.ScmObj* %cpsprim40576, %struct.ScmObj** %stackaddr$prim52005, align 8
%ae41436 = call %struct.ScmObj* @const_init_int(i64 0)
%args50130$k40575$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52006 = alloca %struct.ScmObj*, align 8
%args50130$k40575$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40576, %struct.ScmObj* %args50130$k40575$0)
store volatile %struct.ScmObj* %args50130$k40575$1, %struct.ScmObj** %stackaddr$prim52006, align 8
%stackaddr$prim52007 = alloca %struct.ScmObj*, align 8
%args50130$k40575$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41436, %struct.ScmObj* %args50130$k40575$1)
store volatile %struct.ScmObj* %args50130$k40575$2, %struct.ScmObj** %stackaddr$prim52007, align 8
%clofunc52008 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40575)
musttail call tailcc void %clofunc52008(%struct.ScmObj* %k40575, %struct.ScmObj* %args50130$k40575$2)
ret void
}

define tailcc void @proc_clo$ae41408(%struct.ScmObj* %env$ae41408,%struct.ScmObj* %current_45args50133) {
%stackaddr$prim52009 = alloca %struct.ScmObj*, align 8
%k40577 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50133)
store volatile %struct.ScmObj* %k40577, %struct.ScmObj** %stackaddr$prim52009, align 8
%stackaddr$prim52010 = alloca %struct.ScmObj*, align 8
%current_45args50134 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50133)
store volatile %struct.ScmObj* %current_45args50134, %struct.ScmObj** %stackaddr$prim52010, align 8
%stackaddr$prim52011 = alloca %struct.ScmObj*, align 8
%x40142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50134)
store volatile %struct.ScmObj* %x40142, %struct.ScmObj** %stackaddr$prim52011, align 8
%stackaddr$prim52012 = alloca %struct.ScmObj*, align 8
%cpsprim40578 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40142)
store volatile %struct.ScmObj* %cpsprim40578, %struct.ScmObj** %stackaddr$prim52012, align 8
%ae41411 = call %struct.ScmObj* @const_init_int(i64 0)
%args50136$k40577$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52013 = alloca %struct.ScmObj*, align 8
%args50136$k40577$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40578, %struct.ScmObj* %args50136$k40577$0)
store volatile %struct.ScmObj* %args50136$k40577$1, %struct.ScmObj** %stackaddr$prim52013, align 8
%stackaddr$prim52014 = alloca %struct.ScmObj*, align 8
%args50136$k40577$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41411, %struct.ScmObj* %args50136$k40577$1)
store volatile %struct.ScmObj* %args50136$k40577$2, %struct.ScmObj** %stackaddr$prim52014, align 8
%clofunc52015 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40577)
musttail call tailcc void %clofunc52015(%struct.ScmObj* %k40577, %struct.ScmObj* %args50136$k40577$2)
ret void
}

define tailcc void @proc_clo$ae41384(%struct.ScmObj* %env$ae41384,%struct.ScmObj* %current_45args50139) {
%stackaddr$prim52016 = alloca %struct.ScmObj*, align 8
%k40579 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50139)
store volatile %struct.ScmObj* %k40579, %struct.ScmObj** %stackaddr$prim52016, align 8
%stackaddr$prim52017 = alloca %struct.ScmObj*, align 8
%current_45args50140 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50139)
store volatile %struct.ScmObj* %current_45args50140, %struct.ScmObj** %stackaddr$prim52017, align 8
%stackaddr$prim52018 = alloca %struct.ScmObj*, align 8
%x40144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50140)
store volatile %struct.ScmObj* %x40144, %struct.ScmObj** %stackaddr$prim52018, align 8
%stackaddr$prim52019 = alloca %struct.ScmObj*, align 8
%cpsprim40580 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40144)
store volatile %struct.ScmObj* %cpsprim40580, %struct.ScmObj** %stackaddr$prim52019, align 8
%ae41387 = call %struct.ScmObj* @const_init_int(i64 0)
%args50142$k40579$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52020 = alloca %struct.ScmObj*, align 8
%args50142$k40579$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40580, %struct.ScmObj* %args50142$k40579$0)
store volatile %struct.ScmObj* %args50142$k40579$1, %struct.ScmObj** %stackaddr$prim52020, align 8
%stackaddr$prim52021 = alloca %struct.ScmObj*, align 8
%args50142$k40579$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41387, %struct.ScmObj* %args50142$k40579$1)
store volatile %struct.ScmObj* %args50142$k40579$2, %struct.ScmObj** %stackaddr$prim52021, align 8
%clofunc52022 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40579)
musttail call tailcc void %clofunc52022(%struct.ScmObj* %k40579, %struct.ScmObj* %args50142$k40579$2)
ret void
}

define tailcc void @proc_clo$ae41336(%struct.ScmObj* %env$ae41336,%struct.ScmObj* %current_45args50145) {
%stackaddr$prim52023 = alloca %struct.ScmObj*, align 8
%k40581 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50145)
store volatile %struct.ScmObj* %k40581, %struct.ScmObj** %stackaddr$prim52023, align 8
%stackaddr$prim52024 = alloca %struct.ScmObj*, align 8
%current_45args50146 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50145)
store volatile %struct.ScmObj* %current_45args50146, %struct.ScmObj** %stackaddr$prim52024, align 8
%stackaddr$prim52025 = alloca %struct.ScmObj*, align 8
%lst40140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50146)
store volatile %struct.ScmObj* %lst40140, %struct.ScmObj** %stackaddr$prim52025, align 8
%stackaddr$prim52026 = alloca %struct.ScmObj*, align 8
%current_45args50147 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50146)
store volatile %struct.ScmObj* %current_45args50147, %struct.ScmObj** %stackaddr$prim52026, align 8
%stackaddr$prim52027 = alloca %struct.ScmObj*, align 8
%b40139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50147)
store volatile %struct.ScmObj* %b40139, %struct.ScmObj** %stackaddr$prim52027, align 8
%truthy$cmp52028 = call i64 @is_truthy_value(%struct.ScmObj* %b40139)
%cmp$cmp52028 = icmp eq i64 %truthy$cmp52028, 1
br i1 %cmp$cmp52028, label %truebranch$cmp52028, label %falsebranch$cmp52028
truebranch$cmp52028:
%ae41339 = call %struct.ScmObj* @const_init_int(i64 0)
%args50149$k40581$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52029 = alloca %struct.ScmObj*, align 8
%args50149$k40581$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40139, %struct.ScmObj* %args50149$k40581$0)
store volatile %struct.ScmObj* %args50149$k40581$1, %struct.ScmObj** %stackaddr$prim52029, align 8
%stackaddr$prim52030 = alloca %struct.ScmObj*, align 8
%args50149$k40581$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41339, %struct.ScmObj* %args50149$k40581$1)
store volatile %struct.ScmObj* %args50149$k40581$2, %struct.ScmObj** %stackaddr$prim52030, align 8
%clofunc52031 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40581)
musttail call tailcc void %clofunc52031(%struct.ScmObj* %k40581, %struct.ScmObj* %args50149$k40581$2)
ret void
falsebranch$cmp52028:
%stackaddr$prim52032 = alloca %struct.ScmObj*, align 8
%cpsprim40582 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40140)
store volatile %struct.ScmObj* %cpsprim40582, %struct.ScmObj** %stackaddr$prim52032, align 8
%ae41346 = call %struct.ScmObj* @const_init_int(i64 0)
%args50150$k40581$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52033 = alloca %struct.ScmObj*, align 8
%args50150$k40581$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40582, %struct.ScmObj* %args50150$k40581$0)
store volatile %struct.ScmObj* %args50150$k40581$1, %struct.ScmObj** %stackaddr$prim52033, align 8
%stackaddr$prim52034 = alloca %struct.ScmObj*, align 8
%args50150$k40581$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41346, %struct.ScmObj* %args50150$k40581$1)
store volatile %struct.ScmObj* %args50150$k40581$2, %struct.ScmObj** %stackaddr$prim52034, align 8
%clofunc52035 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40581)
musttail call tailcc void %clofunc52035(%struct.ScmObj* %k40581, %struct.ScmObj* %args50150$k40581$2)
ret void
}

define tailcc void @proc_clo$ae41293(%struct.ScmObj* %env$ae41293,%struct.ScmObj* %current_45args50154) {
%stackaddr$env-ref52036 = alloca %struct.ScmObj*, align 8
%_37take40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41293, i64 0)
store %struct.ScmObj* %_37take40120, %struct.ScmObj** %stackaddr$env-ref52036
%stackaddr$env-ref52037 = alloca %struct.ScmObj*, align 8
%_37length40117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41293, i64 1)
store %struct.ScmObj* %_37length40117, %struct.ScmObj** %stackaddr$env-ref52037
%stackaddr$prim52038 = alloca %struct.ScmObj*, align 8
%k40583 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50154)
store volatile %struct.ScmObj* %k40583, %struct.ScmObj** %stackaddr$prim52038, align 8
%stackaddr$prim52039 = alloca %struct.ScmObj*, align 8
%current_45args50155 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50154)
store volatile %struct.ScmObj* %current_45args50155, %struct.ScmObj** %stackaddr$prim52039, align 8
%stackaddr$prim52040 = alloca %struct.ScmObj*, align 8
%lst40149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50155)
store volatile %struct.ScmObj* %lst40149, %struct.ScmObj** %stackaddr$prim52040, align 8
%stackaddr$prim52041 = alloca %struct.ScmObj*, align 8
%current_45args50156 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50155)
store volatile %struct.ScmObj* %current_45args50156, %struct.ScmObj** %stackaddr$prim52041, align 8
%stackaddr$prim52042 = alloca %struct.ScmObj*, align 8
%n40148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50156)
store volatile %struct.ScmObj* %n40148, %struct.ScmObj** %stackaddr$prim52042, align 8
%stackaddr$makeclosure52043 = alloca %struct.ScmObj*, align 8
%fptrToInt52044 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41295 to i64
%ae41295 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt52044)
store volatile %struct.ScmObj* %ae41295, %struct.ScmObj** %stackaddr$makeclosure52043, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41295, %struct.ScmObj* %_37take40120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41295, %struct.ScmObj* %k40583, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41295, %struct.ScmObj* %lst40149, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41295, %struct.ScmObj* %n40148, i64 3)
%args50162$_37length40117$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52045 = alloca %struct.ScmObj*, align 8
%args50162$_37length40117$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40149, %struct.ScmObj* %args50162$_37length40117$0)
store volatile %struct.ScmObj* %args50162$_37length40117$1, %struct.ScmObj** %stackaddr$prim52045, align 8
%stackaddr$prim52046 = alloca %struct.ScmObj*, align 8
%args50162$_37length40117$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41295, %struct.ScmObj* %args50162$_37length40117$1)
store volatile %struct.ScmObj* %args50162$_37length40117$2, %struct.ScmObj** %stackaddr$prim52046, align 8
%clofunc52047 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40117)
musttail call tailcc void %clofunc52047(%struct.ScmObj* %_37length40117, %struct.ScmObj* %args50162$_37length40117$2)
ret void
}

define tailcc void @proc_clo$ae41295(%struct.ScmObj* %env$ae41295,%struct.ScmObj* %current_45args50158) {
%stackaddr$env-ref52048 = alloca %struct.ScmObj*, align 8
%_37take40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41295, i64 0)
store %struct.ScmObj* %_37take40120, %struct.ScmObj** %stackaddr$env-ref52048
%stackaddr$env-ref52049 = alloca %struct.ScmObj*, align 8
%k40583 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41295, i64 1)
store %struct.ScmObj* %k40583, %struct.ScmObj** %stackaddr$env-ref52049
%stackaddr$env-ref52050 = alloca %struct.ScmObj*, align 8
%lst40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41295, i64 2)
store %struct.ScmObj* %lst40149, %struct.ScmObj** %stackaddr$env-ref52050
%stackaddr$env-ref52051 = alloca %struct.ScmObj*, align 8
%n40148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41295, i64 3)
store %struct.ScmObj* %n40148, %struct.ScmObj** %stackaddr$env-ref52051
%stackaddr$prim52052 = alloca %struct.ScmObj*, align 8
%_95k40584 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50158)
store volatile %struct.ScmObj* %_95k40584, %struct.ScmObj** %stackaddr$prim52052, align 8
%stackaddr$prim52053 = alloca %struct.ScmObj*, align 8
%current_45args50159 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50158)
store volatile %struct.ScmObj* %current_45args50159, %struct.ScmObj** %stackaddr$prim52053, align 8
%stackaddr$prim52054 = alloca %struct.ScmObj*, align 8
%anf_45bind40284 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50159)
store volatile %struct.ScmObj* %anf_45bind40284, %struct.ScmObj** %stackaddr$prim52054, align 8
%stackaddr$prim52055 = alloca %struct.ScmObj*, align 8
%anf_45bind40285 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40284, %struct.ScmObj* %n40148)
store volatile %struct.ScmObj* %anf_45bind40285, %struct.ScmObj** %stackaddr$prim52055, align 8
%args50161$_37take40120$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52056 = alloca %struct.ScmObj*, align 8
%args50161$_37take40120$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40285, %struct.ScmObj* %args50161$_37take40120$0)
store volatile %struct.ScmObj* %args50161$_37take40120$1, %struct.ScmObj** %stackaddr$prim52056, align 8
%stackaddr$prim52057 = alloca %struct.ScmObj*, align 8
%args50161$_37take40120$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40149, %struct.ScmObj* %args50161$_37take40120$1)
store volatile %struct.ScmObj* %args50161$_37take40120$2, %struct.ScmObj** %stackaddr$prim52057, align 8
%stackaddr$prim52058 = alloca %struct.ScmObj*, align 8
%args50161$_37take40120$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40583, %struct.ScmObj* %args50161$_37take40120$2)
store volatile %struct.ScmObj* %args50161$_37take40120$3, %struct.ScmObj** %stackaddr$prim52058, align 8
%clofunc52059 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40120)
musttail call tailcc void %clofunc52059(%struct.ScmObj* %_37take40120, %struct.ScmObj* %args50161$_37take40120$3)
ret void
}

define tailcc void @proc_clo$ae41239(%struct.ScmObj* %env$ae41239,%struct.ScmObj* %current_45args50164) {
%stackaddr$env-ref52060 = alloca %struct.ScmObj*, align 8
%_37foldl140112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41239, i64 0)
store %struct.ScmObj* %_37foldl140112, %struct.ScmObj** %stackaddr$env-ref52060
%stackaddr$prim52061 = alloca %struct.ScmObj*, align 8
%k40585 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50164)
store volatile %struct.ScmObj* %k40585, %struct.ScmObj** %stackaddr$prim52061, align 8
%stackaddr$prim52062 = alloca %struct.ScmObj*, align 8
%current_45args50165 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50164)
store volatile %struct.ScmObj* %current_45args50165, %struct.ScmObj** %stackaddr$prim52062, align 8
%stackaddr$prim52063 = alloca %struct.ScmObj*, align 8
%lst40151 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50165)
store volatile %struct.ScmObj* %lst40151, %struct.ScmObj** %stackaddr$prim52063, align 8
%stackaddr$makeclosure52064 = alloca %struct.ScmObj*, align 8
%fptrToInt52065 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41240 to i64
%ae41240 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt52065)
store volatile %struct.ScmObj* %ae41240, %struct.ScmObj** %stackaddr$makeclosure52064, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41240, %struct.ScmObj* %k40585, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41240, %struct.ScmObj* %lst40151, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41240, %struct.ScmObj* %_37foldl140112, i64 2)
%ae41241 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure52066 = alloca %struct.ScmObj*, align 8
%fptrToInt52067 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41242 to i64
%ae41242 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt52067)
store volatile %struct.ScmObj* %ae41242, %struct.ScmObj** %stackaddr$makeclosure52066, align 8
%args50176$ae41240$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52068 = alloca %struct.ScmObj*, align 8
%args50176$ae41240$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41242, %struct.ScmObj* %args50176$ae41240$0)
store volatile %struct.ScmObj* %args50176$ae41240$1, %struct.ScmObj** %stackaddr$prim52068, align 8
%stackaddr$prim52069 = alloca %struct.ScmObj*, align 8
%args50176$ae41240$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41241, %struct.ScmObj* %args50176$ae41240$1)
store volatile %struct.ScmObj* %args50176$ae41240$2, %struct.ScmObj** %stackaddr$prim52069, align 8
%clofunc52070 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41240)
musttail call tailcc void %clofunc52070(%struct.ScmObj* %ae41240, %struct.ScmObj* %args50176$ae41240$2)
ret void
}

define tailcc void @proc_clo$ae41240(%struct.ScmObj* %env$ae41240,%struct.ScmObj* %current_45args50167) {
%stackaddr$env-ref52071 = alloca %struct.ScmObj*, align 8
%k40585 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41240, i64 0)
store %struct.ScmObj* %k40585, %struct.ScmObj** %stackaddr$env-ref52071
%stackaddr$env-ref52072 = alloca %struct.ScmObj*, align 8
%lst40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41240, i64 1)
store %struct.ScmObj* %lst40151, %struct.ScmObj** %stackaddr$env-ref52072
%stackaddr$env-ref52073 = alloca %struct.ScmObj*, align 8
%_37foldl140112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41240, i64 2)
store %struct.ScmObj* %_37foldl140112, %struct.ScmObj** %stackaddr$env-ref52073
%stackaddr$prim52074 = alloca %struct.ScmObj*, align 8
%_95k40586 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50167)
store volatile %struct.ScmObj* %_95k40586, %struct.ScmObj** %stackaddr$prim52074, align 8
%stackaddr$prim52075 = alloca %struct.ScmObj*, align 8
%current_45args50168 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50167)
store volatile %struct.ScmObj* %current_45args50168, %struct.ScmObj** %stackaddr$prim52075, align 8
%stackaddr$prim52076 = alloca %struct.ScmObj*, align 8
%anf_45bind40283 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50168)
store volatile %struct.ScmObj* %anf_45bind40283, %struct.ScmObj** %stackaddr$prim52076, align 8
%ae41261 = call %struct.ScmObj* @const_init_null()
%args50170$_37foldl140112$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52077 = alloca %struct.ScmObj*, align 8
%args50170$_37foldl140112$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40151, %struct.ScmObj* %args50170$_37foldl140112$0)
store volatile %struct.ScmObj* %args50170$_37foldl140112$1, %struct.ScmObj** %stackaddr$prim52077, align 8
%stackaddr$prim52078 = alloca %struct.ScmObj*, align 8
%args50170$_37foldl140112$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41261, %struct.ScmObj* %args50170$_37foldl140112$1)
store volatile %struct.ScmObj* %args50170$_37foldl140112$2, %struct.ScmObj** %stackaddr$prim52078, align 8
%stackaddr$prim52079 = alloca %struct.ScmObj*, align 8
%args50170$_37foldl140112$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40283, %struct.ScmObj* %args50170$_37foldl140112$2)
store volatile %struct.ScmObj* %args50170$_37foldl140112$3, %struct.ScmObj** %stackaddr$prim52079, align 8
%stackaddr$prim52080 = alloca %struct.ScmObj*, align 8
%args50170$_37foldl140112$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40585, %struct.ScmObj* %args50170$_37foldl140112$3)
store volatile %struct.ScmObj* %args50170$_37foldl140112$4, %struct.ScmObj** %stackaddr$prim52080, align 8
%clofunc52081 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140112)
musttail call tailcc void %clofunc52081(%struct.ScmObj* %_37foldl140112, %struct.ScmObj* %args50170$_37foldl140112$4)
ret void
}

define tailcc void @proc_clo$ae41242(%struct.ScmObj* %env$ae41242,%struct.ScmObj* %current_45args50171) {
%stackaddr$prim52082 = alloca %struct.ScmObj*, align 8
%k40587 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50171)
store volatile %struct.ScmObj* %k40587, %struct.ScmObj** %stackaddr$prim52082, align 8
%stackaddr$prim52083 = alloca %struct.ScmObj*, align 8
%current_45args50172 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50171)
store volatile %struct.ScmObj* %current_45args50172, %struct.ScmObj** %stackaddr$prim52083, align 8
%stackaddr$prim52084 = alloca %struct.ScmObj*, align 8
%x40153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50172)
store volatile %struct.ScmObj* %x40153, %struct.ScmObj** %stackaddr$prim52084, align 8
%stackaddr$prim52085 = alloca %struct.ScmObj*, align 8
%current_45args50173 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50172)
store volatile %struct.ScmObj* %current_45args50173, %struct.ScmObj** %stackaddr$prim52085, align 8
%stackaddr$prim52086 = alloca %struct.ScmObj*, align 8
%y40152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50173)
store volatile %struct.ScmObj* %y40152, %struct.ScmObj** %stackaddr$prim52086, align 8
%ae41244 = call %struct.ScmObj* @const_init_int(i64 0)
%args50175$k40587$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52087 = alloca %struct.ScmObj*, align 8
%args50175$k40587$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x40153, %struct.ScmObj* %args50175$k40587$0)
store volatile %struct.ScmObj* %args50175$k40587$1, %struct.ScmObj** %stackaddr$prim52087, align 8
%stackaddr$prim52088 = alloca %struct.ScmObj*, align 8
%args50175$k40587$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41244, %struct.ScmObj* %args50175$k40587$1)
store volatile %struct.ScmObj* %args50175$k40587$2, %struct.ScmObj** %stackaddr$prim52088, align 8
%clofunc52089 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40587)
musttail call tailcc void %clofunc52089(%struct.ScmObj* %k40587, %struct.ScmObj* %args50175$k40587$2)
ret void
}

define tailcc void @proc_clo$ae41160(%struct.ScmObj* %env$ae41160,%struct.ScmObj* %current_45args50179) {
%stackaddr$prim52090 = alloca %struct.ScmObj*, align 8
%k40588 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50179)
store volatile %struct.ScmObj* %k40588, %struct.ScmObj** %stackaddr$prim52090, align 8
%stackaddr$prim52091 = alloca %struct.ScmObj*, align 8
%current_45args50180 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50179)
store volatile %struct.ScmObj* %current_45args50180, %struct.ScmObj** %stackaddr$prim52091, align 8
%stackaddr$prim52092 = alloca %struct.ScmObj*, align 8
%_37foldl140113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50180)
store volatile %struct.ScmObj* %_37foldl140113, %struct.ScmObj** %stackaddr$prim52092, align 8
%ae41162 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure52093 = alloca %struct.ScmObj*, align 8
%fptrToInt52094 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41163 to i64
%ae41163 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt52094)
store volatile %struct.ScmObj* %ae41163, %struct.ScmObj** %stackaddr$makeclosure52093, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41163, %struct.ScmObj* %_37foldl140113, i64 0)
%args50193$k40588$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52095 = alloca %struct.ScmObj*, align 8
%args50193$k40588$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41163, %struct.ScmObj* %args50193$k40588$0)
store volatile %struct.ScmObj* %args50193$k40588$1, %struct.ScmObj** %stackaddr$prim52095, align 8
%stackaddr$prim52096 = alloca %struct.ScmObj*, align 8
%args50193$k40588$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41162, %struct.ScmObj* %args50193$k40588$1)
store volatile %struct.ScmObj* %args50193$k40588$2, %struct.ScmObj** %stackaddr$prim52096, align 8
%clofunc52097 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40588)
musttail call tailcc void %clofunc52097(%struct.ScmObj* %k40588, %struct.ScmObj* %args50193$k40588$2)
ret void
}

define tailcc void @proc_clo$ae41163(%struct.ScmObj* %env$ae41163,%struct.ScmObj* %current_45args50182) {
%stackaddr$env-ref52098 = alloca %struct.ScmObj*, align 8
%_37foldl140113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41163, i64 0)
store %struct.ScmObj* %_37foldl140113, %struct.ScmObj** %stackaddr$env-ref52098
%stackaddr$prim52099 = alloca %struct.ScmObj*, align 8
%k40589 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50182)
store volatile %struct.ScmObj* %k40589, %struct.ScmObj** %stackaddr$prim52099, align 8
%stackaddr$prim52100 = alloca %struct.ScmObj*, align 8
%current_45args50183 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50182)
store volatile %struct.ScmObj* %current_45args50183, %struct.ScmObj** %stackaddr$prim52100, align 8
%stackaddr$prim52101 = alloca %struct.ScmObj*, align 8
%f40116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50183)
store volatile %struct.ScmObj* %f40116, %struct.ScmObj** %stackaddr$prim52101, align 8
%stackaddr$prim52102 = alloca %struct.ScmObj*, align 8
%current_45args50184 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50183)
store volatile %struct.ScmObj* %current_45args50184, %struct.ScmObj** %stackaddr$prim52102, align 8
%stackaddr$prim52103 = alloca %struct.ScmObj*, align 8
%acc40115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50184)
store volatile %struct.ScmObj* %acc40115, %struct.ScmObj** %stackaddr$prim52103, align 8
%stackaddr$prim52104 = alloca %struct.ScmObj*, align 8
%current_45args50185 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50184)
store volatile %struct.ScmObj* %current_45args50185, %struct.ScmObj** %stackaddr$prim52104, align 8
%stackaddr$prim52105 = alloca %struct.ScmObj*, align 8
%lst40114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50185)
store volatile %struct.ScmObj* %lst40114, %struct.ScmObj** %stackaddr$prim52105, align 8
%stackaddr$prim52106 = alloca %struct.ScmObj*, align 8
%anf_45bind40278 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40114)
store volatile %struct.ScmObj* %anf_45bind40278, %struct.ScmObj** %stackaddr$prim52106, align 8
%truthy$cmp52107 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40278)
%cmp$cmp52107 = icmp eq i64 %truthy$cmp52107, 1
br i1 %cmp$cmp52107, label %truebranch$cmp52107, label %falsebranch$cmp52107
truebranch$cmp52107:
%ae41167 = call %struct.ScmObj* @const_init_int(i64 0)
%args50187$k40589$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52108 = alloca %struct.ScmObj*, align 8
%args50187$k40589$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40115, %struct.ScmObj* %args50187$k40589$0)
store volatile %struct.ScmObj* %args50187$k40589$1, %struct.ScmObj** %stackaddr$prim52108, align 8
%stackaddr$prim52109 = alloca %struct.ScmObj*, align 8
%args50187$k40589$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41167, %struct.ScmObj* %args50187$k40589$1)
store volatile %struct.ScmObj* %args50187$k40589$2, %struct.ScmObj** %stackaddr$prim52109, align 8
%clofunc52110 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40589)
musttail call tailcc void %clofunc52110(%struct.ScmObj* %k40589, %struct.ScmObj* %args50187$k40589$2)
ret void
falsebranch$cmp52107:
%stackaddr$prim52111 = alloca %struct.ScmObj*, align 8
%anf_45bind40279 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40114)
store volatile %struct.ScmObj* %anf_45bind40279, %struct.ScmObj** %stackaddr$prim52111, align 8
%stackaddr$makeclosure52112 = alloca %struct.ScmObj*, align 8
%fptrToInt52113 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41174 to i64
%ae41174 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt52113)
store volatile %struct.ScmObj* %ae41174, %struct.ScmObj** %stackaddr$makeclosure52112, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41174, %struct.ScmObj* %f40116, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41174, %struct.ScmObj* %lst40114, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41174, %struct.ScmObj* %_37foldl140113, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41174, %struct.ScmObj* %k40589, i64 3)
%args50192$f40116$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52114 = alloca %struct.ScmObj*, align 8
%args50192$f40116$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40115, %struct.ScmObj* %args50192$f40116$0)
store volatile %struct.ScmObj* %args50192$f40116$1, %struct.ScmObj** %stackaddr$prim52114, align 8
%stackaddr$prim52115 = alloca %struct.ScmObj*, align 8
%args50192$f40116$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40279, %struct.ScmObj* %args50192$f40116$1)
store volatile %struct.ScmObj* %args50192$f40116$2, %struct.ScmObj** %stackaddr$prim52115, align 8
%stackaddr$prim52116 = alloca %struct.ScmObj*, align 8
%args50192$f40116$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41174, %struct.ScmObj* %args50192$f40116$2)
store volatile %struct.ScmObj* %args50192$f40116$3, %struct.ScmObj** %stackaddr$prim52116, align 8
%clofunc52117 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40116)
musttail call tailcc void %clofunc52117(%struct.ScmObj* %f40116, %struct.ScmObj* %args50192$f40116$3)
ret void
}

define tailcc void @proc_clo$ae41174(%struct.ScmObj* %env$ae41174,%struct.ScmObj* %current_45args50188) {
%stackaddr$env-ref52118 = alloca %struct.ScmObj*, align 8
%f40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41174, i64 0)
store %struct.ScmObj* %f40116, %struct.ScmObj** %stackaddr$env-ref52118
%stackaddr$env-ref52119 = alloca %struct.ScmObj*, align 8
%lst40114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41174, i64 1)
store %struct.ScmObj* %lst40114, %struct.ScmObj** %stackaddr$env-ref52119
%stackaddr$env-ref52120 = alloca %struct.ScmObj*, align 8
%_37foldl140113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41174, i64 2)
store %struct.ScmObj* %_37foldl140113, %struct.ScmObj** %stackaddr$env-ref52120
%stackaddr$env-ref52121 = alloca %struct.ScmObj*, align 8
%k40589 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41174, i64 3)
store %struct.ScmObj* %k40589, %struct.ScmObj** %stackaddr$env-ref52121
%stackaddr$prim52122 = alloca %struct.ScmObj*, align 8
%_95k40590 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50188)
store volatile %struct.ScmObj* %_95k40590, %struct.ScmObj** %stackaddr$prim52122, align 8
%stackaddr$prim52123 = alloca %struct.ScmObj*, align 8
%current_45args50189 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50188)
store volatile %struct.ScmObj* %current_45args50189, %struct.ScmObj** %stackaddr$prim52123, align 8
%stackaddr$prim52124 = alloca %struct.ScmObj*, align 8
%anf_45bind40280 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50189)
store volatile %struct.ScmObj* %anf_45bind40280, %struct.ScmObj** %stackaddr$prim52124, align 8
%stackaddr$prim52125 = alloca %struct.ScmObj*, align 8
%anf_45bind40281 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40114)
store volatile %struct.ScmObj* %anf_45bind40281, %struct.ScmObj** %stackaddr$prim52125, align 8
%args50191$_37foldl140113$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52126 = alloca %struct.ScmObj*, align 8
%args50191$_37foldl140113$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40281, %struct.ScmObj* %args50191$_37foldl140113$0)
store volatile %struct.ScmObj* %args50191$_37foldl140113$1, %struct.ScmObj** %stackaddr$prim52126, align 8
%stackaddr$prim52127 = alloca %struct.ScmObj*, align 8
%args50191$_37foldl140113$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40280, %struct.ScmObj* %args50191$_37foldl140113$1)
store volatile %struct.ScmObj* %args50191$_37foldl140113$2, %struct.ScmObj** %stackaddr$prim52127, align 8
%stackaddr$prim52128 = alloca %struct.ScmObj*, align 8
%args50191$_37foldl140113$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40116, %struct.ScmObj* %args50191$_37foldl140113$2)
store volatile %struct.ScmObj* %args50191$_37foldl140113$3, %struct.ScmObj** %stackaddr$prim52128, align 8
%stackaddr$prim52129 = alloca %struct.ScmObj*, align 8
%args50191$_37foldl140113$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40589, %struct.ScmObj* %args50191$_37foldl140113$3)
store volatile %struct.ScmObj* %args50191$_37foldl140113$4, %struct.ScmObj** %stackaddr$prim52129, align 8
%clofunc52130 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140113)
musttail call tailcc void %clofunc52130(%struct.ScmObj* %_37foldl140113, %struct.ScmObj* %args50191$_37foldl140113$4)
ret void
}

define tailcc void @proc_clo$ae41077(%struct.ScmObj* %env$ae41077,%struct.ScmObj* %current_45args50196) {
%stackaddr$prim52131 = alloca %struct.ScmObj*, align 8
%k40591 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50196)
store volatile %struct.ScmObj* %k40591, %struct.ScmObj** %stackaddr$prim52131, align 8
%stackaddr$prim52132 = alloca %struct.ScmObj*, align 8
%current_45args50197 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50196)
store volatile %struct.ScmObj* %current_45args50197, %struct.ScmObj** %stackaddr$prim52132, align 8
%stackaddr$prim52133 = alloca %struct.ScmObj*, align 8
%_37length40118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50197)
store volatile %struct.ScmObj* %_37length40118, %struct.ScmObj** %stackaddr$prim52133, align 8
%ae41079 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure52134 = alloca %struct.ScmObj*, align 8
%fptrToInt52135 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41080 to i64
%ae41080 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt52135)
store volatile %struct.ScmObj* %ae41080, %struct.ScmObj** %stackaddr$makeclosure52134, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41080, %struct.ScmObj* %_37length40118, i64 0)
%args50208$k40591$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52136 = alloca %struct.ScmObj*, align 8
%args50208$k40591$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41080, %struct.ScmObj* %args50208$k40591$0)
store volatile %struct.ScmObj* %args50208$k40591$1, %struct.ScmObj** %stackaddr$prim52136, align 8
%stackaddr$prim52137 = alloca %struct.ScmObj*, align 8
%args50208$k40591$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41079, %struct.ScmObj* %args50208$k40591$1)
store volatile %struct.ScmObj* %args50208$k40591$2, %struct.ScmObj** %stackaddr$prim52137, align 8
%clofunc52138 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40591)
musttail call tailcc void %clofunc52138(%struct.ScmObj* %k40591, %struct.ScmObj* %args50208$k40591$2)
ret void
}

define tailcc void @proc_clo$ae41080(%struct.ScmObj* %env$ae41080,%struct.ScmObj* %current_45args50199) {
%stackaddr$env-ref52139 = alloca %struct.ScmObj*, align 8
%_37length40118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41080, i64 0)
store %struct.ScmObj* %_37length40118, %struct.ScmObj** %stackaddr$env-ref52139
%stackaddr$prim52140 = alloca %struct.ScmObj*, align 8
%k40592 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50199)
store volatile %struct.ScmObj* %k40592, %struct.ScmObj** %stackaddr$prim52140, align 8
%stackaddr$prim52141 = alloca %struct.ScmObj*, align 8
%current_45args50200 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50199)
store volatile %struct.ScmObj* %current_45args50200, %struct.ScmObj** %stackaddr$prim52141, align 8
%stackaddr$prim52142 = alloca %struct.ScmObj*, align 8
%lst40119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50200)
store volatile %struct.ScmObj* %lst40119, %struct.ScmObj** %stackaddr$prim52142, align 8
%stackaddr$prim52143 = alloca %struct.ScmObj*, align 8
%anf_45bind40274 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40119)
store volatile %struct.ScmObj* %anf_45bind40274, %struct.ScmObj** %stackaddr$prim52143, align 8
%truthy$cmp52144 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40274)
%cmp$cmp52144 = icmp eq i64 %truthy$cmp52144, 1
br i1 %cmp$cmp52144, label %truebranch$cmp52144, label %falsebranch$cmp52144
truebranch$cmp52144:
%ae41084 = call %struct.ScmObj* @const_init_int(i64 0)
%ae41085 = call %struct.ScmObj* @const_init_int(i64 0)
%args50202$k40592$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52145 = alloca %struct.ScmObj*, align 8
%args50202$k40592$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41085, %struct.ScmObj* %args50202$k40592$0)
store volatile %struct.ScmObj* %args50202$k40592$1, %struct.ScmObj** %stackaddr$prim52145, align 8
%stackaddr$prim52146 = alloca %struct.ScmObj*, align 8
%args50202$k40592$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41084, %struct.ScmObj* %args50202$k40592$1)
store volatile %struct.ScmObj* %args50202$k40592$2, %struct.ScmObj** %stackaddr$prim52146, align 8
%clofunc52147 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40592)
musttail call tailcc void %clofunc52147(%struct.ScmObj* %k40592, %struct.ScmObj* %args50202$k40592$2)
ret void
falsebranch$cmp52144:
%stackaddr$prim52148 = alloca %struct.ScmObj*, align 8
%anf_45bind40275 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40119)
store volatile %struct.ScmObj* %anf_45bind40275, %struct.ScmObj** %stackaddr$prim52148, align 8
%stackaddr$makeclosure52149 = alloca %struct.ScmObj*, align 8
%fptrToInt52150 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41094 to i64
%ae41094 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt52150)
store volatile %struct.ScmObj* %ae41094, %struct.ScmObj** %stackaddr$makeclosure52149, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41094, %struct.ScmObj* %k40592, i64 0)
%args50207$_37length40118$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52151 = alloca %struct.ScmObj*, align 8
%args50207$_37length40118$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40275, %struct.ScmObj* %args50207$_37length40118$0)
store volatile %struct.ScmObj* %args50207$_37length40118$1, %struct.ScmObj** %stackaddr$prim52151, align 8
%stackaddr$prim52152 = alloca %struct.ScmObj*, align 8
%args50207$_37length40118$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41094, %struct.ScmObj* %args50207$_37length40118$1)
store volatile %struct.ScmObj* %args50207$_37length40118$2, %struct.ScmObj** %stackaddr$prim52152, align 8
%clofunc52153 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40118)
musttail call tailcc void %clofunc52153(%struct.ScmObj* %_37length40118, %struct.ScmObj* %args50207$_37length40118$2)
ret void
}

define tailcc void @proc_clo$ae41094(%struct.ScmObj* %env$ae41094,%struct.ScmObj* %current_45args50203) {
%stackaddr$env-ref52154 = alloca %struct.ScmObj*, align 8
%k40592 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41094, i64 0)
store %struct.ScmObj* %k40592, %struct.ScmObj** %stackaddr$env-ref52154
%stackaddr$prim52155 = alloca %struct.ScmObj*, align 8
%_95k40593 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50203)
store volatile %struct.ScmObj* %_95k40593, %struct.ScmObj** %stackaddr$prim52155, align 8
%stackaddr$prim52156 = alloca %struct.ScmObj*, align 8
%current_45args50204 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50203)
store volatile %struct.ScmObj* %current_45args50204, %struct.ScmObj** %stackaddr$prim52156, align 8
%stackaddr$prim52157 = alloca %struct.ScmObj*, align 8
%anf_45bind40276 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50204)
store volatile %struct.ScmObj* %anf_45bind40276, %struct.ScmObj** %stackaddr$prim52157, align 8
%ae41096 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim52158 = alloca %struct.ScmObj*, align 8
%cpsprim40594 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae41096, %struct.ScmObj* %anf_45bind40276)
store volatile %struct.ScmObj* %cpsprim40594, %struct.ScmObj** %stackaddr$prim52158, align 8
%ae41099 = call %struct.ScmObj* @const_init_int(i64 0)
%args50206$k40592$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52159 = alloca %struct.ScmObj*, align 8
%args50206$k40592$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40594, %struct.ScmObj* %args50206$k40592$0)
store volatile %struct.ScmObj* %args50206$k40592$1, %struct.ScmObj** %stackaddr$prim52159, align 8
%stackaddr$prim52160 = alloca %struct.ScmObj*, align 8
%args50206$k40592$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41099, %struct.ScmObj* %args50206$k40592$1)
store volatile %struct.ScmObj* %args50206$k40592$2, %struct.ScmObj** %stackaddr$prim52160, align 8
%clofunc52161 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40592)
musttail call tailcc void %clofunc52161(%struct.ScmObj* %k40592, %struct.ScmObj* %args50206$k40592$2)
ret void
}

define tailcc void @proc_clo$ae40927(%struct.ScmObj* %env$ae40927,%struct.ScmObj* %current_45args50211) {
%stackaddr$prim52162 = alloca %struct.ScmObj*, align 8
%k40595 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50211)
store volatile %struct.ScmObj* %k40595, %struct.ScmObj** %stackaddr$prim52162, align 8
%stackaddr$prim52163 = alloca %struct.ScmObj*, align 8
%current_45args50212 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50211)
store volatile %struct.ScmObj* %current_45args50212, %struct.ScmObj** %stackaddr$prim52163, align 8
%stackaddr$prim52164 = alloca %struct.ScmObj*, align 8
%_37take40121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50212)
store volatile %struct.ScmObj* %_37take40121, %struct.ScmObj** %stackaddr$prim52164, align 8
%ae40929 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure52165 = alloca %struct.ScmObj*, align 8
%fptrToInt52166 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40930 to i64
%ae40930 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt52166)
store volatile %struct.ScmObj* %ae40930, %struct.ScmObj** %stackaddr$makeclosure52165, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40930, %struct.ScmObj* %_37take40121, i64 0)
%args50225$k40595$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52167 = alloca %struct.ScmObj*, align 8
%args50225$k40595$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40930, %struct.ScmObj* %args50225$k40595$0)
store volatile %struct.ScmObj* %args50225$k40595$1, %struct.ScmObj** %stackaddr$prim52167, align 8
%stackaddr$prim52168 = alloca %struct.ScmObj*, align 8
%args50225$k40595$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40929, %struct.ScmObj* %args50225$k40595$1)
store volatile %struct.ScmObj* %args50225$k40595$2, %struct.ScmObj** %stackaddr$prim52168, align 8
%clofunc52169 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40595)
musttail call tailcc void %clofunc52169(%struct.ScmObj* %k40595, %struct.ScmObj* %args50225$k40595$2)
ret void
}

define tailcc void @proc_clo$ae40930(%struct.ScmObj* %env$ae40930,%struct.ScmObj* %current_45args50214) {
%stackaddr$env-ref52170 = alloca %struct.ScmObj*, align 8
%_37take40121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40930, i64 0)
store %struct.ScmObj* %_37take40121, %struct.ScmObj** %stackaddr$env-ref52170
%stackaddr$prim52171 = alloca %struct.ScmObj*, align 8
%k40596 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50214)
store volatile %struct.ScmObj* %k40596, %struct.ScmObj** %stackaddr$prim52171, align 8
%stackaddr$prim52172 = alloca %struct.ScmObj*, align 8
%current_45args50215 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50214)
store volatile %struct.ScmObj* %current_45args50215, %struct.ScmObj** %stackaddr$prim52172, align 8
%stackaddr$prim52173 = alloca %struct.ScmObj*, align 8
%lst40123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50215)
store volatile %struct.ScmObj* %lst40123, %struct.ScmObj** %stackaddr$prim52173, align 8
%stackaddr$prim52174 = alloca %struct.ScmObj*, align 8
%current_45args50216 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50215)
store volatile %struct.ScmObj* %current_45args50216, %struct.ScmObj** %stackaddr$prim52174, align 8
%stackaddr$prim52175 = alloca %struct.ScmObj*, align 8
%n40122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50216)
store volatile %struct.ScmObj* %n40122, %struct.ScmObj** %stackaddr$prim52175, align 8
%ae40932 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim52176 = alloca %struct.ScmObj*, align 8
%anf_45bind40267 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n40122, %struct.ScmObj* %ae40932)
store volatile %struct.ScmObj* %anf_45bind40267, %struct.ScmObj** %stackaddr$prim52176, align 8
%truthy$cmp52177 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40267)
%cmp$cmp52177 = icmp eq i64 %truthy$cmp52177, 1
br i1 %cmp$cmp52177, label %truebranch$cmp52177, label %falsebranch$cmp52177
truebranch$cmp52177:
%ae40935 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40936 = call %struct.ScmObj* @const_init_null()
%args50218$k40596$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52178 = alloca %struct.ScmObj*, align 8
%args50218$k40596$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40936, %struct.ScmObj* %args50218$k40596$0)
store volatile %struct.ScmObj* %args50218$k40596$1, %struct.ScmObj** %stackaddr$prim52178, align 8
%stackaddr$prim52179 = alloca %struct.ScmObj*, align 8
%args50218$k40596$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40935, %struct.ScmObj* %args50218$k40596$1)
store volatile %struct.ScmObj* %args50218$k40596$2, %struct.ScmObj** %stackaddr$prim52179, align 8
%clofunc52180 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40596)
musttail call tailcc void %clofunc52180(%struct.ScmObj* %k40596, %struct.ScmObj* %args50218$k40596$2)
ret void
falsebranch$cmp52177:
%stackaddr$prim52181 = alloca %struct.ScmObj*, align 8
%anf_45bind40268 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40123)
store volatile %struct.ScmObj* %anf_45bind40268, %struct.ScmObj** %stackaddr$prim52181, align 8
%truthy$cmp52182 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40268)
%cmp$cmp52182 = icmp eq i64 %truthy$cmp52182, 1
br i1 %cmp$cmp52182, label %truebranch$cmp52182, label %falsebranch$cmp52182
truebranch$cmp52182:
%ae40946 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40947 = call %struct.ScmObj* @const_init_null()
%args50219$k40596$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52183 = alloca %struct.ScmObj*, align 8
%args50219$k40596$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40947, %struct.ScmObj* %args50219$k40596$0)
store volatile %struct.ScmObj* %args50219$k40596$1, %struct.ScmObj** %stackaddr$prim52183, align 8
%stackaddr$prim52184 = alloca %struct.ScmObj*, align 8
%args50219$k40596$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40946, %struct.ScmObj* %args50219$k40596$1)
store volatile %struct.ScmObj* %args50219$k40596$2, %struct.ScmObj** %stackaddr$prim52184, align 8
%clofunc52185 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40596)
musttail call tailcc void %clofunc52185(%struct.ScmObj* %k40596, %struct.ScmObj* %args50219$k40596$2)
ret void
falsebranch$cmp52182:
%stackaddr$prim52186 = alloca %struct.ScmObj*, align 8
%anf_45bind40269 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40123)
store volatile %struct.ScmObj* %anf_45bind40269, %struct.ScmObj** %stackaddr$prim52186, align 8
%stackaddr$prim52187 = alloca %struct.ScmObj*, align 8
%anf_45bind40270 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40123)
store volatile %struct.ScmObj* %anf_45bind40270, %struct.ScmObj** %stackaddr$prim52187, align 8
%ae40957 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim52188 = alloca %struct.ScmObj*, align 8
%anf_45bind40271 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n40122, %struct.ScmObj* %ae40957)
store volatile %struct.ScmObj* %anf_45bind40271, %struct.ScmObj** %stackaddr$prim52188, align 8
%stackaddr$makeclosure52189 = alloca %struct.ScmObj*, align 8
%fptrToInt52190 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40959 to i64
%ae40959 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt52190)
store volatile %struct.ScmObj* %ae40959, %struct.ScmObj** %stackaddr$makeclosure52189, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40959, %struct.ScmObj* %k40596, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40959, %struct.ScmObj* %anf_45bind40269, i64 1)
%args50224$_37take40121$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52191 = alloca %struct.ScmObj*, align 8
%args50224$_37take40121$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40271, %struct.ScmObj* %args50224$_37take40121$0)
store volatile %struct.ScmObj* %args50224$_37take40121$1, %struct.ScmObj** %stackaddr$prim52191, align 8
%stackaddr$prim52192 = alloca %struct.ScmObj*, align 8
%args50224$_37take40121$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40270, %struct.ScmObj* %args50224$_37take40121$1)
store volatile %struct.ScmObj* %args50224$_37take40121$2, %struct.ScmObj** %stackaddr$prim52192, align 8
%stackaddr$prim52193 = alloca %struct.ScmObj*, align 8
%args50224$_37take40121$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40959, %struct.ScmObj* %args50224$_37take40121$2)
store volatile %struct.ScmObj* %args50224$_37take40121$3, %struct.ScmObj** %stackaddr$prim52193, align 8
%clofunc52194 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40121)
musttail call tailcc void %clofunc52194(%struct.ScmObj* %_37take40121, %struct.ScmObj* %args50224$_37take40121$3)
ret void
}

define tailcc void @proc_clo$ae40959(%struct.ScmObj* %env$ae40959,%struct.ScmObj* %current_45args50220) {
%stackaddr$env-ref52195 = alloca %struct.ScmObj*, align 8
%k40596 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40959, i64 0)
store %struct.ScmObj* %k40596, %struct.ScmObj** %stackaddr$env-ref52195
%stackaddr$env-ref52196 = alloca %struct.ScmObj*, align 8
%anf_45bind40269 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40959, i64 1)
store %struct.ScmObj* %anf_45bind40269, %struct.ScmObj** %stackaddr$env-ref52196
%stackaddr$prim52197 = alloca %struct.ScmObj*, align 8
%_95k40597 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50220)
store volatile %struct.ScmObj* %_95k40597, %struct.ScmObj** %stackaddr$prim52197, align 8
%stackaddr$prim52198 = alloca %struct.ScmObj*, align 8
%current_45args50221 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50220)
store volatile %struct.ScmObj* %current_45args50221, %struct.ScmObj** %stackaddr$prim52198, align 8
%stackaddr$prim52199 = alloca %struct.ScmObj*, align 8
%anf_45bind40272 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50221)
store volatile %struct.ScmObj* %anf_45bind40272, %struct.ScmObj** %stackaddr$prim52199, align 8
%stackaddr$prim52200 = alloca %struct.ScmObj*, align 8
%cpsprim40598 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40269, %struct.ScmObj* %anf_45bind40272)
store volatile %struct.ScmObj* %cpsprim40598, %struct.ScmObj** %stackaddr$prim52200, align 8
%ae40965 = call %struct.ScmObj* @const_init_int(i64 0)
%args50223$k40596$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52201 = alloca %struct.ScmObj*, align 8
%args50223$k40596$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40598, %struct.ScmObj* %args50223$k40596$0)
store volatile %struct.ScmObj* %args50223$k40596$1, %struct.ScmObj** %stackaddr$prim52201, align 8
%stackaddr$prim52202 = alloca %struct.ScmObj*, align 8
%args50223$k40596$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40965, %struct.ScmObj* %args50223$k40596$1)
store volatile %struct.ScmObj* %args50223$k40596$2, %struct.ScmObj** %stackaddr$prim52202, align 8
%clofunc52203 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40596)
musttail call tailcc void %clofunc52203(%struct.ScmObj* %k40596, %struct.ScmObj* %args50223$k40596$2)
ret void
}

define tailcc void @proc_clo$ae40830(%struct.ScmObj* %env$ae40830,%struct.ScmObj* %current_45args50228) {
%stackaddr$prim52204 = alloca %struct.ScmObj*, align 8
%k40599 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50228)
store volatile %struct.ScmObj* %k40599, %struct.ScmObj** %stackaddr$prim52204, align 8
%stackaddr$prim52205 = alloca %struct.ScmObj*, align 8
%current_45args50229 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50228)
store volatile %struct.ScmObj* %current_45args50229, %struct.ScmObj** %stackaddr$prim52205, align 8
%stackaddr$prim52206 = alloca %struct.ScmObj*, align 8
%_37map40125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50229)
store volatile %struct.ScmObj* %_37map40125, %struct.ScmObj** %stackaddr$prim52206, align 8
%ae40832 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure52207 = alloca %struct.ScmObj*, align 8
%fptrToInt52208 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40833 to i64
%ae40833 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt52208)
store volatile %struct.ScmObj* %ae40833, %struct.ScmObj** %stackaddr$makeclosure52207, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40833, %struct.ScmObj* %_37map40125, i64 0)
%args50245$k40599$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52209 = alloca %struct.ScmObj*, align 8
%args50245$k40599$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40833, %struct.ScmObj* %args50245$k40599$0)
store volatile %struct.ScmObj* %args50245$k40599$1, %struct.ScmObj** %stackaddr$prim52209, align 8
%stackaddr$prim52210 = alloca %struct.ScmObj*, align 8
%args50245$k40599$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40832, %struct.ScmObj* %args50245$k40599$1)
store volatile %struct.ScmObj* %args50245$k40599$2, %struct.ScmObj** %stackaddr$prim52210, align 8
%clofunc52211 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40599)
musttail call tailcc void %clofunc52211(%struct.ScmObj* %k40599, %struct.ScmObj* %args50245$k40599$2)
ret void
}

define tailcc void @proc_clo$ae40833(%struct.ScmObj* %env$ae40833,%struct.ScmObj* %current_45args50231) {
%stackaddr$env-ref52212 = alloca %struct.ScmObj*, align 8
%_37map40125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40833, i64 0)
store %struct.ScmObj* %_37map40125, %struct.ScmObj** %stackaddr$env-ref52212
%stackaddr$prim52213 = alloca %struct.ScmObj*, align 8
%k40600 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50231)
store volatile %struct.ScmObj* %k40600, %struct.ScmObj** %stackaddr$prim52213, align 8
%stackaddr$prim52214 = alloca %struct.ScmObj*, align 8
%current_45args50232 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50231)
store volatile %struct.ScmObj* %current_45args50232, %struct.ScmObj** %stackaddr$prim52214, align 8
%stackaddr$prim52215 = alloca %struct.ScmObj*, align 8
%f40127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50232)
store volatile %struct.ScmObj* %f40127, %struct.ScmObj** %stackaddr$prim52215, align 8
%stackaddr$prim52216 = alloca %struct.ScmObj*, align 8
%current_45args50233 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50232)
store volatile %struct.ScmObj* %current_45args50233, %struct.ScmObj** %stackaddr$prim52216, align 8
%stackaddr$prim52217 = alloca %struct.ScmObj*, align 8
%lst40126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50233)
store volatile %struct.ScmObj* %lst40126, %struct.ScmObj** %stackaddr$prim52217, align 8
%stackaddr$prim52218 = alloca %struct.ScmObj*, align 8
%anf_45bind40261 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40126)
store volatile %struct.ScmObj* %anf_45bind40261, %struct.ScmObj** %stackaddr$prim52218, align 8
%truthy$cmp52219 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40261)
%cmp$cmp52219 = icmp eq i64 %truthy$cmp52219, 1
br i1 %cmp$cmp52219, label %truebranch$cmp52219, label %falsebranch$cmp52219
truebranch$cmp52219:
%ae40837 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40838 = call %struct.ScmObj* @const_init_null()
%args50235$k40600$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52220 = alloca %struct.ScmObj*, align 8
%args50235$k40600$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40838, %struct.ScmObj* %args50235$k40600$0)
store volatile %struct.ScmObj* %args50235$k40600$1, %struct.ScmObj** %stackaddr$prim52220, align 8
%stackaddr$prim52221 = alloca %struct.ScmObj*, align 8
%args50235$k40600$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40837, %struct.ScmObj* %args50235$k40600$1)
store volatile %struct.ScmObj* %args50235$k40600$2, %struct.ScmObj** %stackaddr$prim52221, align 8
%clofunc52222 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40600)
musttail call tailcc void %clofunc52222(%struct.ScmObj* %k40600, %struct.ScmObj* %args50235$k40600$2)
ret void
falsebranch$cmp52219:
%stackaddr$prim52223 = alloca %struct.ScmObj*, align 8
%anf_45bind40262 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40126)
store volatile %struct.ScmObj* %anf_45bind40262, %struct.ScmObj** %stackaddr$prim52223, align 8
%stackaddr$makeclosure52224 = alloca %struct.ScmObj*, align 8
%fptrToInt52225 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40847 to i64
%ae40847 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt52225)
store volatile %struct.ScmObj* %ae40847, %struct.ScmObj** %stackaddr$makeclosure52224, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40847, %struct.ScmObj* %k40600, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40847, %struct.ScmObj* %f40127, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40847, %struct.ScmObj* %lst40126, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae40847, %struct.ScmObj* %_37map40125, i64 3)
%args50244$f40127$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52226 = alloca %struct.ScmObj*, align 8
%args50244$f40127$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40262, %struct.ScmObj* %args50244$f40127$0)
store volatile %struct.ScmObj* %args50244$f40127$1, %struct.ScmObj** %stackaddr$prim52226, align 8
%stackaddr$prim52227 = alloca %struct.ScmObj*, align 8
%args50244$f40127$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40847, %struct.ScmObj* %args50244$f40127$1)
store volatile %struct.ScmObj* %args50244$f40127$2, %struct.ScmObj** %stackaddr$prim52227, align 8
%clofunc52228 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40127)
musttail call tailcc void %clofunc52228(%struct.ScmObj* %f40127, %struct.ScmObj* %args50244$f40127$2)
ret void
}

define tailcc void @proc_clo$ae40847(%struct.ScmObj* %env$ae40847,%struct.ScmObj* %current_45args50236) {
%stackaddr$env-ref52229 = alloca %struct.ScmObj*, align 8
%k40600 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40847, i64 0)
store %struct.ScmObj* %k40600, %struct.ScmObj** %stackaddr$env-ref52229
%stackaddr$env-ref52230 = alloca %struct.ScmObj*, align 8
%f40127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40847, i64 1)
store %struct.ScmObj* %f40127, %struct.ScmObj** %stackaddr$env-ref52230
%stackaddr$env-ref52231 = alloca %struct.ScmObj*, align 8
%lst40126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40847, i64 2)
store %struct.ScmObj* %lst40126, %struct.ScmObj** %stackaddr$env-ref52231
%stackaddr$env-ref52232 = alloca %struct.ScmObj*, align 8
%_37map40125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40847, i64 3)
store %struct.ScmObj* %_37map40125, %struct.ScmObj** %stackaddr$env-ref52232
%stackaddr$prim52233 = alloca %struct.ScmObj*, align 8
%_95k40601 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50236)
store volatile %struct.ScmObj* %_95k40601, %struct.ScmObj** %stackaddr$prim52233, align 8
%stackaddr$prim52234 = alloca %struct.ScmObj*, align 8
%current_45args50237 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50236)
store volatile %struct.ScmObj* %current_45args50237, %struct.ScmObj** %stackaddr$prim52234, align 8
%stackaddr$prim52235 = alloca %struct.ScmObj*, align 8
%anf_45bind40263 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50237)
store volatile %struct.ScmObj* %anf_45bind40263, %struct.ScmObj** %stackaddr$prim52235, align 8
%stackaddr$prim52236 = alloca %struct.ScmObj*, align 8
%anf_45bind40264 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40126)
store volatile %struct.ScmObj* %anf_45bind40264, %struct.ScmObj** %stackaddr$prim52236, align 8
%stackaddr$makeclosure52237 = alloca %struct.ScmObj*, align 8
%fptrToInt52238 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40851 to i64
%ae40851 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt52238)
store volatile %struct.ScmObj* %ae40851, %struct.ScmObj** %stackaddr$makeclosure52237, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40851, %struct.ScmObj* %k40600, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40851, %struct.ScmObj* %anf_45bind40263, i64 1)
%args50243$_37map40125$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52239 = alloca %struct.ScmObj*, align 8
%args50243$_37map40125$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40264, %struct.ScmObj* %args50243$_37map40125$0)
store volatile %struct.ScmObj* %args50243$_37map40125$1, %struct.ScmObj** %stackaddr$prim52239, align 8
%stackaddr$prim52240 = alloca %struct.ScmObj*, align 8
%args50243$_37map40125$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40127, %struct.ScmObj* %args50243$_37map40125$1)
store volatile %struct.ScmObj* %args50243$_37map40125$2, %struct.ScmObj** %stackaddr$prim52240, align 8
%stackaddr$prim52241 = alloca %struct.ScmObj*, align 8
%args50243$_37map40125$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40851, %struct.ScmObj* %args50243$_37map40125$2)
store volatile %struct.ScmObj* %args50243$_37map40125$3, %struct.ScmObj** %stackaddr$prim52241, align 8
%clofunc52242 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map40125)
musttail call tailcc void %clofunc52242(%struct.ScmObj* %_37map40125, %struct.ScmObj* %args50243$_37map40125$3)
ret void
}

define tailcc void @proc_clo$ae40851(%struct.ScmObj* %env$ae40851,%struct.ScmObj* %current_45args50239) {
%stackaddr$env-ref52243 = alloca %struct.ScmObj*, align 8
%k40600 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40851, i64 0)
store %struct.ScmObj* %k40600, %struct.ScmObj** %stackaddr$env-ref52243
%stackaddr$env-ref52244 = alloca %struct.ScmObj*, align 8
%anf_45bind40263 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40851, i64 1)
store %struct.ScmObj* %anf_45bind40263, %struct.ScmObj** %stackaddr$env-ref52244
%stackaddr$prim52245 = alloca %struct.ScmObj*, align 8
%_95k40602 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50239)
store volatile %struct.ScmObj* %_95k40602, %struct.ScmObj** %stackaddr$prim52245, align 8
%stackaddr$prim52246 = alloca %struct.ScmObj*, align 8
%current_45args50240 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50239)
store volatile %struct.ScmObj* %current_45args50240, %struct.ScmObj** %stackaddr$prim52246, align 8
%stackaddr$prim52247 = alloca %struct.ScmObj*, align 8
%anf_45bind40265 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50240)
store volatile %struct.ScmObj* %anf_45bind40265, %struct.ScmObj** %stackaddr$prim52247, align 8
%stackaddr$prim52248 = alloca %struct.ScmObj*, align 8
%cpsprim40603 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40263, %struct.ScmObj* %anf_45bind40265)
store volatile %struct.ScmObj* %cpsprim40603, %struct.ScmObj** %stackaddr$prim52248, align 8
%ae40857 = call %struct.ScmObj* @const_init_int(i64 0)
%args50242$k40600$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52249 = alloca %struct.ScmObj*, align 8
%args50242$k40600$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40603, %struct.ScmObj* %args50242$k40600$0)
store volatile %struct.ScmObj* %args50242$k40600$1, %struct.ScmObj** %stackaddr$prim52249, align 8
%stackaddr$prim52250 = alloca %struct.ScmObj*, align 8
%args50242$k40600$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40857, %struct.ScmObj* %args50242$k40600$1)
store volatile %struct.ScmObj* %args50242$k40600$2, %struct.ScmObj** %stackaddr$prim52250, align 8
%clofunc52251 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40600)
musttail call tailcc void %clofunc52251(%struct.ScmObj* %k40600, %struct.ScmObj* %args50242$k40600$2)
ret void
}

define tailcc void @proc_clo$ae40750(%struct.ScmObj* %env$ae40750,%struct.ScmObj* %current_45args50248) {
%stackaddr$prim52252 = alloca %struct.ScmObj*, align 8
%k40604 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50248)
store volatile %struct.ScmObj* %k40604, %struct.ScmObj** %stackaddr$prim52252, align 8
%stackaddr$prim52253 = alloca %struct.ScmObj*, align 8
%current_45args50249 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50248)
store volatile %struct.ScmObj* %current_45args50249, %struct.ScmObj** %stackaddr$prim52253, align 8
%stackaddr$prim52254 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50249)
store volatile %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$prim52254, align 8
%ae40752 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure52255 = alloca %struct.ScmObj*, align 8
%fptrToInt52256 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40753 to i64
%ae40753 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt52256)
store volatile %struct.ScmObj* %ae40753, %struct.ScmObj** %stackaddr$makeclosure52255, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40753, %struct.ScmObj* %_37foldr140129, i64 0)
%args50262$k40604$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52257 = alloca %struct.ScmObj*, align 8
%args50262$k40604$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40753, %struct.ScmObj* %args50262$k40604$0)
store volatile %struct.ScmObj* %args50262$k40604$1, %struct.ScmObj** %stackaddr$prim52257, align 8
%stackaddr$prim52258 = alloca %struct.ScmObj*, align 8
%args50262$k40604$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40752, %struct.ScmObj* %args50262$k40604$1)
store volatile %struct.ScmObj* %args50262$k40604$2, %struct.ScmObj** %stackaddr$prim52258, align 8
%clofunc52259 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40604)
musttail call tailcc void %clofunc52259(%struct.ScmObj* %k40604, %struct.ScmObj* %args50262$k40604$2)
ret void
}

define tailcc void @proc_clo$ae40753(%struct.ScmObj* %env$ae40753,%struct.ScmObj* %current_45args50251) {
%stackaddr$env-ref52260 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40753, i64 0)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref52260
%stackaddr$prim52261 = alloca %struct.ScmObj*, align 8
%k40605 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50251)
store volatile %struct.ScmObj* %k40605, %struct.ScmObj** %stackaddr$prim52261, align 8
%stackaddr$prim52262 = alloca %struct.ScmObj*, align 8
%current_45args50252 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50251)
store volatile %struct.ScmObj* %current_45args50252, %struct.ScmObj** %stackaddr$prim52262, align 8
%stackaddr$prim52263 = alloca %struct.ScmObj*, align 8
%f40132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50252)
store volatile %struct.ScmObj* %f40132, %struct.ScmObj** %stackaddr$prim52263, align 8
%stackaddr$prim52264 = alloca %struct.ScmObj*, align 8
%current_45args50253 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50252)
store volatile %struct.ScmObj* %current_45args50253, %struct.ScmObj** %stackaddr$prim52264, align 8
%stackaddr$prim52265 = alloca %struct.ScmObj*, align 8
%acc40131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50253)
store volatile %struct.ScmObj* %acc40131, %struct.ScmObj** %stackaddr$prim52265, align 8
%stackaddr$prim52266 = alloca %struct.ScmObj*, align 8
%current_45args50254 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50253)
store volatile %struct.ScmObj* %current_45args50254, %struct.ScmObj** %stackaddr$prim52266, align 8
%stackaddr$prim52267 = alloca %struct.ScmObj*, align 8
%lst40130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50254)
store volatile %struct.ScmObj* %lst40130, %struct.ScmObj** %stackaddr$prim52267, align 8
%stackaddr$prim52268 = alloca %struct.ScmObj*, align 8
%anf_45bind40256 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40130)
store volatile %struct.ScmObj* %anf_45bind40256, %struct.ScmObj** %stackaddr$prim52268, align 8
%truthy$cmp52269 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40256)
%cmp$cmp52269 = icmp eq i64 %truthy$cmp52269, 1
br i1 %cmp$cmp52269, label %truebranch$cmp52269, label %falsebranch$cmp52269
truebranch$cmp52269:
%ae40757 = call %struct.ScmObj* @const_init_int(i64 0)
%args50256$k40605$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52270 = alloca %struct.ScmObj*, align 8
%args50256$k40605$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40131, %struct.ScmObj* %args50256$k40605$0)
store volatile %struct.ScmObj* %args50256$k40605$1, %struct.ScmObj** %stackaddr$prim52270, align 8
%stackaddr$prim52271 = alloca %struct.ScmObj*, align 8
%args50256$k40605$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40757, %struct.ScmObj* %args50256$k40605$1)
store volatile %struct.ScmObj* %args50256$k40605$2, %struct.ScmObj** %stackaddr$prim52271, align 8
%clofunc52272 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40605)
musttail call tailcc void %clofunc52272(%struct.ScmObj* %k40605, %struct.ScmObj* %args50256$k40605$2)
ret void
falsebranch$cmp52269:
%stackaddr$prim52273 = alloca %struct.ScmObj*, align 8
%anf_45bind40257 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40130)
store volatile %struct.ScmObj* %anf_45bind40257, %struct.ScmObj** %stackaddr$prim52273, align 8
%stackaddr$prim52274 = alloca %struct.ScmObj*, align 8
%anf_45bind40258 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40130)
store volatile %struct.ScmObj* %anf_45bind40258, %struct.ScmObj** %stackaddr$prim52274, align 8
%stackaddr$makeclosure52275 = alloca %struct.ScmObj*, align 8
%fptrToInt52276 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40765 to i64
%ae40765 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt52276)
store volatile %struct.ScmObj* %ae40765, %struct.ScmObj** %stackaddr$makeclosure52275, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40765, %struct.ScmObj* %f40132, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40765, %struct.ScmObj* %anf_45bind40257, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40765, %struct.ScmObj* %k40605, i64 2)
%args50261$_37foldr140129$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52277 = alloca %struct.ScmObj*, align 8
%args50261$_37foldr140129$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40258, %struct.ScmObj* %args50261$_37foldr140129$0)
store volatile %struct.ScmObj* %args50261$_37foldr140129$1, %struct.ScmObj** %stackaddr$prim52277, align 8
%stackaddr$prim52278 = alloca %struct.ScmObj*, align 8
%args50261$_37foldr140129$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40131, %struct.ScmObj* %args50261$_37foldr140129$1)
store volatile %struct.ScmObj* %args50261$_37foldr140129$2, %struct.ScmObj** %stackaddr$prim52278, align 8
%stackaddr$prim52279 = alloca %struct.ScmObj*, align 8
%args50261$_37foldr140129$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40132, %struct.ScmObj* %args50261$_37foldr140129$2)
store volatile %struct.ScmObj* %args50261$_37foldr140129$3, %struct.ScmObj** %stackaddr$prim52279, align 8
%stackaddr$prim52280 = alloca %struct.ScmObj*, align 8
%args50261$_37foldr140129$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40765, %struct.ScmObj* %args50261$_37foldr140129$3)
store volatile %struct.ScmObj* %args50261$_37foldr140129$4, %struct.ScmObj** %stackaddr$prim52280, align 8
%clofunc52281 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140129)
musttail call tailcc void %clofunc52281(%struct.ScmObj* %_37foldr140129, %struct.ScmObj* %args50261$_37foldr140129$4)
ret void
}

define tailcc void @proc_clo$ae40765(%struct.ScmObj* %env$ae40765,%struct.ScmObj* %current_45args50257) {
%stackaddr$env-ref52282 = alloca %struct.ScmObj*, align 8
%f40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40765, i64 0)
store %struct.ScmObj* %f40132, %struct.ScmObj** %stackaddr$env-ref52282
%stackaddr$env-ref52283 = alloca %struct.ScmObj*, align 8
%anf_45bind40257 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40765, i64 1)
store %struct.ScmObj* %anf_45bind40257, %struct.ScmObj** %stackaddr$env-ref52283
%stackaddr$env-ref52284 = alloca %struct.ScmObj*, align 8
%k40605 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40765, i64 2)
store %struct.ScmObj* %k40605, %struct.ScmObj** %stackaddr$env-ref52284
%stackaddr$prim52285 = alloca %struct.ScmObj*, align 8
%_95k40606 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50257)
store volatile %struct.ScmObj* %_95k40606, %struct.ScmObj** %stackaddr$prim52285, align 8
%stackaddr$prim52286 = alloca %struct.ScmObj*, align 8
%current_45args50258 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50257)
store volatile %struct.ScmObj* %current_45args50258, %struct.ScmObj** %stackaddr$prim52286, align 8
%stackaddr$prim52287 = alloca %struct.ScmObj*, align 8
%anf_45bind40259 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50258)
store volatile %struct.ScmObj* %anf_45bind40259, %struct.ScmObj** %stackaddr$prim52287, align 8
%args50260$f40132$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52288 = alloca %struct.ScmObj*, align 8
%args50260$f40132$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40259, %struct.ScmObj* %args50260$f40132$0)
store volatile %struct.ScmObj* %args50260$f40132$1, %struct.ScmObj** %stackaddr$prim52288, align 8
%stackaddr$prim52289 = alloca %struct.ScmObj*, align 8
%args50260$f40132$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40257, %struct.ScmObj* %args50260$f40132$1)
store volatile %struct.ScmObj* %args50260$f40132$2, %struct.ScmObj** %stackaddr$prim52289, align 8
%stackaddr$prim52290 = alloca %struct.ScmObj*, align 8
%args50260$f40132$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40605, %struct.ScmObj* %args50260$f40132$2)
store volatile %struct.ScmObj* %args50260$f40132$3, %struct.ScmObj** %stackaddr$prim52290, align 8
%clofunc52291 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40132)
musttail call tailcc void %clofunc52291(%struct.ScmObj* %f40132, %struct.ScmObj* %args50260$f40132$3)
ret void
}

define tailcc void @proc_clo$ae40633(%struct.ScmObj* %env$ae40633,%struct.ScmObj* %current_45args50265) {
%stackaddr$prim52292 = alloca %struct.ScmObj*, align 8
%k40607 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50265)
store volatile %struct.ScmObj* %k40607, %struct.ScmObj** %stackaddr$prim52292, align 8
%stackaddr$prim52293 = alloca %struct.ScmObj*, align 8
%current_45args50266 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50265)
store volatile %struct.ScmObj* %current_45args50266, %struct.ScmObj** %stackaddr$prim52293, align 8
%stackaddr$prim52294 = alloca %struct.ScmObj*, align 8
%y40109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50266)
store volatile %struct.ScmObj* %y40109, %struct.ScmObj** %stackaddr$prim52294, align 8
%ae40635 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure52295 = alloca %struct.ScmObj*, align 8
%fptrToInt52296 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40636 to i64
%ae40636 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt52296)
store volatile %struct.ScmObj* %ae40636, %struct.ScmObj** %stackaddr$makeclosure52295, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40636, %struct.ScmObj* %y40109, i64 0)
%args50284$k40607$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52297 = alloca %struct.ScmObj*, align 8
%args50284$k40607$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40636, %struct.ScmObj* %args50284$k40607$0)
store volatile %struct.ScmObj* %args50284$k40607$1, %struct.ScmObj** %stackaddr$prim52297, align 8
%stackaddr$prim52298 = alloca %struct.ScmObj*, align 8
%args50284$k40607$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40635, %struct.ScmObj* %args50284$k40607$1)
store volatile %struct.ScmObj* %args50284$k40607$2, %struct.ScmObj** %stackaddr$prim52298, align 8
%clofunc52299 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40607)
musttail call tailcc void %clofunc52299(%struct.ScmObj* %k40607, %struct.ScmObj* %args50284$k40607$2)
ret void
}

define tailcc void @proc_clo$ae40636(%struct.ScmObj* %env$ae40636,%struct.ScmObj* %current_45args50268) {
%stackaddr$env-ref52300 = alloca %struct.ScmObj*, align 8
%y40109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40636, i64 0)
store %struct.ScmObj* %y40109, %struct.ScmObj** %stackaddr$env-ref52300
%stackaddr$prim52301 = alloca %struct.ScmObj*, align 8
%k40608 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50268)
store volatile %struct.ScmObj* %k40608, %struct.ScmObj** %stackaddr$prim52301, align 8
%stackaddr$prim52302 = alloca %struct.ScmObj*, align 8
%current_45args50269 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50268)
store volatile %struct.ScmObj* %current_45args50269, %struct.ScmObj** %stackaddr$prim52302, align 8
%stackaddr$prim52303 = alloca %struct.ScmObj*, align 8
%f40110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50269)
store volatile %struct.ScmObj* %f40110, %struct.ScmObj** %stackaddr$prim52303, align 8
%stackaddr$makeclosure52304 = alloca %struct.ScmObj*, align 8
%fptrToInt52305 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40637 to i64
%ae40637 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt52305)
store volatile %struct.ScmObj* %ae40637, %struct.ScmObj** %stackaddr$makeclosure52304, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40637, %struct.ScmObj* %k40608, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40637, %struct.ScmObj* %f40110, i64 1)
%ae40638 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure52306 = alloca %struct.ScmObj*, align 8
%fptrToInt52307 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40639 to i64
%ae40639 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt52307)
store volatile %struct.ScmObj* %ae40639, %struct.ScmObj** %stackaddr$makeclosure52306, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40639, %struct.ScmObj* %f40110, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40639, %struct.ScmObj* %y40109, i64 1)
%args50283$ae40637$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52308 = alloca %struct.ScmObj*, align 8
%args50283$ae40637$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40639, %struct.ScmObj* %args50283$ae40637$0)
store volatile %struct.ScmObj* %args50283$ae40637$1, %struct.ScmObj** %stackaddr$prim52308, align 8
%stackaddr$prim52309 = alloca %struct.ScmObj*, align 8
%args50283$ae40637$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40638, %struct.ScmObj* %args50283$ae40637$1)
store volatile %struct.ScmObj* %args50283$ae40637$2, %struct.ScmObj** %stackaddr$prim52309, align 8
%clofunc52310 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40637)
musttail call tailcc void %clofunc52310(%struct.ScmObj* %ae40637, %struct.ScmObj* %args50283$ae40637$2)
ret void
}

define tailcc void @proc_clo$ae40637(%struct.ScmObj* %env$ae40637,%struct.ScmObj* %current_45args50271) {
%stackaddr$env-ref52311 = alloca %struct.ScmObj*, align 8
%k40608 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40637, i64 0)
store %struct.ScmObj* %k40608, %struct.ScmObj** %stackaddr$env-ref52311
%stackaddr$env-ref52312 = alloca %struct.ScmObj*, align 8
%f40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40637, i64 1)
store %struct.ScmObj* %f40110, %struct.ScmObj** %stackaddr$env-ref52312
%stackaddr$prim52313 = alloca %struct.ScmObj*, align 8
%_95k40609 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50271)
store volatile %struct.ScmObj* %_95k40609, %struct.ScmObj** %stackaddr$prim52313, align 8
%stackaddr$prim52314 = alloca %struct.ScmObj*, align 8
%current_45args50272 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50271)
store volatile %struct.ScmObj* %current_45args50272, %struct.ScmObj** %stackaddr$prim52314, align 8
%stackaddr$prim52315 = alloca %struct.ScmObj*, align 8
%anf_45bind40254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50272)
store volatile %struct.ScmObj* %anf_45bind40254, %struct.ScmObj** %stackaddr$prim52315, align 8
%args50274$f40110$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52316 = alloca %struct.ScmObj*, align 8
%args50274$f40110$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40254, %struct.ScmObj* %args50274$f40110$0)
store volatile %struct.ScmObj* %args50274$f40110$1, %struct.ScmObj** %stackaddr$prim52316, align 8
%stackaddr$prim52317 = alloca %struct.ScmObj*, align 8
%args50274$f40110$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40608, %struct.ScmObj* %args50274$f40110$1)
store volatile %struct.ScmObj* %args50274$f40110$2, %struct.ScmObj** %stackaddr$prim52317, align 8
%clofunc52318 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40110)
musttail call tailcc void %clofunc52318(%struct.ScmObj* %f40110, %struct.ScmObj* %args50274$f40110$2)
ret void
}

define tailcc void @proc_clo$ae40639(%struct.ScmObj* %env$ae40639,%struct.ScmObj* %args4011140610) {
%stackaddr$env-ref52319 = alloca %struct.ScmObj*, align 8
%f40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40639, i64 0)
store %struct.ScmObj* %f40110, %struct.ScmObj** %stackaddr$env-ref52319
%stackaddr$env-ref52320 = alloca %struct.ScmObj*, align 8
%y40109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40639, i64 1)
store %struct.ScmObj* %y40109, %struct.ScmObj** %stackaddr$env-ref52320
%stackaddr$prim52321 = alloca %struct.ScmObj*, align 8
%k40611 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4011140610)
store volatile %struct.ScmObj* %k40611, %struct.ScmObj** %stackaddr$prim52321, align 8
%stackaddr$prim52322 = alloca %struct.ScmObj*, align 8
%args40111 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4011140610)
store volatile %struct.ScmObj* %args40111, %struct.ScmObj** %stackaddr$prim52322, align 8
%stackaddr$makeclosure52323 = alloca %struct.ScmObj*, align 8
%fptrToInt52324 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40643 to i64
%ae40643 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt52324)
store volatile %struct.ScmObj* %ae40643, %struct.ScmObj** %stackaddr$makeclosure52323, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40643, %struct.ScmObj* %k40611, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40643, %struct.ScmObj* %args40111, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40643, %struct.ScmObj* %f40110, i64 2)
%args50282$y40109$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52325 = alloca %struct.ScmObj*, align 8
%args50282$y40109$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y40109, %struct.ScmObj* %args50282$y40109$0)
store volatile %struct.ScmObj* %args50282$y40109$1, %struct.ScmObj** %stackaddr$prim52325, align 8
%stackaddr$prim52326 = alloca %struct.ScmObj*, align 8
%args50282$y40109$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40643, %struct.ScmObj* %args50282$y40109$1)
store volatile %struct.ScmObj* %args50282$y40109$2, %struct.ScmObj** %stackaddr$prim52326, align 8
%clofunc52327 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y40109)
musttail call tailcc void %clofunc52327(%struct.ScmObj* %y40109, %struct.ScmObj* %args50282$y40109$2)
ret void
}

define tailcc void @proc_clo$ae40643(%struct.ScmObj* %env$ae40643,%struct.ScmObj* %current_45args50275) {
%stackaddr$env-ref52328 = alloca %struct.ScmObj*, align 8
%k40611 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40643, i64 0)
store %struct.ScmObj* %k40611, %struct.ScmObj** %stackaddr$env-ref52328
%stackaddr$env-ref52329 = alloca %struct.ScmObj*, align 8
%args40111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40643, i64 1)
store %struct.ScmObj* %args40111, %struct.ScmObj** %stackaddr$env-ref52329
%stackaddr$env-ref52330 = alloca %struct.ScmObj*, align 8
%f40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40643, i64 2)
store %struct.ScmObj* %f40110, %struct.ScmObj** %stackaddr$env-ref52330
%stackaddr$prim52331 = alloca %struct.ScmObj*, align 8
%_95k40612 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50275)
store volatile %struct.ScmObj* %_95k40612, %struct.ScmObj** %stackaddr$prim52331, align 8
%stackaddr$prim52332 = alloca %struct.ScmObj*, align 8
%current_45args50276 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50275)
store volatile %struct.ScmObj* %current_45args50276, %struct.ScmObj** %stackaddr$prim52332, align 8
%stackaddr$prim52333 = alloca %struct.ScmObj*, align 8
%anf_45bind40252 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50276)
store volatile %struct.ScmObj* %anf_45bind40252, %struct.ScmObj** %stackaddr$prim52333, align 8
%stackaddr$makeclosure52334 = alloca %struct.ScmObj*, align 8
%fptrToInt52335 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40646 to i64
%ae40646 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt52335)
store volatile %struct.ScmObj* %ae40646, %struct.ScmObj** %stackaddr$makeclosure52334, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40646, %struct.ScmObj* %k40611, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40646, %struct.ScmObj* %args40111, i64 1)
%args50281$anf_45bind40252$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52336 = alloca %struct.ScmObj*, align 8
%args50281$anf_45bind40252$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40110, %struct.ScmObj* %args50281$anf_45bind40252$0)
store volatile %struct.ScmObj* %args50281$anf_45bind40252$1, %struct.ScmObj** %stackaddr$prim52336, align 8
%stackaddr$prim52337 = alloca %struct.ScmObj*, align 8
%args50281$anf_45bind40252$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40646, %struct.ScmObj* %args50281$anf_45bind40252$1)
store volatile %struct.ScmObj* %args50281$anf_45bind40252$2, %struct.ScmObj** %stackaddr$prim52337, align 8
%clofunc52338 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40252)
musttail call tailcc void %clofunc52338(%struct.ScmObj* %anf_45bind40252, %struct.ScmObj* %args50281$anf_45bind40252$2)
ret void
}

define tailcc void @proc_clo$ae40646(%struct.ScmObj* %env$ae40646,%struct.ScmObj* %current_45args50278) {
%stackaddr$env-ref52339 = alloca %struct.ScmObj*, align 8
%k40611 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40646, i64 0)
store %struct.ScmObj* %k40611, %struct.ScmObj** %stackaddr$env-ref52339
%stackaddr$env-ref52340 = alloca %struct.ScmObj*, align 8
%args40111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40646, i64 1)
store %struct.ScmObj* %args40111, %struct.ScmObj** %stackaddr$env-ref52340
%stackaddr$prim52341 = alloca %struct.ScmObj*, align 8
%_95k40613 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50278)
store volatile %struct.ScmObj* %_95k40613, %struct.ScmObj** %stackaddr$prim52341, align 8
%stackaddr$prim52342 = alloca %struct.ScmObj*, align 8
%current_45args50279 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50278)
store volatile %struct.ScmObj* %current_45args50279, %struct.ScmObj** %stackaddr$prim52342, align 8
%stackaddr$prim52343 = alloca %struct.ScmObj*, align 8
%anf_45bind40253 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50279)
store volatile %struct.ScmObj* %anf_45bind40253, %struct.ScmObj** %stackaddr$prim52343, align 8
%stackaddr$prim52344 = alloca %struct.ScmObj*, align 8
%cpsargs40614 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40611, %struct.ScmObj* %args40111)
store volatile %struct.ScmObj* %cpsargs40614, %struct.ScmObj** %stackaddr$prim52344, align 8
%clofunc52345 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40253)
musttail call tailcc void %clofunc52345(%struct.ScmObj* %anf_45bind40253, %struct.ScmObj* %cpsargs40614)
ret void
}

define tailcc void @proc_clo$ae40618(%struct.ScmObj* %env$ae40618,%struct.ScmObj* %current_45args50286) {
%stackaddr$prim52346 = alloca %struct.ScmObj*, align 8
%k40615 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50286)
store volatile %struct.ScmObj* %k40615, %struct.ScmObj** %stackaddr$prim52346, align 8
%stackaddr$prim52347 = alloca %struct.ScmObj*, align 8
%current_45args50287 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args50286)
store volatile %struct.ScmObj* %current_45args50287, %struct.ScmObj** %stackaddr$prim52347, align 8
%stackaddr$prim52348 = alloca %struct.ScmObj*, align 8
%yu40108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args50287)
store volatile %struct.ScmObj* %yu40108, %struct.ScmObj** %stackaddr$prim52348, align 8
%args50289$yu40108$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim52349 = alloca %struct.ScmObj*, align 8
%args50289$yu40108$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu40108, %struct.ScmObj* %args50289$yu40108$0)
store volatile %struct.ScmObj* %args50289$yu40108$1, %struct.ScmObj** %stackaddr$prim52349, align 8
%stackaddr$prim52350 = alloca %struct.ScmObj*, align 8
%args50289$yu40108$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40615, %struct.ScmObj* %args50289$yu40108$1)
store volatile %struct.ScmObj* %args50289$yu40108$2, %struct.ScmObj** %stackaddr$prim52350, align 8
%clofunc52351 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu40108)
musttail call tailcc void %clofunc52351(%struct.ScmObj* %yu40108, %struct.ScmObj* %args50289$yu40108$2)
ret void
}