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

@global$sym$ae5125856349 = private unnamed_addr constant [4 x i8] c"get\00", align 8
@global$str$ae5150956362 = private unnamed_addr constant [12 x i8] c"not promise\00", align 8
@global$sym$ae5111356482 = private unnamed_addr constant [8 x i8] c"promise\00", align 8
@global$sym$ae5097656525 = private unnamed_addr constant [8 x i8] c"promise\00", align 8

define ccc i32 @main() {
%mainenv55844 = call %struct.ScmObj* @const_init_null()
%mainargs55845 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv55844, %struct.ScmObj* %mainargs55845)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv55842,%struct.ScmObj* %mainargs55843) {
%stackaddr$makeclosure55846 = alloca %struct.ScmObj*, align 8
%fptrToInt55847 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47532 to i64
%ae47532 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55847)
store volatile %struct.ScmObj* %ae47532, %struct.ScmObj** %stackaddr$makeclosure55846, align 8
%ae47533 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55848 = alloca %struct.ScmObj*, align 8
%fptrToInt55849 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47534 to i64
%ae47534 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55849)
store volatile %struct.ScmObj* %ae47534, %struct.ScmObj** %stackaddr$makeclosure55848, align 8
%argslist55841$ae475320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55850 = alloca %struct.ScmObj*, align 8
%argslist55841$ae475321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47534, %struct.ScmObj* %argslist55841$ae475320)
store volatile %struct.ScmObj* %argslist55841$ae475321, %struct.ScmObj** %stackaddr$prim55850, align 8
%stackaddr$prim55851 = alloca %struct.ScmObj*, align 8
%argslist55841$ae475322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47533, %struct.ScmObj* %argslist55841$ae475321)
store volatile %struct.ScmObj* %argslist55841$ae475322, %struct.ScmObj** %stackaddr$prim55851, align 8
%clofunc55852 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47532)
musttail call tailcc void %clofunc55852(%struct.ScmObj* %ae47532, %struct.ScmObj* %argslist55841$ae475322)
ret void
}

define tailcc void @proc_clo$ae47532(%struct.ScmObj* %env$ae47532,%struct.ScmObj* %current_45args55218) {
%stackaddr$prim55853 = alloca %struct.ScmObj*, align 8
%_95k47346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55218)
store volatile %struct.ScmObj* %_95k47346, %struct.ScmObj** %stackaddr$prim55853, align 8
%stackaddr$prim55854 = alloca %struct.ScmObj*, align 8
%current_45args55219 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55218)
store volatile %struct.ScmObj* %current_45args55219, %struct.ScmObj** %stackaddr$prim55854, align 8
%stackaddr$prim55855 = alloca %struct.ScmObj*, align 8
%anf_45bind47207 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55219)
store volatile %struct.ScmObj* %anf_45bind47207, %struct.ScmObj** %stackaddr$prim55855, align 8
%stackaddr$makeclosure55856 = alloca %struct.ScmObj*, align 8
%fptrToInt55857 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47547 to i64
%ae47547 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55857)
store volatile %struct.ScmObj* %ae47547, %struct.ScmObj** %stackaddr$makeclosure55856, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47547, %struct.ScmObj* %anf_45bind47207, i64 0)
%ae47548 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55858 = alloca %struct.ScmObj*, align 8
%fptrToInt55859 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47549 to i64
%ae47549 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55859)
store volatile %struct.ScmObj* %ae47549, %struct.ScmObj** %stackaddr$makeclosure55858, align 8
%argslist55836$ae475470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55860 = alloca %struct.ScmObj*, align 8
%argslist55836$ae475471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47549, %struct.ScmObj* %argslist55836$ae475470)
store volatile %struct.ScmObj* %argslist55836$ae475471, %struct.ScmObj** %stackaddr$prim55860, align 8
%stackaddr$prim55861 = alloca %struct.ScmObj*, align 8
%argslist55836$ae475472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47548, %struct.ScmObj* %argslist55836$ae475471)
store volatile %struct.ScmObj* %argslist55836$ae475472, %struct.ScmObj** %stackaddr$prim55861, align 8
%clofunc55862 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47547)
musttail call tailcc void %clofunc55862(%struct.ScmObj* %ae47547, %struct.ScmObj* %argslist55836$ae475472)
ret void
}

define tailcc void @proc_clo$ae47547(%struct.ScmObj* %env$ae47547,%struct.ScmObj* %current_45args55221) {
%stackaddr$env-ref55863 = alloca %struct.ScmObj*, align 8
%anf_45bind47207 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47547, i64 0)
store %struct.ScmObj* %anf_45bind47207, %struct.ScmObj** %stackaddr$env-ref55863
%stackaddr$prim55864 = alloca %struct.ScmObj*, align 8
%_95k47347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55221)
store volatile %struct.ScmObj* %_95k47347, %struct.ScmObj** %stackaddr$prim55864, align 8
%stackaddr$prim55865 = alloca %struct.ScmObj*, align 8
%current_45args55222 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55221)
store volatile %struct.ScmObj* %current_45args55222, %struct.ScmObj** %stackaddr$prim55865, align 8
%stackaddr$prim55866 = alloca %struct.ScmObj*, align 8
%anf_45bind47211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55222)
store volatile %struct.ScmObj* %anf_45bind47211, %struct.ScmObj** %stackaddr$prim55866, align 8
%stackaddr$makeclosure55867 = alloca %struct.ScmObj*, align 8
%fptrToInt55868 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47662 to i64
%ae47662 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55868)
store volatile %struct.ScmObj* %ae47662, %struct.ScmObj** %stackaddr$makeclosure55867, align 8
%argslist55815$anf_45bind472070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55869 = alloca %struct.ScmObj*, align 8
%argslist55815$anf_45bind472071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47211, %struct.ScmObj* %argslist55815$anf_45bind472070)
store volatile %struct.ScmObj* %argslist55815$anf_45bind472071, %struct.ScmObj** %stackaddr$prim55869, align 8
%stackaddr$prim55870 = alloca %struct.ScmObj*, align 8
%argslist55815$anf_45bind472072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47662, %struct.ScmObj* %argslist55815$anf_45bind472071)
store volatile %struct.ScmObj* %argslist55815$anf_45bind472072, %struct.ScmObj** %stackaddr$prim55870, align 8
%clofunc55871 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47207)
musttail call tailcc void %clofunc55871(%struct.ScmObj* %anf_45bind47207, %struct.ScmObj* %argslist55815$anf_45bind472072)
ret void
}

define tailcc void @proc_clo$ae47662(%struct.ScmObj* %env$ae47662,%struct.ScmObj* %current_45args55224) {
%stackaddr$prim55872 = alloca %struct.ScmObj*, align 8
%_95k47348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55224)
store volatile %struct.ScmObj* %_95k47348, %struct.ScmObj** %stackaddr$prim55872, align 8
%stackaddr$prim55873 = alloca %struct.ScmObj*, align 8
%current_45args55225 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55224)
store volatile %struct.ScmObj* %current_45args55225, %struct.ScmObj** %stackaddr$prim55873, align 8
%stackaddr$prim55874 = alloca %struct.ScmObj*, align 8
%Ycmb47074 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55225)
store volatile %struct.ScmObj* %Ycmb47074, %struct.ScmObj** %stackaddr$prim55874, align 8
%stackaddr$makeclosure55875 = alloca %struct.ScmObj*, align 8
%fptrToInt55876 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47664 to i64
%ae47664 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55876)
store volatile %struct.ScmObj* %ae47664, %struct.ScmObj** %stackaddr$makeclosure55875, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47664, %struct.ScmObj* %Ycmb47074, i64 0)
%ae47665 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55877 = alloca %struct.ScmObj*, align 8
%fptrToInt55878 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47666 to i64
%ae47666 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55878)
store volatile %struct.ScmObj* %ae47666, %struct.ScmObj** %stackaddr$makeclosure55877, align 8
%argslist55814$ae476640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55879 = alloca %struct.ScmObj*, align 8
%argslist55814$ae476641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47666, %struct.ScmObj* %argslist55814$ae476640)
store volatile %struct.ScmObj* %argslist55814$ae476641, %struct.ScmObj** %stackaddr$prim55879, align 8
%stackaddr$prim55880 = alloca %struct.ScmObj*, align 8
%argslist55814$ae476642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47665, %struct.ScmObj* %argslist55814$ae476641)
store volatile %struct.ScmObj* %argslist55814$ae476642, %struct.ScmObj** %stackaddr$prim55880, align 8
%clofunc55881 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47664)
musttail call tailcc void %clofunc55881(%struct.ScmObj* %ae47664, %struct.ScmObj* %argslist55814$ae476642)
ret void
}

define tailcc void @proc_clo$ae47664(%struct.ScmObj* %env$ae47664,%struct.ScmObj* %current_45args55227) {
%stackaddr$env-ref55882 = alloca %struct.ScmObj*, align 8
%Ycmb47074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47664, i64 0)
store %struct.ScmObj* %Ycmb47074, %struct.ScmObj** %stackaddr$env-ref55882
%stackaddr$prim55883 = alloca %struct.ScmObj*, align 8
%_95k47349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55227)
store volatile %struct.ScmObj* %_95k47349, %struct.ScmObj** %stackaddr$prim55883, align 8
%stackaddr$prim55884 = alloca %struct.ScmObj*, align 8
%current_45args55228 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55227)
store volatile %struct.ScmObj* %current_45args55228, %struct.ScmObj** %stackaddr$prim55884, align 8
%stackaddr$prim55885 = alloca %struct.ScmObj*, align 8
%anf_45bind47216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55228)
store volatile %struct.ScmObj* %anf_45bind47216, %struct.ScmObj** %stackaddr$prim55885, align 8
%stackaddr$makeclosure55886 = alloca %struct.ScmObj*, align 8
%fptrToInt55887 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47742 to i64
%ae47742 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55887)
store volatile %struct.ScmObj* %ae47742, %struct.ScmObj** %stackaddr$makeclosure55886, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47742, %struct.ScmObj* %Ycmb47074, i64 0)
%argslist55798$Ycmb470740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55888 = alloca %struct.ScmObj*, align 8
%argslist55798$Ycmb470741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47216, %struct.ScmObj* %argslist55798$Ycmb470740)
store volatile %struct.ScmObj* %argslist55798$Ycmb470741, %struct.ScmObj** %stackaddr$prim55888, align 8
%stackaddr$prim55889 = alloca %struct.ScmObj*, align 8
%argslist55798$Ycmb470742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47742, %struct.ScmObj* %argslist55798$Ycmb470741)
store volatile %struct.ScmObj* %argslist55798$Ycmb470742, %struct.ScmObj** %stackaddr$prim55889, align 8
%clofunc55890 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47074)
musttail call tailcc void %clofunc55890(%struct.ScmObj* %Ycmb47074, %struct.ScmObj* %argslist55798$Ycmb470742)
ret void
}

define tailcc void @proc_clo$ae47742(%struct.ScmObj* %env$ae47742,%struct.ScmObj* %current_45args55230) {
%stackaddr$env-ref55891 = alloca %struct.ScmObj*, align 8
%Ycmb47074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47742, i64 0)
store %struct.ScmObj* %Ycmb47074, %struct.ScmObj** %stackaddr$env-ref55891
%stackaddr$prim55892 = alloca %struct.ScmObj*, align 8
%_95k47350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55230)
store volatile %struct.ScmObj* %_95k47350, %struct.ScmObj** %stackaddr$prim55892, align 8
%stackaddr$prim55893 = alloca %struct.ScmObj*, align 8
%current_45args55231 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55230)
store volatile %struct.ScmObj* %current_45args55231, %struct.ScmObj** %stackaddr$prim55893, align 8
%stackaddr$prim55894 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55231)
store volatile %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$prim55894, align 8
%stackaddr$makeclosure55895 = alloca %struct.ScmObj*, align 8
%fptrToInt55896 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47744 to i64
%ae47744 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55896)
store volatile %struct.ScmObj* %ae47744, %struct.ScmObj** %stackaddr$makeclosure55895, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47744, %struct.ScmObj* %Ycmb47074, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47744, %struct.ScmObj* %_37foldr147095, i64 1)
%ae47745 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55897 = alloca %struct.ScmObj*, align 8
%fptrToInt55898 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47746 to i64
%ae47746 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55898)
store volatile %struct.ScmObj* %ae47746, %struct.ScmObj** %stackaddr$makeclosure55897, align 8
%argslist55797$ae477440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55899 = alloca %struct.ScmObj*, align 8
%argslist55797$ae477441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47746, %struct.ScmObj* %argslist55797$ae477440)
store volatile %struct.ScmObj* %argslist55797$ae477441, %struct.ScmObj** %stackaddr$prim55899, align 8
%stackaddr$prim55900 = alloca %struct.ScmObj*, align 8
%argslist55797$ae477442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47745, %struct.ScmObj* %argslist55797$ae477441)
store volatile %struct.ScmObj* %argslist55797$ae477442, %struct.ScmObj** %stackaddr$prim55900, align 8
%clofunc55901 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47744)
musttail call tailcc void %clofunc55901(%struct.ScmObj* %ae47744, %struct.ScmObj* %argslist55797$ae477442)
ret void
}

define tailcc void @proc_clo$ae47744(%struct.ScmObj* %env$ae47744,%struct.ScmObj* %current_45args55233) {
%stackaddr$env-ref55902 = alloca %struct.ScmObj*, align 8
%Ycmb47074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47744, i64 0)
store %struct.ScmObj* %Ycmb47074, %struct.ScmObj** %stackaddr$env-ref55902
%stackaddr$env-ref55903 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47744, i64 1)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref55903
%stackaddr$prim55904 = alloca %struct.ScmObj*, align 8
%_95k47351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55233)
store volatile %struct.ScmObj* %_95k47351, %struct.ScmObj** %stackaddr$prim55904, align 8
%stackaddr$prim55905 = alloca %struct.ScmObj*, align 8
%current_45args55234 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55233)
store volatile %struct.ScmObj* %current_45args55234, %struct.ScmObj** %stackaddr$prim55905, align 8
%stackaddr$prim55906 = alloca %struct.ScmObj*, align 8
%anf_45bind47222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55234)
store volatile %struct.ScmObj* %anf_45bind47222, %struct.ScmObj** %stackaddr$prim55906, align 8
%stackaddr$makeclosure55907 = alloca %struct.ScmObj*, align 8
%fptrToInt55908 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47839 to i64
%ae47839 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55908)
store volatile %struct.ScmObj* %ae47839, %struct.ScmObj** %stackaddr$makeclosure55907, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47839, %struct.ScmObj* %Ycmb47074, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47839, %struct.ScmObj* %_37foldr147095, i64 1)
%argslist55778$Ycmb470740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55909 = alloca %struct.ScmObj*, align 8
%argslist55778$Ycmb470741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47222, %struct.ScmObj* %argslist55778$Ycmb470740)
store volatile %struct.ScmObj* %argslist55778$Ycmb470741, %struct.ScmObj** %stackaddr$prim55909, align 8
%stackaddr$prim55910 = alloca %struct.ScmObj*, align 8
%argslist55778$Ycmb470742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47839, %struct.ScmObj* %argslist55778$Ycmb470741)
store volatile %struct.ScmObj* %argslist55778$Ycmb470742, %struct.ScmObj** %stackaddr$prim55910, align 8
%clofunc55911 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47074)
musttail call tailcc void %clofunc55911(%struct.ScmObj* %Ycmb47074, %struct.ScmObj* %argslist55778$Ycmb470742)
ret void
}

define tailcc void @proc_clo$ae47839(%struct.ScmObj* %env$ae47839,%struct.ScmObj* %current_45args55236) {
%stackaddr$env-ref55912 = alloca %struct.ScmObj*, align 8
%Ycmb47074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47839, i64 0)
store %struct.ScmObj* %Ycmb47074, %struct.ScmObj** %stackaddr$env-ref55912
%stackaddr$env-ref55913 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47839, i64 1)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref55913
%stackaddr$prim55914 = alloca %struct.ScmObj*, align 8
%_95k47352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55236)
store volatile %struct.ScmObj* %_95k47352, %struct.ScmObj** %stackaddr$prim55914, align 8
%stackaddr$prim55915 = alloca %struct.ScmObj*, align 8
%current_45args55237 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55236)
store volatile %struct.ScmObj* %current_45args55237, %struct.ScmObj** %stackaddr$prim55915, align 8
%stackaddr$prim55916 = alloca %struct.ScmObj*, align 8
%_37map147091 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55237)
store volatile %struct.ScmObj* %_37map147091, %struct.ScmObj** %stackaddr$prim55916, align 8
%stackaddr$makeclosure55917 = alloca %struct.ScmObj*, align 8
%fptrToInt55918 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47841 to i64
%ae47841 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55918)
store volatile %struct.ScmObj* %ae47841, %struct.ScmObj** %stackaddr$makeclosure55917, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47841, %struct.ScmObj* %_37map147091, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47841, %struct.ScmObj* %Ycmb47074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47841, %struct.ScmObj* %_37foldr147095, i64 2)
%ae47842 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55919 = alloca %struct.ScmObj*, align 8
%fptrToInt55920 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47843 to i64
%ae47843 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55920)
store volatile %struct.ScmObj* %ae47843, %struct.ScmObj** %stackaddr$makeclosure55919, align 8
%argslist55777$ae478410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55921 = alloca %struct.ScmObj*, align 8
%argslist55777$ae478411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47843, %struct.ScmObj* %argslist55777$ae478410)
store volatile %struct.ScmObj* %argslist55777$ae478411, %struct.ScmObj** %stackaddr$prim55921, align 8
%stackaddr$prim55922 = alloca %struct.ScmObj*, align 8
%argslist55777$ae478412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47842, %struct.ScmObj* %argslist55777$ae478411)
store volatile %struct.ScmObj* %argslist55777$ae478412, %struct.ScmObj** %stackaddr$prim55922, align 8
%clofunc55923 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47841)
musttail call tailcc void %clofunc55923(%struct.ScmObj* %ae47841, %struct.ScmObj* %argslist55777$ae478412)
ret void
}

define tailcc void @proc_clo$ae47841(%struct.ScmObj* %env$ae47841,%struct.ScmObj* %current_45args55239) {
%stackaddr$env-ref55924 = alloca %struct.ScmObj*, align 8
%_37map147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47841, i64 0)
store %struct.ScmObj* %_37map147091, %struct.ScmObj** %stackaddr$env-ref55924
%stackaddr$env-ref55925 = alloca %struct.ScmObj*, align 8
%Ycmb47074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47841, i64 1)
store %struct.ScmObj* %Ycmb47074, %struct.ScmObj** %stackaddr$env-ref55925
%stackaddr$env-ref55926 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47841, i64 2)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref55926
%stackaddr$prim55927 = alloca %struct.ScmObj*, align 8
%_95k47353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55239)
store volatile %struct.ScmObj* %_95k47353, %struct.ScmObj** %stackaddr$prim55927, align 8
%stackaddr$prim55928 = alloca %struct.ScmObj*, align 8
%current_45args55240 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55239)
store volatile %struct.ScmObj* %current_45args55240, %struct.ScmObj** %stackaddr$prim55928, align 8
%stackaddr$prim55929 = alloca %struct.ScmObj*, align 8
%anf_45bind47229 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55240)
store volatile %struct.ScmObj* %anf_45bind47229, %struct.ScmObj** %stackaddr$prim55929, align 8
%stackaddr$makeclosure55930 = alloca %struct.ScmObj*, align 8
%fptrToInt55931 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47989 to i64
%ae47989 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55931)
store volatile %struct.ScmObj* %ae47989, %struct.ScmObj** %stackaddr$makeclosure55930, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47989, %struct.ScmObj* %_37map147091, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47989, %struct.ScmObj* %Ycmb47074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47989, %struct.ScmObj* %_37foldr147095, i64 2)
%argslist55761$Ycmb470740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55932 = alloca %struct.ScmObj*, align 8
%argslist55761$Ycmb470741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47229, %struct.ScmObj* %argslist55761$Ycmb470740)
store volatile %struct.ScmObj* %argslist55761$Ycmb470741, %struct.ScmObj** %stackaddr$prim55932, align 8
%stackaddr$prim55933 = alloca %struct.ScmObj*, align 8
%argslist55761$Ycmb470742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47989, %struct.ScmObj* %argslist55761$Ycmb470741)
store volatile %struct.ScmObj* %argslist55761$Ycmb470742, %struct.ScmObj** %stackaddr$prim55933, align 8
%clofunc55934 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47074)
musttail call tailcc void %clofunc55934(%struct.ScmObj* %Ycmb47074, %struct.ScmObj* %argslist55761$Ycmb470742)
ret void
}

define tailcc void @proc_clo$ae47989(%struct.ScmObj* %env$ae47989,%struct.ScmObj* %current_45args55242) {
%stackaddr$env-ref55935 = alloca %struct.ScmObj*, align 8
%_37map147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47989, i64 0)
store %struct.ScmObj* %_37map147091, %struct.ScmObj** %stackaddr$env-ref55935
%stackaddr$env-ref55936 = alloca %struct.ScmObj*, align 8
%Ycmb47074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47989, i64 1)
store %struct.ScmObj* %Ycmb47074, %struct.ScmObj** %stackaddr$env-ref55936
%stackaddr$env-ref55937 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47989, i64 2)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref55937
%stackaddr$prim55938 = alloca %struct.ScmObj*, align 8
%_95k47354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55242)
store volatile %struct.ScmObj* %_95k47354, %struct.ScmObj** %stackaddr$prim55938, align 8
%stackaddr$prim55939 = alloca %struct.ScmObj*, align 8
%current_45args55243 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55242)
store volatile %struct.ScmObj* %current_45args55243, %struct.ScmObj** %stackaddr$prim55939, align 8
%stackaddr$prim55940 = alloca %struct.ScmObj*, align 8
%_37take47087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55243)
store volatile %struct.ScmObj* %_37take47087, %struct.ScmObj** %stackaddr$prim55940, align 8
%stackaddr$makeclosure55941 = alloca %struct.ScmObj*, align 8
%fptrToInt55942 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47991 to i64
%ae47991 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55942)
store volatile %struct.ScmObj* %ae47991, %struct.ScmObj** %stackaddr$makeclosure55941, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47991, %struct.ScmObj* %_37map147091, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47991, %struct.ScmObj* %Ycmb47074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47991, %struct.ScmObj* %_37take47087, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47991, %struct.ScmObj* %_37foldr147095, i64 3)
%ae47992 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55943 = alloca %struct.ScmObj*, align 8
%fptrToInt55944 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47993 to i64
%ae47993 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55944)
store volatile %struct.ScmObj* %ae47993, %struct.ScmObj** %stackaddr$makeclosure55943, align 8
%argslist55760$ae479910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55945 = alloca %struct.ScmObj*, align 8
%argslist55760$ae479911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47993, %struct.ScmObj* %argslist55760$ae479910)
store volatile %struct.ScmObj* %argslist55760$ae479911, %struct.ScmObj** %stackaddr$prim55945, align 8
%stackaddr$prim55946 = alloca %struct.ScmObj*, align 8
%argslist55760$ae479912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47992, %struct.ScmObj* %argslist55760$ae479911)
store volatile %struct.ScmObj* %argslist55760$ae479912, %struct.ScmObj** %stackaddr$prim55946, align 8
%clofunc55947 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47991)
musttail call tailcc void %clofunc55947(%struct.ScmObj* %ae47991, %struct.ScmObj* %argslist55760$ae479912)
ret void
}

define tailcc void @proc_clo$ae47991(%struct.ScmObj* %env$ae47991,%struct.ScmObj* %current_45args55245) {
%stackaddr$env-ref55948 = alloca %struct.ScmObj*, align 8
%_37map147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47991, i64 0)
store %struct.ScmObj* %_37map147091, %struct.ScmObj** %stackaddr$env-ref55948
%stackaddr$env-ref55949 = alloca %struct.ScmObj*, align 8
%Ycmb47074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47991, i64 1)
store %struct.ScmObj* %Ycmb47074, %struct.ScmObj** %stackaddr$env-ref55949
%stackaddr$env-ref55950 = alloca %struct.ScmObj*, align 8
%_37take47087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47991, i64 2)
store %struct.ScmObj* %_37take47087, %struct.ScmObj** %stackaddr$env-ref55950
%stackaddr$env-ref55951 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47991, i64 3)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref55951
%stackaddr$prim55952 = alloca %struct.ScmObj*, align 8
%_95k47355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55245)
store volatile %struct.ScmObj* %_95k47355, %struct.ScmObj** %stackaddr$prim55952, align 8
%stackaddr$prim55953 = alloca %struct.ScmObj*, align 8
%current_45args55246 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55245)
store volatile %struct.ScmObj* %current_45args55246, %struct.ScmObj** %stackaddr$prim55953, align 8
%stackaddr$prim55954 = alloca %struct.ScmObj*, align 8
%anf_45bind47233 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55246)
store volatile %struct.ScmObj* %anf_45bind47233, %struct.ScmObj** %stackaddr$prim55954, align 8
%stackaddr$makeclosure55955 = alloca %struct.ScmObj*, align 8
%fptrToInt55956 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48072 to i64
%ae48072 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55956)
store volatile %struct.ScmObj* %ae48072, %struct.ScmObj** %stackaddr$makeclosure55955, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48072, %struct.ScmObj* %_37map147091, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48072, %struct.ScmObj* %Ycmb47074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48072, %struct.ScmObj* %_37take47087, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48072, %struct.ScmObj* %_37foldr147095, i64 3)
%argslist55746$Ycmb470740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55957 = alloca %struct.ScmObj*, align 8
%argslist55746$Ycmb470741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47233, %struct.ScmObj* %argslist55746$Ycmb470740)
store volatile %struct.ScmObj* %argslist55746$Ycmb470741, %struct.ScmObj** %stackaddr$prim55957, align 8
%stackaddr$prim55958 = alloca %struct.ScmObj*, align 8
%argslist55746$Ycmb470742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48072, %struct.ScmObj* %argslist55746$Ycmb470741)
store volatile %struct.ScmObj* %argslist55746$Ycmb470742, %struct.ScmObj** %stackaddr$prim55958, align 8
%clofunc55959 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47074)
musttail call tailcc void %clofunc55959(%struct.ScmObj* %Ycmb47074, %struct.ScmObj* %argslist55746$Ycmb470742)
ret void
}

define tailcc void @proc_clo$ae48072(%struct.ScmObj* %env$ae48072,%struct.ScmObj* %current_45args55248) {
%stackaddr$env-ref55960 = alloca %struct.ScmObj*, align 8
%_37map147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48072, i64 0)
store %struct.ScmObj* %_37map147091, %struct.ScmObj** %stackaddr$env-ref55960
%stackaddr$env-ref55961 = alloca %struct.ScmObj*, align 8
%Ycmb47074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48072, i64 1)
store %struct.ScmObj* %Ycmb47074, %struct.ScmObj** %stackaddr$env-ref55961
%stackaddr$env-ref55962 = alloca %struct.ScmObj*, align 8
%_37take47087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48072, i64 2)
store %struct.ScmObj* %_37take47087, %struct.ScmObj** %stackaddr$env-ref55962
%stackaddr$env-ref55963 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48072, i64 3)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref55963
%stackaddr$prim55964 = alloca %struct.ScmObj*, align 8
%_95k47356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55248)
store volatile %struct.ScmObj* %_95k47356, %struct.ScmObj** %stackaddr$prim55964, align 8
%stackaddr$prim55965 = alloca %struct.ScmObj*, align 8
%current_45args55249 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55248)
store volatile %struct.ScmObj* %current_45args55249, %struct.ScmObj** %stackaddr$prim55965, align 8
%stackaddr$prim55966 = alloca %struct.ScmObj*, align 8
%_37length47084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55249)
store volatile %struct.ScmObj* %_37length47084, %struct.ScmObj** %stackaddr$prim55966, align 8
%stackaddr$makeclosure55967 = alloca %struct.ScmObj*, align 8
%fptrToInt55968 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48074 to i64
%ae48074 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55968)
store volatile %struct.ScmObj* %ae48074, %struct.ScmObj** %stackaddr$makeclosure55967, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48074, %struct.ScmObj* %_37map147091, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48074, %struct.ScmObj* %Ycmb47074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48074, %struct.ScmObj* %_37take47087, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48074, %struct.ScmObj* %_37length47084, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48074, %struct.ScmObj* %_37foldr147095, i64 4)
%ae48075 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55969 = alloca %struct.ScmObj*, align 8
%fptrToInt55970 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48076 to i64
%ae48076 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55970)
store volatile %struct.ScmObj* %ae48076, %struct.ScmObj** %stackaddr$makeclosure55969, align 8
%argslist55745$ae480740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55971 = alloca %struct.ScmObj*, align 8
%argslist55745$ae480741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48076, %struct.ScmObj* %argslist55745$ae480740)
store volatile %struct.ScmObj* %argslist55745$ae480741, %struct.ScmObj** %stackaddr$prim55971, align 8
%stackaddr$prim55972 = alloca %struct.ScmObj*, align 8
%argslist55745$ae480742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48075, %struct.ScmObj* %argslist55745$ae480741)
store volatile %struct.ScmObj* %argslist55745$ae480742, %struct.ScmObj** %stackaddr$prim55972, align 8
%clofunc55973 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48074)
musttail call tailcc void %clofunc55973(%struct.ScmObj* %ae48074, %struct.ScmObj* %argslist55745$ae480742)
ret void
}

define tailcc void @proc_clo$ae48074(%struct.ScmObj* %env$ae48074,%struct.ScmObj* %current_45args55251) {
%stackaddr$env-ref55974 = alloca %struct.ScmObj*, align 8
%_37map147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48074, i64 0)
store %struct.ScmObj* %_37map147091, %struct.ScmObj** %stackaddr$env-ref55974
%stackaddr$env-ref55975 = alloca %struct.ScmObj*, align 8
%Ycmb47074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48074, i64 1)
store %struct.ScmObj* %Ycmb47074, %struct.ScmObj** %stackaddr$env-ref55975
%stackaddr$env-ref55976 = alloca %struct.ScmObj*, align 8
%_37take47087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48074, i64 2)
store %struct.ScmObj* %_37take47087, %struct.ScmObj** %stackaddr$env-ref55976
%stackaddr$env-ref55977 = alloca %struct.ScmObj*, align 8
%_37length47084 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48074, i64 3)
store %struct.ScmObj* %_37length47084, %struct.ScmObj** %stackaddr$env-ref55977
%stackaddr$env-ref55978 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48074, i64 4)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref55978
%stackaddr$prim55979 = alloca %struct.ScmObj*, align 8
%_95k47357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55251)
store volatile %struct.ScmObj* %_95k47357, %struct.ScmObj** %stackaddr$prim55979, align 8
%stackaddr$prim55980 = alloca %struct.ScmObj*, align 8
%current_45args55252 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55251)
store volatile %struct.ScmObj* %current_45args55252, %struct.ScmObj** %stackaddr$prim55980, align 8
%stackaddr$prim55981 = alloca %struct.ScmObj*, align 8
%anf_45bind47238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55252)
store volatile %struct.ScmObj* %anf_45bind47238, %struct.ScmObj** %stackaddr$prim55981, align 8
%stackaddr$makeclosure55982 = alloca %struct.ScmObj*, align 8
%fptrToInt55983 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48151 to i64
%ae48151 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55983)
store volatile %struct.ScmObj* %ae48151, %struct.ScmObj** %stackaddr$makeclosure55982, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48151, %struct.ScmObj* %_37map147091, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48151, %struct.ScmObj* %Ycmb47074, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48151, %struct.ScmObj* %_37take47087, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48151, %struct.ScmObj* %_37length47084, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48151, %struct.ScmObj* %_37foldr147095, i64 4)
%argslist55729$Ycmb470740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55984 = alloca %struct.ScmObj*, align 8
%argslist55729$Ycmb470741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47238, %struct.ScmObj* %argslist55729$Ycmb470740)
store volatile %struct.ScmObj* %argslist55729$Ycmb470741, %struct.ScmObj** %stackaddr$prim55984, align 8
%stackaddr$prim55985 = alloca %struct.ScmObj*, align 8
%argslist55729$Ycmb470742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48151, %struct.ScmObj* %argslist55729$Ycmb470741)
store volatile %struct.ScmObj* %argslist55729$Ycmb470742, %struct.ScmObj** %stackaddr$prim55985, align 8
%clofunc55986 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47074)
musttail call tailcc void %clofunc55986(%struct.ScmObj* %Ycmb47074, %struct.ScmObj* %argslist55729$Ycmb470742)
ret void
}

define tailcc void @proc_clo$ae48151(%struct.ScmObj* %env$ae48151,%struct.ScmObj* %current_45args55254) {
%stackaddr$env-ref55987 = alloca %struct.ScmObj*, align 8
%_37map147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48151, i64 0)
store %struct.ScmObj* %_37map147091, %struct.ScmObj** %stackaddr$env-ref55987
%stackaddr$env-ref55988 = alloca %struct.ScmObj*, align 8
%Ycmb47074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48151, i64 1)
store %struct.ScmObj* %Ycmb47074, %struct.ScmObj** %stackaddr$env-ref55988
%stackaddr$env-ref55989 = alloca %struct.ScmObj*, align 8
%_37take47087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48151, i64 2)
store %struct.ScmObj* %_37take47087, %struct.ScmObj** %stackaddr$env-ref55989
%stackaddr$env-ref55990 = alloca %struct.ScmObj*, align 8
%_37length47084 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48151, i64 3)
store %struct.ScmObj* %_37length47084, %struct.ScmObj** %stackaddr$env-ref55990
%stackaddr$env-ref55991 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48151, i64 4)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref55991
%stackaddr$prim55992 = alloca %struct.ScmObj*, align 8
%_95k47358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55254)
store volatile %struct.ScmObj* %_95k47358, %struct.ScmObj** %stackaddr$prim55992, align 8
%stackaddr$prim55993 = alloca %struct.ScmObj*, align 8
%current_45args55255 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55254)
store volatile %struct.ScmObj* %current_45args55255, %struct.ScmObj** %stackaddr$prim55993, align 8
%stackaddr$prim55994 = alloca %struct.ScmObj*, align 8
%_37foldl147079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55255)
store volatile %struct.ScmObj* %_37foldl147079, %struct.ScmObj** %stackaddr$prim55994, align 8
%stackaddr$makeclosure55995 = alloca %struct.ScmObj*, align 8
%fptrToInt55996 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48153 to i64
%ae48153 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55996)
store volatile %struct.ScmObj* %ae48153, %struct.ScmObj** %stackaddr$makeclosure55995, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48153, %struct.ScmObj* %_37foldr147095, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48153, %struct.ScmObj* %_37foldl147079, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48153, %struct.ScmObj* %_37map147091, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48153, %struct.ScmObj* %Ycmb47074, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48153, %struct.ScmObj* %_37take47087, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48153, %struct.ScmObj* %_37length47084, i64 5)
%ae48154 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55997 = alloca %struct.ScmObj*, align 8
%fptrToInt55998 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48155 to i64
%ae48155 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55998)
store volatile %struct.ScmObj* %ae48155, %struct.ScmObj** %stackaddr$makeclosure55997, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48155, %struct.ScmObj* %_37foldl147079, i64 0)
%argslist55728$ae481530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55999 = alloca %struct.ScmObj*, align 8
%argslist55728$ae481531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48155, %struct.ScmObj* %argslist55728$ae481530)
store volatile %struct.ScmObj* %argslist55728$ae481531, %struct.ScmObj** %stackaddr$prim55999, align 8
%stackaddr$prim56000 = alloca %struct.ScmObj*, align 8
%argslist55728$ae481532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48154, %struct.ScmObj* %argslist55728$ae481531)
store volatile %struct.ScmObj* %argslist55728$ae481532, %struct.ScmObj** %stackaddr$prim56000, align 8
%clofunc56001 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48153)
musttail call tailcc void %clofunc56001(%struct.ScmObj* %ae48153, %struct.ScmObj* %argslist55728$ae481532)
ret void
}

define tailcc void @proc_clo$ae48153(%struct.ScmObj* %env$ae48153,%struct.ScmObj* %current_45args55257) {
%stackaddr$env-ref56002 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48153, i64 0)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref56002
%stackaddr$env-ref56003 = alloca %struct.ScmObj*, align 8
%_37foldl147079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48153, i64 1)
store %struct.ScmObj* %_37foldl147079, %struct.ScmObj** %stackaddr$env-ref56003
%stackaddr$env-ref56004 = alloca %struct.ScmObj*, align 8
%_37map147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48153, i64 2)
store %struct.ScmObj* %_37map147091, %struct.ScmObj** %stackaddr$env-ref56004
%stackaddr$env-ref56005 = alloca %struct.ScmObj*, align 8
%Ycmb47074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48153, i64 3)
store %struct.ScmObj* %Ycmb47074, %struct.ScmObj** %stackaddr$env-ref56005
%stackaddr$env-ref56006 = alloca %struct.ScmObj*, align 8
%_37take47087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48153, i64 4)
store %struct.ScmObj* %_37take47087, %struct.ScmObj** %stackaddr$env-ref56006
%stackaddr$env-ref56007 = alloca %struct.ScmObj*, align 8
%_37length47084 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48153, i64 5)
store %struct.ScmObj* %_37length47084, %struct.ScmObj** %stackaddr$env-ref56007
%stackaddr$prim56008 = alloca %struct.ScmObj*, align 8
%_95k47359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55257)
store volatile %struct.ScmObj* %_95k47359, %struct.ScmObj** %stackaddr$prim56008, align 8
%stackaddr$prim56009 = alloca %struct.ScmObj*, align 8
%current_45args55258 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55257)
store volatile %struct.ScmObj* %current_45args55258, %struct.ScmObj** %stackaddr$prim56009, align 8
%stackaddr$prim56010 = alloca %struct.ScmObj*, align 8
%_37last47117 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55258)
store volatile %struct.ScmObj* %_37last47117, %struct.ScmObj** %stackaddr$prim56010, align 8
%stackaddr$makeclosure56011 = alloca %struct.ScmObj*, align 8
%fptrToInt56012 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48207 to i64
%ae48207 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56012)
store volatile %struct.ScmObj* %ae48207, %struct.ScmObj** %stackaddr$makeclosure56011, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48207, %struct.ScmObj* %_37foldr147095, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48207, %struct.ScmObj* %_37foldl147079, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48207, %struct.ScmObj* %_37map147091, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48207, %struct.ScmObj* %Ycmb47074, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48207, %struct.ScmObj* %_37last47117, i64 4)
%ae48208 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56013 = alloca %struct.ScmObj*, align 8
%fptrToInt56014 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48209 to i64
%ae48209 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56014)
store volatile %struct.ScmObj* %ae48209, %struct.ScmObj** %stackaddr$makeclosure56013, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48209, %struct.ScmObj* %_37take47087, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48209, %struct.ScmObj* %_37length47084, i64 1)
%argslist55714$ae482070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56015 = alloca %struct.ScmObj*, align 8
%argslist55714$ae482071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48209, %struct.ScmObj* %argslist55714$ae482070)
store volatile %struct.ScmObj* %argslist55714$ae482071, %struct.ScmObj** %stackaddr$prim56015, align 8
%stackaddr$prim56016 = alloca %struct.ScmObj*, align 8
%argslist55714$ae482072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48208, %struct.ScmObj* %argslist55714$ae482071)
store volatile %struct.ScmObj* %argslist55714$ae482072, %struct.ScmObj** %stackaddr$prim56016, align 8
%clofunc56017 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48207)
musttail call tailcc void %clofunc56017(%struct.ScmObj* %ae48207, %struct.ScmObj* %argslist55714$ae482072)
ret void
}

define tailcc void @proc_clo$ae48207(%struct.ScmObj* %env$ae48207,%struct.ScmObj* %current_45args55260) {
%stackaddr$env-ref56018 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48207, i64 0)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref56018
%stackaddr$env-ref56019 = alloca %struct.ScmObj*, align 8
%_37foldl147079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48207, i64 1)
store %struct.ScmObj* %_37foldl147079, %struct.ScmObj** %stackaddr$env-ref56019
%stackaddr$env-ref56020 = alloca %struct.ScmObj*, align 8
%_37map147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48207, i64 2)
store %struct.ScmObj* %_37map147091, %struct.ScmObj** %stackaddr$env-ref56020
%stackaddr$env-ref56021 = alloca %struct.ScmObj*, align 8
%Ycmb47074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48207, i64 3)
store %struct.ScmObj* %Ycmb47074, %struct.ScmObj** %stackaddr$env-ref56021
%stackaddr$env-ref56022 = alloca %struct.ScmObj*, align 8
%_37last47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48207, i64 4)
store %struct.ScmObj* %_37last47117, %struct.ScmObj** %stackaddr$env-ref56022
%stackaddr$prim56023 = alloca %struct.ScmObj*, align 8
%_95k47360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55260)
store volatile %struct.ScmObj* %_95k47360, %struct.ScmObj** %stackaddr$prim56023, align 8
%stackaddr$prim56024 = alloca %struct.ScmObj*, align 8
%current_45args55261 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55260)
store volatile %struct.ScmObj* %current_45args55261, %struct.ScmObj** %stackaddr$prim56024, align 8
%stackaddr$prim56025 = alloca %struct.ScmObj*, align 8
%_37drop_45right47114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55261)
store volatile %struct.ScmObj* %_37drop_45right47114, %struct.ScmObj** %stackaddr$prim56025, align 8
%stackaddr$makeclosure56026 = alloca %struct.ScmObj*, align 8
%fptrToInt56027 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48237 to i64
%ae48237 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56027)
store volatile %struct.ScmObj* %ae48237, %struct.ScmObj** %stackaddr$makeclosure56026, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48237, %struct.ScmObj* %_37foldr147095, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48237, %struct.ScmObj* %_37foldl147079, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48237, %struct.ScmObj* %Ycmb47074, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48237, %struct.ScmObj* %_37last47117, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48237, %struct.ScmObj* %_37drop_45right47114, i64 4)
%ae48238 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56028 = alloca %struct.ScmObj*, align 8
%fptrToInt56029 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48239 to i64
%ae48239 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56029)
store volatile %struct.ScmObj* %ae48239, %struct.ScmObj** %stackaddr$makeclosure56028, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48239, %struct.ScmObj* %_37map147091, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48239, %struct.ScmObj* %_37foldr147095, i64 1)
%argslist55704$ae482370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56030 = alloca %struct.ScmObj*, align 8
%argslist55704$ae482371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48239, %struct.ScmObj* %argslist55704$ae482370)
store volatile %struct.ScmObj* %argslist55704$ae482371, %struct.ScmObj** %stackaddr$prim56030, align 8
%stackaddr$prim56031 = alloca %struct.ScmObj*, align 8
%argslist55704$ae482372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48238, %struct.ScmObj* %argslist55704$ae482371)
store volatile %struct.ScmObj* %argslist55704$ae482372, %struct.ScmObj** %stackaddr$prim56031, align 8
%clofunc56032 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48237)
musttail call tailcc void %clofunc56032(%struct.ScmObj* %ae48237, %struct.ScmObj* %argslist55704$ae482372)
ret void
}

define tailcc void @proc_clo$ae48237(%struct.ScmObj* %env$ae48237,%struct.ScmObj* %current_45args55263) {
%stackaddr$env-ref56033 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48237, i64 0)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref56033
%stackaddr$env-ref56034 = alloca %struct.ScmObj*, align 8
%_37foldl147079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48237, i64 1)
store %struct.ScmObj* %_37foldl147079, %struct.ScmObj** %stackaddr$env-ref56034
%stackaddr$env-ref56035 = alloca %struct.ScmObj*, align 8
%Ycmb47074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48237, i64 2)
store %struct.ScmObj* %Ycmb47074, %struct.ScmObj** %stackaddr$env-ref56035
%stackaddr$env-ref56036 = alloca %struct.ScmObj*, align 8
%_37last47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48237, i64 3)
store %struct.ScmObj* %_37last47117, %struct.ScmObj** %stackaddr$env-ref56036
%stackaddr$env-ref56037 = alloca %struct.ScmObj*, align 8
%_37drop_45right47114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48237, i64 4)
store %struct.ScmObj* %_37drop_45right47114, %struct.ScmObj** %stackaddr$env-ref56037
%stackaddr$prim56038 = alloca %struct.ScmObj*, align 8
%_95k47361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55263)
store volatile %struct.ScmObj* %_95k47361, %struct.ScmObj** %stackaddr$prim56038, align 8
%stackaddr$prim56039 = alloca %struct.ScmObj*, align 8
%current_45args55264 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55263)
store volatile %struct.ScmObj* %current_45args55264, %struct.ScmObj** %stackaddr$prim56039, align 8
%stackaddr$prim56040 = alloca %struct.ScmObj*, align 8
%anf_45bind47254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55264)
store volatile %struct.ScmObj* %anf_45bind47254, %struct.ScmObj** %stackaddr$prim56040, align 8
%stackaddr$makeclosure56041 = alloca %struct.ScmObj*, align 8
%fptrToInt56042 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48621 to i64
%ae48621 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56042)
store volatile %struct.ScmObj* %ae48621, %struct.ScmObj** %stackaddr$makeclosure56041, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48621, %struct.ScmObj* %_37foldr147095, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48621, %struct.ScmObj* %_37foldl147079, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48621, %struct.ScmObj* %Ycmb47074, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48621, %struct.ScmObj* %_37last47117, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48621, %struct.ScmObj* %_37drop_45right47114, i64 4)
%argslist55644$Ycmb470740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56043 = alloca %struct.ScmObj*, align 8
%argslist55644$Ycmb470741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47254, %struct.ScmObj* %argslist55644$Ycmb470740)
store volatile %struct.ScmObj* %argslist55644$Ycmb470741, %struct.ScmObj** %stackaddr$prim56043, align 8
%stackaddr$prim56044 = alloca %struct.ScmObj*, align 8
%argslist55644$Ycmb470742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48621, %struct.ScmObj* %argslist55644$Ycmb470741)
store volatile %struct.ScmObj* %argslist55644$Ycmb470742, %struct.ScmObj** %stackaddr$prim56044, align 8
%clofunc56045 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47074)
musttail call tailcc void %clofunc56045(%struct.ScmObj* %Ycmb47074, %struct.ScmObj* %argslist55644$Ycmb470742)
ret void
}

define tailcc void @proc_clo$ae48621(%struct.ScmObj* %env$ae48621,%struct.ScmObj* %current_45args55266) {
%stackaddr$env-ref56046 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48621, i64 0)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref56046
%stackaddr$env-ref56047 = alloca %struct.ScmObj*, align 8
%_37foldl147079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48621, i64 1)
store %struct.ScmObj* %_37foldl147079, %struct.ScmObj** %stackaddr$env-ref56047
%stackaddr$env-ref56048 = alloca %struct.ScmObj*, align 8
%Ycmb47074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48621, i64 2)
store %struct.ScmObj* %Ycmb47074, %struct.ScmObj** %stackaddr$env-ref56048
%stackaddr$env-ref56049 = alloca %struct.ScmObj*, align 8
%_37last47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48621, i64 3)
store %struct.ScmObj* %_37last47117, %struct.ScmObj** %stackaddr$env-ref56049
%stackaddr$env-ref56050 = alloca %struct.ScmObj*, align 8
%_37drop_45right47114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48621, i64 4)
store %struct.ScmObj* %_37drop_45right47114, %struct.ScmObj** %stackaddr$env-ref56050
%stackaddr$prim56051 = alloca %struct.ScmObj*, align 8
%_95k47362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55266)
store volatile %struct.ScmObj* %_95k47362, %struct.ScmObj** %stackaddr$prim56051, align 8
%stackaddr$prim56052 = alloca %struct.ScmObj*, align 8
%current_45args55267 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55266)
store volatile %struct.ScmObj* %current_45args55267, %struct.ScmObj** %stackaddr$prim56052, align 8
%stackaddr$prim56053 = alloca %struct.ScmObj*, align 8
%_37foldr47100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55267)
store volatile %struct.ScmObj* %_37foldr47100, %struct.ScmObj** %stackaddr$prim56053, align 8
%stackaddr$makeclosure56054 = alloca %struct.ScmObj*, align 8
%fptrToInt56055 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48623 to i64
%ae48623 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt56055)
store volatile %struct.ScmObj* %ae48623, %struct.ScmObj** %stackaddr$makeclosure56054, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48623, %struct.ScmObj* %_37foldr147095, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48623, %struct.ScmObj* %_37foldl147079, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48623, %struct.ScmObj* %Ycmb47074, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48623, %struct.ScmObj* %_37last47117, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48623, %struct.ScmObj* %_37foldr47100, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48623, %struct.ScmObj* %_37drop_45right47114, i64 5)
%ae48624 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56056 = alloca %struct.ScmObj*, align 8
%fptrToInt56057 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48625 to i64
%ae48625 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56057)
store volatile %struct.ScmObj* %ae48625, %struct.ScmObj** %stackaddr$makeclosure56056, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48625, %struct.ScmObj* %_37foldr147095, i64 0)
%argslist55643$ae486230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56058 = alloca %struct.ScmObj*, align 8
%argslist55643$ae486231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48625, %struct.ScmObj* %argslist55643$ae486230)
store volatile %struct.ScmObj* %argslist55643$ae486231, %struct.ScmObj** %stackaddr$prim56058, align 8
%stackaddr$prim56059 = alloca %struct.ScmObj*, align 8
%argslist55643$ae486232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48624, %struct.ScmObj* %argslist55643$ae486231)
store volatile %struct.ScmObj* %argslist55643$ae486232, %struct.ScmObj** %stackaddr$prim56059, align 8
%clofunc56060 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48623)
musttail call tailcc void %clofunc56060(%struct.ScmObj* %ae48623, %struct.ScmObj* %argslist55643$ae486232)
ret void
}

define tailcc void @proc_clo$ae48623(%struct.ScmObj* %env$ae48623,%struct.ScmObj* %current_45args55269) {
%stackaddr$env-ref56061 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48623, i64 0)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref56061
%stackaddr$env-ref56062 = alloca %struct.ScmObj*, align 8
%_37foldl147079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48623, i64 1)
store %struct.ScmObj* %_37foldl147079, %struct.ScmObj** %stackaddr$env-ref56062
%stackaddr$env-ref56063 = alloca %struct.ScmObj*, align 8
%Ycmb47074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48623, i64 2)
store %struct.ScmObj* %Ycmb47074, %struct.ScmObj** %stackaddr$env-ref56063
%stackaddr$env-ref56064 = alloca %struct.ScmObj*, align 8
%_37last47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48623, i64 3)
store %struct.ScmObj* %_37last47117, %struct.ScmObj** %stackaddr$env-ref56064
%stackaddr$env-ref56065 = alloca %struct.ScmObj*, align 8
%_37foldr47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48623, i64 4)
store %struct.ScmObj* %_37foldr47100, %struct.ScmObj** %stackaddr$env-ref56065
%stackaddr$env-ref56066 = alloca %struct.ScmObj*, align 8
%_37drop_45right47114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48623, i64 5)
store %struct.ScmObj* %_37drop_45right47114, %struct.ScmObj** %stackaddr$env-ref56066
%stackaddr$prim56067 = alloca %struct.ScmObj*, align 8
%_95k47363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55269)
store volatile %struct.ScmObj* %_95k47363, %struct.ScmObj** %stackaddr$prim56067, align 8
%stackaddr$prim56068 = alloca %struct.ScmObj*, align 8
%current_45args55270 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55269)
store volatile %struct.ScmObj* %current_45args55270, %struct.ScmObj** %stackaddr$prim56068, align 8
%stackaddr$prim56069 = alloca %struct.ScmObj*, align 8
%_37map147126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55270)
store volatile %struct.ScmObj* %_37map147126, %struct.ScmObj** %stackaddr$prim56069, align 8
%stackaddr$makeclosure56070 = alloca %struct.ScmObj*, align 8
%fptrToInt56071 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48700 to i64
%ae48700 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56071)
store volatile %struct.ScmObj* %ae48700, %struct.ScmObj** %stackaddr$makeclosure56070, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48700, %struct.ScmObj* %_37foldr147095, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48700, %struct.ScmObj* %_37foldl147079, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48700, %struct.ScmObj* %_37map147126, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48700, %struct.ScmObj* %Ycmb47074, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48700, %struct.ScmObj* %_37foldr47100, i64 4)
%ae48701 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56072 = alloca %struct.ScmObj*, align 8
%fptrToInt56073 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48702 to i64
%ae48702 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56073)
store volatile %struct.ScmObj* %ae48702, %struct.ScmObj** %stackaddr$makeclosure56072, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48702, %struct.ScmObj* %_37last47117, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48702, %struct.ScmObj* %_37foldr47100, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48702, %struct.ScmObj* %_37drop_45right47114, i64 2)
%argslist55624$ae487000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56074 = alloca %struct.ScmObj*, align 8
%argslist55624$ae487001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48702, %struct.ScmObj* %argslist55624$ae487000)
store volatile %struct.ScmObj* %argslist55624$ae487001, %struct.ScmObj** %stackaddr$prim56074, align 8
%stackaddr$prim56075 = alloca %struct.ScmObj*, align 8
%argslist55624$ae487002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48701, %struct.ScmObj* %argslist55624$ae487001)
store volatile %struct.ScmObj* %argslist55624$ae487002, %struct.ScmObj** %stackaddr$prim56075, align 8
%clofunc56076 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48700)
musttail call tailcc void %clofunc56076(%struct.ScmObj* %ae48700, %struct.ScmObj* %argslist55624$ae487002)
ret void
}

define tailcc void @proc_clo$ae48700(%struct.ScmObj* %env$ae48700,%struct.ScmObj* %current_45args55272) {
%stackaddr$env-ref56077 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48700, i64 0)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref56077
%stackaddr$env-ref56078 = alloca %struct.ScmObj*, align 8
%_37foldl147079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48700, i64 1)
store %struct.ScmObj* %_37foldl147079, %struct.ScmObj** %stackaddr$env-ref56078
%stackaddr$env-ref56079 = alloca %struct.ScmObj*, align 8
%_37map147126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48700, i64 2)
store %struct.ScmObj* %_37map147126, %struct.ScmObj** %stackaddr$env-ref56079
%stackaddr$env-ref56080 = alloca %struct.ScmObj*, align 8
%Ycmb47074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48700, i64 3)
store %struct.ScmObj* %Ycmb47074, %struct.ScmObj** %stackaddr$env-ref56080
%stackaddr$env-ref56081 = alloca %struct.ScmObj*, align 8
%_37foldr47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48700, i64 4)
store %struct.ScmObj* %_37foldr47100, %struct.ScmObj** %stackaddr$env-ref56081
%stackaddr$prim56082 = alloca %struct.ScmObj*, align 8
%_95k47364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55272)
store volatile %struct.ScmObj* %_95k47364, %struct.ScmObj** %stackaddr$prim56082, align 8
%stackaddr$prim56083 = alloca %struct.ScmObj*, align 8
%current_45args55273 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55272)
store volatile %struct.ScmObj* %current_45args55273, %struct.ScmObj** %stackaddr$prim56083, align 8
%stackaddr$prim56084 = alloca %struct.ScmObj*, align 8
%_37map47121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55273)
store volatile %struct.ScmObj* %_37map47121, %struct.ScmObj** %stackaddr$prim56084, align 8
%stackaddr$makeclosure56085 = alloca %struct.ScmObj*, align 8
%fptrToInt56086 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48846 to i64
%ae48846 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56086)
store volatile %struct.ScmObj* %ae48846, %struct.ScmObj** %stackaddr$makeclosure56085, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48846, %struct.ScmObj* %Ycmb47074, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48846, %struct.ScmObj* %_37foldl147079, i64 1)
%ae48847 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56087 = alloca %struct.ScmObj*, align 8
%fptrToInt56088 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48848 to i64
%ae48848 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56088)
store volatile %struct.ScmObj* %ae48848, %struct.ScmObj** %stackaddr$makeclosure56087, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48848, %struct.ScmObj* %_37map147126, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48848, %struct.ScmObj* %_37foldr47100, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48848, %struct.ScmObj* %_37foldr147095, i64 2)
%argslist55607$ae488460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56089 = alloca %struct.ScmObj*, align 8
%argslist55607$ae488461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48848, %struct.ScmObj* %argslist55607$ae488460)
store volatile %struct.ScmObj* %argslist55607$ae488461, %struct.ScmObj** %stackaddr$prim56089, align 8
%stackaddr$prim56090 = alloca %struct.ScmObj*, align 8
%argslist55607$ae488462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48847, %struct.ScmObj* %argslist55607$ae488461)
store volatile %struct.ScmObj* %argslist55607$ae488462, %struct.ScmObj** %stackaddr$prim56090, align 8
%clofunc56091 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48846)
musttail call tailcc void %clofunc56091(%struct.ScmObj* %ae48846, %struct.ScmObj* %argslist55607$ae488462)
ret void
}

define tailcc void @proc_clo$ae48846(%struct.ScmObj* %env$ae48846,%struct.ScmObj* %current_45args55275) {
%stackaddr$env-ref56092 = alloca %struct.ScmObj*, align 8
%Ycmb47074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48846, i64 0)
store %struct.ScmObj* %Ycmb47074, %struct.ScmObj** %stackaddr$env-ref56092
%stackaddr$env-ref56093 = alloca %struct.ScmObj*, align 8
%_37foldl147079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48846, i64 1)
store %struct.ScmObj* %_37foldl147079, %struct.ScmObj** %stackaddr$env-ref56093
%stackaddr$prim56094 = alloca %struct.ScmObj*, align 8
%_95k47365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55275)
store volatile %struct.ScmObj* %_95k47365, %struct.ScmObj** %stackaddr$prim56094, align 8
%stackaddr$prim56095 = alloca %struct.ScmObj*, align 8
%current_45args55276 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55275)
store volatile %struct.ScmObj* %current_45args55276, %struct.ScmObj** %stackaddr$prim56095, align 8
%stackaddr$prim56096 = alloca %struct.ScmObj*, align 8
%anf_45bind47274 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55276)
store volatile %struct.ScmObj* %anf_45bind47274, %struct.ScmObj** %stackaddr$prim56096, align 8
%stackaddr$makeclosure56097 = alloca %struct.ScmObj*, align 8
%fptrToInt56098 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49238 to i64
%ae49238 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56098)
store volatile %struct.ScmObj* %ae49238, %struct.ScmObj** %stackaddr$makeclosure56097, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49238, %struct.ScmObj* %_37foldl147079, i64 0)
%argslist55547$Ycmb470740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56099 = alloca %struct.ScmObj*, align 8
%argslist55547$Ycmb470741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47274, %struct.ScmObj* %argslist55547$Ycmb470740)
store volatile %struct.ScmObj* %argslist55547$Ycmb470741, %struct.ScmObj** %stackaddr$prim56099, align 8
%stackaddr$prim56100 = alloca %struct.ScmObj*, align 8
%argslist55547$Ycmb470742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49238, %struct.ScmObj* %argslist55547$Ycmb470741)
store volatile %struct.ScmObj* %argslist55547$Ycmb470742, %struct.ScmObj** %stackaddr$prim56100, align 8
%clofunc56101 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47074)
musttail call tailcc void %clofunc56101(%struct.ScmObj* %Ycmb47074, %struct.ScmObj* %argslist55547$Ycmb470742)
ret void
}

define tailcc void @proc_clo$ae49238(%struct.ScmObj* %env$ae49238,%struct.ScmObj* %current_45args55278) {
%stackaddr$env-ref56102 = alloca %struct.ScmObj*, align 8
%_37foldl147079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49238, i64 0)
store %struct.ScmObj* %_37foldl147079, %struct.ScmObj** %stackaddr$env-ref56102
%stackaddr$prim56103 = alloca %struct.ScmObj*, align 8
%_95k47366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55278)
store volatile %struct.ScmObj* %_95k47366, %struct.ScmObj** %stackaddr$prim56103, align 8
%stackaddr$prim56104 = alloca %struct.ScmObj*, align 8
%current_45args55279 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55278)
store volatile %struct.ScmObj* %current_45args55279, %struct.ScmObj** %stackaddr$prim56104, align 8
%stackaddr$prim56105 = alloca %struct.ScmObj*, align 8
%_37foldl47177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55279)
store volatile %struct.ScmObj* %_37foldl47177, %struct.ScmObj** %stackaddr$prim56105, align 8
%stackaddr$makeclosure56106 = alloca %struct.ScmObj*, align 8
%fptrToInt56107 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49240 to i64
%ae49240 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56107)
store volatile %struct.ScmObj* %ae49240, %struct.ScmObj** %stackaddr$makeclosure56106, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49240, %struct.ScmObj* %_37foldl147079, i64 0)
%ae49241 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56108 = alloca %struct.ScmObj*, align 8
%fptrToInt56109 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49242 to i64
%ae49242 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56109)
store volatile %struct.ScmObj* %ae49242, %struct.ScmObj** %stackaddr$makeclosure56108, align 8
%argslist55546$ae492400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56110 = alloca %struct.ScmObj*, align 8
%argslist55546$ae492401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49242, %struct.ScmObj* %argslist55546$ae492400)
store volatile %struct.ScmObj* %argslist55546$ae492401, %struct.ScmObj** %stackaddr$prim56110, align 8
%stackaddr$prim56111 = alloca %struct.ScmObj*, align 8
%argslist55546$ae492402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49241, %struct.ScmObj* %argslist55546$ae492401)
store volatile %struct.ScmObj* %argslist55546$ae492402, %struct.ScmObj** %stackaddr$prim56111, align 8
%clofunc56112 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49240)
musttail call tailcc void %clofunc56112(%struct.ScmObj* %ae49240, %struct.ScmObj* %argslist55546$ae492402)
ret void
}

define tailcc void @proc_clo$ae49240(%struct.ScmObj* %env$ae49240,%struct.ScmObj* %current_45args55281) {
%stackaddr$env-ref56113 = alloca %struct.ScmObj*, align 8
%_37foldl147079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49240, i64 0)
store %struct.ScmObj* %_37foldl147079, %struct.ScmObj** %stackaddr$env-ref56113
%stackaddr$prim56114 = alloca %struct.ScmObj*, align 8
%_95k47367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55281)
store volatile %struct.ScmObj* %_95k47367, %struct.ScmObj** %stackaddr$prim56114, align 8
%stackaddr$prim56115 = alloca %struct.ScmObj*, align 8
%current_45args55282 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55281)
store volatile %struct.ScmObj* %current_45args55282, %struct.ScmObj** %stackaddr$prim56115, align 8
%stackaddr$prim56116 = alloca %struct.ScmObj*, align 8
%_37_6247174 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55282)
store volatile %struct.ScmObj* %_37_6247174, %struct.ScmObj** %stackaddr$prim56116, align 8
%stackaddr$makeclosure56117 = alloca %struct.ScmObj*, align 8
%fptrToInt56118 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49264 to i64
%ae49264 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56118)
store volatile %struct.ScmObj* %ae49264, %struct.ScmObj** %stackaddr$makeclosure56117, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49264, %struct.ScmObj* %_37foldl147079, i64 0)
%ae49265 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56119 = alloca %struct.ScmObj*, align 8
%fptrToInt56120 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49266 to i64
%ae49266 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56120)
store volatile %struct.ScmObj* %ae49266, %struct.ScmObj** %stackaddr$makeclosure56119, align 8
%argslist55540$ae492640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56121 = alloca %struct.ScmObj*, align 8
%argslist55540$ae492641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49266, %struct.ScmObj* %argslist55540$ae492640)
store volatile %struct.ScmObj* %argslist55540$ae492641, %struct.ScmObj** %stackaddr$prim56121, align 8
%stackaddr$prim56122 = alloca %struct.ScmObj*, align 8
%argslist55540$ae492642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49265, %struct.ScmObj* %argslist55540$ae492641)
store volatile %struct.ScmObj* %argslist55540$ae492642, %struct.ScmObj** %stackaddr$prim56122, align 8
%clofunc56123 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49264)
musttail call tailcc void %clofunc56123(%struct.ScmObj* %ae49264, %struct.ScmObj* %argslist55540$ae492642)
ret void
}

define tailcc void @proc_clo$ae49264(%struct.ScmObj* %env$ae49264,%struct.ScmObj* %current_45args55284) {
%stackaddr$env-ref56124 = alloca %struct.ScmObj*, align 8
%_37foldl147079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49264, i64 0)
store %struct.ScmObj* %_37foldl147079, %struct.ScmObj** %stackaddr$env-ref56124
%stackaddr$prim56125 = alloca %struct.ScmObj*, align 8
%_95k47368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55284)
store volatile %struct.ScmObj* %_95k47368, %struct.ScmObj** %stackaddr$prim56125, align 8
%stackaddr$prim56126 = alloca %struct.ScmObj*, align 8
%current_45args55285 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55284)
store volatile %struct.ScmObj* %current_45args55285, %struct.ScmObj** %stackaddr$prim56126, align 8
%stackaddr$prim56127 = alloca %struct.ScmObj*, align 8
%_37_62_6147171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55285)
store volatile %struct.ScmObj* %_37_62_6147171, %struct.ScmObj** %stackaddr$prim56127, align 8
%ae49288 = call %struct.ScmObj* @const_init_int(i64 1)
%ae49289 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56128 = alloca %struct.ScmObj*, align 8
%_37append47167 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49288, %struct.ScmObj* %ae49289)
store volatile %struct.ScmObj* %_37append47167, %struct.ScmObj** %stackaddr$prim56128, align 8
%stackaddr$makeclosure56129 = alloca %struct.ScmObj*, align 8
%fptrToInt56130 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49290 to i64
%ae49290 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56130)
store volatile %struct.ScmObj* %ae49290, %struct.ScmObj** %stackaddr$makeclosure56129, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49290, %struct.ScmObj* %_37append47167, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49290, %struct.ScmObj* %_37foldl147079, i64 1)
%ae49291 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56131 = alloca %struct.ScmObj*, align 8
%fptrToInt56132 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49292 to i64
%ae49292 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56132)
store volatile %struct.ScmObj* %ae49292, %struct.ScmObj** %stackaddr$makeclosure56131, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49292, %struct.ScmObj* %_37append47167, i64 0)
%argslist55534$ae492900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56133 = alloca %struct.ScmObj*, align 8
%argslist55534$ae492901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49292, %struct.ScmObj* %argslist55534$ae492900)
store volatile %struct.ScmObj* %argslist55534$ae492901, %struct.ScmObj** %stackaddr$prim56133, align 8
%stackaddr$prim56134 = alloca %struct.ScmObj*, align 8
%argslist55534$ae492902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49291, %struct.ScmObj* %argslist55534$ae492901)
store volatile %struct.ScmObj* %argslist55534$ae492902, %struct.ScmObj** %stackaddr$prim56134, align 8
%clofunc56135 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49290)
musttail call tailcc void %clofunc56135(%struct.ScmObj* %ae49290, %struct.ScmObj* %argslist55534$ae492902)
ret void
}

define tailcc void @proc_clo$ae49290(%struct.ScmObj* %env$ae49290,%struct.ScmObj* %current_45args55287) {
%stackaddr$env-ref56136 = alloca %struct.ScmObj*, align 8
%_37append47167 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49290, i64 0)
store %struct.ScmObj* %_37append47167, %struct.ScmObj** %stackaddr$env-ref56136
%stackaddr$env-ref56137 = alloca %struct.ScmObj*, align 8
%_37foldl147079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49290, i64 1)
store %struct.ScmObj* %_37foldl147079, %struct.ScmObj** %stackaddr$env-ref56137
%stackaddr$prim56138 = alloca %struct.ScmObj*, align 8
%_95k47369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55287)
store volatile %struct.ScmObj* %_95k47369, %struct.ScmObj** %stackaddr$prim56138, align 8
%stackaddr$prim56139 = alloca %struct.ScmObj*, align 8
%current_45args55288 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55287)
store volatile %struct.ScmObj* %current_45args55288, %struct.ScmObj** %stackaddr$prim56139, align 8
%stackaddr$prim56140 = alloca %struct.ScmObj*, align 8
%anf_45bind47282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55288)
store volatile %struct.ScmObj* %anf_45bind47282, %struct.ScmObj** %stackaddr$prim56140, align 8
%ae49358 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56141 = alloca %struct.ScmObj*, align 8
%_95047168 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append47167, %struct.ScmObj* %ae49358, %struct.ScmObj* %anf_45bind47282)
store volatile %struct.ScmObj* %_95047168, %struct.ScmObj** %stackaddr$prim56141, align 8
%ae49361 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56142 = alloca %struct.ScmObj*, align 8
%_37append47166 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47167, %struct.ScmObj* %ae49361)
store volatile %struct.ScmObj* %_37append47166, %struct.ScmObj** %stackaddr$prim56142, align 8
%stackaddr$makeclosure56143 = alloca %struct.ScmObj*, align 8
%fptrToInt56144 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49362 to i64
%ae49362 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56144)
store volatile %struct.ScmObj* %ae49362, %struct.ScmObj** %stackaddr$makeclosure56143, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49362, %struct.ScmObj* %_37foldl147079, i64 0)
%ae49363 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56145 = alloca %struct.ScmObj*, align 8
%fptrToInt56146 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49364 to i64
%ae49364 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56146)
store volatile %struct.ScmObj* %ae49364, %struct.ScmObj** %stackaddr$makeclosure56145, align 8
%argslist55523$ae493620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56147 = alloca %struct.ScmObj*, align 8
%argslist55523$ae493621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49364, %struct.ScmObj* %argslist55523$ae493620)
store volatile %struct.ScmObj* %argslist55523$ae493621, %struct.ScmObj** %stackaddr$prim56147, align 8
%stackaddr$prim56148 = alloca %struct.ScmObj*, align 8
%argslist55523$ae493622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49363, %struct.ScmObj* %argslist55523$ae493621)
store volatile %struct.ScmObj* %argslist55523$ae493622, %struct.ScmObj** %stackaddr$prim56148, align 8
%clofunc56149 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49362)
musttail call tailcc void %clofunc56149(%struct.ScmObj* %ae49362, %struct.ScmObj* %argslist55523$ae493622)
ret void
}

define tailcc void @proc_clo$ae49362(%struct.ScmObj* %env$ae49362,%struct.ScmObj* %current_45args55290) {
%stackaddr$env-ref56150 = alloca %struct.ScmObj*, align 8
%_37foldl147079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49362, i64 0)
store %struct.ScmObj* %_37foldl147079, %struct.ScmObj** %stackaddr$env-ref56150
%stackaddr$prim56151 = alloca %struct.ScmObj*, align 8
%_95k47370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55290)
store volatile %struct.ScmObj* %_95k47370, %struct.ScmObj** %stackaddr$prim56151, align 8
%stackaddr$prim56152 = alloca %struct.ScmObj*, align 8
%current_45args55291 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55290)
store volatile %struct.ScmObj* %current_45args55291, %struct.ScmObj** %stackaddr$prim56152, align 8
%stackaddr$prim56153 = alloca %struct.ScmObj*, align 8
%_37list_6347159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55291)
store volatile %struct.ScmObj* %_37list_6347159, %struct.ScmObj** %stackaddr$prim56153, align 8
%stackaddr$makeclosure56154 = alloca %struct.ScmObj*, align 8
%fptrToInt56155 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49778 to i64
%ae49778 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56155)
store volatile %struct.ScmObj* %ae49778, %struct.ScmObj** %stackaddr$makeclosure56154, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49778, %struct.ScmObj* %_37foldl147079, i64 0)
%ae49779 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56156 = alloca %struct.ScmObj*, align 8
%fptrToInt56157 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49780 to i64
%ae49780 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56157)
store volatile %struct.ScmObj* %ae49780, %struct.ScmObj** %stackaddr$makeclosure56156, align 8
%argslist55498$ae497780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56158 = alloca %struct.ScmObj*, align 8
%argslist55498$ae497781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49780, %struct.ScmObj* %argslist55498$ae497780)
store volatile %struct.ScmObj* %argslist55498$ae497781, %struct.ScmObj** %stackaddr$prim56158, align 8
%stackaddr$prim56159 = alloca %struct.ScmObj*, align 8
%argslist55498$ae497782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49779, %struct.ScmObj* %argslist55498$ae497781)
store volatile %struct.ScmObj* %argslist55498$ae497782, %struct.ScmObj** %stackaddr$prim56159, align 8
%clofunc56160 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49778)
musttail call tailcc void %clofunc56160(%struct.ScmObj* %ae49778, %struct.ScmObj* %argslist55498$ae497782)
ret void
}

define tailcc void @proc_clo$ae49778(%struct.ScmObj* %env$ae49778,%struct.ScmObj* %current_45args55293) {
%stackaddr$env-ref56161 = alloca %struct.ScmObj*, align 8
%_37foldl147079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49778, i64 0)
store %struct.ScmObj* %_37foldl147079, %struct.ScmObj** %stackaddr$env-ref56161
%stackaddr$prim56162 = alloca %struct.ScmObj*, align 8
%_95k47371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55293)
store volatile %struct.ScmObj* %_95k47371, %struct.ScmObj** %stackaddr$prim56162, align 8
%stackaddr$prim56163 = alloca %struct.ScmObj*, align 8
%current_45args55294 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55293)
store volatile %struct.ScmObj* %current_45args55294, %struct.ScmObj** %stackaddr$prim56163, align 8
%stackaddr$prim56164 = alloca %struct.ScmObj*, align 8
%_37drop47150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55294)
store volatile %struct.ScmObj* %_37drop47150, %struct.ScmObj** %stackaddr$prim56164, align 8
%stackaddr$makeclosure56165 = alloca %struct.ScmObj*, align 8
%fptrToInt56166 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50314 to i64
%ae50314 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56166)
store volatile %struct.ScmObj* %ae50314, %struct.ScmObj** %stackaddr$makeclosure56165, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50314, %struct.ScmObj* %_37foldl147079, i64 0)
%ae50315 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56167 = alloca %struct.ScmObj*, align 8
%fptrToInt56168 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50316 to i64
%ae50316 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56168)
store volatile %struct.ScmObj* %ae50316, %struct.ScmObj** %stackaddr$makeclosure56167, align 8
%argslist55474$ae503140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56169 = alloca %struct.ScmObj*, align 8
%argslist55474$ae503141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50316, %struct.ScmObj* %argslist55474$ae503140)
store volatile %struct.ScmObj* %argslist55474$ae503141, %struct.ScmObj** %stackaddr$prim56169, align 8
%stackaddr$prim56170 = alloca %struct.ScmObj*, align 8
%argslist55474$ae503142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50315, %struct.ScmObj* %argslist55474$ae503141)
store volatile %struct.ScmObj* %argslist55474$ae503142, %struct.ScmObj** %stackaddr$prim56170, align 8
%clofunc56171 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50314)
musttail call tailcc void %clofunc56171(%struct.ScmObj* %ae50314, %struct.ScmObj* %argslist55474$ae503142)
ret void
}

define tailcc void @proc_clo$ae50314(%struct.ScmObj* %env$ae50314,%struct.ScmObj* %current_45args55296) {
%stackaddr$env-ref56172 = alloca %struct.ScmObj*, align 8
%_37foldl147079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50314, i64 0)
store %struct.ScmObj* %_37foldl147079, %struct.ScmObj** %stackaddr$env-ref56172
%stackaddr$prim56173 = alloca %struct.ScmObj*, align 8
%_95k47372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55296)
store volatile %struct.ScmObj* %_95k47372, %struct.ScmObj** %stackaddr$prim56173, align 8
%stackaddr$prim56174 = alloca %struct.ScmObj*, align 8
%current_45args55297 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55296)
store volatile %struct.ScmObj* %current_45args55297, %struct.ScmObj** %stackaddr$prim56174, align 8
%stackaddr$prim56175 = alloca %struct.ScmObj*, align 8
%_37memv47143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55297)
store volatile %struct.ScmObj* %_37memv47143, %struct.ScmObj** %stackaddr$prim56175, align 8
%stackaddr$makeclosure56176 = alloca %struct.ScmObj*, align 8
%fptrToInt56177 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50718 to i64
%ae50718 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56177)
store volatile %struct.ScmObj* %ae50718, %struct.ScmObj** %stackaddr$makeclosure56176, align 8
%ae50719 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56178 = alloca %struct.ScmObj*, align 8
%fptrToInt56179 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50720 to i64
%ae50720 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56179)
store volatile %struct.ScmObj* %ae50720, %struct.ScmObj** %stackaddr$makeclosure56178, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50720, %struct.ScmObj* %_37foldl147079, i64 0)
%argslist55448$ae507180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56180 = alloca %struct.ScmObj*, align 8
%argslist55448$ae507181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50720, %struct.ScmObj* %argslist55448$ae507180)
store volatile %struct.ScmObj* %argslist55448$ae507181, %struct.ScmObj** %stackaddr$prim56180, align 8
%stackaddr$prim56181 = alloca %struct.ScmObj*, align 8
%argslist55448$ae507182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50719, %struct.ScmObj* %argslist55448$ae507181)
store volatile %struct.ScmObj* %argslist55448$ae507182, %struct.ScmObj** %stackaddr$prim56181, align 8
%clofunc56182 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50718)
musttail call tailcc void %clofunc56182(%struct.ScmObj* %ae50718, %struct.ScmObj* %argslist55448$ae507182)
ret void
}

define tailcc void @proc_clo$ae50718(%struct.ScmObj* %env$ae50718,%struct.ScmObj* %current_45args55299) {
%stackaddr$prim56183 = alloca %struct.ScmObj*, align 8
%_95k47373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55299)
store volatile %struct.ScmObj* %_95k47373, %struct.ScmObj** %stackaddr$prim56183, align 8
%stackaddr$prim56184 = alloca %struct.ScmObj*, align 8
%current_45args55300 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55299)
store volatile %struct.ScmObj* %current_45args55300, %struct.ScmObj** %stackaddr$prim56184, align 8
%stackaddr$prim56185 = alloca %struct.ScmObj*, align 8
%_37_4747139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55300)
store volatile %struct.ScmObj* %_37_4747139, %struct.ScmObj** %stackaddr$prim56185, align 8
%stackaddr$makeclosure56186 = alloca %struct.ScmObj*, align 8
%fptrToInt56187 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50816 to i64
%ae50816 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56187)
store volatile %struct.ScmObj* %ae50816, %struct.ScmObj** %stackaddr$makeclosure56186, align 8
%ae50817 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56188 = alloca %struct.ScmObj*, align 8
%fptrToInt56189 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50818 to i64
%ae50818 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56189)
store volatile %struct.ScmObj* %ae50818, %struct.ScmObj** %stackaddr$makeclosure56188, align 8
%argslist55435$ae508160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56190 = alloca %struct.ScmObj*, align 8
%argslist55435$ae508161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50818, %struct.ScmObj* %argslist55435$ae508160)
store volatile %struct.ScmObj* %argslist55435$ae508161, %struct.ScmObj** %stackaddr$prim56190, align 8
%stackaddr$prim56191 = alloca %struct.ScmObj*, align 8
%argslist55435$ae508162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50817, %struct.ScmObj* %argslist55435$ae508161)
store volatile %struct.ScmObj* %argslist55435$ae508162, %struct.ScmObj** %stackaddr$prim56191, align 8
%clofunc56192 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50816)
musttail call tailcc void %clofunc56192(%struct.ScmObj* %ae50816, %struct.ScmObj* %argslist55435$ae508162)
ret void
}

define tailcc void @proc_clo$ae50816(%struct.ScmObj* %env$ae50816,%struct.ScmObj* %current_45args55302) {
%stackaddr$prim56193 = alloca %struct.ScmObj*, align 8
%_95k47374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55302)
store volatile %struct.ScmObj* %_95k47374, %struct.ScmObj** %stackaddr$prim56193, align 8
%stackaddr$prim56194 = alloca %struct.ScmObj*, align 8
%current_45args55303 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55302)
store volatile %struct.ScmObj* %current_45args55303, %struct.ScmObj** %stackaddr$prim56194, align 8
%stackaddr$prim56195 = alloca %struct.ScmObj*, align 8
%_37first47137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55303)
store volatile %struct.ScmObj* %_37first47137, %struct.ScmObj** %stackaddr$prim56195, align 8
%stackaddr$makeclosure56196 = alloca %struct.ScmObj*, align 8
%fptrToInt56197 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50836 to i64
%ae50836 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56197)
store volatile %struct.ScmObj* %ae50836, %struct.ScmObj** %stackaddr$makeclosure56196, align 8
%ae50837 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56198 = alloca %struct.ScmObj*, align 8
%fptrToInt56199 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50838 to i64
%ae50838 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56199)
store volatile %struct.ScmObj* %ae50838, %struct.ScmObj** %stackaddr$makeclosure56198, align 8
%argslist55430$ae508360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56200 = alloca %struct.ScmObj*, align 8
%argslist55430$ae508361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50838, %struct.ScmObj* %argslist55430$ae508360)
store volatile %struct.ScmObj* %argslist55430$ae508361, %struct.ScmObj** %stackaddr$prim56200, align 8
%stackaddr$prim56201 = alloca %struct.ScmObj*, align 8
%argslist55430$ae508362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50837, %struct.ScmObj* %argslist55430$ae508361)
store volatile %struct.ScmObj* %argslist55430$ae508362, %struct.ScmObj** %stackaddr$prim56201, align 8
%clofunc56202 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50836)
musttail call tailcc void %clofunc56202(%struct.ScmObj* %ae50836, %struct.ScmObj* %argslist55430$ae508362)
ret void
}

define tailcc void @proc_clo$ae50836(%struct.ScmObj* %env$ae50836,%struct.ScmObj* %current_45args55305) {
%stackaddr$prim56203 = alloca %struct.ScmObj*, align 8
%_95k47375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55305)
store volatile %struct.ScmObj* %_95k47375, %struct.ScmObj** %stackaddr$prim56203, align 8
%stackaddr$prim56204 = alloca %struct.ScmObj*, align 8
%current_45args55306 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55305)
store volatile %struct.ScmObj* %current_45args55306, %struct.ScmObj** %stackaddr$prim56204, align 8
%stackaddr$prim56205 = alloca %struct.ScmObj*, align 8
%_37second47135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55306)
store volatile %struct.ScmObj* %_37second47135, %struct.ScmObj** %stackaddr$prim56205, align 8
%stackaddr$makeclosure56206 = alloca %struct.ScmObj*, align 8
%fptrToInt56207 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50858 to i64
%ae50858 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56207)
store volatile %struct.ScmObj* %ae50858, %struct.ScmObj** %stackaddr$makeclosure56206, align 8
%ae50859 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56208 = alloca %struct.ScmObj*, align 8
%fptrToInt56209 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50860 to i64
%ae50860 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56209)
store volatile %struct.ScmObj* %ae50860, %struct.ScmObj** %stackaddr$makeclosure56208, align 8
%argslist55425$ae508580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56210 = alloca %struct.ScmObj*, align 8
%argslist55425$ae508581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50860, %struct.ScmObj* %argslist55425$ae508580)
store volatile %struct.ScmObj* %argslist55425$ae508581, %struct.ScmObj** %stackaddr$prim56210, align 8
%stackaddr$prim56211 = alloca %struct.ScmObj*, align 8
%argslist55425$ae508582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50859, %struct.ScmObj* %argslist55425$ae508581)
store volatile %struct.ScmObj* %argslist55425$ae508582, %struct.ScmObj** %stackaddr$prim56211, align 8
%clofunc56212 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50858)
musttail call tailcc void %clofunc56212(%struct.ScmObj* %ae50858, %struct.ScmObj* %argslist55425$ae508582)
ret void
}

define tailcc void @proc_clo$ae50858(%struct.ScmObj* %env$ae50858,%struct.ScmObj* %current_45args55308) {
%stackaddr$prim56213 = alloca %struct.ScmObj*, align 8
%_95k47376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55308)
store volatile %struct.ScmObj* %_95k47376, %struct.ScmObj** %stackaddr$prim56213, align 8
%stackaddr$prim56214 = alloca %struct.ScmObj*, align 8
%current_45args55309 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55308)
store volatile %struct.ScmObj* %current_45args55309, %struct.ScmObj** %stackaddr$prim56214, align 8
%stackaddr$prim56215 = alloca %struct.ScmObj*, align 8
%_37third47133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55309)
store volatile %struct.ScmObj* %_37third47133, %struct.ScmObj** %stackaddr$prim56215, align 8
%stackaddr$makeclosure56216 = alloca %struct.ScmObj*, align 8
%fptrToInt56217 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50882 to i64
%ae50882 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56217)
store volatile %struct.ScmObj* %ae50882, %struct.ScmObj** %stackaddr$makeclosure56216, align 8
%ae50883 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56218 = alloca %struct.ScmObj*, align 8
%fptrToInt56219 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50884 to i64
%ae50884 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56219)
store volatile %struct.ScmObj* %ae50884, %struct.ScmObj** %stackaddr$makeclosure56218, align 8
%argslist55420$ae508820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56220 = alloca %struct.ScmObj*, align 8
%argslist55420$ae508821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50884, %struct.ScmObj* %argslist55420$ae508820)
store volatile %struct.ScmObj* %argslist55420$ae508821, %struct.ScmObj** %stackaddr$prim56220, align 8
%stackaddr$prim56221 = alloca %struct.ScmObj*, align 8
%argslist55420$ae508822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50883, %struct.ScmObj* %argslist55420$ae508821)
store volatile %struct.ScmObj* %argslist55420$ae508822, %struct.ScmObj** %stackaddr$prim56221, align 8
%clofunc56222 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50882)
musttail call tailcc void %clofunc56222(%struct.ScmObj* %ae50882, %struct.ScmObj* %argslist55420$ae508822)
ret void
}

define tailcc void @proc_clo$ae50882(%struct.ScmObj* %env$ae50882,%struct.ScmObj* %current_45args55311) {
%stackaddr$prim56223 = alloca %struct.ScmObj*, align 8
%_95k47377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55311)
store volatile %struct.ScmObj* %_95k47377, %struct.ScmObj** %stackaddr$prim56223, align 8
%stackaddr$prim56224 = alloca %struct.ScmObj*, align 8
%current_45args55312 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55311)
store volatile %struct.ScmObj* %current_45args55312, %struct.ScmObj** %stackaddr$prim56224, align 8
%stackaddr$prim56225 = alloca %struct.ScmObj*, align 8
%_37fourth47131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55312)
store volatile %struct.ScmObj* %_37fourth47131, %struct.ScmObj** %stackaddr$prim56225, align 8
%stackaddr$prim56226 = alloca %struct.ScmObj*, align 8
%anf_45bind47318 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47318, %struct.ScmObj** %stackaddr$prim56226, align 8
%ae50908 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56227 = alloca %struct.ScmObj*, align 8
%gen47193 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50908, %struct.ScmObj* %anf_45bind47318)
store volatile %struct.ScmObj* %gen47193, %struct.ScmObj** %stackaddr$prim56227, align 8
%stackaddr$prim56228 = alloca %struct.ScmObj*, align 8
%anf_45bind47319 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind47319, %struct.ScmObj** %stackaddr$prim56228, align 8
%ae50910 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56229 = alloca %struct.ScmObj*, align 8
%lazy_45take47192 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50910, %struct.ScmObj* %anf_45bind47319)
store volatile %struct.ScmObj* %lazy_45take47192, %struct.ScmObj** %stackaddr$prim56229, align 8
%stackaddr$makeclosure56230 = alloca %struct.ScmObj*, align 8
%fptrToInt56231 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50912 to i64
%ae50912 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56231)
store volatile %struct.ScmObj* %ae50912, %struct.ScmObj** %stackaddr$makeclosure56230, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50912, %struct.ScmObj* %gen47193, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50912, %struct.ScmObj* %lazy_45take47192, i64 1)
%ae50913 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56232 = alloca %struct.ScmObj*, align 8
%fptrToInt56233 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50914 to i64
%ae50914 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56233)
store volatile %struct.ScmObj* %ae50914, %struct.ScmObj** %stackaddr$makeclosure56232, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50914, %struct.ScmObj* %gen47193, i64 0)
%argslist55415$ae509120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56234 = alloca %struct.ScmObj*, align 8
%argslist55415$ae509121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50914, %struct.ScmObj* %argslist55415$ae509120)
store volatile %struct.ScmObj* %argslist55415$ae509121, %struct.ScmObj** %stackaddr$prim56234, align 8
%stackaddr$prim56235 = alloca %struct.ScmObj*, align 8
%argslist55415$ae509122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50913, %struct.ScmObj* %argslist55415$ae509121)
store volatile %struct.ScmObj* %argslist55415$ae509122, %struct.ScmObj** %stackaddr$prim56235, align 8
%clofunc56236 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50912)
musttail call tailcc void %clofunc56236(%struct.ScmObj* %ae50912, %struct.ScmObj* %argslist55415$ae509122)
ret void
}

define tailcc void @proc_clo$ae50912(%struct.ScmObj* %env$ae50912,%struct.ScmObj* %current_45args55314) {
%stackaddr$env-ref56237 = alloca %struct.ScmObj*, align 8
%gen47193 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50912, i64 0)
store %struct.ScmObj* %gen47193, %struct.ScmObj** %stackaddr$env-ref56237
%stackaddr$env-ref56238 = alloca %struct.ScmObj*, align 8
%lazy_45take47192 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50912, i64 1)
store %struct.ScmObj* %lazy_45take47192, %struct.ScmObj** %stackaddr$env-ref56238
%stackaddr$prim56239 = alloca %struct.ScmObj*, align 8
%_95k47378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55314)
store volatile %struct.ScmObj* %_95k47378, %struct.ScmObj** %stackaddr$prim56239, align 8
%stackaddr$prim56240 = alloca %struct.ScmObj*, align 8
%current_45args55315 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55314)
store volatile %struct.ScmObj* %current_45args55315, %struct.ScmObj** %stackaddr$prim56240, align 8
%stackaddr$prim56241 = alloca %struct.ScmObj*, align 8
%anf_45bind47325 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55315)
store volatile %struct.ScmObj* %anf_45bind47325, %struct.ScmObj** %stackaddr$prim56241, align 8
%ae51057 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56242 = alloca %struct.ScmObj*, align 8
%t4706947203 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %gen47193, %struct.ScmObj* %ae51057, %struct.ScmObj* %anf_45bind47325)
store volatile %struct.ScmObj* %t4706947203, %struct.ScmObj** %stackaddr$prim56242, align 8
%stackaddr$makeclosure56243 = alloca %struct.ScmObj*, align 8
%fptrToInt56244 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51059 to i64
%ae51059 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56244)
store volatile %struct.ScmObj* %ae51059, %struct.ScmObj** %stackaddr$makeclosure56243, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51059, %struct.ScmObj* %gen47193, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51059, %struct.ScmObj* %lazy_45take47192, i64 1)
%ae51060 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56245 = alloca %struct.ScmObj*, align 8
%fptrToInt56246 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51061 to i64
%ae51061 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56246)
store volatile %struct.ScmObj* %ae51061, %struct.ScmObj** %stackaddr$makeclosure56245, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51061, %struct.ScmObj* %lazy_45take47192, i64 0)
%argslist55393$ae510590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56247 = alloca %struct.ScmObj*, align 8
%argslist55393$ae510591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51061, %struct.ScmObj* %argslist55393$ae510590)
store volatile %struct.ScmObj* %argslist55393$ae510591, %struct.ScmObj** %stackaddr$prim56247, align 8
%stackaddr$prim56248 = alloca %struct.ScmObj*, align 8
%argslist55393$ae510592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51060, %struct.ScmObj* %argslist55393$ae510591)
store volatile %struct.ScmObj* %argslist55393$ae510592, %struct.ScmObj** %stackaddr$prim56248, align 8
%clofunc56249 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51059)
musttail call tailcc void %clofunc56249(%struct.ScmObj* %ae51059, %struct.ScmObj* %argslist55393$ae510592)
ret void
}

define tailcc void @proc_clo$ae51059(%struct.ScmObj* %env$ae51059,%struct.ScmObj* %current_45args55317) {
%stackaddr$env-ref56250 = alloca %struct.ScmObj*, align 8
%gen47193 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51059, i64 0)
store %struct.ScmObj* %gen47193, %struct.ScmObj** %stackaddr$env-ref56250
%stackaddr$env-ref56251 = alloca %struct.ScmObj*, align 8
%lazy_45take47192 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51059, i64 1)
store %struct.ScmObj* %lazy_45take47192, %struct.ScmObj** %stackaddr$env-ref56251
%stackaddr$prim56252 = alloca %struct.ScmObj*, align 8
%_95k47379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55317)
store volatile %struct.ScmObj* %_95k47379, %struct.ScmObj** %stackaddr$prim56252, align 8
%stackaddr$prim56253 = alloca %struct.ScmObj*, align 8
%current_45args55318 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55317)
store volatile %struct.ScmObj* %current_45args55318, %struct.ScmObj** %stackaddr$prim56253, align 8
%stackaddr$prim56254 = alloca %struct.ScmObj*, align 8
%anf_45bind47342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55318)
store volatile %struct.ScmObj* %anf_45bind47342, %struct.ScmObj** %stackaddr$prim56254, align 8
%ae51923 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56255 = alloca %struct.ScmObj*, align 8
%t4706847194 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lazy_45take47192, %struct.ScmObj* %ae51923, %struct.ScmObj* %anf_45bind47342)
store volatile %struct.ScmObj* %t4706847194, %struct.ScmObj** %stackaddr$prim56255, align 8
%ae51926 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56256 = alloca %struct.ScmObj*, align 8
%anf_45bind47343 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lazy_45take47192, %struct.ScmObj* %ae51926)
store volatile %struct.ScmObj* %anf_45bind47343, %struct.ScmObj** %stackaddr$prim56256, align 8
%ae51928 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56257 = alloca %struct.ScmObj*, align 8
%anf_45bind47344 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %gen47193, %struct.ScmObj* %ae51928)
store volatile %struct.ScmObj* %anf_45bind47344, %struct.ScmObj** %stackaddr$prim56257, align 8
%stackaddr$makeclosure56258 = alloca %struct.ScmObj*, align 8
%fptrToInt56259 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51930 to i64
%ae51930 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56259)
store volatile %struct.ScmObj* %ae51930, %struct.ScmObj** %stackaddr$makeclosure56258, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51930, %struct.ScmObj* %anf_45bind47343, i64 0)
%ae51931 = call %struct.ScmObj* @const_init_int(i64 8)
%argslist55328$anf_45bind473440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56260 = alloca %struct.ScmObj*, align 8
%argslist55328$anf_45bind473441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51931, %struct.ScmObj* %argslist55328$anf_45bind473440)
store volatile %struct.ScmObj* %argslist55328$anf_45bind473441, %struct.ScmObj** %stackaddr$prim56260, align 8
%stackaddr$prim56261 = alloca %struct.ScmObj*, align 8
%argslist55328$anf_45bind473442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51930, %struct.ScmObj* %argslist55328$anf_45bind473441)
store volatile %struct.ScmObj* %argslist55328$anf_45bind473442, %struct.ScmObj** %stackaddr$prim56261, align 8
%clofunc56262 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47344)
musttail call tailcc void %clofunc56262(%struct.ScmObj* %anf_45bind47344, %struct.ScmObj* %argslist55328$anf_45bind473442)
ret void
}

define tailcc void @proc_clo$ae51930(%struct.ScmObj* %env$ae51930,%struct.ScmObj* %current_45args55320) {
%stackaddr$env-ref56263 = alloca %struct.ScmObj*, align 8
%anf_45bind47343 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51930, i64 0)
store %struct.ScmObj* %anf_45bind47343, %struct.ScmObj** %stackaddr$env-ref56263
%stackaddr$prim56264 = alloca %struct.ScmObj*, align 8
%_95k47380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55320)
store volatile %struct.ScmObj* %_95k47380, %struct.ScmObj** %stackaddr$prim56264, align 8
%stackaddr$prim56265 = alloca %struct.ScmObj*, align 8
%current_45args55321 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55320)
store volatile %struct.ScmObj* %current_45args55321, %struct.ScmObj** %stackaddr$prim56265, align 8
%stackaddr$prim56266 = alloca %struct.ScmObj*, align 8
%anf_45bind47345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55321)
store volatile %struct.ScmObj* %anf_45bind47345, %struct.ScmObj** %stackaddr$prim56266, align 8
%stackaddr$makeclosure56267 = alloca %struct.ScmObj*, align 8
%fptrToInt56268 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51936 to i64
%ae51936 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56268)
store volatile %struct.ScmObj* %ae51936, %struct.ScmObj** %stackaddr$makeclosure56267, align 8
%ae51938 = call %struct.ScmObj* @const_init_int(i64 4)
%argslist55327$anf_45bind473430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56269 = alloca %struct.ScmObj*, align 8
%argslist55327$anf_45bind473431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51938, %struct.ScmObj* %argslist55327$anf_45bind473430)
store volatile %struct.ScmObj* %argslist55327$anf_45bind473431, %struct.ScmObj** %stackaddr$prim56269, align 8
%stackaddr$prim56270 = alloca %struct.ScmObj*, align 8
%argslist55327$anf_45bind473432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47345, %struct.ScmObj* %argslist55327$anf_45bind473431)
store volatile %struct.ScmObj* %argslist55327$anf_45bind473432, %struct.ScmObj** %stackaddr$prim56270, align 8
%stackaddr$prim56271 = alloca %struct.ScmObj*, align 8
%argslist55327$anf_45bind473433 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51936, %struct.ScmObj* %argslist55327$anf_45bind473432)
store volatile %struct.ScmObj* %argslist55327$anf_45bind473433, %struct.ScmObj** %stackaddr$prim56271, align 8
%clofunc56272 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47343)
musttail call tailcc void %clofunc56272(%struct.ScmObj* %anf_45bind47343, %struct.ScmObj* %argslist55327$anf_45bind473433)
ret void
}

define tailcc void @proc_clo$ae51936(%struct.ScmObj* %env$ae51936,%struct.ScmObj* %current_45args55323) {
%stackaddr$prim56273 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55323)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim56273, align 8
%stackaddr$prim56274 = alloca %struct.ScmObj*, align 8
%current_45args55324 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55323)
store volatile %struct.ScmObj* %current_45args55324, %struct.ScmObj** %stackaddr$prim56274, align 8
%stackaddr$prim56275 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55324)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim56275, align 8
%stackaddr$prim56276 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim56276, align 8
%argslist55326$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56277 = alloca %struct.ScmObj*, align 8
%argslist55326$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist55326$k0)
store volatile %struct.ScmObj* %argslist55326$k1, %struct.ScmObj** %stackaddr$prim56277, align 8
%clofunc56278 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc56278(%struct.ScmObj* %k, %struct.ScmObj* %argslist55326$k1)
ret void
}

define tailcc void @proc_clo$ae51061(%struct.ScmObj* %env$ae51061,%struct.ScmObj* %current_45args55329) {
%stackaddr$env-ref56279 = alloca %struct.ScmObj*, align 8
%lazy_45take47192 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51061, i64 0)
store %struct.ScmObj* %lazy_45take47192, %struct.ScmObj** %stackaddr$env-ref56279
%stackaddr$prim56280 = alloca %struct.ScmObj*, align 8
%k47381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55329)
store volatile %struct.ScmObj* %k47381, %struct.ScmObj** %stackaddr$prim56280, align 8
%stackaddr$prim56281 = alloca %struct.ScmObj*, align 8
%current_45args55330 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55329)
store volatile %struct.ScmObj* %current_45args55330, %struct.ScmObj** %stackaddr$prim56281, align 8
%stackaddr$prim56282 = alloca %struct.ScmObj*, align 8
%llst47196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55330)
store volatile %struct.ScmObj* %llst47196, %struct.ScmObj** %stackaddr$prim56282, align 8
%stackaddr$prim56283 = alloca %struct.ScmObj*, align 8
%current_45args55331 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55330)
store volatile %struct.ScmObj* %current_45args55331, %struct.ScmObj** %stackaddr$prim56283, align 8
%stackaddr$prim56284 = alloca %struct.ScmObj*, align 8
%n47195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55331)
store volatile %struct.ScmObj* %n47195, %struct.ScmObj** %stackaddr$prim56284, align 8
%ae51063 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56285 = alloca %struct.ScmObj*, align 8
%anf_45bind47326 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n47195, %struct.ScmObj* %ae51063)
store volatile %struct.ScmObj* %anf_45bind47326, %struct.ScmObj** %stackaddr$prim56285, align 8
%truthy$cmp56286 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47326)
%cmp$cmp56286 = icmp eq i64 %truthy$cmp56286, 1
br i1 %cmp$cmp56286, label %truebranch$cmp56286, label %falsebranch$cmp56286
truebranch$cmp56286:
%stackaddr$makeclosure56287 = alloca %struct.ScmObj*, align 8
%fptrToInt56288 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51065 to i64
%ae51065 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56288)
store volatile %struct.ScmObj* %ae51065, %struct.ScmObj** %stackaddr$makeclosure56287, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51065, %struct.ScmObj* %k47381, i64 0)
%ae51066 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56289 = alloca %struct.ScmObj*, align 8
%fptrToInt56290 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51067 to i64
%ae51067 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56290)
store volatile %struct.ScmObj* %ae51067, %struct.ScmObj** %stackaddr$makeclosure56289, align 8
%argslist55338$ae510650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56291 = alloca %struct.ScmObj*, align 8
%argslist55338$ae510651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51067, %struct.ScmObj* %argslist55338$ae510650)
store volatile %struct.ScmObj* %argslist55338$ae510651, %struct.ScmObj** %stackaddr$prim56291, align 8
%stackaddr$prim56292 = alloca %struct.ScmObj*, align 8
%argslist55338$ae510652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51066, %struct.ScmObj* %argslist55338$ae510651)
store volatile %struct.ScmObj* %argslist55338$ae510652, %struct.ScmObj** %stackaddr$prim56292, align 8
%clofunc56293 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51065)
musttail call tailcc void %clofunc56293(%struct.ScmObj* %ae51065, %struct.ScmObj* %argslist55338$ae510652)
ret void
falsebranch$cmp56286:
%stackaddr$prim56294 = alloca %struct.ScmObj*, align 8
%anf_45bind47328 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %llst47196)
store volatile %struct.ScmObj* %anf_45bind47328, %struct.ScmObj** %stackaddr$prim56294, align 8
%ae51099 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56295 = alloca %struct.ScmObj*, align 8
%anf_45bind47329 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lazy_45take47192, %struct.ScmObj* %ae51099)
store volatile %struct.ScmObj* %anf_45bind47329, %struct.ScmObj** %stackaddr$prim56295, align 8
%stackaddr$prim56296 = alloca %struct.ScmObj*, align 8
%thunk4707047198 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %llst47196)
store volatile %struct.ScmObj* %thunk4707047198, %struct.ScmObj** %stackaddr$prim56296, align 8
%stackaddr$makeclosure56297 = alloca %struct.ScmObj*, align 8
%fptrToInt56298 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51101 to i64
%ae51101 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56298)
store volatile %struct.ScmObj* %ae51101, %struct.ScmObj** %stackaddr$makeclosure56297, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51101, %struct.ScmObj* %k47381, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51101, %struct.ScmObj* %anf_45bind47329, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51101, %struct.ScmObj* %anf_45bind47328, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51101, %struct.ScmObj* %thunk4707047198, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51101, %struct.ScmObj* %n47195, i64 4)
%ae51102 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56299 = alloca %struct.ScmObj*, align 8
%fptrToInt56300 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51103 to i64
%ae51103 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56300)
store volatile %struct.ScmObj* %ae51103, %struct.ScmObj** %stackaddr$makeclosure56299, align 8
%argslist55392$ae511010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56301 = alloca %struct.ScmObj*, align 8
%argslist55392$ae511011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51103, %struct.ScmObj* %argslist55392$ae511010)
store volatile %struct.ScmObj* %argslist55392$ae511011, %struct.ScmObj** %stackaddr$prim56301, align 8
%stackaddr$prim56302 = alloca %struct.ScmObj*, align 8
%argslist55392$ae511012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51102, %struct.ScmObj* %argslist55392$ae511011)
store volatile %struct.ScmObj* %argslist55392$ae511012, %struct.ScmObj** %stackaddr$prim56302, align 8
%clofunc56303 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51101)
musttail call tailcc void %clofunc56303(%struct.ScmObj* %ae51101, %struct.ScmObj* %argslist55392$ae511012)
ret void
}

define tailcc void @proc_clo$ae51065(%struct.ScmObj* %env$ae51065,%struct.ScmObj* %current_45args55333) {
%stackaddr$env-ref56304 = alloca %struct.ScmObj*, align 8
%k47381 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51065, i64 0)
store %struct.ScmObj* %k47381, %struct.ScmObj** %stackaddr$env-ref56304
%stackaddr$prim56305 = alloca %struct.ScmObj*, align 8
%_95k47382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55333)
store volatile %struct.ScmObj* %_95k47382, %struct.ScmObj** %stackaddr$prim56305, align 8
%stackaddr$prim56306 = alloca %struct.ScmObj*, align 8
%current_45args55334 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55333)
store volatile %struct.ScmObj* %current_45args55334, %struct.ScmObj** %stackaddr$prim56306, align 8
%stackaddr$prim56307 = alloca %struct.ScmObj*, align 8
%anf_45bind47327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55334)
store volatile %struct.ScmObj* %anf_45bind47327, %struct.ScmObj** %stackaddr$prim56307, align 8
%argslist55336$anf_45bind473270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56308 = alloca %struct.ScmObj*, align 8
%argslist55336$anf_45bind473271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47381, %struct.ScmObj* %argslist55336$anf_45bind473270)
store volatile %struct.ScmObj* %argslist55336$anf_45bind473271, %struct.ScmObj** %stackaddr$prim56308, align 8
%clofunc56309 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47327)
musttail call tailcc void %clofunc56309(%struct.ScmObj* %anf_45bind47327, %struct.ScmObj* %argslist55336$anf_45bind473271)
ret void
}

define tailcc void @proc_clo$ae51067(%struct.ScmObj* %env$ae51067,%struct.ScmObj* %lst4719747383) {
%stackaddr$prim56310 = alloca %struct.ScmObj*, align 8
%k47384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4719747383)
store volatile %struct.ScmObj* %k47384, %struct.ScmObj** %stackaddr$prim56310, align 8
%stackaddr$prim56311 = alloca %struct.ScmObj*, align 8
%lst47197 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4719747383)
store volatile %struct.ScmObj* %lst47197, %struct.ScmObj** %stackaddr$prim56311, align 8
%ae51071 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55337$k473840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56312 = alloca %struct.ScmObj*, align 8
%argslist55337$k473841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47197, %struct.ScmObj* %argslist55337$k473840)
store volatile %struct.ScmObj* %argslist55337$k473841, %struct.ScmObj** %stackaddr$prim56312, align 8
%stackaddr$prim56313 = alloca %struct.ScmObj*, align 8
%argslist55337$k473842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51071, %struct.ScmObj* %argslist55337$k473841)
store volatile %struct.ScmObj* %argslist55337$k473842, %struct.ScmObj** %stackaddr$prim56313, align 8
%clofunc56314 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47384)
musttail call tailcc void %clofunc56314(%struct.ScmObj* %k47384, %struct.ScmObj* %argslist55337$k473842)
ret void
}

define tailcc void @proc_clo$ae51101(%struct.ScmObj* %env$ae51101,%struct.ScmObj* %current_45args55339) {
%stackaddr$env-ref56315 = alloca %struct.ScmObj*, align 8
%k47381 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51101, i64 0)
store %struct.ScmObj* %k47381, %struct.ScmObj** %stackaddr$env-ref56315
%stackaddr$env-ref56316 = alloca %struct.ScmObj*, align 8
%anf_45bind47329 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51101, i64 1)
store %struct.ScmObj* %anf_45bind47329, %struct.ScmObj** %stackaddr$env-ref56316
%stackaddr$env-ref56317 = alloca %struct.ScmObj*, align 8
%anf_45bind47328 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51101, i64 2)
store %struct.ScmObj* %anf_45bind47328, %struct.ScmObj** %stackaddr$env-ref56317
%stackaddr$env-ref56318 = alloca %struct.ScmObj*, align 8
%thunk4707047198 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51101, i64 3)
store %struct.ScmObj* %thunk4707047198, %struct.ScmObj** %stackaddr$env-ref56318
%stackaddr$env-ref56319 = alloca %struct.ScmObj*, align 8
%n47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51101, i64 4)
store %struct.ScmObj* %n47195, %struct.ScmObj** %stackaddr$env-ref56319
%stackaddr$prim56320 = alloca %struct.ScmObj*, align 8
%_95k47385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55339)
store volatile %struct.ScmObj* %_95k47385, %struct.ScmObj** %stackaddr$prim56320, align 8
%stackaddr$prim56321 = alloca %struct.ScmObj*, align 8
%current_45args55340 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55339)
store volatile %struct.ScmObj* %current_45args55340, %struct.ScmObj** %stackaddr$prim56321, align 8
%stackaddr$prim56322 = alloca %struct.ScmObj*, align 8
%anf_45bind47334 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55340)
store volatile %struct.ScmObj* %anf_45bind47334, %struct.ScmObj** %stackaddr$prim56322, align 8
%stackaddr$makeclosure56323 = alloca %struct.ScmObj*, align 8
%fptrToInt56324 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51189 to i64
%ae51189 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56324)
store volatile %struct.ScmObj* %ae51189, %struct.ScmObj** %stackaddr$makeclosure56323, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51189, %struct.ScmObj* %k47381, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51189, %struct.ScmObj* %anf_45bind47329, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51189, %struct.ScmObj* %anf_45bind47328, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51189, %struct.ScmObj* %thunk4707047198, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51189, %struct.ScmObj* %n47195, i64 4)
%argslist55385$anf_45bind473340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56325 = alloca %struct.ScmObj*, align 8
%argslist55385$anf_45bind473341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4707047198, %struct.ScmObj* %argslist55385$anf_45bind473340)
store volatile %struct.ScmObj* %argslist55385$anf_45bind473341, %struct.ScmObj** %stackaddr$prim56325, align 8
%stackaddr$prim56326 = alloca %struct.ScmObj*, align 8
%argslist55385$anf_45bind473342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51189, %struct.ScmObj* %argslist55385$anf_45bind473341)
store volatile %struct.ScmObj* %argslist55385$anf_45bind473342, %struct.ScmObj** %stackaddr$prim56326, align 8
%clofunc56327 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47334)
musttail call tailcc void %clofunc56327(%struct.ScmObj* %anf_45bind47334, %struct.ScmObj* %argslist55385$anf_45bind473342)
ret void
}

define tailcc void @proc_clo$ae51189(%struct.ScmObj* %env$ae51189,%struct.ScmObj* %current_45args55342) {
%stackaddr$env-ref56328 = alloca %struct.ScmObj*, align 8
%k47381 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51189, i64 0)
store %struct.ScmObj* %k47381, %struct.ScmObj** %stackaddr$env-ref56328
%stackaddr$env-ref56329 = alloca %struct.ScmObj*, align 8
%anf_45bind47329 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51189, i64 1)
store %struct.ScmObj* %anf_45bind47329, %struct.ScmObj** %stackaddr$env-ref56329
%stackaddr$env-ref56330 = alloca %struct.ScmObj*, align 8
%anf_45bind47328 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51189, i64 2)
store %struct.ScmObj* %anf_45bind47328, %struct.ScmObj** %stackaddr$env-ref56330
%stackaddr$env-ref56331 = alloca %struct.ScmObj*, align 8
%thunk4707047198 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51189, i64 3)
store %struct.ScmObj* %thunk4707047198, %struct.ScmObj** %stackaddr$env-ref56331
%stackaddr$env-ref56332 = alloca %struct.ScmObj*, align 8
%n47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51189, i64 4)
store %struct.ScmObj* %n47195, %struct.ScmObj** %stackaddr$env-ref56332
%stackaddr$prim56333 = alloca %struct.ScmObj*, align 8
%_95k47386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55342)
store volatile %struct.ScmObj* %_95k47386, %struct.ScmObj** %stackaddr$prim56333, align 8
%stackaddr$prim56334 = alloca %struct.ScmObj*, align 8
%current_45args55343 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55342)
store volatile %struct.ScmObj* %current_45args55343, %struct.ScmObj** %stackaddr$prim56334, align 8
%stackaddr$prim56335 = alloca %struct.ScmObj*, align 8
%anf_45bind47335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55343)
store volatile %struct.ScmObj* %anf_45bind47335, %struct.ScmObj** %stackaddr$prim56335, align 8
%truthy$cmp56336 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47335)
%cmp$cmp56336 = icmp eq i64 %truthy$cmp56336, 1
br i1 %cmp$cmp56336, label %truebranch$cmp56336, label %falsebranch$cmp56336
truebranch$cmp56336:
%ae51193 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56337 = alloca %struct.ScmObj*, align 8
%anf_45bind47336 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4707047198, %struct.ScmObj* %ae51193)
store volatile %struct.ScmObj* %anf_45bind47336, %struct.ScmObj** %stackaddr$prim56337, align 8
%truthy$cmp56338 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47336)
%cmp$cmp56338 = icmp eq i64 %truthy$cmp56338, 1
br i1 %cmp$cmp56338, label %truebranch$cmp56338, label %falsebranch$cmp56338
truebranch$cmp56338:
%ae51196 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim56339 = alloca %struct.ScmObj*, align 8
%cpsprim47390 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4707047198, %struct.ScmObj* %ae51196)
store volatile %struct.ScmObj* %cpsprim47390, %struct.ScmObj** %stackaddr$prim56339, align 8
%stackaddr$makeclosure56340 = alloca %struct.ScmObj*, align 8
%fptrToInt56341 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51197 to i64
%ae51197 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56341)
store volatile %struct.ScmObj* %ae51197, %struct.ScmObj** %stackaddr$makeclosure56340, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51197, %struct.ScmObj* %k47381, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51197, %struct.ScmObj* %anf_45bind47329, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51197, %struct.ScmObj* %anf_45bind47328, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51197, %struct.ScmObj* %n47195, i64 3)
%ae51198 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55353$ae511970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56342 = alloca %struct.ScmObj*, align 8
%argslist55353$ae511971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47390, %struct.ScmObj* %argslist55353$ae511970)
store volatile %struct.ScmObj* %argslist55353$ae511971, %struct.ScmObj** %stackaddr$prim56342, align 8
%stackaddr$prim56343 = alloca %struct.ScmObj*, align 8
%argslist55353$ae511972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51198, %struct.ScmObj* %argslist55353$ae511971)
store volatile %struct.ScmObj* %argslist55353$ae511972, %struct.ScmObj** %stackaddr$prim56343, align 8
%clofunc56344 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51197)
musttail call tailcc void %clofunc56344(%struct.ScmObj* %ae51197, %struct.ScmObj* %argslist55353$ae511972)
ret void
falsebranch$cmp56338:
%ae51252 = call %struct.ScmObj* @const_init_int(i64 1)
%ae51253 = call %struct.ScmObj* @const_init_true()
%stackaddr$prim56345 = alloca %struct.ScmObj*, align 8
%t4707247200 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4707047198, %struct.ScmObj* %ae51252, %struct.ScmObj* %ae51253)
store volatile %struct.ScmObj* %t4707247200, %struct.ScmObj** %stackaddr$prim56345, align 8
%ae51255 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim56346 = alloca %struct.ScmObj*, align 8
%anf_45bind47337 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4707047198, %struct.ScmObj* %ae51255)
store volatile %struct.ScmObj* %anf_45bind47337, %struct.ScmObj** %stackaddr$prim56346, align 8
%stackaddr$makeclosure56347 = alloca %struct.ScmObj*, align 8
%fptrToInt56348 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51257 to i64
%ae51257 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56348)
store volatile %struct.ScmObj* %ae51257, %struct.ScmObj** %stackaddr$makeclosure56347, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51257, %struct.ScmObj* %k47381, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51257, %struct.ScmObj* %anf_45bind47329, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51257, %struct.ScmObj* %anf_45bind47328, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51257, %struct.ScmObj* %thunk4707047198, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51257, %struct.ScmObj* %n47195, i64 4)
%ae51258 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @global$sym$ae5125856349, i32 0, i32 0))
%argslist55366$anf_45bind473370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56350 = alloca %struct.ScmObj*, align 8
%argslist55366$anf_45bind473371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51258, %struct.ScmObj* %argslist55366$anf_45bind473370)
store volatile %struct.ScmObj* %argslist55366$anf_45bind473371, %struct.ScmObj** %stackaddr$prim56350, align 8
%stackaddr$prim56351 = alloca %struct.ScmObj*, align 8
%argslist55366$anf_45bind473372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51257, %struct.ScmObj* %argslist55366$anf_45bind473371)
store volatile %struct.ScmObj* %argslist55366$anf_45bind473372, %struct.ScmObj** %stackaddr$prim56351, align 8
%clofunc56352 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47337)
musttail call tailcc void %clofunc56352(%struct.ScmObj* %anf_45bind47337, %struct.ScmObj* %argslist55366$anf_45bind473372)
ret void
falsebranch$cmp56336:
%stackaddr$prim56353 = alloca %struct.ScmObj*, align 8
%anf_45bind47338 = call %struct.ScmObj* @prim_number_63(%struct.ScmObj* %thunk4707047198)
store volatile %struct.ScmObj* %anf_45bind47338, %struct.ScmObj** %stackaddr$prim56353, align 8
%truthy$cmp56354 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47338)
%cmp$cmp56354 = icmp eq i64 %truthy$cmp56354, 1
br i1 %cmp$cmp56354, label %truebranch$cmp56354, label %falsebranch$cmp56354
truebranch$cmp56354:
%stackaddr$makeclosure56355 = alloca %struct.ScmObj*, align 8
%fptrToInt56356 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51469 to i64
%ae51469 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56356)
store volatile %struct.ScmObj* %ae51469, %struct.ScmObj** %stackaddr$makeclosure56355, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51469, %struct.ScmObj* %k47381, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51469, %struct.ScmObj* %anf_45bind47329, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51469, %struct.ScmObj* %anf_45bind47328, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51469, %struct.ScmObj* %n47195, i64 3)
%ae51470 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55375$ae514690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56357 = alloca %struct.ScmObj*, align 8
%argslist55375$ae514691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4707047198, %struct.ScmObj* %argslist55375$ae514690)
store volatile %struct.ScmObj* %argslist55375$ae514691, %struct.ScmObj** %stackaddr$prim56357, align 8
%stackaddr$prim56358 = alloca %struct.ScmObj*, align 8
%argslist55375$ae514692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51470, %struct.ScmObj* %argslist55375$ae514691)
store volatile %struct.ScmObj* %argslist55375$ae514692, %struct.ScmObj** %stackaddr$prim56358, align 8
%clofunc56359 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51469)
musttail call tailcc void %clofunc56359(%struct.ScmObj* %ae51469, %struct.ScmObj* %argslist55375$ae514692)
ret void
falsebranch$cmp56354:
%stackaddr$makeclosure56360 = alloca %struct.ScmObj*, align 8
%fptrToInt56361 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51507 to i64
%ae51507 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56361)
store volatile %struct.ScmObj* %ae51507, %struct.ScmObj** %stackaddr$makeclosure56360, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51507, %struct.ScmObj* %k47381, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51507, %struct.ScmObj* %anf_45bind47329, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51507, %struct.ScmObj* %anf_45bind47328, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51507, %struct.ScmObj* %n47195, i64 3)
%ae51508 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51509 = call %struct.ScmObj* @const_init_string(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @global$str$ae5150956362, i32 0, i32 0))
%argslist55384$ae515070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56363 = alloca %struct.ScmObj*, align 8
%argslist55384$ae515071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51509, %struct.ScmObj* %argslist55384$ae515070)
store volatile %struct.ScmObj* %argslist55384$ae515071, %struct.ScmObj** %stackaddr$prim56363, align 8
%stackaddr$prim56364 = alloca %struct.ScmObj*, align 8
%argslist55384$ae515072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51508, %struct.ScmObj* %argslist55384$ae515071)
store volatile %struct.ScmObj* %argslist55384$ae515072, %struct.ScmObj** %stackaddr$prim56364, align 8
%clofunc56365 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51507)
musttail call tailcc void %clofunc56365(%struct.ScmObj* %ae51507, %struct.ScmObj* %argslist55384$ae515072)
ret void
}

define tailcc void @proc_clo$ae51197(%struct.ScmObj* %env$ae51197,%struct.ScmObj* %current_45args55345) {
%stackaddr$env-ref56366 = alloca %struct.ScmObj*, align 8
%k47381 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51197, i64 0)
store %struct.ScmObj* %k47381, %struct.ScmObj** %stackaddr$env-ref56366
%stackaddr$env-ref56367 = alloca %struct.ScmObj*, align 8
%anf_45bind47329 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51197, i64 1)
store %struct.ScmObj* %anf_45bind47329, %struct.ScmObj** %stackaddr$env-ref56367
%stackaddr$env-ref56368 = alloca %struct.ScmObj*, align 8
%anf_45bind47328 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51197, i64 2)
store %struct.ScmObj* %anf_45bind47328, %struct.ScmObj** %stackaddr$env-ref56368
%stackaddr$env-ref56369 = alloca %struct.ScmObj*, align 8
%n47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51197, i64 3)
store %struct.ScmObj* %n47195, %struct.ScmObj** %stackaddr$env-ref56369
%stackaddr$prim56370 = alloca %struct.ScmObj*, align 8
%_95k47387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55345)
store volatile %struct.ScmObj* %_95k47387, %struct.ScmObj** %stackaddr$prim56370, align 8
%stackaddr$prim56371 = alloca %struct.ScmObj*, align 8
%current_45args55346 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55345)
store volatile %struct.ScmObj* %current_45args55346, %struct.ScmObj** %stackaddr$prim56371, align 8
%stackaddr$prim56372 = alloca %struct.ScmObj*, align 8
%anf_45bind47339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55346)
store volatile %struct.ScmObj* %anf_45bind47339, %struct.ScmObj** %stackaddr$prim56372, align 8
%ae51204 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56373 = alloca %struct.ScmObj*, align 8
%anf_45bind47340 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n47195, %struct.ScmObj* %ae51204)
store volatile %struct.ScmObj* %anf_45bind47340, %struct.ScmObj** %stackaddr$prim56373, align 8
%stackaddr$makeclosure56374 = alloca %struct.ScmObj*, align 8
%fptrToInt56375 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51206 to i64
%ae51206 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56375)
store volatile %struct.ScmObj* %ae51206, %struct.ScmObj** %stackaddr$makeclosure56374, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51206, %struct.ScmObj* %k47381, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51206, %struct.ScmObj* %anf_45bind47328, i64 1)
%argslist55352$anf_45bind473290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56376 = alloca %struct.ScmObj*, align 8
%argslist55352$anf_45bind473291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47340, %struct.ScmObj* %argslist55352$anf_45bind473290)
store volatile %struct.ScmObj* %argslist55352$anf_45bind473291, %struct.ScmObj** %stackaddr$prim56376, align 8
%stackaddr$prim56377 = alloca %struct.ScmObj*, align 8
%argslist55352$anf_45bind473292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47339, %struct.ScmObj* %argslist55352$anf_45bind473291)
store volatile %struct.ScmObj* %argslist55352$anf_45bind473292, %struct.ScmObj** %stackaddr$prim56377, align 8
%stackaddr$prim56378 = alloca %struct.ScmObj*, align 8
%argslist55352$anf_45bind473293 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51206, %struct.ScmObj* %argslist55352$anf_45bind473292)
store volatile %struct.ScmObj* %argslist55352$anf_45bind473293, %struct.ScmObj** %stackaddr$prim56378, align 8
%clofunc56379 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47329)
musttail call tailcc void %clofunc56379(%struct.ScmObj* %anf_45bind47329, %struct.ScmObj* %argslist55352$anf_45bind473293)
ret void
}

define tailcc void @proc_clo$ae51206(%struct.ScmObj* %env$ae51206,%struct.ScmObj* %current_45args55348) {
%stackaddr$env-ref56380 = alloca %struct.ScmObj*, align 8
%k47381 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51206, i64 0)
store %struct.ScmObj* %k47381, %struct.ScmObj** %stackaddr$env-ref56380
%stackaddr$env-ref56381 = alloca %struct.ScmObj*, align 8
%anf_45bind47328 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51206, i64 1)
store %struct.ScmObj* %anf_45bind47328, %struct.ScmObj** %stackaddr$env-ref56381
%stackaddr$prim56382 = alloca %struct.ScmObj*, align 8
%_95k47388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55348)
store volatile %struct.ScmObj* %_95k47388, %struct.ScmObj** %stackaddr$prim56382, align 8
%stackaddr$prim56383 = alloca %struct.ScmObj*, align 8
%current_45args55349 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55348)
store volatile %struct.ScmObj* %current_45args55349, %struct.ScmObj** %stackaddr$prim56383, align 8
%stackaddr$prim56384 = alloca %struct.ScmObj*, align 8
%anf_45bind47341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55349)
store volatile %struct.ScmObj* %anf_45bind47341, %struct.ScmObj** %stackaddr$prim56384, align 8
%stackaddr$prim56385 = alloca %struct.ScmObj*, align 8
%cpsprim47389 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47328, %struct.ScmObj* %anf_45bind47341)
store volatile %struct.ScmObj* %cpsprim47389, %struct.ScmObj** %stackaddr$prim56385, align 8
%ae51212 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55351$k473810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56386 = alloca %struct.ScmObj*, align 8
%argslist55351$k473811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47389, %struct.ScmObj* %argslist55351$k473810)
store volatile %struct.ScmObj* %argslist55351$k473811, %struct.ScmObj** %stackaddr$prim56386, align 8
%stackaddr$prim56387 = alloca %struct.ScmObj*, align 8
%argslist55351$k473812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51212, %struct.ScmObj* %argslist55351$k473811)
store volatile %struct.ScmObj* %argslist55351$k473812, %struct.ScmObj** %stackaddr$prim56387, align 8
%clofunc56388 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47381)
musttail call tailcc void %clofunc56388(%struct.ScmObj* %k47381, %struct.ScmObj* %argslist55351$k473812)
ret void
}

define tailcc void @proc_clo$ae51257(%struct.ScmObj* %env$ae51257,%struct.ScmObj* %current_45args55354) {
%stackaddr$env-ref56389 = alloca %struct.ScmObj*, align 8
%k47381 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51257, i64 0)
store %struct.ScmObj* %k47381, %struct.ScmObj** %stackaddr$env-ref56389
%stackaddr$env-ref56390 = alloca %struct.ScmObj*, align 8
%anf_45bind47329 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51257, i64 1)
store %struct.ScmObj* %anf_45bind47329, %struct.ScmObj** %stackaddr$env-ref56390
%stackaddr$env-ref56391 = alloca %struct.ScmObj*, align 8
%anf_45bind47328 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51257, i64 2)
store %struct.ScmObj* %anf_45bind47328, %struct.ScmObj** %stackaddr$env-ref56391
%stackaddr$env-ref56392 = alloca %struct.ScmObj*, align 8
%thunk4707047198 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51257, i64 3)
store %struct.ScmObj* %thunk4707047198, %struct.ScmObj** %stackaddr$env-ref56392
%stackaddr$env-ref56393 = alloca %struct.ScmObj*, align 8
%n47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51257, i64 4)
store %struct.ScmObj* %n47195, %struct.ScmObj** %stackaddr$env-ref56393
%stackaddr$prim56394 = alloca %struct.ScmObj*, align 8
%_95k47391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55354)
store volatile %struct.ScmObj* %_95k47391, %struct.ScmObj** %stackaddr$prim56394, align 8
%stackaddr$prim56395 = alloca %struct.ScmObj*, align 8
%current_45args55355 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55354)
store volatile %struct.ScmObj* %current_45args55355, %struct.ScmObj** %stackaddr$prim56395, align 8
%stackaddr$prim56396 = alloca %struct.ScmObj*, align 8
%val4707147202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55355)
store volatile %struct.ScmObj* %val4707147202, %struct.ScmObj** %stackaddr$prim56396, align 8
%ae51263 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim56397 = alloca %struct.ScmObj*, align 8
%t4707347201 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4707047198, %struct.ScmObj* %ae51263, %struct.ScmObj* %val4707147202)
store volatile %struct.ScmObj* %t4707347201, %struct.ScmObj** %stackaddr$prim56397, align 8
%ae51266 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim56398 = alloca %struct.ScmObj*, align 8
%cpsprim47392 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4707047198, %struct.ScmObj* %ae51266)
store volatile %struct.ScmObj* %cpsprim47392, %struct.ScmObj** %stackaddr$prim56398, align 8
%stackaddr$makeclosure56399 = alloca %struct.ScmObj*, align 8
%fptrToInt56400 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51267 to i64
%ae51267 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56400)
store volatile %struct.ScmObj* %ae51267, %struct.ScmObj** %stackaddr$makeclosure56399, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51267, %struct.ScmObj* %k47381, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51267, %struct.ScmObj* %anf_45bind47329, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51267, %struct.ScmObj* %anf_45bind47328, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51267, %struct.ScmObj* %n47195, i64 3)
%ae51268 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55365$ae512670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56401 = alloca %struct.ScmObj*, align 8
%argslist55365$ae512671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47392, %struct.ScmObj* %argslist55365$ae512670)
store volatile %struct.ScmObj* %argslist55365$ae512671, %struct.ScmObj** %stackaddr$prim56401, align 8
%stackaddr$prim56402 = alloca %struct.ScmObj*, align 8
%argslist55365$ae512672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51268, %struct.ScmObj* %argslist55365$ae512671)
store volatile %struct.ScmObj* %argslist55365$ae512672, %struct.ScmObj** %stackaddr$prim56402, align 8
%clofunc56403 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51267)
musttail call tailcc void %clofunc56403(%struct.ScmObj* %ae51267, %struct.ScmObj* %argslist55365$ae512672)
ret void
}

define tailcc void @proc_clo$ae51267(%struct.ScmObj* %env$ae51267,%struct.ScmObj* %current_45args55357) {
%stackaddr$env-ref56404 = alloca %struct.ScmObj*, align 8
%k47381 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51267, i64 0)
store %struct.ScmObj* %k47381, %struct.ScmObj** %stackaddr$env-ref56404
%stackaddr$env-ref56405 = alloca %struct.ScmObj*, align 8
%anf_45bind47329 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51267, i64 1)
store %struct.ScmObj* %anf_45bind47329, %struct.ScmObj** %stackaddr$env-ref56405
%stackaddr$env-ref56406 = alloca %struct.ScmObj*, align 8
%anf_45bind47328 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51267, i64 2)
store %struct.ScmObj* %anf_45bind47328, %struct.ScmObj** %stackaddr$env-ref56406
%stackaddr$env-ref56407 = alloca %struct.ScmObj*, align 8
%n47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51267, i64 3)
store %struct.ScmObj* %n47195, %struct.ScmObj** %stackaddr$env-ref56407
%stackaddr$prim56408 = alloca %struct.ScmObj*, align 8
%_95k47387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55357)
store volatile %struct.ScmObj* %_95k47387, %struct.ScmObj** %stackaddr$prim56408, align 8
%stackaddr$prim56409 = alloca %struct.ScmObj*, align 8
%current_45args55358 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55357)
store volatile %struct.ScmObj* %current_45args55358, %struct.ScmObj** %stackaddr$prim56409, align 8
%stackaddr$prim56410 = alloca %struct.ScmObj*, align 8
%anf_45bind47339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55358)
store volatile %struct.ScmObj* %anf_45bind47339, %struct.ScmObj** %stackaddr$prim56410, align 8
%ae51274 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56411 = alloca %struct.ScmObj*, align 8
%anf_45bind47340 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n47195, %struct.ScmObj* %ae51274)
store volatile %struct.ScmObj* %anf_45bind47340, %struct.ScmObj** %stackaddr$prim56411, align 8
%stackaddr$makeclosure56412 = alloca %struct.ScmObj*, align 8
%fptrToInt56413 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51276 to i64
%ae51276 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56413)
store volatile %struct.ScmObj* %ae51276, %struct.ScmObj** %stackaddr$makeclosure56412, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51276, %struct.ScmObj* %k47381, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51276, %struct.ScmObj* %anf_45bind47328, i64 1)
%argslist55364$anf_45bind473290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56414 = alloca %struct.ScmObj*, align 8
%argslist55364$anf_45bind473291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47340, %struct.ScmObj* %argslist55364$anf_45bind473290)
store volatile %struct.ScmObj* %argslist55364$anf_45bind473291, %struct.ScmObj** %stackaddr$prim56414, align 8
%stackaddr$prim56415 = alloca %struct.ScmObj*, align 8
%argslist55364$anf_45bind473292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47339, %struct.ScmObj* %argslist55364$anf_45bind473291)
store volatile %struct.ScmObj* %argslist55364$anf_45bind473292, %struct.ScmObj** %stackaddr$prim56415, align 8
%stackaddr$prim56416 = alloca %struct.ScmObj*, align 8
%argslist55364$anf_45bind473293 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51276, %struct.ScmObj* %argslist55364$anf_45bind473292)
store volatile %struct.ScmObj* %argslist55364$anf_45bind473293, %struct.ScmObj** %stackaddr$prim56416, align 8
%clofunc56417 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47329)
musttail call tailcc void %clofunc56417(%struct.ScmObj* %anf_45bind47329, %struct.ScmObj* %argslist55364$anf_45bind473293)
ret void
}

define tailcc void @proc_clo$ae51276(%struct.ScmObj* %env$ae51276,%struct.ScmObj* %current_45args55360) {
%stackaddr$env-ref56418 = alloca %struct.ScmObj*, align 8
%k47381 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51276, i64 0)
store %struct.ScmObj* %k47381, %struct.ScmObj** %stackaddr$env-ref56418
%stackaddr$env-ref56419 = alloca %struct.ScmObj*, align 8
%anf_45bind47328 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51276, i64 1)
store %struct.ScmObj* %anf_45bind47328, %struct.ScmObj** %stackaddr$env-ref56419
%stackaddr$prim56420 = alloca %struct.ScmObj*, align 8
%_95k47388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55360)
store volatile %struct.ScmObj* %_95k47388, %struct.ScmObj** %stackaddr$prim56420, align 8
%stackaddr$prim56421 = alloca %struct.ScmObj*, align 8
%current_45args55361 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55360)
store volatile %struct.ScmObj* %current_45args55361, %struct.ScmObj** %stackaddr$prim56421, align 8
%stackaddr$prim56422 = alloca %struct.ScmObj*, align 8
%anf_45bind47341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55361)
store volatile %struct.ScmObj* %anf_45bind47341, %struct.ScmObj** %stackaddr$prim56422, align 8
%stackaddr$prim56423 = alloca %struct.ScmObj*, align 8
%cpsprim47389 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47328, %struct.ScmObj* %anf_45bind47341)
store volatile %struct.ScmObj* %cpsprim47389, %struct.ScmObj** %stackaddr$prim56423, align 8
%ae51282 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55363$k473810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56424 = alloca %struct.ScmObj*, align 8
%argslist55363$k473811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47389, %struct.ScmObj* %argslist55363$k473810)
store volatile %struct.ScmObj* %argslist55363$k473811, %struct.ScmObj** %stackaddr$prim56424, align 8
%stackaddr$prim56425 = alloca %struct.ScmObj*, align 8
%argslist55363$k473812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51282, %struct.ScmObj* %argslist55363$k473811)
store volatile %struct.ScmObj* %argslist55363$k473812, %struct.ScmObj** %stackaddr$prim56425, align 8
%clofunc56426 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47381)
musttail call tailcc void %clofunc56426(%struct.ScmObj* %k47381, %struct.ScmObj* %argslist55363$k473812)
ret void
}

define tailcc void @proc_clo$ae51469(%struct.ScmObj* %env$ae51469,%struct.ScmObj* %current_45args55367) {
%stackaddr$env-ref56427 = alloca %struct.ScmObj*, align 8
%k47381 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51469, i64 0)
store %struct.ScmObj* %k47381, %struct.ScmObj** %stackaddr$env-ref56427
%stackaddr$env-ref56428 = alloca %struct.ScmObj*, align 8
%anf_45bind47329 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51469, i64 1)
store %struct.ScmObj* %anf_45bind47329, %struct.ScmObj** %stackaddr$env-ref56428
%stackaddr$env-ref56429 = alloca %struct.ScmObj*, align 8
%anf_45bind47328 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51469, i64 2)
store %struct.ScmObj* %anf_45bind47328, %struct.ScmObj** %stackaddr$env-ref56429
%stackaddr$env-ref56430 = alloca %struct.ScmObj*, align 8
%n47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51469, i64 3)
store %struct.ScmObj* %n47195, %struct.ScmObj** %stackaddr$env-ref56430
%stackaddr$prim56431 = alloca %struct.ScmObj*, align 8
%_95k47387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55367)
store volatile %struct.ScmObj* %_95k47387, %struct.ScmObj** %stackaddr$prim56431, align 8
%stackaddr$prim56432 = alloca %struct.ScmObj*, align 8
%current_45args55368 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55367)
store volatile %struct.ScmObj* %current_45args55368, %struct.ScmObj** %stackaddr$prim56432, align 8
%stackaddr$prim56433 = alloca %struct.ScmObj*, align 8
%anf_45bind47339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55368)
store volatile %struct.ScmObj* %anf_45bind47339, %struct.ScmObj** %stackaddr$prim56433, align 8
%ae51476 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56434 = alloca %struct.ScmObj*, align 8
%anf_45bind47340 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n47195, %struct.ScmObj* %ae51476)
store volatile %struct.ScmObj* %anf_45bind47340, %struct.ScmObj** %stackaddr$prim56434, align 8
%stackaddr$makeclosure56435 = alloca %struct.ScmObj*, align 8
%fptrToInt56436 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51478 to i64
%ae51478 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56436)
store volatile %struct.ScmObj* %ae51478, %struct.ScmObj** %stackaddr$makeclosure56435, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51478, %struct.ScmObj* %k47381, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51478, %struct.ScmObj* %anf_45bind47328, i64 1)
%argslist55374$anf_45bind473290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56437 = alloca %struct.ScmObj*, align 8
%argslist55374$anf_45bind473291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47340, %struct.ScmObj* %argslist55374$anf_45bind473290)
store volatile %struct.ScmObj* %argslist55374$anf_45bind473291, %struct.ScmObj** %stackaddr$prim56437, align 8
%stackaddr$prim56438 = alloca %struct.ScmObj*, align 8
%argslist55374$anf_45bind473292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47339, %struct.ScmObj* %argslist55374$anf_45bind473291)
store volatile %struct.ScmObj* %argslist55374$anf_45bind473292, %struct.ScmObj** %stackaddr$prim56438, align 8
%stackaddr$prim56439 = alloca %struct.ScmObj*, align 8
%argslist55374$anf_45bind473293 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51478, %struct.ScmObj* %argslist55374$anf_45bind473292)
store volatile %struct.ScmObj* %argslist55374$anf_45bind473293, %struct.ScmObj** %stackaddr$prim56439, align 8
%clofunc56440 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47329)
musttail call tailcc void %clofunc56440(%struct.ScmObj* %anf_45bind47329, %struct.ScmObj* %argslist55374$anf_45bind473293)
ret void
}

define tailcc void @proc_clo$ae51478(%struct.ScmObj* %env$ae51478,%struct.ScmObj* %current_45args55370) {
%stackaddr$env-ref56441 = alloca %struct.ScmObj*, align 8
%k47381 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51478, i64 0)
store %struct.ScmObj* %k47381, %struct.ScmObj** %stackaddr$env-ref56441
%stackaddr$env-ref56442 = alloca %struct.ScmObj*, align 8
%anf_45bind47328 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51478, i64 1)
store %struct.ScmObj* %anf_45bind47328, %struct.ScmObj** %stackaddr$env-ref56442
%stackaddr$prim56443 = alloca %struct.ScmObj*, align 8
%_95k47388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55370)
store volatile %struct.ScmObj* %_95k47388, %struct.ScmObj** %stackaddr$prim56443, align 8
%stackaddr$prim56444 = alloca %struct.ScmObj*, align 8
%current_45args55371 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55370)
store volatile %struct.ScmObj* %current_45args55371, %struct.ScmObj** %stackaddr$prim56444, align 8
%stackaddr$prim56445 = alloca %struct.ScmObj*, align 8
%anf_45bind47341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55371)
store volatile %struct.ScmObj* %anf_45bind47341, %struct.ScmObj** %stackaddr$prim56445, align 8
%stackaddr$prim56446 = alloca %struct.ScmObj*, align 8
%cpsprim47389 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47328, %struct.ScmObj* %anf_45bind47341)
store volatile %struct.ScmObj* %cpsprim47389, %struct.ScmObj** %stackaddr$prim56446, align 8
%ae51484 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55373$k473810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56447 = alloca %struct.ScmObj*, align 8
%argslist55373$k473811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47389, %struct.ScmObj* %argslist55373$k473810)
store volatile %struct.ScmObj* %argslist55373$k473811, %struct.ScmObj** %stackaddr$prim56447, align 8
%stackaddr$prim56448 = alloca %struct.ScmObj*, align 8
%argslist55373$k473812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51484, %struct.ScmObj* %argslist55373$k473811)
store volatile %struct.ScmObj* %argslist55373$k473812, %struct.ScmObj** %stackaddr$prim56448, align 8
%clofunc56449 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47381)
musttail call tailcc void %clofunc56449(%struct.ScmObj* %k47381, %struct.ScmObj* %argslist55373$k473812)
ret void
}

define tailcc void @proc_clo$ae51507(%struct.ScmObj* %env$ae51507,%struct.ScmObj* %current_45args55376) {
%stackaddr$env-ref56450 = alloca %struct.ScmObj*, align 8
%k47381 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51507, i64 0)
store %struct.ScmObj* %k47381, %struct.ScmObj** %stackaddr$env-ref56450
%stackaddr$env-ref56451 = alloca %struct.ScmObj*, align 8
%anf_45bind47329 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51507, i64 1)
store %struct.ScmObj* %anf_45bind47329, %struct.ScmObj** %stackaddr$env-ref56451
%stackaddr$env-ref56452 = alloca %struct.ScmObj*, align 8
%anf_45bind47328 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51507, i64 2)
store %struct.ScmObj* %anf_45bind47328, %struct.ScmObj** %stackaddr$env-ref56452
%stackaddr$env-ref56453 = alloca %struct.ScmObj*, align 8
%n47195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51507, i64 3)
store %struct.ScmObj* %n47195, %struct.ScmObj** %stackaddr$env-ref56453
%stackaddr$prim56454 = alloca %struct.ScmObj*, align 8
%_95k47387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55376)
store volatile %struct.ScmObj* %_95k47387, %struct.ScmObj** %stackaddr$prim56454, align 8
%stackaddr$prim56455 = alloca %struct.ScmObj*, align 8
%current_45args55377 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55376)
store volatile %struct.ScmObj* %current_45args55377, %struct.ScmObj** %stackaddr$prim56455, align 8
%stackaddr$prim56456 = alloca %struct.ScmObj*, align 8
%anf_45bind47339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55377)
store volatile %struct.ScmObj* %anf_45bind47339, %struct.ScmObj** %stackaddr$prim56456, align 8
%ae51517 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56457 = alloca %struct.ScmObj*, align 8
%anf_45bind47340 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n47195, %struct.ScmObj* %ae51517)
store volatile %struct.ScmObj* %anf_45bind47340, %struct.ScmObj** %stackaddr$prim56457, align 8
%stackaddr$makeclosure56458 = alloca %struct.ScmObj*, align 8
%fptrToInt56459 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51519 to i64
%ae51519 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56459)
store volatile %struct.ScmObj* %ae51519, %struct.ScmObj** %stackaddr$makeclosure56458, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51519, %struct.ScmObj* %k47381, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51519, %struct.ScmObj* %anf_45bind47328, i64 1)
%argslist55383$anf_45bind473290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56460 = alloca %struct.ScmObj*, align 8
%argslist55383$anf_45bind473291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47340, %struct.ScmObj* %argslist55383$anf_45bind473290)
store volatile %struct.ScmObj* %argslist55383$anf_45bind473291, %struct.ScmObj** %stackaddr$prim56460, align 8
%stackaddr$prim56461 = alloca %struct.ScmObj*, align 8
%argslist55383$anf_45bind473292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47339, %struct.ScmObj* %argslist55383$anf_45bind473291)
store volatile %struct.ScmObj* %argslist55383$anf_45bind473292, %struct.ScmObj** %stackaddr$prim56461, align 8
%stackaddr$prim56462 = alloca %struct.ScmObj*, align 8
%argslist55383$anf_45bind473293 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51519, %struct.ScmObj* %argslist55383$anf_45bind473292)
store volatile %struct.ScmObj* %argslist55383$anf_45bind473293, %struct.ScmObj** %stackaddr$prim56462, align 8
%clofunc56463 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47329)
musttail call tailcc void %clofunc56463(%struct.ScmObj* %anf_45bind47329, %struct.ScmObj* %argslist55383$anf_45bind473293)
ret void
}

define tailcc void @proc_clo$ae51519(%struct.ScmObj* %env$ae51519,%struct.ScmObj* %current_45args55379) {
%stackaddr$env-ref56464 = alloca %struct.ScmObj*, align 8
%k47381 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51519, i64 0)
store %struct.ScmObj* %k47381, %struct.ScmObj** %stackaddr$env-ref56464
%stackaddr$env-ref56465 = alloca %struct.ScmObj*, align 8
%anf_45bind47328 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51519, i64 1)
store %struct.ScmObj* %anf_45bind47328, %struct.ScmObj** %stackaddr$env-ref56465
%stackaddr$prim56466 = alloca %struct.ScmObj*, align 8
%_95k47388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55379)
store volatile %struct.ScmObj* %_95k47388, %struct.ScmObj** %stackaddr$prim56466, align 8
%stackaddr$prim56467 = alloca %struct.ScmObj*, align 8
%current_45args55380 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55379)
store volatile %struct.ScmObj* %current_45args55380, %struct.ScmObj** %stackaddr$prim56467, align 8
%stackaddr$prim56468 = alloca %struct.ScmObj*, align 8
%anf_45bind47341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55380)
store volatile %struct.ScmObj* %anf_45bind47341, %struct.ScmObj** %stackaddr$prim56468, align 8
%stackaddr$prim56469 = alloca %struct.ScmObj*, align 8
%cpsprim47389 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47328, %struct.ScmObj* %anf_45bind47341)
store volatile %struct.ScmObj* %cpsprim47389, %struct.ScmObj** %stackaddr$prim56469, align 8
%ae51525 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55382$k473810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56470 = alloca %struct.ScmObj*, align 8
%argslist55382$k473811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47389, %struct.ScmObj* %argslist55382$k473810)
store volatile %struct.ScmObj* %argslist55382$k473811, %struct.ScmObj** %stackaddr$prim56470, align 8
%stackaddr$prim56471 = alloca %struct.ScmObj*, align 8
%argslist55382$k473812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51525, %struct.ScmObj* %argslist55382$k473811)
store volatile %struct.ScmObj* %argslist55382$k473812, %struct.ScmObj** %stackaddr$prim56471, align 8
%clofunc56472 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47381)
musttail call tailcc void %clofunc56472(%struct.ScmObj* %k47381, %struct.ScmObj* %argslist55382$k473812)
ret void
}

define tailcc void @proc_clo$ae51103(%struct.ScmObj* %env$ae51103,%struct.ScmObj* %current_45args55386) {
%stackaddr$prim56473 = alloca %struct.ScmObj*, align 8
%k47393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55386)
store volatile %struct.ScmObj* %k47393, %struct.ScmObj** %stackaddr$prim56473, align 8
%stackaddr$prim56474 = alloca %struct.ScmObj*, align 8
%current_45args55387 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55386)
store volatile %struct.ScmObj* %current_45args55387, %struct.ScmObj** %stackaddr$prim56474, align 8
%stackaddr$prim56475 = alloca %struct.ScmObj*, align 8
%thunk47199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55387)
store volatile %struct.ScmObj* %thunk47199, %struct.ScmObj** %stackaddr$prim56475, align 8
%stackaddr$prim56476 = alloca %struct.ScmObj*, align 8
%anf_45bind47330 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk47199)
store volatile %struct.ScmObj* %anf_45bind47330, %struct.ScmObj** %stackaddr$prim56476, align 8
%truthy$cmp56477 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47330)
%cmp$cmp56477 = icmp eq i64 %truthy$cmp56477, 1
br i1 %cmp$cmp56477, label %truebranch$cmp56477, label %falsebranch$cmp56477
truebranch$cmp56477:
%stackaddr$prim56478 = alloca %struct.ScmObj*, align 8
%anf_45bind47331 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk47199)
store volatile %struct.ScmObj* %anf_45bind47331, %struct.ScmObj** %stackaddr$prim56478, align 8
%ae51108 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim56479 = alloca %struct.ScmObj*, align 8
%anf_45bind47332 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind47331, %struct.ScmObj* %ae51108)
store volatile %struct.ScmObj* %anf_45bind47332, %struct.ScmObj** %stackaddr$prim56479, align 8
%truthy$cmp56480 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47332)
%cmp$cmp56480 = icmp eq i64 %truthy$cmp56480, 1
br i1 %cmp$cmp56480, label %truebranch$cmp56480, label %falsebranch$cmp56480
truebranch$cmp56480:
%ae51111 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56481 = alloca %struct.ScmObj*, align 8
%anf_45bind47333 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk47199, %struct.ScmObj* %ae51111)
store volatile %struct.ScmObj* %anf_45bind47333, %struct.ScmObj** %stackaddr$prim56481, align 8
%ae51113 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae5111356482, i32 0, i32 0))
%stackaddr$prim56483 = alloca %struct.ScmObj*, align 8
%cpsprim47394 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind47333, %struct.ScmObj* %ae51113)
store volatile %struct.ScmObj* %cpsprim47394, %struct.ScmObj** %stackaddr$prim56483, align 8
%ae51115 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55389$k473930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56484 = alloca %struct.ScmObj*, align 8
%argslist55389$k473931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47394, %struct.ScmObj* %argslist55389$k473930)
store volatile %struct.ScmObj* %argslist55389$k473931, %struct.ScmObj** %stackaddr$prim56484, align 8
%stackaddr$prim56485 = alloca %struct.ScmObj*, align 8
%argslist55389$k473932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51115, %struct.ScmObj* %argslist55389$k473931)
store volatile %struct.ScmObj* %argslist55389$k473932, %struct.ScmObj** %stackaddr$prim56485, align 8
%clofunc56486 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47393)
musttail call tailcc void %clofunc56486(%struct.ScmObj* %k47393, %struct.ScmObj* %argslist55389$k473932)
ret void
falsebranch$cmp56480:
%ae51133 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51134 = call %struct.ScmObj* @const_init_false()
%argslist55390$k473930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56487 = alloca %struct.ScmObj*, align 8
%argslist55390$k473931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51134, %struct.ScmObj* %argslist55390$k473930)
store volatile %struct.ScmObj* %argslist55390$k473931, %struct.ScmObj** %stackaddr$prim56487, align 8
%stackaddr$prim56488 = alloca %struct.ScmObj*, align 8
%argslist55390$k473932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51133, %struct.ScmObj* %argslist55390$k473931)
store volatile %struct.ScmObj* %argslist55390$k473932, %struct.ScmObj** %stackaddr$prim56488, align 8
%clofunc56489 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47393)
musttail call tailcc void %clofunc56489(%struct.ScmObj* %k47393, %struct.ScmObj* %argslist55390$k473932)
ret void
falsebranch$cmp56477:
%ae51155 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51156 = call %struct.ScmObj* @const_init_false()
%argslist55391$k473930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56490 = alloca %struct.ScmObj*, align 8
%argslist55391$k473931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51156, %struct.ScmObj* %argslist55391$k473930)
store volatile %struct.ScmObj* %argslist55391$k473931, %struct.ScmObj** %stackaddr$prim56490, align 8
%stackaddr$prim56491 = alloca %struct.ScmObj*, align 8
%argslist55391$k473932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51155, %struct.ScmObj* %argslist55391$k473931)
store volatile %struct.ScmObj* %argslist55391$k473932, %struct.ScmObj** %stackaddr$prim56491, align 8
%clofunc56492 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47393)
musttail call tailcc void %clofunc56492(%struct.ScmObj* %k47393, %struct.ScmObj* %argslist55391$k473932)
ret void
}

define tailcc void @proc_clo$ae50914(%struct.ScmObj* %env$ae50914,%struct.ScmObj* %current_45args55394) {
%stackaddr$env-ref56493 = alloca %struct.ScmObj*, align 8
%gen47193 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50914, i64 0)
store %struct.ScmObj* %gen47193, %struct.ScmObj** %stackaddr$env-ref56493
%stackaddr$prim56494 = alloca %struct.ScmObj*, align 8
%k47395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55394)
store volatile %struct.ScmObj* %k47395, %struct.ScmObj** %stackaddr$prim56494, align 8
%stackaddr$prim56495 = alloca %struct.ScmObj*, align 8
%current_45args55395 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55394)
store volatile %struct.ScmObj* %current_45args55395, %struct.ScmObj** %stackaddr$prim56495, align 8
%stackaddr$prim56496 = alloca %struct.ScmObj*, align 8
%n47204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55395)
store volatile %struct.ScmObj* %n47204, %struct.ScmObj** %stackaddr$prim56496, align 8
%stackaddr$makeclosure56497 = alloca %struct.ScmObj*, align 8
%fptrToInt56498 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50915 to i64
%ae50915 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56498)
store volatile %struct.ScmObj* %ae50915, %struct.ScmObj** %stackaddr$makeclosure56497, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50915, %struct.ScmObj* %n47204, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50915, %struct.ScmObj* %k47395, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50915, %struct.ScmObj* %gen47193, i64 2)
%ae50916 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56499 = alloca %struct.ScmObj*, align 8
%fptrToInt56500 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50917 to i64
%ae50917 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56500)
store volatile %struct.ScmObj* %ae50917, %struct.ScmObj** %stackaddr$makeclosure56499, align 8
%argslist55414$ae509150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56501 = alloca %struct.ScmObj*, align 8
%argslist55414$ae509151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50917, %struct.ScmObj* %argslist55414$ae509150)
store volatile %struct.ScmObj* %argslist55414$ae509151, %struct.ScmObj** %stackaddr$prim56501, align 8
%stackaddr$prim56502 = alloca %struct.ScmObj*, align 8
%argslist55414$ae509152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50916, %struct.ScmObj* %argslist55414$ae509151)
store volatile %struct.ScmObj* %argslist55414$ae509152, %struct.ScmObj** %stackaddr$prim56502, align 8
%clofunc56503 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50915)
musttail call tailcc void %clofunc56503(%struct.ScmObj* %ae50915, %struct.ScmObj* %argslist55414$ae509152)
ret void
}

define tailcc void @proc_clo$ae50915(%struct.ScmObj* %env$ae50915,%struct.ScmObj* %current_45args55397) {
%stackaddr$env-ref56504 = alloca %struct.ScmObj*, align 8
%n47204 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50915, i64 0)
store %struct.ScmObj* %n47204, %struct.ScmObj** %stackaddr$env-ref56504
%stackaddr$env-ref56505 = alloca %struct.ScmObj*, align 8
%k47395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50915, i64 1)
store %struct.ScmObj* %k47395, %struct.ScmObj** %stackaddr$env-ref56505
%stackaddr$env-ref56506 = alloca %struct.ScmObj*, align 8
%gen47193 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50915, i64 2)
store %struct.ScmObj* %gen47193, %struct.ScmObj** %stackaddr$env-ref56506
%stackaddr$prim56507 = alloca %struct.ScmObj*, align 8
%_95k47396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55397)
store volatile %struct.ScmObj* %_95k47396, %struct.ScmObj** %stackaddr$prim56507, align 8
%stackaddr$prim56508 = alloca %struct.ScmObj*, align 8
%current_45args55398 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55397)
store volatile %struct.ScmObj* %current_45args55398, %struct.ScmObj** %stackaddr$prim56508, align 8
%stackaddr$prim56509 = alloca %struct.ScmObj*, align 8
%anf_45bind47320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55398)
store volatile %struct.ScmObj* %anf_45bind47320, %struct.ScmObj** %stackaddr$prim56509, align 8
%stackaddr$makeclosure56510 = alloca %struct.ScmObj*, align 8
%fptrToInt56511 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50939 to i64
%ae50939 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56511)
store volatile %struct.ScmObj* %ae50939, %struct.ScmObj** %stackaddr$makeclosure56510, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50939, %struct.ScmObj* %n47204, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50939, %struct.ScmObj* %k47395, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50939, %struct.ScmObj* %anf_45bind47320, i64 2)
%ae50940 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56512 = alloca %struct.ScmObj*, align 8
%fptrToInt56513 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50941 to i64
%ae50941 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56513)
store volatile %struct.ScmObj* %ae50941, %struct.ScmObj** %stackaddr$makeclosure56512, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50941, %struct.ScmObj* %n47204, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50941, %struct.ScmObj* %gen47193, i64 1)
%argslist55412$ae509390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56514 = alloca %struct.ScmObj*, align 8
%argslist55412$ae509391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50941, %struct.ScmObj* %argslist55412$ae509390)
store volatile %struct.ScmObj* %argslist55412$ae509391, %struct.ScmObj** %stackaddr$prim56514, align 8
%stackaddr$prim56515 = alloca %struct.ScmObj*, align 8
%argslist55412$ae509392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50940, %struct.ScmObj* %argslist55412$ae509391)
store volatile %struct.ScmObj* %argslist55412$ae509392, %struct.ScmObj** %stackaddr$prim56515, align 8
%clofunc56516 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50939)
musttail call tailcc void %clofunc56516(%struct.ScmObj* %ae50939, %struct.ScmObj* %argslist55412$ae509392)
ret void
}

define tailcc void @proc_clo$ae50939(%struct.ScmObj* %env$ae50939,%struct.ScmObj* %current_45args55400) {
%stackaddr$env-ref56517 = alloca %struct.ScmObj*, align 8
%n47204 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50939, i64 0)
store %struct.ScmObj* %n47204, %struct.ScmObj** %stackaddr$env-ref56517
%stackaddr$env-ref56518 = alloca %struct.ScmObj*, align 8
%k47395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50939, i64 1)
store %struct.ScmObj* %k47395, %struct.ScmObj** %stackaddr$env-ref56518
%stackaddr$env-ref56519 = alloca %struct.ScmObj*, align 8
%anf_45bind47320 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50939, i64 2)
store %struct.ScmObj* %anf_45bind47320, %struct.ScmObj** %stackaddr$env-ref56519
%stackaddr$prim56520 = alloca %struct.ScmObj*, align 8
%_95k47397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55400)
store volatile %struct.ScmObj* %_95k47397, %struct.ScmObj** %stackaddr$prim56520, align 8
%stackaddr$prim56521 = alloca %struct.ScmObj*, align 8
%current_45args55401 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55400)
store volatile %struct.ScmObj* %current_45args55401, %struct.ScmObj** %stackaddr$prim56521, align 8
%stackaddr$prim56522 = alloca %struct.ScmObj*, align 8
%anf_45bind47323 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55401)
store volatile %struct.ScmObj* %anf_45bind47323, %struct.ScmObj** %stackaddr$prim56522, align 8
%stackaddr$makeclosure56523 = alloca %struct.ScmObj*, align 8
%fptrToInt56524 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50975 to i64
%ae50975 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56524)
store volatile %struct.ScmObj* %ae50975, %struct.ScmObj** %stackaddr$makeclosure56523, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50975, %struct.ScmObj* %n47204, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50975, %struct.ScmObj* %k47395, i64 1)
%ae50976 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae5097656525, i32 0, i32 0))
%ae50977 = call %struct.ScmObj* @const_init_false()
%argslist55407$anf_45bind473200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56526 = alloca %struct.ScmObj*, align 8
%argslist55407$anf_45bind473201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47323, %struct.ScmObj* %argslist55407$anf_45bind473200)
store volatile %struct.ScmObj* %argslist55407$anf_45bind473201, %struct.ScmObj** %stackaddr$prim56526, align 8
%stackaddr$prim56527 = alloca %struct.ScmObj*, align 8
%argslist55407$anf_45bind473202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50977, %struct.ScmObj* %argslist55407$anf_45bind473201)
store volatile %struct.ScmObj* %argslist55407$anf_45bind473202, %struct.ScmObj** %stackaddr$prim56527, align 8
%stackaddr$prim56528 = alloca %struct.ScmObj*, align 8
%argslist55407$anf_45bind473203 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50976, %struct.ScmObj* %argslist55407$anf_45bind473202)
store volatile %struct.ScmObj* %argslist55407$anf_45bind473203, %struct.ScmObj** %stackaddr$prim56528, align 8
%stackaddr$prim56529 = alloca %struct.ScmObj*, align 8
%argslist55407$anf_45bind473204 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50975, %struct.ScmObj* %argslist55407$anf_45bind473203)
store volatile %struct.ScmObj* %argslist55407$anf_45bind473204, %struct.ScmObj** %stackaddr$prim56529, align 8
%clofunc56530 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47320)
musttail call tailcc void %clofunc56530(%struct.ScmObj* %anf_45bind47320, %struct.ScmObj* %argslist55407$anf_45bind473204)
ret void
}

define tailcc void @proc_clo$ae50975(%struct.ScmObj* %env$ae50975,%struct.ScmObj* %current_45args55403) {
%stackaddr$env-ref56531 = alloca %struct.ScmObj*, align 8
%n47204 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50975, i64 0)
store %struct.ScmObj* %n47204, %struct.ScmObj** %stackaddr$env-ref56531
%stackaddr$env-ref56532 = alloca %struct.ScmObj*, align 8
%k47395 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50975, i64 1)
store %struct.ScmObj* %k47395, %struct.ScmObj** %stackaddr$env-ref56532
%stackaddr$prim56533 = alloca %struct.ScmObj*, align 8
%_95k47398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55403)
store volatile %struct.ScmObj* %_95k47398, %struct.ScmObj** %stackaddr$prim56533, align 8
%stackaddr$prim56534 = alloca %struct.ScmObj*, align 8
%current_45args55404 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55403)
store volatile %struct.ScmObj* %current_45args55404, %struct.ScmObj** %stackaddr$prim56534, align 8
%stackaddr$prim56535 = alloca %struct.ScmObj*, align 8
%anf_45bind47324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55404)
store volatile %struct.ScmObj* %anf_45bind47324, %struct.ScmObj** %stackaddr$prim56535, align 8
%stackaddr$prim56536 = alloca %struct.ScmObj*, align 8
%cpsprim47399 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %n47204, %struct.ScmObj* %anf_45bind47324)
store volatile %struct.ScmObj* %cpsprim47399, %struct.ScmObj** %stackaddr$prim56536, align 8
%ae50992 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55406$k473950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56537 = alloca %struct.ScmObj*, align 8
%argslist55406$k473951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47399, %struct.ScmObj* %argslist55406$k473950)
store volatile %struct.ScmObj* %argslist55406$k473951, %struct.ScmObj** %stackaddr$prim56537, align 8
%stackaddr$prim56538 = alloca %struct.ScmObj*, align 8
%argslist55406$k473952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50992, %struct.ScmObj* %argslist55406$k473951)
store volatile %struct.ScmObj* %argslist55406$k473952, %struct.ScmObj** %stackaddr$prim56538, align 8
%clofunc56539 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47395)
musttail call tailcc void %clofunc56539(%struct.ScmObj* %k47395, %struct.ScmObj* %argslist55406$k473952)
ret void
}

define tailcc void @proc_clo$ae50941(%struct.ScmObj* %env$ae50941,%struct.ScmObj* %current_45args55408) {
%stackaddr$env-ref56540 = alloca %struct.ScmObj*, align 8
%n47204 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50941, i64 0)
store %struct.ScmObj* %n47204, %struct.ScmObj** %stackaddr$env-ref56540
%stackaddr$env-ref56541 = alloca %struct.ScmObj*, align 8
%gen47193 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50941, i64 1)
store %struct.ScmObj* %gen47193, %struct.ScmObj** %stackaddr$env-ref56541
%stackaddr$prim56542 = alloca %struct.ScmObj*, align 8
%k47400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55408)
store volatile %struct.ScmObj* %k47400, %struct.ScmObj** %stackaddr$prim56542, align 8
%stackaddr$prim56543 = alloca %struct.ScmObj*, align 8
%current_45args55409 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55408)
store volatile %struct.ScmObj* %current_45args55409, %struct.ScmObj** %stackaddr$prim56543, align 8
%stackaddr$prim56544 = alloca %struct.ScmObj*, align 8
%_9547206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55409)
store volatile %struct.ScmObj* %_9547206, %struct.ScmObj** %stackaddr$prim56544, align 8
%ae50943 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56545 = alloca %struct.ScmObj*, align 8
%anf_45bind47321 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %gen47193, %struct.ScmObj* %ae50943)
store volatile %struct.ScmObj* %anf_45bind47321, %struct.ScmObj** %stackaddr$prim56545, align 8
%ae50945 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56546 = alloca %struct.ScmObj*, align 8
%anf_45bind47322 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n47204, %struct.ScmObj* %ae50945)
store volatile %struct.ScmObj* %anf_45bind47322, %struct.ScmObj** %stackaddr$prim56546, align 8
%argslist55411$anf_45bind473210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56547 = alloca %struct.ScmObj*, align 8
%argslist55411$anf_45bind473211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47322, %struct.ScmObj* %argslist55411$anf_45bind473210)
store volatile %struct.ScmObj* %argslist55411$anf_45bind473211, %struct.ScmObj** %stackaddr$prim56547, align 8
%stackaddr$prim56548 = alloca %struct.ScmObj*, align 8
%argslist55411$anf_45bind473212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47400, %struct.ScmObj* %argslist55411$anf_45bind473211)
store volatile %struct.ScmObj* %argslist55411$anf_45bind473212, %struct.ScmObj** %stackaddr$prim56548, align 8
%clofunc56549 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47321)
musttail call tailcc void %clofunc56549(%struct.ScmObj* %anf_45bind47321, %struct.ScmObj* %argslist55411$anf_45bind473212)
ret void
}

define tailcc void @proc_clo$ae50917(%struct.ScmObj* %env$ae50917,%struct.ScmObj* %el4720547401) {
%stackaddr$prim56550 = alloca %struct.ScmObj*, align 8
%k47402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %el4720547401)
store volatile %struct.ScmObj* %k47402, %struct.ScmObj** %stackaddr$prim56550, align 8
%stackaddr$prim56551 = alloca %struct.ScmObj*, align 8
%el47205 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %el4720547401)
store volatile %struct.ScmObj* %el47205, %struct.ScmObj** %stackaddr$prim56551, align 8
%stackaddr$applyprim56552 = alloca %struct.ScmObj*, align 8
%cpsaprim47403 = call %struct.ScmObj* @applyprim_vector(%struct.ScmObj* %el47205)
store volatile %struct.ScmObj* %cpsaprim47403, %struct.ScmObj** %stackaddr$applyprim56552, align 8
%ae50922 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55413$k474020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56553 = alloca %struct.ScmObj*, align 8
%argslist55413$k474021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim47403, %struct.ScmObj* %argslist55413$k474020)
store volatile %struct.ScmObj* %argslist55413$k474021, %struct.ScmObj** %stackaddr$prim56553, align 8
%stackaddr$prim56554 = alloca %struct.ScmObj*, align 8
%argslist55413$k474022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50922, %struct.ScmObj* %argslist55413$k474021)
store volatile %struct.ScmObj* %argslist55413$k474022, %struct.ScmObj** %stackaddr$prim56554, align 8
%clofunc56555 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47402)
musttail call tailcc void %clofunc56555(%struct.ScmObj* %k47402, %struct.ScmObj* %argslist55413$k474022)
ret void
}

define tailcc void @proc_clo$ae50884(%struct.ScmObj* %env$ae50884,%struct.ScmObj* %current_45args55416) {
%stackaddr$prim56556 = alloca %struct.ScmObj*, align 8
%k47404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55416)
store volatile %struct.ScmObj* %k47404, %struct.ScmObj** %stackaddr$prim56556, align 8
%stackaddr$prim56557 = alloca %struct.ScmObj*, align 8
%current_45args55417 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55416)
store volatile %struct.ScmObj* %current_45args55417, %struct.ScmObj** %stackaddr$prim56557, align 8
%stackaddr$prim56558 = alloca %struct.ScmObj*, align 8
%x47132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55417)
store volatile %struct.ScmObj* %x47132, %struct.ScmObj** %stackaddr$prim56558, align 8
%stackaddr$prim56559 = alloca %struct.ScmObj*, align 8
%anf_45bind47315 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47132)
store volatile %struct.ScmObj* %anf_45bind47315, %struct.ScmObj** %stackaddr$prim56559, align 8
%stackaddr$prim56560 = alloca %struct.ScmObj*, align 8
%anf_45bind47316 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47315)
store volatile %struct.ScmObj* %anf_45bind47316, %struct.ScmObj** %stackaddr$prim56560, align 8
%stackaddr$prim56561 = alloca %struct.ScmObj*, align 8
%anf_45bind47317 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47316)
store volatile %struct.ScmObj* %anf_45bind47317, %struct.ScmObj** %stackaddr$prim56561, align 8
%stackaddr$prim56562 = alloca %struct.ScmObj*, align 8
%cpsprim47405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47317)
store volatile %struct.ScmObj* %cpsprim47405, %struct.ScmObj** %stackaddr$prim56562, align 8
%ae50890 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55419$k474040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56563 = alloca %struct.ScmObj*, align 8
%argslist55419$k474041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47405, %struct.ScmObj* %argslist55419$k474040)
store volatile %struct.ScmObj* %argslist55419$k474041, %struct.ScmObj** %stackaddr$prim56563, align 8
%stackaddr$prim56564 = alloca %struct.ScmObj*, align 8
%argslist55419$k474042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50890, %struct.ScmObj* %argslist55419$k474041)
store volatile %struct.ScmObj* %argslist55419$k474042, %struct.ScmObj** %stackaddr$prim56564, align 8
%clofunc56565 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47404)
musttail call tailcc void %clofunc56565(%struct.ScmObj* %k47404, %struct.ScmObj* %argslist55419$k474042)
ret void
}

define tailcc void @proc_clo$ae50860(%struct.ScmObj* %env$ae50860,%struct.ScmObj* %current_45args55421) {
%stackaddr$prim56566 = alloca %struct.ScmObj*, align 8
%k47406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55421)
store volatile %struct.ScmObj* %k47406, %struct.ScmObj** %stackaddr$prim56566, align 8
%stackaddr$prim56567 = alloca %struct.ScmObj*, align 8
%current_45args55422 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55421)
store volatile %struct.ScmObj* %current_45args55422, %struct.ScmObj** %stackaddr$prim56567, align 8
%stackaddr$prim56568 = alloca %struct.ScmObj*, align 8
%x47134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55422)
store volatile %struct.ScmObj* %x47134, %struct.ScmObj** %stackaddr$prim56568, align 8
%stackaddr$prim56569 = alloca %struct.ScmObj*, align 8
%anf_45bind47313 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47134)
store volatile %struct.ScmObj* %anf_45bind47313, %struct.ScmObj** %stackaddr$prim56569, align 8
%stackaddr$prim56570 = alloca %struct.ScmObj*, align 8
%anf_45bind47314 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47313)
store volatile %struct.ScmObj* %anf_45bind47314, %struct.ScmObj** %stackaddr$prim56570, align 8
%stackaddr$prim56571 = alloca %struct.ScmObj*, align 8
%cpsprim47407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47314)
store volatile %struct.ScmObj* %cpsprim47407, %struct.ScmObj** %stackaddr$prim56571, align 8
%ae50865 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55424$k474060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56572 = alloca %struct.ScmObj*, align 8
%argslist55424$k474061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47407, %struct.ScmObj* %argslist55424$k474060)
store volatile %struct.ScmObj* %argslist55424$k474061, %struct.ScmObj** %stackaddr$prim56572, align 8
%stackaddr$prim56573 = alloca %struct.ScmObj*, align 8
%argslist55424$k474062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50865, %struct.ScmObj* %argslist55424$k474061)
store volatile %struct.ScmObj* %argslist55424$k474062, %struct.ScmObj** %stackaddr$prim56573, align 8
%clofunc56574 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47406)
musttail call tailcc void %clofunc56574(%struct.ScmObj* %k47406, %struct.ScmObj* %argslist55424$k474062)
ret void
}

define tailcc void @proc_clo$ae50838(%struct.ScmObj* %env$ae50838,%struct.ScmObj* %current_45args55426) {
%stackaddr$prim56575 = alloca %struct.ScmObj*, align 8
%k47408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55426)
store volatile %struct.ScmObj* %k47408, %struct.ScmObj** %stackaddr$prim56575, align 8
%stackaddr$prim56576 = alloca %struct.ScmObj*, align 8
%current_45args55427 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55426)
store volatile %struct.ScmObj* %current_45args55427, %struct.ScmObj** %stackaddr$prim56576, align 8
%stackaddr$prim56577 = alloca %struct.ScmObj*, align 8
%x47136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55427)
store volatile %struct.ScmObj* %x47136, %struct.ScmObj** %stackaddr$prim56577, align 8
%stackaddr$prim56578 = alloca %struct.ScmObj*, align 8
%anf_45bind47312 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47136)
store volatile %struct.ScmObj* %anf_45bind47312, %struct.ScmObj** %stackaddr$prim56578, align 8
%stackaddr$prim56579 = alloca %struct.ScmObj*, align 8
%cpsprim47409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47312)
store volatile %struct.ScmObj* %cpsprim47409, %struct.ScmObj** %stackaddr$prim56579, align 8
%ae50842 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55429$k474080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56580 = alloca %struct.ScmObj*, align 8
%argslist55429$k474081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47409, %struct.ScmObj* %argslist55429$k474080)
store volatile %struct.ScmObj* %argslist55429$k474081, %struct.ScmObj** %stackaddr$prim56580, align 8
%stackaddr$prim56581 = alloca %struct.ScmObj*, align 8
%argslist55429$k474082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50842, %struct.ScmObj* %argslist55429$k474081)
store volatile %struct.ScmObj* %argslist55429$k474082, %struct.ScmObj** %stackaddr$prim56581, align 8
%clofunc56582 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47408)
musttail call tailcc void %clofunc56582(%struct.ScmObj* %k47408, %struct.ScmObj* %argslist55429$k474082)
ret void
}

define tailcc void @proc_clo$ae50818(%struct.ScmObj* %env$ae50818,%struct.ScmObj* %current_45args55431) {
%stackaddr$prim56583 = alloca %struct.ScmObj*, align 8
%k47410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55431)
store volatile %struct.ScmObj* %k47410, %struct.ScmObj** %stackaddr$prim56583, align 8
%stackaddr$prim56584 = alloca %struct.ScmObj*, align 8
%current_45args55432 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55431)
store volatile %struct.ScmObj* %current_45args55432, %struct.ScmObj** %stackaddr$prim56584, align 8
%stackaddr$prim56585 = alloca %struct.ScmObj*, align 8
%x47138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55432)
store volatile %struct.ScmObj* %x47138, %struct.ScmObj** %stackaddr$prim56585, align 8
%stackaddr$prim56586 = alloca %struct.ScmObj*, align 8
%cpsprim47411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47138)
store volatile %struct.ScmObj* %cpsprim47411, %struct.ScmObj** %stackaddr$prim56586, align 8
%ae50821 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55434$k474100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56587 = alloca %struct.ScmObj*, align 8
%argslist55434$k474101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47411, %struct.ScmObj* %argslist55434$k474100)
store volatile %struct.ScmObj* %argslist55434$k474101, %struct.ScmObj** %stackaddr$prim56587, align 8
%stackaddr$prim56588 = alloca %struct.ScmObj*, align 8
%argslist55434$k474102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50821, %struct.ScmObj* %argslist55434$k474101)
store volatile %struct.ScmObj* %argslist55434$k474102, %struct.ScmObj** %stackaddr$prim56588, align 8
%clofunc56589 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47410)
musttail call tailcc void %clofunc56589(%struct.ScmObj* %k47410, %struct.ScmObj* %argslist55434$k474102)
ret void
}

define tailcc void @proc_clo$ae50720(%struct.ScmObj* %env$ae50720,%struct.ScmObj* %args4714047412) {
%stackaddr$env-ref56590 = alloca %struct.ScmObj*, align 8
%_37foldl147079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50720, i64 0)
store %struct.ScmObj* %_37foldl147079, %struct.ScmObj** %stackaddr$env-ref56590
%stackaddr$prim56591 = alloca %struct.ScmObj*, align 8
%k47413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4714047412)
store volatile %struct.ScmObj* %k47413, %struct.ScmObj** %stackaddr$prim56591, align 8
%stackaddr$prim56592 = alloca %struct.ScmObj*, align 8
%args47140 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4714047412)
store volatile %struct.ScmObj* %args47140, %struct.ScmObj** %stackaddr$prim56592, align 8
%stackaddr$prim56593 = alloca %struct.ScmObj*, align 8
%anf_45bind47306 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args47140)
store volatile %struct.ScmObj* %anf_45bind47306, %struct.ScmObj** %stackaddr$prim56593, align 8
%truthy$cmp56594 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47306)
%cmp$cmp56594 = icmp eq i64 %truthy$cmp56594, 1
br i1 %cmp$cmp56594, label %truebranch$cmp56594, label %falsebranch$cmp56594
truebranch$cmp56594:
%ae50726 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50727 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist55436$k474130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56595 = alloca %struct.ScmObj*, align 8
%argslist55436$k474131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50727, %struct.ScmObj* %argslist55436$k474130)
store volatile %struct.ScmObj* %argslist55436$k474131, %struct.ScmObj** %stackaddr$prim56595, align 8
%stackaddr$prim56596 = alloca %struct.ScmObj*, align 8
%argslist55436$k474132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50726, %struct.ScmObj* %argslist55436$k474131)
store volatile %struct.ScmObj* %argslist55436$k474132, %struct.ScmObj** %stackaddr$prim56596, align 8
%clofunc56597 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47413)
musttail call tailcc void %clofunc56597(%struct.ScmObj* %k47413, %struct.ScmObj* %argslist55436$k474132)
ret void
falsebranch$cmp56594:
%stackaddr$prim56598 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47140)
store volatile %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$prim56598, align 8
%stackaddr$prim56599 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47307)
store volatile %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$prim56599, align 8
%truthy$cmp56600 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47308)
%cmp$cmp56600 = icmp eq i64 %truthy$cmp56600, 1
br i1 %cmp$cmp56600, label %truebranch$cmp56600, label %falsebranch$cmp56600
truebranch$cmp56600:
%stackaddr$prim56601 = alloca %struct.ScmObj*, align 8
%cpsprim47414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47140)
store volatile %struct.ScmObj* %cpsprim47414, %struct.ScmObj** %stackaddr$prim56601, align 8
%ae50739 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55437$k474130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56602 = alloca %struct.ScmObj*, align 8
%argslist55437$k474131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47414, %struct.ScmObj* %argslist55437$k474130)
store volatile %struct.ScmObj* %argslist55437$k474131, %struct.ScmObj** %stackaddr$prim56602, align 8
%stackaddr$prim56603 = alloca %struct.ScmObj*, align 8
%argslist55437$k474132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50739, %struct.ScmObj* %argslist55437$k474131)
store volatile %struct.ScmObj* %argslist55437$k474132, %struct.ScmObj** %stackaddr$prim56603, align 8
%clofunc56604 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47413)
musttail call tailcc void %clofunc56604(%struct.ScmObj* %k47413, %struct.ScmObj* %argslist55437$k474132)
ret void
falsebranch$cmp56600:
%stackaddr$makeclosure56605 = alloca %struct.ScmObj*, align 8
%fptrToInt56606 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50744 to i64
%ae50744 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56606)
store volatile %struct.ScmObj* %ae50744, %struct.ScmObj** %stackaddr$makeclosure56605, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50744, %struct.ScmObj* %k47413, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50744, %struct.ScmObj* %args47140, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50744, %struct.ScmObj* %_37foldl147079, i64 2)
%ae50745 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56607 = alloca %struct.ScmObj*, align 8
%fptrToInt56608 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50746 to i64
%ae50746 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56608)
store volatile %struct.ScmObj* %ae50746, %struct.ScmObj** %stackaddr$makeclosure56607, align 8
%argslist55447$ae507440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56609 = alloca %struct.ScmObj*, align 8
%argslist55447$ae507441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50746, %struct.ScmObj* %argslist55447$ae507440)
store volatile %struct.ScmObj* %argslist55447$ae507441, %struct.ScmObj** %stackaddr$prim56609, align 8
%stackaddr$prim56610 = alloca %struct.ScmObj*, align 8
%argslist55447$ae507442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50745, %struct.ScmObj* %argslist55447$ae507441)
store volatile %struct.ScmObj* %argslist55447$ae507442, %struct.ScmObj** %stackaddr$prim56610, align 8
%clofunc56611 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50744)
musttail call tailcc void %clofunc56611(%struct.ScmObj* %ae50744, %struct.ScmObj* %argslist55447$ae507442)
ret void
}

define tailcc void @proc_clo$ae50744(%struct.ScmObj* %env$ae50744,%struct.ScmObj* %current_45args55438) {
%stackaddr$env-ref56612 = alloca %struct.ScmObj*, align 8
%k47413 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50744, i64 0)
store %struct.ScmObj* %k47413, %struct.ScmObj** %stackaddr$env-ref56612
%stackaddr$env-ref56613 = alloca %struct.ScmObj*, align 8
%args47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50744, i64 1)
store %struct.ScmObj* %args47140, %struct.ScmObj** %stackaddr$env-ref56613
%stackaddr$env-ref56614 = alloca %struct.ScmObj*, align 8
%_37foldl147079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50744, i64 2)
store %struct.ScmObj* %_37foldl147079, %struct.ScmObj** %stackaddr$env-ref56614
%stackaddr$prim56615 = alloca %struct.ScmObj*, align 8
%_95k47415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55438)
store volatile %struct.ScmObj* %_95k47415, %struct.ScmObj** %stackaddr$prim56615, align 8
%stackaddr$prim56616 = alloca %struct.ScmObj*, align 8
%current_45args55439 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55438)
store volatile %struct.ScmObj* %current_45args55439, %struct.ScmObj** %stackaddr$prim56616, align 8
%stackaddr$prim56617 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55439)
store volatile %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$prim56617, align 8
%stackaddr$prim56618 = alloca %struct.ScmObj*, align 8
%anf_45bind47310 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47140)
store volatile %struct.ScmObj* %anf_45bind47310, %struct.ScmObj** %stackaddr$prim56618, align 8
%stackaddr$prim56619 = alloca %struct.ScmObj*, align 8
%anf_45bind47311 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47140)
store volatile %struct.ScmObj* %anf_45bind47311, %struct.ScmObj** %stackaddr$prim56619, align 8
%argslist55441$_37foldl1470790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56620 = alloca %struct.ScmObj*, align 8
%argslist55441$_37foldl1470791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47311, %struct.ScmObj* %argslist55441$_37foldl1470790)
store volatile %struct.ScmObj* %argslist55441$_37foldl1470791, %struct.ScmObj** %stackaddr$prim56620, align 8
%stackaddr$prim56621 = alloca %struct.ScmObj*, align 8
%argslist55441$_37foldl1470792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47310, %struct.ScmObj* %argslist55441$_37foldl1470791)
store volatile %struct.ScmObj* %argslist55441$_37foldl1470792, %struct.ScmObj** %stackaddr$prim56621, align 8
%stackaddr$prim56622 = alloca %struct.ScmObj*, align 8
%argslist55441$_37foldl1470793 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47309, %struct.ScmObj* %argslist55441$_37foldl1470792)
store volatile %struct.ScmObj* %argslist55441$_37foldl1470793, %struct.ScmObj** %stackaddr$prim56622, align 8
%stackaddr$prim56623 = alloca %struct.ScmObj*, align 8
%argslist55441$_37foldl1470794 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47413, %struct.ScmObj* %argslist55441$_37foldl1470793)
store volatile %struct.ScmObj* %argslist55441$_37foldl1470794, %struct.ScmObj** %stackaddr$prim56623, align 8
%clofunc56624 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147079)
musttail call tailcc void %clofunc56624(%struct.ScmObj* %_37foldl147079, %struct.ScmObj* %argslist55441$_37foldl1470794)
ret void
}

define tailcc void @proc_clo$ae50746(%struct.ScmObj* %env$ae50746,%struct.ScmObj* %current_45args55442) {
%stackaddr$prim56625 = alloca %struct.ScmObj*, align 8
%k47416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55442)
store volatile %struct.ScmObj* %k47416, %struct.ScmObj** %stackaddr$prim56625, align 8
%stackaddr$prim56626 = alloca %struct.ScmObj*, align 8
%current_45args55443 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55442)
store volatile %struct.ScmObj* %current_45args55443, %struct.ScmObj** %stackaddr$prim56626, align 8
%stackaddr$prim56627 = alloca %struct.ScmObj*, align 8
%n47142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55443)
store volatile %struct.ScmObj* %n47142, %struct.ScmObj** %stackaddr$prim56627, align 8
%stackaddr$prim56628 = alloca %struct.ScmObj*, align 8
%current_45args55444 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55443)
store volatile %struct.ScmObj* %current_45args55444, %struct.ScmObj** %stackaddr$prim56628, align 8
%stackaddr$prim56629 = alloca %struct.ScmObj*, align 8
%v47141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55444)
store volatile %struct.ScmObj* %v47141, %struct.ScmObj** %stackaddr$prim56629, align 8
%stackaddr$prim56630 = alloca %struct.ScmObj*, align 8
%cpsprim47417 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v47141, %struct.ScmObj* %n47142)
store volatile %struct.ScmObj* %cpsprim47417, %struct.ScmObj** %stackaddr$prim56630, align 8
%ae50750 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55446$k474160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56631 = alloca %struct.ScmObj*, align 8
%argslist55446$k474161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47417, %struct.ScmObj* %argslist55446$k474160)
store volatile %struct.ScmObj* %argslist55446$k474161, %struct.ScmObj** %stackaddr$prim56631, align 8
%stackaddr$prim56632 = alloca %struct.ScmObj*, align 8
%argslist55446$k474162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50750, %struct.ScmObj* %argslist55446$k474161)
store volatile %struct.ScmObj* %argslist55446$k474162, %struct.ScmObj** %stackaddr$prim56632, align 8
%clofunc56633 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47416)
musttail call tailcc void %clofunc56633(%struct.ScmObj* %k47416, %struct.ScmObj* %argslist55446$k474162)
ret void
}

define tailcc void @proc_clo$ae50316(%struct.ScmObj* %env$ae50316,%struct.ScmObj* %current_45args55449) {
%stackaddr$prim56634 = alloca %struct.ScmObj*, align 8
%k47418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55449)
store volatile %struct.ScmObj* %k47418, %struct.ScmObj** %stackaddr$prim56634, align 8
%stackaddr$prim56635 = alloca %struct.ScmObj*, align 8
%current_45args55450 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55449)
store volatile %struct.ScmObj* %current_45args55450, %struct.ScmObj** %stackaddr$prim56635, align 8
%stackaddr$prim56636 = alloca %struct.ScmObj*, align 8
%v47145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55450)
store volatile %struct.ScmObj* %v47145, %struct.ScmObj** %stackaddr$prim56636, align 8
%stackaddr$prim56637 = alloca %struct.ScmObj*, align 8
%current_45args55451 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55450)
store volatile %struct.ScmObj* %current_45args55451, %struct.ScmObj** %stackaddr$prim56637, align 8
%stackaddr$prim56638 = alloca %struct.ScmObj*, align 8
%lst47144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55451)
store volatile %struct.ScmObj* %lst47144, %struct.ScmObj** %stackaddr$prim56638, align 8
%ae50317 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56639 = alloca %struct.ScmObj*, align 8
%lst47146 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50317, %struct.ScmObj* %lst47144)
store volatile %struct.ScmObj* %lst47146, %struct.ScmObj** %stackaddr$prim56639, align 8
%stackaddr$makeclosure56640 = alloca %struct.ScmObj*, align 8
%fptrToInt56641 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50319 to i64
%ae50319 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56641)
store volatile %struct.ScmObj* %ae50319, %struct.ScmObj** %stackaddr$makeclosure56640, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50319, %struct.ScmObj* %k47418, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50319, %struct.ScmObj* %lst47146, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50319, %struct.ScmObj* %v47145, i64 2)
%ae50320 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56642 = alloca %struct.ScmObj*, align 8
%fptrToInt56643 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50321 to i64
%ae50321 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56643)
store volatile %struct.ScmObj* %ae50321, %struct.ScmObj** %stackaddr$makeclosure56642, align 8
%argslist55473$ae503190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56644 = alloca %struct.ScmObj*, align 8
%argslist55473$ae503191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50321, %struct.ScmObj* %argslist55473$ae503190)
store volatile %struct.ScmObj* %argslist55473$ae503191, %struct.ScmObj** %stackaddr$prim56644, align 8
%stackaddr$prim56645 = alloca %struct.ScmObj*, align 8
%argslist55473$ae503192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50320, %struct.ScmObj* %argslist55473$ae503191)
store volatile %struct.ScmObj* %argslist55473$ae503192, %struct.ScmObj** %stackaddr$prim56645, align 8
%clofunc56646 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50319)
musttail call tailcc void %clofunc56646(%struct.ScmObj* %ae50319, %struct.ScmObj* %argslist55473$ae503192)
ret void
}

define tailcc void @proc_clo$ae50319(%struct.ScmObj* %env$ae50319,%struct.ScmObj* %current_45args55453) {
%stackaddr$env-ref56647 = alloca %struct.ScmObj*, align 8
%k47418 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50319, i64 0)
store %struct.ScmObj* %k47418, %struct.ScmObj** %stackaddr$env-ref56647
%stackaddr$env-ref56648 = alloca %struct.ScmObj*, align 8
%lst47146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50319, i64 1)
store %struct.ScmObj* %lst47146, %struct.ScmObj** %stackaddr$env-ref56648
%stackaddr$env-ref56649 = alloca %struct.ScmObj*, align 8
%v47145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50319, i64 2)
store %struct.ScmObj* %v47145, %struct.ScmObj** %stackaddr$env-ref56649
%stackaddr$prim56650 = alloca %struct.ScmObj*, align 8
%_95k47419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55453)
store volatile %struct.ScmObj* %_95k47419, %struct.ScmObj** %stackaddr$prim56650, align 8
%stackaddr$prim56651 = alloca %struct.ScmObj*, align 8
%current_45args55454 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55453)
store volatile %struct.ScmObj* %current_45args55454, %struct.ScmObj** %stackaddr$prim56651, align 8
%stackaddr$prim56652 = alloca %struct.ScmObj*, align 8
%anf_45bind47298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55454)
store volatile %struct.ScmObj* %anf_45bind47298, %struct.ScmObj** %stackaddr$prim56652, align 8
%stackaddr$makeclosure56653 = alloca %struct.ScmObj*, align 8
%fptrToInt56654 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50335 to i64
%ae50335 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56654)
store volatile %struct.ScmObj* %ae50335, %struct.ScmObj** %stackaddr$makeclosure56653, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50335, %struct.ScmObj* %k47418, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50335, %struct.ScmObj* %lst47146, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50335, %struct.ScmObj* %v47145, i64 2)
%stackaddr$makeclosure56655 = alloca %struct.ScmObj*, align 8
%fptrToInt56656 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50336 to i64
%ae50336 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56656)
store volatile %struct.ScmObj* %ae50336, %struct.ScmObj** %stackaddr$makeclosure56655, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50336, %struct.ScmObj* %k47418, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50336, %struct.ScmObj* %lst47146, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50336, %struct.ScmObj* %v47145, i64 2)
%argslist55468$anf_45bind472980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56657 = alloca %struct.ScmObj*, align 8
%argslist55468$anf_45bind472981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50336, %struct.ScmObj* %argslist55468$anf_45bind472980)
store volatile %struct.ScmObj* %argslist55468$anf_45bind472981, %struct.ScmObj** %stackaddr$prim56657, align 8
%stackaddr$prim56658 = alloca %struct.ScmObj*, align 8
%argslist55468$anf_45bind472982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50335, %struct.ScmObj* %argslist55468$anf_45bind472981)
store volatile %struct.ScmObj* %argslist55468$anf_45bind472982, %struct.ScmObj** %stackaddr$prim56658, align 8
%clofunc56659 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47298)
musttail call tailcc void %clofunc56659(%struct.ScmObj* %anf_45bind47298, %struct.ScmObj* %argslist55468$anf_45bind472982)
ret void
}

define tailcc void @proc_clo$ae50335(%struct.ScmObj* %env$ae50335,%struct.ScmObj* %current_45args55456) {
%stackaddr$env-ref56660 = alloca %struct.ScmObj*, align 8
%k47418 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50335, i64 0)
store %struct.ScmObj* %k47418, %struct.ScmObj** %stackaddr$env-ref56660
%stackaddr$env-ref56661 = alloca %struct.ScmObj*, align 8
%lst47146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50335, i64 1)
store %struct.ScmObj* %lst47146, %struct.ScmObj** %stackaddr$env-ref56661
%stackaddr$env-ref56662 = alloca %struct.ScmObj*, align 8
%v47145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50335, i64 2)
store %struct.ScmObj* %v47145, %struct.ScmObj** %stackaddr$env-ref56662
%stackaddr$prim56663 = alloca %struct.ScmObj*, align 8
%_95k47420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55456)
store volatile %struct.ScmObj* %_95k47420, %struct.ScmObj** %stackaddr$prim56663, align 8
%stackaddr$prim56664 = alloca %struct.ScmObj*, align 8
%current_45args55457 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55456)
store volatile %struct.ScmObj* %current_45args55457, %struct.ScmObj** %stackaddr$prim56664, align 8
%stackaddr$prim56665 = alloca %struct.ScmObj*, align 8
%cc47147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55457)
store volatile %struct.ScmObj* %cc47147, %struct.ScmObj** %stackaddr$prim56665, align 8
%ae50444 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56666 = alloca %struct.ScmObj*, align 8
%anf_45bind47299 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47146, %struct.ScmObj* %ae50444)
store volatile %struct.ScmObj* %anf_45bind47299, %struct.ScmObj** %stackaddr$prim56666, align 8
%stackaddr$prim56667 = alloca %struct.ScmObj*, align 8
%anf_45bind47300 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47299)
store volatile %struct.ScmObj* %anf_45bind47300, %struct.ScmObj** %stackaddr$prim56667, align 8
%truthy$cmp56668 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47300)
%cmp$cmp56668 = icmp eq i64 %truthy$cmp56668, 1
br i1 %cmp$cmp56668, label %truebranch$cmp56668, label %falsebranch$cmp56668
truebranch$cmp56668:
%ae50448 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50449 = call %struct.ScmObj* @const_init_false()
%argslist55459$k474180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56669 = alloca %struct.ScmObj*, align 8
%argslist55459$k474181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50449, %struct.ScmObj* %argslist55459$k474180)
store volatile %struct.ScmObj* %argslist55459$k474181, %struct.ScmObj** %stackaddr$prim56669, align 8
%stackaddr$prim56670 = alloca %struct.ScmObj*, align 8
%argslist55459$k474182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50448, %struct.ScmObj* %argslist55459$k474181)
store volatile %struct.ScmObj* %argslist55459$k474182, %struct.ScmObj** %stackaddr$prim56670, align 8
%clofunc56671 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47418)
musttail call tailcc void %clofunc56671(%struct.ScmObj* %k47418, %struct.ScmObj* %argslist55459$k474182)
ret void
falsebranch$cmp56668:
%ae50457 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56672 = alloca %struct.ScmObj*, align 8
%anf_45bind47301 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47146, %struct.ScmObj* %ae50457)
store volatile %struct.ScmObj* %anf_45bind47301, %struct.ScmObj** %stackaddr$prim56672, align 8
%stackaddr$prim56673 = alloca %struct.ScmObj*, align 8
%anf_45bind47302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47301)
store volatile %struct.ScmObj* %anf_45bind47302, %struct.ScmObj** %stackaddr$prim56673, align 8
%stackaddr$prim56674 = alloca %struct.ScmObj*, align 8
%anf_45bind47303 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47302, %struct.ScmObj* %v47145)
store volatile %struct.ScmObj* %anf_45bind47303, %struct.ScmObj** %stackaddr$prim56674, align 8
%truthy$cmp56675 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47303)
%cmp$cmp56675 = icmp eq i64 %truthy$cmp56675, 1
br i1 %cmp$cmp56675, label %truebranch$cmp56675, label %falsebranch$cmp56675
truebranch$cmp56675:
%ae50463 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56676 = alloca %struct.ScmObj*, align 8
%cpsprim47421 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47146, %struct.ScmObj* %ae50463)
store volatile %struct.ScmObj* %cpsprim47421, %struct.ScmObj** %stackaddr$prim56676, align 8
%ae50465 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55460$k474180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56677 = alloca %struct.ScmObj*, align 8
%argslist55460$k474181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47421, %struct.ScmObj* %argslist55460$k474180)
store volatile %struct.ScmObj* %argslist55460$k474181, %struct.ScmObj** %stackaddr$prim56677, align 8
%stackaddr$prim56678 = alloca %struct.ScmObj*, align 8
%argslist55460$k474182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50465, %struct.ScmObj* %argslist55460$k474181)
store volatile %struct.ScmObj* %argslist55460$k474182, %struct.ScmObj** %stackaddr$prim56678, align 8
%clofunc56679 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47418)
musttail call tailcc void %clofunc56679(%struct.ScmObj* %k47418, %struct.ScmObj* %argslist55460$k474182)
ret void
falsebranch$cmp56675:
%ae50476 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56680 = alloca %struct.ScmObj*, align 8
%anf_45bind47304 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47146, %struct.ScmObj* %ae50476)
store volatile %struct.ScmObj* %anf_45bind47304, %struct.ScmObj** %stackaddr$prim56680, align 8
%stackaddr$prim56681 = alloca %struct.ScmObj*, align 8
%anf_45bind47305 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47304)
store volatile %struct.ScmObj* %anf_45bind47305, %struct.ScmObj** %stackaddr$prim56681, align 8
%ae50479 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56682 = alloca %struct.ScmObj*, align 8
%_95047149 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47146, %struct.ScmObj* %ae50479, %struct.ScmObj* %anf_45bind47305)
store volatile %struct.ScmObj* %_95047149, %struct.ScmObj** %stackaddr$prim56682, align 8
%argslist55461$cc471470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56683 = alloca %struct.ScmObj*, align 8
%argslist55461$cc471471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47147, %struct.ScmObj* %argslist55461$cc471470)
store volatile %struct.ScmObj* %argslist55461$cc471471, %struct.ScmObj** %stackaddr$prim56683, align 8
%stackaddr$prim56684 = alloca %struct.ScmObj*, align 8
%argslist55461$cc471472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47418, %struct.ScmObj* %argslist55461$cc471471)
store volatile %struct.ScmObj* %argslist55461$cc471472, %struct.ScmObj** %stackaddr$prim56684, align 8
%clofunc56685 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47147)
musttail call tailcc void %clofunc56685(%struct.ScmObj* %cc47147, %struct.ScmObj* %argslist55461$cc471472)
ret void
}

define tailcc void @proc_clo$ae50336(%struct.ScmObj* %env$ae50336,%struct.ScmObj* %current_45args55462) {
%stackaddr$env-ref56686 = alloca %struct.ScmObj*, align 8
%k47418 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50336, i64 0)
store %struct.ScmObj* %k47418, %struct.ScmObj** %stackaddr$env-ref56686
%stackaddr$env-ref56687 = alloca %struct.ScmObj*, align 8
%lst47146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50336, i64 1)
store %struct.ScmObj* %lst47146, %struct.ScmObj** %stackaddr$env-ref56687
%stackaddr$env-ref56688 = alloca %struct.ScmObj*, align 8
%v47145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50336, i64 2)
store %struct.ScmObj* %v47145, %struct.ScmObj** %stackaddr$env-ref56688
%stackaddr$prim56689 = alloca %struct.ScmObj*, align 8
%_95k47420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55462)
store volatile %struct.ScmObj* %_95k47420, %struct.ScmObj** %stackaddr$prim56689, align 8
%stackaddr$prim56690 = alloca %struct.ScmObj*, align 8
%current_45args55463 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55462)
store volatile %struct.ScmObj* %current_45args55463, %struct.ScmObj** %stackaddr$prim56690, align 8
%stackaddr$prim56691 = alloca %struct.ScmObj*, align 8
%cc47147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55463)
store volatile %struct.ScmObj* %cc47147, %struct.ScmObj** %stackaddr$prim56691, align 8
%ae50338 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56692 = alloca %struct.ScmObj*, align 8
%anf_45bind47299 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47146, %struct.ScmObj* %ae50338)
store volatile %struct.ScmObj* %anf_45bind47299, %struct.ScmObj** %stackaddr$prim56692, align 8
%stackaddr$prim56693 = alloca %struct.ScmObj*, align 8
%anf_45bind47300 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47299)
store volatile %struct.ScmObj* %anf_45bind47300, %struct.ScmObj** %stackaddr$prim56693, align 8
%truthy$cmp56694 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47300)
%cmp$cmp56694 = icmp eq i64 %truthy$cmp56694, 1
br i1 %cmp$cmp56694, label %truebranch$cmp56694, label %falsebranch$cmp56694
truebranch$cmp56694:
%ae50342 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50343 = call %struct.ScmObj* @const_init_false()
%argslist55465$k474180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56695 = alloca %struct.ScmObj*, align 8
%argslist55465$k474181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50343, %struct.ScmObj* %argslist55465$k474180)
store volatile %struct.ScmObj* %argslist55465$k474181, %struct.ScmObj** %stackaddr$prim56695, align 8
%stackaddr$prim56696 = alloca %struct.ScmObj*, align 8
%argslist55465$k474182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50342, %struct.ScmObj* %argslist55465$k474181)
store volatile %struct.ScmObj* %argslist55465$k474182, %struct.ScmObj** %stackaddr$prim56696, align 8
%clofunc56697 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47418)
musttail call tailcc void %clofunc56697(%struct.ScmObj* %k47418, %struct.ScmObj* %argslist55465$k474182)
ret void
falsebranch$cmp56694:
%ae50351 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56698 = alloca %struct.ScmObj*, align 8
%anf_45bind47301 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47146, %struct.ScmObj* %ae50351)
store volatile %struct.ScmObj* %anf_45bind47301, %struct.ScmObj** %stackaddr$prim56698, align 8
%stackaddr$prim56699 = alloca %struct.ScmObj*, align 8
%anf_45bind47302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47301)
store volatile %struct.ScmObj* %anf_45bind47302, %struct.ScmObj** %stackaddr$prim56699, align 8
%stackaddr$prim56700 = alloca %struct.ScmObj*, align 8
%anf_45bind47303 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47302, %struct.ScmObj* %v47145)
store volatile %struct.ScmObj* %anf_45bind47303, %struct.ScmObj** %stackaddr$prim56700, align 8
%truthy$cmp56701 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47303)
%cmp$cmp56701 = icmp eq i64 %truthy$cmp56701, 1
br i1 %cmp$cmp56701, label %truebranch$cmp56701, label %falsebranch$cmp56701
truebranch$cmp56701:
%ae50357 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56702 = alloca %struct.ScmObj*, align 8
%cpsprim47421 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47146, %struct.ScmObj* %ae50357)
store volatile %struct.ScmObj* %cpsprim47421, %struct.ScmObj** %stackaddr$prim56702, align 8
%ae50359 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55466$k474180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56703 = alloca %struct.ScmObj*, align 8
%argslist55466$k474181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47421, %struct.ScmObj* %argslist55466$k474180)
store volatile %struct.ScmObj* %argslist55466$k474181, %struct.ScmObj** %stackaddr$prim56703, align 8
%stackaddr$prim56704 = alloca %struct.ScmObj*, align 8
%argslist55466$k474182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50359, %struct.ScmObj* %argslist55466$k474181)
store volatile %struct.ScmObj* %argslist55466$k474182, %struct.ScmObj** %stackaddr$prim56704, align 8
%clofunc56705 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47418)
musttail call tailcc void %clofunc56705(%struct.ScmObj* %k47418, %struct.ScmObj* %argslist55466$k474182)
ret void
falsebranch$cmp56701:
%ae50370 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56706 = alloca %struct.ScmObj*, align 8
%anf_45bind47304 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47146, %struct.ScmObj* %ae50370)
store volatile %struct.ScmObj* %anf_45bind47304, %struct.ScmObj** %stackaddr$prim56706, align 8
%stackaddr$prim56707 = alloca %struct.ScmObj*, align 8
%anf_45bind47305 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47304)
store volatile %struct.ScmObj* %anf_45bind47305, %struct.ScmObj** %stackaddr$prim56707, align 8
%ae50373 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56708 = alloca %struct.ScmObj*, align 8
%_95047149 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47146, %struct.ScmObj* %ae50373, %struct.ScmObj* %anf_45bind47305)
store volatile %struct.ScmObj* %_95047149, %struct.ScmObj** %stackaddr$prim56708, align 8
%argslist55467$cc471470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56709 = alloca %struct.ScmObj*, align 8
%argslist55467$cc471471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47147, %struct.ScmObj* %argslist55467$cc471470)
store volatile %struct.ScmObj* %argslist55467$cc471471, %struct.ScmObj** %stackaddr$prim56709, align 8
%stackaddr$prim56710 = alloca %struct.ScmObj*, align 8
%argslist55467$cc471472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47418, %struct.ScmObj* %argslist55467$cc471471)
store volatile %struct.ScmObj* %argslist55467$cc471472, %struct.ScmObj** %stackaddr$prim56710, align 8
%clofunc56711 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47147)
musttail call tailcc void %clofunc56711(%struct.ScmObj* %cc47147, %struct.ScmObj* %argslist55467$cc471472)
ret void
}

define tailcc void @proc_clo$ae50321(%struct.ScmObj* %env$ae50321,%struct.ScmObj* %current_45args55469) {
%stackaddr$prim56712 = alloca %struct.ScmObj*, align 8
%k47422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55469)
store volatile %struct.ScmObj* %k47422, %struct.ScmObj** %stackaddr$prim56712, align 8
%stackaddr$prim56713 = alloca %struct.ScmObj*, align 8
%current_45args55470 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55469)
store volatile %struct.ScmObj* %current_45args55470, %struct.ScmObj** %stackaddr$prim56713, align 8
%stackaddr$prim56714 = alloca %struct.ScmObj*, align 8
%u47148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55470)
store volatile %struct.ScmObj* %u47148, %struct.ScmObj** %stackaddr$prim56714, align 8
%argslist55472$u471480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56715 = alloca %struct.ScmObj*, align 8
%argslist55472$u471481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47148, %struct.ScmObj* %argslist55472$u471480)
store volatile %struct.ScmObj* %argslist55472$u471481, %struct.ScmObj** %stackaddr$prim56715, align 8
%stackaddr$prim56716 = alloca %struct.ScmObj*, align 8
%argslist55472$u471482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47422, %struct.ScmObj* %argslist55472$u471481)
store volatile %struct.ScmObj* %argslist55472$u471482, %struct.ScmObj** %stackaddr$prim56716, align 8
%clofunc56717 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47148)
musttail call tailcc void %clofunc56717(%struct.ScmObj* %u47148, %struct.ScmObj* %argslist55472$u471482)
ret void
}

define tailcc void @proc_clo$ae49780(%struct.ScmObj* %env$ae49780,%struct.ScmObj* %current_45args55475) {
%stackaddr$prim56718 = alloca %struct.ScmObj*, align 8
%k47423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55475)
store volatile %struct.ScmObj* %k47423, %struct.ScmObj** %stackaddr$prim56718, align 8
%stackaddr$prim56719 = alloca %struct.ScmObj*, align 8
%current_45args55476 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55475)
store volatile %struct.ScmObj* %current_45args55476, %struct.ScmObj** %stackaddr$prim56719, align 8
%stackaddr$prim56720 = alloca %struct.ScmObj*, align 8
%lst47152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55476)
store volatile %struct.ScmObj* %lst47152, %struct.ScmObj** %stackaddr$prim56720, align 8
%stackaddr$prim56721 = alloca %struct.ScmObj*, align 8
%current_45args55477 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55476)
store volatile %struct.ScmObj* %current_45args55477, %struct.ScmObj** %stackaddr$prim56721, align 8
%stackaddr$prim56722 = alloca %struct.ScmObj*, align 8
%n47151 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55477)
store volatile %struct.ScmObj* %n47151, %struct.ScmObj** %stackaddr$prim56722, align 8
%ae49781 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56723 = alloca %struct.ScmObj*, align 8
%n47154 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49781, %struct.ScmObj* %n47151)
store volatile %struct.ScmObj* %n47154, %struct.ScmObj** %stackaddr$prim56723, align 8
%ae49783 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56724 = alloca %struct.ScmObj*, align 8
%lst47153 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49783, %struct.ScmObj* %lst47152)
store volatile %struct.ScmObj* %lst47153, %struct.ScmObj** %stackaddr$prim56724, align 8
%stackaddr$makeclosure56725 = alloca %struct.ScmObj*, align 8
%fptrToInt56726 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49785 to i64
%ae49785 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56726)
store volatile %struct.ScmObj* %ae49785, %struct.ScmObj** %stackaddr$makeclosure56725, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49785, %struct.ScmObj* %n47154, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49785, %struct.ScmObj* %lst47153, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49785, %struct.ScmObj* %k47423, i64 2)
%ae49786 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56727 = alloca %struct.ScmObj*, align 8
%fptrToInt56728 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49787 to i64
%ae49787 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56728)
store volatile %struct.ScmObj* %ae49787, %struct.ScmObj** %stackaddr$makeclosure56727, align 8
%argslist55497$ae497850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56729 = alloca %struct.ScmObj*, align 8
%argslist55497$ae497851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49787, %struct.ScmObj* %argslist55497$ae497850)
store volatile %struct.ScmObj* %argslist55497$ae497851, %struct.ScmObj** %stackaddr$prim56729, align 8
%stackaddr$prim56730 = alloca %struct.ScmObj*, align 8
%argslist55497$ae497852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49786, %struct.ScmObj* %argslist55497$ae497851)
store volatile %struct.ScmObj* %argslist55497$ae497852, %struct.ScmObj** %stackaddr$prim56730, align 8
%clofunc56731 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49785)
musttail call tailcc void %clofunc56731(%struct.ScmObj* %ae49785, %struct.ScmObj* %argslist55497$ae497852)
ret void
}

define tailcc void @proc_clo$ae49785(%struct.ScmObj* %env$ae49785,%struct.ScmObj* %current_45args55479) {
%stackaddr$env-ref56732 = alloca %struct.ScmObj*, align 8
%n47154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49785, i64 0)
store %struct.ScmObj* %n47154, %struct.ScmObj** %stackaddr$env-ref56732
%stackaddr$env-ref56733 = alloca %struct.ScmObj*, align 8
%lst47153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49785, i64 1)
store %struct.ScmObj* %lst47153, %struct.ScmObj** %stackaddr$env-ref56733
%stackaddr$env-ref56734 = alloca %struct.ScmObj*, align 8
%k47423 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49785, i64 2)
store %struct.ScmObj* %k47423, %struct.ScmObj** %stackaddr$env-ref56734
%stackaddr$prim56735 = alloca %struct.ScmObj*, align 8
%_95k47424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55479)
store volatile %struct.ScmObj* %_95k47424, %struct.ScmObj** %stackaddr$prim56735, align 8
%stackaddr$prim56736 = alloca %struct.ScmObj*, align 8
%current_45args55480 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55479)
store volatile %struct.ScmObj* %current_45args55480, %struct.ScmObj** %stackaddr$prim56736, align 8
%stackaddr$prim56737 = alloca %struct.ScmObj*, align 8
%anf_45bind47291 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55480)
store volatile %struct.ScmObj* %anf_45bind47291, %struct.ScmObj** %stackaddr$prim56737, align 8
%stackaddr$makeclosure56738 = alloca %struct.ScmObj*, align 8
%fptrToInt56739 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49801 to i64
%ae49801 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56739)
store volatile %struct.ScmObj* %ae49801, %struct.ScmObj** %stackaddr$makeclosure56738, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49801, %struct.ScmObj* %n47154, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49801, %struct.ScmObj* %lst47153, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49801, %struct.ScmObj* %k47423, i64 2)
%stackaddr$makeclosure56740 = alloca %struct.ScmObj*, align 8
%fptrToInt56741 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49802 to i64
%ae49802 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56741)
store volatile %struct.ScmObj* %ae49802, %struct.ScmObj** %stackaddr$makeclosure56740, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49802, %struct.ScmObj* %n47154, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49802, %struct.ScmObj* %lst47153, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49802, %struct.ScmObj* %k47423, i64 2)
%argslist55492$anf_45bind472910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56742 = alloca %struct.ScmObj*, align 8
%argslist55492$anf_45bind472911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49802, %struct.ScmObj* %argslist55492$anf_45bind472910)
store volatile %struct.ScmObj* %argslist55492$anf_45bind472911, %struct.ScmObj** %stackaddr$prim56742, align 8
%stackaddr$prim56743 = alloca %struct.ScmObj*, align 8
%argslist55492$anf_45bind472912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49801, %struct.ScmObj* %argslist55492$anf_45bind472911)
store volatile %struct.ScmObj* %argslist55492$anf_45bind472912, %struct.ScmObj** %stackaddr$prim56743, align 8
%clofunc56744 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47291)
musttail call tailcc void %clofunc56744(%struct.ScmObj* %anf_45bind47291, %struct.ScmObj* %argslist55492$anf_45bind472912)
ret void
}

define tailcc void @proc_clo$ae49801(%struct.ScmObj* %env$ae49801,%struct.ScmObj* %current_45args55482) {
%stackaddr$env-ref56745 = alloca %struct.ScmObj*, align 8
%n47154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49801, i64 0)
store %struct.ScmObj* %n47154, %struct.ScmObj** %stackaddr$env-ref56745
%stackaddr$env-ref56746 = alloca %struct.ScmObj*, align 8
%lst47153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49801, i64 1)
store %struct.ScmObj* %lst47153, %struct.ScmObj** %stackaddr$env-ref56746
%stackaddr$env-ref56747 = alloca %struct.ScmObj*, align 8
%k47423 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49801, i64 2)
store %struct.ScmObj* %k47423, %struct.ScmObj** %stackaddr$env-ref56747
%stackaddr$prim56748 = alloca %struct.ScmObj*, align 8
%_95k47425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55482)
store volatile %struct.ScmObj* %_95k47425, %struct.ScmObj** %stackaddr$prim56748, align 8
%stackaddr$prim56749 = alloca %struct.ScmObj*, align 8
%current_45args55483 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55482)
store volatile %struct.ScmObj* %current_45args55483, %struct.ScmObj** %stackaddr$prim56749, align 8
%stackaddr$prim56750 = alloca %struct.ScmObj*, align 8
%cc47155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55483)
store volatile %struct.ScmObj* %cc47155, %struct.ScmObj** %stackaddr$prim56750, align 8
%ae49944 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56751 = alloca %struct.ScmObj*, align 8
%anf_45bind47292 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47154, %struct.ScmObj* %ae49944)
store volatile %struct.ScmObj* %anf_45bind47292, %struct.ScmObj** %stackaddr$prim56751, align 8
%ae49945 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56752 = alloca %struct.ScmObj*, align 8
%anf_45bind47293 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49945, %struct.ScmObj* %anf_45bind47292)
store volatile %struct.ScmObj* %anf_45bind47293, %struct.ScmObj** %stackaddr$prim56752, align 8
%truthy$cmp56753 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47293)
%cmp$cmp56753 = icmp eq i64 %truthy$cmp56753, 1
br i1 %cmp$cmp56753, label %truebranch$cmp56753, label %falsebranch$cmp56753
truebranch$cmp56753:
%ae49949 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56754 = alloca %struct.ScmObj*, align 8
%cpsprim47426 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47153, %struct.ScmObj* %ae49949)
store volatile %struct.ScmObj* %cpsprim47426, %struct.ScmObj** %stackaddr$prim56754, align 8
%ae49951 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55485$k474230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56755 = alloca %struct.ScmObj*, align 8
%argslist55485$k474231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47426, %struct.ScmObj* %argslist55485$k474230)
store volatile %struct.ScmObj* %argslist55485$k474231, %struct.ScmObj** %stackaddr$prim56755, align 8
%stackaddr$prim56756 = alloca %struct.ScmObj*, align 8
%argslist55485$k474232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49951, %struct.ScmObj* %argslist55485$k474231)
store volatile %struct.ScmObj* %argslist55485$k474232, %struct.ScmObj** %stackaddr$prim56756, align 8
%clofunc56757 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47423)
musttail call tailcc void %clofunc56757(%struct.ScmObj* %k47423, %struct.ScmObj* %argslist55485$k474232)
ret void
falsebranch$cmp56753:
%ae49962 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56758 = alloca %struct.ScmObj*, align 8
%anf_45bind47294 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47153, %struct.ScmObj* %ae49962)
store volatile %struct.ScmObj* %anf_45bind47294, %struct.ScmObj** %stackaddr$prim56758, align 8
%stackaddr$prim56759 = alloca %struct.ScmObj*, align 8
%anf_45bind47295 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47294)
store volatile %struct.ScmObj* %anf_45bind47295, %struct.ScmObj** %stackaddr$prim56759, align 8
%ae49965 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56760 = alloca %struct.ScmObj*, align 8
%_95047158 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47153, %struct.ScmObj* %ae49965, %struct.ScmObj* %anf_45bind47295)
store volatile %struct.ScmObj* %_95047158, %struct.ScmObj** %stackaddr$prim56760, align 8
%ae49968 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56761 = alloca %struct.ScmObj*, align 8
%anf_45bind47296 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47154, %struct.ScmObj* %ae49968)
store volatile %struct.ScmObj* %anf_45bind47296, %struct.ScmObj** %stackaddr$prim56761, align 8
%ae49970 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56762 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47296, %struct.ScmObj* %ae49970)
store volatile %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$prim56762, align 8
%ae49972 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56763 = alloca %struct.ScmObj*, align 8
%_95147157 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47154, %struct.ScmObj* %ae49972, %struct.ScmObj* %anf_45bind47297)
store volatile %struct.ScmObj* %_95147157, %struct.ScmObj** %stackaddr$prim56763, align 8
%argslist55486$cc471550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56764 = alloca %struct.ScmObj*, align 8
%argslist55486$cc471551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47155, %struct.ScmObj* %argslist55486$cc471550)
store volatile %struct.ScmObj* %argslist55486$cc471551, %struct.ScmObj** %stackaddr$prim56764, align 8
%stackaddr$prim56765 = alloca %struct.ScmObj*, align 8
%argslist55486$cc471552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47423, %struct.ScmObj* %argslist55486$cc471551)
store volatile %struct.ScmObj* %argslist55486$cc471552, %struct.ScmObj** %stackaddr$prim56765, align 8
%clofunc56766 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47155)
musttail call tailcc void %clofunc56766(%struct.ScmObj* %cc47155, %struct.ScmObj* %argslist55486$cc471552)
ret void
}

define tailcc void @proc_clo$ae49802(%struct.ScmObj* %env$ae49802,%struct.ScmObj* %current_45args55487) {
%stackaddr$env-ref56767 = alloca %struct.ScmObj*, align 8
%n47154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49802, i64 0)
store %struct.ScmObj* %n47154, %struct.ScmObj** %stackaddr$env-ref56767
%stackaddr$env-ref56768 = alloca %struct.ScmObj*, align 8
%lst47153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49802, i64 1)
store %struct.ScmObj* %lst47153, %struct.ScmObj** %stackaddr$env-ref56768
%stackaddr$env-ref56769 = alloca %struct.ScmObj*, align 8
%k47423 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49802, i64 2)
store %struct.ScmObj* %k47423, %struct.ScmObj** %stackaddr$env-ref56769
%stackaddr$prim56770 = alloca %struct.ScmObj*, align 8
%_95k47425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55487)
store volatile %struct.ScmObj* %_95k47425, %struct.ScmObj** %stackaddr$prim56770, align 8
%stackaddr$prim56771 = alloca %struct.ScmObj*, align 8
%current_45args55488 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55487)
store volatile %struct.ScmObj* %current_45args55488, %struct.ScmObj** %stackaddr$prim56771, align 8
%stackaddr$prim56772 = alloca %struct.ScmObj*, align 8
%cc47155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55488)
store volatile %struct.ScmObj* %cc47155, %struct.ScmObj** %stackaddr$prim56772, align 8
%ae49804 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56773 = alloca %struct.ScmObj*, align 8
%anf_45bind47292 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47154, %struct.ScmObj* %ae49804)
store volatile %struct.ScmObj* %anf_45bind47292, %struct.ScmObj** %stackaddr$prim56773, align 8
%ae49805 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56774 = alloca %struct.ScmObj*, align 8
%anf_45bind47293 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49805, %struct.ScmObj* %anf_45bind47292)
store volatile %struct.ScmObj* %anf_45bind47293, %struct.ScmObj** %stackaddr$prim56774, align 8
%truthy$cmp56775 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47293)
%cmp$cmp56775 = icmp eq i64 %truthy$cmp56775, 1
br i1 %cmp$cmp56775, label %truebranch$cmp56775, label %falsebranch$cmp56775
truebranch$cmp56775:
%ae49809 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56776 = alloca %struct.ScmObj*, align 8
%cpsprim47426 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47153, %struct.ScmObj* %ae49809)
store volatile %struct.ScmObj* %cpsprim47426, %struct.ScmObj** %stackaddr$prim56776, align 8
%ae49811 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55490$k474230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56777 = alloca %struct.ScmObj*, align 8
%argslist55490$k474231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47426, %struct.ScmObj* %argslist55490$k474230)
store volatile %struct.ScmObj* %argslist55490$k474231, %struct.ScmObj** %stackaddr$prim56777, align 8
%stackaddr$prim56778 = alloca %struct.ScmObj*, align 8
%argslist55490$k474232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49811, %struct.ScmObj* %argslist55490$k474231)
store volatile %struct.ScmObj* %argslist55490$k474232, %struct.ScmObj** %stackaddr$prim56778, align 8
%clofunc56779 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47423)
musttail call tailcc void %clofunc56779(%struct.ScmObj* %k47423, %struct.ScmObj* %argslist55490$k474232)
ret void
falsebranch$cmp56775:
%ae49822 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56780 = alloca %struct.ScmObj*, align 8
%anf_45bind47294 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47153, %struct.ScmObj* %ae49822)
store volatile %struct.ScmObj* %anf_45bind47294, %struct.ScmObj** %stackaddr$prim56780, align 8
%stackaddr$prim56781 = alloca %struct.ScmObj*, align 8
%anf_45bind47295 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47294)
store volatile %struct.ScmObj* %anf_45bind47295, %struct.ScmObj** %stackaddr$prim56781, align 8
%ae49825 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56782 = alloca %struct.ScmObj*, align 8
%_95047158 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47153, %struct.ScmObj* %ae49825, %struct.ScmObj* %anf_45bind47295)
store volatile %struct.ScmObj* %_95047158, %struct.ScmObj** %stackaddr$prim56782, align 8
%ae49828 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56783 = alloca %struct.ScmObj*, align 8
%anf_45bind47296 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47154, %struct.ScmObj* %ae49828)
store volatile %struct.ScmObj* %anf_45bind47296, %struct.ScmObj** %stackaddr$prim56783, align 8
%ae49830 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56784 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47296, %struct.ScmObj* %ae49830)
store volatile %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$prim56784, align 8
%ae49832 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56785 = alloca %struct.ScmObj*, align 8
%_95147157 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47154, %struct.ScmObj* %ae49832, %struct.ScmObj* %anf_45bind47297)
store volatile %struct.ScmObj* %_95147157, %struct.ScmObj** %stackaddr$prim56785, align 8
%argslist55491$cc471550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56786 = alloca %struct.ScmObj*, align 8
%argslist55491$cc471551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47155, %struct.ScmObj* %argslist55491$cc471550)
store volatile %struct.ScmObj* %argslist55491$cc471551, %struct.ScmObj** %stackaddr$prim56786, align 8
%stackaddr$prim56787 = alloca %struct.ScmObj*, align 8
%argslist55491$cc471552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47423, %struct.ScmObj* %argslist55491$cc471551)
store volatile %struct.ScmObj* %argslist55491$cc471552, %struct.ScmObj** %stackaddr$prim56787, align 8
%clofunc56788 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47155)
musttail call tailcc void %clofunc56788(%struct.ScmObj* %cc47155, %struct.ScmObj* %argslist55491$cc471552)
ret void
}

define tailcc void @proc_clo$ae49787(%struct.ScmObj* %env$ae49787,%struct.ScmObj* %current_45args55493) {
%stackaddr$prim56789 = alloca %struct.ScmObj*, align 8
%k47427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55493)
store volatile %struct.ScmObj* %k47427, %struct.ScmObj** %stackaddr$prim56789, align 8
%stackaddr$prim56790 = alloca %struct.ScmObj*, align 8
%current_45args55494 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55493)
store volatile %struct.ScmObj* %current_45args55494, %struct.ScmObj** %stackaddr$prim56790, align 8
%stackaddr$prim56791 = alloca %struct.ScmObj*, align 8
%u47156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55494)
store volatile %struct.ScmObj* %u47156, %struct.ScmObj** %stackaddr$prim56791, align 8
%argslist55496$u471560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56792 = alloca %struct.ScmObj*, align 8
%argslist55496$u471561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47156, %struct.ScmObj* %argslist55496$u471560)
store volatile %struct.ScmObj* %argslist55496$u471561, %struct.ScmObj** %stackaddr$prim56792, align 8
%stackaddr$prim56793 = alloca %struct.ScmObj*, align 8
%argslist55496$u471562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47427, %struct.ScmObj* %argslist55496$u471561)
store volatile %struct.ScmObj* %argslist55496$u471562, %struct.ScmObj** %stackaddr$prim56793, align 8
%clofunc56794 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47156)
musttail call tailcc void %clofunc56794(%struct.ScmObj* %u47156, %struct.ScmObj* %argslist55496$u471562)
ret void
}

define tailcc void @proc_clo$ae49364(%struct.ScmObj* %env$ae49364,%struct.ScmObj* %current_45args55499) {
%stackaddr$prim56795 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55499)
store volatile %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$prim56795, align 8
%stackaddr$prim56796 = alloca %struct.ScmObj*, align 8
%current_45args55500 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55499)
store volatile %struct.ScmObj* %current_45args55500, %struct.ScmObj** %stackaddr$prim56796, align 8
%stackaddr$prim56797 = alloca %struct.ScmObj*, align 8
%a47160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55500)
store volatile %struct.ScmObj* %a47160, %struct.ScmObj** %stackaddr$prim56797, align 8
%ae49365 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56798 = alloca %struct.ScmObj*, align 8
%a47161 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49365, %struct.ScmObj* %a47160)
store volatile %struct.ScmObj* %a47161, %struct.ScmObj** %stackaddr$prim56798, align 8
%stackaddr$makeclosure56799 = alloca %struct.ScmObj*, align 8
%fptrToInt56800 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49367 to i64
%ae49367 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56800)
store volatile %struct.ScmObj* %ae49367, %struct.ScmObj** %stackaddr$makeclosure56799, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49367, %struct.ScmObj* %k47428, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49367, %struct.ScmObj* %a47161, i64 1)
%ae49368 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56801 = alloca %struct.ScmObj*, align 8
%fptrToInt56802 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49369 to i64
%ae49369 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56802)
store volatile %struct.ScmObj* %ae49369, %struct.ScmObj** %stackaddr$makeclosure56801, align 8
%argslist55522$ae493670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56803 = alloca %struct.ScmObj*, align 8
%argslist55522$ae493671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49369, %struct.ScmObj* %argslist55522$ae493670)
store volatile %struct.ScmObj* %argslist55522$ae493671, %struct.ScmObj** %stackaddr$prim56803, align 8
%stackaddr$prim56804 = alloca %struct.ScmObj*, align 8
%argslist55522$ae493672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49368, %struct.ScmObj* %argslist55522$ae493671)
store volatile %struct.ScmObj* %argslist55522$ae493672, %struct.ScmObj** %stackaddr$prim56804, align 8
%clofunc56805 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49367)
musttail call tailcc void %clofunc56805(%struct.ScmObj* %ae49367, %struct.ScmObj* %argslist55522$ae493672)
ret void
}

define tailcc void @proc_clo$ae49367(%struct.ScmObj* %env$ae49367,%struct.ScmObj* %current_45args55502) {
%stackaddr$env-ref56806 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49367, i64 0)
store %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$env-ref56806
%stackaddr$env-ref56807 = alloca %struct.ScmObj*, align 8
%a47161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49367, i64 1)
store %struct.ScmObj* %a47161, %struct.ScmObj** %stackaddr$env-ref56807
%stackaddr$prim56808 = alloca %struct.ScmObj*, align 8
%_95k47429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55502)
store volatile %struct.ScmObj* %_95k47429, %struct.ScmObj** %stackaddr$prim56808, align 8
%stackaddr$prim56809 = alloca %struct.ScmObj*, align 8
%current_45args55503 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55502)
store volatile %struct.ScmObj* %current_45args55503, %struct.ScmObj** %stackaddr$prim56809, align 8
%stackaddr$prim56810 = alloca %struct.ScmObj*, align 8
%anf_45bind47283 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55503)
store volatile %struct.ScmObj* %anf_45bind47283, %struct.ScmObj** %stackaddr$prim56810, align 8
%stackaddr$makeclosure56811 = alloca %struct.ScmObj*, align 8
%fptrToInt56812 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49386 to i64
%ae49386 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56812)
store volatile %struct.ScmObj* %ae49386, %struct.ScmObj** %stackaddr$makeclosure56811, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49386, %struct.ScmObj* %k47428, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49386, %struct.ScmObj* %a47161, i64 1)
%stackaddr$makeclosure56813 = alloca %struct.ScmObj*, align 8
%fptrToInt56814 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49387 to i64
%ae49387 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56814)
store volatile %struct.ScmObj* %ae49387, %struct.ScmObj** %stackaddr$makeclosure56813, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49387, %struct.ScmObj* %k47428, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49387, %struct.ScmObj* %a47161, i64 1)
%argslist55517$anf_45bind472830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56815 = alloca %struct.ScmObj*, align 8
%argslist55517$anf_45bind472831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49387, %struct.ScmObj* %argslist55517$anf_45bind472830)
store volatile %struct.ScmObj* %argslist55517$anf_45bind472831, %struct.ScmObj** %stackaddr$prim56815, align 8
%stackaddr$prim56816 = alloca %struct.ScmObj*, align 8
%argslist55517$anf_45bind472832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49386, %struct.ScmObj* %argslist55517$anf_45bind472831)
store volatile %struct.ScmObj* %argslist55517$anf_45bind472832, %struct.ScmObj** %stackaddr$prim56816, align 8
%clofunc56817 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47283)
musttail call tailcc void %clofunc56817(%struct.ScmObj* %anf_45bind47283, %struct.ScmObj* %argslist55517$anf_45bind472832)
ret void
}

define tailcc void @proc_clo$ae49386(%struct.ScmObj* %env$ae49386,%struct.ScmObj* %current_45args55505) {
%stackaddr$env-ref56818 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49386, i64 0)
store %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$env-ref56818
%stackaddr$env-ref56819 = alloca %struct.ScmObj*, align 8
%a47161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49386, i64 1)
store %struct.ScmObj* %a47161, %struct.ScmObj** %stackaddr$env-ref56819
%stackaddr$prim56820 = alloca %struct.ScmObj*, align 8
%_95k47430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55505)
store volatile %struct.ScmObj* %_95k47430, %struct.ScmObj** %stackaddr$prim56820, align 8
%stackaddr$prim56821 = alloca %struct.ScmObj*, align 8
%current_45args55506 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55505)
store volatile %struct.ScmObj* %current_45args55506, %struct.ScmObj** %stackaddr$prim56821, align 8
%stackaddr$prim56822 = alloca %struct.ScmObj*, align 8
%cc47162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55506)
store volatile %struct.ScmObj* %cc47162, %struct.ScmObj** %stackaddr$prim56822, align 8
%ae49502 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56823 = alloca %struct.ScmObj*, align 8
%anf_45bind47284 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47161, %struct.ScmObj* %ae49502)
store volatile %struct.ScmObj* %anf_45bind47284, %struct.ScmObj** %stackaddr$prim56823, align 8
%stackaddr$prim56824 = alloca %struct.ScmObj*, align 8
%anf_45bind47285 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47284)
store volatile %struct.ScmObj* %anf_45bind47285, %struct.ScmObj** %stackaddr$prim56824, align 8
%truthy$cmp56825 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47285)
%cmp$cmp56825 = icmp eq i64 %truthy$cmp56825, 1
br i1 %cmp$cmp56825, label %truebranch$cmp56825, label %falsebranch$cmp56825
truebranch$cmp56825:
%ae49506 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49507 = call %struct.ScmObj* @const_init_true()
%argslist55508$k474280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56826 = alloca %struct.ScmObj*, align 8
%argslist55508$k474281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49507, %struct.ScmObj* %argslist55508$k474280)
store volatile %struct.ScmObj* %argslist55508$k474281, %struct.ScmObj** %stackaddr$prim56826, align 8
%stackaddr$prim56827 = alloca %struct.ScmObj*, align 8
%argslist55508$k474282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49506, %struct.ScmObj* %argslist55508$k474281)
store volatile %struct.ScmObj* %argslist55508$k474282, %struct.ScmObj** %stackaddr$prim56827, align 8
%clofunc56828 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47428)
musttail call tailcc void %clofunc56828(%struct.ScmObj* %k47428, %struct.ScmObj* %argslist55508$k474282)
ret void
falsebranch$cmp56825:
%ae49515 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56829 = alloca %struct.ScmObj*, align 8
%anf_45bind47286 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47161, %struct.ScmObj* %ae49515)
store volatile %struct.ScmObj* %anf_45bind47286, %struct.ScmObj** %stackaddr$prim56829, align 8
%stackaddr$prim56830 = alloca %struct.ScmObj*, align 8
%anf_45bind47287 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47286)
store volatile %struct.ScmObj* %anf_45bind47287, %struct.ScmObj** %stackaddr$prim56830, align 8
%truthy$cmp56831 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47287)
%cmp$cmp56831 = icmp eq i64 %truthy$cmp56831, 1
br i1 %cmp$cmp56831, label %truebranch$cmp56831, label %falsebranch$cmp56831
truebranch$cmp56831:
%ae49519 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56832 = alloca %struct.ScmObj*, align 8
%anf_45bind47288 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47161, %struct.ScmObj* %ae49519)
store volatile %struct.ScmObj* %anf_45bind47288, %struct.ScmObj** %stackaddr$prim56832, align 8
%stackaddr$prim56833 = alloca %struct.ScmObj*, align 8
%b47164 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47288)
store volatile %struct.ScmObj* %b47164, %struct.ScmObj** %stackaddr$prim56833, align 8
%ae49522 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56834 = alloca %struct.ScmObj*, align 8
%anf_45bind47289 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47161, %struct.ScmObj* %ae49522)
store volatile %struct.ScmObj* %anf_45bind47289, %struct.ScmObj** %stackaddr$prim56834, align 8
%stackaddr$prim56835 = alloca %struct.ScmObj*, align 8
%anf_45bind47290 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47289)
store volatile %struct.ScmObj* %anf_45bind47290, %struct.ScmObj** %stackaddr$prim56835, align 8
%ae49525 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56836 = alloca %struct.ScmObj*, align 8
%_95047165 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47161, %struct.ScmObj* %ae49525, %struct.ScmObj* %anf_45bind47290)
store volatile %struct.ScmObj* %_95047165, %struct.ScmObj** %stackaddr$prim56836, align 8
%argslist55509$cc471620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56837 = alloca %struct.ScmObj*, align 8
%argslist55509$cc471621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47162, %struct.ScmObj* %argslist55509$cc471620)
store volatile %struct.ScmObj* %argslist55509$cc471621, %struct.ScmObj** %stackaddr$prim56837, align 8
%stackaddr$prim56838 = alloca %struct.ScmObj*, align 8
%argslist55509$cc471622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47428, %struct.ScmObj* %argslist55509$cc471621)
store volatile %struct.ScmObj* %argslist55509$cc471622, %struct.ScmObj** %stackaddr$prim56838, align 8
%clofunc56839 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47162)
musttail call tailcc void %clofunc56839(%struct.ScmObj* %cc47162, %struct.ScmObj* %argslist55509$cc471622)
ret void
falsebranch$cmp56831:
%ae49558 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49559 = call %struct.ScmObj* @const_init_false()
%argslist55510$k474280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56840 = alloca %struct.ScmObj*, align 8
%argslist55510$k474281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49559, %struct.ScmObj* %argslist55510$k474280)
store volatile %struct.ScmObj* %argslist55510$k474281, %struct.ScmObj** %stackaddr$prim56840, align 8
%stackaddr$prim56841 = alloca %struct.ScmObj*, align 8
%argslist55510$k474282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49558, %struct.ScmObj* %argslist55510$k474281)
store volatile %struct.ScmObj* %argslist55510$k474282, %struct.ScmObj** %stackaddr$prim56841, align 8
%clofunc56842 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47428)
musttail call tailcc void %clofunc56842(%struct.ScmObj* %k47428, %struct.ScmObj* %argslist55510$k474282)
ret void
}

define tailcc void @proc_clo$ae49387(%struct.ScmObj* %env$ae49387,%struct.ScmObj* %current_45args55511) {
%stackaddr$env-ref56843 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49387, i64 0)
store %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$env-ref56843
%stackaddr$env-ref56844 = alloca %struct.ScmObj*, align 8
%a47161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49387, i64 1)
store %struct.ScmObj* %a47161, %struct.ScmObj** %stackaddr$env-ref56844
%stackaddr$prim56845 = alloca %struct.ScmObj*, align 8
%_95k47430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55511)
store volatile %struct.ScmObj* %_95k47430, %struct.ScmObj** %stackaddr$prim56845, align 8
%stackaddr$prim56846 = alloca %struct.ScmObj*, align 8
%current_45args55512 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55511)
store volatile %struct.ScmObj* %current_45args55512, %struct.ScmObj** %stackaddr$prim56846, align 8
%stackaddr$prim56847 = alloca %struct.ScmObj*, align 8
%cc47162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55512)
store volatile %struct.ScmObj* %cc47162, %struct.ScmObj** %stackaddr$prim56847, align 8
%ae49389 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56848 = alloca %struct.ScmObj*, align 8
%anf_45bind47284 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47161, %struct.ScmObj* %ae49389)
store volatile %struct.ScmObj* %anf_45bind47284, %struct.ScmObj** %stackaddr$prim56848, align 8
%stackaddr$prim56849 = alloca %struct.ScmObj*, align 8
%anf_45bind47285 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47284)
store volatile %struct.ScmObj* %anf_45bind47285, %struct.ScmObj** %stackaddr$prim56849, align 8
%truthy$cmp56850 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47285)
%cmp$cmp56850 = icmp eq i64 %truthy$cmp56850, 1
br i1 %cmp$cmp56850, label %truebranch$cmp56850, label %falsebranch$cmp56850
truebranch$cmp56850:
%ae49393 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49394 = call %struct.ScmObj* @const_init_true()
%argslist55514$k474280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56851 = alloca %struct.ScmObj*, align 8
%argslist55514$k474281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49394, %struct.ScmObj* %argslist55514$k474280)
store volatile %struct.ScmObj* %argslist55514$k474281, %struct.ScmObj** %stackaddr$prim56851, align 8
%stackaddr$prim56852 = alloca %struct.ScmObj*, align 8
%argslist55514$k474282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49393, %struct.ScmObj* %argslist55514$k474281)
store volatile %struct.ScmObj* %argslist55514$k474282, %struct.ScmObj** %stackaddr$prim56852, align 8
%clofunc56853 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47428)
musttail call tailcc void %clofunc56853(%struct.ScmObj* %k47428, %struct.ScmObj* %argslist55514$k474282)
ret void
falsebranch$cmp56850:
%ae49402 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56854 = alloca %struct.ScmObj*, align 8
%anf_45bind47286 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47161, %struct.ScmObj* %ae49402)
store volatile %struct.ScmObj* %anf_45bind47286, %struct.ScmObj** %stackaddr$prim56854, align 8
%stackaddr$prim56855 = alloca %struct.ScmObj*, align 8
%anf_45bind47287 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47286)
store volatile %struct.ScmObj* %anf_45bind47287, %struct.ScmObj** %stackaddr$prim56855, align 8
%truthy$cmp56856 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47287)
%cmp$cmp56856 = icmp eq i64 %truthy$cmp56856, 1
br i1 %cmp$cmp56856, label %truebranch$cmp56856, label %falsebranch$cmp56856
truebranch$cmp56856:
%ae49406 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56857 = alloca %struct.ScmObj*, align 8
%anf_45bind47288 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47161, %struct.ScmObj* %ae49406)
store volatile %struct.ScmObj* %anf_45bind47288, %struct.ScmObj** %stackaddr$prim56857, align 8
%stackaddr$prim56858 = alloca %struct.ScmObj*, align 8
%b47164 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47288)
store volatile %struct.ScmObj* %b47164, %struct.ScmObj** %stackaddr$prim56858, align 8
%ae49409 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56859 = alloca %struct.ScmObj*, align 8
%anf_45bind47289 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47161, %struct.ScmObj* %ae49409)
store volatile %struct.ScmObj* %anf_45bind47289, %struct.ScmObj** %stackaddr$prim56859, align 8
%stackaddr$prim56860 = alloca %struct.ScmObj*, align 8
%anf_45bind47290 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47289)
store volatile %struct.ScmObj* %anf_45bind47290, %struct.ScmObj** %stackaddr$prim56860, align 8
%ae49412 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56861 = alloca %struct.ScmObj*, align 8
%_95047165 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47161, %struct.ScmObj* %ae49412, %struct.ScmObj* %anf_45bind47290)
store volatile %struct.ScmObj* %_95047165, %struct.ScmObj** %stackaddr$prim56861, align 8
%argslist55515$cc471620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56862 = alloca %struct.ScmObj*, align 8
%argslist55515$cc471621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47162, %struct.ScmObj* %argslist55515$cc471620)
store volatile %struct.ScmObj* %argslist55515$cc471621, %struct.ScmObj** %stackaddr$prim56862, align 8
%stackaddr$prim56863 = alloca %struct.ScmObj*, align 8
%argslist55515$cc471622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47428, %struct.ScmObj* %argslist55515$cc471621)
store volatile %struct.ScmObj* %argslist55515$cc471622, %struct.ScmObj** %stackaddr$prim56863, align 8
%clofunc56864 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47162)
musttail call tailcc void %clofunc56864(%struct.ScmObj* %cc47162, %struct.ScmObj* %argslist55515$cc471622)
ret void
falsebranch$cmp56856:
%ae49445 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49446 = call %struct.ScmObj* @const_init_false()
%argslist55516$k474280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56865 = alloca %struct.ScmObj*, align 8
%argslist55516$k474281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49446, %struct.ScmObj* %argslist55516$k474280)
store volatile %struct.ScmObj* %argslist55516$k474281, %struct.ScmObj** %stackaddr$prim56865, align 8
%stackaddr$prim56866 = alloca %struct.ScmObj*, align 8
%argslist55516$k474282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49445, %struct.ScmObj* %argslist55516$k474281)
store volatile %struct.ScmObj* %argslist55516$k474282, %struct.ScmObj** %stackaddr$prim56866, align 8
%clofunc56867 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47428)
musttail call tailcc void %clofunc56867(%struct.ScmObj* %k47428, %struct.ScmObj* %argslist55516$k474282)
ret void
}

define tailcc void @proc_clo$ae49369(%struct.ScmObj* %env$ae49369,%struct.ScmObj* %current_45args55518) {
%stackaddr$prim56868 = alloca %struct.ScmObj*, align 8
%k47431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55518)
store volatile %struct.ScmObj* %k47431, %struct.ScmObj** %stackaddr$prim56868, align 8
%stackaddr$prim56869 = alloca %struct.ScmObj*, align 8
%current_45args55519 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55518)
store volatile %struct.ScmObj* %current_45args55519, %struct.ScmObj** %stackaddr$prim56869, align 8
%stackaddr$prim56870 = alloca %struct.ScmObj*, align 8
%k47163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55519)
store volatile %struct.ScmObj* %k47163, %struct.ScmObj** %stackaddr$prim56870, align 8
%ae49371 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55521$k474310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56871 = alloca %struct.ScmObj*, align 8
%argslist55521$k474311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47163, %struct.ScmObj* %argslist55521$k474310)
store volatile %struct.ScmObj* %argslist55521$k474311, %struct.ScmObj** %stackaddr$prim56871, align 8
%stackaddr$prim56872 = alloca %struct.ScmObj*, align 8
%argslist55521$k474312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49371, %struct.ScmObj* %argslist55521$k474311)
store volatile %struct.ScmObj* %argslist55521$k474312, %struct.ScmObj** %stackaddr$prim56872, align 8
%clofunc56873 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47431)
musttail call tailcc void %clofunc56873(%struct.ScmObj* %k47431, %struct.ScmObj* %argslist55521$k474312)
ret void
}

define tailcc void @proc_clo$ae49292(%struct.ScmObj* %env$ae49292,%struct.ScmObj* %current_45args55524) {
%stackaddr$env-ref56874 = alloca %struct.ScmObj*, align 8
%_37append47167 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49292, i64 0)
store %struct.ScmObj* %_37append47167, %struct.ScmObj** %stackaddr$env-ref56874
%stackaddr$prim56875 = alloca %struct.ScmObj*, align 8
%k47432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55524)
store volatile %struct.ScmObj* %k47432, %struct.ScmObj** %stackaddr$prim56875, align 8
%stackaddr$prim56876 = alloca %struct.ScmObj*, align 8
%current_45args55525 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55524)
store volatile %struct.ScmObj* %current_45args55525, %struct.ScmObj** %stackaddr$prim56876, align 8
%stackaddr$prim56877 = alloca %struct.ScmObj*, align 8
%ls047170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55525)
store volatile %struct.ScmObj* %ls047170, %struct.ScmObj** %stackaddr$prim56877, align 8
%stackaddr$prim56878 = alloca %struct.ScmObj*, align 8
%current_45args55526 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55525)
store volatile %struct.ScmObj* %current_45args55526, %struct.ScmObj** %stackaddr$prim56878, align 8
%stackaddr$prim56879 = alloca %struct.ScmObj*, align 8
%ls147169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55526)
store volatile %struct.ScmObj* %ls147169, %struct.ScmObj** %stackaddr$prim56879, align 8
%stackaddr$prim56880 = alloca %struct.ScmObj*, align 8
%anf_45bind47277 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls047170)
store volatile %struct.ScmObj* %anf_45bind47277, %struct.ScmObj** %stackaddr$prim56880, align 8
%truthy$cmp56881 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47277)
%cmp$cmp56881 = icmp eq i64 %truthy$cmp56881, 1
br i1 %cmp$cmp56881, label %truebranch$cmp56881, label %falsebranch$cmp56881
truebranch$cmp56881:
%ae49296 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55528$k474320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56882 = alloca %struct.ScmObj*, align 8
%argslist55528$k474321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147169, %struct.ScmObj* %argslist55528$k474320)
store volatile %struct.ScmObj* %argslist55528$k474321, %struct.ScmObj** %stackaddr$prim56882, align 8
%stackaddr$prim56883 = alloca %struct.ScmObj*, align 8
%argslist55528$k474322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49296, %struct.ScmObj* %argslist55528$k474321)
store volatile %struct.ScmObj* %argslist55528$k474322, %struct.ScmObj** %stackaddr$prim56883, align 8
%clofunc56884 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47432)
musttail call tailcc void %clofunc56884(%struct.ScmObj* %k47432, %struct.ScmObj* %argslist55528$k474322)
ret void
falsebranch$cmp56881:
%stackaddr$prim56885 = alloca %struct.ScmObj*, align 8
%anf_45bind47278 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls047170)
store volatile %struct.ScmObj* %anf_45bind47278, %struct.ScmObj** %stackaddr$prim56885, align 8
%ae49303 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56886 = alloca %struct.ScmObj*, align 8
%anf_45bind47279 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47167, %struct.ScmObj* %ae49303)
store volatile %struct.ScmObj* %anf_45bind47279, %struct.ScmObj** %stackaddr$prim56886, align 8
%stackaddr$prim56887 = alloca %struct.ScmObj*, align 8
%anf_45bind47280 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls047170)
store volatile %struct.ScmObj* %anf_45bind47280, %struct.ScmObj** %stackaddr$prim56887, align 8
%stackaddr$makeclosure56888 = alloca %struct.ScmObj*, align 8
%fptrToInt56889 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49306 to i64
%ae49306 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56889)
store volatile %struct.ScmObj* %ae49306, %struct.ScmObj** %stackaddr$makeclosure56888, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49306, %struct.ScmObj* %anf_45bind47278, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49306, %struct.ScmObj* %k47432, i64 1)
%argslist55533$anf_45bind472790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56890 = alloca %struct.ScmObj*, align 8
%argslist55533$anf_45bind472791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147169, %struct.ScmObj* %argslist55533$anf_45bind472790)
store volatile %struct.ScmObj* %argslist55533$anf_45bind472791, %struct.ScmObj** %stackaddr$prim56890, align 8
%stackaddr$prim56891 = alloca %struct.ScmObj*, align 8
%argslist55533$anf_45bind472792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47280, %struct.ScmObj* %argslist55533$anf_45bind472791)
store volatile %struct.ScmObj* %argslist55533$anf_45bind472792, %struct.ScmObj** %stackaddr$prim56891, align 8
%stackaddr$prim56892 = alloca %struct.ScmObj*, align 8
%argslist55533$anf_45bind472793 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49306, %struct.ScmObj* %argslist55533$anf_45bind472792)
store volatile %struct.ScmObj* %argslist55533$anf_45bind472793, %struct.ScmObj** %stackaddr$prim56892, align 8
%clofunc56893 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47279)
musttail call tailcc void %clofunc56893(%struct.ScmObj* %anf_45bind47279, %struct.ScmObj* %argslist55533$anf_45bind472793)
ret void
}

define tailcc void @proc_clo$ae49306(%struct.ScmObj* %env$ae49306,%struct.ScmObj* %current_45args55529) {
%stackaddr$env-ref56894 = alloca %struct.ScmObj*, align 8
%anf_45bind47278 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49306, i64 0)
store %struct.ScmObj* %anf_45bind47278, %struct.ScmObj** %stackaddr$env-ref56894
%stackaddr$env-ref56895 = alloca %struct.ScmObj*, align 8
%k47432 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49306, i64 1)
store %struct.ScmObj* %k47432, %struct.ScmObj** %stackaddr$env-ref56895
%stackaddr$prim56896 = alloca %struct.ScmObj*, align 8
%_95k47433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55529)
store volatile %struct.ScmObj* %_95k47433, %struct.ScmObj** %stackaddr$prim56896, align 8
%stackaddr$prim56897 = alloca %struct.ScmObj*, align 8
%current_45args55530 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55529)
store volatile %struct.ScmObj* %current_45args55530, %struct.ScmObj** %stackaddr$prim56897, align 8
%stackaddr$prim56898 = alloca %struct.ScmObj*, align 8
%anf_45bind47281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55530)
store volatile %struct.ScmObj* %anf_45bind47281, %struct.ScmObj** %stackaddr$prim56898, align 8
%stackaddr$prim56899 = alloca %struct.ScmObj*, align 8
%cpsprim47434 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47278, %struct.ScmObj* %anf_45bind47281)
store volatile %struct.ScmObj* %cpsprim47434, %struct.ScmObj** %stackaddr$prim56899, align 8
%ae49312 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55532$k474320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56900 = alloca %struct.ScmObj*, align 8
%argslist55532$k474321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47434, %struct.ScmObj* %argslist55532$k474320)
store volatile %struct.ScmObj* %argslist55532$k474321, %struct.ScmObj** %stackaddr$prim56900, align 8
%stackaddr$prim56901 = alloca %struct.ScmObj*, align 8
%argslist55532$k474322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49312, %struct.ScmObj* %argslist55532$k474321)
store volatile %struct.ScmObj* %argslist55532$k474322, %struct.ScmObj** %stackaddr$prim56901, align 8
%clofunc56902 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47432)
musttail call tailcc void %clofunc56902(%struct.ScmObj* %k47432, %struct.ScmObj* %argslist55532$k474322)
ret void
}

define tailcc void @proc_clo$ae49266(%struct.ScmObj* %env$ae49266,%struct.ScmObj* %current_45args55535) {
%stackaddr$prim56903 = alloca %struct.ScmObj*, align 8
%k47435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55535)
store volatile %struct.ScmObj* %k47435, %struct.ScmObj** %stackaddr$prim56903, align 8
%stackaddr$prim56904 = alloca %struct.ScmObj*, align 8
%current_45args55536 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55535)
store volatile %struct.ScmObj* %current_45args55536, %struct.ScmObj** %stackaddr$prim56904, align 8
%stackaddr$prim56905 = alloca %struct.ScmObj*, align 8
%a47173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55536)
store volatile %struct.ScmObj* %a47173, %struct.ScmObj** %stackaddr$prim56905, align 8
%stackaddr$prim56906 = alloca %struct.ScmObj*, align 8
%current_45args55537 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55536)
store volatile %struct.ScmObj* %current_45args55537, %struct.ScmObj** %stackaddr$prim56906, align 8
%stackaddr$prim56907 = alloca %struct.ScmObj*, align 8
%b47172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55537)
store volatile %struct.ScmObj* %b47172, %struct.ScmObj** %stackaddr$prim56907, align 8
%stackaddr$prim56908 = alloca %struct.ScmObj*, align 8
%anf_45bind47276 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a47173, %struct.ScmObj* %b47172)
store volatile %struct.ScmObj* %anf_45bind47276, %struct.ScmObj** %stackaddr$prim56908, align 8
%stackaddr$prim56909 = alloca %struct.ScmObj*, align 8
%cpsprim47436 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47276)
store volatile %struct.ScmObj* %cpsprim47436, %struct.ScmObj** %stackaddr$prim56909, align 8
%ae49271 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55539$k474350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56910 = alloca %struct.ScmObj*, align 8
%argslist55539$k474351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47436, %struct.ScmObj* %argslist55539$k474350)
store volatile %struct.ScmObj* %argslist55539$k474351, %struct.ScmObj** %stackaddr$prim56910, align 8
%stackaddr$prim56911 = alloca %struct.ScmObj*, align 8
%argslist55539$k474352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49271, %struct.ScmObj* %argslist55539$k474351)
store volatile %struct.ScmObj* %argslist55539$k474352, %struct.ScmObj** %stackaddr$prim56911, align 8
%clofunc56912 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47435)
musttail call tailcc void %clofunc56912(%struct.ScmObj* %k47435, %struct.ScmObj* %argslist55539$k474352)
ret void
}

define tailcc void @proc_clo$ae49242(%struct.ScmObj* %env$ae49242,%struct.ScmObj* %current_45args55541) {
%stackaddr$prim56913 = alloca %struct.ScmObj*, align 8
%k47437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55541)
store volatile %struct.ScmObj* %k47437, %struct.ScmObj** %stackaddr$prim56913, align 8
%stackaddr$prim56914 = alloca %struct.ScmObj*, align 8
%current_45args55542 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55541)
store volatile %struct.ScmObj* %current_45args55542, %struct.ScmObj** %stackaddr$prim56914, align 8
%stackaddr$prim56915 = alloca %struct.ScmObj*, align 8
%a47176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55542)
store volatile %struct.ScmObj* %a47176, %struct.ScmObj** %stackaddr$prim56915, align 8
%stackaddr$prim56916 = alloca %struct.ScmObj*, align 8
%current_45args55543 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55542)
store volatile %struct.ScmObj* %current_45args55543, %struct.ScmObj** %stackaddr$prim56916, align 8
%stackaddr$prim56917 = alloca %struct.ScmObj*, align 8
%b47175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55543)
store volatile %struct.ScmObj* %b47175, %struct.ScmObj** %stackaddr$prim56917, align 8
%stackaddr$prim56918 = alloca %struct.ScmObj*, align 8
%anf_45bind47275 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a47176, %struct.ScmObj* %b47175)
store volatile %struct.ScmObj* %anf_45bind47275, %struct.ScmObj** %stackaddr$prim56918, align 8
%stackaddr$prim56919 = alloca %struct.ScmObj*, align 8
%cpsprim47438 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47275)
store volatile %struct.ScmObj* %cpsprim47438, %struct.ScmObj** %stackaddr$prim56919, align 8
%ae49247 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55545$k474370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56920 = alloca %struct.ScmObj*, align 8
%argslist55545$k474371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47438, %struct.ScmObj* %argslist55545$k474370)
store volatile %struct.ScmObj* %argslist55545$k474371, %struct.ScmObj** %stackaddr$prim56920, align 8
%stackaddr$prim56921 = alloca %struct.ScmObj*, align 8
%argslist55545$k474372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49247, %struct.ScmObj* %argslist55545$k474371)
store volatile %struct.ScmObj* %argslist55545$k474372, %struct.ScmObj** %stackaddr$prim56921, align 8
%clofunc56922 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47437)
musttail call tailcc void %clofunc56922(%struct.ScmObj* %k47437, %struct.ScmObj* %argslist55545$k474372)
ret void
}

define tailcc void @proc_clo$ae48848(%struct.ScmObj* %env$ae48848,%struct.ScmObj* %current_45args55548) {
%stackaddr$env-ref56923 = alloca %struct.ScmObj*, align 8
%_37map147126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48848, i64 0)
store %struct.ScmObj* %_37map147126, %struct.ScmObj** %stackaddr$env-ref56923
%stackaddr$env-ref56924 = alloca %struct.ScmObj*, align 8
%_37foldr47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48848, i64 1)
store %struct.ScmObj* %_37foldr47100, %struct.ScmObj** %stackaddr$env-ref56924
%stackaddr$env-ref56925 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48848, i64 2)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref56925
%stackaddr$prim56926 = alloca %struct.ScmObj*, align 8
%k47439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55548)
store volatile %struct.ScmObj* %k47439, %struct.ScmObj** %stackaddr$prim56926, align 8
%stackaddr$prim56927 = alloca %struct.ScmObj*, align 8
%current_45args55549 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55548)
store volatile %struct.ScmObj* %current_45args55549, %struct.ScmObj** %stackaddr$prim56927, align 8
%stackaddr$prim56928 = alloca %struct.ScmObj*, align 8
%_37foldl47178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55549)
store volatile %struct.ScmObj* %_37foldl47178, %struct.ScmObj** %stackaddr$prim56928, align 8
%ae48850 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56929 = alloca %struct.ScmObj*, align 8
%fptrToInt56930 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48851 to i64
%ae48851 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56930)
store volatile %struct.ScmObj* %ae48851, %struct.ScmObj** %stackaddr$makeclosure56929, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48851, %struct.ScmObj* %_37map147126, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48851, %struct.ScmObj* %_37foldr47100, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48851, %struct.ScmObj* %_37foldl47178, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48851, %struct.ScmObj* %_37foldr147095, i64 3)
%argslist55606$k474390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56931 = alloca %struct.ScmObj*, align 8
%argslist55606$k474391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48851, %struct.ScmObj* %argslist55606$k474390)
store volatile %struct.ScmObj* %argslist55606$k474391, %struct.ScmObj** %stackaddr$prim56931, align 8
%stackaddr$prim56932 = alloca %struct.ScmObj*, align 8
%argslist55606$k474392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48850, %struct.ScmObj* %argslist55606$k474391)
store volatile %struct.ScmObj* %argslist55606$k474392, %struct.ScmObj** %stackaddr$prim56932, align 8
%clofunc56933 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47439)
musttail call tailcc void %clofunc56933(%struct.ScmObj* %k47439, %struct.ScmObj* %argslist55606$k474392)
ret void
}

define tailcc void @proc_clo$ae48851(%struct.ScmObj* %env$ae48851,%struct.ScmObj* %args4717947440) {
%stackaddr$env-ref56934 = alloca %struct.ScmObj*, align 8
%_37map147126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48851, i64 0)
store %struct.ScmObj* %_37map147126, %struct.ScmObj** %stackaddr$env-ref56934
%stackaddr$env-ref56935 = alloca %struct.ScmObj*, align 8
%_37foldr47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48851, i64 1)
store %struct.ScmObj* %_37foldr47100, %struct.ScmObj** %stackaddr$env-ref56935
%stackaddr$env-ref56936 = alloca %struct.ScmObj*, align 8
%_37foldl47178 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48851, i64 2)
store %struct.ScmObj* %_37foldl47178, %struct.ScmObj** %stackaddr$env-ref56936
%stackaddr$env-ref56937 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48851, i64 3)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref56937
%stackaddr$prim56938 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4717947440)
store volatile %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$prim56938, align 8
%stackaddr$prim56939 = alloca %struct.ScmObj*, align 8
%args47179 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4717947440)
store volatile %struct.ScmObj* %args47179, %struct.ScmObj** %stackaddr$prim56939, align 8
%stackaddr$prim56940 = alloca %struct.ScmObj*, align 8
%f47182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47179)
store volatile %struct.ScmObj* %f47182, %struct.ScmObj** %stackaddr$prim56940, align 8
%stackaddr$prim56941 = alloca %struct.ScmObj*, align 8
%anf_45bind47263 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47179)
store volatile %struct.ScmObj* %anf_45bind47263, %struct.ScmObj** %stackaddr$prim56941, align 8
%stackaddr$prim56942 = alloca %struct.ScmObj*, align 8
%acc47181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47263)
store volatile %struct.ScmObj* %acc47181, %struct.ScmObj** %stackaddr$prim56942, align 8
%stackaddr$prim56943 = alloca %struct.ScmObj*, align 8
%anf_45bind47264 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47179)
store volatile %struct.ScmObj* %anf_45bind47264, %struct.ScmObj** %stackaddr$prim56943, align 8
%stackaddr$prim56944 = alloca %struct.ScmObj*, align 8
%lsts47180 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47264)
store volatile %struct.ScmObj* %lsts47180, %struct.ScmObj** %stackaddr$prim56944, align 8
%stackaddr$makeclosure56945 = alloca %struct.ScmObj*, align 8
%fptrToInt56946 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48859 to i64
%ae48859 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt56946)
store volatile %struct.ScmObj* %ae48859, %struct.ScmObj** %stackaddr$makeclosure56945, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48859, %struct.ScmObj* %lsts47180, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48859, %struct.ScmObj* %_37foldr47100, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48859, %struct.ScmObj* %_37map147126, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48859, %struct.ScmObj* %k47441, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48859, %struct.ScmObj* %f47182, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48859, %struct.ScmObj* %acc47181, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48859, %struct.ScmObj* %_37foldl47178, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48859, %struct.ScmObj* %_37foldr147095, i64 7)
%ae48860 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56947 = alloca %struct.ScmObj*, align 8
%fptrToInt56948 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48861 to i64
%ae48861 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56948)
store volatile %struct.ScmObj* %ae48861, %struct.ScmObj** %stackaddr$makeclosure56947, align 8
%argslist55605$ae488590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56949 = alloca %struct.ScmObj*, align 8
%argslist55605$ae488591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48861, %struct.ScmObj* %argslist55605$ae488590)
store volatile %struct.ScmObj* %argslist55605$ae488591, %struct.ScmObj** %stackaddr$prim56949, align 8
%stackaddr$prim56950 = alloca %struct.ScmObj*, align 8
%argslist55605$ae488592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48860, %struct.ScmObj* %argslist55605$ae488591)
store volatile %struct.ScmObj* %argslist55605$ae488592, %struct.ScmObj** %stackaddr$prim56950, align 8
%clofunc56951 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48859)
musttail call tailcc void %clofunc56951(%struct.ScmObj* %ae48859, %struct.ScmObj* %argslist55605$ae488592)
ret void
}

define tailcc void @proc_clo$ae48859(%struct.ScmObj* %env$ae48859,%struct.ScmObj* %current_45args55551) {
%stackaddr$env-ref56952 = alloca %struct.ScmObj*, align 8
%lsts47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48859, i64 0)
store %struct.ScmObj* %lsts47180, %struct.ScmObj** %stackaddr$env-ref56952
%stackaddr$env-ref56953 = alloca %struct.ScmObj*, align 8
%_37foldr47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48859, i64 1)
store %struct.ScmObj* %_37foldr47100, %struct.ScmObj** %stackaddr$env-ref56953
%stackaddr$env-ref56954 = alloca %struct.ScmObj*, align 8
%_37map147126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48859, i64 2)
store %struct.ScmObj* %_37map147126, %struct.ScmObj** %stackaddr$env-ref56954
%stackaddr$env-ref56955 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48859, i64 3)
store %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$env-ref56955
%stackaddr$env-ref56956 = alloca %struct.ScmObj*, align 8
%f47182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48859, i64 4)
store %struct.ScmObj* %f47182, %struct.ScmObj** %stackaddr$env-ref56956
%stackaddr$env-ref56957 = alloca %struct.ScmObj*, align 8
%acc47181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48859, i64 5)
store %struct.ScmObj* %acc47181, %struct.ScmObj** %stackaddr$env-ref56957
%stackaddr$env-ref56958 = alloca %struct.ScmObj*, align 8
%_37foldl47178 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48859, i64 6)
store %struct.ScmObj* %_37foldl47178, %struct.ScmObj** %stackaddr$env-ref56958
%stackaddr$env-ref56959 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48859, i64 7)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref56959
%stackaddr$prim56960 = alloca %struct.ScmObj*, align 8
%_95k47442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55551)
store volatile %struct.ScmObj* %_95k47442, %struct.ScmObj** %stackaddr$prim56960, align 8
%stackaddr$prim56961 = alloca %struct.ScmObj*, align 8
%current_45args55552 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55551)
store volatile %struct.ScmObj* %current_45args55552, %struct.ScmObj** %stackaddr$prim56961, align 8
%stackaddr$prim56962 = alloca %struct.ScmObj*, align 8
%anf_45bind47265 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55552)
store volatile %struct.ScmObj* %anf_45bind47265, %struct.ScmObj** %stackaddr$prim56962, align 8
%stackaddr$makeclosure56963 = alloca %struct.ScmObj*, align 8
%fptrToInt56964 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48891 to i64
%ae48891 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56964)
store volatile %struct.ScmObj* %ae48891, %struct.ScmObj** %stackaddr$makeclosure56963, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48891, %struct.ScmObj* %lsts47180, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48891, %struct.ScmObj* %_37foldr47100, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48891, %struct.ScmObj* %_37map147126, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48891, %struct.ScmObj* %k47441, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48891, %struct.ScmObj* %f47182, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48891, %struct.ScmObj* %acc47181, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48891, %struct.ScmObj* %_37foldl47178, i64 6)
%ae48893 = call %struct.ScmObj* @const_init_false()
%argslist55598$_37foldr1470950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56965 = alloca %struct.ScmObj*, align 8
%argslist55598$_37foldr1470951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47180, %struct.ScmObj* %argslist55598$_37foldr1470950)
store volatile %struct.ScmObj* %argslist55598$_37foldr1470951, %struct.ScmObj** %stackaddr$prim56965, align 8
%stackaddr$prim56966 = alloca %struct.ScmObj*, align 8
%argslist55598$_37foldr1470952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48893, %struct.ScmObj* %argslist55598$_37foldr1470951)
store volatile %struct.ScmObj* %argslist55598$_37foldr1470952, %struct.ScmObj** %stackaddr$prim56966, align 8
%stackaddr$prim56967 = alloca %struct.ScmObj*, align 8
%argslist55598$_37foldr1470953 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47265, %struct.ScmObj* %argslist55598$_37foldr1470952)
store volatile %struct.ScmObj* %argslist55598$_37foldr1470953, %struct.ScmObj** %stackaddr$prim56967, align 8
%stackaddr$prim56968 = alloca %struct.ScmObj*, align 8
%argslist55598$_37foldr1470954 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48891, %struct.ScmObj* %argslist55598$_37foldr1470953)
store volatile %struct.ScmObj* %argslist55598$_37foldr1470954, %struct.ScmObj** %stackaddr$prim56968, align 8
%clofunc56969 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147095)
musttail call tailcc void %clofunc56969(%struct.ScmObj* %_37foldr147095, %struct.ScmObj* %argslist55598$_37foldr1470954)
ret void
}

define tailcc void @proc_clo$ae48891(%struct.ScmObj* %env$ae48891,%struct.ScmObj* %current_45args55554) {
%stackaddr$env-ref56970 = alloca %struct.ScmObj*, align 8
%lsts47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48891, i64 0)
store %struct.ScmObj* %lsts47180, %struct.ScmObj** %stackaddr$env-ref56970
%stackaddr$env-ref56971 = alloca %struct.ScmObj*, align 8
%_37foldr47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48891, i64 1)
store %struct.ScmObj* %_37foldr47100, %struct.ScmObj** %stackaddr$env-ref56971
%stackaddr$env-ref56972 = alloca %struct.ScmObj*, align 8
%_37map147126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48891, i64 2)
store %struct.ScmObj* %_37map147126, %struct.ScmObj** %stackaddr$env-ref56972
%stackaddr$env-ref56973 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48891, i64 3)
store %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$env-ref56973
%stackaddr$env-ref56974 = alloca %struct.ScmObj*, align 8
%f47182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48891, i64 4)
store %struct.ScmObj* %f47182, %struct.ScmObj** %stackaddr$env-ref56974
%stackaddr$env-ref56975 = alloca %struct.ScmObj*, align 8
%acc47181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48891, i64 5)
store %struct.ScmObj* %acc47181, %struct.ScmObj** %stackaddr$env-ref56975
%stackaddr$env-ref56976 = alloca %struct.ScmObj*, align 8
%_37foldl47178 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48891, i64 6)
store %struct.ScmObj* %_37foldl47178, %struct.ScmObj** %stackaddr$env-ref56976
%stackaddr$prim56977 = alloca %struct.ScmObj*, align 8
%_95k47443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55554)
store volatile %struct.ScmObj* %_95k47443, %struct.ScmObj** %stackaddr$prim56977, align 8
%stackaddr$prim56978 = alloca %struct.ScmObj*, align 8
%current_45args55555 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55554)
store volatile %struct.ScmObj* %current_45args55555, %struct.ScmObj** %stackaddr$prim56978, align 8
%stackaddr$prim56979 = alloca %struct.ScmObj*, align 8
%anf_45bind47266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55555)
store volatile %struct.ScmObj* %anf_45bind47266, %struct.ScmObj** %stackaddr$prim56979, align 8
%truthy$cmp56980 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47266)
%cmp$cmp56980 = icmp eq i64 %truthy$cmp56980, 1
br i1 %cmp$cmp56980, label %truebranch$cmp56980, label %falsebranch$cmp56980
truebranch$cmp56980:
%ae48902 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55557$k474410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56981 = alloca %struct.ScmObj*, align 8
%argslist55557$k474411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47181, %struct.ScmObj* %argslist55557$k474410)
store volatile %struct.ScmObj* %argslist55557$k474411, %struct.ScmObj** %stackaddr$prim56981, align 8
%stackaddr$prim56982 = alloca %struct.ScmObj*, align 8
%argslist55557$k474412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48902, %struct.ScmObj* %argslist55557$k474411)
store volatile %struct.ScmObj* %argslist55557$k474412, %struct.ScmObj** %stackaddr$prim56982, align 8
%clofunc56983 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47441)
musttail call tailcc void %clofunc56983(%struct.ScmObj* %k47441, %struct.ScmObj* %argslist55557$k474412)
ret void
falsebranch$cmp56980:
%stackaddr$makeclosure56984 = alloca %struct.ScmObj*, align 8
%fptrToInt56985 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48907 to i64
%ae48907 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56985)
store volatile %struct.ScmObj* %ae48907, %struct.ScmObj** %stackaddr$makeclosure56984, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48907, %struct.ScmObj* %lsts47180, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48907, %struct.ScmObj* %_37foldr47100, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48907, %struct.ScmObj* %_37map147126, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48907, %struct.ScmObj* %k47441, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48907, %struct.ScmObj* %f47182, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48907, %struct.ScmObj* %acc47181, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48907, %struct.ScmObj* %_37foldl47178, i64 6)
%ae48908 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56986 = alloca %struct.ScmObj*, align 8
%fptrToInt56987 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48909 to i64
%ae48909 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56987)
store volatile %struct.ScmObj* %ae48909, %struct.ScmObj** %stackaddr$makeclosure56986, align 8
%argslist55597$ae489070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56988 = alloca %struct.ScmObj*, align 8
%argslist55597$ae489071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48909, %struct.ScmObj* %argslist55597$ae489070)
store volatile %struct.ScmObj* %argslist55597$ae489071, %struct.ScmObj** %stackaddr$prim56988, align 8
%stackaddr$prim56989 = alloca %struct.ScmObj*, align 8
%argslist55597$ae489072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48908, %struct.ScmObj* %argslist55597$ae489071)
store volatile %struct.ScmObj* %argslist55597$ae489072, %struct.ScmObj** %stackaddr$prim56989, align 8
%clofunc56990 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48907)
musttail call tailcc void %clofunc56990(%struct.ScmObj* %ae48907, %struct.ScmObj* %argslist55597$ae489072)
ret void
}

define tailcc void @proc_clo$ae48907(%struct.ScmObj* %env$ae48907,%struct.ScmObj* %current_45args55558) {
%stackaddr$env-ref56991 = alloca %struct.ScmObj*, align 8
%lsts47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48907, i64 0)
store %struct.ScmObj* %lsts47180, %struct.ScmObj** %stackaddr$env-ref56991
%stackaddr$env-ref56992 = alloca %struct.ScmObj*, align 8
%_37foldr47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48907, i64 1)
store %struct.ScmObj* %_37foldr47100, %struct.ScmObj** %stackaddr$env-ref56992
%stackaddr$env-ref56993 = alloca %struct.ScmObj*, align 8
%_37map147126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48907, i64 2)
store %struct.ScmObj* %_37map147126, %struct.ScmObj** %stackaddr$env-ref56993
%stackaddr$env-ref56994 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48907, i64 3)
store %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$env-ref56994
%stackaddr$env-ref56995 = alloca %struct.ScmObj*, align 8
%f47182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48907, i64 4)
store %struct.ScmObj* %f47182, %struct.ScmObj** %stackaddr$env-ref56995
%stackaddr$env-ref56996 = alloca %struct.ScmObj*, align 8
%acc47181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48907, i64 5)
store %struct.ScmObj* %acc47181, %struct.ScmObj** %stackaddr$env-ref56996
%stackaddr$env-ref56997 = alloca %struct.ScmObj*, align 8
%_37foldl47178 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48907, i64 6)
store %struct.ScmObj* %_37foldl47178, %struct.ScmObj** %stackaddr$env-ref56997
%stackaddr$prim56998 = alloca %struct.ScmObj*, align 8
%_95k47444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55558)
store volatile %struct.ScmObj* %_95k47444, %struct.ScmObj** %stackaddr$prim56998, align 8
%stackaddr$prim56999 = alloca %struct.ScmObj*, align 8
%current_45args55559 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55558)
store volatile %struct.ScmObj* %current_45args55559, %struct.ScmObj** %stackaddr$prim56999, align 8
%stackaddr$prim57000 = alloca %struct.ScmObj*, align 8
%anf_45bind47267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55559)
store volatile %struct.ScmObj* %anf_45bind47267, %struct.ScmObj** %stackaddr$prim57000, align 8
%stackaddr$makeclosure57001 = alloca %struct.ScmObj*, align 8
%fptrToInt57002 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48928 to i64
%ae48928 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57002)
store volatile %struct.ScmObj* %ae48928, %struct.ScmObj** %stackaddr$makeclosure57001, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48928, %struct.ScmObj* %lsts47180, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48928, %struct.ScmObj* %_37foldr47100, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48928, %struct.ScmObj* %_37map147126, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48928, %struct.ScmObj* %k47441, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48928, %struct.ScmObj* %f47182, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48928, %struct.ScmObj* %acc47181, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48928, %struct.ScmObj* %_37foldl47178, i64 6)
%argslist55592$_37map1471260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57003 = alloca %struct.ScmObj*, align 8
%argslist55592$_37map1471261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47180, %struct.ScmObj* %argslist55592$_37map1471260)
store volatile %struct.ScmObj* %argslist55592$_37map1471261, %struct.ScmObj** %stackaddr$prim57003, align 8
%stackaddr$prim57004 = alloca %struct.ScmObj*, align 8
%argslist55592$_37map1471262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47267, %struct.ScmObj* %argslist55592$_37map1471261)
store volatile %struct.ScmObj* %argslist55592$_37map1471262, %struct.ScmObj** %stackaddr$prim57004, align 8
%stackaddr$prim57005 = alloca %struct.ScmObj*, align 8
%argslist55592$_37map1471263 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48928, %struct.ScmObj* %argslist55592$_37map1471262)
store volatile %struct.ScmObj* %argslist55592$_37map1471263, %struct.ScmObj** %stackaddr$prim57005, align 8
%clofunc57006 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147126)
musttail call tailcc void %clofunc57006(%struct.ScmObj* %_37map147126, %struct.ScmObj* %argslist55592$_37map1471263)
ret void
}

define tailcc void @proc_clo$ae48928(%struct.ScmObj* %env$ae48928,%struct.ScmObj* %current_45args55561) {
%stackaddr$env-ref57007 = alloca %struct.ScmObj*, align 8
%lsts47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48928, i64 0)
store %struct.ScmObj* %lsts47180, %struct.ScmObj** %stackaddr$env-ref57007
%stackaddr$env-ref57008 = alloca %struct.ScmObj*, align 8
%_37foldr47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48928, i64 1)
store %struct.ScmObj* %_37foldr47100, %struct.ScmObj** %stackaddr$env-ref57008
%stackaddr$env-ref57009 = alloca %struct.ScmObj*, align 8
%_37map147126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48928, i64 2)
store %struct.ScmObj* %_37map147126, %struct.ScmObj** %stackaddr$env-ref57009
%stackaddr$env-ref57010 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48928, i64 3)
store %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$env-ref57010
%stackaddr$env-ref57011 = alloca %struct.ScmObj*, align 8
%f47182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48928, i64 4)
store %struct.ScmObj* %f47182, %struct.ScmObj** %stackaddr$env-ref57011
%stackaddr$env-ref57012 = alloca %struct.ScmObj*, align 8
%acc47181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48928, i64 5)
store %struct.ScmObj* %acc47181, %struct.ScmObj** %stackaddr$env-ref57012
%stackaddr$env-ref57013 = alloca %struct.ScmObj*, align 8
%_37foldl47178 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48928, i64 6)
store %struct.ScmObj* %_37foldl47178, %struct.ScmObj** %stackaddr$env-ref57013
%stackaddr$prim57014 = alloca %struct.ScmObj*, align 8
%_95k47445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55561)
store volatile %struct.ScmObj* %_95k47445, %struct.ScmObj** %stackaddr$prim57014, align 8
%stackaddr$prim57015 = alloca %struct.ScmObj*, align 8
%current_45args55562 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55561)
store volatile %struct.ScmObj* %current_45args55562, %struct.ScmObj** %stackaddr$prim57015, align 8
%stackaddr$prim57016 = alloca %struct.ScmObj*, align 8
%lsts_4347187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55562)
store volatile %struct.ScmObj* %lsts_4347187, %struct.ScmObj** %stackaddr$prim57016, align 8
%stackaddr$makeclosure57017 = alloca %struct.ScmObj*, align 8
%fptrToInt57018 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48931 to i64
%ae48931 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt57018)
store volatile %struct.ScmObj* %ae48931, %struct.ScmObj** %stackaddr$makeclosure57017, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48931, %struct.ScmObj* %lsts47180, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48931, %struct.ScmObj* %_37foldr47100, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48931, %struct.ScmObj* %_37map147126, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48931, %struct.ScmObj* %lsts_4347187, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48931, %struct.ScmObj* %k47441, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48931, %struct.ScmObj* %f47182, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48931, %struct.ScmObj* %acc47181, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48931, %struct.ScmObj* %_37foldl47178, i64 7)
%ae48932 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57019 = alloca %struct.ScmObj*, align 8
%fptrToInt57020 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48933 to i64
%ae48933 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57020)
store volatile %struct.ScmObj* %ae48933, %struct.ScmObj** %stackaddr$makeclosure57019, align 8
%argslist55591$ae489310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57021 = alloca %struct.ScmObj*, align 8
%argslist55591$ae489311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48933, %struct.ScmObj* %argslist55591$ae489310)
store volatile %struct.ScmObj* %argslist55591$ae489311, %struct.ScmObj** %stackaddr$prim57021, align 8
%stackaddr$prim57022 = alloca %struct.ScmObj*, align 8
%argslist55591$ae489312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48932, %struct.ScmObj* %argslist55591$ae489311)
store volatile %struct.ScmObj* %argslist55591$ae489312, %struct.ScmObj** %stackaddr$prim57022, align 8
%clofunc57023 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48931)
musttail call tailcc void %clofunc57023(%struct.ScmObj* %ae48931, %struct.ScmObj* %argslist55591$ae489312)
ret void
}

define tailcc void @proc_clo$ae48931(%struct.ScmObj* %env$ae48931,%struct.ScmObj* %current_45args55564) {
%stackaddr$env-ref57024 = alloca %struct.ScmObj*, align 8
%lsts47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48931, i64 0)
store %struct.ScmObj* %lsts47180, %struct.ScmObj** %stackaddr$env-ref57024
%stackaddr$env-ref57025 = alloca %struct.ScmObj*, align 8
%_37foldr47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48931, i64 1)
store %struct.ScmObj* %_37foldr47100, %struct.ScmObj** %stackaddr$env-ref57025
%stackaddr$env-ref57026 = alloca %struct.ScmObj*, align 8
%_37map147126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48931, i64 2)
store %struct.ScmObj* %_37map147126, %struct.ScmObj** %stackaddr$env-ref57026
%stackaddr$env-ref57027 = alloca %struct.ScmObj*, align 8
%lsts_4347187 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48931, i64 3)
store %struct.ScmObj* %lsts_4347187, %struct.ScmObj** %stackaddr$env-ref57027
%stackaddr$env-ref57028 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48931, i64 4)
store %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$env-ref57028
%stackaddr$env-ref57029 = alloca %struct.ScmObj*, align 8
%f47182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48931, i64 5)
store %struct.ScmObj* %f47182, %struct.ScmObj** %stackaddr$env-ref57029
%stackaddr$env-ref57030 = alloca %struct.ScmObj*, align 8
%acc47181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48931, i64 6)
store %struct.ScmObj* %acc47181, %struct.ScmObj** %stackaddr$env-ref57030
%stackaddr$env-ref57031 = alloca %struct.ScmObj*, align 8
%_37foldl47178 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48931, i64 7)
store %struct.ScmObj* %_37foldl47178, %struct.ScmObj** %stackaddr$env-ref57031
%stackaddr$prim57032 = alloca %struct.ScmObj*, align 8
%_95k47446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55564)
store volatile %struct.ScmObj* %_95k47446, %struct.ScmObj** %stackaddr$prim57032, align 8
%stackaddr$prim57033 = alloca %struct.ScmObj*, align 8
%current_45args55565 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55564)
store volatile %struct.ScmObj* %current_45args55565, %struct.ScmObj** %stackaddr$prim57033, align 8
%stackaddr$prim57034 = alloca %struct.ScmObj*, align 8
%anf_45bind47268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55565)
store volatile %struct.ScmObj* %anf_45bind47268, %struct.ScmObj** %stackaddr$prim57034, align 8
%stackaddr$makeclosure57035 = alloca %struct.ScmObj*, align 8
%fptrToInt57036 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48952 to i64
%ae48952 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt57036)
store volatile %struct.ScmObj* %ae48952, %struct.ScmObj** %stackaddr$makeclosure57035, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48952, %struct.ScmObj* %lsts_4347187, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48952, %struct.ScmObj* %k47441, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48952, %struct.ScmObj* %f47182, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48952, %struct.ScmObj* %acc47181, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48952, %struct.ScmObj* %_37foldr47100, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48952, %struct.ScmObj* %_37foldl47178, i64 5)
%argslist55586$_37map1471260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57037 = alloca %struct.ScmObj*, align 8
%argslist55586$_37map1471261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47180, %struct.ScmObj* %argslist55586$_37map1471260)
store volatile %struct.ScmObj* %argslist55586$_37map1471261, %struct.ScmObj** %stackaddr$prim57037, align 8
%stackaddr$prim57038 = alloca %struct.ScmObj*, align 8
%argslist55586$_37map1471262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47268, %struct.ScmObj* %argslist55586$_37map1471261)
store volatile %struct.ScmObj* %argslist55586$_37map1471262, %struct.ScmObj** %stackaddr$prim57038, align 8
%stackaddr$prim57039 = alloca %struct.ScmObj*, align 8
%argslist55586$_37map1471263 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48952, %struct.ScmObj* %argslist55586$_37map1471262)
store volatile %struct.ScmObj* %argslist55586$_37map1471263, %struct.ScmObj** %stackaddr$prim57039, align 8
%clofunc57040 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147126)
musttail call tailcc void %clofunc57040(%struct.ScmObj* %_37map147126, %struct.ScmObj* %argslist55586$_37map1471263)
ret void
}

define tailcc void @proc_clo$ae48952(%struct.ScmObj* %env$ae48952,%struct.ScmObj* %current_45args55567) {
%stackaddr$env-ref57041 = alloca %struct.ScmObj*, align 8
%lsts_4347187 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48952, i64 0)
store %struct.ScmObj* %lsts_4347187, %struct.ScmObj** %stackaddr$env-ref57041
%stackaddr$env-ref57042 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48952, i64 1)
store %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$env-ref57042
%stackaddr$env-ref57043 = alloca %struct.ScmObj*, align 8
%f47182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48952, i64 2)
store %struct.ScmObj* %f47182, %struct.ScmObj** %stackaddr$env-ref57043
%stackaddr$env-ref57044 = alloca %struct.ScmObj*, align 8
%acc47181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48952, i64 3)
store %struct.ScmObj* %acc47181, %struct.ScmObj** %stackaddr$env-ref57044
%stackaddr$env-ref57045 = alloca %struct.ScmObj*, align 8
%_37foldr47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48952, i64 4)
store %struct.ScmObj* %_37foldr47100, %struct.ScmObj** %stackaddr$env-ref57045
%stackaddr$env-ref57046 = alloca %struct.ScmObj*, align 8
%_37foldl47178 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48952, i64 5)
store %struct.ScmObj* %_37foldl47178, %struct.ScmObj** %stackaddr$env-ref57046
%stackaddr$prim57047 = alloca %struct.ScmObj*, align 8
%_95k47447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55567)
store volatile %struct.ScmObj* %_95k47447, %struct.ScmObj** %stackaddr$prim57047, align 8
%stackaddr$prim57048 = alloca %struct.ScmObj*, align 8
%current_45args55568 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55567)
store volatile %struct.ScmObj* %current_45args55568, %struct.ScmObj** %stackaddr$prim57048, align 8
%stackaddr$prim57049 = alloca %struct.ScmObj*, align 8
%vs47185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55568)
store volatile %struct.ScmObj* %vs47185, %struct.ScmObj** %stackaddr$prim57049, align 8
%stackaddr$makeclosure57050 = alloca %struct.ScmObj*, align 8
%fptrToInt57051 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48955 to i64
%ae48955 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57051)
store volatile %struct.ScmObj* %ae48955, %struct.ScmObj** %stackaddr$makeclosure57050, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48955, %struct.ScmObj* %k47441, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48955, %struct.ScmObj* %vs47185, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48955, %struct.ScmObj* %lsts_4347187, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48955, %struct.ScmObj* %f47182, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48955, %struct.ScmObj* %acc47181, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48955, %struct.ScmObj* %_37foldr47100, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48955, %struct.ScmObj* %_37foldl47178, i64 6)
%ae48956 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57052 = alloca %struct.ScmObj*, align 8
%fptrToInt57053 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48957 to i64
%ae48957 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57053)
store volatile %struct.ScmObj* %ae48957, %struct.ScmObj** %stackaddr$makeclosure57052, align 8
%argslist55585$ae489550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57054 = alloca %struct.ScmObj*, align 8
%argslist55585$ae489551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48957, %struct.ScmObj* %argslist55585$ae489550)
store volatile %struct.ScmObj* %argslist55585$ae489551, %struct.ScmObj** %stackaddr$prim57054, align 8
%stackaddr$prim57055 = alloca %struct.ScmObj*, align 8
%argslist55585$ae489552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48956, %struct.ScmObj* %argslist55585$ae489551)
store volatile %struct.ScmObj* %argslist55585$ae489552, %struct.ScmObj** %stackaddr$prim57055, align 8
%clofunc57056 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48955)
musttail call tailcc void %clofunc57056(%struct.ScmObj* %ae48955, %struct.ScmObj* %argslist55585$ae489552)
ret void
}

define tailcc void @proc_clo$ae48955(%struct.ScmObj* %env$ae48955,%struct.ScmObj* %current_45args55570) {
%stackaddr$env-ref57057 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48955, i64 0)
store %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$env-ref57057
%stackaddr$env-ref57058 = alloca %struct.ScmObj*, align 8
%vs47185 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48955, i64 1)
store %struct.ScmObj* %vs47185, %struct.ScmObj** %stackaddr$env-ref57058
%stackaddr$env-ref57059 = alloca %struct.ScmObj*, align 8
%lsts_4347187 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48955, i64 2)
store %struct.ScmObj* %lsts_4347187, %struct.ScmObj** %stackaddr$env-ref57059
%stackaddr$env-ref57060 = alloca %struct.ScmObj*, align 8
%f47182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48955, i64 3)
store %struct.ScmObj* %f47182, %struct.ScmObj** %stackaddr$env-ref57060
%stackaddr$env-ref57061 = alloca %struct.ScmObj*, align 8
%acc47181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48955, i64 4)
store %struct.ScmObj* %acc47181, %struct.ScmObj** %stackaddr$env-ref57061
%stackaddr$env-ref57062 = alloca %struct.ScmObj*, align 8
%_37foldr47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48955, i64 5)
store %struct.ScmObj* %_37foldr47100, %struct.ScmObj** %stackaddr$env-ref57062
%stackaddr$env-ref57063 = alloca %struct.ScmObj*, align 8
%_37foldl47178 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48955, i64 6)
store %struct.ScmObj* %_37foldl47178, %struct.ScmObj** %stackaddr$env-ref57063
%stackaddr$prim57064 = alloca %struct.ScmObj*, align 8
%_95k47448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55570)
store volatile %struct.ScmObj* %_95k47448, %struct.ScmObj** %stackaddr$prim57064, align 8
%stackaddr$prim57065 = alloca %struct.ScmObj*, align 8
%current_45args55571 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55570)
store volatile %struct.ScmObj* %current_45args55571, %struct.ScmObj** %stackaddr$prim57065, align 8
%stackaddr$prim57066 = alloca %struct.ScmObj*, align 8
%anf_45bind47269 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55571)
store volatile %struct.ScmObj* %anf_45bind47269, %struct.ScmObj** %stackaddr$prim57066, align 8
%ae48978 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57067 = alloca %struct.ScmObj*, align 8
%anf_45bind47270 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47181, %struct.ScmObj* %ae48978)
store volatile %struct.ScmObj* %anf_45bind47270, %struct.ScmObj** %stackaddr$prim57067, align 8
%stackaddr$makeclosure57068 = alloca %struct.ScmObj*, align 8
%fptrToInt57069 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48980 to i64
%ae48980 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57069)
store volatile %struct.ScmObj* %ae48980, %struct.ScmObj** %stackaddr$makeclosure57068, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48980, %struct.ScmObj* %lsts_4347187, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48980, %struct.ScmObj* %k47441, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48980, %struct.ScmObj* %f47182, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48980, %struct.ScmObj* %_37foldl47178, i64 3)
%argslist55579$_37foldr471000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57070 = alloca %struct.ScmObj*, align 8
%argslist55579$_37foldr471001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47185, %struct.ScmObj* %argslist55579$_37foldr471000)
store volatile %struct.ScmObj* %argslist55579$_37foldr471001, %struct.ScmObj** %stackaddr$prim57070, align 8
%stackaddr$prim57071 = alloca %struct.ScmObj*, align 8
%argslist55579$_37foldr471002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47270, %struct.ScmObj* %argslist55579$_37foldr471001)
store volatile %struct.ScmObj* %argslist55579$_37foldr471002, %struct.ScmObj** %stackaddr$prim57071, align 8
%stackaddr$prim57072 = alloca %struct.ScmObj*, align 8
%argslist55579$_37foldr471003 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47269, %struct.ScmObj* %argslist55579$_37foldr471002)
store volatile %struct.ScmObj* %argslist55579$_37foldr471003, %struct.ScmObj** %stackaddr$prim57072, align 8
%stackaddr$prim57073 = alloca %struct.ScmObj*, align 8
%argslist55579$_37foldr471004 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48980, %struct.ScmObj* %argslist55579$_37foldr471003)
store volatile %struct.ScmObj* %argslist55579$_37foldr471004, %struct.ScmObj** %stackaddr$prim57073, align 8
%clofunc57074 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47100)
musttail call tailcc void %clofunc57074(%struct.ScmObj* %_37foldr47100, %struct.ScmObj* %argslist55579$_37foldr471004)
ret void
}

define tailcc void @proc_clo$ae48980(%struct.ScmObj* %env$ae48980,%struct.ScmObj* %current_45args55573) {
%stackaddr$env-ref57075 = alloca %struct.ScmObj*, align 8
%lsts_4347187 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48980, i64 0)
store %struct.ScmObj* %lsts_4347187, %struct.ScmObj** %stackaddr$env-ref57075
%stackaddr$env-ref57076 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48980, i64 1)
store %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$env-ref57076
%stackaddr$env-ref57077 = alloca %struct.ScmObj*, align 8
%f47182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48980, i64 2)
store %struct.ScmObj* %f47182, %struct.ScmObj** %stackaddr$env-ref57077
%stackaddr$env-ref57078 = alloca %struct.ScmObj*, align 8
%_37foldl47178 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48980, i64 3)
store %struct.ScmObj* %_37foldl47178, %struct.ScmObj** %stackaddr$env-ref57078
%stackaddr$prim57079 = alloca %struct.ScmObj*, align 8
%_95k47449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55573)
store volatile %struct.ScmObj* %_95k47449, %struct.ScmObj** %stackaddr$prim57079, align 8
%stackaddr$prim57080 = alloca %struct.ScmObj*, align 8
%current_45args55574 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55573)
store volatile %struct.ScmObj* %current_45args55574, %struct.ScmObj** %stackaddr$prim57080, align 8
%stackaddr$prim57081 = alloca %struct.ScmObj*, align 8
%anf_45bind47271 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55574)
store volatile %struct.ScmObj* %anf_45bind47271, %struct.ScmObj** %stackaddr$prim57081, align 8
%stackaddr$makeclosure57082 = alloca %struct.ScmObj*, align 8
%fptrToInt57083 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48984 to i64
%ae48984 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57083)
store volatile %struct.ScmObj* %ae48984, %struct.ScmObj** %stackaddr$makeclosure57082, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48984, %struct.ScmObj* %lsts_4347187, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48984, %struct.ScmObj* %k47441, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48984, %struct.ScmObj* %f47182, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48984, %struct.ScmObj* %_37foldl47178, i64 3)
%stackaddr$prim57084 = alloca %struct.ScmObj*, align 8
%cpsargs47452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48984, %struct.ScmObj* %anf_45bind47271)
store volatile %struct.ScmObj* %cpsargs47452, %struct.ScmObj** %stackaddr$prim57084, align 8
%clofunc57085 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47182)
musttail call tailcc void %clofunc57085(%struct.ScmObj* %f47182, %struct.ScmObj* %cpsargs47452)
ret void
}

define tailcc void @proc_clo$ae48984(%struct.ScmObj* %env$ae48984,%struct.ScmObj* %current_45args55576) {
%stackaddr$env-ref57086 = alloca %struct.ScmObj*, align 8
%lsts_4347187 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48984, i64 0)
store %struct.ScmObj* %lsts_4347187, %struct.ScmObj** %stackaddr$env-ref57086
%stackaddr$env-ref57087 = alloca %struct.ScmObj*, align 8
%k47441 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48984, i64 1)
store %struct.ScmObj* %k47441, %struct.ScmObj** %stackaddr$env-ref57087
%stackaddr$env-ref57088 = alloca %struct.ScmObj*, align 8
%f47182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48984, i64 2)
store %struct.ScmObj* %f47182, %struct.ScmObj** %stackaddr$env-ref57088
%stackaddr$env-ref57089 = alloca %struct.ScmObj*, align 8
%_37foldl47178 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48984, i64 3)
store %struct.ScmObj* %_37foldl47178, %struct.ScmObj** %stackaddr$env-ref57089
%stackaddr$prim57090 = alloca %struct.ScmObj*, align 8
%_95k47450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55576)
store volatile %struct.ScmObj* %_95k47450, %struct.ScmObj** %stackaddr$prim57090, align 8
%stackaddr$prim57091 = alloca %struct.ScmObj*, align 8
%current_45args55577 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55576)
store volatile %struct.ScmObj* %current_45args55577, %struct.ScmObj** %stackaddr$prim57091, align 8
%stackaddr$prim57092 = alloca %struct.ScmObj*, align 8
%acc_4347189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55577)
store volatile %struct.ScmObj* %acc_4347189, %struct.ScmObj** %stackaddr$prim57092, align 8
%stackaddr$prim57093 = alloca %struct.ScmObj*, align 8
%anf_45bind47272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4347189, %struct.ScmObj* %lsts_4347187)
store volatile %struct.ScmObj* %anf_45bind47272, %struct.ScmObj** %stackaddr$prim57093, align 8
%stackaddr$prim57094 = alloca %struct.ScmObj*, align 8
%anf_45bind47273 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47182, %struct.ScmObj* %anf_45bind47272)
store volatile %struct.ScmObj* %anf_45bind47273, %struct.ScmObj** %stackaddr$prim57094, align 8
%stackaddr$prim57095 = alloca %struct.ScmObj*, align 8
%cpsargs47451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47441, %struct.ScmObj* %anf_45bind47273)
store volatile %struct.ScmObj* %cpsargs47451, %struct.ScmObj** %stackaddr$prim57095, align 8
%clofunc57096 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl47178)
musttail call tailcc void %clofunc57096(%struct.ScmObj* %_37foldl47178, %struct.ScmObj* %cpsargs47451)
ret void
}

define tailcc void @proc_clo$ae48957(%struct.ScmObj* %env$ae48957,%struct.ScmObj* %current_45args55580) {
%stackaddr$prim57097 = alloca %struct.ScmObj*, align 8
%k47453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55580)
store volatile %struct.ScmObj* %k47453, %struct.ScmObj** %stackaddr$prim57097, align 8
%stackaddr$prim57098 = alloca %struct.ScmObj*, align 8
%current_45args55581 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55580)
store volatile %struct.ScmObj* %current_45args55581, %struct.ScmObj** %stackaddr$prim57098, align 8
%stackaddr$prim57099 = alloca %struct.ScmObj*, align 8
%a47191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55581)
store volatile %struct.ScmObj* %a47191, %struct.ScmObj** %stackaddr$prim57099, align 8
%stackaddr$prim57100 = alloca %struct.ScmObj*, align 8
%current_45args55582 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55581)
store volatile %struct.ScmObj* %current_45args55582, %struct.ScmObj** %stackaddr$prim57100, align 8
%stackaddr$prim57101 = alloca %struct.ScmObj*, align 8
%b47190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55582)
store volatile %struct.ScmObj* %b47190, %struct.ScmObj** %stackaddr$prim57101, align 8
%stackaddr$prim57102 = alloca %struct.ScmObj*, align 8
%cpsprim47454 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47191, %struct.ScmObj* %b47190)
store volatile %struct.ScmObj* %cpsprim47454, %struct.ScmObj** %stackaddr$prim57102, align 8
%ae48961 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55584$k474530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57103 = alloca %struct.ScmObj*, align 8
%argslist55584$k474531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47454, %struct.ScmObj* %argslist55584$k474530)
store volatile %struct.ScmObj* %argslist55584$k474531, %struct.ScmObj** %stackaddr$prim57103, align 8
%stackaddr$prim57104 = alloca %struct.ScmObj*, align 8
%argslist55584$k474532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48961, %struct.ScmObj* %argslist55584$k474531)
store volatile %struct.ScmObj* %argslist55584$k474532, %struct.ScmObj** %stackaddr$prim57104, align 8
%clofunc57105 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47453)
musttail call tailcc void %clofunc57105(%struct.ScmObj* %k47453, %struct.ScmObj* %argslist55584$k474532)
ret void
}

define tailcc void @proc_clo$ae48933(%struct.ScmObj* %env$ae48933,%struct.ScmObj* %current_45args55587) {
%stackaddr$prim57106 = alloca %struct.ScmObj*, align 8
%k47455 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55587)
store volatile %struct.ScmObj* %k47455, %struct.ScmObj** %stackaddr$prim57106, align 8
%stackaddr$prim57107 = alloca %struct.ScmObj*, align 8
%current_45args55588 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55587)
store volatile %struct.ScmObj* %current_45args55588, %struct.ScmObj** %stackaddr$prim57107, align 8
%stackaddr$prim57108 = alloca %struct.ScmObj*, align 8
%x47186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55588)
store volatile %struct.ScmObj* %x47186, %struct.ScmObj** %stackaddr$prim57108, align 8
%stackaddr$prim57109 = alloca %struct.ScmObj*, align 8
%cpsprim47456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47186)
store volatile %struct.ScmObj* %cpsprim47456, %struct.ScmObj** %stackaddr$prim57109, align 8
%ae48936 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55590$k474550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57110 = alloca %struct.ScmObj*, align 8
%argslist55590$k474551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47456, %struct.ScmObj* %argslist55590$k474550)
store volatile %struct.ScmObj* %argslist55590$k474551, %struct.ScmObj** %stackaddr$prim57110, align 8
%stackaddr$prim57111 = alloca %struct.ScmObj*, align 8
%argslist55590$k474552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48936, %struct.ScmObj* %argslist55590$k474551)
store volatile %struct.ScmObj* %argslist55590$k474552, %struct.ScmObj** %stackaddr$prim57111, align 8
%clofunc57112 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47455)
musttail call tailcc void %clofunc57112(%struct.ScmObj* %k47455, %struct.ScmObj* %argslist55590$k474552)
ret void
}

define tailcc void @proc_clo$ae48909(%struct.ScmObj* %env$ae48909,%struct.ScmObj* %current_45args55593) {
%stackaddr$prim57113 = alloca %struct.ScmObj*, align 8
%k47457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55593)
store volatile %struct.ScmObj* %k47457, %struct.ScmObj** %stackaddr$prim57113, align 8
%stackaddr$prim57114 = alloca %struct.ScmObj*, align 8
%current_45args55594 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55593)
store volatile %struct.ScmObj* %current_45args55594, %struct.ScmObj** %stackaddr$prim57114, align 8
%stackaddr$prim57115 = alloca %struct.ScmObj*, align 8
%x47188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55594)
store volatile %struct.ScmObj* %x47188, %struct.ScmObj** %stackaddr$prim57115, align 8
%stackaddr$prim57116 = alloca %struct.ScmObj*, align 8
%cpsprim47458 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47188)
store volatile %struct.ScmObj* %cpsprim47458, %struct.ScmObj** %stackaddr$prim57116, align 8
%ae48912 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55596$k474570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57117 = alloca %struct.ScmObj*, align 8
%argslist55596$k474571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47458, %struct.ScmObj* %argslist55596$k474570)
store volatile %struct.ScmObj* %argslist55596$k474571, %struct.ScmObj** %stackaddr$prim57117, align 8
%stackaddr$prim57118 = alloca %struct.ScmObj*, align 8
%argslist55596$k474572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48912, %struct.ScmObj* %argslist55596$k474571)
store volatile %struct.ScmObj* %argslist55596$k474572, %struct.ScmObj** %stackaddr$prim57118, align 8
%clofunc57119 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47457)
musttail call tailcc void %clofunc57119(%struct.ScmObj* %k47457, %struct.ScmObj* %argslist55596$k474572)
ret void
}

define tailcc void @proc_clo$ae48861(%struct.ScmObj* %env$ae48861,%struct.ScmObj* %current_45args55599) {
%stackaddr$prim57120 = alloca %struct.ScmObj*, align 8
%k47459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55599)
store volatile %struct.ScmObj* %k47459, %struct.ScmObj** %stackaddr$prim57120, align 8
%stackaddr$prim57121 = alloca %struct.ScmObj*, align 8
%current_45args55600 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55599)
store volatile %struct.ScmObj* %current_45args55600, %struct.ScmObj** %stackaddr$prim57121, align 8
%stackaddr$prim57122 = alloca %struct.ScmObj*, align 8
%lst47184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55600)
store volatile %struct.ScmObj* %lst47184, %struct.ScmObj** %stackaddr$prim57122, align 8
%stackaddr$prim57123 = alloca %struct.ScmObj*, align 8
%current_45args55601 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55600)
store volatile %struct.ScmObj* %current_45args55601, %struct.ScmObj** %stackaddr$prim57123, align 8
%stackaddr$prim57124 = alloca %struct.ScmObj*, align 8
%b47183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55601)
store volatile %struct.ScmObj* %b47183, %struct.ScmObj** %stackaddr$prim57124, align 8
%truthy$cmp57125 = call i64 @is_truthy_value(%struct.ScmObj* %b47183)
%cmp$cmp57125 = icmp eq i64 %truthy$cmp57125, 1
br i1 %cmp$cmp57125, label %truebranch$cmp57125, label %falsebranch$cmp57125
truebranch$cmp57125:
%ae48864 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55603$k474590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57126 = alloca %struct.ScmObj*, align 8
%argslist55603$k474591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47183, %struct.ScmObj* %argslist55603$k474590)
store volatile %struct.ScmObj* %argslist55603$k474591, %struct.ScmObj** %stackaddr$prim57126, align 8
%stackaddr$prim57127 = alloca %struct.ScmObj*, align 8
%argslist55603$k474592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48864, %struct.ScmObj* %argslist55603$k474591)
store volatile %struct.ScmObj* %argslist55603$k474592, %struct.ScmObj** %stackaddr$prim57127, align 8
%clofunc57128 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47459)
musttail call tailcc void %clofunc57128(%struct.ScmObj* %k47459, %struct.ScmObj* %argslist55603$k474592)
ret void
falsebranch$cmp57125:
%stackaddr$prim57129 = alloca %struct.ScmObj*, align 8
%cpsprim47460 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47184)
store volatile %struct.ScmObj* %cpsprim47460, %struct.ScmObj** %stackaddr$prim57129, align 8
%ae48871 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55604$k474590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57130 = alloca %struct.ScmObj*, align 8
%argslist55604$k474591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47460, %struct.ScmObj* %argslist55604$k474590)
store volatile %struct.ScmObj* %argslist55604$k474591, %struct.ScmObj** %stackaddr$prim57130, align 8
%stackaddr$prim57131 = alloca %struct.ScmObj*, align 8
%argslist55604$k474592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48871, %struct.ScmObj* %argslist55604$k474591)
store volatile %struct.ScmObj* %argslist55604$k474592, %struct.ScmObj** %stackaddr$prim57131, align 8
%clofunc57132 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47459)
musttail call tailcc void %clofunc57132(%struct.ScmObj* %k47459, %struct.ScmObj* %argslist55604$k474592)
ret void
}

define tailcc void @proc_clo$ae48702(%struct.ScmObj* %env$ae48702,%struct.ScmObj* %args4712247461) {
%stackaddr$env-ref57133 = alloca %struct.ScmObj*, align 8
%_37last47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48702, i64 0)
store %struct.ScmObj* %_37last47117, %struct.ScmObj** %stackaddr$env-ref57133
%stackaddr$env-ref57134 = alloca %struct.ScmObj*, align 8
%_37foldr47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48702, i64 1)
store %struct.ScmObj* %_37foldr47100, %struct.ScmObj** %stackaddr$env-ref57134
%stackaddr$env-ref57135 = alloca %struct.ScmObj*, align 8
%_37drop_45right47114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48702, i64 2)
store %struct.ScmObj* %_37drop_45right47114, %struct.ScmObj** %stackaddr$env-ref57135
%stackaddr$prim57136 = alloca %struct.ScmObj*, align 8
%k47462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4712247461)
store volatile %struct.ScmObj* %k47462, %struct.ScmObj** %stackaddr$prim57136, align 8
%stackaddr$prim57137 = alloca %struct.ScmObj*, align 8
%args47122 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4712247461)
store volatile %struct.ScmObj* %args47122, %struct.ScmObj** %stackaddr$prim57137, align 8
%stackaddr$prim57138 = alloca %struct.ScmObj*, align 8
%f47124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47122)
store volatile %struct.ScmObj* %f47124, %struct.ScmObj** %stackaddr$prim57138, align 8
%stackaddr$prim57139 = alloca %struct.ScmObj*, align 8
%lsts47123 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47122)
store volatile %struct.ScmObj* %lsts47123, %struct.ScmObj** %stackaddr$prim57139, align 8
%stackaddr$makeclosure57140 = alloca %struct.ScmObj*, align 8
%fptrToInt57141 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48707 to i64
%ae48707 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57141)
store volatile %struct.ScmObj* %ae48707, %struct.ScmObj** %stackaddr$makeclosure57140, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48707, %struct.ScmObj* %k47462, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48707, %struct.ScmObj* %lsts47123, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48707, %struct.ScmObj* %_37foldr47100, i64 2)
%ae48708 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57142 = alloca %struct.ScmObj*, align 8
%fptrToInt57143 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48709 to i64
%ae48709 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57143)
store volatile %struct.ScmObj* %ae48709, %struct.ScmObj** %stackaddr$makeclosure57142, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48709, %struct.ScmObj* %f47124, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48709, %struct.ScmObj* %_37last47117, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48709, %struct.ScmObj* %_37drop_45right47114, i64 2)
%argslist55623$ae487070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57144 = alloca %struct.ScmObj*, align 8
%argslist55623$ae487071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48709, %struct.ScmObj* %argslist55623$ae487070)
store volatile %struct.ScmObj* %argslist55623$ae487071, %struct.ScmObj** %stackaddr$prim57144, align 8
%stackaddr$prim57145 = alloca %struct.ScmObj*, align 8
%argslist55623$ae487072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48708, %struct.ScmObj* %argslist55623$ae487071)
store volatile %struct.ScmObj* %argslist55623$ae487072, %struct.ScmObj** %stackaddr$prim57145, align 8
%clofunc57146 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48707)
musttail call tailcc void %clofunc57146(%struct.ScmObj* %ae48707, %struct.ScmObj* %argslist55623$ae487072)
ret void
}

define tailcc void @proc_clo$ae48707(%struct.ScmObj* %env$ae48707,%struct.ScmObj* %current_45args55608) {
%stackaddr$env-ref57147 = alloca %struct.ScmObj*, align 8
%k47462 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48707, i64 0)
store %struct.ScmObj* %k47462, %struct.ScmObj** %stackaddr$env-ref57147
%stackaddr$env-ref57148 = alloca %struct.ScmObj*, align 8
%lsts47123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48707, i64 1)
store %struct.ScmObj* %lsts47123, %struct.ScmObj** %stackaddr$env-ref57148
%stackaddr$env-ref57149 = alloca %struct.ScmObj*, align 8
%_37foldr47100 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48707, i64 2)
store %struct.ScmObj* %_37foldr47100, %struct.ScmObj** %stackaddr$env-ref57149
%stackaddr$prim57150 = alloca %struct.ScmObj*, align 8
%_95k47463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55608)
store volatile %struct.ScmObj* %_95k47463, %struct.ScmObj** %stackaddr$prim57150, align 8
%stackaddr$prim57151 = alloca %struct.ScmObj*, align 8
%current_45args55609 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55608)
store volatile %struct.ScmObj* %current_45args55609, %struct.ScmObj** %stackaddr$prim57151, align 8
%stackaddr$prim57152 = alloca %struct.ScmObj*, align 8
%anf_45bind47260 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55609)
store volatile %struct.ScmObj* %anf_45bind47260, %struct.ScmObj** %stackaddr$prim57152, align 8
%ae48770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57153 = alloca %struct.ScmObj*, align 8
%anf_45bind47261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48770, %struct.ScmObj* %lsts47123)
store volatile %struct.ScmObj* %anf_45bind47261, %struct.ScmObj** %stackaddr$prim57153, align 8
%stackaddr$prim57154 = alloca %struct.ScmObj*, align 8
%anf_45bind47262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47260, %struct.ScmObj* %anf_45bind47261)
store volatile %struct.ScmObj* %anf_45bind47262, %struct.ScmObj** %stackaddr$prim57154, align 8
%stackaddr$prim57155 = alloca %struct.ScmObj*, align 8
%cpsargs47464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47462, %struct.ScmObj* %anf_45bind47262)
store volatile %struct.ScmObj* %cpsargs47464, %struct.ScmObj** %stackaddr$prim57155, align 8
%clofunc57156 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47100)
musttail call tailcc void %clofunc57156(%struct.ScmObj* %_37foldr47100, %struct.ScmObj* %cpsargs47464)
ret void
}

define tailcc void @proc_clo$ae48709(%struct.ScmObj* %env$ae48709,%struct.ScmObj* %fargs4712547465) {
%stackaddr$env-ref57157 = alloca %struct.ScmObj*, align 8
%f47124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48709, i64 0)
store %struct.ScmObj* %f47124, %struct.ScmObj** %stackaddr$env-ref57157
%stackaddr$env-ref57158 = alloca %struct.ScmObj*, align 8
%_37last47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48709, i64 1)
store %struct.ScmObj* %_37last47117, %struct.ScmObj** %stackaddr$env-ref57158
%stackaddr$env-ref57159 = alloca %struct.ScmObj*, align 8
%_37drop_45right47114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48709, i64 2)
store %struct.ScmObj* %_37drop_45right47114, %struct.ScmObj** %stackaddr$env-ref57159
%stackaddr$prim57160 = alloca %struct.ScmObj*, align 8
%k47466 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4712547465)
store volatile %struct.ScmObj* %k47466, %struct.ScmObj** %stackaddr$prim57160, align 8
%stackaddr$prim57161 = alloca %struct.ScmObj*, align 8
%fargs47125 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4712547465)
store volatile %struct.ScmObj* %fargs47125, %struct.ScmObj** %stackaddr$prim57161, align 8
%stackaddr$makeclosure57162 = alloca %struct.ScmObj*, align 8
%fptrToInt57163 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48713 to i64
%ae48713 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57163)
store volatile %struct.ScmObj* %ae48713, %struct.ScmObj** %stackaddr$makeclosure57162, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48713, %struct.ScmObj* %fargs47125, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48713, %struct.ScmObj* %f47124, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48713, %struct.ScmObj* %_37last47117, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48713, %struct.ScmObj* %k47466, i64 3)
%ae48715 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist55622$_37drop_45right471140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57164 = alloca %struct.ScmObj*, align 8
%argslist55622$_37drop_45right471141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48715, %struct.ScmObj* %argslist55622$_37drop_45right471140)
store volatile %struct.ScmObj* %argslist55622$_37drop_45right471141, %struct.ScmObj** %stackaddr$prim57164, align 8
%stackaddr$prim57165 = alloca %struct.ScmObj*, align 8
%argslist55622$_37drop_45right471142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47125, %struct.ScmObj* %argslist55622$_37drop_45right471141)
store volatile %struct.ScmObj* %argslist55622$_37drop_45right471142, %struct.ScmObj** %stackaddr$prim57165, align 8
%stackaddr$prim57166 = alloca %struct.ScmObj*, align 8
%argslist55622$_37drop_45right471143 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48713, %struct.ScmObj* %argslist55622$_37drop_45right471142)
store volatile %struct.ScmObj* %argslist55622$_37drop_45right471143, %struct.ScmObj** %stackaddr$prim57166, align 8
%clofunc57167 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right47114)
musttail call tailcc void %clofunc57167(%struct.ScmObj* %_37drop_45right47114, %struct.ScmObj* %argslist55622$_37drop_45right471143)
ret void
}

define tailcc void @proc_clo$ae48713(%struct.ScmObj* %env$ae48713,%struct.ScmObj* %current_45args55611) {
%stackaddr$env-ref57168 = alloca %struct.ScmObj*, align 8
%fargs47125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48713, i64 0)
store %struct.ScmObj* %fargs47125, %struct.ScmObj** %stackaddr$env-ref57168
%stackaddr$env-ref57169 = alloca %struct.ScmObj*, align 8
%f47124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48713, i64 1)
store %struct.ScmObj* %f47124, %struct.ScmObj** %stackaddr$env-ref57169
%stackaddr$env-ref57170 = alloca %struct.ScmObj*, align 8
%_37last47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48713, i64 2)
store %struct.ScmObj* %_37last47117, %struct.ScmObj** %stackaddr$env-ref57170
%stackaddr$env-ref57171 = alloca %struct.ScmObj*, align 8
%k47466 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48713, i64 3)
store %struct.ScmObj* %k47466, %struct.ScmObj** %stackaddr$env-ref57171
%stackaddr$prim57172 = alloca %struct.ScmObj*, align 8
%_95k47467 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55611)
store volatile %struct.ScmObj* %_95k47467, %struct.ScmObj** %stackaddr$prim57172, align 8
%stackaddr$prim57173 = alloca %struct.ScmObj*, align 8
%current_45args55612 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55611)
store volatile %struct.ScmObj* %current_45args55612, %struct.ScmObj** %stackaddr$prim57173, align 8
%stackaddr$prim57174 = alloca %struct.ScmObj*, align 8
%anf_45bind47257 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55612)
store volatile %struct.ScmObj* %anf_45bind47257, %struct.ScmObj** %stackaddr$prim57174, align 8
%stackaddr$makeclosure57175 = alloca %struct.ScmObj*, align 8
%fptrToInt57176 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48720 to i64
%ae48720 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57176)
store volatile %struct.ScmObj* %ae48720, %struct.ScmObj** %stackaddr$makeclosure57175, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48720, %struct.ScmObj* %fargs47125, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48720, %struct.ScmObj* %_37last47117, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48720, %struct.ScmObj* %k47466, i64 2)
%stackaddr$prim57177 = alloca %struct.ScmObj*, align 8
%cpsargs47471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48720, %struct.ScmObj* %anf_45bind47257)
store volatile %struct.ScmObj* %cpsargs47471, %struct.ScmObj** %stackaddr$prim57177, align 8
%clofunc57178 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47124)
musttail call tailcc void %clofunc57178(%struct.ScmObj* %f47124, %struct.ScmObj* %cpsargs47471)
ret void
}

define tailcc void @proc_clo$ae48720(%struct.ScmObj* %env$ae48720,%struct.ScmObj* %current_45args55614) {
%stackaddr$env-ref57179 = alloca %struct.ScmObj*, align 8
%fargs47125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48720, i64 0)
store %struct.ScmObj* %fargs47125, %struct.ScmObj** %stackaddr$env-ref57179
%stackaddr$env-ref57180 = alloca %struct.ScmObj*, align 8
%_37last47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48720, i64 1)
store %struct.ScmObj* %_37last47117, %struct.ScmObj** %stackaddr$env-ref57180
%stackaddr$env-ref57181 = alloca %struct.ScmObj*, align 8
%k47466 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48720, i64 2)
store %struct.ScmObj* %k47466, %struct.ScmObj** %stackaddr$env-ref57181
%stackaddr$prim57182 = alloca %struct.ScmObj*, align 8
%_95k47468 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55614)
store volatile %struct.ScmObj* %_95k47468, %struct.ScmObj** %stackaddr$prim57182, align 8
%stackaddr$prim57183 = alloca %struct.ScmObj*, align 8
%current_45args55615 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55614)
store volatile %struct.ScmObj* %current_45args55615, %struct.ScmObj** %stackaddr$prim57183, align 8
%stackaddr$prim57184 = alloca %struct.ScmObj*, align 8
%anf_45bind47258 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55615)
store volatile %struct.ScmObj* %anf_45bind47258, %struct.ScmObj** %stackaddr$prim57184, align 8
%stackaddr$makeclosure57185 = alloca %struct.ScmObj*, align 8
%fptrToInt57186 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48725 to i64
%ae48725 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57186)
store volatile %struct.ScmObj* %ae48725, %struct.ScmObj** %stackaddr$makeclosure57185, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48725, %struct.ScmObj* %k47466, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48725, %struct.ScmObj* %anf_45bind47258, i64 1)
%argslist55621$_37last471170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57187 = alloca %struct.ScmObj*, align 8
%argslist55621$_37last471171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47125, %struct.ScmObj* %argslist55621$_37last471170)
store volatile %struct.ScmObj* %argslist55621$_37last471171, %struct.ScmObj** %stackaddr$prim57187, align 8
%stackaddr$prim57188 = alloca %struct.ScmObj*, align 8
%argslist55621$_37last471172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48725, %struct.ScmObj* %argslist55621$_37last471171)
store volatile %struct.ScmObj* %argslist55621$_37last471172, %struct.ScmObj** %stackaddr$prim57188, align 8
%clofunc57189 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last47117)
musttail call tailcc void %clofunc57189(%struct.ScmObj* %_37last47117, %struct.ScmObj* %argslist55621$_37last471172)
ret void
}

define tailcc void @proc_clo$ae48725(%struct.ScmObj* %env$ae48725,%struct.ScmObj* %current_45args55617) {
%stackaddr$env-ref57190 = alloca %struct.ScmObj*, align 8
%k47466 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48725, i64 0)
store %struct.ScmObj* %k47466, %struct.ScmObj** %stackaddr$env-ref57190
%stackaddr$env-ref57191 = alloca %struct.ScmObj*, align 8
%anf_45bind47258 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48725, i64 1)
store %struct.ScmObj* %anf_45bind47258, %struct.ScmObj** %stackaddr$env-ref57191
%stackaddr$prim57192 = alloca %struct.ScmObj*, align 8
%_95k47469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55617)
store volatile %struct.ScmObj* %_95k47469, %struct.ScmObj** %stackaddr$prim57192, align 8
%stackaddr$prim57193 = alloca %struct.ScmObj*, align 8
%current_45args55618 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55617)
store volatile %struct.ScmObj* %current_45args55618, %struct.ScmObj** %stackaddr$prim57193, align 8
%stackaddr$prim57194 = alloca %struct.ScmObj*, align 8
%anf_45bind47259 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55618)
store volatile %struct.ScmObj* %anf_45bind47259, %struct.ScmObj** %stackaddr$prim57194, align 8
%stackaddr$prim57195 = alloca %struct.ScmObj*, align 8
%cpsprim47470 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47258, %struct.ScmObj* %anf_45bind47259)
store volatile %struct.ScmObj* %cpsprim47470, %struct.ScmObj** %stackaddr$prim57195, align 8
%ae48730 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55620$k474660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57196 = alloca %struct.ScmObj*, align 8
%argslist55620$k474661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47470, %struct.ScmObj* %argslist55620$k474660)
store volatile %struct.ScmObj* %argslist55620$k474661, %struct.ScmObj** %stackaddr$prim57196, align 8
%stackaddr$prim57197 = alloca %struct.ScmObj*, align 8
%argslist55620$k474662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48730, %struct.ScmObj* %argslist55620$k474661)
store volatile %struct.ScmObj* %argslist55620$k474662, %struct.ScmObj** %stackaddr$prim57197, align 8
%clofunc57198 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47466)
musttail call tailcc void %clofunc57198(%struct.ScmObj* %k47466, %struct.ScmObj* %argslist55620$k474662)
ret void
}

define tailcc void @proc_clo$ae48625(%struct.ScmObj* %env$ae48625,%struct.ScmObj* %current_45args55625) {
%stackaddr$env-ref57199 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48625, i64 0)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref57199
%stackaddr$prim57200 = alloca %struct.ScmObj*, align 8
%k47472 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55625)
store volatile %struct.ScmObj* %k47472, %struct.ScmObj** %stackaddr$prim57200, align 8
%stackaddr$prim57201 = alloca %struct.ScmObj*, align 8
%current_45args55626 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55625)
store volatile %struct.ScmObj* %current_45args55626, %struct.ScmObj** %stackaddr$prim57201, align 8
%stackaddr$prim57202 = alloca %struct.ScmObj*, align 8
%f47128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55626)
store volatile %struct.ScmObj* %f47128, %struct.ScmObj** %stackaddr$prim57202, align 8
%stackaddr$prim57203 = alloca %struct.ScmObj*, align 8
%current_45args55627 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55626)
store volatile %struct.ScmObj* %current_45args55627, %struct.ScmObj** %stackaddr$prim57203, align 8
%stackaddr$prim57204 = alloca %struct.ScmObj*, align 8
%lst47127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55627)
store volatile %struct.ScmObj* %lst47127, %struct.ScmObj** %stackaddr$prim57204, align 8
%stackaddr$makeclosure57205 = alloca %struct.ScmObj*, align 8
%fptrToInt57206 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48626 to i64
%ae48626 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57206)
store volatile %struct.ScmObj* %ae48626, %struct.ScmObj** %stackaddr$makeclosure57205, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48626, %struct.ScmObj* %lst47127, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48626, %struct.ScmObj* %_37foldr147095, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48626, %struct.ScmObj* %k47472, i64 2)
%ae48627 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57207 = alloca %struct.ScmObj*, align 8
%fptrToInt57208 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48628 to i64
%ae48628 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57208)
store volatile %struct.ScmObj* %ae48628, %struct.ScmObj** %stackaddr$makeclosure57207, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48628, %struct.ScmObj* %f47128, i64 0)
%argslist55642$ae486260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57209 = alloca %struct.ScmObj*, align 8
%argslist55642$ae486261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48628, %struct.ScmObj* %argslist55642$ae486260)
store volatile %struct.ScmObj* %argslist55642$ae486261, %struct.ScmObj** %stackaddr$prim57209, align 8
%stackaddr$prim57210 = alloca %struct.ScmObj*, align 8
%argslist55642$ae486262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48627, %struct.ScmObj* %argslist55642$ae486261)
store volatile %struct.ScmObj* %argslist55642$ae486262, %struct.ScmObj** %stackaddr$prim57210, align 8
%clofunc57211 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48626)
musttail call tailcc void %clofunc57211(%struct.ScmObj* %ae48626, %struct.ScmObj* %argslist55642$ae486262)
ret void
}

define tailcc void @proc_clo$ae48626(%struct.ScmObj* %env$ae48626,%struct.ScmObj* %current_45args55629) {
%stackaddr$env-ref57212 = alloca %struct.ScmObj*, align 8
%lst47127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48626, i64 0)
store %struct.ScmObj* %lst47127, %struct.ScmObj** %stackaddr$env-ref57212
%stackaddr$env-ref57213 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48626, i64 1)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref57213
%stackaddr$env-ref57214 = alloca %struct.ScmObj*, align 8
%k47472 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48626, i64 2)
store %struct.ScmObj* %k47472, %struct.ScmObj** %stackaddr$env-ref57214
%stackaddr$prim57215 = alloca %struct.ScmObj*, align 8
%_95k47473 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55629)
store volatile %struct.ScmObj* %_95k47473, %struct.ScmObj** %stackaddr$prim57215, align 8
%stackaddr$prim57216 = alloca %struct.ScmObj*, align 8
%current_45args55630 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55629)
store volatile %struct.ScmObj* %current_45args55630, %struct.ScmObj** %stackaddr$prim57216, align 8
%stackaddr$prim57217 = alloca %struct.ScmObj*, align 8
%anf_45bind47256 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55630)
store volatile %struct.ScmObj* %anf_45bind47256, %struct.ScmObj** %stackaddr$prim57217, align 8
%ae48660 = call %struct.ScmObj* @const_init_null()
%argslist55632$_37foldr1470950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57218 = alloca %struct.ScmObj*, align 8
%argslist55632$_37foldr1470951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47127, %struct.ScmObj* %argslist55632$_37foldr1470950)
store volatile %struct.ScmObj* %argslist55632$_37foldr1470951, %struct.ScmObj** %stackaddr$prim57218, align 8
%stackaddr$prim57219 = alloca %struct.ScmObj*, align 8
%argslist55632$_37foldr1470952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48660, %struct.ScmObj* %argslist55632$_37foldr1470951)
store volatile %struct.ScmObj* %argslist55632$_37foldr1470952, %struct.ScmObj** %stackaddr$prim57219, align 8
%stackaddr$prim57220 = alloca %struct.ScmObj*, align 8
%argslist55632$_37foldr1470953 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47256, %struct.ScmObj* %argslist55632$_37foldr1470952)
store volatile %struct.ScmObj* %argslist55632$_37foldr1470953, %struct.ScmObj** %stackaddr$prim57220, align 8
%stackaddr$prim57221 = alloca %struct.ScmObj*, align 8
%argslist55632$_37foldr1470954 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47472, %struct.ScmObj* %argslist55632$_37foldr1470953)
store volatile %struct.ScmObj* %argslist55632$_37foldr1470954, %struct.ScmObj** %stackaddr$prim57221, align 8
%clofunc57222 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147095)
musttail call tailcc void %clofunc57222(%struct.ScmObj* %_37foldr147095, %struct.ScmObj* %argslist55632$_37foldr1470954)
ret void
}

define tailcc void @proc_clo$ae48628(%struct.ScmObj* %env$ae48628,%struct.ScmObj* %current_45args55633) {
%stackaddr$env-ref57223 = alloca %struct.ScmObj*, align 8
%f47128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48628, i64 0)
store %struct.ScmObj* %f47128, %struct.ScmObj** %stackaddr$env-ref57223
%stackaddr$prim57224 = alloca %struct.ScmObj*, align 8
%k47474 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55633)
store volatile %struct.ScmObj* %k47474, %struct.ScmObj** %stackaddr$prim57224, align 8
%stackaddr$prim57225 = alloca %struct.ScmObj*, align 8
%current_45args55634 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55633)
store volatile %struct.ScmObj* %current_45args55634, %struct.ScmObj** %stackaddr$prim57225, align 8
%stackaddr$prim57226 = alloca %struct.ScmObj*, align 8
%v47130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55634)
store volatile %struct.ScmObj* %v47130, %struct.ScmObj** %stackaddr$prim57226, align 8
%stackaddr$prim57227 = alloca %struct.ScmObj*, align 8
%current_45args55635 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55634)
store volatile %struct.ScmObj* %current_45args55635, %struct.ScmObj** %stackaddr$prim57227, align 8
%stackaddr$prim57228 = alloca %struct.ScmObj*, align 8
%r47129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55635)
store volatile %struct.ScmObj* %r47129, %struct.ScmObj** %stackaddr$prim57228, align 8
%stackaddr$makeclosure57229 = alloca %struct.ScmObj*, align 8
%fptrToInt57230 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48630 to i64
%ae48630 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57230)
store volatile %struct.ScmObj* %ae48630, %struct.ScmObj** %stackaddr$makeclosure57229, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48630, %struct.ScmObj* %k47474, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48630, %struct.ScmObj* %r47129, i64 1)
%argslist55641$f471280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57231 = alloca %struct.ScmObj*, align 8
%argslist55641$f471281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v47130, %struct.ScmObj* %argslist55641$f471280)
store volatile %struct.ScmObj* %argslist55641$f471281, %struct.ScmObj** %stackaddr$prim57231, align 8
%stackaddr$prim57232 = alloca %struct.ScmObj*, align 8
%argslist55641$f471282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48630, %struct.ScmObj* %argslist55641$f471281)
store volatile %struct.ScmObj* %argslist55641$f471282, %struct.ScmObj** %stackaddr$prim57232, align 8
%clofunc57233 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47128)
musttail call tailcc void %clofunc57233(%struct.ScmObj* %f47128, %struct.ScmObj* %argslist55641$f471282)
ret void
}

define tailcc void @proc_clo$ae48630(%struct.ScmObj* %env$ae48630,%struct.ScmObj* %current_45args55637) {
%stackaddr$env-ref57234 = alloca %struct.ScmObj*, align 8
%k47474 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48630, i64 0)
store %struct.ScmObj* %k47474, %struct.ScmObj** %stackaddr$env-ref57234
%stackaddr$env-ref57235 = alloca %struct.ScmObj*, align 8
%r47129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48630, i64 1)
store %struct.ScmObj* %r47129, %struct.ScmObj** %stackaddr$env-ref57235
%stackaddr$prim57236 = alloca %struct.ScmObj*, align 8
%_95k47475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55637)
store volatile %struct.ScmObj* %_95k47475, %struct.ScmObj** %stackaddr$prim57236, align 8
%stackaddr$prim57237 = alloca %struct.ScmObj*, align 8
%current_45args55638 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55637)
store volatile %struct.ScmObj* %current_45args55638, %struct.ScmObj** %stackaddr$prim57237, align 8
%stackaddr$prim57238 = alloca %struct.ScmObj*, align 8
%anf_45bind47255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55638)
store volatile %struct.ScmObj* %anf_45bind47255, %struct.ScmObj** %stackaddr$prim57238, align 8
%stackaddr$prim57239 = alloca %struct.ScmObj*, align 8
%cpsprim47476 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47255, %struct.ScmObj* %r47129)
store volatile %struct.ScmObj* %cpsprim47476, %struct.ScmObj** %stackaddr$prim57239, align 8
%ae48635 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55640$k474740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57240 = alloca %struct.ScmObj*, align 8
%argslist55640$k474741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47476, %struct.ScmObj* %argslist55640$k474740)
store volatile %struct.ScmObj* %argslist55640$k474741, %struct.ScmObj** %stackaddr$prim57240, align 8
%stackaddr$prim57241 = alloca %struct.ScmObj*, align 8
%argslist55640$k474742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48635, %struct.ScmObj* %argslist55640$k474741)
store volatile %struct.ScmObj* %argslist55640$k474742, %struct.ScmObj** %stackaddr$prim57241, align 8
%clofunc57242 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47474)
musttail call tailcc void %clofunc57242(%struct.ScmObj* %k47474, %struct.ScmObj* %argslist55640$k474742)
ret void
}

define tailcc void @proc_clo$ae48239(%struct.ScmObj* %env$ae48239,%struct.ScmObj* %current_45args55645) {
%stackaddr$env-ref57243 = alloca %struct.ScmObj*, align 8
%_37map147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48239, i64 0)
store %struct.ScmObj* %_37map147091, %struct.ScmObj** %stackaddr$env-ref57243
%stackaddr$env-ref57244 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48239, i64 1)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref57244
%stackaddr$prim57245 = alloca %struct.ScmObj*, align 8
%k47477 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55645)
store volatile %struct.ScmObj* %k47477, %struct.ScmObj** %stackaddr$prim57245, align 8
%stackaddr$prim57246 = alloca %struct.ScmObj*, align 8
%current_45args55646 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55645)
store volatile %struct.ScmObj* %current_45args55646, %struct.ScmObj** %stackaddr$prim57246, align 8
%stackaddr$prim57247 = alloca %struct.ScmObj*, align 8
%_37foldr47101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55646)
store volatile %struct.ScmObj* %_37foldr47101, %struct.ScmObj** %stackaddr$prim57247, align 8
%ae48241 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57248 = alloca %struct.ScmObj*, align 8
%fptrToInt57249 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48242 to i64
%ae48242 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57249)
store volatile %struct.ScmObj* %ae48242, %struct.ScmObj** %stackaddr$makeclosure57248, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48242, %struct.ScmObj* %_37map147091, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48242, %struct.ScmObj* %_37foldr47101, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48242, %struct.ScmObj* %_37foldr147095, i64 2)
%argslist55703$k474770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57250 = alloca %struct.ScmObj*, align 8
%argslist55703$k474771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48242, %struct.ScmObj* %argslist55703$k474770)
store volatile %struct.ScmObj* %argslist55703$k474771, %struct.ScmObj** %stackaddr$prim57250, align 8
%stackaddr$prim57251 = alloca %struct.ScmObj*, align 8
%argslist55703$k474772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48241, %struct.ScmObj* %argslist55703$k474771)
store volatile %struct.ScmObj* %argslist55703$k474772, %struct.ScmObj** %stackaddr$prim57251, align 8
%clofunc57252 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47477)
musttail call tailcc void %clofunc57252(%struct.ScmObj* %k47477, %struct.ScmObj* %argslist55703$k474772)
ret void
}

define tailcc void @proc_clo$ae48242(%struct.ScmObj* %env$ae48242,%struct.ScmObj* %args4710247478) {
%stackaddr$env-ref57253 = alloca %struct.ScmObj*, align 8
%_37map147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48242, i64 0)
store %struct.ScmObj* %_37map147091, %struct.ScmObj** %stackaddr$env-ref57253
%stackaddr$env-ref57254 = alloca %struct.ScmObj*, align 8
%_37foldr47101 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48242, i64 1)
store %struct.ScmObj* %_37foldr47101, %struct.ScmObj** %stackaddr$env-ref57254
%stackaddr$env-ref57255 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48242, i64 2)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref57255
%stackaddr$prim57256 = alloca %struct.ScmObj*, align 8
%k47479 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4710247478)
store volatile %struct.ScmObj* %k47479, %struct.ScmObj** %stackaddr$prim57256, align 8
%stackaddr$prim57257 = alloca %struct.ScmObj*, align 8
%args47102 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4710247478)
store volatile %struct.ScmObj* %args47102, %struct.ScmObj** %stackaddr$prim57257, align 8
%stackaddr$prim57258 = alloca %struct.ScmObj*, align 8
%f47105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47102)
store volatile %struct.ScmObj* %f47105, %struct.ScmObj** %stackaddr$prim57258, align 8
%stackaddr$prim57259 = alloca %struct.ScmObj*, align 8
%anf_45bind47242 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47102)
store volatile %struct.ScmObj* %anf_45bind47242, %struct.ScmObj** %stackaddr$prim57259, align 8
%stackaddr$prim57260 = alloca %struct.ScmObj*, align 8
%acc47104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47242)
store volatile %struct.ScmObj* %acc47104, %struct.ScmObj** %stackaddr$prim57260, align 8
%stackaddr$prim57261 = alloca %struct.ScmObj*, align 8
%anf_45bind47243 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47102)
store volatile %struct.ScmObj* %anf_45bind47243, %struct.ScmObj** %stackaddr$prim57261, align 8
%stackaddr$prim57262 = alloca %struct.ScmObj*, align 8
%lsts47103 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47243)
store volatile %struct.ScmObj* %lsts47103, %struct.ScmObj** %stackaddr$prim57262, align 8
%stackaddr$makeclosure57263 = alloca %struct.ScmObj*, align 8
%fptrToInt57264 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48250 to i64
%ae48250 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57264)
store volatile %struct.ScmObj* %ae48250, %struct.ScmObj** %stackaddr$makeclosure57263, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48250, %struct.ScmObj* %k47479, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48250, %struct.ScmObj* %_37foldr147095, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48250, %struct.ScmObj* %_37map147091, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48250, %struct.ScmObj* %f47105, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48250, %struct.ScmObj* %acc47104, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48250, %struct.ScmObj* %lsts47103, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48250, %struct.ScmObj* %_37foldr47101, i64 6)
%ae48251 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57265 = alloca %struct.ScmObj*, align 8
%fptrToInt57266 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48252 to i64
%ae48252 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57266)
store volatile %struct.ScmObj* %ae48252, %struct.ScmObj** %stackaddr$makeclosure57265, align 8
%argslist55702$ae482500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57267 = alloca %struct.ScmObj*, align 8
%argslist55702$ae482501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48252, %struct.ScmObj* %argslist55702$ae482500)
store volatile %struct.ScmObj* %argslist55702$ae482501, %struct.ScmObj** %stackaddr$prim57267, align 8
%stackaddr$prim57268 = alloca %struct.ScmObj*, align 8
%argslist55702$ae482502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48251, %struct.ScmObj* %argslist55702$ae482501)
store volatile %struct.ScmObj* %argslist55702$ae482502, %struct.ScmObj** %stackaddr$prim57268, align 8
%clofunc57269 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48250)
musttail call tailcc void %clofunc57269(%struct.ScmObj* %ae48250, %struct.ScmObj* %argslist55702$ae482502)
ret void
}

define tailcc void @proc_clo$ae48250(%struct.ScmObj* %env$ae48250,%struct.ScmObj* %current_45args55648) {
%stackaddr$env-ref57270 = alloca %struct.ScmObj*, align 8
%k47479 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48250, i64 0)
store %struct.ScmObj* %k47479, %struct.ScmObj** %stackaddr$env-ref57270
%stackaddr$env-ref57271 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48250, i64 1)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref57271
%stackaddr$env-ref57272 = alloca %struct.ScmObj*, align 8
%_37map147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48250, i64 2)
store %struct.ScmObj* %_37map147091, %struct.ScmObj** %stackaddr$env-ref57272
%stackaddr$env-ref57273 = alloca %struct.ScmObj*, align 8
%f47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48250, i64 3)
store %struct.ScmObj* %f47105, %struct.ScmObj** %stackaddr$env-ref57273
%stackaddr$env-ref57274 = alloca %struct.ScmObj*, align 8
%acc47104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48250, i64 4)
store %struct.ScmObj* %acc47104, %struct.ScmObj** %stackaddr$env-ref57274
%stackaddr$env-ref57275 = alloca %struct.ScmObj*, align 8
%lsts47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48250, i64 5)
store %struct.ScmObj* %lsts47103, %struct.ScmObj** %stackaddr$env-ref57275
%stackaddr$env-ref57276 = alloca %struct.ScmObj*, align 8
%_37foldr47101 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48250, i64 6)
store %struct.ScmObj* %_37foldr47101, %struct.ScmObj** %stackaddr$env-ref57276
%stackaddr$prim57277 = alloca %struct.ScmObj*, align 8
%_95k47480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55648)
store volatile %struct.ScmObj* %_95k47480, %struct.ScmObj** %stackaddr$prim57277, align 8
%stackaddr$prim57278 = alloca %struct.ScmObj*, align 8
%current_45args55649 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55648)
store volatile %struct.ScmObj* %current_45args55649, %struct.ScmObj** %stackaddr$prim57278, align 8
%stackaddr$prim57279 = alloca %struct.ScmObj*, align 8
%anf_45bind47244 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55649)
store volatile %struct.ScmObj* %anf_45bind47244, %struct.ScmObj** %stackaddr$prim57279, align 8
%stackaddr$makeclosure57280 = alloca %struct.ScmObj*, align 8
%fptrToInt57281 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48282 to i64
%ae48282 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57281)
store volatile %struct.ScmObj* %ae48282, %struct.ScmObj** %stackaddr$makeclosure57280, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48282, %struct.ScmObj* %k47479, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48282, %struct.ScmObj* %_37foldr147095, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48282, %struct.ScmObj* %_37map147091, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48282, %struct.ScmObj* %f47105, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48282, %struct.ScmObj* %acc47104, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48282, %struct.ScmObj* %lsts47103, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48282, %struct.ScmObj* %_37foldr47101, i64 6)
%ae48284 = call %struct.ScmObj* @const_init_false()
%argslist55695$_37foldr1470950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57282 = alloca %struct.ScmObj*, align 8
%argslist55695$_37foldr1470951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47103, %struct.ScmObj* %argslist55695$_37foldr1470950)
store volatile %struct.ScmObj* %argslist55695$_37foldr1470951, %struct.ScmObj** %stackaddr$prim57282, align 8
%stackaddr$prim57283 = alloca %struct.ScmObj*, align 8
%argslist55695$_37foldr1470952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48284, %struct.ScmObj* %argslist55695$_37foldr1470951)
store volatile %struct.ScmObj* %argslist55695$_37foldr1470952, %struct.ScmObj** %stackaddr$prim57283, align 8
%stackaddr$prim57284 = alloca %struct.ScmObj*, align 8
%argslist55695$_37foldr1470953 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47244, %struct.ScmObj* %argslist55695$_37foldr1470952)
store volatile %struct.ScmObj* %argslist55695$_37foldr1470953, %struct.ScmObj** %stackaddr$prim57284, align 8
%stackaddr$prim57285 = alloca %struct.ScmObj*, align 8
%argslist55695$_37foldr1470954 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48282, %struct.ScmObj* %argslist55695$_37foldr1470953)
store volatile %struct.ScmObj* %argslist55695$_37foldr1470954, %struct.ScmObj** %stackaddr$prim57285, align 8
%clofunc57286 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147095)
musttail call tailcc void %clofunc57286(%struct.ScmObj* %_37foldr147095, %struct.ScmObj* %argslist55695$_37foldr1470954)
ret void
}

define tailcc void @proc_clo$ae48282(%struct.ScmObj* %env$ae48282,%struct.ScmObj* %current_45args55651) {
%stackaddr$env-ref57287 = alloca %struct.ScmObj*, align 8
%k47479 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48282, i64 0)
store %struct.ScmObj* %k47479, %struct.ScmObj** %stackaddr$env-ref57287
%stackaddr$env-ref57288 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48282, i64 1)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref57288
%stackaddr$env-ref57289 = alloca %struct.ScmObj*, align 8
%_37map147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48282, i64 2)
store %struct.ScmObj* %_37map147091, %struct.ScmObj** %stackaddr$env-ref57289
%stackaddr$env-ref57290 = alloca %struct.ScmObj*, align 8
%f47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48282, i64 3)
store %struct.ScmObj* %f47105, %struct.ScmObj** %stackaddr$env-ref57290
%stackaddr$env-ref57291 = alloca %struct.ScmObj*, align 8
%acc47104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48282, i64 4)
store %struct.ScmObj* %acc47104, %struct.ScmObj** %stackaddr$env-ref57291
%stackaddr$env-ref57292 = alloca %struct.ScmObj*, align 8
%lsts47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48282, i64 5)
store %struct.ScmObj* %lsts47103, %struct.ScmObj** %stackaddr$env-ref57292
%stackaddr$env-ref57293 = alloca %struct.ScmObj*, align 8
%_37foldr47101 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48282, i64 6)
store %struct.ScmObj* %_37foldr47101, %struct.ScmObj** %stackaddr$env-ref57293
%stackaddr$prim57294 = alloca %struct.ScmObj*, align 8
%_95k47481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55651)
store volatile %struct.ScmObj* %_95k47481, %struct.ScmObj** %stackaddr$prim57294, align 8
%stackaddr$prim57295 = alloca %struct.ScmObj*, align 8
%current_45args55652 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55651)
store volatile %struct.ScmObj* %current_45args55652, %struct.ScmObj** %stackaddr$prim57295, align 8
%stackaddr$prim57296 = alloca %struct.ScmObj*, align 8
%anf_45bind47245 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55652)
store volatile %struct.ScmObj* %anf_45bind47245, %struct.ScmObj** %stackaddr$prim57296, align 8
%truthy$cmp57297 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47245)
%cmp$cmp57297 = icmp eq i64 %truthy$cmp57297, 1
br i1 %cmp$cmp57297, label %truebranch$cmp57297, label %falsebranch$cmp57297
truebranch$cmp57297:
%ae48293 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55654$k474790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57298 = alloca %struct.ScmObj*, align 8
%argslist55654$k474791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47104, %struct.ScmObj* %argslist55654$k474790)
store volatile %struct.ScmObj* %argslist55654$k474791, %struct.ScmObj** %stackaddr$prim57298, align 8
%stackaddr$prim57299 = alloca %struct.ScmObj*, align 8
%argslist55654$k474792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48293, %struct.ScmObj* %argslist55654$k474791)
store volatile %struct.ScmObj* %argslist55654$k474792, %struct.ScmObj** %stackaddr$prim57299, align 8
%clofunc57300 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47479)
musttail call tailcc void %clofunc57300(%struct.ScmObj* %k47479, %struct.ScmObj* %argslist55654$k474792)
ret void
falsebranch$cmp57297:
%stackaddr$makeclosure57301 = alloca %struct.ScmObj*, align 8
%fptrToInt57302 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48298 to i64
%ae48298 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57302)
store volatile %struct.ScmObj* %ae48298, %struct.ScmObj** %stackaddr$makeclosure57301, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48298, %struct.ScmObj* %k47479, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48298, %struct.ScmObj* %_37foldr147095, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48298, %struct.ScmObj* %_37map147091, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48298, %struct.ScmObj* %f47105, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48298, %struct.ScmObj* %acc47104, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48298, %struct.ScmObj* %lsts47103, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48298, %struct.ScmObj* %_37foldr47101, i64 6)
%ae48299 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57303 = alloca %struct.ScmObj*, align 8
%fptrToInt57304 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48300 to i64
%ae48300 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57304)
store volatile %struct.ScmObj* %ae48300, %struct.ScmObj** %stackaddr$makeclosure57303, align 8
%argslist55694$ae482980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57305 = alloca %struct.ScmObj*, align 8
%argslist55694$ae482981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48300, %struct.ScmObj* %argslist55694$ae482980)
store volatile %struct.ScmObj* %argslist55694$ae482981, %struct.ScmObj** %stackaddr$prim57305, align 8
%stackaddr$prim57306 = alloca %struct.ScmObj*, align 8
%argslist55694$ae482982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48299, %struct.ScmObj* %argslist55694$ae482981)
store volatile %struct.ScmObj* %argslist55694$ae482982, %struct.ScmObj** %stackaddr$prim57306, align 8
%clofunc57307 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48298)
musttail call tailcc void %clofunc57307(%struct.ScmObj* %ae48298, %struct.ScmObj* %argslist55694$ae482982)
ret void
}

define tailcc void @proc_clo$ae48298(%struct.ScmObj* %env$ae48298,%struct.ScmObj* %current_45args55655) {
%stackaddr$env-ref57308 = alloca %struct.ScmObj*, align 8
%k47479 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48298, i64 0)
store %struct.ScmObj* %k47479, %struct.ScmObj** %stackaddr$env-ref57308
%stackaddr$env-ref57309 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48298, i64 1)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref57309
%stackaddr$env-ref57310 = alloca %struct.ScmObj*, align 8
%_37map147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48298, i64 2)
store %struct.ScmObj* %_37map147091, %struct.ScmObj** %stackaddr$env-ref57310
%stackaddr$env-ref57311 = alloca %struct.ScmObj*, align 8
%f47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48298, i64 3)
store %struct.ScmObj* %f47105, %struct.ScmObj** %stackaddr$env-ref57311
%stackaddr$env-ref57312 = alloca %struct.ScmObj*, align 8
%acc47104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48298, i64 4)
store %struct.ScmObj* %acc47104, %struct.ScmObj** %stackaddr$env-ref57312
%stackaddr$env-ref57313 = alloca %struct.ScmObj*, align 8
%lsts47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48298, i64 5)
store %struct.ScmObj* %lsts47103, %struct.ScmObj** %stackaddr$env-ref57313
%stackaddr$env-ref57314 = alloca %struct.ScmObj*, align 8
%_37foldr47101 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48298, i64 6)
store %struct.ScmObj* %_37foldr47101, %struct.ScmObj** %stackaddr$env-ref57314
%stackaddr$prim57315 = alloca %struct.ScmObj*, align 8
%_95k47482 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55655)
store volatile %struct.ScmObj* %_95k47482, %struct.ScmObj** %stackaddr$prim57315, align 8
%stackaddr$prim57316 = alloca %struct.ScmObj*, align 8
%current_45args55656 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55655)
store volatile %struct.ScmObj* %current_45args55656, %struct.ScmObj** %stackaddr$prim57316, align 8
%stackaddr$prim57317 = alloca %struct.ScmObj*, align 8
%anf_45bind47246 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55656)
store volatile %struct.ScmObj* %anf_45bind47246, %struct.ScmObj** %stackaddr$prim57317, align 8
%stackaddr$makeclosure57318 = alloca %struct.ScmObj*, align 8
%fptrToInt57319 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48319 to i64
%ae48319 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57319)
store volatile %struct.ScmObj* %ae48319, %struct.ScmObj** %stackaddr$makeclosure57318, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48319, %struct.ScmObj* %k47479, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48319, %struct.ScmObj* %_37foldr147095, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48319, %struct.ScmObj* %_37map147091, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48319, %struct.ScmObj* %f47105, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48319, %struct.ScmObj* %acc47104, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48319, %struct.ScmObj* %lsts47103, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48319, %struct.ScmObj* %_37foldr47101, i64 6)
%argslist55689$_37map1470910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57320 = alloca %struct.ScmObj*, align 8
%argslist55689$_37map1470911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47103, %struct.ScmObj* %argslist55689$_37map1470910)
store volatile %struct.ScmObj* %argslist55689$_37map1470911, %struct.ScmObj** %stackaddr$prim57320, align 8
%stackaddr$prim57321 = alloca %struct.ScmObj*, align 8
%argslist55689$_37map1470912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47246, %struct.ScmObj* %argslist55689$_37map1470911)
store volatile %struct.ScmObj* %argslist55689$_37map1470912, %struct.ScmObj** %stackaddr$prim57321, align 8
%stackaddr$prim57322 = alloca %struct.ScmObj*, align 8
%argslist55689$_37map1470913 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48319, %struct.ScmObj* %argslist55689$_37map1470912)
store volatile %struct.ScmObj* %argslist55689$_37map1470913, %struct.ScmObj** %stackaddr$prim57322, align 8
%clofunc57323 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147091)
musttail call tailcc void %clofunc57323(%struct.ScmObj* %_37map147091, %struct.ScmObj* %argslist55689$_37map1470913)
ret void
}

define tailcc void @proc_clo$ae48319(%struct.ScmObj* %env$ae48319,%struct.ScmObj* %current_45args55658) {
%stackaddr$env-ref57324 = alloca %struct.ScmObj*, align 8
%k47479 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48319, i64 0)
store %struct.ScmObj* %k47479, %struct.ScmObj** %stackaddr$env-ref57324
%stackaddr$env-ref57325 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48319, i64 1)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref57325
%stackaddr$env-ref57326 = alloca %struct.ScmObj*, align 8
%_37map147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48319, i64 2)
store %struct.ScmObj* %_37map147091, %struct.ScmObj** %stackaddr$env-ref57326
%stackaddr$env-ref57327 = alloca %struct.ScmObj*, align 8
%f47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48319, i64 3)
store %struct.ScmObj* %f47105, %struct.ScmObj** %stackaddr$env-ref57327
%stackaddr$env-ref57328 = alloca %struct.ScmObj*, align 8
%acc47104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48319, i64 4)
store %struct.ScmObj* %acc47104, %struct.ScmObj** %stackaddr$env-ref57328
%stackaddr$env-ref57329 = alloca %struct.ScmObj*, align 8
%lsts47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48319, i64 5)
store %struct.ScmObj* %lsts47103, %struct.ScmObj** %stackaddr$env-ref57329
%stackaddr$env-ref57330 = alloca %struct.ScmObj*, align 8
%_37foldr47101 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48319, i64 6)
store %struct.ScmObj* %_37foldr47101, %struct.ScmObj** %stackaddr$env-ref57330
%stackaddr$prim57331 = alloca %struct.ScmObj*, align 8
%_95k47483 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55658)
store volatile %struct.ScmObj* %_95k47483, %struct.ScmObj** %stackaddr$prim57331, align 8
%stackaddr$prim57332 = alloca %struct.ScmObj*, align 8
%current_45args55659 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55658)
store volatile %struct.ScmObj* %current_45args55659, %struct.ScmObj** %stackaddr$prim57332, align 8
%stackaddr$prim57333 = alloca %struct.ScmObj*, align 8
%lsts_4347110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55659)
store volatile %struct.ScmObj* %lsts_4347110, %struct.ScmObj** %stackaddr$prim57333, align 8
%stackaddr$makeclosure57334 = alloca %struct.ScmObj*, align 8
%fptrToInt57335 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48322 to i64
%ae48322 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt57335)
store volatile %struct.ScmObj* %ae48322, %struct.ScmObj** %stackaddr$makeclosure57334, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48322, %struct.ScmObj* %k47479, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48322, %struct.ScmObj* %_37foldr147095, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48322, %struct.ScmObj* %lsts_4347110, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48322, %struct.ScmObj* %_37map147091, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48322, %struct.ScmObj* %f47105, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48322, %struct.ScmObj* %acc47104, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48322, %struct.ScmObj* %lsts47103, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48322, %struct.ScmObj* %_37foldr47101, i64 7)
%ae48323 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57336 = alloca %struct.ScmObj*, align 8
%fptrToInt57337 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48324 to i64
%ae48324 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57337)
store volatile %struct.ScmObj* %ae48324, %struct.ScmObj** %stackaddr$makeclosure57336, align 8
%argslist55688$ae483220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57338 = alloca %struct.ScmObj*, align 8
%argslist55688$ae483221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48324, %struct.ScmObj* %argslist55688$ae483220)
store volatile %struct.ScmObj* %argslist55688$ae483221, %struct.ScmObj** %stackaddr$prim57338, align 8
%stackaddr$prim57339 = alloca %struct.ScmObj*, align 8
%argslist55688$ae483222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48323, %struct.ScmObj* %argslist55688$ae483221)
store volatile %struct.ScmObj* %argslist55688$ae483222, %struct.ScmObj** %stackaddr$prim57339, align 8
%clofunc57340 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48322)
musttail call tailcc void %clofunc57340(%struct.ScmObj* %ae48322, %struct.ScmObj* %argslist55688$ae483222)
ret void
}

define tailcc void @proc_clo$ae48322(%struct.ScmObj* %env$ae48322,%struct.ScmObj* %current_45args55661) {
%stackaddr$env-ref57341 = alloca %struct.ScmObj*, align 8
%k47479 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48322, i64 0)
store %struct.ScmObj* %k47479, %struct.ScmObj** %stackaddr$env-ref57341
%stackaddr$env-ref57342 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48322, i64 1)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref57342
%stackaddr$env-ref57343 = alloca %struct.ScmObj*, align 8
%lsts_4347110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48322, i64 2)
store %struct.ScmObj* %lsts_4347110, %struct.ScmObj** %stackaddr$env-ref57343
%stackaddr$env-ref57344 = alloca %struct.ScmObj*, align 8
%_37map147091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48322, i64 3)
store %struct.ScmObj* %_37map147091, %struct.ScmObj** %stackaddr$env-ref57344
%stackaddr$env-ref57345 = alloca %struct.ScmObj*, align 8
%f47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48322, i64 4)
store %struct.ScmObj* %f47105, %struct.ScmObj** %stackaddr$env-ref57345
%stackaddr$env-ref57346 = alloca %struct.ScmObj*, align 8
%acc47104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48322, i64 5)
store %struct.ScmObj* %acc47104, %struct.ScmObj** %stackaddr$env-ref57346
%stackaddr$env-ref57347 = alloca %struct.ScmObj*, align 8
%lsts47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48322, i64 6)
store %struct.ScmObj* %lsts47103, %struct.ScmObj** %stackaddr$env-ref57347
%stackaddr$env-ref57348 = alloca %struct.ScmObj*, align 8
%_37foldr47101 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48322, i64 7)
store %struct.ScmObj* %_37foldr47101, %struct.ScmObj** %stackaddr$env-ref57348
%stackaddr$prim57349 = alloca %struct.ScmObj*, align 8
%_95k47484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55661)
store volatile %struct.ScmObj* %_95k47484, %struct.ScmObj** %stackaddr$prim57349, align 8
%stackaddr$prim57350 = alloca %struct.ScmObj*, align 8
%current_45args55662 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55661)
store volatile %struct.ScmObj* %current_45args55662, %struct.ScmObj** %stackaddr$prim57350, align 8
%stackaddr$prim57351 = alloca %struct.ScmObj*, align 8
%anf_45bind47247 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55662)
store volatile %struct.ScmObj* %anf_45bind47247, %struct.ScmObj** %stackaddr$prim57351, align 8
%stackaddr$makeclosure57352 = alloca %struct.ScmObj*, align 8
%fptrToInt57353 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48343 to i64
%ae48343 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt57353)
store volatile %struct.ScmObj* %ae48343, %struct.ScmObj** %stackaddr$makeclosure57352, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48343, %struct.ScmObj* %k47479, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48343, %struct.ScmObj* %_37foldr147095, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48343, %struct.ScmObj* %lsts_4347110, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48343, %struct.ScmObj* %f47105, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48343, %struct.ScmObj* %acc47104, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48343, %struct.ScmObj* %_37foldr47101, i64 5)
%argslist55683$_37map1470910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57354 = alloca %struct.ScmObj*, align 8
%argslist55683$_37map1470911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47103, %struct.ScmObj* %argslist55683$_37map1470910)
store volatile %struct.ScmObj* %argslist55683$_37map1470911, %struct.ScmObj** %stackaddr$prim57354, align 8
%stackaddr$prim57355 = alloca %struct.ScmObj*, align 8
%argslist55683$_37map1470912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47247, %struct.ScmObj* %argslist55683$_37map1470911)
store volatile %struct.ScmObj* %argslist55683$_37map1470912, %struct.ScmObj** %stackaddr$prim57355, align 8
%stackaddr$prim57356 = alloca %struct.ScmObj*, align 8
%argslist55683$_37map1470913 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48343, %struct.ScmObj* %argslist55683$_37map1470912)
store volatile %struct.ScmObj* %argslist55683$_37map1470913, %struct.ScmObj** %stackaddr$prim57356, align 8
%clofunc57357 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147091)
musttail call tailcc void %clofunc57357(%struct.ScmObj* %_37map147091, %struct.ScmObj* %argslist55683$_37map1470913)
ret void
}

define tailcc void @proc_clo$ae48343(%struct.ScmObj* %env$ae48343,%struct.ScmObj* %current_45args55664) {
%stackaddr$env-ref57358 = alloca %struct.ScmObj*, align 8
%k47479 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48343, i64 0)
store %struct.ScmObj* %k47479, %struct.ScmObj** %stackaddr$env-ref57358
%stackaddr$env-ref57359 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48343, i64 1)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref57359
%stackaddr$env-ref57360 = alloca %struct.ScmObj*, align 8
%lsts_4347110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48343, i64 2)
store %struct.ScmObj* %lsts_4347110, %struct.ScmObj** %stackaddr$env-ref57360
%stackaddr$env-ref57361 = alloca %struct.ScmObj*, align 8
%f47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48343, i64 3)
store %struct.ScmObj* %f47105, %struct.ScmObj** %stackaddr$env-ref57361
%stackaddr$env-ref57362 = alloca %struct.ScmObj*, align 8
%acc47104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48343, i64 4)
store %struct.ScmObj* %acc47104, %struct.ScmObj** %stackaddr$env-ref57362
%stackaddr$env-ref57363 = alloca %struct.ScmObj*, align 8
%_37foldr47101 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48343, i64 5)
store %struct.ScmObj* %_37foldr47101, %struct.ScmObj** %stackaddr$env-ref57363
%stackaddr$prim57364 = alloca %struct.ScmObj*, align 8
%_95k47485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55664)
store volatile %struct.ScmObj* %_95k47485, %struct.ScmObj** %stackaddr$prim57364, align 8
%stackaddr$prim57365 = alloca %struct.ScmObj*, align 8
%current_45args55665 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55664)
store volatile %struct.ScmObj* %current_45args55665, %struct.ScmObj** %stackaddr$prim57365, align 8
%stackaddr$prim57366 = alloca %struct.ScmObj*, align 8
%vs47108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55665)
store volatile %struct.ScmObj* %vs47108, %struct.ScmObj** %stackaddr$prim57366, align 8
%stackaddr$makeclosure57367 = alloca %struct.ScmObj*, align 8
%fptrToInt57368 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48346 to i64
%ae48346 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57368)
store volatile %struct.ScmObj* %ae48346, %struct.ScmObj** %stackaddr$makeclosure57367, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48346, %struct.ScmObj* %k47479, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48346, %struct.ScmObj* %_37foldr147095, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48346, %struct.ScmObj* %lsts_4347110, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48346, %struct.ScmObj* %vs47108, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48346, %struct.ScmObj* %f47105, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48346, %struct.ScmObj* %acc47104, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48346, %struct.ScmObj* %_37foldr47101, i64 6)
%ae48347 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57369 = alloca %struct.ScmObj*, align 8
%fptrToInt57370 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48348 to i64
%ae48348 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57370)
store volatile %struct.ScmObj* %ae48348, %struct.ScmObj** %stackaddr$makeclosure57369, align 8
%argslist55682$ae483460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57371 = alloca %struct.ScmObj*, align 8
%argslist55682$ae483461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48348, %struct.ScmObj* %argslist55682$ae483460)
store volatile %struct.ScmObj* %argslist55682$ae483461, %struct.ScmObj** %stackaddr$prim57371, align 8
%stackaddr$prim57372 = alloca %struct.ScmObj*, align 8
%argslist55682$ae483462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48347, %struct.ScmObj* %argslist55682$ae483461)
store volatile %struct.ScmObj* %argslist55682$ae483462, %struct.ScmObj** %stackaddr$prim57372, align 8
%clofunc57373 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48346)
musttail call tailcc void %clofunc57373(%struct.ScmObj* %ae48346, %struct.ScmObj* %argslist55682$ae483462)
ret void
}

define tailcc void @proc_clo$ae48346(%struct.ScmObj* %env$ae48346,%struct.ScmObj* %current_45args55667) {
%stackaddr$env-ref57374 = alloca %struct.ScmObj*, align 8
%k47479 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48346, i64 0)
store %struct.ScmObj* %k47479, %struct.ScmObj** %stackaddr$env-ref57374
%stackaddr$env-ref57375 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48346, i64 1)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref57375
%stackaddr$env-ref57376 = alloca %struct.ScmObj*, align 8
%lsts_4347110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48346, i64 2)
store %struct.ScmObj* %lsts_4347110, %struct.ScmObj** %stackaddr$env-ref57376
%stackaddr$env-ref57377 = alloca %struct.ScmObj*, align 8
%vs47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48346, i64 3)
store %struct.ScmObj* %vs47108, %struct.ScmObj** %stackaddr$env-ref57377
%stackaddr$env-ref57378 = alloca %struct.ScmObj*, align 8
%f47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48346, i64 4)
store %struct.ScmObj* %f47105, %struct.ScmObj** %stackaddr$env-ref57378
%stackaddr$env-ref57379 = alloca %struct.ScmObj*, align 8
%acc47104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48346, i64 5)
store %struct.ScmObj* %acc47104, %struct.ScmObj** %stackaddr$env-ref57379
%stackaddr$env-ref57380 = alloca %struct.ScmObj*, align 8
%_37foldr47101 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48346, i64 6)
store %struct.ScmObj* %_37foldr47101, %struct.ScmObj** %stackaddr$env-ref57380
%stackaddr$prim57381 = alloca %struct.ScmObj*, align 8
%_95k47486 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55667)
store volatile %struct.ScmObj* %_95k47486, %struct.ScmObj** %stackaddr$prim57381, align 8
%stackaddr$prim57382 = alloca %struct.ScmObj*, align 8
%current_45args55668 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55667)
store volatile %struct.ScmObj* %current_45args55668, %struct.ScmObj** %stackaddr$prim57382, align 8
%stackaddr$prim57383 = alloca %struct.ScmObj*, align 8
%anf_45bind47248 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55668)
store volatile %struct.ScmObj* %anf_45bind47248, %struct.ScmObj** %stackaddr$prim57383, align 8
%stackaddr$prim57384 = alloca %struct.ScmObj*, align 8
%anf_45bind47249 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47104, %struct.ScmObj* %lsts_4347110)
store volatile %struct.ScmObj* %anf_45bind47249, %struct.ScmObj** %stackaddr$prim57384, align 8
%stackaddr$prim57385 = alloca %struct.ScmObj*, align 8
%anf_45bind47250 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47105, %struct.ScmObj* %anf_45bind47249)
store volatile %struct.ScmObj* %anf_45bind47250, %struct.ScmObj** %stackaddr$prim57385, align 8
%stackaddr$makeclosure57386 = alloca %struct.ScmObj*, align 8
%fptrToInt57387 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48372 to i64
%ae48372 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57387)
store volatile %struct.ScmObj* %ae48372, %struct.ScmObj** %stackaddr$makeclosure57386, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48372, %struct.ScmObj* %k47479, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48372, %struct.ScmObj* %_37foldr147095, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48372, %struct.ScmObj* %vs47108, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48372, %struct.ScmObj* %f47105, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48372, %struct.ScmObj* %anf_45bind47248, i64 4)
%stackaddr$prim57388 = alloca %struct.ScmObj*, align 8
%cpsargs47490 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48372, %struct.ScmObj* %anf_45bind47250)
store volatile %struct.ScmObj* %cpsargs47490, %struct.ScmObj** %stackaddr$prim57388, align 8
%clofunc57389 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47101)
musttail call tailcc void %clofunc57389(%struct.ScmObj* %_37foldr47101, %struct.ScmObj* %cpsargs47490)
ret void
}

define tailcc void @proc_clo$ae48372(%struct.ScmObj* %env$ae48372,%struct.ScmObj* %current_45args55670) {
%stackaddr$env-ref57390 = alloca %struct.ScmObj*, align 8
%k47479 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48372, i64 0)
store %struct.ScmObj* %k47479, %struct.ScmObj** %stackaddr$env-ref57390
%stackaddr$env-ref57391 = alloca %struct.ScmObj*, align 8
%_37foldr147095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48372, i64 1)
store %struct.ScmObj* %_37foldr147095, %struct.ScmObj** %stackaddr$env-ref57391
%stackaddr$env-ref57392 = alloca %struct.ScmObj*, align 8
%vs47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48372, i64 2)
store %struct.ScmObj* %vs47108, %struct.ScmObj** %stackaddr$env-ref57392
%stackaddr$env-ref57393 = alloca %struct.ScmObj*, align 8
%f47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48372, i64 3)
store %struct.ScmObj* %f47105, %struct.ScmObj** %stackaddr$env-ref57393
%stackaddr$env-ref57394 = alloca %struct.ScmObj*, align 8
%anf_45bind47248 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48372, i64 4)
store %struct.ScmObj* %anf_45bind47248, %struct.ScmObj** %stackaddr$env-ref57394
%stackaddr$prim57395 = alloca %struct.ScmObj*, align 8
%_95k47487 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55670)
store volatile %struct.ScmObj* %_95k47487, %struct.ScmObj** %stackaddr$prim57395, align 8
%stackaddr$prim57396 = alloca %struct.ScmObj*, align 8
%current_45args55671 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55670)
store volatile %struct.ScmObj* %current_45args55671, %struct.ScmObj** %stackaddr$prim57396, align 8
%stackaddr$prim57397 = alloca %struct.ScmObj*, align 8
%anf_45bind47251 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55671)
store volatile %struct.ScmObj* %anf_45bind47251, %struct.ScmObj** %stackaddr$prim57397, align 8
%ae48377 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57398 = alloca %struct.ScmObj*, align 8
%anf_45bind47252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47251, %struct.ScmObj* %ae48377)
store volatile %struct.ScmObj* %anf_45bind47252, %struct.ScmObj** %stackaddr$prim57398, align 8
%stackaddr$makeclosure57399 = alloca %struct.ScmObj*, align 8
%fptrToInt57400 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48379 to i64
%ae48379 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57400)
store volatile %struct.ScmObj* %ae48379, %struct.ScmObj** %stackaddr$makeclosure57399, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48379, %struct.ScmObj* %f47105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48379, %struct.ScmObj* %k47479, i64 1)
%argslist55676$_37foldr1470950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57401 = alloca %struct.ScmObj*, align 8
%argslist55676$_37foldr1470951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47108, %struct.ScmObj* %argslist55676$_37foldr1470950)
store volatile %struct.ScmObj* %argslist55676$_37foldr1470951, %struct.ScmObj** %stackaddr$prim57401, align 8
%stackaddr$prim57402 = alloca %struct.ScmObj*, align 8
%argslist55676$_37foldr1470952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47252, %struct.ScmObj* %argslist55676$_37foldr1470951)
store volatile %struct.ScmObj* %argslist55676$_37foldr1470952, %struct.ScmObj** %stackaddr$prim57402, align 8
%stackaddr$prim57403 = alloca %struct.ScmObj*, align 8
%argslist55676$_37foldr1470953 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47248, %struct.ScmObj* %argslist55676$_37foldr1470952)
store volatile %struct.ScmObj* %argslist55676$_37foldr1470953, %struct.ScmObj** %stackaddr$prim57403, align 8
%stackaddr$prim57404 = alloca %struct.ScmObj*, align 8
%argslist55676$_37foldr1470954 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48379, %struct.ScmObj* %argslist55676$_37foldr1470953)
store volatile %struct.ScmObj* %argslist55676$_37foldr1470954, %struct.ScmObj** %stackaddr$prim57404, align 8
%clofunc57405 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147095)
musttail call tailcc void %clofunc57405(%struct.ScmObj* %_37foldr147095, %struct.ScmObj* %argslist55676$_37foldr1470954)
ret void
}

define tailcc void @proc_clo$ae48379(%struct.ScmObj* %env$ae48379,%struct.ScmObj* %current_45args55673) {
%stackaddr$env-ref57406 = alloca %struct.ScmObj*, align 8
%f47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48379, i64 0)
store %struct.ScmObj* %f47105, %struct.ScmObj** %stackaddr$env-ref57406
%stackaddr$env-ref57407 = alloca %struct.ScmObj*, align 8
%k47479 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48379, i64 1)
store %struct.ScmObj* %k47479, %struct.ScmObj** %stackaddr$env-ref57407
%stackaddr$prim57408 = alloca %struct.ScmObj*, align 8
%_95k47488 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55673)
store volatile %struct.ScmObj* %_95k47488, %struct.ScmObj** %stackaddr$prim57408, align 8
%stackaddr$prim57409 = alloca %struct.ScmObj*, align 8
%current_45args55674 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55673)
store volatile %struct.ScmObj* %current_45args55674, %struct.ScmObj** %stackaddr$prim57409, align 8
%stackaddr$prim57410 = alloca %struct.ScmObj*, align 8
%anf_45bind47253 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55674)
store volatile %struct.ScmObj* %anf_45bind47253, %struct.ScmObj** %stackaddr$prim57410, align 8
%stackaddr$prim57411 = alloca %struct.ScmObj*, align 8
%cpsargs47489 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47479, %struct.ScmObj* %anf_45bind47253)
store volatile %struct.ScmObj* %cpsargs47489, %struct.ScmObj** %stackaddr$prim57411, align 8
%clofunc57412 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47105)
musttail call tailcc void %clofunc57412(%struct.ScmObj* %f47105, %struct.ScmObj* %cpsargs47489)
ret void
}

define tailcc void @proc_clo$ae48348(%struct.ScmObj* %env$ae48348,%struct.ScmObj* %current_45args55677) {
%stackaddr$prim57413 = alloca %struct.ScmObj*, align 8
%k47491 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55677)
store volatile %struct.ScmObj* %k47491, %struct.ScmObj** %stackaddr$prim57413, align 8
%stackaddr$prim57414 = alloca %struct.ScmObj*, align 8
%current_45args55678 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55677)
store volatile %struct.ScmObj* %current_45args55678, %struct.ScmObj** %stackaddr$prim57414, align 8
%stackaddr$prim57415 = alloca %struct.ScmObj*, align 8
%a47113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55678)
store volatile %struct.ScmObj* %a47113, %struct.ScmObj** %stackaddr$prim57415, align 8
%stackaddr$prim57416 = alloca %struct.ScmObj*, align 8
%current_45args55679 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55678)
store volatile %struct.ScmObj* %current_45args55679, %struct.ScmObj** %stackaddr$prim57416, align 8
%stackaddr$prim57417 = alloca %struct.ScmObj*, align 8
%b47112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55679)
store volatile %struct.ScmObj* %b47112, %struct.ScmObj** %stackaddr$prim57417, align 8
%stackaddr$prim57418 = alloca %struct.ScmObj*, align 8
%cpsprim47492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47113, %struct.ScmObj* %b47112)
store volatile %struct.ScmObj* %cpsprim47492, %struct.ScmObj** %stackaddr$prim57418, align 8
%ae48352 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55681$k474910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57419 = alloca %struct.ScmObj*, align 8
%argslist55681$k474911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47492, %struct.ScmObj* %argslist55681$k474910)
store volatile %struct.ScmObj* %argslist55681$k474911, %struct.ScmObj** %stackaddr$prim57419, align 8
%stackaddr$prim57420 = alloca %struct.ScmObj*, align 8
%argslist55681$k474912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48352, %struct.ScmObj* %argslist55681$k474911)
store volatile %struct.ScmObj* %argslist55681$k474912, %struct.ScmObj** %stackaddr$prim57420, align 8
%clofunc57421 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47491)
musttail call tailcc void %clofunc57421(%struct.ScmObj* %k47491, %struct.ScmObj* %argslist55681$k474912)
ret void
}

define tailcc void @proc_clo$ae48324(%struct.ScmObj* %env$ae48324,%struct.ScmObj* %current_45args55684) {
%stackaddr$prim57422 = alloca %struct.ScmObj*, align 8
%k47493 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55684)
store volatile %struct.ScmObj* %k47493, %struct.ScmObj** %stackaddr$prim57422, align 8
%stackaddr$prim57423 = alloca %struct.ScmObj*, align 8
%current_45args55685 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55684)
store volatile %struct.ScmObj* %current_45args55685, %struct.ScmObj** %stackaddr$prim57423, align 8
%stackaddr$prim57424 = alloca %struct.ScmObj*, align 8
%x47109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55685)
store volatile %struct.ScmObj* %x47109, %struct.ScmObj** %stackaddr$prim57424, align 8
%stackaddr$prim57425 = alloca %struct.ScmObj*, align 8
%cpsprim47494 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47109)
store volatile %struct.ScmObj* %cpsprim47494, %struct.ScmObj** %stackaddr$prim57425, align 8
%ae48327 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55687$k474930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57426 = alloca %struct.ScmObj*, align 8
%argslist55687$k474931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47494, %struct.ScmObj* %argslist55687$k474930)
store volatile %struct.ScmObj* %argslist55687$k474931, %struct.ScmObj** %stackaddr$prim57426, align 8
%stackaddr$prim57427 = alloca %struct.ScmObj*, align 8
%argslist55687$k474932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48327, %struct.ScmObj* %argslist55687$k474931)
store volatile %struct.ScmObj* %argslist55687$k474932, %struct.ScmObj** %stackaddr$prim57427, align 8
%clofunc57428 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47493)
musttail call tailcc void %clofunc57428(%struct.ScmObj* %k47493, %struct.ScmObj* %argslist55687$k474932)
ret void
}

define tailcc void @proc_clo$ae48300(%struct.ScmObj* %env$ae48300,%struct.ScmObj* %current_45args55690) {
%stackaddr$prim57429 = alloca %struct.ScmObj*, align 8
%k47495 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55690)
store volatile %struct.ScmObj* %k47495, %struct.ScmObj** %stackaddr$prim57429, align 8
%stackaddr$prim57430 = alloca %struct.ScmObj*, align 8
%current_45args55691 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55690)
store volatile %struct.ScmObj* %current_45args55691, %struct.ScmObj** %stackaddr$prim57430, align 8
%stackaddr$prim57431 = alloca %struct.ScmObj*, align 8
%x47111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55691)
store volatile %struct.ScmObj* %x47111, %struct.ScmObj** %stackaddr$prim57431, align 8
%stackaddr$prim57432 = alloca %struct.ScmObj*, align 8
%cpsprim47496 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47111)
store volatile %struct.ScmObj* %cpsprim47496, %struct.ScmObj** %stackaddr$prim57432, align 8
%ae48303 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55693$k474950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57433 = alloca %struct.ScmObj*, align 8
%argslist55693$k474951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47496, %struct.ScmObj* %argslist55693$k474950)
store volatile %struct.ScmObj* %argslist55693$k474951, %struct.ScmObj** %stackaddr$prim57433, align 8
%stackaddr$prim57434 = alloca %struct.ScmObj*, align 8
%argslist55693$k474952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48303, %struct.ScmObj* %argslist55693$k474951)
store volatile %struct.ScmObj* %argslist55693$k474952, %struct.ScmObj** %stackaddr$prim57434, align 8
%clofunc57435 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47495)
musttail call tailcc void %clofunc57435(%struct.ScmObj* %k47495, %struct.ScmObj* %argslist55693$k474952)
ret void
}

define tailcc void @proc_clo$ae48252(%struct.ScmObj* %env$ae48252,%struct.ScmObj* %current_45args55696) {
%stackaddr$prim57436 = alloca %struct.ScmObj*, align 8
%k47497 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55696)
store volatile %struct.ScmObj* %k47497, %struct.ScmObj** %stackaddr$prim57436, align 8
%stackaddr$prim57437 = alloca %struct.ScmObj*, align 8
%current_45args55697 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55696)
store volatile %struct.ScmObj* %current_45args55697, %struct.ScmObj** %stackaddr$prim57437, align 8
%stackaddr$prim57438 = alloca %struct.ScmObj*, align 8
%lst47107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55697)
store volatile %struct.ScmObj* %lst47107, %struct.ScmObj** %stackaddr$prim57438, align 8
%stackaddr$prim57439 = alloca %struct.ScmObj*, align 8
%current_45args55698 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55697)
store volatile %struct.ScmObj* %current_45args55698, %struct.ScmObj** %stackaddr$prim57439, align 8
%stackaddr$prim57440 = alloca %struct.ScmObj*, align 8
%b47106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55698)
store volatile %struct.ScmObj* %b47106, %struct.ScmObj** %stackaddr$prim57440, align 8
%truthy$cmp57441 = call i64 @is_truthy_value(%struct.ScmObj* %b47106)
%cmp$cmp57441 = icmp eq i64 %truthy$cmp57441, 1
br i1 %cmp$cmp57441, label %truebranch$cmp57441, label %falsebranch$cmp57441
truebranch$cmp57441:
%ae48255 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55700$k474970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57442 = alloca %struct.ScmObj*, align 8
%argslist55700$k474971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47106, %struct.ScmObj* %argslist55700$k474970)
store volatile %struct.ScmObj* %argslist55700$k474971, %struct.ScmObj** %stackaddr$prim57442, align 8
%stackaddr$prim57443 = alloca %struct.ScmObj*, align 8
%argslist55700$k474972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48255, %struct.ScmObj* %argslist55700$k474971)
store volatile %struct.ScmObj* %argslist55700$k474972, %struct.ScmObj** %stackaddr$prim57443, align 8
%clofunc57444 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47497)
musttail call tailcc void %clofunc57444(%struct.ScmObj* %k47497, %struct.ScmObj* %argslist55700$k474972)
ret void
falsebranch$cmp57441:
%stackaddr$prim57445 = alloca %struct.ScmObj*, align 8
%cpsprim47498 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47107)
store volatile %struct.ScmObj* %cpsprim47498, %struct.ScmObj** %stackaddr$prim57445, align 8
%ae48262 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55701$k474970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57446 = alloca %struct.ScmObj*, align 8
%argslist55701$k474971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47498, %struct.ScmObj* %argslist55701$k474970)
store volatile %struct.ScmObj* %argslist55701$k474971, %struct.ScmObj** %stackaddr$prim57446, align 8
%stackaddr$prim57447 = alloca %struct.ScmObj*, align 8
%argslist55701$k474972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48262, %struct.ScmObj* %argslist55701$k474971)
store volatile %struct.ScmObj* %argslist55701$k474972, %struct.ScmObj** %stackaddr$prim57447, align 8
%clofunc57448 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47497)
musttail call tailcc void %clofunc57448(%struct.ScmObj* %k47497, %struct.ScmObj* %argslist55701$k474972)
ret void
}

define tailcc void @proc_clo$ae48209(%struct.ScmObj* %env$ae48209,%struct.ScmObj* %current_45args55705) {
%stackaddr$env-ref57449 = alloca %struct.ScmObj*, align 8
%_37take47087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48209, i64 0)
store %struct.ScmObj* %_37take47087, %struct.ScmObj** %stackaddr$env-ref57449
%stackaddr$env-ref57450 = alloca %struct.ScmObj*, align 8
%_37length47084 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48209, i64 1)
store %struct.ScmObj* %_37length47084, %struct.ScmObj** %stackaddr$env-ref57450
%stackaddr$prim57451 = alloca %struct.ScmObj*, align 8
%k47499 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55705)
store volatile %struct.ScmObj* %k47499, %struct.ScmObj** %stackaddr$prim57451, align 8
%stackaddr$prim57452 = alloca %struct.ScmObj*, align 8
%current_45args55706 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55705)
store volatile %struct.ScmObj* %current_45args55706, %struct.ScmObj** %stackaddr$prim57452, align 8
%stackaddr$prim57453 = alloca %struct.ScmObj*, align 8
%lst47116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55706)
store volatile %struct.ScmObj* %lst47116, %struct.ScmObj** %stackaddr$prim57453, align 8
%stackaddr$prim57454 = alloca %struct.ScmObj*, align 8
%current_45args55707 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55706)
store volatile %struct.ScmObj* %current_45args55707, %struct.ScmObj** %stackaddr$prim57454, align 8
%stackaddr$prim57455 = alloca %struct.ScmObj*, align 8
%n47115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55707)
store volatile %struct.ScmObj* %n47115, %struct.ScmObj** %stackaddr$prim57455, align 8
%stackaddr$makeclosure57456 = alloca %struct.ScmObj*, align 8
%fptrToInt57457 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48211 to i64
%ae48211 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57457)
store volatile %struct.ScmObj* %ae48211, %struct.ScmObj** %stackaddr$makeclosure57456, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48211, %struct.ScmObj* %n47115, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48211, %struct.ScmObj* %k47499, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48211, %struct.ScmObj* %_37take47087, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48211, %struct.ScmObj* %lst47116, i64 3)
%argslist55713$_37length470840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57458 = alloca %struct.ScmObj*, align 8
%argslist55713$_37length470841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47116, %struct.ScmObj* %argslist55713$_37length470840)
store volatile %struct.ScmObj* %argslist55713$_37length470841, %struct.ScmObj** %stackaddr$prim57458, align 8
%stackaddr$prim57459 = alloca %struct.ScmObj*, align 8
%argslist55713$_37length470842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48211, %struct.ScmObj* %argslist55713$_37length470841)
store volatile %struct.ScmObj* %argslist55713$_37length470842, %struct.ScmObj** %stackaddr$prim57459, align 8
%clofunc57460 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47084)
musttail call tailcc void %clofunc57460(%struct.ScmObj* %_37length47084, %struct.ScmObj* %argslist55713$_37length470842)
ret void
}

define tailcc void @proc_clo$ae48211(%struct.ScmObj* %env$ae48211,%struct.ScmObj* %current_45args55709) {
%stackaddr$env-ref57461 = alloca %struct.ScmObj*, align 8
%n47115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48211, i64 0)
store %struct.ScmObj* %n47115, %struct.ScmObj** %stackaddr$env-ref57461
%stackaddr$env-ref57462 = alloca %struct.ScmObj*, align 8
%k47499 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48211, i64 1)
store %struct.ScmObj* %k47499, %struct.ScmObj** %stackaddr$env-ref57462
%stackaddr$env-ref57463 = alloca %struct.ScmObj*, align 8
%_37take47087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48211, i64 2)
store %struct.ScmObj* %_37take47087, %struct.ScmObj** %stackaddr$env-ref57463
%stackaddr$env-ref57464 = alloca %struct.ScmObj*, align 8
%lst47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48211, i64 3)
store %struct.ScmObj* %lst47116, %struct.ScmObj** %stackaddr$env-ref57464
%stackaddr$prim57465 = alloca %struct.ScmObj*, align 8
%_95k47500 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55709)
store volatile %struct.ScmObj* %_95k47500, %struct.ScmObj** %stackaddr$prim57465, align 8
%stackaddr$prim57466 = alloca %struct.ScmObj*, align 8
%current_45args55710 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55709)
store volatile %struct.ScmObj* %current_45args55710, %struct.ScmObj** %stackaddr$prim57466, align 8
%stackaddr$prim57467 = alloca %struct.ScmObj*, align 8
%anf_45bind47240 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55710)
store volatile %struct.ScmObj* %anf_45bind47240, %struct.ScmObj** %stackaddr$prim57467, align 8
%stackaddr$prim57468 = alloca %struct.ScmObj*, align 8
%anf_45bind47241 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47240, %struct.ScmObj* %n47115)
store volatile %struct.ScmObj* %anf_45bind47241, %struct.ScmObj** %stackaddr$prim57468, align 8
%argslist55712$_37take470870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57469 = alloca %struct.ScmObj*, align 8
%argslist55712$_37take470871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47241, %struct.ScmObj* %argslist55712$_37take470870)
store volatile %struct.ScmObj* %argslist55712$_37take470871, %struct.ScmObj** %stackaddr$prim57469, align 8
%stackaddr$prim57470 = alloca %struct.ScmObj*, align 8
%argslist55712$_37take470872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47116, %struct.ScmObj* %argslist55712$_37take470871)
store volatile %struct.ScmObj* %argslist55712$_37take470872, %struct.ScmObj** %stackaddr$prim57470, align 8
%stackaddr$prim57471 = alloca %struct.ScmObj*, align 8
%argslist55712$_37take470873 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47499, %struct.ScmObj* %argslist55712$_37take470872)
store volatile %struct.ScmObj* %argslist55712$_37take470873, %struct.ScmObj** %stackaddr$prim57471, align 8
%clofunc57472 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47087)
musttail call tailcc void %clofunc57472(%struct.ScmObj* %_37take47087, %struct.ScmObj* %argslist55712$_37take470873)
ret void
}

define tailcc void @proc_clo$ae48155(%struct.ScmObj* %env$ae48155,%struct.ScmObj* %current_45args55715) {
%stackaddr$env-ref57473 = alloca %struct.ScmObj*, align 8
%_37foldl147079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48155, i64 0)
store %struct.ScmObj* %_37foldl147079, %struct.ScmObj** %stackaddr$env-ref57473
%stackaddr$prim57474 = alloca %struct.ScmObj*, align 8
%k47501 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55715)
store volatile %struct.ScmObj* %k47501, %struct.ScmObj** %stackaddr$prim57474, align 8
%stackaddr$prim57475 = alloca %struct.ScmObj*, align 8
%current_45args55716 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55715)
store volatile %struct.ScmObj* %current_45args55716, %struct.ScmObj** %stackaddr$prim57475, align 8
%stackaddr$prim57476 = alloca %struct.ScmObj*, align 8
%lst47118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55716)
store volatile %struct.ScmObj* %lst47118, %struct.ScmObj** %stackaddr$prim57476, align 8
%stackaddr$makeclosure57477 = alloca %struct.ScmObj*, align 8
%fptrToInt57478 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48156 to i64
%ae48156 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57478)
store volatile %struct.ScmObj* %ae48156, %struct.ScmObj** %stackaddr$makeclosure57477, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48156, %struct.ScmObj* %lst47118, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48156, %struct.ScmObj* %k47501, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48156, %struct.ScmObj* %_37foldl147079, i64 2)
%ae48157 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57479 = alloca %struct.ScmObj*, align 8
%fptrToInt57480 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48158 to i64
%ae48158 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57480)
store volatile %struct.ScmObj* %ae48158, %struct.ScmObj** %stackaddr$makeclosure57479, align 8
%argslist55727$ae481560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57481 = alloca %struct.ScmObj*, align 8
%argslist55727$ae481561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48158, %struct.ScmObj* %argslist55727$ae481560)
store volatile %struct.ScmObj* %argslist55727$ae481561, %struct.ScmObj** %stackaddr$prim57481, align 8
%stackaddr$prim57482 = alloca %struct.ScmObj*, align 8
%argslist55727$ae481562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48157, %struct.ScmObj* %argslist55727$ae481561)
store volatile %struct.ScmObj* %argslist55727$ae481562, %struct.ScmObj** %stackaddr$prim57482, align 8
%clofunc57483 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48156)
musttail call tailcc void %clofunc57483(%struct.ScmObj* %ae48156, %struct.ScmObj* %argslist55727$ae481562)
ret void
}

define tailcc void @proc_clo$ae48156(%struct.ScmObj* %env$ae48156,%struct.ScmObj* %current_45args55718) {
%stackaddr$env-ref57484 = alloca %struct.ScmObj*, align 8
%lst47118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48156, i64 0)
store %struct.ScmObj* %lst47118, %struct.ScmObj** %stackaddr$env-ref57484
%stackaddr$env-ref57485 = alloca %struct.ScmObj*, align 8
%k47501 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48156, i64 1)
store %struct.ScmObj* %k47501, %struct.ScmObj** %stackaddr$env-ref57485
%stackaddr$env-ref57486 = alloca %struct.ScmObj*, align 8
%_37foldl147079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48156, i64 2)
store %struct.ScmObj* %_37foldl147079, %struct.ScmObj** %stackaddr$env-ref57486
%stackaddr$prim57487 = alloca %struct.ScmObj*, align 8
%_95k47502 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55718)
store volatile %struct.ScmObj* %_95k47502, %struct.ScmObj** %stackaddr$prim57487, align 8
%stackaddr$prim57488 = alloca %struct.ScmObj*, align 8
%current_45args55719 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55718)
store volatile %struct.ScmObj* %current_45args55719, %struct.ScmObj** %stackaddr$prim57488, align 8
%stackaddr$prim57489 = alloca %struct.ScmObj*, align 8
%anf_45bind47239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55719)
store volatile %struct.ScmObj* %anf_45bind47239, %struct.ScmObj** %stackaddr$prim57489, align 8
%ae48177 = call %struct.ScmObj* @const_init_null()
%argslist55721$_37foldl1470790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57490 = alloca %struct.ScmObj*, align 8
%argslist55721$_37foldl1470791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47118, %struct.ScmObj* %argslist55721$_37foldl1470790)
store volatile %struct.ScmObj* %argslist55721$_37foldl1470791, %struct.ScmObj** %stackaddr$prim57490, align 8
%stackaddr$prim57491 = alloca %struct.ScmObj*, align 8
%argslist55721$_37foldl1470792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48177, %struct.ScmObj* %argslist55721$_37foldl1470791)
store volatile %struct.ScmObj* %argslist55721$_37foldl1470792, %struct.ScmObj** %stackaddr$prim57491, align 8
%stackaddr$prim57492 = alloca %struct.ScmObj*, align 8
%argslist55721$_37foldl1470793 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47239, %struct.ScmObj* %argslist55721$_37foldl1470792)
store volatile %struct.ScmObj* %argslist55721$_37foldl1470793, %struct.ScmObj** %stackaddr$prim57492, align 8
%stackaddr$prim57493 = alloca %struct.ScmObj*, align 8
%argslist55721$_37foldl1470794 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47501, %struct.ScmObj* %argslist55721$_37foldl1470793)
store volatile %struct.ScmObj* %argslist55721$_37foldl1470794, %struct.ScmObj** %stackaddr$prim57493, align 8
%clofunc57494 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147079)
musttail call tailcc void %clofunc57494(%struct.ScmObj* %_37foldl147079, %struct.ScmObj* %argslist55721$_37foldl1470794)
ret void
}

define tailcc void @proc_clo$ae48158(%struct.ScmObj* %env$ae48158,%struct.ScmObj* %current_45args55722) {
%stackaddr$prim57495 = alloca %struct.ScmObj*, align 8
%k47503 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55722)
store volatile %struct.ScmObj* %k47503, %struct.ScmObj** %stackaddr$prim57495, align 8
%stackaddr$prim57496 = alloca %struct.ScmObj*, align 8
%current_45args55723 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55722)
store volatile %struct.ScmObj* %current_45args55723, %struct.ScmObj** %stackaddr$prim57496, align 8
%stackaddr$prim57497 = alloca %struct.ScmObj*, align 8
%x47120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55723)
store volatile %struct.ScmObj* %x47120, %struct.ScmObj** %stackaddr$prim57497, align 8
%stackaddr$prim57498 = alloca %struct.ScmObj*, align 8
%current_45args55724 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55723)
store volatile %struct.ScmObj* %current_45args55724, %struct.ScmObj** %stackaddr$prim57498, align 8
%stackaddr$prim57499 = alloca %struct.ScmObj*, align 8
%y47119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55724)
store volatile %struct.ScmObj* %y47119, %struct.ScmObj** %stackaddr$prim57499, align 8
%ae48160 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55726$k475030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57500 = alloca %struct.ScmObj*, align 8
%argslist55726$k475031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x47120, %struct.ScmObj* %argslist55726$k475030)
store volatile %struct.ScmObj* %argslist55726$k475031, %struct.ScmObj** %stackaddr$prim57500, align 8
%stackaddr$prim57501 = alloca %struct.ScmObj*, align 8
%argslist55726$k475032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48160, %struct.ScmObj* %argslist55726$k475031)
store volatile %struct.ScmObj* %argslist55726$k475032, %struct.ScmObj** %stackaddr$prim57501, align 8
%clofunc57502 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47503)
musttail call tailcc void %clofunc57502(%struct.ScmObj* %k47503, %struct.ScmObj* %argslist55726$k475032)
ret void
}

define tailcc void @proc_clo$ae48076(%struct.ScmObj* %env$ae48076,%struct.ScmObj* %current_45args55730) {
%stackaddr$prim57503 = alloca %struct.ScmObj*, align 8
%k47504 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55730)
store volatile %struct.ScmObj* %k47504, %struct.ScmObj** %stackaddr$prim57503, align 8
%stackaddr$prim57504 = alloca %struct.ScmObj*, align 8
%current_45args55731 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55730)
store volatile %struct.ScmObj* %current_45args55731, %struct.ScmObj** %stackaddr$prim57504, align 8
%stackaddr$prim57505 = alloca %struct.ScmObj*, align 8
%_37foldl147080 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55731)
store volatile %struct.ScmObj* %_37foldl147080, %struct.ScmObj** %stackaddr$prim57505, align 8
%ae48078 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57506 = alloca %struct.ScmObj*, align 8
%fptrToInt57507 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48079 to i64
%ae48079 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57507)
store volatile %struct.ScmObj* %ae48079, %struct.ScmObj** %stackaddr$makeclosure57506, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48079, %struct.ScmObj* %_37foldl147080, i64 0)
%argslist55744$k475040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57508 = alloca %struct.ScmObj*, align 8
%argslist55744$k475041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48079, %struct.ScmObj* %argslist55744$k475040)
store volatile %struct.ScmObj* %argslist55744$k475041, %struct.ScmObj** %stackaddr$prim57508, align 8
%stackaddr$prim57509 = alloca %struct.ScmObj*, align 8
%argslist55744$k475042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48078, %struct.ScmObj* %argslist55744$k475041)
store volatile %struct.ScmObj* %argslist55744$k475042, %struct.ScmObj** %stackaddr$prim57509, align 8
%clofunc57510 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47504)
musttail call tailcc void %clofunc57510(%struct.ScmObj* %k47504, %struct.ScmObj* %argslist55744$k475042)
ret void
}

define tailcc void @proc_clo$ae48079(%struct.ScmObj* %env$ae48079,%struct.ScmObj* %current_45args55733) {
%stackaddr$env-ref57511 = alloca %struct.ScmObj*, align 8
%_37foldl147080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48079, i64 0)
store %struct.ScmObj* %_37foldl147080, %struct.ScmObj** %stackaddr$env-ref57511
%stackaddr$prim57512 = alloca %struct.ScmObj*, align 8
%k47505 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55733)
store volatile %struct.ScmObj* %k47505, %struct.ScmObj** %stackaddr$prim57512, align 8
%stackaddr$prim57513 = alloca %struct.ScmObj*, align 8
%current_45args55734 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55733)
store volatile %struct.ScmObj* %current_45args55734, %struct.ScmObj** %stackaddr$prim57513, align 8
%stackaddr$prim57514 = alloca %struct.ScmObj*, align 8
%f47083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55734)
store volatile %struct.ScmObj* %f47083, %struct.ScmObj** %stackaddr$prim57514, align 8
%stackaddr$prim57515 = alloca %struct.ScmObj*, align 8
%current_45args55735 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55734)
store volatile %struct.ScmObj* %current_45args55735, %struct.ScmObj** %stackaddr$prim57515, align 8
%stackaddr$prim57516 = alloca %struct.ScmObj*, align 8
%acc47082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55735)
store volatile %struct.ScmObj* %acc47082, %struct.ScmObj** %stackaddr$prim57516, align 8
%stackaddr$prim57517 = alloca %struct.ScmObj*, align 8
%current_45args55736 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55735)
store volatile %struct.ScmObj* %current_45args55736, %struct.ScmObj** %stackaddr$prim57517, align 8
%stackaddr$prim57518 = alloca %struct.ScmObj*, align 8
%lst47081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55736)
store volatile %struct.ScmObj* %lst47081, %struct.ScmObj** %stackaddr$prim57518, align 8
%stackaddr$prim57519 = alloca %struct.ScmObj*, align 8
%anf_45bind47234 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47081)
store volatile %struct.ScmObj* %anf_45bind47234, %struct.ScmObj** %stackaddr$prim57519, align 8
%truthy$cmp57520 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47234)
%cmp$cmp57520 = icmp eq i64 %truthy$cmp57520, 1
br i1 %cmp$cmp57520, label %truebranch$cmp57520, label %falsebranch$cmp57520
truebranch$cmp57520:
%ae48083 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55738$k475050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57521 = alloca %struct.ScmObj*, align 8
%argslist55738$k475051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47082, %struct.ScmObj* %argslist55738$k475050)
store volatile %struct.ScmObj* %argslist55738$k475051, %struct.ScmObj** %stackaddr$prim57521, align 8
%stackaddr$prim57522 = alloca %struct.ScmObj*, align 8
%argslist55738$k475052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48083, %struct.ScmObj* %argslist55738$k475051)
store volatile %struct.ScmObj* %argslist55738$k475052, %struct.ScmObj** %stackaddr$prim57522, align 8
%clofunc57523 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47505)
musttail call tailcc void %clofunc57523(%struct.ScmObj* %k47505, %struct.ScmObj* %argslist55738$k475052)
ret void
falsebranch$cmp57520:
%stackaddr$prim57524 = alloca %struct.ScmObj*, align 8
%anf_45bind47235 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47081)
store volatile %struct.ScmObj* %anf_45bind47235, %struct.ScmObj** %stackaddr$prim57524, align 8
%stackaddr$makeclosure57525 = alloca %struct.ScmObj*, align 8
%fptrToInt57526 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48090 to i64
%ae48090 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57526)
store volatile %struct.ScmObj* %ae48090, %struct.ScmObj** %stackaddr$makeclosure57525, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48090, %struct.ScmObj* %k47505, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48090, %struct.ScmObj* %f47083, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48090, %struct.ScmObj* %lst47081, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48090, %struct.ScmObj* %_37foldl147080, i64 3)
%argslist55743$f470830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57527 = alloca %struct.ScmObj*, align 8
%argslist55743$f470831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47082, %struct.ScmObj* %argslist55743$f470830)
store volatile %struct.ScmObj* %argslist55743$f470831, %struct.ScmObj** %stackaddr$prim57527, align 8
%stackaddr$prim57528 = alloca %struct.ScmObj*, align 8
%argslist55743$f470832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47235, %struct.ScmObj* %argslist55743$f470831)
store volatile %struct.ScmObj* %argslist55743$f470832, %struct.ScmObj** %stackaddr$prim57528, align 8
%stackaddr$prim57529 = alloca %struct.ScmObj*, align 8
%argslist55743$f470833 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48090, %struct.ScmObj* %argslist55743$f470832)
store volatile %struct.ScmObj* %argslist55743$f470833, %struct.ScmObj** %stackaddr$prim57529, align 8
%clofunc57530 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47083)
musttail call tailcc void %clofunc57530(%struct.ScmObj* %f47083, %struct.ScmObj* %argslist55743$f470833)
ret void
}

define tailcc void @proc_clo$ae48090(%struct.ScmObj* %env$ae48090,%struct.ScmObj* %current_45args55739) {
%stackaddr$env-ref57531 = alloca %struct.ScmObj*, align 8
%k47505 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48090, i64 0)
store %struct.ScmObj* %k47505, %struct.ScmObj** %stackaddr$env-ref57531
%stackaddr$env-ref57532 = alloca %struct.ScmObj*, align 8
%f47083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48090, i64 1)
store %struct.ScmObj* %f47083, %struct.ScmObj** %stackaddr$env-ref57532
%stackaddr$env-ref57533 = alloca %struct.ScmObj*, align 8
%lst47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48090, i64 2)
store %struct.ScmObj* %lst47081, %struct.ScmObj** %stackaddr$env-ref57533
%stackaddr$env-ref57534 = alloca %struct.ScmObj*, align 8
%_37foldl147080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48090, i64 3)
store %struct.ScmObj* %_37foldl147080, %struct.ScmObj** %stackaddr$env-ref57534
%stackaddr$prim57535 = alloca %struct.ScmObj*, align 8
%_95k47506 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55739)
store volatile %struct.ScmObj* %_95k47506, %struct.ScmObj** %stackaddr$prim57535, align 8
%stackaddr$prim57536 = alloca %struct.ScmObj*, align 8
%current_45args55740 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55739)
store volatile %struct.ScmObj* %current_45args55740, %struct.ScmObj** %stackaddr$prim57536, align 8
%stackaddr$prim57537 = alloca %struct.ScmObj*, align 8
%anf_45bind47236 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55740)
store volatile %struct.ScmObj* %anf_45bind47236, %struct.ScmObj** %stackaddr$prim57537, align 8
%stackaddr$prim57538 = alloca %struct.ScmObj*, align 8
%anf_45bind47237 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47081)
store volatile %struct.ScmObj* %anf_45bind47237, %struct.ScmObj** %stackaddr$prim57538, align 8
%argslist55742$_37foldl1470800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57539 = alloca %struct.ScmObj*, align 8
%argslist55742$_37foldl1470801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47237, %struct.ScmObj* %argslist55742$_37foldl1470800)
store volatile %struct.ScmObj* %argslist55742$_37foldl1470801, %struct.ScmObj** %stackaddr$prim57539, align 8
%stackaddr$prim57540 = alloca %struct.ScmObj*, align 8
%argslist55742$_37foldl1470802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47236, %struct.ScmObj* %argslist55742$_37foldl1470801)
store volatile %struct.ScmObj* %argslist55742$_37foldl1470802, %struct.ScmObj** %stackaddr$prim57540, align 8
%stackaddr$prim57541 = alloca %struct.ScmObj*, align 8
%argslist55742$_37foldl1470803 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47083, %struct.ScmObj* %argslist55742$_37foldl1470802)
store volatile %struct.ScmObj* %argslist55742$_37foldl1470803, %struct.ScmObj** %stackaddr$prim57541, align 8
%stackaddr$prim57542 = alloca %struct.ScmObj*, align 8
%argslist55742$_37foldl1470804 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47505, %struct.ScmObj* %argslist55742$_37foldl1470803)
store volatile %struct.ScmObj* %argslist55742$_37foldl1470804, %struct.ScmObj** %stackaddr$prim57542, align 8
%clofunc57543 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147080)
musttail call tailcc void %clofunc57543(%struct.ScmObj* %_37foldl147080, %struct.ScmObj* %argslist55742$_37foldl1470804)
ret void
}

define tailcc void @proc_clo$ae47993(%struct.ScmObj* %env$ae47993,%struct.ScmObj* %current_45args55747) {
%stackaddr$prim57544 = alloca %struct.ScmObj*, align 8
%k47507 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55747)
store volatile %struct.ScmObj* %k47507, %struct.ScmObj** %stackaddr$prim57544, align 8
%stackaddr$prim57545 = alloca %struct.ScmObj*, align 8
%current_45args55748 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55747)
store volatile %struct.ScmObj* %current_45args55748, %struct.ScmObj** %stackaddr$prim57545, align 8
%stackaddr$prim57546 = alloca %struct.ScmObj*, align 8
%_37length47085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55748)
store volatile %struct.ScmObj* %_37length47085, %struct.ScmObj** %stackaddr$prim57546, align 8
%ae47995 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57547 = alloca %struct.ScmObj*, align 8
%fptrToInt57548 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47996 to i64
%ae47996 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57548)
store volatile %struct.ScmObj* %ae47996, %struct.ScmObj** %stackaddr$makeclosure57547, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47996, %struct.ScmObj* %_37length47085, i64 0)
%argslist55759$k475070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57549 = alloca %struct.ScmObj*, align 8
%argslist55759$k475071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47996, %struct.ScmObj* %argslist55759$k475070)
store volatile %struct.ScmObj* %argslist55759$k475071, %struct.ScmObj** %stackaddr$prim57549, align 8
%stackaddr$prim57550 = alloca %struct.ScmObj*, align 8
%argslist55759$k475072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47995, %struct.ScmObj* %argslist55759$k475071)
store volatile %struct.ScmObj* %argslist55759$k475072, %struct.ScmObj** %stackaddr$prim57550, align 8
%clofunc57551 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47507)
musttail call tailcc void %clofunc57551(%struct.ScmObj* %k47507, %struct.ScmObj* %argslist55759$k475072)
ret void
}

define tailcc void @proc_clo$ae47996(%struct.ScmObj* %env$ae47996,%struct.ScmObj* %current_45args55750) {
%stackaddr$env-ref57552 = alloca %struct.ScmObj*, align 8
%_37length47085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47996, i64 0)
store %struct.ScmObj* %_37length47085, %struct.ScmObj** %stackaddr$env-ref57552
%stackaddr$prim57553 = alloca %struct.ScmObj*, align 8
%k47508 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55750)
store volatile %struct.ScmObj* %k47508, %struct.ScmObj** %stackaddr$prim57553, align 8
%stackaddr$prim57554 = alloca %struct.ScmObj*, align 8
%current_45args55751 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55750)
store volatile %struct.ScmObj* %current_45args55751, %struct.ScmObj** %stackaddr$prim57554, align 8
%stackaddr$prim57555 = alloca %struct.ScmObj*, align 8
%lst47086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55751)
store volatile %struct.ScmObj* %lst47086, %struct.ScmObj** %stackaddr$prim57555, align 8
%stackaddr$prim57556 = alloca %struct.ScmObj*, align 8
%anf_45bind47230 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47086)
store volatile %struct.ScmObj* %anf_45bind47230, %struct.ScmObj** %stackaddr$prim57556, align 8
%truthy$cmp57557 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47230)
%cmp$cmp57557 = icmp eq i64 %truthy$cmp57557, 1
br i1 %cmp$cmp57557, label %truebranch$cmp57557, label %falsebranch$cmp57557
truebranch$cmp57557:
%ae48000 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48001 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55753$k475080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57558 = alloca %struct.ScmObj*, align 8
%argslist55753$k475081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48001, %struct.ScmObj* %argslist55753$k475080)
store volatile %struct.ScmObj* %argslist55753$k475081, %struct.ScmObj** %stackaddr$prim57558, align 8
%stackaddr$prim57559 = alloca %struct.ScmObj*, align 8
%argslist55753$k475082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48000, %struct.ScmObj* %argslist55753$k475081)
store volatile %struct.ScmObj* %argslist55753$k475082, %struct.ScmObj** %stackaddr$prim57559, align 8
%clofunc57560 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47508)
musttail call tailcc void %clofunc57560(%struct.ScmObj* %k47508, %struct.ScmObj* %argslist55753$k475082)
ret void
falsebranch$cmp57557:
%stackaddr$prim57561 = alloca %struct.ScmObj*, align 8
%anf_45bind47231 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47086)
store volatile %struct.ScmObj* %anf_45bind47231, %struct.ScmObj** %stackaddr$prim57561, align 8
%stackaddr$makeclosure57562 = alloca %struct.ScmObj*, align 8
%fptrToInt57563 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48010 to i64
%ae48010 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57563)
store volatile %struct.ScmObj* %ae48010, %struct.ScmObj** %stackaddr$makeclosure57562, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48010, %struct.ScmObj* %k47508, i64 0)
%argslist55758$_37length470850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57564 = alloca %struct.ScmObj*, align 8
%argslist55758$_37length470851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47231, %struct.ScmObj* %argslist55758$_37length470850)
store volatile %struct.ScmObj* %argslist55758$_37length470851, %struct.ScmObj** %stackaddr$prim57564, align 8
%stackaddr$prim57565 = alloca %struct.ScmObj*, align 8
%argslist55758$_37length470852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48010, %struct.ScmObj* %argslist55758$_37length470851)
store volatile %struct.ScmObj* %argslist55758$_37length470852, %struct.ScmObj** %stackaddr$prim57565, align 8
%clofunc57566 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47085)
musttail call tailcc void %clofunc57566(%struct.ScmObj* %_37length47085, %struct.ScmObj* %argslist55758$_37length470852)
ret void
}

define tailcc void @proc_clo$ae48010(%struct.ScmObj* %env$ae48010,%struct.ScmObj* %current_45args55754) {
%stackaddr$env-ref57567 = alloca %struct.ScmObj*, align 8
%k47508 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48010, i64 0)
store %struct.ScmObj* %k47508, %struct.ScmObj** %stackaddr$env-ref57567
%stackaddr$prim57568 = alloca %struct.ScmObj*, align 8
%_95k47509 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55754)
store volatile %struct.ScmObj* %_95k47509, %struct.ScmObj** %stackaddr$prim57568, align 8
%stackaddr$prim57569 = alloca %struct.ScmObj*, align 8
%current_45args55755 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55754)
store volatile %struct.ScmObj* %current_45args55755, %struct.ScmObj** %stackaddr$prim57569, align 8
%stackaddr$prim57570 = alloca %struct.ScmObj*, align 8
%anf_45bind47232 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55755)
store volatile %struct.ScmObj* %anf_45bind47232, %struct.ScmObj** %stackaddr$prim57570, align 8
%ae48012 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57571 = alloca %struct.ScmObj*, align 8
%cpsprim47510 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae48012, %struct.ScmObj* %anf_45bind47232)
store volatile %struct.ScmObj* %cpsprim47510, %struct.ScmObj** %stackaddr$prim57571, align 8
%ae48015 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55757$k475080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57572 = alloca %struct.ScmObj*, align 8
%argslist55757$k475081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47510, %struct.ScmObj* %argslist55757$k475080)
store volatile %struct.ScmObj* %argslist55757$k475081, %struct.ScmObj** %stackaddr$prim57572, align 8
%stackaddr$prim57573 = alloca %struct.ScmObj*, align 8
%argslist55757$k475082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48015, %struct.ScmObj* %argslist55757$k475081)
store volatile %struct.ScmObj* %argslist55757$k475082, %struct.ScmObj** %stackaddr$prim57573, align 8
%clofunc57574 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47508)
musttail call tailcc void %clofunc57574(%struct.ScmObj* %k47508, %struct.ScmObj* %argslist55757$k475082)
ret void
}

define tailcc void @proc_clo$ae47843(%struct.ScmObj* %env$ae47843,%struct.ScmObj* %current_45args55762) {
%stackaddr$prim57575 = alloca %struct.ScmObj*, align 8
%k47511 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55762)
store volatile %struct.ScmObj* %k47511, %struct.ScmObj** %stackaddr$prim57575, align 8
%stackaddr$prim57576 = alloca %struct.ScmObj*, align 8
%current_45args55763 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55762)
store volatile %struct.ScmObj* %current_45args55763, %struct.ScmObj** %stackaddr$prim57576, align 8
%stackaddr$prim57577 = alloca %struct.ScmObj*, align 8
%_37take47088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55763)
store volatile %struct.ScmObj* %_37take47088, %struct.ScmObj** %stackaddr$prim57577, align 8
%ae47845 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57578 = alloca %struct.ScmObj*, align 8
%fptrToInt57579 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47846 to i64
%ae47846 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57579)
store volatile %struct.ScmObj* %ae47846, %struct.ScmObj** %stackaddr$makeclosure57578, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47846, %struct.ScmObj* %_37take47088, i64 0)
%argslist55776$k475110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57580 = alloca %struct.ScmObj*, align 8
%argslist55776$k475111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47846, %struct.ScmObj* %argslist55776$k475110)
store volatile %struct.ScmObj* %argslist55776$k475111, %struct.ScmObj** %stackaddr$prim57580, align 8
%stackaddr$prim57581 = alloca %struct.ScmObj*, align 8
%argslist55776$k475112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47845, %struct.ScmObj* %argslist55776$k475111)
store volatile %struct.ScmObj* %argslist55776$k475112, %struct.ScmObj** %stackaddr$prim57581, align 8
%clofunc57582 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47511)
musttail call tailcc void %clofunc57582(%struct.ScmObj* %k47511, %struct.ScmObj* %argslist55776$k475112)
ret void
}

define tailcc void @proc_clo$ae47846(%struct.ScmObj* %env$ae47846,%struct.ScmObj* %current_45args55765) {
%stackaddr$env-ref57583 = alloca %struct.ScmObj*, align 8
%_37take47088 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47846, i64 0)
store %struct.ScmObj* %_37take47088, %struct.ScmObj** %stackaddr$env-ref57583
%stackaddr$prim57584 = alloca %struct.ScmObj*, align 8
%k47512 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55765)
store volatile %struct.ScmObj* %k47512, %struct.ScmObj** %stackaddr$prim57584, align 8
%stackaddr$prim57585 = alloca %struct.ScmObj*, align 8
%current_45args55766 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55765)
store volatile %struct.ScmObj* %current_45args55766, %struct.ScmObj** %stackaddr$prim57585, align 8
%stackaddr$prim57586 = alloca %struct.ScmObj*, align 8
%lst47090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55766)
store volatile %struct.ScmObj* %lst47090, %struct.ScmObj** %stackaddr$prim57586, align 8
%stackaddr$prim57587 = alloca %struct.ScmObj*, align 8
%current_45args55767 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55766)
store volatile %struct.ScmObj* %current_45args55767, %struct.ScmObj** %stackaddr$prim57587, align 8
%stackaddr$prim57588 = alloca %struct.ScmObj*, align 8
%n47089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55767)
store volatile %struct.ScmObj* %n47089, %struct.ScmObj** %stackaddr$prim57588, align 8
%ae47848 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57589 = alloca %struct.ScmObj*, align 8
%anf_45bind47223 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n47089, %struct.ScmObj* %ae47848)
store volatile %struct.ScmObj* %anf_45bind47223, %struct.ScmObj** %stackaddr$prim57589, align 8
%truthy$cmp57590 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47223)
%cmp$cmp57590 = icmp eq i64 %truthy$cmp57590, 1
br i1 %cmp$cmp57590, label %truebranch$cmp57590, label %falsebranch$cmp57590
truebranch$cmp57590:
%ae47851 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47852 = call %struct.ScmObj* @const_init_null()
%argslist55769$k475120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57591 = alloca %struct.ScmObj*, align 8
%argslist55769$k475121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47852, %struct.ScmObj* %argslist55769$k475120)
store volatile %struct.ScmObj* %argslist55769$k475121, %struct.ScmObj** %stackaddr$prim57591, align 8
%stackaddr$prim57592 = alloca %struct.ScmObj*, align 8
%argslist55769$k475122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47851, %struct.ScmObj* %argslist55769$k475121)
store volatile %struct.ScmObj* %argslist55769$k475122, %struct.ScmObj** %stackaddr$prim57592, align 8
%clofunc57593 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47512)
musttail call tailcc void %clofunc57593(%struct.ScmObj* %k47512, %struct.ScmObj* %argslist55769$k475122)
ret void
falsebranch$cmp57590:
%stackaddr$prim57594 = alloca %struct.ScmObj*, align 8
%anf_45bind47224 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47090)
store volatile %struct.ScmObj* %anf_45bind47224, %struct.ScmObj** %stackaddr$prim57594, align 8
%truthy$cmp57595 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47224)
%cmp$cmp57595 = icmp eq i64 %truthy$cmp57595, 1
br i1 %cmp$cmp57595, label %truebranch$cmp57595, label %falsebranch$cmp57595
truebranch$cmp57595:
%ae47862 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47863 = call %struct.ScmObj* @const_init_null()
%argslist55770$k475120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57596 = alloca %struct.ScmObj*, align 8
%argslist55770$k475121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47863, %struct.ScmObj* %argslist55770$k475120)
store volatile %struct.ScmObj* %argslist55770$k475121, %struct.ScmObj** %stackaddr$prim57596, align 8
%stackaddr$prim57597 = alloca %struct.ScmObj*, align 8
%argslist55770$k475122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47862, %struct.ScmObj* %argslist55770$k475121)
store volatile %struct.ScmObj* %argslist55770$k475122, %struct.ScmObj** %stackaddr$prim57597, align 8
%clofunc57598 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47512)
musttail call tailcc void %clofunc57598(%struct.ScmObj* %k47512, %struct.ScmObj* %argslist55770$k475122)
ret void
falsebranch$cmp57595:
%stackaddr$prim57599 = alloca %struct.ScmObj*, align 8
%anf_45bind47225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47090)
store volatile %struct.ScmObj* %anf_45bind47225, %struct.ScmObj** %stackaddr$prim57599, align 8
%stackaddr$prim57600 = alloca %struct.ScmObj*, align 8
%anf_45bind47226 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47090)
store volatile %struct.ScmObj* %anf_45bind47226, %struct.ScmObj** %stackaddr$prim57600, align 8
%ae47873 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57601 = alloca %struct.ScmObj*, align 8
%anf_45bind47227 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n47089, %struct.ScmObj* %ae47873)
store volatile %struct.ScmObj* %anf_45bind47227, %struct.ScmObj** %stackaddr$prim57601, align 8
%stackaddr$makeclosure57602 = alloca %struct.ScmObj*, align 8
%fptrToInt57603 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47875 to i64
%ae47875 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57603)
store volatile %struct.ScmObj* %ae47875, %struct.ScmObj** %stackaddr$makeclosure57602, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47875, %struct.ScmObj* %anf_45bind47225, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47875, %struct.ScmObj* %k47512, i64 1)
%argslist55775$_37take470880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57604 = alloca %struct.ScmObj*, align 8
%argslist55775$_37take470881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47227, %struct.ScmObj* %argslist55775$_37take470880)
store volatile %struct.ScmObj* %argslist55775$_37take470881, %struct.ScmObj** %stackaddr$prim57604, align 8
%stackaddr$prim57605 = alloca %struct.ScmObj*, align 8
%argslist55775$_37take470882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47226, %struct.ScmObj* %argslist55775$_37take470881)
store volatile %struct.ScmObj* %argslist55775$_37take470882, %struct.ScmObj** %stackaddr$prim57605, align 8
%stackaddr$prim57606 = alloca %struct.ScmObj*, align 8
%argslist55775$_37take470883 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47875, %struct.ScmObj* %argslist55775$_37take470882)
store volatile %struct.ScmObj* %argslist55775$_37take470883, %struct.ScmObj** %stackaddr$prim57606, align 8
%clofunc57607 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47088)
musttail call tailcc void %clofunc57607(%struct.ScmObj* %_37take47088, %struct.ScmObj* %argslist55775$_37take470883)
ret void
}

define tailcc void @proc_clo$ae47875(%struct.ScmObj* %env$ae47875,%struct.ScmObj* %current_45args55771) {
%stackaddr$env-ref57608 = alloca %struct.ScmObj*, align 8
%anf_45bind47225 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47875, i64 0)
store %struct.ScmObj* %anf_45bind47225, %struct.ScmObj** %stackaddr$env-ref57608
%stackaddr$env-ref57609 = alloca %struct.ScmObj*, align 8
%k47512 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47875, i64 1)
store %struct.ScmObj* %k47512, %struct.ScmObj** %stackaddr$env-ref57609
%stackaddr$prim57610 = alloca %struct.ScmObj*, align 8
%_95k47513 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55771)
store volatile %struct.ScmObj* %_95k47513, %struct.ScmObj** %stackaddr$prim57610, align 8
%stackaddr$prim57611 = alloca %struct.ScmObj*, align 8
%current_45args55772 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55771)
store volatile %struct.ScmObj* %current_45args55772, %struct.ScmObj** %stackaddr$prim57611, align 8
%stackaddr$prim57612 = alloca %struct.ScmObj*, align 8
%anf_45bind47228 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55772)
store volatile %struct.ScmObj* %anf_45bind47228, %struct.ScmObj** %stackaddr$prim57612, align 8
%stackaddr$prim57613 = alloca %struct.ScmObj*, align 8
%cpsprim47514 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47225, %struct.ScmObj* %anf_45bind47228)
store volatile %struct.ScmObj* %cpsprim47514, %struct.ScmObj** %stackaddr$prim57613, align 8
%ae47881 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55774$k475120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57614 = alloca %struct.ScmObj*, align 8
%argslist55774$k475121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47514, %struct.ScmObj* %argslist55774$k475120)
store volatile %struct.ScmObj* %argslist55774$k475121, %struct.ScmObj** %stackaddr$prim57614, align 8
%stackaddr$prim57615 = alloca %struct.ScmObj*, align 8
%argslist55774$k475122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47881, %struct.ScmObj* %argslist55774$k475121)
store volatile %struct.ScmObj* %argslist55774$k475122, %struct.ScmObj** %stackaddr$prim57615, align 8
%clofunc57616 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47512)
musttail call tailcc void %clofunc57616(%struct.ScmObj* %k47512, %struct.ScmObj* %argslist55774$k475122)
ret void
}

define tailcc void @proc_clo$ae47746(%struct.ScmObj* %env$ae47746,%struct.ScmObj* %current_45args55779) {
%stackaddr$prim57617 = alloca %struct.ScmObj*, align 8
%k47515 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55779)
store volatile %struct.ScmObj* %k47515, %struct.ScmObj** %stackaddr$prim57617, align 8
%stackaddr$prim57618 = alloca %struct.ScmObj*, align 8
%current_45args55780 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55779)
store volatile %struct.ScmObj* %current_45args55780, %struct.ScmObj** %stackaddr$prim57618, align 8
%stackaddr$prim57619 = alloca %struct.ScmObj*, align 8
%_37map47092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55780)
store volatile %struct.ScmObj* %_37map47092, %struct.ScmObj** %stackaddr$prim57619, align 8
%ae47748 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57620 = alloca %struct.ScmObj*, align 8
%fptrToInt57621 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47749 to i64
%ae47749 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57621)
store volatile %struct.ScmObj* %ae47749, %struct.ScmObj** %stackaddr$makeclosure57620, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47749, %struct.ScmObj* %_37map47092, i64 0)
%argslist55796$k475150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57622 = alloca %struct.ScmObj*, align 8
%argslist55796$k475151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47749, %struct.ScmObj* %argslist55796$k475150)
store volatile %struct.ScmObj* %argslist55796$k475151, %struct.ScmObj** %stackaddr$prim57622, align 8
%stackaddr$prim57623 = alloca %struct.ScmObj*, align 8
%argslist55796$k475152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47748, %struct.ScmObj* %argslist55796$k475151)
store volatile %struct.ScmObj* %argslist55796$k475152, %struct.ScmObj** %stackaddr$prim57623, align 8
%clofunc57624 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47515)
musttail call tailcc void %clofunc57624(%struct.ScmObj* %k47515, %struct.ScmObj* %argslist55796$k475152)
ret void
}

define tailcc void @proc_clo$ae47749(%struct.ScmObj* %env$ae47749,%struct.ScmObj* %current_45args55782) {
%stackaddr$env-ref57625 = alloca %struct.ScmObj*, align 8
%_37map47092 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47749, i64 0)
store %struct.ScmObj* %_37map47092, %struct.ScmObj** %stackaddr$env-ref57625
%stackaddr$prim57626 = alloca %struct.ScmObj*, align 8
%k47516 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55782)
store volatile %struct.ScmObj* %k47516, %struct.ScmObj** %stackaddr$prim57626, align 8
%stackaddr$prim57627 = alloca %struct.ScmObj*, align 8
%current_45args55783 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55782)
store volatile %struct.ScmObj* %current_45args55783, %struct.ScmObj** %stackaddr$prim57627, align 8
%stackaddr$prim57628 = alloca %struct.ScmObj*, align 8
%f47094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55783)
store volatile %struct.ScmObj* %f47094, %struct.ScmObj** %stackaddr$prim57628, align 8
%stackaddr$prim57629 = alloca %struct.ScmObj*, align 8
%current_45args55784 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55783)
store volatile %struct.ScmObj* %current_45args55784, %struct.ScmObj** %stackaddr$prim57629, align 8
%stackaddr$prim57630 = alloca %struct.ScmObj*, align 8
%lst47093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55784)
store volatile %struct.ScmObj* %lst47093, %struct.ScmObj** %stackaddr$prim57630, align 8
%stackaddr$prim57631 = alloca %struct.ScmObj*, align 8
%anf_45bind47217 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47093)
store volatile %struct.ScmObj* %anf_45bind47217, %struct.ScmObj** %stackaddr$prim57631, align 8
%truthy$cmp57632 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47217)
%cmp$cmp57632 = icmp eq i64 %truthy$cmp57632, 1
br i1 %cmp$cmp57632, label %truebranch$cmp57632, label %falsebranch$cmp57632
truebranch$cmp57632:
%ae47753 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47754 = call %struct.ScmObj* @const_init_null()
%argslist55786$k475160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57633 = alloca %struct.ScmObj*, align 8
%argslist55786$k475161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47754, %struct.ScmObj* %argslist55786$k475160)
store volatile %struct.ScmObj* %argslist55786$k475161, %struct.ScmObj** %stackaddr$prim57633, align 8
%stackaddr$prim57634 = alloca %struct.ScmObj*, align 8
%argslist55786$k475162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47753, %struct.ScmObj* %argslist55786$k475161)
store volatile %struct.ScmObj* %argslist55786$k475162, %struct.ScmObj** %stackaddr$prim57634, align 8
%clofunc57635 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47516)
musttail call tailcc void %clofunc57635(%struct.ScmObj* %k47516, %struct.ScmObj* %argslist55786$k475162)
ret void
falsebranch$cmp57632:
%stackaddr$prim57636 = alloca %struct.ScmObj*, align 8
%anf_45bind47218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47093)
store volatile %struct.ScmObj* %anf_45bind47218, %struct.ScmObj** %stackaddr$prim57636, align 8
%stackaddr$makeclosure57637 = alloca %struct.ScmObj*, align 8
%fptrToInt57638 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47763 to i64
%ae47763 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57638)
store volatile %struct.ScmObj* %ae47763, %struct.ScmObj** %stackaddr$makeclosure57637, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47763, %struct.ScmObj* %f47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47763, %struct.ScmObj* %lst47093, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47763, %struct.ScmObj* %_37map47092, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47763, %struct.ScmObj* %k47516, i64 3)
%argslist55795$f470940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57639 = alloca %struct.ScmObj*, align 8
%argslist55795$f470941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47218, %struct.ScmObj* %argslist55795$f470940)
store volatile %struct.ScmObj* %argslist55795$f470941, %struct.ScmObj** %stackaddr$prim57639, align 8
%stackaddr$prim57640 = alloca %struct.ScmObj*, align 8
%argslist55795$f470942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47763, %struct.ScmObj* %argslist55795$f470941)
store volatile %struct.ScmObj* %argslist55795$f470942, %struct.ScmObj** %stackaddr$prim57640, align 8
%clofunc57641 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47094)
musttail call tailcc void %clofunc57641(%struct.ScmObj* %f47094, %struct.ScmObj* %argslist55795$f470942)
ret void
}

define tailcc void @proc_clo$ae47763(%struct.ScmObj* %env$ae47763,%struct.ScmObj* %current_45args55787) {
%stackaddr$env-ref57642 = alloca %struct.ScmObj*, align 8
%f47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47763, i64 0)
store %struct.ScmObj* %f47094, %struct.ScmObj** %stackaddr$env-ref57642
%stackaddr$env-ref57643 = alloca %struct.ScmObj*, align 8
%lst47093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47763, i64 1)
store %struct.ScmObj* %lst47093, %struct.ScmObj** %stackaddr$env-ref57643
%stackaddr$env-ref57644 = alloca %struct.ScmObj*, align 8
%_37map47092 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47763, i64 2)
store %struct.ScmObj* %_37map47092, %struct.ScmObj** %stackaddr$env-ref57644
%stackaddr$env-ref57645 = alloca %struct.ScmObj*, align 8
%k47516 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47763, i64 3)
store %struct.ScmObj* %k47516, %struct.ScmObj** %stackaddr$env-ref57645
%stackaddr$prim57646 = alloca %struct.ScmObj*, align 8
%_95k47517 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55787)
store volatile %struct.ScmObj* %_95k47517, %struct.ScmObj** %stackaddr$prim57646, align 8
%stackaddr$prim57647 = alloca %struct.ScmObj*, align 8
%current_45args55788 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55787)
store volatile %struct.ScmObj* %current_45args55788, %struct.ScmObj** %stackaddr$prim57647, align 8
%stackaddr$prim57648 = alloca %struct.ScmObj*, align 8
%anf_45bind47219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55788)
store volatile %struct.ScmObj* %anf_45bind47219, %struct.ScmObj** %stackaddr$prim57648, align 8
%stackaddr$prim57649 = alloca %struct.ScmObj*, align 8
%anf_45bind47220 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47093)
store volatile %struct.ScmObj* %anf_45bind47220, %struct.ScmObj** %stackaddr$prim57649, align 8
%stackaddr$makeclosure57650 = alloca %struct.ScmObj*, align 8
%fptrToInt57651 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47767 to i64
%ae47767 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57651)
store volatile %struct.ScmObj* %ae47767, %struct.ScmObj** %stackaddr$makeclosure57650, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47767, %struct.ScmObj* %anf_45bind47219, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47767, %struct.ScmObj* %k47516, i64 1)
%argslist55794$_37map470920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57652 = alloca %struct.ScmObj*, align 8
%argslist55794$_37map470921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47220, %struct.ScmObj* %argslist55794$_37map470920)
store volatile %struct.ScmObj* %argslist55794$_37map470921, %struct.ScmObj** %stackaddr$prim57652, align 8
%stackaddr$prim57653 = alloca %struct.ScmObj*, align 8
%argslist55794$_37map470922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47094, %struct.ScmObj* %argslist55794$_37map470921)
store volatile %struct.ScmObj* %argslist55794$_37map470922, %struct.ScmObj** %stackaddr$prim57653, align 8
%stackaddr$prim57654 = alloca %struct.ScmObj*, align 8
%argslist55794$_37map470923 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47767, %struct.ScmObj* %argslist55794$_37map470922)
store volatile %struct.ScmObj* %argslist55794$_37map470923, %struct.ScmObj** %stackaddr$prim57654, align 8
%clofunc57655 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map47092)
musttail call tailcc void %clofunc57655(%struct.ScmObj* %_37map47092, %struct.ScmObj* %argslist55794$_37map470923)
ret void
}

define tailcc void @proc_clo$ae47767(%struct.ScmObj* %env$ae47767,%struct.ScmObj* %current_45args55790) {
%stackaddr$env-ref57656 = alloca %struct.ScmObj*, align 8
%anf_45bind47219 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47767, i64 0)
store %struct.ScmObj* %anf_45bind47219, %struct.ScmObj** %stackaddr$env-ref57656
%stackaddr$env-ref57657 = alloca %struct.ScmObj*, align 8
%k47516 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47767, i64 1)
store %struct.ScmObj* %k47516, %struct.ScmObj** %stackaddr$env-ref57657
%stackaddr$prim57658 = alloca %struct.ScmObj*, align 8
%_95k47518 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55790)
store volatile %struct.ScmObj* %_95k47518, %struct.ScmObj** %stackaddr$prim57658, align 8
%stackaddr$prim57659 = alloca %struct.ScmObj*, align 8
%current_45args55791 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55790)
store volatile %struct.ScmObj* %current_45args55791, %struct.ScmObj** %stackaddr$prim57659, align 8
%stackaddr$prim57660 = alloca %struct.ScmObj*, align 8
%anf_45bind47221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55791)
store volatile %struct.ScmObj* %anf_45bind47221, %struct.ScmObj** %stackaddr$prim57660, align 8
%stackaddr$prim57661 = alloca %struct.ScmObj*, align 8
%cpsprim47519 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47219, %struct.ScmObj* %anf_45bind47221)
store volatile %struct.ScmObj* %cpsprim47519, %struct.ScmObj** %stackaddr$prim57661, align 8
%ae47773 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55793$k475160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57662 = alloca %struct.ScmObj*, align 8
%argslist55793$k475161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47519, %struct.ScmObj* %argslist55793$k475160)
store volatile %struct.ScmObj* %argslist55793$k475161, %struct.ScmObj** %stackaddr$prim57662, align 8
%stackaddr$prim57663 = alloca %struct.ScmObj*, align 8
%argslist55793$k475162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47773, %struct.ScmObj* %argslist55793$k475161)
store volatile %struct.ScmObj* %argslist55793$k475162, %struct.ScmObj** %stackaddr$prim57663, align 8
%clofunc57664 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47516)
musttail call tailcc void %clofunc57664(%struct.ScmObj* %k47516, %struct.ScmObj* %argslist55793$k475162)
ret void
}

define tailcc void @proc_clo$ae47666(%struct.ScmObj* %env$ae47666,%struct.ScmObj* %current_45args55799) {
%stackaddr$prim57665 = alloca %struct.ScmObj*, align 8
%k47520 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55799)
store volatile %struct.ScmObj* %k47520, %struct.ScmObj** %stackaddr$prim57665, align 8
%stackaddr$prim57666 = alloca %struct.ScmObj*, align 8
%current_45args55800 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55799)
store volatile %struct.ScmObj* %current_45args55800, %struct.ScmObj** %stackaddr$prim57666, align 8
%stackaddr$prim57667 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55800)
store volatile %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$prim57667, align 8
%ae47668 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57668 = alloca %struct.ScmObj*, align 8
%fptrToInt57669 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47669 to i64
%ae47669 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57669)
store volatile %struct.ScmObj* %ae47669, %struct.ScmObj** %stackaddr$makeclosure57668, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47669, %struct.ScmObj* %_37foldr147096, i64 0)
%argslist55813$k475200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57670 = alloca %struct.ScmObj*, align 8
%argslist55813$k475201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47669, %struct.ScmObj* %argslist55813$k475200)
store volatile %struct.ScmObj* %argslist55813$k475201, %struct.ScmObj** %stackaddr$prim57670, align 8
%stackaddr$prim57671 = alloca %struct.ScmObj*, align 8
%argslist55813$k475202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47668, %struct.ScmObj* %argslist55813$k475201)
store volatile %struct.ScmObj* %argslist55813$k475202, %struct.ScmObj** %stackaddr$prim57671, align 8
%clofunc57672 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47520)
musttail call tailcc void %clofunc57672(%struct.ScmObj* %k47520, %struct.ScmObj* %argslist55813$k475202)
ret void
}

define tailcc void @proc_clo$ae47669(%struct.ScmObj* %env$ae47669,%struct.ScmObj* %current_45args55802) {
%stackaddr$env-ref57673 = alloca %struct.ScmObj*, align 8
%_37foldr147096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47669, i64 0)
store %struct.ScmObj* %_37foldr147096, %struct.ScmObj** %stackaddr$env-ref57673
%stackaddr$prim57674 = alloca %struct.ScmObj*, align 8
%k47521 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55802)
store volatile %struct.ScmObj* %k47521, %struct.ScmObj** %stackaddr$prim57674, align 8
%stackaddr$prim57675 = alloca %struct.ScmObj*, align 8
%current_45args55803 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55802)
store volatile %struct.ScmObj* %current_45args55803, %struct.ScmObj** %stackaddr$prim57675, align 8
%stackaddr$prim57676 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55803)
store volatile %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$prim57676, align 8
%stackaddr$prim57677 = alloca %struct.ScmObj*, align 8
%current_45args55804 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55803)
store volatile %struct.ScmObj* %current_45args55804, %struct.ScmObj** %stackaddr$prim57677, align 8
%stackaddr$prim57678 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55804)
store volatile %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$prim57678, align 8
%stackaddr$prim57679 = alloca %struct.ScmObj*, align 8
%current_45args55805 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55804)
store volatile %struct.ScmObj* %current_45args55805, %struct.ScmObj** %stackaddr$prim57679, align 8
%stackaddr$prim57680 = alloca %struct.ScmObj*, align 8
%lst47097 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55805)
store volatile %struct.ScmObj* %lst47097, %struct.ScmObj** %stackaddr$prim57680, align 8
%stackaddr$prim57681 = alloca %struct.ScmObj*, align 8
%anf_45bind47212 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47097)
store volatile %struct.ScmObj* %anf_45bind47212, %struct.ScmObj** %stackaddr$prim57681, align 8
%truthy$cmp57682 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47212)
%cmp$cmp57682 = icmp eq i64 %truthy$cmp57682, 1
br i1 %cmp$cmp57682, label %truebranch$cmp57682, label %falsebranch$cmp57682
truebranch$cmp57682:
%ae47673 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55807$k475210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57683 = alloca %struct.ScmObj*, align 8
%argslist55807$k475211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47098, %struct.ScmObj* %argslist55807$k475210)
store volatile %struct.ScmObj* %argslist55807$k475211, %struct.ScmObj** %stackaddr$prim57683, align 8
%stackaddr$prim57684 = alloca %struct.ScmObj*, align 8
%argslist55807$k475212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47673, %struct.ScmObj* %argslist55807$k475211)
store volatile %struct.ScmObj* %argslist55807$k475212, %struct.ScmObj** %stackaddr$prim57684, align 8
%clofunc57685 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47521)
musttail call tailcc void %clofunc57685(%struct.ScmObj* %k47521, %struct.ScmObj* %argslist55807$k475212)
ret void
falsebranch$cmp57682:
%stackaddr$prim57686 = alloca %struct.ScmObj*, align 8
%anf_45bind47213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47097)
store volatile %struct.ScmObj* %anf_45bind47213, %struct.ScmObj** %stackaddr$prim57686, align 8
%stackaddr$prim57687 = alloca %struct.ScmObj*, align 8
%anf_45bind47214 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47097)
store volatile %struct.ScmObj* %anf_45bind47214, %struct.ScmObj** %stackaddr$prim57687, align 8
%stackaddr$makeclosure57688 = alloca %struct.ScmObj*, align 8
%fptrToInt57689 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47681 to i64
%ae47681 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57689)
store volatile %struct.ScmObj* %ae47681, %struct.ScmObj** %stackaddr$makeclosure57688, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47681, %struct.ScmObj* %k47521, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47681, %struct.ScmObj* %anf_45bind47213, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47681, %struct.ScmObj* %f47099, i64 2)
%argslist55812$_37foldr1470960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57690 = alloca %struct.ScmObj*, align 8
%argslist55812$_37foldr1470961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47214, %struct.ScmObj* %argslist55812$_37foldr1470960)
store volatile %struct.ScmObj* %argslist55812$_37foldr1470961, %struct.ScmObj** %stackaddr$prim57690, align 8
%stackaddr$prim57691 = alloca %struct.ScmObj*, align 8
%argslist55812$_37foldr1470962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47098, %struct.ScmObj* %argslist55812$_37foldr1470961)
store volatile %struct.ScmObj* %argslist55812$_37foldr1470962, %struct.ScmObj** %stackaddr$prim57691, align 8
%stackaddr$prim57692 = alloca %struct.ScmObj*, align 8
%argslist55812$_37foldr1470963 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47099, %struct.ScmObj* %argslist55812$_37foldr1470962)
store volatile %struct.ScmObj* %argslist55812$_37foldr1470963, %struct.ScmObj** %stackaddr$prim57692, align 8
%stackaddr$prim57693 = alloca %struct.ScmObj*, align 8
%argslist55812$_37foldr1470964 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47681, %struct.ScmObj* %argslist55812$_37foldr1470963)
store volatile %struct.ScmObj* %argslist55812$_37foldr1470964, %struct.ScmObj** %stackaddr$prim57693, align 8
%clofunc57694 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147096)
musttail call tailcc void %clofunc57694(%struct.ScmObj* %_37foldr147096, %struct.ScmObj* %argslist55812$_37foldr1470964)
ret void
}

define tailcc void @proc_clo$ae47681(%struct.ScmObj* %env$ae47681,%struct.ScmObj* %current_45args55808) {
%stackaddr$env-ref57695 = alloca %struct.ScmObj*, align 8
%k47521 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47681, i64 0)
store %struct.ScmObj* %k47521, %struct.ScmObj** %stackaddr$env-ref57695
%stackaddr$env-ref57696 = alloca %struct.ScmObj*, align 8
%anf_45bind47213 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47681, i64 1)
store %struct.ScmObj* %anf_45bind47213, %struct.ScmObj** %stackaddr$env-ref57696
%stackaddr$env-ref57697 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47681, i64 2)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref57697
%stackaddr$prim57698 = alloca %struct.ScmObj*, align 8
%_95k47522 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55808)
store volatile %struct.ScmObj* %_95k47522, %struct.ScmObj** %stackaddr$prim57698, align 8
%stackaddr$prim57699 = alloca %struct.ScmObj*, align 8
%current_45args55809 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55808)
store volatile %struct.ScmObj* %current_45args55809, %struct.ScmObj** %stackaddr$prim57699, align 8
%stackaddr$prim57700 = alloca %struct.ScmObj*, align 8
%anf_45bind47215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55809)
store volatile %struct.ScmObj* %anf_45bind47215, %struct.ScmObj** %stackaddr$prim57700, align 8
%argslist55811$f470990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57701 = alloca %struct.ScmObj*, align 8
%argslist55811$f470991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47215, %struct.ScmObj* %argslist55811$f470990)
store volatile %struct.ScmObj* %argslist55811$f470991, %struct.ScmObj** %stackaddr$prim57701, align 8
%stackaddr$prim57702 = alloca %struct.ScmObj*, align 8
%argslist55811$f470992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47213, %struct.ScmObj* %argslist55811$f470991)
store volatile %struct.ScmObj* %argslist55811$f470992, %struct.ScmObj** %stackaddr$prim57702, align 8
%stackaddr$prim57703 = alloca %struct.ScmObj*, align 8
%argslist55811$f470993 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47521, %struct.ScmObj* %argslist55811$f470992)
store volatile %struct.ScmObj* %argslist55811$f470993, %struct.ScmObj** %stackaddr$prim57703, align 8
%clofunc57704 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47099)
musttail call tailcc void %clofunc57704(%struct.ScmObj* %f47099, %struct.ScmObj* %argslist55811$f470993)
ret void
}

define tailcc void @proc_clo$ae47549(%struct.ScmObj* %env$ae47549,%struct.ScmObj* %current_45args55816) {
%stackaddr$prim57705 = alloca %struct.ScmObj*, align 8
%k47523 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55816)
store volatile %struct.ScmObj* %k47523, %struct.ScmObj** %stackaddr$prim57705, align 8
%stackaddr$prim57706 = alloca %struct.ScmObj*, align 8
%current_45args55817 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55816)
store volatile %struct.ScmObj* %current_45args55817, %struct.ScmObj** %stackaddr$prim57706, align 8
%stackaddr$prim57707 = alloca %struct.ScmObj*, align 8
%y47076 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55817)
store volatile %struct.ScmObj* %y47076, %struct.ScmObj** %stackaddr$prim57707, align 8
%ae47551 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57708 = alloca %struct.ScmObj*, align 8
%fptrToInt57709 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47552 to i64
%ae47552 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57709)
store volatile %struct.ScmObj* %ae47552, %struct.ScmObj** %stackaddr$makeclosure57708, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47552, %struct.ScmObj* %y47076, i64 0)
%argslist55835$k475230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57710 = alloca %struct.ScmObj*, align 8
%argslist55835$k475231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47552, %struct.ScmObj* %argslist55835$k475230)
store volatile %struct.ScmObj* %argslist55835$k475231, %struct.ScmObj** %stackaddr$prim57710, align 8
%stackaddr$prim57711 = alloca %struct.ScmObj*, align 8
%argslist55835$k475232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47551, %struct.ScmObj* %argslist55835$k475231)
store volatile %struct.ScmObj* %argslist55835$k475232, %struct.ScmObj** %stackaddr$prim57711, align 8
%clofunc57712 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47523)
musttail call tailcc void %clofunc57712(%struct.ScmObj* %k47523, %struct.ScmObj* %argslist55835$k475232)
ret void
}

define tailcc void @proc_clo$ae47552(%struct.ScmObj* %env$ae47552,%struct.ScmObj* %current_45args55819) {
%stackaddr$env-ref57713 = alloca %struct.ScmObj*, align 8
%y47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47552, i64 0)
store %struct.ScmObj* %y47076, %struct.ScmObj** %stackaddr$env-ref57713
%stackaddr$prim57714 = alloca %struct.ScmObj*, align 8
%k47524 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55819)
store volatile %struct.ScmObj* %k47524, %struct.ScmObj** %stackaddr$prim57714, align 8
%stackaddr$prim57715 = alloca %struct.ScmObj*, align 8
%current_45args55820 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55819)
store volatile %struct.ScmObj* %current_45args55820, %struct.ScmObj** %stackaddr$prim57715, align 8
%stackaddr$prim57716 = alloca %struct.ScmObj*, align 8
%f47077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55820)
store volatile %struct.ScmObj* %f47077, %struct.ScmObj** %stackaddr$prim57716, align 8
%stackaddr$makeclosure57717 = alloca %struct.ScmObj*, align 8
%fptrToInt57718 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47553 to i64
%ae47553 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57718)
store volatile %struct.ScmObj* %ae47553, %struct.ScmObj** %stackaddr$makeclosure57717, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47553, %struct.ScmObj* %f47077, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47553, %struct.ScmObj* %k47524, i64 1)
%ae47554 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57719 = alloca %struct.ScmObj*, align 8
%fptrToInt57720 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47555 to i64
%ae47555 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57720)
store volatile %struct.ScmObj* %ae47555, %struct.ScmObj** %stackaddr$makeclosure57719, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47555, %struct.ScmObj* %f47077, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47555, %struct.ScmObj* %y47076, i64 1)
%argslist55834$ae475530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57721 = alloca %struct.ScmObj*, align 8
%argslist55834$ae475531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47555, %struct.ScmObj* %argslist55834$ae475530)
store volatile %struct.ScmObj* %argslist55834$ae475531, %struct.ScmObj** %stackaddr$prim57721, align 8
%stackaddr$prim57722 = alloca %struct.ScmObj*, align 8
%argslist55834$ae475532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47554, %struct.ScmObj* %argslist55834$ae475531)
store volatile %struct.ScmObj* %argslist55834$ae475532, %struct.ScmObj** %stackaddr$prim57722, align 8
%clofunc57723 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47553)
musttail call tailcc void %clofunc57723(%struct.ScmObj* %ae47553, %struct.ScmObj* %argslist55834$ae475532)
ret void
}

define tailcc void @proc_clo$ae47553(%struct.ScmObj* %env$ae47553,%struct.ScmObj* %current_45args55822) {
%stackaddr$env-ref57724 = alloca %struct.ScmObj*, align 8
%f47077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47553, i64 0)
store %struct.ScmObj* %f47077, %struct.ScmObj** %stackaddr$env-ref57724
%stackaddr$env-ref57725 = alloca %struct.ScmObj*, align 8
%k47524 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47553, i64 1)
store %struct.ScmObj* %k47524, %struct.ScmObj** %stackaddr$env-ref57725
%stackaddr$prim57726 = alloca %struct.ScmObj*, align 8
%_95k47525 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55822)
store volatile %struct.ScmObj* %_95k47525, %struct.ScmObj** %stackaddr$prim57726, align 8
%stackaddr$prim57727 = alloca %struct.ScmObj*, align 8
%current_45args55823 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55822)
store volatile %struct.ScmObj* %current_45args55823, %struct.ScmObj** %stackaddr$prim57727, align 8
%stackaddr$prim57728 = alloca %struct.ScmObj*, align 8
%anf_45bind47210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55823)
store volatile %struct.ScmObj* %anf_45bind47210, %struct.ScmObj** %stackaddr$prim57728, align 8
%argslist55825$f470770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57729 = alloca %struct.ScmObj*, align 8
%argslist55825$f470771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47210, %struct.ScmObj* %argslist55825$f470770)
store volatile %struct.ScmObj* %argslist55825$f470771, %struct.ScmObj** %stackaddr$prim57729, align 8
%stackaddr$prim57730 = alloca %struct.ScmObj*, align 8
%argslist55825$f470772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47524, %struct.ScmObj* %argslist55825$f470771)
store volatile %struct.ScmObj* %argslist55825$f470772, %struct.ScmObj** %stackaddr$prim57730, align 8
%clofunc57731 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47077)
musttail call tailcc void %clofunc57731(%struct.ScmObj* %f47077, %struct.ScmObj* %argslist55825$f470772)
ret void
}

define tailcc void @proc_clo$ae47555(%struct.ScmObj* %env$ae47555,%struct.ScmObj* %args4707847526) {
%stackaddr$env-ref57732 = alloca %struct.ScmObj*, align 8
%f47077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47555, i64 0)
store %struct.ScmObj* %f47077, %struct.ScmObj** %stackaddr$env-ref57732
%stackaddr$env-ref57733 = alloca %struct.ScmObj*, align 8
%y47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47555, i64 1)
store %struct.ScmObj* %y47076, %struct.ScmObj** %stackaddr$env-ref57733
%stackaddr$prim57734 = alloca %struct.ScmObj*, align 8
%k47527 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4707847526)
store volatile %struct.ScmObj* %k47527, %struct.ScmObj** %stackaddr$prim57734, align 8
%stackaddr$prim57735 = alloca %struct.ScmObj*, align 8
%args47078 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4707847526)
store volatile %struct.ScmObj* %args47078, %struct.ScmObj** %stackaddr$prim57735, align 8
%stackaddr$makeclosure57736 = alloca %struct.ScmObj*, align 8
%fptrToInt57737 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47559 to i64
%ae47559 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57737)
store volatile %struct.ScmObj* %ae47559, %struct.ScmObj** %stackaddr$makeclosure57736, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47559, %struct.ScmObj* %args47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47559, %struct.ScmObj* %f47077, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47559, %struct.ScmObj* %k47527, i64 2)
%argslist55833$y470760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57738 = alloca %struct.ScmObj*, align 8
%argslist55833$y470761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y47076, %struct.ScmObj* %argslist55833$y470760)
store volatile %struct.ScmObj* %argslist55833$y470761, %struct.ScmObj** %stackaddr$prim57738, align 8
%stackaddr$prim57739 = alloca %struct.ScmObj*, align 8
%argslist55833$y470762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47559, %struct.ScmObj* %argslist55833$y470761)
store volatile %struct.ScmObj* %argslist55833$y470762, %struct.ScmObj** %stackaddr$prim57739, align 8
%clofunc57740 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y47076)
musttail call tailcc void %clofunc57740(%struct.ScmObj* %y47076, %struct.ScmObj* %argslist55833$y470762)
ret void
}

define tailcc void @proc_clo$ae47559(%struct.ScmObj* %env$ae47559,%struct.ScmObj* %current_45args55826) {
%stackaddr$env-ref57741 = alloca %struct.ScmObj*, align 8
%args47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47559, i64 0)
store %struct.ScmObj* %args47078, %struct.ScmObj** %stackaddr$env-ref57741
%stackaddr$env-ref57742 = alloca %struct.ScmObj*, align 8
%f47077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47559, i64 1)
store %struct.ScmObj* %f47077, %struct.ScmObj** %stackaddr$env-ref57742
%stackaddr$env-ref57743 = alloca %struct.ScmObj*, align 8
%k47527 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47559, i64 2)
store %struct.ScmObj* %k47527, %struct.ScmObj** %stackaddr$env-ref57743
%stackaddr$prim57744 = alloca %struct.ScmObj*, align 8
%_95k47528 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55826)
store volatile %struct.ScmObj* %_95k47528, %struct.ScmObj** %stackaddr$prim57744, align 8
%stackaddr$prim57745 = alloca %struct.ScmObj*, align 8
%current_45args55827 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55826)
store volatile %struct.ScmObj* %current_45args55827, %struct.ScmObj** %stackaddr$prim57745, align 8
%stackaddr$prim57746 = alloca %struct.ScmObj*, align 8
%anf_45bind47208 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55827)
store volatile %struct.ScmObj* %anf_45bind47208, %struct.ScmObj** %stackaddr$prim57746, align 8
%stackaddr$makeclosure57747 = alloca %struct.ScmObj*, align 8
%fptrToInt57748 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47562 to i64
%ae47562 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57748)
store volatile %struct.ScmObj* %ae47562, %struct.ScmObj** %stackaddr$makeclosure57747, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47562, %struct.ScmObj* %args47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47562, %struct.ScmObj* %k47527, i64 1)
%argslist55832$anf_45bind472080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57749 = alloca %struct.ScmObj*, align 8
%argslist55832$anf_45bind472081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47077, %struct.ScmObj* %argslist55832$anf_45bind472080)
store volatile %struct.ScmObj* %argslist55832$anf_45bind472081, %struct.ScmObj** %stackaddr$prim57749, align 8
%stackaddr$prim57750 = alloca %struct.ScmObj*, align 8
%argslist55832$anf_45bind472082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47562, %struct.ScmObj* %argslist55832$anf_45bind472081)
store volatile %struct.ScmObj* %argslist55832$anf_45bind472082, %struct.ScmObj** %stackaddr$prim57750, align 8
%clofunc57751 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47208)
musttail call tailcc void %clofunc57751(%struct.ScmObj* %anf_45bind47208, %struct.ScmObj* %argslist55832$anf_45bind472082)
ret void
}

define tailcc void @proc_clo$ae47562(%struct.ScmObj* %env$ae47562,%struct.ScmObj* %current_45args55829) {
%stackaddr$env-ref57752 = alloca %struct.ScmObj*, align 8
%args47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47562, i64 0)
store %struct.ScmObj* %args47078, %struct.ScmObj** %stackaddr$env-ref57752
%stackaddr$env-ref57753 = alloca %struct.ScmObj*, align 8
%k47527 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47562, i64 1)
store %struct.ScmObj* %k47527, %struct.ScmObj** %stackaddr$env-ref57753
%stackaddr$prim57754 = alloca %struct.ScmObj*, align 8
%_95k47529 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55829)
store volatile %struct.ScmObj* %_95k47529, %struct.ScmObj** %stackaddr$prim57754, align 8
%stackaddr$prim57755 = alloca %struct.ScmObj*, align 8
%current_45args55830 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55829)
store volatile %struct.ScmObj* %current_45args55830, %struct.ScmObj** %stackaddr$prim57755, align 8
%stackaddr$prim57756 = alloca %struct.ScmObj*, align 8
%anf_45bind47209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55830)
store volatile %struct.ScmObj* %anf_45bind47209, %struct.ScmObj** %stackaddr$prim57756, align 8
%stackaddr$prim57757 = alloca %struct.ScmObj*, align 8
%cpsargs47530 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47527, %struct.ScmObj* %args47078)
store volatile %struct.ScmObj* %cpsargs47530, %struct.ScmObj** %stackaddr$prim57757, align 8
%clofunc57758 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47209)
musttail call tailcc void %clofunc57758(%struct.ScmObj* %anf_45bind47209, %struct.ScmObj* %cpsargs47530)
ret void
}

define tailcc void @proc_clo$ae47534(%struct.ScmObj* %env$ae47534,%struct.ScmObj* %current_45args55837) {
%stackaddr$prim57759 = alloca %struct.ScmObj*, align 8
%k47531 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55837)
store volatile %struct.ScmObj* %k47531, %struct.ScmObj** %stackaddr$prim57759, align 8
%stackaddr$prim57760 = alloca %struct.ScmObj*, align 8
%current_45args55838 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55837)
store volatile %struct.ScmObj* %current_45args55838, %struct.ScmObj** %stackaddr$prim57760, align 8
%stackaddr$prim57761 = alloca %struct.ScmObj*, align 8
%yu47075 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55838)
store volatile %struct.ScmObj* %yu47075, %struct.ScmObj** %stackaddr$prim57761, align 8
%argslist55840$yu470750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57762 = alloca %struct.ScmObj*, align 8
%argslist55840$yu470751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu47075, %struct.ScmObj* %argslist55840$yu470750)
store volatile %struct.ScmObj* %argslist55840$yu470751, %struct.ScmObj** %stackaddr$prim57762, align 8
%stackaddr$prim57763 = alloca %struct.ScmObj*, align 8
%argslist55840$yu470752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47531, %struct.ScmObj* %argslist55840$yu470751)
store volatile %struct.ScmObj* %argslist55840$yu470752, %struct.ScmObj** %stackaddr$prim57763, align 8
%clofunc57764 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu47075)
musttail call tailcc void %clofunc57764(%struct.ScmObj* %yu47075, %struct.ScmObj* %argslist55840$yu470752)
ret void
}