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

@global$sym$ae4429249226 = private unnamed_addr constant [4 x i8] c"get\00", align 8
@global$str$ae4454349239 = private unnamed_addr constant [12 x i8] c"not promise\00", align 8
@global$sym$ae4409749382 = private unnamed_addr constant [8 x i8] c"promise\00", align 8
@global$sym$ae4395449422 = private unnamed_addr constant [8 x i8] c"promise\00", align 8

define ccc i32 @main() {
%mainenv48724 = call %struct.ScmObj* @const_init_null()
%mainargs48725 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv48724, %struct.ScmObj* %mainargs48725)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv48722,%struct.ScmObj* %mainargs48723) {
%stackaddr$makeclosure48726 = alloca %struct.ScmObj*, align 8
%fptrToInt48727 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40566 to i64
%ae40566 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48727)
store volatile %struct.ScmObj* %ae40566, %struct.ScmObj** %stackaddr$makeclosure48726, align 8
%ae40567 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48728 = alloca %struct.ScmObj*, align 8
%fptrToInt48729 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40568 to i64
%ae40568 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48729)
store volatile %struct.ScmObj* %ae40568, %struct.ScmObj** %stackaddr$makeclosure48728, align 8
%args48721$ae40566$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48730 = alloca %struct.ScmObj*, align 8
%args48721$ae40566$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40568, %struct.ScmObj* %args48721$ae40566$0)
store volatile %struct.ScmObj* %args48721$ae40566$1, %struct.ScmObj** %stackaddr$prim48730, align 8
%stackaddr$prim48731 = alloca %struct.ScmObj*, align 8
%args48721$ae40566$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40567, %struct.ScmObj* %args48721$ae40566$1)
store volatile %struct.ScmObj* %args48721$ae40566$2, %struct.ScmObj** %stackaddr$prim48731, align 8
%clofunc48732 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40566)
musttail call tailcc void %clofunc48732(%struct.ScmObj* %ae40566, %struct.ScmObj* %args48721$ae40566$2)
ret void
}

define tailcc void @proc_clo$ae40566(%struct.ScmObj* %env$ae40566,%struct.ScmObj* %current_45args48098) {
%stackaddr$prim48733 = alloca %struct.ScmObj*, align 8
%_95k40380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48098)
store volatile %struct.ScmObj* %_95k40380, %struct.ScmObj** %stackaddr$prim48733, align 8
%stackaddr$prim48734 = alloca %struct.ScmObj*, align 8
%current_45args48099 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48098)
store volatile %struct.ScmObj* %current_45args48099, %struct.ScmObj** %stackaddr$prim48734, align 8
%stackaddr$prim48735 = alloca %struct.ScmObj*, align 8
%anf_45bind40242 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48099)
store volatile %struct.ScmObj* %anf_45bind40242, %struct.ScmObj** %stackaddr$prim48735, align 8
%stackaddr$makeclosure48736 = alloca %struct.ScmObj*, align 8
%fptrToInt48737 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40581 to i64
%ae40581 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48737)
store volatile %struct.ScmObj* %ae40581, %struct.ScmObj** %stackaddr$makeclosure48736, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40581, %struct.ScmObj* %anf_45bind40242, i64 0)
%ae40582 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48738 = alloca %struct.ScmObj*, align 8
%fptrToInt48739 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40583 to i64
%ae40583 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48739)
store volatile %struct.ScmObj* %ae40583, %struct.ScmObj** %stackaddr$makeclosure48738, align 8
%args48716$ae40581$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48740 = alloca %struct.ScmObj*, align 8
%args48716$ae40581$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40583, %struct.ScmObj* %args48716$ae40581$0)
store volatile %struct.ScmObj* %args48716$ae40581$1, %struct.ScmObj** %stackaddr$prim48740, align 8
%stackaddr$prim48741 = alloca %struct.ScmObj*, align 8
%args48716$ae40581$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40582, %struct.ScmObj* %args48716$ae40581$1)
store volatile %struct.ScmObj* %args48716$ae40581$2, %struct.ScmObj** %stackaddr$prim48741, align 8
%clofunc48742 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40581)
musttail call tailcc void %clofunc48742(%struct.ScmObj* %ae40581, %struct.ScmObj* %args48716$ae40581$2)
ret void
}

define tailcc void @proc_clo$ae40581(%struct.ScmObj* %env$ae40581,%struct.ScmObj* %current_45args48101) {
%stackaddr$env-ref48743 = alloca %struct.ScmObj*, align 8
%anf_45bind40242 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40581, i64 0)
store %struct.ScmObj* %anf_45bind40242, %struct.ScmObj** %stackaddr$env-ref48743
%stackaddr$prim48744 = alloca %struct.ScmObj*, align 8
%_95k40381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48101)
store volatile %struct.ScmObj* %_95k40381, %struct.ScmObj** %stackaddr$prim48744, align 8
%stackaddr$prim48745 = alloca %struct.ScmObj*, align 8
%current_45args48102 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48101)
store volatile %struct.ScmObj* %current_45args48102, %struct.ScmObj** %stackaddr$prim48745, align 8
%stackaddr$prim48746 = alloca %struct.ScmObj*, align 8
%anf_45bind40246 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48102)
store volatile %struct.ScmObj* %anf_45bind40246, %struct.ScmObj** %stackaddr$prim48746, align 8
%stackaddr$makeclosure48747 = alloca %struct.ScmObj*, align 8
%fptrToInt48748 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40696 to i64
%ae40696 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48748)
store volatile %struct.ScmObj* %ae40696, %struct.ScmObj** %stackaddr$makeclosure48747, align 8
%args48695$anf_45bind40242$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48749 = alloca %struct.ScmObj*, align 8
%args48695$anf_45bind40242$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40246, %struct.ScmObj* %args48695$anf_45bind40242$0)
store volatile %struct.ScmObj* %args48695$anf_45bind40242$1, %struct.ScmObj** %stackaddr$prim48749, align 8
%stackaddr$prim48750 = alloca %struct.ScmObj*, align 8
%args48695$anf_45bind40242$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40696, %struct.ScmObj* %args48695$anf_45bind40242$1)
store volatile %struct.ScmObj* %args48695$anf_45bind40242$2, %struct.ScmObj** %stackaddr$prim48750, align 8
%clofunc48751 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40242)
musttail call tailcc void %clofunc48751(%struct.ScmObj* %anf_45bind40242, %struct.ScmObj* %args48695$anf_45bind40242$2)
ret void
}

define tailcc void @proc_clo$ae40696(%struct.ScmObj* %env$ae40696,%struct.ScmObj* %current_45args48104) {
%stackaddr$prim48752 = alloca %struct.ScmObj*, align 8
%_95k40382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48104)
store volatile %struct.ScmObj* %_95k40382, %struct.ScmObj** %stackaddr$prim48752, align 8
%stackaddr$prim48753 = alloca %struct.ScmObj*, align 8
%current_45args48105 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48104)
store volatile %struct.ScmObj* %current_45args48105, %struct.ScmObj** %stackaddr$prim48753, align 8
%stackaddr$prim48754 = alloca %struct.ScmObj*, align 8
%Ycmb40108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48105)
store volatile %struct.ScmObj* %Ycmb40108, %struct.ScmObj** %stackaddr$prim48754, align 8
%stackaddr$makeclosure48755 = alloca %struct.ScmObj*, align 8
%fptrToInt48756 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40698 to i64
%ae40698 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48756)
store volatile %struct.ScmObj* %ae40698, %struct.ScmObj** %stackaddr$makeclosure48755, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40698, %struct.ScmObj* %Ycmb40108, i64 0)
%ae40699 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48757 = alloca %struct.ScmObj*, align 8
%fptrToInt48758 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40700 to i64
%ae40700 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48758)
store volatile %struct.ScmObj* %ae40700, %struct.ScmObj** %stackaddr$makeclosure48757, align 8
%args48694$ae40698$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48759 = alloca %struct.ScmObj*, align 8
%args48694$ae40698$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40700, %struct.ScmObj* %args48694$ae40698$0)
store volatile %struct.ScmObj* %args48694$ae40698$1, %struct.ScmObj** %stackaddr$prim48759, align 8
%stackaddr$prim48760 = alloca %struct.ScmObj*, align 8
%args48694$ae40698$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40699, %struct.ScmObj* %args48694$ae40698$1)
store volatile %struct.ScmObj* %args48694$ae40698$2, %struct.ScmObj** %stackaddr$prim48760, align 8
%clofunc48761 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40698)
musttail call tailcc void %clofunc48761(%struct.ScmObj* %ae40698, %struct.ScmObj* %args48694$ae40698$2)
ret void
}

define tailcc void @proc_clo$ae40698(%struct.ScmObj* %env$ae40698,%struct.ScmObj* %current_45args48107) {
%stackaddr$env-ref48762 = alloca %struct.ScmObj*, align 8
%Ycmb40108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40698, i64 0)
store %struct.ScmObj* %Ycmb40108, %struct.ScmObj** %stackaddr$env-ref48762
%stackaddr$prim48763 = alloca %struct.ScmObj*, align 8
%_95k40383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48107)
store volatile %struct.ScmObj* %_95k40383, %struct.ScmObj** %stackaddr$prim48763, align 8
%stackaddr$prim48764 = alloca %struct.ScmObj*, align 8
%current_45args48108 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48107)
store volatile %struct.ScmObj* %current_45args48108, %struct.ScmObj** %stackaddr$prim48764, align 8
%stackaddr$prim48765 = alloca %struct.ScmObj*, align 8
%anf_45bind40251 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48108)
store volatile %struct.ScmObj* %anf_45bind40251, %struct.ScmObj** %stackaddr$prim48765, align 8
%stackaddr$makeclosure48766 = alloca %struct.ScmObj*, align 8
%fptrToInt48767 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40776 to i64
%ae40776 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48767)
store volatile %struct.ScmObj* %ae40776, %struct.ScmObj** %stackaddr$makeclosure48766, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40776, %struct.ScmObj* %Ycmb40108, i64 0)
%args48678$Ycmb40108$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48768 = alloca %struct.ScmObj*, align 8
%args48678$Ycmb40108$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40251, %struct.ScmObj* %args48678$Ycmb40108$0)
store volatile %struct.ScmObj* %args48678$Ycmb40108$1, %struct.ScmObj** %stackaddr$prim48768, align 8
%stackaddr$prim48769 = alloca %struct.ScmObj*, align 8
%args48678$Ycmb40108$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40776, %struct.ScmObj* %args48678$Ycmb40108$1)
store volatile %struct.ScmObj* %args48678$Ycmb40108$2, %struct.ScmObj** %stackaddr$prim48769, align 8
%clofunc48770 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40108)
musttail call tailcc void %clofunc48770(%struct.ScmObj* %Ycmb40108, %struct.ScmObj* %args48678$Ycmb40108$2)
ret void
}

define tailcc void @proc_clo$ae40776(%struct.ScmObj* %env$ae40776,%struct.ScmObj* %current_45args48110) {
%stackaddr$env-ref48771 = alloca %struct.ScmObj*, align 8
%Ycmb40108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40776, i64 0)
store %struct.ScmObj* %Ycmb40108, %struct.ScmObj** %stackaddr$env-ref48771
%stackaddr$prim48772 = alloca %struct.ScmObj*, align 8
%_95k40384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48110)
store volatile %struct.ScmObj* %_95k40384, %struct.ScmObj** %stackaddr$prim48772, align 8
%stackaddr$prim48773 = alloca %struct.ScmObj*, align 8
%current_45args48111 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48110)
store volatile %struct.ScmObj* %current_45args48111, %struct.ScmObj** %stackaddr$prim48773, align 8
%stackaddr$prim48774 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48111)
store volatile %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$prim48774, align 8
%stackaddr$makeclosure48775 = alloca %struct.ScmObj*, align 8
%fptrToInt48776 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40778 to i64
%ae40778 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48776)
store volatile %struct.ScmObj* %ae40778, %struct.ScmObj** %stackaddr$makeclosure48775, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40778, %struct.ScmObj* %_37foldr140129, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40778, %struct.ScmObj* %Ycmb40108, i64 1)
%ae40779 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48777 = alloca %struct.ScmObj*, align 8
%fptrToInt48778 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40780 to i64
%ae40780 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48778)
store volatile %struct.ScmObj* %ae40780, %struct.ScmObj** %stackaddr$makeclosure48777, align 8
%args48677$ae40778$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48779 = alloca %struct.ScmObj*, align 8
%args48677$ae40778$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40780, %struct.ScmObj* %args48677$ae40778$0)
store volatile %struct.ScmObj* %args48677$ae40778$1, %struct.ScmObj** %stackaddr$prim48779, align 8
%stackaddr$prim48780 = alloca %struct.ScmObj*, align 8
%args48677$ae40778$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40779, %struct.ScmObj* %args48677$ae40778$1)
store volatile %struct.ScmObj* %args48677$ae40778$2, %struct.ScmObj** %stackaddr$prim48780, align 8
%clofunc48781 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40778)
musttail call tailcc void %clofunc48781(%struct.ScmObj* %ae40778, %struct.ScmObj* %args48677$ae40778$2)
ret void
}

define tailcc void @proc_clo$ae40778(%struct.ScmObj* %env$ae40778,%struct.ScmObj* %current_45args48113) {
%stackaddr$env-ref48782 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40778, i64 0)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref48782
%stackaddr$env-ref48783 = alloca %struct.ScmObj*, align 8
%Ycmb40108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40778, i64 1)
store %struct.ScmObj* %Ycmb40108, %struct.ScmObj** %stackaddr$env-ref48783
%stackaddr$prim48784 = alloca %struct.ScmObj*, align 8
%_95k40385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48113)
store volatile %struct.ScmObj* %_95k40385, %struct.ScmObj** %stackaddr$prim48784, align 8
%stackaddr$prim48785 = alloca %struct.ScmObj*, align 8
%current_45args48114 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48113)
store volatile %struct.ScmObj* %current_45args48114, %struct.ScmObj** %stackaddr$prim48785, align 8
%stackaddr$prim48786 = alloca %struct.ScmObj*, align 8
%anf_45bind40257 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48114)
store volatile %struct.ScmObj* %anf_45bind40257, %struct.ScmObj** %stackaddr$prim48786, align 8
%stackaddr$makeclosure48787 = alloca %struct.ScmObj*, align 8
%fptrToInt48788 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40873 to i64
%ae40873 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48788)
store volatile %struct.ScmObj* %ae40873, %struct.ScmObj** %stackaddr$makeclosure48787, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40873, %struct.ScmObj* %_37foldr140129, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40873, %struct.ScmObj* %Ycmb40108, i64 1)
%args48658$Ycmb40108$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48789 = alloca %struct.ScmObj*, align 8
%args48658$Ycmb40108$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40257, %struct.ScmObj* %args48658$Ycmb40108$0)
store volatile %struct.ScmObj* %args48658$Ycmb40108$1, %struct.ScmObj** %stackaddr$prim48789, align 8
%stackaddr$prim48790 = alloca %struct.ScmObj*, align 8
%args48658$Ycmb40108$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40873, %struct.ScmObj* %args48658$Ycmb40108$1)
store volatile %struct.ScmObj* %args48658$Ycmb40108$2, %struct.ScmObj** %stackaddr$prim48790, align 8
%clofunc48791 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40108)
musttail call tailcc void %clofunc48791(%struct.ScmObj* %Ycmb40108, %struct.ScmObj* %args48658$Ycmb40108$2)
ret void
}

define tailcc void @proc_clo$ae40873(%struct.ScmObj* %env$ae40873,%struct.ScmObj* %current_45args48116) {
%stackaddr$env-ref48792 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40873, i64 0)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref48792
%stackaddr$env-ref48793 = alloca %struct.ScmObj*, align 8
%Ycmb40108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40873, i64 1)
store %struct.ScmObj* %Ycmb40108, %struct.ScmObj** %stackaddr$env-ref48793
%stackaddr$prim48794 = alloca %struct.ScmObj*, align 8
%_95k40386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48116)
store volatile %struct.ScmObj* %_95k40386, %struct.ScmObj** %stackaddr$prim48794, align 8
%stackaddr$prim48795 = alloca %struct.ScmObj*, align 8
%current_45args48117 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48116)
store volatile %struct.ScmObj* %current_45args48117, %struct.ScmObj** %stackaddr$prim48795, align 8
%stackaddr$prim48796 = alloca %struct.ScmObj*, align 8
%_37map140125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48117)
store volatile %struct.ScmObj* %_37map140125, %struct.ScmObj** %stackaddr$prim48796, align 8
%stackaddr$makeclosure48797 = alloca %struct.ScmObj*, align 8
%fptrToInt48798 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40875 to i64
%ae40875 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48798)
store volatile %struct.ScmObj* %ae40875, %struct.ScmObj** %stackaddr$makeclosure48797, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40875, %struct.ScmObj* %_37foldr140129, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40875, %struct.ScmObj* %_37map140125, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40875, %struct.ScmObj* %Ycmb40108, i64 2)
%ae40876 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48799 = alloca %struct.ScmObj*, align 8
%fptrToInt48800 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40877 to i64
%ae40877 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48800)
store volatile %struct.ScmObj* %ae40877, %struct.ScmObj** %stackaddr$makeclosure48799, align 8
%args48657$ae40875$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48801 = alloca %struct.ScmObj*, align 8
%args48657$ae40875$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40877, %struct.ScmObj* %args48657$ae40875$0)
store volatile %struct.ScmObj* %args48657$ae40875$1, %struct.ScmObj** %stackaddr$prim48801, align 8
%stackaddr$prim48802 = alloca %struct.ScmObj*, align 8
%args48657$ae40875$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40876, %struct.ScmObj* %args48657$ae40875$1)
store volatile %struct.ScmObj* %args48657$ae40875$2, %struct.ScmObj** %stackaddr$prim48802, align 8
%clofunc48803 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40875)
musttail call tailcc void %clofunc48803(%struct.ScmObj* %ae40875, %struct.ScmObj* %args48657$ae40875$2)
ret void
}

define tailcc void @proc_clo$ae40875(%struct.ScmObj* %env$ae40875,%struct.ScmObj* %current_45args48119) {
%stackaddr$env-ref48804 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40875, i64 0)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref48804
%stackaddr$env-ref48805 = alloca %struct.ScmObj*, align 8
%_37map140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40875, i64 1)
store %struct.ScmObj* %_37map140125, %struct.ScmObj** %stackaddr$env-ref48805
%stackaddr$env-ref48806 = alloca %struct.ScmObj*, align 8
%Ycmb40108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40875, i64 2)
store %struct.ScmObj* %Ycmb40108, %struct.ScmObj** %stackaddr$env-ref48806
%stackaddr$prim48807 = alloca %struct.ScmObj*, align 8
%_95k40387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48119)
store volatile %struct.ScmObj* %_95k40387, %struct.ScmObj** %stackaddr$prim48807, align 8
%stackaddr$prim48808 = alloca %struct.ScmObj*, align 8
%current_45args48120 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48119)
store volatile %struct.ScmObj* %current_45args48120, %struct.ScmObj** %stackaddr$prim48808, align 8
%stackaddr$prim48809 = alloca %struct.ScmObj*, align 8
%anf_45bind40264 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48120)
store volatile %struct.ScmObj* %anf_45bind40264, %struct.ScmObj** %stackaddr$prim48809, align 8
%stackaddr$makeclosure48810 = alloca %struct.ScmObj*, align 8
%fptrToInt48811 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41023 to i64
%ae41023 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48811)
store volatile %struct.ScmObj* %ae41023, %struct.ScmObj** %stackaddr$makeclosure48810, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41023, %struct.ScmObj* %_37foldr140129, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41023, %struct.ScmObj* %_37map140125, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41023, %struct.ScmObj* %Ycmb40108, i64 2)
%args48641$Ycmb40108$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48812 = alloca %struct.ScmObj*, align 8
%args48641$Ycmb40108$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40264, %struct.ScmObj* %args48641$Ycmb40108$0)
store volatile %struct.ScmObj* %args48641$Ycmb40108$1, %struct.ScmObj** %stackaddr$prim48812, align 8
%stackaddr$prim48813 = alloca %struct.ScmObj*, align 8
%args48641$Ycmb40108$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41023, %struct.ScmObj* %args48641$Ycmb40108$1)
store volatile %struct.ScmObj* %args48641$Ycmb40108$2, %struct.ScmObj** %stackaddr$prim48813, align 8
%clofunc48814 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40108)
musttail call tailcc void %clofunc48814(%struct.ScmObj* %Ycmb40108, %struct.ScmObj* %args48641$Ycmb40108$2)
ret void
}

define tailcc void @proc_clo$ae41023(%struct.ScmObj* %env$ae41023,%struct.ScmObj* %current_45args48122) {
%stackaddr$env-ref48815 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41023, i64 0)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref48815
%stackaddr$env-ref48816 = alloca %struct.ScmObj*, align 8
%_37map140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41023, i64 1)
store %struct.ScmObj* %_37map140125, %struct.ScmObj** %stackaddr$env-ref48816
%stackaddr$env-ref48817 = alloca %struct.ScmObj*, align 8
%Ycmb40108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41023, i64 2)
store %struct.ScmObj* %Ycmb40108, %struct.ScmObj** %stackaddr$env-ref48817
%stackaddr$prim48818 = alloca %struct.ScmObj*, align 8
%_95k40388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48122)
store volatile %struct.ScmObj* %_95k40388, %struct.ScmObj** %stackaddr$prim48818, align 8
%stackaddr$prim48819 = alloca %struct.ScmObj*, align 8
%current_45args48123 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48122)
store volatile %struct.ScmObj* %current_45args48123, %struct.ScmObj** %stackaddr$prim48819, align 8
%stackaddr$prim48820 = alloca %struct.ScmObj*, align 8
%_37take40121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48123)
store volatile %struct.ScmObj* %_37take40121, %struct.ScmObj** %stackaddr$prim48820, align 8
%stackaddr$makeclosure48821 = alloca %struct.ScmObj*, align 8
%fptrToInt48822 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41025 to i64
%ae41025 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48822)
store volatile %struct.ScmObj* %ae41025, %struct.ScmObj** %stackaddr$makeclosure48821, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41025, %struct.ScmObj* %_37take40121, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41025, %struct.ScmObj* %_37foldr140129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41025, %struct.ScmObj* %_37map140125, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41025, %struct.ScmObj* %Ycmb40108, i64 3)
%ae41026 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48823 = alloca %struct.ScmObj*, align 8
%fptrToInt48824 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41027 to i64
%ae41027 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48824)
store volatile %struct.ScmObj* %ae41027, %struct.ScmObj** %stackaddr$makeclosure48823, align 8
%args48640$ae41025$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48825 = alloca %struct.ScmObj*, align 8
%args48640$ae41025$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41027, %struct.ScmObj* %args48640$ae41025$0)
store volatile %struct.ScmObj* %args48640$ae41025$1, %struct.ScmObj** %stackaddr$prim48825, align 8
%stackaddr$prim48826 = alloca %struct.ScmObj*, align 8
%args48640$ae41025$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41026, %struct.ScmObj* %args48640$ae41025$1)
store volatile %struct.ScmObj* %args48640$ae41025$2, %struct.ScmObj** %stackaddr$prim48826, align 8
%clofunc48827 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41025)
musttail call tailcc void %clofunc48827(%struct.ScmObj* %ae41025, %struct.ScmObj* %args48640$ae41025$2)
ret void
}

define tailcc void @proc_clo$ae41025(%struct.ScmObj* %env$ae41025,%struct.ScmObj* %current_45args48125) {
%stackaddr$env-ref48828 = alloca %struct.ScmObj*, align 8
%_37take40121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41025, i64 0)
store %struct.ScmObj* %_37take40121, %struct.ScmObj** %stackaddr$env-ref48828
%stackaddr$env-ref48829 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41025, i64 1)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref48829
%stackaddr$env-ref48830 = alloca %struct.ScmObj*, align 8
%_37map140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41025, i64 2)
store %struct.ScmObj* %_37map140125, %struct.ScmObj** %stackaddr$env-ref48830
%stackaddr$env-ref48831 = alloca %struct.ScmObj*, align 8
%Ycmb40108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41025, i64 3)
store %struct.ScmObj* %Ycmb40108, %struct.ScmObj** %stackaddr$env-ref48831
%stackaddr$prim48832 = alloca %struct.ScmObj*, align 8
%_95k40389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48125)
store volatile %struct.ScmObj* %_95k40389, %struct.ScmObj** %stackaddr$prim48832, align 8
%stackaddr$prim48833 = alloca %struct.ScmObj*, align 8
%current_45args48126 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48125)
store volatile %struct.ScmObj* %current_45args48126, %struct.ScmObj** %stackaddr$prim48833, align 8
%stackaddr$prim48834 = alloca %struct.ScmObj*, align 8
%anf_45bind40268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48126)
store volatile %struct.ScmObj* %anf_45bind40268, %struct.ScmObj** %stackaddr$prim48834, align 8
%stackaddr$makeclosure48835 = alloca %struct.ScmObj*, align 8
%fptrToInt48836 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41106 to i64
%ae41106 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48836)
store volatile %struct.ScmObj* %ae41106, %struct.ScmObj** %stackaddr$makeclosure48835, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41106, %struct.ScmObj* %_37take40121, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41106, %struct.ScmObj* %_37foldr140129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41106, %struct.ScmObj* %_37map140125, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41106, %struct.ScmObj* %Ycmb40108, i64 3)
%args48626$Ycmb40108$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48837 = alloca %struct.ScmObj*, align 8
%args48626$Ycmb40108$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40268, %struct.ScmObj* %args48626$Ycmb40108$0)
store volatile %struct.ScmObj* %args48626$Ycmb40108$1, %struct.ScmObj** %stackaddr$prim48837, align 8
%stackaddr$prim48838 = alloca %struct.ScmObj*, align 8
%args48626$Ycmb40108$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41106, %struct.ScmObj* %args48626$Ycmb40108$1)
store volatile %struct.ScmObj* %args48626$Ycmb40108$2, %struct.ScmObj** %stackaddr$prim48838, align 8
%clofunc48839 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40108)
musttail call tailcc void %clofunc48839(%struct.ScmObj* %Ycmb40108, %struct.ScmObj* %args48626$Ycmb40108$2)
ret void
}

define tailcc void @proc_clo$ae41106(%struct.ScmObj* %env$ae41106,%struct.ScmObj* %current_45args48128) {
%stackaddr$env-ref48840 = alloca %struct.ScmObj*, align 8
%_37take40121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41106, i64 0)
store %struct.ScmObj* %_37take40121, %struct.ScmObj** %stackaddr$env-ref48840
%stackaddr$env-ref48841 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41106, i64 1)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref48841
%stackaddr$env-ref48842 = alloca %struct.ScmObj*, align 8
%_37map140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41106, i64 2)
store %struct.ScmObj* %_37map140125, %struct.ScmObj** %stackaddr$env-ref48842
%stackaddr$env-ref48843 = alloca %struct.ScmObj*, align 8
%Ycmb40108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41106, i64 3)
store %struct.ScmObj* %Ycmb40108, %struct.ScmObj** %stackaddr$env-ref48843
%stackaddr$prim48844 = alloca %struct.ScmObj*, align 8
%_95k40390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48128)
store volatile %struct.ScmObj* %_95k40390, %struct.ScmObj** %stackaddr$prim48844, align 8
%stackaddr$prim48845 = alloca %struct.ScmObj*, align 8
%current_45args48129 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48128)
store volatile %struct.ScmObj* %current_45args48129, %struct.ScmObj** %stackaddr$prim48845, align 8
%stackaddr$prim48846 = alloca %struct.ScmObj*, align 8
%_37length40118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48129)
store volatile %struct.ScmObj* %_37length40118, %struct.ScmObj** %stackaddr$prim48846, align 8
%stackaddr$makeclosure48847 = alloca %struct.ScmObj*, align 8
%fptrToInt48848 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41108 to i64
%ae41108 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt48848)
store volatile %struct.ScmObj* %ae41108, %struct.ScmObj** %stackaddr$makeclosure48847, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41108, %struct.ScmObj* %_37take40121, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41108, %struct.ScmObj* %_37length40118, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41108, %struct.ScmObj* %_37foldr140129, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41108, %struct.ScmObj* %_37map140125, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41108, %struct.ScmObj* %Ycmb40108, i64 4)
%ae41109 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48849 = alloca %struct.ScmObj*, align 8
%fptrToInt48850 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41110 to i64
%ae41110 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48850)
store volatile %struct.ScmObj* %ae41110, %struct.ScmObj** %stackaddr$makeclosure48849, align 8
%args48625$ae41108$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48851 = alloca %struct.ScmObj*, align 8
%args48625$ae41108$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41110, %struct.ScmObj* %args48625$ae41108$0)
store volatile %struct.ScmObj* %args48625$ae41108$1, %struct.ScmObj** %stackaddr$prim48851, align 8
%stackaddr$prim48852 = alloca %struct.ScmObj*, align 8
%args48625$ae41108$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41109, %struct.ScmObj* %args48625$ae41108$1)
store volatile %struct.ScmObj* %args48625$ae41108$2, %struct.ScmObj** %stackaddr$prim48852, align 8
%clofunc48853 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41108)
musttail call tailcc void %clofunc48853(%struct.ScmObj* %ae41108, %struct.ScmObj* %args48625$ae41108$2)
ret void
}

define tailcc void @proc_clo$ae41108(%struct.ScmObj* %env$ae41108,%struct.ScmObj* %current_45args48131) {
%stackaddr$env-ref48854 = alloca %struct.ScmObj*, align 8
%_37take40121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41108, i64 0)
store %struct.ScmObj* %_37take40121, %struct.ScmObj** %stackaddr$env-ref48854
%stackaddr$env-ref48855 = alloca %struct.ScmObj*, align 8
%_37length40118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41108, i64 1)
store %struct.ScmObj* %_37length40118, %struct.ScmObj** %stackaddr$env-ref48855
%stackaddr$env-ref48856 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41108, i64 2)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref48856
%stackaddr$env-ref48857 = alloca %struct.ScmObj*, align 8
%_37map140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41108, i64 3)
store %struct.ScmObj* %_37map140125, %struct.ScmObj** %stackaddr$env-ref48857
%stackaddr$env-ref48858 = alloca %struct.ScmObj*, align 8
%Ycmb40108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41108, i64 4)
store %struct.ScmObj* %Ycmb40108, %struct.ScmObj** %stackaddr$env-ref48858
%stackaddr$prim48859 = alloca %struct.ScmObj*, align 8
%_95k40391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48131)
store volatile %struct.ScmObj* %_95k40391, %struct.ScmObj** %stackaddr$prim48859, align 8
%stackaddr$prim48860 = alloca %struct.ScmObj*, align 8
%current_45args48132 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48131)
store volatile %struct.ScmObj* %current_45args48132, %struct.ScmObj** %stackaddr$prim48860, align 8
%stackaddr$prim48861 = alloca %struct.ScmObj*, align 8
%anf_45bind40273 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48132)
store volatile %struct.ScmObj* %anf_45bind40273, %struct.ScmObj** %stackaddr$prim48861, align 8
%stackaddr$makeclosure48862 = alloca %struct.ScmObj*, align 8
%fptrToInt48863 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41185 to i64
%ae41185 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt48863)
store volatile %struct.ScmObj* %ae41185, %struct.ScmObj** %stackaddr$makeclosure48862, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41185, %struct.ScmObj* %_37take40121, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41185, %struct.ScmObj* %_37length40118, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41185, %struct.ScmObj* %_37foldr140129, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41185, %struct.ScmObj* %_37map140125, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41185, %struct.ScmObj* %Ycmb40108, i64 4)
%args48609$Ycmb40108$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48864 = alloca %struct.ScmObj*, align 8
%args48609$Ycmb40108$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40273, %struct.ScmObj* %args48609$Ycmb40108$0)
store volatile %struct.ScmObj* %args48609$Ycmb40108$1, %struct.ScmObj** %stackaddr$prim48864, align 8
%stackaddr$prim48865 = alloca %struct.ScmObj*, align 8
%args48609$Ycmb40108$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41185, %struct.ScmObj* %args48609$Ycmb40108$1)
store volatile %struct.ScmObj* %args48609$Ycmb40108$2, %struct.ScmObj** %stackaddr$prim48865, align 8
%clofunc48866 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40108)
musttail call tailcc void %clofunc48866(%struct.ScmObj* %Ycmb40108, %struct.ScmObj* %args48609$Ycmb40108$2)
ret void
}

define tailcc void @proc_clo$ae41185(%struct.ScmObj* %env$ae41185,%struct.ScmObj* %current_45args48134) {
%stackaddr$env-ref48867 = alloca %struct.ScmObj*, align 8
%_37take40121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41185, i64 0)
store %struct.ScmObj* %_37take40121, %struct.ScmObj** %stackaddr$env-ref48867
%stackaddr$env-ref48868 = alloca %struct.ScmObj*, align 8
%_37length40118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41185, i64 1)
store %struct.ScmObj* %_37length40118, %struct.ScmObj** %stackaddr$env-ref48868
%stackaddr$env-ref48869 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41185, i64 2)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref48869
%stackaddr$env-ref48870 = alloca %struct.ScmObj*, align 8
%_37map140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41185, i64 3)
store %struct.ScmObj* %_37map140125, %struct.ScmObj** %stackaddr$env-ref48870
%stackaddr$env-ref48871 = alloca %struct.ScmObj*, align 8
%Ycmb40108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41185, i64 4)
store %struct.ScmObj* %Ycmb40108, %struct.ScmObj** %stackaddr$env-ref48871
%stackaddr$prim48872 = alloca %struct.ScmObj*, align 8
%_95k40392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48134)
store volatile %struct.ScmObj* %_95k40392, %struct.ScmObj** %stackaddr$prim48872, align 8
%stackaddr$prim48873 = alloca %struct.ScmObj*, align 8
%current_45args48135 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48134)
store volatile %struct.ScmObj* %current_45args48135, %struct.ScmObj** %stackaddr$prim48873, align 8
%stackaddr$prim48874 = alloca %struct.ScmObj*, align 8
%_37foldl140113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48135)
store volatile %struct.ScmObj* %_37foldl140113, %struct.ScmObj** %stackaddr$prim48874, align 8
%stackaddr$makeclosure48875 = alloca %struct.ScmObj*, align 8
%fptrToInt48876 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41187 to i64
%ae41187 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt48876)
store volatile %struct.ScmObj* %ae41187, %struct.ScmObj** %stackaddr$makeclosure48875, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41187, %struct.ScmObj* %_37foldr140129, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41187, %struct.ScmObj* %_37foldl140113, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41187, %struct.ScmObj* %_37take40121, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41187, %struct.ScmObj* %_37length40118, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41187, %struct.ScmObj* %_37map140125, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41187, %struct.ScmObj* %Ycmb40108, i64 5)
%ae41188 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48877 = alloca %struct.ScmObj*, align 8
%fptrToInt48878 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41189 to i64
%ae41189 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48878)
store volatile %struct.ScmObj* %ae41189, %struct.ScmObj** %stackaddr$makeclosure48877, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41189, %struct.ScmObj* %_37foldl140113, i64 0)
%args48608$ae41187$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48879 = alloca %struct.ScmObj*, align 8
%args48608$ae41187$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41189, %struct.ScmObj* %args48608$ae41187$0)
store volatile %struct.ScmObj* %args48608$ae41187$1, %struct.ScmObj** %stackaddr$prim48879, align 8
%stackaddr$prim48880 = alloca %struct.ScmObj*, align 8
%args48608$ae41187$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41188, %struct.ScmObj* %args48608$ae41187$1)
store volatile %struct.ScmObj* %args48608$ae41187$2, %struct.ScmObj** %stackaddr$prim48880, align 8
%clofunc48881 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41187)
musttail call tailcc void %clofunc48881(%struct.ScmObj* %ae41187, %struct.ScmObj* %args48608$ae41187$2)
ret void
}

define tailcc void @proc_clo$ae41187(%struct.ScmObj* %env$ae41187,%struct.ScmObj* %current_45args48137) {
%stackaddr$env-ref48882 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41187, i64 0)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref48882
%stackaddr$env-ref48883 = alloca %struct.ScmObj*, align 8
%_37foldl140113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41187, i64 1)
store %struct.ScmObj* %_37foldl140113, %struct.ScmObj** %stackaddr$env-ref48883
%stackaddr$env-ref48884 = alloca %struct.ScmObj*, align 8
%_37take40121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41187, i64 2)
store %struct.ScmObj* %_37take40121, %struct.ScmObj** %stackaddr$env-ref48884
%stackaddr$env-ref48885 = alloca %struct.ScmObj*, align 8
%_37length40118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41187, i64 3)
store %struct.ScmObj* %_37length40118, %struct.ScmObj** %stackaddr$env-ref48885
%stackaddr$env-ref48886 = alloca %struct.ScmObj*, align 8
%_37map140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41187, i64 4)
store %struct.ScmObj* %_37map140125, %struct.ScmObj** %stackaddr$env-ref48886
%stackaddr$env-ref48887 = alloca %struct.ScmObj*, align 8
%Ycmb40108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41187, i64 5)
store %struct.ScmObj* %Ycmb40108, %struct.ScmObj** %stackaddr$env-ref48887
%stackaddr$prim48888 = alloca %struct.ScmObj*, align 8
%_95k40393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48137)
store volatile %struct.ScmObj* %_95k40393, %struct.ScmObj** %stackaddr$prim48888, align 8
%stackaddr$prim48889 = alloca %struct.ScmObj*, align 8
%current_45args48138 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48137)
store volatile %struct.ScmObj* %current_45args48138, %struct.ScmObj** %stackaddr$prim48889, align 8
%stackaddr$prim48890 = alloca %struct.ScmObj*, align 8
%_37last40151 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48138)
store volatile %struct.ScmObj* %_37last40151, %struct.ScmObj** %stackaddr$prim48890, align 8
%stackaddr$makeclosure48891 = alloca %struct.ScmObj*, align 8
%fptrToInt48892 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41241 to i64
%ae41241 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt48892)
store volatile %struct.ScmObj* %ae41241, %struct.ScmObj** %stackaddr$makeclosure48891, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41241, %struct.ScmObj* %_37foldr140129, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41241, %struct.ScmObj* %_37foldl140113, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41241, %struct.ScmObj* %_37last40151, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41241, %struct.ScmObj* %_37map140125, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41241, %struct.ScmObj* %Ycmb40108, i64 4)
%ae41242 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48893 = alloca %struct.ScmObj*, align 8
%fptrToInt48894 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41243 to i64
%ae41243 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48894)
store volatile %struct.ScmObj* %ae41243, %struct.ScmObj** %stackaddr$makeclosure48893, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41243, %struct.ScmObj* %_37take40121, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41243, %struct.ScmObj* %_37length40118, i64 1)
%args48594$ae41241$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48895 = alloca %struct.ScmObj*, align 8
%args48594$ae41241$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41243, %struct.ScmObj* %args48594$ae41241$0)
store volatile %struct.ScmObj* %args48594$ae41241$1, %struct.ScmObj** %stackaddr$prim48895, align 8
%stackaddr$prim48896 = alloca %struct.ScmObj*, align 8
%args48594$ae41241$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41242, %struct.ScmObj* %args48594$ae41241$1)
store volatile %struct.ScmObj* %args48594$ae41241$2, %struct.ScmObj** %stackaddr$prim48896, align 8
%clofunc48897 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41241)
musttail call tailcc void %clofunc48897(%struct.ScmObj* %ae41241, %struct.ScmObj* %args48594$ae41241$2)
ret void
}

define tailcc void @proc_clo$ae41241(%struct.ScmObj* %env$ae41241,%struct.ScmObj* %current_45args48140) {
%stackaddr$env-ref48898 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41241, i64 0)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref48898
%stackaddr$env-ref48899 = alloca %struct.ScmObj*, align 8
%_37foldl140113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41241, i64 1)
store %struct.ScmObj* %_37foldl140113, %struct.ScmObj** %stackaddr$env-ref48899
%stackaddr$env-ref48900 = alloca %struct.ScmObj*, align 8
%_37last40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41241, i64 2)
store %struct.ScmObj* %_37last40151, %struct.ScmObj** %stackaddr$env-ref48900
%stackaddr$env-ref48901 = alloca %struct.ScmObj*, align 8
%_37map140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41241, i64 3)
store %struct.ScmObj* %_37map140125, %struct.ScmObj** %stackaddr$env-ref48901
%stackaddr$env-ref48902 = alloca %struct.ScmObj*, align 8
%Ycmb40108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41241, i64 4)
store %struct.ScmObj* %Ycmb40108, %struct.ScmObj** %stackaddr$env-ref48902
%stackaddr$prim48903 = alloca %struct.ScmObj*, align 8
%_95k40394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48140)
store volatile %struct.ScmObj* %_95k40394, %struct.ScmObj** %stackaddr$prim48903, align 8
%stackaddr$prim48904 = alloca %struct.ScmObj*, align 8
%current_45args48141 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48140)
store volatile %struct.ScmObj* %current_45args48141, %struct.ScmObj** %stackaddr$prim48904, align 8
%stackaddr$prim48905 = alloca %struct.ScmObj*, align 8
%_37drop_45right40148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48141)
store volatile %struct.ScmObj* %_37drop_45right40148, %struct.ScmObj** %stackaddr$prim48905, align 8
%stackaddr$makeclosure48906 = alloca %struct.ScmObj*, align 8
%fptrToInt48907 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41271 to i64
%ae41271 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt48907)
store volatile %struct.ScmObj* %ae41271, %struct.ScmObj** %stackaddr$makeclosure48906, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41271, %struct.ScmObj* %_37foldr140129, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41271, %struct.ScmObj* %_37foldl140113, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41271, %struct.ScmObj* %_37last40151, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41271, %struct.ScmObj* %_37drop_45right40148, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41271, %struct.ScmObj* %Ycmb40108, i64 4)
%ae41272 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48908 = alloca %struct.ScmObj*, align 8
%fptrToInt48909 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41273 to i64
%ae41273 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48909)
store volatile %struct.ScmObj* %ae41273, %struct.ScmObj** %stackaddr$makeclosure48908, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41273, %struct.ScmObj* %_37foldr140129, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41273, %struct.ScmObj* %_37map140125, i64 1)
%args48584$ae41271$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48910 = alloca %struct.ScmObj*, align 8
%args48584$ae41271$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41273, %struct.ScmObj* %args48584$ae41271$0)
store volatile %struct.ScmObj* %args48584$ae41271$1, %struct.ScmObj** %stackaddr$prim48910, align 8
%stackaddr$prim48911 = alloca %struct.ScmObj*, align 8
%args48584$ae41271$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41272, %struct.ScmObj* %args48584$ae41271$1)
store volatile %struct.ScmObj* %args48584$ae41271$2, %struct.ScmObj** %stackaddr$prim48911, align 8
%clofunc48912 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41271)
musttail call tailcc void %clofunc48912(%struct.ScmObj* %ae41271, %struct.ScmObj* %args48584$ae41271$2)
ret void
}

define tailcc void @proc_clo$ae41271(%struct.ScmObj* %env$ae41271,%struct.ScmObj* %current_45args48143) {
%stackaddr$env-ref48913 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41271, i64 0)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref48913
%stackaddr$env-ref48914 = alloca %struct.ScmObj*, align 8
%_37foldl140113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41271, i64 1)
store %struct.ScmObj* %_37foldl140113, %struct.ScmObj** %stackaddr$env-ref48914
%stackaddr$env-ref48915 = alloca %struct.ScmObj*, align 8
%_37last40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41271, i64 2)
store %struct.ScmObj* %_37last40151, %struct.ScmObj** %stackaddr$env-ref48915
%stackaddr$env-ref48916 = alloca %struct.ScmObj*, align 8
%_37drop_45right40148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41271, i64 3)
store %struct.ScmObj* %_37drop_45right40148, %struct.ScmObj** %stackaddr$env-ref48916
%stackaddr$env-ref48917 = alloca %struct.ScmObj*, align 8
%Ycmb40108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41271, i64 4)
store %struct.ScmObj* %Ycmb40108, %struct.ScmObj** %stackaddr$env-ref48917
%stackaddr$prim48918 = alloca %struct.ScmObj*, align 8
%_95k40395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48143)
store volatile %struct.ScmObj* %_95k40395, %struct.ScmObj** %stackaddr$prim48918, align 8
%stackaddr$prim48919 = alloca %struct.ScmObj*, align 8
%current_45args48144 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48143)
store volatile %struct.ScmObj* %current_45args48144, %struct.ScmObj** %stackaddr$prim48919, align 8
%stackaddr$prim48920 = alloca %struct.ScmObj*, align 8
%anf_45bind40289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48144)
store volatile %struct.ScmObj* %anf_45bind40289, %struct.ScmObj** %stackaddr$prim48920, align 8
%stackaddr$makeclosure48921 = alloca %struct.ScmObj*, align 8
%fptrToInt48922 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41655 to i64
%ae41655 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt48922)
store volatile %struct.ScmObj* %ae41655, %struct.ScmObj** %stackaddr$makeclosure48921, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41655, %struct.ScmObj* %_37foldr140129, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41655, %struct.ScmObj* %_37foldl140113, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41655, %struct.ScmObj* %_37last40151, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41655, %struct.ScmObj* %_37drop_45right40148, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41655, %struct.ScmObj* %Ycmb40108, i64 4)
%args48524$Ycmb40108$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48923 = alloca %struct.ScmObj*, align 8
%args48524$Ycmb40108$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40289, %struct.ScmObj* %args48524$Ycmb40108$0)
store volatile %struct.ScmObj* %args48524$Ycmb40108$1, %struct.ScmObj** %stackaddr$prim48923, align 8
%stackaddr$prim48924 = alloca %struct.ScmObj*, align 8
%args48524$Ycmb40108$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41655, %struct.ScmObj* %args48524$Ycmb40108$1)
store volatile %struct.ScmObj* %args48524$Ycmb40108$2, %struct.ScmObj** %stackaddr$prim48924, align 8
%clofunc48925 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40108)
musttail call tailcc void %clofunc48925(%struct.ScmObj* %Ycmb40108, %struct.ScmObj* %args48524$Ycmb40108$2)
ret void
}

define tailcc void @proc_clo$ae41655(%struct.ScmObj* %env$ae41655,%struct.ScmObj* %current_45args48146) {
%stackaddr$env-ref48926 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41655, i64 0)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref48926
%stackaddr$env-ref48927 = alloca %struct.ScmObj*, align 8
%_37foldl140113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41655, i64 1)
store %struct.ScmObj* %_37foldl140113, %struct.ScmObj** %stackaddr$env-ref48927
%stackaddr$env-ref48928 = alloca %struct.ScmObj*, align 8
%_37last40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41655, i64 2)
store %struct.ScmObj* %_37last40151, %struct.ScmObj** %stackaddr$env-ref48928
%stackaddr$env-ref48929 = alloca %struct.ScmObj*, align 8
%_37drop_45right40148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41655, i64 3)
store %struct.ScmObj* %_37drop_45right40148, %struct.ScmObj** %stackaddr$env-ref48929
%stackaddr$env-ref48930 = alloca %struct.ScmObj*, align 8
%Ycmb40108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41655, i64 4)
store %struct.ScmObj* %Ycmb40108, %struct.ScmObj** %stackaddr$env-ref48930
%stackaddr$prim48931 = alloca %struct.ScmObj*, align 8
%_95k40396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48146)
store volatile %struct.ScmObj* %_95k40396, %struct.ScmObj** %stackaddr$prim48931, align 8
%stackaddr$prim48932 = alloca %struct.ScmObj*, align 8
%current_45args48147 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48146)
store volatile %struct.ScmObj* %current_45args48147, %struct.ScmObj** %stackaddr$prim48932, align 8
%stackaddr$prim48933 = alloca %struct.ScmObj*, align 8
%_37foldr40134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48147)
store volatile %struct.ScmObj* %_37foldr40134, %struct.ScmObj** %stackaddr$prim48933, align 8
%stackaddr$makeclosure48934 = alloca %struct.ScmObj*, align 8
%fptrToInt48935 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41657 to i64
%ae41657 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt48935)
store volatile %struct.ScmObj* %ae41657, %struct.ScmObj** %stackaddr$makeclosure48934, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41657, %struct.ScmObj* %_37foldr140129, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41657, %struct.ScmObj* %_37foldl140113, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41657, %struct.ScmObj* %_37last40151, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41657, %struct.ScmObj* %_37foldr40134, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41657, %struct.ScmObj* %_37drop_45right40148, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41657, %struct.ScmObj* %Ycmb40108, i64 5)
%ae41658 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48936 = alloca %struct.ScmObj*, align 8
%fptrToInt48937 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41659 to i64
%ae41659 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48937)
store volatile %struct.ScmObj* %ae41659, %struct.ScmObj** %stackaddr$makeclosure48936, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41659, %struct.ScmObj* %_37foldr140129, i64 0)
%args48523$ae41657$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48938 = alloca %struct.ScmObj*, align 8
%args48523$ae41657$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41659, %struct.ScmObj* %args48523$ae41657$0)
store volatile %struct.ScmObj* %args48523$ae41657$1, %struct.ScmObj** %stackaddr$prim48938, align 8
%stackaddr$prim48939 = alloca %struct.ScmObj*, align 8
%args48523$ae41657$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41658, %struct.ScmObj* %args48523$ae41657$1)
store volatile %struct.ScmObj* %args48523$ae41657$2, %struct.ScmObj** %stackaddr$prim48939, align 8
%clofunc48940 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41657)
musttail call tailcc void %clofunc48940(%struct.ScmObj* %ae41657, %struct.ScmObj* %args48523$ae41657$2)
ret void
}

define tailcc void @proc_clo$ae41657(%struct.ScmObj* %env$ae41657,%struct.ScmObj* %current_45args48149) {
%stackaddr$env-ref48941 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41657, i64 0)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref48941
%stackaddr$env-ref48942 = alloca %struct.ScmObj*, align 8
%_37foldl140113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41657, i64 1)
store %struct.ScmObj* %_37foldl140113, %struct.ScmObj** %stackaddr$env-ref48942
%stackaddr$env-ref48943 = alloca %struct.ScmObj*, align 8
%_37last40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41657, i64 2)
store %struct.ScmObj* %_37last40151, %struct.ScmObj** %stackaddr$env-ref48943
%stackaddr$env-ref48944 = alloca %struct.ScmObj*, align 8
%_37foldr40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41657, i64 3)
store %struct.ScmObj* %_37foldr40134, %struct.ScmObj** %stackaddr$env-ref48944
%stackaddr$env-ref48945 = alloca %struct.ScmObj*, align 8
%_37drop_45right40148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41657, i64 4)
store %struct.ScmObj* %_37drop_45right40148, %struct.ScmObj** %stackaddr$env-ref48945
%stackaddr$env-ref48946 = alloca %struct.ScmObj*, align 8
%Ycmb40108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41657, i64 5)
store %struct.ScmObj* %Ycmb40108, %struct.ScmObj** %stackaddr$env-ref48946
%stackaddr$prim48947 = alloca %struct.ScmObj*, align 8
%_95k40397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48149)
store volatile %struct.ScmObj* %_95k40397, %struct.ScmObj** %stackaddr$prim48947, align 8
%stackaddr$prim48948 = alloca %struct.ScmObj*, align 8
%current_45args48150 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48149)
store volatile %struct.ScmObj* %current_45args48150, %struct.ScmObj** %stackaddr$prim48948, align 8
%stackaddr$prim48949 = alloca %struct.ScmObj*, align 8
%_37map140160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48150)
store volatile %struct.ScmObj* %_37map140160, %struct.ScmObj** %stackaddr$prim48949, align 8
%stackaddr$makeclosure48950 = alloca %struct.ScmObj*, align 8
%fptrToInt48951 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41734 to i64
%ae41734 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt48951)
store volatile %struct.ScmObj* %ae41734, %struct.ScmObj** %stackaddr$makeclosure48950, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41734, %struct.ScmObj* %_37foldr140129, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41734, %struct.ScmObj* %_37foldl140113, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41734, %struct.ScmObj* %_37foldr40134, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41734, %struct.ScmObj* %_37map140160, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41734, %struct.ScmObj* %Ycmb40108, i64 4)
%ae41735 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48952 = alloca %struct.ScmObj*, align 8
%fptrToInt48953 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41736 to i64
%ae41736 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48953)
store volatile %struct.ScmObj* %ae41736, %struct.ScmObj** %stackaddr$makeclosure48952, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41736, %struct.ScmObj* %_37last40151, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41736, %struct.ScmObj* %_37foldr40134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41736, %struct.ScmObj* %_37drop_45right40148, i64 2)
%args48504$ae41734$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48954 = alloca %struct.ScmObj*, align 8
%args48504$ae41734$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41736, %struct.ScmObj* %args48504$ae41734$0)
store volatile %struct.ScmObj* %args48504$ae41734$1, %struct.ScmObj** %stackaddr$prim48954, align 8
%stackaddr$prim48955 = alloca %struct.ScmObj*, align 8
%args48504$ae41734$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41735, %struct.ScmObj* %args48504$ae41734$1)
store volatile %struct.ScmObj* %args48504$ae41734$2, %struct.ScmObj** %stackaddr$prim48955, align 8
%clofunc48956 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41734)
musttail call tailcc void %clofunc48956(%struct.ScmObj* %ae41734, %struct.ScmObj* %args48504$ae41734$2)
ret void
}

define tailcc void @proc_clo$ae41734(%struct.ScmObj* %env$ae41734,%struct.ScmObj* %current_45args48152) {
%stackaddr$env-ref48957 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41734, i64 0)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref48957
%stackaddr$env-ref48958 = alloca %struct.ScmObj*, align 8
%_37foldl140113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41734, i64 1)
store %struct.ScmObj* %_37foldl140113, %struct.ScmObj** %stackaddr$env-ref48958
%stackaddr$env-ref48959 = alloca %struct.ScmObj*, align 8
%_37foldr40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41734, i64 2)
store %struct.ScmObj* %_37foldr40134, %struct.ScmObj** %stackaddr$env-ref48959
%stackaddr$env-ref48960 = alloca %struct.ScmObj*, align 8
%_37map140160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41734, i64 3)
store %struct.ScmObj* %_37map140160, %struct.ScmObj** %stackaddr$env-ref48960
%stackaddr$env-ref48961 = alloca %struct.ScmObj*, align 8
%Ycmb40108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41734, i64 4)
store %struct.ScmObj* %Ycmb40108, %struct.ScmObj** %stackaddr$env-ref48961
%stackaddr$prim48962 = alloca %struct.ScmObj*, align 8
%_95k40398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48152)
store volatile %struct.ScmObj* %_95k40398, %struct.ScmObj** %stackaddr$prim48962, align 8
%stackaddr$prim48963 = alloca %struct.ScmObj*, align 8
%current_45args48153 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48152)
store volatile %struct.ScmObj* %current_45args48153, %struct.ScmObj** %stackaddr$prim48963, align 8
%stackaddr$prim48964 = alloca %struct.ScmObj*, align 8
%_37map40155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48153)
store volatile %struct.ScmObj* %_37map40155, %struct.ScmObj** %stackaddr$prim48964, align 8
%stackaddr$makeclosure48965 = alloca %struct.ScmObj*, align 8
%fptrToInt48966 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41880 to i64
%ae41880 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48966)
store volatile %struct.ScmObj* %ae41880, %struct.ScmObj** %stackaddr$makeclosure48965, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41880, %struct.ScmObj* %_37foldl140113, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41880, %struct.ScmObj* %Ycmb40108, i64 1)
%ae41881 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48967 = alloca %struct.ScmObj*, align 8
%fptrToInt48968 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41882 to i64
%ae41882 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48968)
store volatile %struct.ScmObj* %ae41882, %struct.ScmObj** %stackaddr$makeclosure48967, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41882, %struct.ScmObj* %_37foldr40134, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41882, %struct.ScmObj* %_37foldr140129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41882, %struct.ScmObj* %_37map140160, i64 2)
%args48487$ae41880$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48969 = alloca %struct.ScmObj*, align 8
%args48487$ae41880$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41882, %struct.ScmObj* %args48487$ae41880$0)
store volatile %struct.ScmObj* %args48487$ae41880$1, %struct.ScmObj** %stackaddr$prim48969, align 8
%stackaddr$prim48970 = alloca %struct.ScmObj*, align 8
%args48487$ae41880$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41881, %struct.ScmObj* %args48487$ae41880$1)
store volatile %struct.ScmObj* %args48487$ae41880$2, %struct.ScmObj** %stackaddr$prim48970, align 8
%clofunc48971 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41880)
musttail call tailcc void %clofunc48971(%struct.ScmObj* %ae41880, %struct.ScmObj* %args48487$ae41880$2)
ret void
}

define tailcc void @proc_clo$ae41880(%struct.ScmObj* %env$ae41880,%struct.ScmObj* %current_45args48155) {
%stackaddr$env-ref48972 = alloca %struct.ScmObj*, align 8
%_37foldl140113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41880, i64 0)
store %struct.ScmObj* %_37foldl140113, %struct.ScmObj** %stackaddr$env-ref48972
%stackaddr$env-ref48973 = alloca %struct.ScmObj*, align 8
%Ycmb40108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41880, i64 1)
store %struct.ScmObj* %Ycmb40108, %struct.ScmObj** %stackaddr$env-ref48973
%stackaddr$prim48974 = alloca %struct.ScmObj*, align 8
%_95k40399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48155)
store volatile %struct.ScmObj* %_95k40399, %struct.ScmObj** %stackaddr$prim48974, align 8
%stackaddr$prim48975 = alloca %struct.ScmObj*, align 8
%current_45args48156 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48155)
store volatile %struct.ScmObj* %current_45args48156, %struct.ScmObj** %stackaddr$prim48975, align 8
%stackaddr$prim48976 = alloca %struct.ScmObj*, align 8
%anf_45bind40309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48156)
store volatile %struct.ScmObj* %anf_45bind40309, %struct.ScmObj** %stackaddr$prim48976, align 8
%stackaddr$makeclosure48977 = alloca %struct.ScmObj*, align 8
%fptrToInt48978 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42272 to i64
%ae42272 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48978)
store volatile %struct.ScmObj* %ae42272, %struct.ScmObj** %stackaddr$makeclosure48977, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42272, %struct.ScmObj* %_37foldl140113, i64 0)
%args48427$Ycmb40108$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48979 = alloca %struct.ScmObj*, align 8
%args48427$Ycmb40108$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40309, %struct.ScmObj* %args48427$Ycmb40108$0)
store volatile %struct.ScmObj* %args48427$Ycmb40108$1, %struct.ScmObj** %stackaddr$prim48979, align 8
%stackaddr$prim48980 = alloca %struct.ScmObj*, align 8
%args48427$Ycmb40108$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42272, %struct.ScmObj* %args48427$Ycmb40108$1)
store volatile %struct.ScmObj* %args48427$Ycmb40108$2, %struct.ScmObj** %stackaddr$prim48980, align 8
%clofunc48981 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40108)
musttail call tailcc void %clofunc48981(%struct.ScmObj* %Ycmb40108, %struct.ScmObj* %args48427$Ycmb40108$2)
ret void
}

define tailcc void @proc_clo$ae42272(%struct.ScmObj* %env$ae42272,%struct.ScmObj* %current_45args48158) {
%stackaddr$env-ref48982 = alloca %struct.ScmObj*, align 8
%_37foldl140113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42272, i64 0)
store %struct.ScmObj* %_37foldl140113, %struct.ScmObj** %stackaddr$env-ref48982
%stackaddr$prim48983 = alloca %struct.ScmObj*, align 8
%_95k40400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48158)
store volatile %struct.ScmObj* %_95k40400, %struct.ScmObj** %stackaddr$prim48983, align 8
%stackaddr$prim48984 = alloca %struct.ScmObj*, align 8
%current_45args48159 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48158)
store volatile %struct.ScmObj* %current_45args48159, %struct.ScmObj** %stackaddr$prim48984, align 8
%stackaddr$prim48985 = alloca %struct.ScmObj*, align 8
%_37foldl40211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48159)
store volatile %struct.ScmObj* %_37foldl40211, %struct.ScmObj** %stackaddr$prim48985, align 8
%stackaddr$makeclosure48986 = alloca %struct.ScmObj*, align 8
%fptrToInt48987 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42274 to i64
%ae42274 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48987)
store volatile %struct.ScmObj* %ae42274, %struct.ScmObj** %stackaddr$makeclosure48986, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42274, %struct.ScmObj* %_37foldl140113, i64 0)
%ae42275 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48988 = alloca %struct.ScmObj*, align 8
%fptrToInt48989 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42276 to i64
%ae42276 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48989)
store volatile %struct.ScmObj* %ae42276, %struct.ScmObj** %stackaddr$makeclosure48988, align 8
%args48426$ae42274$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48990 = alloca %struct.ScmObj*, align 8
%args48426$ae42274$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42276, %struct.ScmObj* %args48426$ae42274$0)
store volatile %struct.ScmObj* %args48426$ae42274$1, %struct.ScmObj** %stackaddr$prim48990, align 8
%stackaddr$prim48991 = alloca %struct.ScmObj*, align 8
%args48426$ae42274$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42275, %struct.ScmObj* %args48426$ae42274$1)
store volatile %struct.ScmObj* %args48426$ae42274$2, %struct.ScmObj** %stackaddr$prim48991, align 8
%clofunc48992 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42274)
musttail call tailcc void %clofunc48992(%struct.ScmObj* %ae42274, %struct.ScmObj* %args48426$ae42274$2)
ret void
}

define tailcc void @proc_clo$ae42274(%struct.ScmObj* %env$ae42274,%struct.ScmObj* %current_45args48161) {
%stackaddr$env-ref48993 = alloca %struct.ScmObj*, align 8
%_37foldl140113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42274, i64 0)
store %struct.ScmObj* %_37foldl140113, %struct.ScmObj** %stackaddr$env-ref48993
%stackaddr$prim48994 = alloca %struct.ScmObj*, align 8
%_95k40401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48161)
store volatile %struct.ScmObj* %_95k40401, %struct.ScmObj** %stackaddr$prim48994, align 8
%stackaddr$prim48995 = alloca %struct.ScmObj*, align 8
%current_45args48162 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48161)
store volatile %struct.ScmObj* %current_45args48162, %struct.ScmObj** %stackaddr$prim48995, align 8
%stackaddr$prim48996 = alloca %struct.ScmObj*, align 8
%_37_6240208 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48162)
store volatile %struct.ScmObj* %_37_6240208, %struct.ScmObj** %stackaddr$prim48996, align 8
%stackaddr$makeclosure48997 = alloca %struct.ScmObj*, align 8
%fptrToInt48998 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42298 to i64
%ae42298 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt48998)
store volatile %struct.ScmObj* %ae42298, %struct.ScmObj** %stackaddr$makeclosure48997, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42298, %struct.ScmObj* %_37foldl140113, i64 0)
%ae42299 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48999 = alloca %struct.ScmObj*, align 8
%fptrToInt49000 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42300 to i64
%ae42300 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49000)
store volatile %struct.ScmObj* %ae42300, %struct.ScmObj** %stackaddr$makeclosure48999, align 8
%args48420$ae42298$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49001 = alloca %struct.ScmObj*, align 8
%args48420$ae42298$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42300, %struct.ScmObj* %args48420$ae42298$0)
store volatile %struct.ScmObj* %args48420$ae42298$1, %struct.ScmObj** %stackaddr$prim49001, align 8
%stackaddr$prim49002 = alloca %struct.ScmObj*, align 8
%args48420$ae42298$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42299, %struct.ScmObj* %args48420$ae42298$1)
store volatile %struct.ScmObj* %args48420$ae42298$2, %struct.ScmObj** %stackaddr$prim49002, align 8
%clofunc49003 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42298)
musttail call tailcc void %clofunc49003(%struct.ScmObj* %ae42298, %struct.ScmObj* %args48420$ae42298$2)
ret void
}

define tailcc void @proc_clo$ae42298(%struct.ScmObj* %env$ae42298,%struct.ScmObj* %current_45args48164) {
%stackaddr$env-ref49004 = alloca %struct.ScmObj*, align 8
%_37foldl140113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42298, i64 0)
store %struct.ScmObj* %_37foldl140113, %struct.ScmObj** %stackaddr$env-ref49004
%stackaddr$prim49005 = alloca %struct.ScmObj*, align 8
%_95k40402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48164)
store volatile %struct.ScmObj* %_95k40402, %struct.ScmObj** %stackaddr$prim49005, align 8
%stackaddr$prim49006 = alloca %struct.ScmObj*, align 8
%current_45args48165 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48164)
store volatile %struct.ScmObj* %current_45args48165, %struct.ScmObj** %stackaddr$prim49006, align 8
%stackaddr$prim49007 = alloca %struct.ScmObj*, align 8
%_37_62_6140205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48165)
store volatile %struct.ScmObj* %_37_62_6140205, %struct.ScmObj** %stackaddr$prim49007, align 8
%ae42322 = call %struct.ScmObj* @const_init_int(i64 1)
%ae42323 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49008 = alloca %struct.ScmObj*, align 8
%_37append40201 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42322, %struct.ScmObj* %ae42323)
store volatile %struct.ScmObj* %_37append40201, %struct.ScmObj** %stackaddr$prim49008, align 8
%stackaddr$makeclosure49009 = alloca %struct.ScmObj*, align 8
%fptrToInt49010 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42324 to i64
%ae42324 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49010)
store volatile %struct.ScmObj* %ae42324, %struct.ScmObj** %stackaddr$makeclosure49009, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42324, %struct.ScmObj* %_37append40201, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42324, %struct.ScmObj* %_37foldl140113, i64 1)
%ae42325 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49011 = alloca %struct.ScmObj*, align 8
%fptrToInt49012 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42326 to i64
%ae42326 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49012)
store volatile %struct.ScmObj* %ae42326, %struct.ScmObj** %stackaddr$makeclosure49011, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42326, %struct.ScmObj* %_37append40201, i64 0)
%args48414$ae42324$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49013 = alloca %struct.ScmObj*, align 8
%args48414$ae42324$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42326, %struct.ScmObj* %args48414$ae42324$0)
store volatile %struct.ScmObj* %args48414$ae42324$1, %struct.ScmObj** %stackaddr$prim49013, align 8
%stackaddr$prim49014 = alloca %struct.ScmObj*, align 8
%args48414$ae42324$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42325, %struct.ScmObj* %args48414$ae42324$1)
store volatile %struct.ScmObj* %args48414$ae42324$2, %struct.ScmObj** %stackaddr$prim49014, align 8
%clofunc49015 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42324)
musttail call tailcc void %clofunc49015(%struct.ScmObj* %ae42324, %struct.ScmObj* %args48414$ae42324$2)
ret void
}

define tailcc void @proc_clo$ae42324(%struct.ScmObj* %env$ae42324,%struct.ScmObj* %current_45args48167) {
%stackaddr$env-ref49016 = alloca %struct.ScmObj*, align 8
%_37append40201 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42324, i64 0)
store %struct.ScmObj* %_37append40201, %struct.ScmObj** %stackaddr$env-ref49016
%stackaddr$env-ref49017 = alloca %struct.ScmObj*, align 8
%_37foldl140113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42324, i64 1)
store %struct.ScmObj* %_37foldl140113, %struct.ScmObj** %stackaddr$env-ref49017
%stackaddr$prim49018 = alloca %struct.ScmObj*, align 8
%_95k40403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48167)
store volatile %struct.ScmObj* %_95k40403, %struct.ScmObj** %stackaddr$prim49018, align 8
%stackaddr$prim49019 = alloca %struct.ScmObj*, align 8
%current_45args48168 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48167)
store volatile %struct.ScmObj* %current_45args48168, %struct.ScmObj** %stackaddr$prim49019, align 8
%stackaddr$prim49020 = alloca %struct.ScmObj*, align 8
%anf_45bind40317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48168)
store volatile %struct.ScmObj* %anf_45bind40317, %struct.ScmObj** %stackaddr$prim49020, align 8
%ae42392 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49021 = alloca %struct.ScmObj*, align 8
%_95040202 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append40201, %struct.ScmObj* %ae42392, %struct.ScmObj* %anf_45bind40317)
store volatile %struct.ScmObj* %_95040202, %struct.ScmObj** %stackaddr$prim49021, align 8
%ae42395 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49022 = alloca %struct.ScmObj*, align 8
%_37append40200 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40201, %struct.ScmObj* %ae42395)
store volatile %struct.ScmObj* %_37append40200, %struct.ScmObj** %stackaddr$prim49022, align 8
%stackaddr$makeclosure49023 = alloca %struct.ScmObj*, align 8
%fptrToInt49024 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42396 to i64
%ae42396 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49024)
store volatile %struct.ScmObj* %ae42396, %struct.ScmObj** %stackaddr$makeclosure49023, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42396, %struct.ScmObj* %_37foldl140113, i64 0)
%ae42397 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49025 = alloca %struct.ScmObj*, align 8
%fptrToInt49026 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42398 to i64
%ae42398 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49026)
store volatile %struct.ScmObj* %ae42398, %struct.ScmObj** %stackaddr$makeclosure49025, align 8
%args48403$ae42396$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49027 = alloca %struct.ScmObj*, align 8
%args48403$ae42396$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42398, %struct.ScmObj* %args48403$ae42396$0)
store volatile %struct.ScmObj* %args48403$ae42396$1, %struct.ScmObj** %stackaddr$prim49027, align 8
%stackaddr$prim49028 = alloca %struct.ScmObj*, align 8
%args48403$ae42396$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42397, %struct.ScmObj* %args48403$ae42396$1)
store volatile %struct.ScmObj* %args48403$ae42396$2, %struct.ScmObj** %stackaddr$prim49028, align 8
%clofunc49029 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42396)
musttail call tailcc void %clofunc49029(%struct.ScmObj* %ae42396, %struct.ScmObj* %args48403$ae42396$2)
ret void
}

define tailcc void @proc_clo$ae42396(%struct.ScmObj* %env$ae42396,%struct.ScmObj* %current_45args48170) {
%stackaddr$env-ref49030 = alloca %struct.ScmObj*, align 8
%_37foldl140113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42396, i64 0)
store %struct.ScmObj* %_37foldl140113, %struct.ScmObj** %stackaddr$env-ref49030
%stackaddr$prim49031 = alloca %struct.ScmObj*, align 8
%_95k40404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48170)
store volatile %struct.ScmObj* %_95k40404, %struct.ScmObj** %stackaddr$prim49031, align 8
%stackaddr$prim49032 = alloca %struct.ScmObj*, align 8
%current_45args48171 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48170)
store volatile %struct.ScmObj* %current_45args48171, %struct.ScmObj** %stackaddr$prim49032, align 8
%stackaddr$prim49033 = alloca %struct.ScmObj*, align 8
%_37list_6340193 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48171)
store volatile %struct.ScmObj* %_37list_6340193, %struct.ScmObj** %stackaddr$prim49033, align 8
%stackaddr$makeclosure49034 = alloca %struct.ScmObj*, align 8
%fptrToInt49035 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42812 to i64
%ae42812 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49035)
store volatile %struct.ScmObj* %ae42812, %struct.ScmObj** %stackaddr$makeclosure49034, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42812, %struct.ScmObj* %_37foldl140113, i64 0)
%ae42813 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49036 = alloca %struct.ScmObj*, align 8
%fptrToInt49037 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42814 to i64
%ae42814 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49037)
store volatile %struct.ScmObj* %ae42814, %struct.ScmObj** %stackaddr$makeclosure49036, align 8
%args48378$ae42812$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49038 = alloca %struct.ScmObj*, align 8
%args48378$ae42812$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42814, %struct.ScmObj* %args48378$ae42812$0)
store volatile %struct.ScmObj* %args48378$ae42812$1, %struct.ScmObj** %stackaddr$prim49038, align 8
%stackaddr$prim49039 = alloca %struct.ScmObj*, align 8
%args48378$ae42812$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42813, %struct.ScmObj* %args48378$ae42812$1)
store volatile %struct.ScmObj* %args48378$ae42812$2, %struct.ScmObj** %stackaddr$prim49039, align 8
%clofunc49040 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42812)
musttail call tailcc void %clofunc49040(%struct.ScmObj* %ae42812, %struct.ScmObj* %args48378$ae42812$2)
ret void
}

define tailcc void @proc_clo$ae42812(%struct.ScmObj* %env$ae42812,%struct.ScmObj* %current_45args48173) {
%stackaddr$env-ref49041 = alloca %struct.ScmObj*, align 8
%_37foldl140113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42812, i64 0)
store %struct.ScmObj* %_37foldl140113, %struct.ScmObj** %stackaddr$env-ref49041
%stackaddr$prim49042 = alloca %struct.ScmObj*, align 8
%_95k40405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48173)
store volatile %struct.ScmObj* %_95k40405, %struct.ScmObj** %stackaddr$prim49042, align 8
%stackaddr$prim49043 = alloca %struct.ScmObj*, align 8
%current_45args48174 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48173)
store volatile %struct.ScmObj* %current_45args48174, %struct.ScmObj** %stackaddr$prim49043, align 8
%stackaddr$prim49044 = alloca %struct.ScmObj*, align 8
%_37drop40184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48174)
store volatile %struct.ScmObj* %_37drop40184, %struct.ScmObj** %stackaddr$prim49044, align 8
%stackaddr$makeclosure49045 = alloca %struct.ScmObj*, align 8
%fptrToInt49046 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43348 to i64
%ae43348 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49046)
store volatile %struct.ScmObj* %ae43348, %struct.ScmObj** %stackaddr$makeclosure49045, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43348, %struct.ScmObj* %_37foldl140113, i64 0)
%ae43349 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49047 = alloca %struct.ScmObj*, align 8
%fptrToInt49048 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43350 to i64
%ae43350 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49048)
store volatile %struct.ScmObj* %ae43350, %struct.ScmObj** %stackaddr$makeclosure49047, align 8
%args48354$ae43348$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49049 = alloca %struct.ScmObj*, align 8
%args48354$ae43348$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43350, %struct.ScmObj* %args48354$ae43348$0)
store volatile %struct.ScmObj* %args48354$ae43348$1, %struct.ScmObj** %stackaddr$prim49049, align 8
%stackaddr$prim49050 = alloca %struct.ScmObj*, align 8
%args48354$ae43348$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43349, %struct.ScmObj* %args48354$ae43348$1)
store volatile %struct.ScmObj* %args48354$ae43348$2, %struct.ScmObj** %stackaddr$prim49050, align 8
%clofunc49051 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43348)
musttail call tailcc void %clofunc49051(%struct.ScmObj* %ae43348, %struct.ScmObj* %args48354$ae43348$2)
ret void
}

define tailcc void @proc_clo$ae43348(%struct.ScmObj* %env$ae43348,%struct.ScmObj* %current_45args48176) {
%stackaddr$env-ref49052 = alloca %struct.ScmObj*, align 8
%_37foldl140113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43348, i64 0)
store %struct.ScmObj* %_37foldl140113, %struct.ScmObj** %stackaddr$env-ref49052
%stackaddr$prim49053 = alloca %struct.ScmObj*, align 8
%_95k40406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48176)
store volatile %struct.ScmObj* %_95k40406, %struct.ScmObj** %stackaddr$prim49053, align 8
%stackaddr$prim49054 = alloca %struct.ScmObj*, align 8
%current_45args48177 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48176)
store volatile %struct.ScmObj* %current_45args48177, %struct.ScmObj** %stackaddr$prim49054, align 8
%stackaddr$prim49055 = alloca %struct.ScmObj*, align 8
%_37memv40177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48177)
store volatile %struct.ScmObj* %_37memv40177, %struct.ScmObj** %stackaddr$prim49055, align 8
%stackaddr$makeclosure49056 = alloca %struct.ScmObj*, align 8
%fptrToInt49057 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43752 to i64
%ae43752 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49057)
store volatile %struct.ScmObj* %ae43752, %struct.ScmObj** %stackaddr$makeclosure49056, align 8
%ae43753 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49058 = alloca %struct.ScmObj*, align 8
%fptrToInt49059 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43754 to i64
%ae43754 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49059)
store volatile %struct.ScmObj* %ae43754, %struct.ScmObj** %stackaddr$makeclosure49058, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43754, %struct.ScmObj* %_37foldl140113, i64 0)
%args48328$ae43752$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49060 = alloca %struct.ScmObj*, align 8
%args48328$ae43752$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43754, %struct.ScmObj* %args48328$ae43752$0)
store volatile %struct.ScmObj* %args48328$ae43752$1, %struct.ScmObj** %stackaddr$prim49060, align 8
%stackaddr$prim49061 = alloca %struct.ScmObj*, align 8
%args48328$ae43752$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43753, %struct.ScmObj* %args48328$ae43752$1)
store volatile %struct.ScmObj* %args48328$ae43752$2, %struct.ScmObj** %stackaddr$prim49061, align 8
%clofunc49062 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43752)
musttail call tailcc void %clofunc49062(%struct.ScmObj* %ae43752, %struct.ScmObj* %args48328$ae43752$2)
ret void
}

define tailcc void @proc_clo$ae43752(%struct.ScmObj* %env$ae43752,%struct.ScmObj* %current_45args48179) {
%stackaddr$prim49063 = alloca %struct.ScmObj*, align 8
%_95k40407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48179)
store volatile %struct.ScmObj* %_95k40407, %struct.ScmObj** %stackaddr$prim49063, align 8
%stackaddr$prim49064 = alloca %struct.ScmObj*, align 8
%current_45args48180 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48179)
store volatile %struct.ScmObj* %current_45args48180, %struct.ScmObj** %stackaddr$prim49064, align 8
%stackaddr$prim49065 = alloca %struct.ScmObj*, align 8
%_37_4740173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48180)
store volatile %struct.ScmObj* %_37_4740173, %struct.ScmObj** %stackaddr$prim49065, align 8
%stackaddr$makeclosure49066 = alloca %struct.ScmObj*, align 8
%fptrToInt49067 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43850 to i64
%ae43850 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49067)
store volatile %struct.ScmObj* %ae43850, %struct.ScmObj** %stackaddr$makeclosure49066, align 8
%ae43851 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49068 = alloca %struct.ScmObj*, align 8
%fptrToInt49069 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43852 to i64
%ae43852 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49069)
store volatile %struct.ScmObj* %ae43852, %struct.ScmObj** %stackaddr$makeclosure49068, align 8
%args48315$ae43850$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49070 = alloca %struct.ScmObj*, align 8
%args48315$ae43850$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43852, %struct.ScmObj* %args48315$ae43850$0)
store volatile %struct.ScmObj* %args48315$ae43850$1, %struct.ScmObj** %stackaddr$prim49070, align 8
%stackaddr$prim49071 = alloca %struct.ScmObj*, align 8
%args48315$ae43850$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43851, %struct.ScmObj* %args48315$ae43850$1)
store volatile %struct.ScmObj* %args48315$ae43850$2, %struct.ScmObj** %stackaddr$prim49071, align 8
%clofunc49072 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43850)
musttail call tailcc void %clofunc49072(%struct.ScmObj* %ae43850, %struct.ScmObj* %args48315$ae43850$2)
ret void
}

define tailcc void @proc_clo$ae43850(%struct.ScmObj* %env$ae43850,%struct.ScmObj* %current_45args48182) {
%stackaddr$prim49073 = alloca %struct.ScmObj*, align 8
%_95k40408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48182)
store volatile %struct.ScmObj* %_95k40408, %struct.ScmObj** %stackaddr$prim49073, align 8
%stackaddr$prim49074 = alloca %struct.ScmObj*, align 8
%current_45args48183 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48182)
store volatile %struct.ScmObj* %current_45args48183, %struct.ScmObj** %stackaddr$prim49074, align 8
%stackaddr$prim49075 = alloca %struct.ScmObj*, align 8
%_37first40171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48183)
store volatile %struct.ScmObj* %_37first40171, %struct.ScmObj** %stackaddr$prim49075, align 8
%stackaddr$makeclosure49076 = alloca %struct.ScmObj*, align 8
%fptrToInt49077 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43870 to i64
%ae43870 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49077)
store volatile %struct.ScmObj* %ae43870, %struct.ScmObj** %stackaddr$makeclosure49076, align 8
%ae43871 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49078 = alloca %struct.ScmObj*, align 8
%fptrToInt49079 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43872 to i64
%ae43872 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49079)
store volatile %struct.ScmObj* %ae43872, %struct.ScmObj** %stackaddr$makeclosure49078, align 8
%args48310$ae43870$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49080 = alloca %struct.ScmObj*, align 8
%args48310$ae43870$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43872, %struct.ScmObj* %args48310$ae43870$0)
store volatile %struct.ScmObj* %args48310$ae43870$1, %struct.ScmObj** %stackaddr$prim49080, align 8
%stackaddr$prim49081 = alloca %struct.ScmObj*, align 8
%args48310$ae43870$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43871, %struct.ScmObj* %args48310$ae43870$1)
store volatile %struct.ScmObj* %args48310$ae43870$2, %struct.ScmObj** %stackaddr$prim49081, align 8
%clofunc49082 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43870)
musttail call tailcc void %clofunc49082(%struct.ScmObj* %ae43870, %struct.ScmObj* %args48310$ae43870$2)
ret void
}

define tailcc void @proc_clo$ae43870(%struct.ScmObj* %env$ae43870,%struct.ScmObj* %current_45args48185) {
%stackaddr$prim49083 = alloca %struct.ScmObj*, align 8
%_95k40409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48185)
store volatile %struct.ScmObj* %_95k40409, %struct.ScmObj** %stackaddr$prim49083, align 8
%stackaddr$prim49084 = alloca %struct.ScmObj*, align 8
%current_45args48186 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48185)
store volatile %struct.ScmObj* %current_45args48186, %struct.ScmObj** %stackaddr$prim49084, align 8
%stackaddr$prim49085 = alloca %struct.ScmObj*, align 8
%_37second40169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48186)
store volatile %struct.ScmObj* %_37second40169, %struct.ScmObj** %stackaddr$prim49085, align 8
%stackaddr$makeclosure49086 = alloca %struct.ScmObj*, align 8
%fptrToInt49087 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43892 to i64
%ae43892 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49087)
store volatile %struct.ScmObj* %ae43892, %struct.ScmObj** %stackaddr$makeclosure49086, align 8
%ae43893 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49088 = alloca %struct.ScmObj*, align 8
%fptrToInt49089 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43894 to i64
%ae43894 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49089)
store volatile %struct.ScmObj* %ae43894, %struct.ScmObj** %stackaddr$makeclosure49088, align 8
%args48305$ae43892$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49090 = alloca %struct.ScmObj*, align 8
%args48305$ae43892$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43894, %struct.ScmObj* %args48305$ae43892$0)
store volatile %struct.ScmObj* %args48305$ae43892$1, %struct.ScmObj** %stackaddr$prim49090, align 8
%stackaddr$prim49091 = alloca %struct.ScmObj*, align 8
%args48305$ae43892$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43893, %struct.ScmObj* %args48305$ae43892$1)
store volatile %struct.ScmObj* %args48305$ae43892$2, %struct.ScmObj** %stackaddr$prim49091, align 8
%clofunc49092 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43892)
musttail call tailcc void %clofunc49092(%struct.ScmObj* %ae43892, %struct.ScmObj* %args48305$ae43892$2)
ret void
}

define tailcc void @proc_clo$ae43892(%struct.ScmObj* %env$ae43892,%struct.ScmObj* %current_45args48188) {
%stackaddr$prim49093 = alloca %struct.ScmObj*, align 8
%_95k40410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48188)
store volatile %struct.ScmObj* %_95k40410, %struct.ScmObj** %stackaddr$prim49093, align 8
%stackaddr$prim49094 = alloca %struct.ScmObj*, align 8
%current_45args48189 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48188)
store volatile %struct.ScmObj* %current_45args48189, %struct.ScmObj** %stackaddr$prim49094, align 8
%stackaddr$prim49095 = alloca %struct.ScmObj*, align 8
%_37third40167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48189)
store volatile %struct.ScmObj* %_37third40167, %struct.ScmObj** %stackaddr$prim49095, align 8
%stackaddr$makeclosure49096 = alloca %struct.ScmObj*, align 8
%fptrToInt49097 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43916 to i64
%ae43916 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49097)
store volatile %struct.ScmObj* %ae43916, %struct.ScmObj** %stackaddr$makeclosure49096, align 8
%ae43917 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49098 = alloca %struct.ScmObj*, align 8
%fptrToInt49099 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43918 to i64
%ae43918 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49099)
store volatile %struct.ScmObj* %ae43918, %struct.ScmObj** %stackaddr$makeclosure49098, align 8
%args48300$ae43916$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49100 = alloca %struct.ScmObj*, align 8
%args48300$ae43916$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43918, %struct.ScmObj* %args48300$ae43916$0)
store volatile %struct.ScmObj* %args48300$ae43916$1, %struct.ScmObj** %stackaddr$prim49100, align 8
%stackaddr$prim49101 = alloca %struct.ScmObj*, align 8
%args48300$ae43916$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43917, %struct.ScmObj* %args48300$ae43916$1)
store volatile %struct.ScmObj* %args48300$ae43916$2, %struct.ScmObj** %stackaddr$prim49101, align 8
%clofunc49102 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43916)
musttail call tailcc void %clofunc49102(%struct.ScmObj* %ae43916, %struct.ScmObj* %args48300$ae43916$2)
ret void
}

define tailcc void @proc_clo$ae43916(%struct.ScmObj* %env$ae43916,%struct.ScmObj* %current_45args48191) {
%stackaddr$prim49103 = alloca %struct.ScmObj*, align 8
%_95k40411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48191)
store volatile %struct.ScmObj* %_95k40411, %struct.ScmObj** %stackaddr$prim49103, align 8
%stackaddr$prim49104 = alloca %struct.ScmObj*, align 8
%current_45args48192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48191)
store volatile %struct.ScmObj* %current_45args48192, %struct.ScmObj** %stackaddr$prim49104, align 8
%stackaddr$prim49105 = alloca %struct.ScmObj*, align 8
%_37fourth40165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48192)
store volatile %struct.ScmObj* %_37fourth40165, %struct.ScmObj** %stackaddr$prim49105, align 8
%stackaddr$makeclosure49106 = alloca %struct.ScmObj*, align 8
%fptrToInt49107 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43942 to i64
%ae43942 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49107)
store volatile %struct.ScmObj* %ae43942, %struct.ScmObj** %stackaddr$makeclosure49106, align 8
%ae43943 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49108 = alloca %struct.ScmObj*, align 8
%fptrToInt49109 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43944 to i64
%ae43944 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49109)
store volatile %struct.ScmObj* %ae43944, %struct.ScmObj** %stackaddr$makeclosure49108, align 8
%args48295$ae43942$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49110 = alloca %struct.ScmObj*, align 8
%args48295$ae43942$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43944, %struct.ScmObj* %args48295$ae43942$0)
store volatile %struct.ScmObj* %args48295$ae43942$1, %struct.ScmObj** %stackaddr$prim49110, align 8
%stackaddr$prim49111 = alloca %struct.ScmObj*, align 8
%args48295$ae43942$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43943, %struct.ScmObj* %args48295$ae43942$1)
store volatile %struct.ScmObj* %args48295$ae43942$2, %struct.ScmObj** %stackaddr$prim49111, align 8
%clofunc49112 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43942)
musttail call tailcc void %clofunc49112(%struct.ScmObj* %ae43942, %struct.ScmObj* %args48295$ae43942$2)
ret void
}

define tailcc void @proc_clo$ae43942(%struct.ScmObj* %env$ae43942,%struct.ScmObj* %current_45args48194) {
%stackaddr$prim49113 = alloca %struct.ScmObj*, align 8
%_95k40412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48194)
store volatile %struct.ScmObj* %_95k40412, %struct.ScmObj** %stackaddr$prim49113, align 8
%stackaddr$prim49114 = alloca %struct.ScmObj*, align 8
%current_45args48195 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48194)
store volatile %struct.ScmObj* %current_45args48195, %struct.ScmObj** %stackaddr$prim49114, align 8
%stackaddr$prim49115 = alloca %struct.ScmObj*, align 8
%promise_6340226 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48195)
store volatile %struct.ScmObj* %promise_6340226, %struct.ScmObj** %stackaddr$prim49115, align 8
%stackaddr$prim49116 = alloca %struct.ScmObj*, align 8
%anf_45bind40357 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40357, %struct.ScmObj** %stackaddr$prim49116, align 8
%ae44029 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49117 = alloca %struct.ScmObj*, align 8
%gen40229 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44029, %struct.ScmObj* %anf_45bind40357)
store volatile %struct.ScmObj* %gen40229, %struct.ScmObj** %stackaddr$prim49117, align 8
%stackaddr$prim49118 = alloca %struct.ScmObj*, align 8
%anf_45bind40358 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind40358, %struct.ScmObj** %stackaddr$prim49118, align 8
%ae44031 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49119 = alloca %struct.ScmObj*, align 8
%lazy_45take40228 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae44031, %struct.ScmObj* %anf_45bind40358)
store volatile %struct.ScmObj* %lazy_45take40228, %struct.ScmObj** %stackaddr$prim49119, align 8
%stackaddr$makeclosure49120 = alloca %struct.ScmObj*, align 8
%fptrToInt49121 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44033 to i64
%ae44033 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49121)
store volatile %struct.ScmObj* %ae44033, %struct.ScmObj** %stackaddr$makeclosure49120, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44033, %struct.ScmObj* %gen40229, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44033, %struct.ScmObj* %lazy_45take40228, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44033, %struct.ScmObj* %promise_6340226, i64 2)
%ae44034 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49122 = alloca %struct.ScmObj*, align 8
%fptrToInt49123 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44035 to i64
%ae44035 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49123)
store volatile %struct.ScmObj* %ae44035, %struct.ScmObj** %stackaddr$makeclosure49122, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44035, %struct.ScmObj* %gen40229, i64 0)
%args48288$ae44033$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49124 = alloca %struct.ScmObj*, align 8
%args48288$ae44033$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44035, %struct.ScmObj* %args48288$ae44033$0)
store volatile %struct.ScmObj* %args48288$ae44033$1, %struct.ScmObj** %stackaddr$prim49124, align 8
%stackaddr$prim49125 = alloca %struct.ScmObj*, align 8
%args48288$ae44033$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44034, %struct.ScmObj* %args48288$ae44033$1)
store volatile %struct.ScmObj* %args48288$ae44033$2, %struct.ScmObj** %stackaddr$prim49125, align 8
%clofunc49126 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44033)
musttail call tailcc void %clofunc49126(%struct.ScmObj* %ae44033, %struct.ScmObj* %args48288$ae44033$2)
ret void
}

define tailcc void @proc_clo$ae44033(%struct.ScmObj* %env$ae44033,%struct.ScmObj* %current_45args48197) {
%stackaddr$env-ref49127 = alloca %struct.ScmObj*, align 8
%gen40229 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44033, i64 0)
store %struct.ScmObj* %gen40229, %struct.ScmObj** %stackaddr$env-ref49127
%stackaddr$env-ref49128 = alloca %struct.ScmObj*, align 8
%lazy_45take40228 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44033, i64 1)
store %struct.ScmObj* %lazy_45take40228, %struct.ScmObj** %stackaddr$env-ref49128
%stackaddr$env-ref49129 = alloca %struct.ScmObj*, align 8
%promise_6340226 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44033, i64 2)
store %struct.ScmObj* %promise_6340226, %struct.ScmObj** %stackaddr$env-ref49129
%stackaddr$prim49130 = alloca %struct.ScmObj*, align 8
%_95k40413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48197)
store volatile %struct.ScmObj* %_95k40413, %struct.ScmObj** %stackaddr$prim49130, align 8
%stackaddr$prim49131 = alloca %struct.ScmObj*, align 8
%current_45args48198 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48197)
store volatile %struct.ScmObj* %current_45args48198, %struct.ScmObj** %stackaddr$prim49131, align 8
%stackaddr$prim49132 = alloca %struct.ScmObj*, align 8
%anf_45bind40364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48198)
store volatile %struct.ScmObj* %anf_45bind40364, %struct.ScmObj** %stackaddr$prim49132, align 8
%ae44178 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49133 = alloca %struct.ScmObj*, align 8
%t4010340238 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %gen40229, %struct.ScmObj* %ae44178, %struct.ScmObj* %anf_45bind40364)
store volatile %struct.ScmObj* %t4010340238, %struct.ScmObj** %stackaddr$prim49133, align 8
%stackaddr$makeclosure49134 = alloca %struct.ScmObj*, align 8
%fptrToInt49135 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44180 to i64
%ae44180 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49135)
store volatile %struct.ScmObj* %ae44180, %struct.ScmObj** %stackaddr$makeclosure49134, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44180, %struct.ScmObj* %gen40229, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44180, %struct.ScmObj* %lazy_45take40228, i64 1)
%ae44181 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49136 = alloca %struct.ScmObj*, align 8
%fptrToInt49137 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44182 to i64
%ae44182 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49137)
store volatile %struct.ScmObj* %ae44182, %struct.ScmObj** %stackaddr$makeclosure49136, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44182, %struct.ScmObj* %lazy_45take40228, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44182, %struct.ScmObj* %promise_6340226, i64 1)
%args48266$ae44180$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49138 = alloca %struct.ScmObj*, align 8
%args48266$ae44180$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44182, %struct.ScmObj* %args48266$ae44180$0)
store volatile %struct.ScmObj* %args48266$ae44180$1, %struct.ScmObj** %stackaddr$prim49138, align 8
%stackaddr$prim49139 = alloca %struct.ScmObj*, align 8
%args48266$ae44180$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44181, %struct.ScmObj* %args48266$ae44180$1)
store volatile %struct.ScmObj* %args48266$ae44180$2, %struct.ScmObj** %stackaddr$prim49139, align 8
%clofunc49140 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44180)
musttail call tailcc void %clofunc49140(%struct.ScmObj* %ae44180, %struct.ScmObj* %args48266$ae44180$2)
ret void
}

define tailcc void @proc_clo$ae44180(%struct.ScmObj* %env$ae44180,%struct.ScmObj* %current_45args48200) {
%stackaddr$env-ref49141 = alloca %struct.ScmObj*, align 8
%gen40229 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44180, i64 0)
store %struct.ScmObj* %gen40229, %struct.ScmObj** %stackaddr$env-ref49141
%stackaddr$env-ref49142 = alloca %struct.ScmObj*, align 8
%lazy_45take40228 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44180, i64 1)
store %struct.ScmObj* %lazy_45take40228, %struct.ScmObj** %stackaddr$env-ref49142
%stackaddr$prim49143 = alloca %struct.ScmObj*, align 8
%_95k40414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48200)
store volatile %struct.ScmObj* %_95k40414, %struct.ScmObj** %stackaddr$prim49143, align 8
%stackaddr$prim49144 = alloca %struct.ScmObj*, align 8
%current_45args48201 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48200)
store volatile %struct.ScmObj* %current_45args48201, %struct.ScmObj** %stackaddr$prim49144, align 8
%stackaddr$prim49145 = alloca %struct.ScmObj*, align 8
%anf_45bind40376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48201)
store volatile %struct.ScmObj* %anf_45bind40376, %struct.ScmObj** %stackaddr$prim49145, align 8
%ae44869 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49146 = alloca %struct.ScmObj*, align 8
%t4010240230 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lazy_45take40228, %struct.ScmObj* %ae44869, %struct.ScmObj* %anf_45bind40376)
store volatile %struct.ScmObj* %t4010240230, %struct.ScmObj** %stackaddr$prim49146, align 8
%ae44872 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49147 = alloca %struct.ScmObj*, align 8
%anf_45bind40377 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lazy_45take40228, %struct.ScmObj* %ae44872)
store volatile %struct.ScmObj* %anf_45bind40377, %struct.ScmObj** %stackaddr$prim49147, align 8
%ae44874 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49148 = alloca %struct.ScmObj*, align 8
%anf_45bind40378 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %gen40229, %struct.ScmObj* %ae44874)
store volatile %struct.ScmObj* %anf_45bind40378, %struct.ScmObj** %stackaddr$prim49148, align 8
%stackaddr$makeclosure49149 = alloca %struct.ScmObj*, align 8
%fptrToInt49150 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44876 to i64
%ae44876 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49150)
store volatile %struct.ScmObj* %ae44876, %struct.ScmObj** %stackaddr$makeclosure49149, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44876, %struct.ScmObj* %anf_45bind40377, i64 0)
%ae44877 = call %struct.ScmObj* @const_init_int(i64 8)
%args48211$anf_45bind40378$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49151 = alloca %struct.ScmObj*, align 8
%args48211$anf_45bind40378$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44877, %struct.ScmObj* %args48211$anf_45bind40378$0)
store volatile %struct.ScmObj* %args48211$anf_45bind40378$1, %struct.ScmObj** %stackaddr$prim49151, align 8
%stackaddr$prim49152 = alloca %struct.ScmObj*, align 8
%args48211$anf_45bind40378$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44876, %struct.ScmObj* %args48211$anf_45bind40378$1)
store volatile %struct.ScmObj* %args48211$anf_45bind40378$2, %struct.ScmObj** %stackaddr$prim49152, align 8
%clofunc49153 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40378)
musttail call tailcc void %clofunc49153(%struct.ScmObj* %anf_45bind40378, %struct.ScmObj* %args48211$anf_45bind40378$2)
ret void
}

define tailcc void @proc_clo$ae44876(%struct.ScmObj* %env$ae44876,%struct.ScmObj* %current_45args48203) {
%stackaddr$env-ref49154 = alloca %struct.ScmObj*, align 8
%anf_45bind40377 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44876, i64 0)
store %struct.ScmObj* %anf_45bind40377, %struct.ScmObj** %stackaddr$env-ref49154
%stackaddr$prim49155 = alloca %struct.ScmObj*, align 8
%_95k40415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48203)
store volatile %struct.ScmObj* %_95k40415, %struct.ScmObj** %stackaddr$prim49155, align 8
%stackaddr$prim49156 = alloca %struct.ScmObj*, align 8
%current_45args48204 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48203)
store volatile %struct.ScmObj* %current_45args48204, %struct.ScmObj** %stackaddr$prim49156, align 8
%stackaddr$prim49157 = alloca %struct.ScmObj*, align 8
%anf_45bind40379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48204)
store volatile %struct.ScmObj* %anf_45bind40379, %struct.ScmObj** %stackaddr$prim49157, align 8
%stackaddr$makeclosure49158 = alloca %struct.ScmObj*, align 8
%fptrToInt49159 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44882 to i64
%ae44882 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49159)
store volatile %struct.ScmObj* %ae44882, %struct.ScmObj** %stackaddr$makeclosure49158, align 8
%ae44884 = call %struct.ScmObj* @const_init_int(i64 4)
%args48210$anf_45bind40377$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49160 = alloca %struct.ScmObj*, align 8
%args48210$anf_45bind40377$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44884, %struct.ScmObj* %args48210$anf_45bind40377$0)
store volatile %struct.ScmObj* %args48210$anf_45bind40377$1, %struct.ScmObj** %stackaddr$prim49160, align 8
%stackaddr$prim49161 = alloca %struct.ScmObj*, align 8
%args48210$anf_45bind40377$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40379, %struct.ScmObj* %args48210$anf_45bind40377$1)
store volatile %struct.ScmObj* %args48210$anf_45bind40377$2, %struct.ScmObj** %stackaddr$prim49161, align 8
%stackaddr$prim49162 = alloca %struct.ScmObj*, align 8
%args48210$anf_45bind40377$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44882, %struct.ScmObj* %args48210$anf_45bind40377$2)
store volatile %struct.ScmObj* %args48210$anf_45bind40377$3, %struct.ScmObj** %stackaddr$prim49162, align 8
%clofunc49163 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40377)
musttail call tailcc void %clofunc49163(%struct.ScmObj* %anf_45bind40377, %struct.ScmObj* %args48210$anf_45bind40377$3)
ret void
}

define tailcc void @proc_clo$ae44882(%struct.ScmObj* %env$ae44882,%struct.ScmObj* %current_45args48206) {
%stackaddr$prim49164 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48206)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim49164, align 8
%stackaddr$prim49165 = alloca %struct.ScmObj*, align 8
%current_45args48207 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48206)
store volatile %struct.ScmObj* %current_45args48207, %struct.ScmObj** %stackaddr$prim49165, align 8
%stackaddr$prim49166 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48207)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim49166, align 8
%stackaddr$prim49167 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim49167, align 8
%args48209$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49168 = alloca %struct.ScmObj*, align 8
%args48209$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args48209$k$0)
store volatile %struct.ScmObj* %args48209$k$1, %struct.ScmObj** %stackaddr$prim49168, align 8
%clofunc49169 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc49169(%struct.ScmObj* %k, %struct.ScmObj* %args48209$k$1)
ret void
}

define tailcc void @proc_clo$ae44182(%struct.ScmObj* %env$ae44182,%struct.ScmObj* %current_45args48212) {
%stackaddr$env-ref49170 = alloca %struct.ScmObj*, align 8
%lazy_45take40228 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44182, i64 0)
store %struct.ScmObj* %lazy_45take40228, %struct.ScmObj** %stackaddr$env-ref49170
%stackaddr$env-ref49171 = alloca %struct.ScmObj*, align 8
%promise_6340226 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44182, i64 1)
store %struct.ScmObj* %promise_6340226, %struct.ScmObj** %stackaddr$env-ref49171
%stackaddr$prim49172 = alloca %struct.ScmObj*, align 8
%k40416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48212)
store volatile %struct.ScmObj* %k40416, %struct.ScmObj** %stackaddr$prim49172, align 8
%stackaddr$prim49173 = alloca %struct.ScmObj*, align 8
%current_45args48213 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48212)
store volatile %struct.ScmObj* %current_45args48213, %struct.ScmObj** %stackaddr$prim49173, align 8
%stackaddr$prim49174 = alloca %struct.ScmObj*, align 8
%llst40232 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48213)
store volatile %struct.ScmObj* %llst40232, %struct.ScmObj** %stackaddr$prim49174, align 8
%stackaddr$prim49175 = alloca %struct.ScmObj*, align 8
%current_45args48214 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48213)
store volatile %struct.ScmObj* %current_45args48214, %struct.ScmObj** %stackaddr$prim49175, align 8
%stackaddr$prim49176 = alloca %struct.ScmObj*, align 8
%n40231 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48214)
store volatile %struct.ScmObj* %n40231, %struct.ScmObj** %stackaddr$prim49176, align 8
%ae44184 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49177 = alloca %struct.ScmObj*, align 8
%anf_45bind40365 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n40231, %struct.ScmObj* %ae44184)
store volatile %struct.ScmObj* %anf_45bind40365, %struct.ScmObj** %stackaddr$prim49177, align 8
%truthy$cmp49178 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40365)
%cmp$cmp49178 = icmp eq i64 %truthy$cmp49178, 1
br i1 %cmp$cmp49178, label %truebranch$cmp49178, label %falsebranch$cmp49178
truebranch$cmp49178:
%stackaddr$makeclosure49179 = alloca %struct.ScmObj*, align 8
%fptrToInt49180 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44186 to i64
%ae44186 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49180)
store volatile %struct.ScmObj* %ae44186, %struct.ScmObj** %stackaddr$makeclosure49179, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44186, %struct.ScmObj* %k40416, i64 0)
%ae44187 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49181 = alloca %struct.ScmObj*, align 8
%fptrToInt49182 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44188 to i64
%ae44188 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49182)
store volatile %struct.ScmObj* %ae44188, %struct.ScmObj** %stackaddr$makeclosure49181, align 8
%args48221$ae44186$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49183 = alloca %struct.ScmObj*, align 8
%args48221$ae44186$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44188, %struct.ScmObj* %args48221$ae44186$0)
store volatile %struct.ScmObj* %args48221$ae44186$1, %struct.ScmObj** %stackaddr$prim49183, align 8
%stackaddr$prim49184 = alloca %struct.ScmObj*, align 8
%args48221$ae44186$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44187, %struct.ScmObj* %args48221$ae44186$1)
store volatile %struct.ScmObj* %args48221$ae44186$2, %struct.ScmObj** %stackaddr$prim49184, align 8
%clofunc49185 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44186)
musttail call tailcc void %clofunc49185(%struct.ScmObj* %ae44186, %struct.ScmObj* %args48221$ae44186$2)
ret void
falsebranch$cmp49178:
%stackaddr$prim49186 = alloca %struct.ScmObj*, align 8
%anf_45bind40367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %llst40232)
store volatile %struct.ScmObj* %anf_45bind40367, %struct.ScmObj** %stackaddr$prim49186, align 8
%ae44220 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49187 = alloca %struct.ScmObj*, align 8
%anf_45bind40368 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lazy_45take40228, %struct.ScmObj* %ae44220)
store volatile %struct.ScmObj* %anf_45bind40368, %struct.ScmObj** %stackaddr$prim49187, align 8
%stackaddr$prim49188 = alloca %struct.ScmObj*, align 8
%thunk4010440234 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %llst40232)
store volatile %struct.ScmObj* %thunk4010440234, %struct.ScmObj** %stackaddr$prim49188, align 8
%stackaddr$makeclosure49189 = alloca %struct.ScmObj*, align 8
%fptrToInt49190 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44223 to i64
%ae44223 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49190)
store volatile %struct.ScmObj* %ae44223, %struct.ScmObj** %stackaddr$makeclosure49189, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44223, %struct.ScmObj* %k40416, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44223, %struct.ScmObj* %anf_45bind40368, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44223, %struct.ScmObj* %n40231, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44223, %struct.ScmObj* %anf_45bind40367, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44223, %struct.ScmObj* %thunk4010440234, i64 4)
%args48265$promise_6340226$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49191 = alloca %struct.ScmObj*, align 8
%args48265$promise_6340226$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4010440234, %struct.ScmObj* %args48265$promise_6340226$0)
store volatile %struct.ScmObj* %args48265$promise_6340226$1, %struct.ScmObj** %stackaddr$prim49191, align 8
%stackaddr$prim49192 = alloca %struct.ScmObj*, align 8
%args48265$promise_6340226$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44223, %struct.ScmObj* %args48265$promise_6340226$1)
store volatile %struct.ScmObj* %args48265$promise_6340226$2, %struct.ScmObj** %stackaddr$prim49192, align 8
%clofunc49193 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %promise_6340226)
musttail call tailcc void %clofunc49193(%struct.ScmObj* %promise_6340226, %struct.ScmObj* %args48265$promise_6340226$2)
ret void
}

define tailcc void @proc_clo$ae44186(%struct.ScmObj* %env$ae44186,%struct.ScmObj* %current_45args48216) {
%stackaddr$env-ref49194 = alloca %struct.ScmObj*, align 8
%k40416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44186, i64 0)
store %struct.ScmObj* %k40416, %struct.ScmObj** %stackaddr$env-ref49194
%stackaddr$prim49195 = alloca %struct.ScmObj*, align 8
%_95k40417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48216)
store volatile %struct.ScmObj* %_95k40417, %struct.ScmObj** %stackaddr$prim49195, align 8
%stackaddr$prim49196 = alloca %struct.ScmObj*, align 8
%current_45args48217 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48216)
store volatile %struct.ScmObj* %current_45args48217, %struct.ScmObj** %stackaddr$prim49196, align 8
%stackaddr$prim49197 = alloca %struct.ScmObj*, align 8
%anf_45bind40366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48217)
store volatile %struct.ScmObj* %anf_45bind40366, %struct.ScmObj** %stackaddr$prim49197, align 8
%args48219$anf_45bind40366$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49198 = alloca %struct.ScmObj*, align 8
%args48219$anf_45bind40366$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40416, %struct.ScmObj* %args48219$anf_45bind40366$0)
store volatile %struct.ScmObj* %args48219$anf_45bind40366$1, %struct.ScmObj** %stackaddr$prim49198, align 8
%clofunc49199 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40366)
musttail call tailcc void %clofunc49199(%struct.ScmObj* %anf_45bind40366, %struct.ScmObj* %args48219$anf_45bind40366$1)
ret void
}

define tailcc void @proc_clo$ae44188(%struct.ScmObj* %env$ae44188,%struct.ScmObj* %lst4023340418) {
%stackaddr$prim49200 = alloca %struct.ScmObj*, align 8
%k40419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4023340418)
store volatile %struct.ScmObj* %k40419, %struct.ScmObj** %stackaddr$prim49200, align 8
%stackaddr$prim49201 = alloca %struct.ScmObj*, align 8
%lst40233 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4023340418)
store volatile %struct.ScmObj* %lst40233, %struct.ScmObj** %stackaddr$prim49201, align 8
%ae44192 = call %struct.ScmObj* @const_init_int(i64 0)
%args48220$k40419$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49202 = alloca %struct.ScmObj*, align 8
%args48220$k40419$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40233, %struct.ScmObj* %args48220$k40419$0)
store volatile %struct.ScmObj* %args48220$k40419$1, %struct.ScmObj** %stackaddr$prim49202, align 8
%stackaddr$prim49203 = alloca %struct.ScmObj*, align 8
%args48220$k40419$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44192, %struct.ScmObj* %args48220$k40419$1)
store volatile %struct.ScmObj* %args48220$k40419$2, %struct.ScmObj** %stackaddr$prim49203, align 8
%clofunc49204 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40419)
musttail call tailcc void %clofunc49204(%struct.ScmObj* %k40419, %struct.ScmObj* %args48220$k40419$2)
ret void
}

define tailcc void @proc_clo$ae44223(%struct.ScmObj* %env$ae44223,%struct.ScmObj* %current_45args48222) {
%stackaddr$env-ref49205 = alloca %struct.ScmObj*, align 8
%k40416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44223, i64 0)
store %struct.ScmObj* %k40416, %struct.ScmObj** %stackaddr$env-ref49205
%stackaddr$env-ref49206 = alloca %struct.ScmObj*, align 8
%anf_45bind40368 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44223, i64 1)
store %struct.ScmObj* %anf_45bind40368, %struct.ScmObj** %stackaddr$env-ref49206
%stackaddr$env-ref49207 = alloca %struct.ScmObj*, align 8
%n40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44223, i64 2)
store %struct.ScmObj* %n40231, %struct.ScmObj** %stackaddr$env-ref49207
%stackaddr$env-ref49208 = alloca %struct.ScmObj*, align 8
%anf_45bind40367 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44223, i64 3)
store %struct.ScmObj* %anf_45bind40367, %struct.ScmObj** %stackaddr$env-ref49208
%stackaddr$env-ref49209 = alloca %struct.ScmObj*, align 8
%thunk4010440234 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44223, i64 4)
store %struct.ScmObj* %thunk4010440234, %struct.ScmObj** %stackaddr$env-ref49209
%stackaddr$prim49210 = alloca %struct.ScmObj*, align 8
%_95k40420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48222)
store volatile %struct.ScmObj* %_95k40420, %struct.ScmObj** %stackaddr$prim49210, align 8
%stackaddr$prim49211 = alloca %struct.ScmObj*, align 8
%current_45args48223 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48222)
store volatile %struct.ScmObj* %current_45args48223, %struct.ScmObj** %stackaddr$prim49211, align 8
%stackaddr$prim49212 = alloca %struct.ScmObj*, align 8
%anf_45bind40369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48223)
store volatile %struct.ScmObj* %anf_45bind40369, %struct.ScmObj** %stackaddr$prim49212, align 8
%truthy$cmp49213 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40369)
%cmp$cmp49213 = icmp eq i64 %truthy$cmp49213, 1
br i1 %cmp$cmp49213, label %truebranch$cmp49213, label %falsebranch$cmp49213
truebranch$cmp49213:
%ae44227 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49214 = alloca %struct.ScmObj*, align 8
%anf_45bind40370 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4010440234, %struct.ScmObj* %ae44227)
store volatile %struct.ScmObj* %anf_45bind40370, %struct.ScmObj** %stackaddr$prim49214, align 8
%truthy$cmp49215 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40370)
%cmp$cmp49215 = icmp eq i64 %truthy$cmp49215, 1
br i1 %cmp$cmp49215, label %truebranch$cmp49215, label %falsebranch$cmp49215
truebranch$cmp49215:
%ae44230 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49216 = alloca %struct.ScmObj*, align 8
%cpsprim40424 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4010440234, %struct.ScmObj* %ae44230)
store volatile %struct.ScmObj* %cpsprim40424, %struct.ScmObj** %stackaddr$prim49216, align 8
%stackaddr$makeclosure49217 = alloca %struct.ScmObj*, align 8
%fptrToInt49218 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44231 to i64
%ae44231 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49218)
store volatile %struct.ScmObj* %ae44231, %struct.ScmObj** %stackaddr$makeclosure49217, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44231, %struct.ScmObj* %k40416, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44231, %struct.ScmObj* %anf_45bind40368, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44231, %struct.ScmObj* %n40231, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44231, %struct.ScmObj* %anf_45bind40367, i64 3)
%ae44232 = call %struct.ScmObj* @const_init_int(i64 0)
%args48233$ae44231$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49219 = alloca %struct.ScmObj*, align 8
%args48233$ae44231$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40424, %struct.ScmObj* %args48233$ae44231$0)
store volatile %struct.ScmObj* %args48233$ae44231$1, %struct.ScmObj** %stackaddr$prim49219, align 8
%stackaddr$prim49220 = alloca %struct.ScmObj*, align 8
%args48233$ae44231$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44232, %struct.ScmObj* %args48233$ae44231$1)
store volatile %struct.ScmObj* %args48233$ae44231$2, %struct.ScmObj** %stackaddr$prim49220, align 8
%clofunc49221 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44231)
musttail call tailcc void %clofunc49221(%struct.ScmObj* %ae44231, %struct.ScmObj* %args48233$ae44231$2)
ret void
falsebranch$cmp49215:
%ae44286 = call %struct.ScmObj* @const_init_int(i64 1)
%ae44287 = call %struct.ScmObj* @const_init_true()
%stackaddr$prim49222 = alloca %struct.ScmObj*, align 8
%t4010640235 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4010440234, %struct.ScmObj* %ae44286, %struct.ScmObj* %ae44287)
store volatile %struct.ScmObj* %t4010640235, %struct.ScmObj** %stackaddr$prim49222, align 8
%ae44289 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49223 = alloca %struct.ScmObj*, align 8
%anf_45bind40371 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4010440234, %struct.ScmObj* %ae44289)
store volatile %struct.ScmObj* %anf_45bind40371, %struct.ScmObj** %stackaddr$prim49223, align 8
%stackaddr$makeclosure49224 = alloca %struct.ScmObj*, align 8
%fptrToInt49225 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44291 to i64
%ae44291 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49225)
store volatile %struct.ScmObj* %ae44291, %struct.ScmObj** %stackaddr$makeclosure49224, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44291, %struct.ScmObj* %k40416, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44291, %struct.ScmObj* %anf_45bind40368, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44291, %struct.ScmObj* %n40231, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44291, %struct.ScmObj* %anf_45bind40367, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44291, %struct.ScmObj* %thunk4010440234, i64 4)
%ae44292 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @global$sym$ae4429249226, i32 0, i32 0))
%args48246$anf_45bind40371$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49227 = alloca %struct.ScmObj*, align 8
%args48246$anf_45bind40371$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44292, %struct.ScmObj* %args48246$anf_45bind40371$0)
store volatile %struct.ScmObj* %args48246$anf_45bind40371$1, %struct.ScmObj** %stackaddr$prim49227, align 8
%stackaddr$prim49228 = alloca %struct.ScmObj*, align 8
%args48246$anf_45bind40371$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44291, %struct.ScmObj* %args48246$anf_45bind40371$1)
store volatile %struct.ScmObj* %args48246$anf_45bind40371$2, %struct.ScmObj** %stackaddr$prim49228, align 8
%clofunc49229 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40371)
musttail call tailcc void %clofunc49229(%struct.ScmObj* %anf_45bind40371, %struct.ScmObj* %args48246$anf_45bind40371$2)
ret void
falsebranch$cmp49213:
%stackaddr$prim49230 = alloca %struct.ScmObj*, align 8
%anf_45bind40372 = call %struct.ScmObj* @prim_number_63(%struct.ScmObj* %thunk4010440234)
store volatile %struct.ScmObj* %anf_45bind40372, %struct.ScmObj** %stackaddr$prim49230, align 8
%truthy$cmp49231 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40372)
%cmp$cmp49231 = icmp eq i64 %truthy$cmp49231, 1
br i1 %cmp$cmp49231, label %truebranch$cmp49231, label %falsebranch$cmp49231
truebranch$cmp49231:
%stackaddr$makeclosure49232 = alloca %struct.ScmObj*, align 8
%fptrToInt49233 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44503 to i64
%ae44503 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49233)
store volatile %struct.ScmObj* %ae44503, %struct.ScmObj** %stackaddr$makeclosure49232, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44503, %struct.ScmObj* %k40416, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44503, %struct.ScmObj* %anf_45bind40368, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44503, %struct.ScmObj* %n40231, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44503, %struct.ScmObj* %anf_45bind40367, i64 3)
%ae44504 = call %struct.ScmObj* @const_init_int(i64 0)
%args48255$ae44503$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49234 = alloca %struct.ScmObj*, align 8
%args48255$ae44503$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4010440234, %struct.ScmObj* %args48255$ae44503$0)
store volatile %struct.ScmObj* %args48255$ae44503$1, %struct.ScmObj** %stackaddr$prim49234, align 8
%stackaddr$prim49235 = alloca %struct.ScmObj*, align 8
%args48255$ae44503$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44504, %struct.ScmObj* %args48255$ae44503$1)
store volatile %struct.ScmObj* %args48255$ae44503$2, %struct.ScmObj** %stackaddr$prim49235, align 8
%clofunc49236 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44503)
musttail call tailcc void %clofunc49236(%struct.ScmObj* %ae44503, %struct.ScmObj* %args48255$ae44503$2)
ret void
falsebranch$cmp49231:
%stackaddr$makeclosure49237 = alloca %struct.ScmObj*, align 8
%fptrToInt49238 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44541 to i64
%ae44541 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49238)
store volatile %struct.ScmObj* %ae44541, %struct.ScmObj** %stackaddr$makeclosure49237, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44541, %struct.ScmObj* %k40416, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44541, %struct.ScmObj* %anf_45bind40368, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44541, %struct.ScmObj* %n40231, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44541, %struct.ScmObj* %anf_45bind40367, i64 3)
%ae44542 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44543 = call %struct.ScmObj* @const_init_string(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @global$str$ae4454349239, i32 0, i32 0))
%args48264$ae44541$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49240 = alloca %struct.ScmObj*, align 8
%args48264$ae44541$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44543, %struct.ScmObj* %args48264$ae44541$0)
store volatile %struct.ScmObj* %args48264$ae44541$1, %struct.ScmObj** %stackaddr$prim49240, align 8
%stackaddr$prim49241 = alloca %struct.ScmObj*, align 8
%args48264$ae44541$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44542, %struct.ScmObj* %args48264$ae44541$1)
store volatile %struct.ScmObj* %args48264$ae44541$2, %struct.ScmObj** %stackaddr$prim49241, align 8
%clofunc49242 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44541)
musttail call tailcc void %clofunc49242(%struct.ScmObj* %ae44541, %struct.ScmObj* %args48264$ae44541$2)
ret void
}

define tailcc void @proc_clo$ae44231(%struct.ScmObj* %env$ae44231,%struct.ScmObj* %current_45args48225) {
%stackaddr$env-ref49243 = alloca %struct.ScmObj*, align 8
%k40416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44231, i64 0)
store %struct.ScmObj* %k40416, %struct.ScmObj** %stackaddr$env-ref49243
%stackaddr$env-ref49244 = alloca %struct.ScmObj*, align 8
%anf_45bind40368 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44231, i64 1)
store %struct.ScmObj* %anf_45bind40368, %struct.ScmObj** %stackaddr$env-ref49244
%stackaddr$env-ref49245 = alloca %struct.ScmObj*, align 8
%n40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44231, i64 2)
store %struct.ScmObj* %n40231, %struct.ScmObj** %stackaddr$env-ref49245
%stackaddr$env-ref49246 = alloca %struct.ScmObj*, align 8
%anf_45bind40367 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44231, i64 3)
store %struct.ScmObj* %anf_45bind40367, %struct.ScmObj** %stackaddr$env-ref49246
%stackaddr$prim49247 = alloca %struct.ScmObj*, align 8
%_95k40421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48225)
store volatile %struct.ScmObj* %_95k40421, %struct.ScmObj** %stackaddr$prim49247, align 8
%stackaddr$prim49248 = alloca %struct.ScmObj*, align 8
%current_45args48226 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48225)
store volatile %struct.ScmObj* %current_45args48226, %struct.ScmObj** %stackaddr$prim49248, align 8
%stackaddr$prim49249 = alloca %struct.ScmObj*, align 8
%anf_45bind40373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48226)
store volatile %struct.ScmObj* %anf_45bind40373, %struct.ScmObj** %stackaddr$prim49249, align 8
%ae44238 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49250 = alloca %struct.ScmObj*, align 8
%anf_45bind40374 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n40231, %struct.ScmObj* %ae44238)
store volatile %struct.ScmObj* %anf_45bind40374, %struct.ScmObj** %stackaddr$prim49250, align 8
%stackaddr$makeclosure49251 = alloca %struct.ScmObj*, align 8
%fptrToInt49252 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44240 to i64
%ae44240 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49252)
store volatile %struct.ScmObj* %ae44240, %struct.ScmObj** %stackaddr$makeclosure49251, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44240, %struct.ScmObj* %k40416, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44240, %struct.ScmObj* %anf_45bind40367, i64 1)
%args48232$anf_45bind40368$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49253 = alloca %struct.ScmObj*, align 8
%args48232$anf_45bind40368$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40374, %struct.ScmObj* %args48232$anf_45bind40368$0)
store volatile %struct.ScmObj* %args48232$anf_45bind40368$1, %struct.ScmObj** %stackaddr$prim49253, align 8
%stackaddr$prim49254 = alloca %struct.ScmObj*, align 8
%args48232$anf_45bind40368$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40373, %struct.ScmObj* %args48232$anf_45bind40368$1)
store volatile %struct.ScmObj* %args48232$anf_45bind40368$2, %struct.ScmObj** %stackaddr$prim49254, align 8
%stackaddr$prim49255 = alloca %struct.ScmObj*, align 8
%args48232$anf_45bind40368$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44240, %struct.ScmObj* %args48232$anf_45bind40368$2)
store volatile %struct.ScmObj* %args48232$anf_45bind40368$3, %struct.ScmObj** %stackaddr$prim49255, align 8
%clofunc49256 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40368)
musttail call tailcc void %clofunc49256(%struct.ScmObj* %anf_45bind40368, %struct.ScmObj* %args48232$anf_45bind40368$3)
ret void
}

define tailcc void @proc_clo$ae44240(%struct.ScmObj* %env$ae44240,%struct.ScmObj* %current_45args48228) {
%stackaddr$env-ref49257 = alloca %struct.ScmObj*, align 8
%k40416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44240, i64 0)
store %struct.ScmObj* %k40416, %struct.ScmObj** %stackaddr$env-ref49257
%stackaddr$env-ref49258 = alloca %struct.ScmObj*, align 8
%anf_45bind40367 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44240, i64 1)
store %struct.ScmObj* %anf_45bind40367, %struct.ScmObj** %stackaddr$env-ref49258
%stackaddr$prim49259 = alloca %struct.ScmObj*, align 8
%_95k40422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48228)
store volatile %struct.ScmObj* %_95k40422, %struct.ScmObj** %stackaddr$prim49259, align 8
%stackaddr$prim49260 = alloca %struct.ScmObj*, align 8
%current_45args48229 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48228)
store volatile %struct.ScmObj* %current_45args48229, %struct.ScmObj** %stackaddr$prim49260, align 8
%stackaddr$prim49261 = alloca %struct.ScmObj*, align 8
%anf_45bind40375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48229)
store volatile %struct.ScmObj* %anf_45bind40375, %struct.ScmObj** %stackaddr$prim49261, align 8
%stackaddr$prim49262 = alloca %struct.ScmObj*, align 8
%cpsprim40423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40367, %struct.ScmObj* %anf_45bind40375)
store volatile %struct.ScmObj* %cpsprim40423, %struct.ScmObj** %stackaddr$prim49262, align 8
%ae44246 = call %struct.ScmObj* @const_init_int(i64 0)
%args48231$k40416$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49263 = alloca %struct.ScmObj*, align 8
%args48231$k40416$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40423, %struct.ScmObj* %args48231$k40416$0)
store volatile %struct.ScmObj* %args48231$k40416$1, %struct.ScmObj** %stackaddr$prim49263, align 8
%stackaddr$prim49264 = alloca %struct.ScmObj*, align 8
%args48231$k40416$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44246, %struct.ScmObj* %args48231$k40416$1)
store volatile %struct.ScmObj* %args48231$k40416$2, %struct.ScmObj** %stackaddr$prim49264, align 8
%clofunc49265 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40416)
musttail call tailcc void %clofunc49265(%struct.ScmObj* %k40416, %struct.ScmObj* %args48231$k40416$2)
ret void
}

define tailcc void @proc_clo$ae44291(%struct.ScmObj* %env$ae44291,%struct.ScmObj* %current_45args48234) {
%stackaddr$env-ref49266 = alloca %struct.ScmObj*, align 8
%k40416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44291, i64 0)
store %struct.ScmObj* %k40416, %struct.ScmObj** %stackaddr$env-ref49266
%stackaddr$env-ref49267 = alloca %struct.ScmObj*, align 8
%anf_45bind40368 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44291, i64 1)
store %struct.ScmObj* %anf_45bind40368, %struct.ScmObj** %stackaddr$env-ref49267
%stackaddr$env-ref49268 = alloca %struct.ScmObj*, align 8
%n40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44291, i64 2)
store %struct.ScmObj* %n40231, %struct.ScmObj** %stackaddr$env-ref49268
%stackaddr$env-ref49269 = alloca %struct.ScmObj*, align 8
%anf_45bind40367 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44291, i64 3)
store %struct.ScmObj* %anf_45bind40367, %struct.ScmObj** %stackaddr$env-ref49269
%stackaddr$env-ref49270 = alloca %struct.ScmObj*, align 8
%thunk4010440234 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44291, i64 4)
store %struct.ScmObj* %thunk4010440234, %struct.ScmObj** %stackaddr$env-ref49270
%stackaddr$prim49271 = alloca %struct.ScmObj*, align 8
%_95k40425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48234)
store volatile %struct.ScmObj* %_95k40425, %struct.ScmObj** %stackaddr$prim49271, align 8
%stackaddr$prim49272 = alloca %struct.ScmObj*, align 8
%current_45args48235 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48234)
store volatile %struct.ScmObj* %current_45args48235, %struct.ScmObj** %stackaddr$prim49272, align 8
%stackaddr$prim49273 = alloca %struct.ScmObj*, align 8
%val4010540237 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48235)
store volatile %struct.ScmObj* %val4010540237, %struct.ScmObj** %stackaddr$prim49273, align 8
%ae44297 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49274 = alloca %struct.ScmObj*, align 8
%t4010740236 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4010440234, %struct.ScmObj* %ae44297, %struct.ScmObj* %val4010540237)
store volatile %struct.ScmObj* %t4010740236, %struct.ScmObj** %stackaddr$prim49274, align 8
%ae44300 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49275 = alloca %struct.ScmObj*, align 8
%cpsprim40426 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4010440234, %struct.ScmObj* %ae44300)
store volatile %struct.ScmObj* %cpsprim40426, %struct.ScmObj** %stackaddr$prim49275, align 8
%stackaddr$makeclosure49276 = alloca %struct.ScmObj*, align 8
%fptrToInt49277 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44301 to i64
%ae44301 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49277)
store volatile %struct.ScmObj* %ae44301, %struct.ScmObj** %stackaddr$makeclosure49276, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44301, %struct.ScmObj* %k40416, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44301, %struct.ScmObj* %anf_45bind40368, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44301, %struct.ScmObj* %n40231, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44301, %struct.ScmObj* %anf_45bind40367, i64 3)
%ae44302 = call %struct.ScmObj* @const_init_int(i64 0)
%args48245$ae44301$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49278 = alloca %struct.ScmObj*, align 8
%args48245$ae44301$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40426, %struct.ScmObj* %args48245$ae44301$0)
store volatile %struct.ScmObj* %args48245$ae44301$1, %struct.ScmObj** %stackaddr$prim49278, align 8
%stackaddr$prim49279 = alloca %struct.ScmObj*, align 8
%args48245$ae44301$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44302, %struct.ScmObj* %args48245$ae44301$1)
store volatile %struct.ScmObj* %args48245$ae44301$2, %struct.ScmObj** %stackaddr$prim49279, align 8
%clofunc49280 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44301)
musttail call tailcc void %clofunc49280(%struct.ScmObj* %ae44301, %struct.ScmObj* %args48245$ae44301$2)
ret void
}

define tailcc void @proc_clo$ae44301(%struct.ScmObj* %env$ae44301,%struct.ScmObj* %current_45args48237) {
%stackaddr$env-ref49281 = alloca %struct.ScmObj*, align 8
%k40416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44301, i64 0)
store %struct.ScmObj* %k40416, %struct.ScmObj** %stackaddr$env-ref49281
%stackaddr$env-ref49282 = alloca %struct.ScmObj*, align 8
%anf_45bind40368 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44301, i64 1)
store %struct.ScmObj* %anf_45bind40368, %struct.ScmObj** %stackaddr$env-ref49282
%stackaddr$env-ref49283 = alloca %struct.ScmObj*, align 8
%n40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44301, i64 2)
store %struct.ScmObj* %n40231, %struct.ScmObj** %stackaddr$env-ref49283
%stackaddr$env-ref49284 = alloca %struct.ScmObj*, align 8
%anf_45bind40367 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44301, i64 3)
store %struct.ScmObj* %anf_45bind40367, %struct.ScmObj** %stackaddr$env-ref49284
%stackaddr$prim49285 = alloca %struct.ScmObj*, align 8
%_95k40421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48237)
store volatile %struct.ScmObj* %_95k40421, %struct.ScmObj** %stackaddr$prim49285, align 8
%stackaddr$prim49286 = alloca %struct.ScmObj*, align 8
%current_45args48238 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48237)
store volatile %struct.ScmObj* %current_45args48238, %struct.ScmObj** %stackaddr$prim49286, align 8
%stackaddr$prim49287 = alloca %struct.ScmObj*, align 8
%anf_45bind40373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48238)
store volatile %struct.ScmObj* %anf_45bind40373, %struct.ScmObj** %stackaddr$prim49287, align 8
%ae44308 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49288 = alloca %struct.ScmObj*, align 8
%anf_45bind40374 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n40231, %struct.ScmObj* %ae44308)
store volatile %struct.ScmObj* %anf_45bind40374, %struct.ScmObj** %stackaddr$prim49288, align 8
%stackaddr$makeclosure49289 = alloca %struct.ScmObj*, align 8
%fptrToInt49290 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44310 to i64
%ae44310 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49290)
store volatile %struct.ScmObj* %ae44310, %struct.ScmObj** %stackaddr$makeclosure49289, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44310, %struct.ScmObj* %k40416, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44310, %struct.ScmObj* %anf_45bind40367, i64 1)
%args48244$anf_45bind40368$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49291 = alloca %struct.ScmObj*, align 8
%args48244$anf_45bind40368$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40374, %struct.ScmObj* %args48244$anf_45bind40368$0)
store volatile %struct.ScmObj* %args48244$anf_45bind40368$1, %struct.ScmObj** %stackaddr$prim49291, align 8
%stackaddr$prim49292 = alloca %struct.ScmObj*, align 8
%args48244$anf_45bind40368$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40373, %struct.ScmObj* %args48244$anf_45bind40368$1)
store volatile %struct.ScmObj* %args48244$anf_45bind40368$2, %struct.ScmObj** %stackaddr$prim49292, align 8
%stackaddr$prim49293 = alloca %struct.ScmObj*, align 8
%args48244$anf_45bind40368$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44310, %struct.ScmObj* %args48244$anf_45bind40368$2)
store volatile %struct.ScmObj* %args48244$anf_45bind40368$3, %struct.ScmObj** %stackaddr$prim49293, align 8
%clofunc49294 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40368)
musttail call tailcc void %clofunc49294(%struct.ScmObj* %anf_45bind40368, %struct.ScmObj* %args48244$anf_45bind40368$3)
ret void
}

define tailcc void @proc_clo$ae44310(%struct.ScmObj* %env$ae44310,%struct.ScmObj* %current_45args48240) {
%stackaddr$env-ref49295 = alloca %struct.ScmObj*, align 8
%k40416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44310, i64 0)
store %struct.ScmObj* %k40416, %struct.ScmObj** %stackaddr$env-ref49295
%stackaddr$env-ref49296 = alloca %struct.ScmObj*, align 8
%anf_45bind40367 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44310, i64 1)
store %struct.ScmObj* %anf_45bind40367, %struct.ScmObj** %stackaddr$env-ref49296
%stackaddr$prim49297 = alloca %struct.ScmObj*, align 8
%_95k40422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48240)
store volatile %struct.ScmObj* %_95k40422, %struct.ScmObj** %stackaddr$prim49297, align 8
%stackaddr$prim49298 = alloca %struct.ScmObj*, align 8
%current_45args48241 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48240)
store volatile %struct.ScmObj* %current_45args48241, %struct.ScmObj** %stackaddr$prim49298, align 8
%stackaddr$prim49299 = alloca %struct.ScmObj*, align 8
%anf_45bind40375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48241)
store volatile %struct.ScmObj* %anf_45bind40375, %struct.ScmObj** %stackaddr$prim49299, align 8
%stackaddr$prim49300 = alloca %struct.ScmObj*, align 8
%cpsprim40423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40367, %struct.ScmObj* %anf_45bind40375)
store volatile %struct.ScmObj* %cpsprim40423, %struct.ScmObj** %stackaddr$prim49300, align 8
%ae44316 = call %struct.ScmObj* @const_init_int(i64 0)
%args48243$k40416$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49301 = alloca %struct.ScmObj*, align 8
%args48243$k40416$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40423, %struct.ScmObj* %args48243$k40416$0)
store volatile %struct.ScmObj* %args48243$k40416$1, %struct.ScmObj** %stackaddr$prim49301, align 8
%stackaddr$prim49302 = alloca %struct.ScmObj*, align 8
%args48243$k40416$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44316, %struct.ScmObj* %args48243$k40416$1)
store volatile %struct.ScmObj* %args48243$k40416$2, %struct.ScmObj** %stackaddr$prim49302, align 8
%clofunc49303 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40416)
musttail call tailcc void %clofunc49303(%struct.ScmObj* %k40416, %struct.ScmObj* %args48243$k40416$2)
ret void
}

define tailcc void @proc_clo$ae44503(%struct.ScmObj* %env$ae44503,%struct.ScmObj* %current_45args48247) {
%stackaddr$env-ref49304 = alloca %struct.ScmObj*, align 8
%k40416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44503, i64 0)
store %struct.ScmObj* %k40416, %struct.ScmObj** %stackaddr$env-ref49304
%stackaddr$env-ref49305 = alloca %struct.ScmObj*, align 8
%anf_45bind40368 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44503, i64 1)
store %struct.ScmObj* %anf_45bind40368, %struct.ScmObj** %stackaddr$env-ref49305
%stackaddr$env-ref49306 = alloca %struct.ScmObj*, align 8
%n40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44503, i64 2)
store %struct.ScmObj* %n40231, %struct.ScmObj** %stackaddr$env-ref49306
%stackaddr$env-ref49307 = alloca %struct.ScmObj*, align 8
%anf_45bind40367 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44503, i64 3)
store %struct.ScmObj* %anf_45bind40367, %struct.ScmObj** %stackaddr$env-ref49307
%stackaddr$prim49308 = alloca %struct.ScmObj*, align 8
%_95k40421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48247)
store volatile %struct.ScmObj* %_95k40421, %struct.ScmObj** %stackaddr$prim49308, align 8
%stackaddr$prim49309 = alloca %struct.ScmObj*, align 8
%current_45args48248 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48247)
store volatile %struct.ScmObj* %current_45args48248, %struct.ScmObj** %stackaddr$prim49309, align 8
%stackaddr$prim49310 = alloca %struct.ScmObj*, align 8
%anf_45bind40373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48248)
store volatile %struct.ScmObj* %anf_45bind40373, %struct.ScmObj** %stackaddr$prim49310, align 8
%ae44510 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49311 = alloca %struct.ScmObj*, align 8
%anf_45bind40374 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n40231, %struct.ScmObj* %ae44510)
store volatile %struct.ScmObj* %anf_45bind40374, %struct.ScmObj** %stackaddr$prim49311, align 8
%stackaddr$makeclosure49312 = alloca %struct.ScmObj*, align 8
%fptrToInt49313 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44512 to i64
%ae44512 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49313)
store volatile %struct.ScmObj* %ae44512, %struct.ScmObj** %stackaddr$makeclosure49312, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44512, %struct.ScmObj* %k40416, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44512, %struct.ScmObj* %anf_45bind40367, i64 1)
%args48254$anf_45bind40368$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49314 = alloca %struct.ScmObj*, align 8
%args48254$anf_45bind40368$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40374, %struct.ScmObj* %args48254$anf_45bind40368$0)
store volatile %struct.ScmObj* %args48254$anf_45bind40368$1, %struct.ScmObj** %stackaddr$prim49314, align 8
%stackaddr$prim49315 = alloca %struct.ScmObj*, align 8
%args48254$anf_45bind40368$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40373, %struct.ScmObj* %args48254$anf_45bind40368$1)
store volatile %struct.ScmObj* %args48254$anf_45bind40368$2, %struct.ScmObj** %stackaddr$prim49315, align 8
%stackaddr$prim49316 = alloca %struct.ScmObj*, align 8
%args48254$anf_45bind40368$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44512, %struct.ScmObj* %args48254$anf_45bind40368$2)
store volatile %struct.ScmObj* %args48254$anf_45bind40368$3, %struct.ScmObj** %stackaddr$prim49316, align 8
%clofunc49317 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40368)
musttail call tailcc void %clofunc49317(%struct.ScmObj* %anf_45bind40368, %struct.ScmObj* %args48254$anf_45bind40368$3)
ret void
}

define tailcc void @proc_clo$ae44512(%struct.ScmObj* %env$ae44512,%struct.ScmObj* %current_45args48250) {
%stackaddr$env-ref49318 = alloca %struct.ScmObj*, align 8
%k40416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44512, i64 0)
store %struct.ScmObj* %k40416, %struct.ScmObj** %stackaddr$env-ref49318
%stackaddr$env-ref49319 = alloca %struct.ScmObj*, align 8
%anf_45bind40367 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44512, i64 1)
store %struct.ScmObj* %anf_45bind40367, %struct.ScmObj** %stackaddr$env-ref49319
%stackaddr$prim49320 = alloca %struct.ScmObj*, align 8
%_95k40422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48250)
store volatile %struct.ScmObj* %_95k40422, %struct.ScmObj** %stackaddr$prim49320, align 8
%stackaddr$prim49321 = alloca %struct.ScmObj*, align 8
%current_45args48251 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48250)
store volatile %struct.ScmObj* %current_45args48251, %struct.ScmObj** %stackaddr$prim49321, align 8
%stackaddr$prim49322 = alloca %struct.ScmObj*, align 8
%anf_45bind40375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48251)
store volatile %struct.ScmObj* %anf_45bind40375, %struct.ScmObj** %stackaddr$prim49322, align 8
%stackaddr$prim49323 = alloca %struct.ScmObj*, align 8
%cpsprim40423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40367, %struct.ScmObj* %anf_45bind40375)
store volatile %struct.ScmObj* %cpsprim40423, %struct.ScmObj** %stackaddr$prim49323, align 8
%ae44518 = call %struct.ScmObj* @const_init_int(i64 0)
%args48253$k40416$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49324 = alloca %struct.ScmObj*, align 8
%args48253$k40416$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40423, %struct.ScmObj* %args48253$k40416$0)
store volatile %struct.ScmObj* %args48253$k40416$1, %struct.ScmObj** %stackaddr$prim49324, align 8
%stackaddr$prim49325 = alloca %struct.ScmObj*, align 8
%args48253$k40416$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44518, %struct.ScmObj* %args48253$k40416$1)
store volatile %struct.ScmObj* %args48253$k40416$2, %struct.ScmObj** %stackaddr$prim49325, align 8
%clofunc49326 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40416)
musttail call tailcc void %clofunc49326(%struct.ScmObj* %k40416, %struct.ScmObj* %args48253$k40416$2)
ret void
}

define tailcc void @proc_clo$ae44541(%struct.ScmObj* %env$ae44541,%struct.ScmObj* %current_45args48256) {
%stackaddr$env-ref49327 = alloca %struct.ScmObj*, align 8
%k40416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44541, i64 0)
store %struct.ScmObj* %k40416, %struct.ScmObj** %stackaddr$env-ref49327
%stackaddr$env-ref49328 = alloca %struct.ScmObj*, align 8
%anf_45bind40368 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44541, i64 1)
store %struct.ScmObj* %anf_45bind40368, %struct.ScmObj** %stackaddr$env-ref49328
%stackaddr$env-ref49329 = alloca %struct.ScmObj*, align 8
%n40231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44541, i64 2)
store %struct.ScmObj* %n40231, %struct.ScmObj** %stackaddr$env-ref49329
%stackaddr$env-ref49330 = alloca %struct.ScmObj*, align 8
%anf_45bind40367 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44541, i64 3)
store %struct.ScmObj* %anf_45bind40367, %struct.ScmObj** %stackaddr$env-ref49330
%stackaddr$prim49331 = alloca %struct.ScmObj*, align 8
%_95k40421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48256)
store volatile %struct.ScmObj* %_95k40421, %struct.ScmObj** %stackaddr$prim49331, align 8
%stackaddr$prim49332 = alloca %struct.ScmObj*, align 8
%current_45args48257 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48256)
store volatile %struct.ScmObj* %current_45args48257, %struct.ScmObj** %stackaddr$prim49332, align 8
%stackaddr$prim49333 = alloca %struct.ScmObj*, align 8
%anf_45bind40373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48257)
store volatile %struct.ScmObj* %anf_45bind40373, %struct.ScmObj** %stackaddr$prim49333, align 8
%ae44551 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49334 = alloca %struct.ScmObj*, align 8
%anf_45bind40374 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n40231, %struct.ScmObj* %ae44551)
store volatile %struct.ScmObj* %anf_45bind40374, %struct.ScmObj** %stackaddr$prim49334, align 8
%stackaddr$makeclosure49335 = alloca %struct.ScmObj*, align 8
%fptrToInt49336 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44553 to i64
%ae44553 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49336)
store volatile %struct.ScmObj* %ae44553, %struct.ScmObj** %stackaddr$makeclosure49335, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44553, %struct.ScmObj* %k40416, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44553, %struct.ScmObj* %anf_45bind40367, i64 1)
%args48263$anf_45bind40368$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49337 = alloca %struct.ScmObj*, align 8
%args48263$anf_45bind40368$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40374, %struct.ScmObj* %args48263$anf_45bind40368$0)
store volatile %struct.ScmObj* %args48263$anf_45bind40368$1, %struct.ScmObj** %stackaddr$prim49337, align 8
%stackaddr$prim49338 = alloca %struct.ScmObj*, align 8
%args48263$anf_45bind40368$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40373, %struct.ScmObj* %args48263$anf_45bind40368$1)
store volatile %struct.ScmObj* %args48263$anf_45bind40368$2, %struct.ScmObj** %stackaddr$prim49338, align 8
%stackaddr$prim49339 = alloca %struct.ScmObj*, align 8
%args48263$anf_45bind40368$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44553, %struct.ScmObj* %args48263$anf_45bind40368$2)
store volatile %struct.ScmObj* %args48263$anf_45bind40368$3, %struct.ScmObj** %stackaddr$prim49339, align 8
%clofunc49340 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40368)
musttail call tailcc void %clofunc49340(%struct.ScmObj* %anf_45bind40368, %struct.ScmObj* %args48263$anf_45bind40368$3)
ret void
}

define tailcc void @proc_clo$ae44553(%struct.ScmObj* %env$ae44553,%struct.ScmObj* %current_45args48259) {
%stackaddr$env-ref49341 = alloca %struct.ScmObj*, align 8
%k40416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44553, i64 0)
store %struct.ScmObj* %k40416, %struct.ScmObj** %stackaddr$env-ref49341
%stackaddr$env-ref49342 = alloca %struct.ScmObj*, align 8
%anf_45bind40367 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44553, i64 1)
store %struct.ScmObj* %anf_45bind40367, %struct.ScmObj** %stackaddr$env-ref49342
%stackaddr$prim49343 = alloca %struct.ScmObj*, align 8
%_95k40422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48259)
store volatile %struct.ScmObj* %_95k40422, %struct.ScmObj** %stackaddr$prim49343, align 8
%stackaddr$prim49344 = alloca %struct.ScmObj*, align 8
%current_45args48260 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48259)
store volatile %struct.ScmObj* %current_45args48260, %struct.ScmObj** %stackaddr$prim49344, align 8
%stackaddr$prim49345 = alloca %struct.ScmObj*, align 8
%anf_45bind40375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48260)
store volatile %struct.ScmObj* %anf_45bind40375, %struct.ScmObj** %stackaddr$prim49345, align 8
%stackaddr$prim49346 = alloca %struct.ScmObj*, align 8
%cpsprim40423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40367, %struct.ScmObj* %anf_45bind40375)
store volatile %struct.ScmObj* %cpsprim40423, %struct.ScmObj** %stackaddr$prim49346, align 8
%ae44559 = call %struct.ScmObj* @const_init_int(i64 0)
%args48262$k40416$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49347 = alloca %struct.ScmObj*, align 8
%args48262$k40416$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40423, %struct.ScmObj* %args48262$k40416$0)
store volatile %struct.ScmObj* %args48262$k40416$1, %struct.ScmObj** %stackaddr$prim49347, align 8
%stackaddr$prim49348 = alloca %struct.ScmObj*, align 8
%args48262$k40416$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44559, %struct.ScmObj* %args48262$k40416$1)
store volatile %struct.ScmObj* %args48262$k40416$2, %struct.ScmObj** %stackaddr$prim49348, align 8
%clofunc49349 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40416)
musttail call tailcc void %clofunc49349(%struct.ScmObj* %k40416, %struct.ScmObj* %args48262$k40416$2)
ret void
}

define tailcc void @proc_clo$ae44035(%struct.ScmObj* %env$ae44035,%struct.ScmObj* %current_45args48267) {
%stackaddr$env-ref49350 = alloca %struct.ScmObj*, align 8
%gen40229 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44035, i64 0)
store %struct.ScmObj* %gen40229, %struct.ScmObj** %stackaddr$env-ref49350
%stackaddr$prim49351 = alloca %struct.ScmObj*, align 8
%k40427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48267)
store volatile %struct.ScmObj* %k40427, %struct.ScmObj** %stackaddr$prim49351, align 8
%stackaddr$prim49352 = alloca %struct.ScmObj*, align 8
%current_45args48268 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48267)
store volatile %struct.ScmObj* %current_45args48268, %struct.ScmObj** %stackaddr$prim49352, align 8
%stackaddr$prim49353 = alloca %struct.ScmObj*, align 8
%n40239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48268)
store volatile %struct.ScmObj* %n40239, %struct.ScmObj** %stackaddr$prim49353, align 8
%stackaddr$makeclosure49354 = alloca %struct.ScmObj*, align 8
%fptrToInt49355 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44036 to i64
%ae44036 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49355)
store volatile %struct.ScmObj* %ae44036, %struct.ScmObj** %stackaddr$makeclosure49354, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44036, %struct.ScmObj* %gen40229, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44036, %struct.ScmObj* %n40239, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44036, %struct.ScmObj* %k40427, i64 2)
%ae44037 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49356 = alloca %struct.ScmObj*, align 8
%fptrToInt49357 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44038 to i64
%ae44038 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49357)
store volatile %struct.ScmObj* %ae44038, %struct.ScmObj** %stackaddr$makeclosure49356, align 8
%args48287$ae44036$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49358 = alloca %struct.ScmObj*, align 8
%args48287$ae44036$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44038, %struct.ScmObj* %args48287$ae44036$0)
store volatile %struct.ScmObj* %args48287$ae44036$1, %struct.ScmObj** %stackaddr$prim49358, align 8
%stackaddr$prim49359 = alloca %struct.ScmObj*, align 8
%args48287$ae44036$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44037, %struct.ScmObj* %args48287$ae44036$1)
store volatile %struct.ScmObj* %args48287$ae44036$2, %struct.ScmObj** %stackaddr$prim49359, align 8
%clofunc49360 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44036)
musttail call tailcc void %clofunc49360(%struct.ScmObj* %ae44036, %struct.ScmObj* %args48287$ae44036$2)
ret void
}

define tailcc void @proc_clo$ae44036(%struct.ScmObj* %env$ae44036,%struct.ScmObj* %current_45args48270) {
%stackaddr$env-ref49361 = alloca %struct.ScmObj*, align 8
%gen40229 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44036, i64 0)
store %struct.ScmObj* %gen40229, %struct.ScmObj** %stackaddr$env-ref49361
%stackaddr$env-ref49362 = alloca %struct.ScmObj*, align 8
%n40239 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44036, i64 1)
store %struct.ScmObj* %n40239, %struct.ScmObj** %stackaddr$env-ref49362
%stackaddr$env-ref49363 = alloca %struct.ScmObj*, align 8
%k40427 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44036, i64 2)
store %struct.ScmObj* %k40427, %struct.ScmObj** %stackaddr$env-ref49363
%stackaddr$prim49364 = alloca %struct.ScmObj*, align 8
%_95k40428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48270)
store volatile %struct.ScmObj* %_95k40428, %struct.ScmObj** %stackaddr$prim49364, align 8
%stackaddr$prim49365 = alloca %struct.ScmObj*, align 8
%current_45args48271 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48270)
store volatile %struct.ScmObj* %current_45args48271, %struct.ScmObj** %stackaddr$prim49365, align 8
%stackaddr$prim49366 = alloca %struct.ScmObj*, align 8
%anf_45bind40359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48271)
store volatile %struct.ScmObj* %anf_45bind40359, %struct.ScmObj** %stackaddr$prim49366, align 8
%stackaddr$makeclosure49367 = alloca %struct.ScmObj*, align 8
%fptrToInt49368 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44060 to i64
%ae44060 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49368)
store volatile %struct.ScmObj* %ae44060, %struct.ScmObj** %stackaddr$makeclosure49367, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44060, %struct.ScmObj* %anf_45bind40359, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44060, %struct.ScmObj* %n40239, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44060, %struct.ScmObj* %k40427, i64 2)
%ae44061 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49369 = alloca %struct.ScmObj*, align 8
%fptrToInt49370 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44062 to i64
%ae44062 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49370)
store volatile %struct.ScmObj* %ae44062, %struct.ScmObj** %stackaddr$makeclosure49369, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44062, %struct.ScmObj* %gen40229, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44062, %struct.ScmObj* %n40239, i64 1)
%args48285$ae44060$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49371 = alloca %struct.ScmObj*, align 8
%args48285$ae44060$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44062, %struct.ScmObj* %args48285$ae44060$0)
store volatile %struct.ScmObj* %args48285$ae44060$1, %struct.ScmObj** %stackaddr$prim49371, align 8
%stackaddr$prim49372 = alloca %struct.ScmObj*, align 8
%args48285$ae44060$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44061, %struct.ScmObj* %args48285$ae44060$1)
store volatile %struct.ScmObj* %args48285$ae44060$2, %struct.ScmObj** %stackaddr$prim49372, align 8
%clofunc49373 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44060)
musttail call tailcc void %clofunc49373(%struct.ScmObj* %ae44060, %struct.ScmObj* %args48285$ae44060$2)
ret void
}

define tailcc void @proc_clo$ae44060(%struct.ScmObj* %env$ae44060,%struct.ScmObj* %current_45args48273) {
%stackaddr$env-ref49374 = alloca %struct.ScmObj*, align 8
%anf_45bind40359 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44060, i64 0)
store %struct.ScmObj* %anf_45bind40359, %struct.ScmObj** %stackaddr$env-ref49374
%stackaddr$env-ref49375 = alloca %struct.ScmObj*, align 8
%n40239 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44060, i64 1)
store %struct.ScmObj* %n40239, %struct.ScmObj** %stackaddr$env-ref49375
%stackaddr$env-ref49376 = alloca %struct.ScmObj*, align 8
%k40427 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44060, i64 2)
store %struct.ScmObj* %k40427, %struct.ScmObj** %stackaddr$env-ref49376
%stackaddr$prim49377 = alloca %struct.ScmObj*, align 8
%_95k40429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48273)
store volatile %struct.ScmObj* %_95k40429, %struct.ScmObj** %stackaddr$prim49377, align 8
%stackaddr$prim49378 = alloca %struct.ScmObj*, align 8
%current_45args48274 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48273)
store volatile %struct.ScmObj* %current_45args48274, %struct.ScmObj** %stackaddr$prim49378, align 8
%stackaddr$prim49379 = alloca %struct.ScmObj*, align 8
%anf_45bind40362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48274)
store volatile %struct.ScmObj* %anf_45bind40362, %struct.ScmObj** %stackaddr$prim49379, align 8
%stackaddr$makeclosure49380 = alloca %struct.ScmObj*, align 8
%fptrToInt49381 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44096 to i64
%ae44096 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49381)
store volatile %struct.ScmObj* %ae44096, %struct.ScmObj** %stackaddr$makeclosure49380, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44096, %struct.ScmObj* %n40239, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44096, %struct.ScmObj* %k40427, i64 1)
%ae44097 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae4409749382, i32 0, i32 0))
%ae44098 = call %struct.ScmObj* @const_init_false()
%args48280$anf_45bind40359$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49383 = alloca %struct.ScmObj*, align 8
%args48280$anf_45bind40359$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40362, %struct.ScmObj* %args48280$anf_45bind40359$0)
store volatile %struct.ScmObj* %args48280$anf_45bind40359$1, %struct.ScmObj** %stackaddr$prim49383, align 8
%stackaddr$prim49384 = alloca %struct.ScmObj*, align 8
%args48280$anf_45bind40359$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44098, %struct.ScmObj* %args48280$anf_45bind40359$1)
store volatile %struct.ScmObj* %args48280$anf_45bind40359$2, %struct.ScmObj** %stackaddr$prim49384, align 8
%stackaddr$prim49385 = alloca %struct.ScmObj*, align 8
%args48280$anf_45bind40359$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44097, %struct.ScmObj* %args48280$anf_45bind40359$2)
store volatile %struct.ScmObj* %args48280$anf_45bind40359$3, %struct.ScmObj** %stackaddr$prim49385, align 8
%stackaddr$prim49386 = alloca %struct.ScmObj*, align 8
%args48280$anf_45bind40359$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44096, %struct.ScmObj* %args48280$anf_45bind40359$3)
store volatile %struct.ScmObj* %args48280$anf_45bind40359$4, %struct.ScmObj** %stackaddr$prim49386, align 8
%clofunc49387 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40359)
musttail call tailcc void %clofunc49387(%struct.ScmObj* %anf_45bind40359, %struct.ScmObj* %args48280$anf_45bind40359$4)
ret void
}

define tailcc void @proc_clo$ae44096(%struct.ScmObj* %env$ae44096,%struct.ScmObj* %current_45args48276) {
%stackaddr$env-ref49388 = alloca %struct.ScmObj*, align 8
%n40239 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44096, i64 0)
store %struct.ScmObj* %n40239, %struct.ScmObj** %stackaddr$env-ref49388
%stackaddr$env-ref49389 = alloca %struct.ScmObj*, align 8
%k40427 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44096, i64 1)
store %struct.ScmObj* %k40427, %struct.ScmObj** %stackaddr$env-ref49389
%stackaddr$prim49390 = alloca %struct.ScmObj*, align 8
%_95k40430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48276)
store volatile %struct.ScmObj* %_95k40430, %struct.ScmObj** %stackaddr$prim49390, align 8
%stackaddr$prim49391 = alloca %struct.ScmObj*, align 8
%current_45args48277 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48276)
store volatile %struct.ScmObj* %current_45args48277, %struct.ScmObj** %stackaddr$prim49391, align 8
%stackaddr$prim49392 = alloca %struct.ScmObj*, align 8
%anf_45bind40363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48277)
store volatile %struct.ScmObj* %anf_45bind40363, %struct.ScmObj** %stackaddr$prim49392, align 8
%stackaddr$prim49393 = alloca %struct.ScmObj*, align 8
%cpsprim40431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %n40239, %struct.ScmObj* %anf_45bind40363)
store volatile %struct.ScmObj* %cpsprim40431, %struct.ScmObj** %stackaddr$prim49393, align 8
%ae44113 = call %struct.ScmObj* @const_init_int(i64 0)
%args48279$k40427$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49394 = alloca %struct.ScmObj*, align 8
%args48279$k40427$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40431, %struct.ScmObj* %args48279$k40427$0)
store volatile %struct.ScmObj* %args48279$k40427$1, %struct.ScmObj** %stackaddr$prim49394, align 8
%stackaddr$prim49395 = alloca %struct.ScmObj*, align 8
%args48279$k40427$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44113, %struct.ScmObj* %args48279$k40427$1)
store volatile %struct.ScmObj* %args48279$k40427$2, %struct.ScmObj** %stackaddr$prim49395, align 8
%clofunc49396 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40427)
musttail call tailcc void %clofunc49396(%struct.ScmObj* %k40427, %struct.ScmObj* %args48279$k40427$2)
ret void
}

define tailcc void @proc_clo$ae44062(%struct.ScmObj* %env$ae44062,%struct.ScmObj* %current_45args48281) {
%stackaddr$env-ref49397 = alloca %struct.ScmObj*, align 8
%gen40229 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44062, i64 0)
store %struct.ScmObj* %gen40229, %struct.ScmObj** %stackaddr$env-ref49397
%stackaddr$env-ref49398 = alloca %struct.ScmObj*, align 8
%n40239 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44062, i64 1)
store %struct.ScmObj* %n40239, %struct.ScmObj** %stackaddr$env-ref49398
%stackaddr$prim49399 = alloca %struct.ScmObj*, align 8
%k40432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48281)
store volatile %struct.ScmObj* %k40432, %struct.ScmObj** %stackaddr$prim49399, align 8
%stackaddr$prim49400 = alloca %struct.ScmObj*, align 8
%current_45args48282 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48281)
store volatile %struct.ScmObj* %current_45args48282, %struct.ScmObj** %stackaddr$prim49400, align 8
%stackaddr$prim49401 = alloca %struct.ScmObj*, align 8
%_9540241 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48282)
store volatile %struct.ScmObj* %_9540241, %struct.ScmObj** %stackaddr$prim49401, align 8
%ae44064 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49402 = alloca %struct.ScmObj*, align 8
%anf_45bind40360 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %gen40229, %struct.ScmObj* %ae44064)
store volatile %struct.ScmObj* %anf_45bind40360, %struct.ScmObj** %stackaddr$prim49402, align 8
%ae44066 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49403 = alloca %struct.ScmObj*, align 8
%anf_45bind40361 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n40239, %struct.ScmObj* %ae44066)
store volatile %struct.ScmObj* %anf_45bind40361, %struct.ScmObj** %stackaddr$prim49403, align 8
%args48284$anf_45bind40360$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49404 = alloca %struct.ScmObj*, align 8
%args48284$anf_45bind40360$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40361, %struct.ScmObj* %args48284$anf_45bind40360$0)
store volatile %struct.ScmObj* %args48284$anf_45bind40360$1, %struct.ScmObj** %stackaddr$prim49404, align 8
%stackaddr$prim49405 = alloca %struct.ScmObj*, align 8
%args48284$anf_45bind40360$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40432, %struct.ScmObj* %args48284$anf_45bind40360$1)
store volatile %struct.ScmObj* %args48284$anf_45bind40360$2, %struct.ScmObj** %stackaddr$prim49405, align 8
%clofunc49406 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40360)
musttail call tailcc void %clofunc49406(%struct.ScmObj* %anf_45bind40360, %struct.ScmObj* %args48284$anf_45bind40360$2)
ret void
}

define tailcc void @proc_clo$ae44038(%struct.ScmObj* %env$ae44038,%struct.ScmObj* %el4024040433) {
%stackaddr$prim49407 = alloca %struct.ScmObj*, align 8
%k40434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %el4024040433)
store volatile %struct.ScmObj* %k40434, %struct.ScmObj** %stackaddr$prim49407, align 8
%stackaddr$prim49408 = alloca %struct.ScmObj*, align 8
%el40240 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %el4024040433)
store volatile %struct.ScmObj* %el40240, %struct.ScmObj** %stackaddr$prim49408, align 8
%stackaddr$applyprim49409 = alloca %struct.ScmObj*, align 8
%cpsaprim40435 = call %struct.ScmObj* @applyprim_vector(%struct.ScmObj* %el40240)
store volatile %struct.ScmObj* %cpsaprim40435, %struct.ScmObj** %stackaddr$applyprim49409, align 8
%ae44043 = call %struct.ScmObj* @const_init_int(i64 0)
%args48286$k40434$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49410 = alloca %struct.ScmObj*, align 8
%args48286$k40434$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim40435, %struct.ScmObj* %args48286$k40434$0)
store volatile %struct.ScmObj* %args48286$k40434$1, %struct.ScmObj** %stackaddr$prim49410, align 8
%stackaddr$prim49411 = alloca %struct.ScmObj*, align 8
%args48286$k40434$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44043, %struct.ScmObj* %args48286$k40434$1)
store volatile %struct.ScmObj* %args48286$k40434$2, %struct.ScmObj** %stackaddr$prim49411, align 8
%clofunc49412 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40434)
musttail call tailcc void %clofunc49412(%struct.ScmObj* %k40434, %struct.ScmObj* %args48286$k40434$2)
ret void
}

define tailcc void @proc_clo$ae43944(%struct.ScmObj* %env$ae43944,%struct.ScmObj* %current_45args48289) {
%stackaddr$prim49413 = alloca %struct.ScmObj*, align 8
%k40436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48289)
store volatile %struct.ScmObj* %k40436, %struct.ScmObj** %stackaddr$prim49413, align 8
%stackaddr$prim49414 = alloca %struct.ScmObj*, align 8
%current_45args48290 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48289)
store volatile %struct.ScmObj* %current_45args48290, %struct.ScmObj** %stackaddr$prim49414, align 8
%stackaddr$prim49415 = alloca %struct.ScmObj*, align 8
%thunk40227 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48290)
store volatile %struct.ScmObj* %thunk40227, %struct.ScmObj** %stackaddr$prim49415, align 8
%stackaddr$prim49416 = alloca %struct.ScmObj*, align 8
%anf_45bind40353 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk40227)
store volatile %struct.ScmObj* %anf_45bind40353, %struct.ScmObj** %stackaddr$prim49416, align 8
%truthy$cmp49417 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40353)
%cmp$cmp49417 = icmp eq i64 %truthy$cmp49417, 1
br i1 %cmp$cmp49417, label %truebranch$cmp49417, label %falsebranch$cmp49417
truebranch$cmp49417:
%stackaddr$prim49418 = alloca %struct.ScmObj*, align 8
%anf_45bind40354 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk40227)
store volatile %struct.ScmObj* %anf_45bind40354, %struct.ScmObj** %stackaddr$prim49418, align 8
%ae43949 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim49419 = alloca %struct.ScmObj*, align 8
%anf_45bind40355 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40354, %struct.ScmObj* %ae43949)
store volatile %struct.ScmObj* %anf_45bind40355, %struct.ScmObj** %stackaddr$prim49419, align 8
%truthy$cmp49420 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40355)
%cmp$cmp49420 = icmp eq i64 %truthy$cmp49420, 1
br i1 %cmp$cmp49420, label %truebranch$cmp49420, label %falsebranch$cmp49420
truebranch$cmp49420:
%ae43952 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49421 = alloca %struct.ScmObj*, align 8
%anf_45bind40356 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk40227, %struct.ScmObj* %ae43952)
store volatile %struct.ScmObj* %anf_45bind40356, %struct.ScmObj** %stackaddr$prim49421, align 8
%ae43954 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae4395449422, i32 0, i32 0))
%stackaddr$prim49423 = alloca %struct.ScmObj*, align 8
%cpsprim40437 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40356, %struct.ScmObj* %ae43954)
store volatile %struct.ScmObj* %cpsprim40437, %struct.ScmObj** %stackaddr$prim49423, align 8
%ae43956 = call %struct.ScmObj* @const_init_int(i64 0)
%args48292$k40436$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49424 = alloca %struct.ScmObj*, align 8
%args48292$k40436$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40437, %struct.ScmObj* %args48292$k40436$0)
store volatile %struct.ScmObj* %args48292$k40436$1, %struct.ScmObj** %stackaddr$prim49424, align 8
%stackaddr$prim49425 = alloca %struct.ScmObj*, align 8
%args48292$k40436$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43956, %struct.ScmObj* %args48292$k40436$1)
store volatile %struct.ScmObj* %args48292$k40436$2, %struct.ScmObj** %stackaddr$prim49425, align 8
%clofunc49426 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40436)
musttail call tailcc void %clofunc49426(%struct.ScmObj* %k40436, %struct.ScmObj* %args48292$k40436$2)
ret void
falsebranch$cmp49420:
%ae43974 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43975 = call %struct.ScmObj* @const_init_false()
%args48293$k40436$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49427 = alloca %struct.ScmObj*, align 8
%args48293$k40436$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43975, %struct.ScmObj* %args48293$k40436$0)
store volatile %struct.ScmObj* %args48293$k40436$1, %struct.ScmObj** %stackaddr$prim49427, align 8
%stackaddr$prim49428 = alloca %struct.ScmObj*, align 8
%args48293$k40436$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43974, %struct.ScmObj* %args48293$k40436$1)
store volatile %struct.ScmObj* %args48293$k40436$2, %struct.ScmObj** %stackaddr$prim49428, align 8
%clofunc49429 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40436)
musttail call tailcc void %clofunc49429(%struct.ScmObj* %k40436, %struct.ScmObj* %args48293$k40436$2)
ret void
falsebranch$cmp49417:
%ae43996 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43997 = call %struct.ScmObj* @const_init_false()
%args48294$k40436$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49430 = alloca %struct.ScmObj*, align 8
%args48294$k40436$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43997, %struct.ScmObj* %args48294$k40436$0)
store volatile %struct.ScmObj* %args48294$k40436$1, %struct.ScmObj** %stackaddr$prim49430, align 8
%stackaddr$prim49431 = alloca %struct.ScmObj*, align 8
%args48294$k40436$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43996, %struct.ScmObj* %args48294$k40436$1)
store volatile %struct.ScmObj* %args48294$k40436$2, %struct.ScmObj** %stackaddr$prim49431, align 8
%clofunc49432 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40436)
musttail call tailcc void %clofunc49432(%struct.ScmObj* %k40436, %struct.ScmObj* %args48294$k40436$2)
ret void
}

define tailcc void @proc_clo$ae43918(%struct.ScmObj* %env$ae43918,%struct.ScmObj* %current_45args48296) {
%stackaddr$prim49433 = alloca %struct.ScmObj*, align 8
%k40438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48296)
store volatile %struct.ScmObj* %k40438, %struct.ScmObj** %stackaddr$prim49433, align 8
%stackaddr$prim49434 = alloca %struct.ScmObj*, align 8
%current_45args48297 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48296)
store volatile %struct.ScmObj* %current_45args48297, %struct.ScmObj** %stackaddr$prim49434, align 8
%stackaddr$prim49435 = alloca %struct.ScmObj*, align 8
%x40166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48297)
store volatile %struct.ScmObj* %x40166, %struct.ScmObj** %stackaddr$prim49435, align 8
%stackaddr$prim49436 = alloca %struct.ScmObj*, align 8
%anf_45bind40350 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40166)
store volatile %struct.ScmObj* %anf_45bind40350, %struct.ScmObj** %stackaddr$prim49436, align 8
%stackaddr$prim49437 = alloca %struct.ScmObj*, align 8
%anf_45bind40351 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40350)
store volatile %struct.ScmObj* %anf_45bind40351, %struct.ScmObj** %stackaddr$prim49437, align 8
%stackaddr$prim49438 = alloca %struct.ScmObj*, align 8
%anf_45bind40352 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40351)
store volatile %struct.ScmObj* %anf_45bind40352, %struct.ScmObj** %stackaddr$prim49438, align 8
%stackaddr$prim49439 = alloca %struct.ScmObj*, align 8
%cpsprim40439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40352)
store volatile %struct.ScmObj* %cpsprim40439, %struct.ScmObj** %stackaddr$prim49439, align 8
%ae43924 = call %struct.ScmObj* @const_init_int(i64 0)
%args48299$k40438$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49440 = alloca %struct.ScmObj*, align 8
%args48299$k40438$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40439, %struct.ScmObj* %args48299$k40438$0)
store volatile %struct.ScmObj* %args48299$k40438$1, %struct.ScmObj** %stackaddr$prim49440, align 8
%stackaddr$prim49441 = alloca %struct.ScmObj*, align 8
%args48299$k40438$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43924, %struct.ScmObj* %args48299$k40438$1)
store volatile %struct.ScmObj* %args48299$k40438$2, %struct.ScmObj** %stackaddr$prim49441, align 8
%clofunc49442 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40438)
musttail call tailcc void %clofunc49442(%struct.ScmObj* %k40438, %struct.ScmObj* %args48299$k40438$2)
ret void
}

define tailcc void @proc_clo$ae43894(%struct.ScmObj* %env$ae43894,%struct.ScmObj* %current_45args48301) {
%stackaddr$prim49443 = alloca %struct.ScmObj*, align 8
%k40440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48301)
store volatile %struct.ScmObj* %k40440, %struct.ScmObj** %stackaddr$prim49443, align 8
%stackaddr$prim49444 = alloca %struct.ScmObj*, align 8
%current_45args48302 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48301)
store volatile %struct.ScmObj* %current_45args48302, %struct.ScmObj** %stackaddr$prim49444, align 8
%stackaddr$prim49445 = alloca %struct.ScmObj*, align 8
%x40168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48302)
store volatile %struct.ScmObj* %x40168, %struct.ScmObj** %stackaddr$prim49445, align 8
%stackaddr$prim49446 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40168)
store volatile %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$prim49446, align 8
%stackaddr$prim49447 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40348)
store volatile %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$prim49447, align 8
%stackaddr$prim49448 = alloca %struct.ScmObj*, align 8
%cpsprim40441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40349)
store volatile %struct.ScmObj* %cpsprim40441, %struct.ScmObj** %stackaddr$prim49448, align 8
%ae43899 = call %struct.ScmObj* @const_init_int(i64 0)
%args48304$k40440$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49449 = alloca %struct.ScmObj*, align 8
%args48304$k40440$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40441, %struct.ScmObj* %args48304$k40440$0)
store volatile %struct.ScmObj* %args48304$k40440$1, %struct.ScmObj** %stackaddr$prim49449, align 8
%stackaddr$prim49450 = alloca %struct.ScmObj*, align 8
%args48304$k40440$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43899, %struct.ScmObj* %args48304$k40440$1)
store volatile %struct.ScmObj* %args48304$k40440$2, %struct.ScmObj** %stackaddr$prim49450, align 8
%clofunc49451 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40440)
musttail call tailcc void %clofunc49451(%struct.ScmObj* %k40440, %struct.ScmObj* %args48304$k40440$2)
ret void
}

define tailcc void @proc_clo$ae43872(%struct.ScmObj* %env$ae43872,%struct.ScmObj* %current_45args48306) {
%stackaddr$prim49452 = alloca %struct.ScmObj*, align 8
%k40442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48306)
store volatile %struct.ScmObj* %k40442, %struct.ScmObj** %stackaddr$prim49452, align 8
%stackaddr$prim49453 = alloca %struct.ScmObj*, align 8
%current_45args48307 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48306)
store volatile %struct.ScmObj* %current_45args48307, %struct.ScmObj** %stackaddr$prim49453, align 8
%stackaddr$prim49454 = alloca %struct.ScmObj*, align 8
%x40170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48307)
store volatile %struct.ScmObj* %x40170, %struct.ScmObj** %stackaddr$prim49454, align 8
%stackaddr$prim49455 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40170)
store volatile %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$prim49455, align 8
%stackaddr$prim49456 = alloca %struct.ScmObj*, align 8
%cpsprim40443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40347)
store volatile %struct.ScmObj* %cpsprim40443, %struct.ScmObj** %stackaddr$prim49456, align 8
%ae43876 = call %struct.ScmObj* @const_init_int(i64 0)
%args48309$k40442$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49457 = alloca %struct.ScmObj*, align 8
%args48309$k40442$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40443, %struct.ScmObj* %args48309$k40442$0)
store volatile %struct.ScmObj* %args48309$k40442$1, %struct.ScmObj** %stackaddr$prim49457, align 8
%stackaddr$prim49458 = alloca %struct.ScmObj*, align 8
%args48309$k40442$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43876, %struct.ScmObj* %args48309$k40442$1)
store volatile %struct.ScmObj* %args48309$k40442$2, %struct.ScmObj** %stackaddr$prim49458, align 8
%clofunc49459 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40442)
musttail call tailcc void %clofunc49459(%struct.ScmObj* %k40442, %struct.ScmObj* %args48309$k40442$2)
ret void
}

define tailcc void @proc_clo$ae43852(%struct.ScmObj* %env$ae43852,%struct.ScmObj* %current_45args48311) {
%stackaddr$prim49460 = alloca %struct.ScmObj*, align 8
%k40444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48311)
store volatile %struct.ScmObj* %k40444, %struct.ScmObj** %stackaddr$prim49460, align 8
%stackaddr$prim49461 = alloca %struct.ScmObj*, align 8
%current_45args48312 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48311)
store volatile %struct.ScmObj* %current_45args48312, %struct.ScmObj** %stackaddr$prim49461, align 8
%stackaddr$prim49462 = alloca %struct.ScmObj*, align 8
%x40172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48312)
store volatile %struct.ScmObj* %x40172, %struct.ScmObj** %stackaddr$prim49462, align 8
%stackaddr$prim49463 = alloca %struct.ScmObj*, align 8
%cpsprim40445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40172)
store volatile %struct.ScmObj* %cpsprim40445, %struct.ScmObj** %stackaddr$prim49463, align 8
%ae43855 = call %struct.ScmObj* @const_init_int(i64 0)
%args48314$k40444$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49464 = alloca %struct.ScmObj*, align 8
%args48314$k40444$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40445, %struct.ScmObj* %args48314$k40444$0)
store volatile %struct.ScmObj* %args48314$k40444$1, %struct.ScmObj** %stackaddr$prim49464, align 8
%stackaddr$prim49465 = alloca %struct.ScmObj*, align 8
%args48314$k40444$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43855, %struct.ScmObj* %args48314$k40444$1)
store volatile %struct.ScmObj* %args48314$k40444$2, %struct.ScmObj** %stackaddr$prim49465, align 8
%clofunc49466 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40444)
musttail call tailcc void %clofunc49466(%struct.ScmObj* %k40444, %struct.ScmObj* %args48314$k40444$2)
ret void
}

define tailcc void @proc_clo$ae43754(%struct.ScmObj* %env$ae43754,%struct.ScmObj* %args4017440446) {
%stackaddr$env-ref49467 = alloca %struct.ScmObj*, align 8
%_37foldl140113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43754, i64 0)
store %struct.ScmObj* %_37foldl140113, %struct.ScmObj** %stackaddr$env-ref49467
%stackaddr$prim49468 = alloca %struct.ScmObj*, align 8
%k40447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4017440446)
store volatile %struct.ScmObj* %k40447, %struct.ScmObj** %stackaddr$prim49468, align 8
%stackaddr$prim49469 = alloca %struct.ScmObj*, align 8
%args40174 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4017440446)
store volatile %struct.ScmObj* %args40174, %struct.ScmObj** %stackaddr$prim49469, align 8
%stackaddr$prim49470 = alloca %struct.ScmObj*, align 8
%anf_45bind40341 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args40174)
store volatile %struct.ScmObj* %anf_45bind40341, %struct.ScmObj** %stackaddr$prim49470, align 8
%truthy$cmp49471 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40341)
%cmp$cmp49471 = icmp eq i64 %truthy$cmp49471, 1
br i1 %cmp$cmp49471, label %truebranch$cmp49471, label %falsebranch$cmp49471
truebranch$cmp49471:
%ae43760 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43761 = call %struct.ScmObj* @const_init_int(i64 1)
%args48316$k40447$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49472 = alloca %struct.ScmObj*, align 8
%args48316$k40447$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43761, %struct.ScmObj* %args48316$k40447$0)
store volatile %struct.ScmObj* %args48316$k40447$1, %struct.ScmObj** %stackaddr$prim49472, align 8
%stackaddr$prim49473 = alloca %struct.ScmObj*, align 8
%args48316$k40447$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43760, %struct.ScmObj* %args48316$k40447$1)
store volatile %struct.ScmObj* %args48316$k40447$2, %struct.ScmObj** %stackaddr$prim49473, align 8
%clofunc49474 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40447)
musttail call tailcc void %clofunc49474(%struct.ScmObj* %k40447, %struct.ScmObj* %args48316$k40447$2)
ret void
falsebranch$cmp49471:
%stackaddr$prim49475 = alloca %struct.ScmObj*, align 8
%anf_45bind40342 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40174)
store volatile %struct.ScmObj* %anf_45bind40342, %struct.ScmObj** %stackaddr$prim49475, align 8
%stackaddr$prim49476 = alloca %struct.ScmObj*, align 8
%anf_45bind40343 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40342)
store volatile %struct.ScmObj* %anf_45bind40343, %struct.ScmObj** %stackaddr$prim49476, align 8
%truthy$cmp49477 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40343)
%cmp$cmp49477 = icmp eq i64 %truthy$cmp49477, 1
br i1 %cmp$cmp49477, label %truebranch$cmp49477, label %falsebranch$cmp49477
truebranch$cmp49477:
%stackaddr$prim49478 = alloca %struct.ScmObj*, align 8
%cpsprim40448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40174)
store volatile %struct.ScmObj* %cpsprim40448, %struct.ScmObj** %stackaddr$prim49478, align 8
%ae43773 = call %struct.ScmObj* @const_init_int(i64 0)
%args48317$k40447$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49479 = alloca %struct.ScmObj*, align 8
%args48317$k40447$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40448, %struct.ScmObj* %args48317$k40447$0)
store volatile %struct.ScmObj* %args48317$k40447$1, %struct.ScmObj** %stackaddr$prim49479, align 8
%stackaddr$prim49480 = alloca %struct.ScmObj*, align 8
%args48317$k40447$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43773, %struct.ScmObj* %args48317$k40447$1)
store volatile %struct.ScmObj* %args48317$k40447$2, %struct.ScmObj** %stackaddr$prim49480, align 8
%clofunc49481 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40447)
musttail call tailcc void %clofunc49481(%struct.ScmObj* %k40447, %struct.ScmObj* %args48317$k40447$2)
ret void
falsebranch$cmp49477:
%stackaddr$makeclosure49482 = alloca %struct.ScmObj*, align 8
%fptrToInt49483 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43778 to i64
%ae43778 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49483)
store volatile %struct.ScmObj* %ae43778, %struct.ScmObj** %stackaddr$makeclosure49482, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43778, %struct.ScmObj* %_37foldl140113, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43778, %struct.ScmObj* %k40447, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43778, %struct.ScmObj* %args40174, i64 2)
%ae43779 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49484 = alloca %struct.ScmObj*, align 8
%fptrToInt49485 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43780 to i64
%ae43780 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49485)
store volatile %struct.ScmObj* %ae43780, %struct.ScmObj** %stackaddr$makeclosure49484, align 8
%args48327$ae43778$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49486 = alloca %struct.ScmObj*, align 8
%args48327$ae43778$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43780, %struct.ScmObj* %args48327$ae43778$0)
store volatile %struct.ScmObj* %args48327$ae43778$1, %struct.ScmObj** %stackaddr$prim49486, align 8
%stackaddr$prim49487 = alloca %struct.ScmObj*, align 8
%args48327$ae43778$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43779, %struct.ScmObj* %args48327$ae43778$1)
store volatile %struct.ScmObj* %args48327$ae43778$2, %struct.ScmObj** %stackaddr$prim49487, align 8
%clofunc49488 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43778)
musttail call tailcc void %clofunc49488(%struct.ScmObj* %ae43778, %struct.ScmObj* %args48327$ae43778$2)
ret void
}

define tailcc void @proc_clo$ae43778(%struct.ScmObj* %env$ae43778,%struct.ScmObj* %current_45args48318) {
%stackaddr$env-ref49489 = alloca %struct.ScmObj*, align 8
%_37foldl140113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43778, i64 0)
store %struct.ScmObj* %_37foldl140113, %struct.ScmObj** %stackaddr$env-ref49489
%stackaddr$env-ref49490 = alloca %struct.ScmObj*, align 8
%k40447 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43778, i64 1)
store %struct.ScmObj* %k40447, %struct.ScmObj** %stackaddr$env-ref49490
%stackaddr$env-ref49491 = alloca %struct.ScmObj*, align 8
%args40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43778, i64 2)
store %struct.ScmObj* %args40174, %struct.ScmObj** %stackaddr$env-ref49491
%stackaddr$prim49492 = alloca %struct.ScmObj*, align 8
%_95k40449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48318)
store volatile %struct.ScmObj* %_95k40449, %struct.ScmObj** %stackaddr$prim49492, align 8
%stackaddr$prim49493 = alloca %struct.ScmObj*, align 8
%current_45args48319 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48318)
store volatile %struct.ScmObj* %current_45args48319, %struct.ScmObj** %stackaddr$prim49493, align 8
%stackaddr$prim49494 = alloca %struct.ScmObj*, align 8
%anf_45bind40344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48319)
store volatile %struct.ScmObj* %anf_45bind40344, %struct.ScmObj** %stackaddr$prim49494, align 8
%stackaddr$prim49495 = alloca %struct.ScmObj*, align 8
%anf_45bind40345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40174)
store volatile %struct.ScmObj* %anf_45bind40345, %struct.ScmObj** %stackaddr$prim49495, align 8
%stackaddr$prim49496 = alloca %struct.ScmObj*, align 8
%anf_45bind40346 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40174)
store volatile %struct.ScmObj* %anf_45bind40346, %struct.ScmObj** %stackaddr$prim49496, align 8
%args48321$_37foldl140113$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49497 = alloca %struct.ScmObj*, align 8
%args48321$_37foldl140113$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40346, %struct.ScmObj* %args48321$_37foldl140113$0)
store volatile %struct.ScmObj* %args48321$_37foldl140113$1, %struct.ScmObj** %stackaddr$prim49497, align 8
%stackaddr$prim49498 = alloca %struct.ScmObj*, align 8
%args48321$_37foldl140113$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40345, %struct.ScmObj* %args48321$_37foldl140113$1)
store volatile %struct.ScmObj* %args48321$_37foldl140113$2, %struct.ScmObj** %stackaddr$prim49498, align 8
%stackaddr$prim49499 = alloca %struct.ScmObj*, align 8
%args48321$_37foldl140113$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40344, %struct.ScmObj* %args48321$_37foldl140113$2)
store volatile %struct.ScmObj* %args48321$_37foldl140113$3, %struct.ScmObj** %stackaddr$prim49499, align 8
%stackaddr$prim49500 = alloca %struct.ScmObj*, align 8
%args48321$_37foldl140113$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40447, %struct.ScmObj* %args48321$_37foldl140113$3)
store volatile %struct.ScmObj* %args48321$_37foldl140113$4, %struct.ScmObj** %stackaddr$prim49500, align 8
%clofunc49501 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140113)
musttail call tailcc void %clofunc49501(%struct.ScmObj* %_37foldl140113, %struct.ScmObj* %args48321$_37foldl140113$4)
ret void
}

define tailcc void @proc_clo$ae43780(%struct.ScmObj* %env$ae43780,%struct.ScmObj* %current_45args48322) {
%stackaddr$prim49502 = alloca %struct.ScmObj*, align 8
%k40450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48322)
store volatile %struct.ScmObj* %k40450, %struct.ScmObj** %stackaddr$prim49502, align 8
%stackaddr$prim49503 = alloca %struct.ScmObj*, align 8
%current_45args48323 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48322)
store volatile %struct.ScmObj* %current_45args48323, %struct.ScmObj** %stackaddr$prim49503, align 8
%stackaddr$prim49504 = alloca %struct.ScmObj*, align 8
%n40176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48323)
store volatile %struct.ScmObj* %n40176, %struct.ScmObj** %stackaddr$prim49504, align 8
%stackaddr$prim49505 = alloca %struct.ScmObj*, align 8
%current_45args48324 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48323)
store volatile %struct.ScmObj* %current_45args48324, %struct.ScmObj** %stackaddr$prim49505, align 8
%stackaddr$prim49506 = alloca %struct.ScmObj*, align 8
%v40175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48324)
store volatile %struct.ScmObj* %v40175, %struct.ScmObj** %stackaddr$prim49506, align 8
%stackaddr$prim49507 = alloca %struct.ScmObj*, align 8
%cpsprim40451 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v40175, %struct.ScmObj* %n40176)
store volatile %struct.ScmObj* %cpsprim40451, %struct.ScmObj** %stackaddr$prim49507, align 8
%ae43784 = call %struct.ScmObj* @const_init_int(i64 0)
%args48326$k40450$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49508 = alloca %struct.ScmObj*, align 8
%args48326$k40450$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40451, %struct.ScmObj* %args48326$k40450$0)
store volatile %struct.ScmObj* %args48326$k40450$1, %struct.ScmObj** %stackaddr$prim49508, align 8
%stackaddr$prim49509 = alloca %struct.ScmObj*, align 8
%args48326$k40450$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43784, %struct.ScmObj* %args48326$k40450$1)
store volatile %struct.ScmObj* %args48326$k40450$2, %struct.ScmObj** %stackaddr$prim49509, align 8
%clofunc49510 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40450)
musttail call tailcc void %clofunc49510(%struct.ScmObj* %k40450, %struct.ScmObj* %args48326$k40450$2)
ret void
}

define tailcc void @proc_clo$ae43350(%struct.ScmObj* %env$ae43350,%struct.ScmObj* %current_45args48329) {
%stackaddr$prim49511 = alloca %struct.ScmObj*, align 8
%k40452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48329)
store volatile %struct.ScmObj* %k40452, %struct.ScmObj** %stackaddr$prim49511, align 8
%stackaddr$prim49512 = alloca %struct.ScmObj*, align 8
%current_45args48330 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48329)
store volatile %struct.ScmObj* %current_45args48330, %struct.ScmObj** %stackaddr$prim49512, align 8
%stackaddr$prim49513 = alloca %struct.ScmObj*, align 8
%v40179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48330)
store volatile %struct.ScmObj* %v40179, %struct.ScmObj** %stackaddr$prim49513, align 8
%stackaddr$prim49514 = alloca %struct.ScmObj*, align 8
%current_45args48331 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48330)
store volatile %struct.ScmObj* %current_45args48331, %struct.ScmObj** %stackaddr$prim49514, align 8
%stackaddr$prim49515 = alloca %struct.ScmObj*, align 8
%lst40178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48331)
store volatile %struct.ScmObj* %lst40178, %struct.ScmObj** %stackaddr$prim49515, align 8
%ae43351 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49516 = alloca %struct.ScmObj*, align 8
%lst40180 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae43351, %struct.ScmObj* %lst40178)
store volatile %struct.ScmObj* %lst40180, %struct.ScmObj** %stackaddr$prim49516, align 8
%stackaddr$makeclosure49517 = alloca %struct.ScmObj*, align 8
%fptrToInt49518 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43353 to i64
%ae43353 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49518)
store volatile %struct.ScmObj* %ae43353, %struct.ScmObj** %stackaddr$makeclosure49517, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43353, %struct.ScmObj* %k40452, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43353, %struct.ScmObj* %lst40180, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43353, %struct.ScmObj* %v40179, i64 2)
%ae43354 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49519 = alloca %struct.ScmObj*, align 8
%fptrToInt49520 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43355 to i64
%ae43355 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49520)
store volatile %struct.ScmObj* %ae43355, %struct.ScmObj** %stackaddr$makeclosure49519, align 8
%args48353$ae43353$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49521 = alloca %struct.ScmObj*, align 8
%args48353$ae43353$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43355, %struct.ScmObj* %args48353$ae43353$0)
store volatile %struct.ScmObj* %args48353$ae43353$1, %struct.ScmObj** %stackaddr$prim49521, align 8
%stackaddr$prim49522 = alloca %struct.ScmObj*, align 8
%args48353$ae43353$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43354, %struct.ScmObj* %args48353$ae43353$1)
store volatile %struct.ScmObj* %args48353$ae43353$2, %struct.ScmObj** %stackaddr$prim49522, align 8
%clofunc49523 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43353)
musttail call tailcc void %clofunc49523(%struct.ScmObj* %ae43353, %struct.ScmObj* %args48353$ae43353$2)
ret void
}

define tailcc void @proc_clo$ae43353(%struct.ScmObj* %env$ae43353,%struct.ScmObj* %current_45args48333) {
%stackaddr$env-ref49524 = alloca %struct.ScmObj*, align 8
%k40452 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43353, i64 0)
store %struct.ScmObj* %k40452, %struct.ScmObj** %stackaddr$env-ref49524
%stackaddr$env-ref49525 = alloca %struct.ScmObj*, align 8
%lst40180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43353, i64 1)
store %struct.ScmObj* %lst40180, %struct.ScmObj** %stackaddr$env-ref49525
%stackaddr$env-ref49526 = alloca %struct.ScmObj*, align 8
%v40179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43353, i64 2)
store %struct.ScmObj* %v40179, %struct.ScmObj** %stackaddr$env-ref49526
%stackaddr$prim49527 = alloca %struct.ScmObj*, align 8
%_95k40453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48333)
store volatile %struct.ScmObj* %_95k40453, %struct.ScmObj** %stackaddr$prim49527, align 8
%stackaddr$prim49528 = alloca %struct.ScmObj*, align 8
%current_45args48334 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48333)
store volatile %struct.ScmObj* %current_45args48334, %struct.ScmObj** %stackaddr$prim49528, align 8
%stackaddr$prim49529 = alloca %struct.ScmObj*, align 8
%anf_45bind40333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48334)
store volatile %struct.ScmObj* %anf_45bind40333, %struct.ScmObj** %stackaddr$prim49529, align 8
%stackaddr$makeclosure49530 = alloca %struct.ScmObj*, align 8
%fptrToInt49531 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43369 to i64
%ae43369 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49531)
store volatile %struct.ScmObj* %ae43369, %struct.ScmObj** %stackaddr$makeclosure49530, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43369, %struct.ScmObj* %k40452, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43369, %struct.ScmObj* %lst40180, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43369, %struct.ScmObj* %v40179, i64 2)
%stackaddr$makeclosure49532 = alloca %struct.ScmObj*, align 8
%fptrToInt49533 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43370 to i64
%ae43370 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49533)
store volatile %struct.ScmObj* %ae43370, %struct.ScmObj** %stackaddr$makeclosure49532, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43370, %struct.ScmObj* %k40452, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43370, %struct.ScmObj* %lst40180, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43370, %struct.ScmObj* %v40179, i64 2)
%args48348$anf_45bind40333$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49534 = alloca %struct.ScmObj*, align 8
%args48348$anf_45bind40333$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43370, %struct.ScmObj* %args48348$anf_45bind40333$0)
store volatile %struct.ScmObj* %args48348$anf_45bind40333$1, %struct.ScmObj** %stackaddr$prim49534, align 8
%stackaddr$prim49535 = alloca %struct.ScmObj*, align 8
%args48348$anf_45bind40333$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43369, %struct.ScmObj* %args48348$anf_45bind40333$1)
store volatile %struct.ScmObj* %args48348$anf_45bind40333$2, %struct.ScmObj** %stackaddr$prim49535, align 8
%clofunc49536 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40333)
musttail call tailcc void %clofunc49536(%struct.ScmObj* %anf_45bind40333, %struct.ScmObj* %args48348$anf_45bind40333$2)
ret void
}

define tailcc void @proc_clo$ae43369(%struct.ScmObj* %env$ae43369,%struct.ScmObj* %current_45args48336) {
%stackaddr$env-ref49537 = alloca %struct.ScmObj*, align 8
%k40452 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43369, i64 0)
store %struct.ScmObj* %k40452, %struct.ScmObj** %stackaddr$env-ref49537
%stackaddr$env-ref49538 = alloca %struct.ScmObj*, align 8
%lst40180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43369, i64 1)
store %struct.ScmObj* %lst40180, %struct.ScmObj** %stackaddr$env-ref49538
%stackaddr$env-ref49539 = alloca %struct.ScmObj*, align 8
%v40179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43369, i64 2)
store %struct.ScmObj* %v40179, %struct.ScmObj** %stackaddr$env-ref49539
%stackaddr$prim49540 = alloca %struct.ScmObj*, align 8
%_95k40454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48336)
store volatile %struct.ScmObj* %_95k40454, %struct.ScmObj** %stackaddr$prim49540, align 8
%stackaddr$prim49541 = alloca %struct.ScmObj*, align 8
%current_45args48337 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48336)
store volatile %struct.ScmObj* %current_45args48337, %struct.ScmObj** %stackaddr$prim49541, align 8
%stackaddr$prim49542 = alloca %struct.ScmObj*, align 8
%cc40181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48337)
store volatile %struct.ScmObj* %cc40181, %struct.ScmObj** %stackaddr$prim49542, align 8
%ae43478 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49543 = alloca %struct.ScmObj*, align 8
%anf_45bind40334 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40180, %struct.ScmObj* %ae43478)
store volatile %struct.ScmObj* %anf_45bind40334, %struct.ScmObj** %stackaddr$prim49543, align 8
%stackaddr$prim49544 = alloca %struct.ScmObj*, align 8
%anf_45bind40335 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40334)
store volatile %struct.ScmObj* %anf_45bind40335, %struct.ScmObj** %stackaddr$prim49544, align 8
%truthy$cmp49545 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40335)
%cmp$cmp49545 = icmp eq i64 %truthy$cmp49545, 1
br i1 %cmp$cmp49545, label %truebranch$cmp49545, label %falsebranch$cmp49545
truebranch$cmp49545:
%ae43482 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43483 = call %struct.ScmObj* @const_init_false()
%args48339$k40452$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49546 = alloca %struct.ScmObj*, align 8
%args48339$k40452$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43483, %struct.ScmObj* %args48339$k40452$0)
store volatile %struct.ScmObj* %args48339$k40452$1, %struct.ScmObj** %stackaddr$prim49546, align 8
%stackaddr$prim49547 = alloca %struct.ScmObj*, align 8
%args48339$k40452$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43482, %struct.ScmObj* %args48339$k40452$1)
store volatile %struct.ScmObj* %args48339$k40452$2, %struct.ScmObj** %stackaddr$prim49547, align 8
%clofunc49548 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40452)
musttail call tailcc void %clofunc49548(%struct.ScmObj* %k40452, %struct.ScmObj* %args48339$k40452$2)
ret void
falsebranch$cmp49545:
%ae43491 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49549 = alloca %struct.ScmObj*, align 8
%anf_45bind40336 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40180, %struct.ScmObj* %ae43491)
store volatile %struct.ScmObj* %anf_45bind40336, %struct.ScmObj** %stackaddr$prim49549, align 8
%stackaddr$prim49550 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40336)
store volatile %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$prim49550, align 8
%stackaddr$prim49551 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40337, %struct.ScmObj* %v40179)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim49551, align 8
%truthy$cmp49552 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40338)
%cmp$cmp49552 = icmp eq i64 %truthy$cmp49552, 1
br i1 %cmp$cmp49552, label %truebranch$cmp49552, label %falsebranch$cmp49552
truebranch$cmp49552:
%ae43497 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49553 = alloca %struct.ScmObj*, align 8
%cpsprim40455 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40180, %struct.ScmObj* %ae43497)
store volatile %struct.ScmObj* %cpsprim40455, %struct.ScmObj** %stackaddr$prim49553, align 8
%ae43499 = call %struct.ScmObj* @const_init_int(i64 0)
%args48340$k40452$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49554 = alloca %struct.ScmObj*, align 8
%args48340$k40452$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40455, %struct.ScmObj* %args48340$k40452$0)
store volatile %struct.ScmObj* %args48340$k40452$1, %struct.ScmObj** %stackaddr$prim49554, align 8
%stackaddr$prim49555 = alloca %struct.ScmObj*, align 8
%args48340$k40452$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43499, %struct.ScmObj* %args48340$k40452$1)
store volatile %struct.ScmObj* %args48340$k40452$2, %struct.ScmObj** %stackaddr$prim49555, align 8
%clofunc49556 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40452)
musttail call tailcc void %clofunc49556(%struct.ScmObj* %k40452, %struct.ScmObj* %args48340$k40452$2)
ret void
falsebranch$cmp49552:
%ae43510 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49557 = alloca %struct.ScmObj*, align 8
%anf_45bind40339 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40180, %struct.ScmObj* %ae43510)
store volatile %struct.ScmObj* %anf_45bind40339, %struct.ScmObj** %stackaddr$prim49557, align 8
%stackaddr$prim49558 = alloca %struct.ScmObj*, align 8
%anf_45bind40340 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40339)
store volatile %struct.ScmObj* %anf_45bind40340, %struct.ScmObj** %stackaddr$prim49558, align 8
%ae43513 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49559 = alloca %struct.ScmObj*, align 8
%_95040183 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40180, %struct.ScmObj* %ae43513, %struct.ScmObj* %anf_45bind40340)
store volatile %struct.ScmObj* %_95040183, %struct.ScmObj** %stackaddr$prim49559, align 8
%args48341$cc40181$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49560 = alloca %struct.ScmObj*, align 8
%args48341$cc40181$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40181, %struct.ScmObj* %args48341$cc40181$0)
store volatile %struct.ScmObj* %args48341$cc40181$1, %struct.ScmObj** %stackaddr$prim49560, align 8
%stackaddr$prim49561 = alloca %struct.ScmObj*, align 8
%args48341$cc40181$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40452, %struct.ScmObj* %args48341$cc40181$1)
store volatile %struct.ScmObj* %args48341$cc40181$2, %struct.ScmObj** %stackaddr$prim49561, align 8
%clofunc49562 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40181)
musttail call tailcc void %clofunc49562(%struct.ScmObj* %cc40181, %struct.ScmObj* %args48341$cc40181$2)
ret void
}

define tailcc void @proc_clo$ae43370(%struct.ScmObj* %env$ae43370,%struct.ScmObj* %current_45args48342) {
%stackaddr$env-ref49563 = alloca %struct.ScmObj*, align 8
%k40452 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43370, i64 0)
store %struct.ScmObj* %k40452, %struct.ScmObj** %stackaddr$env-ref49563
%stackaddr$env-ref49564 = alloca %struct.ScmObj*, align 8
%lst40180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43370, i64 1)
store %struct.ScmObj* %lst40180, %struct.ScmObj** %stackaddr$env-ref49564
%stackaddr$env-ref49565 = alloca %struct.ScmObj*, align 8
%v40179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43370, i64 2)
store %struct.ScmObj* %v40179, %struct.ScmObj** %stackaddr$env-ref49565
%stackaddr$prim49566 = alloca %struct.ScmObj*, align 8
%_95k40454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48342)
store volatile %struct.ScmObj* %_95k40454, %struct.ScmObj** %stackaddr$prim49566, align 8
%stackaddr$prim49567 = alloca %struct.ScmObj*, align 8
%current_45args48343 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48342)
store volatile %struct.ScmObj* %current_45args48343, %struct.ScmObj** %stackaddr$prim49567, align 8
%stackaddr$prim49568 = alloca %struct.ScmObj*, align 8
%cc40181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48343)
store volatile %struct.ScmObj* %cc40181, %struct.ScmObj** %stackaddr$prim49568, align 8
%ae43372 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49569 = alloca %struct.ScmObj*, align 8
%anf_45bind40334 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40180, %struct.ScmObj* %ae43372)
store volatile %struct.ScmObj* %anf_45bind40334, %struct.ScmObj** %stackaddr$prim49569, align 8
%stackaddr$prim49570 = alloca %struct.ScmObj*, align 8
%anf_45bind40335 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40334)
store volatile %struct.ScmObj* %anf_45bind40335, %struct.ScmObj** %stackaddr$prim49570, align 8
%truthy$cmp49571 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40335)
%cmp$cmp49571 = icmp eq i64 %truthy$cmp49571, 1
br i1 %cmp$cmp49571, label %truebranch$cmp49571, label %falsebranch$cmp49571
truebranch$cmp49571:
%ae43376 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43377 = call %struct.ScmObj* @const_init_false()
%args48345$k40452$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49572 = alloca %struct.ScmObj*, align 8
%args48345$k40452$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43377, %struct.ScmObj* %args48345$k40452$0)
store volatile %struct.ScmObj* %args48345$k40452$1, %struct.ScmObj** %stackaddr$prim49572, align 8
%stackaddr$prim49573 = alloca %struct.ScmObj*, align 8
%args48345$k40452$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43376, %struct.ScmObj* %args48345$k40452$1)
store volatile %struct.ScmObj* %args48345$k40452$2, %struct.ScmObj** %stackaddr$prim49573, align 8
%clofunc49574 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40452)
musttail call tailcc void %clofunc49574(%struct.ScmObj* %k40452, %struct.ScmObj* %args48345$k40452$2)
ret void
falsebranch$cmp49571:
%ae43385 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49575 = alloca %struct.ScmObj*, align 8
%anf_45bind40336 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40180, %struct.ScmObj* %ae43385)
store volatile %struct.ScmObj* %anf_45bind40336, %struct.ScmObj** %stackaddr$prim49575, align 8
%stackaddr$prim49576 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40336)
store volatile %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$prim49576, align 8
%stackaddr$prim49577 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40337, %struct.ScmObj* %v40179)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim49577, align 8
%truthy$cmp49578 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40338)
%cmp$cmp49578 = icmp eq i64 %truthy$cmp49578, 1
br i1 %cmp$cmp49578, label %truebranch$cmp49578, label %falsebranch$cmp49578
truebranch$cmp49578:
%ae43391 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49579 = alloca %struct.ScmObj*, align 8
%cpsprim40455 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40180, %struct.ScmObj* %ae43391)
store volatile %struct.ScmObj* %cpsprim40455, %struct.ScmObj** %stackaddr$prim49579, align 8
%ae43393 = call %struct.ScmObj* @const_init_int(i64 0)
%args48346$k40452$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49580 = alloca %struct.ScmObj*, align 8
%args48346$k40452$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40455, %struct.ScmObj* %args48346$k40452$0)
store volatile %struct.ScmObj* %args48346$k40452$1, %struct.ScmObj** %stackaddr$prim49580, align 8
%stackaddr$prim49581 = alloca %struct.ScmObj*, align 8
%args48346$k40452$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43393, %struct.ScmObj* %args48346$k40452$1)
store volatile %struct.ScmObj* %args48346$k40452$2, %struct.ScmObj** %stackaddr$prim49581, align 8
%clofunc49582 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40452)
musttail call tailcc void %clofunc49582(%struct.ScmObj* %k40452, %struct.ScmObj* %args48346$k40452$2)
ret void
falsebranch$cmp49578:
%ae43404 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49583 = alloca %struct.ScmObj*, align 8
%anf_45bind40339 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40180, %struct.ScmObj* %ae43404)
store volatile %struct.ScmObj* %anf_45bind40339, %struct.ScmObj** %stackaddr$prim49583, align 8
%stackaddr$prim49584 = alloca %struct.ScmObj*, align 8
%anf_45bind40340 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40339)
store volatile %struct.ScmObj* %anf_45bind40340, %struct.ScmObj** %stackaddr$prim49584, align 8
%ae43407 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49585 = alloca %struct.ScmObj*, align 8
%_95040183 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40180, %struct.ScmObj* %ae43407, %struct.ScmObj* %anf_45bind40340)
store volatile %struct.ScmObj* %_95040183, %struct.ScmObj** %stackaddr$prim49585, align 8
%args48347$cc40181$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49586 = alloca %struct.ScmObj*, align 8
%args48347$cc40181$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40181, %struct.ScmObj* %args48347$cc40181$0)
store volatile %struct.ScmObj* %args48347$cc40181$1, %struct.ScmObj** %stackaddr$prim49586, align 8
%stackaddr$prim49587 = alloca %struct.ScmObj*, align 8
%args48347$cc40181$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40452, %struct.ScmObj* %args48347$cc40181$1)
store volatile %struct.ScmObj* %args48347$cc40181$2, %struct.ScmObj** %stackaddr$prim49587, align 8
%clofunc49588 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40181)
musttail call tailcc void %clofunc49588(%struct.ScmObj* %cc40181, %struct.ScmObj* %args48347$cc40181$2)
ret void
}

define tailcc void @proc_clo$ae43355(%struct.ScmObj* %env$ae43355,%struct.ScmObj* %current_45args48349) {
%stackaddr$prim49589 = alloca %struct.ScmObj*, align 8
%k40456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48349)
store volatile %struct.ScmObj* %k40456, %struct.ScmObj** %stackaddr$prim49589, align 8
%stackaddr$prim49590 = alloca %struct.ScmObj*, align 8
%current_45args48350 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48349)
store volatile %struct.ScmObj* %current_45args48350, %struct.ScmObj** %stackaddr$prim49590, align 8
%stackaddr$prim49591 = alloca %struct.ScmObj*, align 8
%u40182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48350)
store volatile %struct.ScmObj* %u40182, %struct.ScmObj** %stackaddr$prim49591, align 8
%args48352$u40182$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49592 = alloca %struct.ScmObj*, align 8
%args48352$u40182$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40182, %struct.ScmObj* %args48352$u40182$0)
store volatile %struct.ScmObj* %args48352$u40182$1, %struct.ScmObj** %stackaddr$prim49592, align 8
%stackaddr$prim49593 = alloca %struct.ScmObj*, align 8
%args48352$u40182$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40456, %struct.ScmObj* %args48352$u40182$1)
store volatile %struct.ScmObj* %args48352$u40182$2, %struct.ScmObj** %stackaddr$prim49593, align 8
%clofunc49594 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40182)
musttail call tailcc void %clofunc49594(%struct.ScmObj* %u40182, %struct.ScmObj* %args48352$u40182$2)
ret void
}

define tailcc void @proc_clo$ae42814(%struct.ScmObj* %env$ae42814,%struct.ScmObj* %current_45args48355) {
%stackaddr$prim49595 = alloca %struct.ScmObj*, align 8
%k40457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48355)
store volatile %struct.ScmObj* %k40457, %struct.ScmObj** %stackaddr$prim49595, align 8
%stackaddr$prim49596 = alloca %struct.ScmObj*, align 8
%current_45args48356 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48355)
store volatile %struct.ScmObj* %current_45args48356, %struct.ScmObj** %stackaddr$prim49596, align 8
%stackaddr$prim49597 = alloca %struct.ScmObj*, align 8
%lst40186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48356)
store volatile %struct.ScmObj* %lst40186, %struct.ScmObj** %stackaddr$prim49597, align 8
%stackaddr$prim49598 = alloca %struct.ScmObj*, align 8
%current_45args48357 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48356)
store volatile %struct.ScmObj* %current_45args48357, %struct.ScmObj** %stackaddr$prim49598, align 8
%stackaddr$prim49599 = alloca %struct.ScmObj*, align 8
%n40185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48357)
store volatile %struct.ScmObj* %n40185, %struct.ScmObj** %stackaddr$prim49599, align 8
%ae42815 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49600 = alloca %struct.ScmObj*, align 8
%n40188 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42815, %struct.ScmObj* %n40185)
store volatile %struct.ScmObj* %n40188, %struct.ScmObj** %stackaddr$prim49600, align 8
%ae42817 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49601 = alloca %struct.ScmObj*, align 8
%lst40187 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42817, %struct.ScmObj* %lst40186)
store volatile %struct.ScmObj* %lst40187, %struct.ScmObj** %stackaddr$prim49601, align 8
%stackaddr$makeclosure49602 = alloca %struct.ScmObj*, align 8
%fptrToInt49603 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42819 to i64
%ae42819 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49603)
store volatile %struct.ScmObj* %ae42819, %struct.ScmObj** %stackaddr$makeclosure49602, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42819, %struct.ScmObj* %k40457, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42819, %struct.ScmObj* %n40188, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42819, %struct.ScmObj* %lst40187, i64 2)
%ae42820 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49604 = alloca %struct.ScmObj*, align 8
%fptrToInt49605 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42821 to i64
%ae42821 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49605)
store volatile %struct.ScmObj* %ae42821, %struct.ScmObj** %stackaddr$makeclosure49604, align 8
%args48377$ae42819$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49606 = alloca %struct.ScmObj*, align 8
%args48377$ae42819$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42821, %struct.ScmObj* %args48377$ae42819$0)
store volatile %struct.ScmObj* %args48377$ae42819$1, %struct.ScmObj** %stackaddr$prim49606, align 8
%stackaddr$prim49607 = alloca %struct.ScmObj*, align 8
%args48377$ae42819$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42820, %struct.ScmObj* %args48377$ae42819$1)
store volatile %struct.ScmObj* %args48377$ae42819$2, %struct.ScmObj** %stackaddr$prim49607, align 8
%clofunc49608 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42819)
musttail call tailcc void %clofunc49608(%struct.ScmObj* %ae42819, %struct.ScmObj* %args48377$ae42819$2)
ret void
}

define tailcc void @proc_clo$ae42819(%struct.ScmObj* %env$ae42819,%struct.ScmObj* %current_45args48359) {
%stackaddr$env-ref49609 = alloca %struct.ScmObj*, align 8
%k40457 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42819, i64 0)
store %struct.ScmObj* %k40457, %struct.ScmObj** %stackaddr$env-ref49609
%stackaddr$env-ref49610 = alloca %struct.ScmObj*, align 8
%n40188 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42819, i64 1)
store %struct.ScmObj* %n40188, %struct.ScmObj** %stackaddr$env-ref49610
%stackaddr$env-ref49611 = alloca %struct.ScmObj*, align 8
%lst40187 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42819, i64 2)
store %struct.ScmObj* %lst40187, %struct.ScmObj** %stackaddr$env-ref49611
%stackaddr$prim49612 = alloca %struct.ScmObj*, align 8
%_95k40458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48359)
store volatile %struct.ScmObj* %_95k40458, %struct.ScmObj** %stackaddr$prim49612, align 8
%stackaddr$prim49613 = alloca %struct.ScmObj*, align 8
%current_45args48360 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48359)
store volatile %struct.ScmObj* %current_45args48360, %struct.ScmObj** %stackaddr$prim49613, align 8
%stackaddr$prim49614 = alloca %struct.ScmObj*, align 8
%anf_45bind40326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48360)
store volatile %struct.ScmObj* %anf_45bind40326, %struct.ScmObj** %stackaddr$prim49614, align 8
%stackaddr$makeclosure49615 = alloca %struct.ScmObj*, align 8
%fptrToInt49616 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42835 to i64
%ae42835 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49616)
store volatile %struct.ScmObj* %ae42835, %struct.ScmObj** %stackaddr$makeclosure49615, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42835, %struct.ScmObj* %k40457, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42835, %struct.ScmObj* %n40188, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42835, %struct.ScmObj* %lst40187, i64 2)
%stackaddr$makeclosure49617 = alloca %struct.ScmObj*, align 8
%fptrToInt49618 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42836 to i64
%ae42836 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49618)
store volatile %struct.ScmObj* %ae42836, %struct.ScmObj** %stackaddr$makeclosure49617, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42836, %struct.ScmObj* %k40457, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42836, %struct.ScmObj* %n40188, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42836, %struct.ScmObj* %lst40187, i64 2)
%args48372$anf_45bind40326$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49619 = alloca %struct.ScmObj*, align 8
%args48372$anf_45bind40326$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42836, %struct.ScmObj* %args48372$anf_45bind40326$0)
store volatile %struct.ScmObj* %args48372$anf_45bind40326$1, %struct.ScmObj** %stackaddr$prim49619, align 8
%stackaddr$prim49620 = alloca %struct.ScmObj*, align 8
%args48372$anf_45bind40326$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42835, %struct.ScmObj* %args48372$anf_45bind40326$1)
store volatile %struct.ScmObj* %args48372$anf_45bind40326$2, %struct.ScmObj** %stackaddr$prim49620, align 8
%clofunc49621 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40326)
musttail call tailcc void %clofunc49621(%struct.ScmObj* %anf_45bind40326, %struct.ScmObj* %args48372$anf_45bind40326$2)
ret void
}

define tailcc void @proc_clo$ae42835(%struct.ScmObj* %env$ae42835,%struct.ScmObj* %current_45args48362) {
%stackaddr$env-ref49622 = alloca %struct.ScmObj*, align 8
%k40457 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42835, i64 0)
store %struct.ScmObj* %k40457, %struct.ScmObj** %stackaddr$env-ref49622
%stackaddr$env-ref49623 = alloca %struct.ScmObj*, align 8
%n40188 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42835, i64 1)
store %struct.ScmObj* %n40188, %struct.ScmObj** %stackaddr$env-ref49623
%stackaddr$env-ref49624 = alloca %struct.ScmObj*, align 8
%lst40187 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42835, i64 2)
store %struct.ScmObj* %lst40187, %struct.ScmObj** %stackaddr$env-ref49624
%stackaddr$prim49625 = alloca %struct.ScmObj*, align 8
%_95k40459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48362)
store volatile %struct.ScmObj* %_95k40459, %struct.ScmObj** %stackaddr$prim49625, align 8
%stackaddr$prim49626 = alloca %struct.ScmObj*, align 8
%current_45args48363 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48362)
store volatile %struct.ScmObj* %current_45args48363, %struct.ScmObj** %stackaddr$prim49626, align 8
%stackaddr$prim49627 = alloca %struct.ScmObj*, align 8
%cc40189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48363)
store volatile %struct.ScmObj* %cc40189, %struct.ScmObj** %stackaddr$prim49627, align 8
%ae42978 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49628 = alloca %struct.ScmObj*, align 8
%anf_45bind40327 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40188, %struct.ScmObj* %ae42978)
store volatile %struct.ScmObj* %anf_45bind40327, %struct.ScmObj** %stackaddr$prim49628, align 8
%ae42979 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49629 = alloca %struct.ScmObj*, align 8
%anf_45bind40328 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42979, %struct.ScmObj* %anf_45bind40327)
store volatile %struct.ScmObj* %anf_45bind40328, %struct.ScmObj** %stackaddr$prim49629, align 8
%truthy$cmp49630 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40328)
%cmp$cmp49630 = icmp eq i64 %truthy$cmp49630, 1
br i1 %cmp$cmp49630, label %truebranch$cmp49630, label %falsebranch$cmp49630
truebranch$cmp49630:
%ae42983 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49631 = alloca %struct.ScmObj*, align 8
%cpsprim40460 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40187, %struct.ScmObj* %ae42983)
store volatile %struct.ScmObj* %cpsprim40460, %struct.ScmObj** %stackaddr$prim49631, align 8
%ae42985 = call %struct.ScmObj* @const_init_int(i64 0)
%args48365$k40457$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49632 = alloca %struct.ScmObj*, align 8
%args48365$k40457$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40460, %struct.ScmObj* %args48365$k40457$0)
store volatile %struct.ScmObj* %args48365$k40457$1, %struct.ScmObj** %stackaddr$prim49632, align 8
%stackaddr$prim49633 = alloca %struct.ScmObj*, align 8
%args48365$k40457$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42985, %struct.ScmObj* %args48365$k40457$1)
store volatile %struct.ScmObj* %args48365$k40457$2, %struct.ScmObj** %stackaddr$prim49633, align 8
%clofunc49634 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40457)
musttail call tailcc void %clofunc49634(%struct.ScmObj* %k40457, %struct.ScmObj* %args48365$k40457$2)
ret void
falsebranch$cmp49630:
%ae42996 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49635 = alloca %struct.ScmObj*, align 8
%anf_45bind40329 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40187, %struct.ScmObj* %ae42996)
store volatile %struct.ScmObj* %anf_45bind40329, %struct.ScmObj** %stackaddr$prim49635, align 8
%stackaddr$prim49636 = alloca %struct.ScmObj*, align 8
%anf_45bind40330 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40329)
store volatile %struct.ScmObj* %anf_45bind40330, %struct.ScmObj** %stackaddr$prim49636, align 8
%ae42999 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49637 = alloca %struct.ScmObj*, align 8
%_95040192 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40187, %struct.ScmObj* %ae42999, %struct.ScmObj* %anf_45bind40330)
store volatile %struct.ScmObj* %_95040192, %struct.ScmObj** %stackaddr$prim49637, align 8
%ae43002 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49638 = alloca %struct.ScmObj*, align 8
%anf_45bind40331 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40188, %struct.ScmObj* %ae43002)
store volatile %struct.ScmObj* %anf_45bind40331, %struct.ScmObj** %stackaddr$prim49638, align 8
%ae43004 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49639 = alloca %struct.ScmObj*, align 8
%anf_45bind40332 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40331, %struct.ScmObj* %ae43004)
store volatile %struct.ScmObj* %anf_45bind40332, %struct.ScmObj** %stackaddr$prim49639, align 8
%ae43006 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49640 = alloca %struct.ScmObj*, align 8
%_95140191 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40188, %struct.ScmObj* %ae43006, %struct.ScmObj* %anf_45bind40332)
store volatile %struct.ScmObj* %_95140191, %struct.ScmObj** %stackaddr$prim49640, align 8
%args48366$cc40189$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49641 = alloca %struct.ScmObj*, align 8
%args48366$cc40189$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40189, %struct.ScmObj* %args48366$cc40189$0)
store volatile %struct.ScmObj* %args48366$cc40189$1, %struct.ScmObj** %stackaddr$prim49641, align 8
%stackaddr$prim49642 = alloca %struct.ScmObj*, align 8
%args48366$cc40189$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40457, %struct.ScmObj* %args48366$cc40189$1)
store volatile %struct.ScmObj* %args48366$cc40189$2, %struct.ScmObj** %stackaddr$prim49642, align 8
%clofunc49643 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40189)
musttail call tailcc void %clofunc49643(%struct.ScmObj* %cc40189, %struct.ScmObj* %args48366$cc40189$2)
ret void
}

define tailcc void @proc_clo$ae42836(%struct.ScmObj* %env$ae42836,%struct.ScmObj* %current_45args48367) {
%stackaddr$env-ref49644 = alloca %struct.ScmObj*, align 8
%k40457 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42836, i64 0)
store %struct.ScmObj* %k40457, %struct.ScmObj** %stackaddr$env-ref49644
%stackaddr$env-ref49645 = alloca %struct.ScmObj*, align 8
%n40188 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42836, i64 1)
store %struct.ScmObj* %n40188, %struct.ScmObj** %stackaddr$env-ref49645
%stackaddr$env-ref49646 = alloca %struct.ScmObj*, align 8
%lst40187 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42836, i64 2)
store %struct.ScmObj* %lst40187, %struct.ScmObj** %stackaddr$env-ref49646
%stackaddr$prim49647 = alloca %struct.ScmObj*, align 8
%_95k40459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48367)
store volatile %struct.ScmObj* %_95k40459, %struct.ScmObj** %stackaddr$prim49647, align 8
%stackaddr$prim49648 = alloca %struct.ScmObj*, align 8
%current_45args48368 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48367)
store volatile %struct.ScmObj* %current_45args48368, %struct.ScmObj** %stackaddr$prim49648, align 8
%stackaddr$prim49649 = alloca %struct.ScmObj*, align 8
%cc40189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48368)
store volatile %struct.ScmObj* %cc40189, %struct.ScmObj** %stackaddr$prim49649, align 8
%ae42838 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49650 = alloca %struct.ScmObj*, align 8
%anf_45bind40327 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40188, %struct.ScmObj* %ae42838)
store volatile %struct.ScmObj* %anf_45bind40327, %struct.ScmObj** %stackaddr$prim49650, align 8
%ae42839 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49651 = alloca %struct.ScmObj*, align 8
%anf_45bind40328 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42839, %struct.ScmObj* %anf_45bind40327)
store volatile %struct.ScmObj* %anf_45bind40328, %struct.ScmObj** %stackaddr$prim49651, align 8
%truthy$cmp49652 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40328)
%cmp$cmp49652 = icmp eq i64 %truthy$cmp49652, 1
br i1 %cmp$cmp49652, label %truebranch$cmp49652, label %falsebranch$cmp49652
truebranch$cmp49652:
%ae42843 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49653 = alloca %struct.ScmObj*, align 8
%cpsprim40460 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40187, %struct.ScmObj* %ae42843)
store volatile %struct.ScmObj* %cpsprim40460, %struct.ScmObj** %stackaddr$prim49653, align 8
%ae42845 = call %struct.ScmObj* @const_init_int(i64 0)
%args48370$k40457$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49654 = alloca %struct.ScmObj*, align 8
%args48370$k40457$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40460, %struct.ScmObj* %args48370$k40457$0)
store volatile %struct.ScmObj* %args48370$k40457$1, %struct.ScmObj** %stackaddr$prim49654, align 8
%stackaddr$prim49655 = alloca %struct.ScmObj*, align 8
%args48370$k40457$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42845, %struct.ScmObj* %args48370$k40457$1)
store volatile %struct.ScmObj* %args48370$k40457$2, %struct.ScmObj** %stackaddr$prim49655, align 8
%clofunc49656 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40457)
musttail call tailcc void %clofunc49656(%struct.ScmObj* %k40457, %struct.ScmObj* %args48370$k40457$2)
ret void
falsebranch$cmp49652:
%ae42856 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49657 = alloca %struct.ScmObj*, align 8
%anf_45bind40329 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40187, %struct.ScmObj* %ae42856)
store volatile %struct.ScmObj* %anf_45bind40329, %struct.ScmObj** %stackaddr$prim49657, align 8
%stackaddr$prim49658 = alloca %struct.ScmObj*, align 8
%anf_45bind40330 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40329)
store volatile %struct.ScmObj* %anf_45bind40330, %struct.ScmObj** %stackaddr$prim49658, align 8
%ae42859 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49659 = alloca %struct.ScmObj*, align 8
%_95040192 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40187, %struct.ScmObj* %ae42859, %struct.ScmObj* %anf_45bind40330)
store volatile %struct.ScmObj* %_95040192, %struct.ScmObj** %stackaddr$prim49659, align 8
%ae42862 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49660 = alloca %struct.ScmObj*, align 8
%anf_45bind40331 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40188, %struct.ScmObj* %ae42862)
store volatile %struct.ScmObj* %anf_45bind40331, %struct.ScmObj** %stackaddr$prim49660, align 8
%ae42864 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49661 = alloca %struct.ScmObj*, align 8
%anf_45bind40332 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40331, %struct.ScmObj* %ae42864)
store volatile %struct.ScmObj* %anf_45bind40332, %struct.ScmObj** %stackaddr$prim49661, align 8
%ae42866 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49662 = alloca %struct.ScmObj*, align 8
%_95140191 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40188, %struct.ScmObj* %ae42866, %struct.ScmObj* %anf_45bind40332)
store volatile %struct.ScmObj* %_95140191, %struct.ScmObj** %stackaddr$prim49662, align 8
%args48371$cc40189$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49663 = alloca %struct.ScmObj*, align 8
%args48371$cc40189$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40189, %struct.ScmObj* %args48371$cc40189$0)
store volatile %struct.ScmObj* %args48371$cc40189$1, %struct.ScmObj** %stackaddr$prim49663, align 8
%stackaddr$prim49664 = alloca %struct.ScmObj*, align 8
%args48371$cc40189$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40457, %struct.ScmObj* %args48371$cc40189$1)
store volatile %struct.ScmObj* %args48371$cc40189$2, %struct.ScmObj** %stackaddr$prim49664, align 8
%clofunc49665 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40189)
musttail call tailcc void %clofunc49665(%struct.ScmObj* %cc40189, %struct.ScmObj* %args48371$cc40189$2)
ret void
}

define tailcc void @proc_clo$ae42821(%struct.ScmObj* %env$ae42821,%struct.ScmObj* %current_45args48373) {
%stackaddr$prim49666 = alloca %struct.ScmObj*, align 8
%k40461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48373)
store volatile %struct.ScmObj* %k40461, %struct.ScmObj** %stackaddr$prim49666, align 8
%stackaddr$prim49667 = alloca %struct.ScmObj*, align 8
%current_45args48374 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48373)
store volatile %struct.ScmObj* %current_45args48374, %struct.ScmObj** %stackaddr$prim49667, align 8
%stackaddr$prim49668 = alloca %struct.ScmObj*, align 8
%u40190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48374)
store volatile %struct.ScmObj* %u40190, %struct.ScmObj** %stackaddr$prim49668, align 8
%args48376$u40190$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49669 = alloca %struct.ScmObj*, align 8
%args48376$u40190$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40190, %struct.ScmObj* %args48376$u40190$0)
store volatile %struct.ScmObj* %args48376$u40190$1, %struct.ScmObj** %stackaddr$prim49669, align 8
%stackaddr$prim49670 = alloca %struct.ScmObj*, align 8
%args48376$u40190$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40461, %struct.ScmObj* %args48376$u40190$1)
store volatile %struct.ScmObj* %args48376$u40190$2, %struct.ScmObj** %stackaddr$prim49670, align 8
%clofunc49671 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40190)
musttail call tailcc void %clofunc49671(%struct.ScmObj* %u40190, %struct.ScmObj* %args48376$u40190$2)
ret void
}

define tailcc void @proc_clo$ae42398(%struct.ScmObj* %env$ae42398,%struct.ScmObj* %current_45args48379) {
%stackaddr$prim49672 = alloca %struct.ScmObj*, align 8
%k40462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48379)
store volatile %struct.ScmObj* %k40462, %struct.ScmObj** %stackaddr$prim49672, align 8
%stackaddr$prim49673 = alloca %struct.ScmObj*, align 8
%current_45args48380 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48379)
store volatile %struct.ScmObj* %current_45args48380, %struct.ScmObj** %stackaddr$prim49673, align 8
%stackaddr$prim49674 = alloca %struct.ScmObj*, align 8
%a40194 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48380)
store volatile %struct.ScmObj* %a40194, %struct.ScmObj** %stackaddr$prim49674, align 8
%ae42399 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49675 = alloca %struct.ScmObj*, align 8
%a40195 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42399, %struct.ScmObj* %a40194)
store volatile %struct.ScmObj* %a40195, %struct.ScmObj** %stackaddr$prim49675, align 8
%stackaddr$makeclosure49676 = alloca %struct.ScmObj*, align 8
%fptrToInt49677 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42401 to i64
%ae42401 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49677)
store volatile %struct.ScmObj* %ae42401, %struct.ScmObj** %stackaddr$makeclosure49676, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42401, %struct.ScmObj* %a40195, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42401, %struct.ScmObj* %k40462, i64 1)
%ae42402 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49678 = alloca %struct.ScmObj*, align 8
%fptrToInt49679 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42403 to i64
%ae42403 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49679)
store volatile %struct.ScmObj* %ae42403, %struct.ScmObj** %stackaddr$makeclosure49678, align 8
%args48402$ae42401$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49680 = alloca %struct.ScmObj*, align 8
%args48402$ae42401$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42403, %struct.ScmObj* %args48402$ae42401$0)
store volatile %struct.ScmObj* %args48402$ae42401$1, %struct.ScmObj** %stackaddr$prim49680, align 8
%stackaddr$prim49681 = alloca %struct.ScmObj*, align 8
%args48402$ae42401$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42402, %struct.ScmObj* %args48402$ae42401$1)
store volatile %struct.ScmObj* %args48402$ae42401$2, %struct.ScmObj** %stackaddr$prim49681, align 8
%clofunc49682 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42401)
musttail call tailcc void %clofunc49682(%struct.ScmObj* %ae42401, %struct.ScmObj* %args48402$ae42401$2)
ret void
}

define tailcc void @proc_clo$ae42401(%struct.ScmObj* %env$ae42401,%struct.ScmObj* %current_45args48382) {
%stackaddr$env-ref49683 = alloca %struct.ScmObj*, align 8
%a40195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42401, i64 0)
store %struct.ScmObj* %a40195, %struct.ScmObj** %stackaddr$env-ref49683
%stackaddr$env-ref49684 = alloca %struct.ScmObj*, align 8
%k40462 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42401, i64 1)
store %struct.ScmObj* %k40462, %struct.ScmObj** %stackaddr$env-ref49684
%stackaddr$prim49685 = alloca %struct.ScmObj*, align 8
%_95k40463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48382)
store volatile %struct.ScmObj* %_95k40463, %struct.ScmObj** %stackaddr$prim49685, align 8
%stackaddr$prim49686 = alloca %struct.ScmObj*, align 8
%current_45args48383 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48382)
store volatile %struct.ScmObj* %current_45args48383, %struct.ScmObj** %stackaddr$prim49686, align 8
%stackaddr$prim49687 = alloca %struct.ScmObj*, align 8
%anf_45bind40318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48383)
store volatile %struct.ScmObj* %anf_45bind40318, %struct.ScmObj** %stackaddr$prim49687, align 8
%stackaddr$makeclosure49688 = alloca %struct.ScmObj*, align 8
%fptrToInt49689 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42420 to i64
%ae42420 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49689)
store volatile %struct.ScmObj* %ae42420, %struct.ScmObj** %stackaddr$makeclosure49688, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42420, %struct.ScmObj* %a40195, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42420, %struct.ScmObj* %k40462, i64 1)
%stackaddr$makeclosure49690 = alloca %struct.ScmObj*, align 8
%fptrToInt49691 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42421 to i64
%ae42421 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49691)
store volatile %struct.ScmObj* %ae42421, %struct.ScmObj** %stackaddr$makeclosure49690, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42421, %struct.ScmObj* %a40195, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42421, %struct.ScmObj* %k40462, i64 1)
%args48397$anf_45bind40318$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49692 = alloca %struct.ScmObj*, align 8
%args48397$anf_45bind40318$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42421, %struct.ScmObj* %args48397$anf_45bind40318$0)
store volatile %struct.ScmObj* %args48397$anf_45bind40318$1, %struct.ScmObj** %stackaddr$prim49692, align 8
%stackaddr$prim49693 = alloca %struct.ScmObj*, align 8
%args48397$anf_45bind40318$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42420, %struct.ScmObj* %args48397$anf_45bind40318$1)
store volatile %struct.ScmObj* %args48397$anf_45bind40318$2, %struct.ScmObj** %stackaddr$prim49693, align 8
%clofunc49694 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40318)
musttail call tailcc void %clofunc49694(%struct.ScmObj* %anf_45bind40318, %struct.ScmObj* %args48397$anf_45bind40318$2)
ret void
}

define tailcc void @proc_clo$ae42420(%struct.ScmObj* %env$ae42420,%struct.ScmObj* %current_45args48385) {
%stackaddr$env-ref49695 = alloca %struct.ScmObj*, align 8
%a40195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42420, i64 0)
store %struct.ScmObj* %a40195, %struct.ScmObj** %stackaddr$env-ref49695
%stackaddr$env-ref49696 = alloca %struct.ScmObj*, align 8
%k40462 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42420, i64 1)
store %struct.ScmObj* %k40462, %struct.ScmObj** %stackaddr$env-ref49696
%stackaddr$prim49697 = alloca %struct.ScmObj*, align 8
%_95k40464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48385)
store volatile %struct.ScmObj* %_95k40464, %struct.ScmObj** %stackaddr$prim49697, align 8
%stackaddr$prim49698 = alloca %struct.ScmObj*, align 8
%current_45args48386 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48385)
store volatile %struct.ScmObj* %current_45args48386, %struct.ScmObj** %stackaddr$prim49698, align 8
%stackaddr$prim49699 = alloca %struct.ScmObj*, align 8
%cc40196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48386)
store volatile %struct.ScmObj* %cc40196, %struct.ScmObj** %stackaddr$prim49699, align 8
%ae42536 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49700 = alloca %struct.ScmObj*, align 8
%anf_45bind40319 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40195, %struct.ScmObj* %ae42536)
store volatile %struct.ScmObj* %anf_45bind40319, %struct.ScmObj** %stackaddr$prim49700, align 8
%stackaddr$prim49701 = alloca %struct.ScmObj*, align 8
%anf_45bind40320 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40319)
store volatile %struct.ScmObj* %anf_45bind40320, %struct.ScmObj** %stackaddr$prim49701, align 8
%truthy$cmp49702 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40320)
%cmp$cmp49702 = icmp eq i64 %truthy$cmp49702, 1
br i1 %cmp$cmp49702, label %truebranch$cmp49702, label %falsebranch$cmp49702
truebranch$cmp49702:
%ae42540 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42541 = call %struct.ScmObj* @const_init_true()
%args48388$k40462$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49703 = alloca %struct.ScmObj*, align 8
%args48388$k40462$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42541, %struct.ScmObj* %args48388$k40462$0)
store volatile %struct.ScmObj* %args48388$k40462$1, %struct.ScmObj** %stackaddr$prim49703, align 8
%stackaddr$prim49704 = alloca %struct.ScmObj*, align 8
%args48388$k40462$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42540, %struct.ScmObj* %args48388$k40462$1)
store volatile %struct.ScmObj* %args48388$k40462$2, %struct.ScmObj** %stackaddr$prim49704, align 8
%clofunc49705 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40462)
musttail call tailcc void %clofunc49705(%struct.ScmObj* %k40462, %struct.ScmObj* %args48388$k40462$2)
ret void
falsebranch$cmp49702:
%ae42549 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49706 = alloca %struct.ScmObj*, align 8
%anf_45bind40321 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40195, %struct.ScmObj* %ae42549)
store volatile %struct.ScmObj* %anf_45bind40321, %struct.ScmObj** %stackaddr$prim49706, align 8
%stackaddr$prim49707 = alloca %struct.ScmObj*, align 8
%anf_45bind40322 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40321)
store volatile %struct.ScmObj* %anf_45bind40322, %struct.ScmObj** %stackaddr$prim49707, align 8
%truthy$cmp49708 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40322)
%cmp$cmp49708 = icmp eq i64 %truthy$cmp49708, 1
br i1 %cmp$cmp49708, label %truebranch$cmp49708, label %falsebranch$cmp49708
truebranch$cmp49708:
%ae42553 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49709 = alloca %struct.ScmObj*, align 8
%anf_45bind40323 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40195, %struct.ScmObj* %ae42553)
store volatile %struct.ScmObj* %anf_45bind40323, %struct.ScmObj** %stackaddr$prim49709, align 8
%stackaddr$prim49710 = alloca %struct.ScmObj*, align 8
%b40198 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40323)
store volatile %struct.ScmObj* %b40198, %struct.ScmObj** %stackaddr$prim49710, align 8
%ae42556 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49711 = alloca %struct.ScmObj*, align 8
%anf_45bind40324 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40195, %struct.ScmObj* %ae42556)
store volatile %struct.ScmObj* %anf_45bind40324, %struct.ScmObj** %stackaddr$prim49711, align 8
%stackaddr$prim49712 = alloca %struct.ScmObj*, align 8
%anf_45bind40325 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40324)
store volatile %struct.ScmObj* %anf_45bind40325, %struct.ScmObj** %stackaddr$prim49712, align 8
%ae42559 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49713 = alloca %struct.ScmObj*, align 8
%_95040199 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40195, %struct.ScmObj* %ae42559, %struct.ScmObj* %anf_45bind40325)
store volatile %struct.ScmObj* %_95040199, %struct.ScmObj** %stackaddr$prim49713, align 8
%args48389$cc40196$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49714 = alloca %struct.ScmObj*, align 8
%args48389$cc40196$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40196, %struct.ScmObj* %args48389$cc40196$0)
store volatile %struct.ScmObj* %args48389$cc40196$1, %struct.ScmObj** %stackaddr$prim49714, align 8
%stackaddr$prim49715 = alloca %struct.ScmObj*, align 8
%args48389$cc40196$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40462, %struct.ScmObj* %args48389$cc40196$1)
store volatile %struct.ScmObj* %args48389$cc40196$2, %struct.ScmObj** %stackaddr$prim49715, align 8
%clofunc49716 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40196)
musttail call tailcc void %clofunc49716(%struct.ScmObj* %cc40196, %struct.ScmObj* %args48389$cc40196$2)
ret void
falsebranch$cmp49708:
%ae42592 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42593 = call %struct.ScmObj* @const_init_false()
%args48390$k40462$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49717 = alloca %struct.ScmObj*, align 8
%args48390$k40462$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42593, %struct.ScmObj* %args48390$k40462$0)
store volatile %struct.ScmObj* %args48390$k40462$1, %struct.ScmObj** %stackaddr$prim49717, align 8
%stackaddr$prim49718 = alloca %struct.ScmObj*, align 8
%args48390$k40462$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42592, %struct.ScmObj* %args48390$k40462$1)
store volatile %struct.ScmObj* %args48390$k40462$2, %struct.ScmObj** %stackaddr$prim49718, align 8
%clofunc49719 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40462)
musttail call tailcc void %clofunc49719(%struct.ScmObj* %k40462, %struct.ScmObj* %args48390$k40462$2)
ret void
}

define tailcc void @proc_clo$ae42421(%struct.ScmObj* %env$ae42421,%struct.ScmObj* %current_45args48391) {
%stackaddr$env-ref49720 = alloca %struct.ScmObj*, align 8
%a40195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42421, i64 0)
store %struct.ScmObj* %a40195, %struct.ScmObj** %stackaddr$env-ref49720
%stackaddr$env-ref49721 = alloca %struct.ScmObj*, align 8
%k40462 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42421, i64 1)
store %struct.ScmObj* %k40462, %struct.ScmObj** %stackaddr$env-ref49721
%stackaddr$prim49722 = alloca %struct.ScmObj*, align 8
%_95k40464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48391)
store volatile %struct.ScmObj* %_95k40464, %struct.ScmObj** %stackaddr$prim49722, align 8
%stackaddr$prim49723 = alloca %struct.ScmObj*, align 8
%current_45args48392 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48391)
store volatile %struct.ScmObj* %current_45args48392, %struct.ScmObj** %stackaddr$prim49723, align 8
%stackaddr$prim49724 = alloca %struct.ScmObj*, align 8
%cc40196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48392)
store volatile %struct.ScmObj* %cc40196, %struct.ScmObj** %stackaddr$prim49724, align 8
%ae42423 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49725 = alloca %struct.ScmObj*, align 8
%anf_45bind40319 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40195, %struct.ScmObj* %ae42423)
store volatile %struct.ScmObj* %anf_45bind40319, %struct.ScmObj** %stackaddr$prim49725, align 8
%stackaddr$prim49726 = alloca %struct.ScmObj*, align 8
%anf_45bind40320 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40319)
store volatile %struct.ScmObj* %anf_45bind40320, %struct.ScmObj** %stackaddr$prim49726, align 8
%truthy$cmp49727 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40320)
%cmp$cmp49727 = icmp eq i64 %truthy$cmp49727, 1
br i1 %cmp$cmp49727, label %truebranch$cmp49727, label %falsebranch$cmp49727
truebranch$cmp49727:
%ae42427 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42428 = call %struct.ScmObj* @const_init_true()
%args48394$k40462$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49728 = alloca %struct.ScmObj*, align 8
%args48394$k40462$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42428, %struct.ScmObj* %args48394$k40462$0)
store volatile %struct.ScmObj* %args48394$k40462$1, %struct.ScmObj** %stackaddr$prim49728, align 8
%stackaddr$prim49729 = alloca %struct.ScmObj*, align 8
%args48394$k40462$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42427, %struct.ScmObj* %args48394$k40462$1)
store volatile %struct.ScmObj* %args48394$k40462$2, %struct.ScmObj** %stackaddr$prim49729, align 8
%clofunc49730 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40462)
musttail call tailcc void %clofunc49730(%struct.ScmObj* %k40462, %struct.ScmObj* %args48394$k40462$2)
ret void
falsebranch$cmp49727:
%ae42436 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49731 = alloca %struct.ScmObj*, align 8
%anf_45bind40321 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40195, %struct.ScmObj* %ae42436)
store volatile %struct.ScmObj* %anf_45bind40321, %struct.ScmObj** %stackaddr$prim49731, align 8
%stackaddr$prim49732 = alloca %struct.ScmObj*, align 8
%anf_45bind40322 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40321)
store volatile %struct.ScmObj* %anf_45bind40322, %struct.ScmObj** %stackaddr$prim49732, align 8
%truthy$cmp49733 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40322)
%cmp$cmp49733 = icmp eq i64 %truthy$cmp49733, 1
br i1 %cmp$cmp49733, label %truebranch$cmp49733, label %falsebranch$cmp49733
truebranch$cmp49733:
%ae42440 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49734 = alloca %struct.ScmObj*, align 8
%anf_45bind40323 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40195, %struct.ScmObj* %ae42440)
store volatile %struct.ScmObj* %anf_45bind40323, %struct.ScmObj** %stackaddr$prim49734, align 8
%stackaddr$prim49735 = alloca %struct.ScmObj*, align 8
%b40198 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40323)
store volatile %struct.ScmObj* %b40198, %struct.ScmObj** %stackaddr$prim49735, align 8
%ae42443 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49736 = alloca %struct.ScmObj*, align 8
%anf_45bind40324 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40195, %struct.ScmObj* %ae42443)
store volatile %struct.ScmObj* %anf_45bind40324, %struct.ScmObj** %stackaddr$prim49736, align 8
%stackaddr$prim49737 = alloca %struct.ScmObj*, align 8
%anf_45bind40325 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40324)
store volatile %struct.ScmObj* %anf_45bind40325, %struct.ScmObj** %stackaddr$prim49737, align 8
%ae42446 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49738 = alloca %struct.ScmObj*, align 8
%_95040199 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40195, %struct.ScmObj* %ae42446, %struct.ScmObj* %anf_45bind40325)
store volatile %struct.ScmObj* %_95040199, %struct.ScmObj** %stackaddr$prim49738, align 8
%args48395$cc40196$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49739 = alloca %struct.ScmObj*, align 8
%args48395$cc40196$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40196, %struct.ScmObj* %args48395$cc40196$0)
store volatile %struct.ScmObj* %args48395$cc40196$1, %struct.ScmObj** %stackaddr$prim49739, align 8
%stackaddr$prim49740 = alloca %struct.ScmObj*, align 8
%args48395$cc40196$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40462, %struct.ScmObj* %args48395$cc40196$1)
store volatile %struct.ScmObj* %args48395$cc40196$2, %struct.ScmObj** %stackaddr$prim49740, align 8
%clofunc49741 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40196)
musttail call tailcc void %clofunc49741(%struct.ScmObj* %cc40196, %struct.ScmObj* %args48395$cc40196$2)
ret void
falsebranch$cmp49733:
%ae42479 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42480 = call %struct.ScmObj* @const_init_false()
%args48396$k40462$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49742 = alloca %struct.ScmObj*, align 8
%args48396$k40462$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42480, %struct.ScmObj* %args48396$k40462$0)
store volatile %struct.ScmObj* %args48396$k40462$1, %struct.ScmObj** %stackaddr$prim49742, align 8
%stackaddr$prim49743 = alloca %struct.ScmObj*, align 8
%args48396$k40462$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42479, %struct.ScmObj* %args48396$k40462$1)
store volatile %struct.ScmObj* %args48396$k40462$2, %struct.ScmObj** %stackaddr$prim49743, align 8
%clofunc49744 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40462)
musttail call tailcc void %clofunc49744(%struct.ScmObj* %k40462, %struct.ScmObj* %args48396$k40462$2)
ret void
}

define tailcc void @proc_clo$ae42403(%struct.ScmObj* %env$ae42403,%struct.ScmObj* %current_45args48398) {
%stackaddr$prim49745 = alloca %struct.ScmObj*, align 8
%k40465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48398)
store volatile %struct.ScmObj* %k40465, %struct.ScmObj** %stackaddr$prim49745, align 8
%stackaddr$prim49746 = alloca %struct.ScmObj*, align 8
%current_45args48399 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48398)
store volatile %struct.ScmObj* %current_45args48399, %struct.ScmObj** %stackaddr$prim49746, align 8
%stackaddr$prim49747 = alloca %struct.ScmObj*, align 8
%k40197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48399)
store volatile %struct.ScmObj* %k40197, %struct.ScmObj** %stackaddr$prim49747, align 8
%ae42405 = call %struct.ScmObj* @const_init_int(i64 0)
%args48401$k40465$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49748 = alloca %struct.ScmObj*, align 8
%args48401$k40465$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40197, %struct.ScmObj* %args48401$k40465$0)
store volatile %struct.ScmObj* %args48401$k40465$1, %struct.ScmObj** %stackaddr$prim49748, align 8
%stackaddr$prim49749 = alloca %struct.ScmObj*, align 8
%args48401$k40465$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42405, %struct.ScmObj* %args48401$k40465$1)
store volatile %struct.ScmObj* %args48401$k40465$2, %struct.ScmObj** %stackaddr$prim49749, align 8
%clofunc49750 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40465)
musttail call tailcc void %clofunc49750(%struct.ScmObj* %k40465, %struct.ScmObj* %args48401$k40465$2)
ret void
}

define tailcc void @proc_clo$ae42326(%struct.ScmObj* %env$ae42326,%struct.ScmObj* %current_45args48404) {
%stackaddr$env-ref49751 = alloca %struct.ScmObj*, align 8
%_37append40201 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42326, i64 0)
store %struct.ScmObj* %_37append40201, %struct.ScmObj** %stackaddr$env-ref49751
%stackaddr$prim49752 = alloca %struct.ScmObj*, align 8
%k40466 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48404)
store volatile %struct.ScmObj* %k40466, %struct.ScmObj** %stackaddr$prim49752, align 8
%stackaddr$prim49753 = alloca %struct.ScmObj*, align 8
%current_45args48405 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48404)
store volatile %struct.ScmObj* %current_45args48405, %struct.ScmObj** %stackaddr$prim49753, align 8
%stackaddr$prim49754 = alloca %struct.ScmObj*, align 8
%ls040204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48405)
store volatile %struct.ScmObj* %ls040204, %struct.ScmObj** %stackaddr$prim49754, align 8
%stackaddr$prim49755 = alloca %struct.ScmObj*, align 8
%current_45args48406 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48405)
store volatile %struct.ScmObj* %current_45args48406, %struct.ScmObj** %stackaddr$prim49755, align 8
%stackaddr$prim49756 = alloca %struct.ScmObj*, align 8
%ls140203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48406)
store volatile %struct.ScmObj* %ls140203, %struct.ScmObj** %stackaddr$prim49756, align 8
%stackaddr$prim49757 = alloca %struct.ScmObj*, align 8
%anf_45bind40312 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls040204)
store volatile %struct.ScmObj* %anf_45bind40312, %struct.ScmObj** %stackaddr$prim49757, align 8
%truthy$cmp49758 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40312)
%cmp$cmp49758 = icmp eq i64 %truthy$cmp49758, 1
br i1 %cmp$cmp49758, label %truebranch$cmp49758, label %falsebranch$cmp49758
truebranch$cmp49758:
%ae42330 = call %struct.ScmObj* @const_init_int(i64 0)
%args48408$k40466$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49759 = alloca %struct.ScmObj*, align 8
%args48408$k40466$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140203, %struct.ScmObj* %args48408$k40466$0)
store volatile %struct.ScmObj* %args48408$k40466$1, %struct.ScmObj** %stackaddr$prim49759, align 8
%stackaddr$prim49760 = alloca %struct.ScmObj*, align 8
%args48408$k40466$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42330, %struct.ScmObj* %args48408$k40466$1)
store volatile %struct.ScmObj* %args48408$k40466$2, %struct.ScmObj** %stackaddr$prim49760, align 8
%clofunc49761 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40466)
musttail call tailcc void %clofunc49761(%struct.ScmObj* %k40466, %struct.ScmObj* %args48408$k40466$2)
ret void
falsebranch$cmp49758:
%stackaddr$prim49762 = alloca %struct.ScmObj*, align 8
%anf_45bind40313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls040204)
store volatile %struct.ScmObj* %anf_45bind40313, %struct.ScmObj** %stackaddr$prim49762, align 8
%ae42337 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49763 = alloca %struct.ScmObj*, align 8
%anf_45bind40314 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40201, %struct.ScmObj* %ae42337)
store volatile %struct.ScmObj* %anf_45bind40314, %struct.ScmObj** %stackaddr$prim49763, align 8
%stackaddr$prim49764 = alloca %struct.ScmObj*, align 8
%anf_45bind40315 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls040204)
store volatile %struct.ScmObj* %anf_45bind40315, %struct.ScmObj** %stackaddr$prim49764, align 8
%stackaddr$makeclosure49765 = alloca %struct.ScmObj*, align 8
%fptrToInt49766 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42340 to i64
%ae42340 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49766)
store volatile %struct.ScmObj* %ae42340, %struct.ScmObj** %stackaddr$makeclosure49765, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42340, %struct.ScmObj* %anf_45bind40313, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42340, %struct.ScmObj* %k40466, i64 1)
%args48413$anf_45bind40314$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49767 = alloca %struct.ScmObj*, align 8
%args48413$anf_45bind40314$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140203, %struct.ScmObj* %args48413$anf_45bind40314$0)
store volatile %struct.ScmObj* %args48413$anf_45bind40314$1, %struct.ScmObj** %stackaddr$prim49767, align 8
%stackaddr$prim49768 = alloca %struct.ScmObj*, align 8
%args48413$anf_45bind40314$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40315, %struct.ScmObj* %args48413$anf_45bind40314$1)
store volatile %struct.ScmObj* %args48413$anf_45bind40314$2, %struct.ScmObj** %stackaddr$prim49768, align 8
%stackaddr$prim49769 = alloca %struct.ScmObj*, align 8
%args48413$anf_45bind40314$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42340, %struct.ScmObj* %args48413$anf_45bind40314$2)
store volatile %struct.ScmObj* %args48413$anf_45bind40314$3, %struct.ScmObj** %stackaddr$prim49769, align 8
%clofunc49770 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40314)
musttail call tailcc void %clofunc49770(%struct.ScmObj* %anf_45bind40314, %struct.ScmObj* %args48413$anf_45bind40314$3)
ret void
}

define tailcc void @proc_clo$ae42340(%struct.ScmObj* %env$ae42340,%struct.ScmObj* %current_45args48409) {
%stackaddr$env-ref49771 = alloca %struct.ScmObj*, align 8
%anf_45bind40313 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42340, i64 0)
store %struct.ScmObj* %anf_45bind40313, %struct.ScmObj** %stackaddr$env-ref49771
%stackaddr$env-ref49772 = alloca %struct.ScmObj*, align 8
%k40466 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42340, i64 1)
store %struct.ScmObj* %k40466, %struct.ScmObj** %stackaddr$env-ref49772
%stackaddr$prim49773 = alloca %struct.ScmObj*, align 8
%_95k40467 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48409)
store volatile %struct.ScmObj* %_95k40467, %struct.ScmObj** %stackaddr$prim49773, align 8
%stackaddr$prim49774 = alloca %struct.ScmObj*, align 8
%current_45args48410 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48409)
store volatile %struct.ScmObj* %current_45args48410, %struct.ScmObj** %stackaddr$prim49774, align 8
%stackaddr$prim49775 = alloca %struct.ScmObj*, align 8
%anf_45bind40316 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48410)
store volatile %struct.ScmObj* %anf_45bind40316, %struct.ScmObj** %stackaddr$prim49775, align 8
%stackaddr$prim49776 = alloca %struct.ScmObj*, align 8
%cpsprim40468 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40313, %struct.ScmObj* %anf_45bind40316)
store volatile %struct.ScmObj* %cpsprim40468, %struct.ScmObj** %stackaddr$prim49776, align 8
%ae42346 = call %struct.ScmObj* @const_init_int(i64 0)
%args48412$k40466$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49777 = alloca %struct.ScmObj*, align 8
%args48412$k40466$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40468, %struct.ScmObj* %args48412$k40466$0)
store volatile %struct.ScmObj* %args48412$k40466$1, %struct.ScmObj** %stackaddr$prim49777, align 8
%stackaddr$prim49778 = alloca %struct.ScmObj*, align 8
%args48412$k40466$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42346, %struct.ScmObj* %args48412$k40466$1)
store volatile %struct.ScmObj* %args48412$k40466$2, %struct.ScmObj** %stackaddr$prim49778, align 8
%clofunc49779 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40466)
musttail call tailcc void %clofunc49779(%struct.ScmObj* %k40466, %struct.ScmObj* %args48412$k40466$2)
ret void
}

define tailcc void @proc_clo$ae42300(%struct.ScmObj* %env$ae42300,%struct.ScmObj* %current_45args48415) {
%stackaddr$prim49780 = alloca %struct.ScmObj*, align 8
%k40469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48415)
store volatile %struct.ScmObj* %k40469, %struct.ScmObj** %stackaddr$prim49780, align 8
%stackaddr$prim49781 = alloca %struct.ScmObj*, align 8
%current_45args48416 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48415)
store volatile %struct.ScmObj* %current_45args48416, %struct.ScmObj** %stackaddr$prim49781, align 8
%stackaddr$prim49782 = alloca %struct.ScmObj*, align 8
%a40207 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48416)
store volatile %struct.ScmObj* %a40207, %struct.ScmObj** %stackaddr$prim49782, align 8
%stackaddr$prim49783 = alloca %struct.ScmObj*, align 8
%current_45args48417 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48416)
store volatile %struct.ScmObj* %current_45args48417, %struct.ScmObj** %stackaddr$prim49783, align 8
%stackaddr$prim49784 = alloca %struct.ScmObj*, align 8
%b40206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48417)
store volatile %struct.ScmObj* %b40206, %struct.ScmObj** %stackaddr$prim49784, align 8
%stackaddr$prim49785 = alloca %struct.ScmObj*, align 8
%anf_45bind40311 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a40207, %struct.ScmObj* %b40206)
store volatile %struct.ScmObj* %anf_45bind40311, %struct.ScmObj** %stackaddr$prim49785, align 8
%stackaddr$prim49786 = alloca %struct.ScmObj*, align 8
%cpsprim40470 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40311)
store volatile %struct.ScmObj* %cpsprim40470, %struct.ScmObj** %stackaddr$prim49786, align 8
%ae42305 = call %struct.ScmObj* @const_init_int(i64 0)
%args48419$k40469$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49787 = alloca %struct.ScmObj*, align 8
%args48419$k40469$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40470, %struct.ScmObj* %args48419$k40469$0)
store volatile %struct.ScmObj* %args48419$k40469$1, %struct.ScmObj** %stackaddr$prim49787, align 8
%stackaddr$prim49788 = alloca %struct.ScmObj*, align 8
%args48419$k40469$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42305, %struct.ScmObj* %args48419$k40469$1)
store volatile %struct.ScmObj* %args48419$k40469$2, %struct.ScmObj** %stackaddr$prim49788, align 8
%clofunc49789 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40469)
musttail call tailcc void %clofunc49789(%struct.ScmObj* %k40469, %struct.ScmObj* %args48419$k40469$2)
ret void
}

define tailcc void @proc_clo$ae42276(%struct.ScmObj* %env$ae42276,%struct.ScmObj* %current_45args48421) {
%stackaddr$prim49790 = alloca %struct.ScmObj*, align 8
%k40471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48421)
store volatile %struct.ScmObj* %k40471, %struct.ScmObj** %stackaddr$prim49790, align 8
%stackaddr$prim49791 = alloca %struct.ScmObj*, align 8
%current_45args48422 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48421)
store volatile %struct.ScmObj* %current_45args48422, %struct.ScmObj** %stackaddr$prim49791, align 8
%stackaddr$prim49792 = alloca %struct.ScmObj*, align 8
%a40210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48422)
store volatile %struct.ScmObj* %a40210, %struct.ScmObj** %stackaddr$prim49792, align 8
%stackaddr$prim49793 = alloca %struct.ScmObj*, align 8
%current_45args48423 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48422)
store volatile %struct.ScmObj* %current_45args48423, %struct.ScmObj** %stackaddr$prim49793, align 8
%stackaddr$prim49794 = alloca %struct.ScmObj*, align 8
%b40209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48423)
store volatile %struct.ScmObj* %b40209, %struct.ScmObj** %stackaddr$prim49794, align 8
%stackaddr$prim49795 = alloca %struct.ScmObj*, align 8
%anf_45bind40310 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a40210, %struct.ScmObj* %b40209)
store volatile %struct.ScmObj* %anf_45bind40310, %struct.ScmObj** %stackaddr$prim49795, align 8
%stackaddr$prim49796 = alloca %struct.ScmObj*, align 8
%cpsprim40472 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40310)
store volatile %struct.ScmObj* %cpsprim40472, %struct.ScmObj** %stackaddr$prim49796, align 8
%ae42281 = call %struct.ScmObj* @const_init_int(i64 0)
%args48425$k40471$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49797 = alloca %struct.ScmObj*, align 8
%args48425$k40471$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40472, %struct.ScmObj* %args48425$k40471$0)
store volatile %struct.ScmObj* %args48425$k40471$1, %struct.ScmObj** %stackaddr$prim49797, align 8
%stackaddr$prim49798 = alloca %struct.ScmObj*, align 8
%args48425$k40471$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42281, %struct.ScmObj* %args48425$k40471$1)
store volatile %struct.ScmObj* %args48425$k40471$2, %struct.ScmObj** %stackaddr$prim49798, align 8
%clofunc49799 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40471)
musttail call tailcc void %clofunc49799(%struct.ScmObj* %k40471, %struct.ScmObj* %args48425$k40471$2)
ret void
}

define tailcc void @proc_clo$ae41882(%struct.ScmObj* %env$ae41882,%struct.ScmObj* %current_45args48428) {
%stackaddr$env-ref49800 = alloca %struct.ScmObj*, align 8
%_37foldr40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41882, i64 0)
store %struct.ScmObj* %_37foldr40134, %struct.ScmObj** %stackaddr$env-ref49800
%stackaddr$env-ref49801 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41882, i64 1)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref49801
%stackaddr$env-ref49802 = alloca %struct.ScmObj*, align 8
%_37map140160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41882, i64 2)
store %struct.ScmObj* %_37map140160, %struct.ScmObj** %stackaddr$env-ref49802
%stackaddr$prim49803 = alloca %struct.ScmObj*, align 8
%k40473 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48428)
store volatile %struct.ScmObj* %k40473, %struct.ScmObj** %stackaddr$prim49803, align 8
%stackaddr$prim49804 = alloca %struct.ScmObj*, align 8
%current_45args48429 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48428)
store volatile %struct.ScmObj* %current_45args48429, %struct.ScmObj** %stackaddr$prim49804, align 8
%stackaddr$prim49805 = alloca %struct.ScmObj*, align 8
%_37foldl40212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48429)
store volatile %struct.ScmObj* %_37foldl40212, %struct.ScmObj** %stackaddr$prim49805, align 8
%ae41884 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49806 = alloca %struct.ScmObj*, align 8
%fptrToInt49807 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41885 to i64
%ae41885 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49807)
store volatile %struct.ScmObj* %ae41885, %struct.ScmObj** %stackaddr$makeclosure49806, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41885, %struct.ScmObj* %_37foldr40134, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41885, %struct.ScmObj* %_37foldl40212, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41885, %struct.ScmObj* %_37foldr140129, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41885, %struct.ScmObj* %_37map140160, i64 3)
%args48486$k40473$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49808 = alloca %struct.ScmObj*, align 8
%args48486$k40473$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41885, %struct.ScmObj* %args48486$k40473$0)
store volatile %struct.ScmObj* %args48486$k40473$1, %struct.ScmObj** %stackaddr$prim49808, align 8
%stackaddr$prim49809 = alloca %struct.ScmObj*, align 8
%args48486$k40473$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41884, %struct.ScmObj* %args48486$k40473$1)
store volatile %struct.ScmObj* %args48486$k40473$2, %struct.ScmObj** %stackaddr$prim49809, align 8
%clofunc49810 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40473)
musttail call tailcc void %clofunc49810(%struct.ScmObj* %k40473, %struct.ScmObj* %args48486$k40473$2)
ret void
}

define tailcc void @proc_clo$ae41885(%struct.ScmObj* %env$ae41885,%struct.ScmObj* %args4021340474) {
%stackaddr$env-ref49811 = alloca %struct.ScmObj*, align 8
%_37foldr40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41885, i64 0)
store %struct.ScmObj* %_37foldr40134, %struct.ScmObj** %stackaddr$env-ref49811
%stackaddr$env-ref49812 = alloca %struct.ScmObj*, align 8
%_37foldl40212 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41885, i64 1)
store %struct.ScmObj* %_37foldl40212, %struct.ScmObj** %stackaddr$env-ref49812
%stackaddr$env-ref49813 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41885, i64 2)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref49813
%stackaddr$env-ref49814 = alloca %struct.ScmObj*, align 8
%_37map140160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41885, i64 3)
store %struct.ScmObj* %_37map140160, %struct.ScmObj** %stackaddr$env-ref49814
%stackaddr$prim49815 = alloca %struct.ScmObj*, align 8
%k40475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4021340474)
store volatile %struct.ScmObj* %k40475, %struct.ScmObj** %stackaddr$prim49815, align 8
%stackaddr$prim49816 = alloca %struct.ScmObj*, align 8
%args40213 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4021340474)
store volatile %struct.ScmObj* %args40213, %struct.ScmObj** %stackaddr$prim49816, align 8
%stackaddr$prim49817 = alloca %struct.ScmObj*, align 8
%f40216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40213)
store volatile %struct.ScmObj* %f40216, %struct.ScmObj** %stackaddr$prim49817, align 8
%stackaddr$prim49818 = alloca %struct.ScmObj*, align 8
%anf_45bind40298 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40213)
store volatile %struct.ScmObj* %anf_45bind40298, %struct.ScmObj** %stackaddr$prim49818, align 8
%stackaddr$prim49819 = alloca %struct.ScmObj*, align 8
%acc40215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40298)
store volatile %struct.ScmObj* %acc40215, %struct.ScmObj** %stackaddr$prim49819, align 8
%stackaddr$prim49820 = alloca %struct.ScmObj*, align 8
%anf_45bind40299 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40213)
store volatile %struct.ScmObj* %anf_45bind40299, %struct.ScmObj** %stackaddr$prim49820, align 8
%stackaddr$prim49821 = alloca %struct.ScmObj*, align 8
%lsts40214 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40299)
store volatile %struct.ScmObj* %lsts40214, %struct.ScmObj** %stackaddr$prim49821, align 8
%stackaddr$makeclosure49822 = alloca %struct.ScmObj*, align 8
%fptrToInt49823 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41893 to i64
%ae41893 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt49823)
store volatile %struct.ScmObj* %ae41893, %struct.ScmObj** %stackaddr$makeclosure49822, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41893, %struct.ScmObj* %lsts40214, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41893, %struct.ScmObj* %_37foldr40134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41893, %struct.ScmObj* %f40216, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41893, %struct.ScmObj* %acc40215, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41893, %struct.ScmObj* %_37foldl40212, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41893, %struct.ScmObj* %_37foldr140129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41893, %struct.ScmObj* %_37map140160, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41893, %struct.ScmObj* %k40475, i64 7)
%ae41894 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49824 = alloca %struct.ScmObj*, align 8
%fptrToInt49825 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41895 to i64
%ae41895 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49825)
store volatile %struct.ScmObj* %ae41895, %struct.ScmObj** %stackaddr$makeclosure49824, align 8
%args48485$ae41893$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49826 = alloca %struct.ScmObj*, align 8
%args48485$ae41893$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41895, %struct.ScmObj* %args48485$ae41893$0)
store volatile %struct.ScmObj* %args48485$ae41893$1, %struct.ScmObj** %stackaddr$prim49826, align 8
%stackaddr$prim49827 = alloca %struct.ScmObj*, align 8
%args48485$ae41893$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41894, %struct.ScmObj* %args48485$ae41893$1)
store volatile %struct.ScmObj* %args48485$ae41893$2, %struct.ScmObj** %stackaddr$prim49827, align 8
%clofunc49828 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41893)
musttail call tailcc void %clofunc49828(%struct.ScmObj* %ae41893, %struct.ScmObj* %args48485$ae41893$2)
ret void
}

define tailcc void @proc_clo$ae41893(%struct.ScmObj* %env$ae41893,%struct.ScmObj* %current_45args48431) {
%stackaddr$env-ref49829 = alloca %struct.ScmObj*, align 8
%lsts40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41893, i64 0)
store %struct.ScmObj* %lsts40214, %struct.ScmObj** %stackaddr$env-ref49829
%stackaddr$env-ref49830 = alloca %struct.ScmObj*, align 8
%_37foldr40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41893, i64 1)
store %struct.ScmObj* %_37foldr40134, %struct.ScmObj** %stackaddr$env-ref49830
%stackaddr$env-ref49831 = alloca %struct.ScmObj*, align 8
%f40216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41893, i64 2)
store %struct.ScmObj* %f40216, %struct.ScmObj** %stackaddr$env-ref49831
%stackaddr$env-ref49832 = alloca %struct.ScmObj*, align 8
%acc40215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41893, i64 3)
store %struct.ScmObj* %acc40215, %struct.ScmObj** %stackaddr$env-ref49832
%stackaddr$env-ref49833 = alloca %struct.ScmObj*, align 8
%_37foldl40212 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41893, i64 4)
store %struct.ScmObj* %_37foldl40212, %struct.ScmObj** %stackaddr$env-ref49833
%stackaddr$env-ref49834 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41893, i64 5)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref49834
%stackaddr$env-ref49835 = alloca %struct.ScmObj*, align 8
%_37map140160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41893, i64 6)
store %struct.ScmObj* %_37map140160, %struct.ScmObj** %stackaddr$env-ref49835
%stackaddr$env-ref49836 = alloca %struct.ScmObj*, align 8
%k40475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41893, i64 7)
store %struct.ScmObj* %k40475, %struct.ScmObj** %stackaddr$env-ref49836
%stackaddr$prim49837 = alloca %struct.ScmObj*, align 8
%_95k40476 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48431)
store volatile %struct.ScmObj* %_95k40476, %struct.ScmObj** %stackaddr$prim49837, align 8
%stackaddr$prim49838 = alloca %struct.ScmObj*, align 8
%current_45args48432 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48431)
store volatile %struct.ScmObj* %current_45args48432, %struct.ScmObj** %stackaddr$prim49838, align 8
%stackaddr$prim49839 = alloca %struct.ScmObj*, align 8
%anf_45bind40300 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48432)
store volatile %struct.ScmObj* %anf_45bind40300, %struct.ScmObj** %stackaddr$prim49839, align 8
%stackaddr$makeclosure49840 = alloca %struct.ScmObj*, align 8
%fptrToInt49841 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41925 to i64
%ae41925 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49841)
store volatile %struct.ScmObj* %ae41925, %struct.ScmObj** %stackaddr$makeclosure49840, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41925, %struct.ScmObj* %lsts40214, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41925, %struct.ScmObj* %_37foldr40134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41925, %struct.ScmObj* %f40216, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41925, %struct.ScmObj* %acc40215, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41925, %struct.ScmObj* %_37foldl40212, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41925, %struct.ScmObj* %_37map140160, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41925, %struct.ScmObj* %k40475, i64 6)
%ae41927 = call %struct.ScmObj* @const_init_false()
%args48478$_37foldr140129$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49842 = alloca %struct.ScmObj*, align 8
%args48478$_37foldr140129$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40214, %struct.ScmObj* %args48478$_37foldr140129$0)
store volatile %struct.ScmObj* %args48478$_37foldr140129$1, %struct.ScmObj** %stackaddr$prim49842, align 8
%stackaddr$prim49843 = alloca %struct.ScmObj*, align 8
%args48478$_37foldr140129$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41927, %struct.ScmObj* %args48478$_37foldr140129$1)
store volatile %struct.ScmObj* %args48478$_37foldr140129$2, %struct.ScmObj** %stackaddr$prim49843, align 8
%stackaddr$prim49844 = alloca %struct.ScmObj*, align 8
%args48478$_37foldr140129$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40300, %struct.ScmObj* %args48478$_37foldr140129$2)
store volatile %struct.ScmObj* %args48478$_37foldr140129$3, %struct.ScmObj** %stackaddr$prim49844, align 8
%stackaddr$prim49845 = alloca %struct.ScmObj*, align 8
%args48478$_37foldr140129$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41925, %struct.ScmObj* %args48478$_37foldr140129$3)
store volatile %struct.ScmObj* %args48478$_37foldr140129$4, %struct.ScmObj** %stackaddr$prim49845, align 8
%clofunc49846 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140129)
musttail call tailcc void %clofunc49846(%struct.ScmObj* %_37foldr140129, %struct.ScmObj* %args48478$_37foldr140129$4)
ret void
}

define tailcc void @proc_clo$ae41925(%struct.ScmObj* %env$ae41925,%struct.ScmObj* %current_45args48434) {
%stackaddr$env-ref49847 = alloca %struct.ScmObj*, align 8
%lsts40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41925, i64 0)
store %struct.ScmObj* %lsts40214, %struct.ScmObj** %stackaddr$env-ref49847
%stackaddr$env-ref49848 = alloca %struct.ScmObj*, align 8
%_37foldr40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41925, i64 1)
store %struct.ScmObj* %_37foldr40134, %struct.ScmObj** %stackaddr$env-ref49848
%stackaddr$env-ref49849 = alloca %struct.ScmObj*, align 8
%f40216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41925, i64 2)
store %struct.ScmObj* %f40216, %struct.ScmObj** %stackaddr$env-ref49849
%stackaddr$env-ref49850 = alloca %struct.ScmObj*, align 8
%acc40215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41925, i64 3)
store %struct.ScmObj* %acc40215, %struct.ScmObj** %stackaddr$env-ref49850
%stackaddr$env-ref49851 = alloca %struct.ScmObj*, align 8
%_37foldl40212 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41925, i64 4)
store %struct.ScmObj* %_37foldl40212, %struct.ScmObj** %stackaddr$env-ref49851
%stackaddr$env-ref49852 = alloca %struct.ScmObj*, align 8
%_37map140160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41925, i64 5)
store %struct.ScmObj* %_37map140160, %struct.ScmObj** %stackaddr$env-ref49852
%stackaddr$env-ref49853 = alloca %struct.ScmObj*, align 8
%k40475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41925, i64 6)
store %struct.ScmObj* %k40475, %struct.ScmObj** %stackaddr$env-ref49853
%stackaddr$prim49854 = alloca %struct.ScmObj*, align 8
%_95k40477 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48434)
store volatile %struct.ScmObj* %_95k40477, %struct.ScmObj** %stackaddr$prim49854, align 8
%stackaddr$prim49855 = alloca %struct.ScmObj*, align 8
%current_45args48435 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48434)
store volatile %struct.ScmObj* %current_45args48435, %struct.ScmObj** %stackaddr$prim49855, align 8
%stackaddr$prim49856 = alloca %struct.ScmObj*, align 8
%anf_45bind40301 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48435)
store volatile %struct.ScmObj* %anf_45bind40301, %struct.ScmObj** %stackaddr$prim49856, align 8
%truthy$cmp49857 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40301)
%cmp$cmp49857 = icmp eq i64 %truthy$cmp49857, 1
br i1 %cmp$cmp49857, label %truebranch$cmp49857, label %falsebranch$cmp49857
truebranch$cmp49857:
%ae41936 = call %struct.ScmObj* @const_init_int(i64 0)
%args48437$k40475$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49858 = alloca %struct.ScmObj*, align 8
%args48437$k40475$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40215, %struct.ScmObj* %args48437$k40475$0)
store volatile %struct.ScmObj* %args48437$k40475$1, %struct.ScmObj** %stackaddr$prim49858, align 8
%stackaddr$prim49859 = alloca %struct.ScmObj*, align 8
%args48437$k40475$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41936, %struct.ScmObj* %args48437$k40475$1)
store volatile %struct.ScmObj* %args48437$k40475$2, %struct.ScmObj** %stackaddr$prim49859, align 8
%clofunc49860 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40475)
musttail call tailcc void %clofunc49860(%struct.ScmObj* %k40475, %struct.ScmObj* %args48437$k40475$2)
ret void
falsebranch$cmp49857:
%stackaddr$makeclosure49861 = alloca %struct.ScmObj*, align 8
%fptrToInt49862 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41941 to i64
%ae41941 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49862)
store volatile %struct.ScmObj* %ae41941, %struct.ScmObj** %stackaddr$makeclosure49861, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41941, %struct.ScmObj* %lsts40214, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41941, %struct.ScmObj* %_37foldr40134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41941, %struct.ScmObj* %f40216, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41941, %struct.ScmObj* %acc40215, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41941, %struct.ScmObj* %_37foldl40212, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41941, %struct.ScmObj* %_37map140160, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41941, %struct.ScmObj* %k40475, i64 6)
%ae41942 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49863 = alloca %struct.ScmObj*, align 8
%fptrToInt49864 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41943 to i64
%ae41943 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49864)
store volatile %struct.ScmObj* %ae41943, %struct.ScmObj** %stackaddr$makeclosure49863, align 8
%args48477$ae41941$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49865 = alloca %struct.ScmObj*, align 8
%args48477$ae41941$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41943, %struct.ScmObj* %args48477$ae41941$0)
store volatile %struct.ScmObj* %args48477$ae41941$1, %struct.ScmObj** %stackaddr$prim49865, align 8
%stackaddr$prim49866 = alloca %struct.ScmObj*, align 8
%args48477$ae41941$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41942, %struct.ScmObj* %args48477$ae41941$1)
store volatile %struct.ScmObj* %args48477$ae41941$2, %struct.ScmObj** %stackaddr$prim49866, align 8
%clofunc49867 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41941)
musttail call tailcc void %clofunc49867(%struct.ScmObj* %ae41941, %struct.ScmObj* %args48477$ae41941$2)
ret void
}

define tailcc void @proc_clo$ae41941(%struct.ScmObj* %env$ae41941,%struct.ScmObj* %current_45args48438) {
%stackaddr$env-ref49868 = alloca %struct.ScmObj*, align 8
%lsts40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41941, i64 0)
store %struct.ScmObj* %lsts40214, %struct.ScmObj** %stackaddr$env-ref49868
%stackaddr$env-ref49869 = alloca %struct.ScmObj*, align 8
%_37foldr40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41941, i64 1)
store %struct.ScmObj* %_37foldr40134, %struct.ScmObj** %stackaddr$env-ref49869
%stackaddr$env-ref49870 = alloca %struct.ScmObj*, align 8
%f40216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41941, i64 2)
store %struct.ScmObj* %f40216, %struct.ScmObj** %stackaddr$env-ref49870
%stackaddr$env-ref49871 = alloca %struct.ScmObj*, align 8
%acc40215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41941, i64 3)
store %struct.ScmObj* %acc40215, %struct.ScmObj** %stackaddr$env-ref49871
%stackaddr$env-ref49872 = alloca %struct.ScmObj*, align 8
%_37foldl40212 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41941, i64 4)
store %struct.ScmObj* %_37foldl40212, %struct.ScmObj** %stackaddr$env-ref49872
%stackaddr$env-ref49873 = alloca %struct.ScmObj*, align 8
%_37map140160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41941, i64 5)
store %struct.ScmObj* %_37map140160, %struct.ScmObj** %stackaddr$env-ref49873
%stackaddr$env-ref49874 = alloca %struct.ScmObj*, align 8
%k40475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41941, i64 6)
store %struct.ScmObj* %k40475, %struct.ScmObj** %stackaddr$env-ref49874
%stackaddr$prim49875 = alloca %struct.ScmObj*, align 8
%_95k40478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48438)
store volatile %struct.ScmObj* %_95k40478, %struct.ScmObj** %stackaddr$prim49875, align 8
%stackaddr$prim49876 = alloca %struct.ScmObj*, align 8
%current_45args48439 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48438)
store volatile %struct.ScmObj* %current_45args48439, %struct.ScmObj** %stackaddr$prim49876, align 8
%stackaddr$prim49877 = alloca %struct.ScmObj*, align 8
%anf_45bind40302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48439)
store volatile %struct.ScmObj* %anf_45bind40302, %struct.ScmObj** %stackaddr$prim49877, align 8
%stackaddr$makeclosure49878 = alloca %struct.ScmObj*, align 8
%fptrToInt49879 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41962 to i64
%ae41962 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49879)
store volatile %struct.ScmObj* %ae41962, %struct.ScmObj** %stackaddr$makeclosure49878, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41962, %struct.ScmObj* %lsts40214, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41962, %struct.ScmObj* %_37foldr40134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41962, %struct.ScmObj* %f40216, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41962, %struct.ScmObj* %acc40215, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41962, %struct.ScmObj* %_37foldl40212, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41962, %struct.ScmObj* %_37map140160, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41962, %struct.ScmObj* %k40475, i64 6)
%args48472$_37map140160$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49880 = alloca %struct.ScmObj*, align 8
%args48472$_37map140160$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40214, %struct.ScmObj* %args48472$_37map140160$0)
store volatile %struct.ScmObj* %args48472$_37map140160$1, %struct.ScmObj** %stackaddr$prim49880, align 8
%stackaddr$prim49881 = alloca %struct.ScmObj*, align 8
%args48472$_37map140160$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40302, %struct.ScmObj* %args48472$_37map140160$1)
store volatile %struct.ScmObj* %args48472$_37map140160$2, %struct.ScmObj** %stackaddr$prim49881, align 8
%stackaddr$prim49882 = alloca %struct.ScmObj*, align 8
%args48472$_37map140160$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41962, %struct.ScmObj* %args48472$_37map140160$2)
store volatile %struct.ScmObj* %args48472$_37map140160$3, %struct.ScmObj** %stackaddr$prim49882, align 8
%clofunc49883 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140160)
musttail call tailcc void %clofunc49883(%struct.ScmObj* %_37map140160, %struct.ScmObj* %args48472$_37map140160$3)
ret void
}

define tailcc void @proc_clo$ae41962(%struct.ScmObj* %env$ae41962,%struct.ScmObj* %current_45args48441) {
%stackaddr$env-ref49884 = alloca %struct.ScmObj*, align 8
%lsts40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41962, i64 0)
store %struct.ScmObj* %lsts40214, %struct.ScmObj** %stackaddr$env-ref49884
%stackaddr$env-ref49885 = alloca %struct.ScmObj*, align 8
%_37foldr40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41962, i64 1)
store %struct.ScmObj* %_37foldr40134, %struct.ScmObj** %stackaddr$env-ref49885
%stackaddr$env-ref49886 = alloca %struct.ScmObj*, align 8
%f40216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41962, i64 2)
store %struct.ScmObj* %f40216, %struct.ScmObj** %stackaddr$env-ref49886
%stackaddr$env-ref49887 = alloca %struct.ScmObj*, align 8
%acc40215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41962, i64 3)
store %struct.ScmObj* %acc40215, %struct.ScmObj** %stackaddr$env-ref49887
%stackaddr$env-ref49888 = alloca %struct.ScmObj*, align 8
%_37foldl40212 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41962, i64 4)
store %struct.ScmObj* %_37foldl40212, %struct.ScmObj** %stackaddr$env-ref49888
%stackaddr$env-ref49889 = alloca %struct.ScmObj*, align 8
%_37map140160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41962, i64 5)
store %struct.ScmObj* %_37map140160, %struct.ScmObj** %stackaddr$env-ref49889
%stackaddr$env-ref49890 = alloca %struct.ScmObj*, align 8
%k40475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41962, i64 6)
store %struct.ScmObj* %k40475, %struct.ScmObj** %stackaddr$env-ref49890
%stackaddr$prim49891 = alloca %struct.ScmObj*, align 8
%_95k40479 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48441)
store volatile %struct.ScmObj* %_95k40479, %struct.ScmObj** %stackaddr$prim49891, align 8
%stackaddr$prim49892 = alloca %struct.ScmObj*, align 8
%current_45args48442 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48441)
store volatile %struct.ScmObj* %current_45args48442, %struct.ScmObj** %stackaddr$prim49892, align 8
%stackaddr$prim49893 = alloca %struct.ScmObj*, align 8
%lsts_4340221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48442)
store volatile %struct.ScmObj* %lsts_4340221, %struct.ScmObj** %stackaddr$prim49893, align 8
%stackaddr$makeclosure49894 = alloca %struct.ScmObj*, align 8
%fptrToInt49895 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41965 to i64
%ae41965 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt49895)
store volatile %struct.ScmObj* %ae41965, %struct.ScmObj** %stackaddr$makeclosure49894, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41965, %struct.ScmObj* %lsts40214, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41965, %struct.ScmObj* %_37foldr40134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41965, %struct.ScmObj* %f40216, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41965, %struct.ScmObj* %acc40215, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41965, %struct.ScmObj* %_37foldl40212, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41965, %struct.ScmObj* %_37map140160, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41965, %struct.ScmObj* %lsts_4340221, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41965, %struct.ScmObj* %k40475, i64 7)
%ae41966 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49896 = alloca %struct.ScmObj*, align 8
%fptrToInt49897 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41967 to i64
%ae41967 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49897)
store volatile %struct.ScmObj* %ae41967, %struct.ScmObj** %stackaddr$makeclosure49896, align 8
%args48471$ae41965$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49898 = alloca %struct.ScmObj*, align 8
%args48471$ae41965$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41967, %struct.ScmObj* %args48471$ae41965$0)
store volatile %struct.ScmObj* %args48471$ae41965$1, %struct.ScmObj** %stackaddr$prim49898, align 8
%stackaddr$prim49899 = alloca %struct.ScmObj*, align 8
%args48471$ae41965$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41966, %struct.ScmObj* %args48471$ae41965$1)
store volatile %struct.ScmObj* %args48471$ae41965$2, %struct.ScmObj** %stackaddr$prim49899, align 8
%clofunc49900 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41965)
musttail call tailcc void %clofunc49900(%struct.ScmObj* %ae41965, %struct.ScmObj* %args48471$ae41965$2)
ret void
}

define tailcc void @proc_clo$ae41965(%struct.ScmObj* %env$ae41965,%struct.ScmObj* %current_45args48444) {
%stackaddr$env-ref49901 = alloca %struct.ScmObj*, align 8
%lsts40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41965, i64 0)
store %struct.ScmObj* %lsts40214, %struct.ScmObj** %stackaddr$env-ref49901
%stackaddr$env-ref49902 = alloca %struct.ScmObj*, align 8
%_37foldr40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41965, i64 1)
store %struct.ScmObj* %_37foldr40134, %struct.ScmObj** %stackaddr$env-ref49902
%stackaddr$env-ref49903 = alloca %struct.ScmObj*, align 8
%f40216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41965, i64 2)
store %struct.ScmObj* %f40216, %struct.ScmObj** %stackaddr$env-ref49903
%stackaddr$env-ref49904 = alloca %struct.ScmObj*, align 8
%acc40215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41965, i64 3)
store %struct.ScmObj* %acc40215, %struct.ScmObj** %stackaddr$env-ref49904
%stackaddr$env-ref49905 = alloca %struct.ScmObj*, align 8
%_37foldl40212 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41965, i64 4)
store %struct.ScmObj* %_37foldl40212, %struct.ScmObj** %stackaddr$env-ref49905
%stackaddr$env-ref49906 = alloca %struct.ScmObj*, align 8
%_37map140160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41965, i64 5)
store %struct.ScmObj* %_37map140160, %struct.ScmObj** %stackaddr$env-ref49906
%stackaddr$env-ref49907 = alloca %struct.ScmObj*, align 8
%lsts_4340221 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41965, i64 6)
store %struct.ScmObj* %lsts_4340221, %struct.ScmObj** %stackaddr$env-ref49907
%stackaddr$env-ref49908 = alloca %struct.ScmObj*, align 8
%k40475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41965, i64 7)
store %struct.ScmObj* %k40475, %struct.ScmObj** %stackaddr$env-ref49908
%stackaddr$prim49909 = alloca %struct.ScmObj*, align 8
%_95k40480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48444)
store volatile %struct.ScmObj* %_95k40480, %struct.ScmObj** %stackaddr$prim49909, align 8
%stackaddr$prim49910 = alloca %struct.ScmObj*, align 8
%current_45args48445 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48444)
store volatile %struct.ScmObj* %current_45args48445, %struct.ScmObj** %stackaddr$prim49910, align 8
%stackaddr$prim49911 = alloca %struct.ScmObj*, align 8
%anf_45bind40303 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48445)
store volatile %struct.ScmObj* %anf_45bind40303, %struct.ScmObj** %stackaddr$prim49911, align 8
%stackaddr$makeclosure49912 = alloca %struct.ScmObj*, align 8
%fptrToInt49913 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41986 to i64
%ae41986 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt49913)
store volatile %struct.ScmObj* %ae41986, %struct.ScmObj** %stackaddr$makeclosure49912, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41986, %struct.ScmObj* %f40216, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41986, %struct.ScmObj* %acc40215, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41986, %struct.ScmObj* %_37foldr40134, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41986, %struct.ScmObj* %_37foldl40212, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41986, %struct.ScmObj* %lsts_4340221, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41986, %struct.ScmObj* %k40475, i64 5)
%args48466$_37map140160$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49914 = alloca %struct.ScmObj*, align 8
%args48466$_37map140160$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40214, %struct.ScmObj* %args48466$_37map140160$0)
store volatile %struct.ScmObj* %args48466$_37map140160$1, %struct.ScmObj** %stackaddr$prim49914, align 8
%stackaddr$prim49915 = alloca %struct.ScmObj*, align 8
%args48466$_37map140160$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40303, %struct.ScmObj* %args48466$_37map140160$1)
store volatile %struct.ScmObj* %args48466$_37map140160$2, %struct.ScmObj** %stackaddr$prim49915, align 8
%stackaddr$prim49916 = alloca %struct.ScmObj*, align 8
%args48466$_37map140160$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41986, %struct.ScmObj* %args48466$_37map140160$2)
store volatile %struct.ScmObj* %args48466$_37map140160$3, %struct.ScmObj** %stackaddr$prim49916, align 8
%clofunc49917 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140160)
musttail call tailcc void %clofunc49917(%struct.ScmObj* %_37map140160, %struct.ScmObj* %args48466$_37map140160$3)
ret void
}

define tailcc void @proc_clo$ae41986(%struct.ScmObj* %env$ae41986,%struct.ScmObj* %current_45args48447) {
%stackaddr$env-ref49918 = alloca %struct.ScmObj*, align 8
%f40216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41986, i64 0)
store %struct.ScmObj* %f40216, %struct.ScmObj** %stackaddr$env-ref49918
%stackaddr$env-ref49919 = alloca %struct.ScmObj*, align 8
%acc40215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41986, i64 1)
store %struct.ScmObj* %acc40215, %struct.ScmObj** %stackaddr$env-ref49919
%stackaddr$env-ref49920 = alloca %struct.ScmObj*, align 8
%_37foldr40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41986, i64 2)
store %struct.ScmObj* %_37foldr40134, %struct.ScmObj** %stackaddr$env-ref49920
%stackaddr$env-ref49921 = alloca %struct.ScmObj*, align 8
%_37foldl40212 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41986, i64 3)
store %struct.ScmObj* %_37foldl40212, %struct.ScmObj** %stackaddr$env-ref49921
%stackaddr$env-ref49922 = alloca %struct.ScmObj*, align 8
%lsts_4340221 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41986, i64 4)
store %struct.ScmObj* %lsts_4340221, %struct.ScmObj** %stackaddr$env-ref49922
%stackaddr$env-ref49923 = alloca %struct.ScmObj*, align 8
%k40475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41986, i64 5)
store %struct.ScmObj* %k40475, %struct.ScmObj** %stackaddr$env-ref49923
%stackaddr$prim49924 = alloca %struct.ScmObj*, align 8
%_95k40481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48447)
store volatile %struct.ScmObj* %_95k40481, %struct.ScmObj** %stackaddr$prim49924, align 8
%stackaddr$prim49925 = alloca %struct.ScmObj*, align 8
%current_45args48448 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48447)
store volatile %struct.ScmObj* %current_45args48448, %struct.ScmObj** %stackaddr$prim49925, align 8
%stackaddr$prim49926 = alloca %struct.ScmObj*, align 8
%vs40219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48448)
store volatile %struct.ScmObj* %vs40219, %struct.ScmObj** %stackaddr$prim49926, align 8
%stackaddr$makeclosure49927 = alloca %struct.ScmObj*, align 8
%fptrToInt49928 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41989 to i64
%ae41989 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49928)
store volatile %struct.ScmObj* %ae41989, %struct.ScmObj** %stackaddr$makeclosure49927, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41989, %struct.ScmObj* %k40475, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41989, %struct.ScmObj* %vs40219, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41989, %struct.ScmObj* %f40216, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41989, %struct.ScmObj* %acc40215, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41989, %struct.ScmObj* %_37foldr40134, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41989, %struct.ScmObj* %_37foldl40212, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41989, %struct.ScmObj* %lsts_4340221, i64 6)
%ae41990 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49929 = alloca %struct.ScmObj*, align 8
%fptrToInt49930 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41991 to i64
%ae41991 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49930)
store volatile %struct.ScmObj* %ae41991, %struct.ScmObj** %stackaddr$makeclosure49929, align 8
%args48465$ae41989$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49931 = alloca %struct.ScmObj*, align 8
%args48465$ae41989$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41991, %struct.ScmObj* %args48465$ae41989$0)
store volatile %struct.ScmObj* %args48465$ae41989$1, %struct.ScmObj** %stackaddr$prim49931, align 8
%stackaddr$prim49932 = alloca %struct.ScmObj*, align 8
%args48465$ae41989$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41990, %struct.ScmObj* %args48465$ae41989$1)
store volatile %struct.ScmObj* %args48465$ae41989$2, %struct.ScmObj** %stackaddr$prim49932, align 8
%clofunc49933 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41989)
musttail call tailcc void %clofunc49933(%struct.ScmObj* %ae41989, %struct.ScmObj* %args48465$ae41989$2)
ret void
}

define tailcc void @proc_clo$ae41989(%struct.ScmObj* %env$ae41989,%struct.ScmObj* %current_45args48450) {
%stackaddr$env-ref49934 = alloca %struct.ScmObj*, align 8
%k40475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41989, i64 0)
store %struct.ScmObj* %k40475, %struct.ScmObj** %stackaddr$env-ref49934
%stackaddr$env-ref49935 = alloca %struct.ScmObj*, align 8
%vs40219 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41989, i64 1)
store %struct.ScmObj* %vs40219, %struct.ScmObj** %stackaddr$env-ref49935
%stackaddr$env-ref49936 = alloca %struct.ScmObj*, align 8
%f40216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41989, i64 2)
store %struct.ScmObj* %f40216, %struct.ScmObj** %stackaddr$env-ref49936
%stackaddr$env-ref49937 = alloca %struct.ScmObj*, align 8
%acc40215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41989, i64 3)
store %struct.ScmObj* %acc40215, %struct.ScmObj** %stackaddr$env-ref49937
%stackaddr$env-ref49938 = alloca %struct.ScmObj*, align 8
%_37foldr40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41989, i64 4)
store %struct.ScmObj* %_37foldr40134, %struct.ScmObj** %stackaddr$env-ref49938
%stackaddr$env-ref49939 = alloca %struct.ScmObj*, align 8
%_37foldl40212 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41989, i64 5)
store %struct.ScmObj* %_37foldl40212, %struct.ScmObj** %stackaddr$env-ref49939
%stackaddr$env-ref49940 = alloca %struct.ScmObj*, align 8
%lsts_4340221 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41989, i64 6)
store %struct.ScmObj* %lsts_4340221, %struct.ScmObj** %stackaddr$env-ref49940
%stackaddr$prim49941 = alloca %struct.ScmObj*, align 8
%_95k40482 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48450)
store volatile %struct.ScmObj* %_95k40482, %struct.ScmObj** %stackaddr$prim49941, align 8
%stackaddr$prim49942 = alloca %struct.ScmObj*, align 8
%current_45args48451 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48450)
store volatile %struct.ScmObj* %current_45args48451, %struct.ScmObj** %stackaddr$prim49942, align 8
%stackaddr$prim49943 = alloca %struct.ScmObj*, align 8
%anf_45bind40304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48451)
store volatile %struct.ScmObj* %anf_45bind40304, %struct.ScmObj** %stackaddr$prim49943, align 8
%ae42012 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49944 = alloca %struct.ScmObj*, align 8
%anf_45bind40305 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40215, %struct.ScmObj* %ae42012)
store volatile %struct.ScmObj* %anf_45bind40305, %struct.ScmObj** %stackaddr$prim49944, align 8
%stackaddr$makeclosure49945 = alloca %struct.ScmObj*, align 8
%fptrToInt49946 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42014 to i64
%ae42014 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49946)
store volatile %struct.ScmObj* %ae42014, %struct.ScmObj** %stackaddr$makeclosure49945, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42014, %struct.ScmObj* %f40216, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42014, %struct.ScmObj* %_37foldl40212, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42014, %struct.ScmObj* %lsts_4340221, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae42014, %struct.ScmObj* %k40475, i64 3)
%args48459$_37foldr40134$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49947 = alloca %struct.ScmObj*, align 8
%args48459$_37foldr40134$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40219, %struct.ScmObj* %args48459$_37foldr40134$0)
store volatile %struct.ScmObj* %args48459$_37foldr40134$1, %struct.ScmObj** %stackaddr$prim49947, align 8
%stackaddr$prim49948 = alloca %struct.ScmObj*, align 8
%args48459$_37foldr40134$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40305, %struct.ScmObj* %args48459$_37foldr40134$1)
store volatile %struct.ScmObj* %args48459$_37foldr40134$2, %struct.ScmObj** %stackaddr$prim49948, align 8
%stackaddr$prim49949 = alloca %struct.ScmObj*, align 8
%args48459$_37foldr40134$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40304, %struct.ScmObj* %args48459$_37foldr40134$2)
store volatile %struct.ScmObj* %args48459$_37foldr40134$3, %struct.ScmObj** %stackaddr$prim49949, align 8
%stackaddr$prim49950 = alloca %struct.ScmObj*, align 8
%args48459$_37foldr40134$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42014, %struct.ScmObj* %args48459$_37foldr40134$3)
store volatile %struct.ScmObj* %args48459$_37foldr40134$4, %struct.ScmObj** %stackaddr$prim49950, align 8
%clofunc49951 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40134)
musttail call tailcc void %clofunc49951(%struct.ScmObj* %_37foldr40134, %struct.ScmObj* %args48459$_37foldr40134$4)
ret void
}

define tailcc void @proc_clo$ae42014(%struct.ScmObj* %env$ae42014,%struct.ScmObj* %current_45args48453) {
%stackaddr$env-ref49952 = alloca %struct.ScmObj*, align 8
%f40216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42014, i64 0)
store %struct.ScmObj* %f40216, %struct.ScmObj** %stackaddr$env-ref49952
%stackaddr$env-ref49953 = alloca %struct.ScmObj*, align 8
%_37foldl40212 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42014, i64 1)
store %struct.ScmObj* %_37foldl40212, %struct.ScmObj** %stackaddr$env-ref49953
%stackaddr$env-ref49954 = alloca %struct.ScmObj*, align 8
%lsts_4340221 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42014, i64 2)
store %struct.ScmObj* %lsts_4340221, %struct.ScmObj** %stackaddr$env-ref49954
%stackaddr$env-ref49955 = alloca %struct.ScmObj*, align 8
%k40475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42014, i64 3)
store %struct.ScmObj* %k40475, %struct.ScmObj** %stackaddr$env-ref49955
%stackaddr$prim49956 = alloca %struct.ScmObj*, align 8
%_95k40483 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48453)
store volatile %struct.ScmObj* %_95k40483, %struct.ScmObj** %stackaddr$prim49956, align 8
%stackaddr$prim49957 = alloca %struct.ScmObj*, align 8
%current_45args48454 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48453)
store volatile %struct.ScmObj* %current_45args48454, %struct.ScmObj** %stackaddr$prim49957, align 8
%stackaddr$prim49958 = alloca %struct.ScmObj*, align 8
%anf_45bind40306 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48454)
store volatile %struct.ScmObj* %anf_45bind40306, %struct.ScmObj** %stackaddr$prim49958, align 8
%stackaddr$makeclosure49959 = alloca %struct.ScmObj*, align 8
%fptrToInt49960 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42018 to i64
%ae42018 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49960)
store volatile %struct.ScmObj* %ae42018, %struct.ScmObj** %stackaddr$makeclosure49959, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42018, %struct.ScmObj* %f40216, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42018, %struct.ScmObj* %_37foldl40212, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42018, %struct.ScmObj* %lsts_4340221, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae42018, %struct.ScmObj* %k40475, i64 3)
%stackaddr$prim49961 = alloca %struct.ScmObj*, align 8
%cpsargs40486 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42018, %struct.ScmObj* %anf_45bind40306)
store volatile %struct.ScmObj* %cpsargs40486, %struct.ScmObj** %stackaddr$prim49961, align 8
%clofunc49962 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40216)
musttail call tailcc void %clofunc49962(%struct.ScmObj* %f40216, %struct.ScmObj* %cpsargs40486)
ret void
}

define tailcc void @proc_clo$ae42018(%struct.ScmObj* %env$ae42018,%struct.ScmObj* %current_45args48456) {
%stackaddr$env-ref49963 = alloca %struct.ScmObj*, align 8
%f40216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42018, i64 0)
store %struct.ScmObj* %f40216, %struct.ScmObj** %stackaddr$env-ref49963
%stackaddr$env-ref49964 = alloca %struct.ScmObj*, align 8
%_37foldl40212 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42018, i64 1)
store %struct.ScmObj* %_37foldl40212, %struct.ScmObj** %stackaddr$env-ref49964
%stackaddr$env-ref49965 = alloca %struct.ScmObj*, align 8
%lsts_4340221 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42018, i64 2)
store %struct.ScmObj* %lsts_4340221, %struct.ScmObj** %stackaddr$env-ref49965
%stackaddr$env-ref49966 = alloca %struct.ScmObj*, align 8
%k40475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42018, i64 3)
store %struct.ScmObj* %k40475, %struct.ScmObj** %stackaddr$env-ref49966
%stackaddr$prim49967 = alloca %struct.ScmObj*, align 8
%_95k40484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48456)
store volatile %struct.ScmObj* %_95k40484, %struct.ScmObj** %stackaddr$prim49967, align 8
%stackaddr$prim49968 = alloca %struct.ScmObj*, align 8
%current_45args48457 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48456)
store volatile %struct.ScmObj* %current_45args48457, %struct.ScmObj** %stackaddr$prim49968, align 8
%stackaddr$prim49969 = alloca %struct.ScmObj*, align 8
%acc_4340223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48457)
store volatile %struct.ScmObj* %acc_4340223, %struct.ScmObj** %stackaddr$prim49969, align 8
%stackaddr$prim49970 = alloca %struct.ScmObj*, align 8
%anf_45bind40307 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4340223, %struct.ScmObj* %lsts_4340221)
store volatile %struct.ScmObj* %anf_45bind40307, %struct.ScmObj** %stackaddr$prim49970, align 8
%stackaddr$prim49971 = alloca %struct.ScmObj*, align 8
%anf_45bind40308 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40216, %struct.ScmObj* %anf_45bind40307)
store volatile %struct.ScmObj* %anf_45bind40308, %struct.ScmObj** %stackaddr$prim49971, align 8
%stackaddr$prim49972 = alloca %struct.ScmObj*, align 8
%cpsargs40485 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40475, %struct.ScmObj* %anf_45bind40308)
store volatile %struct.ScmObj* %cpsargs40485, %struct.ScmObj** %stackaddr$prim49972, align 8
%clofunc49973 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl40212)
musttail call tailcc void %clofunc49973(%struct.ScmObj* %_37foldl40212, %struct.ScmObj* %cpsargs40485)
ret void
}

define tailcc void @proc_clo$ae41991(%struct.ScmObj* %env$ae41991,%struct.ScmObj* %current_45args48460) {
%stackaddr$prim49974 = alloca %struct.ScmObj*, align 8
%k40487 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48460)
store volatile %struct.ScmObj* %k40487, %struct.ScmObj** %stackaddr$prim49974, align 8
%stackaddr$prim49975 = alloca %struct.ScmObj*, align 8
%current_45args48461 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48460)
store volatile %struct.ScmObj* %current_45args48461, %struct.ScmObj** %stackaddr$prim49975, align 8
%stackaddr$prim49976 = alloca %struct.ScmObj*, align 8
%a40225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48461)
store volatile %struct.ScmObj* %a40225, %struct.ScmObj** %stackaddr$prim49976, align 8
%stackaddr$prim49977 = alloca %struct.ScmObj*, align 8
%current_45args48462 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48461)
store volatile %struct.ScmObj* %current_45args48462, %struct.ScmObj** %stackaddr$prim49977, align 8
%stackaddr$prim49978 = alloca %struct.ScmObj*, align 8
%b40224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48462)
store volatile %struct.ScmObj* %b40224, %struct.ScmObj** %stackaddr$prim49978, align 8
%stackaddr$prim49979 = alloca %struct.ScmObj*, align 8
%cpsprim40488 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40225, %struct.ScmObj* %b40224)
store volatile %struct.ScmObj* %cpsprim40488, %struct.ScmObj** %stackaddr$prim49979, align 8
%ae41995 = call %struct.ScmObj* @const_init_int(i64 0)
%args48464$k40487$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49980 = alloca %struct.ScmObj*, align 8
%args48464$k40487$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40488, %struct.ScmObj* %args48464$k40487$0)
store volatile %struct.ScmObj* %args48464$k40487$1, %struct.ScmObj** %stackaddr$prim49980, align 8
%stackaddr$prim49981 = alloca %struct.ScmObj*, align 8
%args48464$k40487$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41995, %struct.ScmObj* %args48464$k40487$1)
store volatile %struct.ScmObj* %args48464$k40487$2, %struct.ScmObj** %stackaddr$prim49981, align 8
%clofunc49982 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40487)
musttail call tailcc void %clofunc49982(%struct.ScmObj* %k40487, %struct.ScmObj* %args48464$k40487$2)
ret void
}

define tailcc void @proc_clo$ae41967(%struct.ScmObj* %env$ae41967,%struct.ScmObj* %current_45args48467) {
%stackaddr$prim49983 = alloca %struct.ScmObj*, align 8
%k40489 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48467)
store volatile %struct.ScmObj* %k40489, %struct.ScmObj** %stackaddr$prim49983, align 8
%stackaddr$prim49984 = alloca %struct.ScmObj*, align 8
%current_45args48468 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48467)
store volatile %struct.ScmObj* %current_45args48468, %struct.ScmObj** %stackaddr$prim49984, align 8
%stackaddr$prim49985 = alloca %struct.ScmObj*, align 8
%x40220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48468)
store volatile %struct.ScmObj* %x40220, %struct.ScmObj** %stackaddr$prim49985, align 8
%stackaddr$prim49986 = alloca %struct.ScmObj*, align 8
%cpsprim40490 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40220)
store volatile %struct.ScmObj* %cpsprim40490, %struct.ScmObj** %stackaddr$prim49986, align 8
%ae41970 = call %struct.ScmObj* @const_init_int(i64 0)
%args48470$k40489$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49987 = alloca %struct.ScmObj*, align 8
%args48470$k40489$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40490, %struct.ScmObj* %args48470$k40489$0)
store volatile %struct.ScmObj* %args48470$k40489$1, %struct.ScmObj** %stackaddr$prim49987, align 8
%stackaddr$prim49988 = alloca %struct.ScmObj*, align 8
%args48470$k40489$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41970, %struct.ScmObj* %args48470$k40489$1)
store volatile %struct.ScmObj* %args48470$k40489$2, %struct.ScmObj** %stackaddr$prim49988, align 8
%clofunc49989 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40489)
musttail call tailcc void %clofunc49989(%struct.ScmObj* %k40489, %struct.ScmObj* %args48470$k40489$2)
ret void
}

define tailcc void @proc_clo$ae41943(%struct.ScmObj* %env$ae41943,%struct.ScmObj* %current_45args48473) {
%stackaddr$prim49990 = alloca %struct.ScmObj*, align 8
%k40491 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48473)
store volatile %struct.ScmObj* %k40491, %struct.ScmObj** %stackaddr$prim49990, align 8
%stackaddr$prim49991 = alloca %struct.ScmObj*, align 8
%current_45args48474 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48473)
store volatile %struct.ScmObj* %current_45args48474, %struct.ScmObj** %stackaddr$prim49991, align 8
%stackaddr$prim49992 = alloca %struct.ScmObj*, align 8
%x40222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48474)
store volatile %struct.ScmObj* %x40222, %struct.ScmObj** %stackaddr$prim49992, align 8
%stackaddr$prim49993 = alloca %struct.ScmObj*, align 8
%cpsprim40492 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40222)
store volatile %struct.ScmObj* %cpsprim40492, %struct.ScmObj** %stackaddr$prim49993, align 8
%ae41946 = call %struct.ScmObj* @const_init_int(i64 0)
%args48476$k40491$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49994 = alloca %struct.ScmObj*, align 8
%args48476$k40491$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40492, %struct.ScmObj* %args48476$k40491$0)
store volatile %struct.ScmObj* %args48476$k40491$1, %struct.ScmObj** %stackaddr$prim49994, align 8
%stackaddr$prim49995 = alloca %struct.ScmObj*, align 8
%args48476$k40491$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41946, %struct.ScmObj* %args48476$k40491$1)
store volatile %struct.ScmObj* %args48476$k40491$2, %struct.ScmObj** %stackaddr$prim49995, align 8
%clofunc49996 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40491)
musttail call tailcc void %clofunc49996(%struct.ScmObj* %k40491, %struct.ScmObj* %args48476$k40491$2)
ret void
}

define tailcc void @proc_clo$ae41895(%struct.ScmObj* %env$ae41895,%struct.ScmObj* %current_45args48479) {
%stackaddr$prim49997 = alloca %struct.ScmObj*, align 8
%k40493 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48479)
store volatile %struct.ScmObj* %k40493, %struct.ScmObj** %stackaddr$prim49997, align 8
%stackaddr$prim49998 = alloca %struct.ScmObj*, align 8
%current_45args48480 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48479)
store volatile %struct.ScmObj* %current_45args48480, %struct.ScmObj** %stackaddr$prim49998, align 8
%stackaddr$prim49999 = alloca %struct.ScmObj*, align 8
%lst40218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48480)
store volatile %struct.ScmObj* %lst40218, %struct.ScmObj** %stackaddr$prim49999, align 8
%stackaddr$prim50000 = alloca %struct.ScmObj*, align 8
%current_45args48481 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48480)
store volatile %struct.ScmObj* %current_45args48481, %struct.ScmObj** %stackaddr$prim50000, align 8
%stackaddr$prim50001 = alloca %struct.ScmObj*, align 8
%b40217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48481)
store volatile %struct.ScmObj* %b40217, %struct.ScmObj** %stackaddr$prim50001, align 8
%truthy$cmp50002 = call i64 @is_truthy_value(%struct.ScmObj* %b40217)
%cmp$cmp50002 = icmp eq i64 %truthy$cmp50002, 1
br i1 %cmp$cmp50002, label %truebranch$cmp50002, label %falsebranch$cmp50002
truebranch$cmp50002:
%ae41898 = call %struct.ScmObj* @const_init_int(i64 0)
%args48483$k40493$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50003 = alloca %struct.ScmObj*, align 8
%args48483$k40493$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40217, %struct.ScmObj* %args48483$k40493$0)
store volatile %struct.ScmObj* %args48483$k40493$1, %struct.ScmObj** %stackaddr$prim50003, align 8
%stackaddr$prim50004 = alloca %struct.ScmObj*, align 8
%args48483$k40493$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41898, %struct.ScmObj* %args48483$k40493$1)
store volatile %struct.ScmObj* %args48483$k40493$2, %struct.ScmObj** %stackaddr$prim50004, align 8
%clofunc50005 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40493)
musttail call tailcc void %clofunc50005(%struct.ScmObj* %k40493, %struct.ScmObj* %args48483$k40493$2)
ret void
falsebranch$cmp50002:
%stackaddr$prim50006 = alloca %struct.ScmObj*, align 8
%cpsprim40494 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40218)
store volatile %struct.ScmObj* %cpsprim40494, %struct.ScmObj** %stackaddr$prim50006, align 8
%ae41905 = call %struct.ScmObj* @const_init_int(i64 0)
%args48484$k40493$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50007 = alloca %struct.ScmObj*, align 8
%args48484$k40493$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40494, %struct.ScmObj* %args48484$k40493$0)
store volatile %struct.ScmObj* %args48484$k40493$1, %struct.ScmObj** %stackaddr$prim50007, align 8
%stackaddr$prim50008 = alloca %struct.ScmObj*, align 8
%args48484$k40493$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41905, %struct.ScmObj* %args48484$k40493$1)
store volatile %struct.ScmObj* %args48484$k40493$2, %struct.ScmObj** %stackaddr$prim50008, align 8
%clofunc50009 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40493)
musttail call tailcc void %clofunc50009(%struct.ScmObj* %k40493, %struct.ScmObj* %args48484$k40493$2)
ret void
}

define tailcc void @proc_clo$ae41736(%struct.ScmObj* %env$ae41736,%struct.ScmObj* %args4015640495) {
%stackaddr$env-ref50010 = alloca %struct.ScmObj*, align 8
%_37last40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41736, i64 0)
store %struct.ScmObj* %_37last40151, %struct.ScmObj** %stackaddr$env-ref50010
%stackaddr$env-ref50011 = alloca %struct.ScmObj*, align 8
%_37foldr40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41736, i64 1)
store %struct.ScmObj* %_37foldr40134, %struct.ScmObj** %stackaddr$env-ref50011
%stackaddr$env-ref50012 = alloca %struct.ScmObj*, align 8
%_37drop_45right40148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41736, i64 2)
store %struct.ScmObj* %_37drop_45right40148, %struct.ScmObj** %stackaddr$env-ref50012
%stackaddr$prim50013 = alloca %struct.ScmObj*, align 8
%k40496 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4015640495)
store volatile %struct.ScmObj* %k40496, %struct.ScmObj** %stackaddr$prim50013, align 8
%stackaddr$prim50014 = alloca %struct.ScmObj*, align 8
%args40156 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4015640495)
store volatile %struct.ScmObj* %args40156, %struct.ScmObj** %stackaddr$prim50014, align 8
%stackaddr$prim50015 = alloca %struct.ScmObj*, align 8
%f40158 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40156)
store volatile %struct.ScmObj* %f40158, %struct.ScmObj** %stackaddr$prim50015, align 8
%stackaddr$prim50016 = alloca %struct.ScmObj*, align 8
%lsts40157 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40156)
store volatile %struct.ScmObj* %lsts40157, %struct.ScmObj** %stackaddr$prim50016, align 8
%stackaddr$makeclosure50017 = alloca %struct.ScmObj*, align 8
%fptrToInt50018 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41741 to i64
%ae41741 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50018)
store volatile %struct.ScmObj* %ae41741, %struct.ScmObj** %stackaddr$makeclosure50017, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41741, %struct.ScmObj* %_37foldr40134, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41741, %struct.ScmObj* %k40496, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41741, %struct.ScmObj* %lsts40157, i64 2)
%ae41742 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50019 = alloca %struct.ScmObj*, align 8
%fptrToInt50020 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41743 to i64
%ae41743 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50020)
store volatile %struct.ScmObj* %ae41743, %struct.ScmObj** %stackaddr$makeclosure50019, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41743, %struct.ScmObj* %_37last40151, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41743, %struct.ScmObj* %_37drop_45right40148, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41743, %struct.ScmObj* %f40158, i64 2)
%args48503$ae41741$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50021 = alloca %struct.ScmObj*, align 8
%args48503$ae41741$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41743, %struct.ScmObj* %args48503$ae41741$0)
store volatile %struct.ScmObj* %args48503$ae41741$1, %struct.ScmObj** %stackaddr$prim50021, align 8
%stackaddr$prim50022 = alloca %struct.ScmObj*, align 8
%args48503$ae41741$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41742, %struct.ScmObj* %args48503$ae41741$1)
store volatile %struct.ScmObj* %args48503$ae41741$2, %struct.ScmObj** %stackaddr$prim50022, align 8
%clofunc50023 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41741)
musttail call tailcc void %clofunc50023(%struct.ScmObj* %ae41741, %struct.ScmObj* %args48503$ae41741$2)
ret void
}

define tailcc void @proc_clo$ae41741(%struct.ScmObj* %env$ae41741,%struct.ScmObj* %current_45args48488) {
%stackaddr$env-ref50024 = alloca %struct.ScmObj*, align 8
%_37foldr40134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41741, i64 0)
store %struct.ScmObj* %_37foldr40134, %struct.ScmObj** %stackaddr$env-ref50024
%stackaddr$env-ref50025 = alloca %struct.ScmObj*, align 8
%k40496 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41741, i64 1)
store %struct.ScmObj* %k40496, %struct.ScmObj** %stackaddr$env-ref50025
%stackaddr$env-ref50026 = alloca %struct.ScmObj*, align 8
%lsts40157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41741, i64 2)
store %struct.ScmObj* %lsts40157, %struct.ScmObj** %stackaddr$env-ref50026
%stackaddr$prim50027 = alloca %struct.ScmObj*, align 8
%_95k40497 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48488)
store volatile %struct.ScmObj* %_95k40497, %struct.ScmObj** %stackaddr$prim50027, align 8
%stackaddr$prim50028 = alloca %struct.ScmObj*, align 8
%current_45args48489 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48488)
store volatile %struct.ScmObj* %current_45args48489, %struct.ScmObj** %stackaddr$prim50028, align 8
%stackaddr$prim50029 = alloca %struct.ScmObj*, align 8
%anf_45bind40295 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48489)
store volatile %struct.ScmObj* %anf_45bind40295, %struct.ScmObj** %stackaddr$prim50029, align 8
%ae41804 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50030 = alloca %struct.ScmObj*, align 8
%anf_45bind40296 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41804, %struct.ScmObj* %lsts40157)
store volatile %struct.ScmObj* %anf_45bind40296, %struct.ScmObj** %stackaddr$prim50030, align 8
%stackaddr$prim50031 = alloca %struct.ScmObj*, align 8
%anf_45bind40297 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40295, %struct.ScmObj* %anf_45bind40296)
store volatile %struct.ScmObj* %anf_45bind40297, %struct.ScmObj** %stackaddr$prim50031, align 8
%stackaddr$prim50032 = alloca %struct.ScmObj*, align 8
%cpsargs40498 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40496, %struct.ScmObj* %anf_45bind40297)
store volatile %struct.ScmObj* %cpsargs40498, %struct.ScmObj** %stackaddr$prim50032, align 8
%clofunc50033 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40134)
musttail call tailcc void %clofunc50033(%struct.ScmObj* %_37foldr40134, %struct.ScmObj* %cpsargs40498)
ret void
}

define tailcc void @proc_clo$ae41743(%struct.ScmObj* %env$ae41743,%struct.ScmObj* %fargs4015940499) {
%stackaddr$env-ref50034 = alloca %struct.ScmObj*, align 8
%_37last40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41743, i64 0)
store %struct.ScmObj* %_37last40151, %struct.ScmObj** %stackaddr$env-ref50034
%stackaddr$env-ref50035 = alloca %struct.ScmObj*, align 8
%_37drop_45right40148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41743, i64 1)
store %struct.ScmObj* %_37drop_45right40148, %struct.ScmObj** %stackaddr$env-ref50035
%stackaddr$env-ref50036 = alloca %struct.ScmObj*, align 8
%f40158 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41743, i64 2)
store %struct.ScmObj* %f40158, %struct.ScmObj** %stackaddr$env-ref50036
%stackaddr$prim50037 = alloca %struct.ScmObj*, align 8
%k40500 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4015940499)
store volatile %struct.ScmObj* %k40500, %struct.ScmObj** %stackaddr$prim50037, align 8
%stackaddr$prim50038 = alloca %struct.ScmObj*, align 8
%fargs40159 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4015940499)
store volatile %struct.ScmObj* %fargs40159, %struct.ScmObj** %stackaddr$prim50038, align 8
%stackaddr$makeclosure50039 = alloca %struct.ScmObj*, align 8
%fptrToInt50040 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41747 to i64
%ae41747 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50040)
store volatile %struct.ScmObj* %ae41747, %struct.ScmObj** %stackaddr$makeclosure50039, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41747, %struct.ScmObj* %_37last40151, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41747, %struct.ScmObj* %k40500, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41747, %struct.ScmObj* %fargs40159, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41747, %struct.ScmObj* %f40158, i64 3)
%ae41749 = call %struct.ScmObj* @const_init_int(i64 1)
%args48502$_37drop_45right40148$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50041 = alloca %struct.ScmObj*, align 8
%args48502$_37drop_45right40148$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41749, %struct.ScmObj* %args48502$_37drop_45right40148$0)
store volatile %struct.ScmObj* %args48502$_37drop_45right40148$1, %struct.ScmObj** %stackaddr$prim50041, align 8
%stackaddr$prim50042 = alloca %struct.ScmObj*, align 8
%args48502$_37drop_45right40148$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40159, %struct.ScmObj* %args48502$_37drop_45right40148$1)
store volatile %struct.ScmObj* %args48502$_37drop_45right40148$2, %struct.ScmObj** %stackaddr$prim50042, align 8
%stackaddr$prim50043 = alloca %struct.ScmObj*, align 8
%args48502$_37drop_45right40148$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41747, %struct.ScmObj* %args48502$_37drop_45right40148$2)
store volatile %struct.ScmObj* %args48502$_37drop_45right40148$3, %struct.ScmObj** %stackaddr$prim50043, align 8
%clofunc50044 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right40148)
musttail call tailcc void %clofunc50044(%struct.ScmObj* %_37drop_45right40148, %struct.ScmObj* %args48502$_37drop_45right40148$3)
ret void
}

define tailcc void @proc_clo$ae41747(%struct.ScmObj* %env$ae41747,%struct.ScmObj* %current_45args48491) {
%stackaddr$env-ref50045 = alloca %struct.ScmObj*, align 8
%_37last40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41747, i64 0)
store %struct.ScmObj* %_37last40151, %struct.ScmObj** %stackaddr$env-ref50045
%stackaddr$env-ref50046 = alloca %struct.ScmObj*, align 8
%k40500 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41747, i64 1)
store %struct.ScmObj* %k40500, %struct.ScmObj** %stackaddr$env-ref50046
%stackaddr$env-ref50047 = alloca %struct.ScmObj*, align 8
%fargs40159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41747, i64 2)
store %struct.ScmObj* %fargs40159, %struct.ScmObj** %stackaddr$env-ref50047
%stackaddr$env-ref50048 = alloca %struct.ScmObj*, align 8
%f40158 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41747, i64 3)
store %struct.ScmObj* %f40158, %struct.ScmObj** %stackaddr$env-ref50048
%stackaddr$prim50049 = alloca %struct.ScmObj*, align 8
%_95k40501 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48491)
store volatile %struct.ScmObj* %_95k40501, %struct.ScmObj** %stackaddr$prim50049, align 8
%stackaddr$prim50050 = alloca %struct.ScmObj*, align 8
%current_45args48492 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48491)
store volatile %struct.ScmObj* %current_45args48492, %struct.ScmObj** %stackaddr$prim50050, align 8
%stackaddr$prim50051 = alloca %struct.ScmObj*, align 8
%anf_45bind40292 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48492)
store volatile %struct.ScmObj* %anf_45bind40292, %struct.ScmObj** %stackaddr$prim50051, align 8
%stackaddr$makeclosure50052 = alloca %struct.ScmObj*, align 8
%fptrToInt50053 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41754 to i64
%ae41754 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50053)
store volatile %struct.ScmObj* %ae41754, %struct.ScmObj** %stackaddr$makeclosure50052, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41754, %struct.ScmObj* %_37last40151, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41754, %struct.ScmObj* %k40500, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41754, %struct.ScmObj* %fargs40159, i64 2)
%stackaddr$prim50054 = alloca %struct.ScmObj*, align 8
%cpsargs40505 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41754, %struct.ScmObj* %anf_45bind40292)
store volatile %struct.ScmObj* %cpsargs40505, %struct.ScmObj** %stackaddr$prim50054, align 8
%clofunc50055 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40158)
musttail call tailcc void %clofunc50055(%struct.ScmObj* %f40158, %struct.ScmObj* %cpsargs40505)
ret void
}

define tailcc void @proc_clo$ae41754(%struct.ScmObj* %env$ae41754,%struct.ScmObj* %current_45args48494) {
%stackaddr$env-ref50056 = alloca %struct.ScmObj*, align 8
%_37last40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41754, i64 0)
store %struct.ScmObj* %_37last40151, %struct.ScmObj** %stackaddr$env-ref50056
%stackaddr$env-ref50057 = alloca %struct.ScmObj*, align 8
%k40500 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41754, i64 1)
store %struct.ScmObj* %k40500, %struct.ScmObj** %stackaddr$env-ref50057
%stackaddr$env-ref50058 = alloca %struct.ScmObj*, align 8
%fargs40159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41754, i64 2)
store %struct.ScmObj* %fargs40159, %struct.ScmObj** %stackaddr$env-ref50058
%stackaddr$prim50059 = alloca %struct.ScmObj*, align 8
%_95k40502 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48494)
store volatile %struct.ScmObj* %_95k40502, %struct.ScmObj** %stackaddr$prim50059, align 8
%stackaddr$prim50060 = alloca %struct.ScmObj*, align 8
%current_45args48495 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48494)
store volatile %struct.ScmObj* %current_45args48495, %struct.ScmObj** %stackaddr$prim50060, align 8
%stackaddr$prim50061 = alloca %struct.ScmObj*, align 8
%anf_45bind40293 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48495)
store volatile %struct.ScmObj* %anf_45bind40293, %struct.ScmObj** %stackaddr$prim50061, align 8
%stackaddr$makeclosure50062 = alloca %struct.ScmObj*, align 8
%fptrToInt50063 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41759 to i64
%ae41759 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50063)
store volatile %struct.ScmObj* %ae41759, %struct.ScmObj** %stackaddr$makeclosure50062, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41759, %struct.ScmObj* %anf_45bind40293, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41759, %struct.ScmObj* %k40500, i64 1)
%args48501$_37last40151$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50064 = alloca %struct.ScmObj*, align 8
%args48501$_37last40151$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40159, %struct.ScmObj* %args48501$_37last40151$0)
store volatile %struct.ScmObj* %args48501$_37last40151$1, %struct.ScmObj** %stackaddr$prim50064, align 8
%stackaddr$prim50065 = alloca %struct.ScmObj*, align 8
%args48501$_37last40151$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41759, %struct.ScmObj* %args48501$_37last40151$1)
store volatile %struct.ScmObj* %args48501$_37last40151$2, %struct.ScmObj** %stackaddr$prim50065, align 8
%clofunc50066 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last40151)
musttail call tailcc void %clofunc50066(%struct.ScmObj* %_37last40151, %struct.ScmObj* %args48501$_37last40151$2)
ret void
}

define tailcc void @proc_clo$ae41759(%struct.ScmObj* %env$ae41759,%struct.ScmObj* %current_45args48497) {
%stackaddr$env-ref50067 = alloca %struct.ScmObj*, align 8
%anf_45bind40293 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41759, i64 0)
store %struct.ScmObj* %anf_45bind40293, %struct.ScmObj** %stackaddr$env-ref50067
%stackaddr$env-ref50068 = alloca %struct.ScmObj*, align 8
%k40500 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41759, i64 1)
store %struct.ScmObj* %k40500, %struct.ScmObj** %stackaddr$env-ref50068
%stackaddr$prim50069 = alloca %struct.ScmObj*, align 8
%_95k40503 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48497)
store volatile %struct.ScmObj* %_95k40503, %struct.ScmObj** %stackaddr$prim50069, align 8
%stackaddr$prim50070 = alloca %struct.ScmObj*, align 8
%current_45args48498 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48497)
store volatile %struct.ScmObj* %current_45args48498, %struct.ScmObj** %stackaddr$prim50070, align 8
%stackaddr$prim50071 = alloca %struct.ScmObj*, align 8
%anf_45bind40294 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48498)
store volatile %struct.ScmObj* %anf_45bind40294, %struct.ScmObj** %stackaddr$prim50071, align 8
%stackaddr$prim50072 = alloca %struct.ScmObj*, align 8
%cpsprim40504 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40293, %struct.ScmObj* %anf_45bind40294)
store volatile %struct.ScmObj* %cpsprim40504, %struct.ScmObj** %stackaddr$prim50072, align 8
%ae41764 = call %struct.ScmObj* @const_init_int(i64 0)
%args48500$k40500$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50073 = alloca %struct.ScmObj*, align 8
%args48500$k40500$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40504, %struct.ScmObj* %args48500$k40500$0)
store volatile %struct.ScmObj* %args48500$k40500$1, %struct.ScmObj** %stackaddr$prim50073, align 8
%stackaddr$prim50074 = alloca %struct.ScmObj*, align 8
%args48500$k40500$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41764, %struct.ScmObj* %args48500$k40500$1)
store volatile %struct.ScmObj* %args48500$k40500$2, %struct.ScmObj** %stackaddr$prim50074, align 8
%clofunc50075 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40500)
musttail call tailcc void %clofunc50075(%struct.ScmObj* %k40500, %struct.ScmObj* %args48500$k40500$2)
ret void
}

define tailcc void @proc_clo$ae41659(%struct.ScmObj* %env$ae41659,%struct.ScmObj* %current_45args48505) {
%stackaddr$env-ref50076 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41659, i64 0)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref50076
%stackaddr$prim50077 = alloca %struct.ScmObj*, align 8
%k40506 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48505)
store volatile %struct.ScmObj* %k40506, %struct.ScmObj** %stackaddr$prim50077, align 8
%stackaddr$prim50078 = alloca %struct.ScmObj*, align 8
%current_45args48506 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48505)
store volatile %struct.ScmObj* %current_45args48506, %struct.ScmObj** %stackaddr$prim50078, align 8
%stackaddr$prim50079 = alloca %struct.ScmObj*, align 8
%f40162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48506)
store volatile %struct.ScmObj* %f40162, %struct.ScmObj** %stackaddr$prim50079, align 8
%stackaddr$prim50080 = alloca %struct.ScmObj*, align 8
%current_45args48507 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48506)
store volatile %struct.ScmObj* %current_45args48507, %struct.ScmObj** %stackaddr$prim50080, align 8
%stackaddr$prim50081 = alloca %struct.ScmObj*, align 8
%lst40161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48507)
store volatile %struct.ScmObj* %lst40161, %struct.ScmObj** %stackaddr$prim50081, align 8
%stackaddr$makeclosure50082 = alloca %struct.ScmObj*, align 8
%fptrToInt50083 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41660 to i64
%ae41660 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50083)
store volatile %struct.ScmObj* %ae41660, %struct.ScmObj** %stackaddr$makeclosure50082, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41660, %struct.ScmObj* %lst40161, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41660, %struct.ScmObj* %_37foldr140129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41660, %struct.ScmObj* %k40506, i64 2)
%ae41661 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50084 = alloca %struct.ScmObj*, align 8
%fptrToInt50085 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41662 to i64
%ae41662 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50085)
store volatile %struct.ScmObj* %ae41662, %struct.ScmObj** %stackaddr$makeclosure50084, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41662, %struct.ScmObj* %f40162, i64 0)
%args48522$ae41660$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50086 = alloca %struct.ScmObj*, align 8
%args48522$ae41660$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41662, %struct.ScmObj* %args48522$ae41660$0)
store volatile %struct.ScmObj* %args48522$ae41660$1, %struct.ScmObj** %stackaddr$prim50086, align 8
%stackaddr$prim50087 = alloca %struct.ScmObj*, align 8
%args48522$ae41660$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41661, %struct.ScmObj* %args48522$ae41660$1)
store volatile %struct.ScmObj* %args48522$ae41660$2, %struct.ScmObj** %stackaddr$prim50087, align 8
%clofunc50088 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41660)
musttail call tailcc void %clofunc50088(%struct.ScmObj* %ae41660, %struct.ScmObj* %args48522$ae41660$2)
ret void
}

define tailcc void @proc_clo$ae41660(%struct.ScmObj* %env$ae41660,%struct.ScmObj* %current_45args48509) {
%stackaddr$env-ref50089 = alloca %struct.ScmObj*, align 8
%lst40161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41660, i64 0)
store %struct.ScmObj* %lst40161, %struct.ScmObj** %stackaddr$env-ref50089
%stackaddr$env-ref50090 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41660, i64 1)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref50090
%stackaddr$env-ref50091 = alloca %struct.ScmObj*, align 8
%k40506 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41660, i64 2)
store %struct.ScmObj* %k40506, %struct.ScmObj** %stackaddr$env-ref50091
%stackaddr$prim50092 = alloca %struct.ScmObj*, align 8
%_95k40507 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48509)
store volatile %struct.ScmObj* %_95k40507, %struct.ScmObj** %stackaddr$prim50092, align 8
%stackaddr$prim50093 = alloca %struct.ScmObj*, align 8
%current_45args48510 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48509)
store volatile %struct.ScmObj* %current_45args48510, %struct.ScmObj** %stackaddr$prim50093, align 8
%stackaddr$prim50094 = alloca %struct.ScmObj*, align 8
%anf_45bind40291 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48510)
store volatile %struct.ScmObj* %anf_45bind40291, %struct.ScmObj** %stackaddr$prim50094, align 8
%ae41694 = call %struct.ScmObj* @const_init_null()
%args48512$_37foldr140129$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50095 = alloca %struct.ScmObj*, align 8
%args48512$_37foldr140129$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40161, %struct.ScmObj* %args48512$_37foldr140129$0)
store volatile %struct.ScmObj* %args48512$_37foldr140129$1, %struct.ScmObj** %stackaddr$prim50095, align 8
%stackaddr$prim50096 = alloca %struct.ScmObj*, align 8
%args48512$_37foldr140129$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41694, %struct.ScmObj* %args48512$_37foldr140129$1)
store volatile %struct.ScmObj* %args48512$_37foldr140129$2, %struct.ScmObj** %stackaddr$prim50096, align 8
%stackaddr$prim50097 = alloca %struct.ScmObj*, align 8
%args48512$_37foldr140129$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40291, %struct.ScmObj* %args48512$_37foldr140129$2)
store volatile %struct.ScmObj* %args48512$_37foldr140129$3, %struct.ScmObj** %stackaddr$prim50097, align 8
%stackaddr$prim50098 = alloca %struct.ScmObj*, align 8
%args48512$_37foldr140129$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40506, %struct.ScmObj* %args48512$_37foldr140129$3)
store volatile %struct.ScmObj* %args48512$_37foldr140129$4, %struct.ScmObj** %stackaddr$prim50098, align 8
%clofunc50099 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140129)
musttail call tailcc void %clofunc50099(%struct.ScmObj* %_37foldr140129, %struct.ScmObj* %args48512$_37foldr140129$4)
ret void
}

define tailcc void @proc_clo$ae41662(%struct.ScmObj* %env$ae41662,%struct.ScmObj* %current_45args48513) {
%stackaddr$env-ref50100 = alloca %struct.ScmObj*, align 8
%f40162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41662, i64 0)
store %struct.ScmObj* %f40162, %struct.ScmObj** %stackaddr$env-ref50100
%stackaddr$prim50101 = alloca %struct.ScmObj*, align 8
%k40508 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48513)
store volatile %struct.ScmObj* %k40508, %struct.ScmObj** %stackaddr$prim50101, align 8
%stackaddr$prim50102 = alloca %struct.ScmObj*, align 8
%current_45args48514 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48513)
store volatile %struct.ScmObj* %current_45args48514, %struct.ScmObj** %stackaddr$prim50102, align 8
%stackaddr$prim50103 = alloca %struct.ScmObj*, align 8
%v40164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48514)
store volatile %struct.ScmObj* %v40164, %struct.ScmObj** %stackaddr$prim50103, align 8
%stackaddr$prim50104 = alloca %struct.ScmObj*, align 8
%current_45args48515 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48514)
store volatile %struct.ScmObj* %current_45args48515, %struct.ScmObj** %stackaddr$prim50104, align 8
%stackaddr$prim50105 = alloca %struct.ScmObj*, align 8
%r40163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48515)
store volatile %struct.ScmObj* %r40163, %struct.ScmObj** %stackaddr$prim50105, align 8
%stackaddr$makeclosure50106 = alloca %struct.ScmObj*, align 8
%fptrToInt50107 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41664 to i64
%ae41664 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50107)
store volatile %struct.ScmObj* %ae41664, %struct.ScmObj** %stackaddr$makeclosure50106, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41664, %struct.ScmObj* %r40163, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41664, %struct.ScmObj* %k40508, i64 1)
%args48521$f40162$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50108 = alloca %struct.ScmObj*, align 8
%args48521$f40162$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v40164, %struct.ScmObj* %args48521$f40162$0)
store volatile %struct.ScmObj* %args48521$f40162$1, %struct.ScmObj** %stackaddr$prim50108, align 8
%stackaddr$prim50109 = alloca %struct.ScmObj*, align 8
%args48521$f40162$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41664, %struct.ScmObj* %args48521$f40162$1)
store volatile %struct.ScmObj* %args48521$f40162$2, %struct.ScmObj** %stackaddr$prim50109, align 8
%clofunc50110 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40162)
musttail call tailcc void %clofunc50110(%struct.ScmObj* %f40162, %struct.ScmObj* %args48521$f40162$2)
ret void
}

define tailcc void @proc_clo$ae41664(%struct.ScmObj* %env$ae41664,%struct.ScmObj* %current_45args48517) {
%stackaddr$env-ref50111 = alloca %struct.ScmObj*, align 8
%r40163 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41664, i64 0)
store %struct.ScmObj* %r40163, %struct.ScmObj** %stackaddr$env-ref50111
%stackaddr$env-ref50112 = alloca %struct.ScmObj*, align 8
%k40508 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41664, i64 1)
store %struct.ScmObj* %k40508, %struct.ScmObj** %stackaddr$env-ref50112
%stackaddr$prim50113 = alloca %struct.ScmObj*, align 8
%_95k40509 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48517)
store volatile %struct.ScmObj* %_95k40509, %struct.ScmObj** %stackaddr$prim50113, align 8
%stackaddr$prim50114 = alloca %struct.ScmObj*, align 8
%current_45args48518 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48517)
store volatile %struct.ScmObj* %current_45args48518, %struct.ScmObj** %stackaddr$prim50114, align 8
%stackaddr$prim50115 = alloca %struct.ScmObj*, align 8
%anf_45bind40290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48518)
store volatile %struct.ScmObj* %anf_45bind40290, %struct.ScmObj** %stackaddr$prim50115, align 8
%stackaddr$prim50116 = alloca %struct.ScmObj*, align 8
%cpsprim40510 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40290, %struct.ScmObj* %r40163)
store volatile %struct.ScmObj* %cpsprim40510, %struct.ScmObj** %stackaddr$prim50116, align 8
%ae41669 = call %struct.ScmObj* @const_init_int(i64 0)
%args48520$k40508$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50117 = alloca %struct.ScmObj*, align 8
%args48520$k40508$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40510, %struct.ScmObj* %args48520$k40508$0)
store volatile %struct.ScmObj* %args48520$k40508$1, %struct.ScmObj** %stackaddr$prim50117, align 8
%stackaddr$prim50118 = alloca %struct.ScmObj*, align 8
%args48520$k40508$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41669, %struct.ScmObj* %args48520$k40508$1)
store volatile %struct.ScmObj* %args48520$k40508$2, %struct.ScmObj** %stackaddr$prim50118, align 8
%clofunc50119 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40508)
musttail call tailcc void %clofunc50119(%struct.ScmObj* %k40508, %struct.ScmObj* %args48520$k40508$2)
ret void
}

define tailcc void @proc_clo$ae41273(%struct.ScmObj* %env$ae41273,%struct.ScmObj* %current_45args48525) {
%stackaddr$env-ref50120 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41273, i64 0)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref50120
%stackaddr$env-ref50121 = alloca %struct.ScmObj*, align 8
%_37map140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41273, i64 1)
store %struct.ScmObj* %_37map140125, %struct.ScmObj** %stackaddr$env-ref50121
%stackaddr$prim50122 = alloca %struct.ScmObj*, align 8
%k40511 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48525)
store volatile %struct.ScmObj* %k40511, %struct.ScmObj** %stackaddr$prim50122, align 8
%stackaddr$prim50123 = alloca %struct.ScmObj*, align 8
%current_45args48526 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48525)
store volatile %struct.ScmObj* %current_45args48526, %struct.ScmObj** %stackaddr$prim50123, align 8
%stackaddr$prim50124 = alloca %struct.ScmObj*, align 8
%_37foldr40135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48526)
store volatile %struct.ScmObj* %_37foldr40135, %struct.ScmObj** %stackaddr$prim50124, align 8
%ae41275 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50125 = alloca %struct.ScmObj*, align 8
%fptrToInt50126 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41276 to i64
%ae41276 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50126)
store volatile %struct.ScmObj* %ae41276, %struct.ScmObj** %stackaddr$makeclosure50125, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41276, %struct.ScmObj* %_37foldr40135, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41276, %struct.ScmObj* %_37foldr140129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41276, %struct.ScmObj* %_37map140125, i64 2)
%args48583$k40511$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50127 = alloca %struct.ScmObj*, align 8
%args48583$k40511$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41276, %struct.ScmObj* %args48583$k40511$0)
store volatile %struct.ScmObj* %args48583$k40511$1, %struct.ScmObj** %stackaddr$prim50127, align 8
%stackaddr$prim50128 = alloca %struct.ScmObj*, align 8
%args48583$k40511$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41275, %struct.ScmObj* %args48583$k40511$1)
store volatile %struct.ScmObj* %args48583$k40511$2, %struct.ScmObj** %stackaddr$prim50128, align 8
%clofunc50129 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40511)
musttail call tailcc void %clofunc50129(%struct.ScmObj* %k40511, %struct.ScmObj* %args48583$k40511$2)
ret void
}

define tailcc void @proc_clo$ae41276(%struct.ScmObj* %env$ae41276,%struct.ScmObj* %args4013640512) {
%stackaddr$env-ref50130 = alloca %struct.ScmObj*, align 8
%_37foldr40135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41276, i64 0)
store %struct.ScmObj* %_37foldr40135, %struct.ScmObj** %stackaddr$env-ref50130
%stackaddr$env-ref50131 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41276, i64 1)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref50131
%stackaddr$env-ref50132 = alloca %struct.ScmObj*, align 8
%_37map140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41276, i64 2)
store %struct.ScmObj* %_37map140125, %struct.ScmObj** %stackaddr$env-ref50132
%stackaddr$prim50133 = alloca %struct.ScmObj*, align 8
%k40513 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4013640512)
store volatile %struct.ScmObj* %k40513, %struct.ScmObj** %stackaddr$prim50133, align 8
%stackaddr$prim50134 = alloca %struct.ScmObj*, align 8
%args40136 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4013640512)
store volatile %struct.ScmObj* %args40136, %struct.ScmObj** %stackaddr$prim50134, align 8
%stackaddr$prim50135 = alloca %struct.ScmObj*, align 8
%f40139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40136)
store volatile %struct.ScmObj* %f40139, %struct.ScmObj** %stackaddr$prim50135, align 8
%stackaddr$prim50136 = alloca %struct.ScmObj*, align 8
%anf_45bind40277 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40136)
store volatile %struct.ScmObj* %anf_45bind40277, %struct.ScmObj** %stackaddr$prim50136, align 8
%stackaddr$prim50137 = alloca %struct.ScmObj*, align 8
%acc40138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40277)
store volatile %struct.ScmObj* %acc40138, %struct.ScmObj** %stackaddr$prim50137, align 8
%stackaddr$prim50138 = alloca %struct.ScmObj*, align 8
%anf_45bind40278 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40136)
store volatile %struct.ScmObj* %anf_45bind40278, %struct.ScmObj** %stackaddr$prim50138, align 8
%stackaddr$prim50139 = alloca %struct.ScmObj*, align 8
%lsts40137 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40278)
store volatile %struct.ScmObj* %lsts40137, %struct.ScmObj** %stackaddr$prim50139, align 8
%stackaddr$makeclosure50140 = alloca %struct.ScmObj*, align 8
%fptrToInt50141 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41284 to i64
%ae41284 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50141)
store volatile %struct.ScmObj* %ae41284, %struct.ScmObj** %stackaddr$makeclosure50140, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41284, %struct.ScmObj* %k40513, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41284, %struct.ScmObj* %_37foldr140129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41284, %struct.ScmObj* %lsts40137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41284, %struct.ScmObj* %_37foldr40135, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41284, %struct.ScmObj* %_37map140125, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41284, %struct.ScmObj* %f40139, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41284, %struct.ScmObj* %acc40138, i64 6)
%ae41285 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50142 = alloca %struct.ScmObj*, align 8
%fptrToInt50143 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41286 to i64
%ae41286 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50143)
store volatile %struct.ScmObj* %ae41286, %struct.ScmObj** %stackaddr$makeclosure50142, align 8
%args48582$ae41284$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50144 = alloca %struct.ScmObj*, align 8
%args48582$ae41284$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41286, %struct.ScmObj* %args48582$ae41284$0)
store volatile %struct.ScmObj* %args48582$ae41284$1, %struct.ScmObj** %stackaddr$prim50144, align 8
%stackaddr$prim50145 = alloca %struct.ScmObj*, align 8
%args48582$ae41284$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41285, %struct.ScmObj* %args48582$ae41284$1)
store volatile %struct.ScmObj* %args48582$ae41284$2, %struct.ScmObj** %stackaddr$prim50145, align 8
%clofunc50146 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41284)
musttail call tailcc void %clofunc50146(%struct.ScmObj* %ae41284, %struct.ScmObj* %args48582$ae41284$2)
ret void
}

define tailcc void @proc_clo$ae41284(%struct.ScmObj* %env$ae41284,%struct.ScmObj* %current_45args48528) {
%stackaddr$env-ref50147 = alloca %struct.ScmObj*, align 8
%k40513 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41284, i64 0)
store %struct.ScmObj* %k40513, %struct.ScmObj** %stackaddr$env-ref50147
%stackaddr$env-ref50148 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41284, i64 1)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref50148
%stackaddr$env-ref50149 = alloca %struct.ScmObj*, align 8
%lsts40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41284, i64 2)
store %struct.ScmObj* %lsts40137, %struct.ScmObj** %stackaddr$env-ref50149
%stackaddr$env-ref50150 = alloca %struct.ScmObj*, align 8
%_37foldr40135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41284, i64 3)
store %struct.ScmObj* %_37foldr40135, %struct.ScmObj** %stackaddr$env-ref50150
%stackaddr$env-ref50151 = alloca %struct.ScmObj*, align 8
%_37map140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41284, i64 4)
store %struct.ScmObj* %_37map140125, %struct.ScmObj** %stackaddr$env-ref50151
%stackaddr$env-ref50152 = alloca %struct.ScmObj*, align 8
%f40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41284, i64 5)
store %struct.ScmObj* %f40139, %struct.ScmObj** %stackaddr$env-ref50152
%stackaddr$env-ref50153 = alloca %struct.ScmObj*, align 8
%acc40138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41284, i64 6)
store %struct.ScmObj* %acc40138, %struct.ScmObj** %stackaddr$env-ref50153
%stackaddr$prim50154 = alloca %struct.ScmObj*, align 8
%_95k40514 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48528)
store volatile %struct.ScmObj* %_95k40514, %struct.ScmObj** %stackaddr$prim50154, align 8
%stackaddr$prim50155 = alloca %struct.ScmObj*, align 8
%current_45args48529 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48528)
store volatile %struct.ScmObj* %current_45args48529, %struct.ScmObj** %stackaddr$prim50155, align 8
%stackaddr$prim50156 = alloca %struct.ScmObj*, align 8
%anf_45bind40279 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48529)
store volatile %struct.ScmObj* %anf_45bind40279, %struct.ScmObj** %stackaddr$prim50156, align 8
%stackaddr$makeclosure50157 = alloca %struct.ScmObj*, align 8
%fptrToInt50158 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41316 to i64
%ae41316 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50158)
store volatile %struct.ScmObj* %ae41316, %struct.ScmObj** %stackaddr$makeclosure50157, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41316, %struct.ScmObj* %k40513, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41316, %struct.ScmObj* %_37foldr140129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41316, %struct.ScmObj* %lsts40137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41316, %struct.ScmObj* %_37foldr40135, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41316, %struct.ScmObj* %_37map140125, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41316, %struct.ScmObj* %f40139, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41316, %struct.ScmObj* %acc40138, i64 6)
%ae41318 = call %struct.ScmObj* @const_init_false()
%args48575$_37foldr140129$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50159 = alloca %struct.ScmObj*, align 8
%args48575$_37foldr140129$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40137, %struct.ScmObj* %args48575$_37foldr140129$0)
store volatile %struct.ScmObj* %args48575$_37foldr140129$1, %struct.ScmObj** %stackaddr$prim50159, align 8
%stackaddr$prim50160 = alloca %struct.ScmObj*, align 8
%args48575$_37foldr140129$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41318, %struct.ScmObj* %args48575$_37foldr140129$1)
store volatile %struct.ScmObj* %args48575$_37foldr140129$2, %struct.ScmObj** %stackaddr$prim50160, align 8
%stackaddr$prim50161 = alloca %struct.ScmObj*, align 8
%args48575$_37foldr140129$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40279, %struct.ScmObj* %args48575$_37foldr140129$2)
store volatile %struct.ScmObj* %args48575$_37foldr140129$3, %struct.ScmObj** %stackaddr$prim50161, align 8
%stackaddr$prim50162 = alloca %struct.ScmObj*, align 8
%args48575$_37foldr140129$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41316, %struct.ScmObj* %args48575$_37foldr140129$3)
store volatile %struct.ScmObj* %args48575$_37foldr140129$4, %struct.ScmObj** %stackaddr$prim50162, align 8
%clofunc50163 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140129)
musttail call tailcc void %clofunc50163(%struct.ScmObj* %_37foldr140129, %struct.ScmObj* %args48575$_37foldr140129$4)
ret void
}

define tailcc void @proc_clo$ae41316(%struct.ScmObj* %env$ae41316,%struct.ScmObj* %current_45args48531) {
%stackaddr$env-ref50164 = alloca %struct.ScmObj*, align 8
%k40513 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41316, i64 0)
store %struct.ScmObj* %k40513, %struct.ScmObj** %stackaddr$env-ref50164
%stackaddr$env-ref50165 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41316, i64 1)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref50165
%stackaddr$env-ref50166 = alloca %struct.ScmObj*, align 8
%lsts40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41316, i64 2)
store %struct.ScmObj* %lsts40137, %struct.ScmObj** %stackaddr$env-ref50166
%stackaddr$env-ref50167 = alloca %struct.ScmObj*, align 8
%_37foldr40135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41316, i64 3)
store %struct.ScmObj* %_37foldr40135, %struct.ScmObj** %stackaddr$env-ref50167
%stackaddr$env-ref50168 = alloca %struct.ScmObj*, align 8
%_37map140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41316, i64 4)
store %struct.ScmObj* %_37map140125, %struct.ScmObj** %stackaddr$env-ref50168
%stackaddr$env-ref50169 = alloca %struct.ScmObj*, align 8
%f40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41316, i64 5)
store %struct.ScmObj* %f40139, %struct.ScmObj** %stackaddr$env-ref50169
%stackaddr$env-ref50170 = alloca %struct.ScmObj*, align 8
%acc40138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41316, i64 6)
store %struct.ScmObj* %acc40138, %struct.ScmObj** %stackaddr$env-ref50170
%stackaddr$prim50171 = alloca %struct.ScmObj*, align 8
%_95k40515 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48531)
store volatile %struct.ScmObj* %_95k40515, %struct.ScmObj** %stackaddr$prim50171, align 8
%stackaddr$prim50172 = alloca %struct.ScmObj*, align 8
%current_45args48532 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48531)
store volatile %struct.ScmObj* %current_45args48532, %struct.ScmObj** %stackaddr$prim50172, align 8
%stackaddr$prim50173 = alloca %struct.ScmObj*, align 8
%anf_45bind40280 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48532)
store volatile %struct.ScmObj* %anf_45bind40280, %struct.ScmObj** %stackaddr$prim50173, align 8
%truthy$cmp50174 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40280)
%cmp$cmp50174 = icmp eq i64 %truthy$cmp50174, 1
br i1 %cmp$cmp50174, label %truebranch$cmp50174, label %falsebranch$cmp50174
truebranch$cmp50174:
%ae41327 = call %struct.ScmObj* @const_init_int(i64 0)
%args48534$k40513$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50175 = alloca %struct.ScmObj*, align 8
%args48534$k40513$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40138, %struct.ScmObj* %args48534$k40513$0)
store volatile %struct.ScmObj* %args48534$k40513$1, %struct.ScmObj** %stackaddr$prim50175, align 8
%stackaddr$prim50176 = alloca %struct.ScmObj*, align 8
%args48534$k40513$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41327, %struct.ScmObj* %args48534$k40513$1)
store volatile %struct.ScmObj* %args48534$k40513$2, %struct.ScmObj** %stackaddr$prim50176, align 8
%clofunc50177 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40513)
musttail call tailcc void %clofunc50177(%struct.ScmObj* %k40513, %struct.ScmObj* %args48534$k40513$2)
ret void
falsebranch$cmp50174:
%stackaddr$makeclosure50178 = alloca %struct.ScmObj*, align 8
%fptrToInt50179 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41332 to i64
%ae41332 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50179)
store volatile %struct.ScmObj* %ae41332, %struct.ScmObj** %stackaddr$makeclosure50178, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41332, %struct.ScmObj* %k40513, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41332, %struct.ScmObj* %_37foldr140129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41332, %struct.ScmObj* %lsts40137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41332, %struct.ScmObj* %_37foldr40135, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41332, %struct.ScmObj* %_37map140125, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41332, %struct.ScmObj* %f40139, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41332, %struct.ScmObj* %acc40138, i64 6)
%ae41333 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50180 = alloca %struct.ScmObj*, align 8
%fptrToInt50181 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41334 to i64
%ae41334 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50181)
store volatile %struct.ScmObj* %ae41334, %struct.ScmObj** %stackaddr$makeclosure50180, align 8
%args48574$ae41332$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50182 = alloca %struct.ScmObj*, align 8
%args48574$ae41332$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41334, %struct.ScmObj* %args48574$ae41332$0)
store volatile %struct.ScmObj* %args48574$ae41332$1, %struct.ScmObj** %stackaddr$prim50182, align 8
%stackaddr$prim50183 = alloca %struct.ScmObj*, align 8
%args48574$ae41332$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41333, %struct.ScmObj* %args48574$ae41332$1)
store volatile %struct.ScmObj* %args48574$ae41332$2, %struct.ScmObj** %stackaddr$prim50183, align 8
%clofunc50184 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41332)
musttail call tailcc void %clofunc50184(%struct.ScmObj* %ae41332, %struct.ScmObj* %args48574$ae41332$2)
ret void
}

define tailcc void @proc_clo$ae41332(%struct.ScmObj* %env$ae41332,%struct.ScmObj* %current_45args48535) {
%stackaddr$env-ref50185 = alloca %struct.ScmObj*, align 8
%k40513 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41332, i64 0)
store %struct.ScmObj* %k40513, %struct.ScmObj** %stackaddr$env-ref50185
%stackaddr$env-ref50186 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41332, i64 1)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref50186
%stackaddr$env-ref50187 = alloca %struct.ScmObj*, align 8
%lsts40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41332, i64 2)
store %struct.ScmObj* %lsts40137, %struct.ScmObj** %stackaddr$env-ref50187
%stackaddr$env-ref50188 = alloca %struct.ScmObj*, align 8
%_37foldr40135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41332, i64 3)
store %struct.ScmObj* %_37foldr40135, %struct.ScmObj** %stackaddr$env-ref50188
%stackaddr$env-ref50189 = alloca %struct.ScmObj*, align 8
%_37map140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41332, i64 4)
store %struct.ScmObj* %_37map140125, %struct.ScmObj** %stackaddr$env-ref50189
%stackaddr$env-ref50190 = alloca %struct.ScmObj*, align 8
%f40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41332, i64 5)
store %struct.ScmObj* %f40139, %struct.ScmObj** %stackaddr$env-ref50190
%stackaddr$env-ref50191 = alloca %struct.ScmObj*, align 8
%acc40138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41332, i64 6)
store %struct.ScmObj* %acc40138, %struct.ScmObj** %stackaddr$env-ref50191
%stackaddr$prim50192 = alloca %struct.ScmObj*, align 8
%_95k40516 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48535)
store volatile %struct.ScmObj* %_95k40516, %struct.ScmObj** %stackaddr$prim50192, align 8
%stackaddr$prim50193 = alloca %struct.ScmObj*, align 8
%current_45args48536 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48535)
store volatile %struct.ScmObj* %current_45args48536, %struct.ScmObj** %stackaddr$prim50193, align 8
%stackaddr$prim50194 = alloca %struct.ScmObj*, align 8
%anf_45bind40281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48536)
store volatile %struct.ScmObj* %anf_45bind40281, %struct.ScmObj** %stackaddr$prim50194, align 8
%stackaddr$makeclosure50195 = alloca %struct.ScmObj*, align 8
%fptrToInt50196 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41353 to i64
%ae41353 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50196)
store volatile %struct.ScmObj* %ae41353, %struct.ScmObj** %stackaddr$makeclosure50195, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41353, %struct.ScmObj* %k40513, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41353, %struct.ScmObj* %_37foldr140129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41353, %struct.ScmObj* %lsts40137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41353, %struct.ScmObj* %_37foldr40135, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41353, %struct.ScmObj* %_37map140125, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41353, %struct.ScmObj* %f40139, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41353, %struct.ScmObj* %acc40138, i64 6)
%args48569$_37map140125$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50197 = alloca %struct.ScmObj*, align 8
%args48569$_37map140125$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40137, %struct.ScmObj* %args48569$_37map140125$0)
store volatile %struct.ScmObj* %args48569$_37map140125$1, %struct.ScmObj** %stackaddr$prim50197, align 8
%stackaddr$prim50198 = alloca %struct.ScmObj*, align 8
%args48569$_37map140125$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40281, %struct.ScmObj* %args48569$_37map140125$1)
store volatile %struct.ScmObj* %args48569$_37map140125$2, %struct.ScmObj** %stackaddr$prim50198, align 8
%stackaddr$prim50199 = alloca %struct.ScmObj*, align 8
%args48569$_37map140125$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41353, %struct.ScmObj* %args48569$_37map140125$2)
store volatile %struct.ScmObj* %args48569$_37map140125$3, %struct.ScmObj** %stackaddr$prim50199, align 8
%clofunc50200 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140125)
musttail call tailcc void %clofunc50200(%struct.ScmObj* %_37map140125, %struct.ScmObj* %args48569$_37map140125$3)
ret void
}

define tailcc void @proc_clo$ae41353(%struct.ScmObj* %env$ae41353,%struct.ScmObj* %current_45args48538) {
%stackaddr$env-ref50201 = alloca %struct.ScmObj*, align 8
%k40513 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41353, i64 0)
store %struct.ScmObj* %k40513, %struct.ScmObj** %stackaddr$env-ref50201
%stackaddr$env-ref50202 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41353, i64 1)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref50202
%stackaddr$env-ref50203 = alloca %struct.ScmObj*, align 8
%lsts40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41353, i64 2)
store %struct.ScmObj* %lsts40137, %struct.ScmObj** %stackaddr$env-ref50203
%stackaddr$env-ref50204 = alloca %struct.ScmObj*, align 8
%_37foldr40135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41353, i64 3)
store %struct.ScmObj* %_37foldr40135, %struct.ScmObj** %stackaddr$env-ref50204
%stackaddr$env-ref50205 = alloca %struct.ScmObj*, align 8
%_37map140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41353, i64 4)
store %struct.ScmObj* %_37map140125, %struct.ScmObj** %stackaddr$env-ref50205
%stackaddr$env-ref50206 = alloca %struct.ScmObj*, align 8
%f40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41353, i64 5)
store %struct.ScmObj* %f40139, %struct.ScmObj** %stackaddr$env-ref50206
%stackaddr$env-ref50207 = alloca %struct.ScmObj*, align 8
%acc40138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41353, i64 6)
store %struct.ScmObj* %acc40138, %struct.ScmObj** %stackaddr$env-ref50207
%stackaddr$prim50208 = alloca %struct.ScmObj*, align 8
%_95k40517 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48538)
store volatile %struct.ScmObj* %_95k40517, %struct.ScmObj** %stackaddr$prim50208, align 8
%stackaddr$prim50209 = alloca %struct.ScmObj*, align 8
%current_45args48539 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48538)
store volatile %struct.ScmObj* %current_45args48539, %struct.ScmObj** %stackaddr$prim50209, align 8
%stackaddr$prim50210 = alloca %struct.ScmObj*, align 8
%lsts_4340144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48539)
store volatile %struct.ScmObj* %lsts_4340144, %struct.ScmObj** %stackaddr$prim50210, align 8
%stackaddr$makeclosure50211 = alloca %struct.ScmObj*, align 8
%fptrToInt50212 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41356 to i64
%ae41356 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt50212)
store volatile %struct.ScmObj* %ae41356, %struct.ScmObj** %stackaddr$makeclosure50211, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41356, %struct.ScmObj* %k40513, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41356, %struct.ScmObj* %_37foldr140129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41356, %struct.ScmObj* %lsts40137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41356, %struct.ScmObj* %_37foldr40135, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41356, %struct.ScmObj* %lsts_4340144, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41356, %struct.ScmObj* %_37map140125, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41356, %struct.ScmObj* %f40139, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41356, %struct.ScmObj* %acc40138, i64 7)
%ae41357 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50213 = alloca %struct.ScmObj*, align 8
%fptrToInt50214 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41358 to i64
%ae41358 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50214)
store volatile %struct.ScmObj* %ae41358, %struct.ScmObj** %stackaddr$makeclosure50213, align 8
%args48568$ae41356$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50215 = alloca %struct.ScmObj*, align 8
%args48568$ae41356$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41358, %struct.ScmObj* %args48568$ae41356$0)
store volatile %struct.ScmObj* %args48568$ae41356$1, %struct.ScmObj** %stackaddr$prim50215, align 8
%stackaddr$prim50216 = alloca %struct.ScmObj*, align 8
%args48568$ae41356$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41357, %struct.ScmObj* %args48568$ae41356$1)
store volatile %struct.ScmObj* %args48568$ae41356$2, %struct.ScmObj** %stackaddr$prim50216, align 8
%clofunc50217 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41356)
musttail call tailcc void %clofunc50217(%struct.ScmObj* %ae41356, %struct.ScmObj* %args48568$ae41356$2)
ret void
}

define tailcc void @proc_clo$ae41356(%struct.ScmObj* %env$ae41356,%struct.ScmObj* %current_45args48541) {
%stackaddr$env-ref50218 = alloca %struct.ScmObj*, align 8
%k40513 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41356, i64 0)
store %struct.ScmObj* %k40513, %struct.ScmObj** %stackaddr$env-ref50218
%stackaddr$env-ref50219 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41356, i64 1)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref50219
%stackaddr$env-ref50220 = alloca %struct.ScmObj*, align 8
%lsts40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41356, i64 2)
store %struct.ScmObj* %lsts40137, %struct.ScmObj** %stackaddr$env-ref50220
%stackaddr$env-ref50221 = alloca %struct.ScmObj*, align 8
%_37foldr40135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41356, i64 3)
store %struct.ScmObj* %_37foldr40135, %struct.ScmObj** %stackaddr$env-ref50221
%stackaddr$env-ref50222 = alloca %struct.ScmObj*, align 8
%lsts_4340144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41356, i64 4)
store %struct.ScmObj* %lsts_4340144, %struct.ScmObj** %stackaddr$env-ref50222
%stackaddr$env-ref50223 = alloca %struct.ScmObj*, align 8
%_37map140125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41356, i64 5)
store %struct.ScmObj* %_37map140125, %struct.ScmObj** %stackaddr$env-ref50223
%stackaddr$env-ref50224 = alloca %struct.ScmObj*, align 8
%f40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41356, i64 6)
store %struct.ScmObj* %f40139, %struct.ScmObj** %stackaddr$env-ref50224
%stackaddr$env-ref50225 = alloca %struct.ScmObj*, align 8
%acc40138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41356, i64 7)
store %struct.ScmObj* %acc40138, %struct.ScmObj** %stackaddr$env-ref50225
%stackaddr$prim50226 = alloca %struct.ScmObj*, align 8
%_95k40518 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48541)
store volatile %struct.ScmObj* %_95k40518, %struct.ScmObj** %stackaddr$prim50226, align 8
%stackaddr$prim50227 = alloca %struct.ScmObj*, align 8
%current_45args48542 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48541)
store volatile %struct.ScmObj* %current_45args48542, %struct.ScmObj** %stackaddr$prim50227, align 8
%stackaddr$prim50228 = alloca %struct.ScmObj*, align 8
%anf_45bind40282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48542)
store volatile %struct.ScmObj* %anf_45bind40282, %struct.ScmObj** %stackaddr$prim50228, align 8
%stackaddr$makeclosure50229 = alloca %struct.ScmObj*, align 8
%fptrToInt50230 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41377 to i64
%ae41377 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt50230)
store volatile %struct.ScmObj* %ae41377, %struct.ScmObj** %stackaddr$makeclosure50229, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41377, %struct.ScmObj* %k40513, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41377, %struct.ScmObj* %_37foldr140129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41377, %struct.ScmObj* %_37foldr40135, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41377, %struct.ScmObj* %lsts_4340144, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41377, %struct.ScmObj* %f40139, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41377, %struct.ScmObj* %acc40138, i64 5)
%args48563$_37map140125$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50231 = alloca %struct.ScmObj*, align 8
%args48563$_37map140125$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40137, %struct.ScmObj* %args48563$_37map140125$0)
store volatile %struct.ScmObj* %args48563$_37map140125$1, %struct.ScmObj** %stackaddr$prim50231, align 8
%stackaddr$prim50232 = alloca %struct.ScmObj*, align 8
%args48563$_37map140125$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40282, %struct.ScmObj* %args48563$_37map140125$1)
store volatile %struct.ScmObj* %args48563$_37map140125$2, %struct.ScmObj** %stackaddr$prim50232, align 8
%stackaddr$prim50233 = alloca %struct.ScmObj*, align 8
%args48563$_37map140125$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41377, %struct.ScmObj* %args48563$_37map140125$2)
store volatile %struct.ScmObj* %args48563$_37map140125$3, %struct.ScmObj** %stackaddr$prim50233, align 8
%clofunc50234 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140125)
musttail call tailcc void %clofunc50234(%struct.ScmObj* %_37map140125, %struct.ScmObj* %args48563$_37map140125$3)
ret void
}

define tailcc void @proc_clo$ae41377(%struct.ScmObj* %env$ae41377,%struct.ScmObj* %current_45args48544) {
%stackaddr$env-ref50235 = alloca %struct.ScmObj*, align 8
%k40513 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41377, i64 0)
store %struct.ScmObj* %k40513, %struct.ScmObj** %stackaddr$env-ref50235
%stackaddr$env-ref50236 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41377, i64 1)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref50236
%stackaddr$env-ref50237 = alloca %struct.ScmObj*, align 8
%_37foldr40135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41377, i64 2)
store %struct.ScmObj* %_37foldr40135, %struct.ScmObj** %stackaddr$env-ref50237
%stackaddr$env-ref50238 = alloca %struct.ScmObj*, align 8
%lsts_4340144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41377, i64 3)
store %struct.ScmObj* %lsts_4340144, %struct.ScmObj** %stackaddr$env-ref50238
%stackaddr$env-ref50239 = alloca %struct.ScmObj*, align 8
%f40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41377, i64 4)
store %struct.ScmObj* %f40139, %struct.ScmObj** %stackaddr$env-ref50239
%stackaddr$env-ref50240 = alloca %struct.ScmObj*, align 8
%acc40138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41377, i64 5)
store %struct.ScmObj* %acc40138, %struct.ScmObj** %stackaddr$env-ref50240
%stackaddr$prim50241 = alloca %struct.ScmObj*, align 8
%_95k40519 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48544)
store volatile %struct.ScmObj* %_95k40519, %struct.ScmObj** %stackaddr$prim50241, align 8
%stackaddr$prim50242 = alloca %struct.ScmObj*, align 8
%current_45args48545 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48544)
store volatile %struct.ScmObj* %current_45args48545, %struct.ScmObj** %stackaddr$prim50242, align 8
%stackaddr$prim50243 = alloca %struct.ScmObj*, align 8
%vs40142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48545)
store volatile %struct.ScmObj* %vs40142, %struct.ScmObj** %stackaddr$prim50243, align 8
%stackaddr$makeclosure50244 = alloca %struct.ScmObj*, align 8
%fptrToInt50245 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41380 to i64
%ae41380 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50245)
store volatile %struct.ScmObj* %ae41380, %struct.ScmObj** %stackaddr$makeclosure50244, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41380, %struct.ScmObj* %k40513, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41380, %struct.ScmObj* %_37foldr140129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41380, %struct.ScmObj* %_37foldr40135, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41380, %struct.ScmObj* %lsts_4340144, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41380, %struct.ScmObj* %vs40142, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41380, %struct.ScmObj* %f40139, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41380, %struct.ScmObj* %acc40138, i64 6)
%ae41381 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50246 = alloca %struct.ScmObj*, align 8
%fptrToInt50247 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41382 to i64
%ae41382 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50247)
store volatile %struct.ScmObj* %ae41382, %struct.ScmObj** %stackaddr$makeclosure50246, align 8
%args48562$ae41380$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50248 = alloca %struct.ScmObj*, align 8
%args48562$ae41380$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41382, %struct.ScmObj* %args48562$ae41380$0)
store volatile %struct.ScmObj* %args48562$ae41380$1, %struct.ScmObj** %stackaddr$prim50248, align 8
%stackaddr$prim50249 = alloca %struct.ScmObj*, align 8
%args48562$ae41380$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41381, %struct.ScmObj* %args48562$ae41380$1)
store volatile %struct.ScmObj* %args48562$ae41380$2, %struct.ScmObj** %stackaddr$prim50249, align 8
%clofunc50250 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41380)
musttail call tailcc void %clofunc50250(%struct.ScmObj* %ae41380, %struct.ScmObj* %args48562$ae41380$2)
ret void
}

define tailcc void @proc_clo$ae41380(%struct.ScmObj* %env$ae41380,%struct.ScmObj* %current_45args48547) {
%stackaddr$env-ref50251 = alloca %struct.ScmObj*, align 8
%k40513 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41380, i64 0)
store %struct.ScmObj* %k40513, %struct.ScmObj** %stackaddr$env-ref50251
%stackaddr$env-ref50252 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41380, i64 1)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref50252
%stackaddr$env-ref50253 = alloca %struct.ScmObj*, align 8
%_37foldr40135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41380, i64 2)
store %struct.ScmObj* %_37foldr40135, %struct.ScmObj** %stackaddr$env-ref50253
%stackaddr$env-ref50254 = alloca %struct.ScmObj*, align 8
%lsts_4340144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41380, i64 3)
store %struct.ScmObj* %lsts_4340144, %struct.ScmObj** %stackaddr$env-ref50254
%stackaddr$env-ref50255 = alloca %struct.ScmObj*, align 8
%vs40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41380, i64 4)
store %struct.ScmObj* %vs40142, %struct.ScmObj** %stackaddr$env-ref50255
%stackaddr$env-ref50256 = alloca %struct.ScmObj*, align 8
%f40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41380, i64 5)
store %struct.ScmObj* %f40139, %struct.ScmObj** %stackaddr$env-ref50256
%stackaddr$env-ref50257 = alloca %struct.ScmObj*, align 8
%acc40138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41380, i64 6)
store %struct.ScmObj* %acc40138, %struct.ScmObj** %stackaddr$env-ref50257
%stackaddr$prim50258 = alloca %struct.ScmObj*, align 8
%_95k40520 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48547)
store volatile %struct.ScmObj* %_95k40520, %struct.ScmObj** %stackaddr$prim50258, align 8
%stackaddr$prim50259 = alloca %struct.ScmObj*, align 8
%current_45args48548 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48547)
store volatile %struct.ScmObj* %current_45args48548, %struct.ScmObj** %stackaddr$prim50259, align 8
%stackaddr$prim50260 = alloca %struct.ScmObj*, align 8
%anf_45bind40283 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48548)
store volatile %struct.ScmObj* %anf_45bind40283, %struct.ScmObj** %stackaddr$prim50260, align 8
%stackaddr$prim50261 = alloca %struct.ScmObj*, align 8
%anf_45bind40284 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40138, %struct.ScmObj* %lsts_4340144)
store volatile %struct.ScmObj* %anf_45bind40284, %struct.ScmObj** %stackaddr$prim50261, align 8
%stackaddr$prim50262 = alloca %struct.ScmObj*, align 8
%anf_45bind40285 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40139, %struct.ScmObj* %anf_45bind40284)
store volatile %struct.ScmObj* %anf_45bind40285, %struct.ScmObj** %stackaddr$prim50262, align 8
%stackaddr$makeclosure50263 = alloca %struct.ScmObj*, align 8
%fptrToInt50264 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41406 to i64
%ae41406 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt50264)
store volatile %struct.ScmObj* %ae41406, %struct.ScmObj** %stackaddr$makeclosure50263, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41406, %struct.ScmObj* %k40513, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41406, %struct.ScmObj* %_37foldr140129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41406, %struct.ScmObj* %anf_45bind40283, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41406, %struct.ScmObj* %f40139, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41406, %struct.ScmObj* %vs40142, i64 4)
%stackaddr$prim50265 = alloca %struct.ScmObj*, align 8
%cpsargs40524 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41406, %struct.ScmObj* %anf_45bind40285)
store volatile %struct.ScmObj* %cpsargs40524, %struct.ScmObj** %stackaddr$prim50265, align 8
%clofunc50266 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40135)
musttail call tailcc void %clofunc50266(%struct.ScmObj* %_37foldr40135, %struct.ScmObj* %cpsargs40524)
ret void
}

define tailcc void @proc_clo$ae41406(%struct.ScmObj* %env$ae41406,%struct.ScmObj* %current_45args48550) {
%stackaddr$env-ref50267 = alloca %struct.ScmObj*, align 8
%k40513 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41406, i64 0)
store %struct.ScmObj* %k40513, %struct.ScmObj** %stackaddr$env-ref50267
%stackaddr$env-ref50268 = alloca %struct.ScmObj*, align 8
%_37foldr140129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41406, i64 1)
store %struct.ScmObj* %_37foldr140129, %struct.ScmObj** %stackaddr$env-ref50268
%stackaddr$env-ref50269 = alloca %struct.ScmObj*, align 8
%anf_45bind40283 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41406, i64 2)
store %struct.ScmObj* %anf_45bind40283, %struct.ScmObj** %stackaddr$env-ref50269
%stackaddr$env-ref50270 = alloca %struct.ScmObj*, align 8
%f40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41406, i64 3)
store %struct.ScmObj* %f40139, %struct.ScmObj** %stackaddr$env-ref50270
%stackaddr$env-ref50271 = alloca %struct.ScmObj*, align 8
%vs40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41406, i64 4)
store %struct.ScmObj* %vs40142, %struct.ScmObj** %stackaddr$env-ref50271
%stackaddr$prim50272 = alloca %struct.ScmObj*, align 8
%_95k40521 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48550)
store volatile %struct.ScmObj* %_95k40521, %struct.ScmObj** %stackaddr$prim50272, align 8
%stackaddr$prim50273 = alloca %struct.ScmObj*, align 8
%current_45args48551 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48550)
store volatile %struct.ScmObj* %current_45args48551, %struct.ScmObj** %stackaddr$prim50273, align 8
%stackaddr$prim50274 = alloca %struct.ScmObj*, align 8
%anf_45bind40286 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48551)
store volatile %struct.ScmObj* %anf_45bind40286, %struct.ScmObj** %stackaddr$prim50274, align 8
%ae41411 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50275 = alloca %struct.ScmObj*, align 8
%anf_45bind40287 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40286, %struct.ScmObj* %ae41411)
store volatile %struct.ScmObj* %anf_45bind40287, %struct.ScmObj** %stackaddr$prim50275, align 8
%stackaddr$makeclosure50276 = alloca %struct.ScmObj*, align 8
%fptrToInt50277 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41413 to i64
%ae41413 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50277)
store volatile %struct.ScmObj* %ae41413, %struct.ScmObj** %stackaddr$makeclosure50276, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41413, %struct.ScmObj* %k40513, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41413, %struct.ScmObj* %f40139, i64 1)
%args48556$_37foldr140129$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50278 = alloca %struct.ScmObj*, align 8
%args48556$_37foldr140129$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40142, %struct.ScmObj* %args48556$_37foldr140129$0)
store volatile %struct.ScmObj* %args48556$_37foldr140129$1, %struct.ScmObj** %stackaddr$prim50278, align 8
%stackaddr$prim50279 = alloca %struct.ScmObj*, align 8
%args48556$_37foldr140129$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40287, %struct.ScmObj* %args48556$_37foldr140129$1)
store volatile %struct.ScmObj* %args48556$_37foldr140129$2, %struct.ScmObj** %stackaddr$prim50279, align 8
%stackaddr$prim50280 = alloca %struct.ScmObj*, align 8
%args48556$_37foldr140129$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40283, %struct.ScmObj* %args48556$_37foldr140129$2)
store volatile %struct.ScmObj* %args48556$_37foldr140129$3, %struct.ScmObj** %stackaddr$prim50280, align 8
%stackaddr$prim50281 = alloca %struct.ScmObj*, align 8
%args48556$_37foldr140129$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41413, %struct.ScmObj* %args48556$_37foldr140129$3)
store volatile %struct.ScmObj* %args48556$_37foldr140129$4, %struct.ScmObj** %stackaddr$prim50281, align 8
%clofunc50282 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140129)
musttail call tailcc void %clofunc50282(%struct.ScmObj* %_37foldr140129, %struct.ScmObj* %args48556$_37foldr140129$4)
ret void
}

define tailcc void @proc_clo$ae41413(%struct.ScmObj* %env$ae41413,%struct.ScmObj* %current_45args48553) {
%stackaddr$env-ref50283 = alloca %struct.ScmObj*, align 8
%k40513 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41413, i64 0)
store %struct.ScmObj* %k40513, %struct.ScmObj** %stackaddr$env-ref50283
%stackaddr$env-ref50284 = alloca %struct.ScmObj*, align 8
%f40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41413, i64 1)
store %struct.ScmObj* %f40139, %struct.ScmObj** %stackaddr$env-ref50284
%stackaddr$prim50285 = alloca %struct.ScmObj*, align 8
%_95k40522 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48553)
store volatile %struct.ScmObj* %_95k40522, %struct.ScmObj** %stackaddr$prim50285, align 8
%stackaddr$prim50286 = alloca %struct.ScmObj*, align 8
%current_45args48554 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48553)
store volatile %struct.ScmObj* %current_45args48554, %struct.ScmObj** %stackaddr$prim50286, align 8
%stackaddr$prim50287 = alloca %struct.ScmObj*, align 8
%anf_45bind40288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48554)
store volatile %struct.ScmObj* %anf_45bind40288, %struct.ScmObj** %stackaddr$prim50287, align 8
%stackaddr$prim50288 = alloca %struct.ScmObj*, align 8
%cpsargs40523 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40513, %struct.ScmObj* %anf_45bind40288)
store volatile %struct.ScmObj* %cpsargs40523, %struct.ScmObj** %stackaddr$prim50288, align 8
%clofunc50289 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40139)
musttail call tailcc void %clofunc50289(%struct.ScmObj* %f40139, %struct.ScmObj* %cpsargs40523)
ret void
}

define tailcc void @proc_clo$ae41382(%struct.ScmObj* %env$ae41382,%struct.ScmObj* %current_45args48557) {
%stackaddr$prim50290 = alloca %struct.ScmObj*, align 8
%k40525 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48557)
store volatile %struct.ScmObj* %k40525, %struct.ScmObj** %stackaddr$prim50290, align 8
%stackaddr$prim50291 = alloca %struct.ScmObj*, align 8
%current_45args48558 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48557)
store volatile %struct.ScmObj* %current_45args48558, %struct.ScmObj** %stackaddr$prim50291, align 8
%stackaddr$prim50292 = alloca %struct.ScmObj*, align 8
%a40147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48558)
store volatile %struct.ScmObj* %a40147, %struct.ScmObj** %stackaddr$prim50292, align 8
%stackaddr$prim50293 = alloca %struct.ScmObj*, align 8
%current_45args48559 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48558)
store volatile %struct.ScmObj* %current_45args48559, %struct.ScmObj** %stackaddr$prim50293, align 8
%stackaddr$prim50294 = alloca %struct.ScmObj*, align 8
%b40146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48559)
store volatile %struct.ScmObj* %b40146, %struct.ScmObj** %stackaddr$prim50294, align 8
%stackaddr$prim50295 = alloca %struct.ScmObj*, align 8
%cpsprim40526 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40147, %struct.ScmObj* %b40146)
store volatile %struct.ScmObj* %cpsprim40526, %struct.ScmObj** %stackaddr$prim50295, align 8
%ae41386 = call %struct.ScmObj* @const_init_int(i64 0)
%args48561$k40525$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50296 = alloca %struct.ScmObj*, align 8
%args48561$k40525$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40526, %struct.ScmObj* %args48561$k40525$0)
store volatile %struct.ScmObj* %args48561$k40525$1, %struct.ScmObj** %stackaddr$prim50296, align 8
%stackaddr$prim50297 = alloca %struct.ScmObj*, align 8
%args48561$k40525$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41386, %struct.ScmObj* %args48561$k40525$1)
store volatile %struct.ScmObj* %args48561$k40525$2, %struct.ScmObj** %stackaddr$prim50297, align 8
%clofunc50298 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40525)
musttail call tailcc void %clofunc50298(%struct.ScmObj* %k40525, %struct.ScmObj* %args48561$k40525$2)
ret void
}

define tailcc void @proc_clo$ae41358(%struct.ScmObj* %env$ae41358,%struct.ScmObj* %current_45args48564) {
%stackaddr$prim50299 = alloca %struct.ScmObj*, align 8
%k40527 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48564)
store volatile %struct.ScmObj* %k40527, %struct.ScmObj** %stackaddr$prim50299, align 8
%stackaddr$prim50300 = alloca %struct.ScmObj*, align 8
%current_45args48565 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48564)
store volatile %struct.ScmObj* %current_45args48565, %struct.ScmObj** %stackaddr$prim50300, align 8
%stackaddr$prim50301 = alloca %struct.ScmObj*, align 8
%x40143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48565)
store volatile %struct.ScmObj* %x40143, %struct.ScmObj** %stackaddr$prim50301, align 8
%stackaddr$prim50302 = alloca %struct.ScmObj*, align 8
%cpsprim40528 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40143)
store volatile %struct.ScmObj* %cpsprim40528, %struct.ScmObj** %stackaddr$prim50302, align 8
%ae41361 = call %struct.ScmObj* @const_init_int(i64 0)
%args48567$k40527$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50303 = alloca %struct.ScmObj*, align 8
%args48567$k40527$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40528, %struct.ScmObj* %args48567$k40527$0)
store volatile %struct.ScmObj* %args48567$k40527$1, %struct.ScmObj** %stackaddr$prim50303, align 8
%stackaddr$prim50304 = alloca %struct.ScmObj*, align 8
%args48567$k40527$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41361, %struct.ScmObj* %args48567$k40527$1)
store volatile %struct.ScmObj* %args48567$k40527$2, %struct.ScmObj** %stackaddr$prim50304, align 8
%clofunc50305 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40527)
musttail call tailcc void %clofunc50305(%struct.ScmObj* %k40527, %struct.ScmObj* %args48567$k40527$2)
ret void
}

define tailcc void @proc_clo$ae41334(%struct.ScmObj* %env$ae41334,%struct.ScmObj* %current_45args48570) {
%stackaddr$prim50306 = alloca %struct.ScmObj*, align 8
%k40529 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48570)
store volatile %struct.ScmObj* %k40529, %struct.ScmObj** %stackaddr$prim50306, align 8
%stackaddr$prim50307 = alloca %struct.ScmObj*, align 8
%current_45args48571 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48570)
store volatile %struct.ScmObj* %current_45args48571, %struct.ScmObj** %stackaddr$prim50307, align 8
%stackaddr$prim50308 = alloca %struct.ScmObj*, align 8
%x40145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48571)
store volatile %struct.ScmObj* %x40145, %struct.ScmObj** %stackaddr$prim50308, align 8
%stackaddr$prim50309 = alloca %struct.ScmObj*, align 8
%cpsprim40530 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40145)
store volatile %struct.ScmObj* %cpsprim40530, %struct.ScmObj** %stackaddr$prim50309, align 8
%ae41337 = call %struct.ScmObj* @const_init_int(i64 0)
%args48573$k40529$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50310 = alloca %struct.ScmObj*, align 8
%args48573$k40529$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40530, %struct.ScmObj* %args48573$k40529$0)
store volatile %struct.ScmObj* %args48573$k40529$1, %struct.ScmObj** %stackaddr$prim50310, align 8
%stackaddr$prim50311 = alloca %struct.ScmObj*, align 8
%args48573$k40529$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41337, %struct.ScmObj* %args48573$k40529$1)
store volatile %struct.ScmObj* %args48573$k40529$2, %struct.ScmObj** %stackaddr$prim50311, align 8
%clofunc50312 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40529)
musttail call tailcc void %clofunc50312(%struct.ScmObj* %k40529, %struct.ScmObj* %args48573$k40529$2)
ret void
}

define tailcc void @proc_clo$ae41286(%struct.ScmObj* %env$ae41286,%struct.ScmObj* %current_45args48576) {
%stackaddr$prim50313 = alloca %struct.ScmObj*, align 8
%k40531 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48576)
store volatile %struct.ScmObj* %k40531, %struct.ScmObj** %stackaddr$prim50313, align 8
%stackaddr$prim50314 = alloca %struct.ScmObj*, align 8
%current_45args48577 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48576)
store volatile %struct.ScmObj* %current_45args48577, %struct.ScmObj** %stackaddr$prim50314, align 8
%stackaddr$prim50315 = alloca %struct.ScmObj*, align 8
%lst40141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48577)
store volatile %struct.ScmObj* %lst40141, %struct.ScmObj** %stackaddr$prim50315, align 8
%stackaddr$prim50316 = alloca %struct.ScmObj*, align 8
%current_45args48578 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48577)
store volatile %struct.ScmObj* %current_45args48578, %struct.ScmObj** %stackaddr$prim50316, align 8
%stackaddr$prim50317 = alloca %struct.ScmObj*, align 8
%b40140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48578)
store volatile %struct.ScmObj* %b40140, %struct.ScmObj** %stackaddr$prim50317, align 8
%truthy$cmp50318 = call i64 @is_truthy_value(%struct.ScmObj* %b40140)
%cmp$cmp50318 = icmp eq i64 %truthy$cmp50318, 1
br i1 %cmp$cmp50318, label %truebranch$cmp50318, label %falsebranch$cmp50318
truebranch$cmp50318:
%ae41289 = call %struct.ScmObj* @const_init_int(i64 0)
%args48580$k40531$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50319 = alloca %struct.ScmObj*, align 8
%args48580$k40531$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40140, %struct.ScmObj* %args48580$k40531$0)
store volatile %struct.ScmObj* %args48580$k40531$1, %struct.ScmObj** %stackaddr$prim50319, align 8
%stackaddr$prim50320 = alloca %struct.ScmObj*, align 8
%args48580$k40531$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41289, %struct.ScmObj* %args48580$k40531$1)
store volatile %struct.ScmObj* %args48580$k40531$2, %struct.ScmObj** %stackaddr$prim50320, align 8
%clofunc50321 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40531)
musttail call tailcc void %clofunc50321(%struct.ScmObj* %k40531, %struct.ScmObj* %args48580$k40531$2)
ret void
falsebranch$cmp50318:
%stackaddr$prim50322 = alloca %struct.ScmObj*, align 8
%cpsprim40532 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40141)
store volatile %struct.ScmObj* %cpsprim40532, %struct.ScmObj** %stackaddr$prim50322, align 8
%ae41296 = call %struct.ScmObj* @const_init_int(i64 0)
%args48581$k40531$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50323 = alloca %struct.ScmObj*, align 8
%args48581$k40531$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40532, %struct.ScmObj* %args48581$k40531$0)
store volatile %struct.ScmObj* %args48581$k40531$1, %struct.ScmObj** %stackaddr$prim50323, align 8
%stackaddr$prim50324 = alloca %struct.ScmObj*, align 8
%args48581$k40531$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41296, %struct.ScmObj* %args48581$k40531$1)
store volatile %struct.ScmObj* %args48581$k40531$2, %struct.ScmObj** %stackaddr$prim50324, align 8
%clofunc50325 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40531)
musttail call tailcc void %clofunc50325(%struct.ScmObj* %k40531, %struct.ScmObj* %args48581$k40531$2)
ret void
}

define tailcc void @proc_clo$ae41243(%struct.ScmObj* %env$ae41243,%struct.ScmObj* %current_45args48585) {
%stackaddr$env-ref50326 = alloca %struct.ScmObj*, align 8
%_37take40121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41243, i64 0)
store %struct.ScmObj* %_37take40121, %struct.ScmObj** %stackaddr$env-ref50326
%stackaddr$env-ref50327 = alloca %struct.ScmObj*, align 8
%_37length40118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41243, i64 1)
store %struct.ScmObj* %_37length40118, %struct.ScmObj** %stackaddr$env-ref50327
%stackaddr$prim50328 = alloca %struct.ScmObj*, align 8
%k40533 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48585)
store volatile %struct.ScmObj* %k40533, %struct.ScmObj** %stackaddr$prim50328, align 8
%stackaddr$prim50329 = alloca %struct.ScmObj*, align 8
%current_45args48586 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48585)
store volatile %struct.ScmObj* %current_45args48586, %struct.ScmObj** %stackaddr$prim50329, align 8
%stackaddr$prim50330 = alloca %struct.ScmObj*, align 8
%lst40150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48586)
store volatile %struct.ScmObj* %lst40150, %struct.ScmObj** %stackaddr$prim50330, align 8
%stackaddr$prim50331 = alloca %struct.ScmObj*, align 8
%current_45args48587 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48586)
store volatile %struct.ScmObj* %current_45args48587, %struct.ScmObj** %stackaddr$prim50331, align 8
%stackaddr$prim50332 = alloca %struct.ScmObj*, align 8
%n40149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48587)
store volatile %struct.ScmObj* %n40149, %struct.ScmObj** %stackaddr$prim50332, align 8
%stackaddr$makeclosure50333 = alloca %struct.ScmObj*, align 8
%fptrToInt50334 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41245 to i64
%ae41245 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50334)
store volatile %struct.ScmObj* %ae41245, %struct.ScmObj** %stackaddr$makeclosure50333, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41245, %struct.ScmObj* %k40533, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41245, %struct.ScmObj* %n40149, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41245, %struct.ScmObj* %_37take40121, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41245, %struct.ScmObj* %lst40150, i64 3)
%args48593$_37length40118$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50335 = alloca %struct.ScmObj*, align 8
%args48593$_37length40118$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40150, %struct.ScmObj* %args48593$_37length40118$0)
store volatile %struct.ScmObj* %args48593$_37length40118$1, %struct.ScmObj** %stackaddr$prim50335, align 8
%stackaddr$prim50336 = alloca %struct.ScmObj*, align 8
%args48593$_37length40118$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41245, %struct.ScmObj* %args48593$_37length40118$1)
store volatile %struct.ScmObj* %args48593$_37length40118$2, %struct.ScmObj** %stackaddr$prim50336, align 8
%clofunc50337 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40118)
musttail call tailcc void %clofunc50337(%struct.ScmObj* %_37length40118, %struct.ScmObj* %args48593$_37length40118$2)
ret void
}

define tailcc void @proc_clo$ae41245(%struct.ScmObj* %env$ae41245,%struct.ScmObj* %current_45args48589) {
%stackaddr$env-ref50338 = alloca %struct.ScmObj*, align 8
%k40533 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41245, i64 0)
store %struct.ScmObj* %k40533, %struct.ScmObj** %stackaddr$env-ref50338
%stackaddr$env-ref50339 = alloca %struct.ScmObj*, align 8
%n40149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41245, i64 1)
store %struct.ScmObj* %n40149, %struct.ScmObj** %stackaddr$env-ref50339
%stackaddr$env-ref50340 = alloca %struct.ScmObj*, align 8
%_37take40121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41245, i64 2)
store %struct.ScmObj* %_37take40121, %struct.ScmObj** %stackaddr$env-ref50340
%stackaddr$env-ref50341 = alloca %struct.ScmObj*, align 8
%lst40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41245, i64 3)
store %struct.ScmObj* %lst40150, %struct.ScmObj** %stackaddr$env-ref50341
%stackaddr$prim50342 = alloca %struct.ScmObj*, align 8
%_95k40534 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48589)
store volatile %struct.ScmObj* %_95k40534, %struct.ScmObj** %stackaddr$prim50342, align 8
%stackaddr$prim50343 = alloca %struct.ScmObj*, align 8
%current_45args48590 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48589)
store volatile %struct.ScmObj* %current_45args48590, %struct.ScmObj** %stackaddr$prim50343, align 8
%stackaddr$prim50344 = alloca %struct.ScmObj*, align 8
%anf_45bind40275 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48590)
store volatile %struct.ScmObj* %anf_45bind40275, %struct.ScmObj** %stackaddr$prim50344, align 8
%stackaddr$prim50345 = alloca %struct.ScmObj*, align 8
%anf_45bind40276 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40275, %struct.ScmObj* %n40149)
store volatile %struct.ScmObj* %anf_45bind40276, %struct.ScmObj** %stackaddr$prim50345, align 8
%args48592$_37take40121$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50346 = alloca %struct.ScmObj*, align 8
%args48592$_37take40121$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40276, %struct.ScmObj* %args48592$_37take40121$0)
store volatile %struct.ScmObj* %args48592$_37take40121$1, %struct.ScmObj** %stackaddr$prim50346, align 8
%stackaddr$prim50347 = alloca %struct.ScmObj*, align 8
%args48592$_37take40121$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40150, %struct.ScmObj* %args48592$_37take40121$1)
store volatile %struct.ScmObj* %args48592$_37take40121$2, %struct.ScmObj** %stackaddr$prim50347, align 8
%stackaddr$prim50348 = alloca %struct.ScmObj*, align 8
%args48592$_37take40121$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40533, %struct.ScmObj* %args48592$_37take40121$2)
store volatile %struct.ScmObj* %args48592$_37take40121$3, %struct.ScmObj** %stackaddr$prim50348, align 8
%clofunc50349 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40121)
musttail call tailcc void %clofunc50349(%struct.ScmObj* %_37take40121, %struct.ScmObj* %args48592$_37take40121$3)
ret void
}

define tailcc void @proc_clo$ae41189(%struct.ScmObj* %env$ae41189,%struct.ScmObj* %current_45args48595) {
%stackaddr$env-ref50350 = alloca %struct.ScmObj*, align 8
%_37foldl140113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41189, i64 0)
store %struct.ScmObj* %_37foldl140113, %struct.ScmObj** %stackaddr$env-ref50350
%stackaddr$prim50351 = alloca %struct.ScmObj*, align 8
%k40535 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48595)
store volatile %struct.ScmObj* %k40535, %struct.ScmObj** %stackaddr$prim50351, align 8
%stackaddr$prim50352 = alloca %struct.ScmObj*, align 8
%current_45args48596 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48595)
store volatile %struct.ScmObj* %current_45args48596, %struct.ScmObj** %stackaddr$prim50352, align 8
%stackaddr$prim50353 = alloca %struct.ScmObj*, align 8
%lst40152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48596)
store volatile %struct.ScmObj* %lst40152, %struct.ScmObj** %stackaddr$prim50353, align 8
%stackaddr$makeclosure50354 = alloca %struct.ScmObj*, align 8
%fptrToInt50355 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41190 to i64
%ae41190 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50355)
store volatile %struct.ScmObj* %ae41190, %struct.ScmObj** %stackaddr$makeclosure50354, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41190, %struct.ScmObj* %lst40152, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41190, %struct.ScmObj* %k40535, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41190, %struct.ScmObj* %_37foldl140113, i64 2)
%ae41191 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50356 = alloca %struct.ScmObj*, align 8
%fptrToInt50357 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41192 to i64
%ae41192 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50357)
store volatile %struct.ScmObj* %ae41192, %struct.ScmObj** %stackaddr$makeclosure50356, align 8
%args48607$ae41190$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50358 = alloca %struct.ScmObj*, align 8
%args48607$ae41190$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41192, %struct.ScmObj* %args48607$ae41190$0)
store volatile %struct.ScmObj* %args48607$ae41190$1, %struct.ScmObj** %stackaddr$prim50358, align 8
%stackaddr$prim50359 = alloca %struct.ScmObj*, align 8
%args48607$ae41190$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41191, %struct.ScmObj* %args48607$ae41190$1)
store volatile %struct.ScmObj* %args48607$ae41190$2, %struct.ScmObj** %stackaddr$prim50359, align 8
%clofunc50360 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41190)
musttail call tailcc void %clofunc50360(%struct.ScmObj* %ae41190, %struct.ScmObj* %args48607$ae41190$2)
ret void
}

define tailcc void @proc_clo$ae41190(%struct.ScmObj* %env$ae41190,%struct.ScmObj* %current_45args48598) {
%stackaddr$env-ref50361 = alloca %struct.ScmObj*, align 8
%lst40152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41190, i64 0)
store %struct.ScmObj* %lst40152, %struct.ScmObj** %stackaddr$env-ref50361
%stackaddr$env-ref50362 = alloca %struct.ScmObj*, align 8
%k40535 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41190, i64 1)
store %struct.ScmObj* %k40535, %struct.ScmObj** %stackaddr$env-ref50362
%stackaddr$env-ref50363 = alloca %struct.ScmObj*, align 8
%_37foldl140113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41190, i64 2)
store %struct.ScmObj* %_37foldl140113, %struct.ScmObj** %stackaddr$env-ref50363
%stackaddr$prim50364 = alloca %struct.ScmObj*, align 8
%_95k40536 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48598)
store volatile %struct.ScmObj* %_95k40536, %struct.ScmObj** %stackaddr$prim50364, align 8
%stackaddr$prim50365 = alloca %struct.ScmObj*, align 8
%current_45args48599 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48598)
store volatile %struct.ScmObj* %current_45args48599, %struct.ScmObj** %stackaddr$prim50365, align 8
%stackaddr$prim50366 = alloca %struct.ScmObj*, align 8
%anf_45bind40274 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48599)
store volatile %struct.ScmObj* %anf_45bind40274, %struct.ScmObj** %stackaddr$prim50366, align 8
%ae41211 = call %struct.ScmObj* @const_init_null()
%args48601$_37foldl140113$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50367 = alloca %struct.ScmObj*, align 8
%args48601$_37foldl140113$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40152, %struct.ScmObj* %args48601$_37foldl140113$0)
store volatile %struct.ScmObj* %args48601$_37foldl140113$1, %struct.ScmObj** %stackaddr$prim50367, align 8
%stackaddr$prim50368 = alloca %struct.ScmObj*, align 8
%args48601$_37foldl140113$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41211, %struct.ScmObj* %args48601$_37foldl140113$1)
store volatile %struct.ScmObj* %args48601$_37foldl140113$2, %struct.ScmObj** %stackaddr$prim50368, align 8
%stackaddr$prim50369 = alloca %struct.ScmObj*, align 8
%args48601$_37foldl140113$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40274, %struct.ScmObj* %args48601$_37foldl140113$2)
store volatile %struct.ScmObj* %args48601$_37foldl140113$3, %struct.ScmObj** %stackaddr$prim50369, align 8
%stackaddr$prim50370 = alloca %struct.ScmObj*, align 8
%args48601$_37foldl140113$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40535, %struct.ScmObj* %args48601$_37foldl140113$3)
store volatile %struct.ScmObj* %args48601$_37foldl140113$4, %struct.ScmObj** %stackaddr$prim50370, align 8
%clofunc50371 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140113)
musttail call tailcc void %clofunc50371(%struct.ScmObj* %_37foldl140113, %struct.ScmObj* %args48601$_37foldl140113$4)
ret void
}

define tailcc void @proc_clo$ae41192(%struct.ScmObj* %env$ae41192,%struct.ScmObj* %current_45args48602) {
%stackaddr$prim50372 = alloca %struct.ScmObj*, align 8
%k40537 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48602)
store volatile %struct.ScmObj* %k40537, %struct.ScmObj** %stackaddr$prim50372, align 8
%stackaddr$prim50373 = alloca %struct.ScmObj*, align 8
%current_45args48603 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48602)
store volatile %struct.ScmObj* %current_45args48603, %struct.ScmObj** %stackaddr$prim50373, align 8
%stackaddr$prim50374 = alloca %struct.ScmObj*, align 8
%x40154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48603)
store volatile %struct.ScmObj* %x40154, %struct.ScmObj** %stackaddr$prim50374, align 8
%stackaddr$prim50375 = alloca %struct.ScmObj*, align 8
%current_45args48604 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48603)
store volatile %struct.ScmObj* %current_45args48604, %struct.ScmObj** %stackaddr$prim50375, align 8
%stackaddr$prim50376 = alloca %struct.ScmObj*, align 8
%y40153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48604)
store volatile %struct.ScmObj* %y40153, %struct.ScmObj** %stackaddr$prim50376, align 8
%ae41194 = call %struct.ScmObj* @const_init_int(i64 0)
%args48606$k40537$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50377 = alloca %struct.ScmObj*, align 8
%args48606$k40537$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x40154, %struct.ScmObj* %args48606$k40537$0)
store volatile %struct.ScmObj* %args48606$k40537$1, %struct.ScmObj** %stackaddr$prim50377, align 8
%stackaddr$prim50378 = alloca %struct.ScmObj*, align 8
%args48606$k40537$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41194, %struct.ScmObj* %args48606$k40537$1)
store volatile %struct.ScmObj* %args48606$k40537$2, %struct.ScmObj** %stackaddr$prim50378, align 8
%clofunc50379 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40537)
musttail call tailcc void %clofunc50379(%struct.ScmObj* %k40537, %struct.ScmObj* %args48606$k40537$2)
ret void
}

define tailcc void @proc_clo$ae41110(%struct.ScmObj* %env$ae41110,%struct.ScmObj* %current_45args48610) {
%stackaddr$prim50380 = alloca %struct.ScmObj*, align 8
%k40538 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48610)
store volatile %struct.ScmObj* %k40538, %struct.ScmObj** %stackaddr$prim50380, align 8
%stackaddr$prim50381 = alloca %struct.ScmObj*, align 8
%current_45args48611 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48610)
store volatile %struct.ScmObj* %current_45args48611, %struct.ScmObj** %stackaddr$prim50381, align 8
%stackaddr$prim50382 = alloca %struct.ScmObj*, align 8
%_37foldl140114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48611)
store volatile %struct.ScmObj* %_37foldl140114, %struct.ScmObj** %stackaddr$prim50382, align 8
%ae41112 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50383 = alloca %struct.ScmObj*, align 8
%fptrToInt50384 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41113 to i64
%ae41113 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50384)
store volatile %struct.ScmObj* %ae41113, %struct.ScmObj** %stackaddr$makeclosure50383, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41113, %struct.ScmObj* %_37foldl140114, i64 0)
%args48624$k40538$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50385 = alloca %struct.ScmObj*, align 8
%args48624$k40538$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41113, %struct.ScmObj* %args48624$k40538$0)
store volatile %struct.ScmObj* %args48624$k40538$1, %struct.ScmObj** %stackaddr$prim50385, align 8
%stackaddr$prim50386 = alloca %struct.ScmObj*, align 8
%args48624$k40538$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41112, %struct.ScmObj* %args48624$k40538$1)
store volatile %struct.ScmObj* %args48624$k40538$2, %struct.ScmObj** %stackaddr$prim50386, align 8
%clofunc50387 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40538)
musttail call tailcc void %clofunc50387(%struct.ScmObj* %k40538, %struct.ScmObj* %args48624$k40538$2)
ret void
}

define tailcc void @proc_clo$ae41113(%struct.ScmObj* %env$ae41113,%struct.ScmObj* %current_45args48613) {
%stackaddr$env-ref50388 = alloca %struct.ScmObj*, align 8
%_37foldl140114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41113, i64 0)
store %struct.ScmObj* %_37foldl140114, %struct.ScmObj** %stackaddr$env-ref50388
%stackaddr$prim50389 = alloca %struct.ScmObj*, align 8
%k40539 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48613)
store volatile %struct.ScmObj* %k40539, %struct.ScmObj** %stackaddr$prim50389, align 8
%stackaddr$prim50390 = alloca %struct.ScmObj*, align 8
%current_45args48614 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48613)
store volatile %struct.ScmObj* %current_45args48614, %struct.ScmObj** %stackaddr$prim50390, align 8
%stackaddr$prim50391 = alloca %struct.ScmObj*, align 8
%f40117 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48614)
store volatile %struct.ScmObj* %f40117, %struct.ScmObj** %stackaddr$prim50391, align 8
%stackaddr$prim50392 = alloca %struct.ScmObj*, align 8
%current_45args48615 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48614)
store volatile %struct.ScmObj* %current_45args48615, %struct.ScmObj** %stackaddr$prim50392, align 8
%stackaddr$prim50393 = alloca %struct.ScmObj*, align 8
%acc40116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48615)
store volatile %struct.ScmObj* %acc40116, %struct.ScmObj** %stackaddr$prim50393, align 8
%stackaddr$prim50394 = alloca %struct.ScmObj*, align 8
%current_45args48616 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48615)
store volatile %struct.ScmObj* %current_45args48616, %struct.ScmObj** %stackaddr$prim50394, align 8
%stackaddr$prim50395 = alloca %struct.ScmObj*, align 8
%lst40115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48616)
store volatile %struct.ScmObj* %lst40115, %struct.ScmObj** %stackaddr$prim50395, align 8
%stackaddr$prim50396 = alloca %struct.ScmObj*, align 8
%anf_45bind40269 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40115)
store volatile %struct.ScmObj* %anf_45bind40269, %struct.ScmObj** %stackaddr$prim50396, align 8
%truthy$cmp50397 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40269)
%cmp$cmp50397 = icmp eq i64 %truthy$cmp50397, 1
br i1 %cmp$cmp50397, label %truebranch$cmp50397, label %falsebranch$cmp50397
truebranch$cmp50397:
%ae41117 = call %struct.ScmObj* @const_init_int(i64 0)
%args48618$k40539$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50398 = alloca %struct.ScmObj*, align 8
%args48618$k40539$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40116, %struct.ScmObj* %args48618$k40539$0)
store volatile %struct.ScmObj* %args48618$k40539$1, %struct.ScmObj** %stackaddr$prim50398, align 8
%stackaddr$prim50399 = alloca %struct.ScmObj*, align 8
%args48618$k40539$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41117, %struct.ScmObj* %args48618$k40539$1)
store volatile %struct.ScmObj* %args48618$k40539$2, %struct.ScmObj** %stackaddr$prim50399, align 8
%clofunc50400 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40539)
musttail call tailcc void %clofunc50400(%struct.ScmObj* %k40539, %struct.ScmObj* %args48618$k40539$2)
ret void
falsebranch$cmp50397:
%stackaddr$prim50401 = alloca %struct.ScmObj*, align 8
%anf_45bind40270 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40115)
store volatile %struct.ScmObj* %anf_45bind40270, %struct.ScmObj** %stackaddr$prim50401, align 8
%stackaddr$makeclosure50402 = alloca %struct.ScmObj*, align 8
%fptrToInt50403 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41124 to i64
%ae41124 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50403)
store volatile %struct.ScmObj* %ae41124, %struct.ScmObj** %stackaddr$makeclosure50402, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41124, %struct.ScmObj* %f40117, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41124, %struct.ScmObj* %lst40115, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41124, %struct.ScmObj* %_37foldl140114, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41124, %struct.ScmObj* %k40539, i64 3)
%args48623$f40117$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50404 = alloca %struct.ScmObj*, align 8
%args48623$f40117$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40116, %struct.ScmObj* %args48623$f40117$0)
store volatile %struct.ScmObj* %args48623$f40117$1, %struct.ScmObj** %stackaddr$prim50404, align 8
%stackaddr$prim50405 = alloca %struct.ScmObj*, align 8
%args48623$f40117$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40270, %struct.ScmObj* %args48623$f40117$1)
store volatile %struct.ScmObj* %args48623$f40117$2, %struct.ScmObj** %stackaddr$prim50405, align 8
%stackaddr$prim50406 = alloca %struct.ScmObj*, align 8
%args48623$f40117$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41124, %struct.ScmObj* %args48623$f40117$2)
store volatile %struct.ScmObj* %args48623$f40117$3, %struct.ScmObj** %stackaddr$prim50406, align 8
%clofunc50407 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40117)
musttail call tailcc void %clofunc50407(%struct.ScmObj* %f40117, %struct.ScmObj* %args48623$f40117$3)
ret void
}

define tailcc void @proc_clo$ae41124(%struct.ScmObj* %env$ae41124,%struct.ScmObj* %current_45args48619) {
%stackaddr$env-ref50408 = alloca %struct.ScmObj*, align 8
%f40117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41124, i64 0)
store %struct.ScmObj* %f40117, %struct.ScmObj** %stackaddr$env-ref50408
%stackaddr$env-ref50409 = alloca %struct.ScmObj*, align 8
%lst40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41124, i64 1)
store %struct.ScmObj* %lst40115, %struct.ScmObj** %stackaddr$env-ref50409
%stackaddr$env-ref50410 = alloca %struct.ScmObj*, align 8
%_37foldl140114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41124, i64 2)
store %struct.ScmObj* %_37foldl140114, %struct.ScmObj** %stackaddr$env-ref50410
%stackaddr$env-ref50411 = alloca %struct.ScmObj*, align 8
%k40539 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41124, i64 3)
store %struct.ScmObj* %k40539, %struct.ScmObj** %stackaddr$env-ref50411
%stackaddr$prim50412 = alloca %struct.ScmObj*, align 8
%_95k40540 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48619)
store volatile %struct.ScmObj* %_95k40540, %struct.ScmObj** %stackaddr$prim50412, align 8
%stackaddr$prim50413 = alloca %struct.ScmObj*, align 8
%current_45args48620 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48619)
store volatile %struct.ScmObj* %current_45args48620, %struct.ScmObj** %stackaddr$prim50413, align 8
%stackaddr$prim50414 = alloca %struct.ScmObj*, align 8
%anf_45bind40271 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48620)
store volatile %struct.ScmObj* %anf_45bind40271, %struct.ScmObj** %stackaddr$prim50414, align 8
%stackaddr$prim50415 = alloca %struct.ScmObj*, align 8
%anf_45bind40272 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40115)
store volatile %struct.ScmObj* %anf_45bind40272, %struct.ScmObj** %stackaddr$prim50415, align 8
%args48622$_37foldl140114$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50416 = alloca %struct.ScmObj*, align 8
%args48622$_37foldl140114$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40272, %struct.ScmObj* %args48622$_37foldl140114$0)
store volatile %struct.ScmObj* %args48622$_37foldl140114$1, %struct.ScmObj** %stackaddr$prim50416, align 8
%stackaddr$prim50417 = alloca %struct.ScmObj*, align 8
%args48622$_37foldl140114$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40271, %struct.ScmObj* %args48622$_37foldl140114$1)
store volatile %struct.ScmObj* %args48622$_37foldl140114$2, %struct.ScmObj** %stackaddr$prim50417, align 8
%stackaddr$prim50418 = alloca %struct.ScmObj*, align 8
%args48622$_37foldl140114$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40117, %struct.ScmObj* %args48622$_37foldl140114$2)
store volatile %struct.ScmObj* %args48622$_37foldl140114$3, %struct.ScmObj** %stackaddr$prim50418, align 8
%stackaddr$prim50419 = alloca %struct.ScmObj*, align 8
%args48622$_37foldl140114$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40539, %struct.ScmObj* %args48622$_37foldl140114$3)
store volatile %struct.ScmObj* %args48622$_37foldl140114$4, %struct.ScmObj** %stackaddr$prim50419, align 8
%clofunc50420 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140114)
musttail call tailcc void %clofunc50420(%struct.ScmObj* %_37foldl140114, %struct.ScmObj* %args48622$_37foldl140114$4)
ret void
}

define tailcc void @proc_clo$ae41027(%struct.ScmObj* %env$ae41027,%struct.ScmObj* %current_45args48627) {
%stackaddr$prim50421 = alloca %struct.ScmObj*, align 8
%k40541 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48627)
store volatile %struct.ScmObj* %k40541, %struct.ScmObj** %stackaddr$prim50421, align 8
%stackaddr$prim50422 = alloca %struct.ScmObj*, align 8
%current_45args48628 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48627)
store volatile %struct.ScmObj* %current_45args48628, %struct.ScmObj** %stackaddr$prim50422, align 8
%stackaddr$prim50423 = alloca %struct.ScmObj*, align 8
%_37length40119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48628)
store volatile %struct.ScmObj* %_37length40119, %struct.ScmObj** %stackaddr$prim50423, align 8
%ae41029 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50424 = alloca %struct.ScmObj*, align 8
%fptrToInt50425 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41030 to i64
%ae41030 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50425)
store volatile %struct.ScmObj* %ae41030, %struct.ScmObj** %stackaddr$makeclosure50424, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41030, %struct.ScmObj* %_37length40119, i64 0)
%args48639$k40541$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50426 = alloca %struct.ScmObj*, align 8
%args48639$k40541$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41030, %struct.ScmObj* %args48639$k40541$0)
store volatile %struct.ScmObj* %args48639$k40541$1, %struct.ScmObj** %stackaddr$prim50426, align 8
%stackaddr$prim50427 = alloca %struct.ScmObj*, align 8
%args48639$k40541$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41029, %struct.ScmObj* %args48639$k40541$1)
store volatile %struct.ScmObj* %args48639$k40541$2, %struct.ScmObj** %stackaddr$prim50427, align 8
%clofunc50428 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40541)
musttail call tailcc void %clofunc50428(%struct.ScmObj* %k40541, %struct.ScmObj* %args48639$k40541$2)
ret void
}

define tailcc void @proc_clo$ae41030(%struct.ScmObj* %env$ae41030,%struct.ScmObj* %current_45args48630) {
%stackaddr$env-ref50429 = alloca %struct.ScmObj*, align 8
%_37length40119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41030, i64 0)
store %struct.ScmObj* %_37length40119, %struct.ScmObj** %stackaddr$env-ref50429
%stackaddr$prim50430 = alloca %struct.ScmObj*, align 8
%k40542 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48630)
store volatile %struct.ScmObj* %k40542, %struct.ScmObj** %stackaddr$prim50430, align 8
%stackaddr$prim50431 = alloca %struct.ScmObj*, align 8
%current_45args48631 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48630)
store volatile %struct.ScmObj* %current_45args48631, %struct.ScmObj** %stackaddr$prim50431, align 8
%stackaddr$prim50432 = alloca %struct.ScmObj*, align 8
%lst40120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48631)
store volatile %struct.ScmObj* %lst40120, %struct.ScmObj** %stackaddr$prim50432, align 8
%stackaddr$prim50433 = alloca %struct.ScmObj*, align 8
%anf_45bind40265 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40120)
store volatile %struct.ScmObj* %anf_45bind40265, %struct.ScmObj** %stackaddr$prim50433, align 8
%truthy$cmp50434 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40265)
%cmp$cmp50434 = icmp eq i64 %truthy$cmp50434, 1
br i1 %cmp$cmp50434, label %truebranch$cmp50434, label %falsebranch$cmp50434
truebranch$cmp50434:
%ae41034 = call %struct.ScmObj* @const_init_int(i64 0)
%ae41035 = call %struct.ScmObj* @const_init_int(i64 0)
%args48633$k40542$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50435 = alloca %struct.ScmObj*, align 8
%args48633$k40542$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41035, %struct.ScmObj* %args48633$k40542$0)
store volatile %struct.ScmObj* %args48633$k40542$1, %struct.ScmObj** %stackaddr$prim50435, align 8
%stackaddr$prim50436 = alloca %struct.ScmObj*, align 8
%args48633$k40542$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41034, %struct.ScmObj* %args48633$k40542$1)
store volatile %struct.ScmObj* %args48633$k40542$2, %struct.ScmObj** %stackaddr$prim50436, align 8
%clofunc50437 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40542)
musttail call tailcc void %clofunc50437(%struct.ScmObj* %k40542, %struct.ScmObj* %args48633$k40542$2)
ret void
falsebranch$cmp50434:
%stackaddr$prim50438 = alloca %struct.ScmObj*, align 8
%anf_45bind40266 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40120)
store volatile %struct.ScmObj* %anf_45bind40266, %struct.ScmObj** %stackaddr$prim50438, align 8
%stackaddr$makeclosure50439 = alloca %struct.ScmObj*, align 8
%fptrToInt50440 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41044 to i64
%ae41044 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50440)
store volatile %struct.ScmObj* %ae41044, %struct.ScmObj** %stackaddr$makeclosure50439, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41044, %struct.ScmObj* %k40542, i64 0)
%args48638$_37length40119$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50441 = alloca %struct.ScmObj*, align 8
%args48638$_37length40119$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40266, %struct.ScmObj* %args48638$_37length40119$0)
store volatile %struct.ScmObj* %args48638$_37length40119$1, %struct.ScmObj** %stackaddr$prim50441, align 8
%stackaddr$prim50442 = alloca %struct.ScmObj*, align 8
%args48638$_37length40119$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41044, %struct.ScmObj* %args48638$_37length40119$1)
store volatile %struct.ScmObj* %args48638$_37length40119$2, %struct.ScmObj** %stackaddr$prim50442, align 8
%clofunc50443 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40119)
musttail call tailcc void %clofunc50443(%struct.ScmObj* %_37length40119, %struct.ScmObj* %args48638$_37length40119$2)
ret void
}

define tailcc void @proc_clo$ae41044(%struct.ScmObj* %env$ae41044,%struct.ScmObj* %current_45args48634) {
%stackaddr$env-ref50444 = alloca %struct.ScmObj*, align 8
%k40542 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41044, i64 0)
store %struct.ScmObj* %k40542, %struct.ScmObj** %stackaddr$env-ref50444
%stackaddr$prim50445 = alloca %struct.ScmObj*, align 8
%_95k40543 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48634)
store volatile %struct.ScmObj* %_95k40543, %struct.ScmObj** %stackaddr$prim50445, align 8
%stackaddr$prim50446 = alloca %struct.ScmObj*, align 8
%current_45args48635 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48634)
store volatile %struct.ScmObj* %current_45args48635, %struct.ScmObj** %stackaddr$prim50446, align 8
%stackaddr$prim50447 = alloca %struct.ScmObj*, align 8
%anf_45bind40267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48635)
store volatile %struct.ScmObj* %anf_45bind40267, %struct.ScmObj** %stackaddr$prim50447, align 8
%ae41046 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50448 = alloca %struct.ScmObj*, align 8
%cpsprim40544 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae41046, %struct.ScmObj* %anf_45bind40267)
store volatile %struct.ScmObj* %cpsprim40544, %struct.ScmObj** %stackaddr$prim50448, align 8
%ae41049 = call %struct.ScmObj* @const_init_int(i64 0)
%args48637$k40542$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50449 = alloca %struct.ScmObj*, align 8
%args48637$k40542$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40544, %struct.ScmObj* %args48637$k40542$0)
store volatile %struct.ScmObj* %args48637$k40542$1, %struct.ScmObj** %stackaddr$prim50449, align 8
%stackaddr$prim50450 = alloca %struct.ScmObj*, align 8
%args48637$k40542$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41049, %struct.ScmObj* %args48637$k40542$1)
store volatile %struct.ScmObj* %args48637$k40542$2, %struct.ScmObj** %stackaddr$prim50450, align 8
%clofunc50451 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40542)
musttail call tailcc void %clofunc50451(%struct.ScmObj* %k40542, %struct.ScmObj* %args48637$k40542$2)
ret void
}

define tailcc void @proc_clo$ae40877(%struct.ScmObj* %env$ae40877,%struct.ScmObj* %current_45args48642) {
%stackaddr$prim50452 = alloca %struct.ScmObj*, align 8
%k40545 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48642)
store volatile %struct.ScmObj* %k40545, %struct.ScmObj** %stackaddr$prim50452, align 8
%stackaddr$prim50453 = alloca %struct.ScmObj*, align 8
%current_45args48643 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48642)
store volatile %struct.ScmObj* %current_45args48643, %struct.ScmObj** %stackaddr$prim50453, align 8
%stackaddr$prim50454 = alloca %struct.ScmObj*, align 8
%_37take40122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48643)
store volatile %struct.ScmObj* %_37take40122, %struct.ScmObj** %stackaddr$prim50454, align 8
%ae40879 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50455 = alloca %struct.ScmObj*, align 8
%fptrToInt50456 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40880 to i64
%ae40880 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50456)
store volatile %struct.ScmObj* %ae40880, %struct.ScmObj** %stackaddr$makeclosure50455, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40880, %struct.ScmObj* %_37take40122, i64 0)
%args48656$k40545$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50457 = alloca %struct.ScmObj*, align 8
%args48656$k40545$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40880, %struct.ScmObj* %args48656$k40545$0)
store volatile %struct.ScmObj* %args48656$k40545$1, %struct.ScmObj** %stackaddr$prim50457, align 8
%stackaddr$prim50458 = alloca %struct.ScmObj*, align 8
%args48656$k40545$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40879, %struct.ScmObj* %args48656$k40545$1)
store volatile %struct.ScmObj* %args48656$k40545$2, %struct.ScmObj** %stackaddr$prim50458, align 8
%clofunc50459 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40545)
musttail call tailcc void %clofunc50459(%struct.ScmObj* %k40545, %struct.ScmObj* %args48656$k40545$2)
ret void
}

define tailcc void @proc_clo$ae40880(%struct.ScmObj* %env$ae40880,%struct.ScmObj* %current_45args48645) {
%stackaddr$env-ref50460 = alloca %struct.ScmObj*, align 8
%_37take40122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40880, i64 0)
store %struct.ScmObj* %_37take40122, %struct.ScmObj** %stackaddr$env-ref50460
%stackaddr$prim50461 = alloca %struct.ScmObj*, align 8
%k40546 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48645)
store volatile %struct.ScmObj* %k40546, %struct.ScmObj** %stackaddr$prim50461, align 8
%stackaddr$prim50462 = alloca %struct.ScmObj*, align 8
%current_45args48646 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48645)
store volatile %struct.ScmObj* %current_45args48646, %struct.ScmObj** %stackaddr$prim50462, align 8
%stackaddr$prim50463 = alloca %struct.ScmObj*, align 8
%lst40124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48646)
store volatile %struct.ScmObj* %lst40124, %struct.ScmObj** %stackaddr$prim50463, align 8
%stackaddr$prim50464 = alloca %struct.ScmObj*, align 8
%current_45args48647 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48646)
store volatile %struct.ScmObj* %current_45args48647, %struct.ScmObj** %stackaddr$prim50464, align 8
%stackaddr$prim50465 = alloca %struct.ScmObj*, align 8
%n40123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48647)
store volatile %struct.ScmObj* %n40123, %struct.ScmObj** %stackaddr$prim50465, align 8
%ae40882 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50466 = alloca %struct.ScmObj*, align 8
%anf_45bind40258 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n40123, %struct.ScmObj* %ae40882)
store volatile %struct.ScmObj* %anf_45bind40258, %struct.ScmObj** %stackaddr$prim50466, align 8
%truthy$cmp50467 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40258)
%cmp$cmp50467 = icmp eq i64 %truthy$cmp50467, 1
br i1 %cmp$cmp50467, label %truebranch$cmp50467, label %falsebranch$cmp50467
truebranch$cmp50467:
%ae40885 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40886 = call %struct.ScmObj* @const_init_null()
%args48649$k40546$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50468 = alloca %struct.ScmObj*, align 8
%args48649$k40546$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40886, %struct.ScmObj* %args48649$k40546$0)
store volatile %struct.ScmObj* %args48649$k40546$1, %struct.ScmObj** %stackaddr$prim50468, align 8
%stackaddr$prim50469 = alloca %struct.ScmObj*, align 8
%args48649$k40546$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40885, %struct.ScmObj* %args48649$k40546$1)
store volatile %struct.ScmObj* %args48649$k40546$2, %struct.ScmObj** %stackaddr$prim50469, align 8
%clofunc50470 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40546)
musttail call tailcc void %clofunc50470(%struct.ScmObj* %k40546, %struct.ScmObj* %args48649$k40546$2)
ret void
falsebranch$cmp50467:
%stackaddr$prim50471 = alloca %struct.ScmObj*, align 8
%anf_45bind40259 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40124)
store volatile %struct.ScmObj* %anf_45bind40259, %struct.ScmObj** %stackaddr$prim50471, align 8
%truthy$cmp50472 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40259)
%cmp$cmp50472 = icmp eq i64 %truthy$cmp50472, 1
br i1 %cmp$cmp50472, label %truebranch$cmp50472, label %falsebranch$cmp50472
truebranch$cmp50472:
%ae40896 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40897 = call %struct.ScmObj* @const_init_null()
%args48650$k40546$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50473 = alloca %struct.ScmObj*, align 8
%args48650$k40546$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40897, %struct.ScmObj* %args48650$k40546$0)
store volatile %struct.ScmObj* %args48650$k40546$1, %struct.ScmObj** %stackaddr$prim50473, align 8
%stackaddr$prim50474 = alloca %struct.ScmObj*, align 8
%args48650$k40546$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40896, %struct.ScmObj* %args48650$k40546$1)
store volatile %struct.ScmObj* %args48650$k40546$2, %struct.ScmObj** %stackaddr$prim50474, align 8
%clofunc50475 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40546)
musttail call tailcc void %clofunc50475(%struct.ScmObj* %k40546, %struct.ScmObj* %args48650$k40546$2)
ret void
falsebranch$cmp50472:
%stackaddr$prim50476 = alloca %struct.ScmObj*, align 8
%anf_45bind40260 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40124)
store volatile %struct.ScmObj* %anf_45bind40260, %struct.ScmObj** %stackaddr$prim50476, align 8
%stackaddr$prim50477 = alloca %struct.ScmObj*, align 8
%anf_45bind40261 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40124)
store volatile %struct.ScmObj* %anf_45bind40261, %struct.ScmObj** %stackaddr$prim50477, align 8
%ae40907 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50478 = alloca %struct.ScmObj*, align 8
%anf_45bind40262 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n40123, %struct.ScmObj* %ae40907)
store volatile %struct.ScmObj* %anf_45bind40262, %struct.ScmObj** %stackaddr$prim50478, align 8
%stackaddr$makeclosure50479 = alloca %struct.ScmObj*, align 8
%fptrToInt50480 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40909 to i64
%ae40909 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50480)
store volatile %struct.ScmObj* %ae40909, %struct.ScmObj** %stackaddr$makeclosure50479, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40909, %struct.ScmObj* %anf_45bind40260, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40909, %struct.ScmObj* %k40546, i64 1)
%args48655$_37take40122$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50481 = alloca %struct.ScmObj*, align 8
%args48655$_37take40122$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40262, %struct.ScmObj* %args48655$_37take40122$0)
store volatile %struct.ScmObj* %args48655$_37take40122$1, %struct.ScmObj** %stackaddr$prim50481, align 8
%stackaddr$prim50482 = alloca %struct.ScmObj*, align 8
%args48655$_37take40122$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40261, %struct.ScmObj* %args48655$_37take40122$1)
store volatile %struct.ScmObj* %args48655$_37take40122$2, %struct.ScmObj** %stackaddr$prim50482, align 8
%stackaddr$prim50483 = alloca %struct.ScmObj*, align 8
%args48655$_37take40122$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40909, %struct.ScmObj* %args48655$_37take40122$2)
store volatile %struct.ScmObj* %args48655$_37take40122$3, %struct.ScmObj** %stackaddr$prim50483, align 8
%clofunc50484 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40122)
musttail call tailcc void %clofunc50484(%struct.ScmObj* %_37take40122, %struct.ScmObj* %args48655$_37take40122$3)
ret void
}

define tailcc void @proc_clo$ae40909(%struct.ScmObj* %env$ae40909,%struct.ScmObj* %current_45args48651) {
%stackaddr$env-ref50485 = alloca %struct.ScmObj*, align 8
%anf_45bind40260 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40909, i64 0)
store %struct.ScmObj* %anf_45bind40260, %struct.ScmObj** %stackaddr$env-ref50485
%stackaddr$env-ref50486 = alloca %struct.ScmObj*, align 8
%k40546 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40909, i64 1)
store %struct.ScmObj* %k40546, %struct.ScmObj** %stackaddr$env-ref50486
%stackaddr$prim50487 = alloca %struct.ScmObj*, align 8
%_95k40547 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48651)
store volatile %struct.ScmObj* %_95k40547, %struct.ScmObj** %stackaddr$prim50487, align 8
%stackaddr$prim50488 = alloca %struct.ScmObj*, align 8
%current_45args48652 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48651)
store volatile %struct.ScmObj* %current_45args48652, %struct.ScmObj** %stackaddr$prim50488, align 8
%stackaddr$prim50489 = alloca %struct.ScmObj*, align 8
%anf_45bind40263 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48652)
store volatile %struct.ScmObj* %anf_45bind40263, %struct.ScmObj** %stackaddr$prim50489, align 8
%stackaddr$prim50490 = alloca %struct.ScmObj*, align 8
%cpsprim40548 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40260, %struct.ScmObj* %anf_45bind40263)
store volatile %struct.ScmObj* %cpsprim40548, %struct.ScmObj** %stackaddr$prim50490, align 8
%ae40915 = call %struct.ScmObj* @const_init_int(i64 0)
%args48654$k40546$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50491 = alloca %struct.ScmObj*, align 8
%args48654$k40546$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40548, %struct.ScmObj* %args48654$k40546$0)
store volatile %struct.ScmObj* %args48654$k40546$1, %struct.ScmObj** %stackaddr$prim50491, align 8
%stackaddr$prim50492 = alloca %struct.ScmObj*, align 8
%args48654$k40546$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40915, %struct.ScmObj* %args48654$k40546$1)
store volatile %struct.ScmObj* %args48654$k40546$2, %struct.ScmObj** %stackaddr$prim50492, align 8
%clofunc50493 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40546)
musttail call tailcc void %clofunc50493(%struct.ScmObj* %k40546, %struct.ScmObj* %args48654$k40546$2)
ret void
}

define tailcc void @proc_clo$ae40780(%struct.ScmObj* %env$ae40780,%struct.ScmObj* %current_45args48659) {
%stackaddr$prim50494 = alloca %struct.ScmObj*, align 8
%k40549 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48659)
store volatile %struct.ScmObj* %k40549, %struct.ScmObj** %stackaddr$prim50494, align 8
%stackaddr$prim50495 = alloca %struct.ScmObj*, align 8
%current_45args48660 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48659)
store volatile %struct.ScmObj* %current_45args48660, %struct.ScmObj** %stackaddr$prim50495, align 8
%stackaddr$prim50496 = alloca %struct.ScmObj*, align 8
%_37map40126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48660)
store volatile %struct.ScmObj* %_37map40126, %struct.ScmObj** %stackaddr$prim50496, align 8
%ae40782 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50497 = alloca %struct.ScmObj*, align 8
%fptrToInt50498 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40783 to i64
%ae40783 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50498)
store volatile %struct.ScmObj* %ae40783, %struct.ScmObj** %stackaddr$makeclosure50497, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40783, %struct.ScmObj* %_37map40126, i64 0)
%args48676$k40549$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50499 = alloca %struct.ScmObj*, align 8
%args48676$k40549$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40783, %struct.ScmObj* %args48676$k40549$0)
store volatile %struct.ScmObj* %args48676$k40549$1, %struct.ScmObj** %stackaddr$prim50499, align 8
%stackaddr$prim50500 = alloca %struct.ScmObj*, align 8
%args48676$k40549$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40782, %struct.ScmObj* %args48676$k40549$1)
store volatile %struct.ScmObj* %args48676$k40549$2, %struct.ScmObj** %stackaddr$prim50500, align 8
%clofunc50501 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40549)
musttail call tailcc void %clofunc50501(%struct.ScmObj* %k40549, %struct.ScmObj* %args48676$k40549$2)
ret void
}

define tailcc void @proc_clo$ae40783(%struct.ScmObj* %env$ae40783,%struct.ScmObj* %current_45args48662) {
%stackaddr$env-ref50502 = alloca %struct.ScmObj*, align 8
%_37map40126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40783, i64 0)
store %struct.ScmObj* %_37map40126, %struct.ScmObj** %stackaddr$env-ref50502
%stackaddr$prim50503 = alloca %struct.ScmObj*, align 8
%k40550 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48662)
store volatile %struct.ScmObj* %k40550, %struct.ScmObj** %stackaddr$prim50503, align 8
%stackaddr$prim50504 = alloca %struct.ScmObj*, align 8
%current_45args48663 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48662)
store volatile %struct.ScmObj* %current_45args48663, %struct.ScmObj** %stackaddr$prim50504, align 8
%stackaddr$prim50505 = alloca %struct.ScmObj*, align 8
%f40128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48663)
store volatile %struct.ScmObj* %f40128, %struct.ScmObj** %stackaddr$prim50505, align 8
%stackaddr$prim50506 = alloca %struct.ScmObj*, align 8
%current_45args48664 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48663)
store volatile %struct.ScmObj* %current_45args48664, %struct.ScmObj** %stackaddr$prim50506, align 8
%stackaddr$prim50507 = alloca %struct.ScmObj*, align 8
%lst40127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48664)
store volatile %struct.ScmObj* %lst40127, %struct.ScmObj** %stackaddr$prim50507, align 8
%stackaddr$prim50508 = alloca %struct.ScmObj*, align 8
%anf_45bind40252 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40127)
store volatile %struct.ScmObj* %anf_45bind40252, %struct.ScmObj** %stackaddr$prim50508, align 8
%truthy$cmp50509 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40252)
%cmp$cmp50509 = icmp eq i64 %truthy$cmp50509, 1
br i1 %cmp$cmp50509, label %truebranch$cmp50509, label %falsebranch$cmp50509
truebranch$cmp50509:
%ae40787 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40788 = call %struct.ScmObj* @const_init_null()
%args48666$k40550$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50510 = alloca %struct.ScmObj*, align 8
%args48666$k40550$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40788, %struct.ScmObj* %args48666$k40550$0)
store volatile %struct.ScmObj* %args48666$k40550$1, %struct.ScmObj** %stackaddr$prim50510, align 8
%stackaddr$prim50511 = alloca %struct.ScmObj*, align 8
%args48666$k40550$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40787, %struct.ScmObj* %args48666$k40550$1)
store volatile %struct.ScmObj* %args48666$k40550$2, %struct.ScmObj** %stackaddr$prim50511, align 8
%clofunc50512 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40550)
musttail call tailcc void %clofunc50512(%struct.ScmObj* %k40550, %struct.ScmObj* %args48666$k40550$2)
ret void
falsebranch$cmp50509:
%stackaddr$prim50513 = alloca %struct.ScmObj*, align 8
%anf_45bind40253 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40127)
store volatile %struct.ScmObj* %anf_45bind40253, %struct.ScmObj** %stackaddr$prim50513, align 8
%stackaddr$makeclosure50514 = alloca %struct.ScmObj*, align 8
%fptrToInt50515 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40797 to i64
%ae40797 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50515)
store volatile %struct.ScmObj* %ae40797, %struct.ScmObj** %stackaddr$makeclosure50514, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40797, %struct.ScmObj* %k40550, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40797, %struct.ScmObj* %f40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40797, %struct.ScmObj* %lst40127, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae40797, %struct.ScmObj* %_37map40126, i64 3)
%args48675$f40128$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50516 = alloca %struct.ScmObj*, align 8
%args48675$f40128$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40253, %struct.ScmObj* %args48675$f40128$0)
store volatile %struct.ScmObj* %args48675$f40128$1, %struct.ScmObj** %stackaddr$prim50516, align 8
%stackaddr$prim50517 = alloca %struct.ScmObj*, align 8
%args48675$f40128$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40797, %struct.ScmObj* %args48675$f40128$1)
store volatile %struct.ScmObj* %args48675$f40128$2, %struct.ScmObj** %stackaddr$prim50517, align 8
%clofunc50518 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40128)
musttail call tailcc void %clofunc50518(%struct.ScmObj* %f40128, %struct.ScmObj* %args48675$f40128$2)
ret void
}

define tailcc void @proc_clo$ae40797(%struct.ScmObj* %env$ae40797,%struct.ScmObj* %current_45args48667) {
%stackaddr$env-ref50519 = alloca %struct.ScmObj*, align 8
%k40550 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40797, i64 0)
store %struct.ScmObj* %k40550, %struct.ScmObj** %stackaddr$env-ref50519
%stackaddr$env-ref50520 = alloca %struct.ScmObj*, align 8
%f40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40797, i64 1)
store %struct.ScmObj* %f40128, %struct.ScmObj** %stackaddr$env-ref50520
%stackaddr$env-ref50521 = alloca %struct.ScmObj*, align 8
%lst40127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40797, i64 2)
store %struct.ScmObj* %lst40127, %struct.ScmObj** %stackaddr$env-ref50521
%stackaddr$env-ref50522 = alloca %struct.ScmObj*, align 8
%_37map40126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40797, i64 3)
store %struct.ScmObj* %_37map40126, %struct.ScmObj** %stackaddr$env-ref50522
%stackaddr$prim50523 = alloca %struct.ScmObj*, align 8
%_95k40551 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48667)
store volatile %struct.ScmObj* %_95k40551, %struct.ScmObj** %stackaddr$prim50523, align 8
%stackaddr$prim50524 = alloca %struct.ScmObj*, align 8
%current_45args48668 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48667)
store volatile %struct.ScmObj* %current_45args48668, %struct.ScmObj** %stackaddr$prim50524, align 8
%stackaddr$prim50525 = alloca %struct.ScmObj*, align 8
%anf_45bind40254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48668)
store volatile %struct.ScmObj* %anf_45bind40254, %struct.ScmObj** %stackaddr$prim50525, align 8
%stackaddr$prim50526 = alloca %struct.ScmObj*, align 8
%anf_45bind40255 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40127)
store volatile %struct.ScmObj* %anf_45bind40255, %struct.ScmObj** %stackaddr$prim50526, align 8
%stackaddr$makeclosure50527 = alloca %struct.ScmObj*, align 8
%fptrToInt50528 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40801 to i64
%ae40801 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50528)
store volatile %struct.ScmObj* %ae40801, %struct.ScmObj** %stackaddr$makeclosure50527, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40801, %struct.ScmObj* %k40550, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40801, %struct.ScmObj* %anf_45bind40254, i64 1)
%args48674$_37map40126$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50529 = alloca %struct.ScmObj*, align 8
%args48674$_37map40126$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40255, %struct.ScmObj* %args48674$_37map40126$0)
store volatile %struct.ScmObj* %args48674$_37map40126$1, %struct.ScmObj** %stackaddr$prim50529, align 8
%stackaddr$prim50530 = alloca %struct.ScmObj*, align 8
%args48674$_37map40126$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40128, %struct.ScmObj* %args48674$_37map40126$1)
store volatile %struct.ScmObj* %args48674$_37map40126$2, %struct.ScmObj** %stackaddr$prim50530, align 8
%stackaddr$prim50531 = alloca %struct.ScmObj*, align 8
%args48674$_37map40126$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40801, %struct.ScmObj* %args48674$_37map40126$2)
store volatile %struct.ScmObj* %args48674$_37map40126$3, %struct.ScmObj** %stackaddr$prim50531, align 8
%clofunc50532 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map40126)
musttail call tailcc void %clofunc50532(%struct.ScmObj* %_37map40126, %struct.ScmObj* %args48674$_37map40126$3)
ret void
}

define tailcc void @proc_clo$ae40801(%struct.ScmObj* %env$ae40801,%struct.ScmObj* %current_45args48670) {
%stackaddr$env-ref50533 = alloca %struct.ScmObj*, align 8
%k40550 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40801, i64 0)
store %struct.ScmObj* %k40550, %struct.ScmObj** %stackaddr$env-ref50533
%stackaddr$env-ref50534 = alloca %struct.ScmObj*, align 8
%anf_45bind40254 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40801, i64 1)
store %struct.ScmObj* %anf_45bind40254, %struct.ScmObj** %stackaddr$env-ref50534
%stackaddr$prim50535 = alloca %struct.ScmObj*, align 8
%_95k40552 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48670)
store volatile %struct.ScmObj* %_95k40552, %struct.ScmObj** %stackaddr$prim50535, align 8
%stackaddr$prim50536 = alloca %struct.ScmObj*, align 8
%current_45args48671 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48670)
store volatile %struct.ScmObj* %current_45args48671, %struct.ScmObj** %stackaddr$prim50536, align 8
%stackaddr$prim50537 = alloca %struct.ScmObj*, align 8
%anf_45bind40256 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48671)
store volatile %struct.ScmObj* %anf_45bind40256, %struct.ScmObj** %stackaddr$prim50537, align 8
%stackaddr$prim50538 = alloca %struct.ScmObj*, align 8
%cpsprim40553 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40254, %struct.ScmObj* %anf_45bind40256)
store volatile %struct.ScmObj* %cpsprim40553, %struct.ScmObj** %stackaddr$prim50538, align 8
%ae40807 = call %struct.ScmObj* @const_init_int(i64 0)
%args48673$k40550$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50539 = alloca %struct.ScmObj*, align 8
%args48673$k40550$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40553, %struct.ScmObj* %args48673$k40550$0)
store volatile %struct.ScmObj* %args48673$k40550$1, %struct.ScmObj** %stackaddr$prim50539, align 8
%stackaddr$prim50540 = alloca %struct.ScmObj*, align 8
%args48673$k40550$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40807, %struct.ScmObj* %args48673$k40550$1)
store volatile %struct.ScmObj* %args48673$k40550$2, %struct.ScmObj** %stackaddr$prim50540, align 8
%clofunc50541 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40550)
musttail call tailcc void %clofunc50541(%struct.ScmObj* %k40550, %struct.ScmObj* %args48673$k40550$2)
ret void
}

define tailcc void @proc_clo$ae40700(%struct.ScmObj* %env$ae40700,%struct.ScmObj* %current_45args48679) {
%stackaddr$prim50542 = alloca %struct.ScmObj*, align 8
%k40554 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48679)
store volatile %struct.ScmObj* %k40554, %struct.ScmObj** %stackaddr$prim50542, align 8
%stackaddr$prim50543 = alloca %struct.ScmObj*, align 8
%current_45args48680 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48679)
store volatile %struct.ScmObj* %current_45args48680, %struct.ScmObj** %stackaddr$prim50543, align 8
%stackaddr$prim50544 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48680)
store volatile %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$prim50544, align 8
%ae40702 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50545 = alloca %struct.ScmObj*, align 8
%fptrToInt50546 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40703 to i64
%ae40703 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50546)
store volatile %struct.ScmObj* %ae40703, %struct.ScmObj** %stackaddr$makeclosure50545, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40703, %struct.ScmObj* %_37foldr140130, i64 0)
%args48693$k40554$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50547 = alloca %struct.ScmObj*, align 8
%args48693$k40554$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40703, %struct.ScmObj* %args48693$k40554$0)
store volatile %struct.ScmObj* %args48693$k40554$1, %struct.ScmObj** %stackaddr$prim50547, align 8
%stackaddr$prim50548 = alloca %struct.ScmObj*, align 8
%args48693$k40554$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40702, %struct.ScmObj* %args48693$k40554$1)
store volatile %struct.ScmObj* %args48693$k40554$2, %struct.ScmObj** %stackaddr$prim50548, align 8
%clofunc50549 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40554)
musttail call tailcc void %clofunc50549(%struct.ScmObj* %k40554, %struct.ScmObj* %args48693$k40554$2)
ret void
}

define tailcc void @proc_clo$ae40703(%struct.ScmObj* %env$ae40703,%struct.ScmObj* %current_45args48682) {
%stackaddr$env-ref50550 = alloca %struct.ScmObj*, align 8
%_37foldr140130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40703, i64 0)
store %struct.ScmObj* %_37foldr140130, %struct.ScmObj** %stackaddr$env-ref50550
%stackaddr$prim50551 = alloca %struct.ScmObj*, align 8
%k40555 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48682)
store volatile %struct.ScmObj* %k40555, %struct.ScmObj** %stackaddr$prim50551, align 8
%stackaddr$prim50552 = alloca %struct.ScmObj*, align 8
%current_45args48683 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48682)
store volatile %struct.ScmObj* %current_45args48683, %struct.ScmObj** %stackaddr$prim50552, align 8
%stackaddr$prim50553 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48683)
store volatile %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$prim50553, align 8
%stackaddr$prim50554 = alloca %struct.ScmObj*, align 8
%current_45args48684 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48683)
store volatile %struct.ScmObj* %current_45args48684, %struct.ScmObj** %stackaddr$prim50554, align 8
%stackaddr$prim50555 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48684)
store volatile %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$prim50555, align 8
%stackaddr$prim50556 = alloca %struct.ScmObj*, align 8
%current_45args48685 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48684)
store volatile %struct.ScmObj* %current_45args48685, %struct.ScmObj** %stackaddr$prim50556, align 8
%stackaddr$prim50557 = alloca %struct.ScmObj*, align 8
%lst40131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48685)
store volatile %struct.ScmObj* %lst40131, %struct.ScmObj** %stackaddr$prim50557, align 8
%stackaddr$prim50558 = alloca %struct.ScmObj*, align 8
%anf_45bind40247 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40131)
store volatile %struct.ScmObj* %anf_45bind40247, %struct.ScmObj** %stackaddr$prim50558, align 8
%truthy$cmp50559 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40247)
%cmp$cmp50559 = icmp eq i64 %truthy$cmp50559, 1
br i1 %cmp$cmp50559, label %truebranch$cmp50559, label %falsebranch$cmp50559
truebranch$cmp50559:
%ae40707 = call %struct.ScmObj* @const_init_int(i64 0)
%args48687$k40555$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50560 = alloca %struct.ScmObj*, align 8
%args48687$k40555$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40132, %struct.ScmObj* %args48687$k40555$0)
store volatile %struct.ScmObj* %args48687$k40555$1, %struct.ScmObj** %stackaddr$prim50560, align 8
%stackaddr$prim50561 = alloca %struct.ScmObj*, align 8
%args48687$k40555$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40707, %struct.ScmObj* %args48687$k40555$1)
store volatile %struct.ScmObj* %args48687$k40555$2, %struct.ScmObj** %stackaddr$prim50561, align 8
%clofunc50562 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40555)
musttail call tailcc void %clofunc50562(%struct.ScmObj* %k40555, %struct.ScmObj* %args48687$k40555$2)
ret void
falsebranch$cmp50559:
%stackaddr$prim50563 = alloca %struct.ScmObj*, align 8
%anf_45bind40248 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40131)
store volatile %struct.ScmObj* %anf_45bind40248, %struct.ScmObj** %stackaddr$prim50563, align 8
%stackaddr$prim50564 = alloca %struct.ScmObj*, align 8
%anf_45bind40249 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40131)
store volatile %struct.ScmObj* %anf_45bind40249, %struct.ScmObj** %stackaddr$prim50564, align 8
%stackaddr$makeclosure50565 = alloca %struct.ScmObj*, align 8
%fptrToInt50566 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40715 to i64
%ae40715 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50566)
store volatile %struct.ScmObj* %ae40715, %struct.ScmObj** %stackaddr$makeclosure50565, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40715, %struct.ScmObj* %anf_45bind40248, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40715, %struct.ScmObj* %f40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40715, %struct.ScmObj* %k40555, i64 2)
%args48692$_37foldr140130$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50567 = alloca %struct.ScmObj*, align 8
%args48692$_37foldr140130$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40249, %struct.ScmObj* %args48692$_37foldr140130$0)
store volatile %struct.ScmObj* %args48692$_37foldr140130$1, %struct.ScmObj** %stackaddr$prim50567, align 8
%stackaddr$prim50568 = alloca %struct.ScmObj*, align 8
%args48692$_37foldr140130$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40132, %struct.ScmObj* %args48692$_37foldr140130$1)
store volatile %struct.ScmObj* %args48692$_37foldr140130$2, %struct.ScmObj** %stackaddr$prim50568, align 8
%stackaddr$prim50569 = alloca %struct.ScmObj*, align 8
%args48692$_37foldr140130$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40133, %struct.ScmObj* %args48692$_37foldr140130$2)
store volatile %struct.ScmObj* %args48692$_37foldr140130$3, %struct.ScmObj** %stackaddr$prim50569, align 8
%stackaddr$prim50570 = alloca %struct.ScmObj*, align 8
%args48692$_37foldr140130$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40715, %struct.ScmObj* %args48692$_37foldr140130$3)
store volatile %struct.ScmObj* %args48692$_37foldr140130$4, %struct.ScmObj** %stackaddr$prim50570, align 8
%clofunc50571 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140130)
musttail call tailcc void %clofunc50571(%struct.ScmObj* %_37foldr140130, %struct.ScmObj* %args48692$_37foldr140130$4)
ret void
}

define tailcc void @proc_clo$ae40715(%struct.ScmObj* %env$ae40715,%struct.ScmObj* %current_45args48688) {
%stackaddr$env-ref50572 = alloca %struct.ScmObj*, align 8
%anf_45bind40248 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40715, i64 0)
store %struct.ScmObj* %anf_45bind40248, %struct.ScmObj** %stackaddr$env-ref50572
%stackaddr$env-ref50573 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40715, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref50573
%stackaddr$env-ref50574 = alloca %struct.ScmObj*, align 8
%k40555 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40715, i64 2)
store %struct.ScmObj* %k40555, %struct.ScmObj** %stackaddr$env-ref50574
%stackaddr$prim50575 = alloca %struct.ScmObj*, align 8
%_95k40556 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48688)
store volatile %struct.ScmObj* %_95k40556, %struct.ScmObj** %stackaddr$prim50575, align 8
%stackaddr$prim50576 = alloca %struct.ScmObj*, align 8
%current_45args48689 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48688)
store volatile %struct.ScmObj* %current_45args48689, %struct.ScmObj** %stackaddr$prim50576, align 8
%stackaddr$prim50577 = alloca %struct.ScmObj*, align 8
%anf_45bind40250 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48689)
store volatile %struct.ScmObj* %anf_45bind40250, %struct.ScmObj** %stackaddr$prim50577, align 8
%args48691$f40133$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50578 = alloca %struct.ScmObj*, align 8
%args48691$f40133$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40250, %struct.ScmObj* %args48691$f40133$0)
store volatile %struct.ScmObj* %args48691$f40133$1, %struct.ScmObj** %stackaddr$prim50578, align 8
%stackaddr$prim50579 = alloca %struct.ScmObj*, align 8
%args48691$f40133$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40248, %struct.ScmObj* %args48691$f40133$1)
store volatile %struct.ScmObj* %args48691$f40133$2, %struct.ScmObj** %stackaddr$prim50579, align 8
%stackaddr$prim50580 = alloca %struct.ScmObj*, align 8
%args48691$f40133$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40555, %struct.ScmObj* %args48691$f40133$2)
store volatile %struct.ScmObj* %args48691$f40133$3, %struct.ScmObj** %stackaddr$prim50580, align 8
%clofunc50581 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40133)
musttail call tailcc void %clofunc50581(%struct.ScmObj* %f40133, %struct.ScmObj* %args48691$f40133$3)
ret void
}

define tailcc void @proc_clo$ae40583(%struct.ScmObj* %env$ae40583,%struct.ScmObj* %current_45args48696) {
%stackaddr$prim50582 = alloca %struct.ScmObj*, align 8
%k40557 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48696)
store volatile %struct.ScmObj* %k40557, %struct.ScmObj** %stackaddr$prim50582, align 8
%stackaddr$prim50583 = alloca %struct.ScmObj*, align 8
%current_45args48697 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48696)
store volatile %struct.ScmObj* %current_45args48697, %struct.ScmObj** %stackaddr$prim50583, align 8
%stackaddr$prim50584 = alloca %struct.ScmObj*, align 8
%y40110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48697)
store volatile %struct.ScmObj* %y40110, %struct.ScmObj** %stackaddr$prim50584, align 8
%ae40585 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50585 = alloca %struct.ScmObj*, align 8
%fptrToInt50586 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40586 to i64
%ae40586 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50586)
store volatile %struct.ScmObj* %ae40586, %struct.ScmObj** %stackaddr$makeclosure50585, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40586, %struct.ScmObj* %y40110, i64 0)
%args48715$k40557$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50587 = alloca %struct.ScmObj*, align 8
%args48715$k40557$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40586, %struct.ScmObj* %args48715$k40557$0)
store volatile %struct.ScmObj* %args48715$k40557$1, %struct.ScmObj** %stackaddr$prim50587, align 8
%stackaddr$prim50588 = alloca %struct.ScmObj*, align 8
%args48715$k40557$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40585, %struct.ScmObj* %args48715$k40557$1)
store volatile %struct.ScmObj* %args48715$k40557$2, %struct.ScmObj** %stackaddr$prim50588, align 8
%clofunc50589 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40557)
musttail call tailcc void %clofunc50589(%struct.ScmObj* %k40557, %struct.ScmObj* %args48715$k40557$2)
ret void
}

define tailcc void @proc_clo$ae40586(%struct.ScmObj* %env$ae40586,%struct.ScmObj* %current_45args48699) {
%stackaddr$env-ref50590 = alloca %struct.ScmObj*, align 8
%y40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40586, i64 0)
store %struct.ScmObj* %y40110, %struct.ScmObj** %stackaddr$env-ref50590
%stackaddr$prim50591 = alloca %struct.ScmObj*, align 8
%k40558 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48699)
store volatile %struct.ScmObj* %k40558, %struct.ScmObj** %stackaddr$prim50591, align 8
%stackaddr$prim50592 = alloca %struct.ScmObj*, align 8
%current_45args48700 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48699)
store volatile %struct.ScmObj* %current_45args48700, %struct.ScmObj** %stackaddr$prim50592, align 8
%stackaddr$prim50593 = alloca %struct.ScmObj*, align 8
%f40111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48700)
store volatile %struct.ScmObj* %f40111, %struct.ScmObj** %stackaddr$prim50593, align 8
%stackaddr$makeclosure50594 = alloca %struct.ScmObj*, align 8
%fptrToInt50595 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40587 to i64
%ae40587 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50595)
store volatile %struct.ScmObj* %ae40587, %struct.ScmObj** %stackaddr$makeclosure50594, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40587, %struct.ScmObj* %f40111, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40587, %struct.ScmObj* %k40558, i64 1)
%ae40588 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50596 = alloca %struct.ScmObj*, align 8
%fptrToInt50597 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40589 to i64
%ae40589 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50597)
store volatile %struct.ScmObj* %ae40589, %struct.ScmObj** %stackaddr$makeclosure50596, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40589, %struct.ScmObj* %f40111, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40589, %struct.ScmObj* %y40110, i64 1)
%args48714$ae40587$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50598 = alloca %struct.ScmObj*, align 8
%args48714$ae40587$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40589, %struct.ScmObj* %args48714$ae40587$0)
store volatile %struct.ScmObj* %args48714$ae40587$1, %struct.ScmObj** %stackaddr$prim50598, align 8
%stackaddr$prim50599 = alloca %struct.ScmObj*, align 8
%args48714$ae40587$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40588, %struct.ScmObj* %args48714$ae40587$1)
store volatile %struct.ScmObj* %args48714$ae40587$2, %struct.ScmObj** %stackaddr$prim50599, align 8
%clofunc50600 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40587)
musttail call tailcc void %clofunc50600(%struct.ScmObj* %ae40587, %struct.ScmObj* %args48714$ae40587$2)
ret void
}

define tailcc void @proc_clo$ae40587(%struct.ScmObj* %env$ae40587,%struct.ScmObj* %current_45args48702) {
%stackaddr$env-ref50601 = alloca %struct.ScmObj*, align 8
%f40111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40587, i64 0)
store %struct.ScmObj* %f40111, %struct.ScmObj** %stackaddr$env-ref50601
%stackaddr$env-ref50602 = alloca %struct.ScmObj*, align 8
%k40558 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40587, i64 1)
store %struct.ScmObj* %k40558, %struct.ScmObj** %stackaddr$env-ref50602
%stackaddr$prim50603 = alloca %struct.ScmObj*, align 8
%_95k40559 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48702)
store volatile %struct.ScmObj* %_95k40559, %struct.ScmObj** %stackaddr$prim50603, align 8
%stackaddr$prim50604 = alloca %struct.ScmObj*, align 8
%current_45args48703 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48702)
store volatile %struct.ScmObj* %current_45args48703, %struct.ScmObj** %stackaddr$prim50604, align 8
%stackaddr$prim50605 = alloca %struct.ScmObj*, align 8
%anf_45bind40245 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48703)
store volatile %struct.ScmObj* %anf_45bind40245, %struct.ScmObj** %stackaddr$prim50605, align 8
%args48705$f40111$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50606 = alloca %struct.ScmObj*, align 8
%args48705$f40111$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40245, %struct.ScmObj* %args48705$f40111$0)
store volatile %struct.ScmObj* %args48705$f40111$1, %struct.ScmObj** %stackaddr$prim50606, align 8
%stackaddr$prim50607 = alloca %struct.ScmObj*, align 8
%args48705$f40111$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40558, %struct.ScmObj* %args48705$f40111$1)
store volatile %struct.ScmObj* %args48705$f40111$2, %struct.ScmObj** %stackaddr$prim50607, align 8
%clofunc50608 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40111)
musttail call tailcc void %clofunc50608(%struct.ScmObj* %f40111, %struct.ScmObj* %args48705$f40111$2)
ret void
}

define tailcc void @proc_clo$ae40589(%struct.ScmObj* %env$ae40589,%struct.ScmObj* %args4011240560) {
%stackaddr$env-ref50609 = alloca %struct.ScmObj*, align 8
%f40111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40589, i64 0)
store %struct.ScmObj* %f40111, %struct.ScmObj** %stackaddr$env-ref50609
%stackaddr$env-ref50610 = alloca %struct.ScmObj*, align 8
%y40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40589, i64 1)
store %struct.ScmObj* %y40110, %struct.ScmObj** %stackaddr$env-ref50610
%stackaddr$prim50611 = alloca %struct.ScmObj*, align 8
%k40561 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4011240560)
store volatile %struct.ScmObj* %k40561, %struct.ScmObj** %stackaddr$prim50611, align 8
%stackaddr$prim50612 = alloca %struct.ScmObj*, align 8
%args40112 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4011240560)
store volatile %struct.ScmObj* %args40112, %struct.ScmObj** %stackaddr$prim50612, align 8
%stackaddr$makeclosure50613 = alloca %struct.ScmObj*, align 8
%fptrToInt50614 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40593 to i64
%ae40593 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50614)
store volatile %struct.ScmObj* %ae40593, %struct.ScmObj** %stackaddr$makeclosure50613, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40593, %struct.ScmObj* %k40561, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40593, %struct.ScmObj* %args40112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40593, %struct.ScmObj* %f40111, i64 2)
%args48713$y40110$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50615 = alloca %struct.ScmObj*, align 8
%args48713$y40110$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y40110, %struct.ScmObj* %args48713$y40110$0)
store volatile %struct.ScmObj* %args48713$y40110$1, %struct.ScmObj** %stackaddr$prim50615, align 8
%stackaddr$prim50616 = alloca %struct.ScmObj*, align 8
%args48713$y40110$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40593, %struct.ScmObj* %args48713$y40110$1)
store volatile %struct.ScmObj* %args48713$y40110$2, %struct.ScmObj** %stackaddr$prim50616, align 8
%clofunc50617 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y40110)
musttail call tailcc void %clofunc50617(%struct.ScmObj* %y40110, %struct.ScmObj* %args48713$y40110$2)
ret void
}

define tailcc void @proc_clo$ae40593(%struct.ScmObj* %env$ae40593,%struct.ScmObj* %current_45args48706) {
%stackaddr$env-ref50618 = alloca %struct.ScmObj*, align 8
%k40561 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40593, i64 0)
store %struct.ScmObj* %k40561, %struct.ScmObj** %stackaddr$env-ref50618
%stackaddr$env-ref50619 = alloca %struct.ScmObj*, align 8
%args40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40593, i64 1)
store %struct.ScmObj* %args40112, %struct.ScmObj** %stackaddr$env-ref50619
%stackaddr$env-ref50620 = alloca %struct.ScmObj*, align 8
%f40111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40593, i64 2)
store %struct.ScmObj* %f40111, %struct.ScmObj** %stackaddr$env-ref50620
%stackaddr$prim50621 = alloca %struct.ScmObj*, align 8
%_95k40562 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48706)
store volatile %struct.ScmObj* %_95k40562, %struct.ScmObj** %stackaddr$prim50621, align 8
%stackaddr$prim50622 = alloca %struct.ScmObj*, align 8
%current_45args48707 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48706)
store volatile %struct.ScmObj* %current_45args48707, %struct.ScmObj** %stackaddr$prim50622, align 8
%stackaddr$prim50623 = alloca %struct.ScmObj*, align 8
%anf_45bind40243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48707)
store volatile %struct.ScmObj* %anf_45bind40243, %struct.ScmObj** %stackaddr$prim50623, align 8
%stackaddr$makeclosure50624 = alloca %struct.ScmObj*, align 8
%fptrToInt50625 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40596 to i64
%ae40596 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50625)
store volatile %struct.ScmObj* %ae40596, %struct.ScmObj** %stackaddr$makeclosure50624, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40596, %struct.ScmObj* %k40561, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40596, %struct.ScmObj* %args40112, i64 1)
%args48712$anf_45bind40243$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50626 = alloca %struct.ScmObj*, align 8
%args48712$anf_45bind40243$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40111, %struct.ScmObj* %args48712$anf_45bind40243$0)
store volatile %struct.ScmObj* %args48712$anf_45bind40243$1, %struct.ScmObj** %stackaddr$prim50626, align 8
%stackaddr$prim50627 = alloca %struct.ScmObj*, align 8
%args48712$anf_45bind40243$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40596, %struct.ScmObj* %args48712$anf_45bind40243$1)
store volatile %struct.ScmObj* %args48712$anf_45bind40243$2, %struct.ScmObj** %stackaddr$prim50627, align 8
%clofunc50628 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40243)
musttail call tailcc void %clofunc50628(%struct.ScmObj* %anf_45bind40243, %struct.ScmObj* %args48712$anf_45bind40243$2)
ret void
}

define tailcc void @proc_clo$ae40596(%struct.ScmObj* %env$ae40596,%struct.ScmObj* %current_45args48709) {
%stackaddr$env-ref50629 = alloca %struct.ScmObj*, align 8
%k40561 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40596, i64 0)
store %struct.ScmObj* %k40561, %struct.ScmObj** %stackaddr$env-ref50629
%stackaddr$env-ref50630 = alloca %struct.ScmObj*, align 8
%args40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40596, i64 1)
store %struct.ScmObj* %args40112, %struct.ScmObj** %stackaddr$env-ref50630
%stackaddr$prim50631 = alloca %struct.ScmObj*, align 8
%_95k40563 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48709)
store volatile %struct.ScmObj* %_95k40563, %struct.ScmObj** %stackaddr$prim50631, align 8
%stackaddr$prim50632 = alloca %struct.ScmObj*, align 8
%current_45args48710 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48709)
store volatile %struct.ScmObj* %current_45args48710, %struct.ScmObj** %stackaddr$prim50632, align 8
%stackaddr$prim50633 = alloca %struct.ScmObj*, align 8
%anf_45bind40244 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48710)
store volatile %struct.ScmObj* %anf_45bind40244, %struct.ScmObj** %stackaddr$prim50633, align 8
%stackaddr$prim50634 = alloca %struct.ScmObj*, align 8
%cpsargs40564 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40561, %struct.ScmObj* %args40112)
store volatile %struct.ScmObj* %cpsargs40564, %struct.ScmObj** %stackaddr$prim50634, align 8
%clofunc50635 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40244)
musttail call tailcc void %clofunc50635(%struct.ScmObj* %anf_45bind40244, %struct.ScmObj* %cpsargs40564)
ret void
}

define tailcc void @proc_clo$ae40568(%struct.ScmObj* %env$ae40568,%struct.ScmObj* %current_45args48717) {
%stackaddr$prim50636 = alloca %struct.ScmObj*, align 8
%k40565 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48717)
store volatile %struct.ScmObj* %k40565, %struct.ScmObj** %stackaddr$prim50636, align 8
%stackaddr$prim50637 = alloca %struct.ScmObj*, align 8
%current_45args48718 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48717)
store volatile %struct.ScmObj* %current_45args48718, %struct.ScmObj** %stackaddr$prim50637, align 8
%stackaddr$prim50638 = alloca %struct.ScmObj*, align 8
%yu40109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48718)
store volatile %struct.ScmObj* %yu40109, %struct.ScmObj** %stackaddr$prim50638, align 8
%args48720$yu40109$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50639 = alloca %struct.ScmObj*, align 8
%args48720$yu40109$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu40109, %struct.ScmObj* %args48720$yu40109$0)
store volatile %struct.ScmObj* %args48720$yu40109$1, %struct.ScmObj** %stackaddr$prim50639, align 8
%stackaddr$prim50640 = alloca %struct.ScmObj*, align 8
%args48720$yu40109$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40565, %struct.ScmObj* %args48720$yu40109$1)
store volatile %struct.ScmObj* %args48720$yu40109$2, %struct.ScmObj** %stackaddr$prim50640, align 8
%clofunc50641 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu40109)
musttail call tailcc void %clofunc50641(%struct.ScmObj* %yu40109, %struct.ScmObj* %args48720$yu40109$2)
ret void
}