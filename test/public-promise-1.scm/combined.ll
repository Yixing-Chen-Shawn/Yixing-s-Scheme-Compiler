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

@global$sym$ae5221557306 = private unnamed_addr constant [4 x i8] c"get\00", align 8
@global$str$ae5246657319 = private unnamed_addr constant [12 x i8] c"not promise\00", align 8
@global$sym$ae5207057439 = private unnamed_addr constant [8 x i8] c"promise\00", align 8
@global$sym$ae5193357482 = private unnamed_addr constant [8 x i8] c"promise\00", align 8

define ccc i32 @main() {
%mainenv56801 = call %struct.ScmObj* @const_init_null()
%mainargs56802 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv56801, %struct.ScmObj* %mainargs56802)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv56799,%struct.ScmObj* %mainargs56800) {
%stackaddr$makeclosure56803 = alloca %struct.ScmObj*, align 8
%fptrToInt56804 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48489 to i64
%ae48489 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56804)
store volatile %struct.ScmObj* %ae48489, %struct.ScmObj** %stackaddr$makeclosure56803, align 8
%ae48490 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56805 = alloca %struct.ScmObj*, align 8
%fptrToInt56806 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48491 to i64
%ae48491 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56806)
store volatile %struct.ScmObj* %ae48491, %struct.ScmObj** %stackaddr$makeclosure56805, align 8
%argslist56798$ae484890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56807 = alloca %struct.ScmObj*, align 8
%argslist56798$ae484891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48491, %struct.ScmObj* %argslist56798$ae484890)
store volatile %struct.ScmObj* %argslist56798$ae484891, %struct.ScmObj** %stackaddr$prim56807, align 8
%stackaddr$prim56808 = alloca %struct.ScmObj*, align 8
%argslist56798$ae484892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48490, %struct.ScmObj* %argslist56798$ae484891)
store volatile %struct.ScmObj* %argslist56798$ae484892, %struct.ScmObj** %stackaddr$prim56808, align 8
%clofunc56809 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48489)
musttail call tailcc void %clofunc56809(%struct.ScmObj* %ae48489, %struct.ScmObj* %argslist56798$ae484892)
ret void
}

define tailcc void @proc_clo$ae48489(%struct.ScmObj* %env$ae48489,%struct.ScmObj* %current_45args56175) {
%stackaddr$prim56810 = alloca %struct.ScmObj*, align 8
%_95k48303 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56175)
store volatile %struct.ScmObj* %_95k48303, %struct.ScmObj** %stackaddr$prim56810, align 8
%stackaddr$prim56811 = alloca %struct.ScmObj*, align 8
%current_45args56176 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56175)
store volatile %struct.ScmObj* %current_45args56176, %struct.ScmObj** %stackaddr$prim56811, align 8
%stackaddr$prim56812 = alloca %struct.ScmObj*, align 8
%anf_45bind48164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56176)
store volatile %struct.ScmObj* %anf_45bind48164, %struct.ScmObj** %stackaddr$prim56812, align 8
%stackaddr$makeclosure56813 = alloca %struct.ScmObj*, align 8
%fptrToInt56814 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48504 to i64
%ae48504 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56814)
store volatile %struct.ScmObj* %ae48504, %struct.ScmObj** %stackaddr$makeclosure56813, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48504, %struct.ScmObj* %anf_45bind48164, i64 0)
%ae48505 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56815 = alloca %struct.ScmObj*, align 8
%fptrToInt56816 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48506 to i64
%ae48506 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56816)
store volatile %struct.ScmObj* %ae48506, %struct.ScmObj** %stackaddr$makeclosure56815, align 8
%argslist56793$ae485040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56817 = alloca %struct.ScmObj*, align 8
%argslist56793$ae485041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48506, %struct.ScmObj* %argslist56793$ae485040)
store volatile %struct.ScmObj* %argslist56793$ae485041, %struct.ScmObj** %stackaddr$prim56817, align 8
%stackaddr$prim56818 = alloca %struct.ScmObj*, align 8
%argslist56793$ae485042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48505, %struct.ScmObj* %argslist56793$ae485041)
store volatile %struct.ScmObj* %argslist56793$ae485042, %struct.ScmObj** %stackaddr$prim56818, align 8
%clofunc56819 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48504)
musttail call tailcc void %clofunc56819(%struct.ScmObj* %ae48504, %struct.ScmObj* %argslist56793$ae485042)
ret void
}

define tailcc void @proc_clo$ae48504(%struct.ScmObj* %env$ae48504,%struct.ScmObj* %current_45args56178) {
%stackaddr$env-ref56820 = alloca %struct.ScmObj*, align 8
%anf_45bind48164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48504, i64 0)
store %struct.ScmObj* %anf_45bind48164, %struct.ScmObj** %stackaddr$env-ref56820
%stackaddr$prim56821 = alloca %struct.ScmObj*, align 8
%_95k48304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56178)
store volatile %struct.ScmObj* %_95k48304, %struct.ScmObj** %stackaddr$prim56821, align 8
%stackaddr$prim56822 = alloca %struct.ScmObj*, align 8
%current_45args56179 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56178)
store volatile %struct.ScmObj* %current_45args56179, %struct.ScmObj** %stackaddr$prim56822, align 8
%stackaddr$prim56823 = alloca %struct.ScmObj*, align 8
%anf_45bind48168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56179)
store volatile %struct.ScmObj* %anf_45bind48168, %struct.ScmObj** %stackaddr$prim56823, align 8
%stackaddr$makeclosure56824 = alloca %struct.ScmObj*, align 8
%fptrToInt56825 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48619 to i64
%ae48619 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56825)
store volatile %struct.ScmObj* %ae48619, %struct.ScmObj** %stackaddr$makeclosure56824, align 8
%argslist56772$anf_45bind481640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56826 = alloca %struct.ScmObj*, align 8
%argslist56772$anf_45bind481641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48168, %struct.ScmObj* %argslist56772$anf_45bind481640)
store volatile %struct.ScmObj* %argslist56772$anf_45bind481641, %struct.ScmObj** %stackaddr$prim56826, align 8
%stackaddr$prim56827 = alloca %struct.ScmObj*, align 8
%argslist56772$anf_45bind481642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48619, %struct.ScmObj* %argslist56772$anf_45bind481641)
store volatile %struct.ScmObj* %argslist56772$anf_45bind481642, %struct.ScmObj** %stackaddr$prim56827, align 8
%clofunc56828 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48164)
musttail call tailcc void %clofunc56828(%struct.ScmObj* %anf_45bind48164, %struct.ScmObj* %argslist56772$anf_45bind481642)
ret void
}

define tailcc void @proc_clo$ae48619(%struct.ScmObj* %env$ae48619,%struct.ScmObj* %current_45args56181) {
%stackaddr$prim56829 = alloca %struct.ScmObj*, align 8
%_95k48305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56181)
store volatile %struct.ScmObj* %_95k48305, %struct.ScmObj** %stackaddr$prim56829, align 8
%stackaddr$prim56830 = alloca %struct.ScmObj*, align 8
%current_45args56182 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56181)
store volatile %struct.ScmObj* %current_45args56182, %struct.ScmObj** %stackaddr$prim56830, align 8
%stackaddr$prim56831 = alloca %struct.ScmObj*, align 8
%Ycmb48031 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56182)
store volatile %struct.ScmObj* %Ycmb48031, %struct.ScmObj** %stackaddr$prim56831, align 8
%stackaddr$makeclosure56832 = alloca %struct.ScmObj*, align 8
%fptrToInt56833 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48621 to i64
%ae48621 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56833)
store volatile %struct.ScmObj* %ae48621, %struct.ScmObj** %stackaddr$makeclosure56832, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48621, %struct.ScmObj* %Ycmb48031, i64 0)
%ae48622 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56834 = alloca %struct.ScmObj*, align 8
%fptrToInt56835 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48623 to i64
%ae48623 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56835)
store volatile %struct.ScmObj* %ae48623, %struct.ScmObj** %stackaddr$makeclosure56834, align 8
%argslist56771$ae486210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56836 = alloca %struct.ScmObj*, align 8
%argslist56771$ae486211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48623, %struct.ScmObj* %argslist56771$ae486210)
store volatile %struct.ScmObj* %argslist56771$ae486211, %struct.ScmObj** %stackaddr$prim56836, align 8
%stackaddr$prim56837 = alloca %struct.ScmObj*, align 8
%argslist56771$ae486212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48622, %struct.ScmObj* %argslist56771$ae486211)
store volatile %struct.ScmObj* %argslist56771$ae486212, %struct.ScmObj** %stackaddr$prim56837, align 8
%clofunc56838 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48621)
musttail call tailcc void %clofunc56838(%struct.ScmObj* %ae48621, %struct.ScmObj* %argslist56771$ae486212)
ret void
}

define tailcc void @proc_clo$ae48621(%struct.ScmObj* %env$ae48621,%struct.ScmObj* %current_45args56184) {
%stackaddr$env-ref56839 = alloca %struct.ScmObj*, align 8
%Ycmb48031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48621, i64 0)
store %struct.ScmObj* %Ycmb48031, %struct.ScmObj** %stackaddr$env-ref56839
%stackaddr$prim56840 = alloca %struct.ScmObj*, align 8
%_95k48306 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56184)
store volatile %struct.ScmObj* %_95k48306, %struct.ScmObj** %stackaddr$prim56840, align 8
%stackaddr$prim56841 = alloca %struct.ScmObj*, align 8
%current_45args56185 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56184)
store volatile %struct.ScmObj* %current_45args56185, %struct.ScmObj** %stackaddr$prim56841, align 8
%stackaddr$prim56842 = alloca %struct.ScmObj*, align 8
%anf_45bind48173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56185)
store volatile %struct.ScmObj* %anf_45bind48173, %struct.ScmObj** %stackaddr$prim56842, align 8
%stackaddr$makeclosure56843 = alloca %struct.ScmObj*, align 8
%fptrToInt56844 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48699 to i64
%ae48699 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56844)
store volatile %struct.ScmObj* %ae48699, %struct.ScmObj** %stackaddr$makeclosure56843, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48699, %struct.ScmObj* %Ycmb48031, i64 0)
%argslist56755$Ycmb480310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56845 = alloca %struct.ScmObj*, align 8
%argslist56755$Ycmb480311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48173, %struct.ScmObj* %argslist56755$Ycmb480310)
store volatile %struct.ScmObj* %argslist56755$Ycmb480311, %struct.ScmObj** %stackaddr$prim56845, align 8
%stackaddr$prim56846 = alloca %struct.ScmObj*, align 8
%argslist56755$Ycmb480312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48699, %struct.ScmObj* %argslist56755$Ycmb480311)
store volatile %struct.ScmObj* %argslist56755$Ycmb480312, %struct.ScmObj** %stackaddr$prim56846, align 8
%clofunc56847 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48031)
musttail call tailcc void %clofunc56847(%struct.ScmObj* %Ycmb48031, %struct.ScmObj* %argslist56755$Ycmb480312)
ret void
}

define tailcc void @proc_clo$ae48699(%struct.ScmObj* %env$ae48699,%struct.ScmObj* %current_45args56187) {
%stackaddr$env-ref56848 = alloca %struct.ScmObj*, align 8
%Ycmb48031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48699, i64 0)
store %struct.ScmObj* %Ycmb48031, %struct.ScmObj** %stackaddr$env-ref56848
%stackaddr$prim56849 = alloca %struct.ScmObj*, align 8
%_95k48307 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56187)
store volatile %struct.ScmObj* %_95k48307, %struct.ScmObj** %stackaddr$prim56849, align 8
%stackaddr$prim56850 = alloca %struct.ScmObj*, align 8
%current_45args56188 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56187)
store volatile %struct.ScmObj* %current_45args56188, %struct.ScmObj** %stackaddr$prim56850, align 8
%stackaddr$prim56851 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56188)
store volatile %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$prim56851, align 8
%stackaddr$makeclosure56852 = alloca %struct.ScmObj*, align 8
%fptrToInt56853 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48701 to i64
%ae48701 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56853)
store volatile %struct.ScmObj* %ae48701, %struct.ScmObj** %stackaddr$makeclosure56852, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48701, %struct.ScmObj* %_37foldr148052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48701, %struct.ScmObj* %Ycmb48031, i64 1)
%ae48702 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56854 = alloca %struct.ScmObj*, align 8
%fptrToInt56855 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48703 to i64
%ae48703 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56855)
store volatile %struct.ScmObj* %ae48703, %struct.ScmObj** %stackaddr$makeclosure56854, align 8
%argslist56754$ae487010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56856 = alloca %struct.ScmObj*, align 8
%argslist56754$ae487011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48703, %struct.ScmObj* %argslist56754$ae487010)
store volatile %struct.ScmObj* %argslist56754$ae487011, %struct.ScmObj** %stackaddr$prim56856, align 8
%stackaddr$prim56857 = alloca %struct.ScmObj*, align 8
%argslist56754$ae487012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48702, %struct.ScmObj* %argslist56754$ae487011)
store volatile %struct.ScmObj* %argslist56754$ae487012, %struct.ScmObj** %stackaddr$prim56857, align 8
%clofunc56858 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48701)
musttail call tailcc void %clofunc56858(%struct.ScmObj* %ae48701, %struct.ScmObj* %argslist56754$ae487012)
ret void
}

define tailcc void @proc_clo$ae48701(%struct.ScmObj* %env$ae48701,%struct.ScmObj* %current_45args56190) {
%stackaddr$env-ref56859 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48701, i64 0)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref56859
%stackaddr$env-ref56860 = alloca %struct.ScmObj*, align 8
%Ycmb48031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48701, i64 1)
store %struct.ScmObj* %Ycmb48031, %struct.ScmObj** %stackaddr$env-ref56860
%stackaddr$prim56861 = alloca %struct.ScmObj*, align 8
%_95k48308 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56190)
store volatile %struct.ScmObj* %_95k48308, %struct.ScmObj** %stackaddr$prim56861, align 8
%stackaddr$prim56862 = alloca %struct.ScmObj*, align 8
%current_45args56191 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56190)
store volatile %struct.ScmObj* %current_45args56191, %struct.ScmObj** %stackaddr$prim56862, align 8
%stackaddr$prim56863 = alloca %struct.ScmObj*, align 8
%anf_45bind48179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56191)
store volatile %struct.ScmObj* %anf_45bind48179, %struct.ScmObj** %stackaddr$prim56863, align 8
%stackaddr$makeclosure56864 = alloca %struct.ScmObj*, align 8
%fptrToInt56865 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48796 to i64
%ae48796 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56865)
store volatile %struct.ScmObj* %ae48796, %struct.ScmObj** %stackaddr$makeclosure56864, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48796, %struct.ScmObj* %_37foldr148052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48796, %struct.ScmObj* %Ycmb48031, i64 1)
%argslist56735$Ycmb480310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56866 = alloca %struct.ScmObj*, align 8
%argslist56735$Ycmb480311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48179, %struct.ScmObj* %argslist56735$Ycmb480310)
store volatile %struct.ScmObj* %argslist56735$Ycmb480311, %struct.ScmObj** %stackaddr$prim56866, align 8
%stackaddr$prim56867 = alloca %struct.ScmObj*, align 8
%argslist56735$Ycmb480312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48796, %struct.ScmObj* %argslist56735$Ycmb480311)
store volatile %struct.ScmObj* %argslist56735$Ycmb480312, %struct.ScmObj** %stackaddr$prim56867, align 8
%clofunc56868 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48031)
musttail call tailcc void %clofunc56868(%struct.ScmObj* %Ycmb48031, %struct.ScmObj* %argslist56735$Ycmb480312)
ret void
}

define tailcc void @proc_clo$ae48796(%struct.ScmObj* %env$ae48796,%struct.ScmObj* %current_45args56193) {
%stackaddr$env-ref56869 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48796, i64 0)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref56869
%stackaddr$env-ref56870 = alloca %struct.ScmObj*, align 8
%Ycmb48031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48796, i64 1)
store %struct.ScmObj* %Ycmb48031, %struct.ScmObj** %stackaddr$env-ref56870
%stackaddr$prim56871 = alloca %struct.ScmObj*, align 8
%_95k48309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56193)
store volatile %struct.ScmObj* %_95k48309, %struct.ScmObj** %stackaddr$prim56871, align 8
%stackaddr$prim56872 = alloca %struct.ScmObj*, align 8
%current_45args56194 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56193)
store volatile %struct.ScmObj* %current_45args56194, %struct.ScmObj** %stackaddr$prim56872, align 8
%stackaddr$prim56873 = alloca %struct.ScmObj*, align 8
%_37map148048 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56194)
store volatile %struct.ScmObj* %_37map148048, %struct.ScmObj** %stackaddr$prim56873, align 8
%stackaddr$makeclosure56874 = alloca %struct.ScmObj*, align 8
%fptrToInt56875 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48798 to i64
%ae48798 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56875)
store volatile %struct.ScmObj* %ae48798, %struct.ScmObj** %stackaddr$makeclosure56874, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48798, %struct.ScmObj* %_37foldr148052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48798, %struct.ScmObj* %_37map148048, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48798, %struct.ScmObj* %Ycmb48031, i64 2)
%ae48799 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56876 = alloca %struct.ScmObj*, align 8
%fptrToInt56877 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48800 to i64
%ae48800 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56877)
store volatile %struct.ScmObj* %ae48800, %struct.ScmObj** %stackaddr$makeclosure56876, align 8
%argslist56734$ae487980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56878 = alloca %struct.ScmObj*, align 8
%argslist56734$ae487981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48800, %struct.ScmObj* %argslist56734$ae487980)
store volatile %struct.ScmObj* %argslist56734$ae487981, %struct.ScmObj** %stackaddr$prim56878, align 8
%stackaddr$prim56879 = alloca %struct.ScmObj*, align 8
%argslist56734$ae487982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48799, %struct.ScmObj* %argslist56734$ae487981)
store volatile %struct.ScmObj* %argslist56734$ae487982, %struct.ScmObj** %stackaddr$prim56879, align 8
%clofunc56880 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48798)
musttail call tailcc void %clofunc56880(%struct.ScmObj* %ae48798, %struct.ScmObj* %argslist56734$ae487982)
ret void
}

define tailcc void @proc_clo$ae48798(%struct.ScmObj* %env$ae48798,%struct.ScmObj* %current_45args56196) {
%stackaddr$env-ref56881 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48798, i64 0)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref56881
%stackaddr$env-ref56882 = alloca %struct.ScmObj*, align 8
%_37map148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48798, i64 1)
store %struct.ScmObj* %_37map148048, %struct.ScmObj** %stackaddr$env-ref56882
%stackaddr$env-ref56883 = alloca %struct.ScmObj*, align 8
%Ycmb48031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48798, i64 2)
store %struct.ScmObj* %Ycmb48031, %struct.ScmObj** %stackaddr$env-ref56883
%stackaddr$prim56884 = alloca %struct.ScmObj*, align 8
%_95k48310 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56196)
store volatile %struct.ScmObj* %_95k48310, %struct.ScmObj** %stackaddr$prim56884, align 8
%stackaddr$prim56885 = alloca %struct.ScmObj*, align 8
%current_45args56197 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56196)
store volatile %struct.ScmObj* %current_45args56197, %struct.ScmObj** %stackaddr$prim56885, align 8
%stackaddr$prim56886 = alloca %struct.ScmObj*, align 8
%anf_45bind48186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56197)
store volatile %struct.ScmObj* %anf_45bind48186, %struct.ScmObj** %stackaddr$prim56886, align 8
%stackaddr$makeclosure56887 = alloca %struct.ScmObj*, align 8
%fptrToInt56888 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48946 to i64
%ae48946 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56888)
store volatile %struct.ScmObj* %ae48946, %struct.ScmObj** %stackaddr$makeclosure56887, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48946, %struct.ScmObj* %_37foldr148052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48946, %struct.ScmObj* %_37map148048, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48946, %struct.ScmObj* %Ycmb48031, i64 2)
%argslist56718$Ycmb480310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56889 = alloca %struct.ScmObj*, align 8
%argslist56718$Ycmb480311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48186, %struct.ScmObj* %argslist56718$Ycmb480310)
store volatile %struct.ScmObj* %argslist56718$Ycmb480311, %struct.ScmObj** %stackaddr$prim56889, align 8
%stackaddr$prim56890 = alloca %struct.ScmObj*, align 8
%argslist56718$Ycmb480312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48946, %struct.ScmObj* %argslist56718$Ycmb480311)
store volatile %struct.ScmObj* %argslist56718$Ycmb480312, %struct.ScmObj** %stackaddr$prim56890, align 8
%clofunc56891 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48031)
musttail call tailcc void %clofunc56891(%struct.ScmObj* %Ycmb48031, %struct.ScmObj* %argslist56718$Ycmb480312)
ret void
}

define tailcc void @proc_clo$ae48946(%struct.ScmObj* %env$ae48946,%struct.ScmObj* %current_45args56199) {
%stackaddr$env-ref56892 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48946, i64 0)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref56892
%stackaddr$env-ref56893 = alloca %struct.ScmObj*, align 8
%_37map148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48946, i64 1)
store %struct.ScmObj* %_37map148048, %struct.ScmObj** %stackaddr$env-ref56893
%stackaddr$env-ref56894 = alloca %struct.ScmObj*, align 8
%Ycmb48031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48946, i64 2)
store %struct.ScmObj* %Ycmb48031, %struct.ScmObj** %stackaddr$env-ref56894
%stackaddr$prim56895 = alloca %struct.ScmObj*, align 8
%_95k48311 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56199)
store volatile %struct.ScmObj* %_95k48311, %struct.ScmObj** %stackaddr$prim56895, align 8
%stackaddr$prim56896 = alloca %struct.ScmObj*, align 8
%current_45args56200 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56199)
store volatile %struct.ScmObj* %current_45args56200, %struct.ScmObj** %stackaddr$prim56896, align 8
%stackaddr$prim56897 = alloca %struct.ScmObj*, align 8
%_37take48044 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56200)
store volatile %struct.ScmObj* %_37take48044, %struct.ScmObj** %stackaddr$prim56897, align 8
%stackaddr$makeclosure56898 = alloca %struct.ScmObj*, align 8
%fptrToInt56899 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48948 to i64
%ae48948 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56899)
store volatile %struct.ScmObj* %ae48948, %struct.ScmObj** %stackaddr$makeclosure56898, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48948, %struct.ScmObj* %_37foldr148052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48948, %struct.ScmObj* %_37map148048, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48948, %struct.ScmObj* %Ycmb48031, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48948, %struct.ScmObj* %_37take48044, i64 3)
%ae48949 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56900 = alloca %struct.ScmObj*, align 8
%fptrToInt56901 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48950 to i64
%ae48950 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56901)
store volatile %struct.ScmObj* %ae48950, %struct.ScmObj** %stackaddr$makeclosure56900, align 8
%argslist56717$ae489480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56902 = alloca %struct.ScmObj*, align 8
%argslist56717$ae489481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48950, %struct.ScmObj* %argslist56717$ae489480)
store volatile %struct.ScmObj* %argslist56717$ae489481, %struct.ScmObj** %stackaddr$prim56902, align 8
%stackaddr$prim56903 = alloca %struct.ScmObj*, align 8
%argslist56717$ae489482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48949, %struct.ScmObj* %argslist56717$ae489481)
store volatile %struct.ScmObj* %argslist56717$ae489482, %struct.ScmObj** %stackaddr$prim56903, align 8
%clofunc56904 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48948)
musttail call tailcc void %clofunc56904(%struct.ScmObj* %ae48948, %struct.ScmObj* %argslist56717$ae489482)
ret void
}

define tailcc void @proc_clo$ae48948(%struct.ScmObj* %env$ae48948,%struct.ScmObj* %current_45args56202) {
%stackaddr$env-ref56905 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48948, i64 0)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref56905
%stackaddr$env-ref56906 = alloca %struct.ScmObj*, align 8
%_37map148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48948, i64 1)
store %struct.ScmObj* %_37map148048, %struct.ScmObj** %stackaddr$env-ref56906
%stackaddr$env-ref56907 = alloca %struct.ScmObj*, align 8
%Ycmb48031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48948, i64 2)
store %struct.ScmObj* %Ycmb48031, %struct.ScmObj** %stackaddr$env-ref56907
%stackaddr$env-ref56908 = alloca %struct.ScmObj*, align 8
%_37take48044 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48948, i64 3)
store %struct.ScmObj* %_37take48044, %struct.ScmObj** %stackaddr$env-ref56908
%stackaddr$prim56909 = alloca %struct.ScmObj*, align 8
%_95k48312 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56202)
store volatile %struct.ScmObj* %_95k48312, %struct.ScmObj** %stackaddr$prim56909, align 8
%stackaddr$prim56910 = alloca %struct.ScmObj*, align 8
%current_45args56203 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56202)
store volatile %struct.ScmObj* %current_45args56203, %struct.ScmObj** %stackaddr$prim56910, align 8
%stackaddr$prim56911 = alloca %struct.ScmObj*, align 8
%anf_45bind48190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56203)
store volatile %struct.ScmObj* %anf_45bind48190, %struct.ScmObj** %stackaddr$prim56911, align 8
%stackaddr$makeclosure56912 = alloca %struct.ScmObj*, align 8
%fptrToInt56913 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49029 to i64
%ae49029 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56913)
store volatile %struct.ScmObj* %ae49029, %struct.ScmObj** %stackaddr$makeclosure56912, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49029, %struct.ScmObj* %_37foldr148052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49029, %struct.ScmObj* %_37map148048, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49029, %struct.ScmObj* %Ycmb48031, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49029, %struct.ScmObj* %_37take48044, i64 3)
%argslist56703$Ycmb480310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56914 = alloca %struct.ScmObj*, align 8
%argslist56703$Ycmb480311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48190, %struct.ScmObj* %argslist56703$Ycmb480310)
store volatile %struct.ScmObj* %argslist56703$Ycmb480311, %struct.ScmObj** %stackaddr$prim56914, align 8
%stackaddr$prim56915 = alloca %struct.ScmObj*, align 8
%argslist56703$Ycmb480312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49029, %struct.ScmObj* %argslist56703$Ycmb480311)
store volatile %struct.ScmObj* %argslist56703$Ycmb480312, %struct.ScmObj** %stackaddr$prim56915, align 8
%clofunc56916 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48031)
musttail call tailcc void %clofunc56916(%struct.ScmObj* %Ycmb48031, %struct.ScmObj* %argslist56703$Ycmb480312)
ret void
}

define tailcc void @proc_clo$ae49029(%struct.ScmObj* %env$ae49029,%struct.ScmObj* %current_45args56205) {
%stackaddr$env-ref56917 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49029, i64 0)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref56917
%stackaddr$env-ref56918 = alloca %struct.ScmObj*, align 8
%_37map148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49029, i64 1)
store %struct.ScmObj* %_37map148048, %struct.ScmObj** %stackaddr$env-ref56918
%stackaddr$env-ref56919 = alloca %struct.ScmObj*, align 8
%Ycmb48031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49029, i64 2)
store %struct.ScmObj* %Ycmb48031, %struct.ScmObj** %stackaddr$env-ref56919
%stackaddr$env-ref56920 = alloca %struct.ScmObj*, align 8
%_37take48044 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49029, i64 3)
store %struct.ScmObj* %_37take48044, %struct.ScmObj** %stackaddr$env-ref56920
%stackaddr$prim56921 = alloca %struct.ScmObj*, align 8
%_95k48313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56205)
store volatile %struct.ScmObj* %_95k48313, %struct.ScmObj** %stackaddr$prim56921, align 8
%stackaddr$prim56922 = alloca %struct.ScmObj*, align 8
%current_45args56206 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56205)
store volatile %struct.ScmObj* %current_45args56206, %struct.ScmObj** %stackaddr$prim56922, align 8
%stackaddr$prim56923 = alloca %struct.ScmObj*, align 8
%_37length48041 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56206)
store volatile %struct.ScmObj* %_37length48041, %struct.ScmObj** %stackaddr$prim56923, align 8
%stackaddr$makeclosure56924 = alloca %struct.ScmObj*, align 8
%fptrToInt56925 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49031 to i64
%ae49031 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56925)
store volatile %struct.ScmObj* %ae49031, %struct.ScmObj** %stackaddr$makeclosure56924, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49031, %struct.ScmObj* %_37length48041, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49031, %struct.ScmObj* %_37foldr148052, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49031, %struct.ScmObj* %_37map148048, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49031, %struct.ScmObj* %Ycmb48031, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49031, %struct.ScmObj* %_37take48044, i64 4)
%ae49032 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56926 = alloca %struct.ScmObj*, align 8
%fptrToInt56927 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49033 to i64
%ae49033 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56927)
store volatile %struct.ScmObj* %ae49033, %struct.ScmObj** %stackaddr$makeclosure56926, align 8
%argslist56702$ae490310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56928 = alloca %struct.ScmObj*, align 8
%argslist56702$ae490311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49033, %struct.ScmObj* %argslist56702$ae490310)
store volatile %struct.ScmObj* %argslist56702$ae490311, %struct.ScmObj** %stackaddr$prim56928, align 8
%stackaddr$prim56929 = alloca %struct.ScmObj*, align 8
%argslist56702$ae490312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49032, %struct.ScmObj* %argslist56702$ae490311)
store volatile %struct.ScmObj* %argslist56702$ae490312, %struct.ScmObj** %stackaddr$prim56929, align 8
%clofunc56930 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49031)
musttail call tailcc void %clofunc56930(%struct.ScmObj* %ae49031, %struct.ScmObj* %argslist56702$ae490312)
ret void
}

define tailcc void @proc_clo$ae49031(%struct.ScmObj* %env$ae49031,%struct.ScmObj* %current_45args56208) {
%stackaddr$env-ref56931 = alloca %struct.ScmObj*, align 8
%_37length48041 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49031, i64 0)
store %struct.ScmObj* %_37length48041, %struct.ScmObj** %stackaddr$env-ref56931
%stackaddr$env-ref56932 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49031, i64 1)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref56932
%stackaddr$env-ref56933 = alloca %struct.ScmObj*, align 8
%_37map148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49031, i64 2)
store %struct.ScmObj* %_37map148048, %struct.ScmObj** %stackaddr$env-ref56933
%stackaddr$env-ref56934 = alloca %struct.ScmObj*, align 8
%Ycmb48031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49031, i64 3)
store %struct.ScmObj* %Ycmb48031, %struct.ScmObj** %stackaddr$env-ref56934
%stackaddr$env-ref56935 = alloca %struct.ScmObj*, align 8
%_37take48044 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49031, i64 4)
store %struct.ScmObj* %_37take48044, %struct.ScmObj** %stackaddr$env-ref56935
%stackaddr$prim56936 = alloca %struct.ScmObj*, align 8
%_95k48314 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56208)
store volatile %struct.ScmObj* %_95k48314, %struct.ScmObj** %stackaddr$prim56936, align 8
%stackaddr$prim56937 = alloca %struct.ScmObj*, align 8
%current_45args56209 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56208)
store volatile %struct.ScmObj* %current_45args56209, %struct.ScmObj** %stackaddr$prim56937, align 8
%stackaddr$prim56938 = alloca %struct.ScmObj*, align 8
%anf_45bind48195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56209)
store volatile %struct.ScmObj* %anf_45bind48195, %struct.ScmObj** %stackaddr$prim56938, align 8
%stackaddr$makeclosure56939 = alloca %struct.ScmObj*, align 8
%fptrToInt56940 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49108 to i64
%ae49108 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56940)
store volatile %struct.ScmObj* %ae49108, %struct.ScmObj** %stackaddr$makeclosure56939, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49108, %struct.ScmObj* %_37length48041, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49108, %struct.ScmObj* %_37foldr148052, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49108, %struct.ScmObj* %_37map148048, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49108, %struct.ScmObj* %Ycmb48031, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49108, %struct.ScmObj* %_37take48044, i64 4)
%argslist56686$Ycmb480310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56941 = alloca %struct.ScmObj*, align 8
%argslist56686$Ycmb480311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48195, %struct.ScmObj* %argslist56686$Ycmb480310)
store volatile %struct.ScmObj* %argslist56686$Ycmb480311, %struct.ScmObj** %stackaddr$prim56941, align 8
%stackaddr$prim56942 = alloca %struct.ScmObj*, align 8
%argslist56686$Ycmb480312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49108, %struct.ScmObj* %argslist56686$Ycmb480311)
store volatile %struct.ScmObj* %argslist56686$Ycmb480312, %struct.ScmObj** %stackaddr$prim56942, align 8
%clofunc56943 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48031)
musttail call tailcc void %clofunc56943(%struct.ScmObj* %Ycmb48031, %struct.ScmObj* %argslist56686$Ycmb480312)
ret void
}

define tailcc void @proc_clo$ae49108(%struct.ScmObj* %env$ae49108,%struct.ScmObj* %current_45args56211) {
%stackaddr$env-ref56944 = alloca %struct.ScmObj*, align 8
%_37length48041 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49108, i64 0)
store %struct.ScmObj* %_37length48041, %struct.ScmObj** %stackaddr$env-ref56944
%stackaddr$env-ref56945 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49108, i64 1)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref56945
%stackaddr$env-ref56946 = alloca %struct.ScmObj*, align 8
%_37map148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49108, i64 2)
store %struct.ScmObj* %_37map148048, %struct.ScmObj** %stackaddr$env-ref56946
%stackaddr$env-ref56947 = alloca %struct.ScmObj*, align 8
%Ycmb48031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49108, i64 3)
store %struct.ScmObj* %Ycmb48031, %struct.ScmObj** %stackaddr$env-ref56947
%stackaddr$env-ref56948 = alloca %struct.ScmObj*, align 8
%_37take48044 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49108, i64 4)
store %struct.ScmObj* %_37take48044, %struct.ScmObj** %stackaddr$env-ref56948
%stackaddr$prim56949 = alloca %struct.ScmObj*, align 8
%_95k48315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56211)
store volatile %struct.ScmObj* %_95k48315, %struct.ScmObj** %stackaddr$prim56949, align 8
%stackaddr$prim56950 = alloca %struct.ScmObj*, align 8
%current_45args56212 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56211)
store volatile %struct.ScmObj* %current_45args56212, %struct.ScmObj** %stackaddr$prim56950, align 8
%stackaddr$prim56951 = alloca %struct.ScmObj*, align 8
%_37foldl148036 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56212)
store volatile %struct.ScmObj* %_37foldl148036, %struct.ScmObj** %stackaddr$prim56951, align 8
%stackaddr$makeclosure56952 = alloca %struct.ScmObj*, align 8
%fptrToInt56953 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49110 to i64
%ae49110 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt56953)
store volatile %struct.ScmObj* %ae49110, %struct.ScmObj** %stackaddr$makeclosure56952, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49110, %struct.ScmObj* %_37foldr148052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49110, %struct.ScmObj* %_37foldl148036, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49110, %struct.ScmObj* %_37length48041, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49110, %struct.ScmObj* %_37map148048, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49110, %struct.ScmObj* %Ycmb48031, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49110, %struct.ScmObj* %_37take48044, i64 5)
%ae49111 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56954 = alloca %struct.ScmObj*, align 8
%fptrToInt56955 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49112 to i64
%ae49112 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56955)
store volatile %struct.ScmObj* %ae49112, %struct.ScmObj** %stackaddr$makeclosure56954, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49112, %struct.ScmObj* %_37foldl148036, i64 0)
%argslist56685$ae491100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56956 = alloca %struct.ScmObj*, align 8
%argslist56685$ae491101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49112, %struct.ScmObj* %argslist56685$ae491100)
store volatile %struct.ScmObj* %argslist56685$ae491101, %struct.ScmObj** %stackaddr$prim56956, align 8
%stackaddr$prim56957 = alloca %struct.ScmObj*, align 8
%argslist56685$ae491102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49111, %struct.ScmObj* %argslist56685$ae491101)
store volatile %struct.ScmObj* %argslist56685$ae491102, %struct.ScmObj** %stackaddr$prim56957, align 8
%clofunc56958 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49110)
musttail call tailcc void %clofunc56958(%struct.ScmObj* %ae49110, %struct.ScmObj* %argslist56685$ae491102)
ret void
}

define tailcc void @proc_clo$ae49110(%struct.ScmObj* %env$ae49110,%struct.ScmObj* %current_45args56214) {
%stackaddr$env-ref56959 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49110, i64 0)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref56959
%stackaddr$env-ref56960 = alloca %struct.ScmObj*, align 8
%_37foldl148036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49110, i64 1)
store %struct.ScmObj* %_37foldl148036, %struct.ScmObj** %stackaddr$env-ref56960
%stackaddr$env-ref56961 = alloca %struct.ScmObj*, align 8
%_37length48041 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49110, i64 2)
store %struct.ScmObj* %_37length48041, %struct.ScmObj** %stackaddr$env-ref56961
%stackaddr$env-ref56962 = alloca %struct.ScmObj*, align 8
%_37map148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49110, i64 3)
store %struct.ScmObj* %_37map148048, %struct.ScmObj** %stackaddr$env-ref56962
%stackaddr$env-ref56963 = alloca %struct.ScmObj*, align 8
%Ycmb48031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49110, i64 4)
store %struct.ScmObj* %Ycmb48031, %struct.ScmObj** %stackaddr$env-ref56963
%stackaddr$env-ref56964 = alloca %struct.ScmObj*, align 8
%_37take48044 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49110, i64 5)
store %struct.ScmObj* %_37take48044, %struct.ScmObj** %stackaddr$env-ref56964
%stackaddr$prim56965 = alloca %struct.ScmObj*, align 8
%_95k48316 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56214)
store volatile %struct.ScmObj* %_95k48316, %struct.ScmObj** %stackaddr$prim56965, align 8
%stackaddr$prim56966 = alloca %struct.ScmObj*, align 8
%current_45args56215 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56214)
store volatile %struct.ScmObj* %current_45args56215, %struct.ScmObj** %stackaddr$prim56966, align 8
%stackaddr$prim56967 = alloca %struct.ScmObj*, align 8
%_37last48074 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56215)
store volatile %struct.ScmObj* %_37last48074, %struct.ScmObj** %stackaddr$prim56967, align 8
%stackaddr$makeclosure56968 = alloca %struct.ScmObj*, align 8
%fptrToInt56969 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49164 to i64
%ae49164 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56969)
store volatile %struct.ScmObj* %ae49164, %struct.ScmObj** %stackaddr$makeclosure56968, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49164, %struct.ScmObj* %_37foldr148052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49164, %struct.ScmObj* %_37foldl148036, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49164, %struct.ScmObj* %_37map148048, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49164, %struct.ScmObj* %Ycmb48031, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49164, %struct.ScmObj* %_37last48074, i64 4)
%ae49165 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56970 = alloca %struct.ScmObj*, align 8
%fptrToInt56971 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49166 to i64
%ae49166 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56971)
store volatile %struct.ScmObj* %ae49166, %struct.ScmObj** %stackaddr$makeclosure56970, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49166, %struct.ScmObj* %_37length48041, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49166, %struct.ScmObj* %_37take48044, i64 1)
%argslist56671$ae491640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56972 = alloca %struct.ScmObj*, align 8
%argslist56671$ae491641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49166, %struct.ScmObj* %argslist56671$ae491640)
store volatile %struct.ScmObj* %argslist56671$ae491641, %struct.ScmObj** %stackaddr$prim56972, align 8
%stackaddr$prim56973 = alloca %struct.ScmObj*, align 8
%argslist56671$ae491642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49165, %struct.ScmObj* %argslist56671$ae491641)
store volatile %struct.ScmObj* %argslist56671$ae491642, %struct.ScmObj** %stackaddr$prim56973, align 8
%clofunc56974 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49164)
musttail call tailcc void %clofunc56974(%struct.ScmObj* %ae49164, %struct.ScmObj* %argslist56671$ae491642)
ret void
}

define tailcc void @proc_clo$ae49164(%struct.ScmObj* %env$ae49164,%struct.ScmObj* %current_45args56217) {
%stackaddr$env-ref56975 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49164, i64 0)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref56975
%stackaddr$env-ref56976 = alloca %struct.ScmObj*, align 8
%_37foldl148036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49164, i64 1)
store %struct.ScmObj* %_37foldl148036, %struct.ScmObj** %stackaddr$env-ref56976
%stackaddr$env-ref56977 = alloca %struct.ScmObj*, align 8
%_37map148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49164, i64 2)
store %struct.ScmObj* %_37map148048, %struct.ScmObj** %stackaddr$env-ref56977
%stackaddr$env-ref56978 = alloca %struct.ScmObj*, align 8
%Ycmb48031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49164, i64 3)
store %struct.ScmObj* %Ycmb48031, %struct.ScmObj** %stackaddr$env-ref56978
%stackaddr$env-ref56979 = alloca %struct.ScmObj*, align 8
%_37last48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49164, i64 4)
store %struct.ScmObj* %_37last48074, %struct.ScmObj** %stackaddr$env-ref56979
%stackaddr$prim56980 = alloca %struct.ScmObj*, align 8
%_95k48317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56217)
store volatile %struct.ScmObj* %_95k48317, %struct.ScmObj** %stackaddr$prim56980, align 8
%stackaddr$prim56981 = alloca %struct.ScmObj*, align 8
%current_45args56218 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56217)
store volatile %struct.ScmObj* %current_45args56218, %struct.ScmObj** %stackaddr$prim56981, align 8
%stackaddr$prim56982 = alloca %struct.ScmObj*, align 8
%_37drop_45right48071 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56218)
store volatile %struct.ScmObj* %_37drop_45right48071, %struct.ScmObj** %stackaddr$prim56982, align 8
%stackaddr$makeclosure56983 = alloca %struct.ScmObj*, align 8
%fptrToInt56984 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49194 to i64
%ae49194 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56984)
store volatile %struct.ScmObj* %ae49194, %struct.ScmObj** %stackaddr$makeclosure56983, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49194, %struct.ScmObj* %_37foldr148052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49194, %struct.ScmObj* %_37foldl148036, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49194, %struct.ScmObj* %_37drop_45right48071, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49194, %struct.ScmObj* %Ycmb48031, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49194, %struct.ScmObj* %_37last48074, i64 4)
%ae49195 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56985 = alloca %struct.ScmObj*, align 8
%fptrToInt56986 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49196 to i64
%ae49196 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56986)
store volatile %struct.ScmObj* %ae49196, %struct.ScmObj** %stackaddr$makeclosure56985, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49196, %struct.ScmObj* %_37foldr148052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49196, %struct.ScmObj* %_37map148048, i64 1)
%argslist56661$ae491940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56987 = alloca %struct.ScmObj*, align 8
%argslist56661$ae491941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49196, %struct.ScmObj* %argslist56661$ae491940)
store volatile %struct.ScmObj* %argslist56661$ae491941, %struct.ScmObj** %stackaddr$prim56987, align 8
%stackaddr$prim56988 = alloca %struct.ScmObj*, align 8
%argslist56661$ae491942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49195, %struct.ScmObj* %argslist56661$ae491941)
store volatile %struct.ScmObj* %argslist56661$ae491942, %struct.ScmObj** %stackaddr$prim56988, align 8
%clofunc56989 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49194)
musttail call tailcc void %clofunc56989(%struct.ScmObj* %ae49194, %struct.ScmObj* %argslist56661$ae491942)
ret void
}

define tailcc void @proc_clo$ae49194(%struct.ScmObj* %env$ae49194,%struct.ScmObj* %current_45args56220) {
%stackaddr$env-ref56990 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49194, i64 0)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref56990
%stackaddr$env-ref56991 = alloca %struct.ScmObj*, align 8
%_37foldl148036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49194, i64 1)
store %struct.ScmObj* %_37foldl148036, %struct.ScmObj** %stackaddr$env-ref56991
%stackaddr$env-ref56992 = alloca %struct.ScmObj*, align 8
%_37drop_45right48071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49194, i64 2)
store %struct.ScmObj* %_37drop_45right48071, %struct.ScmObj** %stackaddr$env-ref56992
%stackaddr$env-ref56993 = alloca %struct.ScmObj*, align 8
%Ycmb48031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49194, i64 3)
store %struct.ScmObj* %Ycmb48031, %struct.ScmObj** %stackaddr$env-ref56993
%stackaddr$env-ref56994 = alloca %struct.ScmObj*, align 8
%_37last48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49194, i64 4)
store %struct.ScmObj* %_37last48074, %struct.ScmObj** %stackaddr$env-ref56994
%stackaddr$prim56995 = alloca %struct.ScmObj*, align 8
%_95k48318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56220)
store volatile %struct.ScmObj* %_95k48318, %struct.ScmObj** %stackaddr$prim56995, align 8
%stackaddr$prim56996 = alloca %struct.ScmObj*, align 8
%current_45args56221 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56220)
store volatile %struct.ScmObj* %current_45args56221, %struct.ScmObj** %stackaddr$prim56996, align 8
%stackaddr$prim56997 = alloca %struct.ScmObj*, align 8
%anf_45bind48211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56221)
store volatile %struct.ScmObj* %anf_45bind48211, %struct.ScmObj** %stackaddr$prim56997, align 8
%stackaddr$makeclosure56998 = alloca %struct.ScmObj*, align 8
%fptrToInt56999 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49578 to i64
%ae49578 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56999)
store volatile %struct.ScmObj* %ae49578, %struct.ScmObj** %stackaddr$makeclosure56998, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49578, %struct.ScmObj* %_37foldr148052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49578, %struct.ScmObj* %_37foldl148036, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49578, %struct.ScmObj* %_37drop_45right48071, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49578, %struct.ScmObj* %Ycmb48031, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49578, %struct.ScmObj* %_37last48074, i64 4)
%argslist56601$Ycmb480310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57000 = alloca %struct.ScmObj*, align 8
%argslist56601$Ycmb480311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48211, %struct.ScmObj* %argslist56601$Ycmb480310)
store volatile %struct.ScmObj* %argslist56601$Ycmb480311, %struct.ScmObj** %stackaddr$prim57000, align 8
%stackaddr$prim57001 = alloca %struct.ScmObj*, align 8
%argslist56601$Ycmb480312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49578, %struct.ScmObj* %argslist56601$Ycmb480311)
store volatile %struct.ScmObj* %argslist56601$Ycmb480312, %struct.ScmObj** %stackaddr$prim57001, align 8
%clofunc57002 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48031)
musttail call tailcc void %clofunc57002(%struct.ScmObj* %Ycmb48031, %struct.ScmObj* %argslist56601$Ycmb480312)
ret void
}

define tailcc void @proc_clo$ae49578(%struct.ScmObj* %env$ae49578,%struct.ScmObj* %current_45args56223) {
%stackaddr$env-ref57003 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49578, i64 0)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref57003
%stackaddr$env-ref57004 = alloca %struct.ScmObj*, align 8
%_37foldl148036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49578, i64 1)
store %struct.ScmObj* %_37foldl148036, %struct.ScmObj** %stackaddr$env-ref57004
%stackaddr$env-ref57005 = alloca %struct.ScmObj*, align 8
%_37drop_45right48071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49578, i64 2)
store %struct.ScmObj* %_37drop_45right48071, %struct.ScmObj** %stackaddr$env-ref57005
%stackaddr$env-ref57006 = alloca %struct.ScmObj*, align 8
%Ycmb48031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49578, i64 3)
store %struct.ScmObj* %Ycmb48031, %struct.ScmObj** %stackaddr$env-ref57006
%stackaddr$env-ref57007 = alloca %struct.ScmObj*, align 8
%_37last48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49578, i64 4)
store %struct.ScmObj* %_37last48074, %struct.ScmObj** %stackaddr$env-ref57007
%stackaddr$prim57008 = alloca %struct.ScmObj*, align 8
%_95k48319 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56223)
store volatile %struct.ScmObj* %_95k48319, %struct.ScmObj** %stackaddr$prim57008, align 8
%stackaddr$prim57009 = alloca %struct.ScmObj*, align 8
%current_45args56224 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56223)
store volatile %struct.ScmObj* %current_45args56224, %struct.ScmObj** %stackaddr$prim57009, align 8
%stackaddr$prim57010 = alloca %struct.ScmObj*, align 8
%_37foldr48057 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56224)
store volatile %struct.ScmObj* %_37foldr48057, %struct.ScmObj** %stackaddr$prim57010, align 8
%stackaddr$makeclosure57011 = alloca %struct.ScmObj*, align 8
%fptrToInt57012 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49580 to i64
%ae49580 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt57012)
store volatile %struct.ScmObj* %ae49580, %struct.ScmObj** %stackaddr$makeclosure57011, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49580, %struct.ScmObj* %_37foldr148052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49580, %struct.ScmObj* %_37foldl148036, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49580, %struct.ScmObj* %_37foldr48057, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49580, %struct.ScmObj* %_37drop_45right48071, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49580, %struct.ScmObj* %Ycmb48031, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49580, %struct.ScmObj* %_37last48074, i64 5)
%ae49581 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57013 = alloca %struct.ScmObj*, align 8
%fptrToInt57014 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49582 to i64
%ae49582 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57014)
store volatile %struct.ScmObj* %ae49582, %struct.ScmObj** %stackaddr$makeclosure57013, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49582, %struct.ScmObj* %_37foldr148052, i64 0)
%argslist56600$ae495800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57015 = alloca %struct.ScmObj*, align 8
%argslist56600$ae495801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49582, %struct.ScmObj* %argslist56600$ae495800)
store volatile %struct.ScmObj* %argslist56600$ae495801, %struct.ScmObj** %stackaddr$prim57015, align 8
%stackaddr$prim57016 = alloca %struct.ScmObj*, align 8
%argslist56600$ae495802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49581, %struct.ScmObj* %argslist56600$ae495801)
store volatile %struct.ScmObj* %argslist56600$ae495802, %struct.ScmObj** %stackaddr$prim57016, align 8
%clofunc57017 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49580)
musttail call tailcc void %clofunc57017(%struct.ScmObj* %ae49580, %struct.ScmObj* %argslist56600$ae495802)
ret void
}

define tailcc void @proc_clo$ae49580(%struct.ScmObj* %env$ae49580,%struct.ScmObj* %current_45args56226) {
%stackaddr$env-ref57018 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49580, i64 0)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref57018
%stackaddr$env-ref57019 = alloca %struct.ScmObj*, align 8
%_37foldl148036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49580, i64 1)
store %struct.ScmObj* %_37foldl148036, %struct.ScmObj** %stackaddr$env-ref57019
%stackaddr$env-ref57020 = alloca %struct.ScmObj*, align 8
%_37foldr48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49580, i64 2)
store %struct.ScmObj* %_37foldr48057, %struct.ScmObj** %stackaddr$env-ref57020
%stackaddr$env-ref57021 = alloca %struct.ScmObj*, align 8
%_37drop_45right48071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49580, i64 3)
store %struct.ScmObj* %_37drop_45right48071, %struct.ScmObj** %stackaddr$env-ref57021
%stackaddr$env-ref57022 = alloca %struct.ScmObj*, align 8
%Ycmb48031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49580, i64 4)
store %struct.ScmObj* %Ycmb48031, %struct.ScmObj** %stackaddr$env-ref57022
%stackaddr$env-ref57023 = alloca %struct.ScmObj*, align 8
%_37last48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49580, i64 5)
store %struct.ScmObj* %_37last48074, %struct.ScmObj** %stackaddr$env-ref57023
%stackaddr$prim57024 = alloca %struct.ScmObj*, align 8
%_95k48320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56226)
store volatile %struct.ScmObj* %_95k48320, %struct.ScmObj** %stackaddr$prim57024, align 8
%stackaddr$prim57025 = alloca %struct.ScmObj*, align 8
%current_45args56227 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56226)
store volatile %struct.ScmObj* %current_45args56227, %struct.ScmObj** %stackaddr$prim57025, align 8
%stackaddr$prim57026 = alloca %struct.ScmObj*, align 8
%_37map148083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56227)
store volatile %struct.ScmObj* %_37map148083, %struct.ScmObj** %stackaddr$prim57026, align 8
%stackaddr$makeclosure57027 = alloca %struct.ScmObj*, align 8
%fptrToInt57028 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49657 to i64
%ae49657 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57028)
store volatile %struct.ScmObj* %ae49657, %struct.ScmObj** %stackaddr$makeclosure57027, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49657, %struct.ScmObj* %_37foldr148052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49657, %struct.ScmObj* %_37foldl148036, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49657, %struct.ScmObj* %_37foldr48057, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49657, %struct.ScmObj* %_37map148083, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49657, %struct.ScmObj* %Ycmb48031, i64 4)
%ae49658 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57029 = alloca %struct.ScmObj*, align 8
%fptrToInt57030 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49659 to i64
%ae49659 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57030)
store volatile %struct.ScmObj* %ae49659, %struct.ScmObj** %stackaddr$makeclosure57029, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49659, %struct.ScmObj* %_37foldr48057, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49659, %struct.ScmObj* %_37drop_45right48071, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49659, %struct.ScmObj* %_37last48074, i64 2)
%argslist56581$ae496570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57031 = alloca %struct.ScmObj*, align 8
%argslist56581$ae496571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49659, %struct.ScmObj* %argslist56581$ae496570)
store volatile %struct.ScmObj* %argslist56581$ae496571, %struct.ScmObj** %stackaddr$prim57031, align 8
%stackaddr$prim57032 = alloca %struct.ScmObj*, align 8
%argslist56581$ae496572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49658, %struct.ScmObj* %argslist56581$ae496571)
store volatile %struct.ScmObj* %argslist56581$ae496572, %struct.ScmObj** %stackaddr$prim57032, align 8
%clofunc57033 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49657)
musttail call tailcc void %clofunc57033(%struct.ScmObj* %ae49657, %struct.ScmObj* %argslist56581$ae496572)
ret void
}

define tailcc void @proc_clo$ae49657(%struct.ScmObj* %env$ae49657,%struct.ScmObj* %current_45args56229) {
%stackaddr$env-ref57034 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49657, i64 0)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref57034
%stackaddr$env-ref57035 = alloca %struct.ScmObj*, align 8
%_37foldl148036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49657, i64 1)
store %struct.ScmObj* %_37foldl148036, %struct.ScmObj** %stackaddr$env-ref57035
%stackaddr$env-ref57036 = alloca %struct.ScmObj*, align 8
%_37foldr48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49657, i64 2)
store %struct.ScmObj* %_37foldr48057, %struct.ScmObj** %stackaddr$env-ref57036
%stackaddr$env-ref57037 = alloca %struct.ScmObj*, align 8
%_37map148083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49657, i64 3)
store %struct.ScmObj* %_37map148083, %struct.ScmObj** %stackaddr$env-ref57037
%stackaddr$env-ref57038 = alloca %struct.ScmObj*, align 8
%Ycmb48031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49657, i64 4)
store %struct.ScmObj* %Ycmb48031, %struct.ScmObj** %stackaddr$env-ref57038
%stackaddr$prim57039 = alloca %struct.ScmObj*, align 8
%_95k48321 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56229)
store volatile %struct.ScmObj* %_95k48321, %struct.ScmObj** %stackaddr$prim57039, align 8
%stackaddr$prim57040 = alloca %struct.ScmObj*, align 8
%current_45args56230 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56229)
store volatile %struct.ScmObj* %current_45args56230, %struct.ScmObj** %stackaddr$prim57040, align 8
%stackaddr$prim57041 = alloca %struct.ScmObj*, align 8
%_37map48078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56230)
store volatile %struct.ScmObj* %_37map48078, %struct.ScmObj** %stackaddr$prim57041, align 8
%stackaddr$makeclosure57042 = alloca %struct.ScmObj*, align 8
%fptrToInt57043 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49803 to i64
%ae49803 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57043)
store volatile %struct.ScmObj* %ae49803, %struct.ScmObj** %stackaddr$makeclosure57042, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49803, %struct.ScmObj* %_37foldl148036, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49803, %struct.ScmObj* %Ycmb48031, i64 1)
%ae49804 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57044 = alloca %struct.ScmObj*, align 8
%fptrToInt57045 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49805 to i64
%ae49805 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57045)
store volatile %struct.ScmObj* %ae49805, %struct.ScmObj** %stackaddr$makeclosure57044, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49805, %struct.ScmObj* %_37foldr48057, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49805, %struct.ScmObj* %_37foldr148052, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49805, %struct.ScmObj* %_37map148083, i64 2)
%argslist56564$ae498030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57046 = alloca %struct.ScmObj*, align 8
%argslist56564$ae498031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49805, %struct.ScmObj* %argslist56564$ae498030)
store volatile %struct.ScmObj* %argslist56564$ae498031, %struct.ScmObj** %stackaddr$prim57046, align 8
%stackaddr$prim57047 = alloca %struct.ScmObj*, align 8
%argslist56564$ae498032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49804, %struct.ScmObj* %argslist56564$ae498031)
store volatile %struct.ScmObj* %argslist56564$ae498032, %struct.ScmObj** %stackaddr$prim57047, align 8
%clofunc57048 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49803)
musttail call tailcc void %clofunc57048(%struct.ScmObj* %ae49803, %struct.ScmObj* %argslist56564$ae498032)
ret void
}

define tailcc void @proc_clo$ae49803(%struct.ScmObj* %env$ae49803,%struct.ScmObj* %current_45args56232) {
%stackaddr$env-ref57049 = alloca %struct.ScmObj*, align 8
%_37foldl148036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49803, i64 0)
store %struct.ScmObj* %_37foldl148036, %struct.ScmObj** %stackaddr$env-ref57049
%stackaddr$env-ref57050 = alloca %struct.ScmObj*, align 8
%Ycmb48031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49803, i64 1)
store %struct.ScmObj* %Ycmb48031, %struct.ScmObj** %stackaddr$env-ref57050
%stackaddr$prim57051 = alloca %struct.ScmObj*, align 8
%_95k48322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56232)
store volatile %struct.ScmObj* %_95k48322, %struct.ScmObj** %stackaddr$prim57051, align 8
%stackaddr$prim57052 = alloca %struct.ScmObj*, align 8
%current_45args56233 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56232)
store volatile %struct.ScmObj* %current_45args56233, %struct.ScmObj** %stackaddr$prim57052, align 8
%stackaddr$prim57053 = alloca %struct.ScmObj*, align 8
%anf_45bind48231 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56233)
store volatile %struct.ScmObj* %anf_45bind48231, %struct.ScmObj** %stackaddr$prim57053, align 8
%stackaddr$makeclosure57054 = alloca %struct.ScmObj*, align 8
%fptrToInt57055 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50195 to i64
%ae50195 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57055)
store volatile %struct.ScmObj* %ae50195, %struct.ScmObj** %stackaddr$makeclosure57054, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50195, %struct.ScmObj* %_37foldl148036, i64 0)
%argslist56504$Ycmb480310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57056 = alloca %struct.ScmObj*, align 8
%argslist56504$Ycmb480311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48231, %struct.ScmObj* %argslist56504$Ycmb480310)
store volatile %struct.ScmObj* %argslist56504$Ycmb480311, %struct.ScmObj** %stackaddr$prim57056, align 8
%stackaddr$prim57057 = alloca %struct.ScmObj*, align 8
%argslist56504$Ycmb480312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50195, %struct.ScmObj* %argslist56504$Ycmb480311)
store volatile %struct.ScmObj* %argslist56504$Ycmb480312, %struct.ScmObj** %stackaddr$prim57057, align 8
%clofunc57058 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48031)
musttail call tailcc void %clofunc57058(%struct.ScmObj* %Ycmb48031, %struct.ScmObj* %argslist56504$Ycmb480312)
ret void
}

define tailcc void @proc_clo$ae50195(%struct.ScmObj* %env$ae50195,%struct.ScmObj* %current_45args56235) {
%stackaddr$env-ref57059 = alloca %struct.ScmObj*, align 8
%_37foldl148036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50195, i64 0)
store %struct.ScmObj* %_37foldl148036, %struct.ScmObj** %stackaddr$env-ref57059
%stackaddr$prim57060 = alloca %struct.ScmObj*, align 8
%_95k48323 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56235)
store volatile %struct.ScmObj* %_95k48323, %struct.ScmObj** %stackaddr$prim57060, align 8
%stackaddr$prim57061 = alloca %struct.ScmObj*, align 8
%current_45args56236 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56235)
store volatile %struct.ScmObj* %current_45args56236, %struct.ScmObj** %stackaddr$prim57061, align 8
%stackaddr$prim57062 = alloca %struct.ScmObj*, align 8
%_37foldl48134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56236)
store volatile %struct.ScmObj* %_37foldl48134, %struct.ScmObj** %stackaddr$prim57062, align 8
%stackaddr$makeclosure57063 = alloca %struct.ScmObj*, align 8
%fptrToInt57064 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50197 to i64
%ae50197 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57064)
store volatile %struct.ScmObj* %ae50197, %struct.ScmObj** %stackaddr$makeclosure57063, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50197, %struct.ScmObj* %_37foldl148036, i64 0)
%ae50198 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57065 = alloca %struct.ScmObj*, align 8
%fptrToInt57066 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50199 to i64
%ae50199 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57066)
store volatile %struct.ScmObj* %ae50199, %struct.ScmObj** %stackaddr$makeclosure57065, align 8
%argslist56503$ae501970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57067 = alloca %struct.ScmObj*, align 8
%argslist56503$ae501971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50199, %struct.ScmObj* %argslist56503$ae501970)
store volatile %struct.ScmObj* %argslist56503$ae501971, %struct.ScmObj** %stackaddr$prim57067, align 8
%stackaddr$prim57068 = alloca %struct.ScmObj*, align 8
%argslist56503$ae501972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50198, %struct.ScmObj* %argslist56503$ae501971)
store volatile %struct.ScmObj* %argslist56503$ae501972, %struct.ScmObj** %stackaddr$prim57068, align 8
%clofunc57069 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50197)
musttail call tailcc void %clofunc57069(%struct.ScmObj* %ae50197, %struct.ScmObj* %argslist56503$ae501972)
ret void
}

define tailcc void @proc_clo$ae50197(%struct.ScmObj* %env$ae50197,%struct.ScmObj* %current_45args56238) {
%stackaddr$env-ref57070 = alloca %struct.ScmObj*, align 8
%_37foldl148036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50197, i64 0)
store %struct.ScmObj* %_37foldl148036, %struct.ScmObj** %stackaddr$env-ref57070
%stackaddr$prim57071 = alloca %struct.ScmObj*, align 8
%_95k48324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56238)
store volatile %struct.ScmObj* %_95k48324, %struct.ScmObj** %stackaddr$prim57071, align 8
%stackaddr$prim57072 = alloca %struct.ScmObj*, align 8
%current_45args56239 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56238)
store volatile %struct.ScmObj* %current_45args56239, %struct.ScmObj** %stackaddr$prim57072, align 8
%stackaddr$prim57073 = alloca %struct.ScmObj*, align 8
%_37_6248131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56239)
store volatile %struct.ScmObj* %_37_6248131, %struct.ScmObj** %stackaddr$prim57073, align 8
%stackaddr$makeclosure57074 = alloca %struct.ScmObj*, align 8
%fptrToInt57075 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50221 to i64
%ae50221 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57075)
store volatile %struct.ScmObj* %ae50221, %struct.ScmObj** %stackaddr$makeclosure57074, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50221, %struct.ScmObj* %_37foldl148036, i64 0)
%ae50222 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57076 = alloca %struct.ScmObj*, align 8
%fptrToInt57077 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50223 to i64
%ae50223 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57077)
store volatile %struct.ScmObj* %ae50223, %struct.ScmObj** %stackaddr$makeclosure57076, align 8
%argslist56497$ae502210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57078 = alloca %struct.ScmObj*, align 8
%argslist56497$ae502211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50223, %struct.ScmObj* %argslist56497$ae502210)
store volatile %struct.ScmObj* %argslist56497$ae502211, %struct.ScmObj** %stackaddr$prim57078, align 8
%stackaddr$prim57079 = alloca %struct.ScmObj*, align 8
%argslist56497$ae502212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50222, %struct.ScmObj* %argslist56497$ae502211)
store volatile %struct.ScmObj* %argslist56497$ae502212, %struct.ScmObj** %stackaddr$prim57079, align 8
%clofunc57080 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50221)
musttail call tailcc void %clofunc57080(%struct.ScmObj* %ae50221, %struct.ScmObj* %argslist56497$ae502212)
ret void
}

define tailcc void @proc_clo$ae50221(%struct.ScmObj* %env$ae50221,%struct.ScmObj* %current_45args56241) {
%stackaddr$env-ref57081 = alloca %struct.ScmObj*, align 8
%_37foldl148036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50221, i64 0)
store %struct.ScmObj* %_37foldl148036, %struct.ScmObj** %stackaddr$env-ref57081
%stackaddr$prim57082 = alloca %struct.ScmObj*, align 8
%_95k48325 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56241)
store volatile %struct.ScmObj* %_95k48325, %struct.ScmObj** %stackaddr$prim57082, align 8
%stackaddr$prim57083 = alloca %struct.ScmObj*, align 8
%current_45args56242 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56241)
store volatile %struct.ScmObj* %current_45args56242, %struct.ScmObj** %stackaddr$prim57083, align 8
%stackaddr$prim57084 = alloca %struct.ScmObj*, align 8
%_37_62_6148128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56242)
store volatile %struct.ScmObj* %_37_62_6148128, %struct.ScmObj** %stackaddr$prim57084, align 8
%ae50245 = call %struct.ScmObj* @const_init_int(i64 1)
%ae50246 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57085 = alloca %struct.ScmObj*, align 8
%_37append48124 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50245, %struct.ScmObj* %ae50246)
store volatile %struct.ScmObj* %_37append48124, %struct.ScmObj** %stackaddr$prim57085, align 8
%stackaddr$makeclosure57086 = alloca %struct.ScmObj*, align 8
%fptrToInt57087 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50247 to i64
%ae50247 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57087)
store volatile %struct.ScmObj* %ae50247, %struct.ScmObj** %stackaddr$makeclosure57086, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50247, %struct.ScmObj* %_37foldl148036, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50247, %struct.ScmObj* %_37append48124, i64 1)
%ae50248 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57088 = alloca %struct.ScmObj*, align 8
%fptrToInt57089 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50249 to i64
%ae50249 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57089)
store volatile %struct.ScmObj* %ae50249, %struct.ScmObj** %stackaddr$makeclosure57088, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50249, %struct.ScmObj* %_37append48124, i64 0)
%argslist56491$ae502470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57090 = alloca %struct.ScmObj*, align 8
%argslist56491$ae502471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50249, %struct.ScmObj* %argslist56491$ae502470)
store volatile %struct.ScmObj* %argslist56491$ae502471, %struct.ScmObj** %stackaddr$prim57090, align 8
%stackaddr$prim57091 = alloca %struct.ScmObj*, align 8
%argslist56491$ae502472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50248, %struct.ScmObj* %argslist56491$ae502471)
store volatile %struct.ScmObj* %argslist56491$ae502472, %struct.ScmObj** %stackaddr$prim57091, align 8
%clofunc57092 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50247)
musttail call tailcc void %clofunc57092(%struct.ScmObj* %ae50247, %struct.ScmObj* %argslist56491$ae502472)
ret void
}

define tailcc void @proc_clo$ae50247(%struct.ScmObj* %env$ae50247,%struct.ScmObj* %current_45args56244) {
%stackaddr$env-ref57093 = alloca %struct.ScmObj*, align 8
%_37foldl148036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50247, i64 0)
store %struct.ScmObj* %_37foldl148036, %struct.ScmObj** %stackaddr$env-ref57093
%stackaddr$env-ref57094 = alloca %struct.ScmObj*, align 8
%_37append48124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50247, i64 1)
store %struct.ScmObj* %_37append48124, %struct.ScmObj** %stackaddr$env-ref57094
%stackaddr$prim57095 = alloca %struct.ScmObj*, align 8
%_95k48326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56244)
store volatile %struct.ScmObj* %_95k48326, %struct.ScmObj** %stackaddr$prim57095, align 8
%stackaddr$prim57096 = alloca %struct.ScmObj*, align 8
%current_45args56245 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56244)
store volatile %struct.ScmObj* %current_45args56245, %struct.ScmObj** %stackaddr$prim57096, align 8
%stackaddr$prim57097 = alloca %struct.ScmObj*, align 8
%anf_45bind48239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56245)
store volatile %struct.ScmObj* %anf_45bind48239, %struct.ScmObj** %stackaddr$prim57097, align 8
%ae50315 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57098 = alloca %struct.ScmObj*, align 8
%_95048125 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append48124, %struct.ScmObj* %ae50315, %struct.ScmObj* %anf_45bind48239)
store volatile %struct.ScmObj* %_95048125, %struct.ScmObj** %stackaddr$prim57098, align 8
%ae50318 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57099 = alloca %struct.ScmObj*, align 8
%_37append48123 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48124, %struct.ScmObj* %ae50318)
store volatile %struct.ScmObj* %_37append48123, %struct.ScmObj** %stackaddr$prim57099, align 8
%stackaddr$makeclosure57100 = alloca %struct.ScmObj*, align 8
%fptrToInt57101 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50319 to i64
%ae50319 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57101)
store volatile %struct.ScmObj* %ae50319, %struct.ScmObj** %stackaddr$makeclosure57100, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50319, %struct.ScmObj* %_37foldl148036, i64 0)
%ae50320 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57102 = alloca %struct.ScmObj*, align 8
%fptrToInt57103 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50321 to i64
%ae50321 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57103)
store volatile %struct.ScmObj* %ae50321, %struct.ScmObj** %stackaddr$makeclosure57102, align 8
%argslist56480$ae503190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57104 = alloca %struct.ScmObj*, align 8
%argslist56480$ae503191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50321, %struct.ScmObj* %argslist56480$ae503190)
store volatile %struct.ScmObj* %argslist56480$ae503191, %struct.ScmObj** %stackaddr$prim57104, align 8
%stackaddr$prim57105 = alloca %struct.ScmObj*, align 8
%argslist56480$ae503192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50320, %struct.ScmObj* %argslist56480$ae503191)
store volatile %struct.ScmObj* %argslist56480$ae503192, %struct.ScmObj** %stackaddr$prim57105, align 8
%clofunc57106 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50319)
musttail call tailcc void %clofunc57106(%struct.ScmObj* %ae50319, %struct.ScmObj* %argslist56480$ae503192)
ret void
}

define tailcc void @proc_clo$ae50319(%struct.ScmObj* %env$ae50319,%struct.ScmObj* %current_45args56247) {
%stackaddr$env-ref57107 = alloca %struct.ScmObj*, align 8
%_37foldl148036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50319, i64 0)
store %struct.ScmObj* %_37foldl148036, %struct.ScmObj** %stackaddr$env-ref57107
%stackaddr$prim57108 = alloca %struct.ScmObj*, align 8
%_95k48327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56247)
store volatile %struct.ScmObj* %_95k48327, %struct.ScmObj** %stackaddr$prim57108, align 8
%stackaddr$prim57109 = alloca %struct.ScmObj*, align 8
%current_45args56248 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56247)
store volatile %struct.ScmObj* %current_45args56248, %struct.ScmObj** %stackaddr$prim57109, align 8
%stackaddr$prim57110 = alloca %struct.ScmObj*, align 8
%_37list_6348116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56248)
store volatile %struct.ScmObj* %_37list_6348116, %struct.ScmObj** %stackaddr$prim57110, align 8
%stackaddr$makeclosure57111 = alloca %struct.ScmObj*, align 8
%fptrToInt57112 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50735 to i64
%ae50735 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57112)
store volatile %struct.ScmObj* %ae50735, %struct.ScmObj** %stackaddr$makeclosure57111, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50735, %struct.ScmObj* %_37foldl148036, i64 0)
%ae50736 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57113 = alloca %struct.ScmObj*, align 8
%fptrToInt57114 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50737 to i64
%ae50737 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57114)
store volatile %struct.ScmObj* %ae50737, %struct.ScmObj** %stackaddr$makeclosure57113, align 8
%argslist56455$ae507350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57115 = alloca %struct.ScmObj*, align 8
%argslist56455$ae507351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50737, %struct.ScmObj* %argslist56455$ae507350)
store volatile %struct.ScmObj* %argslist56455$ae507351, %struct.ScmObj** %stackaddr$prim57115, align 8
%stackaddr$prim57116 = alloca %struct.ScmObj*, align 8
%argslist56455$ae507352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50736, %struct.ScmObj* %argslist56455$ae507351)
store volatile %struct.ScmObj* %argslist56455$ae507352, %struct.ScmObj** %stackaddr$prim57116, align 8
%clofunc57117 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50735)
musttail call tailcc void %clofunc57117(%struct.ScmObj* %ae50735, %struct.ScmObj* %argslist56455$ae507352)
ret void
}

define tailcc void @proc_clo$ae50735(%struct.ScmObj* %env$ae50735,%struct.ScmObj* %current_45args56250) {
%stackaddr$env-ref57118 = alloca %struct.ScmObj*, align 8
%_37foldl148036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50735, i64 0)
store %struct.ScmObj* %_37foldl148036, %struct.ScmObj** %stackaddr$env-ref57118
%stackaddr$prim57119 = alloca %struct.ScmObj*, align 8
%_95k48328 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56250)
store volatile %struct.ScmObj* %_95k48328, %struct.ScmObj** %stackaddr$prim57119, align 8
%stackaddr$prim57120 = alloca %struct.ScmObj*, align 8
%current_45args56251 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56250)
store volatile %struct.ScmObj* %current_45args56251, %struct.ScmObj** %stackaddr$prim57120, align 8
%stackaddr$prim57121 = alloca %struct.ScmObj*, align 8
%_37drop48107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56251)
store volatile %struct.ScmObj* %_37drop48107, %struct.ScmObj** %stackaddr$prim57121, align 8
%stackaddr$makeclosure57122 = alloca %struct.ScmObj*, align 8
%fptrToInt57123 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51271 to i64
%ae51271 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57123)
store volatile %struct.ScmObj* %ae51271, %struct.ScmObj** %stackaddr$makeclosure57122, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51271, %struct.ScmObj* %_37foldl148036, i64 0)
%ae51272 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57124 = alloca %struct.ScmObj*, align 8
%fptrToInt57125 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51273 to i64
%ae51273 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57125)
store volatile %struct.ScmObj* %ae51273, %struct.ScmObj** %stackaddr$makeclosure57124, align 8
%argslist56431$ae512710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57126 = alloca %struct.ScmObj*, align 8
%argslist56431$ae512711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51273, %struct.ScmObj* %argslist56431$ae512710)
store volatile %struct.ScmObj* %argslist56431$ae512711, %struct.ScmObj** %stackaddr$prim57126, align 8
%stackaddr$prim57127 = alloca %struct.ScmObj*, align 8
%argslist56431$ae512712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51272, %struct.ScmObj* %argslist56431$ae512711)
store volatile %struct.ScmObj* %argslist56431$ae512712, %struct.ScmObj** %stackaddr$prim57127, align 8
%clofunc57128 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51271)
musttail call tailcc void %clofunc57128(%struct.ScmObj* %ae51271, %struct.ScmObj* %argslist56431$ae512712)
ret void
}

define tailcc void @proc_clo$ae51271(%struct.ScmObj* %env$ae51271,%struct.ScmObj* %current_45args56253) {
%stackaddr$env-ref57129 = alloca %struct.ScmObj*, align 8
%_37foldl148036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51271, i64 0)
store %struct.ScmObj* %_37foldl148036, %struct.ScmObj** %stackaddr$env-ref57129
%stackaddr$prim57130 = alloca %struct.ScmObj*, align 8
%_95k48329 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56253)
store volatile %struct.ScmObj* %_95k48329, %struct.ScmObj** %stackaddr$prim57130, align 8
%stackaddr$prim57131 = alloca %struct.ScmObj*, align 8
%current_45args56254 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56253)
store volatile %struct.ScmObj* %current_45args56254, %struct.ScmObj** %stackaddr$prim57131, align 8
%stackaddr$prim57132 = alloca %struct.ScmObj*, align 8
%_37memv48100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56254)
store volatile %struct.ScmObj* %_37memv48100, %struct.ScmObj** %stackaddr$prim57132, align 8
%stackaddr$makeclosure57133 = alloca %struct.ScmObj*, align 8
%fptrToInt57134 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51675 to i64
%ae51675 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57134)
store volatile %struct.ScmObj* %ae51675, %struct.ScmObj** %stackaddr$makeclosure57133, align 8
%ae51676 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57135 = alloca %struct.ScmObj*, align 8
%fptrToInt57136 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51677 to i64
%ae51677 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57136)
store volatile %struct.ScmObj* %ae51677, %struct.ScmObj** %stackaddr$makeclosure57135, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51677, %struct.ScmObj* %_37foldl148036, i64 0)
%argslist56405$ae516750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57137 = alloca %struct.ScmObj*, align 8
%argslist56405$ae516751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51677, %struct.ScmObj* %argslist56405$ae516750)
store volatile %struct.ScmObj* %argslist56405$ae516751, %struct.ScmObj** %stackaddr$prim57137, align 8
%stackaddr$prim57138 = alloca %struct.ScmObj*, align 8
%argslist56405$ae516752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51676, %struct.ScmObj* %argslist56405$ae516751)
store volatile %struct.ScmObj* %argslist56405$ae516752, %struct.ScmObj** %stackaddr$prim57138, align 8
%clofunc57139 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51675)
musttail call tailcc void %clofunc57139(%struct.ScmObj* %ae51675, %struct.ScmObj* %argslist56405$ae516752)
ret void
}

define tailcc void @proc_clo$ae51675(%struct.ScmObj* %env$ae51675,%struct.ScmObj* %current_45args56256) {
%stackaddr$prim57140 = alloca %struct.ScmObj*, align 8
%_95k48330 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56256)
store volatile %struct.ScmObj* %_95k48330, %struct.ScmObj** %stackaddr$prim57140, align 8
%stackaddr$prim57141 = alloca %struct.ScmObj*, align 8
%current_45args56257 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56256)
store volatile %struct.ScmObj* %current_45args56257, %struct.ScmObj** %stackaddr$prim57141, align 8
%stackaddr$prim57142 = alloca %struct.ScmObj*, align 8
%_37_4748096 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56257)
store volatile %struct.ScmObj* %_37_4748096, %struct.ScmObj** %stackaddr$prim57142, align 8
%stackaddr$makeclosure57143 = alloca %struct.ScmObj*, align 8
%fptrToInt57144 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51773 to i64
%ae51773 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57144)
store volatile %struct.ScmObj* %ae51773, %struct.ScmObj** %stackaddr$makeclosure57143, align 8
%ae51774 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57145 = alloca %struct.ScmObj*, align 8
%fptrToInt57146 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51775 to i64
%ae51775 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57146)
store volatile %struct.ScmObj* %ae51775, %struct.ScmObj** %stackaddr$makeclosure57145, align 8
%argslist56392$ae517730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57147 = alloca %struct.ScmObj*, align 8
%argslist56392$ae517731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51775, %struct.ScmObj* %argslist56392$ae517730)
store volatile %struct.ScmObj* %argslist56392$ae517731, %struct.ScmObj** %stackaddr$prim57147, align 8
%stackaddr$prim57148 = alloca %struct.ScmObj*, align 8
%argslist56392$ae517732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51774, %struct.ScmObj* %argslist56392$ae517731)
store volatile %struct.ScmObj* %argslist56392$ae517732, %struct.ScmObj** %stackaddr$prim57148, align 8
%clofunc57149 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51773)
musttail call tailcc void %clofunc57149(%struct.ScmObj* %ae51773, %struct.ScmObj* %argslist56392$ae517732)
ret void
}

define tailcc void @proc_clo$ae51773(%struct.ScmObj* %env$ae51773,%struct.ScmObj* %current_45args56259) {
%stackaddr$prim57150 = alloca %struct.ScmObj*, align 8
%_95k48331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56259)
store volatile %struct.ScmObj* %_95k48331, %struct.ScmObj** %stackaddr$prim57150, align 8
%stackaddr$prim57151 = alloca %struct.ScmObj*, align 8
%current_45args56260 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56259)
store volatile %struct.ScmObj* %current_45args56260, %struct.ScmObj** %stackaddr$prim57151, align 8
%stackaddr$prim57152 = alloca %struct.ScmObj*, align 8
%_37first48094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56260)
store volatile %struct.ScmObj* %_37first48094, %struct.ScmObj** %stackaddr$prim57152, align 8
%stackaddr$makeclosure57153 = alloca %struct.ScmObj*, align 8
%fptrToInt57154 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51793 to i64
%ae51793 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57154)
store volatile %struct.ScmObj* %ae51793, %struct.ScmObj** %stackaddr$makeclosure57153, align 8
%ae51794 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57155 = alloca %struct.ScmObj*, align 8
%fptrToInt57156 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51795 to i64
%ae51795 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57156)
store volatile %struct.ScmObj* %ae51795, %struct.ScmObj** %stackaddr$makeclosure57155, align 8
%argslist56387$ae517930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57157 = alloca %struct.ScmObj*, align 8
%argslist56387$ae517931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51795, %struct.ScmObj* %argslist56387$ae517930)
store volatile %struct.ScmObj* %argslist56387$ae517931, %struct.ScmObj** %stackaddr$prim57157, align 8
%stackaddr$prim57158 = alloca %struct.ScmObj*, align 8
%argslist56387$ae517932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51794, %struct.ScmObj* %argslist56387$ae517931)
store volatile %struct.ScmObj* %argslist56387$ae517932, %struct.ScmObj** %stackaddr$prim57158, align 8
%clofunc57159 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51793)
musttail call tailcc void %clofunc57159(%struct.ScmObj* %ae51793, %struct.ScmObj* %argslist56387$ae517932)
ret void
}

define tailcc void @proc_clo$ae51793(%struct.ScmObj* %env$ae51793,%struct.ScmObj* %current_45args56262) {
%stackaddr$prim57160 = alloca %struct.ScmObj*, align 8
%_95k48332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56262)
store volatile %struct.ScmObj* %_95k48332, %struct.ScmObj** %stackaddr$prim57160, align 8
%stackaddr$prim57161 = alloca %struct.ScmObj*, align 8
%current_45args56263 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56262)
store volatile %struct.ScmObj* %current_45args56263, %struct.ScmObj** %stackaddr$prim57161, align 8
%stackaddr$prim57162 = alloca %struct.ScmObj*, align 8
%_37second48092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56263)
store volatile %struct.ScmObj* %_37second48092, %struct.ScmObj** %stackaddr$prim57162, align 8
%stackaddr$makeclosure57163 = alloca %struct.ScmObj*, align 8
%fptrToInt57164 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51815 to i64
%ae51815 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57164)
store volatile %struct.ScmObj* %ae51815, %struct.ScmObj** %stackaddr$makeclosure57163, align 8
%ae51816 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57165 = alloca %struct.ScmObj*, align 8
%fptrToInt57166 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51817 to i64
%ae51817 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57166)
store volatile %struct.ScmObj* %ae51817, %struct.ScmObj** %stackaddr$makeclosure57165, align 8
%argslist56382$ae518150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57167 = alloca %struct.ScmObj*, align 8
%argslist56382$ae518151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51817, %struct.ScmObj* %argslist56382$ae518150)
store volatile %struct.ScmObj* %argslist56382$ae518151, %struct.ScmObj** %stackaddr$prim57167, align 8
%stackaddr$prim57168 = alloca %struct.ScmObj*, align 8
%argslist56382$ae518152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51816, %struct.ScmObj* %argslist56382$ae518151)
store volatile %struct.ScmObj* %argslist56382$ae518152, %struct.ScmObj** %stackaddr$prim57168, align 8
%clofunc57169 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51815)
musttail call tailcc void %clofunc57169(%struct.ScmObj* %ae51815, %struct.ScmObj* %argslist56382$ae518152)
ret void
}

define tailcc void @proc_clo$ae51815(%struct.ScmObj* %env$ae51815,%struct.ScmObj* %current_45args56265) {
%stackaddr$prim57170 = alloca %struct.ScmObj*, align 8
%_95k48333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56265)
store volatile %struct.ScmObj* %_95k48333, %struct.ScmObj** %stackaddr$prim57170, align 8
%stackaddr$prim57171 = alloca %struct.ScmObj*, align 8
%current_45args56266 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56265)
store volatile %struct.ScmObj* %current_45args56266, %struct.ScmObj** %stackaddr$prim57171, align 8
%stackaddr$prim57172 = alloca %struct.ScmObj*, align 8
%_37third48090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56266)
store volatile %struct.ScmObj* %_37third48090, %struct.ScmObj** %stackaddr$prim57172, align 8
%stackaddr$makeclosure57173 = alloca %struct.ScmObj*, align 8
%fptrToInt57174 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51839 to i64
%ae51839 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57174)
store volatile %struct.ScmObj* %ae51839, %struct.ScmObj** %stackaddr$makeclosure57173, align 8
%ae51840 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57175 = alloca %struct.ScmObj*, align 8
%fptrToInt57176 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51841 to i64
%ae51841 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57176)
store volatile %struct.ScmObj* %ae51841, %struct.ScmObj** %stackaddr$makeclosure57175, align 8
%argslist56377$ae518390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57177 = alloca %struct.ScmObj*, align 8
%argslist56377$ae518391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51841, %struct.ScmObj* %argslist56377$ae518390)
store volatile %struct.ScmObj* %argslist56377$ae518391, %struct.ScmObj** %stackaddr$prim57177, align 8
%stackaddr$prim57178 = alloca %struct.ScmObj*, align 8
%argslist56377$ae518392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51840, %struct.ScmObj* %argslist56377$ae518391)
store volatile %struct.ScmObj* %argslist56377$ae518392, %struct.ScmObj** %stackaddr$prim57178, align 8
%clofunc57179 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51839)
musttail call tailcc void %clofunc57179(%struct.ScmObj* %ae51839, %struct.ScmObj* %argslist56377$ae518392)
ret void
}

define tailcc void @proc_clo$ae51839(%struct.ScmObj* %env$ae51839,%struct.ScmObj* %current_45args56268) {
%stackaddr$prim57180 = alloca %struct.ScmObj*, align 8
%_95k48334 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56268)
store volatile %struct.ScmObj* %_95k48334, %struct.ScmObj** %stackaddr$prim57180, align 8
%stackaddr$prim57181 = alloca %struct.ScmObj*, align 8
%current_45args56269 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56268)
store volatile %struct.ScmObj* %current_45args56269, %struct.ScmObj** %stackaddr$prim57181, align 8
%stackaddr$prim57182 = alloca %struct.ScmObj*, align 8
%_37fourth48088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56269)
store volatile %struct.ScmObj* %_37fourth48088, %struct.ScmObj** %stackaddr$prim57182, align 8
%stackaddr$prim57183 = alloca %struct.ScmObj*, align 8
%anf_45bind48275 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48275, %struct.ScmObj** %stackaddr$prim57183, align 8
%ae51865 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57184 = alloca %struct.ScmObj*, align 8
%gen48150 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51865, %struct.ScmObj* %anf_45bind48275)
store volatile %struct.ScmObj* %gen48150, %struct.ScmObj** %stackaddr$prim57184, align 8
%stackaddr$prim57185 = alloca %struct.ScmObj*, align 8
%anf_45bind48276 = call %struct.ScmObj* @prim_void()
store volatile %struct.ScmObj* %anf_45bind48276, %struct.ScmObj** %stackaddr$prim57185, align 8
%ae51867 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57186 = alloca %struct.ScmObj*, align 8
%lazy_45take48149 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51867, %struct.ScmObj* %anf_45bind48276)
store volatile %struct.ScmObj* %lazy_45take48149, %struct.ScmObj** %stackaddr$prim57186, align 8
%stackaddr$makeclosure57187 = alloca %struct.ScmObj*, align 8
%fptrToInt57188 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51869 to i64
%ae51869 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57188)
store volatile %struct.ScmObj* %ae51869, %struct.ScmObj** %stackaddr$makeclosure57187, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51869, %struct.ScmObj* %gen48150, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51869, %struct.ScmObj* %lazy_45take48149, i64 1)
%ae51870 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57189 = alloca %struct.ScmObj*, align 8
%fptrToInt57190 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51871 to i64
%ae51871 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57190)
store volatile %struct.ScmObj* %ae51871, %struct.ScmObj** %stackaddr$makeclosure57189, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51871, %struct.ScmObj* %gen48150, i64 0)
%argslist56372$ae518690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57191 = alloca %struct.ScmObj*, align 8
%argslist56372$ae518691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51871, %struct.ScmObj* %argslist56372$ae518690)
store volatile %struct.ScmObj* %argslist56372$ae518691, %struct.ScmObj** %stackaddr$prim57191, align 8
%stackaddr$prim57192 = alloca %struct.ScmObj*, align 8
%argslist56372$ae518692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51870, %struct.ScmObj* %argslist56372$ae518691)
store volatile %struct.ScmObj* %argslist56372$ae518692, %struct.ScmObj** %stackaddr$prim57192, align 8
%clofunc57193 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51869)
musttail call tailcc void %clofunc57193(%struct.ScmObj* %ae51869, %struct.ScmObj* %argslist56372$ae518692)
ret void
}

define tailcc void @proc_clo$ae51869(%struct.ScmObj* %env$ae51869,%struct.ScmObj* %current_45args56271) {
%stackaddr$env-ref57194 = alloca %struct.ScmObj*, align 8
%gen48150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51869, i64 0)
store %struct.ScmObj* %gen48150, %struct.ScmObj** %stackaddr$env-ref57194
%stackaddr$env-ref57195 = alloca %struct.ScmObj*, align 8
%lazy_45take48149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51869, i64 1)
store %struct.ScmObj* %lazy_45take48149, %struct.ScmObj** %stackaddr$env-ref57195
%stackaddr$prim57196 = alloca %struct.ScmObj*, align 8
%_95k48335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56271)
store volatile %struct.ScmObj* %_95k48335, %struct.ScmObj** %stackaddr$prim57196, align 8
%stackaddr$prim57197 = alloca %struct.ScmObj*, align 8
%current_45args56272 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56271)
store volatile %struct.ScmObj* %current_45args56272, %struct.ScmObj** %stackaddr$prim57197, align 8
%stackaddr$prim57198 = alloca %struct.ScmObj*, align 8
%anf_45bind48282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56272)
store volatile %struct.ScmObj* %anf_45bind48282, %struct.ScmObj** %stackaddr$prim57198, align 8
%ae52014 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57199 = alloca %struct.ScmObj*, align 8
%t4802648160 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %gen48150, %struct.ScmObj* %ae52014, %struct.ScmObj* %anf_45bind48282)
store volatile %struct.ScmObj* %t4802648160, %struct.ScmObj** %stackaddr$prim57199, align 8
%stackaddr$makeclosure57200 = alloca %struct.ScmObj*, align 8
%fptrToInt57201 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52016 to i64
%ae52016 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57201)
store volatile %struct.ScmObj* %ae52016, %struct.ScmObj** %stackaddr$makeclosure57200, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52016, %struct.ScmObj* %gen48150, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52016, %struct.ScmObj* %lazy_45take48149, i64 1)
%ae52017 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57202 = alloca %struct.ScmObj*, align 8
%fptrToInt57203 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52018 to i64
%ae52018 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57203)
store volatile %struct.ScmObj* %ae52018, %struct.ScmObj** %stackaddr$makeclosure57202, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52018, %struct.ScmObj* %lazy_45take48149, i64 0)
%argslist56350$ae520160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57204 = alloca %struct.ScmObj*, align 8
%argslist56350$ae520161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52018, %struct.ScmObj* %argslist56350$ae520160)
store volatile %struct.ScmObj* %argslist56350$ae520161, %struct.ScmObj** %stackaddr$prim57204, align 8
%stackaddr$prim57205 = alloca %struct.ScmObj*, align 8
%argslist56350$ae520162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52017, %struct.ScmObj* %argslist56350$ae520161)
store volatile %struct.ScmObj* %argslist56350$ae520162, %struct.ScmObj** %stackaddr$prim57205, align 8
%clofunc57206 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52016)
musttail call tailcc void %clofunc57206(%struct.ScmObj* %ae52016, %struct.ScmObj* %argslist56350$ae520162)
ret void
}

define tailcc void @proc_clo$ae52016(%struct.ScmObj* %env$ae52016,%struct.ScmObj* %current_45args56274) {
%stackaddr$env-ref57207 = alloca %struct.ScmObj*, align 8
%gen48150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52016, i64 0)
store %struct.ScmObj* %gen48150, %struct.ScmObj** %stackaddr$env-ref57207
%stackaddr$env-ref57208 = alloca %struct.ScmObj*, align 8
%lazy_45take48149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52016, i64 1)
store %struct.ScmObj* %lazy_45take48149, %struct.ScmObj** %stackaddr$env-ref57208
%stackaddr$prim57209 = alloca %struct.ScmObj*, align 8
%_95k48336 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56274)
store volatile %struct.ScmObj* %_95k48336, %struct.ScmObj** %stackaddr$prim57209, align 8
%stackaddr$prim57210 = alloca %struct.ScmObj*, align 8
%current_45args56275 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56274)
store volatile %struct.ScmObj* %current_45args56275, %struct.ScmObj** %stackaddr$prim57210, align 8
%stackaddr$prim57211 = alloca %struct.ScmObj*, align 8
%anf_45bind48299 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56275)
store volatile %struct.ScmObj* %anf_45bind48299, %struct.ScmObj** %stackaddr$prim57211, align 8
%ae52880 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57212 = alloca %struct.ScmObj*, align 8
%t4802548151 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lazy_45take48149, %struct.ScmObj* %ae52880, %struct.ScmObj* %anf_45bind48299)
store volatile %struct.ScmObj* %t4802548151, %struct.ScmObj** %stackaddr$prim57212, align 8
%ae52883 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57213 = alloca %struct.ScmObj*, align 8
%anf_45bind48300 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lazy_45take48149, %struct.ScmObj* %ae52883)
store volatile %struct.ScmObj* %anf_45bind48300, %struct.ScmObj** %stackaddr$prim57213, align 8
%ae52885 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57214 = alloca %struct.ScmObj*, align 8
%anf_45bind48301 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %gen48150, %struct.ScmObj* %ae52885)
store volatile %struct.ScmObj* %anf_45bind48301, %struct.ScmObj** %stackaddr$prim57214, align 8
%stackaddr$makeclosure57215 = alloca %struct.ScmObj*, align 8
%fptrToInt57216 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52887 to i64
%ae52887 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57216)
store volatile %struct.ScmObj* %ae52887, %struct.ScmObj** %stackaddr$makeclosure57215, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52887, %struct.ScmObj* %anf_45bind48300, i64 0)
%ae52888 = call %struct.ScmObj* @const_init_int(i64 8)
%argslist56285$anf_45bind483010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57217 = alloca %struct.ScmObj*, align 8
%argslist56285$anf_45bind483011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52888, %struct.ScmObj* %argslist56285$anf_45bind483010)
store volatile %struct.ScmObj* %argslist56285$anf_45bind483011, %struct.ScmObj** %stackaddr$prim57217, align 8
%stackaddr$prim57218 = alloca %struct.ScmObj*, align 8
%argslist56285$anf_45bind483012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52887, %struct.ScmObj* %argslist56285$anf_45bind483011)
store volatile %struct.ScmObj* %argslist56285$anf_45bind483012, %struct.ScmObj** %stackaddr$prim57218, align 8
%clofunc57219 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48301)
musttail call tailcc void %clofunc57219(%struct.ScmObj* %anf_45bind48301, %struct.ScmObj* %argslist56285$anf_45bind483012)
ret void
}

define tailcc void @proc_clo$ae52887(%struct.ScmObj* %env$ae52887,%struct.ScmObj* %current_45args56277) {
%stackaddr$env-ref57220 = alloca %struct.ScmObj*, align 8
%anf_45bind48300 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52887, i64 0)
store %struct.ScmObj* %anf_45bind48300, %struct.ScmObj** %stackaddr$env-ref57220
%stackaddr$prim57221 = alloca %struct.ScmObj*, align 8
%_95k48337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56277)
store volatile %struct.ScmObj* %_95k48337, %struct.ScmObj** %stackaddr$prim57221, align 8
%stackaddr$prim57222 = alloca %struct.ScmObj*, align 8
%current_45args56278 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56277)
store volatile %struct.ScmObj* %current_45args56278, %struct.ScmObj** %stackaddr$prim57222, align 8
%stackaddr$prim57223 = alloca %struct.ScmObj*, align 8
%anf_45bind48302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56278)
store volatile %struct.ScmObj* %anf_45bind48302, %struct.ScmObj** %stackaddr$prim57223, align 8
%stackaddr$makeclosure57224 = alloca %struct.ScmObj*, align 8
%fptrToInt57225 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52893 to i64
%ae52893 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57225)
store volatile %struct.ScmObj* %ae52893, %struct.ScmObj** %stackaddr$makeclosure57224, align 8
%ae52895 = call %struct.ScmObj* @const_init_int(i64 4)
%argslist56284$anf_45bind483000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57226 = alloca %struct.ScmObj*, align 8
%argslist56284$anf_45bind483001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52895, %struct.ScmObj* %argslist56284$anf_45bind483000)
store volatile %struct.ScmObj* %argslist56284$anf_45bind483001, %struct.ScmObj** %stackaddr$prim57226, align 8
%stackaddr$prim57227 = alloca %struct.ScmObj*, align 8
%argslist56284$anf_45bind483002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48302, %struct.ScmObj* %argslist56284$anf_45bind483001)
store volatile %struct.ScmObj* %argslist56284$anf_45bind483002, %struct.ScmObj** %stackaddr$prim57227, align 8
%stackaddr$prim57228 = alloca %struct.ScmObj*, align 8
%argslist56284$anf_45bind483003 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52893, %struct.ScmObj* %argslist56284$anf_45bind483002)
store volatile %struct.ScmObj* %argslist56284$anf_45bind483003, %struct.ScmObj** %stackaddr$prim57228, align 8
%clofunc57229 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48300)
musttail call tailcc void %clofunc57229(%struct.ScmObj* %anf_45bind48300, %struct.ScmObj* %argslist56284$anf_45bind483003)
ret void
}

define tailcc void @proc_clo$ae52893(%struct.ScmObj* %env$ae52893,%struct.ScmObj* %current_45args56280) {
%stackaddr$prim57230 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56280)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim57230, align 8
%stackaddr$prim57231 = alloca %struct.ScmObj*, align 8
%current_45args56281 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56280)
store volatile %struct.ScmObj* %current_45args56281, %struct.ScmObj** %stackaddr$prim57231, align 8
%stackaddr$prim57232 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56281)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim57232, align 8
%stackaddr$prim57233 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim57233, align 8
%argslist56283$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57234 = alloca %struct.ScmObj*, align 8
%argslist56283$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist56283$k0)
store volatile %struct.ScmObj* %argslist56283$k1, %struct.ScmObj** %stackaddr$prim57234, align 8
%clofunc57235 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc57235(%struct.ScmObj* %k, %struct.ScmObj* %argslist56283$k1)
ret void
}

define tailcc void @proc_clo$ae52018(%struct.ScmObj* %env$ae52018,%struct.ScmObj* %current_45args56286) {
%stackaddr$env-ref57236 = alloca %struct.ScmObj*, align 8
%lazy_45take48149 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52018, i64 0)
store %struct.ScmObj* %lazy_45take48149, %struct.ScmObj** %stackaddr$env-ref57236
%stackaddr$prim57237 = alloca %struct.ScmObj*, align 8
%k48338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56286)
store volatile %struct.ScmObj* %k48338, %struct.ScmObj** %stackaddr$prim57237, align 8
%stackaddr$prim57238 = alloca %struct.ScmObj*, align 8
%current_45args56287 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56286)
store volatile %struct.ScmObj* %current_45args56287, %struct.ScmObj** %stackaddr$prim57238, align 8
%stackaddr$prim57239 = alloca %struct.ScmObj*, align 8
%llst48153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56287)
store volatile %struct.ScmObj* %llst48153, %struct.ScmObj** %stackaddr$prim57239, align 8
%stackaddr$prim57240 = alloca %struct.ScmObj*, align 8
%current_45args56288 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56287)
store volatile %struct.ScmObj* %current_45args56288, %struct.ScmObj** %stackaddr$prim57240, align 8
%stackaddr$prim57241 = alloca %struct.ScmObj*, align 8
%n48152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56288)
store volatile %struct.ScmObj* %n48152, %struct.ScmObj** %stackaddr$prim57241, align 8
%ae52020 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57242 = alloca %struct.ScmObj*, align 8
%anf_45bind48283 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n48152, %struct.ScmObj* %ae52020)
store volatile %struct.ScmObj* %anf_45bind48283, %struct.ScmObj** %stackaddr$prim57242, align 8
%truthy$cmp57243 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48283)
%cmp$cmp57243 = icmp eq i64 %truthy$cmp57243, 1
br i1 %cmp$cmp57243, label %truebranch$cmp57243, label %falsebranch$cmp57243
truebranch$cmp57243:
%stackaddr$makeclosure57244 = alloca %struct.ScmObj*, align 8
%fptrToInt57245 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52022 to i64
%ae52022 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57245)
store volatile %struct.ScmObj* %ae52022, %struct.ScmObj** %stackaddr$makeclosure57244, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52022, %struct.ScmObj* %k48338, i64 0)
%ae52023 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57246 = alloca %struct.ScmObj*, align 8
%fptrToInt57247 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52024 to i64
%ae52024 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57247)
store volatile %struct.ScmObj* %ae52024, %struct.ScmObj** %stackaddr$makeclosure57246, align 8
%argslist56295$ae520220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57248 = alloca %struct.ScmObj*, align 8
%argslist56295$ae520221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52024, %struct.ScmObj* %argslist56295$ae520220)
store volatile %struct.ScmObj* %argslist56295$ae520221, %struct.ScmObj** %stackaddr$prim57248, align 8
%stackaddr$prim57249 = alloca %struct.ScmObj*, align 8
%argslist56295$ae520222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52023, %struct.ScmObj* %argslist56295$ae520221)
store volatile %struct.ScmObj* %argslist56295$ae520222, %struct.ScmObj** %stackaddr$prim57249, align 8
%clofunc57250 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52022)
musttail call tailcc void %clofunc57250(%struct.ScmObj* %ae52022, %struct.ScmObj* %argslist56295$ae520222)
ret void
falsebranch$cmp57243:
%stackaddr$prim57251 = alloca %struct.ScmObj*, align 8
%anf_45bind48285 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %llst48153)
store volatile %struct.ScmObj* %anf_45bind48285, %struct.ScmObj** %stackaddr$prim57251, align 8
%ae52056 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57252 = alloca %struct.ScmObj*, align 8
%anf_45bind48286 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lazy_45take48149, %struct.ScmObj* %ae52056)
store volatile %struct.ScmObj* %anf_45bind48286, %struct.ScmObj** %stackaddr$prim57252, align 8
%stackaddr$prim57253 = alloca %struct.ScmObj*, align 8
%thunk4802748155 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %llst48153)
store volatile %struct.ScmObj* %thunk4802748155, %struct.ScmObj** %stackaddr$prim57253, align 8
%stackaddr$makeclosure57254 = alloca %struct.ScmObj*, align 8
%fptrToInt57255 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52058 to i64
%ae52058 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57255)
store volatile %struct.ScmObj* %ae52058, %struct.ScmObj** %stackaddr$makeclosure57254, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52058, %struct.ScmObj* %n48152, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52058, %struct.ScmObj* %k48338, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52058, %struct.ScmObj* %anf_45bind48286, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52058, %struct.ScmObj* %anf_45bind48285, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52058, %struct.ScmObj* %thunk4802748155, i64 4)
%ae52059 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57256 = alloca %struct.ScmObj*, align 8
%fptrToInt57257 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52060 to i64
%ae52060 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57257)
store volatile %struct.ScmObj* %ae52060, %struct.ScmObj** %stackaddr$makeclosure57256, align 8
%argslist56349$ae520580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57258 = alloca %struct.ScmObj*, align 8
%argslist56349$ae520581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52060, %struct.ScmObj* %argslist56349$ae520580)
store volatile %struct.ScmObj* %argslist56349$ae520581, %struct.ScmObj** %stackaddr$prim57258, align 8
%stackaddr$prim57259 = alloca %struct.ScmObj*, align 8
%argslist56349$ae520582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52059, %struct.ScmObj* %argslist56349$ae520581)
store volatile %struct.ScmObj* %argslist56349$ae520582, %struct.ScmObj** %stackaddr$prim57259, align 8
%clofunc57260 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52058)
musttail call tailcc void %clofunc57260(%struct.ScmObj* %ae52058, %struct.ScmObj* %argslist56349$ae520582)
ret void
}

define tailcc void @proc_clo$ae52022(%struct.ScmObj* %env$ae52022,%struct.ScmObj* %current_45args56290) {
%stackaddr$env-ref57261 = alloca %struct.ScmObj*, align 8
%k48338 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52022, i64 0)
store %struct.ScmObj* %k48338, %struct.ScmObj** %stackaddr$env-ref57261
%stackaddr$prim57262 = alloca %struct.ScmObj*, align 8
%_95k48339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56290)
store volatile %struct.ScmObj* %_95k48339, %struct.ScmObj** %stackaddr$prim57262, align 8
%stackaddr$prim57263 = alloca %struct.ScmObj*, align 8
%current_45args56291 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56290)
store volatile %struct.ScmObj* %current_45args56291, %struct.ScmObj** %stackaddr$prim57263, align 8
%stackaddr$prim57264 = alloca %struct.ScmObj*, align 8
%anf_45bind48284 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56291)
store volatile %struct.ScmObj* %anf_45bind48284, %struct.ScmObj** %stackaddr$prim57264, align 8
%argslist56293$anf_45bind482840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57265 = alloca %struct.ScmObj*, align 8
%argslist56293$anf_45bind482841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48338, %struct.ScmObj* %argslist56293$anf_45bind482840)
store volatile %struct.ScmObj* %argslist56293$anf_45bind482841, %struct.ScmObj** %stackaddr$prim57265, align 8
%clofunc57266 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48284)
musttail call tailcc void %clofunc57266(%struct.ScmObj* %anf_45bind48284, %struct.ScmObj* %argslist56293$anf_45bind482841)
ret void
}

define tailcc void @proc_clo$ae52024(%struct.ScmObj* %env$ae52024,%struct.ScmObj* %lst4815448340) {
%stackaddr$prim57267 = alloca %struct.ScmObj*, align 8
%k48341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4815448340)
store volatile %struct.ScmObj* %k48341, %struct.ScmObj** %stackaddr$prim57267, align 8
%stackaddr$prim57268 = alloca %struct.ScmObj*, align 8
%lst48154 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4815448340)
store volatile %struct.ScmObj* %lst48154, %struct.ScmObj** %stackaddr$prim57268, align 8
%ae52028 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56294$k483410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57269 = alloca %struct.ScmObj*, align 8
%argslist56294$k483411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48154, %struct.ScmObj* %argslist56294$k483410)
store volatile %struct.ScmObj* %argslist56294$k483411, %struct.ScmObj** %stackaddr$prim57269, align 8
%stackaddr$prim57270 = alloca %struct.ScmObj*, align 8
%argslist56294$k483412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52028, %struct.ScmObj* %argslist56294$k483411)
store volatile %struct.ScmObj* %argslist56294$k483412, %struct.ScmObj** %stackaddr$prim57270, align 8
%clofunc57271 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48341)
musttail call tailcc void %clofunc57271(%struct.ScmObj* %k48341, %struct.ScmObj* %argslist56294$k483412)
ret void
}

define tailcc void @proc_clo$ae52058(%struct.ScmObj* %env$ae52058,%struct.ScmObj* %current_45args56296) {
%stackaddr$env-ref57272 = alloca %struct.ScmObj*, align 8
%n48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52058, i64 0)
store %struct.ScmObj* %n48152, %struct.ScmObj** %stackaddr$env-ref57272
%stackaddr$env-ref57273 = alloca %struct.ScmObj*, align 8
%k48338 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52058, i64 1)
store %struct.ScmObj* %k48338, %struct.ScmObj** %stackaddr$env-ref57273
%stackaddr$env-ref57274 = alloca %struct.ScmObj*, align 8
%anf_45bind48286 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52058, i64 2)
store %struct.ScmObj* %anf_45bind48286, %struct.ScmObj** %stackaddr$env-ref57274
%stackaddr$env-ref57275 = alloca %struct.ScmObj*, align 8
%anf_45bind48285 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52058, i64 3)
store %struct.ScmObj* %anf_45bind48285, %struct.ScmObj** %stackaddr$env-ref57275
%stackaddr$env-ref57276 = alloca %struct.ScmObj*, align 8
%thunk4802748155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52058, i64 4)
store %struct.ScmObj* %thunk4802748155, %struct.ScmObj** %stackaddr$env-ref57276
%stackaddr$prim57277 = alloca %struct.ScmObj*, align 8
%_95k48342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56296)
store volatile %struct.ScmObj* %_95k48342, %struct.ScmObj** %stackaddr$prim57277, align 8
%stackaddr$prim57278 = alloca %struct.ScmObj*, align 8
%current_45args56297 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56296)
store volatile %struct.ScmObj* %current_45args56297, %struct.ScmObj** %stackaddr$prim57278, align 8
%stackaddr$prim57279 = alloca %struct.ScmObj*, align 8
%anf_45bind48291 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56297)
store volatile %struct.ScmObj* %anf_45bind48291, %struct.ScmObj** %stackaddr$prim57279, align 8
%stackaddr$makeclosure57280 = alloca %struct.ScmObj*, align 8
%fptrToInt57281 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52146 to i64
%ae52146 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57281)
store volatile %struct.ScmObj* %ae52146, %struct.ScmObj** %stackaddr$makeclosure57280, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52146, %struct.ScmObj* %n48152, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52146, %struct.ScmObj* %k48338, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52146, %struct.ScmObj* %anf_45bind48286, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52146, %struct.ScmObj* %anf_45bind48285, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52146, %struct.ScmObj* %thunk4802748155, i64 4)
%argslist56342$anf_45bind482910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57282 = alloca %struct.ScmObj*, align 8
%argslist56342$anf_45bind482911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4802748155, %struct.ScmObj* %argslist56342$anf_45bind482910)
store volatile %struct.ScmObj* %argslist56342$anf_45bind482911, %struct.ScmObj** %stackaddr$prim57282, align 8
%stackaddr$prim57283 = alloca %struct.ScmObj*, align 8
%argslist56342$anf_45bind482912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52146, %struct.ScmObj* %argslist56342$anf_45bind482911)
store volatile %struct.ScmObj* %argslist56342$anf_45bind482912, %struct.ScmObj** %stackaddr$prim57283, align 8
%clofunc57284 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48291)
musttail call tailcc void %clofunc57284(%struct.ScmObj* %anf_45bind48291, %struct.ScmObj* %argslist56342$anf_45bind482912)
ret void
}

define tailcc void @proc_clo$ae52146(%struct.ScmObj* %env$ae52146,%struct.ScmObj* %current_45args56299) {
%stackaddr$env-ref57285 = alloca %struct.ScmObj*, align 8
%n48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52146, i64 0)
store %struct.ScmObj* %n48152, %struct.ScmObj** %stackaddr$env-ref57285
%stackaddr$env-ref57286 = alloca %struct.ScmObj*, align 8
%k48338 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52146, i64 1)
store %struct.ScmObj* %k48338, %struct.ScmObj** %stackaddr$env-ref57286
%stackaddr$env-ref57287 = alloca %struct.ScmObj*, align 8
%anf_45bind48286 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52146, i64 2)
store %struct.ScmObj* %anf_45bind48286, %struct.ScmObj** %stackaddr$env-ref57287
%stackaddr$env-ref57288 = alloca %struct.ScmObj*, align 8
%anf_45bind48285 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52146, i64 3)
store %struct.ScmObj* %anf_45bind48285, %struct.ScmObj** %stackaddr$env-ref57288
%stackaddr$env-ref57289 = alloca %struct.ScmObj*, align 8
%thunk4802748155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52146, i64 4)
store %struct.ScmObj* %thunk4802748155, %struct.ScmObj** %stackaddr$env-ref57289
%stackaddr$prim57290 = alloca %struct.ScmObj*, align 8
%_95k48343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56299)
store volatile %struct.ScmObj* %_95k48343, %struct.ScmObj** %stackaddr$prim57290, align 8
%stackaddr$prim57291 = alloca %struct.ScmObj*, align 8
%current_45args56300 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56299)
store volatile %struct.ScmObj* %current_45args56300, %struct.ScmObj** %stackaddr$prim57291, align 8
%stackaddr$prim57292 = alloca %struct.ScmObj*, align 8
%anf_45bind48292 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56300)
store volatile %struct.ScmObj* %anf_45bind48292, %struct.ScmObj** %stackaddr$prim57292, align 8
%truthy$cmp57293 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48292)
%cmp$cmp57293 = icmp eq i64 %truthy$cmp57293, 1
br i1 %cmp$cmp57293, label %truebranch$cmp57293, label %falsebranch$cmp57293
truebranch$cmp57293:
%ae52150 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57294 = alloca %struct.ScmObj*, align 8
%anf_45bind48293 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4802748155, %struct.ScmObj* %ae52150)
store volatile %struct.ScmObj* %anf_45bind48293, %struct.ScmObj** %stackaddr$prim57294, align 8
%truthy$cmp57295 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48293)
%cmp$cmp57295 = icmp eq i64 %truthy$cmp57295, 1
br i1 %cmp$cmp57295, label %truebranch$cmp57295, label %falsebranch$cmp57295
truebranch$cmp57295:
%ae52153 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim57296 = alloca %struct.ScmObj*, align 8
%cpsprim48347 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4802748155, %struct.ScmObj* %ae52153)
store volatile %struct.ScmObj* %cpsprim48347, %struct.ScmObj** %stackaddr$prim57296, align 8
%stackaddr$makeclosure57297 = alloca %struct.ScmObj*, align 8
%fptrToInt57298 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52154 to i64
%ae52154 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57298)
store volatile %struct.ScmObj* %ae52154, %struct.ScmObj** %stackaddr$makeclosure57297, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52154, %struct.ScmObj* %n48152, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52154, %struct.ScmObj* %k48338, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52154, %struct.ScmObj* %anf_45bind48286, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52154, %struct.ScmObj* %anf_45bind48285, i64 3)
%ae52155 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56310$ae521540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57299 = alloca %struct.ScmObj*, align 8
%argslist56310$ae521541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48347, %struct.ScmObj* %argslist56310$ae521540)
store volatile %struct.ScmObj* %argslist56310$ae521541, %struct.ScmObj** %stackaddr$prim57299, align 8
%stackaddr$prim57300 = alloca %struct.ScmObj*, align 8
%argslist56310$ae521542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52155, %struct.ScmObj* %argslist56310$ae521541)
store volatile %struct.ScmObj* %argslist56310$ae521542, %struct.ScmObj** %stackaddr$prim57300, align 8
%clofunc57301 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52154)
musttail call tailcc void %clofunc57301(%struct.ScmObj* %ae52154, %struct.ScmObj* %argslist56310$ae521542)
ret void
falsebranch$cmp57295:
%ae52209 = call %struct.ScmObj* @const_init_int(i64 1)
%ae52210 = call %struct.ScmObj* @const_init_true()
%stackaddr$prim57302 = alloca %struct.ScmObj*, align 8
%t4802948157 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4802748155, %struct.ScmObj* %ae52209, %struct.ScmObj* %ae52210)
store volatile %struct.ScmObj* %t4802948157, %struct.ScmObj** %stackaddr$prim57302, align 8
%ae52212 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim57303 = alloca %struct.ScmObj*, align 8
%anf_45bind48294 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4802748155, %struct.ScmObj* %ae52212)
store volatile %struct.ScmObj* %anf_45bind48294, %struct.ScmObj** %stackaddr$prim57303, align 8
%stackaddr$makeclosure57304 = alloca %struct.ScmObj*, align 8
%fptrToInt57305 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52214 to i64
%ae52214 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57305)
store volatile %struct.ScmObj* %ae52214, %struct.ScmObj** %stackaddr$makeclosure57304, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52214, %struct.ScmObj* %n48152, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52214, %struct.ScmObj* %k48338, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52214, %struct.ScmObj* %anf_45bind48286, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52214, %struct.ScmObj* %anf_45bind48285, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52214, %struct.ScmObj* %thunk4802748155, i64 4)
%ae52215 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @global$sym$ae5221557306, i32 0, i32 0))
%argslist56323$anf_45bind482940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57307 = alloca %struct.ScmObj*, align 8
%argslist56323$anf_45bind482941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52215, %struct.ScmObj* %argslist56323$anf_45bind482940)
store volatile %struct.ScmObj* %argslist56323$anf_45bind482941, %struct.ScmObj** %stackaddr$prim57307, align 8
%stackaddr$prim57308 = alloca %struct.ScmObj*, align 8
%argslist56323$anf_45bind482942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52214, %struct.ScmObj* %argslist56323$anf_45bind482941)
store volatile %struct.ScmObj* %argslist56323$anf_45bind482942, %struct.ScmObj** %stackaddr$prim57308, align 8
%clofunc57309 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48294)
musttail call tailcc void %clofunc57309(%struct.ScmObj* %anf_45bind48294, %struct.ScmObj* %argslist56323$anf_45bind482942)
ret void
falsebranch$cmp57293:
%stackaddr$prim57310 = alloca %struct.ScmObj*, align 8
%anf_45bind48295 = call %struct.ScmObj* @prim_number_63(%struct.ScmObj* %thunk4802748155)
store volatile %struct.ScmObj* %anf_45bind48295, %struct.ScmObj** %stackaddr$prim57310, align 8
%truthy$cmp57311 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48295)
%cmp$cmp57311 = icmp eq i64 %truthy$cmp57311, 1
br i1 %cmp$cmp57311, label %truebranch$cmp57311, label %falsebranch$cmp57311
truebranch$cmp57311:
%stackaddr$makeclosure57312 = alloca %struct.ScmObj*, align 8
%fptrToInt57313 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52426 to i64
%ae52426 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57313)
store volatile %struct.ScmObj* %ae52426, %struct.ScmObj** %stackaddr$makeclosure57312, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52426, %struct.ScmObj* %n48152, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52426, %struct.ScmObj* %k48338, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52426, %struct.ScmObj* %anf_45bind48286, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52426, %struct.ScmObj* %anf_45bind48285, i64 3)
%ae52427 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56332$ae524260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57314 = alloca %struct.ScmObj*, align 8
%argslist56332$ae524261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4802748155, %struct.ScmObj* %argslist56332$ae524260)
store volatile %struct.ScmObj* %argslist56332$ae524261, %struct.ScmObj** %stackaddr$prim57314, align 8
%stackaddr$prim57315 = alloca %struct.ScmObj*, align 8
%argslist56332$ae524262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52427, %struct.ScmObj* %argslist56332$ae524261)
store volatile %struct.ScmObj* %argslist56332$ae524262, %struct.ScmObj** %stackaddr$prim57315, align 8
%clofunc57316 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52426)
musttail call tailcc void %clofunc57316(%struct.ScmObj* %ae52426, %struct.ScmObj* %argslist56332$ae524262)
ret void
falsebranch$cmp57311:
%stackaddr$makeclosure57317 = alloca %struct.ScmObj*, align 8
%fptrToInt57318 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52464 to i64
%ae52464 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57318)
store volatile %struct.ScmObj* %ae52464, %struct.ScmObj** %stackaddr$makeclosure57317, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52464, %struct.ScmObj* %n48152, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52464, %struct.ScmObj* %k48338, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52464, %struct.ScmObj* %anf_45bind48286, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52464, %struct.ScmObj* %anf_45bind48285, i64 3)
%ae52465 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52466 = call %struct.ScmObj* @const_init_string(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @global$str$ae5246657319, i32 0, i32 0))
%argslist56341$ae524640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57320 = alloca %struct.ScmObj*, align 8
%argslist56341$ae524641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52466, %struct.ScmObj* %argslist56341$ae524640)
store volatile %struct.ScmObj* %argslist56341$ae524641, %struct.ScmObj** %stackaddr$prim57320, align 8
%stackaddr$prim57321 = alloca %struct.ScmObj*, align 8
%argslist56341$ae524642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52465, %struct.ScmObj* %argslist56341$ae524641)
store volatile %struct.ScmObj* %argslist56341$ae524642, %struct.ScmObj** %stackaddr$prim57321, align 8
%clofunc57322 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52464)
musttail call tailcc void %clofunc57322(%struct.ScmObj* %ae52464, %struct.ScmObj* %argslist56341$ae524642)
ret void
}

define tailcc void @proc_clo$ae52154(%struct.ScmObj* %env$ae52154,%struct.ScmObj* %current_45args56302) {
%stackaddr$env-ref57323 = alloca %struct.ScmObj*, align 8
%n48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52154, i64 0)
store %struct.ScmObj* %n48152, %struct.ScmObj** %stackaddr$env-ref57323
%stackaddr$env-ref57324 = alloca %struct.ScmObj*, align 8
%k48338 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52154, i64 1)
store %struct.ScmObj* %k48338, %struct.ScmObj** %stackaddr$env-ref57324
%stackaddr$env-ref57325 = alloca %struct.ScmObj*, align 8
%anf_45bind48286 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52154, i64 2)
store %struct.ScmObj* %anf_45bind48286, %struct.ScmObj** %stackaddr$env-ref57325
%stackaddr$env-ref57326 = alloca %struct.ScmObj*, align 8
%anf_45bind48285 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52154, i64 3)
store %struct.ScmObj* %anf_45bind48285, %struct.ScmObj** %stackaddr$env-ref57326
%stackaddr$prim57327 = alloca %struct.ScmObj*, align 8
%_95k48344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56302)
store volatile %struct.ScmObj* %_95k48344, %struct.ScmObj** %stackaddr$prim57327, align 8
%stackaddr$prim57328 = alloca %struct.ScmObj*, align 8
%current_45args56303 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56302)
store volatile %struct.ScmObj* %current_45args56303, %struct.ScmObj** %stackaddr$prim57328, align 8
%stackaddr$prim57329 = alloca %struct.ScmObj*, align 8
%anf_45bind48296 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56303)
store volatile %struct.ScmObj* %anf_45bind48296, %struct.ScmObj** %stackaddr$prim57329, align 8
%ae52161 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57330 = alloca %struct.ScmObj*, align 8
%anf_45bind48297 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n48152, %struct.ScmObj* %ae52161)
store volatile %struct.ScmObj* %anf_45bind48297, %struct.ScmObj** %stackaddr$prim57330, align 8
%stackaddr$makeclosure57331 = alloca %struct.ScmObj*, align 8
%fptrToInt57332 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52163 to i64
%ae52163 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57332)
store volatile %struct.ScmObj* %ae52163, %struct.ScmObj** %stackaddr$makeclosure57331, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52163, %struct.ScmObj* %k48338, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52163, %struct.ScmObj* %anf_45bind48285, i64 1)
%argslist56309$anf_45bind482860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57333 = alloca %struct.ScmObj*, align 8
%argslist56309$anf_45bind482861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48297, %struct.ScmObj* %argslist56309$anf_45bind482860)
store volatile %struct.ScmObj* %argslist56309$anf_45bind482861, %struct.ScmObj** %stackaddr$prim57333, align 8
%stackaddr$prim57334 = alloca %struct.ScmObj*, align 8
%argslist56309$anf_45bind482862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48296, %struct.ScmObj* %argslist56309$anf_45bind482861)
store volatile %struct.ScmObj* %argslist56309$anf_45bind482862, %struct.ScmObj** %stackaddr$prim57334, align 8
%stackaddr$prim57335 = alloca %struct.ScmObj*, align 8
%argslist56309$anf_45bind482863 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52163, %struct.ScmObj* %argslist56309$anf_45bind482862)
store volatile %struct.ScmObj* %argslist56309$anf_45bind482863, %struct.ScmObj** %stackaddr$prim57335, align 8
%clofunc57336 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48286)
musttail call tailcc void %clofunc57336(%struct.ScmObj* %anf_45bind48286, %struct.ScmObj* %argslist56309$anf_45bind482863)
ret void
}

define tailcc void @proc_clo$ae52163(%struct.ScmObj* %env$ae52163,%struct.ScmObj* %current_45args56305) {
%stackaddr$env-ref57337 = alloca %struct.ScmObj*, align 8
%k48338 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52163, i64 0)
store %struct.ScmObj* %k48338, %struct.ScmObj** %stackaddr$env-ref57337
%stackaddr$env-ref57338 = alloca %struct.ScmObj*, align 8
%anf_45bind48285 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52163, i64 1)
store %struct.ScmObj* %anf_45bind48285, %struct.ScmObj** %stackaddr$env-ref57338
%stackaddr$prim57339 = alloca %struct.ScmObj*, align 8
%_95k48345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56305)
store volatile %struct.ScmObj* %_95k48345, %struct.ScmObj** %stackaddr$prim57339, align 8
%stackaddr$prim57340 = alloca %struct.ScmObj*, align 8
%current_45args56306 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56305)
store volatile %struct.ScmObj* %current_45args56306, %struct.ScmObj** %stackaddr$prim57340, align 8
%stackaddr$prim57341 = alloca %struct.ScmObj*, align 8
%anf_45bind48298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56306)
store volatile %struct.ScmObj* %anf_45bind48298, %struct.ScmObj** %stackaddr$prim57341, align 8
%stackaddr$prim57342 = alloca %struct.ScmObj*, align 8
%cpsprim48346 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48285, %struct.ScmObj* %anf_45bind48298)
store volatile %struct.ScmObj* %cpsprim48346, %struct.ScmObj** %stackaddr$prim57342, align 8
%ae52169 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56308$k483380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57343 = alloca %struct.ScmObj*, align 8
%argslist56308$k483381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48346, %struct.ScmObj* %argslist56308$k483380)
store volatile %struct.ScmObj* %argslist56308$k483381, %struct.ScmObj** %stackaddr$prim57343, align 8
%stackaddr$prim57344 = alloca %struct.ScmObj*, align 8
%argslist56308$k483382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52169, %struct.ScmObj* %argslist56308$k483381)
store volatile %struct.ScmObj* %argslist56308$k483382, %struct.ScmObj** %stackaddr$prim57344, align 8
%clofunc57345 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48338)
musttail call tailcc void %clofunc57345(%struct.ScmObj* %k48338, %struct.ScmObj* %argslist56308$k483382)
ret void
}

define tailcc void @proc_clo$ae52214(%struct.ScmObj* %env$ae52214,%struct.ScmObj* %current_45args56311) {
%stackaddr$env-ref57346 = alloca %struct.ScmObj*, align 8
%n48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52214, i64 0)
store %struct.ScmObj* %n48152, %struct.ScmObj** %stackaddr$env-ref57346
%stackaddr$env-ref57347 = alloca %struct.ScmObj*, align 8
%k48338 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52214, i64 1)
store %struct.ScmObj* %k48338, %struct.ScmObj** %stackaddr$env-ref57347
%stackaddr$env-ref57348 = alloca %struct.ScmObj*, align 8
%anf_45bind48286 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52214, i64 2)
store %struct.ScmObj* %anf_45bind48286, %struct.ScmObj** %stackaddr$env-ref57348
%stackaddr$env-ref57349 = alloca %struct.ScmObj*, align 8
%anf_45bind48285 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52214, i64 3)
store %struct.ScmObj* %anf_45bind48285, %struct.ScmObj** %stackaddr$env-ref57349
%stackaddr$env-ref57350 = alloca %struct.ScmObj*, align 8
%thunk4802748155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52214, i64 4)
store %struct.ScmObj* %thunk4802748155, %struct.ScmObj** %stackaddr$env-ref57350
%stackaddr$prim57351 = alloca %struct.ScmObj*, align 8
%_95k48348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56311)
store volatile %struct.ScmObj* %_95k48348, %struct.ScmObj** %stackaddr$prim57351, align 8
%stackaddr$prim57352 = alloca %struct.ScmObj*, align 8
%current_45args56312 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56311)
store volatile %struct.ScmObj* %current_45args56312, %struct.ScmObj** %stackaddr$prim57352, align 8
%stackaddr$prim57353 = alloca %struct.ScmObj*, align 8
%val4802848159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56312)
store volatile %struct.ScmObj* %val4802848159, %struct.ScmObj** %stackaddr$prim57353, align 8
%ae52220 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim57354 = alloca %struct.ScmObj*, align 8
%t4803048158 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4802748155, %struct.ScmObj* %ae52220, %struct.ScmObj* %val4802848159)
store volatile %struct.ScmObj* %t4803048158, %struct.ScmObj** %stackaddr$prim57354, align 8
%ae52223 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim57355 = alloca %struct.ScmObj*, align 8
%cpsprim48349 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4802748155, %struct.ScmObj* %ae52223)
store volatile %struct.ScmObj* %cpsprim48349, %struct.ScmObj** %stackaddr$prim57355, align 8
%stackaddr$makeclosure57356 = alloca %struct.ScmObj*, align 8
%fptrToInt57357 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52224 to i64
%ae52224 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57357)
store volatile %struct.ScmObj* %ae52224, %struct.ScmObj** %stackaddr$makeclosure57356, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52224, %struct.ScmObj* %n48152, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52224, %struct.ScmObj* %k48338, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52224, %struct.ScmObj* %anf_45bind48286, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52224, %struct.ScmObj* %anf_45bind48285, i64 3)
%ae52225 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56322$ae522240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57358 = alloca %struct.ScmObj*, align 8
%argslist56322$ae522241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48349, %struct.ScmObj* %argslist56322$ae522240)
store volatile %struct.ScmObj* %argslist56322$ae522241, %struct.ScmObj** %stackaddr$prim57358, align 8
%stackaddr$prim57359 = alloca %struct.ScmObj*, align 8
%argslist56322$ae522242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52225, %struct.ScmObj* %argslist56322$ae522241)
store volatile %struct.ScmObj* %argslist56322$ae522242, %struct.ScmObj** %stackaddr$prim57359, align 8
%clofunc57360 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52224)
musttail call tailcc void %clofunc57360(%struct.ScmObj* %ae52224, %struct.ScmObj* %argslist56322$ae522242)
ret void
}

define tailcc void @proc_clo$ae52224(%struct.ScmObj* %env$ae52224,%struct.ScmObj* %current_45args56314) {
%stackaddr$env-ref57361 = alloca %struct.ScmObj*, align 8
%n48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52224, i64 0)
store %struct.ScmObj* %n48152, %struct.ScmObj** %stackaddr$env-ref57361
%stackaddr$env-ref57362 = alloca %struct.ScmObj*, align 8
%k48338 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52224, i64 1)
store %struct.ScmObj* %k48338, %struct.ScmObj** %stackaddr$env-ref57362
%stackaddr$env-ref57363 = alloca %struct.ScmObj*, align 8
%anf_45bind48286 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52224, i64 2)
store %struct.ScmObj* %anf_45bind48286, %struct.ScmObj** %stackaddr$env-ref57363
%stackaddr$env-ref57364 = alloca %struct.ScmObj*, align 8
%anf_45bind48285 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52224, i64 3)
store %struct.ScmObj* %anf_45bind48285, %struct.ScmObj** %stackaddr$env-ref57364
%stackaddr$prim57365 = alloca %struct.ScmObj*, align 8
%_95k48344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56314)
store volatile %struct.ScmObj* %_95k48344, %struct.ScmObj** %stackaddr$prim57365, align 8
%stackaddr$prim57366 = alloca %struct.ScmObj*, align 8
%current_45args56315 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56314)
store volatile %struct.ScmObj* %current_45args56315, %struct.ScmObj** %stackaddr$prim57366, align 8
%stackaddr$prim57367 = alloca %struct.ScmObj*, align 8
%anf_45bind48296 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56315)
store volatile %struct.ScmObj* %anf_45bind48296, %struct.ScmObj** %stackaddr$prim57367, align 8
%ae52231 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57368 = alloca %struct.ScmObj*, align 8
%anf_45bind48297 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n48152, %struct.ScmObj* %ae52231)
store volatile %struct.ScmObj* %anf_45bind48297, %struct.ScmObj** %stackaddr$prim57368, align 8
%stackaddr$makeclosure57369 = alloca %struct.ScmObj*, align 8
%fptrToInt57370 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52233 to i64
%ae52233 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57370)
store volatile %struct.ScmObj* %ae52233, %struct.ScmObj** %stackaddr$makeclosure57369, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52233, %struct.ScmObj* %k48338, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52233, %struct.ScmObj* %anf_45bind48285, i64 1)
%argslist56321$anf_45bind482860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57371 = alloca %struct.ScmObj*, align 8
%argslist56321$anf_45bind482861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48297, %struct.ScmObj* %argslist56321$anf_45bind482860)
store volatile %struct.ScmObj* %argslist56321$anf_45bind482861, %struct.ScmObj** %stackaddr$prim57371, align 8
%stackaddr$prim57372 = alloca %struct.ScmObj*, align 8
%argslist56321$anf_45bind482862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48296, %struct.ScmObj* %argslist56321$anf_45bind482861)
store volatile %struct.ScmObj* %argslist56321$anf_45bind482862, %struct.ScmObj** %stackaddr$prim57372, align 8
%stackaddr$prim57373 = alloca %struct.ScmObj*, align 8
%argslist56321$anf_45bind482863 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52233, %struct.ScmObj* %argslist56321$anf_45bind482862)
store volatile %struct.ScmObj* %argslist56321$anf_45bind482863, %struct.ScmObj** %stackaddr$prim57373, align 8
%clofunc57374 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48286)
musttail call tailcc void %clofunc57374(%struct.ScmObj* %anf_45bind48286, %struct.ScmObj* %argslist56321$anf_45bind482863)
ret void
}

define tailcc void @proc_clo$ae52233(%struct.ScmObj* %env$ae52233,%struct.ScmObj* %current_45args56317) {
%stackaddr$env-ref57375 = alloca %struct.ScmObj*, align 8
%k48338 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52233, i64 0)
store %struct.ScmObj* %k48338, %struct.ScmObj** %stackaddr$env-ref57375
%stackaddr$env-ref57376 = alloca %struct.ScmObj*, align 8
%anf_45bind48285 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52233, i64 1)
store %struct.ScmObj* %anf_45bind48285, %struct.ScmObj** %stackaddr$env-ref57376
%stackaddr$prim57377 = alloca %struct.ScmObj*, align 8
%_95k48345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56317)
store volatile %struct.ScmObj* %_95k48345, %struct.ScmObj** %stackaddr$prim57377, align 8
%stackaddr$prim57378 = alloca %struct.ScmObj*, align 8
%current_45args56318 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56317)
store volatile %struct.ScmObj* %current_45args56318, %struct.ScmObj** %stackaddr$prim57378, align 8
%stackaddr$prim57379 = alloca %struct.ScmObj*, align 8
%anf_45bind48298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56318)
store volatile %struct.ScmObj* %anf_45bind48298, %struct.ScmObj** %stackaddr$prim57379, align 8
%stackaddr$prim57380 = alloca %struct.ScmObj*, align 8
%cpsprim48346 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48285, %struct.ScmObj* %anf_45bind48298)
store volatile %struct.ScmObj* %cpsprim48346, %struct.ScmObj** %stackaddr$prim57380, align 8
%ae52239 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56320$k483380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57381 = alloca %struct.ScmObj*, align 8
%argslist56320$k483381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48346, %struct.ScmObj* %argslist56320$k483380)
store volatile %struct.ScmObj* %argslist56320$k483381, %struct.ScmObj** %stackaddr$prim57381, align 8
%stackaddr$prim57382 = alloca %struct.ScmObj*, align 8
%argslist56320$k483382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52239, %struct.ScmObj* %argslist56320$k483381)
store volatile %struct.ScmObj* %argslist56320$k483382, %struct.ScmObj** %stackaddr$prim57382, align 8
%clofunc57383 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48338)
musttail call tailcc void %clofunc57383(%struct.ScmObj* %k48338, %struct.ScmObj* %argslist56320$k483382)
ret void
}

define tailcc void @proc_clo$ae52426(%struct.ScmObj* %env$ae52426,%struct.ScmObj* %current_45args56324) {
%stackaddr$env-ref57384 = alloca %struct.ScmObj*, align 8
%n48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52426, i64 0)
store %struct.ScmObj* %n48152, %struct.ScmObj** %stackaddr$env-ref57384
%stackaddr$env-ref57385 = alloca %struct.ScmObj*, align 8
%k48338 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52426, i64 1)
store %struct.ScmObj* %k48338, %struct.ScmObj** %stackaddr$env-ref57385
%stackaddr$env-ref57386 = alloca %struct.ScmObj*, align 8
%anf_45bind48286 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52426, i64 2)
store %struct.ScmObj* %anf_45bind48286, %struct.ScmObj** %stackaddr$env-ref57386
%stackaddr$env-ref57387 = alloca %struct.ScmObj*, align 8
%anf_45bind48285 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52426, i64 3)
store %struct.ScmObj* %anf_45bind48285, %struct.ScmObj** %stackaddr$env-ref57387
%stackaddr$prim57388 = alloca %struct.ScmObj*, align 8
%_95k48344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56324)
store volatile %struct.ScmObj* %_95k48344, %struct.ScmObj** %stackaddr$prim57388, align 8
%stackaddr$prim57389 = alloca %struct.ScmObj*, align 8
%current_45args56325 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56324)
store volatile %struct.ScmObj* %current_45args56325, %struct.ScmObj** %stackaddr$prim57389, align 8
%stackaddr$prim57390 = alloca %struct.ScmObj*, align 8
%anf_45bind48296 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56325)
store volatile %struct.ScmObj* %anf_45bind48296, %struct.ScmObj** %stackaddr$prim57390, align 8
%ae52433 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57391 = alloca %struct.ScmObj*, align 8
%anf_45bind48297 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n48152, %struct.ScmObj* %ae52433)
store volatile %struct.ScmObj* %anf_45bind48297, %struct.ScmObj** %stackaddr$prim57391, align 8
%stackaddr$makeclosure57392 = alloca %struct.ScmObj*, align 8
%fptrToInt57393 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52435 to i64
%ae52435 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57393)
store volatile %struct.ScmObj* %ae52435, %struct.ScmObj** %stackaddr$makeclosure57392, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52435, %struct.ScmObj* %k48338, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52435, %struct.ScmObj* %anf_45bind48285, i64 1)
%argslist56331$anf_45bind482860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57394 = alloca %struct.ScmObj*, align 8
%argslist56331$anf_45bind482861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48297, %struct.ScmObj* %argslist56331$anf_45bind482860)
store volatile %struct.ScmObj* %argslist56331$anf_45bind482861, %struct.ScmObj** %stackaddr$prim57394, align 8
%stackaddr$prim57395 = alloca %struct.ScmObj*, align 8
%argslist56331$anf_45bind482862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48296, %struct.ScmObj* %argslist56331$anf_45bind482861)
store volatile %struct.ScmObj* %argslist56331$anf_45bind482862, %struct.ScmObj** %stackaddr$prim57395, align 8
%stackaddr$prim57396 = alloca %struct.ScmObj*, align 8
%argslist56331$anf_45bind482863 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52435, %struct.ScmObj* %argslist56331$anf_45bind482862)
store volatile %struct.ScmObj* %argslist56331$anf_45bind482863, %struct.ScmObj** %stackaddr$prim57396, align 8
%clofunc57397 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48286)
musttail call tailcc void %clofunc57397(%struct.ScmObj* %anf_45bind48286, %struct.ScmObj* %argslist56331$anf_45bind482863)
ret void
}

define tailcc void @proc_clo$ae52435(%struct.ScmObj* %env$ae52435,%struct.ScmObj* %current_45args56327) {
%stackaddr$env-ref57398 = alloca %struct.ScmObj*, align 8
%k48338 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52435, i64 0)
store %struct.ScmObj* %k48338, %struct.ScmObj** %stackaddr$env-ref57398
%stackaddr$env-ref57399 = alloca %struct.ScmObj*, align 8
%anf_45bind48285 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52435, i64 1)
store %struct.ScmObj* %anf_45bind48285, %struct.ScmObj** %stackaddr$env-ref57399
%stackaddr$prim57400 = alloca %struct.ScmObj*, align 8
%_95k48345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56327)
store volatile %struct.ScmObj* %_95k48345, %struct.ScmObj** %stackaddr$prim57400, align 8
%stackaddr$prim57401 = alloca %struct.ScmObj*, align 8
%current_45args56328 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56327)
store volatile %struct.ScmObj* %current_45args56328, %struct.ScmObj** %stackaddr$prim57401, align 8
%stackaddr$prim57402 = alloca %struct.ScmObj*, align 8
%anf_45bind48298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56328)
store volatile %struct.ScmObj* %anf_45bind48298, %struct.ScmObj** %stackaddr$prim57402, align 8
%stackaddr$prim57403 = alloca %struct.ScmObj*, align 8
%cpsprim48346 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48285, %struct.ScmObj* %anf_45bind48298)
store volatile %struct.ScmObj* %cpsprim48346, %struct.ScmObj** %stackaddr$prim57403, align 8
%ae52441 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56330$k483380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57404 = alloca %struct.ScmObj*, align 8
%argslist56330$k483381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48346, %struct.ScmObj* %argslist56330$k483380)
store volatile %struct.ScmObj* %argslist56330$k483381, %struct.ScmObj** %stackaddr$prim57404, align 8
%stackaddr$prim57405 = alloca %struct.ScmObj*, align 8
%argslist56330$k483382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52441, %struct.ScmObj* %argslist56330$k483381)
store volatile %struct.ScmObj* %argslist56330$k483382, %struct.ScmObj** %stackaddr$prim57405, align 8
%clofunc57406 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48338)
musttail call tailcc void %clofunc57406(%struct.ScmObj* %k48338, %struct.ScmObj* %argslist56330$k483382)
ret void
}

define tailcc void @proc_clo$ae52464(%struct.ScmObj* %env$ae52464,%struct.ScmObj* %current_45args56333) {
%stackaddr$env-ref57407 = alloca %struct.ScmObj*, align 8
%n48152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52464, i64 0)
store %struct.ScmObj* %n48152, %struct.ScmObj** %stackaddr$env-ref57407
%stackaddr$env-ref57408 = alloca %struct.ScmObj*, align 8
%k48338 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52464, i64 1)
store %struct.ScmObj* %k48338, %struct.ScmObj** %stackaddr$env-ref57408
%stackaddr$env-ref57409 = alloca %struct.ScmObj*, align 8
%anf_45bind48286 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52464, i64 2)
store %struct.ScmObj* %anf_45bind48286, %struct.ScmObj** %stackaddr$env-ref57409
%stackaddr$env-ref57410 = alloca %struct.ScmObj*, align 8
%anf_45bind48285 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52464, i64 3)
store %struct.ScmObj* %anf_45bind48285, %struct.ScmObj** %stackaddr$env-ref57410
%stackaddr$prim57411 = alloca %struct.ScmObj*, align 8
%_95k48344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56333)
store volatile %struct.ScmObj* %_95k48344, %struct.ScmObj** %stackaddr$prim57411, align 8
%stackaddr$prim57412 = alloca %struct.ScmObj*, align 8
%current_45args56334 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56333)
store volatile %struct.ScmObj* %current_45args56334, %struct.ScmObj** %stackaddr$prim57412, align 8
%stackaddr$prim57413 = alloca %struct.ScmObj*, align 8
%anf_45bind48296 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56334)
store volatile %struct.ScmObj* %anf_45bind48296, %struct.ScmObj** %stackaddr$prim57413, align 8
%ae52474 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57414 = alloca %struct.ScmObj*, align 8
%anf_45bind48297 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n48152, %struct.ScmObj* %ae52474)
store volatile %struct.ScmObj* %anf_45bind48297, %struct.ScmObj** %stackaddr$prim57414, align 8
%stackaddr$makeclosure57415 = alloca %struct.ScmObj*, align 8
%fptrToInt57416 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52476 to i64
%ae52476 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57416)
store volatile %struct.ScmObj* %ae52476, %struct.ScmObj** %stackaddr$makeclosure57415, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52476, %struct.ScmObj* %k48338, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52476, %struct.ScmObj* %anf_45bind48285, i64 1)
%argslist56340$anf_45bind482860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57417 = alloca %struct.ScmObj*, align 8
%argslist56340$anf_45bind482861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48297, %struct.ScmObj* %argslist56340$anf_45bind482860)
store volatile %struct.ScmObj* %argslist56340$anf_45bind482861, %struct.ScmObj** %stackaddr$prim57417, align 8
%stackaddr$prim57418 = alloca %struct.ScmObj*, align 8
%argslist56340$anf_45bind482862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48296, %struct.ScmObj* %argslist56340$anf_45bind482861)
store volatile %struct.ScmObj* %argslist56340$anf_45bind482862, %struct.ScmObj** %stackaddr$prim57418, align 8
%stackaddr$prim57419 = alloca %struct.ScmObj*, align 8
%argslist56340$anf_45bind482863 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52476, %struct.ScmObj* %argslist56340$anf_45bind482862)
store volatile %struct.ScmObj* %argslist56340$anf_45bind482863, %struct.ScmObj** %stackaddr$prim57419, align 8
%clofunc57420 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48286)
musttail call tailcc void %clofunc57420(%struct.ScmObj* %anf_45bind48286, %struct.ScmObj* %argslist56340$anf_45bind482863)
ret void
}

define tailcc void @proc_clo$ae52476(%struct.ScmObj* %env$ae52476,%struct.ScmObj* %current_45args56336) {
%stackaddr$env-ref57421 = alloca %struct.ScmObj*, align 8
%k48338 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52476, i64 0)
store %struct.ScmObj* %k48338, %struct.ScmObj** %stackaddr$env-ref57421
%stackaddr$env-ref57422 = alloca %struct.ScmObj*, align 8
%anf_45bind48285 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52476, i64 1)
store %struct.ScmObj* %anf_45bind48285, %struct.ScmObj** %stackaddr$env-ref57422
%stackaddr$prim57423 = alloca %struct.ScmObj*, align 8
%_95k48345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56336)
store volatile %struct.ScmObj* %_95k48345, %struct.ScmObj** %stackaddr$prim57423, align 8
%stackaddr$prim57424 = alloca %struct.ScmObj*, align 8
%current_45args56337 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56336)
store volatile %struct.ScmObj* %current_45args56337, %struct.ScmObj** %stackaddr$prim57424, align 8
%stackaddr$prim57425 = alloca %struct.ScmObj*, align 8
%anf_45bind48298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56337)
store volatile %struct.ScmObj* %anf_45bind48298, %struct.ScmObj** %stackaddr$prim57425, align 8
%stackaddr$prim57426 = alloca %struct.ScmObj*, align 8
%cpsprim48346 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48285, %struct.ScmObj* %anf_45bind48298)
store volatile %struct.ScmObj* %cpsprim48346, %struct.ScmObj** %stackaddr$prim57426, align 8
%ae52482 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56339$k483380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57427 = alloca %struct.ScmObj*, align 8
%argslist56339$k483381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48346, %struct.ScmObj* %argslist56339$k483380)
store volatile %struct.ScmObj* %argslist56339$k483381, %struct.ScmObj** %stackaddr$prim57427, align 8
%stackaddr$prim57428 = alloca %struct.ScmObj*, align 8
%argslist56339$k483382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52482, %struct.ScmObj* %argslist56339$k483381)
store volatile %struct.ScmObj* %argslist56339$k483382, %struct.ScmObj** %stackaddr$prim57428, align 8
%clofunc57429 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48338)
musttail call tailcc void %clofunc57429(%struct.ScmObj* %k48338, %struct.ScmObj* %argslist56339$k483382)
ret void
}

define tailcc void @proc_clo$ae52060(%struct.ScmObj* %env$ae52060,%struct.ScmObj* %current_45args56343) {
%stackaddr$prim57430 = alloca %struct.ScmObj*, align 8
%k48350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56343)
store volatile %struct.ScmObj* %k48350, %struct.ScmObj** %stackaddr$prim57430, align 8
%stackaddr$prim57431 = alloca %struct.ScmObj*, align 8
%current_45args56344 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56343)
store volatile %struct.ScmObj* %current_45args56344, %struct.ScmObj** %stackaddr$prim57431, align 8
%stackaddr$prim57432 = alloca %struct.ScmObj*, align 8
%thunk48156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56344)
store volatile %struct.ScmObj* %thunk48156, %struct.ScmObj** %stackaddr$prim57432, align 8
%stackaddr$prim57433 = alloca %struct.ScmObj*, align 8
%anf_45bind48287 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk48156)
store volatile %struct.ScmObj* %anf_45bind48287, %struct.ScmObj** %stackaddr$prim57433, align 8
%truthy$cmp57434 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48287)
%cmp$cmp57434 = icmp eq i64 %truthy$cmp57434, 1
br i1 %cmp$cmp57434, label %truebranch$cmp57434, label %falsebranch$cmp57434
truebranch$cmp57434:
%stackaddr$prim57435 = alloca %struct.ScmObj*, align 8
%anf_45bind48288 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk48156)
store volatile %struct.ScmObj* %anf_45bind48288, %struct.ScmObj** %stackaddr$prim57435, align 8
%ae52065 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim57436 = alloca %struct.ScmObj*, align 8
%anf_45bind48289 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind48288, %struct.ScmObj* %ae52065)
store volatile %struct.ScmObj* %anf_45bind48289, %struct.ScmObj** %stackaddr$prim57436, align 8
%truthy$cmp57437 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48289)
%cmp$cmp57437 = icmp eq i64 %truthy$cmp57437, 1
br i1 %cmp$cmp57437, label %truebranch$cmp57437, label %falsebranch$cmp57437
truebranch$cmp57437:
%ae52068 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57438 = alloca %struct.ScmObj*, align 8
%anf_45bind48290 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk48156, %struct.ScmObj* %ae52068)
store volatile %struct.ScmObj* %anf_45bind48290, %struct.ScmObj** %stackaddr$prim57438, align 8
%ae52070 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae5207057439, i32 0, i32 0))
%stackaddr$prim57440 = alloca %struct.ScmObj*, align 8
%cpsprim48351 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind48290, %struct.ScmObj* %ae52070)
store volatile %struct.ScmObj* %cpsprim48351, %struct.ScmObj** %stackaddr$prim57440, align 8
%ae52072 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56346$k483500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57441 = alloca %struct.ScmObj*, align 8
%argslist56346$k483501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48351, %struct.ScmObj* %argslist56346$k483500)
store volatile %struct.ScmObj* %argslist56346$k483501, %struct.ScmObj** %stackaddr$prim57441, align 8
%stackaddr$prim57442 = alloca %struct.ScmObj*, align 8
%argslist56346$k483502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52072, %struct.ScmObj* %argslist56346$k483501)
store volatile %struct.ScmObj* %argslist56346$k483502, %struct.ScmObj** %stackaddr$prim57442, align 8
%clofunc57443 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48350)
musttail call tailcc void %clofunc57443(%struct.ScmObj* %k48350, %struct.ScmObj* %argslist56346$k483502)
ret void
falsebranch$cmp57437:
%ae52090 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52091 = call %struct.ScmObj* @const_init_false()
%argslist56347$k483500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57444 = alloca %struct.ScmObj*, align 8
%argslist56347$k483501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52091, %struct.ScmObj* %argslist56347$k483500)
store volatile %struct.ScmObj* %argslist56347$k483501, %struct.ScmObj** %stackaddr$prim57444, align 8
%stackaddr$prim57445 = alloca %struct.ScmObj*, align 8
%argslist56347$k483502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52090, %struct.ScmObj* %argslist56347$k483501)
store volatile %struct.ScmObj* %argslist56347$k483502, %struct.ScmObj** %stackaddr$prim57445, align 8
%clofunc57446 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48350)
musttail call tailcc void %clofunc57446(%struct.ScmObj* %k48350, %struct.ScmObj* %argslist56347$k483502)
ret void
falsebranch$cmp57434:
%ae52112 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52113 = call %struct.ScmObj* @const_init_false()
%argslist56348$k483500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57447 = alloca %struct.ScmObj*, align 8
%argslist56348$k483501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52113, %struct.ScmObj* %argslist56348$k483500)
store volatile %struct.ScmObj* %argslist56348$k483501, %struct.ScmObj** %stackaddr$prim57447, align 8
%stackaddr$prim57448 = alloca %struct.ScmObj*, align 8
%argslist56348$k483502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52112, %struct.ScmObj* %argslist56348$k483501)
store volatile %struct.ScmObj* %argslist56348$k483502, %struct.ScmObj** %stackaddr$prim57448, align 8
%clofunc57449 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48350)
musttail call tailcc void %clofunc57449(%struct.ScmObj* %k48350, %struct.ScmObj* %argslist56348$k483502)
ret void
}

define tailcc void @proc_clo$ae51871(%struct.ScmObj* %env$ae51871,%struct.ScmObj* %current_45args56351) {
%stackaddr$env-ref57450 = alloca %struct.ScmObj*, align 8
%gen48150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51871, i64 0)
store %struct.ScmObj* %gen48150, %struct.ScmObj** %stackaddr$env-ref57450
%stackaddr$prim57451 = alloca %struct.ScmObj*, align 8
%k48352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56351)
store volatile %struct.ScmObj* %k48352, %struct.ScmObj** %stackaddr$prim57451, align 8
%stackaddr$prim57452 = alloca %struct.ScmObj*, align 8
%current_45args56352 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56351)
store volatile %struct.ScmObj* %current_45args56352, %struct.ScmObj** %stackaddr$prim57452, align 8
%stackaddr$prim57453 = alloca %struct.ScmObj*, align 8
%n48161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56352)
store volatile %struct.ScmObj* %n48161, %struct.ScmObj** %stackaddr$prim57453, align 8
%stackaddr$makeclosure57454 = alloca %struct.ScmObj*, align 8
%fptrToInt57455 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51872 to i64
%ae51872 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57455)
store volatile %struct.ScmObj* %ae51872, %struct.ScmObj** %stackaddr$makeclosure57454, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51872, %struct.ScmObj* %gen48150, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51872, %struct.ScmObj* %n48161, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51872, %struct.ScmObj* %k48352, i64 2)
%ae51873 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57456 = alloca %struct.ScmObj*, align 8
%fptrToInt57457 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51874 to i64
%ae51874 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57457)
store volatile %struct.ScmObj* %ae51874, %struct.ScmObj** %stackaddr$makeclosure57456, align 8
%argslist56371$ae518720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57458 = alloca %struct.ScmObj*, align 8
%argslist56371$ae518721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51874, %struct.ScmObj* %argslist56371$ae518720)
store volatile %struct.ScmObj* %argslist56371$ae518721, %struct.ScmObj** %stackaddr$prim57458, align 8
%stackaddr$prim57459 = alloca %struct.ScmObj*, align 8
%argslist56371$ae518722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51873, %struct.ScmObj* %argslist56371$ae518721)
store volatile %struct.ScmObj* %argslist56371$ae518722, %struct.ScmObj** %stackaddr$prim57459, align 8
%clofunc57460 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51872)
musttail call tailcc void %clofunc57460(%struct.ScmObj* %ae51872, %struct.ScmObj* %argslist56371$ae518722)
ret void
}

define tailcc void @proc_clo$ae51872(%struct.ScmObj* %env$ae51872,%struct.ScmObj* %current_45args56354) {
%stackaddr$env-ref57461 = alloca %struct.ScmObj*, align 8
%gen48150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51872, i64 0)
store %struct.ScmObj* %gen48150, %struct.ScmObj** %stackaddr$env-ref57461
%stackaddr$env-ref57462 = alloca %struct.ScmObj*, align 8
%n48161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51872, i64 1)
store %struct.ScmObj* %n48161, %struct.ScmObj** %stackaddr$env-ref57462
%stackaddr$env-ref57463 = alloca %struct.ScmObj*, align 8
%k48352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51872, i64 2)
store %struct.ScmObj* %k48352, %struct.ScmObj** %stackaddr$env-ref57463
%stackaddr$prim57464 = alloca %struct.ScmObj*, align 8
%_95k48353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56354)
store volatile %struct.ScmObj* %_95k48353, %struct.ScmObj** %stackaddr$prim57464, align 8
%stackaddr$prim57465 = alloca %struct.ScmObj*, align 8
%current_45args56355 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56354)
store volatile %struct.ScmObj* %current_45args56355, %struct.ScmObj** %stackaddr$prim57465, align 8
%stackaddr$prim57466 = alloca %struct.ScmObj*, align 8
%anf_45bind48277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56355)
store volatile %struct.ScmObj* %anf_45bind48277, %struct.ScmObj** %stackaddr$prim57466, align 8
%stackaddr$makeclosure57467 = alloca %struct.ScmObj*, align 8
%fptrToInt57468 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51896 to i64
%ae51896 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57468)
store volatile %struct.ScmObj* %ae51896, %struct.ScmObj** %stackaddr$makeclosure57467, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51896, %struct.ScmObj* %anf_45bind48277, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51896, %struct.ScmObj* %n48161, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51896, %struct.ScmObj* %k48352, i64 2)
%ae51897 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57469 = alloca %struct.ScmObj*, align 8
%fptrToInt57470 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51898 to i64
%ae51898 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57470)
store volatile %struct.ScmObj* %ae51898, %struct.ScmObj** %stackaddr$makeclosure57469, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51898, %struct.ScmObj* %gen48150, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51898, %struct.ScmObj* %n48161, i64 1)
%argslist56369$ae518960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57471 = alloca %struct.ScmObj*, align 8
%argslist56369$ae518961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51898, %struct.ScmObj* %argslist56369$ae518960)
store volatile %struct.ScmObj* %argslist56369$ae518961, %struct.ScmObj** %stackaddr$prim57471, align 8
%stackaddr$prim57472 = alloca %struct.ScmObj*, align 8
%argslist56369$ae518962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51897, %struct.ScmObj* %argslist56369$ae518961)
store volatile %struct.ScmObj* %argslist56369$ae518962, %struct.ScmObj** %stackaddr$prim57472, align 8
%clofunc57473 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51896)
musttail call tailcc void %clofunc57473(%struct.ScmObj* %ae51896, %struct.ScmObj* %argslist56369$ae518962)
ret void
}

define tailcc void @proc_clo$ae51896(%struct.ScmObj* %env$ae51896,%struct.ScmObj* %current_45args56357) {
%stackaddr$env-ref57474 = alloca %struct.ScmObj*, align 8
%anf_45bind48277 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51896, i64 0)
store %struct.ScmObj* %anf_45bind48277, %struct.ScmObj** %stackaddr$env-ref57474
%stackaddr$env-ref57475 = alloca %struct.ScmObj*, align 8
%n48161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51896, i64 1)
store %struct.ScmObj* %n48161, %struct.ScmObj** %stackaddr$env-ref57475
%stackaddr$env-ref57476 = alloca %struct.ScmObj*, align 8
%k48352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51896, i64 2)
store %struct.ScmObj* %k48352, %struct.ScmObj** %stackaddr$env-ref57476
%stackaddr$prim57477 = alloca %struct.ScmObj*, align 8
%_95k48354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56357)
store volatile %struct.ScmObj* %_95k48354, %struct.ScmObj** %stackaddr$prim57477, align 8
%stackaddr$prim57478 = alloca %struct.ScmObj*, align 8
%current_45args56358 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56357)
store volatile %struct.ScmObj* %current_45args56358, %struct.ScmObj** %stackaddr$prim57478, align 8
%stackaddr$prim57479 = alloca %struct.ScmObj*, align 8
%anf_45bind48280 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56358)
store volatile %struct.ScmObj* %anf_45bind48280, %struct.ScmObj** %stackaddr$prim57479, align 8
%stackaddr$makeclosure57480 = alloca %struct.ScmObj*, align 8
%fptrToInt57481 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51932 to i64
%ae51932 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57481)
store volatile %struct.ScmObj* %ae51932, %struct.ScmObj** %stackaddr$makeclosure57480, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51932, %struct.ScmObj* %n48161, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51932, %struct.ScmObj* %k48352, i64 1)
%ae51933 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae5193357482, i32 0, i32 0))
%ae51934 = call %struct.ScmObj* @const_init_false()
%argslist56364$anf_45bind482770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57483 = alloca %struct.ScmObj*, align 8
%argslist56364$anf_45bind482771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48280, %struct.ScmObj* %argslist56364$anf_45bind482770)
store volatile %struct.ScmObj* %argslist56364$anf_45bind482771, %struct.ScmObj** %stackaddr$prim57483, align 8
%stackaddr$prim57484 = alloca %struct.ScmObj*, align 8
%argslist56364$anf_45bind482772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51934, %struct.ScmObj* %argslist56364$anf_45bind482771)
store volatile %struct.ScmObj* %argslist56364$anf_45bind482772, %struct.ScmObj** %stackaddr$prim57484, align 8
%stackaddr$prim57485 = alloca %struct.ScmObj*, align 8
%argslist56364$anf_45bind482773 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51933, %struct.ScmObj* %argslist56364$anf_45bind482772)
store volatile %struct.ScmObj* %argslist56364$anf_45bind482773, %struct.ScmObj** %stackaddr$prim57485, align 8
%stackaddr$prim57486 = alloca %struct.ScmObj*, align 8
%argslist56364$anf_45bind482774 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51932, %struct.ScmObj* %argslist56364$anf_45bind482773)
store volatile %struct.ScmObj* %argslist56364$anf_45bind482774, %struct.ScmObj** %stackaddr$prim57486, align 8
%clofunc57487 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48277)
musttail call tailcc void %clofunc57487(%struct.ScmObj* %anf_45bind48277, %struct.ScmObj* %argslist56364$anf_45bind482774)
ret void
}

define tailcc void @proc_clo$ae51932(%struct.ScmObj* %env$ae51932,%struct.ScmObj* %current_45args56360) {
%stackaddr$env-ref57488 = alloca %struct.ScmObj*, align 8
%n48161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51932, i64 0)
store %struct.ScmObj* %n48161, %struct.ScmObj** %stackaddr$env-ref57488
%stackaddr$env-ref57489 = alloca %struct.ScmObj*, align 8
%k48352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51932, i64 1)
store %struct.ScmObj* %k48352, %struct.ScmObj** %stackaddr$env-ref57489
%stackaddr$prim57490 = alloca %struct.ScmObj*, align 8
%_95k48355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56360)
store volatile %struct.ScmObj* %_95k48355, %struct.ScmObj** %stackaddr$prim57490, align 8
%stackaddr$prim57491 = alloca %struct.ScmObj*, align 8
%current_45args56361 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56360)
store volatile %struct.ScmObj* %current_45args56361, %struct.ScmObj** %stackaddr$prim57491, align 8
%stackaddr$prim57492 = alloca %struct.ScmObj*, align 8
%anf_45bind48281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56361)
store volatile %struct.ScmObj* %anf_45bind48281, %struct.ScmObj** %stackaddr$prim57492, align 8
%stackaddr$prim57493 = alloca %struct.ScmObj*, align 8
%cpsprim48356 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %n48161, %struct.ScmObj* %anf_45bind48281)
store volatile %struct.ScmObj* %cpsprim48356, %struct.ScmObj** %stackaddr$prim57493, align 8
%ae51949 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56363$k483520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57494 = alloca %struct.ScmObj*, align 8
%argslist56363$k483521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48356, %struct.ScmObj* %argslist56363$k483520)
store volatile %struct.ScmObj* %argslist56363$k483521, %struct.ScmObj** %stackaddr$prim57494, align 8
%stackaddr$prim57495 = alloca %struct.ScmObj*, align 8
%argslist56363$k483522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51949, %struct.ScmObj* %argslist56363$k483521)
store volatile %struct.ScmObj* %argslist56363$k483522, %struct.ScmObj** %stackaddr$prim57495, align 8
%clofunc57496 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48352)
musttail call tailcc void %clofunc57496(%struct.ScmObj* %k48352, %struct.ScmObj* %argslist56363$k483522)
ret void
}

define tailcc void @proc_clo$ae51898(%struct.ScmObj* %env$ae51898,%struct.ScmObj* %current_45args56365) {
%stackaddr$env-ref57497 = alloca %struct.ScmObj*, align 8
%gen48150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51898, i64 0)
store %struct.ScmObj* %gen48150, %struct.ScmObj** %stackaddr$env-ref57497
%stackaddr$env-ref57498 = alloca %struct.ScmObj*, align 8
%n48161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51898, i64 1)
store %struct.ScmObj* %n48161, %struct.ScmObj** %stackaddr$env-ref57498
%stackaddr$prim57499 = alloca %struct.ScmObj*, align 8
%k48357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56365)
store volatile %struct.ScmObj* %k48357, %struct.ScmObj** %stackaddr$prim57499, align 8
%stackaddr$prim57500 = alloca %struct.ScmObj*, align 8
%current_45args56366 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56365)
store volatile %struct.ScmObj* %current_45args56366, %struct.ScmObj** %stackaddr$prim57500, align 8
%stackaddr$prim57501 = alloca %struct.ScmObj*, align 8
%_9548163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56366)
store volatile %struct.ScmObj* %_9548163, %struct.ScmObj** %stackaddr$prim57501, align 8
%ae51900 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57502 = alloca %struct.ScmObj*, align 8
%anf_45bind48278 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %gen48150, %struct.ScmObj* %ae51900)
store volatile %struct.ScmObj* %anf_45bind48278, %struct.ScmObj** %stackaddr$prim57502, align 8
%ae51902 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57503 = alloca %struct.ScmObj*, align 8
%anf_45bind48279 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n48161, %struct.ScmObj* %ae51902)
store volatile %struct.ScmObj* %anf_45bind48279, %struct.ScmObj** %stackaddr$prim57503, align 8
%argslist56368$anf_45bind482780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57504 = alloca %struct.ScmObj*, align 8
%argslist56368$anf_45bind482781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48279, %struct.ScmObj* %argslist56368$anf_45bind482780)
store volatile %struct.ScmObj* %argslist56368$anf_45bind482781, %struct.ScmObj** %stackaddr$prim57504, align 8
%stackaddr$prim57505 = alloca %struct.ScmObj*, align 8
%argslist56368$anf_45bind482782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48357, %struct.ScmObj* %argslist56368$anf_45bind482781)
store volatile %struct.ScmObj* %argslist56368$anf_45bind482782, %struct.ScmObj** %stackaddr$prim57505, align 8
%clofunc57506 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48278)
musttail call tailcc void %clofunc57506(%struct.ScmObj* %anf_45bind48278, %struct.ScmObj* %argslist56368$anf_45bind482782)
ret void
}

define tailcc void @proc_clo$ae51874(%struct.ScmObj* %env$ae51874,%struct.ScmObj* %el4816248358) {
%stackaddr$prim57507 = alloca %struct.ScmObj*, align 8
%k48359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %el4816248358)
store volatile %struct.ScmObj* %k48359, %struct.ScmObj** %stackaddr$prim57507, align 8
%stackaddr$prim57508 = alloca %struct.ScmObj*, align 8
%el48162 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %el4816248358)
store volatile %struct.ScmObj* %el48162, %struct.ScmObj** %stackaddr$prim57508, align 8
%stackaddr$applyprim57509 = alloca %struct.ScmObj*, align 8
%cpsaprim48360 = call %struct.ScmObj* @applyprim_vector(%struct.ScmObj* %el48162)
store volatile %struct.ScmObj* %cpsaprim48360, %struct.ScmObj** %stackaddr$applyprim57509, align 8
%ae51879 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56370$k483590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57510 = alloca %struct.ScmObj*, align 8
%argslist56370$k483591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim48360, %struct.ScmObj* %argslist56370$k483590)
store volatile %struct.ScmObj* %argslist56370$k483591, %struct.ScmObj** %stackaddr$prim57510, align 8
%stackaddr$prim57511 = alloca %struct.ScmObj*, align 8
%argslist56370$k483592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51879, %struct.ScmObj* %argslist56370$k483591)
store volatile %struct.ScmObj* %argslist56370$k483592, %struct.ScmObj** %stackaddr$prim57511, align 8
%clofunc57512 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48359)
musttail call tailcc void %clofunc57512(%struct.ScmObj* %k48359, %struct.ScmObj* %argslist56370$k483592)
ret void
}

define tailcc void @proc_clo$ae51841(%struct.ScmObj* %env$ae51841,%struct.ScmObj* %current_45args56373) {
%stackaddr$prim57513 = alloca %struct.ScmObj*, align 8
%k48361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56373)
store volatile %struct.ScmObj* %k48361, %struct.ScmObj** %stackaddr$prim57513, align 8
%stackaddr$prim57514 = alloca %struct.ScmObj*, align 8
%current_45args56374 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56373)
store volatile %struct.ScmObj* %current_45args56374, %struct.ScmObj** %stackaddr$prim57514, align 8
%stackaddr$prim57515 = alloca %struct.ScmObj*, align 8
%x48089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56374)
store volatile %struct.ScmObj* %x48089, %struct.ScmObj** %stackaddr$prim57515, align 8
%stackaddr$prim57516 = alloca %struct.ScmObj*, align 8
%anf_45bind48272 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48089)
store volatile %struct.ScmObj* %anf_45bind48272, %struct.ScmObj** %stackaddr$prim57516, align 8
%stackaddr$prim57517 = alloca %struct.ScmObj*, align 8
%anf_45bind48273 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48272)
store volatile %struct.ScmObj* %anf_45bind48273, %struct.ScmObj** %stackaddr$prim57517, align 8
%stackaddr$prim57518 = alloca %struct.ScmObj*, align 8
%anf_45bind48274 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48273)
store volatile %struct.ScmObj* %anf_45bind48274, %struct.ScmObj** %stackaddr$prim57518, align 8
%stackaddr$prim57519 = alloca %struct.ScmObj*, align 8
%cpsprim48362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48274)
store volatile %struct.ScmObj* %cpsprim48362, %struct.ScmObj** %stackaddr$prim57519, align 8
%ae51847 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56376$k483610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57520 = alloca %struct.ScmObj*, align 8
%argslist56376$k483611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48362, %struct.ScmObj* %argslist56376$k483610)
store volatile %struct.ScmObj* %argslist56376$k483611, %struct.ScmObj** %stackaddr$prim57520, align 8
%stackaddr$prim57521 = alloca %struct.ScmObj*, align 8
%argslist56376$k483612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51847, %struct.ScmObj* %argslist56376$k483611)
store volatile %struct.ScmObj* %argslist56376$k483612, %struct.ScmObj** %stackaddr$prim57521, align 8
%clofunc57522 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48361)
musttail call tailcc void %clofunc57522(%struct.ScmObj* %k48361, %struct.ScmObj* %argslist56376$k483612)
ret void
}

define tailcc void @proc_clo$ae51817(%struct.ScmObj* %env$ae51817,%struct.ScmObj* %current_45args56378) {
%stackaddr$prim57523 = alloca %struct.ScmObj*, align 8
%k48363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56378)
store volatile %struct.ScmObj* %k48363, %struct.ScmObj** %stackaddr$prim57523, align 8
%stackaddr$prim57524 = alloca %struct.ScmObj*, align 8
%current_45args56379 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56378)
store volatile %struct.ScmObj* %current_45args56379, %struct.ScmObj** %stackaddr$prim57524, align 8
%stackaddr$prim57525 = alloca %struct.ScmObj*, align 8
%x48091 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56379)
store volatile %struct.ScmObj* %x48091, %struct.ScmObj** %stackaddr$prim57525, align 8
%stackaddr$prim57526 = alloca %struct.ScmObj*, align 8
%anf_45bind48270 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48091)
store volatile %struct.ScmObj* %anf_45bind48270, %struct.ScmObj** %stackaddr$prim57526, align 8
%stackaddr$prim57527 = alloca %struct.ScmObj*, align 8
%anf_45bind48271 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48270)
store volatile %struct.ScmObj* %anf_45bind48271, %struct.ScmObj** %stackaddr$prim57527, align 8
%stackaddr$prim57528 = alloca %struct.ScmObj*, align 8
%cpsprim48364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48271)
store volatile %struct.ScmObj* %cpsprim48364, %struct.ScmObj** %stackaddr$prim57528, align 8
%ae51822 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56381$k483630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57529 = alloca %struct.ScmObj*, align 8
%argslist56381$k483631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48364, %struct.ScmObj* %argslist56381$k483630)
store volatile %struct.ScmObj* %argslist56381$k483631, %struct.ScmObj** %stackaddr$prim57529, align 8
%stackaddr$prim57530 = alloca %struct.ScmObj*, align 8
%argslist56381$k483632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51822, %struct.ScmObj* %argslist56381$k483631)
store volatile %struct.ScmObj* %argslist56381$k483632, %struct.ScmObj** %stackaddr$prim57530, align 8
%clofunc57531 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48363)
musttail call tailcc void %clofunc57531(%struct.ScmObj* %k48363, %struct.ScmObj* %argslist56381$k483632)
ret void
}

define tailcc void @proc_clo$ae51795(%struct.ScmObj* %env$ae51795,%struct.ScmObj* %current_45args56383) {
%stackaddr$prim57532 = alloca %struct.ScmObj*, align 8
%k48365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56383)
store volatile %struct.ScmObj* %k48365, %struct.ScmObj** %stackaddr$prim57532, align 8
%stackaddr$prim57533 = alloca %struct.ScmObj*, align 8
%current_45args56384 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56383)
store volatile %struct.ScmObj* %current_45args56384, %struct.ScmObj** %stackaddr$prim57533, align 8
%stackaddr$prim57534 = alloca %struct.ScmObj*, align 8
%x48093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56384)
store volatile %struct.ScmObj* %x48093, %struct.ScmObj** %stackaddr$prim57534, align 8
%stackaddr$prim57535 = alloca %struct.ScmObj*, align 8
%anf_45bind48269 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48093)
store volatile %struct.ScmObj* %anf_45bind48269, %struct.ScmObj** %stackaddr$prim57535, align 8
%stackaddr$prim57536 = alloca %struct.ScmObj*, align 8
%cpsprim48366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48269)
store volatile %struct.ScmObj* %cpsprim48366, %struct.ScmObj** %stackaddr$prim57536, align 8
%ae51799 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56386$k483650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57537 = alloca %struct.ScmObj*, align 8
%argslist56386$k483651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48366, %struct.ScmObj* %argslist56386$k483650)
store volatile %struct.ScmObj* %argslist56386$k483651, %struct.ScmObj** %stackaddr$prim57537, align 8
%stackaddr$prim57538 = alloca %struct.ScmObj*, align 8
%argslist56386$k483652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51799, %struct.ScmObj* %argslist56386$k483651)
store volatile %struct.ScmObj* %argslist56386$k483652, %struct.ScmObj** %stackaddr$prim57538, align 8
%clofunc57539 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48365)
musttail call tailcc void %clofunc57539(%struct.ScmObj* %k48365, %struct.ScmObj* %argslist56386$k483652)
ret void
}

define tailcc void @proc_clo$ae51775(%struct.ScmObj* %env$ae51775,%struct.ScmObj* %current_45args56388) {
%stackaddr$prim57540 = alloca %struct.ScmObj*, align 8
%k48367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56388)
store volatile %struct.ScmObj* %k48367, %struct.ScmObj** %stackaddr$prim57540, align 8
%stackaddr$prim57541 = alloca %struct.ScmObj*, align 8
%current_45args56389 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56388)
store volatile %struct.ScmObj* %current_45args56389, %struct.ScmObj** %stackaddr$prim57541, align 8
%stackaddr$prim57542 = alloca %struct.ScmObj*, align 8
%x48095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56389)
store volatile %struct.ScmObj* %x48095, %struct.ScmObj** %stackaddr$prim57542, align 8
%stackaddr$prim57543 = alloca %struct.ScmObj*, align 8
%cpsprim48368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48095)
store volatile %struct.ScmObj* %cpsprim48368, %struct.ScmObj** %stackaddr$prim57543, align 8
%ae51778 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56391$k483670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57544 = alloca %struct.ScmObj*, align 8
%argslist56391$k483671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48368, %struct.ScmObj* %argslist56391$k483670)
store volatile %struct.ScmObj* %argslist56391$k483671, %struct.ScmObj** %stackaddr$prim57544, align 8
%stackaddr$prim57545 = alloca %struct.ScmObj*, align 8
%argslist56391$k483672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51778, %struct.ScmObj* %argslist56391$k483671)
store volatile %struct.ScmObj* %argslist56391$k483672, %struct.ScmObj** %stackaddr$prim57545, align 8
%clofunc57546 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48367)
musttail call tailcc void %clofunc57546(%struct.ScmObj* %k48367, %struct.ScmObj* %argslist56391$k483672)
ret void
}

define tailcc void @proc_clo$ae51677(%struct.ScmObj* %env$ae51677,%struct.ScmObj* %args4809748369) {
%stackaddr$env-ref57547 = alloca %struct.ScmObj*, align 8
%_37foldl148036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51677, i64 0)
store %struct.ScmObj* %_37foldl148036, %struct.ScmObj** %stackaddr$env-ref57547
%stackaddr$prim57548 = alloca %struct.ScmObj*, align 8
%k48370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4809748369)
store volatile %struct.ScmObj* %k48370, %struct.ScmObj** %stackaddr$prim57548, align 8
%stackaddr$prim57549 = alloca %struct.ScmObj*, align 8
%args48097 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4809748369)
store volatile %struct.ScmObj* %args48097, %struct.ScmObj** %stackaddr$prim57549, align 8
%stackaddr$prim57550 = alloca %struct.ScmObj*, align 8
%anf_45bind48263 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args48097)
store volatile %struct.ScmObj* %anf_45bind48263, %struct.ScmObj** %stackaddr$prim57550, align 8
%truthy$cmp57551 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48263)
%cmp$cmp57551 = icmp eq i64 %truthy$cmp57551, 1
br i1 %cmp$cmp57551, label %truebranch$cmp57551, label %falsebranch$cmp57551
truebranch$cmp57551:
%ae51683 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51684 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist56393$k483700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57552 = alloca %struct.ScmObj*, align 8
%argslist56393$k483701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51684, %struct.ScmObj* %argslist56393$k483700)
store volatile %struct.ScmObj* %argslist56393$k483701, %struct.ScmObj** %stackaddr$prim57552, align 8
%stackaddr$prim57553 = alloca %struct.ScmObj*, align 8
%argslist56393$k483702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51683, %struct.ScmObj* %argslist56393$k483701)
store volatile %struct.ScmObj* %argslist56393$k483702, %struct.ScmObj** %stackaddr$prim57553, align 8
%clofunc57554 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48370)
musttail call tailcc void %clofunc57554(%struct.ScmObj* %k48370, %struct.ScmObj* %argslist56393$k483702)
ret void
falsebranch$cmp57551:
%stackaddr$prim57555 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48097)
store volatile %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$prim57555, align 8
%stackaddr$prim57556 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48264)
store volatile %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$prim57556, align 8
%truthy$cmp57557 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48265)
%cmp$cmp57557 = icmp eq i64 %truthy$cmp57557, 1
br i1 %cmp$cmp57557, label %truebranch$cmp57557, label %falsebranch$cmp57557
truebranch$cmp57557:
%stackaddr$prim57558 = alloca %struct.ScmObj*, align 8
%cpsprim48371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48097)
store volatile %struct.ScmObj* %cpsprim48371, %struct.ScmObj** %stackaddr$prim57558, align 8
%ae51696 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56394$k483700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57559 = alloca %struct.ScmObj*, align 8
%argslist56394$k483701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48371, %struct.ScmObj* %argslist56394$k483700)
store volatile %struct.ScmObj* %argslist56394$k483701, %struct.ScmObj** %stackaddr$prim57559, align 8
%stackaddr$prim57560 = alloca %struct.ScmObj*, align 8
%argslist56394$k483702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51696, %struct.ScmObj* %argslist56394$k483701)
store volatile %struct.ScmObj* %argslist56394$k483702, %struct.ScmObj** %stackaddr$prim57560, align 8
%clofunc57561 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48370)
musttail call tailcc void %clofunc57561(%struct.ScmObj* %k48370, %struct.ScmObj* %argslist56394$k483702)
ret void
falsebranch$cmp57557:
%stackaddr$makeclosure57562 = alloca %struct.ScmObj*, align 8
%fptrToInt57563 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51701 to i64
%ae51701 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57563)
store volatile %struct.ScmObj* %ae51701, %struct.ScmObj** %stackaddr$makeclosure57562, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51701, %struct.ScmObj* %_37foldl148036, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51701, %struct.ScmObj* %k48370, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51701, %struct.ScmObj* %args48097, i64 2)
%ae51702 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57564 = alloca %struct.ScmObj*, align 8
%fptrToInt57565 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51703 to i64
%ae51703 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57565)
store volatile %struct.ScmObj* %ae51703, %struct.ScmObj** %stackaddr$makeclosure57564, align 8
%argslist56404$ae517010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57566 = alloca %struct.ScmObj*, align 8
%argslist56404$ae517011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51703, %struct.ScmObj* %argslist56404$ae517010)
store volatile %struct.ScmObj* %argslist56404$ae517011, %struct.ScmObj** %stackaddr$prim57566, align 8
%stackaddr$prim57567 = alloca %struct.ScmObj*, align 8
%argslist56404$ae517012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51702, %struct.ScmObj* %argslist56404$ae517011)
store volatile %struct.ScmObj* %argslist56404$ae517012, %struct.ScmObj** %stackaddr$prim57567, align 8
%clofunc57568 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51701)
musttail call tailcc void %clofunc57568(%struct.ScmObj* %ae51701, %struct.ScmObj* %argslist56404$ae517012)
ret void
}

define tailcc void @proc_clo$ae51701(%struct.ScmObj* %env$ae51701,%struct.ScmObj* %current_45args56395) {
%stackaddr$env-ref57569 = alloca %struct.ScmObj*, align 8
%_37foldl148036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51701, i64 0)
store %struct.ScmObj* %_37foldl148036, %struct.ScmObj** %stackaddr$env-ref57569
%stackaddr$env-ref57570 = alloca %struct.ScmObj*, align 8
%k48370 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51701, i64 1)
store %struct.ScmObj* %k48370, %struct.ScmObj** %stackaddr$env-ref57570
%stackaddr$env-ref57571 = alloca %struct.ScmObj*, align 8
%args48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51701, i64 2)
store %struct.ScmObj* %args48097, %struct.ScmObj** %stackaddr$env-ref57571
%stackaddr$prim57572 = alloca %struct.ScmObj*, align 8
%_95k48372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56395)
store volatile %struct.ScmObj* %_95k48372, %struct.ScmObj** %stackaddr$prim57572, align 8
%stackaddr$prim57573 = alloca %struct.ScmObj*, align 8
%current_45args56396 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56395)
store volatile %struct.ScmObj* %current_45args56396, %struct.ScmObj** %stackaddr$prim57573, align 8
%stackaddr$prim57574 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56396)
store volatile %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$prim57574, align 8
%stackaddr$prim57575 = alloca %struct.ScmObj*, align 8
%anf_45bind48267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48097)
store volatile %struct.ScmObj* %anf_45bind48267, %struct.ScmObj** %stackaddr$prim57575, align 8
%stackaddr$prim57576 = alloca %struct.ScmObj*, align 8
%anf_45bind48268 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48097)
store volatile %struct.ScmObj* %anf_45bind48268, %struct.ScmObj** %stackaddr$prim57576, align 8
%argslist56398$_37foldl1480360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57577 = alloca %struct.ScmObj*, align 8
%argslist56398$_37foldl1480361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48268, %struct.ScmObj* %argslist56398$_37foldl1480360)
store volatile %struct.ScmObj* %argslist56398$_37foldl1480361, %struct.ScmObj** %stackaddr$prim57577, align 8
%stackaddr$prim57578 = alloca %struct.ScmObj*, align 8
%argslist56398$_37foldl1480362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48267, %struct.ScmObj* %argslist56398$_37foldl1480361)
store volatile %struct.ScmObj* %argslist56398$_37foldl1480362, %struct.ScmObj** %stackaddr$prim57578, align 8
%stackaddr$prim57579 = alloca %struct.ScmObj*, align 8
%argslist56398$_37foldl1480363 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48266, %struct.ScmObj* %argslist56398$_37foldl1480362)
store volatile %struct.ScmObj* %argslist56398$_37foldl1480363, %struct.ScmObj** %stackaddr$prim57579, align 8
%stackaddr$prim57580 = alloca %struct.ScmObj*, align 8
%argslist56398$_37foldl1480364 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48370, %struct.ScmObj* %argslist56398$_37foldl1480363)
store volatile %struct.ScmObj* %argslist56398$_37foldl1480364, %struct.ScmObj** %stackaddr$prim57580, align 8
%clofunc57581 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148036)
musttail call tailcc void %clofunc57581(%struct.ScmObj* %_37foldl148036, %struct.ScmObj* %argslist56398$_37foldl1480364)
ret void
}

define tailcc void @proc_clo$ae51703(%struct.ScmObj* %env$ae51703,%struct.ScmObj* %current_45args56399) {
%stackaddr$prim57582 = alloca %struct.ScmObj*, align 8
%k48373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56399)
store volatile %struct.ScmObj* %k48373, %struct.ScmObj** %stackaddr$prim57582, align 8
%stackaddr$prim57583 = alloca %struct.ScmObj*, align 8
%current_45args56400 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56399)
store volatile %struct.ScmObj* %current_45args56400, %struct.ScmObj** %stackaddr$prim57583, align 8
%stackaddr$prim57584 = alloca %struct.ScmObj*, align 8
%n48099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56400)
store volatile %struct.ScmObj* %n48099, %struct.ScmObj** %stackaddr$prim57584, align 8
%stackaddr$prim57585 = alloca %struct.ScmObj*, align 8
%current_45args56401 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56400)
store volatile %struct.ScmObj* %current_45args56401, %struct.ScmObj** %stackaddr$prim57585, align 8
%stackaddr$prim57586 = alloca %struct.ScmObj*, align 8
%v48098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56401)
store volatile %struct.ScmObj* %v48098, %struct.ScmObj** %stackaddr$prim57586, align 8
%stackaddr$prim57587 = alloca %struct.ScmObj*, align 8
%cpsprim48374 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v48098, %struct.ScmObj* %n48099)
store volatile %struct.ScmObj* %cpsprim48374, %struct.ScmObj** %stackaddr$prim57587, align 8
%ae51707 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56403$k483730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57588 = alloca %struct.ScmObj*, align 8
%argslist56403$k483731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48374, %struct.ScmObj* %argslist56403$k483730)
store volatile %struct.ScmObj* %argslist56403$k483731, %struct.ScmObj** %stackaddr$prim57588, align 8
%stackaddr$prim57589 = alloca %struct.ScmObj*, align 8
%argslist56403$k483732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51707, %struct.ScmObj* %argslist56403$k483731)
store volatile %struct.ScmObj* %argslist56403$k483732, %struct.ScmObj** %stackaddr$prim57589, align 8
%clofunc57590 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48373)
musttail call tailcc void %clofunc57590(%struct.ScmObj* %k48373, %struct.ScmObj* %argslist56403$k483732)
ret void
}

define tailcc void @proc_clo$ae51273(%struct.ScmObj* %env$ae51273,%struct.ScmObj* %current_45args56406) {
%stackaddr$prim57591 = alloca %struct.ScmObj*, align 8
%k48375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56406)
store volatile %struct.ScmObj* %k48375, %struct.ScmObj** %stackaddr$prim57591, align 8
%stackaddr$prim57592 = alloca %struct.ScmObj*, align 8
%current_45args56407 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56406)
store volatile %struct.ScmObj* %current_45args56407, %struct.ScmObj** %stackaddr$prim57592, align 8
%stackaddr$prim57593 = alloca %struct.ScmObj*, align 8
%v48102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56407)
store volatile %struct.ScmObj* %v48102, %struct.ScmObj** %stackaddr$prim57593, align 8
%stackaddr$prim57594 = alloca %struct.ScmObj*, align 8
%current_45args56408 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56407)
store volatile %struct.ScmObj* %current_45args56408, %struct.ScmObj** %stackaddr$prim57594, align 8
%stackaddr$prim57595 = alloca %struct.ScmObj*, align 8
%lst48101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56408)
store volatile %struct.ScmObj* %lst48101, %struct.ScmObj** %stackaddr$prim57595, align 8
%ae51274 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57596 = alloca %struct.ScmObj*, align 8
%lst48103 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51274, %struct.ScmObj* %lst48101)
store volatile %struct.ScmObj* %lst48103, %struct.ScmObj** %stackaddr$prim57596, align 8
%stackaddr$makeclosure57597 = alloca %struct.ScmObj*, align 8
%fptrToInt57598 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51276 to i64
%ae51276 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57598)
store volatile %struct.ScmObj* %ae51276, %struct.ScmObj** %stackaddr$makeclosure57597, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51276, %struct.ScmObj* %k48375, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51276, %struct.ScmObj* %lst48103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51276, %struct.ScmObj* %v48102, i64 2)
%ae51277 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57599 = alloca %struct.ScmObj*, align 8
%fptrToInt57600 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51278 to i64
%ae51278 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57600)
store volatile %struct.ScmObj* %ae51278, %struct.ScmObj** %stackaddr$makeclosure57599, align 8
%argslist56430$ae512760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57601 = alloca %struct.ScmObj*, align 8
%argslist56430$ae512761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51278, %struct.ScmObj* %argslist56430$ae512760)
store volatile %struct.ScmObj* %argslist56430$ae512761, %struct.ScmObj** %stackaddr$prim57601, align 8
%stackaddr$prim57602 = alloca %struct.ScmObj*, align 8
%argslist56430$ae512762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51277, %struct.ScmObj* %argslist56430$ae512761)
store volatile %struct.ScmObj* %argslist56430$ae512762, %struct.ScmObj** %stackaddr$prim57602, align 8
%clofunc57603 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51276)
musttail call tailcc void %clofunc57603(%struct.ScmObj* %ae51276, %struct.ScmObj* %argslist56430$ae512762)
ret void
}

define tailcc void @proc_clo$ae51276(%struct.ScmObj* %env$ae51276,%struct.ScmObj* %current_45args56410) {
%stackaddr$env-ref57604 = alloca %struct.ScmObj*, align 8
%k48375 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51276, i64 0)
store %struct.ScmObj* %k48375, %struct.ScmObj** %stackaddr$env-ref57604
%stackaddr$env-ref57605 = alloca %struct.ScmObj*, align 8
%lst48103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51276, i64 1)
store %struct.ScmObj* %lst48103, %struct.ScmObj** %stackaddr$env-ref57605
%stackaddr$env-ref57606 = alloca %struct.ScmObj*, align 8
%v48102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51276, i64 2)
store %struct.ScmObj* %v48102, %struct.ScmObj** %stackaddr$env-ref57606
%stackaddr$prim57607 = alloca %struct.ScmObj*, align 8
%_95k48376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56410)
store volatile %struct.ScmObj* %_95k48376, %struct.ScmObj** %stackaddr$prim57607, align 8
%stackaddr$prim57608 = alloca %struct.ScmObj*, align 8
%current_45args56411 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56410)
store volatile %struct.ScmObj* %current_45args56411, %struct.ScmObj** %stackaddr$prim57608, align 8
%stackaddr$prim57609 = alloca %struct.ScmObj*, align 8
%anf_45bind48255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56411)
store volatile %struct.ScmObj* %anf_45bind48255, %struct.ScmObj** %stackaddr$prim57609, align 8
%stackaddr$makeclosure57610 = alloca %struct.ScmObj*, align 8
%fptrToInt57611 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51292 to i64
%ae51292 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57611)
store volatile %struct.ScmObj* %ae51292, %struct.ScmObj** %stackaddr$makeclosure57610, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51292, %struct.ScmObj* %k48375, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51292, %struct.ScmObj* %lst48103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51292, %struct.ScmObj* %v48102, i64 2)
%stackaddr$makeclosure57612 = alloca %struct.ScmObj*, align 8
%fptrToInt57613 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51293 to i64
%ae51293 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57613)
store volatile %struct.ScmObj* %ae51293, %struct.ScmObj** %stackaddr$makeclosure57612, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51293, %struct.ScmObj* %k48375, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51293, %struct.ScmObj* %lst48103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51293, %struct.ScmObj* %v48102, i64 2)
%argslist56425$anf_45bind482550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57614 = alloca %struct.ScmObj*, align 8
%argslist56425$anf_45bind482551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51293, %struct.ScmObj* %argslist56425$anf_45bind482550)
store volatile %struct.ScmObj* %argslist56425$anf_45bind482551, %struct.ScmObj** %stackaddr$prim57614, align 8
%stackaddr$prim57615 = alloca %struct.ScmObj*, align 8
%argslist56425$anf_45bind482552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51292, %struct.ScmObj* %argslist56425$anf_45bind482551)
store volatile %struct.ScmObj* %argslist56425$anf_45bind482552, %struct.ScmObj** %stackaddr$prim57615, align 8
%clofunc57616 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48255)
musttail call tailcc void %clofunc57616(%struct.ScmObj* %anf_45bind48255, %struct.ScmObj* %argslist56425$anf_45bind482552)
ret void
}

define tailcc void @proc_clo$ae51292(%struct.ScmObj* %env$ae51292,%struct.ScmObj* %current_45args56413) {
%stackaddr$env-ref57617 = alloca %struct.ScmObj*, align 8
%k48375 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51292, i64 0)
store %struct.ScmObj* %k48375, %struct.ScmObj** %stackaddr$env-ref57617
%stackaddr$env-ref57618 = alloca %struct.ScmObj*, align 8
%lst48103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51292, i64 1)
store %struct.ScmObj* %lst48103, %struct.ScmObj** %stackaddr$env-ref57618
%stackaddr$env-ref57619 = alloca %struct.ScmObj*, align 8
%v48102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51292, i64 2)
store %struct.ScmObj* %v48102, %struct.ScmObj** %stackaddr$env-ref57619
%stackaddr$prim57620 = alloca %struct.ScmObj*, align 8
%_95k48377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56413)
store volatile %struct.ScmObj* %_95k48377, %struct.ScmObj** %stackaddr$prim57620, align 8
%stackaddr$prim57621 = alloca %struct.ScmObj*, align 8
%current_45args56414 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56413)
store volatile %struct.ScmObj* %current_45args56414, %struct.ScmObj** %stackaddr$prim57621, align 8
%stackaddr$prim57622 = alloca %struct.ScmObj*, align 8
%cc48104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56414)
store volatile %struct.ScmObj* %cc48104, %struct.ScmObj** %stackaddr$prim57622, align 8
%ae51401 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57623 = alloca %struct.ScmObj*, align 8
%anf_45bind48256 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48103, %struct.ScmObj* %ae51401)
store volatile %struct.ScmObj* %anf_45bind48256, %struct.ScmObj** %stackaddr$prim57623, align 8
%stackaddr$prim57624 = alloca %struct.ScmObj*, align 8
%anf_45bind48257 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48256)
store volatile %struct.ScmObj* %anf_45bind48257, %struct.ScmObj** %stackaddr$prim57624, align 8
%truthy$cmp57625 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48257)
%cmp$cmp57625 = icmp eq i64 %truthy$cmp57625, 1
br i1 %cmp$cmp57625, label %truebranch$cmp57625, label %falsebranch$cmp57625
truebranch$cmp57625:
%ae51405 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51406 = call %struct.ScmObj* @const_init_false()
%argslist56416$k483750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57626 = alloca %struct.ScmObj*, align 8
%argslist56416$k483751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51406, %struct.ScmObj* %argslist56416$k483750)
store volatile %struct.ScmObj* %argslist56416$k483751, %struct.ScmObj** %stackaddr$prim57626, align 8
%stackaddr$prim57627 = alloca %struct.ScmObj*, align 8
%argslist56416$k483752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51405, %struct.ScmObj* %argslist56416$k483751)
store volatile %struct.ScmObj* %argslist56416$k483752, %struct.ScmObj** %stackaddr$prim57627, align 8
%clofunc57628 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48375)
musttail call tailcc void %clofunc57628(%struct.ScmObj* %k48375, %struct.ScmObj* %argslist56416$k483752)
ret void
falsebranch$cmp57625:
%ae51414 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57629 = alloca %struct.ScmObj*, align 8
%anf_45bind48258 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48103, %struct.ScmObj* %ae51414)
store volatile %struct.ScmObj* %anf_45bind48258, %struct.ScmObj** %stackaddr$prim57629, align 8
%stackaddr$prim57630 = alloca %struct.ScmObj*, align 8
%anf_45bind48259 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48258)
store volatile %struct.ScmObj* %anf_45bind48259, %struct.ScmObj** %stackaddr$prim57630, align 8
%stackaddr$prim57631 = alloca %struct.ScmObj*, align 8
%anf_45bind48260 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48259, %struct.ScmObj* %v48102)
store volatile %struct.ScmObj* %anf_45bind48260, %struct.ScmObj** %stackaddr$prim57631, align 8
%truthy$cmp57632 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48260)
%cmp$cmp57632 = icmp eq i64 %truthy$cmp57632, 1
br i1 %cmp$cmp57632, label %truebranch$cmp57632, label %falsebranch$cmp57632
truebranch$cmp57632:
%ae51420 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57633 = alloca %struct.ScmObj*, align 8
%cpsprim48378 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48103, %struct.ScmObj* %ae51420)
store volatile %struct.ScmObj* %cpsprim48378, %struct.ScmObj** %stackaddr$prim57633, align 8
%ae51422 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56417$k483750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57634 = alloca %struct.ScmObj*, align 8
%argslist56417$k483751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48378, %struct.ScmObj* %argslist56417$k483750)
store volatile %struct.ScmObj* %argslist56417$k483751, %struct.ScmObj** %stackaddr$prim57634, align 8
%stackaddr$prim57635 = alloca %struct.ScmObj*, align 8
%argslist56417$k483752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51422, %struct.ScmObj* %argslist56417$k483751)
store volatile %struct.ScmObj* %argslist56417$k483752, %struct.ScmObj** %stackaddr$prim57635, align 8
%clofunc57636 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48375)
musttail call tailcc void %clofunc57636(%struct.ScmObj* %k48375, %struct.ScmObj* %argslist56417$k483752)
ret void
falsebranch$cmp57632:
%ae51433 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57637 = alloca %struct.ScmObj*, align 8
%anf_45bind48261 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48103, %struct.ScmObj* %ae51433)
store volatile %struct.ScmObj* %anf_45bind48261, %struct.ScmObj** %stackaddr$prim57637, align 8
%stackaddr$prim57638 = alloca %struct.ScmObj*, align 8
%anf_45bind48262 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48261)
store volatile %struct.ScmObj* %anf_45bind48262, %struct.ScmObj** %stackaddr$prim57638, align 8
%ae51436 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57639 = alloca %struct.ScmObj*, align 8
%_95048106 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48103, %struct.ScmObj* %ae51436, %struct.ScmObj* %anf_45bind48262)
store volatile %struct.ScmObj* %_95048106, %struct.ScmObj** %stackaddr$prim57639, align 8
%argslist56418$cc481040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57640 = alloca %struct.ScmObj*, align 8
%argslist56418$cc481041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48104, %struct.ScmObj* %argslist56418$cc481040)
store volatile %struct.ScmObj* %argslist56418$cc481041, %struct.ScmObj** %stackaddr$prim57640, align 8
%stackaddr$prim57641 = alloca %struct.ScmObj*, align 8
%argslist56418$cc481042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48375, %struct.ScmObj* %argslist56418$cc481041)
store volatile %struct.ScmObj* %argslist56418$cc481042, %struct.ScmObj** %stackaddr$prim57641, align 8
%clofunc57642 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48104)
musttail call tailcc void %clofunc57642(%struct.ScmObj* %cc48104, %struct.ScmObj* %argslist56418$cc481042)
ret void
}

define tailcc void @proc_clo$ae51293(%struct.ScmObj* %env$ae51293,%struct.ScmObj* %current_45args56419) {
%stackaddr$env-ref57643 = alloca %struct.ScmObj*, align 8
%k48375 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51293, i64 0)
store %struct.ScmObj* %k48375, %struct.ScmObj** %stackaddr$env-ref57643
%stackaddr$env-ref57644 = alloca %struct.ScmObj*, align 8
%lst48103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51293, i64 1)
store %struct.ScmObj* %lst48103, %struct.ScmObj** %stackaddr$env-ref57644
%stackaddr$env-ref57645 = alloca %struct.ScmObj*, align 8
%v48102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51293, i64 2)
store %struct.ScmObj* %v48102, %struct.ScmObj** %stackaddr$env-ref57645
%stackaddr$prim57646 = alloca %struct.ScmObj*, align 8
%_95k48377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56419)
store volatile %struct.ScmObj* %_95k48377, %struct.ScmObj** %stackaddr$prim57646, align 8
%stackaddr$prim57647 = alloca %struct.ScmObj*, align 8
%current_45args56420 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56419)
store volatile %struct.ScmObj* %current_45args56420, %struct.ScmObj** %stackaddr$prim57647, align 8
%stackaddr$prim57648 = alloca %struct.ScmObj*, align 8
%cc48104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56420)
store volatile %struct.ScmObj* %cc48104, %struct.ScmObj** %stackaddr$prim57648, align 8
%ae51295 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57649 = alloca %struct.ScmObj*, align 8
%anf_45bind48256 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48103, %struct.ScmObj* %ae51295)
store volatile %struct.ScmObj* %anf_45bind48256, %struct.ScmObj** %stackaddr$prim57649, align 8
%stackaddr$prim57650 = alloca %struct.ScmObj*, align 8
%anf_45bind48257 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48256)
store volatile %struct.ScmObj* %anf_45bind48257, %struct.ScmObj** %stackaddr$prim57650, align 8
%truthy$cmp57651 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48257)
%cmp$cmp57651 = icmp eq i64 %truthy$cmp57651, 1
br i1 %cmp$cmp57651, label %truebranch$cmp57651, label %falsebranch$cmp57651
truebranch$cmp57651:
%ae51299 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51300 = call %struct.ScmObj* @const_init_false()
%argslist56422$k483750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57652 = alloca %struct.ScmObj*, align 8
%argslist56422$k483751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51300, %struct.ScmObj* %argslist56422$k483750)
store volatile %struct.ScmObj* %argslist56422$k483751, %struct.ScmObj** %stackaddr$prim57652, align 8
%stackaddr$prim57653 = alloca %struct.ScmObj*, align 8
%argslist56422$k483752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51299, %struct.ScmObj* %argslist56422$k483751)
store volatile %struct.ScmObj* %argslist56422$k483752, %struct.ScmObj** %stackaddr$prim57653, align 8
%clofunc57654 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48375)
musttail call tailcc void %clofunc57654(%struct.ScmObj* %k48375, %struct.ScmObj* %argslist56422$k483752)
ret void
falsebranch$cmp57651:
%ae51308 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57655 = alloca %struct.ScmObj*, align 8
%anf_45bind48258 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48103, %struct.ScmObj* %ae51308)
store volatile %struct.ScmObj* %anf_45bind48258, %struct.ScmObj** %stackaddr$prim57655, align 8
%stackaddr$prim57656 = alloca %struct.ScmObj*, align 8
%anf_45bind48259 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48258)
store volatile %struct.ScmObj* %anf_45bind48259, %struct.ScmObj** %stackaddr$prim57656, align 8
%stackaddr$prim57657 = alloca %struct.ScmObj*, align 8
%anf_45bind48260 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48259, %struct.ScmObj* %v48102)
store volatile %struct.ScmObj* %anf_45bind48260, %struct.ScmObj** %stackaddr$prim57657, align 8
%truthy$cmp57658 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48260)
%cmp$cmp57658 = icmp eq i64 %truthy$cmp57658, 1
br i1 %cmp$cmp57658, label %truebranch$cmp57658, label %falsebranch$cmp57658
truebranch$cmp57658:
%ae51314 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57659 = alloca %struct.ScmObj*, align 8
%cpsprim48378 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48103, %struct.ScmObj* %ae51314)
store volatile %struct.ScmObj* %cpsprim48378, %struct.ScmObj** %stackaddr$prim57659, align 8
%ae51316 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56423$k483750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57660 = alloca %struct.ScmObj*, align 8
%argslist56423$k483751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48378, %struct.ScmObj* %argslist56423$k483750)
store volatile %struct.ScmObj* %argslist56423$k483751, %struct.ScmObj** %stackaddr$prim57660, align 8
%stackaddr$prim57661 = alloca %struct.ScmObj*, align 8
%argslist56423$k483752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51316, %struct.ScmObj* %argslist56423$k483751)
store volatile %struct.ScmObj* %argslist56423$k483752, %struct.ScmObj** %stackaddr$prim57661, align 8
%clofunc57662 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48375)
musttail call tailcc void %clofunc57662(%struct.ScmObj* %k48375, %struct.ScmObj* %argslist56423$k483752)
ret void
falsebranch$cmp57658:
%ae51327 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57663 = alloca %struct.ScmObj*, align 8
%anf_45bind48261 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48103, %struct.ScmObj* %ae51327)
store volatile %struct.ScmObj* %anf_45bind48261, %struct.ScmObj** %stackaddr$prim57663, align 8
%stackaddr$prim57664 = alloca %struct.ScmObj*, align 8
%anf_45bind48262 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48261)
store volatile %struct.ScmObj* %anf_45bind48262, %struct.ScmObj** %stackaddr$prim57664, align 8
%ae51330 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57665 = alloca %struct.ScmObj*, align 8
%_95048106 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48103, %struct.ScmObj* %ae51330, %struct.ScmObj* %anf_45bind48262)
store volatile %struct.ScmObj* %_95048106, %struct.ScmObj** %stackaddr$prim57665, align 8
%argslist56424$cc481040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57666 = alloca %struct.ScmObj*, align 8
%argslist56424$cc481041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48104, %struct.ScmObj* %argslist56424$cc481040)
store volatile %struct.ScmObj* %argslist56424$cc481041, %struct.ScmObj** %stackaddr$prim57666, align 8
%stackaddr$prim57667 = alloca %struct.ScmObj*, align 8
%argslist56424$cc481042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48375, %struct.ScmObj* %argslist56424$cc481041)
store volatile %struct.ScmObj* %argslist56424$cc481042, %struct.ScmObj** %stackaddr$prim57667, align 8
%clofunc57668 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48104)
musttail call tailcc void %clofunc57668(%struct.ScmObj* %cc48104, %struct.ScmObj* %argslist56424$cc481042)
ret void
}

define tailcc void @proc_clo$ae51278(%struct.ScmObj* %env$ae51278,%struct.ScmObj* %current_45args56426) {
%stackaddr$prim57669 = alloca %struct.ScmObj*, align 8
%k48379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56426)
store volatile %struct.ScmObj* %k48379, %struct.ScmObj** %stackaddr$prim57669, align 8
%stackaddr$prim57670 = alloca %struct.ScmObj*, align 8
%current_45args56427 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56426)
store volatile %struct.ScmObj* %current_45args56427, %struct.ScmObj** %stackaddr$prim57670, align 8
%stackaddr$prim57671 = alloca %struct.ScmObj*, align 8
%u48105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56427)
store volatile %struct.ScmObj* %u48105, %struct.ScmObj** %stackaddr$prim57671, align 8
%argslist56429$u481050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57672 = alloca %struct.ScmObj*, align 8
%argslist56429$u481051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48105, %struct.ScmObj* %argslist56429$u481050)
store volatile %struct.ScmObj* %argslist56429$u481051, %struct.ScmObj** %stackaddr$prim57672, align 8
%stackaddr$prim57673 = alloca %struct.ScmObj*, align 8
%argslist56429$u481052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48379, %struct.ScmObj* %argslist56429$u481051)
store volatile %struct.ScmObj* %argslist56429$u481052, %struct.ScmObj** %stackaddr$prim57673, align 8
%clofunc57674 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48105)
musttail call tailcc void %clofunc57674(%struct.ScmObj* %u48105, %struct.ScmObj* %argslist56429$u481052)
ret void
}

define tailcc void @proc_clo$ae50737(%struct.ScmObj* %env$ae50737,%struct.ScmObj* %current_45args56432) {
%stackaddr$prim57675 = alloca %struct.ScmObj*, align 8
%k48380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56432)
store volatile %struct.ScmObj* %k48380, %struct.ScmObj** %stackaddr$prim57675, align 8
%stackaddr$prim57676 = alloca %struct.ScmObj*, align 8
%current_45args56433 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56432)
store volatile %struct.ScmObj* %current_45args56433, %struct.ScmObj** %stackaddr$prim57676, align 8
%stackaddr$prim57677 = alloca %struct.ScmObj*, align 8
%lst48109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56433)
store volatile %struct.ScmObj* %lst48109, %struct.ScmObj** %stackaddr$prim57677, align 8
%stackaddr$prim57678 = alloca %struct.ScmObj*, align 8
%current_45args56434 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56433)
store volatile %struct.ScmObj* %current_45args56434, %struct.ScmObj** %stackaddr$prim57678, align 8
%stackaddr$prim57679 = alloca %struct.ScmObj*, align 8
%n48108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56434)
store volatile %struct.ScmObj* %n48108, %struct.ScmObj** %stackaddr$prim57679, align 8
%ae50738 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57680 = alloca %struct.ScmObj*, align 8
%n48111 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50738, %struct.ScmObj* %n48108)
store volatile %struct.ScmObj* %n48111, %struct.ScmObj** %stackaddr$prim57680, align 8
%ae50740 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57681 = alloca %struct.ScmObj*, align 8
%lst48110 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50740, %struct.ScmObj* %lst48109)
store volatile %struct.ScmObj* %lst48110, %struct.ScmObj** %stackaddr$prim57681, align 8
%stackaddr$makeclosure57682 = alloca %struct.ScmObj*, align 8
%fptrToInt57683 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50742 to i64
%ae50742 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57683)
store volatile %struct.ScmObj* %ae50742, %struct.ScmObj** %stackaddr$makeclosure57682, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50742, %struct.ScmObj* %n48111, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50742, %struct.ScmObj* %lst48110, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50742, %struct.ScmObj* %k48380, i64 2)
%ae50743 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57684 = alloca %struct.ScmObj*, align 8
%fptrToInt57685 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50744 to i64
%ae50744 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57685)
store volatile %struct.ScmObj* %ae50744, %struct.ScmObj** %stackaddr$makeclosure57684, align 8
%argslist56454$ae507420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57686 = alloca %struct.ScmObj*, align 8
%argslist56454$ae507421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50744, %struct.ScmObj* %argslist56454$ae507420)
store volatile %struct.ScmObj* %argslist56454$ae507421, %struct.ScmObj** %stackaddr$prim57686, align 8
%stackaddr$prim57687 = alloca %struct.ScmObj*, align 8
%argslist56454$ae507422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50743, %struct.ScmObj* %argslist56454$ae507421)
store volatile %struct.ScmObj* %argslist56454$ae507422, %struct.ScmObj** %stackaddr$prim57687, align 8
%clofunc57688 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50742)
musttail call tailcc void %clofunc57688(%struct.ScmObj* %ae50742, %struct.ScmObj* %argslist56454$ae507422)
ret void
}

define tailcc void @proc_clo$ae50742(%struct.ScmObj* %env$ae50742,%struct.ScmObj* %current_45args56436) {
%stackaddr$env-ref57689 = alloca %struct.ScmObj*, align 8
%n48111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50742, i64 0)
store %struct.ScmObj* %n48111, %struct.ScmObj** %stackaddr$env-ref57689
%stackaddr$env-ref57690 = alloca %struct.ScmObj*, align 8
%lst48110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50742, i64 1)
store %struct.ScmObj* %lst48110, %struct.ScmObj** %stackaddr$env-ref57690
%stackaddr$env-ref57691 = alloca %struct.ScmObj*, align 8
%k48380 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50742, i64 2)
store %struct.ScmObj* %k48380, %struct.ScmObj** %stackaddr$env-ref57691
%stackaddr$prim57692 = alloca %struct.ScmObj*, align 8
%_95k48381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56436)
store volatile %struct.ScmObj* %_95k48381, %struct.ScmObj** %stackaddr$prim57692, align 8
%stackaddr$prim57693 = alloca %struct.ScmObj*, align 8
%current_45args56437 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56436)
store volatile %struct.ScmObj* %current_45args56437, %struct.ScmObj** %stackaddr$prim57693, align 8
%stackaddr$prim57694 = alloca %struct.ScmObj*, align 8
%anf_45bind48248 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56437)
store volatile %struct.ScmObj* %anf_45bind48248, %struct.ScmObj** %stackaddr$prim57694, align 8
%stackaddr$makeclosure57695 = alloca %struct.ScmObj*, align 8
%fptrToInt57696 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50758 to i64
%ae50758 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57696)
store volatile %struct.ScmObj* %ae50758, %struct.ScmObj** %stackaddr$makeclosure57695, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50758, %struct.ScmObj* %n48111, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50758, %struct.ScmObj* %lst48110, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50758, %struct.ScmObj* %k48380, i64 2)
%stackaddr$makeclosure57697 = alloca %struct.ScmObj*, align 8
%fptrToInt57698 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50759 to i64
%ae50759 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57698)
store volatile %struct.ScmObj* %ae50759, %struct.ScmObj** %stackaddr$makeclosure57697, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50759, %struct.ScmObj* %n48111, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50759, %struct.ScmObj* %lst48110, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50759, %struct.ScmObj* %k48380, i64 2)
%argslist56449$anf_45bind482480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57699 = alloca %struct.ScmObj*, align 8
%argslist56449$anf_45bind482481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50759, %struct.ScmObj* %argslist56449$anf_45bind482480)
store volatile %struct.ScmObj* %argslist56449$anf_45bind482481, %struct.ScmObj** %stackaddr$prim57699, align 8
%stackaddr$prim57700 = alloca %struct.ScmObj*, align 8
%argslist56449$anf_45bind482482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50758, %struct.ScmObj* %argslist56449$anf_45bind482481)
store volatile %struct.ScmObj* %argslist56449$anf_45bind482482, %struct.ScmObj** %stackaddr$prim57700, align 8
%clofunc57701 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48248)
musttail call tailcc void %clofunc57701(%struct.ScmObj* %anf_45bind48248, %struct.ScmObj* %argslist56449$anf_45bind482482)
ret void
}

define tailcc void @proc_clo$ae50758(%struct.ScmObj* %env$ae50758,%struct.ScmObj* %current_45args56439) {
%stackaddr$env-ref57702 = alloca %struct.ScmObj*, align 8
%n48111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50758, i64 0)
store %struct.ScmObj* %n48111, %struct.ScmObj** %stackaddr$env-ref57702
%stackaddr$env-ref57703 = alloca %struct.ScmObj*, align 8
%lst48110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50758, i64 1)
store %struct.ScmObj* %lst48110, %struct.ScmObj** %stackaddr$env-ref57703
%stackaddr$env-ref57704 = alloca %struct.ScmObj*, align 8
%k48380 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50758, i64 2)
store %struct.ScmObj* %k48380, %struct.ScmObj** %stackaddr$env-ref57704
%stackaddr$prim57705 = alloca %struct.ScmObj*, align 8
%_95k48382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56439)
store volatile %struct.ScmObj* %_95k48382, %struct.ScmObj** %stackaddr$prim57705, align 8
%stackaddr$prim57706 = alloca %struct.ScmObj*, align 8
%current_45args56440 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56439)
store volatile %struct.ScmObj* %current_45args56440, %struct.ScmObj** %stackaddr$prim57706, align 8
%stackaddr$prim57707 = alloca %struct.ScmObj*, align 8
%cc48112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56440)
store volatile %struct.ScmObj* %cc48112, %struct.ScmObj** %stackaddr$prim57707, align 8
%ae50901 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57708 = alloca %struct.ScmObj*, align 8
%anf_45bind48249 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48111, %struct.ScmObj* %ae50901)
store volatile %struct.ScmObj* %anf_45bind48249, %struct.ScmObj** %stackaddr$prim57708, align 8
%ae50902 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57709 = alloca %struct.ScmObj*, align 8
%anf_45bind48250 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50902, %struct.ScmObj* %anf_45bind48249)
store volatile %struct.ScmObj* %anf_45bind48250, %struct.ScmObj** %stackaddr$prim57709, align 8
%truthy$cmp57710 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48250)
%cmp$cmp57710 = icmp eq i64 %truthy$cmp57710, 1
br i1 %cmp$cmp57710, label %truebranch$cmp57710, label %falsebranch$cmp57710
truebranch$cmp57710:
%ae50906 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57711 = alloca %struct.ScmObj*, align 8
%cpsprim48383 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48110, %struct.ScmObj* %ae50906)
store volatile %struct.ScmObj* %cpsprim48383, %struct.ScmObj** %stackaddr$prim57711, align 8
%ae50908 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56442$k483800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57712 = alloca %struct.ScmObj*, align 8
%argslist56442$k483801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48383, %struct.ScmObj* %argslist56442$k483800)
store volatile %struct.ScmObj* %argslist56442$k483801, %struct.ScmObj** %stackaddr$prim57712, align 8
%stackaddr$prim57713 = alloca %struct.ScmObj*, align 8
%argslist56442$k483802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50908, %struct.ScmObj* %argslist56442$k483801)
store volatile %struct.ScmObj* %argslist56442$k483802, %struct.ScmObj** %stackaddr$prim57713, align 8
%clofunc57714 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48380)
musttail call tailcc void %clofunc57714(%struct.ScmObj* %k48380, %struct.ScmObj* %argslist56442$k483802)
ret void
falsebranch$cmp57710:
%ae50919 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57715 = alloca %struct.ScmObj*, align 8
%anf_45bind48251 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48110, %struct.ScmObj* %ae50919)
store volatile %struct.ScmObj* %anf_45bind48251, %struct.ScmObj** %stackaddr$prim57715, align 8
%stackaddr$prim57716 = alloca %struct.ScmObj*, align 8
%anf_45bind48252 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48251)
store volatile %struct.ScmObj* %anf_45bind48252, %struct.ScmObj** %stackaddr$prim57716, align 8
%ae50922 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57717 = alloca %struct.ScmObj*, align 8
%_95048115 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48110, %struct.ScmObj* %ae50922, %struct.ScmObj* %anf_45bind48252)
store volatile %struct.ScmObj* %_95048115, %struct.ScmObj** %stackaddr$prim57717, align 8
%ae50925 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57718 = alloca %struct.ScmObj*, align 8
%anf_45bind48253 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48111, %struct.ScmObj* %ae50925)
store volatile %struct.ScmObj* %anf_45bind48253, %struct.ScmObj** %stackaddr$prim57718, align 8
%ae50927 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57719 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48253, %struct.ScmObj* %ae50927)
store volatile %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$prim57719, align 8
%ae50929 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57720 = alloca %struct.ScmObj*, align 8
%_95148114 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48111, %struct.ScmObj* %ae50929, %struct.ScmObj* %anf_45bind48254)
store volatile %struct.ScmObj* %_95148114, %struct.ScmObj** %stackaddr$prim57720, align 8
%argslist56443$cc481120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57721 = alloca %struct.ScmObj*, align 8
%argslist56443$cc481121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48112, %struct.ScmObj* %argslist56443$cc481120)
store volatile %struct.ScmObj* %argslist56443$cc481121, %struct.ScmObj** %stackaddr$prim57721, align 8
%stackaddr$prim57722 = alloca %struct.ScmObj*, align 8
%argslist56443$cc481122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48380, %struct.ScmObj* %argslist56443$cc481121)
store volatile %struct.ScmObj* %argslist56443$cc481122, %struct.ScmObj** %stackaddr$prim57722, align 8
%clofunc57723 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48112)
musttail call tailcc void %clofunc57723(%struct.ScmObj* %cc48112, %struct.ScmObj* %argslist56443$cc481122)
ret void
}

define tailcc void @proc_clo$ae50759(%struct.ScmObj* %env$ae50759,%struct.ScmObj* %current_45args56444) {
%stackaddr$env-ref57724 = alloca %struct.ScmObj*, align 8
%n48111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50759, i64 0)
store %struct.ScmObj* %n48111, %struct.ScmObj** %stackaddr$env-ref57724
%stackaddr$env-ref57725 = alloca %struct.ScmObj*, align 8
%lst48110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50759, i64 1)
store %struct.ScmObj* %lst48110, %struct.ScmObj** %stackaddr$env-ref57725
%stackaddr$env-ref57726 = alloca %struct.ScmObj*, align 8
%k48380 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50759, i64 2)
store %struct.ScmObj* %k48380, %struct.ScmObj** %stackaddr$env-ref57726
%stackaddr$prim57727 = alloca %struct.ScmObj*, align 8
%_95k48382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56444)
store volatile %struct.ScmObj* %_95k48382, %struct.ScmObj** %stackaddr$prim57727, align 8
%stackaddr$prim57728 = alloca %struct.ScmObj*, align 8
%current_45args56445 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56444)
store volatile %struct.ScmObj* %current_45args56445, %struct.ScmObj** %stackaddr$prim57728, align 8
%stackaddr$prim57729 = alloca %struct.ScmObj*, align 8
%cc48112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56445)
store volatile %struct.ScmObj* %cc48112, %struct.ScmObj** %stackaddr$prim57729, align 8
%ae50761 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57730 = alloca %struct.ScmObj*, align 8
%anf_45bind48249 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48111, %struct.ScmObj* %ae50761)
store volatile %struct.ScmObj* %anf_45bind48249, %struct.ScmObj** %stackaddr$prim57730, align 8
%ae50762 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57731 = alloca %struct.ScmObj*, align 8
%anf_45bind48250 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50762, %struct.ScmObj* %anf_45bind48249)
store volatile %struct.ScmObj* %anf_45bind48250, %struct.ScmObj** %stackaddr$prim57731, align 8
%truthy$cmp57732 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48250)
%cmp$cmp57732 = icmp eq i64 %truthy$cmp57732, 1
br i1 %cmp$cmp57732, label %truebranch$cmp57732, label %falsebranch$cmp57732
truebranch$cmp57732:
%ae50766 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57733 = alloca %struct.ScmObj*, align 8
%cpsprim48383 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48110, %struct.ScmObj* %ae50766)
store volatile %struct.ScmObj* %cpsprim48383, %struct.ScmObj** %stackaddr$prim57733, align 8
%ae50768 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56447$k483800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57734 = alloca %struct.ScmObj*, align 8
%argslist56447$k483801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48383, %struct.ScmObj* %argslist56447$k483800)
store volatile %struct.ScmObj* %argslist56447$k483801, %struct.ScmObj** %stackaddr$prim57734, align 8
%stackaddr$prim57735 = alloca %struct.ScmObj*, align 8
%argslist56447$k483802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50768, %struct.ScmObj* %argslist56447$k483801)
store volatile %struct.ScmObj* %argslist56447$k483802, %struct.ScmObj** %stackaddr$prim57735, align 8
%clofunc57736 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48380)
musttail call tailcc void %clofunc57736(%struct.ScmObj* %k48380, %struct.ScmObj* %argslist56447$k483802)
ret void
falsebranch$cmp57732:
%ae50779 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57737 = alloca %struct.ScmObj*, align 8
%anf_45bind48251 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48110, %struct.ScmObj* %ae50779)
store volatile %struct.ScmObj* %anf_45bind48251, %struct.ScmObj** %stackaddr$prim57737, align 8
%stackaddr$prim57738 = alloca %struct.ScmObj*, align 8
%anf_45bind48252 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48251)
store volatile %struct.ScmObj* %anf_45bind48252, %struct.ScmObj** %stackaddr$prim57738, align 8
%ae50782 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57739 = alloca %struct.ScmObj*, align 8
%_95048115 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48110, %struct.ScmObj* %ae50782, %struct.ScmObj* %anf_45bind48252)
store volatile %struct.ScmObj* %_95048115, %struct.ScmObj** %stackaddr$prim57739, align 8
%ae50785 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57740 = alloca %struct.ScmObj*, align 8
%anf_45bind48253 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48111, %struct.ScmObj* %ae50785)
store volatile %struct.ScmObj* %anf_45bind48253, %struct.ScmObj** %stackaddr$prim57740, align 8
%ae50787 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57741 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48253, %struct.ScmObj* %ae50787)
store volatile %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$prim57741, align 8
%ae50789 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57742 = alloca %struct.ScmObj*, align 8
%_95148114 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48111, %struct.ScmObj* %ae50789, %struct.ScmObj* %anf_45bind48254)
store volatile %struct.ScmObj* %_95148114, %struct.ScmObj** %stackaddr$prim57742, align 8
%argslist56448$cc481120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57743 = alloca %struct.ScmObj*, align 8
%argslist56448$cc481121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48112, %struct.ScmObj* %argslist56448$cc481120)
store volatile %struct.ScmObj* %argslist56448$cc481121, %struct.ScmObj** %stackaddr$prim57743, align 8
%stackaddr$prim57744 = alloca %struct.ScmObj*, align 8
%argslist56448$cc481122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48380, %struct.ScmObj* %argslist56448$cc481121)
store volatile %struct.ScmObj* %argslist56448$cc481122, %struct.ScmObj** %stackaddr$prim57744, align 8
%clofunc57745 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48112)
musttail call tailcc void %clofunc57745(%struct.ScmObj* %cc48112, %struct.ScmObj* %argslist56448$cc481122)
ret void
}

define tailcc void @proc_clo$ae50744(%struct.ScmObj* %env$ae50744,%struct.ScmObj* %current_45args56450) {
%stackaddr$prim57746 = alloca %struct.ScmObj*, align 8
%k48384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56450)
store volatile %struct.ScmObj* %k48384, %struct.ScmObj** %stackaddr$prim57746, align 8
%stackaddr$prim57747 = alloca %struct.ScmObj*, align 8
%current_45args56451 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56450)
store volatile %struct.ScmObj* %current_45args56451, %struct.ScmObj** %stackaddr$prim57747, align 8
%stackaddr$prim57748 = alloca %struct.ScmObj*, align 8
%u48113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56451)
store volatile %struct.ScmObj* %u48113, %struct.ScmObj** %stackaddr$prim57748, align 8
%argslist56453$u481130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57749 = alloca %struct.ScmObj*, align 8
%argslist56453$u481131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48113, %struct.ScmObj* %argslist56453$u481130)
store volatile %struct.ScmObj* %argslist56453$u481131, %struct.ScmObj** %stackaddr$prim57749, align 8
%stackaddr$prim57750 = alloca %struct.ScmObj*, align 8
%argslist56453$u481132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48384, %struct.ScmObj* %argslist56453$u481131)
store volatile %struct.ScmObj* %argslist56453$u481132, %struct.ScmObj** %stackaddr$prim57750, align 8
%clofunc57751 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48113)
musttail call tailcc void %clofunc57751(%struct.ScmObj* %u48113, %struct.ScmObj* %argslist56453$u481132)
ret void
}

define tailcc void @proc_clo$ae50321(%struct.ScmObj* %env$ae50321,%struct.ScmObj* %current_45args56456) {
%stackaddr$prim57752 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56456)
store volatile %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$prim57752, align 8
%stackaddr$prim57753 = alloca %struct.ScmObj*, align 8
%current_45args56457 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56456)
store volatile %struct.ScmObj* %current_45args56457, %struct.ScmObj** %stackaddr$prim57753, align 8
%stackaddr$prim57754 = alloca %struct.ScmObj*, align 8
%a48117 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56457)
store volatile %struct.ScmObj* %a48117, %struct.ScmObj** %stackaddr$prim57754, align 8
%ae50322 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57755 = alloca %struct.ScmObj*, align 8
%a48118 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50322, %struct.ScmObj* %a48117)
store volatile %struct.ScmObj* %a48118, %struct.ScmObj** %stackaddr$prim57755, align 8
%stackaddr$makeclosure57756 = alloca %struct.ScmObj*, align 8
%fptrToInt57757 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50324 to i64
%ae50324 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57757)
store volatile %struct.ScmObj* %ae50324, %struct.ScmObj** %stackaddr$makeclosure57756, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50324, %struct.ScmObj* %a48118, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50324, %struct.ScmObj* %k48385, i64 1)
%ae50325 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57758 = alloca %struct.ScmObj*, align 8
%fptrToInt57759 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50326 to i64
%ae50326 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57759)
store volatile %struct.ScmObj* %ae50326, %struct.ScmObj** %stackaddr$makeclosure57758, align 8
%argslist56479$ae503240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57760 = alloca %struct.ScmObj*, align 8
%argslist56479$ae503241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50326, %struct.ScmObj* %argslist56479$ae503240)
store volatile %struct.ScmObj* %argslist56479$ae503241, %struct.ScmObj** %stackaddr$prim57760, align 8
%stackaddr$prim57761 = alloca %struct.ScmObj*, align 8
%argslist56479$ae503242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50325, %struct.ScmObj* %argslist56479$ae503241)
store volatile %struct.ScmObj* %argslist56479$ae503242, %struct.ScmObj** %stackaddr$prim57761, align 8
%clofunc57762 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50324)
musttail call tailcc void %clofunc57762(%struct.ScmObj* %ae50324, %struct.ScmObj* %argslist56479$ae503242)
ret void
}

define tailcc void @proc_clo$ae50324(%struct.ScmObj* %env$ae50324,%struct.ScmObj* %current_45args56459) {
%stackaddr$env-ref57763 = alloca %struct.ScmObj*, align 8
%a48118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50324, i64 0)
store %struct.ScmObj* %a48118, %struct.ScmObj** %stackaddr$env-ref57763
%stackaddr$env-ref57764 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50324, i64 1)
store %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$env-ref57764
%stackaddr$prim57765 = alloca %struct.ScmObj*, align 8
%_95k48386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56459)
store volatile %struct.ScmObj* %_95k48386, %struct.ScmObj** %stackaddr$prim57765, align 8
%stackaddr$prim57766 = alloca %struct.ScmObj*, align 8
%current_45args56460 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56459)
store volatile %struct.ScmObj* %current_45args56460, %struct.ScmObj** %stackaddr$prim57766, align 8
%stackaddr$prim57767 = alloca %struct.ScmObj*, align 8
%anf_45bind48240 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56460)
store volatile %struct.ScmObj* %anf_45bind48240, %struct.ScmObj** %stackaddr$prim57767, align 8
%stackaddr$makeclosure57768 = alloca %struct.ScmObj*, align 8
%fptrToInt57769 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50343 to i64
%ae50343 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57769)
store volatile %struct.ScmObj* %ae50343, %struct.ScmObj** %stackaddr$makeclosure57768, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50343, %struct.ScmObj* %a48118, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50343, %struct.ScmObj* %k48385, i64 1)
%stackaddr$makeclosure57770 = alloca %struct.ScmObj*, align 8
%fptrToInt57771 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50344 to i64
%ae50344 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57771)
store volatile %struct.ScmObj* %ae50344, %struct.ScmObj** %stackaddr$makeclosure57770, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50344, %struct.ScmObj* %a48118, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50344, %struct.ScmObj* %k48385, i64 1)
%argslist56474$anf_45bind482400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57772 = alloca %struct.ScmObj*, align 8
%argslist56474$anf_45bind482401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50344, %struct.ScmObj* %argslist56474$anf_45bind482400)
store volatile %struct.ScmObj* %argslist56474$anf_45bind482401, %struct.ScmObj** %stackaddr$prim57772, align 8
%stackaddr$prim57773 = alloca %struct.ScmObj*, align 8
%argslist56474$anf_45bind482402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50343, %struct.ScmObj* %argslist56474$anf_45bind482401)
store volatile %struct.ScmObj* %argslist56474$anf_45bind482402, %struct.ScmObj** %stackaddr$prim57773, align 8
%clofunc57774 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48240)
musttail call tailcc void %clofunc57774(%struct.ScmObj* %anf_45bind48240, %struct.ScmObj* %argslist56474$anf_45bind482402)
ret void
}

define tailcc void @proc_clo$ae50343(%struct.ScmObj* %env$ae50343,%struct.ScmObj* %current_45args56462) {
%stackaddr$env-ref57775 = alloca %struct.ScmObj*, align 8
%a48118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50343, i64 0)
store %struct.ScmObj* %a48118, %struct.ScmObj** %stackaddr$env-ref57775
%stackaddr$env-ref57776 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50343, i64 1)
store %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$env-ref57776
%stackaddr$prim57777 = alloca %struct.ScmObj*, align 8
%_95k48387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56462)
store volatile %struct.ScmObj* %_95k48387, %struct.ScmObj** %stackaddr$prim57777, align 8
%stackaddr$prim57778 = alloca %struct.ScmObj*, align 8
%current_45args56463 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56462)
store volatile %struct.ScmObj* %current_45args56463, %struct.ScmObj** %stackaddr$prim57778, align 8
%stackaddr$prim57779 = alloca %struct.ScmObj*, align 8
%cc48119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56463)
store volatile %struct.ScmObj* %cc48119, %struct.ScmObj** %stackaddr$prim57779, align 8
%ae50459 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57780 = alloca %struct.ScmObj*, align 8
%anf_45bind48241 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48118, %struct.ScmObj* %ae50459)
store volatile %struct.ScmObj* %anf_45bind48241, %struct.ScmObj** %stackaddr$prim57780, align 8
%stackaddr$prim57781 = alloca %struct.ScmObj*, align 8
%anf_45bind48242 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48241)
store volatile %struct.ScmObj* %anf_45bind48242, %struct.ScmObj** %stackaddr$prim57781, align 8
%truthy$cmp57782 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48242)
%cmp$cmp57782 = icmp eq i64 %truthy$cmp57782, 1
br i1 %cmp$cmp57782, label %truebranch$cmp57782, label %falsebranch$cmp57782
truebranch$cmp57782:
%ae50463 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50464 = call %struct.ScmObj* @const_init_true()
%argslist56465$k483850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57783 = alloca %struct.ScmObj*, align 8
%argslist56465$k483851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50464, %struct.ScmObj* %argslist56465$k483850)
store volatile %struct.ScmObj* %argslist56465$k483851, %struct.ScmObj** %stackaddr$prim57783, align 8
%stackaddr$prim57784 = alloca %struct.ScmObj*, align 8
%argslist56465$k483852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50463, %struct.ScmObj* %argslist56465$k483851)
store volatile %struct.ScmObj* %argslist56465$k483852, %struct.ScmObj** %stackaddr$prim57784, align 8
%clofunc57785 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48385)
musttail call tailcc void %clofunc57785(%struct.ScmObj* %k48385, %struct.ScmObj* %argslist56465$k483852)
ret void
falsebranch$cmp57782:
%ae50472 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57786 = alloca %struct.ScmObj*, align 8
%anf_45bind48243 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48118, %struct.ScmObj* %ae50472)
store volatile %struct.ScmObj* %anf_45bind48243, %struct.ScmObj** %stackaddr$prim57786, align 8
%stackaddr$prim57787 = alloca %struct.ScmObj*, align 8
%anf_45bind48244 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48243)
store volatile %struct.ScmObj* %anf_45bind48244, %struct.ScmObj** %stackaddr$prim57787, align 8
%truthy$cmp57788 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48244)
%cmp$cmp57788 = icmp eq i64 %truthy$cmp57788, 1
br i1 %cmp$cmp57788, label %truebranch$cmp57788, label %falsebranch$cmp57788
truebranch$cmp57788:
%ae50476 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57789 = alloca %struct.ScmObj*, align 8
%anf_45bind48245 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48118, %struct.ScmObj* %ae50476)
store volatile %struct.ScmObj* %anf_45bind48245, %struct.ScmObj** %stackaddr$prim57789, align 8
%stackaddr$prim57790 = alloca %struct.ScmObj*, align 8
%b48121 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48245)
store volatile %struct.ScmObj* %b48121, %struct.ScmObj** %stackaddr$prim57790, align 8
%ae50479 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57791 = alloca %struct.ScmObj*, align 8
%anf_45bind48246 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48118, %struct.ScmObj* %ae50479)
store volatile %struct.ScmObj* %anf_45bind48246, %struct.ScmObj** %stackaddr$prim57791, align 8
%stackaddr$prim57792 = alloca %struct.ScmObj*, align 8
%anf_45bind48247 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48246)
store volatile %struct.ScmObj* %anf_45bind48247, %struct.ScmObj** %stackaddr$prim57792, align 8
%ae50482 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57793 = alloca %struct.ScmObj*, align 8
%_95048122 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48118, %struct.ScmObj* %ae50482, %struct.ScmObj* %anf_45bind48247)
store volatile %struct.ScmObj* %_95048122, %struct.ScmObj** %stackaddr$prim57793, align 8
%argslist56466$cc481190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57794 = alloca %struct.ScmObj*, align 8
%argslist56466$cc481191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48119, %struct.ScmObj* %argslist56466$cc481190)
store volatile %struct.ScmObj* %argslist56466$cc481191, %struct.ScmObj** %stackaddr$prim57794, align 8
%stackaddr$prim57795 = alloca %struct.ScmObj*, align 8
%argslist56466$cc481192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48385, %struct.ScmObj* %argslist56466$cc481191)
store volatile %struct.ScmObj* %argslist56466$cc481192, %struct.ScmObj** %stackaddr$prim57795, align 8
%clofunc57796 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48119)
musttail call tailcc void %clofunc57796(%struct.ScmObj* %cc48119, %struct.ScmObj* %argslist56466$cc481192)
ret void
falsebranch$cmp57788:
%ae50515 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50516 = call %struct.ScmObj* @const_init_false()
%argslist56467$k483850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57797 = alloca %struct.ScmObj*, align 8
%argslist56467$k483851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50516, %struct.ScmObj* %argslist56467$k483850)
store volatile %struct.ScmObj* %argslist56467$k483851, %struct.ScmObj** %stackaddr$prim57797, align 8
%stackaddr$prim57798 = alloca %struct.ScmObj*, align 8
%argslist56467$k483852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50515, %struct.ScmObj* %argslist56467$k483851)
store volatile %struct.ScmObj* %argslist56467$k483852, %struct.ScmObj** %stackaddr$prim57798, align 8
%clofunc57799 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48385)
musttail call tailcc void %clofunc57799(%struct.ScmObj* %k48385, %struct.ScmObj* %argslist56467$k483852)
ret void
}

define tailcc void @proc_clo$ae50344(%struct.ScmObj* %env$ae50344,%struct.ScmObj* %current_45args56468) {
%stackaddr$env-ref57800 = alloca %struct.ScmObj*, align 8
%a48118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50344, i64 0)
store %struct.ScmObj* %a48118, %struct.ScmObj** %stackaddr$env-ref57800
%stackaddr$env-ref57801 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50344, i64 1)
store %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$env-ref57801
%stackaddr$prim57802 = alloca %struct.ScmObj*, align 8
%_95k48387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56468)
store volatile %struct.ScmObj* %_95k48387, %struct.ScmObj** %stackaddr$prim57802, align 8
%stackaddr$prim57803 = alloca %struct.ScmObj*, align 8
%current_45args56469 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56468)
store volatile %struct.ScmObj* %current_45args56469, %struct.ScmObj** %stackaddr$prim57803, align 8
%stackaddr$prim57804 = alloca %struct.ScmObj*, align 8
%cc48119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56469)
store volatile %struct.ScmObj* %cc48119, %struct.ScmObj** %stackaddr$prim57804, align 8
%ae50346 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57805 = alloca %struct.ScmObj*, align 8
%anf_45bind48241 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48118, %struct.ScmObj* %ae50346)
store volatile %struct.ScmObj* %anf_45bind48241, %struct.ScmObj** %stackaddr$prim57805, align 8
%stackaddr$prim57806 = alloca %struct.ScmObj*, align 8
%anf_45bind48242 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48241)
store volatile %struct.ScmObj* %anf_45bind48242, %struct.ScmObj** %stackaddr$prim57806, align 8
%truthy$cmp57807 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48242)
%cmp$cmp57807 = icmp eq i64 %truthy$cmp57807, 1
br i1 %cmp$cmp57807, label %truebranch$cmp57807, label %falsebranch$cmp57807
truebranch$cmp57807:
%ae50350 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50351 = call %struct.ScmObj* @const_init_true()
%argslist56471$k483850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57808 = alloca %struct.ScmObj*, align 8
%argslist56471$k483851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50351, %struct.ScmObj* %argslist56471$k483850)
store volatile %struct.ScmObj* %argslist56471$k483851, %struct.ScmObj** %stackaddr$prim57808, align 8
%stackaddr$prim57809 = alloca %struct.ScmObj*, align 8
%argslist56471$k483852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50350, %struct.ScmObj* %argslist56471$k483851)
store volatile %struct.ScmObj* %argslist56471$k483852, %struct.ScmObj** %stackaddr$prim57809, align 8
%clofunc57810 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48385)
musttail call tailcc void %clofunc57810(%struct.ScmObj* %k48385, %struct.ScmObj* %argslist56471$k483852)
ret void
falsebranch$cmp57807:
%ae50359 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57811 = alloca %struct.ScmObj*, align 8
%anf_45bind48243 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48118, %struct.ScmObj* %ae50359)
store volatile %struct.ScmObj* %anf_45bind48243, %struct.ScmObj** %stackaddr$prim57811, align 8
%stackaddr$prim57812 = alloca %struct.ScmObj*, align 8
%anf_45bind48244 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48243)
store volatile %struct.ScmObj* %anf_45bind48244, %struct.ScmObj** %stackaddr$prim57812, align 8
%truthy$cmp57813 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48244)
%cmp$cmp57813 = icmp eq i64 %truthy$cmp57813, 1
br i1 %cmp$cmp57813, label %truebranch$cmp57813, label %falsebranch$cmp57813
truebranch$cmp57813:
%ae50363 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57814 = alloca %struct.ScmObj*, align 8
%anf_45bind48245 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48118, %struct.ScmObj* %ae50363)
store volatile %struct.ScmObj* %anf_45bind48245, %struct.ScmObj** %stackaddr$prim57814, align 8
%stackaddr$prim57815 = alloca %struct.ScmObj*, align 8
%b48121 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48245)
store volatile %struct.ScmObj* %b48121, %struct.ScmObj** %stackaddr$prim57815, align 8
%ae50366 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57816 = alloca %struct.ScmObj*, align 8
%anf_45bind48246 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48118, %struct.ScmObj* %ae50366)
store volatile %struct.ScmObj* %anf_45bind48246, %struct.ScmObj** %stackaddr$prim57816, align 8
%stackaddr$prim57817 = alloca %struct.ScmObj*, align 8
%anf_45bind48247 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48246)
store volatile %struct.ScmObj* %anf_45bind48247, %struct.ScmObj** %stackaddr$prim57817, align 8
%ae50369 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57818 = alloca %struct.ScmObj*, align 8
%_95048122 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48118, %struct.ScmObj* %ae50369, %struct.ScmObj* %anf_45bind48247)
store volatile %struct.ScmObj* %_95048122, %struct.ScmObj** %stackaddr$prim57818, align 8
%argslist56472$cc481190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57819 = alloca %struct.ScmObj*, align 8
%argslist56472$cc481191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48119, %struct.ScmObj* %argslist56472$cc481190)
store volatile %struct.ScmObj* %argslist56472$cc481191, %struct.ScmObj** %stackaddr$prim57819, align 8
%stackaddr$prim57820 = alloca %struct.ScmObj*, align 8
%argslist56472$cc481192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48385, %struct.ScmObj* %argslist56472$cc481191)
store volatile %struct.ScmObj* %argslist56472$cc481192, %struct.ScmObj** %stackaddr$prim57820, align 8
%clofunc57821 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48119)
musttail call tailcc void %clofunc57821(%struct.ScmObj* %cc48119, %struct.ScmObj* %argslist56472$cc481192)
ret void
falsebranch$cmp57813:
%ae50402 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50403 = call %struct.ScmObj* @const_init_false()
%argslist56473$k483850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57822 = alloca %struct.ScmObj*, align 8
%argslist56473$k483851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50403, %struct.ScmObj* %argslist56473$k483850)
store volatile %struct.ScmObj* %argslist56473$k483851, %struct.ScmObj** %stackaddr$prim57822, align 8
%stackaddr$prim57823 = alloca %struct.ScmObj*, align 8
%argslist56473$k483852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50402, %struct.ScmObj* %argslist56473$k483851)
store volatile %struct.ScmObj* %argslist56473$k483852, %struct.ScmObj** %stackaddr$prim57823, align 8
%clofunc57824 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48385)
musttail call tailcc void %clofunc57824(%struct.ScmObj* %k48385, %struct.ScmObj* %argslist56473$k483852)
ret void
}

define tailcc void @proc_clo$ae50326(%struct.ScmObj* %env$ae50326,%struct.ScmObj* %current_45args56475) {
%stackaddr$prim57825 = alloca %struct.ScmObj*, align 8
%k48388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56475)
store volatile %struct.ScmObj* %k48388, %struct.ScmObj** %stackaddr$prim57825, align 8
%stackaddr$prim57826 = alloca %struct.ScmObj*, align 8
%current_45args56476 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56475)
store volatile %struct.ScmObj* %current_45args56476, %struct.ScmObj** %stackaddr$prim57826, align 8
%stackaddr$prim57827 = alloca %struct.ScmObj*, align 8
%k48120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56476)
store volatile %struct.ScmObj* %k48120, %struct.ScmObj** %stackaddr$prim57827, align 8
%ae50328 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56478$k483880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57828 = alloca %struct.ScmObj*, align 8
%argslist56478$k483881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48120, %struct.ScmObj* %argslist56478$k483880)
store volatile %struct.ScmObj* %argslist56478$k483881, %struct.ScmObj** %stackaddr$prim57828, align 8
%stackaddr$prim57829 = alloca %struct.ScmObj*, align 8
%argslist56478$k483882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50328, %struct.ScmObj* %argslist56478$k483881)
store volatile %struct.ScmObj* %argslist56478$k483882, %struct.ScmObj** %stackaddr$prim57829, align 8
%clofunc57830 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48388)
musttail call tailcc void %clofunc57830(%struct.ScmObj* %k48388, %struct.ScmObj* %argslist56478$k483882)
ret void
}

define tailcc void @proc_clo$ae50249(%struct.ScmObj* %env$ae50249,%struct.ScmObj* %current_45args56481) {
%stackaddr$env-ref57831 = alloca %struct.ScmObj*, align 8
%_37append48124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50249, i64 0)
store %struct.ScmObj* %_37append48124, %struct.ScmObj** %stackaddr$env-ref57831
%stackaddr$prim57832 = alloca %struct.ScmObj*, align 8
%k48389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56481)
store volatile %struct.ScmObj* %k48389, %struct.ScmObj** %stackaddr$prim57832, align 8
%stackaddr$prim57833 = alloca %struct.ScmObj*, align 8
%current_45args56482 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56481)
store volatile %struct.ScmObj* %current_45args56482, %struct.ScmObj** %stackaddr$prim57833, align 8
%stackaddr$prim57834 = alloca %struct.ScmObj*, align 8
%ls048127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56482)
store volatile %struct.ScmObj* %ls048127, %struct.ScmObj** %stackaddr$prim57834, align 8
%stackaddr$prim57835 = alloca %struct.ScmObj*, align 8
%current_45args56483 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56482)
store volatile %struct.ScmObj* %current_45args56483, %struct.ScmObj** %stackaddr$prim57835, align 8
%stackaddr$prim57836 = alloca %struct.ScmObj*, align 8
%ls148126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56483)
store volatile %struct.ScmObj* %ls148126, %struct.ScmObj** %stackaddr$prim57836, align 8
%stackaddr$prim57837 = alloca %struct.ScmObj*, align 8
%anf_45bind48234 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls048127)
store volatile %struct.ScmObj* %anf_45bind48234, %struct.ScmObj** %stackaddr$prim57837, align 8
%truthy$cmp57838 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48234)
%cmp$cmp57838 = icmp eq i64 %truthy$cmp57838, 1
br i1 %cmp$cmp57838, label %truebranch$cmp57838, label %falsebranch$cmp57838
truebranch$cmp57838:
%ae50253 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56485$k483890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57839 = alloca %struct.ScmObj*, align 8
%argslist56485$k483891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148126, %struct.ScmObj* %argslist56485$k483890)
store volatile %struct.ScmObj* %argslist56485$k483891, %struct.ScmObj** %stackaddr$prim57839, align 8
%stackaddr$prim57840 = alloca %struct.ScmObj*, align 8
%argslist56485$k483892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50253, %struct.ScmObj* %argslist56485$k483891)
store volatile %struct.ScmObj* %argslist56485$k483892, %struct.ScmObj** %stackaddr$prim57840, align 8
%clofunc57841 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48389)
musttail call tailcc void %clofunc57841(%struct.ScmObj* %k48389, %struct.ScmObj* %argslist56485$k483892)
ret void
falsebranch$cmp57838:
%stackaddr$prim57842 = alloca %struct.ScmObj*, align 8
%anf_45bind48235 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls048127)
store volatile %struct.ScmObj* %anf_45bind48235, %struct.ScmObj** %stackaddr$prim57842, align 8
%ae50260 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57843 = alloca %struct.ScmObj*, align 8
%anf_45bind48236 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48124, %struct.ScmObj* %ae50260)
store volatile %struct.ScmObj* %anf_45bind48236, %struct.ScmObj** %stackaddr$prim57843, align 8
%stackaddr$prim57844 = alloca %struct.ScmObj*, align 8
%anf_45bind48237 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls048127)
store volatile %struct.ScmObj* %anf_45bind48237, %struct.ScmObj** %stackaddr$prim57844, align 8
%stackaddr$makeclosure57845 = alloca %struct.ScmObj*, align 8
%fptrToInt57846 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50263 to i64
%ae50263 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57846)
store volatile %struct.ScmObj* %ae50263, %struct.ScmObj** %stackaddr$makeclosure57845, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50263, %struct.ScmObj* %k48389, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50263, %struct.ScmObj* %anf_45bind48235, i64 1)
%argslist56490$anf_45bind482360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57847 = alloca %struct.ScmObj*, align 8
%argslist56490$anf_45bind482361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148126, %struct.ScmObj* %argslist56490$anf_45bind482360)
store volatile %struct.ScmObj* %argslist56490$anf_45bind482361, %struct.ScmObj** %stackaddr$prim57847, align 8
%stackaddr$prim57848 = alloca %struct.ScmObj*, align 8
%argslist56490$anf_45bind482362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48237, %struct.ScmObj* %argslist56490$anf_45bind482361)
store volatile %struct.ScmObj* %argslist56490$anf_45bind482362, %struct.ScmObj** %stackaddr$prim57848, align 8
%stackaddr$prim57849 = alloca %struct.ScmObj*, align 8
%argslist56490$anf_45bind482363 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50263, %struct.ScmObj* %argslist56490$anf_45bind482362)
store volatile %struct.ScmObj* %argslist56490$anf_45bind482363, %struct.ScmObj** %stackaddr$prim57849, align 8
%clofunc57850 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48236)
musttail call tailcc void %clofunc57850(%struct.ScmObj* %anf_45bind48236, %struct.ScmObj* %argslist56490$anf_45bind482363)
ret void
}

define tailcc void @proc_clo$ae50263(%struct.ScmObj* %env$ae50263,%struct.ScmObj* %current_45args56486) {
%stackaddr$env-ref57851 = alloca %struct.ScmObj*, align 8
%k48389 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50263, i64 0)
store %struct.ScmObj* %k48389, %struct.ScmObj** %stackaddr$env-ref57851
%stackaddr$env-ref57852 = alloca %struct.ScmObj*, align 8
%anf_45bind48235 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50263, i64 1)
store %struct.ScmObj* %anf_45bind48235, %struct.ScmObj** %stackaddr$env-ref57852
%stackaddr$prim57853 = alloca %struct.ScmObj*, align 8
%_95k48390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56486)
store volatile %struct.ScmObj* %_95k48390, %struct.ScmObj** %stackaddr$prim57853, align 8
%stackaddr$prim57854 = alloca %struct.ScmObj*, align 8
%current_45args56487 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56486)
store volatile %struct.ScmObj* %current_45args56487, %struct.ScmObj** %stackaddr$prim57854, align 8
%stackaddr$prim57855 = alloca %struct.ScmObj*, align 8
%anf_45bind48238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56487)
store volatile %struct.ScmObj* %anf_45bind48238, %struct.ScmObj** %stackaddr$prim57855, align 8
%stackaddr$prim57856 = alloca %struct.ScmObj*, align 8
%cpsprim48391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48235, %struct.ScmObj* %anf_45bind48238)
store volatile %struct.ScmObj* %cpsprim48391, %struct.ScmObj** %stackaddr$prim57856, align 8
%ae50269 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56489$k483890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57857 = alloca %struct.ScmObj*, align 8
%argslist56489$k483891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48391, %struct.ScmObj* %argslist56489$k483890)
store volatile %struct.ScmObj* %argslist56489$k483891, %struct.ScmObj** %stackaddr$prim57857, align 8
%stackaddr$prim57858 = alloca %struct.ScmObj*, align 8
%argslist56489$k483892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50269, %struct.ScmObj* %argslist56489$k483891)
store volatile %struct.ScmObj* %argslist56489$k483892, %struct.ScmObj** %stackaddr$prim57858, align 8
%clofunc57859 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48389)
musttail call tailcc void %clofunc57859(%struct.ScmObj* %k48389, %struct.ScmObj* %argslist56489$k483892)
ret void
}

define tailcc void @proc_clo$ae50223(%struct.ScmObj* %env$ae50223,%struct.ScmObj* %current_45args56492) {
%stackaddr$prim57860 = alloca %struct.ScmObj*, align 8
%k48392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56492)
store volatile %struct.ScmObj* %k48392, %struct.ScmObj** %stackaddr$prim57860, align 8
%stackaddr$prim57861 = alloca %struct.ScmObj*, align 8
%current_45args56493 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56492)
store volatile %struct.ScmObj* %current_45args56493, %struct.ScmObj** %stackaddr$prim57861, align 8
%stackaddr$prim57862 = alloca %struct.ScmObj*, align 8
%a48130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56493)
store volatile %struct.ScmObj* %a48130, %struct.ScmObj** %stackaddr$prim57862, align 8
%stackaddr$prim57863 = alloca %struct.ScmObj*, align 8
%current_45args56494 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56493)
store volatile %struct.ScmObj* %current_45args56494, %struct.ScmObj** %stackaddr$prim57863, align 8
%stackaddr$prim57864 = alloca %struct.ScmObj*, align 8
%b48129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56494)
store volatile %struct.ScmObj* %b48129, %struct.ScmObj** %stackaddr$prim57864, align 8
%stackaddr$prim57865 = alloca %struct.ScmObj*, align 8
%anf_45bind48233 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a48130, %struct.ScmObj* %b48129)
store volatile %struct.ScmObj* %anf_45bind48233, %struct.ScmObj** %stackaddr$prim57865, align 8
%stackaddr$prim57866 = alloca %struct.ScmObj*, align 8
%cpsprim48393 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48233)
store volatile %struct.ScmObj* %cpsprim48393, %struct.ScmObj** %stackaddr$prim57866, align 8
%ae50228 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56496$k483920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57867 = alloca %struct.ScmObj*, align 8
%argslist56496$k483921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48393, %struct.ScmObj* %argslist56496$k483920)
store volatile %struct.ScmObj* %argslist56496$k483921, %struct.ScmObj** %stackaddr$prim57867, align 8
%stackaddr$prim57868 = alloca %struct.ScmObj*, align 8
%argslist56496$k483922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50228, %struct.ScmObj* %argslist56496$k483921)
store volatile %struct.ScmObj* %argslist56496$k483922, %struct.ScmObj** %stackaddr$prim57868, align 8
%clofunc57869 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48392)
musttail call tailcc void %clofunc57869(%struct.ScmObj* %k48392, %struct.ScmObj* %argslist56496$k483922)
ret void
}

define tailcc void @proc_clo$ae50199(%struct.ScmObj* %env$ae50199,%struct.ScmObj* %current_45args56498) {
%stackaddr$prim57870 = alloca %struct.ScmObj*, align 8
%k48394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56498)
store volatile %struct.ScmObj* %k48394, %struct.ScmObj** %stackaddr$prim57870, align 8
%stackaddr$prim57871 = alloca %struct.ScmObj*, align 8
%current_45args56499 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56498)
store volatile %struct.ScmObj* %current_45args56499, %struct.ScmObj** %stackaddr$prim57871, align 8
%stackaddr$prim57872 = alloca %struct.ScmObj*, align 8
%a48133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56499)
store volatile %struct.ScmObj* %a48133, %struct.ScmObj** %stackaddr$prim57872, align 8
%stackaddr$prim57873 = alloca %struct.ScmObj*, align 8
%current_45args56500 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56499)
store volatile %struct.ScmObj* %current_45args56500, %struct.ScmObj** %stackaddr$prim57873, align 8
%stackaddr$prim57874 = alloca %struct.ScmObj*, align 8
%b48132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56500)
store volatile %struct.ScmObj* %b48132, %struct.ScmObj** %stackaddr$prim57874, align 8
%stackaddr$prim57875 = alloca %struct.ScmObj*, align 8
%anf_45bind48232 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a48133, %struct.ScmObj* %b48132)
store volatile %struct.ScmObj* %anf_45bind48232, %struct.ScmObj** %stackaddr$prim57875, align 8
%stackaddr$prim57876 = alloca %struct.ScmObj*, align 8
%cpsprim48395 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48232)
store volatile %struct.ScmObj* %cpsprim48395, %struct.ScmObj** %stackaddr$prim57876, align 8
%ae50204 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56502$k483940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57877 = alloca %struct.ScmObj*, align 8
%argslist56502$k483941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48395, %struct.ScmObj* %argslist56502$k483940)
store volatile %struct.ScmObj* %argslist56502$k483941, %struct.ScmObj** %stackaddr$prim57877, align 8
%stackaddr$prim57878 = alloca %struct.ScmObj*, align 8
%argslist56502$k483942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50204, %struct.ScmObj* %argslist56502$k483941)
store volatile %struct.ScmObj* %argslist56502$k483942, %struct.ScmObj** %stackaddr$prim57878, align 8
%clofunc57879 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48394)
musttail call tailcc void %clofunc57879(%struct.ScmObj* %k48394, %struct.ScmObj* %argslist56502$k483942)
ret void
}

define tailcc void @proc_clo$ae49805(%struct.ScmObj* %env$ae49805,%struct.ScmObj* %current_45args56505) {
%stackaddr$env-ref57880 = alloca %struct.ScmObj*, align 8
%_37foldr48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49805, i64 0)
store %struct.ScmObj* %_37foldr48057, %struct.ScmObj** %stackaddr$env-ref57880
%stackaddr$env-ref57881 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49805, i64 1)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref57881
%stackaddr$env-ref57882 = alloca %struct.ScmObj*, align 8
%_37map148083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49805, i64 2)
store %struct.ScmObj* %_37map148083, %struct.ScmObj** %stackaddr$env-ref57882
%stackaddr$prim57883 = alloca %struct.ScmObj*, align 8
%k48396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56505)
store volatile %struct.ScmObj* %k48396, %struct.ScmObj** %stackaddr$prim57883, align 8
%stackaddr$prim57884 = alloca %struct.ScmObj*, align 8
%current_45args56506 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56505)
store volatile %struct.ScmObj* %current_45args56506, %struct.ScmObj** %stackaddr$prim57884, align 8
%stackaddr$prim57885 = alloca %struct.ScmObj*, align 8
%_37foldl48135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56506)
store volatile %struct.ScmObj* %_37foldl48135, %struct.ScmObj** %stackaddr$prim57885, align 8
%ae49807 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57886 = alloca %struct.ScmObj*, align 8
%fptrToInt57887 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49808 to i64
%ae49808 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57887)
store volatile %struct.ScmObj* %ae49808, %struct.ScmObj** %stackaddr$makeclosure57886, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49808, %struct.ScmObj* %_37foldr48057, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49808, %struct.ScmObj* %_37foldl48135, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49808, %struct.ScmObj* %_37foldr148052, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49808, %struct.ScmObj* %_37map148083, i64 3)
%argslist56563$k483960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57888 = alloca %struct.ScmObj*, align 8
%argslist56563$k483961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49808, %struct.ScmObj* %argslist56563$k483960)
store volatile %struct.ScmObj* %argslist56563$k483961, %struct.ScmObj** %stackaddr$prim57888, align 8
%stackaddr$prim57889 = alloca %struct.ScmObj*, align 8
%argslist56563$k483962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49807, %struct.ScmObj* %argslist56563$k483961)
store volatile %struct.ScmObj* %argslist56563$k483962, %struct.ScmObj** %stackaddr$prim57889, align 8
%clofunc57890 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48396)
musttail call tailcc void %clofunc57890(%struct.ScmObj* %k48396, %struct.ScmObj* %argslist56563$k483962)
ret void
}

define tailcc void @proc_clo$ae49808(%struct.ScmObj* %env$ae49808,%struct.ScmObj* %args4813648397) {
%stackaddr$env-ref57891 = alloca %struct.ScmObj*, align 8
%_37foldr48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49808, i64 0)
store %struct.ScmObj* %_37foldr48057, %struct.ScmObj** %stackaddr$env-ref57891
%stackaddr$env-ref57892 = alloca %struct.ScmObj*, align 8
%_37foldl48135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49808, i64 1)
store %struct.ScmObj* %_37foldl48135, %struct.ScmObj** %stackaddr$env-ref57892
%stackaddr$env-ref57893 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49808, i64 2)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref57893
%stackaddr$env-ref57894 = alloca %struct.ScmObj*, align 8
%_37map148083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49808, i64 3)
store %struct.ScmObj* %_37map148083, %struct.ScmObj** %stackaddr$env-ref57894
%stackaddr$prim57895 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4813648397)
store volatile %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$prim57895, align 8
%stackaddr$prim57896 = alloca %struct.ScmObj*, align 8
%args48136 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4813648397)
store volatile %struct.ScmObj* %args48136, %struct.ScmObj** %stackaddr$prim57896, align 8
%stackaddr$prim57897 = alloca %struct.ScmObj*, align 8
%f48139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48136)
store volatile %struct.ScmObj* %f48139, %struct.ScmObj** %stackaddr$prim57897, align 8
%stackaddr$prim57898 = alloca %struct.ScmObj*, align 8
%anf_45bind48220 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48136)
store volatile %struct.ScmObj* %anf_45bind48220, %struct.ScmObj** %stackaddr$prim57898, align 8
%stackaddr$prim57899 = alloca %struct.ScmObj*, align 8
%acc48138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48220)
store volatile %struct.ScmObj* %acc48138, %struct.ScmObj** %stackaddr$prim57899, align 8
%stackaddr$prim57900 = alloca %struct.ScmObj*, align 8
%anf_45bind48221 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48136)
store volatile %struct.ScmObj* %anf_45bind48221, %struct.ScmObj** %stackaddr$prim57900, align 8
%stackaddr$prim57901 = alloca %struct.ScmObj*, align 8
%lsts48137 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48221)
store volatile %struct.ScmObj* %lsts48137, %struct.ScmObj** %stackaddr$prim57901, align 8
%stackaddr$makeclosure57902 = alloca %struct.ScmObj*, align 8
%fptrToInt57903 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49816 to i64
%ae49816 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt57903)
store volatile %struct.ScmObj* %ae49816, %struct.ScmObj** %stackaddr$makeclosure57902, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49816, %struct.ScmObj* %lsts48137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49816, %struct.ScmObj* %_37foldr48057, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49816, %struct.ScmObj* %_37foldl48135, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49816, %struct.ScmObj* %_37foldr148052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49816, %struct.ScmObj* %_37map148083, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49816, %struct.ScmObj* %k48398, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49816, %struct.ScmObj* %f48139, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49816, %struct.ScmObj* %acc48138, i64 7)
%ae49817 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57904 = alloca %struct.ScmObj*, align 8
%fptrToInt57905 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49818 to i64
%ae49818 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57905)
store volatile %struct.ScmObj* %ae49818, %struct.ScmObj** %stackaddr$makeclosure57904, align 8
%argslist56562$ae498160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57906 = alloca %struct.ScmObj*, align 8
%argslist56562$ae498161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49818, %struct.ScmObj* %argslist56562$ae498160)
store volatile %struct.ScmObj* %argslist56562$ae498161, %struct.ScmObj** %stackaddr$prim57906, align 8
%stackaddr$prim57907 = alloca %struct.ScmObj*, align 8
%argslist56562$ae498162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49817, %struct.ScmObj* %argslist56562$ae498161)
store volatile %struct.ScmObj* %argslist56562$ae498162, %struct.ScmObj** %stackaddr$prim57907, align 8
%clofunc57908 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49816)
musttail call tailcc void %clofunc57908(%struct.ScmObj* %ae49816, %struct.ScmObj* %argslist56562$ae498162)
ret void
}

define tailcc void @proc_clo$ae49816(%struct.ScmObj* %env$ae49816,%struct.ScmObj* %current_45args56508) {
%stackaddr$env-ref57909 = alloca %struct.ScmObj*, align 8
%lsts48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49816, i64 0)
store %struct.ScmObj* %lsts48137, %struct.ScmObj** %stackaddr$env-ref57909
%stackaddr$env-ref57910 = alloca %struct.ScmObj*, align 8
%_37foldr48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49816, i64 1)
store %struct.ScmObj* %_37foldr48057, %struct.ScmObj** %stackaddr$env-ref57910
%stackaddr$env-ref57911 = alloca %struct.ScmObj*, align 8
%_37foldl48135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49816, i64 2)
store %struct.ScmObj* %_37foldl48135, %struct.ScmObj** %stackaddr$env-ref57911
%stackaddr$env-ref57912 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49816, i64 3)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref57912
%stackaddr$env-ref57913 = alloca %struct.ScmObj*, align 8
%_37map148083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49816, i64 4)
store %struct.ScmObj* %_37map148083, %struct.ScmObj** %stackaddr$env-ref57913
%stackaddr$env-ref57914 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49816, i64 5)
store %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$env-ref57914
%stackaddr$env-ref57915 = alloca %struct.ScmObj*, align 8
%f48139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49816, i64 6)
store %struct.ScmObj* %f48139, %struct.ScmObj** %stackaddr$env-ref57915
%stackaddr$env-ref57916 = alloca %struct.ScmObj*, align 8
%acc48138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49816, i64 7)
store %struct.ScmObj* %acc48138, %struct.ScmObj** %stackaddr$env-ref57916
%stackaddr$prim57917 = alloca %struct.ScmObj*, align 8
%_95k48399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56508)
store volatile %struct.ScmObj* %_95k48399, %struct.ScmObj** %stackaddr$prim57917, align 8
%stackaddr$prim57918 = alloca %struct.ScmObj*, align 8
%current_45args56509 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56508)
store volatile %struct.ScmObj* %current_45args56509, %struct.ScmObj** %stackaddr$prim57918, align 8
%stackaddr$prim57919 = alloca %struct.ScmObj*, align 8
%anf_45bind48222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56509)
store volatile %struct.ScmObj* %anf_45bind48222, %struct.ScmObj** %stackaddr$prim57919, align 8
%stackaddr$makeclosure57920 = alloca %struct.ScmObj*, align 8
%fptrToInt57921 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49848 to i64
%ae49848 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57921)
store volatile %struct.ScmObj* %ae49848, %struct.ScmObj** %stackaddr$makeclosure57920, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49848, %struct.ScmObj* %lsts48137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49848, %struct.ScmObj* %_37foldr48057, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49848, %struct.ScmObj* %_37foldl48135, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49848, %struct.ScmObj* %_37map148083, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49848, %struct.ScmObj* %k48398, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49848, %struct.ScmObj* %f48139, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49848, %struct.ScmObj* %acc48138, i64 6)
%ae49850 = call %struct.ScmObj* @const_init_false()
%argslist56555$_37foldr1480520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57922 = alloca %struct.ScmObj*, align 8
%argslist56555$_37foldr1480521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48137, %struct.ScmObj* %argslist56555$_37foldr1480520)
store volatile %struct.ScmObj* %argslist56555$_37foldr1480521, %struct.ScmObj** %stackaddr$prim57922, align 8
%stackaddr$prim57923 = alloca %struct.ScmObj*, align 8
%argslist56555$_37foldr1480522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49850, %struct.ScmObj* %argslist56555$_37foldr1480521)
store volatile %struct.ScmObj* %argslist56555$_37foldr1480522, %struct.ScmObj** %stackaddr$prim57923, align 8
%stackaddr$prim57924 = alloca %struct.ScmObj*, align 8
%argslist56555$_37foldr1480523 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48222, %struct.ScmObj* %argslist56555$_37foldr1480522)
store volatile %struct.ScmObj* %argslist56555$_37foldr1480523, %struct.ScmObj** %stackaddr$prim57924, align 8
%stackaddr$prim57925 = alloca %struct.ScmObj*, align 8
%argslist56555$_37foldr1480524 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49848, %struct.ScmObj* %argslist56555$_37foldr1480523)
store volatile %struct.ScmObj* %argslist56555$_37foldr1480524, %struct.ScmObj** %stackaddr$prim57925, align 8
%clofunc57926 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148052)
musttail call tailcc void %clofunc57926(%struct.ScmObj* %_37foldr148052, %struct.ScmObj* %argslist56555$_37foldr1480524)
ret void
}

define tailcc void @proc_clo$ae49848(%struct.ScmObj* %env$ae49848,%struct.ScmObj* %current_45args56511) {
%stackaddr$env-ref57927 = alloca %struct.ScmObj*, align 8
%lsts48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49848, i64 0)
store %struct.ScmObj* %lsts48137, %struct.ScmObj** %stackaddr$env-ref57927
%stackaddr$env-ref57928 = alloca %struct.ScmObj*, align 8
%_37foldr48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49848, i64 1)
store %struct.ScmObj* %_37foldr48057, %struct.ScmObj** %stackaddr$env-ref57928
%stackaddr$env-ref57929 = alloca %struct.ScmObj*, align 8
%_37foldl48135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49848, i64 2)
store %struct.ScmObj* %_37foldl48135, %struct.ScmObj** %stackaddr$env-ref57929
%stackaddr$env-ref57930 = alloca %struct.ScmObj*, align 8
%_37map148083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49848, i64 3)
store %struct.ScmObj* %_37map148083, %struct.ScmObj** %stackaddr$env-ref57930
%stackaddr$env-ref57931 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49848, i64 4)
store %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$env-ref57931
%stackaddr$env-ref57932 = alloca %struct.ScmObj*, align 8
%f48139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49848, i64 5)
store %struct.ScmObj* %f48139, %struct.ScmObj** %stackaddr$env-ref57932
%stackaddr$env-ref57933 = alloca %struct.ScmObj*, align 8
%acc48138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49848, i64 6)
store %struct.ScmObj* %acc48138, %struct.ScmObj** %stackaddr$env-ref57933
%stackaddr$prim57934 = alloca %struct.ScmObj*, align 8
%_95k48400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56511)
store volatile %struct.ScmObj* %_95k48400, %struct.ScmObj** %stackaddr$prim57934, align 8
%stackaddr$prim57935 = alloca %struct.ScmObj*, align 8
%current_45args56512 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56511)
store volatile %struct.ScmObj* %current_45args56512, %struct.ScmObj** %stackaddr$prim57935, align 8
%stackaddr$prim57936 = alloca %struct.ScmObj*, align 8
%anf_45bind48223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56512)
store volatile %struct.ScmObj* %anf_45bind48223, %struct.ScmObj** %stackaddr$prim57936, align 8
%truthy$cmp57937 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48223)
%cmp$cmp57937 = icmp eq i64 %truthy$cmp57937, 1
br i1 %cmp$cmp57937, label %truebranch$cmp57937, label %falsebranch$cmp57937
truebranch$cmp57937:
%ae49859 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56514$k483980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57938 = alloca %struct.ScmObj*, align 8
%argslist56514$k483981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48138, %struct.ScmObj* %argslist56514$k483980)
store volatile %struct.ScmObj* %argslist56514$k483981, %struct.ScmObj** %stackaddr$prim57938, align 8
%stackaddr$prim57939 = alloca %struct.ScmObj*, align 8
%argslist56514$k483982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49859, %struct.ScmObj* %argslist56514$k483981)
store volatile %struct.ScmObj* %argslist56514$k483982, %struct.ScmObj** %stackaddr$prim57939, align 8
%clofunc57940 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48398)
musttail call tailcc void %clofunc57940(%struct.ScmObj* %k48398, %struct.ScmObj* %argslist56514$k483982)
ret void
falsebranch$cmp57937:
%stackaddr$makeclosure57941 = alloca %struct.ScmObj*, align 8
%fptrToInt57942 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49864 to i64
%ae49864 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57942)
store volatile %struct.ScmObj* %ae49864, %struct.ScmObj** %stackaddr$makeclosure57941, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49864, %struct.ScmObj* %lsts48137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49864, %struct.ScmObj* %_37foldr48057, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49864, %struct.ScmObj* %_37foldl48135, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49864, %struct.ScmObj* %_37map148083, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49864, %struct.ScmObj* %k48398, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49864, %struct.ScmObj* %f48139, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49864, %struct.ScmObj* %acc48138, i64 6)
%ae49865 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57943 = alloca %struct.ScmObj*, align 8
%fptrToInt57944 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49866 to i64
%ae49866 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57944)
store volatile %struct.ScmObj* %ae49866, %struct.ScmObj** %stackaddr$makeclosure57943, align 8
%argslist56554$ae498640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57945 = alloca %struct.ScmObj*, align 8
%argslist56554$ae498641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49866, %struct.ScmObj* %argslist56554$ae498640)
store volatile %struct.ScmObj* %argslist56554$ae498641, %struct.ScmObj** %stackaddr$prim57945, align 8
%stackaddr$prim57946 = alloca %struct.ScmObj*, align 8
%argslist56554$ae498642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49865, %struct.ScmObj* %argslist56554$ae498641)
store volatile %struct.ScmObj* %argslist56554$ae498642, %struct.ScmObj** %stackaddr$prim57946, align 8
%clofunc57947 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49864)
musttail call tailcc void %clofunc57947(%struct.ScmObj* %ae49864, %struct.ScmObj* %argslist56554$ae498642)
ret void
}

define tailcc void @proc_clo$ae49864(%struct.ScmObj* %env$ae49864,%struct.ScmObj* %current_45args56515) {
%stackaddr$env-ref57948 = alloca %struct.ScmObj*, align 8
%lsts48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49864, i64 0)
store %struct.ScmObj* %lsts48137, %struct.ScmObj** %stackaddr$env-ref57948
%stackaddr$env-ref57949 = alloca %struct.ScmObj*, align 8
%_37foldr48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49864, i64 1)
store %struct.ScmObj* %_37foldr48057, %struct.ScmObj** %stackaddr$env-ref57949
%stackaddr$env-ref57950 = alloca %struct.ScmObj*, align 8
%_37foldl48135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49864, i64 2)
store %struct.ScmObj* %_37foldl48135, %struct.ScmObj** %stackaddr$env-ref57950
%stackaddr$env-ref57951 = alloca %struct.ScmObj*, align 8
%_37map148083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49864, i64 3)
store %struct.ScmObj* %_37map148083, %struct.ScmObj** %stackaddr$env-ref57951
%stackaddr$env-ref57952 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49864, i64 4)
store %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$env-ref57952
%stackaddr$env-ref57953 = alloca %struct.ScmObj*, align 8
%f48139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49864, i64 5)
store %struct.ScmObj* %f48139, %struct.ScmObj** %stackaddr$env-ref57953
%stackaddr$env-ref57954 = alloca %struct.ScmObj*, align 8
%acc48138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49864, i64 6)
store %struct.ScmObj* %acc48138, %struct.ScmObj** %stackaddr$env-ref57954
%stackaddr$prim57955 = alloca %struct.ScmObj*, align 8
%_95k48401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56515)
store volatile %struct.ScmObj* %_95k48401, %struct.ScmObj** %stackaddr$prim57955, align 8
%stackaddr$prim57956 = alloca %struct.ScmObj*, align 8
%current_45args56516 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56515)
store volatile %struct.ScmObj* %current_45args56516, %struct.ScmObj** %stackaddr$prim57956, align 8
%stackaddr$prim57957 = alloca %struct.ScmObj*, align 8
%anf_45bind48224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56516)
store volatile %struct.ScmObj* %anf_45bind48224, %struct.ScmObj** %stackaddr$prim57957, align 8
%stackaddr$makeclosure57958 = alloca %struct.ScmObj*, align 8
%fptrToInt57959 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49885 to i64
%ae49885 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt57959)
store volatile %struct.ScmObj* %ae49885, %struct.ScmObj** %stackaddr$makeclosure57958, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49885, %struct.ScmObj* %lsts48137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49885, %struct.ScmObj* %_37foldr48057, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49885, %struct.ScmObj* %_37foldl48135, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49885, %struct.ScmObj* %_37map148083, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49885, %struct.ScmObj* %k48398, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49885, %struct.ScmObj* %f48139, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49885, %struct.ScmObj* %acc48138, i64 6)
%argslist56549$_37map1480830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57960 = alloca %struct.ScmObj*, align 8
%argslist56549$_37map1480831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48137, %struct.ScmObj* %argslist56549$_37map1480830)
store volatile %struct.ScmObj* %argslist56549$_37map1480831, %struct.ScmObj** %stackaddr$prim57960, align 8
%stackaddr$prim57961 = alloca %struct.ScmObj*, align 8
%argslist56549$_37map1480832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48224, %struct.ScmObj* %argslist56549$_37map1480831)
store volatile %struct.ScmObj* %argslist56549$_37map1480832, %struct.ScmObj** %stackaddr$prim57961, align 8
%stackaddr$prim57962 = alloca %struct.ScmObj*, align 8
%argslist56549$_37map1480833 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49885, %struct.ScmObj* %argslist56549$_37map1480832)
store volatile %struct.ScmObj* %argslist56549$_37map1480833, %struct.ScmObj** %stackaddr$prim57962, align 8
%clofunc57963 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148083)
musttail call tailcc void %clofunc57963(%struct.ScmObj* %_37map148083, %struct.ScmObj* %argslist56549$_37map1480833)
ret void
}

define tailcc void @proc_clo$ae49885(%struct.ScmObj* %env$ae49885,%struct.ScmObj* %current_45args56518) {
%stackaddr$env-ref57964 = alloca %struct.ScmObj*, align 8
%lsts48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49885, i64 0)
store %struct.ScmObj* %lsts48137, %struct.ScmObj** %stackaddr$env-ref57964
%stackaddr$env-ref57965 = alloca %struct.ScmObj*, align 8
%_37foldr48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49885, i64 1)
store %struct.ScmObj* %_37foldr48057, %struct.ScmObj** %stackaddr$env-ref57965
%stackaddr$env-ref57966 = alloca %struct.ScmObj*, align 8
%_37foldl48135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49885, i64 2)
store %struct.ScmObj* %_37foldl48135, %struct.ScmObj** %stackaddr$env-ref57966
%stackaddr$env-ref57967 = alloca %struct.ScmObj*, align 8
%_37map148083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49885, i64 3)
store %struct.ScmObj* %_37map148083, %struct.ScmObj** %stackaddr$env-ref57967
%stackaddr$env-ref57968 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49885, i64 4)
store %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$env-ref57968
%stackaddr$env-ref57969 = alloca %struct.ScmObj*, align 8
%f48139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49885, i64 5)
store %struct.ScmObj* %f48139, %struct.ScmObj** %stackaddr$env-ref57969
%stackaddr$env-ref57970 = alloca %struct.ScmObj*, align 8
%acc48138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49885, i64 6)
store %struct.ScmObj* %acc48138, %struct.ScmObj** %stackaddr$env-ref57970
%stackaddr$prim57971 = alloca %struct.ScmObj*, align 8
%_95k48402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56518)
store volatile %struct.ScmObj* %_95k48402, %struct.ScmObj** %stackaddr$prim57971, align 8
%stackaddr$prim57972 = alloca %struct.ScmObj*, align 8
%current_45args56519 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56518)
store volatile %struct.ScmObj* %current_45args56519, %struct.ScmObj** %stackaddr$prim57972, align 8
%stackaddr$prim57973 = alloca %struct.ScmObj*, align 8
%lsts_4348144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56519)
store volatile %struct.ScmObj* %lsts_4348144, %struct.ScmObj** %stackaddr$prim57973, align 8
%stackaddr$makeclosure57974 = alloca %struct.ScmObj*, align 8
%fptrToInt57975 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49888 to i64
%ae49888 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt57975)
store volatile %struct.ScmObj* %ae49888, %struct.ScmObj** %stackaddr$makeclosure57974, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49888, %struct.ScmObj* %lsts48137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49888, %struct.ScmObj* %_37foldr48057, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49888, %struct.ScmObj* %_37foldl48135, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49888, %struct.ScmObj* %_37map148083, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49888, %struct.ScmObj* %lsts_4348144, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49888, %struct.ScmObj* %k48398, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49888, %struct.ScmObj* %f48139, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49888, %struct.ScmObj* %acc48138, i64 7)
%ae49889 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57976 = alloca %struct.ScmObj*, align 8
%fptrToInt57977 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49890 to i64
%ae49890 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57977)
store volatile %struct.ScmObj* %ae49890, %struct.ScmObj** %stackaddr$makeclosure57976, align 8
%argslist56548$ae498880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57978 = alloca %struct.ScmObj*, align 8
%argslist56548$ae498881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49890, %struct.ScmObj* %argslist56548$ae498880)
store volatile %struct.ScmObj* %argslist56548$ae498881, %struct.ScmObj** %stackaddr$prim57978, align 8
%stackaddr$prim57979 = alloca %struct.ScmObj*, align 8
%argslist56548$ae498882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49889, %struct.ScmObj* %argslist56548$ae498881)
store volatile %struct.ScmObj* %argslist56548$ae498882, %struct.ScmObj** %stackaddr$prim57979, align 8
%clofunc57980 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49888)
musttail call tailcc void %clofunc57980(%struct.ScmObj* %ae49888, %struct.ScmObj* %argslist56548$ae498882)
ret void
}

define tailcc void @proc_clo$ae49888(%struct.ScmObj* %env$ae49888,%struct.ScmObj* %current_45args56521) {
%stackaddr$env-ref57981 = alloca %struct.ScmObj*, align 8
%lsts48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49888, i64 0)
store %struct.ScmObj* %lsts48137, %struct.ScmObj** %stackaddr$env-ref57981
%stackaddr$env-ref57982 = alloca %struct.ScmObj*, align 8
%_37foldr48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49888, i64 1)
store %struct.ScmObj* %_37foldr48057, %struct.ScmObj** %stackaddr$env-ref57982
%stackaddr$env-ref57983 = alloca %struct.ScmObj*, align 8
%_37foldl48135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49888, i64 2)
store %struct.ScmObj* %_37foldl48135, %struct.ScmObj** %stackaddr$env-ref57983
%stackaddr$env-ref57984 = alloca %struct.ScmObj*, align 8
%_37map148083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49888, i64 3)
store %struct.ScmObj* %_37map148083, %struct.ScmObj** %stackaddr$env-ref57984
%stackaddr$env-ref57985 = alloca %struct.ScmObj*, align 8
%lsts_4348144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49888, i64 4)
store %struct.ScmObj* %lsts_4348144, %struct.ScmObj** %stackaddr$env-ref57985
%stackaddr$env-ref57986 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49888, i64 5)
store %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$env-ref57986
%stackaddr$env-ref57987 = alloca %struct.ScmObj*, align 8
%f48139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49888, i64 6)
store %struct.ScmObj* %f48139, %struct.ScmObj** %stackaddr$env-ref57987
%stackaddr$env-ref57988 = alloca %struct.ScmObj*, align 8
%acc48138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49888, i64 7)
store %struct.ScmObj* %acc48138, %struct.ScmObj** %stackaddr$env-ref57988
%stackaddr$prim57989 = alloca %struct.ScmObj*, align 8
%_95k48403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56521)
store volatile %struct.ScmObj* %_95k48403, %struct.ScmObj** %stackaddr$prim57989, align 8
%stackaddr$prim57990 = alloca %struct.ScmObj*, align 8
%current_45args56522 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56521)
store volatile %struct.ScmObj* %current_45args56522, %struct.ScmObj** %stackaddr$prim57990, align 8
%stackaddr$prim57991 = alloca %struct.ScmObj*, align 8
%anf_45bind48225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56522)
store volatile %struct.ScmObj* %anf_45bind48225, %struct.ScmObj** %stackaddr$prim57991, align 8
%stackaddr$makeclosure57992 = alloca %struct.ScmObj*, align 8
%fptrToInt57993 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49909 to i64
%ae49909 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt57993)
store volatile %struct.ScmObj* %ae49909, %struct.ScmObj** %stackaddr$makeclosure57992, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49909, %struct.ScmObj* %_37foldr48057, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49909, %struct.ScmObj* %_37foldl48135, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49909, %struct.ScmObj* %lsts_4348144, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49909, %struct.ScmObj* %k48398, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49909, %struct.ScmObj* %f48139, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49909, %struct.ScmObj* %acc48138, i64 5)
%argslist56543$_37map1480830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57994 = alloca %struct.ScmObj*, align 8
%argslist56543$_37map1480831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48137, %struct.ScmObj* %argslist56543$_37map1480830)
store volatile %struct.ScmObj* %argslist56543$_37map1480831, %struct.ScmObj** %stackaddr$prim57994, align 8
%stackaddr$prim57995 = alloca %struct.ScmObj*, align 8
%argslist56543$_37map1480832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48225, %struct.ScmObj* %argslist56543$_37map1480831)
store volatile %struct.ScmObj* %argslist56543$_37map1480832, %struct.ScmObj** %stackaddr$prim57995, align 8
%stackaddr$prim57996 = alloca %struct.ScmObj*, align 8
%argslist56543$_37map1480833 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49909, %struct.ScmObj* %argslist56543$_37map1480832)
store volatile %struct.ScmObj* %argslist56543$_37map1480833, %struct.ScmObj** %stackaddr$prim57996, align 8
%clofunc57997 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148083)
musttail call tailcc void %clofunc57997(%struct.ScmObj* %_37map148083, %struct.ScmObj* %argslist56543$_37map1480833)
ret void
}

define tailcc void @proc_clo$ae49909(%struct.ScmObj* %env$ae49909,%struct.ScmObj* %current_45args56524) {
%stackaddr$env-ref57998 = alloca %struct.ScmObj*, align 8
%_37foldr48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49909, i64 0)
store %struct.ScmObj* %_37foldr48057, %struct.ScmObj** %stackaddr$env-ref57998
%stackaddr$env-ref57999 = alloca %struct.ScmObj*, align 8
%_37foldl48135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49909, i64 1)
store %struct.ScmObj* %_37foldl48135, %struct.ScmObj** %stackaddr$env-ref57999
%stackaddr$env-ref58000 = alloca %struct.ScmObj*, align 8
%lsts_4348144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49909, i64 2)
store %struct.ScmObj* %lsts_4348144, %struct.ScmObj** %stackaddr$env-ref58000
%stackaddr$env-ref58001 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49909, i64 3)
store %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$env-ref58001
%stackaddr$env-ref58002 = alloca %struct.ScmObj*, align 8
%f48139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49909, i64 4)
store %struct.ScmObj* %f48139, %struct.ScmObj** %stackaddr$env-ref58002
%stackaddr$env-ref58003 = alloca %struct.ScmObj*, align 8
%acc48138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49909, i64 5)
store %struct.ScmObj* %acc48138, %struct.ScmObj** %stackaddr$env-ref58003
%stackaddr$prim58004 = alloca %struct.ScmObj*, align 8
%_95k48404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56524)
store volatile %struct.ScmObj* %_95k48404, %struct.ScmObj** %stackaddr$prim58004, align 8
%stackaddr$prim58005 = alloca %struct.ScmObj*, align 8
%current_45args56525 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56524)
store volatile %struct.ScmObj* %current_45args56525, %struct.ScmObj** %stackaddr$prim58005, align 8
%stackaddr$prim58006 = alloca %struct.ScmObj*, align 8
%vs48142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56525)
store volatile %struct.ScmObj* %vs48142, %struct.ScmObj** %stackaddr$prim58006, align 8
%stackaddr$makeclosure58007 = alloca %struct.ScmObj*, align 8
%fptrToInt58008 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49912 to i64
%ae49912 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58008)
store volatile %struct.ScmObj* %ae49912, %struct.ScmObj** %stackaddr$makeclosure58007, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49912, %struct.ScmObj* %k48398, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49912, %struct.ScmObj* %vs48142, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49912, %struct.ScmObj* %_37foldr48057, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49912, %struct.ScmObj* %_37foldl48135, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49912, %struct.ScmObj* %lsts_4348144, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49912, %struct.ScmObj* %f48139, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49912, %struct.ScmObj* %acc48138, i64 6)
%ae49913 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58009 = alloca %struct.ScmObj*, align 8
%fptrToInt58010 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49914 to i64
%ae49914 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58010)
store volatile %struct.ScmObj* %ae49914, %struct.ScmObj** %stackaddr$makeclosure58009, align 8
%argslist56542$ae499120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58011 = alloca %struct.ScmObj*, align 8
%argslist56542$ae499121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49914, %struct.ScmObj* %argslist56542$ae499120)
store volatile %struct.ScmObj* %argslist56542$ae499121, %struct.ScmObj** %stackaddr$prim58011, align 8
%stackaddr$prim58012 = alloca %struct.ScmObj*, align 8
%argslist56542$ae499122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49913, %struct.ScmObj* %argslist56542$ae499121)
store volatile %struct.ScmObj* %argslist56542$ae499122, %struct.ScmObj** %stackaddr$prim58012, align 8
%clofunc58013 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49912)
musttail call tailcc void %clofunc58013(%struct.ScmObj* %ae49912, %struct.ScmObj* %argslist56542$ae499122)
ret void
}

define tailcc void @proc_clo$ae49912(%struct.ScmObj* %env$ae49912,%struct.ScmObj* %current_45args56527) {
%stackaddr$env-ref58014 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49912, i64 0)
store %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$env-ref58014
%stackaddr$env-ref58015 = alloca %struct.ScmObj*, align 8
%vs48142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49912, i64 1)
store %struct.ScmObj* %vs48142, %struct.ScmObj** %stackaddr$env-ref58015
%stackaddr$env-ref58016 = alloca %struct.ScmObj*, align 8
%_37foldr48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49912, i64 2)
store %struct.ScmObj* %_37foldr48057, %struct.ScmObj** %stackaddr$env-ref58016
%stackaddr$env-ref58017 = alloca %struct.ScmObj*, align 8
%_37foldl48135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49912, i64 3)
store %struct.ScmObj* %_37foldl48135, %struct.ScmObj** %stackaddr$env-ref58017
%stackaddr$env-ref58018 = alloca %struct.ScmObj*, align 8
%lsts_4348144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49912, i64 4)
store %struct.ScmObj* %lsts_4348144, %struct.ScmObj** %stackaddr$env-ref58018
%stackaddr$env-ref58019 = alloca %struct.ScmObj*, align 8
%f48139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49912, i64 5)
store %struct.ScmObj* %f48139, %struct.ScmObj** %stackaddr$env-ref58019
%stackaddr$env-ref58020 = alloca %struct.ScmObj*, align 8
%acc48138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49912, i64 6)
store %struct.ScmObj* %acc48138, %struct.ScmObj** %stackaddr$env-ref58020
%stackaddr$prim58021 = alloca %struct.ScmObj*, align 8
%_95k48405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56527)
store volatile %struct.ScmObj* %_95k48405, %struct.ScmObj** %stackaddr$prim58021, align 8
%stackaddr$prim58022 = alloca %struct.ScmObj*, align 8
%current_45args56528 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56527)
store volatile %struct.ScmObj* %current_45args56528, %struct.ScmObj** %stackaddr$prim58022, align 8
%stackaddr$prim58023 = alloca %struct.ScmObj*, align 8
%anf_45bind48226 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56528)
store volatile %struct.ScmObj* %anf_45bind48226, %struct.ScmObj** %stackaddr$prim58023, align 8
%ae49935 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58024 = alloca %struct.ScmObj*, align 8
%anf_45bind48227 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48138, %struct.ScmObj* %ae49935)
store volatile %struct.ScmObj* %anf_45bind48227, %struct.ScmObj** %stackaddr$prim58024, align 8
%stackaddr$makeclosure58025 = alloca %struct.ScmObj*, align 8
%fptrToInt58026 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49937 to i64
%ae49937 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58026)
store volatile %struct.ScmObj* %ae49937, %struct.ScmObj** %stackaddr$makeclosure58025, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49937, %struct.ScmObj* %_37foldl48135, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49937, %struct.ScmObj* %lsts_4348144, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49937, %struct.ScmObj* %k48398, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49937, %struct.ScmObj* %f48139, i64 3)
%argslist56536$_37foldr480570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58027 = alloca %struct.ScmObj*, align 8
%argslist56536$_37foldr480571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48142, %struct.ScmObj* %argslist56536$_37foldr480570)
store volatile %struct.ScmObj* %argslist56536$_37foldr480571, %struct.ScmObj** %stackaddr$prim58027, align 8
%stackaddr$prim58028 = alloca %struct.ScmObj*, align 8
%argslist56536$_37foldr480572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48227, %struct.ScmObj* %argslist56536$_37foldr480571)
store volatile %struct.ScmObj* %argslist56536$_37foldr480572, %struct.ScmObj** %stackaddr$prim58028, align 8
%stackaddr$prim58029 = alloca %struct.ScmObj*, align 8
%argslist56536$_37foldr480573 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48226, %struct.ScmObj* %argslist56536$_37foldr480572)
store volatile %struct.ScmObj* %argslist56536$_37foldr480573, %struct.ScmObj** %stackaddr$prim58029, align 8
%stackaddr$prim58030 = alloca %struct.ScmObj*, align 8
%argslist56536$_37foldr480574 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49937, %struct.ScmObj* %argslist56536$_37foldr480573)
store volatile %struct.ScmObj* %argslist56536$_37foldr480574, %struct.ScmObj** %stackaddr$prim58030, align 8
%clofunc58031 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48057)
musttail call tailcc void %clofunc58031(%struct.ScmObj* %_37foldr48057, %struct.ScmObj* %argslist56536$_37foldr480574)
ret void
}

define tailcc void @proc_clo$ae49937(%struct.ScmObj* %env$ae49937,%struct.ScmObj* %current_45args56530) {
%stackaddr$env-ref58032 = alloca %struct.ScmObj*, align 8
%_37foldl48135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49937, i64 0)
store %struct.ScmObj* %_37foldl48135, %struct.ScmObj** %stackaddr$env-ref58032
%stackaddr$env-ref58033 = alloca %struct.ScmObj*, align 8
%lsts_4348144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49937, i64 1)
store %struct.ScmObj* %lsts_4348144, %struct.ScmObj** %stackaddr$env-ref58033
%stackaddr$env-ref58034 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49937, i64 2)
store %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$env-ref58034
%stackaddr$env-ref58035 = alloca %struct.ScmObj*, align 8
%f48139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49937, i64 3)
store %struct.ScmObj* %f48139, %struct.ScmObj** %stackaddr$env-ref58035
%stackaddr$prim58036 = alloca %struct.ScmObj*, align 8
%_95k48406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56530)
store volatile %struct.ScmObj* %_95k48406, %struct.ScmObj** %stackaddr$prim58036, align 8
%stackaddr$prim58037 = alloca %struct.ScmObj*, align 8
%current_45args56531 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56530)
store volatile %struct.ScmObj* %current_45args56531, %struct.ScmObj** %stackaddr$prim58037, align 8
%stackaddr$prim58038 = alloca %struct.ScmObj*, align 8
%anf_45bind48228 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56531)
store volatile %struct.ScmObj* %anf_45bind48228, %struct.ScmObj** %stackaddr$prim58038, align 8
%stackaddr$makeclosure58039 = alloca %struct.ScmObj*, align 8
%fptrToInt58040 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49941 to i64
%ae49941 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58040)
store volatile %struct.ScmObj* %ae49941, %struct.ScmObj** %stackaddr$makeclosure58039, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49941, %struct.ScmObj* %_37foldl48135, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49941, %struct.ScmObj* %lsts_4348144, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49941, %struct.ScmObj* %k48398, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49941, %struct.ScmObj* %f48139, i64 3)
%stackaddr$prim58041 = alloca %struct.ScmObj*, align 8
%cpsargs48409 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49941, %struct.ScmObj* %anf_45bind48228)
store volatile %struct.ScmObj* %cpsargs48409, %struct.ScmObj** %stackaddr$prim58041, align 8
%clofunc58042 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48139)
musttail call tailcc void %clofunc58042(%struct.ScmObj* %f48139, %struct.ScmObj* %cpsargs48409)
ret void
}

define tailcc void @proc_clo$ae49941(%struct.ScmObj* %env$ae49941,%struct.ScmObj* %current_45args56533) {
%stackaddr$env-ref58043 = alloca %struct.ScmObj*, align 8
%_37foldl48135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49941, i64 0)
store %struct.ScmObj* %_37foldl48135, %struct.ScmObj** %stackaddr$env-ref58043
%stackaddr$env-ref58044 = alloca %struct.ScmObj*, align 8
%lsts_4348144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49941, i64 1)
store %struct.ScmObj* %lsts_4348144, %struct.ScmObj** %stackaddr$env-ref58044
%stackaddr$env-ref58045 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49941, i64 2)
store %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$env-ref58045
%stackaddr$env-ref58046 = alloca %struct.ScmObj*, align 8
%f48139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49941, i64 3)
store %struct.ScmObj* %f48139, %struct.ScmObj** %stackaddr$env-ref58046
%stackaddr$prim58047 = alloca %struct.ScmObj*, align 8
%_95k48407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56533)
store volatile %struct.ScmObj* %_95k48407, %struct.ScmObj** %stackaddr$prim58047, align 8
%stackaddr$prim58048 = alloca %struct.ScmObj*, align 8
%current_45args56534 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56533)
store volatile %struct.ScmObj* %current_45args56534, %struct.ScmObj** %stackaddr$prim58048, align 8
%stackaddr$prim58049 = alloca %struct.ScmObj*, align 8
%acc_4348146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56534)
store volatile %struct.ScmObj* %acc_4348146, %struct.ScmObj** %stackaddr$prim58049, align 8
%stackaddr$prim58050 = alloca %struct.ScmObj*, align 8
%anf_45bind48229 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4348146, %struct.ScmObj* %lsts_4348144)
store volatile %struct.ScmObj* %anf_45bind48229, %struct.ScmObj** %stackaddr$prim58050, align 8
%stackaddr$prim58051 = alloca %struct.ScmObj*, align 8
%anf_45bind48230 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48139, %struct.ScmObj* %anf_45bind48229)
store volatile %struct.ScmObj* %anf_45bind48230, %struct.ScmObj** %stackaddr$prim58051, align 8
%stackaddr$prim58052 = alloca %struct.ScmObj*, align 8
%cpsargs48408 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48398, %struct.ScmObj* %anf_45bind48230)
store volatile %struct.ScmObj* %cpsargs48408, %struct.ScmObj** %stackaddr$prim58052, align 8
%clofunc58053 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl48135)
musttail call tailcc void %clofunc58053(%struct.ScmObj* %_37foldl48135, %struct.ScmObj* %cpsargs48408)
ret void
}

define tailcc void @proc_clo$ae49914(%struct.ScmObj* %env$ae49914,%struct.ScmObj* %current_45args56537) {
%stackaddr$prim58054 = alloca %struct.ScmObj*, align 8
%k48410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56537)
store volatile %struct.ScmObj* %k48410, %struct.ScmObj** %stackaddr$prim58054, align 8
%stackaddr$prim58055 = alloca %struct.ScmObj*, align 8
%current_45args56538 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56537)
store volatile %struct.ScmObj* %current_45args56538, %struct.ScmObj** %stackaddr$prim58055, align 8
%stackaddr$prim58056 = alloca %struct.ScmObj*, align 8
%a48148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56538)
store volatile %struct.ScmObj* %a48148, %struct.ScmObj** %stackaddr$prim58056, align 8
%stackaddr$prim58057 = alloca %struct.ScmObj*, align 8
%current_45args56539 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56538)
store volatile %struct.ScmObj* %current_45args56539, %struct.ScmObj** %stackaddr$prim58057, align 8
%stackaddr$prim58058 = alloca %struct.ScmObj*, align 8
%b48147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56539)
store volatile %struct.ScmObj* %b48147, %struct.ScmObj** %stackaddr$prim58058, align 8
%stackaddr$prim58059 = alloca %struct.ScmObj*, align 8
%cpsprim48411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48148, %struct.ScmObj* %b48147)
store volatile %struct.ScmObj* %cpsprim48411, %struct.ScmObj** %stackaddr$prim58059, align 8
%ae49918 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56541$k484100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58060 = alloca %struct.ScmObj*, align 8
%argslist56541$k484101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48411, %struct.ScmObj* %argslist56541$k484100)
store volatile %struct.ScmObj* %argslist56541$k484101, %struct.ScmObj** %stackaddr$prim58060, align 8
%stackaddr$prim58061 = alloca %struct.ScmObj*, align 8
%argslist56541$k484102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49918, %struct.ScmObj* %argslist56541$k484101)
store volatile %struct.ScmObj* %argslist56541$k484102, %struct.ScmObj** %stackaddr$prim58061, align 8
%clofunc58062 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48410)
musttail call tailcc void %clofunc58062(%struct.ScmObj* %k48410, %struct.ScmObj* %argslist56541$k484102)
ret void
}

define tailcc void @proc_clo$ae49890(%struct.ScmObj* %env$ae49890,%struct.ScmObj* %current_45args56544) {
%stackaddr$prim58063 = alloca %struct.ScmObj*, align 8
%k48412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56544)
store volatile %struct.ScmObj* %k48412, %struct.ScmObj** %stackaddr$prim58063, align 8
%stackaddr$prim58064 = alloca %struct.ScmObj*, align 8
%current_45args56545 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56544)
store volatile %struct.ScmObj* %current_45args56545, %struct.ScmObj** %stackaddr$prim58064, align 8
%stackaddr$prim58065 = alloca %struct.ScmObj*, align 8
%x48143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56545)
store volatile %struct.ScmObj* %x48143, %struct.ScmObj** %stackaddr$prim58065, align 8
%stackaddr$prim58066 = alloca %struct.ScmObj*, align 8
%cpsprim48413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48143)
store volatile %struct.ScmObj* %cpsprim48413, %struct.ScmObj** %stackaddr$prim58066, align 8
%ae49893 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56547$k484120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58067 = alloca %struct.ScmObj*, align 8
%argslist56547$k484121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48413, %struct.ScmObj* %argslist56547$k484120)
store volatile %struct.ScmObj* %argslist56547$k484121, %struct.ScmObj** %stackaddr$prim58067, align 8
%stackaddr$prim58068 = alloca %struct.ScmObj*, align 8
%argslist56547$k484122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49893, %struct.ScmObj* %argslist56547$k484121)
store volatile %struct.ScmObj* %argslist56547$k484122, %struct.ScmObj** %stackaddr$prim58068, align 8
%clofunc58069 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48412)
musttail call tailcc void %clofunc58069(%struct.ScmObj* %k48412, %struct.ScmObj* %argslist56547$k484122)
ret void
}

define tailcc void @proc_clo$ae49866(%struct.ScmObj* %env$ae49866,%struct.ScmObj* %current_45args56550) {
%stackaddr$prim58070 = alloca %struct.ScmObj*, align 8
%k48414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56550)
store volatile %struct.ScmObj* %k48414, %struct.ScmObj** %stackaddr$prim58070, align 8
%stackaddr$prim58071 = alloca %struct.ScmObj*, align 8
%current_45args56551 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56550)
store volatile %struct.ScmObj* %current_45args56551, %struct.ScmObj** %stackaddr$prim58071, align 8
%stackaddr$prim58072 = alloca %struct.ScmObj*, align 8
%x48145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56551)
store volatile %struct.ScmObj* %x48145, %struct.ScmObj** %stackaddr$prim58072, align 8
%stackaddr$prim58073 = alloca %struct.ScmObj*, align 8
%cpsprim48415 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48145)
store volatile %struct.ScmObj* %cpsprim48415, %struct.ScmObj** %stackaddr$prim58073, align 8
%ae49869 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56553$k484140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58074 = alloca %struct.ScmObj*, align 8
%argslist56553$k484141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48415, %struct.ScmObj* %argslist56553$k484140)
store volatile %struct.ScmObj* %argslist56553$k484141, %struct.ScmObj** %stackaddr$prim58074, align 8
%stackaddr$prim58075 = alloca %struct.ScmObj*, align 8
%argslist56553$k484142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49869, %struct.ScmObj* %argslist56553$k484141)
store volatile %struct.ScmObj* %argslist56553$k484142, %struct.ScmObj** %stackaddr$prim58075, align 8
%clofunc58076 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48414)
musttail call tailcc void %clofunc58076(%struct.ScmObj* %k48414, %struct.ScmObj* %argslist56553$k484142)
ret void
}

define tailcc void @proc_clo$ae49818(%struct.ScmObj* %env$ae49818,%struct.ScmObj* %current_45args56556) {
%stackaddr$prim58077 = alloca %struct.ScmObj*, align 8
%k48416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56556)
store volatile %struct.ScmObj* %k48416, %struct.ScmObj** %stackaddr$prim58077, align 8
%stackaddr$prim58078 = alloca %struct.ScmObj*, align 8
%current_45args56557 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56556)
store volatile %struct.ScmObj* %current_45args56557, %struct.ScmObj** %stackaddr$prim58078, align 8
%stackaddr$prim58079 = alloca %struct.ScmObj*, align 8
%lst48141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56557)
store volatile %struct.ScmObj* %lst48141, %struct.ScmObj** %stackaddr$prim58079, align 8
%stackaddr$prim58080 = alloca %struct.ScmObj*, align 8
%current_45args56558 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56557)
store volatile %struct.ScmObj* %current_45args56558, %struct.ScmObj** %stackaddr$prim58080, align 8
%stackaddr$prim58081 = alloca %struct.ScmObj*, align 8
%b48140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56558)
store volatile %struct.ScmObj* %b48140, %struct.ScmObj** %stackaddr$prim58081, align 8
%truthy$cmp58082 = call i64 @is_truthy_value(%struct.ScmObj* %b48140)
%cmp$cmp58082 = icmp eq i64 %truthy$cmp58082, 1
br i1 %cmp$cmp58082, label %truebranch$cmp58082, label %falsebranch$cmp58082
truebranch$cmp58082:
%ae49821 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56560$k484160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58083 = alloca %struct.ScmObj*, align 8
%argslist56560$k484161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48140, %struct.ScmObj* %argslist56560$k484160)
store volatile %struct.ScmObj* %argslist56560$k484161, %struct.ScmObj** %stackaddr$prim58083, align 8
%stackaddr$prim58084 = alloca %struct.ScmObj*, align 8
%argslist56560$k484162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49821, %struct.ScmObj* %argslist56560$k484161)
store volatile %struct.ScmObj* %argslist56560$k484162, %struct.ScmObj** %stackaddr$prim58084, align 8
%clofunc58085 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48416)
musttail call tailcc void %clofunc58085(%struct.ScmObj* %k48416, %struct.ScmObj* %argslist56560$k484162)
ret void
falsebranch$cmp58082:
%stackaddr$prim58086 = alloca %struct.ScmObj*, align 8
%cpsprim48417 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48141)
store volatile %struct.ScmObj* %cpsprim48417, %struct.ScmObj** %stackaddr$prim58086, align 8
%ae49828 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56561$k484160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58087 = alloca %struct.ScmObj*, align 8
%argslist56561$k484161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48417, %struct.ScmObj* %argslist56561$k484160)
store volatile %struct.ScmObj* %argslist56561$k484161, %struct.ScmObj** %stackaddr$prim58087, align 8
%stackaddr$prim58088 = alloca %struct.ScmObj*, align 8
%argslist56561$k484162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49828, %struct.ScmObj* %argslist56561$k484161)
store volatile %struct.ScmObj* %argslist56561$k484162, %struct.ScmObj** %stackaddr$prim58088, align 8
%clofunc58089 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48416)
musttail call tailcc void %clofunc58089(%struct.ScmObj* %k48416, %struct.ScmObj* %argslist56561$k484162)
ret void
}

define tailcc void @proc_clo$ae49659(%struct.ScmObj* %env$ae49659,%struct.ScmObj* %args4807948418) {
%stackaddr$env-ref58090 = alloca %struct.ScmObj*, align 8
%_37foldr48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49659, i64 0)
store %struct.ScmObj* %_37foldr48057, %struct.ScmObj** %stackaddr$env-ref58090
%stackaddr$env-ref58091 = alloca %struct.ScmObj*, align 8
%_37drop_45right48071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49659, i64 1)
store %struct.ScmObj* %_37drop_45right48071, %struct.ScmObj** %stackaddr$env-ref58091
%stackaddr$env-ref58092 = alloca %struct.ScmObj*, align 8
%_37last48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49659, i64 2)
store %struct.ScmObj* %_37last48074, %struct.ScmObj** %stackaddr$env-ref58092
%stackaddr$prim58093 = alloca %struct.ScmObj*, align 8
%k48419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4807948418)
store volatile %struct.ScmObj* %k48419, %struct.ScmObj** %stackaddr$prim58093, align 8
%stackaddr$prim58094 = alloca %struct.ScmObj*, align 8
%args48079 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4807948418)
store volatile %struct.ScmObj* %args48079, %struct.ScmObj** %stackaddr$prim58094, align 8
%stackaddr$prim58095 = alloca %struct.ScmObj*, align 8
%f48081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48079)
store volatile %struct.ScmObj* %f48081, %struct.ScmObj** %stackaddr$prim58095, align 8
%stackaddr$prim58096 = alloca %struct.ScmObj*, align 8
%lsts48080 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48079)
store volatile %struct.ScmObj* %lsts48080, %struct.ScmObj** %stackaddr$prim58096, align 8
%stackaddr$makeclosure58097 = alloca %struct.ScmObj*, align 8
%fptrToInt58098 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49664 to i64
%ae49664 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58098)
store volatile %struct.ScmObj* %ae49664, %struct.ScmObj** %stackaddr$makeclosure58097, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49664, %struct.ScmObj* %_37foldr48057, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49664, %struct.ScmObj* %k48419, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49664, %struct.ScmObj* %lsts48080, i64 2)
%ae49665 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58099 = alloca %struct.ScmObj*, align 8
%fptrToInt58100 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49666 to i64
%ae49666 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58100)
store volatile %struct.ScmObj* %ae49666, %struct.ScmObj** %stackaddr$makeclosure58099, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49666, %struct.ScmObj* %_37drop_45right48071, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49666, %struct.ScmObj* %f48081, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49666, %struct.ScmObj* %_37last48074, i64 2)
%argslist56580$ae496640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58101 = alloca %struct.ScmObj*, align 8
%argslist56580$ae496641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49666, %struct.ScmObj* %argslist56580$ae496640)
store volatile %struct.ScmObj* %argslist56580$ae496641, %struct.ScmObj** %stackaddr$prim58101, align 8
%stackaddr$prim58102 = alloca %struct.ScmObj*, align 8
%argslist56580$ae496642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49665, %struct.ScmObj* %argslist56580$ae496641)
store volatile %struct.ScmObj* %argslist56580$ae496642, %struct.ScmObj** %stackaddr$prim58102, align 8
%clofunc58103 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49664)
musttail call tailcc void %clofunc58103(%struct.ScmObj* %ae49664, %struct.ScmObj* %argslist56580$ae496642)
ret void
}

define tailcc void @proc_clo$ae49664(%struct.ScmObj* %env$ae49664,%struct.ScmObj* %current_45args56565) {
%stackaddr$env-ref58104 = alloca %struct.ScmObj*, align 8
%_37foldr48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49664, i64 0)
store %struct.ScmObj* %_37foldr48057, %struct.ScmObj** %stackaddr$env-ref58104
%stackaddr$env-ref58105 = alloca %struct.ScmObj*, align 8
%k48419 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49664, i64 1)
store %struct.ScmObj* %k48419, %struct.ScmObj** %stackaddr$env-ref58105
%stackaddr$env-ref58106 = alloca %struct.ScmObj*, align 8
%lsts48080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49664, i64 2)
store %struct.ScmObj* %lsts48080, %struct.ScmObj** %stackaddr$env-ref58106
%stackaddr$prim58107 = alloca %struct.ScmObj*, align 8
%_95k48420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56565)
store volatile %struct.ScmObj* %_95k48420, %struct.ScmObj** %stackaddr$prim58107, align 8
%stackaddr$prim58108 = alloca %struct.ScmObj*, align 8
%current_45args56566 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56565)
store volatile %struct.ScmObj* %current_45args56566, %struct.ScmObj** %stackaddr$prim58108, align 8
%stackaddr$prim58109 = alloca %struct.ScmObj*, align 8
%anf_45bind48217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56566)
store volatile %struct.ScmObj* %anf_45bind48217, %struct.ScmObj** %stackaddr$prim58109, align 8
%ae49727 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58110 = alloca %struct.ScmObj*, align 8
%anf_45bind48218 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49727, %struct.ScmObj* %lsts48080)
store volatile %struct.ScmObj* %anf_45bind48218, %struct.ScmObj** %stackaddr$prim58110, align 8
%stackaddr$prim58111 = alloca %struct.ScmObj*, align 8
%anf_45bind48219 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48217, %struct.ScmObj* %anf_45bind48218)
store volatile %struct.ScmObj* %anf_45bind48219, %struct.ScmObj** %stackaddr$prim58111, align 8
%stackaddr$prim58112 = alloca %struct.ScmObj*, align 8
%cpsargs48421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48419, %struct.ScmObj* %anf_45bind48219)
store volatile %struct.ScmObj* %cpsargs48421, %struct.ScmObj** %stackaddr$prim58112, align 8
%clofunc58113 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48057)
musttail call tailcc void %clofunc58113(%struct.ScmObj* %_37foldr48057, %struct.ScmObj* %cpsargs48421)
ret void
}

define tailcc void @proc_clo$ae49666(%struct.ScmObj* %env$ae49666,%struct.ScmObj* %fargs4808248422) {
%stackaddr$env-ref58114 = alloca %struct.ScmObj*, align 8
%_37drop_45right48071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49666, i64 0)
store %struct.ScmObj* %_37drop_45right48071, %struct.ScmObj** %stackaddr$env-ref58114
%stackaddr$env-ref58115 = alloca %struct.ScmObj*, align 8
%f48081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49666, i64 1)
store %struct.ScmObj* %f48081, %struct.ScmObj** %stackaddr$env-ref58115
%stackaddr$env-ref58116 = alloca %struct.ScmObj*, align 8
%_37last48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49666, i64 2)
store %struct.ScmObj* %_37last48074, %struct.ScmObj** %stackaddr$env-ref58116
%stackaddr$prim58117 = alloca %struct.ScmObj*, align 8
%k48423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4808248422)
store volatile %struct.ScmObj* %k48423, %struct.ScmObj** %stackaddr$prim58117, align 8
%stackaddr$prim58118 = alloca %struct.ScmObj*, align 8
%fargs48082 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4808248422)
store volatile %struct.ScmObj* %fargs48082, %struct.ScmObj** %stackaddr$prim58118, align 8
%stackaddr$makeclosure58119 = alloca %struct.ScmObj*, align 8
%fptrToInt58120 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49670 to i64
%ae49670 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58120)
store volatile %struct.ScmObj* %ae49670, %struct.ScmObj** %stackaddr$makeclosure58119, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49670, %struct.ScmObj* %k48423, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49670, %struct.ScmObj* %fargs48082, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49670, %struct.ScmObj* %f48081, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49670, %struct.ScmObj* %_37last48074, i64 3)
%ae49672 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist56579$_37drop_45right480710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58121 = alloca %struct.ScmObj*, align 8
%argslist56579$_37drop_45right480711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49672, %struct.ScmObj* %argslist56579$_37drop_45right480710)
store volatile %struct.ScmObj* %argslist56579$_37drop_45right480711, %struct.ScmObj** %stackaddr$prim58121, align 8
%stackaddr$prim58122 = alloca %struct.ScmObj*, align 8
%argslist56579$_37drop_45right480712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48082, %struct.ScmObj* %argslist56579$_37drop_45right480711)
store volatile %struct.ScmObj* %argslist56579$_37drop_45right480712, %struct.ScmObj** %stackaddr$prim58122, align 8
%stackaddr$prim58123 = alloca %struct.ScmObj*, align 8
%argslist56579$_37drop_45right480713 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49670, %struct.ScmObj* %argslist56579$_37drop_45right480712)
store volatile %struct.ScmObj* %argslist56579$_37drop_45right480713, %struct.ScmObj** %stackaddr$prim58123, align 8
%clofunc58124 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right48071)
musttail call tailcc void %clofunc58124(%struct.ScmObj* %_37drop_45right48071, %struct.ScmObj* %argslist56579$_37drop_45right480713)
ret void
}

define tailcc void @proc_clo$ae49670(%struct.ScmObj* %env$ae49670,%struct.ScmObj* %current_45args56568) {
%stackaddr$env-ref58125 = alloca %struct.ScmObj*, align 8
%k48423 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49670, i64 0)
store %struct.ScmObj* %k48423, %struct.ScmObj** %stackaddr$env-ref58125
%stackaddr$env-ref58126 = alloca %struct.ScmObj*, align 8
%fargs48082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49670, i64 1)
store %struct.ScmObj* %fargs48082, %struct.ScmObj** %stackaddr$env-ref58126
%stackaddr$env-ref58127 = alloca %struct.ScmObj*, align 8
%f48081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49670, i64 2)
store %struct.ScmObj* %f48081, %struct.ScmObj** %stackaddr$env-ref58127
%stackaddr$env-ref58128 = alloca %struct.ScmObj*, align 8
%_37last48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49670, i64 3)
store %struct.ScmObj* %_37last48074, %struct.ScmObj** %stackaddr$env-ref58128
%stackaddr$prim58129 = alloca %struct.ScmObj*, align 8
%_95k48424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56568)
store volatile %struct.ScmObj* %_95k48424, %struct.ScmObj** %stackaddr$prim58129, align 8
%stackaddr$prim58130 = alloca %struct.ScmObj*, align 8
%current_45args56569 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56568)
store volatile %struct.ScmObj* %current_45args56569, %struct.ScmObj** %stackaddr$prim58130, align 8
%stackaddr$prim58131 = alloca %struct.ScmObj*, align 8
%anf_45bind48214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56569)
store volatile %struct.ScmObj* %anf_45bind48214, %struct.ScmObj** %stackaddr$prim58131, align 8
%stackaddr$makeclosure58132 = alloca %struct.ScmObj*, align 8
%fptrToInt58133 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49677 to i64
%ae49677 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58133)
store volatile %struct.ScmObj* %ae49677, %struct.ScmObj** %stackaddr$makeclosure58132, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49677, %struct.ScmObj* %k48423, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49677, %struct.ScmObj* %fargs48082, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49677, %struct.ScmObj* %_37last48074, i64 2)
%stackaddr$prim58134 = alloca %struct.ScmObj*, align 8
%cpsargs48428 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49677, %struct.ScmObj* %anf_45bind48214)
store volatile %struct.ScmObj* %cpsargs48428, %struct.ScmObj** %stackaddr$prim58134, align 8
%clofunc58135 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48081)
musttail call tailcc void %clofunc58135(%struct.ScmObj* %f48081, %struct.ScmObj* %cpsargs48428)
ret void
}

define tailcc void @proc_clo$ae49677(%struct.ScmObj* %env$ae49677,%struct.ScmObj* %current_45args56571) {
%stackaddr$env-ref58136 = alloca %struct.ScmObj*, align 8
%k48423 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49677, i64 0)
store %struct.ScmObj* %k48423, %struct.ScmObj** %stackaddr$env-ref58136
%stackaddr$env-ref58137 = alloca %struct.ScmObj*, align 8
%fargs48082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49677, i64 1)
store %struct.ScmObj* %fargs48082, %struct.ScmObj** %stackaddr$env-ref58137
%stackaddr$env-ref58138 = alloca %struct.ScmObj*, align 8
%_37last48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49677, i64 2)
store %struct.ScmObj* %_37last48074, %struct.ScmObj** %stackaddr$env-ref58138
%stackaddr$prim58139 = alloca %struct.ScmObj*, align 8
%_95k48425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56571)
store volatile %struct.ScmObj* %_95k48425, %struct.ScmObj** %stackaddr$prim58139, align 8
%stackaddr$prim58140 = alloca %struct.ScmObj*, align 8
%current_45args56572 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56571)
store volatile %struct.ScmObj* %current_45args56572, %struct.ScmObj** %stackaddr$prim58140, align 8
%stackaddr$prim58141 = alloca %struct.ScmObj*, align 8
%anf_45bind48215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56572)
store volatile %struct.ScmObj* %anf_45bind48215, %struct.ScmObj** %stackaddr$prim58141, align 8
%stackaddr$makeclosure58142 = alloca %struct.ScmObj*, align 8
%fptrToInt58143 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49682 to i64
%ae49682 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58143)
store volatile %struct.ScmObj* %ae49682, %struct.ScmObj** %stackaddr$makeclosure58142, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49682, %struct.ScmObj* %anf_45bind48215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49682, %struct.ScmObj* %k48423, i64 1)
%argslist56578$_37last480740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58144 = alloca %struct.ScmObj*, align 8
%argslist56578$_37last480741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48082, %struct.ScmObj* %argslist56578$_37last480740)
store volatile %struct.ScmObj* %argslist56578$_37last480741, %struct.ScmObj** %stackaddr$prim58144, align 8
%stackaddr$prim58145 = alloca %struct.ScmObj*, align 8
%argslist56578$_37last480742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49682, %struct.ScmObj* %argslist56578$_37last480741)
store volatile %struct.ScmObj* %argslist56578$_37last480742, %struct.ScmObj** %stackaddr$prim58145, align 8
%clofunc58146 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last48074)
musttail call tailcc void %clofunc58146(%struct.ScmObj* %_37last48074, %struct.ScmObj* %argslist56578$_37last480742)
ret void
}

define tailcc void @proc_clo$ae49682(%struct.ScmObj* %env$ae49682,%struct.ScmObj* %current_45args56574) {
%stackaddr$env-ref58147 = alloca %struct.ScmObj*, align 8
%anf_45bind48215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49682, i64 0)
store %struct.ScmObj* %anf_45bind48215, %struct.ScmObj** %stackaddr$env-ref58147
%stackaddr$env-ref58148 = alloca %struct.ScmObj*, align 8
%k48423 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49682, i64 1)
store %struct.ScmObj* %k48423, %struct.ScmObj** %stackaddr$env-ref58148
%stackaddr$prim58149 = alloca %struct.ScmObj*, align 8
%_95k48426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56574)
store volatile %struct.ScmObj* %_95k48426, %struct.ScmObj** %stackaddr$prim58149, align 8
%stackaddr$prim58150 = alloca %struct.ScmObj*, align 8
%current_45args56575 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56574)
store volatile %struct.ScmObj* %current_45args56575, %struct.ScmObj** %stackaddr$prim58150, align 8
%stackaddr$prim58151 = alloca %struct.ScmObj*, align 8
%anf_45bind48216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56575)
store volatile %struct.ScmObj* %anf_45bind48216, %struct.ScmObj** %stackaddr$prim58151, align 8
%stackaddr$prim58152 = alloca %struct.ScmObj*, align 8
%cpsprim48427 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48215, %struct.ScmObj* %anf_45bind48216)
store volatile %struct.ScmObj* %cpsprim48427, %struct.ScmObj** %stackaddr$prim58152, align 8
%ae49687 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56577$k484230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58153 = alloca %struct.ScmObj*, align 8
%argslist56577$k484231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48427, %struct.ScmObj* %argslist56577$k484230)
store volatile %struct.ScmObj* %argslist56577$k484231, %struct.ScmObj** %stackaddr$prim58153, align 8
%stackaddr$prim58154 = alloca %struct.ScmObj*, align 8
%argslist56577$k484232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49687, %struct.ScmObj* %argslist56577$k484231)
store volatile %struct.ScmObj* %argslist56577$k484232, %struct.ScmObj** %stackaddr$prim58154, align 8
%clofunc58155 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48423)
musttail call tailcc void %clofunc58155(%struct.ScmObj* %k48423, %struct.ScmObj* %argslist56577$k484232)
ret void
}

define tailcc void @proc_clo$ae49582(%struct.ScmObj* %env$ae49582,%struct.ScmObj* %current_45args56582) {
%stackaddr$env-ref58156 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49582, i64 0)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref58156
%stackaddr$prim58157 = alloca %struct.ScmObj*, align 8
%k48429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56582)
store volatile %struct.ScmObj* %k48429, %struct.ScmObj** %stackaddr$prim58157, align 8
%stackaddr$prim58158 = alloca %struct.ScmObj*, align 8
%current_45args56583 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56582)
store volatile %struct.ScmObj* %current_45args56583, %struct.ScmObj** %stackaddr$prim58158, align 8
%stackaddr$prim58159 = alloca %struct.ScmObj*, align 8
%f48085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56583)
store volatile %struct.ScmObj* %f48085, %struct.ScmObj** %stackaddr$prim58159, align 8
%stackaddr$prim58160 = alloca %struct.ScmObj*, align 8
%current_45args56584 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56583)
store volatile %struct.ScmObj* %current_45args56584, %struct.ScmObj** %stackaddr$prim58160, align 8
%stackaddr$prim58161 = alloca %struct.ScmObj*, align 8
%lst48084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56584)
store volatile %struct.ScmObj* %lst48084, %struct.ScmObj** %stackaddr$prim58161, align 8
%stackaddr$makeclosure58162 = alloca %struct.ScmObj*, align 8
%fptrToInt58163 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49583 to i64
%ae49583 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58163)
store volatile %struct.ScmObj* %ae49583, %struct.ScmObj** %stackaddr$makeclosure58162, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49583, %struct.ScmObj* %lst48084, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49583, %struct.ScmObj* %_37foldr148052, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49583, %struct.ScmObj* %k48429, i64 2)
%ae49584 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58164 = alloca %struct.ScmObj*, align 8
%fptrToInt58165 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49585 to i64
%ae49585 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58165)
store volatile %struct.ScmObj* %ae49585, %struct.ScmObj** %stackaddr$makeclosure58164, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49585, %struct.ScmObj* %f48085, i64 0)
%argslist56599$ae495830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58166 = alloca %struct.ScmObj*, align 8
%argslist56599$ae495831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49585, %struct.ScmObj* %argslist56599$ae495830)
store volatile %struct.ScmObj* %argslist56599$ae495831, %struct.ScmObj** %stackaddr$prim58166, align 8
%stackaddr$prim58167 = alloca %struct.ScmObj*, align 8
%argslist56599$ae495832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49584, %struct.ScmObj* %argslist56599$ae495831)
store volatile %struct.ScmObj* %argslist56599$ae495832, %struct.ScmObj** %stackaddr$prim58167, align 8
%clofunc58168 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49583)
musttail call tailcc void %clofunc58168(%struct.ScmObj* %ae49583, %struct.ScmObj* %argslist56599$ae495832)
ret void
}

define tailcc void @proc_clo$ae49583(%struct.ScmObj* %env$ae49583,%struct.ScmObj* %current_45args56586) {
%stackaddr$env-ref58169 = alloca %struct.ScmObj*, align 8
%lst48084 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49583, i64 0)
store %struct.ScmObj* %lst48084, %struct.ScmObj** %stackaddr$env-ref58169
%stackaddr$env-ref58170 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49583, i64 1)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref58170
%stackaddr$env-ref58171 = alloca %struct.ScmObj*, align 8
%k48429 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49583, i64 2)
store %struct.ScmObj* %k48429, %struct.ScmObj** %stackaddr$env-ref58171
%stackaddr$prim58172 = alloca %struct.ScmObj*, align 8
%_95k48430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56586)
store volatile %struct.ScmObj* %_95k48430, %struct.ScmObj** %stackaddr$prim58172, align 8
%stackaddr$prim58173 = alloca %struct.ScmObj*, align 8
%current_45args56587 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56586)
store volatile %struct.ScmObj* %current_45args56587, %struct.ScmObj** %stackaddr$prim58173, align 8
%stackaddr$prim58174 = alloca %struct.ScmObj*, align 8
%anf_45bind48213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56587)
store volatile %struct.ScmObj* %anf_45bind48213, %struct.ScmObj** %stackaddr$prim58174, align 8
%ae49617 = call %struct.ScmObj* @const_init_null()
%argslist56589$_37foldr1480520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58175 = alloca %struct.ScmObj*, align 8
%argslist56589$_37foldr1480521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48084, %struct.ScmObj* %argslist56589$_37foldr1480520)
store volatile %struct.ScmObj* %argslist56589$_37foldr1480521, %struct.ScmObj** %stackaddr$prim58175, align 8
%stackaddr$prim58176 = alloca %struct.ScmObj*, align 8
%argslist56589$_37foldr1480522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49617, %struct.ScmObj* %argslist56589$_37foldr1480521)
store volatile %struct.ScmObj* %argslist56589$_37foldr1480522, %struct.ScmObj** %stackaddr$prim58176, align 8
%stackaddr$prim58177 = alloca %struct.ScmObj*, align 8
%argslist56589$_37foldr1480523 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48213, %struct.ScmObj* %argslist56589$_37foldr1480522)
store volatile %struct.ScmObj* %argslist56589$_37foldr1480523, %struct.ScmObj** %stackaddr$prim58177, align 8
%stackaddr$prim58178 = alloca %struct.ScmObj*, align 8
%argslist56589$_37foldr1480524 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48429, %struct.ScmObj* %argslist56589$_37foldr1480523)
store volatile %struct.ScmObj* %argslist56589$_37foldr1480524, %struct.ScmObj** %stackaddr$prim58178, align 8
%clofunc58179 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148052)
musttail call tailcc void %clofunc58179(%struct.ScmObj* %_37foldr148052, %struct.ScmObj* %argslist56589$_37foldr1480524)
ret void
}

define tailcc void @proc_clo$ae49585(%struct.ScmObj* %env$ae49585,%struct.ScmObj* %current_45args56590) {
%stackaddr$env-ref58180 = alloca %struct.ScmObj*, align 8
%f48085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49585, i64 0)
store %struct.ScmObj* %f48085, %struct.ScmObj** %stackaddr$env-ref58180
%stackaddr$prim58181 = alloca %struct.ScmObj*, align 8
%k48431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56590)
store volatile %struct.ScmObj* %k48431, %struct.ScmObj** %stackaddr$prim58181, align 8
%stackaddr$prim58182 = alloca %struct.ScmObj*, align 8
%current_45args56591 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56590)
store volatile %struct.ScmObj* %current_45args56591, %struct.ScmObj** %stackaddr$prim58182, align 8
%stackaddr$prim58183 = alloca %struct.ScmObj*, align 8
%v48087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56591)
store volatile %struct.ScmObj* %v48087, %struct.ScmObj** %stackaddr$prim58183, align 8
%stackaddr$prim58184 = alloca %struct.ScmObj*, align 8
%current_45args56592 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56591)
store volatile %struct.ScmObj* %current_45args56592, %struct.ScmObj** %stackaddr$prim58184, align 8
%stackaddr$prim58185 = alloca %struct.ScmObj*, align 8
%r48086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56592)
store volatile %struct.ScmObj* %r48086, %struct.ScmObj** %stackaddr$prim58185, align 8
%stackaddr$makeclosure58186 = alloca %struct.ScmObj*, align 8
%fptrToInt58187 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49587 to i64
%ae49587 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58187)
store volatile %struct.ScmObj* %ae49587, %struct.ScmObj** %stackaddr$makeclosure58186, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49587, %struct.ScmObj* %r48086, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49587, %struct.ScmObj* %k48431, i64 1)
%argslist56598$f480850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58188 = alloca %struct.ScmObj*, align 8
%argslist56598$f480851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v48087, %struct.ScmObj* %argslist56598$f480850)
store volatile %struct.ScmObj* %argslist56598$f480851, %struct.ScmObj** %stackaddr$prim58188, align 8
%stackaddr$prim58189 = alloca %struct.ScmObj*, align 8
%argslist56598$f480852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49587, %struct.ScmObj* %argslist56598$f480851)
store volatile %struct.ScmObj* %argslist56598$f480852, %struct.ScmObj** %stackaddr$prim58189, align 8
%clofunc58190 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48085)
musttail call tailcc void %clofunc58190(%struct.ScmObj* %f48085, %struct.ScmObj* %argslist56598$f480852)
ret void
}

define tailcc void @proc_clo$ae49587(%struct.ScmObj* %env$ae49587,%struct.ScmObj* %current_45args56594) {
%stackaddr$env-ref58191 = alloca %struct.ScmObj*, align 8
%r48086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49587, i64 0)
store %struct.ScmObj* %r48086, %struct.ScmObj** %stackaddr$env-ref58191
%stackaddr$env-ref58192 = alloca %struct.ScmObj*, align 8
%k48431 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49587, i64 1)
store %struct.ScmObj* %k48431, %struct.ScmObj** %stackaddr$env-ref58192
%stackaddr$prim58193 = alloca %struct.ScmObj*, align 8
%_95k48432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56594)
store volatile %struct.ScmObj* %_95k48432, %struct.ScmObj** %stackaddr$prim58193, align 8
%stackaddr$prim58194 = alloca %struct.ScmObj*, align 8
%current_45args56595 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56594)
store volatile %struct.ScmObj* %current_45args56595, %struct.ScmObj** %stackaddr$prim58194, align 8
%stackaddr$prim58195 = alloca %struct.ScmObj*, align 8
%anf_45bind48212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56595)
store volatile %struct.ScmObj* %anf_45bind48212, %struct.ScmObj** %stackaddr$prim58195, align 8
%stackaddr$prim58196 = alloca %struct.ScmObj*, align 8
%cpsprim48433 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48212, %struct.ScmObj* %r48086)
store volatile %struct.ScmObj* %cpsprim48433, %struct.ScmObj** %stackaddr$prim58196, align 8
%ae49592 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56597$k484310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58197 = alloca %struct.ScmObj*, align 8
%argslist56597$k484311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48433, %struct.ScmObj* %argslist56597$k484310)
store volatile %struct.ScmObj* %argslist56597$k484311, %struct.ScmObj** %stackaddr$prim58197, align 8
%stackaddr$prim58198 = alloca %struct.ScmObj*, align 8
%argslist56597$k484312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49592, %struct.ScmObj* %argslist56597$k484311)
store volatile %struct.ScmObj* %argslist56597$k484312, %struct.ScmObj** %stackaddr$prim58198, align 8
%clofunc58199 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48431)
musttail call tailcc void %clofunc58199(%struct.ScmObj* %k48431, %struct.ScmObj* %argslist56597$k484312)
ret void
}

define tailcc void @proc_clo$ae49196(%struct.ScmObj* %env$ae49196,%struct.ScmObj* %current_45args56602) {
%stackaddr$env-ref58200 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49196, i64 0)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref58200
%stackaddr$env-ref58201 = alloca %struct.ScmObj*, align 8
%_37map148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49196, i64 1)
store %struct.ScmObj* %_37map148048, %struct.ScmObj** %stackaddr$env-ref58201
%stackaddr$prim58202 = alloca %struct.ScmObj*, align 8
%k48434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56602)
store volatile %struct.ScmObj* %k48434, %struct.ScmObj** %stackaddr$prim58202, align 8
%stackaddr$prim58203 = alloca %struct.ScmObj*, align 8
%current_45args56603 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56602)
store volatile %struct.ScmObj* %current_45args56603, %struct.ScmObj** %stackaddr$prim58203, align 8
%stackaddr$prim58204 = alloca %struct.ScmObj*, align 8
%_37foldr48058 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56603)
store volatile %struct.ScmObj* %_37foldr48058, %struct.ScmObj** %stackaddr$prim58204, align 8
%ae49198 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58205 = alloca %struct.ScmObj*, align 8
%fptrToInt58206 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49199 to i64
%ae49199 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58206)
store volatile %struct.ScmObj* %ae49199, %struct.ScmObj** %stackaddr$makeclosure58205, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49199, %struct.ScmObj* %_37foldr148052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49199, %struct.ScmObj* %_37map148048, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49199, %struct.ScmObj* %_37foldr48058, i64 2)
%argslist56660$k484340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58207 = alloca %struct.ScmObj*, align 8
%argslist56660$k484341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49199, %struct.ScmObj* %argslist56660$k484340)
store volatile %struct.ScmObj* %argslist56660$k484341, %struct.ScmObj** %stackaddr$prim58207, align 8
%stackaddr$prim58208 = alloca %struct.ScmObj*, align 8
%argslist56660$k484342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49198, %struct.ScmObj* %argslist56660$k484341)
store volatile %struct.ScmObj* %argslist56660$k484342, %struct.ScmObj** %stackaddr$prim58208, align 8
%clofunc58209 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48434)
musttail call tailcc void %clofunc58209(%struct.ScmObj* %k48434, %struct.ScmObj* %argslist56660$k484342)
ret void
}

define tailcc void @proc_clo$ae49199(%struct.ScmObj* %env$ae49199,%struct.ScmObj* %args4805948435) {
%stackaddr$env-ref58210 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49199, i64 0)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref58210
%stackaddr$env-ref58211 = alloca %struct.ScmObj*, align 8
%_37map148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49199, i64 1)
store %struct.ScmObj* %_37map148048, %struct.ScmObj** %stackaddr$env-ref58211
%stackaddr$env-ref58212 = alloca %struct.ScmObj*, align 8
%_37foldr48058 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49199, i64 2)
store %struct.ScmObj* %_37foldr48058, %struct.ScmObj** %stackaddr$env-ref58212
%stackaddr$prim58213 = alloca %struct.ScmObj*, align 8
%k48436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4805948435)
store volatile %struct.ScmObj* %k48436, %struct.ScmObj** %stackaddr$prim58213, align 8
%stackaddr$prim58214 = alloca %struct.ScmObj*, align 8
%args48059 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4805948435)
store volatile %struct.ScmObj* %args48059, %struct.ScmObj** %stackaddr$prim58214, align 8
%stackaddr$prim58215 = alloca %struct.ScmObj*, align 8
%f48062 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48059)
store volatile %struct.ScmObj* %f48062, %struct.ScmObj** %stackaddr$prim58215, align 8
%stackaddr$prim58216 = alloca %struct.ScmObj*, align 8
%anf_45bind48199 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48059)
store volatile %struct.ScmObj* %anf_45bind48199, %struct.ScmObj** %stackaddr$prim58216, align 8
%stackaddr$prim58217 = alloca %struct.ScmObj*, align 8
%acc48061 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48199)
store volatile %struct.ScmObj* %acc48061, %struct.ScmObj** %stackaddr$prim58217, align 8
%stackaddr$prim58218 = alloca %struct.ScmObj*, align 8
%anf_45bind48200 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48059)
store volatile %struct.ScmObj* %anf_45bind48200, %struct.ScmObj** %stackaddr$prim58218, align 8
%stackaddr$prim58219 = alloca %struct.ScmObj*, align 8
%lsts48060 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48200)
store volatile %struct.ScmObj* %lsts48060, %struct.ScmObj** %stackaddr$prim58219, align 8
%stackaddr$makeclosure58220 = alloca %struct.ScmObj*, align 8
%fptrToInt58221 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49207 to i64
%ae49207 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58221)
store volatile %struct.ScmObj* %ae49207, %struct.ScmObj** %stackaddr$makeclosure58220, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49207, %struct.ScmObj* %_37foldr148052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49207, %struct.ScmObj* %k48436, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49207, %struct.ScmObj* %_37map148048, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49207, %struct.ScmObj* %f48062, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49207, %struct.ScmObj* %acc48061, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49207, %struct.ScmObj* %lsts48060, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49207, %struct.ScmObj* %_37foldr48058, i64 6)
%ae49208 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58222 = alloca %struct.ScmObj*, align 8
%fptrToInt58223 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49209 to i64
%ae49209 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58223)
store volatile %struct.ScmObj* %ae49209, %struct.ScmObj** %stackaddr$makeclosure58222, align 8
%argslist56659$ae492070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58224 = alloca %struct.ScmObj*, align 8
%argslist56659$ae492071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49209, %struct.ScmObj* %argslist56659$ae492070)
store volatile %struct.ScmObj* %argslist56659$ae492071, %struct.ScmObj** %stackaddr$prim58224, align 8
%stackaddr$prim58225 = alloca %struct.ScmObj*, align 8
%argslist56659$ae492072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49208, %struct.ScmObj* %argslist56659$ae492071)
store volatile %struct.ScmObj* %argslist56659$ae492072, %struct.ScmObj** %stackaddr$prim58225, align 8
%clofunc58226 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49207)
musttail call tailcc void %clofunc58226(%struct.ScmObj* %ae49207, %struct.ScmObj* %argslist56659$ae492072)
ret void
}

define tailcc void @proc_clo$ae49207(%struct.ScmObj* %env$ae49207,%struct.ScmObj* %current_45args56605) {
%stackaddr$env-ref58227 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49207, i64 0)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref58227
%stackaddr$env-ref58228 = alloca %struct.ScmObj*, align 8
%k48436 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49207, i64 1)
store %struct.ScmObj* %k48436, %struct.ScmObj** %stackaddr$env-ref58228
%stackaddr$env-ref58229 = alloca %struct.ScmObj*, align 8
%_37map148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49207, i64 2)
store %struct.ScmObj* %_37map148048, %struct.ScmObj** %stackaddr$env-ref58229
%stackaddr$env-ref58230 = alloca %struct.ScmObj*, align 8
%f48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49207, i64 3)
store %struct.ScmObj* %f48062, %struct.ScmObj** %stackaddr$env-ref58230
%stackaddr$env-ref58231 = alloca %struct.ScmObj*, align 8
%acc48061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49207, i64 4)
store %struct.ScmObj* %acc48061, %struct.ScmObj** %stackaddr$env-ref58231
%stackaddr$env-ref58232 = alloca %struct.ScmObj*, align 8
%lsts48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49207, i64 5)
store %struct.ScmObj* %lsts48060, %struct.ScmObj** %stackaddr$env-ref58232
%stackaddr$env-ref58233 = alloca %struct.ScmObj*, align 8
%_37foldr48058 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49207, i64 6)
store %struct.ScmObj* %_37foldr48058, %struct.ScmObj** %stackaddr$env-ref58233
%stackaddr$prim58234 = alloca %struct.ScmObj*, align 8
%_95k48437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56605)
store volatile %struct.ScmObj* %_95k48437, %struct.ScmObj** %stackaddr$prim58234, align 8
%stackaddr$prim58235 = alloca %struct.ScmObj*, align 8
%current_45args56606 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56605)
store volatile %struct.ScmObj* %current_45args56606, %struct.ScmObj** %stackaddr$prim58235, align 8
%stackaddr$prim58236 = alloca %struct.ScmObj*, align 8
%anf_45bind48201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56606)
store volatile %struct.ScmObj* %anf_45bind48201, %struct.ScmObj** %stackaddr$prim58236, align 8
%stackaddr$makeclosure58237 = alloca %struct.ScmObj*, align 8
%fptrToInt58238 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49239 to i64
%ae49239 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58238)
store volatile %struct.ScmObj* %ae49239, %struct.ScmObj** %stackaddr$makeclosure58237, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49239, %struct.ScmObj* %_37foldr148052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49239, %struct.ScmObj* %k48436, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49239, %struct.ScmObj* %_37map148048, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49239, %struct.ScmObj* %f48062, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49239, %struct.ScmObj* %acc48061, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49239, %struct.ScmObj* %lsts48060, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49239, %struct.ScmObj* %_37foldr48058, i64 6)
%ae49241 = call %struct.ScmObj* @const_init_false()
%argslist56652$_37foldr1480520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58239 = alloca %struct.ScmObj*, align 8
%argslist56652$_37foldr1480521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48060, %struct.ScmObj* %argslist56652$_37foldr1480520)
store volatile %struct.ScmObj* %argslist56652$_37foldr1480521, %struct.ScmObj** %stackaddr$prim58239, align 8
%stackaddr$prim58240 = alloca %struct.ScmObj*, align 8
%argslist56652$_37foldr1480522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49241, %struct.ScmObj* %argslist56652$_37foldr1480521)
store volatile %struct.ScmObj* %argslist56652$_37foldr1480522, %struct.ScmObj** %stackaddr$prim58240, align 8
%stackaddr$prim58241 = alloca %struct.ScmObj*, align 8
%argslist56652$_37foldr1480523 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48201, %struct.ScmObj* %argslist56652$_37foldr1480522)
store volatile %struct.ScmObj* %argslist56652$_37foldr1480523, %struct.ScmObj** %stackaddr$prim58241, align 8
%stackaddr$prim58242 = alloca %struct.ScmObj*, align 8
%argslist56652$_37foldr1480524 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49239, %struct.ScmObj* %argslist56652$_37foldr1480523)
store volatile %struct.ScmObj* %argslist56652$_37foldr1480524, %struct.ScmObj** %stackaddr$prim58242, align 8
%clofunc58243 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148052)
musttail call tailcc void %clofunc58243(%struct.ScmObj* %_37foldr148052, %struct.ScmObj* %argslist56652$_37foldr1480524)
ret void
}

define tailcc void @proc_clo$ae49239(%struct.ScmObj* %env$ae49239,%struct.ScmObj* %current_45args56608) {
%stackaddr$env-ref58244 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49239, i64 0)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref58244
%stackaddr$env-ref58245 = alloca %struct.ScmObj*, align 8
%k48436 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49239, i64 1)
store %struct.ScmObj* %k48436, %struct.ScmObj** %stackaddr$env-ref58245
%stackaddr$env-ref58246 = alloca %struct.ScmObj*, align 8
%_37map148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49239, i64 2)
store %struct.ScmObj* %_37map148048, %struct.ScmObj** %stackaddr$env-ref58246
%stackaddr$env-ref58247 = alloca %struct.ScmObj*, align 8
%f48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49239, i64 3)
store %struct.ScmObj* %f48062, %struct.ScmObj** %stackaddr$env-ref58247
%stackaddr$env-ref58248 = alloca %struct.ScmObj*, align 8
%acc48061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49239, i64 4)
store %struct.ScmObj* %acc48061, %struct.ScmObj** %stackaddr$env-ref58248
%stackaddr$env-ref58249 = alloca %struct.ScmObj*, align 8
%lsts48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49239, i64 5)
store %struct.ScmObj* %lsts48060, %struct.ScmObj** %stackaddr$env-ref58249
%stackaddr$env-ref58250 = alloca %struct.ScmObj*, align 8
%_37foldr48058 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49239, i64 6)
store %struct.ScmObj* %_37foldr48058, %struct.ScmObj** %stackaddr$env-ref58250
%stackaddr$prim58251 = alloca %struct.ScmObj*, align 8
%_95k48438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56608)
store volatile %struct.ScmObj* %_95k48438, %struct.ScmObj** %stackaddr$prim58251, align 8
%stackaddr$prim58252 = alloca %struct.ScmObj*, align 8
%current_45args56609 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56608)
store volatile %struct.ScmObj* %current_45args56609, %struct.ScmObj** %stackaddr$prim58252, align 8
%stackaddr$prim58253 = alloca %struct.ScmObj*, align 8
%anf_45bind48202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56609)
store volatile %struct.ScmObj* %anf_45bind48202, %struct.ScmObj** %stackaddr$prim58253, align 8
%truthy$cmp58254 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48202)
%cmp$cmp58254 = icmp eq i64 %truthy$cmp58254, 1
br i1 %cmp$cmp58254, label %truebranch$cmp58254, label %falsebranch$cmp58254
truebranch$cmp58254:
%ae49250 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56611$k484360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58255 = alloca %struct.ScmObj*, align 8
%argslist56611$k484361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48061, %struct.ScmObj* %argslist56611$k484360)
store volatile %struct.ScmObj* %argslist56611$k484361, %struct.ScmObj** %stackaddr$prim58255, align 8
%stackaddr$prim58256 = alloca %struct.ScmObj*, align 8
%argslist56611$k484362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49250, %struct.ScmObj* %argslist56611$k484361)
store volatile %struct.ScmObj* %argslist56611$k484362, %struct.ScmObj** %stackaddr$prim58256, align 8
%clofunc58257 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48436)
musttail call tailcc void %clofunc58257(%struct.ScmObj* %k48436, %struct.ScmObj* %argslist56611$k484362)
ret void
falsebranch$cmp58254:
%stackaddr$makeclosure58258 = alloca %struct.ScmObj*, align 8
%fptrToInt58259 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49255 to i64
%ae49255 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58259)
store volatile %struct.ScmObj* %ae49255, %struct.ScmObj** %stackaddr$makeclosure58258, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49255, %struct.ScmObj* %_37foldr148052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49255, %struct.ScmObj* %k48436, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49255, %struct.ScmObj* %_37map148048, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49255, %struct.ScmObj* %f48062, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49255, %struct.ScmObj* %acc48061, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49255, %struct.ScmObj* %lsts48060, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49255, %struct.ScmObj* %_37foldr48058, i64 6)
%ae49256 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58260 = alloca %struct.ScmObj*, align 8
%fptrToInt58261 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49257 to i64
%ae49257 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58261)
store volatile %struct.ScmObj* %ae49257, %struct.ScmObj** %stackaddr$makeclosure58260, align 8
%argslist56651$ae492550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58262 = alloca %struct.ScmObj*, align 8
%argslist56651$ae492551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49257, %struct.ScmObj* %argslist56651$ae492550)
store volatile %struct.ScmObj* %argslist56651$ae492551, %struct.ScmObj** %stackaddr$prim58262, align 8
%stackaddr$prim58263 = alloca %struct.ScmObj*, align 8
%argslist56651$ae492552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49256, %struct.ScmObj* %argslist56651$ae492551)
store volatile %struct.ScmObj* %argslist56651$ae492552, %struct.ScmObj** %stackaddr$prim58263, align 8
%clofunc58264 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49255)
musttail call tailcc void %clofunc58264(%struct.ScmObj* %ae49255, %struct.ScmObj* %argslist56651$ae492552)
ret void
}

define tailcc void @proc_clo$ae49255(%struct.ScmObj* %env$ae49255,%struct.ScmObj* %current_45args56612) {
%stackaddr$env-ref58265 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49255, i64 0)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref58265
%stackaddr$env-ref58266 = alloca %struct.ScmObj*, align 8
%k48436 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49255, i64 1)
store %struct.ScmObj* %k48436, %struct.ScmObj** %stackaddr$env-ref58266
%stackaddr$env-ref58267 = alloca %struct.ScmObj*, align 8
%_37map148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49255, i64 2)
store %struct.ScmObj* %_37map148048, %struct.ScmObj** %stackaddr$env-ref58267
%stackaddr$env-ref58268 = alloca %struct.ScmObj*, align 8
%f48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49255, i64 3)
store %struct.ScmObj* %f48062, %struct.ScmObj** %stackaddr$env-ref58268
%stackaddr$env-ref58269 = alloca %struct.ScmObj*, align 8
%acc48061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49255, i64 4)
store %struct.ScmObj* %acc48061, %struct.ScmObj** %stackaddr$env-ref58269
%stackaddr$env-ref58270 = alloca %struct.ScmObj*, align 8
%lsts48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49255, i64 5)
store %struct.ScmObj* %lsts48060, %struct.ScmObj** %stackaddr$env-ref58270
%stackaddr$env-ref58271 = alloca %struct.ScmObj*, align 8
%_37foldr48058 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49255, i64 6)
store %struct.ScmObj* %_37foldr48058, %struct.ScmObj** %stackaddr$env-ref58271
%stackaddr$prim58272 = alloca %struct.ScmObj*, align 8
%_95k48439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56612)
store volatile %struct.ScmObj* %_95k48439, %struct.ScmObj** %stackaddr$prim58272, align 8
%stackaddr$prim58273 = alloca %struct.ScmObj*, align 8
%current_45args56613 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56612)
store volatile %struct.ScmObj* %current_45args56613, %struct.ScmObj** %stackaddr$prim58273, align 8
%stackaddr$prim58274 = alloca %struct.ScmObj*, align 8
%anf_45bind48203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56613)
store volatile %struct.ScmObj* %anf_45bind48203, %struct.ScmObj** %stackaddr$prim58274, align 8
%stackaddr$makeclosure58275 = alloca %struct.ScmObj*, align 8
%fptrToInt58276 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49276 to i64
%ae49276 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58276)
store volatile %struct.ScmObj* %ae49276, %struct.ScmObj** %stackaddr$makeclosure58275, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %_37foldr148052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %k48436, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %_37map148048, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %f48062, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %acc48061, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %lsts48060, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49276, %struct.ScmObj* %_37foldr48058, i64 6)
%argslist56646$_37map1480480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58277 = alloca %struct.ScmObj*, align 8
%argslist56646$_37map1480481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48060, %struct.ScmObj* %argslist56646$_37map1480480)
store volatile %struct.ScmObj* %argslist56646$_37map1480481, %struct.ScmObj** %stackaddr$prim58277, align 8
%stackaddr$prim58278 = alloca %struct.ScmObj*, align 8
%argslist56646$_37map1480482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48203, %struct.ScmObj* %argslist56646$_37map1480481)
store volatile %struct.ScmObj* %argslist56646$_37map1480482, %struct.ScmObj** %stackaddr$prim58278, align 8
%stackaddr$prim58279 = alloca %struct.ScmObj*, align 8
%argslist56646$_37map1480483 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49276, %struct.ScmObj* %argslist56646$_37map1480482)
store volatile %struct.ScmObj* %argslist56646$_37map1480483, %struct.ScmObj** %stackaddr$prim58279, align 8
%clofunc58280 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148048)
musttail call tailcc void %clofunc58280(%struct.ScmObj* %_37map148048, %struct.ScmObj* %argslist56646$_37map1480483)
ret void
}

define tailcc void @proc_clo$ae49276(%struct.ScmObj* %env$ae49276,%struct.ScmObj* %current_45args56615) {
%stackaddr$env-ref58281 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 0)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref58281
%stackaddr$env-ref58282 = alloca %struct.ScmObj*, align 8
%k48436 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 1)
store %struct.ScmObj* %k48436, %struct.ScmObj** %stackaddr$env-ref58282
%stackaddr$env-ref58283 = alloca %struct.ScmObj*, align 8
%_37map148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 2)
store %struct.ScmObj* %_37map148048, %struct.ScmObj** %stackaddr$env-ref58283
%stackaddr$env-ref58284 = alloca %struct.ScmObj*, align 8
%f48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 3)
store %struct.ScmObj* %f48062, %struct.ScmObj** %stackaddr$env-ref58284
%stackaddr$env-ref58285 = alloca %struct.ScmObj*, align 8
%acc48061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 4)
store %struct.ScmObj* %acc48061, %struct.ScmObj** %stackaddr$env-ref58285
%stackaddr$env-ref58286 = alloca %struct.ScmObj*, align 8
%lsts48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 5)
store %struct.ScmObj* %lsts48060, %struct.ScmObj** %stackaddr$env-ref58286
%stackaddr$env-ref58287 = alloca %struct.ScmObj*, align 8
%_37foldr48058 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49276, i64 6)
store %struct.ScmObj* %_37foldr48058, %struct.ScmObj** %stackaddr$env-ref58287
%stackaddr$prim58288 = alloca %struct.ScmObj*, align 8
%_95k48440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56615)
store volatile %struct.ScmObj* %_95k48440, %struct.ScmObj** %stackaddr$prim58288, align 8
%stackaddr$prim58289 = alloca %struct.ScmObj*, align 8
%current_45args56616 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56615)
store volatile %struct.ScmObj* %current_45args56616, %struct.ScmObj** %stackaddr$prim58289, align 8
%stackaddr$prim58290 = alloca %struct.ScmObj*, align 8
%lsts_4348067 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56616)
store volatile %struct.ScmObj* %lsts_4348067, %struct.ScmObj** %stackaddr$prim58290, align 8
%stackaddr$makeclosure58291 = alloca %struct.ScmObj*, align 8
%fptrToInt58292 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49279 to i64
%ae49279 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt58292)
store volatile %struct.ScmObj* %ae49279, %struct.ScmObj** %stackaddr$makeclosure58291, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49279, %struct.ScmObj* %_37foldr148052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49279, %struct.ScmObj* %k48436, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49279, %struct.ScmObj* %lsts_4348067, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49279, %struct.ScmObj* %_37map148048, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49279, %struct.ScmObj* %f48062, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49279, %struct.ScmObj* %acc48061, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49279, %struct.ScmObj* %lsts48060, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49279, %struct.ScmObj* %_37foldr48058, i64 7)
%ae49280 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58293 = alloca %struct.ScmObj*, align 8
%fptrToInt58294 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49281 to i64
%ae49281 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58294)
store volatile %struct.ScmObj* %ae49281, %struct.ScmObj** %stackaddr$makeclosure58293, align 8
%argslist56645$ae492790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58295 = alloca %struct.ScmObj*, align 8
%argslist56645$ae492791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49281, %struct.ScmObj* %argslist56645$ae492790)
store volatile %struct.ScmObj* %argslist56645$ae492791, %struct.ScmObj** %stackaddr$prim58295, align 8
%stackaddr$prim58296 = alloca %struct.ScmObj*, align 8
%argslist56645$ae492792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49280, %struct.ScmObj* %argslist56645$ae492791)
store volatile %struct.ScmObj* %argslist56645$ae492792, %struct.ScmObj** %stackaddr$prim58296, align 8
%clofunc58297 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49279)
musttail call tailcc void %clofunc58297(%struct.ScmObj* %ae49279, %struct.ScmObj* %argslist56645$ae492792)
ret void
}

define tailcc void @proc_clo$ae49279(%struct.ScmObj* %env$ae49279,%struct.ScmObj* %current_45args56618) {
%stackaddr$env-ref58298 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49279, i64 0)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref58298
%stackaddr$env-ref58299 = alloca %struct.ScmObj*, align 8
%k48436 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49279, i64 1)
store %struct.ScmObj* %k48436, %struct.ScmObj** %stackaddr$env-ref58299
%stackaddr$env-ref58300 = alloca %struct.ScmObj*, align 8
%lsts_4348067 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49279, i64 2)
store %struct.ScmObj* %lsts_4348067, %struct.ScmObj** %stackaddr$env-ref58300
%stackaddr$env-ref58301 = alloca %struct.ScmObj*, align 8
%_37map148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49279, i64 3)
store %struct.ScmObj* %_37map148048, %struct.ScmObj** %stackaddr$env-ref58301
%stackaddr$env-ref58302 = alloca %struct.ScmObj*, align 8
%f48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49279, i64 4)
store %struct.ScmObj* %f48062, %struct.ScmObj** %stackaddr$env-ref58302
%stackaddr$env-ref58303 = alloca %struct.ScmObj*, align 8
%acc48061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49279, i64 5)
store %struct.ScmObj* %acc48061, %struct.ScmObj** %stackaddr$env-ref58303
%stackaddr$env-ref58304 = alloca %struct.ScmObj*, align 8
%lsts48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49279, i64 6)
store %struct.ScmObj* %lsts48060, %struct.ScmObj** %stackaddr$env-ref58304
%stackaddr$env-ref58305 = alloca %struct.ScmObj*, align 8
%_37foldr48058 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49279, i64 7)
store %struct.ScmObj* %_37foldr48058, %struct.ScmObj** %stackaddr$env-ref58305
%stackaddr$prim58306 = alloca %struct.ScmObj*, align 8
%_95k48441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56618)
store volatile %struct.ScmObj* %_95k48441, %struct.ScmObj** %stackaddr$prim58306, align 8
%stackaddr$prim58307 = alloca %struct.ScmObj*, align 8
%current_45args56619 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56618)
store volatile %struct.ScmObj* %current_45args56619, %struct.ScmObj** %stackaddr$prim58307, align 8
%stackaddr$prim58308 = alloca %struct.ScmObj*, align 8
%anf_45bind48204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56619)
store volatile %struct.ScmObj* %anf_45bind48204, %struct.ScmObj** %stackaddr$prim58308, align 8
%stackaddr$makeclosure58309 = alloca %struct.ScmObj*, align 8
%fptrToInt58310 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49300 to i64
%ae49300 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt58310)
store volatile %struct.ScmObj* %ae49300, %struct.ScmObj** %stackaddr$makeclosure58309, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49300, %struct.ScmObj* %_37foldr148052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49300, %struct.ScmObj* %k48436, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49300, %struct.ScmObj* %lsts_4348067, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49300, %struct.ScmObj* %f48062, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49300, %struct.ScmObj* %acc48061, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49300, %struct.ScmObj* %_37foldr48058, i64 5)
%argslist56640$_37map1480480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58311 = alloca %struct.ScmObj*, align 8
%argslist56640$_37map1480481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48060, %struct.ScmObj* %argslist56640$_37map1480480)
store volatile %struct.ScmObj* %argslist56640$_37map1480481, %struct.ScmObj** %stackaddr$prim58311, align 8
%stackaddr$prim58312 = alloca %struct.ScmObj*, align 8
%argslist56640$_37map1480482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48204, %struct.ScmObj* %argslist56640$_37map1480481)
store volatile %struct.ScmObj* %argslist56640$_37map1480482, %struct.ScmObj** %stackaddr$prim58312, align 8
%stackaddr$prim58313 = alloca %struct.ScmObj*, align 8
%argslist56640$_37map1480483 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49300, %struct.ScmObj* %argslist56640$_37map1480482)
store volatile %struct.ScmObj* %argslist56640$_37map1480483, %struct.ScmObj** %stackaddr$prim58313, align 8
%clofunc58314 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148048)
musttail call tailcc void %clofunc58314(%struct.ScmObj* %_37map148048, %struct.ScmObj* %argslist56640$_37map1480483)
ret void
}

define tailcc void @proc_clo$ae49300(%struct.ScmObj* %env$ae49300,%struct.ScmObj* %current_45args56621) {
%stackaddr$env-ref58315 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49300, i64 0)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref58315
%stackaddr$env-ref58316 = alloca %struct.ScmObj*, align 8
%k48436 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49300, i64 1)
store %struct.ScmObj* %k48436, %struct.ScmObj** %stackaddr$env-ref58316
%stackaddr$env-ref58317 = alloca %struct.ScmObj*, align 8
%lsts_4348067 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49300, i64 2)
store %struct.ScmObj* %lsts_4348067, %struct.ScmObj** %stackaddr$env-ref58317
%stackaddr$env-ref58318 = alloca %struct.ScmObj*, align 8
%f48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49300, i64 3)
store %struct.ScmObj* %f48062, %struct.ScmObj** %stackaddr$env-ref58318
%stackaddr$env-ref58319 = alloca %struct.ScmObj*, align 8
%acc48061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49300, i64 4)
store %struct.ScmObj* %acc48061, %struct.ScmObj** %stackaddr$env-ref58319
%stackaddr$env-ref58320 = alloca %struct.ScmObj*, align 8
%_37foldr48058 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49300, i64 5)
store %struct.ScmObj* %_37foldr48058, %struct.ScmObj** %stackaddr$env-ref58320
%stackaddr$prim58321 = alloca %struct.ScmObj*, align 8
%_95k48442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56621)
store volatile %struct.ScmObj* %_95k48442, %struct.ScmObj** %stackaddr$prim58321, align 8
%stackaddr$prim58322 = alloca %struct.ScmObj*, align 8
%current_45args56622 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56621)
store volatile %struct.ScmObj* %current_45args56622, %struct.ScmObj** %stackaddr$prim58322, align 8
%stackaddr$prim58323 = alloca %struct.ScmObj*, align 8
%vs48065 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56622)
store volatile %struct.ScmObj* %vs48065, %struct.ScmObj** %stackaddr$prim58323, align 8
%stackaddr$makeclosure58324 = alloca %struct.ScmObj*, align 8
%fptrToInt58325 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49303 to i64
%ae49303 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58325)
store volatile %struct.ScmObj* %ae49303, %struct.ScmObj** %stackaddr$makeclosure58324, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49303, %struct.ScmObj* %_37foldr148052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49303, %struct.ScmObj* %k48436, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49303, %struct.ScmObj* %lsts_4348067, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49303, %struct.ScmObj* %vs48065, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49303, %struct.ScmObj* %f48062, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49303, %struct.ScmObj* %acc48061, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49303, %struct.ScmObj* %_37foldr48058, i64 6)
%ae49304 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58326 = alloca %struct.ScmObj*, align 8
%fptrToInt58327 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49305 to i64
%ae49305 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58327)
store volatile %struct.ScmObj* %ae49305, %struct.ScmObj** %stackaddr$makeclosure58326, align 8
%argslist56639$ae493030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58328 = alloca %struct.ScmObj*, align 8
%argslist56639$ae493031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49305, %struct.ScmObj* %argslist56639$ae493030)
store volatile %struct.ScmObj* %argslist56639$ae493031, %struct.ScmObj** %stackaddr$prim58328, align 8
%stackaddr$prim58329 = alloca %struct.ScmObj*, align 8
%argslist56639$ae493032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49304, %struct.ScmObj* %argslist56639$ae493031)
store volatile %struct.ScmObj* %argslist56639$ae493032, %struct.ScmObj** %stackaddr$prim58329, align 8
%clofunc58330 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49303)
musttail call tailcc void %clofunc58330(%struct.ScmObj* %ae49303, %struct.ScmObj* %argslist56639$ae493032)
ret void
}

define tailcc void @proc_clo$ae49303(%struct.ScmObj* %env$ae49303,%struct.ScmObj* %current_45args56624) {
%stackaddr$env-ref58331 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49303, i64 0)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref58331
%stackaddr$env-ref58332 = alloca %struct.ScmObj*, align 8
%k48436 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49303, i64 1)
store %struct.ScmObj* %k48436, %struct.ScmObj** %stackaddr$env-ref58332
%stackaddr$env-ref58333 = alloca %struct.ScmObj*, align 8
%lsts_4348067 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49303, i64 2)
store %struct.ScmObj* %lsts_4348067, %struct.ScmObj** %stackaddr$env-ref58333
%stackaddr$env-ref58334 = alloca %struct.ScmObj*, align 8
%vs48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49303, i64 3)
store %struct.ScmObj* %vs48065, %struct.ScmObj** %stackaddr$env-ref58334
%stackaddr$env-ref58335 = alloca %struct.ScmObj*, align 8
%f48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49303, i64 4)
store %struct.ScmObj* %f48062, %struct.ScmObj** %stackaddr$env-ref58335
%stackaddr$env-ref58336 = alloca %struct.ScmObj*, align 8
%acc48061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49303, i64 5)
store %struct.ScmObj* %acc48061, %struct.ScmObj** %stackaddr$env-ref58336
%stackaddr$env-ref58337 = alloca %struct.ScmObj*, align 8
%_37foldr48058 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49303, i64 6)
store %struct.ScmObj* %_37foldr48058, %struct.ScmObj** %stackaddr$env-ref58337
%stackaddr$prim58338 = alloca %struct.ScmObj*, align 8
%_95k48443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56624)
store volatile %struct.ScmObj* %_95k48443, %struct.ScmObj** %stackaddr$prim58338, align 8
%stackaddr$prim58339 = alloca %struct.ScmObj*, align 8
%current_45args56625 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56624)
store volatile %struct.ScmObj* %current_45args56625, %struct.ScmObj** %stackaddr$prim58339, align 8
%stackaddr$prim58340 = alloca %struct.ScmObj*, align 8
%anf_45bind48205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56625)
store volatile %struct.ScmObj* %anf_45bind48205, %struct.ScmObj** %stackaddr$prim58340, align 8
%stackaddr$prim58341 = alloca %struct.ScmObj*, align 8
%anf_45bind48206 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48061, %struct.ScmObj* %lsts_4348067)
store volatile %struct.ScmObj* %anf_45bind48206, %struct.ScmObj** %stackaddr$prim58341, align 8
%stackaddr$prim58342 = alloca %struct.ScmObj*, align 8
%anf_45bind48207 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48062, %struct.ScmObj* %anf_45bind48206)
store volatile %struct.ScmObj* %anf_45bind48207, %struct.ScmObj** %stackaddr$prim58342, align 8
%stackaddr$makeclosure58343 = alloca %struct.ScmObj*, align 8
%fptrToInt58344 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49329 to i64
%ae49329 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt58344)
store volatile %struct.ScmObj* %ae49329, %struct.ScmObj** %stackaddr$makeclosure58343, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49329, %struct.ScmObj* %_37foldr148052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49329, %struct.ScmObj* %k48436, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49329, %struct.ScmObj* %vs48065, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49329, %struct.ScmObj* %f48062, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49329, %struct.ScmObj* %anf_45bind48205, i64 4)
%stackaddr$prim58345 = alloca %struct.ScmObj*, align 8
%cpsargs48447 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49329, %struct.ScmObj* %anf_45bind48207)
store volatile %struct.ScmObj* %cpsargs48447, %struct.ScmObj** %stackaddr$prim58345, align 8
%clofunc58346 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48058)
musttail call tailcc void %clofunc58346(%struct.ScmObj* %_37foldr48058, %struct.ScmObj* %cpsargs48447)
ret void
}

define tailcc void @proc_clo$ae49329(%struct.ScmObj* %env$ae49329,%struct.ScmObj* %current_45args56627) {
%stackaddr$env-ref58347 = alloca %struct.ScmObj*, align 8
%_37foldr148052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49329, i64 0)
store %struct.ScmObj* %_37foldr148052, %struct.ScmObj** %stackaddr$env-ref58347
%stackaddr$env-ref58348 = alloca %struct.ScmObj*, align 8
%k48436 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49329, i64 1)
store %struct.ScmObj* %k48436, %struct.ScmObj** %stackaddr$env-ref58348
%stackaddr$env-ref58349 = alloca %struct.ScmObj*, align 8
%vs48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49329, i64 2)
store %struct.ScmObj* %vs48065, %struct.ScmObj** %stackaddr$env-ref58349
%stackaddr$env-ref58350 = alloca %struct.ScmObj*, align 8
%f48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49329, i64 3)
store %struct.ScmObj* %f48062, %struct.ScmObj** %stackaddr$env-ref58350
%stackaddr$env-ref58351 = alloca %struct.ScmObj*, align 8
%anf_45bind48205 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49329, i64 4)
store %struct.ScmObj* %anf_45bind48205, %struct.ScmObj** %stackaddr$env-ref58351
%stackaddr$prim58352 = alloca %struct.ScmObj*, align 8
%_95k48444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56627)
store volatile %struct.ScmObj* %_95k48444, %struct.ScmObj** %stackaddr$prim58352, align 8
%stackaddr$prim58353 = alloca %struct.ScmObj*, align 8
%current_45args56628 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56627)
store volatile %struct.ScmObj* %current_45args56628, %struct.ScmObj** %stackaddr$prim58353, align 8
%stackaddr$prim58354 = alloca %struct.ScmObj*, align 8
%anf_45bind48208 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56628)
store volatile %struct.ScmObj* %anf_45bind48208, %struct.ScmObj** %stackaddr$prim58354, align 8
%ae49334 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58355 = alloca %struct.ScmObj*, align 8
%anf_45bind48209 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48208, %struct.ScmObj* %ae49334)
store volatile %struct.ScmObj* %anf_45bind48209, %struct.ScmObj** %stackaddr$prim58355, align 8
%stackaddr$makeclosure58356 = alloca %struct.ScmObj*, align 8
%fptrToInt58357 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49336 to i64
%ae49336 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58357)
store volatile %struct.ScmObj* %ae49336, %struct.ScmObj** %stackaddr$makeclosure58356, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49336, %struct.ScmObj* %k48436, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49336, %struct.ScmObj* %f48062, i64 1)
%argslist56633$_37foldr1480520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58358 = alloca %struct.ScmObj*, align 8
%argslist56633$_37foldr1480521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48065, %struct.ScmObj* %argslist56633$_37foldr1480520)
store volatile %struct.ScmObj* %argslist56633$_37foldr1480521, %struct.ScmObj** %stackaddr$prim58358, align 8
%stackaddr$prim58359 = alloca %struct.ScmObj*, align 8
%argslist56633$_37foldr1480522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48209, %struct.ScmObj* %argslist56633$_37foldr1480521)
store volatile %struct.ScmObj* %argslist56633$_37foldr1480522, %struct.ScmObj** %stackaddr$prim58359, align 8
%stackaddr$prim58360 = alloca %struct.ScmObj*, align 8
%argslist56633$_37foldr1480523 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48205, %struct.ScmObj* %argslist56633$_37foldr1480522)
store volatile %struct.ScmObj* %argslist56633$_37foldr1480523, %struct.ScmObj** %stackaddr$prim58360, align 8
%stackaddr$prim58361 = alloca %struct.ScmObj*, align 8
%argslist56633$_37foldr1480524 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49336, %struct.ScmObj* %argslist56633$_37foldr1480523)
store volatile %struct.ScmObj* %argslist56633$_37foldr1480524, %struct.ScmObj** %stackaddr$prim58361, align 8
%clofunc58362 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148052)
musttail call tailcc void %clofunc58362(%struct.ScmObj* %_37foldr148052, %struct.ScmObj* %argslist56633$_37foldr1480524)
ret void
}

define tailcc void @proc_clo$ae49336(%struct.ScmObj* %env$ae49336,%struct.ScmObj* %current_45args56630) {
%stackaddr$env-ref58363 = alloca %struct.ScmObj*, align 8
%k48436 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49336, i64 0)
store %struct.ScmObj* %k48436, %struct.ScmObj** %stackaddr$env-ref58363
%stackaddr$env-ref58364 = alloca %struct.ScmObj*, align 8
%f48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49336, i64 1)
store %struct.ScmObj* %f48062, %struct.ScmObj** %stackaddr$env-ref58364
%stackaddr$prim58365 = alloca %struct.ScmObj*, align 8
%_95k48445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56630)
store volatile %struct.ScmObj* %_95k48445, %struct.ScmObj** %stackaddr$prim58365, align 8
%stackaddr$prim58366 = alloca %struct.ScmObj*, align 8
%current_45args56631 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56630)
store volatile %struct.ScmObj* %current_45args56631, %struct.ScmObj** %stackaddr$prim58366, align 8
%stackaddr$prim58367 = alloca %struct.ScmObj*, align 8
%anf_45bind48210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56631)
store volatile %struct.ScmObj* %anf_45bind48210, %struct.ScmObj** %stackaddr$prim58367, align 8
%stackaddr$prim58368 = alloca %struct.ScmObj*, align 8
%cpsargs48446 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48436, %struct.ScmObj* %anf_45bind48210)
store volatile %struct.ScmObj* %cpsargs48446, %struct.ScmObj** %stackaddr$prim58368, align 8
%clofunc58369 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48062)
musttail call tailcc void %clofunc58369(%struct.ScmObj* %f48062, %struct.ScmObj* %cpsargs48446)
ret void
}

define tailcc void @proc_clo$ae49305(%struct.ScmObj* %env$ae49305,%struct.ScmObj* %current_45args56634) {
%stackaddr$prim58370 = alloca %struct.ScmObj*, align 8
%k48448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56634)
store volatile %struct.ScmObj* %k48448, %struct.ScmObj** %stackaddr$prim58370, align 8
%stackaddr$prim58371 = alloca %struct.ScmObj*, align 8
%current_45args56635 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56634)
store volatile %struct.ScmObj* %current_45args56635, %struct.ScmObj** %stackaddr$prim58371, align 8
%stackaddr$prim58372 = alloca %struct.ScmObj*, align 8
%a48070 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56635)
store volatile %struct.ScmObj* %a48070, %struct.ScmObj** %stackaddr$prim58372, align 8
%stackaddr$prim58373 = alloca %struct.ScmObj*, align 8
%current_45args56636 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56635)
store volatile %struct.ScmObj* %current_45args56636, %struct.ScmObj** %stackaddr$prim58373, align 8
%stackaddr$prim58374 = alloca %struct.ScmObj*, align 8
%b48069 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56636)
store volatile %struct.ScmObj* %b48069, %struct.ScmObj** %stackaddr$prim58374, align 8
%stackaddr$prim58375 = alloca %struct.ScmObj*, align 8
%cpsprim48449 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48070, %struct.ScmObj* %b48069)
store volatile %struct.ScmObj* %cpsprim48449, %struct.ScmObj** %stackaddr$prim58375, align 8
%ae49309 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56638$k484480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58376 = alloca %struct.ScmObj*, align 8
%argslist56638$k484481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48449, %struct.ScmObj* %argslist56638$k484480)
store volatile %struct.ScmObj* %argslist56638$k484481, %struct.ScmObj** %stackaddr$prim58376, align 8
%stackaddr$prim58377 = alloca %struct.ScmObj*, align 8
%argslist56638$k484482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49309, %struct.ScmObj* %argslist56638$k484481)
store volatile %struct.ScmObj* %argslist56638$k484482, %struct.ScmObj** %stackaddr$prim58377, align 8
%clofunc58378 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48448)
musttail call tailcc void %clofunc58378(%struct.ScmObj* %k48448, %struct.ScmObj* %argslist56638$k484482)
ret void
}

define tailcc void @proc_clo$ae49281(%struct.ScmObj* %env$ae49281,%struct.ScmObj* %current_45args56641) {
%stackaddr$prim58379 = alloca %struct.ScmObj*, align 8
%k48450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56641)
store volatile %struct.ScmObj* %k48450, %struct.ScmObj** %stackaddr$prim58379, align 8
%stackaddr$prim58380 = alloca %struct.ScmObj*, align 8
%current_45args56642 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56641)
store volatile %struct.ScmObj* %current_45args56642, %struct.ScmObj** %stackaddr$prim58380, align 8
%stackaddr$prim58381 = alloca %struct.ScmObj*, align 8
%x48066 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56642)
store volatile %struct.ScmObj* %x48066, %struct.ScmObj** %stackaddr$prim58381, align 8
%stackaddr$prim58382 = alloca %struct.ScmObj*, align 8
%cpsprim48451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48066)
store volatile %struct.ScmObj* %cpsprim48451, %struct.ScmObj** %stackaddr$prim58382, align 8
%ae49284 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56644$k484500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58383 = alloca %struct.ScmObj*, align 8
%argslist56644$k484501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48451, %struct.ScmObj* %argslist56644$k484500)
store volatile %struct.ScmObj* %argslist56644$k484501, %struct.ScmObj** %stackaddr$prim58383, align 8
%stackaddr$prim58384 = alloca %struct.ScmObj*, align 8
%argslist56644$k484502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49284, %struct.ScmObj* %argslist56644$k484501)
store volatile %struct.ScmObj* %argslist56644$k484502, %struct.ScmObj** %stackaddr$prim58384, align 8
%clofunc58385 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48450)
musttail call tailcc void %clofunc58385(%struct.ScmObj* %k48450, %struct.ScmObj* %argslist56644$k484502)
ret void
}

define tailcc void @proc_clo$ae49257(%struct.ScmObj* %env$ae49257,%struct.ScmObj* %current_45args56647) {
%stackaddr$prim58386 = alloca %struct.ScmObj*, align 8
%k48452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56647)
store volatile %struct.ScmObj* %k48452, %struct.ScmObj** %stackaddr$prim58386, align 8
%stackaddr$prim58387 = alloca %struct.ScmObj*, align 8
%current_45args56648 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56647)
store volatile %struct.ScmObj* %current_45args56648, %struct.ScmObj** %stackaddr$prim58387, align 8
%stackaddr$prim58388 = alloca %struct.ScmObj*, align 8
%x48068 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56648)
store volatile %struct.ScmObj* %x48068, %struct.ScmObj** %stackaddr$prim58388, align 8
%stackaddr$prim58389 = alloca %struct.ScmObj*, align 8
%cpsprim48453 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48068)
store volatile %struct.ScmObj* %cpsprim48453, %struct.ScmObj** %stackaddr$prim58389, align 8
%ae49260 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56650$k484520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58390 = alloca %struct.ScmObj*, align 8
%argslist56650$k484521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48453, %struct.ScmObj* %argslist56650$k484520)
store volatile %struct.ScmObj* %argslist56650$k484521, %struct.ScmObj** %stackaddr$prim58390, align 8
%stackaddr$prim58391 = alloca %struct.ScmObj*, align 8
%argslist56650$k484522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49260, %struct.ScmObj* %argslist56650$k484521)
store volatile %struct.ScmObj* %argslist56650$k484522, %struct.ScmObj** %stackaddr$prim58391, align 8
%clofunc58392 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48452)
musttail call tailcc void %clofunc58392(%struct.ScmObj* %k48452, %struct.ScmObj* %argslist56650$k484522)
ret void
}

define tailcc void @proc_clo$ae49209(%struct.ScmObj* %env$ae49209,%struct.ScmObj* %current_45args56653) {
%stackaddr$prim58393 = alloca %struct.ScmObj*, align 8
%k48454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56653)
store volatile %struct.ScmObj* %k48454, %struct.ScmObj** %stackaddr$prim58393, align 8
%stackaddr$prim58394 = alloca %struct.ScmObj*, align 8
%current_45args56654 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56653)
store volatile %struct.ScmObj* %current_45args56654, %struct.ScmObj** %stackaddr$prim58394, align 8
%stackaddr$prim58395 = alloca %struct.ScmObj*, align 8
%lst48064 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56654)
store volatile %struct.ScmObj* %lst48064, %struct.ScmObj** %stackaddr$prim58395, align 8
%stackaddr$prim58396 = alloca %struct.ScmObj*, align 8
%current_45args56655 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56654)
store volatile %struct.ScmObj* %current_45args56655, %struct.ScmObj** %stackaddr$prim58396, align 8
%stackaddr$prim58397 = alloca %struct.ScmObj*, align 8
%b48063 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56655)
store volatile %struct.ScmObj* %b48063, %struct.ScmObj** %stackaddr$prim58397, align 8
%truthy$cmp58398 = call i64 @is_truthy_value(%struct.ScmObj* %b48063)
%cmp$cmp58398 = icmp eq i64 %truthy$cmp58398, 1
br i1 %cmp$cmp58398, label %truebranch$cmp58398, label %falsebranch$cmp58398
truebranch$cmp58398:
%ae49212 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56657$k484540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58399 = alloca %struct.ScmObj*, align 8
%argslist56657$k484541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48063, %struct.ScmObj* %argslist56657$k484540)
store volatile %struct.ScmObj* %argslist56657$k484541, %struct.ScmObj** %stackaddr$prim58399, align 8
%stackaddr$prim58400 = alloca %struct.ScmObj*, align 8
%argslist56657$k484542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49212, %struct.ScmObj* %argslist56657$k484541)
store volatile %struct.ScmObj* %argslist56657$k484542, %struct.ScmObj** %stackaddr$prim58400, align 8
%clofunc58401 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48454)
musttail call tailcc void %clofunc58401(%struct.ScmObj* %k48454, %struct.ScmObj* %argslist56657$k484542)
ret void
falsebranch$cmp58398:
%stackaddr$prim58402 = alloca %struct.ScmObj*, align 8
%cpsprim48455 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48064)
store volatile %struct.ScmObj* %cpsprim48455, %struct.ScmObj** %stackaddr$prim58402, align 8
%ae49219 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56658$k484540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58403 = alloca %struct.ScmObj*, align 8
%argslist56658$k484541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48455, %struct.ScmObj* %argslist56658$k484540)
store volatile %struct.ScmObj* %argslist56658$k484541, %struct.ScmObj** %stackaddr$prim58403, align 8
%stackaddr$prim58404 = alloca %struct.ScmObj*, align 8
%argslist56658$k484542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49219, %struct.ScmObj* %argslist56658$k484541)
store volatile %struct.ScmObj* %argslist56658$k484542, %struct.ScmObj** %stackaddr$prim58404, align 8
%clofunc58405 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48454)
musttail call tailcc void %clofunc58405(%struct.ScmObj* %k48454, %struct.ScmObj* %argslist56658$k484542)
ret void
}

define tailcc void @proc_clo$ae49166(%struct.ScmObj* %env$ae49166,%struct.ScmObj* %current_45args56662) {
%stackaddr$env-ref58406 = alloca %struct.ScmObj*, align 8
%_37length48041 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49166, i64 0)
store %struct.ScmObj* %_37length48041, %struct.ScmObj** %stackaddr$env-ref58406
%stackaddr$env-ref58407 = alloca %struct.ScmObj*, align 8
%_37take48044 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49166, i64 1)
store %struct.ScmObj* %_37take48044, %struct.ScmObj** %stackaddr$env-ref58407
%stackaddr$prim58408 = alloca %struct.ScmObj*, align 8
%k48456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56662)
store volatile %struct.ScmObj* %k48456, %struct.ScmObj** %stackaddr$prim58408, align 8
%stackaddr$prim58409 = alloca %struct.ScmObj*, align 8
%current_45args56663 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56662)
store volatile %struct.ScmObj* %current_45args56663, %struct.ScmObj** %stackaddr$prim58409, align 8
%stackaddr$prim58410 = alloca %struct.ScmObj*, align 8
%lst48073 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56663)
store volatile %struct.ScmObj* %lst48073, %struct.ScmObj** %stackaddr$prim58410, align 8
%stackaddr$prim58411 = alloca %struct.ScmObj*, align 8
%current_45args56664 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56663)
store volatile %struct.ScmObj* %current_45args56664, %struct.ScmObj** %stackaddr$prim58411, align 8
%stackaddr$prim58412 = alloca %struct.ScmObj*, align 8
%n48072 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56664)
store volatile %struct.ScmObj* %n48072, %struct.ScmObj** %stackaddr$prim58412, align 8
%stackaddr$makeclosure58413 = alloca %struct.ScmObj*, align 8
%fptrToInt58414 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49168 to i64
%ae49168 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58414)
store volatile %struct.ScmObj* %ae49168, %struct.ScmObj** %stackaddr$makeclosure58413, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49168, %struct.ScmObj* %n48072, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49168, %struct.ScmObj* %k48456, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49168, %struct.ScmObj* %lst48073, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49168, %struct.ScmObj* %_37take48044, i64 3)
%argslist56670$_37length480410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58415 = alloca %struct.ScmObj*, align 8
%argslist56670$_37length480411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48073, %struct.ScmObj* %argslist56670$_37length480410)
store volatile %struct.ScmObj* %argslist56670$_37length480411, %struct.ScmObj** %stackaddr$prim58415, align 8
%stackaddr$prim58416 = alloca %struct.ScmObj*, align 8
%argslist56670$_37length480412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49168, %struct.ScmObj* %argslist56670$_37length480411)
store volatile %struct.ScmObj* %argslist56670$_37length480412, %struct.ScmObj** %stackaddr$prim58416, align 8
%clofunc58417 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48041)
musttail call tailcc void %clofunc58417(%struct.ScmObj* %_37length48041, %struct.ScmObj* %argslist56670$_37length480412)
ret void
}

define tailcc void @proc_clo$ae49168(%struct.ScmObj* %env$ae49168,%struct.ScmObj* %current_45args56666) {
%stackaddr$env-ref58418 = alloca %struct.ScmObj*, align 8
%n48072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49168, i64 0)
store %struct.ScmObj* %n48072, %struct.ScmObj** %stackaddr$env-ref58418
%stackaddr$env-ref58419 = alloca %struct.ScmObj*, align 8
%k48456 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49168, i64 1)
store %struct.ScmObj* %k48456, %struct.ScmObj** %stackaddr$env-ref58419
%stackaddr$env-ref58420 = alloca %struct.ScmObj*, align 8
%lst48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49168, i64 2)
store %struct.ScmObj* %lst48073, %struct.ScmObj** %stackaddr$env-ref58420
%stackaddr$env-ref58421 = alloca %struct.ScmObj*, align 8
%_37take48044 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49168, i64 3)
store %struct.ScmObj* %_37take48044, %struct.ScmObj** %stackaddr$env-ref58421
%stackaddr$prim58422 = alloca %struct.ScmObj*, align 8
%_95k48457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56666)
store volatile %struct.ScmObj* %_95k48457, %struct.ScmObj** %stackaddr$prim58422, align 8
%stackaddr$prim58423 = alloca %struct.ScmObj*, align 8
%current_45args56667 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56666)
store volatile %struct.ScmObj* %current_45args56667, %struct.ScmObj** %stackaddr$prim58423, align 8
%stackaddr$prim58424 = alloca %struct.ScmObj*, align 8
%anf_45bind48197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56667)
store volatile %struct.ScmObj* %anf_45bind48197, %struct.ScmObj** %stackaddr$prim58424, align 8
%stackaddr$prim58425 = alloca %struct.ScmObj*, align 8
%anf_45bind48198 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48197, %struct.ScmObj* %n48072)
store volatile %struct.ScmObj* %anf_45bind48198, %struct.ScmObj** %stackaddr$prim58425, align 8
%argslist56669$_37take480440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58426 = alloca %struct.ScmObj*, align 8
%argslist56669$_37take480441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48198, %struct.ScmObj* %argslist56669$_37take480440)
store volatile %struct.ScmObj* %argslist56669$_37take480441, %struct.ScmObj** %stackaddr$prim58426, align 8
%stackaddr$prim58427 = alloca %struct.ScmObj*, align 8
%argslist56669$_37take480442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48073, %struct.ScmObj* %argslist56669$_37take480441)
store volatile %struct.ScmObj* %argslist56669$_37take480442, %struct.ScmObj** %stackaddr$prim58427, align 8
%stackaddr$prim58428 = alloca %struct.ScmObj*, align 8
%argslist56669$_37take480443 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48456, %struct.ScmObj* %argslist56669$_37take480442)
store volatile %struct.ScmObj* %argslist56669$_37take480443, %struct.ScmObj** %stackaddr$prim58428, align 8
%clofunc58429 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48044)
musttail call tailcc void %clofunc58429(%struct.ScmObj* %_37take48044, %struct.ScmObj* %argslist56669$_37take480443)
ret void
}

define tailcc void @proc_clo$ae49112(%struct.ScmObj* %env$ae49112,%struct.ScmObj* %current_45args56672) {
%stackaddr$env-ref58430 = alloca %struct.ScmObj*, align 8
%_37foldl148036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49112, i64 0)
store %struct.ScmObj* %_37foldl148036, %struct.ScmObj** %stackaddr$env-ref58430
%stackaddr$prim58431 = alloca %struct.ScmObj*, align 8
%k48458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56672)
store volatile %struct.ScmObj* %k48458, %struct.ScmObj** %stackaddr$prim58431, align 8
%stackaddr$prim58432 = alloca %struct.ScmObj*, align 8
%current_45args56673 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56672)
store volatile %struct.ScmObj* %current_45args56673, %struct.ScmObj** %stackaddr$prim58432, align 8
%stackaddr$prim58433 = alloca %struct.ScmObj*, align 8
%lst48075 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56673)
store volatile %struct.ScmObj* %lst48075, %struct.ScmObj** %stackaddr$prim58433, align 8
%stackaddr$makeclosure58434 = alloca %struct.ScmObj*, align 8
%fptrToInt58435 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49113 to i64
%ae49113 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58435)
store volatile %struct.ScmObj* %ae49113, %struct.ScmObj** %stackaddr$makeclosure58434, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49113, %struct.ScmObj* %_37foldl148036, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49113, %struct.ScmObj* %lst48075, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49113, %struct.ScmObj* %k48458, i64 2)
%ae49114 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58436 = alloca %struct.ScmObj*, align 8
%fptrToInt58437 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49115 to i64
%ae49115 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58437)
store volatile %struct.ScmObj* %ae49115, %struct.ScmObj** %stackaddr$makeclosure58436, align 8
%argslist56684$ae491130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58438 = alloca %struct.ScmObj*, align 8
%argslist56684$ae491131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49115, %struct.ScmObj* %argslist56684$ae491130)
store volatile %struct.ScmObj* %argslist56684$ae491131, %struct.ScmObj** %stackaddr$prim58438, align 8
%stackaddr$prim58439 = alloca %struct.ScmObj*, align 8
%argslist56684$ae491132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49114, %struct.ScmObj* %argslist56684$ae491131)
store volatile %struct.ScmObj* %argslist56684$ae491132, %struct.ScmObj** %stackaddr$prim58439, align 8
%clofunc58440 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49113)
musttail call tailcc void %clofunc58440(%struct.ScmObj* %ae49113, %struct.ScmObj* %argslist56684$ae491132)
ret void
}

define tailcc void @proc_clo$ae49113(%struct.ScmObj* %env$ae49113,%struct.ScmObj* %current_45args56675) {
%stackaddr$env-ref58441 = alloca %struct.ScmObj*, align 8
%_37foldl148036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49113, i64 0)
store %struct.ScmObj* %_37foldl148036, %struct.ScmObj** %stackaddr$env-ref58441
%stackaddr$env-ref58442 = alloca %struct.ScmObj*, align 8
%lst48075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49113, i64 1)
store %struct.ScmObj* %lst48075, %struct.ScmObj** %stackaddr$env-ref58442
%stackaddr$env-ref58443 = alloca %struct.ScmObj*, align 8
%k48458 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49113, i64 2)
store %struct.ScmObj* %k48458, %struct.ScmObj** %stackaddr$env-ref58443
%stackaddr$prim58444 = alloca %struct.ScmObj*, align 8
%_95k48459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56675)
store volatile %struct.ScmObj* %_95k48459, %struct.ScmObj** %stackaddr$prim58444, align 8
%stackaddr$prim58445 = alloca %struct.ScmObj*, align 8
%current_45args56676 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56675)
store volatile %struct.ScmObj* %current_45args56676, %struct.ScmObj** %stackaddr$prim58445, align 8
%stackaddr$prim58446 = alloca %struct.ScmObj*, align 8
%anf_45bind48196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56676)
store volatile %struct.ScmObj* %anf_45bind48196, %struct.ScmObj** %stackaddr$prim58446, align 8
%ae49134 = call %struct.ScmObj* @const_init_null()
%argslist56678$_37foldl1480360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58447 = alloca %struct.ScmObj*, align 8
%argslist56678$_37foldl1480361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48075, %struct.ScmObj* %argslist56678$_37foldl1480360)
store volatile %struct.ScmObj* %argslist56678$_37foldl1480361, %struct.ScmObj** %stackaddr$prim58447, align 8
%stackaddr$prim58448 = alloca %struct.ScmObj*, align 8
%argslist56678$_37foldl1480362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49134, %struct.ScmObj* %argslist56678$_37foldl1480361)
store volatile %struct.ScmObj* %argslist56678$_37foldl1480362, %struct.ScmObj** %stackaddr$prim58448, align 8
%stackaddr$prim58449 = alloca %struct.ScmObj*, align 8
%argslist56678$_37foldl1480363 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48196, %struct.ScmObj* %argslist56678$_37foldl1480362)
store volatile %struct.ScmObj* %argslist56678$_37foldl1480363, %struct.ScmObj** %stackaddr$prim58449, align 8
%stackaddr$prim58450 = alloca %struct.ScmObj*, align 8
%argslist56678$_37foldl1480364 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48458, %struct.ScmObj* %argslist56678$_37foldl1480363)
store volatile %struct.ScmObj* %argslist56678$_37foldl1480364, %struct.ScmObj** %stackaddr$prim58450, align 8
%clofunc58451 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148036)
musttail call tailcc void %clofunc58451(%struct.ScmObj* %_37foldl148036, %struct.ScmObj* %argslist56678$_37foldl1480364)
ret void
}

define tailcc void @proc_clo$ae49115(%struct.ScmObj* %env$ae49115,%struct.ScmObj* %current_45args56679) {
%stackaddr$prim58452 = alloca %struct.ScmObj*, align 8
%k48460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56679)
store volatile %struct.ScmObj* %k48460, %struct.ScmObj** %stackaddr$prim58452, align 8
%stackaddr$prim58453 = alloca %struct.ScmObj*, align 8
%current_45args56680 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56679)
store volatile %struct.ScmObj* %current_45args56680, %struct.ScmObj** %stackaddr$prim58453, align 8
%stackaddr$prim58454 = alloca %struct.ScmObj*, align 8
%x48077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56680)
store volatile %struct.ScmObj* %x48077, %struct.ScmObj** %stackaddr$prim58454, align 8
%stackaddr$prim58455 = alloca %struct.ScmObj*, align 8
%current_45args56681 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56680)
store volatile %struct.ScmObj* %current_45args56681, %struct.ScmObj** %stackaddr$prim58455, align 8
%stackaddr$prim58456 = alloca %struct.ScmObj*, align 8
%y48076 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56681)
store volatile %struct.ScmObj* %y48076, %struct.ScmObj** %stackaddr$prim58456, align 8
%ae49117 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56683$k484600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58457 = alloca %struct.ScmObj*, align 8
%argslist56683$k484601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x48077, %struct.ScmObj* %argslist56683$k484600)
store volatile %struct.ScmObj* %argslist56683$k484601, %struct.ScmObj** %stackaddr$prim58457, align 8
%stackaddr$prim58458 = alloca %struct.ScmObj*, align 8
%argslist56683$k484602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49117, %struct.ScmObj* %argslist56683$k484601)
store volatile %struct.ScmObj* %argslist56683$k484602, %struct.ScmObj** %stackaddr$prim58458, align 8
%clofunc58459 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48460)
musttail call tailcc void %clofunc58459(%struct.ScmObj* %k48460, %struct.ScmObj* %argslist56683$k484602)
ret void
}

define tailcc void @proc_clo$ae49033(%struct.ScmObj* %env$ae49033,%struct.ScmObj* %current_45args56687) {
%stackaddr$prim58460 = alloca %struct.ScmObj*, align 8
%k48461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56687)
store volatile %struct.ScmObj* %k48461, %struct.ScmObj** %stackaddr$prim58460, align 8
%stackaddr$prim58461 = alloca %struct.ScmObj*, align 8
%current_45args56688 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56687)
store volatile %struct.ScmObj* %current_45args56688, %struct.ScmObj** %stackaddr$prim58461, align 8
%stackaddr$prim58462 = alloca %struct.ScmObj*, align 8
%_37foldl148037 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56688)
store volatile %struct.ScmObj* %_37foldl148037, %struct.ScmObj** %stackaddr$prim58462, align 8
%ae49035 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58463 = alloca %struct.ScmObj*, align 8
%fptrToInt58464 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49036 to i64
%ae49036 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58464)
store volatile %struct.ScmObj* %ae49036, %struct.ScmObj** %stackaddr$makeclosure58463, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49036, %struct.ScmObj* %_37foldl148037, i64 0)
%argslist56701$k484610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58465 = alloca %struct.ScmObj*, align 8
%argslist56701$k484611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49036, %struct.ScmObj* %argslist56701$k484610)
store volatile %struct.ScmObj* %argslist56701$k484611, %struct.ScmObj** %stackaddr$prim58465, align 8
%stackaddr$prim58466 = alloca %struct.ScmObj*, align 8
%argslist56701$k484612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49035, %struct.ScmObj* %argslist56701$k484611)
store volatile %struct.ScmObj* %argslist56701$k484612, %struct.ScmObj** %stackaddr$prim58466, align 8
%clofunc58467 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48461)
musttail call tailcc void %clofunc58467(%struct.ScmObj* %k48461, %struct.ScmObj* %argslist56701$k484612)
ret void
}

define tailcc void @proc_clo$ae49036(%struct.ScmObj* %env$ae49036,%struct.ScmObj* %current_45args56690) {
%stackaddr$env-ref58468 = alloca %struct.ScmObj*, align 8
%_37foldl148037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49036, i64 0)
store %struct.ScmObj* %_37foldl148037, %struct.ScmObj** %stackaddr$env-ref58468
%stackaddr$prim58469 = alloca %struct.ScmObj*, align 8
%k48462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56690)
store volatile %struct.ScmObj* %k48462, %struct.ScmObj** %stackaddr$prim58469, align 8
%stackaddr$prim58470 = alloca %struct.ScmObj*, align 8
%current_45args56691 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56690)
store volatile %struct.ScmObj* %current_45args56691, %struct.ScmObj** %stackaddr$prim58470, align 8
%stackaddr$prim58471 = alloca %struct.ScmObj*, align 8
%f48040 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56691)
store volatile %struct.ScmObj* %f48040, %struct.ScmObj** %stackaddr$prim58471, align 8
%stackaddr$prim58472 = alloca %struct.ScmObj*, align 8
%current_45args56692 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56691)
store volatile %struct.ScmObj* %current_45args56692, %struct.ScmObj** %stackaddr$prim58472, align 8
%stackaddr$prim58473 = alloca %struct.ScmObj*, align 8
%acc48039 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56692)
store volatile %struct.ScmObj* %acc48039, %struct.ScmObj** %stackaddr$prim58473, align 8
%stackaddr$prim58474 = alloca %struct.ScmObj*, align 8
%current_45args56693 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56692)
store volatile %struct.ScmObj* %current_45args56693, %struct.ScmObj** %stackaddr$prim58474, align 8
%stackaddr$prim58475 = alloca %struct.ScmObj*, align 8
%lst48038 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56693)
store volatile %struct.ScmObj* %lst48038, %struct.ScmObj** %stackaddr$prim58475, align 8
%stackaddr$prim58476 = alloca %struct.ScmObj*, align 8
%anf_45bind48191 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48038)
store volatile %struct.ScmObj* %anf_45bind48191, %struct.ScmObj** %stackaddr$prim58476, align 8
%truthy$cmp58477 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48191)
%cmp$cmp58477 = icmp eq i64 %truthy$cmp58477, 1
br i1 %cmp$cmp58477, label %truebranch$cmp58477, label %falsebranch$cmp58477
truebranch$cmp58477:
%ae49040 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56695$k484620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58478 = alloca %struct.ScmObj*, align 8
%argslist56695$k484621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48039, %struct.ScmObj* %argslist56695$k484620)
store volatile %struct.ScmObj* %argslist56695$k484621, %struct.ScmObj** %stackaddr$prim58478, align 8
%stackaddr$prim58479 = alloca %struct.ScmObj*, align 8
%argslist56695$k484622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49040, %struct.ScmObj* %argslist56695$k484621)
store volatile %struct.ScmObj* %argslist56695$k484622, %struct.ScmObj** %stackaddr$prim58479, align 8
%clofunc58480 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48462)
musttail call tailcc void %clofunc58480(%struct.ScmObj* %k48462, %struct.ScmObj* %argslist56695$k484622)
ret void
falsebranch$cmp58477:
%stackaddr$prim58481 = alloca %struct.ScmObj*, align 8
%anf_45bind48192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48038)
store volatile %struct.ScmObj* %anf_45bind48192, %struct.ScmObj** %stackaddr$prim58481, align 8
%stackaddr$makeclosure58482 = alloca %struct.ScmObj*, align 8
%fptrToInt58483 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49047 to i64
%ae49047 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58483)
store volatile %struct.ScmObj* %ae49047, %struct.ScmObj** %stackaddr$makeclosure58482, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49047, %struct.ScmObj* %f48040, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49047, %struct.ScmObj* %lst48038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49047, %struct.ScmObj* %_37foldl148037, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49047, %struct.ScmObj* %k48462, i64 3)
%argslist56700$f480400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58484 = alloca %struct.ScmObj*, align 8
%argslist56700$f480401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48039, %struct.ScmObj* %argslist56700$f480400)
store volatile %struct.ScmObj* %argslist56700$f480401, %struct.ScmObj** %stackaddr$prim58484, align 8
%stackaddr$prim58485 = alloca %struct.ScmObj*, align 8
%argslist56700$f480402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48192, %struct.ScmObj* %argslist56700$f480401)
store volatile %struct.ScmObj* %argslist56700$f480402, %struct.ScmObj** %stackaddr$prim58485, align 8
%stackaddr$prim58486 = alloca %struct.ScmObj*, align 8
%argslist56700$f480403 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49047, %struct.ScmObj* %argslist56700$f480402)
store volatile %struct.ScmObj* %argslist56700$f480403, %struct.ScmObj** %stackaddr$prim58486, align 8
%clofunc58487 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48040)
musttail call tailcc void %clofunc58487(%struct.ScmObj* %f48040, %struct.ScmObj* %argslist56700$f480403)
ret void
}

define tailcc void @proc_clo$ae49047(%struct.ScmObj* %env$ae49047,%struct.ScmObj* %current_45args56696) {
%stackaddr$env-ref58488 = alloca %struct.ScmObj*, align 8
%f48040 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49047, i64 0)
store %struct.ScmObj* %f48040, %struct.ScmObj** %stackaddr$env-ref58488
%stackaddr$env-ref58489 = alloca %struct.ScmObj*, align 8
%lst48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49047, i64 1)
store %struct.ScmObj* %lst48038, %struct.ScmObj** %stackaddr$env-ref58489
%stackaddr$env-ref58490 = alloca %struct.ScmObj*, align 8
%_37foldl148037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49047, i64 2)
store %struct.ScmObj* %_37foldl148037, %struct.ScmObj** %stackaddr$env-ref58490
%stackaddr$env-ref58491 = alloca %struct.ScmObj*, align 8
%k48462 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49047, i64 3)
store %struct.ScmObj* %k48462, %struct.ScmObj** %stackaddr$env-ref58491
%stackaddr$prim58492 = alloca %struct.ScmObj*, align 8
%_95k48463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56696)
store volatile %struct.ScmObj* %_95k48463, %struct.ScmObj** %stackaddr$prim58492, align 8
%stackaddr$prim58493 = alloca %struct.ScmObj*, align 8
%current_45args56697 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56696)
store volatile %struct.ScmObj* %current_45args56697, %struct.ScmObj** %stackaddr$prim58493, align 8
%stackaddr$prim58494 = alloca %struct.ScmObj*, align 8
%anf_45bind48193 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56697)
store volatile %struct.ScmObj* %anf_45bind48193, %struct.ScmObj** %stackaddr$prim58494, align 8
%stackaddr$prim58495 = alloca %struct.ScmObj*, align 8
%anf_45bind48194 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48038)
store volatile %struct.ScmObj* %anf_45bind48194, %struct.ScmObj** %stackaddr$prim58495, align 8
%argslist56699$_37foldl1480370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58496 = alloca %struct.ScmObj*, align 8
%argslist56699$_37foldl1480371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48194, %struct.ScmObj* %argslist56699$_37foldl1480370)
store volatile %struct.ScmObj* %argslist56699$_37foldl1480371, %struct.ScmObj** %stackaddr$prim58496, align 8
%stackaddr$prim58497 = alloca %struct.ScmObj*, align 8
%argslist56699$_37foldl1480372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48193, %struct.ScmObj* %argslist56699$_37foldl1480371)
store volatile %struct.ScmObj* %argslist56699$_37foldl1480372, %struct.ScmObj** %stackaddr$prim58497, align 8
%stackaddr$prim58498 = alloca %struct.ScmObj*, align 8
%argslist56699$_37foldl1480373 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48040, %struct.ScmObj* %argslist56699$_37foldl1480372)
store volatile %struct.ScmObj* %argslist56699$_37foldl1480373, %struct.ScmObj** %stackaddr$prim58498, align 8
%stackaddr$prim58499 = alloca %struct.ScmObj*, align 8
%argslist56699$_37foldl1480374 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48462, %struct.ScmObj* %argslist56699$_37foldl1480373)
store volatile %struct.ScmObj* %argslist56699$_37foldl1480374, %struct.ScmObj** %stackaddr$prim58499, align 8
%clofunc58500 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148037)
musttail call tailcc void %clofunc58500(%struct.ScmObj* %_37foldl148037, %struct.ScmObj* %argslist56699$_37foldl1480374)
ret void
}

define tailcc void @proc_clo$ae48950(%struct.ScmObj* %env$ae48950,%struct.ScmObj* %current_45args56704) {
%stackaddr$prim58501 = alloca %struct.ScmObj*, align 8
%k48464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56704)
store volatile %struct.ScmObj* %k48464, %struct.ScmObj** %stackaddr$prim58501, align 8
%stackaddr$prim58502 = alloca %struct.ScmObj*, align 8
%current_45args56705 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56704)
store volatile %struct.ScmObj* %current_45args56705, %struct.ScmObj** %stackaddr$prim58502, align 8
%stackaddr$prim58503 = alloca %struct.ScmObj*, align 8
%_37length48042 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56705)
store volatile %struct.ScmObj* %_37length48042, %struct.ScmObj** %stackaddr$prim58503, align 8
%ae48952 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58504 = alloca %struct.ScmObj*, align 8
%fptrToInt58505 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48953 to i64
%ae48953 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58505)
store volatile %struct.ScmObj* %ae48953, %struct.ScmObj** %stackaddr$makeclosure58504, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48953, %struct.ScmObj* %_37length48042, i64 0)
%argslist56716$k484640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58506 = alloca %struct.ScmObj*, align 8
%argslist56716$k484641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48953, %struct.ScmObj* %argslist56716$k484640)
store volatile %struct.ScmObj* %argslist56716$k484641, %struct.ScmObj** %stackaddr$prim58506, align 8
%stackaddr$prim58507 = alloca %struct.ScmObj*, align 8
%argslist56716$k484642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48952, %struct.ScmObj* %argslist56716$k484641)
store volatile %struct.ScmObj* %argslist56716$k484642, %struct.ScmObj** %stackaddr$prim58507, align 8
%clofunc58508 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48464)
musttail call tailcc void %clofunc58508(%struct.ScmObj* %k48464, %struct.ScmObj* %argslist56716$k484642)
ret void
}

define tailcc void @proc_clo$ae48953(%struct.ScmObj* %env$ae48953,%struct.ScmObj* %current_45args56707) {
%stackaddr$env-ref58509 = alloca %struct.ScmObj*, align 8
%_37length48042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48953, i64 0)
store %struct.ScmObj* %_37length48042, %struct.ScmObj** %stackaddr$env-ref58509
%stackaddr$prim58510 = alloca %struct.ScmObj*, align 8
%k48465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56707)
store volatile %struct.ScmObj* %k48465, %struct.ScmObj** %stackaddr$prim58510, align 8
%stackaddr$prim58511 = alloca %struct.ScmObj*, align 8
%current_45args56708 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56707)
store volatile %struct.ScmObj* %current_45args56708, %struct.ScmObj** %stackaddr$prim58511, align 8
%stackaddr$prim58512 = alloca %struct.ScmObj*, align 8
%lst48043 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56708)
store volatile %struct.ScmObj* %lst48043, %struct.ScmObj** %stackaddr$prim58512, align 8
%stackaddr$prim58513 = alloca %struct.ScmObj*, align 8
%anf_45bind48187 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48043)
store volatile %struct.ScmObj* %anf_45bind48187, %struct.ScmObj** %stackaddr$prim58513, align 8
%truthy$cmp58514 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48187)
%cmp$cmp58514 = icmp eq i64 %truthy$cmp58514, 1
br i1 %cmp$cmp58514, label %truebranch$cmp58514, label %falsebranch$cmp58514
truebranch$cmp58514:
%ae48957 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48958 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56710$k484650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58515 = alloca %struct.ScmObj*, align 8
%argslist56710$k484651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48958, %struct.ScmObj* %argslist56710$k484650)
store volatile %struct.ScmObj* %argslist56710$k484651, %struct.ScmObj** %stackaddr$prim58515, align 8
%stackaddr$prim58516 = alloca %struct.ScmObj*, align 8
%argslist56710$k484652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48957, %struct.ScmObj* %argslist56710$k484651)
store volatile %struct.ScmObj* %argslist56710$k484652, %struct.ScmObj** %stackaddr$prim58516, align 8
%clofunc58517 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48465)
musttail call tailcc void %clofunc58517(%struct.ScmObj* %k48465, %struct.ScmObj* %argslist56710$k484652)
ret void
falsebranch$cmp58514:
%stackaddr$prim58518 = alloca %struct.ScmObj*, align 8
%anf_45bind48188 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48043)
store volatile %struct.ScmObj* %anf_45bind48188, %struct.ScmObj** %stackaddr$prim58518, align 8
%stackaddr$makeclosure58519 = alloca %struct.ScmObj*, align 8
%fptrToInt58520 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48967 to i64
%ae48967 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58520)
store volatile %struct.ScmObj* %ae48967, %struct.ScmObj** %stackaddr$makeclosure58519, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48967, %struct.ScmObj* %k48465, i64 0)
%argslist56715$_37length480420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58521 = alloca %struct.ScmObj*, align 8
%argslist56715$_37length480421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48188, %struct.ScmObj* %argslist56715$_37length480420)
store volatile %struct.ScmObj* %argslist56715$_37length480421, %struct.ScmObj** %stackaddr$prim58521, align 8
%stackaddr$prim58522 = alloca %struct.ScmObj*, align 8
%argslist56715$_37length480422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48967, %struct.ScmObj* %argslist56715$_37length480421)
store volatile %struct.ScmObj* %argslist56715$_37length480422, %struct.ScmObj** %stackaddr$prim58522, align 8
%clofunc58523 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48042)
musttail call tailcc void %clofunc58523(%struct.ScmObj* %_37length48042, %struct.ScmObj* %argslist56715$_37length480422)
ret void
}

define tailcc void @proc_clo$ae48967(%struct.ScmObj* %env$ae48967,%struct.ScmObj* %current_45args56711) {
%stackaddr$env-ref58524 = alloca %struct.ScmObj*, align 8
%k48465 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48967, i64 0)
store %struct.ScmObj* %k48465, %struct.ScmObj** %stackaddr$env-ref58524
%stackaddr$prim58525 = alloca %struct.ScmObj*, align 8
%_95k48466 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56711)
store volatile %struct.ScmObj* %_95k48466, %struct.ScmObj** %stackaddr$prim58525, align 8
%stackaddr$prim58526 = alloca %struct.ScmObj*, align 8
%current_45args56712 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56711)
store volatile %struct.ScmObj* %current_45args56712, %struct.ScmObj** %stackaddr$prim58526, align 8
%stackaddr$prim58527 = alloca %struct.ScmObj*, align 8
%anf_45bind48189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56712)
store volatile %struct.ScmObj* %anf_45bind48189, %struct.ScmObj** %stackaddr$prim58527, align 8
%ae48969 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58528 = alloca %struct.ScmObj*, align 8
%cpsprim48467 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae48969, %struct.ScmObj* %anf_45bind48189)
store volatile %struct.ScmObj* %cpsprim48467, %struct.ScmObj** %stackaddr$prim58528, align 8
%ae48972 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56714$k484650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58529 = alloca %struct.ScmObj*, align 8
%argslist56714$k484651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48467, %struct.ScmObj* %argslist56714$k484650)
store volatile %struct.ScmObj* %argslist56714$k484651, %struct.ScmObj** %stackaddr$prim58529, align 8
%stackaddr$prim58530 = alloca %struct.ScmObj*, align 8
%argslist56714$k484652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48972, %struct.ScmObj* %argslist56714$k484651)
store volatile %struct.ScmObj* %argslist56714$k484652, %struct.ScmObj** %stackaddr$prim58530, align 8
%clofunc58531 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48465)
musttail call tailcc void %clofunc58531(%struct.ScmObj* %k48465, %struct.ScmObj* %argslist56714$k484652)
ret void
}

define tailcc void @proc_clo$ae48800(%struct.ScmObj* %env$ae48800,%struct.ScmObj* %current_45args56719) {
%stackaddr$prim58532 = alloca %struct.ScmObj*, align 8
%k48468 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56719)
store volatile %struct.ScmObj* %k48468, %struct.ScmObj** %stackaddr$prim58532, align 8
%stackaddr$prim58533 = alloca %struct.ScmObj*, align 8
%current_45args56720 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56719)
store volatile %struct.ScmObj* %current_45args56720, %struct.ScmObj** %stackaddr$prim58533, align 8
%stackaddr$prim58534 = alloca %struct.ScmObj*, align 8
%_37take48045 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56720)
store volatile %struct.ScmObj* %_37take48045, %struct.ScmObj** %stackaddr$prim58534, align 8
%ae48802 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58535 = alloca %struct.ScmObj*, align 8
%fptrToInt58536 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48803 to i64
%ae48803 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58536)
store volatile %struct.ScmObj* %ae48803, %struct.ScmObj** %stackaddr$makeclosure58535, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48803, %struct.ScmObj* %_37take48045, i64 0)
%argslist56733$k484680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58537 = alloca %struct.ScmObj*, align 8
%argslist56733$k484681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48803, %struct.ScmObj* %argslist56733$k484680)
store volatile %struct.ScmObj* %argslist56733$k484681, %struct.ScmObj** %stackaddr$prim58537, align 8
%stackaddr$prim58538 = alloca %struct.ScmObj*, align 8
%argslist56733$k484682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48802, %struct.ScmObj* %argslist56733$k484681)
store volatile %struct.ScmObj* %argslist56733$k484682, %struct.ScmObj** %stackaddr$prim58538, align 8
%clofunc58539 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48468)
musttail call tailcc void %clofunc58539(%struct.ScmObj* %k48468, %struct.ScmObj* %argslist56733$k484682)
ret void
}

define tailcc void @proc_clo$ae48803(%struct.ScmObj* %env$ae48803,%struct.ScmObj* %current_45args56722) {
%stackaddr$env-ref58540 = alloca %struct.ScmObj*, align 8
%_37take48045 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48803, i64 0)
store %struct.ScmObj* %_37take48045, %struct.ScmObj** %stackaddr$env-ref58540
%stackaddr$prim58541 = alloca %struct.ScmObj*, align 8
%k48469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56722)
store volatile %struct.ScmObj* %k48469, %struct.ScmObj** %stackaddr$prim58541, align 8
%stackaddr$prim58542 = alloca %struct.ScmObj*, align 8
%current_45args56723 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56722)
store volatile %struct.ScmObj* %current_45args56723, %struct.ScmObj** %stackaddr$prim58542, align 8
%stackaddr$prim58543 = alloca %struct.ScmObj*, align 8
%lst48047 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56723)
store volatile %struct.ScmObj* %lst48047, %struct.ScmObj** %stackaddr$prim58543, align 8
%stackaddr$prim58544 = alloca %struct.ScmObj*, align 8
%current_45args56724 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56723)
store volatile %struct.ScmObj* %current_45args56724, %struct.ScmObj** %stackaddr$prim58544, align 8
%stackaddr$prim58545 = alloca %struct.ScmObj*, align 8
%n48046 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56724)
store volatile %struct.ScmObj* %n48046, %struct.ScmObj** %stackaddr$prim58545, align 8
%ae48805 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58546 = alloca %struct.ScmObj*, align 8
%anf_45bind48180 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n48046, %struct.ScmObj* %ae48805)
store volatile %struct.ScmObj* %anf_45bind48180, %struct.ScmObj** %stackaddr$prim58546, align 8
%truthy$cmp58547 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48180)
%cmp$cmp58547 = icmp eq i64 %truthy$cmp58547, 1
br i1 %cmp$cmp58547, label %truebranch$cmp58547, label %falsebranch$cmp58547
truebranch$cmp58547:
%ae48808 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48809 = call %struct.ScmObj* @const_init_null()
%argslist56726$k484690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58548 = alloca %struct.ScmObj*, align 8
%argslist56726$k484691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48809, %struct.ScmObj* %argslist56726$k484690)
store volatile %struct.ScmObj* %argslist56726$k484691, %struct.ScmObj** %stackaddr$prim58548, align 8
%stackaddr$prim58549 = alloca %struct.ScmObj*, align 8
%argslist56726$k484692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48808, %struct.ScmObj* %argslist56726$k484691)
store volatile %struct.ScmObj* %argslist56726$k484692, %struct.ScmObj** %stackaddr$prim58549, align 8
%clofunc58550 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48469)
musttail call tailcc void %clofunc58550(%struct.ScmObj* %k48469, %struct.ScmObj* %argslist56726$k484692)
ret void
falsebranch$cmp58547:
%stackaddr$prim58551 = alloca %struct.ScmObj*, align 8
%anf_45bind48181 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48047)
store volatile %struct.ScmObj* %anf_45bind48181, %struct.ScmObj** %stackaddr$prim58551, align 8
%truthy$cmp58552 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48181)
%cmp$cmp58552 = icmp eq i64 %truthy$cmp58552, 1
br i1 %cmp$cmp58552, label %truebranch$cmp58552, label %falsebranch$cmp58552
truebranch$cmp58552:
%ae48819 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48820 = call %struct.ScmObj* @const_init_null()
%argslist56727$k484690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58553 = alloca %struct.ScmObj*, align 8
%argslist56727$k484691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48820, %struct.ScmObj* %argslist56727$k484690)
store volatile %struct.ScmObj* %argslist56727$k484691, %struct.ScmObj** %stackaddr$prim58553, align 8
%stackaddr$prim58554 = alloca %struct.ScmObj*, align 8
%argslist56727$k484692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48819, %struct.ScmObj* %argslist56727$k484691)
store volatile %struct.ScmObj* %argslist56727$k484692, %struct.ScmObj** %stackaddr$prim58554, align 8
%clofunc58555 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48469)
musttail call tailcc void %clofunc58555(%struct.ScmObj* %k48469, %struct.ScmObj* %argslist56727$k484692)
ret void
falsebranch$cmp58552:
%stackaddr$prim58556 = alloca %struct.ScmObj*, align 8
%anf_45bind48182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48047)
store volatile %struct.ScmObj* %anf_45bind48182, %struct.ScmObj** %stackaddr$prim58556, align 8
%stackaddr$prim58557 = alloca %struct.ScmObj*, align 8
%anf_45bind48183 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48047)
store volatile %struct.ScmObj* %anf_45bind48183, %struct.ScmObj** %stackaddr$prim58557, align 8
%ae48830 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58558 = alloca %struct.ScmObj*, align 8
%anf_45bind48184 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n48046, %struct.ScmObj* %ae48830)
store volatile %struct.ScmObj* %anf_45bind48184, %struct.ScmObj** %stackaddr$prim58558, align 8
%stackaddr$makeclosure58559 = alloca %struct.ScmObj*, align 8
%fptrToInt58560 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48832 to i64
%ae48832 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58560)
store volatile %struct.ScmObj* %ae48832, %struct.ScmObj** %stackaddr$makeclosure58559, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48832, %struct.ScmObj* %anf_45bind48182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48832, %struct.ScmObj* %k48469, i64 1)
%argslist56732$_37take480450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58561 = alloca %struct.ScmObj*, align 8
%argslist56732$_37take480451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48184, %struct.ScmObj* %argslist56732$_37take480450)
store volatile %struct.ScmObj* %argslist56732$_37take480451, %struct.ScmObj** %stackaddr$prim58561, align 8
%stackaddr$prim58562 = alloca %struct.ScmObj*, align 8
%argslist56732$_37take480452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48183, %struct.ScmObj* %argslist56732$_37take480451)
store volatile %struct.ScmObj* %argslist56732$_37take480452, %struct.ScmObj** %stackaddr$prim58562, align 8
%stackaddr$prim58563 = alloca %struct.ScmObj*, align 8
%argslist56732$_37take480453 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48832, %struct.ScmObj* %argslist56732$_37take480452)
store volatile %struct.ScmObj* %argslist56732$_37take480453, %struct.ScmObj** %stackaddr$prim58563, align 8
%clofunc58564 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48045)
musttail call tailcc void %clofunc58564(%struct.ScmObj* %_37take48045, %struct.ScmObj* %argslist56732$_37take480453)
ret void
}

define tailcc void @proc_clo$ae48832(%struct.ScmObj* %env$ae48832,%struct.ScmObj* %current_45args56728) {
%stackaddr$env-ref58565 = alloca %struct.ScmObj*, align 8
%anf_45bind48182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48832, i64 0)
store %struct.ScmObj* %anf_45bind48182, %struct.ScmObj** %stackaddr$env-ref58565
%stackaddr$env-ref58566 = alloca %struct.ScmObj*, align 8
%k48469 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48832, i64 1)
store %struct.ScmObj* %k48469, %struct.ScmObj** %stackaddr$env-ref58566
%stackaddr$prim58567 = alloca %struct.ScmObj*, align 8
%_95k48470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56728)
store volatile %struct.ScmObj* %_95k48470, %struct.ScmObj** %stackaddr$prim58567, align 8
%stackaddr$prim58568 = alloca %struct.ScmObj*, align 8
%current_45args56729 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56728)
store volatile %struct.ScmObj* %current_45args56729, %struct.ScmObj** %stackaddr$prim58568, align 8
%stackaddr$prim58569 = alloca %struct.ScmObj*, align 8
%anf_45bind48185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56729)
store volatile %struct.ScmObj* %anf_45bind48185, %struct.ScmObj** %stackaddr$prim58569, align 8
%stackaddr$prim58570 = alloca %struct.ScmObj*, align 8
%cpsprim48471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48182, %struct.ScmObj* %anf_45bind48185)
store volatile %struct.ScmObj* %cpsprim48471, %struct.ScmObj** %stackaddr$prim58570, align 8
%ae48838 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56731$k484690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58571 = alloca %struct.ScmObj*, align 8
%argslist56731$k484691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48471, %struct.ScmObj* %argslist56731$k484690)
store volatile %struct.ScmObj* %argslist56731$k484691, %struct.ScmObj** %stackaddr$prim58571, align 8
%stackaddr$prim58572 = alloca %struct.ScmObj*, align 8
%argslist56731$k484692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48838, %struct.ScmObj* %argslist56731$k484691)
store volatile %struct.ScmObj* %argslist56731$k484692, %struct.ScmObj** %stackaddr$prim58572, align 8
%clofunc58573 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48469)
musttail call tailcc void %clofunc58573(%struct.ScmObj* %k48469, %struct.ScmObj* %argslist56731$k484692)
ret void
}

define tailcc void @proc_clo$ae48703(%struct.ScmObj* %env$ae48703,%struct.ScmObj* %current_45args56736) {
%stackaddr$prim58574 = alloca %struct.ScmObj*, align 8
%k48472 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56736)
store volatile %struct.ScmObj* %k48472, %struct.ScmObj** %stackaddr$prim58574, align 8
%stackaddr$prim58575 = alloca %struct.ScmObj*, align 8
%current_45args56737 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56736)
store volatile %struct.ScmObj* %current_45args56737, %struct.ScmObj** %stackaddr$prim58575, align 8
%stackaddr$prim58576 = alloca %struct.ScmObj*, align 8
%_37map48049 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56737)
store volatile %struct.ScmObj* %_37map48049, %struct.ScmObj** %stackaddr$prim58576, align 8
%ae48705 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58577 = alloca %struct.ScmObj*, align 8
%fptrToInt58578 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48706 to i64
%ae48706 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58578)
store volatile %struct.ScmObj* %ae48706, %struct.ScmObj** %stackaddr$makeclosure58577, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48706, %struct.ScmObj* %_37map48049, i64 0)
%argslist56753$k484720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58579 = alloca %struct.ScmObj*, align 8
%argslist56753$k484721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48706, %struct.ScmObj* %argslist56753$k484720)
store volatile %struct.ScmObj* %argslist56753$k484721, %struct.ScmObj** %stackaddr$prim58579, align 8
%stackaddr$prim58580 = alloca %struct.ScmObj*, align 8
%argslist56753$k484722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48705, %struct.ScmObj* %argslist56753$k484721)
store volatile %struct.ScmObj* %argslist56753$k484722, %struct.ScmObj** %stackaddr$prim58580, align 8
%clofunc58581 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48472)
musttail call tailcc void %clofunc58581(%struct.ScmObj* %k48472, %struct.ScmObj* %argslist56753$k484722)
ret void
}

define tailcc void @proc_clo$ae48706(%struct.ScmObj* %env$ae48706,%struct.ScmObj* %current_45args56739) {
%stackaddr$env-ref58582 = alloca %struct.ScmObj*, align 8
%_37map48049 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48706, i64 0)
store %struct.ScmObj* %_37map48049, %struct.ScmObj** %stackaddr$env-ref58582
%stackaddr$prim58583 = alloca %struct.ScmObj*, align 8
%k48473 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56739)
store volatile %struct.ScmObj* %k48473, %struct.ScmObj** %stackaddr$prim58583, align 8
%stackaddr$prim58584 = alloca %struct.ScmObj*, align 8
%current_45args56740 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56739)
store volatile %struct.ScmObj* %current_45args56740, %struct.ScmObj** %stackaddr$prim58584, align 8
%stackaddr$prim58585 = alloca %struct.ScmObj*, align 8
%f48051 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56740)
store volatile %struct.ScmObj* %f48051, %struct.ScmObj** %stackaddr$prim58585, align 8
%stackaddr$prim58586 = alloca %struct.ScmObj*, align 8
%current_45args56741 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56740)
store volatile %struct.ScmObj* %current_45args56741, %struct.ScmObj** %stackaddr$prim58586, align 8
%stackaddr$prim58587 = alloca %struct.ScmObj*, align 8
%lst48050 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56741)
store volatile %struct.ScmObj* %lst48050, %struct.ScmObj** %stackaddr$prim58587, align 8
%stackaddr$prim58588 = alloca %struct.ScmObj*, align 8
%anf_45bind48174 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48050)
store volatile %struct.ScmObj* %anf_45bind48174, %struct.ScmObj** %stackaddr$prim58588, align 8
%truthy$cmp58589 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48174)
%cmp$cmp58589 = icmp eq i64 %truthy$cmp58589, 1
br i1 %cmp$cmp58589, label %truebranch$cmp58589, label %falsebranch$cmp58589
truebranch$cmp58589:
%ae48710 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48711 = call %struct.ScmObj* @const_init_null()
%argslist56743$k484730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58590 = alloca %struct.ScmObj*, align 8
%argslist56743$k484731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48711, %struct.ScmObj* %argslist56743$k484730)
store volatile %struct.ScmObj* %argslist56743$k484731, %struct.ScmObj** %stackaddr$prim58590, align 8
%stackaddr$prim58591 = alloca %struct.ScmObj*, align 8
%argslist56743$k484732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48710, %struct.ScmObj* %argslist56743$k484731)
store volatile %struct.ScmObj* %argslist56743$k484732, %struct.ScmObj** %stackaddr$prim58591, align 8
%clofunc58592 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48473)
musttail call tailcc void %clofunc58592(%struct.ScmObj* %k48473, %struct.ScmObj* %argslist56743$k484732)
ret void
falsebranch$cmp58589:
%stackaddr$prim58593 = alloca %struct.ScmObj*, align 8
%anf_45bind48175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48050)
store volatile %struct.ScmObj* %anf_45bind48175, %struct.ScmObj** %stackaddr$prim58593, align 8
%stackaddr$makeclosure58594 = alloca %struct.ScmObj*, align 8
%fptrToInt58595 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48720 to i64
%ae48720 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58595)
store volatile %struct.ScmObj* %ae48720, %struct.ScmObj** %stackaddr$makeclosure58594, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48720, %struct.ScmObj* %k48473, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48720, %struct.ScmObj* %f48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48720, %struct.ScmObj* %lst48050, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48720, %struct.ScmObj* %_37map48049, i64 3)
%argslist56752$f480510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58596 = alloca %struct.ScmObj*, align 8
%argslist56752$f480511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48175, %struct.ScmObj* %argslist56752$f480510)
store volatile %struct.ScmObj* %argslist56752$f480511, %struct.ScmObj** %stackaddr$prim58596, align 8
%stackaddr$prim58597 = alloca %struct.ScmObj*, align 8
%argslist56752$f480512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48720, %struct.ScmObj* %argslist56752$f480511)
store volatile %struct.ScmObj* %argslist56752$f480512, %struct.ScmObj** %stackaddr$prim58597, align 8
%clofunc58598 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48051)
musttail call tailcc void %clofunc58598(%struct.ScmObj* %f48051, %struct.ScmObj* %argslist56752$f480512)
ret void
}

define tailcc void @proc_clo$ae48720(%struct.ScmObj* %env$ae48720,%struct.ScmObj* %current_45args56744) {
%stackaddr$env-ref58599 = alloca %struct.ScmObj*, align 8
%k48473 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48720, i64 0)
store %struct.ScmObj* %k48473, %struct.ScmObj** %stackaddr$env-ref58599
%stackaddr$env-ref58600 = alloca %struct.ScmObj*, align 8
%f48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48720, i64 1)
store %struct.ScmObj* %f48051, %struct.ScmObj** %stackaddr$env-ref58600
%stackaddr$env-ref58601 = alloca %struct.ScmObj*, align 8
%lst48050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48720, i64 2)
store %struct.ScmObj* %lst48050, %struct.ScmObj** %stackaddr$env-ref58601
%stackaddr$env-ref58602 = alloca %struct.ScmObj*, align 8
%_37map48049 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48720, i64 3)
store %struct.ScmObj* %_37map48049, %struct.ScmObj** %stackaddr$env-ref58602
%stackaddr$prim58603 = alloca %struct.ScmObj*, align 8
%_95k48474 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56744)
store volatile %struct.ScmObj* %_95k48474, %struct.ScmObj** %stackaddr$prim58603, align 8
%stackaddr$prim58604 = alloca %struct.ScmObj*, align 8
%current_45args56745 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56744)
store volatile %struct.ScmObj* %current_45args56745, %struct.ScmObj** %stackaddr$prim58604, align 8
%stackaddr$prim58605 = alloca %struct.ScmObj*, align 8
%anf_45bind48176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56745)
store volatile %struct.ScmObj* %anf_45bind48176, %struct.ScmObj** %stackaddr$prim58605, align 8
%stackaddr$prim58606 = alloca %struct.ScmObj*, align 8
%anf_45bind48177 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48050)
store volatile %struct.ScmObj* %anf_45bind48177, %struct.ScmObj** %stackaddr$prim58606, align 8
%stackaddr$makeclosure58607 = alloca %struct.ScmObj*, align 8
%fptrToInt58608 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48724 to i64
%ae48724 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58608)
store volatile %struct.ScmObj* %ae48724, %struct.ScmObj** %stackaddr$makeclosure58607, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48724, %struct.ScmObj* %k48473, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48724, %struct.ScmObj* %anf_45bind48176, i64 1)
%argslist56751$_37map480490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58609 = alloca %struct.ScmObj*, align 8
%argslist56751$_37map480491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48177, %struct.ScmObj* %argslist56751$_37map480490)
store volatile %struct.ScmObj* %argslist56751$_37map480491, %struct.ScmObj** %stackaddr$prim58609, align 8
%stackaddr$prim58610 = alloca %struct.ScmObj*, align 8
%argslist56751$_37map480492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48051, %struct.ScmObj* %argslist56751$_37map480491)
store volatile %struct.ScmObj* %argslist56751$_37map480492, %struct.ScmObj** %stackaddr$prim58610, align 8
%stackaddr$prim58611 = alloca %struct.ScmObj*, align 8
%argslist56751$_37map480493 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48724, %struct.ScmObj* %argslist56751$_37map480492)
store volatile %struct.ScmObj* %argslist56751$_37map480493, %struct.ScmObj** %stackaddr$prim58611, align 8
%clofunc58612 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map48049)
musttail call tailcc void %clofunc58612(%struct.ScmObj* %_37map48049, %struct.ScmObj* %argslist56751$_37map480493)
ret void
}

define tailcc void @proc_clo$ae48724(%struct.ScmObj* %env$ae48724,%struct.ScmObj* %current_45args56747) {
%stackaddr$env-ref58613 = alloca %struct.ScmObj*, align 8
%k48473 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48724, i64 0)
store %struct.ScmObj* %k48473, %struct.ScmObj** %stackaddr$env-ref58613
%stackaddr$env-ref58614 = alloca %struct.ScmObj*, align 8
%anf_45bind48176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48724, i64 1)
store %struct.ScmObj* %anf_45bind48176, %struct.ScmObj** %stackaddr$env-ref58614
%stackaddr$prim58615 = alloca %struct.ScmObj*, align 8
%_95k48475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56747)
store volatile %struct.ScmObj* %_95k48475, %struct.ScmObj** %stackaddr$prim58615, align 8
%stackaddr$prim58616 = alloca %struct.ScmObj*, align 8
%current_45args56748 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56747)
store volatile %struct.ScmObj* %current_45args56748, %struct.ScmObj** %stackaddr$prim58616, align 8
%stackaddr$prim58617 = alloca %struct.ScmObj*, align 8
%anf_45bind48178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56748)
store volatile %struct.ScmObj* %anf_45bind48178, %struct.ScmObj** %stackaddr$prim58617, align 8
%stackaddr$prim58618 = alloca %struct.ScmObj*, align 8
%cpsprim48476 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48176, %struct.ScmObj* %anf_45bind48178)
store volatile %struct.ScmObj* %cpsprim48476, %struct.ScmObj** %stackaddr$prim58618, align 8
%ae48730 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56750$k484730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58619 = alloca %struct.ScmObj*, align 8
%argslist56750$k484731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48476, %struct.ScmObj* %argslist56750$k484730)
store volatile %struct.ScmObj* %argslist56750$k484731, %struct.ScmObj** %stackaddr$prim58619, align 8
%stackaddr$prim58620 = alloca %struct.ScmObj*, align 8
%argslist56750$k484732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48730, %struct.ScmObj* %argslist56750$k484731)
store volatile %struct.ScmObj* %argslist56750$k484732, %struct.ScmObj** %stackaddr$prim58620, align 8
%clofunc58621 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48473)
musttail call tailcc void %clofunc58621(%struct.ScmObj* %k48473, %struct.ScmObj* %argslist56750$k484732)
ret void
}

define tailcc void @proc_clo$ae48623(%struct.ScmObj* %env$ae48623,%struct.ScmObj* %current_45args56756) {
%stackaddr$prim58622 = alloca %struct.ScmObj*, align 8
%k48477 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56756)
store volatile %struct.ScmObj* %k48477, %struct.ScmObj** %stackaddr$prim58622, align 8
%stackaddr$prim58623 = alloca %struct.ScmObj*, align 8
%current_45args56757 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56756)
store volatile %struct.ScmObj* %current_45args56757, %struct.ScmObj** %stackaddr$prim58623, align 8
%stackaddr$prim58624 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56757)
store volatile %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$prim58624, align 8
%ae48625 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58625 = alloca %struct.ScmObj*, align 8
%fptrToInt58626 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48626 to i64
%ae48626 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58626)
store volatile %struct.ScmObj* %ae48626, %struct.ScmObj** %stackaddr$makeclosure58625, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48626, %struct.ScmObj* %_37foldr148053, i64 0)
%argslist56770$k484770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58627 = alloca %struct.ScmObj*, align 8
%argslist56770$k484771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48626, %struct.ScmObj* %argslist56770$k484770)
store volatile %struct.ScmObj* %argslist56770$k484771, %struct.ScmObj** %stackaddr$prim58627, align 8
%stackaddr$prim58628 = alloca %struct.ScmObj*, align 8
%argslist56770$k484772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48625, %struct.ScmObj* %argslist56770$k484771)
store volatile %struct.ScmObj* %argslist56770$k484772, %struct.ScmObj** %stackaddr$prim58628, align 8
%clofunc58629 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48477)
musttail call tailcc void %clofunc58629(%struct.ScmObj* %k48477, %struct.ScmObj* %argslist56770$k484772)
ret void
}

define tailcc void @proc_clo$ae48626(%struct.ScmObj* %env$ae48626,%struct.ScmObj* %current_45args56759) {
%stackaddr$env-ref58630 = alloca %struct.ScmObj*, align 8
%_37foldr148053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48626, i64 0)
store %struct.ScmObj* %_37foldr148053, %struct.ScmObj** %stackaddr$env-ref58630
%stackaddr$prim58631 = alloca %struct.ScmObj*, align 8
%k48478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56759)
store volatile %struct.ScmObj* %k48478, %struct.ScmObj** %stackaddr$prim58631, align 8
%stackaddr$prim58632 = alloca %struct.ScmObj*, align 8
%current_45args56760 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56759)
store volatile %struct.ScmObj* %current_45args56760, %struct.ScmObj** %stackaddr$prim58632, align 8
%stackaddr$prim58633 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56760)
store volatile %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$prim58633, align 8
%stackaddr$prim58634 = alloca %struct.ScmObj*, align 8
%current_45args56761 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56760)
store volatile %struct.ScmObj* %current_45args56761, %struct.ScmObj** %stackaddr$prim58634, align 8
%stackaddr$prim58635 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56761)
store volatile %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$prim58635, align 8
%stackaddr$prim58636 = alloca %struct.ScmObj*, align 8
%current_45args56762 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56761)
store volatile %struct.ScmObj* %current_45args56762, %struct.ScmObj** %stackaddr$prim58636, align 8
%stackaddr$prim58637 = alloca %struct.ScmObj*, align 8
%lst48054 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56762)
store volatile %struct.ScmObj* %lst48054, %struct.ScmObj** %stackaddr$prim58637, align 8
%stackaddr$prim58638 = alloca %struct.ScmObj*, align 8
%anf_45bind48169 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48054)
store volatile %struct.ScmObj* %anf_45bind48169, %struct.ScmObj** %stackaddr$prim58638, align 8
%truthy$cmp58639 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48169)
%cmp$cmp58639 = icmp eq i64 %truthy$cmp58639, 1
br i1 %cmp$cmp58639, label %truebranch$cmp58639, label %falsebranch$cmp58639
truebranch$cmp58639:
%ae48630 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56764$k484780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58640 = alloca %struct.ScmObj*, align 8
%argslist56764$k484781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48055, %struct.ScmObj* %argslist56764$k484780)
store volatile %struct.ScmObj* %argslist56764$k484781, %struct.ScmObj** %stackaddr$prim58640, align 8
%stackaddr$prim58641 = alloca %struct.ScmObj*, align 8
%argslist56764$k484782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48630, %struct.ScmObj* %argslist56764$k484781)
store volatile %struct.ScmObj* %argslist56764$k484782, %struct.ScmObj** %stackaddr$prim58641, align 8
%clofunc58642 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48478)
musttail call tailcc void %clofunc58642(%struct.ScmObj* %k48478, %struct.ScmObj* %argslist56764$k484782)
ret void
falsebranch$cmp58639:
%stackaddr$prim58643 = alloca %struct.ScmObj*, align 8
%anf_45bind48170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48054)
store volatile %struct.ScmObj* %anf_45bind48170, %struct.ScmObj** %stackaddr$prim58643, align 8
%stackaddr$prim58644 = alloca %struct.ScmObj*, align 8
%anf_45bind48171 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48054)
store volatile %struct.ScmObj* %anf_45bind48171, %struct.ScmObj** %stackaddr$prim58644, align 8
%stackaddr$makeclosure58645 = alloca %struct.ScmObj*, align 8
%fptrToInt58646 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48638 to i64
%ae48638 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58646)
store volatile %struct.ScmObj* %ae48638, %struct.ScmObj** %stackaddr$makeclosure58645, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48638, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48638, %struct.ScmObj* %k48478, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48638, %struct.ScmObj* %anf_45bind48170, i64 2)
%argslist56769$_37foldr1480530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58647 = alloca %struct.ScmObj*, align 8
%argslist56769$_37foldr1480531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48171, %struct.ScmObj* %argslist56769$_37foldr1480530)
store volatile %struct.ScmObj* %argslist56769$_37foldr1480531, %struct.ScmObj** %stackaddr$prim58647, align 8
%stackaddr$prim58648 = alloca %struct.ScmObj*, align 8
%argslist56769$_37foldr1480532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48055, %struct.ScmObj* %argslist56769$_37foldr1480531)
store volatile %struct.ScmObj* %argslist56769$_37foldr1480532, %struct.ScmObj** %stackaddr$prim58648, align 8
%stackaddr$prim58649 = alloca %struct.ScmObj*, align 8
%argslist56769$_37foldr1480533 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48056, %struct.ScmObj* %argslist56769$_37foldr1480532)
store volatile %struct.ScmObj* %argslist56769$_37foldr1480533, %struct.ScmObj** %stackaddr$prim58649, align 8
%stackaddr$prim58650 = alloca %struct.ScmObj*, align 8
%argslist56769$_37foldr1480534 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48638, %struct.ScmObj* %argslist56769$_37foldr1480533)
store volatile %struct.ScmObj* %argslist56769$_37foldr1480534, %struct.ScmObj** %stackaddr$prim58650, align 8
%clofunc58651 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148053)
musttail call tailcc void %clofunc58651(%struct.ScmObj* %_37foldr148053, %struct.ScmObj* %argslist56769$_37foldr1480534)
ret void
}

define tailcc void @proc_clo$ae48638(%struct.ScmObj* %env$ae48638,%struct.ScmObj* %current_45args56765) {
%stackaddr$env-ref58652 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48638, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref58652
%stackaddr$env-ref58653 = alloca %struct.ScmObj*, align 8
%k48478 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48638, i64 1)
store %struct.ScmObj* %k48478, %struct.ScmObj** %stackaddr$env-ref58653
%stackaddr$env-ref58654 = alloca %struct.ScmObj*, align 8
%anf_45bind48170 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48638, i64 2)
store %struct.ScmObj* %anf_45bind48170, %struct.ScmObj** %stackaddr$env-ref58654
%stackaddr$prim58655 = alloca %struct.ScmObj*, align 8
%_95k48479 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56765)
store volatile %struct.ScmObj* %_95k48479, %struct.ScmObj** %stackaddr$prim58655, align 8
%stackaddr$prim58656 = alloca %struct.ScmObj*, align 8
%current_45args56766 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56765)
store volatile %struct.ScmObj* %current_45args56766, %struct.ScmObj** %stackaddr$prim58656, align 8
%stackaddr$prim58657 = alloca %struct.ScmObj*, align 8
%anf_45bind48172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56766)
store volatile %struct.ScmObj* %anf_45bind48172, %struct.ScmObj** %stackaddr$prim58657, align 8
%argslist56768$f480560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58658 = alloca %struct.ScmObj*, align 8
%argslist56768$f480561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48172, %struct.ScmObj* %argslist56768$f480560)
store volatile %struct.ScmObj* %argslist56768$f480561, %struct.ScmObj** %stackaddr$prim58658, align 8
%stackaddr$prim58659 = alloca %struct.ScmObj*, align 8
%argslist56768$f480562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48170, %struct.ScmObj* %argslist56768$f480561)
store volatile %struct.ScmObj* %argslist56768$f480562, %struct.ScmObj** %stackaddr$prim58659, align 8
%stackaddr$prim58660 = alloca %struct.ScmObj*, align 8
%argslist56768$f480563 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48478, %struct.ScmObj* %argslist56768$f480562)
store volatile %struct.ScmObj* %argslist56768$f480563, %struct.ScmObj** %stackaddr$prim58660, align 8
%clofunc58661 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48056)
musttail call tailcc void %clofunc58661(%struct.ScmObj* %f48056, %struct.ScmObj* %argslist56768$f480563)
ret void
}

define tailcc void @proc_clo$ae48506(%struct.ScmObj* %env$ae48506,%struct.ScmObj* %current_45args56773) {
%stackaddr$prim58662 = alloca %struct.ScmObj*, align 8
%k48480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56773)
store volatile %struct.ScmObj* %k48480, %struct.ScmObj** %stackaddr$prim58662, align 8
%stackaddr$prim58663 = alloca %struct.ScmObj*, align 8
%current_45args56774 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56773)
store volatile %struct.ScmObj* %current_45args56774, %struct.ScmObj** %stackaddr$prim58663, align 8
%stackaddr$prim58664 = alloca %struct.ScmObj*, align 8
%y48033 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56774)
store volatile %struct.ScmObj* %y48033, %struct.ScmObj** %stackaddr$prim58664, align 8
%ae48508 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58665 = alloca %struct.ScmObj*, align 8
%fptrToInt58666 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48509 to i64
%ae48509 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58666)
store volatile %struct.ScmObj* %ae48509, %struct.ScmObj** %stackaddr$makeclosure58665, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48509, %struct.ScmObj* %y48033, i64 0)
%argslist56792$k484800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58667 = alloca %struct.ScmObj*, align 8
%argslist56792$k484801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48509, %struct.ScmObj* %argslist56792$k484800)
store volatile %struct.ScmObj* %argslist56792$k484801, %struct.ScmObj** %stackaddr$prim58667, align 8
%stackaddr$prim58668 = alloca %struct.ScmObj*, align 8
%argslist56792$k484802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48508, %struct.ScmObj* %argslist56792$k484801)
store volatile %struct.ScmObj* %argslist56792$k484802, %struct.ScmObj** %stackaddr$prim58668, align 8
%clofunc58669 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48480)
musttail call tailcc void %clofunc58669(%struct.ScmObj* %k48480, %struct.ScmObj* %argslist56792$k484802)
ret void
}

define tailcc void @proc_clo$ae48509(%struct.ScmObj* %env$ae48509,%struct.ScmObj* %current_45args56776) {
%stackaddr$env-ref58670 = alloca %struct.ScmObj*, align 8
%y48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48509, i64 0)
store %struct.ScmObj* %y48033, %struct.ScmObj** %stackaddr$env-ref58670
%stackaddr$prim58671 = alloca %struct.ScmObj*, align 8
%k48481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56776)
store volatile %struct.ScmObj* %k48481, %struct.ScmObj** %stackaddr$prim58671, align 8
%stackaddr$prim58672 = alloca %struct.ScmObj*, align 8
%current_45args56777 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56776)
store volatile %struct.ScmObj* %current_45args56777, %struct.ScmObj** %stackaddr$prim58672, align 8
%stackaddr$prim58673 = alloca %struct.ScmObj*, align 8
%f48034 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56777)
store volatile %struct.ScmObj* %f48034, %struct.ScmObj** %stackaddr$prim58673, align 8
%stackaddr$makeclosure58674 = alloca %struct.ScmObj*, align 8
%fptrToInt58675 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48510 to i64
%ae48510 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58675)
store volatile %struct.ScmObj* %ae48510, %struct.ScmObj** %stackaddr$makeclosure58674, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48510, %struct.ScmObj* %f48034, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48510, %struct.ScmObj* %k48481, i64 1)
%ae48511 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58676 = alloca %struct.ScmObj*, align 8
%fptrToInt58677 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48512 to i64
%ae48512 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58677)
store volatile %struct.ScmObj* %ae48512, %struct.ScmObj** %stackaddr$makeclosure58676, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48512, %struct.ScmObj* %f48034, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48512, %struct.ScmObj* %y48033, i64 1)
%argslist56791$ae485100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58678 = alloca %struct.ScmObj*, align 8
%argslist56791$ae485101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48512, %struct.ScmObj* %argslist56791$ae485100)
store volatile %struct.ScmObj* %argslist56791$ae485101, %struct.ScmObj** %stackaddr$prim58678, align 8
%stackaddr$prim58679 = alloca %struct.ScmObj*, align 8
%argslist56791$ae485102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48511, %struct.ScmObj* %argslist56791$ae485101)
store volatile %struct.ScmObj* %argslist56791$ae485102, %struct.ScmObj** %stackaddr$prim58679, align 8
%clofunc58680 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48510)
musttail call tailcc void %clofunc58680(%struct.ScmObj* %ae48510, %struct.ScmObj* %argslist56791$ae485102)
ret void
}

define tailcc void @proc_clo$ae48510(%struct.ScmObj* %env$ae48510,%struct.ScmObj* %current_45args56779) {
%stackaddr$env-ref58681 = alloca %struct.ScmObj*, align 8
%f48034 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48510, i64 0)
store %struct.ScmObj* %f48034, %struct.ScmObj** %stackaddr$env-ref58681
%stackaddr$env-ref58682 = alloca %struct.ScmObj*, align 8
%k48481 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48510, i64 1)
store %struct.ScmObj* %k48481, %struct.ScmObj** %stackaddr$env-ref58682
%stackaddr$prim58683 = alloca %struct.ScmObj*, align 8
%_95k48482 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56779)
store volatile %struct.ScmObj* %_95k48482, %struct.ScmObj** %stackaddr$prim58683, align 8
%stackaddr$prim58684 = alloca %struct.ScmObj*, align 8
%current_45args56780 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56779)
store volatile %struct.ScmObj* %current_45args56780, %struct.ScmObj** %stackaddr$prim58684, align 8
%stackaddr$prim58685 = alloca %struct.ScmObj*, align 8
%anf_45bind48167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56780)
store volatile %struct.ScmObj* %anf_45bind48167, %struct.ScmObj** %stackaddr$prim58685, align 8
%argslist56782$f480340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58686 = alloca %struct.ScmObj*, align 8
%argslist56782$f480341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48167, %struct.ScmObj* %argslist56782$f480340)
store volatile %struct.ScmObj* %argslist56782$f480341, %struct.ScmObj** %stackaddr$prim58686, align 8
%stackaddr$prim58687 = alloca %struct.ScmObj*, align 8
%argslist56782$f480342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48481, %struct.ScmObj* %argslist56782$f480341)
store volatile %struct.ScmObj* %argslist56782$f480342, %struct.ScmObj** %stackaddr$prim58687, align 8
%clofunc58688 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48034)
musttail call tailcc void %clofunc58688(%struct.ScmObj* %f48034, %struct.ScmObj* %argslist56782$f480342)
ret void
}

define tailcc void @proc_clo$ae48512(%struct.ScmObj* %env$ae48512,%struct.ScmObj* %args4803548483) {
%stackaddr$env-ref58689 = alloca %struct.ScmObj*, align 8
%f48034 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48512, i64 0)
store %struct.ScmObj* %f48034, %struct.ScmObj** %stackaddr$env-ref58689
%stackaddr$env-ref58690 = alloca %struct.ScmObj*, align 8
%y48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48512, i64 1)
store %struct.ScmObj* %y48033, %struct.ScmObj** %stackaddr$env-ref58690
%stackaddr$prim58691 = alloca %struct.ScmObj*, align 8
%k48484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4803548483)
store volatile %struct.ScmObj* %k48484, %struct.ScmObj** %stackaddr$prim58691, align 8
%stackaddr$prim58692 = alloca %struct.ScmObj*, align 8
%args48035 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4803548483)
store volatile %struct.ScmObj* %args48035, %struct.ScmObj** %stackaddr$prim58692, align 8
%stackaddr$makeclosure58693 = alloca %struct.ScmObj*, align 8
%fptrToInt58694 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48516 to i64
%ae48516 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58694)
store volatile %struct.ScmObj* %ae48516, %struct.ScmObj** %stackaddr$makeclosure58693, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48516, %struct.ScmObj* %k48484, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48516, %struct.ScmObj* %args48035, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48516, %struct.ScmObj* %f48034, i64 2)
%argslist56790$y480330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58695 = alloca %struct.ScmObj*, align 8
%argslist56790$y480331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y48033, %struct.ScmObj* %argslist56790$y480330)
store volatile %struct.ScmObj* %argslist56790$y480331, %struct.ScmObj** %stackaddr$prim58695, align 8
%stackaddr$prim58696 = alloca %struct.ScmObj*, align 8
%argslist56790$y480332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48516, %struct.ScmObj* %argslist56790$y480331)
store volatile %struct.ScmObj* %argslist56790$y480332, %struct.ScmObj** %stackaddr$prim58696, align 8
%clofunc58697 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y48033)
musttail call tailcc void %clofunc58697(%struct.ScmObj* %y48033, %struct.ScmObj* %argslist56790$y480332)
ret void
}

define tailcc void @proc_clo$ae48516(%struct.ScmObj* %env$ae48516,%struct.ScmObj* %current_45args56783) {
%stackaddr$env-ref58698 = alloca %struct.ScmObj*, align 8
%k48484 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48516, i64 0)
store %struct.ScmObj* %k48484, %struct.ScmObj** %stackaddr$env-ref58698
%stackaddr$env-ref58699 = alloca %struct.ScmObj*, align 8
%args48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48516, i64 1)
store %struct.ScmObj* %args48035, %struct.ScmObj** %stackaddr$env-ref58699
%stackaddr$env-ref58700 = alloca %struct.ScmObj*, align 8
%f48034 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48516, i64 2)
store %struct.ScmObj* %f48034, %struct.ScmObj** %stackaddr$env-ref58700
%stackaddr$prim58701 = alloca %struct.ScmObj*, align 8
%_95k48485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56783)
store volatile %struct.ScmObj* %_95k48485, %struct.ScmObj** %stackaddr$prim58701, align 8
%stackaddr$prim58702 = alloca %struct.ScmObj*, align 8
%current_45args56784 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56783)
store volatile %struct.ScmObj* %current_45args56784, %struct.ScmObj** %stackaddr$prim58702, align 8
%stackaddr$prim58703 = alloca %struct.ScmObj*, align 8
%anf_45bind48165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56784)
store volatile %struct.ScmObj* %anf_45bind48165, %struct.ScmObj** %stackaddr$prim58703, align 8
%stackaddr$makeclosure58704 = alloca %struct.ScmObj*, align 8
%fptrToInt58705 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48519 to i64
%ae48519 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58705)
store volatile %struct.ScmObj* %ae48519, %struct.ScmObj** %stackaddr$makeclosure58704, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48519, %struct.ScmObj* %k48484, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48519, %struct.ScmObj* %args48035, i64 1)
%argslist56789$anf_45bind481650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58706 = alloca %struct.ScmObj*, align 8
%argslist56789$anf_45bind481651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48034, %struct.ScmObj* %argslist56789$anf_45bind481650)
store volatile %struct.ScmObj* %argslist56789$anf_45bind481651, %struct.ScmObj** %stackaddr$prim58706, align 8
%stackaddr$prim58707 = alloca %struct.ScmObj*, align 8
%argslist56789$anf_45bind481652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48519, %struct.ScmObj* %argslist56789$anf_45bind481651)
store volatile %struct.ScmObj* %argslist56789$anf_45bind481652, %struct.ScmObj** %stackaddr$prim58707, align 8
%clofunc58708 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48165)
musttail call tailcc void %clofunc58708(%struct.ScmObj* %anf_45bind48165, %struct.ScmObj* %argslist56789$anf_45bind481652)
ret void
}

define tailcc void @proc_clo$ae48519(%struct.ScmObj* %env$ae48519,%struct.ScmObj* %current_45args56786) {
%stackaddr$env-ref58709 = alloca %struct.ScmObj*, align 8
%k48484 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48519, i64 0)
store %struct.ScmObj* %k48484, %struct.ScmObj** %stackaddr$env-ref58709
%stackaddr$env-ref58710 = alloca %struct.ScmObj*, align 8
%args48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48519, i64 1)
store %struct.ScmObj* %args48035, %struct.ScmObj** %stackaddr$env-ref58710
%stackaddr$prim58711 = alloca %struct.ScmObj*, align 8
%_95k48486 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56786)
store volatile %struct.ScmObj* %_95k48486, %struct.ScmObj** %stackaddr$prim58711, align 8
%stackaddr$prim58712 = alloca %struct.ScmObj*, align 8
%current_45args56787 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56786)
store volatile %struct.ScmObj* %current_45args56787, %struct.ScmObj** %stackaddr$prim58712, align 8
%stackaddr$prim58713 = alloca %struct.ScmObj*, align 8
%anf_45bind48166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56787)
store volatile %struct.ScmObj* %anf_45bind48166, %struct.ScmObj** %stackaddr$prim58713, align 8
%stackaddr$prim58714 = alloca %struct.ScmObj*, align 8
%cpsargs48487 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48484, %struct.ScmObj* %args48035)
store volatile %struct.ScmObj* %cpsargs48487, %struct.ScmObj** %stackaddr$prim58714, align 8
%clofunc58715 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48166)
musttail call tailcc void %clofunc58715(%struct.ScmObj* %anf_45bind48166, %struct.ScmObj* %cpsargs48487)
ret void
}

define tailcc void @proc_clo$ae48491(%struct.ScmObj* %env$ae48491,%struct.ScmObj* %current_45args56794) {
%stackaddr$prim58716 = alloca %struct.ScmObj*, align 8
%k48488 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56794)
store volatile %struct.ScmObj* %k48488, %struct.ScmObj** %stackaddr$prim58716, align 8
%stackaddr$prim58717 = alloca %struct.ScmObj*, align 8
%current_45args56795 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56794)
store volatile %struct.ScmObj* %current_45args56795, %struct.ScmObj** %stackaddr$prim58717, align 8
%stackaddr$prim58718 = alloca %struct.ScmObj*, align 8
%yu48032 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56795)
store volatile %struct.ScmObj* %yu48032, %struct.ScmObj** %stackaddr$prim58718, align 8
%argslist56797$yu480320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58719 = alloca %struct.ScmObj*, align 8
%argslist56797$yu480321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu48032, %struct.ScmObj* %argslist56797$yu480320)
store volatile %struct.ScmObj* %argslist56797$yu480321, %struct.ScmObj** %stackaddr$prim58719, align 8
%stackaddr$prim58720 = alloca %struct.ScmObj*, align 8
%argslist56797$yu480322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48488, %struct.ScmObj* %argslist56797$yu480321)
store volatile %struct.ScmObj* %argslist56797$yu480322, %struct.ScmObj** %stackaddr$prim58720, align 8
%clofunc58721 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu48032)
musttail call tailcc void %clofunc58721(%struct.ScmObj* %yu48032, %struct.ScmObj* %argslist56797$yu480322)
ret void
}