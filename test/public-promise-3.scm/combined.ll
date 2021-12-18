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

@global$sym$ae4405449476 = private unnamed_addr constant [8 x i8] c"promise\00", align 8
@global$sym$ae4434049509 = private unnamed_addr constant [4 x i8] c"get\00", align 8
@global$str$ae4518249522 = private unnamed_addr constant [12 x i8] c"not promise\00", align 8
@global$sym$ae4411849552 = private unnamed_addr constant [4 x i8] c"get\00", align 8
@global$str$ae4426149565 = private unnamed_addr constant [12 x i8] c"not promise\00", align 8
@global$sym$ae4439149642 = private unnamed_addr constant [4 x i8] c"get\00", align 8
@global$str$ae4453449655 = private unnamed_addr constant [12 x i8] c"not promise\00", align 8
@global$sym$ae4501849720 = private unnamed_addr constant [4 x i8] c"get\00", align 8
@global$str$ae4516149733 = private unnamed_addr constant [12 x i8] c"not promise\00", align 8
@global$sym$ae4522549798 = private unnamed_addr constant [4 x i8] c"get\00", align 8
@global$str$ae4536849811 = private unnamed_addr constant [12 x i8] c"not promise\00", align 8
@global$sym$ae4393249871 = private unnamed_addr constant [8 x i8] c"promise\00", align 8

define ccc i32 @main() {
%mainenv49059 = call %struct.ScmObj* @const_init_null()
%mainargs49060 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv49059, %struct.ScmObj* %mainargs49060)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv49057,%struct.ScmObj* %mainargs49058) {
%stackaddr$makeclosure49061 = alloca %struct.ScmObj*, align 8
%fptrToInt49062 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40544 to i64
%ae40544 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49062)
store volatile %struct.ScmObj* %ae40544, %struct.ScmObj** %stackaddr$makeclosure49061, align 8
%ae40545 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49063 = alloca %struct.ScmObj*, align 8
%fptrToInt49064 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40546 to i64
%ae40546 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49064)
store volatile %struct.ScmObj* %ae40546, %struct.ScmObj** %stackaddr$makeclosure49063, align 8
%args49056$ae40544$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49065 = alloca %struct.ScmObj*, align 8
%args49056$ae40544$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40546, %struct.ScmObj* %args49056$ae40544$0)
store volatile %struct.ScmObj* %args49056$ae40544$1, %struct.ScmObj** %stackaddr$prim49065, align 8
%stackaddr$prim49066 = alloca %struct.ScmObj*, align 8
%args49056$ae40544$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40545, %struct.ScmObj* %args49056$ae40544$1)
store volatile %struct.ScmObj* %args49056$ae40544$2, %struct.ScmObj** %stackaddr$prim49066, align 8
%clofunc49067 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40544)
musttail call tailcc void %clofunc49067(%struct.ScmObj* %ae40544, %struct.ScmObj* %args49056$ae40544$2)
ret void
}

define tailcc void @proc_clo$ae40544(%struct.ScmObj* %env$ae40544,%struct.ScmObj* %current_45args48372) {
%stackaddr$prim49068 = alloca %struct.ScmObj*, align 8
%_95k40365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48372)
store volatile %struct.ScmObj* %_95k40365, %struct.ScmObj** %stackaddr$prim49068, align 8
%stackaddr$prim49069 = alloca %struct.ScmObj*, align 8
%current_45args48373 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48372)
store volatile %struct.ScmObj* %current_45args48373, %struct.ScmObj** %stackaddr$prim49069, align 8
%stackaddr$prim49070 = alloca %struct.ScmObj*, align 8
%anf_45bind40240 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48373)
store volatile %struct.ScmObj* %anf_45bind40240, %struct.ScmObj** %stackaddr$prim49070, align 8
%stackaddr$makeclosure49071 = alloca %struct.ScmObj*, align 8
%fptrToInt49072 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40559 to i64
%ae40559 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49072)
store volatile %struct.ScmObj* %ae40559, %struct.ScmObj** %stackaddr$makeclosure49071, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40559, %struct.ScmObj* %anf_45bind40240, i64 0)
%ae40560 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49073 = alloca %struct.ScmObj*, align 8
%fptrToInt49074 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40561 to i64
%ae40561 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49074)
store volatile %struct.ScmObj* %ae40561, %struct.ScmObj** %stackaddr$makeclosure49073, align 8
%args49051$ae40559$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49075 = alloca %struct.ScmObj*, align 8
%args49051$ae40559$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40561, %struct.ScmObj* %args49051$ae40559$0)
store volatile %struct.ScmObj* %args49051$ae40559$1, %struct.ScmObj** %stackaddr$prim49075, align 8
%stackaddr$prim49076 = alloca %struct.ScmObj*, align 8
%args49051$ae40559$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40560, %struct.ScmObj* %args49051$ae40559$1)
store volatile %struct.ScmObj* %args49051$ae40559$2, %struct.ScmObj** %stackaddr$prim49076, align 8
%clofunc49077 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40559)
musttail call tailcc void %clofunc49077(%struct.ScmObj* %ae40559, %struct.ScmObj* %args49051$ae40559$2)
ret void
}

define tailcc void @proc_clo$ae40559(%struct.ScmObj* %env$ae40559,%struct.ScmObj* %current_45args48375) {
%stackaddr$env-ref49078 = alloca %struct.ScmObj*, align 8
%anf_45bind40240 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40559, i64 0)
store %struct.ScmObj* %anf_45bind40240, %struct.ScmObj** %stackaddr$env-ref49078
%stackaddr$prim49079 = alloca %struct.ScmObj*, align 8
%_95k40366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48375)
store volatile %struct.ScmObj* %_95k40366, %struct.ScmObj** %stackaddr$prim49079, align 8
%stackaddr$prim49080 = alloca %struct.ScmObj*, align 8
%current_45args48376 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48375)
store volatile %struct.ScmObj* %current_45args48376, %struct.ScmObj** %stackaddr$prim49080, align 8
%stackaddr$prim49081 = alloca %struct.ScmObj*, align 8
%anf_45bind40244 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48376)
store volatile %struct.ScmObj* %anf_45bind40244, %struct.ScmObj** %stackaddr$prim49081, align 8
%stackaddr$makeclosure49082 = alloca %struct.ScmObj*, align 8
%fptrToInt49083 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40674 to i64
%ae40674 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49083)
store volatile %struct.ScmObj* %ae40674, %struct.ScmObj** %stackaddr$makeclosure49082, align 8
%args49030$anf_45bind40240$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49084 = alloca %struct.ScmObj*, align 8
%args49030$anf_45bind40240$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40244, %struct.ScmObj* %args49030$anf_45bind40240$0)
store volatile %struct.ScmObj* %args49030$anf_45bind40240$1, %struct.ScmObj** %stackaddr$prim49084, align 8
%stackaddr$prim49085 = alloca %struct.ScmObj*, align 8
%args49030$anf_45bind40240$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40674, %struct.ScmObj* %args49030$anf_45bind40240$1)
store volatile %struct.ScmObj* %args49030$anf_45bind40240$2, %struct.ScmObj** %stackaddr$prim49085, align 8
%clofunc49086 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40240)
musttail call tailcc void %clofunc49086(%struct.ScmObj* %anf_45bind40240, %struct.ScmObj* %args49030$anf_45bind40240$2)
ret void
}

define tailcc void @proc_clo$ae40674(%struct.ScmObj* %env$ae40674,%struct.ScmObj* %current_45args48378) {
%stackaddr$prim49087 = alloca %struct.ScmObj*, align 8
%_95k40367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48378)
store volatile %struct.ScmObj* %_95k40367, %struct.ScmObj** %stackaddr$prim49087, align 8
%stackaddr$prim49088 = alloca %struct.ScmObj*, align 8
%current_45args48379 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48378)
store volatile %struct.ScmObj* %current_45args48379, %struct.ScmObj** %stackaddr$prim49088, align 8
%stackaddr$prim49089 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48379)
store volatile %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$prim49089, align 8
%stackaddr$makeclosure49090 = alloca %struct.ScmObj*, align 8
%fptrToInt49091 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40676 to i64
%ae40676 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49091)
store volatile %struct.ScmObj* %ae40676, %struct.ScmObj** %stackaddr$makeclosure49090, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40676, %struct.ScmObj* %Ycmb40110, i64 0)
%ae40677 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49092 = alloca %struct.ScmObj*, align 8
%fptrToInt49093 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40678 to i64
%ae40678 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49093)
store volatile %struct.ScmObj* %ae40678, %struct.ScmObj** %stackaddr$makeclosure49092, align 8
%args49029$ae40676$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49094 = alloca %struct.ScmObj*, align 8
%args49029$ae40676$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40678, %struct.ScmObj* %args49029$ae40676$0)
store volatile %struct.ScmObj* %args49029$ae40676$1, %struct.ScmObj** %stackaddr$prim49094, align 8
%stackaddr$prim49095 = alloca %struct.ScmObj*, align 8
%args49029$ae40676$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40677, %struct.ScmObj* %args49029$ae40676$1)
store volatile %struct.ScmObj* %args49029$ae40676$2, %struct.ScmObj** %stackaddr$prim49095, align 8
%clofunc49096 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40676)
musttail call tailcc void %clofunc49096(%struct.ScmObj* %ae40676, %struct.ScmObj* %args49029$ae40676$2)
ret void
}

define tailcc void @proc_clo$ae40676(%struct.ScmObj* %env$ae40676,%struct.ScmObj* %current_45args48381) {
%stackaddr$env-ref49097 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40676, i64 0)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref49097
%stackaddr$prim49098 = alloca %struct.ScmObj*, align 8
%_95k40368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48381)
store volatile %struct.ScmObj* %_95k40368, %struct.ScmObj** %stackaddr$prim49098, align 8
%stackaddr$prim49099 = alloca %struct.ScmObj*, align 8
%current_45args48382 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48381)
store volatile %struct.ScmObj* %current_45args48382, %struct.ScmObj** %stackaddr$prim49099, align 8
%stackaddr$prim49100 = alloca %struct.ScmObj*, align 8
%anf_45bind40249 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48382)
store volatile %struct.ScmObj* %anf_45bind40249, %struct.ScmObj** %stackaddr$prim49100, align 8
%stackaddr$makeclosure49101 = alloca %struct.ScmObj*, align 8
%fptrToInt49102 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40754 to i64
%ae40754 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49102)
store volatile %struct.ScmObj* %ae40754, %struct.ScmObj** %stackaddr$makeclosure49101, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40754, %struct.ScmObj* %Ycmb40110, i64 0)
%args49013$Ycmb40110$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49103 = alloca %struct.ScmObj*, align 8
%args49013$Ycmb40110$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40249, %struct.ScmObj* %args49013$Ycmb40110$0)
store volatile %struct.ScmObj* %args49013$Ycmb40110$1, %struct.ScmObj** %stackaddr$prim49103, align 8
%stackaddr$prim49104 = alloca %struct.ScmObj*, align 8
%args49013$Ycmb40110$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40754, %struct.ScmObj* %args49013$Ycmb40110$1)
store volatile %struct.ScmObj* %args49013$Ycmb40110$2, %struct.ScmObj** %stackaddr$prim49104, align 8
%clofunc49105 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40110)
musttail call tailcc void %clofunc49105(%struct.ScmObj* %Ycmb40110, %struct.ScmObj* %args49013$Ycmb40110$2)
ret void
}

define tailcc void @proc_clo$ae40754(%struct.ScmObj* %env$ae40754,%struct.ScmObj* %current_45args48384) {
%stackaddr$env-ref49106 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40754, i64 0)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref49106
%stackaddr$prim49107 = alloca %struct.ScmObj*, align 8
%_95k40369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48384)
store volatile %struct.ScmObj* %_95k40369, %struct.ScmObj** %stackaddr$prim49107, align 8
%stackaddr$prim49108 = alloca %struct.ScmObj*, align 8
%current_45args48385 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48384)
store volatile %struct.ScmObj* %current_45args48385, %struct.ScmObj** %stackaddr$prim49108, align 8
%stackaddr$prim49109 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48385)
store volatile %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$prim49109, align 8
%stackaddr$makeclosure49110 = alloca %struct.ScmObj*, align 8
%fptrToInt49111 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40756 to i64
%ae40756 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49111)
store volatile %struct.ScmObj* %ae40756, %struct.ScmObj** %stackaddr$makeclosure49110, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40756, %struct.ScmObj* %_37foldr140131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40756, %struct.ScmObj* %Ycmb40110, i64 1)
%ae40757 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49112 = alloca %struct.ScmObj*, align 8
%fptrToInt49113 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40758 to i64
%ae40758 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49113)
store volatile %struct.ScmObj* %ae40758, %struct.ScmObj** %stackaddr$makeclosure49112, align 8
%args49012$ae40756$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49114 = alloca %struct.ScmObj*, align 8
%args49012$ae40756$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40758, %struct.ScmObj* %args49012$ae40756$0)
store volatile %struct.ScmObj* %args49012$ae40756$1, %struct.ScmObj** %stackaddr$prim49114, align 8
%stackaddr$prim49115 = alloca %struct.ScmObj*, align 8
%args49012$ae40756$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40757, %struct.ScmObj* %args49012$ae40756$1)
store volatile %struct.ScmObj* %args49012$ae40756$2, %struct.ScmObj** %stackaddr$prim49115, align 8
%clofunc49116 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40756)
musttail call tailcc void %clofunc49116(%struct.ScmObj* %ae40756, %struct.ScmObj* %args49012$ae40756$2)
ret void
}

define tailcc void @proc_clo$ae40756(%struct.ScmObj* %env$ae40756,%struct.ScmObj* %current_45args48387) {
%stackaddr$env-ref49117 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40756, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref49117
%stackaddr$env-ref49118 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40756, i64 1)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref49118
%stackaddr$prim49119 = alloca %struct.ScmObj*, align 8
%_95k40370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48387)
store volatile %struct.ScmObj* %_95k40370, %struct.ScmObj** %stackaddr$prim49119, align 8
%stackaddr$prim49120 = alloca %struct.ScmObj*, align 8
%current_45args48388 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48387)
store volatile %struct.ScmObj* %current_45args48388, %struct.ScmObj** %stackaddr$prim49120, align 8
%stackaddr$prim49121 = alloca %struct.ScmObj*, align 8
%anf_45bind40255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48388)
store volatile %struct.ScmObj* %anf_45bind40255, %struct.ScmObj** %stackaddr$prim49121, align 8
%stackaddr$makeclosure49122 = alloca %struct.ScmObj*, align 8
%fptrToInt49123 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40851 to i64
%ae40851 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49123)
store volatile %struct.ScmObj* %ae40851, %struct.ScmObj** %stackaddr$makeclosure49122, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40851, %struct.ScmObj* %_37foldr140131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40851, %struct.ScmObj* %Ycmb40110, i64 1)
%args48993$Ycmb40110$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49124 = alloca %struct.ScmObj*, align 8
%args48993$Ycmb40110$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40255, %struct.ScmObj* %args48993$Ycmb40110$0)
store volatile %struct.ScmObj* %args48993$Ycmb40110$1, %struct.ScmObj** %stackaddr$prim49124, align 8
%stackaddr$prim49125 = alloca %struct.ScmObj*, align 8
%args48993$Ycmb40110$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40851, %struct.ScmObj* %args48993$Ycmb40110$1)
store volatile %struct.ScmObj* %args48993$Ycmb40110$2, %struct.ScmObj** %stackaddr$prim49125, align 8
%clofunc49126 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40110)
musttail call tailcc void %clofunc49126(%struct.ScmObj* %Ycmb40110, %struct.ScmObj* %args48993$Ycmb40110$2)
ret void
}

define tailcc void @proc_clo$ae40851(%struct.ScmObj* %env$ae40851,%struct.ScmObj* %current_45args48390) {
%stackaddr$env-ref49127 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40851, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref49127
%stackaddr$env-ref49128 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40851, i64 1)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref49128
%stackaddr$prim49129 = alloca %struct.ScmObj*, align 8
%_95k40371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48390)
store volatile %struct.ScmObj* %_95k40371, %struct.ScmObj** %stackaddr$prim49129, align 8
%stackaddr$prim49130 = alloca %struct.ScmObj*, align 8
%current_45args48391 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48390)
store volatile %struct.ScmObj* %current_45args48391, %struct.ScmObj** %stackaddr$prim49130, align 8
%stackaddr$prim49131 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48391)
store volatile %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$prim49131, align 8
%stackaddr$makeclosure49132 = alloca %struct.ScmObj*, align 8
%fptrToInt49133 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40853 to i64
%ae40853 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49133)
store volatile %struct.ScmObj* %ae40853, %struct.ScmObj** %stackaddr$makeclosure49132, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40853, %struct.ScmObj* %_37foldr140131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40853, %struct.ScmObj* %_37map140127, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40853, %struct.ScmObj* %Ycmb40110, i64 2)
%ae40854 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49134 = alloca %struct.ScmObj*, align 8
%fptrToInt49135 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40855 to i64
%ae40855 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49135)
store volatile %struct.ScmObj* %ae40855, %struct.ScmObj** %stackaddr$makeclosure49134, align 8
%args48992$ae40853$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49136 = alloca %struct.ScmObj*, align 8
%args48992$ae40853$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40855, %struct.ScmObj* %args48992$ae40853$0)
store volatile %struct.ScmObj* %args48992$ae40853$1, %struct.ScmObj** %stackaddr$prim49136, align 8
%stackaddr$prim49137 = alloca %struct.ScmObj*, align 8
%args48992$ae40853$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40854, %struct.ScmObj* %args48992$ae40853$1)
store volatile %struct.ScmObj* %args48992$ae40853$2, %struct.ScmObj** %stackaddr$prim49137, align 8
%clofunc49138 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40853)
musttail call tailcc void %clofunc49138(%struct.ScmObj* %ae40853, %struct.ScmObj* %args48992$ae40853$2)
ret void
}

define tailcc void @proc_clo$ae40853(%struct.ScmObj* %env$ae40853,%struct.ScmObj* %current_45args48393) {
%stackaddr$env-ref49139 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40853, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref49139
%stackaddr$env-ref49140 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40853, i64 1)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref49140
%stackaddr$env-ref49141 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40853, i64 2)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref49141
%stackaddr$prim49142 = alloca %struct.ScmObj*, align 8
%_95k40372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48393)
store volatile %struct.ScmObj* %_95k40372, %struct.ScmObj** %stackaddr$prim49142, align 8
%stackaddr$prim49143 = alloca %struct.ScmObj*, align 8
%current_45args48394 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48393)
store volatile %struct.ScmObj* %current_45args48394, %struct.ScmObj** %stackaddr$prim49143, align 8
%stackaddr$prim49144 = alloca %struct.ScmObj*, align 8
%anf_45bind40262 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48394)
store volatile %struct.ScmObj* %anf_45bind40262, %struct.ScmObj** %stackaddr$prim49144, align 8
%stackaddr$makeclosure49145 = alloca %struct.ScmObj*, align 8
%fptrToInt49146 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41001 to i64
%ae41001 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49146)
store volatile %struct.ScmObj* %ae41001, %struct.ScmObj** %stackaddr$makeclosure49145, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41001, %struct.ScmObj* %_37foldr140131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41001, %struct.ScmObj* %_37map140127, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41001, %struct.ScmObj* %Ycmb40110, i64 2)
%args48976$Ycmb40110$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49147 = alloca %struct.ScmObj*, align 8
%args48976$Ycmb40110$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40262, %struct.ScmObj* %args48976$Ycmb40110$0)
store volatile %struct.ScmObj* %args48976$Ycmb40110$1, %struct.ScmObj** %stackaddr$prim49147, align 8
%stackaddr$prim49148 = alloca %struct.ScmObj*, align 8
%args48976$Ycmb40110$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41001, %struct.ScmObj* %args48976$Ycmb40110$1)
store volatile %struct.ScmObj* %args48976$Ycmb40110$2, %struct.ScmObj** %stackaddr$prim49148, align 8
%clofunc49149 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40110)
musttail call tailcc void %clofunc49149(%struct.ScmObj* %Ycmb40110, %struct.ScmObj* %args48976$Ycmb40110$2)
ret void
}

define tailcc void @proc_clo$ae41001(%struct.ScmObj* %env$ae41001,%struct.ScmObj* %current_45args48396) {
%stackaddr$env-ref49150 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41001, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref49150
%stackaddr$env-ref49151 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41001, i64 1)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref49151
%stackaddr$env-ref49152 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41001, i64 2)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref49152
%stackaddr$prim49153 = alloca %struct.ScmObj*, align 8
%_95k40373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48396)
store volatile %struct.ScmObj* %_95k40373, %struct.ScmObj** %stackaddr$prim49153, align 8
%stackaddr$prim49154 = alloca %struct.ScmObj*, align 8
%current_45args48397 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48396)
store volatile %struct.ScmObj* %current_45args48397, %struct.ScmObj** %stackaddr$prim49154, align 8
%stackaddr$prim49155 = alloca %struct.ScmObj*, align 8
%_37take40123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48397)
store volatile %struct.ScmObj* %_37take40123, %struct.ScmObj** %stackaddr$prim49155, align 8
%stackaddr$makeclosure49156 = alloca %struct.ScmObj*, align 8
%fptrToInt49157 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41003 to i64
%ae41003 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49157)
store volatile %struct.ScmObj* %ae41003, %struct.ScmObj** %stackaddr$makeclosure49156, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41003, %struct.ScmObj* %_37foldr140131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41003, %struct.ScmObj* %_37map140127, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41003, %struct.ScmObj* %Ycmb40110, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41003, %struct.ScmObj* %_37take40123, i64 3)
%ae41004 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49158 = alloca %struct.ScmObj*, align 8
%fptrToInt49159 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41005 to i64
%ae41005 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49159)
store volatile %struct.ScmObj* %ae41005, %struct.ScmObj** %stackaddr$makeclosure49158, align 8
%args48975$ae41003$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49160 = alloca %struct.ScmObj*, align 8
%args48975$ae41003$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41005, %struct.ScmObj* %args48975$ae41003$0)
store volatile %struct.ScmObj* %args48975$ae41003$1, %struct.ScmObj** %stackaddr$prim49160, align 8
%stackaddr$prim49161 = alloca %struct.ScmObj*, align 8
%args48975$ae41003$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41004, %struct.ScmObj* %args48975$ae41003$1)
store volatile %struct.ScmObj* %args48975$ae41003$2, %struct.ScmObj** %stackaddr$prim49161, align 8
%clofunc49162 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41003)
musttail call tailcc void %clofunc49162(%struct.ScmObj* %ae41003, %struct.ScmObj* %args48975$ae41003$2)
ret void
}

define tailcc void @proc_clo$ae41003(%struct.ScmObj* %env$ae41003,%struct.ScmObj* %current_45args48399) {
%stackaddr$env-ref49163 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41003, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref49163
%stackaddr$env-ref49164 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41003, i64 1)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref49164
%stackaddr$env-ref49165 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41003, i64 2)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref49165
%stackaddr$env-ref49166 = alloca %struct.ScmObj*, align 8
%_37take40123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41003, i64 3)
store %struct.ScmObj* %_37take40123, %struct.ScmObj** %stackaddr$env-ref49166
%stackaddr$prim49167 = alloca %struct.ScmObj*, align 8
%_95k40374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48399)
store volatile %struct.ScmObj* %_95k40374, %struct.ScmObj** %stackaddr$prim49167, align 8
%stackaddr$prim49168 = alloca %struct.ScmObj*, align 8
%current_45args48400 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48399)
store volatile %struct.ScmObj* %current_45args48400, %struct.ScmObj** %stackaddr$prim49168, align 8
%stackaddr$prim49169 = alloca %struct.ScmObj*, align 8
%anf_45bind40266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48400)
store volatile %struct.ScmObj* %anf_45bind40266, %struct.ScmObj** %stackaddr$prim49169, align 8
%stackaddr$makeclosure49170 = alloca %struct.ScmObj*, align 8
%fptrToInt49171 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41084 to i64
%ae41084 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49171)
store volatile %struct.ScmObj* %ae41084, %struct.ScmObj** %stackaddr$makeclosure49170, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41084, %struct.ScmObj* %_37foldr140131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41084, %struct.ScmObj* %_37map140127, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41084, %struct.ScmObj* %Ycmb40110, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41084, %struct.ScmObj* %_37take40123, i64 3)
%args48961$Ycmb40110$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49172 = alloca %struct.ScmObj*, align 8
%args48961$Ycmb40110$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40266, %struct.ScmObj* %args48961$Ycmb40110$0)
store volatile %struct.ScmObj* %args48961$Ycmb40110$1, %struct.ScmObj** %stackaddr$prim49172, align 8
%stackaddr$prim49173 = alloca %struct.ScmObj*, align 8
%args48961$Ycmb40110$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41084, %struct.ScmObj* %args48961$Ycmb40110$1)
store volatile %struct.ScmObj* %args48961$Ycmb40110$2, %struct.ScmObj** %stackaddr$prim49173, align 8
%clofunc49174 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40110)
musttail call tailcc void %clofunc49174(%struct.ScmObj* %Ycmb40110, %struct.ScmObj* %args48961$Ycmb40110$2)
ret void
}

define tailcc void @proc_clo$ae41084(%struct.ScmObj* %env$ae41084,%struct.ScmObj* %current_45args48402) {
%stackaddr$env-ref49175 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41084, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref49175
%stackaddr$env-ref49176 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41084, i64 1)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref49176
%stackaddr$env-ref49177 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41084, i64 2)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref49177
%stackaddr$env-ref49178 = alloca %struct.ScmObj*, align 8
%_37take40123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41084, i64 3)
store %struct.ScmObj* %_37take40123, %struct.ScmObj** %stackaddr$env-ref49178
%stackaddr$prim49179 = alloca %struct.ScmObj*, align 8
%_95k40375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48402)
store volatile %struct.ScmObj* %_95k40375, %struct.ScmObj** %stackaddr$prim49179, align 8
%stackaddr$prim49180 = alloca %struct.ScmObj*, align 8
%current_45args48403 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48402)
store volatile %struct.ScmObj* %current_45args48403, %struct.ScmObj** %stackaddr$prim49180, align 8
%stackaddr$prim49181 = alloca %struct.ScmObj*, align 8
%_37length40120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48403)
store volatile %struct.ScmObj* %_37length40120, %struct.ScmObj** %stackaddr$prim49181, align 8
%stackaddr$makeclosure49182 = alloca %struct.ScmObj*, align 8
%fptrToInt49183 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41086 to i64
%ae41086 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49183)
store volatile %struct.ScmObj* %ae41086, %struct.ScmObj** %stackaddr$makeclosure49182, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41086, %struct.ScmObj* %_37length40120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41086, %struct.ScmObj* %_37foldr140131, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41086, %struct.ScmObj* %_37map140127, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41086, %struct.ScmObj* %Ycmb40110, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41086, %struct.ScmObj* %_37take40123, i64 4)
%ae41087 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49184 = alloca %struct.ScmObj*, align 8
%fptrToInt49185 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41088 to i64
%ae41088 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49185)
store volatile %struct.ScmObj* %ae41088, %struct.ScmObj** %stackaddr$makeclosure49184, align 8
%args48960$ae41086$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49186 = alloca %struct.ScmObj*, align 8
%args48960$ae41086$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41088, %struct.ScmObj* %args48960$ae41086$0)
store volatile %struct.ScmObj* %args48960$ae41086$1, %struct.ScmObj** %stackaddr$prim49186, align 8
%stackaddr$prim49187 = alloca %struct.ScmObj*, align 8
%args48960$ae41086$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41087, %struct.ScmObj* %args48960$ae41086$1)
store volatile %struct.ScmObj* %args48960$ae41086$2, %struct.ScmObj** %stackaddr$prim49187, align 8
%clofunc49188 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41086)
musttail call tailcc void %clofunc49188(%struct.ScmObj* %ae41086, %struct.ScmObj* %args48960$ae41086$2)
ret void
}

define tailcc void @proc_clo$ae41086(%struct.ScmObj* %env$ae41086,%struct.ScmObj* %current_45args48405) {
%stackaddr$env-ref49189 = alloca %struct.ScmObj*, align 8
%_37length40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41086, i64 0)
store %struct.ScmObj* %_37length40120, %struct.ScmObj** %stackaddr$env-ref49189
%stackaddr$env-ref49190 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41086, i64 1)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref49190
%stackaddr$env-ref49191 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41086, i64 2)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref49191
%stackaddr$env-ref49192 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41086, i64 3)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref49192
%stackaddr$env-ref49193 = alloca %struct.ScmObj*, align 8
%_37take40123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41086, i64 4)
store %struct.ScmObj* %_37take40123, %struct.ScmObj** %stackaddr$env-ref49193
%stackaddr$prim49194 = alloca %struct.ScmObj*, align 8
%_95k40376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48405)
store volatile %struct.ScmObj* %_95k40376, %struct.ScmObj** %stackaddr$prim49194, align 8
%stackaddr$prim49195 = alloca %struct.ScmObj*, align 8
%current_45args48406 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48405)
store volatile %struct.ScmObj* %current_45args48406, %struct.ScmObj** %stackaddr$prim49195, align 8
%stackaddr$prim49196 = alloca %struct.ScmObj*, align 8
%anf_45bind40271 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48406)
store volatile %struct.ScmObj* %anf_45bind40271, %struct.ScmObj** %stackaddr$prim49196, align 8
%stackaddr$makeclosure49197 = alloca %struct.ScmObj*, align 8
%fptrToInt49198 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41163 to i64
%ae41163 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49198)
store volatile %struct.ScmObj* %ae41163, %struct.ScmObj** %stackaddr$makeclosure49197, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41163, %struct.ScmObj* %_37length40120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41163, %struct.ScmObj* %_37foldr140131, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41163, %struct.ScmObj* %_37map140127, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41163, %struct.ScmObj* %Ycmb40110, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41163, %struct.ScmObj* %_37take40123, i64 4)
%args48944$Ycmb40110$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49199 = alloca %struct.ScmObj*, align 8
%args48944$Ycmb40110$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40271, %struct.ScmObj* %args48944$Ycmb40110$0)
store volatile %struct.ScmObj* %args48944$Ycmb40110$1, %struct.ScmObj** %stackaddr$prim49199, align 8
%stackaddr$prim49200 = alloca %struct.ScmObj*, align 8
%args48944$Ycmb40110$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41163, %struct.ScmObj* %args48944$Ycmb40110$1)
store volatile %struct.ScmObj* %args48944$Ycmb40110$2, %struct.ScmObj** %stackaddr$prim49200, align 8
%clofunc49201 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40110)
musttail call tailcc void %clofunc49201(%struct.ScmObj* %Ycmb40110, %struct.ScmObj* %args48944$Ycmb40110$2)
ret void
}

define tailcc void @proc_clo$ae41163(%struct.ScmObj* %env$ae41163,%struct.ScmObj* %current_45args48408) {
%stackaddr$env-ref49202 = alloca %struct.ScmObj*, align 8
%_37length40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41163, i64 0)
store %struct.ScmObj* %_37length40120, %struct.ScmObj** %stackaddr$env-ref49202
%stackaddr$env-ref49203 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41163, i64 1)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref49203
%stackaddr$env-ref49204 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41163, i64 2)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref49204
%stackaddr$env-ref49205 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41163, i64 3)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref49205
%stackaddr$env-ref49206 = alloca %struct.ScmObj*, align 8
%_37take40123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41163, i64 4)
store %struct.ScmObj* %_37take40123, %struct.ScmObj** %stackaddr$env-ref49206
%stackaddr$prim49207 = alloca %struct.ScmObj*, align 8
%_95k40377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48408)
store volatile %struct.ScmObj* %_95k40377, %struct.ScmObj** %stackaddr$prim49207, align 8
%stackaddr$prim49208 = alloca %struct.ScmObj*, align 8
%current_45args48409 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48408)
store volatile %struct.ScmObj* %current_45args48409, %struct.ScmObj** %stackaddr$prim49208, align 8
%stackaddr$prim49209 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48409)
store volatile %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$prim49209, align 8
%stackaddr$makeclosure49210 = alloca %struct.ScmObj*, align 8
%fptrToInt49211 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41165 to i64
%ae41165 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt49211)
store volatile %struct.ScmObj* %ae41165, %struct.ScmObj** %stackaddr$makeclosure49210, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41165, %struct.ScmObj* %_37foldr140131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41165, %struct.ScmObj* %_37foldl140115, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41165, %struct.ScmObj* %_37length40120, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41165, %struct.ScmObj* %_37map140127, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41165, %struct.ScmObj* %Ycmb40110, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41165, %struct.ScmObj* %_37take40123, i64 5)
%ae41166 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49212 = alloca %struct.ScmObj*, align 8
%fptrToInt49213 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41167 to i64
%ae41167 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49213)
store volatile %struct.ScmObj* %ae41167, %struct.ScmObj** %stackaddr$makeclosure49212, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41167, %struct.ScmObj* %_37foldl140115, i64 0)
%args48943$ae41165$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49214 = alloca %struct.ScmObj*, align 8
%args48943$ae41165$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41167, %struct.ScmObj* %args48943$ae41165$0)
store volatile %struct.ScmObj* %args48943$ae41165$1, %struct.ScmObj** %stackaddr$prim49214, align 8
%stackaddr$prim49215 = alloca %struct.ScmObj*, align 8
%args48943$ae41165$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41166, %struct.ScmObj* %args48943$ae41165$1)
store volatile %struct.ScmObj* %args48943$ae41165$2, %struct.ScmObj** %stackaddr$prim49215, align 8
%clofunc49216 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41165)
musttail call tailcc void %clofunc49216(%struct.ScmObj* %ae41165, %struct.ScmObj* %args48943$ae41165$2)
ret void
}

define tailcc void @proc_clo$ae41165(%struct.ScmObj* %env$ae41165,%struct.ScmObj* %current_45args48411) {
%stackaddr$env-ref49217 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41165, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref49217
%stackaddr$env-ref49218 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41165, i64 1)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref49218
%stackaddr$env-ref49219 = alloca %struct.ScmObj*, align 8
%_37length40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41165, i64 2)
store %struct.ScmObj* %_37length40120, %struct.ScmObj** %stackaddr$env-ref49219
%stackaddr$env-ref49220 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41165, i64 3)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref49220
%stackaddr$env-ref49221 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41165, i64 4)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref49221
%stackaddr$env-ref49222 = alloca %struct.ScmObj*, align 8
%_37take40123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41165, i64 5)
store %struct.ScmObj* %_37take40123, %struct.ScmObj** %stackaddr$env-ref49222
%stackaddr$prim49223 = alloca %struct.ScmObj*, align 8
%_95k40378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48411)
store volatile %struct.ScmObj* %_95k40378, %struct.ScmObj** %stackaddr$prim49223, align 8
%stackaddr$prim49224 = alloca %struct.ScmObj*, align 8
%current_45args48412 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48411)
store volatile %struct.ScmObj* %current_45args48412, %struct.ScmObj** %stackaddr$prim49224, align 8
%stackaddr$prim49225 = alloca %struct.ScmObj*, align 8
%_37last40153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48412)
store volatile %struct.ScmObj* %_37last40153, %struct.ScmObj** %stackaddr$prim49225, align 8
%stackaddr$makeclosure49226 = alloca %struct.ScmObj*, align 8
%fptrToInt49227 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41219 to i64
%ae41219 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49227)
store volatile %struct.ScmObj* %ae41219, %struct.ScmObj** %stackaddr$makeclosure49226, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41219, %struct.ScmObj* %_37foldr140131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41219, %struct.ScmObj* %_37foldl140115, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41219, %struct.ScmObj* %_37last40153, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41219, %struct.ScmObj* %_37map140127, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41219, %struct.ScmObj* %Ycmb40110, i64 4)
%ae41220 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49228 = alloca %struct.ScmObj*, align 8
%fptrToInt49229 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41221 to i64
%ae41221 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49229)
store volatile %struct.ScmObj* %ae41221, %struct.ScmObj** %stackaddr$makeclosure49228, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41221, %struct.ScmObj* %_37length40120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41221, %struct.ScmObj* %_37take40123, i64 1)
%args48929$ae41219$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49230 = alloca %struct.ScmObj*, align 8
%args48929$ae41219$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41221, %struct.ScmObj* %args48929$ae41219$0)
store volatile %struct.ScmObj* %args48929$ae41219$1, %struct.ScmObj** %stackaddr$prim49230, align 8
%stackaddr$prim49231 = alloca %struct.ScmObj*, align 8
%args48929$ae41219$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41220, %struct.ScmObj* %args48929$ae41219$1)
store volatile %struct.ScmObj* %args48929$ae41219$2, %struct.ScmObj** %stackaddr$prim49231, align 8
%clofunc49232 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41219)
musttail call tailcc void %clofunc49232(%struct.ScmObj* %ae41219, %struct.ScmObj* %args48929$ae41219$2)
ret void
}

define tailcc void @proc_clo$ae41219(%struct.ScmObj* %env$ae41219,%struct.ScmObj* %current_45args48414) {
%stackaddr$env-ref49233 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41219, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref49233
%stackaddr$env-ref49234 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41219, i64 1)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref49234
%stackaddr$env-ref49235 = alloca %struct.ScmObj*, align 8
%_37last40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41219, i64 2)
store %struct.ScmObj* %_37last40153, %struct.ScmObj** %stackaddr$env-ref49235
%stackaddr$env-ref49236 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41219, i64 3)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref49236
%stackaddr$env-ref49237 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41219, i64 4)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref49237
%stackaddr$prim49238 = alloca %struct.ScmObj*, align 8
%_95k40379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48414)
store volatile %struct.ScmObj* %_95k40379, %struct.ScmObj** %stackaddr$prim49238, align 8
%stackaddr$prim49239 = alloca %struct.ScmObj*, align 8
%current_45args48415 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48414)
store volatile %struct.ScmObj* %current_45args48415, %struct.ScmObj** %stackaddr$prim49239, align 8
%stackaddr$prim49240 = alloca %struct.ScmObj*, align 8
%_37drop_45right40150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48415)
store volatile %struct.ScmObj* %_37drop_45right40150, %struct.ScmObj** %stackaddr$prim49240, align 8
%stackaddr$makeclosure49241 = alloca %struct.ScmObj*, align 8
%fptrToInt49242 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41249 to i64
%ae41249 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49242)
store volatile %struct.ScmObj* %ae41249, %struct.ScmObj** %stackaddr$makeclosure49241, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41249, %struct.ScmObj* %_37foldr140131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41249, %struct.ScmObj* %_37foldl140115, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41249, %struct.ScmObj* %_37last40153, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41249, %struct.ScmObj* %_37drop_45right40150, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41249, %struct.ScmObj* %Ycmb40110, i64 4)
%ae41250 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49243 = alloca %struct.ScmObj*, align 8
%fptrToInt49244 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41251 to i64
%ae41251 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49244)
store volatile %struct.ScmObj* %ae41251, %struct.ScmObj** %stackaddr$makeclosure49243, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41251, %struct.ScmObj* %_37foldr140131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41251, %struct.ScmObj* %_37map140127, i64 1)
%args48919$ae41249$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49245 = alloca %struct.ScmObj*, align 8
%args48919$ae41249$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41251, %struct.ScmObj* %args48919$ae41249$0)
store volatile %struct.ScmObj* %args48919$ae41249$1, %struct.ScmObj** %stackaddr$prim49245, align 8
%stackaddr$prim49246 = alloca %struct.ScmObj*, align 8
%args48919$ae41249$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41250, %struct.ScmObj* %args48919$ae41249$1)
store volatile %struct.ScmObj* %args48919$ae41249$2, %struct.ScmObj** %stackaddr$prim49246, align 8
%clofunc49247 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41249)
musttail call tailcc void %clofunc49247(%struct.ScmObj* %ae41249, %struct.ScmObj* %args48919$ae41249$2)
ret void
}

define tailcc void @proc_clo$ae41249(%struct.ScmObj* %env$ae41249,%struct.ScmObj* %current_45args48417) {
%stackaddr$env-ref49248 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41249, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref49248
%stackaddr$env-ref49249 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41249, i64 1)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref49249
%stackaddr$env-ref49250 = alloca %struct.ScmObj*, align 8
%_37last40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41249, i64 2)
store %struct.ScmObj* %_37last40153, %struct.ScmObj** %stackaddr$env-ref49250
%stackaddr$env-ref49251 = alloca %struct.ScmObj*, align 8
%_37drop_45right40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41249, i64 3)
store %struct.ScmObj* %_37drop_45right40150, %struct.ScmObj** %stackaddr$env-ref49251
%stackaddr$env-ref49252 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41249, i64 4)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref49252
%stackaddr$prim49253 = alloca %struct.ScmObj*, align 8
%_95k40380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48417)
store volatile %struct.ScmObj* %_95k40380, %struct.ScmObj** %stackaddr$prim49253, align 8
%stackaddr$prim49254 = alloca %struct.ScmObj*, align 8
%current_45args48418 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48417)
store volatile %struct.ScmObj* %current_45args48418, %struct.ScmObj** %stackaddr$prim49254, align 8
%stackaddr$prim49255 = alloca %struct.ScmObj*, align 8
%anf_45bind40287 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48418)
store volatile %struct.ScmObj* %anf_45bind40287, %struct.ScmObj** %stackaddr$prim49255, align 8
%stackaddr$makeclosure49256 = alloca %struct.ScmObj*, align 8
%fptrToInt49257 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41633 to i64
%ae41633 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49257)
store volatile %struct.ScmObj* %ae41633, %struct.ScmObj** %stackaddr$makeclosure49256, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41633, %struct.ScmObj* %_37foldr140131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41633, %struct.ScmObj* %_37foldl140115, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41633, %struct.ScmObj* %_37last40153, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41633, %struct.ScmObj* %_37drop_45right40150, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41633, %struct.ScmObj* %Ycmb40110, i64 4)
%args48859$Ycmb40110$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49258 = alloca %struct.ScmObj*, align 8
%args48859$Ycmb40110$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40287, %struct.ScmObj* %args48859$Ycmb40110$0)
store volatile %struct.ScmObj* %args48859$Ycmb40110$1, %struct.ScmObj** %stackaddr$prim49258, align 8
%stackaddr$prim49259 = alloca %struct.ScmObj*, align 8
%args48859$Ycmb40110$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41633, %struct.ScmObj* %args48859$Ycmb40110$1)
store volatile %struct.ScmObj* %args48859$Ycmb40110$2, %struct.ScmObj** %stackaddr$prim49259, align 8
%clofunc49260 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40110)
musttail call tailcc void %clofunc49260(%struct.ScmObj* %Ycmb40110, %struct.ScmObj* %args48859$Ycmb40110$2)
ret void
}

define tailcc void @proc_clo$ae41633(%struct.ScmObj* %env$ae41633,%struct.ScmObj* %current_45args48420) {
%stackaddr$env-ref49261 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41633, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref49261
%stackaddr$env-ref49262 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41633, i64 1)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref49262
%stackaddr$env-ref49263 = alloca %struct.ScmObj*, align 8
%_37last40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41633, i64 2)
store %struct.ScmObj* %_37last40153, %struct.ScmObj** %stackaddr$env-ref49263
%stackaddr$env-ref49264 = alloca %struct.ScmObj*, align 8
%_37drop_45right40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41633, i64 3)
store %struct.ScmObj* %_37drop_45right40150, %struct.ScmObj** %stackaddr$env-ref49264
%stackaddr$env-ref49265 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41633, i64 4)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref49265
%stackaddr$prim49266 = alloca %struct.ScmObj*, align 8
%_95k40381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48420)
store volatile %struct.ScmObj* %_95k40381, %struct.ScmObj** %stackaddr$prim49266, align 8
%stackaddr$prim49267 = alloca %struct.ScmObj*, align 8
%current_45args48421 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48420)
store volatile %struct.ScmObj* %current_45args48421, %struct.ScmObj** %stackaddr$prim49267, align 8
%stackaddr$prim49268 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48421)
store volatile %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$prim49268, align 8
%stackaddr$makeclosure49269 = alloca %struct.ScmObj*, align 8
%fptrToInt49270 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41635 to i64
%ae41635 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt49270)
store volatile %struct.ScmObj* %ae41635, %struct.ScmObj** %stackaddr$makeclosure49269, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41635, %struct.ScmObj* %_37foldr140131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41635, %struct.ScmObj* %_37foldl140115, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41635, %struct.ScmObj* %_37last40153, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41635, %struct.ScmObj* %_37foldr40136, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41635, %struct.ScmObj* %_37drop_45right40150, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41635, %struct.ScmObj* %Ycmb40110, i64 5)
%ae41636 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49271 = alloca %struct.ScmObj*, align 8
%fptrToInt49272 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41637 to i64
%ae41637 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49272)
store volatile %struct.ScmObj* %ae41637, %struct.ScmObj** %stackaddr$makeclosure49271, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41637, %struct.ScmObj* %_37foldr140131, i64 0)
%args48858$ae41635$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49273 = alloca %struct.ScmObj*, align 8
%args48858$ae41635$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41637, %struct.ScmObj* %args48858$ae41635$0)
store volatile %struct.ScmObj* %args48858$ae41635$1, %struct.ScmObj** %stackaddr$prim49273, align 8
%stackaddr$prim49274 = alloca %struct.ScmObj*, align 8
%args48858$ae41635$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41636, %struct.ScmObj* %args48858$ae41635$1)
store volatile %struct.ScmObj* %args48858$ae41635$2, %struct.ScmObj** %stackaddr$prim49274, align 8
%clofunc49275 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41635)
musttail call tailcc void %clofunc49275(%struct.ScmObj* %ae41635, %struct.ScmObj* %args48858$ae41635$2)
ret void
}

define tailcc void @proc_clo$ae41635(%struct.ScmObj* %env$ae41635,%struct.ScmObj* %current_45args48423) {
%stackaddr$env-ref49276 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41635, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref49276
%stackaddr$env-ref49277 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41635, i64 1)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref49277
%stackaddr$env-ref49278 = alloca %struct.ScmObj*, align 8
%_37last40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41635, i64 2)
store %struct.ScmObj* %_37last40153, %struct.ScmObj** %stackaddr$env-ref49278
%stackaddr$env-ref49279 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41635, i64 3)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref49279
%stackaddr$env-ref49280 = alloca %struct.ScmObj*, align 8
%_37drop_45right40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41635, i64 4)
store %struct.ScmObj* %_37drop_45right40150, %struct.ScmObj** %stackaddr$env-ref49280
%stackaddr$env-ref49281 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41635, i64 5)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref49281
%stackaddr$prim49282 = alloca %struct.ScmObj*, align 8
%_95k40382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48423)
store volatile %struct.ScmObj* %_95k40382, %struct.ScmObj** %stackaddr$prim49282, align 8
%stackaddr$prim49283 = alloca %struct.ScmObj*, align 8
%current_45args48424 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48423)
store volatile %struct.ScmObj* %current_45args48424, %struct.ScmObj** %stackaddr$prim49283, align 8
%stackaddr$prim49284 = alloca %struct.ScmObj*, align 8
%_37map140162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48424)
store volatile %struct.ScmObj* %_37map140162, %struct.ScmObj** %stackaddr$prim49284, align 8
%stackaddr$makeclosure49285 = alloca %struct.ScmObj*, align 8
%fptrToInt49286 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41712 to i64
%ae41712 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49286)
store volatile %struct.ScmObj* %ae41712, %struct.ScmObj** %stackaddr$makeclosure49285, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41712, %struct.ScmObj* %_37foldr140131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41712, %struct.ScmObj* %_37foldl140115, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41712, %struct.ScmObj* %_37foldr40136, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41712, %struct.ScmObj* %_37map140162, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41712, %struct.ScmObj* %Ycmb40110, i64 4)
%ae41713 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49287 = alloca %struct.ScmObj*, align 8
%fptrToInt49288 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41714 to i64
%ae41714 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49288)
store volatile %struct.ScmObj* %ae41714, %struct.ScmObj** %stackaddr$makeclosure49287, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41714, %struct.ScmObj* %_37last40153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41714, %struct.ScmObj* %_37foldr40136, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41714, %struct.ScmObj* %_37drop_45right40150, i64 2)
%args48839$ae41712$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49289 = alloca %struct.ScmObj*, align 8
%args48839$ae41712$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41714, %struct.ScmObj* %args48839$ae41712$0)
store volatile %struct.ScmObj* %args48839$ae41712$1, %struct.ScmObj** %stackaddr$prim49289, align 8
%stackaddr$prim49290 = alloca %struct.ScmObj*, align 8
%args48839$ae41712$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41713, %struct.ScmObj* %args48839$ae41712$1)
store volatile %struct.ScmObj* %args48839$ae41712$2, %struct.ScmObj** %stackaddr$prim49290, align 8
%clofunc49291 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41712)
musttail call tailcc void %clofunc49291(%struct.ScmObj* %ae41712, %struct.ScmObj* %args48839$ae41712$2)
ret void
}

define tailcc void @proc_clo$ae41712(%struct.ScmObj* %env$ae41712,%struct.ScmObj* %current_45args48426) {
%stackaddr$env-ref49292 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41712, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref49292
%stackaddr$env-ref49293 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41712, i64 1)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref49293
%stackaddr$env-ref49294 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41712, i64 2)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref49294
%stackaddr$env-ref49295 = alloca %struct.ScmObj*, align 8
%_37map140162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41712, i64 3)
store %struct.ScmObj* %_37map140162, %struct.ScmObj** %stackaddr$env-ref49295
%stackaddr$env-ref49296 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41712, i64 4)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref49296
%stackaddr$prim49297 = alloca %struct.ScmObj*, align 8
%_95k40383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48426)
store volatile %struct.ScmObj* %_95k40383, %struct.ScmObj** %stackaddr$prim49297, align 8
%stackaddr$prim49298 = alloca %struct.ScmObj*, align 8
%current_45args48427 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48426)
store volatile %struct.ScmObj* %current_45args48427, %struct.ScmObj** %stackaddr$prim49298, align 8
%stackaddr$prim49299 = alloca %struct.ScmObj*, align 8
%_37map40157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48427)
store volatile %struct.ScmObj* %_37map40157, %struct.ScmObj** %stackaddr$prim49299, align 8
%stackaddr$makeclosure49300 = alloca %struct.ScmObj*, align 8
%fptrToInt49301 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41858 to i64
%ae41858 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49301)
store volatile %struct.ScmObj* %ae41858, %struct.ScmObj** %stackaddr$makeclosure49300, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41858, %struct.ScmObj* %_37foldl140115, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41858, %struct.ScmObj* %Ycmb40110, i64 1)
%ae41859 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49302 = alloca %struct.ScmObj*, align 8
%fptrToInt49303 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41860 to i64
%ae41860 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49303)
store volatile %struct.ScmObj* %ae41860, %struct.ScmObj** %stackaddr$makeclosure49302, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41860, %struct.ScmObj* %_37foldr40136, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41860, %struct.ScmObj* %_37foldr140131, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41860, %struct.ScmObj* %_37map140162, i64 2)
%args48822$ae41858$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49304 = alloca %struct.ScmObj*, align 8
%args48822$ae41858$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41860, %struct.ScmObj* %args48822$ae41858$0)
store volatile %struct.ScmObj* %args48822$ae41858$1, %struct.ScmObj** %stackaddr$prim49304, align 8
%stackaddr$prim49305 = alloca %struct.ScmObj*, align 8
%args48822$ae41858$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41859, %struct.ScmObj* %args48822$ae41858$1)
store volatile %struct.ScmObj* %args48822$ae41858$2, %struct.ScmObj** %stackaddr$prim49305, align 8
%clofunc49306 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41858)
musttail call tailcc void %clofunc49306(%struct.ScmObj* %ae41858, %struct.ScmObj* %args48822$ae41858$2)
ret void
}

define tailcc void @proc_clo$ae41858(%struct.ScmObj* %env$ae41858,%struct.ScmObj* %current_45args48429) {
%stackaddr$env-ref49307 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41858, i64 0)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref49307
%stackaddr$env-ref49308 = alloca %struct.ScmObj*, align 8
%Ycmb40110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41858, i64 1)
store %struct.ScmObj* %Ycmb40110, %struct.ScmObj** %stackaddr$env-ref49308
%stackaddr$prim49309 = alloca %struct.ScmObj*, align 8
%_95k40384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48429)
store volatile %struct.ScmObj* %_95k40384, %struct.ScmObj** %stackaddr$prim49309, align 8
%stackaddr$prim49310 = alloca %struct.ScmObj*, align 8
%current_45args48430 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48429)
store volatile %struct.ScmObj* %current_45args48430, %struct.ScmObj** %stackaddr$prim49310, align 8
%stackaddr$prim49311 = alloca %struct.ScmObj*, align 8
%anf_45bind40307 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48430)
store volatile %struct.ScmObj* %anf_45bind40307, %struct.ScmObj** %stackaddr$prim49311, align 8
%stackaddr$makeclosure49312 = alloca %struct.ScmObj*, align 8
%fptrToInt49313 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42250 to i64
%ae42250 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49313)
store volatile %struct.ScmObj* %ae42250, %struct.ScmObj** %stackaddr$makeclosure49312, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42250, %struct.ScmObj* %_37foldl140115, i64 0)
%args48762$Ycmb40110$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49314 = alloca %struct.ScmObj*, align 8
%args48762$Ycmb40110$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40307, %struct.ScmObj* %args48762$Ycmb40110$0)
store volatile %struct.ScmObj* %args48762$Ycmb40110$1, %struct.ScmObj** %stackaddr$prim49314, align 8
%stackaddr$prim49315 = alloca %struct.ScmObj*, align 8
%args48762$Ycmb40110$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42250, %struct.ScmObj* %args48762$Ycmb40110$1)
store volatile %struct.ScmObj* %args48762$Ycmb40110$2, %struct.ScmObj** %stackaddr$prim49315, align 8
%clofunc49316 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40110)
musttail call tailcc void %clofunc49316(%struct.ScmObj* %Ycmb40110, %struct.ScmObj* %args48762$Ycmb40110$2)
ret void
}

define tailcc void @proc_clo$ae42250(%struct.ScmObj* %env$ae42250,%struct.ScmObj* %current_45args48432) {
%stackaddr$env-ref49317 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42250, i64 0)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref49317
%stackaddr$prim49318 = alloca %struct.ScmObj*, align 8
%_95k40385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48432)
store volatile %struct.ScmObj* %_95k40385, %struct.ScmObj** %stackaddr$prim49318, align 8
%stackaddr$prim49319 = alloca %struct.ScmObj*, align 8
%current_45args48433 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48432)
store volatile %struct.ScmObj* %current_45args48433, %struct.ScmObj** %stackaddr$prim49319, align 8
%stackaddr$prim49320 = alloca %struct.ScmObj*, align 8
%_37foldl40213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48433)
store volatile %struct.ScmObj* %_37foldl40213, %struct.ScmObj** %stackaddr$prim49320, align 8
%stackaddr$makeclosure49321 = alloca %struct.ScmObj*, align 8
%fptrToInt49322 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42252 to i64
%ae42252 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49322)
store volatile %struct.ScmObj* %ae42252, %struct.ScmObj** %stackaddr$makeclosure49321, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42252, %struct.ScmObj* %_37foldl140115, i64 0)
%ae42253 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49323 = alloca %struct.ScmObj*, align 8
%fptrToInt49324 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42254 to i64
%ae42254 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49324)
store volatile %struct.ScmObj* %ae42254, %struct.ScmObj** %stackaddr$makeclosure49323, align 8
%args48761$ae42252$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49325 = alloca %struct.ScmObj*, align 8
%args48761$ae42252$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42254, %struct.ScmObj* %args48761$ae42252$0)
store volatile %struct.ScmObj* %args48761$ae42252$1, %struct.ScmObj** %stackaddr$prim49325, align 8
%stackaddr$prim49326 = alloca %struct.ScmObj*, align 8
%args48761$ae42252$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42253, %struct.ScmObj* %args48761$ae42252$1)
store volatile %struct.ScmObj* %args48761$ae42252$2, %struct.ScmObj** %stackaddr$prim49326, align 8
%clofunc49327 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42252)
musttail call tailcc void %clofunc49327(%struct.ScmObj* %ae42252, %struct.ScmObj* %args48761$ae42252$2)
ret void
}

define tailcc void @proc_clo$ae42252(%struct.ScmObj* %env$ae42252,%struct.ScmObj* %current_45args48435) {
%stackaddr$env-ref49328 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42252, i64 0)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref49328
%stackaddr$prim49329 = alloca %struct.ScmObj*, align 8
%_95k40386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48435)
store volatile %struct.ScmObj* %_95k40386, %struct.ScmObj** %stackaddr$prim49329, align 8
%stackaddr$prim49330 = alloca %struct.ScmObj*, align 8
%current_45args48436 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48435)
store volatile %struct.ScmObj* %current_45args48436, %struct.ScmObj** %stackaddr$prim49330, align 8
%stackaddr$prim49331 = alloca %struct.ScmObj*, align 8
%_37_6240210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48436)
store volatile %struct.ScmObj* %_37_6240210, %struct.ScmObj** %stackaddr$prim49331, align 8
%stackaddr$makeclosure49332 = alloca %struct.ScmObj*, align 8
%fptrToInt49333 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42276 to i64
%ae42276 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49333)
store volatile %struct.ScmObj* %ae42276, %struct.ScmObj** %stackaddr$makeclosure49332, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42276, %struct.ScmObj* %_37foldl140115, i64 0)
%ae42277 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49334 = alloca %struct.ScmObj*, align 8
%fptrToInt49335 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42278 to i64
%ae42278 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49335)
store volatile %struct.ScmObj* %ae42278, %struct.ScmObj** %stackaddr$makeclosure49334, align 8
%args48755$ae42276$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49336 = alloca %struct.ScmObj*, align 8
%args48755$ae42276$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42278, %struct.ScmObj* %args48755$ae42276$0)
store volatile %struct.ScmObj* %args48755$ae42276$1, %struct.ScmObj** %stackaddr$prim49336, align 8
%stackaddr$prim49337 = alloca %struct.ScmObj*, align 8
%args48755$ae42276$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42277, %struct.ScmObj* %args48755$ae42276$1)
store volatile %struct.ScmObj* %args48755$ae42276$2, %struct.ScmObj** %stackaddr$prim49337, align 8
%clofunc49338 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42276)
musttail call tailcc void %clofunc49338(%struct.ScmObj* %ae42276, %struct.ScmObj* %args48755$ae42276$2)
ret void
}

define tailcc void @proc_clo$ae42276(%struct.ScmObj* %env$ae42276,%struct.ScmObj* %current_45args48438) {
%stackaddr$env-ref49339 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42276, i64 0)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref49339
%stackaddr$prim49340 = alloca %struct.ScmObj*, align 8
%_95k40387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48438)
store volatile %struct.ScmObj* %_95k40387, %struct.ScmObj** %stackaddr$prim49340, align 8
%stackaddr$prim49341 = alloca %struct.ScmObj*, align 8
%current_45args48439 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48438)
store volatile %struct.ScmObj* %current_45args48439, %struct.ScmObj** %stackaddr$prim49341, align 8
%stackaddr$prim49342 = alloca %struct.ScmObj*, align 8
%_37_62_6140207 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48439)
store volatile %struct.ScmObj* %_37_62_6140207, %struct.ScmObj** %stackaddr$prim49342, align 8
%ae42300 = call %struct.ScmObj* @const_init_int(i64 1)
%ae42301 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49343 = alloca %struct.ScmObj*, align 8
%_37append40203 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42300, %struct.ScmObj* %ae42301)
store volatile %struct.ScmObj* %_37append40203, %struct.ScmObj** %stackaddr$prim49343, align 8
%stackaddr$makeclosure49344 = alloca %struct.ScmObj*, align 8
%fptrToInt49345 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42302 to i64
%ae42302 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49345)
store volatile %struct.ScmObj* %ae42302, %struct.ScmObj** %stackaddr$makeclosure49344, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42302, %struct.ScmObj* %_37foldl140115, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42302, %struct.ScmObj* %_37append40203, i64 1)
%ae42303 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49346 = alloca %struct.ScmObj*, align 8
%fptrToInt49347 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42304 to i64
%ae42304 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49347)
store volatile %struct.ScmObj* %ae42304, %struct.ScmObj** %stackaddr$makeclosure49346, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42304, %struct.ScmObj* %_37append40203, i64 0)
%args48749$ae42302$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49348 = alloca %struct.ScmObj*, align 8
%args48749$ae42302$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42304, %struct.ScmObj* %args48749$ae42302$0)
store volatile %struct.ScmObj* %args48749$ae42302$1, %struct.ScmObj** %stackaddr$prim49348, align 8
%stackaddr$prim49349 = alloca %struct.ScmObj*, align 8
%args48749$ae42302$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42303, %struct.ScmObj* %args48749$ae42302$1)
store volatile %struct.ScmObj* %args48749$ae42302$2, %struct.ScmObj** %stackaddr$prim49349, align 8
%clofunc49350 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42302)
musttail call tailcc void %clofunc49350(%struct.ScmObj* %ae42302, %struct.ScmObj* %args48749$ae42302$2)
ret void
}

define tailcc void @proc_clo$ae42302(%struct.ScmObj* %env$ae42302,%struct.ScmObj* %current_45args48441) {
%stackaddr$env-ref49351 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42302, i64 0)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref49351
%stackaddr$env-ref49352 = alloca %struct.ScmObj*, align 8
%_37append40203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42302, i64 1)
store %struct.ScmObj* %_37append40203, %struct.ScmObj** %stackaddr$env-ref49352
%stackaddr$prim49353 = alloca %struct.ScmObj*, align 8
%_95k40388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48441)
store volatile %struct.ScmObj* %_95k40388, %struct.ScmObj** %stackaddr$prim49353, align 8
%stackaddr$prim49354 = alloca %struct.ScmObj*, align 8
%current_45args48442 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48441)
store volatile %struct.ScmObj* %current_45args48442, %struct.ScmObj** %stackaddr$prim49354, align 8
%stackaddr$prim49355 = alloca %struct.ScmObj*, align 8
%anf_45bind40315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48442)
store volatile %struct.ScmObj* %anf_45bind40315, %struct.ScmObj** %stackaddr$prim49355, align 8
%ae42370 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49356 = alloca %struct.ScmObj*, align 8
%_95040204 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append40203, %struct.ScmObj* %ae42370, %struct.ScmObj* %anf_45bind40315)
store volatile %struct.ScmObj* %_95040204, %struct.ScmObj** %stackaddr$prim49356, align 8
%ae42373 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49357 = alloca %struct.ScmObj*, align 8
%_37append40202 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40203, %struct.ScmObj* %ae42373)
store volatile %struct.ScmObj* %_37append40202, %struct.ScmObj** %stackaddr$prim49357, align 8
%stackaddr$makeclosure49358 = alloca %struct.ScmObj*, align 8
%fptrToInt49359 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42374 to i64
%ae42374 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49359)
store volatile %struct.ScmObj* %ae42374, %struct.ScmObj** %stackaddr$makeclosure49358, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42374, %struct.ScmObj* %_37foldl140115, i64 0)
%ae42375 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49360 = alloca %struct.ScmObj*, align 8
%fptrToInt49361 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42376 to i64
%ae42376 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49361)
store volatile %struct.ScmObj* %ae42376, %struct.ScmObj** %stackaddr$makeclosure49360, align 8
%args48738$ae42374$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49362 = alloca %struct.ScmObj*, align 8
%args48738$ae42374$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42376, %struct.ScmObj* %args48738$ae42374$0)
store volatile %struct.ScmObj* %args48738$ae42374$1, %struct.ScmObj** %stackaddr$prim49362, align 8
%stackaddr$prim49363 = alloca %struct.ScmObj*, align 8
%args48738$ae42374$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42375, %struct.ScmObj* %args48738$ae42374$1)
store volatile %struct.ScmObj* %args48738$ae42374$2, %struct.ScmObj** %stackaddr$prim49363, align 8
%clofunc49364 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42374)
musttail call tailcc void %clofunc49364(%struct.ScmObj* %ae42374, %struct.ScmObj* %args48738$ae42374$2)
ret void
}

define tailcc void @proc_clo$ae42374(%struct.ScmObj* %env$ae42374,%struct.ScmObj* %current_45args48444) {
%stackaddr$env-ref49365 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42374, i64 0)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref49365
%stackaddr$prim49366 = alloca %struct.ScmObj*, align 8
%_95k40389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48444)
store volatile %struct.ScmObj* %_95k40389, %struct.ScmObj** %stackaddr$prim49366, align 8
%stackaddr$prim49367 = alloca %struct.ScmObj*, align 8
%current_45args48445 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48444)
store volatile %struct.ScmObj* %current_45args48445, %struct.ScmObj** %stackaddr$prim49367, align 8
%stackaddr$prim49368 = alloca %struct.ScmObj*, align 8
%_37list_6340195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48445)
store volatile %struct.ScmObj* %_37list_6340195, %struct.ScmObj** %stackaddr$prim49368, align 8
%stackaddr$makeclosure49369 = alloca %struct.ScmObj*, align 8
%fptrToInt49370 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42790 to i64
%ae42790 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49370)
store volatile %struct.ScmObj* %ae42790, %struct.ScmObj** %stackaddr$makeclosure49369, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42790, %struct.ScmObj* %_37foldl140115, i64 0)
%ae42791 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49371 = alloca %struct.ScmObj*, align 8
%fptrToInt49372 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42792 to i64
%ae42792 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49372)
store volatile %struct.ScmObj* %ae42792, %struct.ScmObj** %stackaddr$makeclosure49371, align 8
%args48713$ae42790$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49373 = alloca %struct.ScmObj*, align 8
%args48713$ae42790$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42792, %struct.ScmObj* %args48713$ae42790$0)
store volatile %struct.ScmObj* %args48713$ae42790$1, %struct.ScmObj** %stackaddr$prim49373, align 8
%stackaddr$prim49374 = alloca %struct.ScmObj*, align 8
%args48713$ae42790$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42791, %struct.ScmObj* %args48713$ae42790$1)
store volatile %struct.ScmObj* %args48713$ae42790$2, %struct.ScmObj** %stackaddr$prim49374, align 8
%clofunc49375 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42790)
musttail call tailcc void %clofunc49375(%struct.ScmObj* %ae42790, %struct.ScmObj* %args48713$ae42790$2)
ret void
}

define tailcc void @proc_clo$ae42790(%struct.ScmObj* %env$ae42790,%struct.ScmObj* %current_45args48447) {
%stackaddr$env-ref49376 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42790, i64 0)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref49376
%stackaddr$prim49377 = alloca %struct.ScmObj*, align 8
%_95k40390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48447)
store volatile %struct.ScmObj* %_95k40390, %struct.ScmObj** %stackaddr$prim49377, align 8
%stackaddr$prim49378 = alloca %struct.ScmObj*, align 8
%current_45args48448 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48447)
store volatile %struct.ScmObj* %current_45args48448, %struct.ScmObj** %stackaddr$prim49378, align 8
%stackaddr$prim49379 = alloca %struct.ScmObj*, align 8
%_37drop40186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48448)
store volatile %struct.ScmObj* %_37drop40186, %struct.ScmObj** %stackaddr$prim49379, align 8
%stackaddr$makeclosure49380 = alloca %struct.ScmObj*, align 8
%fptrToInt49381 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43326 to i64
%ae43326 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49381)
store volatile %struct.ScmObj* %ae43326, %struct.ScmObj** %stackaddr$makeclosure49380, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43326, %struct.ScmObj* %_37foldl140115, i64 0)
%ae43327 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49382 = alloca %struct.ScmObj*, align 8
%fptrToInt49383 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43328 to i64
%ae43328 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49383)
store volatile %struct.ScmObj* %ae43328, %struct.ScmObj** %stackaddr$makeclosure49382, align 8
%args48689$ae43326$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49384 = alloca %struct.ScmObj*, align 8
%args48689$ae43326$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43328, %struct.ScmObj* %args48689$ae43326$0)
store volatile %struct.ScmObj* %args48689$ae43326$1, %struct.ScmObj** %stackaddr$prim49384, align 8
%stackaddr$prim49385 = alloca %struct.ScmObj*, align 8
%args48689$ae43326$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43327, %struct.ScmObj* %args48689$ae43326$1)
store volatile %struct.ScmObj* %args48689$ae43326$2, %struct.ScmObj** %stackaddr$prim49385, align 8
%clofunc49386 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43326)
musttail call tailcc void %clofunc49386(%struct.ScmObj* %ae43326, %struct.ScmObj* %args48689$ae43326$2)
ret void
}

define tailcc void @proc_clo$ae43326(%struct.ScmObj* %env$ae43326,%struct.ScmObj* %current_45args48450) {
%stackaddr$env-ref49387 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43326, i64 0)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref49387
%stackaddr$prim49388 = alloca %struct.ScmObj*, align 8
%_95k40391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48450)
store volatile %struct.ScmObj* %_95k40391, %struct.ScmObj** %stackaddr$prim49388, align 8
%stackaddr$prim49389 = alloca %struct.ScmObj*, align 8
%current_45args48451 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48450)
store volatile %struct.ScmObj* %current_45args48451, %struct.ScmObj** %stackaddr$prim49389, align 8
%stackaddr$prim49390 = alloca %struct.ScmObj*, align 8
%_37memv40179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48451)
store volatile %struct.ScmObj* %_37memv40179, %struct.ScmObj** %stackaddr$prim49390, align 8
%stackaddr$makeclosure49391 = alloca %struct.ScmObj*, align 8
%fptrToInt49392 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43730 to i64
%ae43730 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49392)
store volatile %struct.ScmObj* %ae43730, %struct.ScmObj** %stackaddr$makeclosure49391, align 8
%ae43731 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49393 = alloca %struct.ScmObj*, align 8
%fptrToInt49394 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43732 to i64
%ae43732 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49394)
store volatile %struct.ScmObj* %ae43732, %struct.ScmObj** %stackaddr$makeclosure49393, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43732, %struct.ScmObj* %_37foldl140115, i64 0)
%args48663$ae43730$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49395 = alloca %struct.ScmObj*, align 8
%args48663$ae43730$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43732, %struct.ScmObj* %args48663$ae43730$0)
store volatile %struct.ScmObj* %args48663$ae43730$1, %struct.ScmObj** %stackaddr$prim49395, align 8
%stackaddr$prim49396 = alloca %struct.ScmObj*, align 8
%args48663$ae43730$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43731, %struct.ScmObj* %args48663$ae43730$1)
store volatile %struct.ScmObj* %args48663$ae43730$2, %struct.ScmObj** %stackaddr$prim49396, align 8
%clofunc49397 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43730)
musttail call tailcc void %clofunc49397(%struct.ScmObj* %ae43730, %struct.ScmObj* %args48663$ae43730$2)
ret void
}

define tailcc void @proc_clo$ae43730(%struct.ScmObj* %env$ae43730,%struct.ScmObj* %current_45args48453) {
%stackaddr$prim49398 = alloca %struct.ScmObj*, align 8
%_95k40392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48453)
store volatile %struct.ScmObj* %_95k40392, %struct.ScmObj** %stackaddr$prim49398, align 8
%stackaddr$prim49399 = alloca %struct.ScmObj*, align 8
%current_45args48454 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48453)
store volatile %struct.ScmObj* %current_45args48454, %struct.ScmObj** %stackaddr$prim49399, align 8
%stackaddr$prim49400 = alloca %struct.ScmObj*, align 8
%_37_4740175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48454)
store volatile %struct.ScmObj* %_37_4740175, %struct.ScmObj** %stackaddr$prim49400, align 8
%stackaddr$makeclosure49401 = alloca %struct.ScmObj*, align 8
%fptrToInt49402 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43828 to i64
%ae43828 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49402)
store volatile %struct.ScmObj* %ae43828, %struct.ScmObj** %stackaddr$makeclosure49401, align 8
%ae43829 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49403 = alloca %struct.ScmObj*, align 8
%fptrToInt49404 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43830 to i64
%ae43830 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49404)
store volatile %struct.ScmObj* %ae43830, %struct.ScmObj** %stackaddr$makeclosure49403, align 8
%args48650$ae43828$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49405 = alloca %struct.ScmObj*, align 8
%args48650$ae43828$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43830, %struct.ScmObj* %args48650$ae43828$0)
store volatile %struct.ScmObj* %args48650$ae43828$1, %struct.ScmObj** %stackaddr$prim49405, align 8
%stackaddr$prim49406 = alloca %struct.ScmObj*, align 8
%args48650$ae43828$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43829, %struct.ScmObj* %args48650$ae43828$1)
store volatile %struct.ScmObj* %args48650$ae43828$2, %struct.ScmObj** %stackaddr$prim49406, align 8
%clofunc49407 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43828)
musttail call tailcc void %clofunc49407(%struct.ScmObj* %ae43828, %struct.ScmObj* %args48650$ae43828$2)
ret void
}

define tailcc void @proc_clo$ae43828(%struct.ScmObj* %env$ae43828,%struct.ScmObj* %current_45args48456) {
%stackaddr$prim49408 = alloca %struct.ScmObj*, align 8
%_95k40393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48456)
store volatile %struct.ScmObj* %_95k40393, %struct.ScmObj** %stackaddr$prim49408, align 8
%stackaddr$prim49409 = alloca %struct.ScmObj*, align 8
%current_45args48457 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48456)
store volatile %struct.ScmObj* %current_45args48457, %struct.ScmObj** %stackaddr$prim49409, align 8
%stackaddr$prim49410 = alloca %struct.ScmObj*, align 8
%_37first40173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48457)
store volatile %struct.ScmObj* %_37first40173, %struct.ScmObj** %stackaddr$prim49410, align 8
%stackaddr$makeclosure49411 = alloca %struct.ScmObj*, align 8
%fptrToInt49412 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43848 to i64
%ae43848 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49412)
store volatile %struct.ScmObj* %ae43848, %struct.ScmObj** %stackaddr$makeclosure49411, align 8
%ae43849 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49413 = alloca %struct.ScmObj*, align 8
%fptrToInt49414 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43850 to i64
%ae43850 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49414)
store volatile %struct.ScmObj* %ae43850, %struct.ScmObj** %stackaddr$makeclosure49413, align 8
%args48645$ae43848$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49415 = alloca %struct.ScmObj*, align 8
%args48645$ae43848$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43850, %struct.ScmObj* %args48645$ae43848$0)
store volatile %struct.ScmObj* %args48645$ae43848$1, %struct.ScmObj** %stackaddr$prim49415, align 8
%stackaddr$prim49416 = alloca %struct.ScmObj*, align 8
%args48645$ae43848$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43849, %struct.ScmObj* %args48645$ae43848$1)
store volatile %struct.ScmObj* %args48645$ae43848$2, %struct.ScmObj** %stackaddr$prim49416, align 8
%clofunc49417 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43848)
musttail call tailcc void %clofunc49417(%struct.ScmObj* %ae43848, %struct.ScmObj* %args48645$ae43848$2)
ret void
}

define tailcc void @proc_clo$ae43848(%struct.ScmObj* %env$ae43848,%struct.ScmObj* %current_45args48459) {
%stackaddr$prim49418 = alloca %struct.ScmObj*, align 8
%_95k40394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48459)
store volatile %struct.ScmObj* %_95k40394, %struct.ScmObj** %stackaddr$prim49418, align 8
%stackaddr$prim49419 = alloca %struct.ScmObj*, align 8
%current_45args48460 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48459)
store volatile %struct.ScmObj* %current_45args48460, %struct.ScmObj** %stackaddr$prim49419, align 8
%stackaddr$prim49420 = alloca %struct.ScmObj*, align 8
%_37second40171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48460)
store volatile %struct.ScmObj* %_37second40171, %struct.ScmObj** %stackaddr$prim49420, align 8
%stackaddr$makeclosure49421 = alloca %struct.ScmObj*, align 8
%fptrToInt49422 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43870 to i64
%ae43870 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49422)
store volatile %struct.ScmObj* %ae43870, %struct.ScmObj** %stackaddr$makeclosure49421, align 8
%ae43871 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49423 = alloca %struct.ScmObj*, align 8
%fptrToInt49424 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43872 to i64
%ae43872 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49424)
store volatile %struct.ScmObj* %ae43872, %struct.ScmObj** %stackaddr$makeclosure49423, align 8
%args48640$ae43870$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49425 = alloca %struct.ScmObj*, align 8
%args48640$ae43870$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43872, %struct.ScmObj* %args48640$ae43870$0)
store volatile %struct.ScmObj* %args48640$ae43870$1, %struct.ScmObj** %stackaddr$prim49425, align 8
%stackaddr$prim49426 = alloca %struct.ScmObj*, align 8
%args48640$ae43870$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43871, %struct.ScmObj* %args48640$ae43870$1)
store volatile %struct.ScmObj* %args48640$ae43870$2, %struct.ScmObj** %stackaddr$prim49426, align 8
%clofunc49427 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43870)
musttail call tailcc void %clofunc49427(%struct.ScmObj* %ae43870, %struct.ScmObj* %args48640$ae43870$2)
ret void
}

define tailcc void @proc_clo$ae43870(%struct.ScmObj* %env$ae43870,%struct.ScmObj* %current_45args48462) {
%stackaddr$prim49428 = alloca %struct.ScmObj*, align 8
%_95k40395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48462)
store volatile %struct.ScmObj* %_95k40395, %struct.ScmObj** %stackaddr$prim49428, align 8
%stackaddr$prim49429 = alloca %struct.ScmObj*, align 8
%current_45args48463 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48462)
store volatile %struct.ScmObj* %current_45args48463, %struct.ScmObj** %stackaddr$prim49429, align 8
%stackaddr$prim49430 = alloca %struct.ScmObj*, align 8
%_37third40169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48463)
store volatile %struct.ScmObj* %_37third40169, %struct.ScmObj** %stackaddr$prim49430, align 8
%stackaddr$makeclosure49431 = alloca %struct.ScmObj*, align 8
%fptrToInt49432 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43894 to i64
%ae43894 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49432)
store volatile %struct.ScmObj* %ae43894, %struct.ScmObj** %stackaddr$makeclosure49431, align 8
%ae43895 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49433 = alloca %struct.ScmObj*, align 8
%fptrToInt49434 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43896 to i64
%ae43896 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49434)
store volatile %struct.ScmObj* %ae43896, %struct.ScmObj** %stackaddr$makeclosure49433, align 8
%args48635$ae43894$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49435 = alloca %struct.ScmObj*, align 8
%args48635$ae43894$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43896, %struct.ScmObj* %args48635$ae43894$0)
store volatile %struct.ScmObj* %args48635$ae43894$1, %struct.ScmObj** %stackaddr$prim49435, align 8
%stackaddr$prim49436 = alloca %struct.ScmObj*, align 8
%args48635$ae43894$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43895, %struct.ScmObj* %args48635$ae43894$1)
store volatile %struct.ScmObj* %args48635$ae43894$2, %struct.ScmObj** %stackaddr$prim49436, align 8
%clofunc49437 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43894)
musttail call tailcc void %clofunc49437(%struct.ScmObj* %ae43894, %struct.ScmObj* %args48635$ae43894$2)
ret void
}

define tailcc void @proc_clo$ae43894(%struct.ScmObj* %env$ae43894,%struct.ScmObj* %current_45args48465) {
%stackaddr$prim49438 = alloca %struct.ScmObj*, align 8
%_95k40396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48465)
store volatile %struct.ScmObj* %_95k40396, %struct.ScmObj** %stackaddr$prim49438, align 8
%stackaddr$prim49439 = alloca %struct.ScmObj*, align 8
%current_45args48466 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48465)
store volatile %struct.ScmObj* %current_45args48466, %struct.ScmObj** %stackaddr$prim49439, align 8
%stackaddr$prim49440 = alloca %struct.ScmObj*, align 8
%_37fourth40167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48466)
store volatile %struct.ScmObj* %_37fourth40167, %struct.ScmObj** %stackaddr$prim49440, align 8
%stackaddr$makeclosure49441 = alloca %struct.ScmObj*, align 8
%fptrToInt49442 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43920 to i64
%ae43920 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49442)
store volatile %struct.ScmObj* %ae43920, %struct.ScmObj** %stackaddr$makeclosure49441, align 8
%ae43921 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49443 = alloca %struct.ScmObj*, align 8
%fptrToInt49444 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43922 to i64
%ae43922 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49444)
store volatile %struct.ScmObj* %ae43922, %struct.ScmObj** %stackaddr$makeclosure49443, align 8
%args48630$ae43920$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49445 = alloca %struct.ScmObj*, align 8
%args48630$ae43920$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43922, %struct.ScmObj* %args48630$ae43920$0)
store volatile %struct.ScmObj* %args48630$ae43920$1, %struct.ScmObj** %stackaddr$prim49445, align 8
%stackaddr$prim49446 = alloca %struct.ScmObj*, align 8
%args48630$ae43920$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43921, %struct.ScmObj* %args48630$ae43920$1)
store volatile %struct.ScmObj* %args48630$ae43920$2, %struct.ScmObj** %stackaddr$prim49446, align 8
%clofunc49447 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43920)
musttail call tailcc void %clofunc49447(%struct.ScmObj* %ae43920, %struct.ScmObj* %args48630$ae43920$2)
ret void
}

define tailcc void @proc_clo$ae43920(%struct.ScmObj* %env$ae43920,%struct.ScmObj* %current_45args48468) {
%stackaddr$prim49448 = alloca %struct.ScmObj*, align 8
%_95k40397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48468)
store volatile %struct.ScmObj* %_95k40397, %struct.ScmObj** %stackaddr$prim49448, align 8
%stackaddr$prim49449 = alloca %struct.ScmObj*, align 8
%current_45args48469 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48468)
store volatile %struct.ScmObj* %current_45args48469, %struct.ScmObj** %stackaddr$prim49449, align 8
%stackaddr$prim49450 = alloca %struct.ScmObj*, align 8
%promise_6340228 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48469)
store volatile %struct.ScmObj* %promise_6340228, %struct.ScmObj** %stackaddr$prim49450, align 8
%stackaddr$makeclosure49451 = alloca %struct.ScmObj*, align 8
%fptrToInt49452 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44007 to i64
%ae44007 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49452)
store volatile %struct.ScmObj* %ae44007, %struct.ScmObj** %stackaddr$makeclosure49451, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44007, %struct.ScmObj* %promise_6340228, i64 0)
%ae44008 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49453 = alloca %struct.ScmObj*, align 8
%fptrToInt49454 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44009 to i64
%ae44009 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49454)
store volatile %struct.ScmObj* %ae44009, %struct.ScmObj** %stackaddr$makeclosure49453, align 8
%args48623$ae44007$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49455 = alloca %struct.ScmObj*, align 8
%args48623$ae44007$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44009, %struct.ScmObj* %args48623$ae44007$0)
store volatile %struct.ScmObj* %args48623$ae44007$1, %struct.ScmObj** %stackaddr$prim49455, align 8
%stackaddr$prim49456 = alloca %struct.ScmObj*, align 8
%args48623$ae44007$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44008, %struct.ScmObj* %args48623$ae44007$1)
store volatile %struct.ScmObj* %args48623$ae44007$2, %struct.ScmObj** %stackaddr$prim49456, align 8
%clofunc49457 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44007)
musttail call tailcc void %clofunc49457(%struct.ScmObj* %ae44007, %struct.ScmObj* %args48623$ae44007$2)
ret void
}

define tailcc void @proc_clo$ae44007(%struct.ScmObj* %env$ae44007,%struct.ScmObj* %current_45args48471) {
%stackaddr$env-ref49458 = alloca %struct.ScmObj*, align 8
%promise_6340228 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44007, i64 0)
store %struct.ScmObj* %promise_6340228, %struct.ScmObj** %stackaddr$env-ref49458
%stackaddr$prim49459 = alloca %struct.ScmObj*, align 8
%_95k40398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48471)
store volatile %struct.ScmObj* %_95k40398, %struct.ScmObj** %stackaddr$prim49459, align 8
%stackaddr$prim49460 = alloca %struct.ScmObj*, align 8
%current_45args48472 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48471)
store volatile %struct.ScmObj* %current_45args48472, %struct.ScmObj** %stackaddr$prim49460, align 8
%stackaddr$prim49461 = alloca %struct.ScmObj*, align 8
%anf_45bind40355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48472)
store volatile %struct.ScmObj* %anf_45bind40355, %struct.ScmObj** %stackaddr$prim49461, align 8
%stackaddr$makeclosure49462 = alloca %struct.ScmObj*, align 8
%fptrToInt49463 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44031 to i64
%ae44031 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49463)
store volatile %struct.ScmObj* %ae44031, %struct.ScmObj** %stackaddr$makeclosure49462, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44031, %struct.ScmObj* %promise_6340228, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44031, %struct.ScmObj* %anf_45bind40355, i64 1)
%ae44032 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49464 = alloca %struct.ScmObj*, align 8
%fptrToInt49465 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44033 to i64
%ae44033 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49465)
store volatile %struct.ScmObj* %ae44033, %struct.ScmObj** %stackaddr$makeclosure49464, align 8
%args48621$ae44031$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49466 = alloca %struct.ScmObj*, align 8
%args48621$ae44031$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44033, %struct.ScmObj* %args48621$ae44031$0)
store volatile %struct.ScmObj* %args48621$ae44031$1, %struct.ScmObj** %stackaddr$prim49466, align 8
%stackaddr$prim49467 = alloca %struct.ScmObj*, align 8
%args48621$ae44031$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44032, %struct.ScmObj* %args48621$ae44031$1)
store volatile %struct.ScmObj* %args48621$ae44031$2, %struct.ScmObj** %stackaddr$prim49467, align 8
%clofunc49468 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44031)
musttail call tailcc void %clofunc49468(%struct.ScmObj* %ae44031, %struct.ScmObj* %args48621$ae44031$2)
ret void
}

define tailcc void @proc_clo$ae44031(%struct.ScmObj* %env$ae44031,%struct.ScmObj* %current_45args48474) {
%stackaddr$env-ref49469 = alloca %struct.ScmObj*, align 8
%promise_6340228 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44031, i64 0)
store %struct.ScmObj* %promise_6340228, %struct.ScmObj** %stackaddr$env-ref49469
%stackaddr$env-ref49470 = alloca %struct.ScmObj*, align 8
%anf_45bind40355 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44031, i64 1)
store %struct.ScmObj* %anf_45bind40355, %struct.ScmObj** %stackaddr$env-ref49470
%stackaddr$prim49471 = alloca %struct.ScmObj*, align 8
%_95k40399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48474)
store volatile %struct.ScmObj* %_95k40399, %struct.ScmObj** %stackaddr$prim49471, align 8
%stackaddr$prim49472 = alloca %struct.ScmObj*, align 8
%current_45args48475 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48474)
store volatile %struct.ScmObj* %current_45args48475, %struct.ScmObj** %stackaddr$prim49472, align 8
%stackaddr$prim49473 = alloca %struct.ScmObj*, align 8
%anf_45bind40356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48475)
store volatile %struct.ScmObj* %anf_45bind40356, %struct.ScmObj** %stackaddr$prim49473, align 8
%stackaddr$makeclosure49474 = alloca %struct.ScmObj*, align 8
%fptrToInt49475 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44053 to i64
%ae44053 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49475)
store volatile %struct.ScmObj* %ae44053, %struct.ScmObj** %stackaddr$makeclosure49474, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44053, %struct.ScmObj* %promise_6340228, i64 0)
%ae44054 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae4405449476, i32 0, i32 0))
%ae44055 = call %struct.ScmObj* @const_init_false()
%args48616$anf_45bind40355$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49477 = alloca %struct.ScmObj*, align 8
%args48616$anf_45bind40355$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40356, %struct.ScmObj* %args48616$anf_45bind40355$0)
store volatile %struct.ScmObj* %args48616$anf_45bind40355$1, %struct.ScmObj** %stackaddr$prim49477, align 8
%stackaddr$prim49478 = alloca %struct.ScmObj*, align 8
%args48616$anf_45bind40355$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44055, %struct.ScmObj* %args48616$anf_45bind40355$1)
store volatile %struct.ScmObj* %args48616$anf_45bind40355$2, %struct.ScmObj** %stackaddr$prim49478, align 8
%stackaddr$prim49479 = alloca %struct.ScmObj*, align 8
%args48616$anf_45bind40355$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44054, %struct.ScmObj* %args48616$anf_45bind40355$2)
store volatile %struct.ScmObj* %args48616$anf_45bind40355$3, %struct.ScmObj** %stackaddr$prim49479, align 8
%stackaddr$prim49480 = alloca %struct.ScmObj*, align 8
%args48616$anf_45bind40355$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44053, %struct.ScmObj* %args48616$anf_45bind40355$3)
store volatile %struct.ScmObj* %args48616$anf_45bind40355$4, %struct.ScmObj** %stackaddr$prim49480, align 8
%clofunc49481 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40355)
musttail call tailcc void %clofunc49481(%struct.ScmObj* %anf_45bind40355, %struct.ScmObj* %args48616$anf_45bind40355$4)
ret void
}

define tailcc void @proc_clo$ae44053(%struct.ScmObj* %env$ae44053,%struct.ScmObj* %current_45args48477) {
%stackaddr$env-ref49482 = alloca %struct.ScmObj*, align 8
%promise_6340228 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44053, i64 0)
store %struct.ScmObj* %promise_6340228, %struct.ScmObj** %stackaddr$env-ref49482
%stackaddr$prim49483 = alloca %struct.ScmObj*, align 8
%_95k40400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48477)
store volatile %struct.ScmObj* %_95k40400, %struct.ScmObj** %stackaddr$prim49483, align 8
%stackaddr$prim49484 = alloca %struct.ScmObj*, align 8
%current_45args48478 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48477)
store volatile %struct.ScmObj* %current_45args48478, %struct.ScmObj** %stackaddr$prim49484, align 8
%stackaddr$prim49485 = alloca %struct.ScmObj*, align 8
%thunk4010440231 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48478)
store volatile %struct.ScmObj* %thunk4010440231, %struct.ScmObj** %stackaddr$prim49485, align 8
%stackaddr$makeclosure49486 = alloca %struct.ScmObj*, align 8
%fptrToInt49487 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44068 to i64
%ae44068 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49487)
store volatile %struct.ScmObj* %ae44068, %struct.ScmObj** %stackaddr$makeclosure49486, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44068, %struct.ScmObj* %thunk4010440231, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44068, %struct.ScmObj* %promise_6340228, i64 1)
%args48615$promise_6340228$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49488 = alloca %struct.ScmObj*, align 8
%args48615$promise_6340228$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4010440231, %struct.ScmObj* %args48615$promise_6340228$0)
store volatile %struct.ScmObj* %args48615$promise_6340228$1, %struct.ScmObj** %stackaddr$prim49488, align 8
%stackaddr$prim49489 = alloca %struct.ScmObj*, align 8
%args48615$promise_6340228$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44068, %struct.ScmObj* %args48615$promise_6340228$1)
store volatile %struct.ScmObj* %args48615$promise_6340228$2, %struct.ScmObj** %stackaddr$prim49489, align 8
%clofunc49490 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %promise_6340228)
musttail call tailcc void %clofunc49490(%struct.ScmObj* %promise_6340228, %struct.ScmObj* %args48615$promise_6340228$2)
ret void
}

define tailcc void @proc_clo$ae44068(%struct.ScmObj* %env$ae44068,%struct.ScmObj* %current_45args48480) {
%stackaddr$env-ref49491 = alloca %struct.ScmObj*, align 8
%thunk4010440231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44068, i64 0)
store %struct.ScmObj* %thunk4010440231, %struct.ScmObj** %stackaddr$env-ref49491
%stackaddr$env-ref49492 = alloca %struct.ScmObj*, align 8
%promise_6340228 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44068, i64 1)
store %struct.ScmObj* %promise_6340228, %struct.ScmObj** %stackaddr$env-ref49492
%stackaddr$prim49493 = alloca %struct.ScmObj*, align 8
%_95k40401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48480)
store volatile %struct.ScmObj* %_95k40401, %struct.ScmObj** %stackaddr$prim49493, align 8
%stackaddr$prim49494 = alloca %struct.ScmObj*, align 8
%current_45args48481 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48480)
store volatile %struct.ScmObj* %current_45args48481, %struct.ScmObj** %stackaddr$prim49494, align 8
%stackaddr$prim49495 = alloca %struct.ScmObj*, align 8
%anf_45bind40357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48481)
store volatile %struct.ScmObj* %anf_45bind40357, %struct.ScmObj** %stackaddr$prim49495, align 8
%truthy$cmp49496 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40357)
%cmp$cmp49496 = icmp eq i64 %truthy$cmp49496, 1
br i1 %cmp$cmp49496, label %truebranch$cmp49496, label %falsebranch$cmp49496
truebranch$cmp49496:
%ae44072 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49497 = alloca %struct.ScmObj*, align 8
%anf_45bind40358 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4010440231, %struct.ScmObj* %ae44072)
store volatile %struct.ScmObj* %anf_45bind40358, %struct.ScmObj** %stackaddr$prim49497, align 8
%truthy$cmp49498 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40358)
%cmp$cmp49498 = icmp eq i64 %truthy$cmp49498, 1
br i1 %cmp$cmp49498, label %truebranch$cmp49498, label %falsebranch$cmp49498
truebranch$cmp49498:
%ae44075 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49499 = alloca %struct.ScmObj*, align 8
%cpsprim40407 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4010440231, %struct.ScmObj* %ae44075)
store volatile %struct.ScmObj* %cpsprim40407, %struct.ScmObj** %stackaddr$prim49499, align 8
%stackaddr$makeclosure49500 = alloca %struct.ScmObj*, align 8
%fptrToInt49501 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44076 to i64
%ae44076 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49501)
store volatile %struct.ScmObj* %ae44076, %struct.ScmObj** %stackaddr$makeclosure49500, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44076, %struct.ScmObj* %promise_6340228, i64 0)
%ae44077 = call %struct.ScmObj* @const_init_int(i64 0)
%args48514$ae44076$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49502 = alloca %struct.ScmObj*, align 8
%args48514$ae44076$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40407, %struct.ScmObj* %args48514$ae44076$0)
store volatile %struct.ScmObj* %args48514$ae44076$1, %struct.ScmObj** %stackaddr$prim49502, align 8
%stackaddr$prim49503 = alloca %struct.ScmObj*, align 8
%args48514$ae44076$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44077, %struct.ScmObj* %args48514$ae44076$1)
store volatile %struct.ScmObj* %args48514$ae44076$2, %struct.ScmObj** %stackaddr$prim49503, align 8
%clofunc49504 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44076)
musttail call tailcc void %clofunc49504(%struct.ScmObj* %ae44076, %struct.ScmObj* %args48514$ae44076$2)
ret void
falsebranch$cmp49498:
%ae44334 = call %struct.ScmObj* @const_init_int(i64 1)
%ae44335 = call %struct.ScmObj* @const_init_true()
%stackaddr$prim49505 = alloca %struct.ScmObj*, align 8
%t4010640234 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4010440231, %struct.ScmObj* %ae44334, %struct.ScmObj* %ae44335)
store volatile %struct.ScmObj* %t4010640234, %struct.ScmObj** %stackaddr$prim49505, align 8
%ae44337 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49506 = alloca %struct.ScmObj*, align 8
%anf_45bind40359 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4010440231, %struct.ScmObj* %ae44337)
store volatile %struct.ScmObj* %anf_45bind40359, %struct.ScmObj** %stackaddr$prim49506, align 8
%stackaddr$makeclosure49507 = alloca %struct.ScmObj*, align 8
%fptrToInt49508 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44339 to i64
%ae44339 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49508)
store volatile %struct.ScmObj* %ae44339, %struct.ScmObj** %stackaddr$makeclosure49507, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44339, %struct.ScmObj* %thunk4010440231, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44339, %struct.ScmObj* %promise_6340228, i64 1)
%ae44340 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @global$sym$ae4434049509, i32 0, i32 0))
%args48550$anf_45bind40359$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49510 = alloca %struct.ScmObj*, align 8
%args48550$anf_45bind40359$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44340, %struct.ScmObj* %args48550$anf_45bind40359$0)
store volatile %struct.ScmObj* %args48550$anf_45bind40359$1, %struct.ScmObj** %stackaddr$prim49510, align 8
%stackaddr$prim49511 = alloca %struct.ScmObj*, align 8
%args48550$anf_45bind40359$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44339, %struct.ScmObj* %args48550$anf_45bind40359$1)
store volatile %struct.ScmObj* %args48550$anf_45bind40359$2, %struct.ScmObj** %stackaddr$prim49511, align 8
%clofunc49512 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40359)
musttail call tailcc void %clofunc49512(%struct.ScmObj* %anf_45bind40359, %struct.ScmObj* %args48550$anf_45bind40359$2)
ret void
falsebranch$cmp49496:
%stackaddr$prim49513 = alloca %struct.ScmObj*, align 8
%anf_45bind40360 = call %struct.ScmObj* @prim_number_63(%struct.ScmObj* %thunk4010440231)
store volatile %struct.ScmObj* %anf_45bind40360, %struct.ScmObj** %stackaddr$prim49513, align 8
%truthy$cmp49514 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40360)
%cmp$cmp49514 = icmp eq i64 %truthy$cmp49514, 1
br i1 %cmp$cmp49514, label %truebranch$cmp49514, label %falsebranch$cmp49514
truebranch$cmp49514:
%stackaddr$makeclosure49515 = alloca %struct.ScmObj*, align 8
%fptrToInt49516 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44976 to i64
%ae44976 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49516)
store volatile %struct.ScmObj* %ae44976, %struct.ScmObj** %stackaddr$makeclosure49515, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44976, %struct.ScmObj* %promise_6340228, i64 0)
%ae44977 = call %struct.ScmObj* @const_init_int(i64 0)
%args48582$ae44976$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49517 = alloca %struct.ScmObj*, align 8
%args48582$ae44976$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4010440231, %struct.ScmObj* %args48582$ae44976$0)
store volatile %struct.ScmObj* %args48582$ae44976$1, %struct.ScmObj** %stackaddr$prim49517, align 8
%stackaddr$prim49518 = alloca %struct.ScmObj*, align 8
%args48582$ae44976$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44977, %struct.ScmObj* %args48582$ae44976$1)
store volatile %struct.ScmObj* %args48582$ae44976$2, %struct.ScmObj** %stackaddr$prim49518, align 8
%clofunc49519 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44976)
musttail call tailcc void %clofunc49519(%struct.ScmObj* %ae44976, %struct.ScmObj* %args48582$ae44976$2)
ret void
falsebranch$cmp49514:
%stackaddr$makeclosure49520 = alloca %struct.ScmObj*, align 8
%fptrToInt49521 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45180 to i64
%ae45180 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49521)
store volatile %struct.ScmObj* %ae45180, %struct.ScmObj** %stackaddr$makeclosure49520, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae45180, %struct.ScmObj* %promise_6340228, i64 0)
%ae45181 = call %struct.ScmObj* @const_init_int(i64 0)
%ae45182 = call %struct.ScmObj* @const_init_string(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @global$str$ae4518249522, i32 0, i32 0))
%args48614$ae45180$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49523 = alloca %struct.ScmObj*, align 8
%args48614$ae45180$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45182, %struct.ScmObj* %args48614$ae45180$0)
store volatile %struct.ScmObj* %args48614$ae45180$1, %struct.ScmObj** %stackaddr$prim49523, align 8
%stackaddr$prim49524 = alloca %struct.ScmObj*, align 8
%args48614$ae45180$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45181, %struct.ScmObj* %args48614$ae45180$1)
store volatile %struct.ScmObj* %args48614$ae45180$2, %struct.ScmObj** %stackaddr$prim49524, align 8
%clofunc49525 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae45180)
musttail call tailcc void %clofunc49525(%struct.ScmObj* %ae45180, %struct.ScmObj* %args48614$ae45180$2)
ret void
}

define tailcc void @proc_clo$ae44076(%struct.ScmObj* %env$ae44076,%struct.ScmObj* %current_45args48483) {
%stackaddr$env-ref49526 = alloca %struct.ScmObj*, align 8
%promise_6340228 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44076, i64 0)
store %struct.ScmObj* %promise_6340228, %struct.ScmObj** %stackaddr$env-ref49526
%stackaddr$prim49527 = alloca %struct.ScmObj*, align 8
%_95k40402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48483)
store volatile %struct.ScmObj* %_95k40402, %struct.ScmObj** %stackaddr$prim49527, align 8
%stackaddr$prim49528 = alloca %struct.ScmObj*, align 8
%current_45args48484 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48483)
store volatile %struct.ScmObj* %current_45args48484, %struct.ScmObj** %stackaddr$prim49528, align 8
%stackaddr$prim49529 = alloca %struct.ScmObj*, align 8
%thunk4010240230 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48484)
store volatile %struct.ScmObj* %thunk4010240230, %struct.ScmObj** %stackaddr$prim49529, align 8
%stackaddr$makeclosure49530 = alloca %struct.ScmObj*, align 8
%fptrToInt49531 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44083 to i64
%ae44083 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49531)
store volatile %struct.ScmObj* %ae44083, %struct.ScmObj** %stackaddr$makeclosure49530, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44083, %struct.ScmObj* %thunk4010240230, i64 0)
%args48513$promise_6340228$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49532 = alloca %struct.ScmObj*, align 8
%args48513$promise_6340228$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %args48513$promise_6340228$0)
store volatile %struct.ScmObj* %args48513$promise_6340228$1, %struct.ScmObj** %stackaddr$prim49532, align 8
%stackaddr$prim49533 = alloca %struct.ScmObj*, align 8
%args48513$promise_6340228$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44083, %struct.ScmObj* %args48513$promise_6340228$1)
store volatile %struct.ScmObj* %args48513$promise_6340228$2, %struct.ScmObj** %stackaddr$prim49533, align 8
%clofunc49534 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %promise_6340228)
musttail call tailcc void %clofunc49534(%struct.ScmObj* %promise_6340228, %struct.ScmObj* %args48513$promise_6340228$2)
ret void
}

define tailcc void @proc_clo$ae44083(%struct.ScmObj* %env$ae44083,%struct.ScmObj* %current_45args48486) {
%stackaddr$env-ref49535 = alloca %struct.ScmObj*, align 8
%thunk4010240230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44083, i64 0)
store %struct.ScmObj* %thunk4010240230, %struct.ScmObj** %stackaddr$env-ref49535
%stackaddr$prim49536 = alloca %struct.ScmObj*, align 8
%_95k40403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48486)
store volatile %struct.ScmObj* %_95k40403, %struct.ScmObj** %stackaddr$prim49536, align 8
%stackaddr$prim49537 = alloca %struct.ScmObj*, align 8
%current_45args48487 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48486)
store volatile %struct.ScmObj* %current_45args48487, %struct.ScmObj** %stackaddr$prim49537, align 8
%stackaddr$prim49538 = alloca %struct.ScmObj*, align 8
%anf_45bind40361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48487)
store volatile %struct.ScmObj* %anf_45bind40361, %struct.ScmObj** %stackaddr$prim49538, align 8
%truthy$cmp49539 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40361)
%cmp$cmp49539 = icmp eq i64 %truthy$cmp49539, 1
br i1 %cmp$cmp49539, label %truebranch$cmp49539, label %falsebranch$cmp49539
truebranch$cmp49539:
%ae44087 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49540 = alloca %struct.ScmObj*, align 8
%anf_45bind40362 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %ae44087)
store volatile %struct.ScmObj* %anf_45bind40362, %struct.ScmObj** %stackaddr$prim49540, align 8
%truthy$cmp49541 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40362)
%cmp$cmp49541 = icmp eq i64 %truthy$cmp49541, 1
br i1 %cmp$cmp49541, label %truebranch$cmp49541, label %falsebranch$cmp49541
truebranch$cmp49541:
%ae44090 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49542 = alloca %struct.ScmObj*, align 8
%cpsprim40404 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %ae44090)
store volatile %struct.ScmObj* %cpsprim40404, %struct.ScmObj** %stackaddr$prim49542, align 8
%stackaddr$makeclosure49543 = alloca %struct.ScmObj*, align 8
%fptrToInt49544 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44091 to i64
%ae44091 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49544)
store volatile %struct.ScmObj* %ae44091, %struct.ScmObj** %stackaddr$makeclosure49543, align 8
%ae44092 = call %struct.ScmObj* @const_init_int(i64 0)
%args48493$ae44091$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49545 = alloca %struct.ScmObj*, align 8
%args48493$ae44091$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40404, %struct.ScmObj* %args48493$ae44091$0)
store volatile %struct.ScmObj* %args48493$ae44091$1, %struct.ScmObj** %stackaddr$prim49545, align 8
%stackaddr$prim49546 = alloca %struct.ScmObj*, align 8
%args48493$ae44091$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44092, %struct.ScmObj* %args48493$ae44091$1)
store volatile %struct.ScmObj* %args48493$ae44091$2, %struct.ScmObj** %stackaddr$prim49546, align 8
%clofunc49547 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44091)
musttail call tailcc void %clofunc49547(%struct.ScmObj* %ae44091, %struct.ScmObj* %args48493$ae44091$2)
ret void
falsebranch$cmp49541:
%ae44112 = call %struct.ScmObj* @const_init_int(i64 1)
%ae44113 = call %struct.ScmObj* @const_init_true()
%stackaddr$prim49548 = alloca %struct.ScmObj*, align 8
%t4010840237 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %ae44112, %struct.ScmObj* %ae44113)
store volatile %struct.ScmObj* %t4010840237, %struct.ScmObj** %stackaddr$prim49548, align 8
%ae44115 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49549 = alloca %struct.ScmObj*, align 8
%anf_45bind40363 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %ae44115)
store volatile %struct.ScmObj* %anf_45bind40363, %struct.ScmObj** %stackaddr$prim49549, align 8
%stackaddr$makeclosure49550 = alloca %struct.ScmObj*, align 8
%fptrToInt49551 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44117 to i64
%ae44117 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49551)
store volatile %struct.ScmObj* %ae44117, %struct.ScmObj** %stackaddr$makeclosure49550, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44117, %struct.ScmObj* %thunk4010240230, i64 0)
%ae44118 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @global$sym$ae4411849552, i32 0, i32 0))
%args48502$anf_45bind40363$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49553 = alloca %struct.ScmObj*, align 8
%args48502$anf_45bind40363$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44118, %struct.ScmObj* %args48502$anf_45bind40363$0)
store volatile %struct.ScmObj* %args48502$anf_45bind40363$1, %struct.ScmObj** %stackaddr$prim49553, align 8
%stackaddr$prim49554 = alloca %struct.ScmObj*, align 8
%args48502$anf_45bind40363$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44117, %struct.ScmObj* %args48502$anf_45bind40363$1)
store volatile %struct.ScmObj* %args48502$anf_45bind40363$2, %struct.ScmObj** %stackaddr$prim49554, align 8
%clofunc49555 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40363)
musttail call tailcc void %clofunc49555(%struct.ScmObj* %anf_45bind40363, %struct.ScmObj* %args48502$anf_45bind40363$2)
ret void
falsebranch$cmp49539:
%stackaddr$prim49556 = alloca %struct.ScmObj*, align 8
%anf_45bind40364 = call %struct.ScmObj* @prim_number_63(%struct.ScmObj* %thunk4010240230)
store volatile %struct.ScmObj* %anf_45bind40364, %struct.ScmObj** %stackaddr$prim49556, align 8
%truthy$cmp49557 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40364)
%cmp$cmp49557 = icmp eq i64 %truthy$cmp49557, 1
br i1 %cmp$cmp49557, label %truebranch$cmp49557, label %falsebranch$cmp49557
truebranch$cmp49557:
%stackaddr$makeclosure49558 = alloca %struct.ScmObj*, align 8
%fptrToInt49559 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44247 to i64
%ae44247 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49559)
store volatile %struct.ScmObj* %ae44247, %struct.ScmObj** %stackaddr$makeclosure49558, align 8
%ae44248 = call %struct.ScmObj* @const_init_int(i64 0)
%args48507$ae44247$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49560 = alloca %struct.ScmObj*, align 8
%args48507$ae44247$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %args48507$ae44247$0)
store volatile %struct.ScmObj* %args48507$ae44247$1, %struct.ScmObj** %stackaddr$prim49560, align 8
%stackaddr$prim49561 = alloca %struct.ScmObj*, align 8
%args48507$ae44247$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44248, %struct.ScmObj* %args48507$ae44247$1)
store volatile %struct.ScmObj* %args48507$ae44247$2, %struct.ScmObj** %stackaddr$prim49561, align 8
%clofunc49562 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44247)
musttail call tailcc void %clofunc49562(%struct.ScmObj* %ae44247, %struct.ScmObj* %args48507$ae44247$2)
ret void
falsebranch$cmp49557:
%stackaddr$makeclosure49563 = alloca %struct.ScmObj*, align 8
%fptrToInt49564 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44259 to i64
%ae44259 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49564)
store volatile %struct.ScmObj* %ae44259, %struct.ScmObj** %stackaddr$makeclosure49563, align 8
%ae44260 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44261 = call %struct.ScmObj* @const_init_string(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @global$str$ae4426149565, i32 0, i32 0))
%args48512$ae44259$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49566 = alloca %struct.ScmObj*, align 8
%args48512$ae44259$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44261, %struct.ScmObj* %args48512$ae44259$0)
store volatile %struct.ScmObj* %args48512$ae44259$1, %struct.ScmObj** %stackaddr$prim49566, align 8
%stackaddr$prim49567 = alloca %struct.ScmObj*, align 8
%args48512$ae44259$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44260, %struct.ScmObj* %args48512$ae44259$1)
store volatile %struct.ScmObj* %args48512$ae44259$2, %struct.ScmObj** %stackaddr$prim49567, align 8
%clofunc49568 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44259)
musttail call tailcc void %clofunc49568(%struct.ScmObj* %ae44259, %struct.ScmObj* %args48512$ae44259$2)
ret void
}

define tailcc void @proc_clo$ae44091(%struct.ScmObj* %env$ae44091,%struct.ScmObj* %current_45args48489) {
%stackaddr$prim49569 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48489)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim49569, align 8
%stackaddr$prim49570 = alloca %struct.ScmObj*, align 8
%current_45args48490 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48489)
store volatile %struct.ScmObj* %current_45args48490, %struct.ScmObj** %stackaddr$prim49570, align 8
%stackaddr$prim49571 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48490)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim49571, align 8
%stackaddr$prim49572 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim49572, align 8
%args48492$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49573 = alloca %struct.ScmObj*, align 8
%args48492$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args48492$k$0)
store volatile %struct.ScmObj* %args48492$k$1, %struct.ScmObj** %stackaddr$prim49573, align 8
%clofunc49574 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc49574(%struct.ScmObj* %k, %struct.ScmObj* %args48492$k$1)
ret void
}

define tailcc void @proc_clo$ae44117(%struct.ScmObj* %env$ae44117,%struct.ScmObj* %current_45args48494) {
%stackaddr$env-ref49575 = alloca %struct.ScmObj*, align 8
%thunk4010240230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44117, i64 0)
store %struct.ScmObj* %thunk4010240230, %struct.ScmObj** %stackaddr$env-ref49575
%stackaddr$prim49576 = alloca %struct.ScmObj*, align 8
%_95k40405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48494)
store volatile %struct.ScmObj* %_95k40405, %struct.ScmObj** %stackaddr$prim49576, align 8
%stackaddr$prim49577 = alloca %struct.ScmObj*, align 8
%current_45args48495 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48494)
store volatile %struct.ScmObj* %current_45args48495, %struct.ScmObj** %stackaddr$prim49577, align 8
%stackaddr$prim49578 = alloca %struct.ScmObj*, align 8
%val4010340239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48495)
store volatile %struct.ScmObj* %val4010340239, %struct.ScmObj** %stackaddr$prim49578, align 8
%ae44123 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49579 = alloca %struct.ScmObj*, align 8
%t4010940238 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %ae44123, %struct.ScmObj* %val4010340239)
store volatile %struct.ScmObj* %t4010940238, %struct.ScmObj** %stackaddr$prim49579, align 8
%ae44126 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49580 = alloca %struct.ScmObj*, align 8
%cpsprim40406 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %ae44126)
store volatile %struct.ScmObj* %cpsprim40406, %struct.ScmObj** %stackaddr$prim49580, align 8
%stackaddr$makeclosure49581 = alloca %struct.ScmObj*, align 8
%fptrToInt49582 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44127 to i64
%ae44127 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49582)
store volatile %struct.ScmObj* %ae44127, %struct.ScmObj** %stackaddr$makeclosure49581, align 8
%ae44128 = call %struct.ScmObj* @const_init_int(i64 0)
%args48501$ae44127$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49583 = alloca %struct.ScmObj*, align 8
%args48501$ae44127$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40406, %struct.ScmObj* %args48501$ae44127$0)
store volatile %struct.ScmObj* %args48501$ae44127$1, %struct.ScmObj** %stackaddr$prim49583, align 8
%stackaddr$prim49584 = alloca %struct.ScmObj*, align 8
%args48501$ae44127$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44128, %struct.ScmObj* %args48501$ae44127$1)
store volatile %struct.ScmObj* %args48501$ae44127$2, %struct.ScmObj** %stackaddr$prim49584, align 8
%clofunc49585 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44127)
musttail call tailcc void %clofunc49585(%struct.ScmObj* %ae44127, %struct.ScmObj* %args48501$ae44127$2)
ret void
}

define tailcc void @proc_clo$ae44127(%struct.ScmObj* %env$ae44127,%struct.ScmObj* %current_45args48497) {
%stackaddr$prim49586 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48497)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim49586, align 8
%stackaddr$prim49587 = alloca %struct.ScmObj*, align 8
%current_45args48498 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48497)
store volatile %struct.ScmObj* %current_45args48498, %struct.ScmObj** %stackaddr$prim49587, align 8
%stackaddr$prim49588 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48498)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim49588, align 8
%stackaddr$prim49589 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim49589, align 8
%args48500$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49590 = alloca %struct.ScmObj*, align 8
%args48500$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args48500$k$0)
store volatile %struct.ScmObj* %args48500$k$1, %struct.ScmObj** %stackaddr$prim49590, align 8
%clofunc49591 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc49591(%struct.ScmObj* %k, %struct.ScmObj* %args48500$k$1)
ret void
}

define tailcc void @proc_clo$ae44247(%struct.ScmObj* %env$ae44247,%struct.ScmObj* %current_45args48503) {
%stackaddr$prim49592 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48503)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim49592, align 8
%stackaddr$prim49593 = alloca %struct.ScmObj*, align 8
%current_45args48504 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48503)
store volatile %struct.ScmObj* %current_45args48504, %struct.ScmObj** %stackaddr$prim49593, align 8
%stackaddr$prim49594 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48504)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim49594, align 8
%stackaddr$prim49595 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim49595, align 8
%args48506$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49596 = alloca %struct.ScmObj*, align 8
%args48506$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args48506$k$0)
store volatile %struct.ScmObj* %args48506$k$1, %struct.ScmObj** %stackaddr$prim49596, align 8
%clofunc49597 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc49597(%struct.ScmObj* %k, %struct.ScmObj* %args48506$k$1)
ret void
}

define tailcc void @proc_clo$ae44259(%struct.ScmObj* %env$ae44259,%struct.ScmObj* %current_45args48508) {
%stackaddr$prim49598 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48508)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim49598, align 8
%stackaddr$prim49599 = alloca %struct.ScmObj*, align 8
%current_45args48509 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48508)
store volatile %struct.ScmObj* %current_45args48509, %struct.ScmObj** %stackaddr$prim49599, align 8
%stackaddr$prim49600 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48509)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim49600, align 8
%stackaddr$prim49601 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim49601, align 8
%args48511$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49602 = alloca %struct.ScmObj*, align 8
%args48511$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args48511$k$0)
store volatile %struct.ScmObj* %args48511$k$1, %struct.ScmObj** %stackaddr$prim49602, align 8
%clofunc49603 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc49603(%struct.ScmObj* %k, %struct.ScmObj* %args48511$k$1)
ret void
}

define tailcc void @proc_clo$ae44339(%struct.ScmObj* %env$ae44339,%struct.ScmObj* %current_45args48515) {
%stackaddr$env-ref49604 = alloca %struct.ScmObj*, align 8
%thunk4010440231 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44339, i64 0)
store %struct.ScmObj* %thunk4010440231, %struct.ScmObj** %stackaddr$env-ref49604
%stackaddr$env-ref49605 = alloca %struct.ScmObj*, align 8
%promise_6340228 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44339, i64 1)
store %struct.ScmObj* %promise_6340228, %struct.ScmObj** %stackaddr$env-ref49605
%stackaddr$prim49606 = alloca %struct.ScmObj*, align 8
%_95k40408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48515)
store volatile %struct.ScmObj* %_95k40408, %struct.ScmObj** %stackaddr$prim49606, align 8
%stackaddr$prim49607 = alloca %struct.ScmObj*, align 8
%current_45args48516 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48515)
store volatile %struct.ScmObj* %current_45args48516, %struct.ScmObj** %stackaddr$prim49607, align 8
%stackaddr$prim49608 = alloca %struct.ScmObj*, align 8
%val4010540236 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48516)
store volatile %struct.ScmObj* %val4010540236, %struct.ScmObj** %stackaddr$prim49608, align 8
%ae44345 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49609 = alloca %struct.ScmObj*, align 8
%t4010740235 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4010440231, %struct.ScmObj* %ae44345, %struct.ScmObj* %val4010540236)
store volatile %struct.ScmObj* %t4010740235, %struct.ScmObj** %stackaddr$prim49609, align 8
%ae44348 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49610 = alloca %struct.ScmObj*, align 8
%cpsprim40409 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4010440231, %struct.ScmObj* %ae44348)
store volatile %struct.ScmObj* %cpsprim40409, %struct.ScmObj** %stackaddr$prim49610, align 8
%stackaddr$makeclosure49611 = alloca %struct.ScmObj*, align 8
%fptrToInt49612 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44349 to i64
%ae44349 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49612)
store volatile %struct.ScmObj* %ae44349, %struct.ScmObj** %stackaddr$makeclosure49611, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44349, %struct.ScmObj* %promise_6340228, i64 0)
%ae44350 = call %struct.ScmObj* @const_init_int(i64 0)
%args48549$ae44349$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49613 = alloca %struct.ScmObj*, align 8
%args48549$ae44349$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40409, %struct.ScmObj* %args48549$ae44349$0)
store volatile %struct.ScmObj* %args48549$ae44349$1, %struct.ScmObj** %stackaddr$prim49613, align 8
%stackaddr$prim49614 = alloca %struct.ScmObj*, align 8
%args48549$ae44349$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44350, %struct.ScmObj* %args48549$ae44349$1)
store volatile %struct.ScmObj* %args48549$ae44349$2, %struct.ScmObj** %stackaddr$prim49614, align 8
%clofunc49615 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44349)
musttail call tailcc void %clofunc49615(%struct.ScmObj* %ae44349, %struct.ScmObj* %args48549$ae44349$2)
ret void
}

define tailcc void @proc_clo$ae44349(%struct.ScmObj* %env$ae44349,%struct.ScmObj* %current_45args48518) {
%stackaddr$env-ref49616 = alloca %struct.ScmObj*, align 8
%promise_6340228 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44349, i64 0)
store %struct.ScmObj* %promise_6340228, %struct.ScmObj** %stackaddr$env-ref49616
%stackaddr$prim49617 = alloca %struct.ScmObj*, align 8
%_95k40402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48518)
store volatile %struct.ScmObj* %_95k40402, %struct.ScmObj** %stackaddr$prim49617, align 8
%stackaddr$prim49618 = alloca %struct.ScmObj*, align 8
%current_45args48519 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48518)
store volatile %struct.ScmObj* %current_45args48519, %struct.ScmObj** %stackaddr$prim49618, align 8
%stackaddr$prim49619 = alloca %struct.ScmObj*, align 8
%thunk4010240230 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48519)
store volatile %struct.ScmObj* %thunk4010240230, %struct.ScmObj** %stackaddr$prim49619, align 8
%stackaddr$makeclosure49620 = alloca %struct.ScmObj*, align 8
%fptrToInt49621 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44356 to i64
%ae44356 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49621)
store volatile %struct.ScmObj* %ae44356, %struct.ScmObj** %stackaddr$makeclosure49620, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44356, %struct.ScmObj* %thunk4010240230, i64 0)
%args48548$promise_6340228$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49622 = alloca %struct.ScmObj*, align 8
%args48548$promise_6340228$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %args48548$promise_6340228$0)
store volatile %struct.ScmObj* %args48548$promise_6340228$1, %struct.ScmObj** %stackaddr$prim49622, align 8
%stackaddr$prim49623 = alloca %struct.ScmObj*, align 8
%args48548$promise_6340228$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44356, %struct.ScmObj* %args48548$promise_6340228$1)
store volatile %struct.ScmObj* %args48548$promise_6340228$2, %struct.ScmObj** %stackaddr$prim49623, align 8
%clofunc49624 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %promise_6340228)
musttail call tailcc void %clofunc49624(%struct.ScmObj* %promise_6340228, %struct.ScmObj* %args48548$promise_6340228$2)
ret void
}

define tailcc void @proc_clo$ae44356(%struct.ScmObj* %env$ae44356,%struct.ScmObj* %current_45args48521) {
%stackaddr$env-ref49625 = alloca %struct.ScmObj*, align 8
%thunk4010240230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44356, i64 0)
store %struct.ScmObj* %thunk4010240230, %struct.ScmObj** %stackaddr$env-ref49625
%stackaddr$prim49626 = alloca %struct.ScmObj*, align 8
%_95k40403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48521)
store volatile %struct.ScmObj* %_95k40403, %struct.ScmObj** %stackaddr$prim49626, align 8
%stackaddr$prim49627 = alloca %struct.ScmObj*, align 8
%current_45args48522 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48521)
store volatile %struct.ScmObj* %current_45args48522, %struct.ScmObj** %stackaddr$prim49627, align 8
%stackaddr$prim49628 = alloca %struct.ScmObj*, align 8
%anf_45bind40361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48522)
store volatile %struct.ScmObj* %anf_45bind40361, %struct.ScmObj** %stackaddr$prim49628, align 8
%truthy$cmp49629 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40361)
%cmp$cmp49629 = icmp eq i64 %truthy$cmp49629, 1
br i1 %cmp$cmp49629, label %truebranch$cmp49629, label %falsebranch$cmp49629
truebranch$cmp49629:
%ae44360 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49630 = alloca %struct.ScmObj*, align 8
%anf_45bind40362 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %ae44360)
store volatile %struct.ScmObj* %anf_45bind40362, %struct.ScmObj** %stackaddr$prim49630, align 8
%truthy$cmp49631 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40362)
%cmp$cmp49631 = icmp eq i64 %truthy$cmp49631, 1
br i1 %cmp$cmp49631, label %truebranch$cmp49631, label %falsebranch$cmp49631
truebranch$cmp49631:
%ae44363 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49632 = alloca %struct.ScmObj*, align 8
%cpsprim40404 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %ae44363)
store volatile %struct.ScmObj* %cpsprim40404, %struct.ScmObj** %stackaddr$prim49632, align 8
%stackaddr$makeclosure49633 = alloca %struct.ScmObj*, align 8
%fptrToInt49634 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44364 to i64
%ae44364 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49634)
store volatile %struct.ScmObj* %ae44364, %struct.ScmObj** %stackaddr$makeclosure49633, align 8
%ae44365 = call %struct.ScmObj* @const_init_int(i64 0)
%args48528$ae44364$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49635 = alloca %struct.ScmObj*, align 8
%args48528$ae44364$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40404, %struct.ScmObj* %args48528$ae44364$0)
store volatile %struct.ScmObj* %args48528$ae44364$1, %struct.ScmObj** %stackaddr$prim49635, align 8
%stackaddr$prim49636 = alloca %struct.ScmObj*, align 8
%args48528$ae44364$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44365, %struct.ScmObj* %args48528$ae44364$1)
store volatile %struct.ScmObj* %args48528$ae44364$2, %struct.ScmObj** %stackaddr$prim49636, align 8
%clofunc49637 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44364)
musttail call tailcc void %clofunc49637(%struct.ScmObj* %ae44364, %struct.ScmObj* %args48528$ae44364$2)
ret void
falsebranch$cmp49631:
%ae44385 = call %struct.ScmObj* @const_init_int(i64 1)
%ae44386 = call %struct.ScmObj* @const_init_true()
%stackaddr$prim49638 = alloca %struct.ScmObj*, align 8
%t4010840237 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %ae44385, %struct.ScmObj* %ae44386)
store volatile %struct.ScmObj* %t4010840237, %struct.ScmObj** %stackaddr$prim49638, align 8
%ae44388 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49639 = alloca %struct.ScmObj*, align 8
%anf_45bind40363 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %ae44388)
store volatile %struct.ScmObj* %anf_45bind40363, %struct.ScmObj** %stackaddr$prim49639, align 8
%stackaddr$makeclosure49640 = alloca %struct.ScmObj*, align 8
%fptrToInt49641 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44390 to i64
%ae44390 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49641)
store volatile %struct.ScmObj* %ae44390, %struct.ScmObj** %stackaddr$makeclosure49640, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44390, %struct.ScmObj* %thunk4010240230, i64 0)
%ae44391 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @global$sym$ae4439149642, i32 0, i32 0))
%args48537$anf_45bind40363$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49643 = alloca %struct.ScmObj*, align 8
%args48537$anf_45bind40363$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44391, %struct.ScmObj* %args48537$anf_45bind40363$0)
store volatile %struct.ScmObj* %args48537$anf_45bind40363$1, %struct.ScmObj** %stackaddr$prim49643, align 8
%stackaddr$prim49644 = alloca %struct.ScmObj*, align 8
%args48537$anf_45bind40363$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44390, %struct.ScmObj* %args48537$anf_45bind40363$1)
store volatile %struct.ScmObj* %args48537$anf_45bind40363$2, %struct.ScmObj** %stackaddr$prim49644, align 8
%clofunc49645 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40363)
musttail call tailcc void %clofunc49645(%struct.ScmObj* %anf_45bind40363, %struct.ScmObj* %args48537$anf_45bind40363$2)
ret void
falsebranch$cmp49629:
%stackaddr$prim49646 = alloca %struct.ScmObj*, align 8
%anf_45bind40364 = call %struct.ScmObj* @prim_number_63(%struct.ScmObj* %thunk4010240230)
store volatile %struct.ScmObj* %anf_45bind40364, %struct.ScmObj** %stackaddr$prim49646, align 8
%truthy$cmp49647 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40364)
%cmp$cmp49647 = icmp eq i64 %truthy$cmp49647, 1
br i1 %cmp$cmp49647, label %truebranch$cmp49647, label %falsebranch$cmp49647
truebranch$cmp49647:
%stackaddr$makeclosure49648 = alloca %struct.ScmObj*, align 8
%fptrToInt49649 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44520 to i64
%ae44520 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49649)
store volatile %struct.ScmObj* %ae44520, %struct.ScmObj** %stackaddr$makeclosure49648, align 8
%ae44521 = call %struct.ScmObj* @const_init_int(i64 0)
%args48542$ae44520$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49650 = alloca %struct.ScmObj*, align 8
%args48542$ae44520$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %args48542$ae44520$0)
store volatile %struct.ScmObj* %args48542$ae44520$1, %struct.ScmObj** %stackaddr$prim49650, align 8
%stackaddr$prim49651 = alloca %struct.ScmObj*, align 8
%args48542$ae44520$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44521, %struct.ScmObj* %args48542$ae44520$1)
store volatile %struct.ScmObj* %args48542$ae44520$2, %struct.ScmObj** %stackaddr$prim49651, align 8
%clofunc49652 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44520)
musttail call tailcc void %clofunc49652(%struct.ScmObj* %ae44520, %struct.ScmObj* %args48542$ae44520$2)
ret void
falsebranch$cmp49647:
%stackaddr$makeclosure49653 = alloca %struct.ScmObj*, align 8
%fptrToInt49654 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44532 to i64
%ae44532 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49654)
store volatile %struct.ScmObj* %ae44532, %struct.ScmObj** %stackaddr$makeclosure49653, align 8
%ae44533 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44534 = call %struct.ScmObj* @const_init_string(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @global$str$ae4453449655, i32 0, i32 0))
%args48547$ae44532$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49656 = alloca %struct.ScmObj*, align 8
%args48547$ae44532$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44534, %struct.ScmObj* %args48547$ae44532$0)
store volatile %struct.ScmObj* %args48547$ae44532$1, %struct.ScmObj** %stackaddr$prim49656, align 8
%stackaddr$prim49657 = alloca %struct.ScmObj*, align 8
%args48547$ae44532$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44533, %struct.ScmObj* %args48547$ae44532$1)
store volatile %struct.ScmObj* %args48547$ae44532$2, %struct.ScmObj** %stackaddr$prim49657, align 8
%clofunc49658 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44532)
musttail call tailcc void %clofunc49658(%struct.ScmObj* %ae44532, %struct.ScmObj* %args48547$ae44532$2)
ret void
}

define tailcc void @proc_clo$ae44364(%struct.ScmObj* %env$ae44364,%struct.ScmObj* %current_45args48524) {
%stackaddr$prim49659 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48524)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim49659, align 8
%stackaddr$prim49660 = alloca %struct.ScmObj*, align 8
%current_45args48525 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48524)
store volatile %struct.ScmObj* %current_45args48525, %struct.ScmObj** %stackaddr$prim49660, align 8
%stackaddr$prim49661 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48525)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim49661, align 8
%stackaddr$prim49662 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim49662, align 8
%args48527$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49663 = alloca %struct.ScmObj*, align 8
%args48527$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args48527$k$0)
store volatile %struct.ScmObj* %args48527$k$1, %struct.ScmObj** %stackaddr$prim49663, align 8
%clofunc49664 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc49664(%struct.ScmObj* %k, %struct.ScmObj* %args48527$k$1)
ret void
}

define tailcc void @proc_clo$ae44390(%struct.ScmObj* %env$ae44390,%struct.ScmObj* %current_45args48529) {
%stackaddr$env-ref49665 = alloca %struct.ScmObj*, align 8
%thunk4010240230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44390, i64 0)
store %struct.ScmObj* %thunk4010240230, %struct.ScmObj** %stackaddr$env-ref49665
%stackaddr$prim49666 = alloca %struct.ScmObj*, align 8
%_95k40405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48529)
store volatile %struct.ScmObj* %_95k40405, %struct.ScmObj** %stackaddr$prim49666, align 8
%stackaddr$prim49667 = alloca %struct.ScmObj*, align 8
%current_45args48530 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48529)
store volatile %struct.ScmObj* %current_45args48530, %struct.ScmObj** %stackaddr$prim49667, align 8
%stackaddr$prim49668 = alloca %struct.ScmObj*, align 8
%val4010340239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48530)
store volatile %struct.ScmObj* %val4010340239, %struct.ScmObj** %stackaddr$prim49668, align 8
%ae44396 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49669 = alloca %struct.ScmObj*, align 8
%t4010940238 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %ae44396, %struct.ScmObj* %val4010340239)
store volatile %struct.ScmObj* %t4010940238, %struct.ScmObj** %stackaddr$prim49669, align 8
%ae44399 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49670 = alloca %struct.ScmObj*, align 8
%cpsprim40406 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %ae44399)
store volatile %struct.ScmObj* %cpsprim40406, %struct.ScmObj** %stackaddr$prim49670, align 8
%stackaddr$makeclosure49671 = alloca %struct.ScmObj*, align 8
%fptrToInt49672 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44400 to i64
%ae44400 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49672)
store volatile %struct.ScmObj* %ae44400, %struct.ScmObj** %stackaddr$makeclosure49671, align 8
%ae44401 = call %struct.ScmObj* @const_init_int(i64 0)
%args48536$ae44400$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49673 = alloca %struct.ScmObj*, align 8
%args48536$ae44400$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40406, %struct.ScmObj* %args48536$ae44400$0)
store volatile %struct.ScmObj* %args48536$ae44400$1, %struct.ScmObj** %stackaddr$prim49673, align 8
%stackaddr$prim49674 = alloca %struct.ScmObj*, align 8
%args48536$ae44400$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44401, %struct.ScmObj* %args48536$ae44400$1)
store volatile %struct.ScmObj* %args48536$ae44400$2, %struct.ScmObj** %stackaddr$prim49674, align 8
%clofunc49675 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44400)
musttail call tailcc void %clofunc49675(%struct.ScmObj* %ae44400, %struct.ScmObj* %args48536$ae44400$2)
ret void
}

define tailcc void @proc_clo$ae44400(%struct.ScmObj* %env$ae44400,%struct.ScmObj* %current_45args48532) {
%stackaddr$prim49676 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48532)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim49676, align 8
%stackaddr$prim49677 = alloca %struct.ScmObj*, align 8
%current_45args48533 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48532)
store volatile %struct.ScmObj* %current_45args48533, %struct.ScmObj** %stackaddr$prim49677, align 8
%stackaddr$prim49678 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48533)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim49678, align 8
%stackaddr$prim49679 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim49679, align 8
%args48535$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49680 = alloca %struct.ScmObj*, align 8
%args48535$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args48535$k$0)
store volatile %struct.ScmObj* %args48535$k$1, %struct.ScmObj** %stackaddr$prim49680, align 8
%clofunc49681 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc49681(%struct.ScmObj* %k, %struct.ScmObj* %args48535$k$1)
ret void
}

define tailcc void @proc_clo$ae44520(%struct.ScmObj* %env$ae44520,%struct.ScmObj* %current_45args48538) {
%stackaddr$prim49682 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48538)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim49682, align 8
%stackaddr$prim49683 = alloca %struct.ScmObj*, align 8
%current_45args48539 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48538)
store volatile %struct.ScmObj* %current_45args48539, %struct.ScmObj** %stackaddr$prim49683, align 8
%stackaddr$prim49684 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48539)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim49684, align 8
%stackaddr$prim49685 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim49685, align 8
%args48541$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49686 = alloca %struct.ScmObj*, align 8
%args48541$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args48541$k$0)
store volatile %struct.ScmObj* %args48541$k$1, %struct.ScmObj** %stackaddr$prim49686, align 8
%clofunc49687 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc49687(%struct.ScmObj* %k, %struct.ScmObj* %args48541$k$1)
ret void
}

define tailcc void @proc_clo$ae44532(%struct.ScmObj* %env$ae44532,%struct.ScmObj* %current_45args48543) {
%stackaddr$prim49688 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48543)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim49688, align 8
%stackaddr$prim49689 = alloca %struct.ScmObj*, align 8
%current_45args48544 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48543)
store volatile %struct.ScmObj* %current_45args48544, %struct.ScmObj** %stackaddr$prim49689, align 8
%stackaddr$prim49690 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48544)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim49690, align 8
%stackaddr$prim49691 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim49691, align 8
%args48546$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49692 = alloca %struct.ScmObj*, align 8
%args48546$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args48546$k$0)
store volatile %struct.ScmObj* %args48546$k$1, %struct.ScmObj** %stackaddr$prim49692, align 8
%clofunc49693 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc49693(%struct.ScmObj* %k, %struct.ScmObj* %args48546$k$1)
ret void
}

define tailcc void @proc_clo$ae44976(%struct.ScmObj* %env$ae44976,%struct.ScmObj* %current_45args48551) {
%stackaddr$env-ref49694 = alloca %struct.ScmObj*, align 8
%promise_6340228 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44976, i64 0)
store %struct.ScmObj* %promise_6340228, %struct.ScmObj** %stackaddr$env-ref49694
%stackaddr$prim49695 = alloca %struct.ScmObj*, align 8
%_95k40402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48551)
store volatile %struct.ScmObj* %_95k40402, %struct.ScmObj** %stackaddr$prim49695, align 8
%stackaddr$prim49696 = alloca %struct.ScmObj*, align 8
%current_45args48552 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48551)
store volatile %struct.ScmObj* %current_45args48552, %struct.ScmObj** %stackaddr$prim49696, align 8
%stackaddr$prim49697 = alloca %struct.ScmObj*, align 8
%thunk4010240230 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48552)
store volatile %struct.ScmObj* %thunk4010240230, %struct.ScmObj** %stackaddr$prim49697, align 8
%stackaddr$makeclosure49698 = alloca %struct.ScmObj*, align 8
%fptrToInt49699 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44983 to i64
%ae44983 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49699)
store volatile %struct.ScmObj* %ae44983, %struct.ScmObj** %stackaddr$makeclosure49698, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44983, %struct.ScmObj* %thunk4010240230, i64 0)
%args48581$promise_6340228$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49700 = alloca %struct.ScmObj*, align 8
%args48581$promise_6340228$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %args48581$promise_6340228$0)
store volatile %struct.ScmObj* %args48581$promise_6340228$1, %struct.ScmObj** %stackaddr$prim49700, align 8
%stackaddr$prim49701 = alloca %struct.ScmObj*, align 8
%args48581$promise_6340228$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44983, %struct.ScmObj* %args48581$promise_6340228$1)
store volatile %struct.ScmObj* %args48581$promise_6340228$2, %struct.ScmObj** %stackaddr$prim49701, align 8
%clofunc49702 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %promise_6340228)
musttail call tailcc void %clofunc49702(%struct.ScmObj* %promise_6340228, %struct.ScmObj* %args48581$promise_6340228$2)
ret void
}

define tailcc void @proc_clo$ae44983(%struct.ScmObj* %env$ae44983,%struct.ScmObj* %current_45args48554) {
%stackaddr$env-ref49703 = alloca %struct.ScmObj*, align 8
%thunk4010240230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44983, i64 0)
store %struct.ScmObj* %thunk4010240230, %struct.ScmObj** %stackaddr$env-ref49703
%stackaddr$prim49704 = alloca %struct.ScmObj*, align 8
%_95k40403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48554)
store volatile %struct.ScmObj* %_95k40403, %struct.ScmObj** %stackaddr$prim49704, align 8
%stackaddr$prim49705 = alloca %struct.ScmObj*, align 8
%current_45args48555 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48554)
store volatile %struct.ScmObj* %current_45args48555, %struct.ScmObj** %stackaddr$prim49705, align 8
%stackaddr$prim49706 = alloca %struct.ScmObj*, align 8
%anf_45bind40361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48555)
store volatile %struct.ScmObj* %anf_45bind40361, %struct.ScmObj** %stackaddr$prim49706, align 8
%truthy$cmp49707 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40361)
%cmp$cmp49707 = icmp eq i64 %truthy$cmp49707, 1
br i1 %cmp$cmp49707, label %truebranch$cmp49707, label %falsebranch$cmp49707
truebranch$cmp49707:
%ae44987 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49708 = alloca %struct.ScmObj*, align 8
%anf_45bind40362 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %ae44987)
store volatile %struct.ScmObj* %anf_45bind40362, %struct.ScmObj** %stackaddr$prim49708, align 8
%truthy$cmp49709 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40362)
%cmp$cmp49709 = icmp eq i64 %truthy$cmp49709, 1
br i1 %cmp$cmp49709, label %truebranch$cmp49709, label %falsebranch$cmp49709
truebranch$cmp49709:
%ae44990 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49710 = alloca %struct.ScmObj*, align 8
%cpsprim40404 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %ae44990)
store volatile %struct.ScmObj* %cpsprim40404, %struct.ScmObj** %stackaddr$prim49710, align 8
%stackaddr$makeclosure49711 = alloca %struct.ScmObj*, align 8
%fptrToInt49712 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44991 to i64
%ae44991 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49712)
store volatile %struct.ScmObj* %ae44991, %struct.ScmObj** %stackaddr$makeclosure49711, align 8
%ae44992 = call %struct.ScmObj* @const_init_int(i64 0)
%args48561$ae44991$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49713 = alloca %struct.ScmObj*, align 8
%args48561$ae44991$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40404, %struct.ScmObj* %args48561$ae44991$0)
store volatile %struct.ScmObj* %args48561$ae44991$1, %struct.ScmObj** %stackaddr$prim49713, align 8
%stackaddr$prim49714 = alloca %struct.ScmObj*, align 8
%args48561$ae44991$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44992, %struct.ScmObj* %args48561$ae44991$1)
store volatile %struct.ScmObj* %args48561$ae44991$2, %struct.ScmObj** %stackaddr$prim49714, align 8
%clofunc49715 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44991)
musttail call tailcc void %clofunc49715(%struct.ScmObj* %ae44991, %struct.ScmObj* %args48561$ae44991$2)
ret void
falsebranch$cmp49709:
%ae45012 = call %struct.ScmObj* @const_init_int(i64 1)
%ae45013 = call %struct.ScmObj* @const_init_true()
%stackaddr$prim49716 = alloca %struct.ScmObj*, align 8
%t4010840237 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %ae45012, %struct.ScmObj* %ae45013)
store volatile %struct.ScmObj* %t4010840237, %struct.ScmObj** %stackaddr$prim49716, align 8
%ae45015 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49717 = alloca %struct.ScmObj*, align 8
%anf_45bind40363 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %ae45015)
store volatile %struct.ScmObj* %anf_45bind40363, %struct.ScmObj** %stackaddr$prim49717, align 8
%stackaddr$makeclosure49718 = alloca %struct.ScmObj*, align 8
%fptrToInt49719 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45017 to i64
%ae45017 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49719)
store volatile %struct.ScmObj* %ae45017, %struct.ScmObj** %stackaddr$makeclosure49718, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae45017, %struct.ScmObj* %thunk4010240230, i64 0)
%ae45018 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @global$sym$ae4501849720, i32 0, i32 0))
%args48570$anf_45bind40363$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49721 = alloca %struct.ScmObj*, align 8
%args48570$anf_45bind40363$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45018, %struct.ScmObj* %args48570$anf_45bind40363$0)
store volatile %struct.ScmObj* %args48570$anf_45bind40363$1, %struct.ScmObj** %stackaddr$prim49721, align 8
%stackaddr$prim49722 = alloca %struct.ScmObj*, align 8
%args48570$anf_45bind40363$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45017, %struct.ScmObj* %args48570$anf_45bind40363$1)
store volatile %struct.ScmObj* %args48570$anf_45bind40363$2, %struct.ScmObj** %stackaddr$prim49722, align 8
%clofunc49723 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40363)
musttail call tailcc void %clofunc49723(%struct.ScmObj* %anf_45bind40363, %struct.ScmObj* %args48570$anf_45bind40363$2)
ret void
falsebranch$cmp49707:
%stackaddr$prim49724 = alloca %struct.ScmObj*, align 8
%anf_45bind40364 = call %struct.ScmObj* @prim_number_63(%struct.ScmObj* %thunk4010240230)
store volatile %struct.ScmObj* %anf_45bind40364, %struct.ScmObj** %stackaddr$prim49724, align 8
%truthy$cmp49725 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40364)
%cmp$cmp49725 = icmp eq i64 %truthy$cmp49725, 1
br i1 %cmp$cmp49725, label %truebranch$cmp49725, label %falsebranch$cmp49725
truebranch$cmp49725:
%stackaddr$makeclosure49726 = alloca %struct.ScmObj*, align 8
%fptrToInt49727 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45147 to i64
%ae45147 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49727)
store volatile %struct.ScmObj* %ae45147, %struct.ScmObj** %stackaddr$makeclosure49726, align 8
%ae45148 = call %struct.ScmObj* @const_init_int(i64 0)
%args48575$ae45147$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49728 = alloca %struct.ScmObj*, align 8
%args48575$ae45147$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %args48575$ae45147$0)
store volatile %struct.ScmObj* %args48575$ae45147$1, %struct.ScmObj** %stackaddr$prim49728, align 8
%stackaddr$prim49729 = alloca %struct.ScmObj*, align 8
%args48575$ae45147$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45148, %struct.ScmObj* %args48575$ae45147$1)
store volatile %struct.ScmObj* %args48575$ae45147$2, %struct.ScmObj** %stackaddr$prim49729, align 8
%clofunc49730 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae45147)
musttail call tailcc void %clofunc49730(%struct.ScmObj* %ae45147, %struct.ScmObj* %args48575$ae45147$2)
ret void
falsebranch$cmp49725:
%stackaddr$makeclosure49731 = alloca %struct.ScmObj*, align 8
%fptrToInt49732 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45159 to i64
%ae45159 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49732)
store volatile %struct.ScmObj* %ae45159, %struct.ScmObj** %stackaddr$makeclosure49731, align 8
%ae45160 = call %struct.ScmObj* @const_init_int(i64 0)
%ae45161 = call %struct.ScmObj* @const_init_string(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @global$str$ae4516149733, i32 0, i32 0))
%args48580$ae45159$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49734 = alloca %struct.ScmObj*, align 8
%args48580$ae45159$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45161, %struct.ScmObj* %args48580$ae45159$0)
store volatile %struct.ScmObj* %args48580$ae45159$1, %struct.ScmObj** %stackaddr$prim49734, align 8
%stackaddr$prim49735 = alloca %struct.ScmObj*, align 8
%args48580$ae45159$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45160, %struct.ScmObj* %args48580$ae45159$1)
store volatile %struct.ScmObj* %args48580$ae45159$2, %struct.ScmObj** %stackaddr$prim49735, align 8
%clofunc49736 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae45159)
musttail call tailcc void %clofunc49736(%struct.ScmObj* %ae45159, %struct.ScmObj* %args48580$ae45159$2)
ret void
}

define tailcc void @proc_clo$ae44991(%struct.ScmObj* %env$ae44991,%struct.ScmObj* %current_45args48557) {
%stackaddr$prim49737 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48557)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim49737, align 8
%stackaddr$prim49738 = alloca %struct.ScmObj*, align 8
%current_45args48558 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48557)
store volatile %struct.ScmObj* %current_45args48558, %struct.ScmObj** %stackaddr$prim49738, align 8
%stackaddr$prim49739 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48558)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim49739, align 8
%stackaddr$prim49740 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim49740, align 8
%args48560$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49741 = alloca %struct.ScmObj*, align 8
%args48560$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args48560$k$0)
store volatile %struct.ScmObj* %args48560$k$1, %struct.ScmObj** %stackaddr$prim49741, align 8
%clofunc49742 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc49742(%struct.ScmObj* %k, %struct.ScmObj* %args48560$k$1)
ret void
}

define tailcc void @proc_clo$ae45017(%struct.ScmObj* %env$ae45017,%struct.ScmObj* %current_45args48562) {
%stackaddr$env-ref49743 = alloca %struct.ScmObj*, align 8
%thunk4010240230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45017, i64 0)
store %struct.ScmObj* %thunk4010240230, %struct.ScmObj** %stackaddr$env-ref49743
%stackaddr$prim49744 = alloca %struct.ScmObj*, align 8
%_95k40405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48562)
store volatile %struct.ScmObj* %_95k40405, %struct.ScmObj** %stackaddr$prim49744, align 8
%stackaddr$prim49745 = alloca %struct.ScmObj*, align 8
%current_45args48563 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48562)
store volatile %struct.ScmObj* %current_45args48563, %struct.ScmObj** %stackaddr$prim49745, align 8
%stackaddr$prim49746 = alloca %struct.ScmObj*, align 8
%val4010340239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48563)
store volatile %struct.ScmObj* %val4010340239, %struct.ScmObj** %stackaddr$prim49746, align 8
%ae45023 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49747 = alloca %struct.ScmObj*, align 8
%t4010940238 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %ae45023, %struct.ScmObj* %val4010340239)
store volatile %struct.ScmObj* %t4010940238, %struct.ScmObj** %stackaddr$prim49747, align 8
%ae45026 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49748 = alloca %struct.ScmObj*, align 8
%cpsprim40406 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %ae45026)
store volatile %struct.ScmObj* %cpsprim40406, %struct.ScmObj** %stackaddr$prim49748, align 8
%stackaddr$makeclosure49749 = alloca %struct.ScmObj*, align 8
%fptrToInt49750 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45027 to i64
%ae45027 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49750)
store volatile %struct.ScmObj* %ae45027, %struct.ScmObj** %stackaddr$makeclosure49749, align 8
%ae45028 = call %struct.ScmObj* @const_init_int(i64 0)
%args48569$ae45027$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49751 = alloca %struct.ScmObj*, align 8
%args48569$ae45027$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40406, %struct.ScmObj* %args48569$ae45027$0)
store volatile %struct.ScmObj* %args48569$ae45027$1, %struct.ScmObj** %stackaddr$prim49751, align 8
%stackaddr$prim49752 = alloca %struct.ScmObj*, align 8
%args48569$ae45027$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45028, %struct.ScmObj* %args48569$ae45027$1)
store volatile %struct.ScmObj* %args48569$ae45027$2, %struct.ScmObj** %stackaddr$prim49752, align 8
%clofunc49753 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae45027)
musttail call tailcc void %clofunc49753(%struct.ScmObj* %ae45027, %struct.ScmObj* %args48569$ae45027$2)
ret void
}

define tailcc void @proc_clo$ae45027(%struct.ScmObj* %env$ae45027,%struct.ScmObj* %current_45args48565) {
%stackaddr$prim49754 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48565)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim49754, align 8
%stackaddr$prim49755 = alloca %struct.ScmObj*, align 8
%current_45args48566 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48565)
store volatile %struct.ScmObj* %current_45args48566, %struct.ScmObj** %stackaddr$prim49755, align 8
%stackaddr$prim49756 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48566)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim49756, align 8
%stackaddr$prim49757 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim49757, align 8
%args48568$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49758 = alloca %struct.ScmObj*, align 8
%args48568$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args48568$k$0)
store volatile %struct.ScmObj* %args48568$k$1, %struct.ScmObj** %stackaddr$prim49758, align 8
%clofunc49759 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc49759(%struct.ScmObj* %k, %struct.ScmObj* %args48568$k$1)
ret void
}

define tailcc void @proc_clo$ae45147(%struct.ScmObj* %env$ae45147,%struct.ScmObj* %current_45args48571) {
%stackaddr$prim49760 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48571)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim49760, align 8
%stackaddr$prim49761 = alloca %struct.ScmObj*, align 8
%current_45args48572 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48571)
store volatile %struct.ScmObj* %current_45args48572, %struct.ScmObj** %stackaddr$prim49761, align 8
%stackaddr$prim49762 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48572)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim49762, align 8
%stackaddr$prim49763 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim49763, align 8
%args48574$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49764 = alloca %struct.ScmObj*, align 8
%args48574$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args48574$k$0)
store volatile %struct.ScmObj* %args48574$k$1, %struct.ScmObj** %stackaddr$prim49764, align 8
%clofunc49765 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc49765(%struct.ScmObj* %k, %struct.ScmObj* %args48574$k$1)
ret void
}

define tailcc void @proc_clo$ae45159(%struct.ScmObj* %env$ae45159,%struct.ScmObj* %current_45args48576) {
%stackaddr$prim49766 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48576)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim49766, align 8
%stackaddr$prim49767 = alloca %struct.ScmObj*, align 8
%current_45args48577 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48576)
store volatile %struct.ScmObj* %current_45args48577, %struct.ScmObj** %stackaddr$prim49767, align 8
%stackaddr$prim49768 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48577)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim49768, align 8
%stackaddr$prim49769 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim49769, align 8
%args48579$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49770 = alloca %struct.ScmObj*, align 8
%args48579$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args48579$k$0)
store volatile %struct.ScmObj* %args48579$k$1, %struct.ScmObj** %stackaddr$prim49770, align 8
%clofunc49771 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc49771(%struct.ScmObj* %k, %struct.ScmObj* %args48579$k$1)
ret void
}

define tailcc void @proc_clo$ae45180(%struct.ScmObj* %env$ae45180,%struct.ScmObj* %current_45args48583) {
%stackaddr$env-ref49772 = alloca %struct.ScmObj*, align 8
%promise_6340228 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45180, i64 0)
store %struct.ScmObj* %promise_6340228, %struct.ScmObj** %stackaddr$env-ref49772
%stackaddr$prim49773 = alloca %struct.ScmObj*, align 8
%_95k40402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48583)
store volatile %struct.ScmObj* %_95k40402, %struct.ScmObj** %stackaddr$prim49773, align 8
%stackaddr$prim49774 = alloca %struct.ScmObj*, align 8
%current_45args48584 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48583)
store volatile %struct.ScmObj* %current_45args48584, %struct.ScmObj** %stackaddr$prim49774, align 8
%stackaddr$prim49775 = alloca %struct.ScmObj*, align 8
%thunk4010240230 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48584)
store volatile %struct.ScmObj* %thunk4010240230, %struct.ScmObj** %stackaddr$prim49775, align 8
%stackaddr$makeclosure49776 = alloca %struct.ScmObj*, align 8
%fptrToInt49777 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45190 to i64
%ae45190 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49777)
store volatile %struct.ScmObj* %ae45190, %struct.ScmObj** %stackaddr$makeclosure49776, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae45190, %struct.ScmObj* %thunk4010240230, i64 0)
%args48613$promise_6340228$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49778 = alloca %struct.ScmObj*, align 8
%args48613$promise_6340228$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %args48613$promise_6340228$0)
store volatile %struct.ScmObj* %args48613$promise_6340228$1, %struct.ScmObj** %stackaddr$prim49778, align 8
%stackaddr$prim49779 = alloca %struct.ScmObj*, align 8
%args48613$promise_6340228$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45190, %struct.ScmObj* %args48613$promise_6340228$1)
store volatile %struct.ScmObj* %args48613$promise_6340228$2, %struct.ScmObj** %stackaddr$prim49779, align 8
%clofunc49780 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %promise_6340228)
musttail call tailcc void %clofunc49780(%struct.ScmObj* %promise_6340228, %struct.ScmObj* %args48613$promise_6340228$2)
ret void
}

define tailcc void @proc_clo$ae45190(%struct.ScmObj* %env$ae45190,%struct.ScmObj* %current_45args48586) {
%stackaddr$env-ref49781 = alloca %struct.ScmObj*, align 8
%thunk4010240230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45190, i64 0)
store %struct.ScmObj* %thunk4010240230, %struct.ScmObj** %stackaddr$env-ref49781
%stackaddr$prim49782 = alloca %struct.ScmObj*, align 8
%_95k40403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48586)
store volatile %struct.ScmObj* %_95k40403, %struct.ScmObj** %stackaddr$prim49782, align 8
%stackaddr$prim49783 = alloca %struct.ScmObj*, align 8
%current_45args48587 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48586)
store volatile %struct.ScmObj* %current_45args48587, %struct.ScmObj** %stackaddr$prim49783, align 8
%stackaddr$prim49784 = alloca %struct.ScmObj*, align 8
%anf_45bind40361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48587)
store volatile %struct.ScmObj* %anf_45bind40361, %struct.ScmObj** %stackaddr$prim49784, align 8
%truthy$cmp49785 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40361)
%cmp$cmp49785 = icmp eq i64 %truthy$cmp49785, 1
br i1 %cmp$cmp49785, label %truebranch$cmp49785, label %falsebranch$cmp49785
truebranch$cmp49785:
%ae45194 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49786 = alloca %struct.ScmObj*, align 8
%anf_45bind40362 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %ae45194)
store volatile %struct.ScmObj* %anf_45bind40362, %struct.ScmObj** %stackaddr$prim49786, align 8
%truthy$cmp49787 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40362)
%cmp$cmp49787 = icmp eq i64 %truthy$cmp49787, 1
br i1 %cmp$cmp49787, label %truebranch$cmp49787, label %falsebranch$cmp49787
truebranch$cmp49787:
%ae45197 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49788 = alloca %struct.ScmObj*, align 8
%cpsprim40404 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %ae45197)
store volatile %struct.ScmObj* %cpsprim40404, %struct.ScmObj** %stackaddr$prim49788, align 8
%stackaddr$makeclosure49789 = alloca %struct.ScmObj*, align 8
%fptrToInt49790 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45198 to i64
%ae45198 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49790)
store volatile %struct.ScmObj* %ae45198, %struct.ScmObj** %stackaddr$makeclosure49789, align 8
%ae45199 = call %struct.ScmObj* @const_init_int(i64 0)
%args48593$ae45198$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49791 = alloca %struct.ScmObj*, align 8
%args48593$ae45198$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40404, %struct.ScmObj* %args48593$ae45198$0)
store volatile %struct.ScmObj* %args48593$ae45198$1, %struct.ScmObj** %stackaddr$prim49791, align 8
%stackaddr$prim49792 = alloca %struct.ScmObj*, align 8
%args48593$ae45198$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45199, %struct.ScmObj* %args48593$ae45198$1)
store volatile %struct.ScmObj* %args48593$ae45198$2, %struct.ScmObj** %stackaddr$prim49792, align 8
%clofunc49793 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae45198)
musttail call tailcc void %clofunc49793(%struct.ScmObj* %ae45198, %struct.ScmObj* %args48593$ae45198$2)
ret void
falsebranch$cmp49787:
%ae45219 = call %struct.ScmObj* @const_init_int(i64 1)
%ae45220 = call %struct.ScmObj* @const_init_true()
%stackaddr$prim49794 = alloca %struct.ScmObj*, align 8
%t4010840237 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %ae45219, %struct.ScmObj* %ae45220)
store volatile %struct.ScmObj* %t4010840237, %struct.ScmObj** %stackaddr$prim49794, align 8
%ae45222 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49795 = alloca %struct.ScmObj*, align 8
%anf_45bind40363 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %ae45222)
store volatile %struct.ScmObj* %anf_45bind40363, %struct.ScmObj** %stackaddr$prim49795, align 8
%stackaddr$makeclosure49796 = alloca %struct.ScmObj*, align 8
%fptrToInt49797 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45224 to i64
%ae45224 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49797)
store volatile %struct.ScmObj* %ae45224, %struct.ScmObj** %stackaddr$makeclosure49796, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae45224, %struct.ScmObj* %thunk4010240230, i64 0)
%ae45225 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @global$sym$ae4522549798, i32 0, i32 0))
%args48602$anf_45bind40363$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49799 = alloca %struct.ScmObj*, align 8
%args48602$anf_45bind40363$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45225, %struct.ScmObj* %args48602$anf_45bind40363$0)
store volatile %struct.ScmObj* %args48602$anf_45bind40363$1, %struct.ScmObj** %stackaddr$prim49799, align 8
%stackaddr$prim49800 = alloca %struct.ScmObj*, align 8
%args48602$anf_45bind40363$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45224, %struct.ScmObj* %args48602$anf_45bind40363$1)
store volatile %struct.ScmObj* %args48602$anf_45bind40363$2, %struct.ScmObj** %stackaddr$prim49800, align 8
%clofunc49801 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40363)
musttail call tailcc void %clofunc49801(%struct.ScmObj* %anf_45bind40363, %struct.ScmObj* %args48602$anf_45bind40363$2)
ret void
falsebranch$cmp49785:
%stackaddr$prim49802 = alloca %struct.ScmObj*, align 8
%anf_45bind40364 = call %struct.ScmObj* @prim_number_63(%struct.ScmObj* %thunk4010240230)
store volatile %struct.ScmObj* %anf_45bind40364, %struct.ScmObj** %stackaddr$prim49802, align 8
%truthy$cmp49803 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40364)
%cmp$cmp49803 = icmp eq i64 %truthy$cmp49803, 1
br i1 %cmp$cmp49803, label %truebranch$cmp49803, label %falsebranch$cmp49803
truebranch$cmp49803:
%stackaddr$makeclosure49804 = alloca %struct.ScmObj*, align 8
%fptrToInt49805 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45354 to i64
%ae45354 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49805)
store volatile %struct.ScmObj* %ae45354, %struct.ScmObj** %stackaddr$makeclosure49804, align 8
%ae45355 = call %struct.ScmObj* @const_init_int(i64 0)
%args48607$ae45354$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49806 = alloca %struct.ScmObj*, align 8
%args48607$ae45354$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %args48607$ae45354$0)
store volatile %struct.ScmObj* %args48607$ae45354$1, %struct.ScmObj** %stackaddr$prim49806, align 8
%stackaddr$prim49807 = alloca %struct.ScmObj*, align 8
%args48607$ae45354$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45355, %struct.ScmObj* %args48607$ae45354$1)
store volatile %struct.ScmObj* %args48607$ae45354$2, %struct.ScmObj** %stackaddr$prim49807, align 8
%clofunc49808 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae45354)
musttail call tailcc void %clofunc49808(%struct.ScmObj* %ae45354, %struct.ScmObj* %args48607$ae45354$2)
ret void
falsebranch$cmp49803:
%stackaddr$makeclosure49809 = alloca %struct.ScmObj*, align 8
%fptrToInt49810 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45366 to i64
%ae45366 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49810)
store volatile %struct.ScmObj* %ae45366, %struct.ScmObj** %stackaddr$makeclosure49809, align 8
%ae45367 = call %struct.ScmObj* @const_init_int(i64 0)
%ae45368 = call %struct.ScmObj* @const_init_string(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @global$str$ae4536849811, i32 0, i32 0))
%args48612$ae45366$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49812 = alloca %struct.ScmObj*, align 8
%args48612$ae45366$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45368, %struct.ScmObj* %args48612$ae45366$0)
store volatile %struct.ScmObj* %args48612$ae45366$1, %struct.ScmObj** %stackaddr$prim49812, align 8
%stackaddr$prim49813 = alloca %struct.ScmObj*, align 8
%args48612$ae45366$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45367, %struct.ScmObj* %args48612$ae45366$1)
store volatile %struct.ScmObj* %args48612$ae45366$2, %struct.ScmObj** %stackaddr$prim49813, align 8
%clofunc49814 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae45366)
musttail call tailcc void %clofunc49814(%struct.ScmObj* %ae45366, %struct.ScmObj* %args48612$ae45366$2)
ret void
}

define tailcc void @proc_clo$ae45198(%struct.ScmObj* %env$ae45198,%struct.ScmObj* %current_45args48589) {
%stackaddr$prim49815 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48589)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim49815, align 8
%stackaddr$prim49816 = alloca %struct.ScmObj*, align 8
%current_45args48590 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48589)
store volatile %struct.ScmObj* %current_45args48590, %struct.ScmObj** %stackaddr$prim49816, align 8
%stackaddr$prim49817 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48590)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim49817, align 8
%stackaddr$prim49818 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim49818, align 8
%args48592$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49819 = alloca %struct.ScmObj*, align 8
%args48592$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args48592$k$0)
store volatile %struct.ScmObj* %args48592$k$1, %struct.ScmObj** %stackaddr$prim49819, align 8
%clofunc49820 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc49820(%struct.ScmObj* %k, %struct.ScmObj* %args48592$k$1)
ret void
}

define tailcc void @proc_clo$ae45224(%struct.ScmObj* %env$ae45224,%struct.ScmObj* %current_45args48594) {
%stackaddr$env-ref49821 = alloca %struct.ScmObj*, align 8
%thunk4010240230 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae45224, i64 0)
store %struct.ScmObj* %thunk4010240230, %struct.ScmObj** %stackaddr$env-ref49821
%stackaddr$prim49822 = alloca %struct.ScmObj*, align 8
%_95k40405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48594)
store volatile %struct.ScmObj* %_95k40405, %struct.ScmObj** %stackaddr$prim49822, align 8
%stackaddr$prim49823 = alloca %struct.ScmObj*, align 8
%current_45args48595 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48594)
store volatile %struct.ScmObj* %current_45args48595, %struct.ScmObj** %stackaddr$prim49823, align 8
%stackaddr$prim49824 = alloca %struct.ScmObj*, align 8
%val4010340239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48595)
store volatile %struct.ScmObj* %val4010340239, %struct.ScmObj** %stackaddr$prim49824, align 8
%ae45230 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49825 = alloca %struct.ScmObj*, align 8
%t4010940238 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %ae45230, %struct.ScmObj* %val4010340239)
store volatile %struct.ScmObj* %t4010940238, %struct.ScmObj** %stackaddr$prim49825, align 8
%ae45233 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim49826 = alloca %struct.ScmObj*, align 8
%cpsprim40406 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4010240230, %struct.ScmObj* %ae45233)
store volatile %struct.ScmObj* %cpsprim40406, %struct.ScmObj** %stackaddr$prim49826, align 8
%stackaddr$makeclosure49827 = alloca %struct.ScmObj*, align 8
%fptrToInt49828 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae45234 to i64
%ae45234 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49828)
store volatile %struct.ScmObj* %ae45234, %struct.ScmObj** %stackaddr$makeclosure49827, align 8
%ae45235 = call %struct.ScmObj* @const_init_int(i64 0)
%args48601$ae45234$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49829 = alloca %struct.ScmObj*, align 8
%args48601$ae45234$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40406, %struct.ScmObj* %args48601$ae45234$0)
store volatile %struct.ScmObj* %args48601$ae45234$1, %struct.ScmObj** %stackaddr$prim49829, align 8
%stackaddr$prim49830 = alloca %struct.ScmObj*, align 8
%args48601$ae45234$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae45235, %struct.ScmObj* %args48601$ae45234$1)
store volatile %struct.ScmObj* %args48601$ae45234$2, %struct.ScmObj** %stackaddr$prim49830, align 8
%clofunc49831 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae45234)
musttail call tailcc void %clofunc49831(%struct.ScmObj* %ae45234, %struct.ScmObj* %args48601$ae45234$2)
ret void
}

define tailcc void @proc_clo$ae45234(%struct.ScmObj* %env$ae45234,%struct.ScmObj* %current_45args48597) {
%stackaddr$prim49832 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48597)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim49832, align 8
%stackaddr$prim49833 = alloca %struct.ScmObj*, align 8
%current_45args48598 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48597)
store volatile %struct.ScmObj* %current_45args48598, %struct.ScmObj** %stackaddr$prim49833, align 8
%stackaddr$prim49834 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48598)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim49834, align 8
%stackaddr$prim49835 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim49835, align 8
%args48600$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49836 = alloca %struct.ScmObj*, align 8
%args48600$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args48600$k$0)
store volatile %struct.ScmObj* %args48600$k$1, %struct.ScmObj** %stackaddr$prim49836, align 8
%clofunc49837 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc49837(%struct.ScmObj* %k, %struct.ScmObj* %args48600$k$1)
ret void
}

define tailcc void @proc_clo$ae45354(%struct.ScmObj* %env$ae45354,%struct.ScmObj* %current_45args48603) {
%stackaddr$prim49838 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48603)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim49838, align 8
%stackaddr$prim49839 = alloca %struct.ScmObj*, align 8
%current_45args48604 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48603)
store volatile %struct.ScmObj* %current_45args48604, %struct.ScmObj** %stackaddr$prim49839, align 8
%stackaddr$prim49840 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48604)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim49840, align 8
%stackaddr$prim49841 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim49841, align 8
%args48606$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49842 = alloca %struct.ScmObj*, align 8
%args48606$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args48606$k$0)
store volatile %struct.ScmObj* %args48606$k$1, %struct.ScmObj** %stackaddr$prim49842, align 8
%clofunc49843 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc49843(%struct.ScmObj* %k, %struct.ScmObj* %args48606$k$1)
ret void
}

define tailcc void @proc_clo$ae45366(%struct.ScmObj* %env$ae45366,%struct.ScmObj* %current_45args48608) {
%stackaddr$prim49844 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48608)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim49844, align 8
%stackaddr$prim49845 = alloca %struct.ScmObj*, align 8
%current_45args48609 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48608)
store volatile %struct.ScmObj* %current_45args48609, %struct.ScmObj** %stackaddr$prim49845, align 8
%stackaddr$prim49846 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48609)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim49846, align 8
%stackaddr$prim49847 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim49847, align 8
%args48611$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49848 = alloca %struct.ScmObj*, align 8
%args48611$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args48611$k$0)
store volatile %struct.ScmObj* %args48611$k$1, %struct.ScmObj** %stackaddr$prim49848, align 8
%clofunc49849 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc49849(%struct.ScmObj* %k, %struct.ScmObj* %args48611$k$1)
ret void
}

define tailcc void @proc_clo$ae44033(%struct.ScmObj* %env$ae44033,%struct.ScmObj* %current_45args48617) {
%stackaddr$prim49850 = alloca %struct.ScmObj*, align 8
%k40410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48617)
store volatile %struct.ScmObj* %k40410, %struct.ScmObj** %stackaddr$prim49850, align 8
%stackaddr$prim49851 = alloca %struct.ScmObj*, align 8
%current_45args48618 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48617)
store volatile %struct.ScmObj* %current_45args48618, %struct.ScmObj** %stackaddr$prim49851, align 8
%stackaddr$prim49852 = alloca %struct.ScmObj*, align 8
%_9540233 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48618)
store volatile %struct.ScmObj* %_9540233, %struct.ScmObj** %stackaddr$prim49852, align 8
%ae44035 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44036 = call %struct.ScmObj* @const_init_int(i64 29)
%args48620$k40410$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49853 = alloca %struct.ScmObj*, align 8
%args48620$k40410$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44036, %struct.ScmObj* %args48620$k40410$0)
store volatile %struct.ScmObj* %args48620$k40410$1, %struct.ScmObj** %stackaddr$prim49853, align 8
%stackaddr$prim49854 = alloca %struct.ScmObj*, align 8
%args48620$k40410$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44035, %struct.ScmObj* %args48620$k40410$1)
store volatile %struct.ScmObj* %args48620$k40410$2, %struct.ScmObj** %stackaddr$prim49854, align 8
%clofunc49855 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40410)
musttail call tailcc void %clofunc49855(%struct.ScmObj* %k40410, %struct.ScmObj* %args48620$k40410$2)
ret void
}

define tailcc void @proc_clo$ae44009(%struct.ScmObj* %env$ae44009,%struct.ScmObj* %el4023240411) {
%stackaddr$prim49856 = alloca %struct.ScmObj*, align 8
%k40412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %el4023240411)
store volatile %struct.ScmObj* %k40412, %struct.ScmObj** %stackaddr$prim49856, align 8
%stackaddr$prim49857 = alloca %struct.ScmObj*, align 8
%el40232 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %el4023240411)
store volatile %struct.ScmObj* %el40232, %struct.ScmObj** %stackaddr$prim49857, align 8
%stackaddr$applyprim49858 = alloca %struct.ScmObj*, align 8
%cpsaprim40413 = call %struct.ScmObj* @applyprim_vector(%struct.ScmObj* %el40232)
store volatile %struct.ScmObj* %cpsaprim40413, %struct.ScmObj** %stackaddr$applyprim49858, align 8
%ae44014 = call %struct.ScmObj* @const_init_int(i64 0)
%args48622$k40412$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49859 = alloca %struct.ScmObj*, align 8
%args48622$k40412$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim40413, %struct.ScmObj* %args48622$k40412$0)
store volatile %struct.ScmObj* %args48622$k40412$1, %struct.ScmObj** %stackaddr$prim49859, align 8
%stackaddr$prim49860 = alloca %struct.ScmObj*, align 8
%args48622$k40412$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44014, %struct.ScmObj* %args48622$k40412$1)
store volatile %struct.ScmObj* %args48622$k40412$2, %struct.ScmObj** %stackaddr$prim49860, align 8
%clofunc49861 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40412)
musttail call tailcc void %clofunc49861(%struct.ScmObj* %k40412, %struct.ScmObj* %args48622$k40412$2)
ret void
}

define tailcc void @proc_clo$ae43922(%struct.ScmObj* %env$ae43922,%struct.ScmObj* %current_45args48624) {
%stackaddr$prim49862 = alloca %struct.ScmObj*, align 8
%k40414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48624)
store volatile %struct.ScmObj* %k40414, %struct.ScmObj** %stackaddr$prim49862, align 8
%stackaddr$prim49863 = alloca %struct.ScmObj*, align 8
%current_45args48625 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48624)
store volatile %struct.ScmObj* %current_45args48625, %struct.ScmObj** %stackaddr$prim49863, align 8
%stackaddr$prim49864 = alloca %struct.ScmObj*, align 8
%thunk40229 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48625)
store volatile %struct.ScmObj* %thunk40229, %struct.ScmObj** %stackaddr$prim49864, align 8
%stackaddr$prim49865 = alloca %struct.ScmObj*, align 8
%anf_45bind40351 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk40229)
store volatile %struct.ScmObj* %anf_45bind40351, %struct.ScmObj** %stackaddr$prim49865, align 8
%truthy$cmp49866 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40351)
%cmp$cmp49866 = icmp eq i64 %truthy$cmp49866, 1
br i1 %cmp$cmp49866, label %truebranch$cmp49866, label %falsebranch$cmp49866
truebranch$cmp49866:
%stackaddr$prim49867 = alloca %struct.ScmObj*, align 8
%anf_45bind40352 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk40229)
store volatile %struct.ScmObj* %anf_45bind40352, %struct.ScmObj** %stackaddr$prim49867, align 8
%ae43927 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim49868 = alloca %struct.ScmObj*, align 8
%anf_45bind40353 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40352, %struct.ScmObj* %ae43927)
store volatile %struct.ScmObj* %anf_45bind40353, %struct.ScmObj** %stackaddr$prim49868, align 8
%truthy$cmp49869 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40353)
%cmp$cmp49869 = icmp eq i64 %truthy$cmp49869, 1
br i1 %cmp$cmp49869, label %truebranch$cmp49869, label %falsebranch$cmp49869
truebranch$cmp49869:
%ae43930 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49870 = alloca %struct.ScmObj*, align 8
%anf_45bind40354 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk40229, %struct.ScmObj* %ae43930)
store volatile %struct.ScmObj* %anf_45bind40354, %struct.ScmObj** %stackaddr$prim49870, align 8
%ae43932 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae4393249871, i32 0, i32 0))
%stackaddr$prim49872 = alloca %struct.ScmObj*, align 8
%cpsprim40415 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40354, %struct.ScmObj* %ae43932)
store volatile %struct.ScmObj* %cpsprim40415, %struct.ScmObj** %stackaddr$prim49872, align 8
%ae43934 = call %struct.ScmObj* @const_init_int(i64 0)
%args48627$k40414$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49873 = alloca %struct.ScmObj*, align 8
%args48627$k40414$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40415, %struct.ScmObj* %args48627$k40414$0)
store volatile %struct.ScmObj* %args48627$k40414$1, %struct.ScmObj** %stackaddr$prim49873, align 8
%stackaddr$prim49874 = alloca %struct.ScmObj*, align 8
%args48627$k40414$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43934, %struct.ScmObj* %args48627$k40414$1)
store volatile %struct.ScmObj* %args48627$k40414$2, %struct.ScmObj** %stackaddr$prim49874, align 8
%clofunc49875 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40414)
musttail call tailcc void %clofunc49875(%struct.ScmObj* %k40414, %struct.ScmObj* %args48627$k40414$2)
ret void
falsebranch$cmp49869:
%ae43952 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43953 = call %struct.ScmObj* @const_init_false()
%args48628$k40414$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49876 = alloca %struct.ScmObj*, align 8
%args48628$k40414$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43953, %struct.ScmObj* %args48628$k40414$0)
store volatile %struct.ScmObj* %args48628$k40414$1, %struct.ScmObj** %stackaddr$prim49876, align 8
%stackaddr$prim49877 = alloca %struct.ScmObj*, align 8
%args48628$k40414$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43952, %struct.ScmObj* %args48628$k40414$1)
store volatile %struct.ScmObj* %args48628$k40414$2, %struct.ScmObj** %stackaddr$prim49877, align 8
%clofunc49878 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40414)
musttail call tailcc void %clofunc49878(%struct.ScmObj* %k40414, %struct.ScmObj* %args48628$k40414$2)
ret void
falsebranch$cmp49866:
%ae43974 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43975 = call %struct.ScmObj* @const_init_false()
%args48629$k40414$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49879 = alloca %struct.ScmObj*, align 8
%args48629$k40414$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43975, %struct.ScmObj* %args48629$k40414$0)
store volatile %struct.ScmObj* %args48629$k40414$1, %struct.ScmObj** %stackaddr$prim49879, align 8
%stackaddr$prim49880 = alloca %struct.ScmObj*, align 8
%args48629$k40414$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43974, %struct.ScmObj* %args48629$k40414$1)
store volatile %struct.ScmObj* %args48629$k40414$2, %struct.ScmObj** %stackaddr$prim49880, align 8
%clofunc49881 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40414)
musttail call tailcc void %clofunc49881(%struct.ScmObj* %k40414, %struct.ScmObj* %args48629$k40414$2)
ret void
}

define tailcc void @proc_clo$ae43896(%struct.ScmObj* %env$ae43896,%struct.ScmObj* %current_45args48631) {
%stackaddr$prim49882 = alloca %struct.ScmObj*, align 8
%k40416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48631)
store volatile %struct.ScmObj* %k40416, %struct.ScmObj** %stackaddr$prim49882, align 8
%stackaddr$prim49883 = alloca %struct.ScmObj*, align 8
%current_45args48632 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48631)
store volatile %struct.ScmObj* %current_45args48632, %struct.ScmObj** %stackaddr$prim49883, align 8
%stackaddr$prim49884 = alloca %struct.ScmObj*, align 8
%x40168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48632)
store volatile %struct.ScmObj* %x40168, %struct.ScmObj** %stackaddr$prim49884, align 8
%stackaddr$prim49885 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40168)
store volatile %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$prim49885, align 8
%stackaddr$prim49886 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40348)
store volatile %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$prim49886, align 8
%stackaddr$prim49887 = alloca %struct.ScmObj*, align 8
%anf_45bind40350 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40349)
store volatile %struct.ScmObj* %anf_45bind40350, %struct.ScmObj** %stackaddr$prim49887, align 8
%stackaddr$prim49888 = alloca %struct.ScmObj*, align 8
%cpsprim40417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40350)
store volatile %struct.ScmObj* %cpsprim40417, %struct.ScmObj** %stackaddr$prim49888, align 8
%ae43902 = call %struct.ScmObj* @const_init_int(i64 0)
%args48634$k40416$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49889 = alloca %struct.ScmObj*, align 8
%args48634$k40416$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40417, %struct.ScmObj* %args48634$k40416$0)
store volatile %struct.ScmObj* %args48634$k40416$1, %struct.ScmObj** %stackaddr$prim49889, align 8
%stackaddr$prim49890 = alloca %struct.ScmObj*, align 8
%args48634$k40416$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43902, %struct.ScmObj* %args48634$k40416$1)
store volatile %struct.ScmObj* %args48634$k40416$2, %struct.ScmObj** %stackaddr$prim49890, align 8
%clofunc49891 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40416)
musttail call tailcc void %clofunc49891(%struct.ScmObj* %k40416, %struct.ScmObj* %args48634$k40416$2)
ret void
}

define tailcc void @proc_clo$ae43872(%struct.ScmObj* %env$ae43872,%struct.ScmObj* %current_45args48636) {
%stackaddr$prim49892 = alloca %struct.ScmObj*, align 8
%k40418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48636)
store volatile %struct.ScmObj* %k40418, %struct.ScmObj** %stackaddr$prim49892, align 8
%stackaddr$prim49893 = alloca %struct.ScmObj*, align 8
%current_45args48637 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48636)
store volatile %struct.ScmObj* %current_45args48637, %struct.ScmObj** %stackaddr$prim49893, align 8
%stackaddr$prim49894 = alloca %struct.ScmObj*, align 8
%x40170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48637)
store volatile %struct.ScmObj* %x40170, %struct.ScmObj** %stackaddr$prim49894, align 8
%stackaddr$prim49895 = alloca %struct.ScmObj*, align 8
%anf_45bind40346 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40170)
store volatile %struct.ScmObj* %anf_45bind40346, %struct.ScmObj** %stackaddr$prim49895, align 8
%stackaddr$prim49896 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40346)
store volatile %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$prim49896, align 8
%stackaddr$prim49897 = alloca %struct.ScmObj*, align 8
%cpsprim40419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40347)
store volatile %struct.ScmObj* %cpsprim40419, %struct.ScmObj** %stackaddr$prim49897, align 8
%ae43877 = call %struct.ScmObj* @const_init_int(i64 0)
%args48639$k40418$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49898 = alloca %struct.ScmObj*, align 8
%args48639$k40418$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40419, %struct.ScmObj* %args48639$k40418$0)
store volatile %struct.ScmObj* %args48639$k40418$1, %struct.ScmObj** %stackaddr$prim49898, align 8
%stackaddr$prim49899 = alloca %struct.ScmObj*, align 8
%args48639$k40418$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43877, %struct.ScmObj* %args48639$k40418$1)
store volatile %struct.ScmObj* %args48639$k40418$2, %struct.ScmObj** %stackaddr$prim49899, align 8
%clofunc49900 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40418)
musttail call tailcc void %clofunc49900(%struct.ScmObj* %k40418, %struct.ScmObj* %args48639$k40418$2)
ret void
}

define tailcc void @proc_clo$ae43850(%struct.ScmObj* %env$ae43850,%struct.ScmObj* %current_45args48641) {
%stackaddr$prim49901 = alloca %struct.ScmObj*, align 8
%k40420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48641)
store volatile %struct.ScmObj* %k40420, %struct.ScmObj** %stackaddr$prim49901, align 8
%stackaddr$prim49902 = alloca %struct.ScmObj*, align 8
%current_45args48642 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48641)
store volatile %struct.ScmObj* %current_45args48642, %struct.ScmObj** %stackaddr$prim49902, align 8
%stackaddr$prim49903 = alloca %struct.ScmObj*, align 8
%x40172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48642)
store volatile %struct.ScmObj* %x40172, %struct.ScmObj** %stackaddr$prim49903, align 8
%stackaddr$prim49904 = alloca %struct.ScmObj*, align 8
%anf_45bind40345 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40172)
store volatile %struct.ScmObj* %anf_45bind40345, %struct.ScmObj** %stackaddr$prim49904, align 8
%stackaddr$prim49905 = alloca %struct.ScmObj*, align 8
%cpsprim40421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40345)
store volatile %struct.ScmObj* %cpsprim40421, %struct.ScmObj** %stackaddr$prim49905, align 8
%ae43854 = call %struct.ScmObj* @const_init_int(i64 0)
%args48644$k40420$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49906 = alloca %struct.ScmObj*, align 8
%args48644$k40420$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40421, %struct.ScmObj* %args48644$k40420$0)
store volatile %struct.ScmObj* %args48644$k40420$1, %struct.ScmObj** %stackaddr$prim49906, align 8
%stackaddr$prim49907 = alloca %struct.ScmObj*, align 8
%args48644$k40420$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43854, %struct.ScmObj* %args48644$k40420$1)
store volatile %struct.ScmObj* %args48644$k40420$2, %struct.ScmObj** %stackaddr$prim49907, align 8
%clofunc49908 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40420)
musttail call tailcc void %clofunc49908(%struct.ScmObj* %k40420, %struct.ScmObj* %args48644$k40420$2)
ret void
}

define tailcc void @proc_clo$ae43830(%struct.ScmObj* %env$ae43830,%struct.ScmObj* %current_45args48646) {
%stackaddr$prim49909 = alloca %struct.ScmObj*, align 8
%k40422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48646)
store volatile %struct.ScmObj* %k40422, %struct.ScmObj** %stackaddr$prim49909, align 8
%stackaddr$prim49910 = alloca %struct.ScmObj*, align 8
%current_45args48647 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48646)
store volatile %struct.ScmObj* %current_45args48647, %struct.ScmObj** %stackaddr$prim49910, align 8
%stackaddr$prim49911 = alloca %struct.ScmObj*, align 8
%x40174 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48647)
store volatile %struct.ScmObj* %x40174, %struct.ScmObj** %stackaddr$prim49911, align 8
%stackaddr$prim49912 = alloca %struct.ScmObj*, align 8
%cpsprim40423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40174)
store volatile %struct.ScmObj* %cpsprim40423, %struct.ScmObj** %stackaddr$prim49912, align 8
%ae43833 = call %struct.ScmObj* @const_init_int(i64 0)
%args48649$k40422$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49913 = alloca %struct.ScmObj*, align 8
%args48649$k40422$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40423, %struct.ScmObj* %args48649$k40422$0)
store volatile %struct.ScmObj* %args48649$k40422$1, %struct.ScmObj** %stackaddr$prim49913, align 8
%stackaddr$prim49914 = alloca %struct.ScmObj*, align 8
%args48649$k40422$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43833, %struct.ScmObj* %args48649$k40422$1)
store volatile %struct.ScmObj* %args48649$k40422$2, %struct.ScmObj** %stackaddr$prim49914, align 8
%clofunc49915 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40422)
musttail call tailcc void %clofunc49915(%struct.ScmObj* %k40422, %struct.ScmObj* %args48649$k40422$2)
ret void
}

define tailcc void @proc_clo$ae43732(%struct.ScmObj* %env$ae43732,%struct.ScmObj* %args4017640424) {
%stackaddr$env-ref49916 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43732, i64 0)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref49916
%stackaddr$prim49917 = alloca %struct.ScmObj*, align 8
%k40425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4017640424)
store volatile %struct.ScmObj* %k40425, %struct.ScmObj** %stackaddr$prim49917, align 8
%stackaddr$prim49918 = alloca %struct.ScmObj*, align 8
%args40176 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4017640424)
store volatile %struct.ScmObj* %args40176, %struct.ScmObj** %stackaddr$prim49918, align 8
%stackaddr$prim49919 = alloca %struct.ScmObj*, align 8
%anf_45bind40339 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args40176)
store volatile %struct.ScmObj* %anf_45bind40339, %struct.ScmObj** %stackaddr$prim49919, align 8
%truthy$cmp49920 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40339)
%cmp$cmp49920 = icmp eq i64 %truthy$cmp49920, 1
br i1 %cmp$cmp49920, label %truebranch$cmp49920, label %falsebranch$cmp49920
truebranch$cmp49920:
%ae43738 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43739 = call %struct.ScmObj* @const_init_int(i64 1)
%args48651$k40425$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49921 = alloca %struct.ScmObj*, align 8
%args48651$k40425$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43739, %struct.ScmObj* %args48651$k40425$0)
store volatile %struct.ScmObj* %args48651$k40425$1, %struct.ScmObj** %stackaddr$prim49921, align 8
%stackaddr$prim49922 = alloca %struct.ScmObj*, align 8
%args48651$k40425$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43738, %struct.ScmObj* %args48651$k40425$1)
store volatile %struct.ScmObj* %args48651$k40425$2, %struct.ScmObj** %stackaddr$prim49922, align 8
%clofunc49923 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40425)
musttail call tailcc void %clofunc49923(%struct.ScmObj* %k40425, %struct.ScmObj* %args48651$k40425$2)
ret void
falsebranch$cmp49920:
%stackaddr$prim49924 = alloca %struct.ScmObj*, align 8
%anf_45bind40340 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40176)
store volatile %struct.ScmObj* %anf_45bind40340, %struct.ScmObj** %stackaddr$prim49924, align 8
%stackaddr$prim49925 = alloca %struct.ScmObj*, align 8
%anf_45bind40341 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40340)
store volatile %struct.ScmObj* %anf_45bind40341, %struct.ScmObj** %stackaddr$prim49925, align 8
%truthy$cmp49926 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40341)
%cmp$cmp49926 = icmp eq i64 %truthy$cmp49926, 1
br i1 %cmp$cmp49926, label %truebranch$cmp49926, label %falsebranch$cmp49926
truebranch$cmp49926:
%stackaddr$prim49927 = alloca %struct.ScmObj*, align 8
%cpsprim40426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40176)
store volatile %struct.ScmObj* %cpsprim40426, %struct.ScmObj** %stackaddr$prim49927, align 8
%ae43751 = call %struct.ScmObj* @const_init_int(i64 0)
%args48652$k40425$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49928 = alloca %struct.ScmObj*, align 8
%args48652$k40425$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40426, %struct.ScmObj* %args48652$k40425$0)
store volatile %struct.ScmObj* %args48652$k40425$1, %struct.ScmObj** %stackaddr$prim49928, align 8
%stackaddr$prim49929 = alloca %struct.ScmObj*, align 8
%args48652$k40425$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43751, %struct.ScmObj* %args48652$k40425$1)
store volatile %struct.ScmObj* %args48652$k40425$2, %struct.ScmObj** %stackaddr$prim49929, align 8
%clofunc49930 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40425)
musttail call tailcc void %clofunc49930(%struct.ScmObj* %k40425, %struct.ScmObj* %args48652$k40425$2)
ret void
falsebranch$cmp49926:
%stackaddr$makeclosure49931 = alloca %struct.ScmObj*, align 8
%fptrToInt49932 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43756 to i64
%ae43756 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49932)
store volatile %struct.ScmObj* %ae43756, %struct.ScmObj** %stackaddr$makeclosure49931, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43756, %struct.ScmObj* %k40425, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43756, %struct.ScmObj* %_37foldl140115, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43756, %struct.ScmObj* %args40176, i64 2)
%ae43757 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49933 = alloca %struct.ScmObj*, align 8
%fptrToInt49934 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43758 to i64
%ae43758 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49934)
store volatile %struct.ScmObj* %ae43758, %struct.ScmObj** %stackaddr$makeclosure49933, align 8
%args48662$ae43756$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49935 = alloca %struct.ScmObj*, align 8
%args48662$ae43756$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43758, %struct.ScmObj* %args48662$ae43756$0)
store volatile %struct.ScmObj* %args48662$ae43756$1, %struct.ScmObj** %stackaddr$prim49935, align 8
%stackaddr$prim49936 = alloca %struct.ScmObj*, align 8
%args48662$ae43756$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43757, %struct.ScmObj* %args48662$ae43756$1)
store volatile %struct.ScmObj* %args48662$ae43756$2, %struct.ScmObj** %stackaddr$prim49936, align 8
%clofunc49937 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43756)
musttail call tailcc void %clofunc49937(%struct.ScmObj* %ae43756, %struct.ScmObj* %args48662$ae43756$2)
ret void
}

define tailcc void @proc_clo$ae43756(%struct.ScmObj* %env$ae43756,%struct.ScmObj* %current_45args48653) {
%stackaddr$env-ref49938 = alloca %struct.ScmObj*, align 8
%k40425 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43756, i64 0)
store %struct.ScmObj* %k40425, %struct.ScmObj** %stackaddr$env-ref49938
%stackaddr$env-ref49939 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43756, i64 1)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref49939
%stackaddr$env-ref49940 = alloca %struct.ScmObj*, align 8
%args40176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43756, i64 2)
store %struct.ScmObj* %args40176, %struct.ScmObj** %stackaddr$env-ref49940
%stackaddr$prim49941 = alloca %struct.ScmObj*, align 8
%_95k40427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48653)
store volatile %struct.ScmObj* %_95k40427, %struct.ScmObj** %stackaddr$prim49941, align 8
%stackaddr$prim49942 = alloca %struct.ScmObj*, align 8
%current_45args48654 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48653)
store volatile %struct.ScmObj* %current_45args48654, %struct.ScmObj** %stackaddr$prim49942, align 8
%stackaddr$prim49943 = alloca %struct.ScmObj*, align 8
%anf_45bind40342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48654)
store volatile %struct.ScmObj* %anf_45bind40342, %struct.ScmObj** %stackaddr$prim49943, align 8
%stackaddr$prim49944 = alloca %struct.ScmObj*, align 8
%anf_45bind40343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40176)
store volatile %struct.ScmObj* %anf_45bind40343, %struct.ScmObj** %stackaddr$prim49944, align 8
%stackaddr$prim49945 = alloca %struct.ScmObj*, align 8
%anf_45bind40344 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40176)
store volatile %struct.ScmObj* %anf_45bind40344, %struct.ScmObj** %stackaddr$prim49945, align 8
%args48656$_37foldl140115$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49946 = alloca %struct.ScmObj*, align 8
%args48656$_37foldl140115$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40344, %struct.ScmObj* %args48656$_37foldl140115$0)
store volatile %struct.ScmObj* %args48656$_37foldl140115$1, %struct.ScmObj** %stackaddr$prim49946, align 8
%stackaddr$prim49947 = alloca %struct.ScmObj*, align 8
%args48656$_37foldl140115$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40343, %struct.ScmObj* %args48656$_37foldl140115$1)
store volatile %struct.ScmObj* %args48656$_37foldl140115$2, %struct.ScmObj** %stackaddr$prim49947, align 8
%stackaddr$prim49948 = alloca %struct.ScmObj*, align 8
%args48656$_37foldl140115$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40342, %struct.ScmObj* %args48656$_37foldl140115$2)
store volatile %struct.ScmObj* %args48656$_37foldl140115$3, %struct.ScmObj** %stackaddr$prim49948, align 8
%stackaddr$prim49949 = alloca %struct.ScmObj*, align 8
%args48656$_37foldl140115$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40425, %struct.ScmObj* %args48656$_37foldl140115$3)
store volatile %struct.ScmObj* %args48656$_37foldl140115$4, %struct.ScmObj** %stackaddr$prim49949, align 8
%clofunc49950 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140115)
musttail call tailcc void %clofunc49950(%struct.ScmObj* %_37foldl140115, %struct.ScmObj* %args48656$_37foldl140115$4)
ret void
}

define tailcc void @proc_clo$ae43758(%struct.ScmObj* %env$ae43758,%struct.ScmObj* %current_45args48657) {
%stackaddr$prim49951 = alloca %struct.ScmObj*, align 8
%k40428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48657)
store volatile %struct.ScmObj* %k40428, %struct.ScmObj** %stackaddr$prim49951, align 8
%stackaddr$prim49952 = alloca %struct.ScmObj*, align 8
%current_45args48658 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48657)
store volatile %struct.ScmObj* %current_45args48658, %struct.ScmObj** %stackaddr$prim49952, align 8
%stackaddr$prim49953 = alloca %struct.ScmObj*, align 8
%n40178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48658)
store volatile %struct.ScmObj* %n40178, %struct.ScmObj** %stackaddr$prim49953, align 8
%stackaddr$prim49954 = alloca %struct.ScmObj*, align 8
%current_45args48659 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48658)
store volatile %struct.ScmObj* %current_45args48659, %struct.ScmObj** %stackaddr$prim49954, align 8
%stackaddr$prim49955 = alloca %struct.ScmObj*, align 8
%v40177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48659)
store volatile %struct.ScmObj* %v40177, %struct.ScmObj** %stackaddr$prim49955, align 8
%stackaddr$prim49956 = alloca %struct.ScmObj*, align 8
%cpsprim40429 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v40177, %struct.ScmObj* %n40178)
store volatile %struct.ScmObj* %cpsprim40429, %struct.ScmObj** %stackaddr$prim49956, align 8
%ae43762 = call %struct.ScmObj* @const_init_int(i64 0)
%args48661$k40428$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49957 = alloca %struct.ScmObj*, align 8
%args48661$k40428$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40429, %struct.ScmObj* %args48661$k40428$0)
store volatile %struct.ScmObj* %args48661$k40428$1, %struct.ScmObj** %stackaddr$prim49957, align 8
%stackaddr$prim49958 = alloca %struct.ScmObj*, align 8
%args48661$k40428$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43762, %struct.ScmObj* %args48661$k40428$1)
store volatile %struct.ScmObj* %args48661$k40428$2, %struct.ScmObj** %stackaddr$prim49958, align 8
%clofunc49959 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40428)
musttail call tailcc void %clofunc49959(%struct.ScmObj* %k40428, %struct.ScmObj* %args48661$k40428$2)
ret void
}

define tailcc void @proc_clo$ae43328(%struct.ScmObj* %env$ae43328,%struct.ScmObj* %current_45args48664) {
%stackaddr$prim49960 = alloca %struct.ScmObj*, align 8
%k40430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48664)
store volatile %struct.ScmObj* %k40430, %struct.ScmObj** %stackaddr$prim49960, align 8
%stackaddr$prim49961 = alloca %struct.ScmObj*, align 8
%current_45args48665 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48664)
store volatile %struct.ScmObj* %current_45args48665, %struct.ScmObj** %stackaddr$prim49961, align 8
%stackaddr$prim49962 = alloca %struct.ScmObj*, align 8
%v40181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48665)
store volatile %struct.ScmObj* %v40181, %struct.ScmObj** %stackaddr$prim49962, align 8
%stackaddr$prim49963 = alloca %struct.ScmObj*, align 8
%current_45args48666 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48665)
store volatile %struct.ScmObj* %current_45args48666, %struct.ScmObj** %stackaddr$prim49963, align 8
%stackaddr$prim49964 = alloca %struct.ScmObj*, align 8
%lst40180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48666)
store volatile %struct.ScmObj* %lst40180, %struct.ScmObj** %stackaddr$prim49964, align 8
%ae43329 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49965 = alloca %struct.ScmObj*, align 8
%lst40182 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae43329, %struct.ScmObj* %lst40180)
store volatile %struct.ScmObj* %lst40182, %struct.ScmObj** %stackaddr$prim49965, align 8
%stackaddr$makeclosure49966 = alloca %struct.ScmObj*, align 8
%fptrToInt49967 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43331 to i64
%ae43331 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49967)
store volatile %struct.ScmObj* %ae43331, %struct.ScmObj** %stackaddr$makeclosure49966, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43331, %struct.ScmObj* %lst40182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43331, %struct.ScmObj* %v40181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43331, %struct.ScmObj* %k40430, i64 2)
%ae43332 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49968 = alloca %struct.ScmObj*, align 8
%fptrToInt49969 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43333 to i64
%ae43333 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49969)
store volatile %struct.ScmObj* %ae43333, %struct.ScmObj** %stackaddr$makeclosure49968, align 8
%args48688$ae43331$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49970 = alloca %struct.ScmObj*, align 8
%args48688$ae43331$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43333, %struct.ScmObj* %args48688$ae43331$0)
store volatile %struct.ScmObj* %args48688$ae43331$1, %struct.ScmObj** %stackaddr$prim49970, align 8
%stackaddr$prim49971 = alloca %struct.ScmObj*, align 8
%args48688$ae43331$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43332, %struct.ScmObj* %args48688$ae43331$1)
store volatile %struct.ScmObj* %args48688$ae43331$2, %struct.ScmObj** %stackaddr$prim49971, align 8
%clofunc49972 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43331)
musttail call tailcc void %clofunc49972(%struct.ScmObj* %ae43331, %struct.ScmObj* %args48688$ae43331$2)
ret void
}

define tailcc void @proc_clo$ae43331(%struct.ScmObj* %env$ae43331,%struct.ScmObj* %current_45args48668) {
%stackaddr$env-ref49973 = alloca %struct.ScmObj*, align 8
%lst40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43331, i64 0)
store %struct.ScmObj* %lst40182, %struct.ScmObj** %stackaddr$env-ref49973
%stackaddr$env-ref49974 = alloca %struct.ScmObj*, align 8
%v40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43331, i64 1)
store %struct.ScmObj* %v40181, %struct.ScmObj** %stackaddr$env-ref49974
%stackaddr$env-ref49975 = alloca %struct.ScmObj*, align 8
%k40430 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43331, i64 2)
store %struct.ScmObj* %k40430, %struct.ScmObj** %stackaddr$env-ref49975
%stackaddr$prim49976 = alloca %struct.ScmObj*, align 8
%_95k40431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48668)
store volatile %struct.ScmObj* %_95k40431, %struct.ScmObj** %stackaddr$prim49976, align 8
%stackaddr$prim49977 = alloca %struct.ScmObj*, align 8
%current_45args48669 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48668)
store volatile %struct.ScmObj* %current_45args48669, %struct.ScmObj** %stackaddr$prim49977, align 8
%stackaddr$prim49978 = alloca %struct.ScmObj*, align 8
%anf_45bind40331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48669)
store volatile %struct.ScmObj* %anf_45bind40331, %struct.ScmObj** %stackaddr$prim49978, align 8
%stackaddr$makeclosure49979 = alloca %struct.ScmObj*, align 8
%fptrToInt49980 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43347 to i64
%ae43347 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49980)
store volatile %struct.ScmObj* %ae43347, %struct.ScmObj** %stackaddr$makeclosure49979, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43347, %struct.ScmObj* %lst40182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43347, %struct.ScmObj* %v40181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43347, %struct.ScmObj* %k40430, i64 2)
%stackaddr$makeclosure49981 = alloca %struct.ScmObj*, align 8
%fptrToInt49982 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43348 to i64
%ae43348 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49982)
store volatile %struct.ScmObj* %ae43348, %struct.ScmObj** %stackaddr$makeclosure49981, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43348, %struct.ScmObj* %lst40182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43348, %struct.ScmObj* %v40181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43348, %struct.ScmObj* %k40430, i64 2)
%args48683$anf_45bind40331$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49983 = alloca %struct.ScmObj*, align 8
%args48683$anf_45bind40331$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43348, %struct.ScmObj* %args48683$anf_45bind40331$0)
store volatile %struct.ScmObj* %args48683$anf_45bind40331$1, %struct.ScmObj** %stackaddr$prim49983, align 8
%stackaddr$prim49984 = alloca %struct.ScmObj*, align 8
%args48683$anf_45bind40331$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43347, %struct.ScmObj* %args48683$anf_45bind40331$1)
store volatile %struct.ScmObj* %args48683$anf_45bind40331$2, %struct.ScmObj** %stackaddr$prim49984, align 8
%clofunc49985 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40331)
musttail call tailcc void %clofunc49985(%struct.ScmObj* %anf_45bind40331, %struct.ScmObj* %args48683$anf_45bind40331$2)
ret void
}

define tailcc void @proc_clo$ae43347(%struct.ScmObj* %env$ae43347,%struct.ScmObj* %current_45args48671) {
%stackaddr$env-ref49986 = alloca %struct.ScmObj*, align 8
%lst40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43347, i64 0)
store %struct.ScmObj* %lst40182, %struct.ScmObj** %stackaddr$env-ref49986
%stackaddr$env-ref49987 = alloca %struct.ScmObj*, align 8
%v40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43347, i64 1)
store %struct.ScmObj* %v40181, %struct.ScmObj** %stackaddr$env-ref49987
%stackaddr$env-ref49988 = alloca %struct.ScmObj*, align 8
%k40430 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43347, i64 2)
store %struct.ScmObj* %k40430, %struct.ScmObj** %stackaddr$env-ref49988
%stackaddr$prim49989 = alloca %struct.ScmObj*, align 8
%_95k40432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48671)
store volatile %struct.ScmObj* %_95k40432, %struct.ScmObj** %stackaddr$prim49989, align 8
%stackaddr$prim49990 = alloca %struct.ScmObj*, align 8
%current_45args48672 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48671)
store volatile %struct.ScmObj* %current_45args48672, %struct.ScmObj** %stackaddr$prim49990, align 8
%stackaddr$prim49991 = alloca %struct.ScmObj*, align 8
%cc40183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48672)
store volatile %struct.ScmObj* %cc40183, %struct.ScmObj** %stackaddr$prim49991, align 8
%ae43456 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49992 = alloca %struct.ScmObj*, align 8
%anf_45bind40332 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae43456)
store volatile %struct.ScmObj* %anf_45bind40332, %struct.ScmObj** %stackaddr$prim49992, align 8
%stackaddr$prim49993 = alloca %struct.ScmObj*, align 8
%anf_45bind40333 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40332)
store volatile %struct.ScmObj* %anf_45bind40333, %struct.ScmObj** %stackaddr$prim49993, align 8
%truthy$cmp49994 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40333)
%cmp$cmp49994 = icmp eq i64 %truthy$cmp49994, 1
br i1 %cmp$cmp49994, label %truebranch$cmp49994, label %falsebranch$cmp49994
truebranch$cmp49994:
%ae43460 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43461 = call %struct.ScmObj* @const_init_false()
%args48674$k40430$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49995 = alloca %struct.ScmObj*, align 8
%args48674$k40430$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43461, %struct.ScmObj* %args48674$k40430$0)
store volatile %struct.ScmObj* %args48674$k40430$1, %struct.ScmObj** %stackaddr$prim49995, align 8
%stackaddr$prim49996 = alloca %struct.ScmObj*, align 8
%args48674$k40430$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43460, %struct.ScmObj* %args48674$k40430$1)
store volatile %struct.ScmObj* %args48674$k40430$2, %struct.ScmObj** %stackaddr$prim49996, align 8
%clofunc49997 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40430)
musttail call tailcc void %clofunc49997(%struct.ScmObj* %k40430, %struct.ScmObj* %args48674$k40430$2)
ret void
falsebranch$cmp49994:
%ae43469 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49998 = alloca %struct.ScmObj*, align 8
%anf_45bind40334 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae43469)
store volatile %struct.ScmObj* %anf_45bind40334, %struct.ScmObj** %stackaddr$prim49998, align 8
%stackaddr$prim49999 = alloca %struct.ScmObj*, align 8
%anf_45bind40335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40334)
store volatile %struct.ScmObj* %anf_45bind40335, %struct.ScmObj** %stackaddr$prim49999, align 8
%stackaddr$prim50000 = alloca %struct.ScmObj*, align 8
%anf_45bind40336 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40335, %struct.ScmObj* %v40181)
store volatile %struct.ScmObj* %anf_45bind40336, %struct.ScmObj** %stackaddr$prim50000, align 8
%truthy$cmp50001 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40336)
%cmp$cmp50001 = icmp eq i64 %truthy$cmp50001, 1
br i1 %cmp$cmp50001, label %truebranch$cmp50001, label %falsebranch$cmp50001
truebranch$cmp50001:
%ae43475 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50002 = alloca %struct.ScmObj*, align 8
%cpsprim40433 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae43475)
store volatile %struct.ScmObj* %cpsprim40433, %struct.ScmObj** %stackaddr$prim50002, align 8
%ae43477 = call %struct.ScmObj* @const_init_int(i64 0)
%args48675$k40430$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50003 = alloca %struct.ScmObj*, align 8
%args48675$k40430$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40433, %struct.ScmObj* %args48675$k40430$0)
store volatile %struct.ScmObj* %args48675$k40430$1, %struct.ScmObj** %stackaddr$prim50003, align 8
%stackaddr$prim50004 = alloca %struct.ScmObj*, align 8
%args48675$k40430$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43477, %struct.ScmObj* %args48675$k40430$1)
store volatile %struct.ScmObj* %args48675$k40430$2, %struct.ScmObj** %stackaddr$prim50004, align 8
%clofunc50005 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40430)
musttail call tailcc void %clofunc50005(%struct.ScmObj* %k40430, %struct.ScmObj* %args48675$k40430$2)
ret void
falsebranch$cmp50001:
%ae43488 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50006 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae43488)
store volatile %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$prim50006, align 8
%stackaddr$prim50007 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40337)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim50007, align 8
%ae43491 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50008 = alloca %struct.ScmObj*, align 8
%_95040185 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae43491, %struct.ScmObj* %anf_45bind40338)
store volatile %struct.ScmObj* %_95040185, %struct.ScmObj** %stackaddr$prim50008, align 8
%args48676$cc40183$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50009 = alloca %struct.ScmObj*, align 8
%args48676$cc40183$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40183, %struct.ScmObj* %args48676$cc40183$0)
store volatile %struct.ScmObj* %args48676$cc40183$1, %struct.ScmObj** %stackaddr$prim50009, align 8
%stackaddr$prim50010 = alloca %struct.ScmObj*, align 8
%args48676$cc40183$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40430, %struct.ScmObj* %args48676$cc40183$1)
store volatile %struct.ScmObj* %args48676$cc40183$2, %struct.ScmObj** %stackaddr$prim50010, align 8
%clofunc50011 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40183)
musttail call tailcc void %clofunc50011(%struct.ScmObj* %cc40183, %struct.ScmObj* %args48676$cc40183$2)
ret void
}

define tailcc void @proc_clo$ae43348(%struct.ScmObj* %env$ae43348,%struct.ScmObj* %current_45args48677) {
%stackaddr$env-ref50012 = alloca %struct.ScmObj*, align 8
%lst40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43348, i64 0)
store %struct.ScmObj* %lst40182, %struct.ScmObj** %stackaddr$env-ref50012
%stackaddr$env-ref50013 = alloca %struct.ScmObj*, align 8
%v40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43348, i64 1)
store %struct.ScmObj* %v40181, %struct.ScmObj** %stackaddr$env-ref50013
%stackaddr$env-ref50014 = alloca %struct.ScmObj*, align 8
%k40430 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43348, i64 2)
store %struct.ScmObj* %k40430, %struct.ScmObj** %stackaddr$env-ref50014
%stackaddr$prim50015 = alloca %struct.ScmObj*, align 8
%_95k40432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48677)
store volatile %struct.ScmObj* %_95k40432, %struct.ScmObj** %stackaddr$prim50015, align 8
%stackaddr$prim50016 = alloca %struct.ScmObj*, align 8
%current_45args48678 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48677)
store volatile %struct.ScmObj* %current_45args48678, %struct.ScmObj** %stackaddr$prim50016, align 8
%stackaddr$prim50017 = alloca %struct.ScmObj*, align 8
%cc40183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48678)
store volatile %struct.ScmObj* %cc40183, %struct.ScmObj** %stackaddr$prim50017, align 8
%ae43350 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50018 = alloca %struct.ScmObj*, align 8
%anf_45bind40332 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae43350)
store volatile %struct.ScmObj* %anf_45bind40332, %struct.ScmObj** %stackaddr$prim50018, align 8
%stackaddr$prim50019 = alloca %struct.ScmObj*, align 8
%anf_45bind40333 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40332)
store volatile %struct.ScmObj* %anf_45bind40333, %struct.ScmObj** %stackaddr$prim50019, align 8
%truthy$cmp50020 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40333)
%cmp$cmp50020 = icmp eq i64 %truthy$cmp50020, 1
br i1 %cmp$cmp50020, label %truebranch$cmp50020, label %falsebranch$cmp50020
truebranch$cmp50020:
%ae43354 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43355 = call %struct.ScmObj* @const_init_false()
%args48680$k40430$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50021 = alloca %struct.ScmObj*, align 8
%args48680$k40430$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43355, %struct.ScmObj* %args48680$k40430$0)
store volatile %struct.ScmObj* %args48680$k40430$1, %struct.ScmObj** %stackaddr$prim50021, align 8
%stackaddr$prim50022 = alloca %struct.ScmObj*, align 8
%args48680$k40430$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43354, %struct.ScmObj* %args48680$k40430$1)
store volatile %struct.ScmObj* %args48680$k40430$2, %struct.ScmObj** %stackaddr$prim50022, align 8
%clofunc50023 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40430)
musttail call tailcc void %clofunc50023(%struct.ScmObj* %k40430, %struct.ScmObj* %args48680$k40430$2)
ret void
falsebranch$cmp50020:
%ae43363 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50024 = alloca %struct.ScmObj*, align 8
%anf_45bind40334 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae43363)
store volatile %struct.ScmObj* %anf_45bind40334, %struct.ScmObj** %stackaddr$prim50024, align 8
%stackaddr$prim50025 = alloca %struct.ScmObj*, align 8
%anf_45bind40335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40334)
store volatile %struct.ScmObj* %anf_45bind40335, %struct.ScmObj** %stackaddr$prim50025, align 8
%stackaddr$prim50026 = alloca %struct.ScmObj*, align 8
%anf_45bind40336 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40335, %struct.ScmObj* %v40181)
store volatile %struct.ScmObj* %anf_45bind40336, %struct.ScmObj** %stackaddr$prim50026, align 8
%truthy$cmp50027 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40336)
%cmp$cmp50027 = icmp eq i64 %truthy$cmp50027, 1
br i1 %cmp$cmp50027, label %truebranch$cmp50027, label %falsebranch$cmp50027
truebranch$cmp50027:
%ae43369 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50028 = alloca %struct.ScmObj*, align 8
%cpsprim40433 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae43369)
store volatile %struct.ScmObj* %cpsprim40433, %struct.ScmObj** %stackaddr$prim50028, align 8
%ae43371 = call %struct.ScmObj* @const_init_int(i64 0)
%args48681$k40430$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50029 = alloca %struct.ScmObj*, align 8
%args48681$k40430$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40433, %struct.ScmObj* %args48681$k40430$0)
store volatile %struct.ScmObj* %args48681$k40430$1, %struct.ScmObj** %stackaddr$prim50029, align 8
%stackaddr$prim50030 = alloca %struct.ScmObj*, align 8
%args48681$k40430$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43371, %struct.ScmObj* %args48681$k40430$1)
store volatile %struct.ScmObj* %args48681$k40430$2, %struct.ScmObj** %stackaddr$prim50030, align 8
%clofunc50031 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40430)
musttail call tailcc void %clofunc50031(%struct.ScmObj* %k40430, %struct.ScmObj* %args48681$k40430$2)
ret void
falsebranch$cmp50027:
%ae43382 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50032 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae43382)
store volatile %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$prim50032, align 8
%stackaddr$prim50033 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40337)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim50033, align 8
%ae43385 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50034 = alloca %struct.ScmObj*, align 8
%_95040185 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40182, %struct.ScmObj* %ae43385, %struct.ScmObj* %anf_45bind40338)
store volatile %struct.ScmObj* %_95040185, %struct.ScmObj** %stackaddr$prim50034, align 8
%args48682$cc40183$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50035 = alloca %struct.ScmObj*, align 8
%args48682$cc40183$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40183, %struct.ScmObj* %args48682$cc40183$0)
store volatile %struct.ScmObj* %args48682$cc40183$1, %struct.ScmObj** %stackaddr$prim50035, align 8
%stackaddr$prim50036 = alloca %struct.ScmObj*, align 8
%args48682$cc40183$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40430, %struct.ScmObj* %args48682$cc40183$1)
store volatile %struct.ScmObj* %args48682$cc40183$2, %struct.ScmObj** %stackaddr$prim50036, align 8
%clofunc50037 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40183)
musttail call tailcc void %clofunc50037(%struct.ScmObj* %cc40183, %struct.ScmObj* %args48682$cc40183$2)
ret void
}

define tailcc void @proc_clo$ae43333(%struct.ScmObj* %env$ae43333,%struct.ScmObj* %current_45args48684) {
%stackaddr$prim50038 = alloca %struct.ScmObj*, align 8
%k40434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48684)
store volatile %struct.ScmObj* %k40434, %struct.ScmObj** %stackaddr$prim50038, align 8
%stackaddr$prim50039 = alloca %struct.ScmObj*, align 8
%current_45args48685 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48684)
store volatile %struct.ScmObj* %current_45args48685, %struct.ScmObj** %stackaddr$prim50039, align 8
%stackaddr$prim50040 = alloca %struct.ScmObj*, align 8
%u40184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48685)
store volatile %struct.ScmObj* %u40184, %struct.ScmObj** %stackaddr$prim50040, align 8
%args48687$u40184$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50041 = alloca %struct.ScmObj*, align 8
%args48687$u40184$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40184, %struct.ScmObj* %args48687$u40184$0)
store volatile %struct.ScmObj* %args48687$u40184$1, %struct.ScmObj** %stackaddr$prim50041, align 8
%stackaddr$prim50042 = alloca %struct.ScmObj*, align 8
%args48687$u40184$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40434, %struct.ScmObj* %args48687$u40184$1)
store volatile %struct.ScmObj* %args48687$u40184$2, %struct.ScmObj** %stackaddr$prim50042, align 8
%clofunc50043 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40184)
musttail call tailcc void %clofunc50043(%struct.ScmObj* %u40184, %struct.ScmObj* %args48687$u40184$2)
ret void
}

define tailcc void @proc_clo$ae42792(%struct.ScmObj* %env$ae42792,%struct.ScmObj* %current_45args48690) {
%stackaddr$prim50044 = alloca %struct.ScmObj*, align 8
%k40435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48690)
store volatile %struct.ScmObj* %k40435, %struct.ScmObj** %stackaddr$prim50044, align 8
%stackaddr$prim50045 = alloca %struct.ScmObj*, align 8
%current_45args48691 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48690)
store volatile %struct.ScmObj* %current_45args48691, %struct.ScmObj** %stackaddr$prim50045, align 8
%stackaddr$prim50046 = alloca %struct.ScmObj*, align 8
%lst40188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48691)
store volatile %struct.ScmObj* %lst40188, %struct.ScmObj** %stackaddr$prim50046, align 8
%stackaddr$prim50047 = alloca %struct.ScmObj*, align 8
%current_45args48692 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48691)
store volatile %struct.ScmObj* %current_45args48692, %struct.ScmObj** %stackaddr$prim50047, align 8
%stackaddr$prim50048 = alloca %struct.ScmObj*, align 8
%n40187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48692)
store volatile %struct.ScmObj* %n40187, %struct.ScmObj** %stackaddr$prim50048, align 8
%ae42793 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50049 = alloca %struct.ScmObj*, align 8
%n40190 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42793, %struct.ScmObj* %n40187)
store volatile %struct.ScmObj* %n40190, %struct.ScmObj** %stackaddr$prim50049, align 8
%ae42795 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50050 = alloca %struct.ScmObj*, align 8
%lst40189 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42795, %struct.ScmObj* %lst40188)
store volatile %struct.ScmObj* %lst40189, %struct.ScmObj** %stackaddr$prim50050, align 8
%stackaddr$makeclosure50051 = alloca %struct.ScmObj*, align 8
%fptrToInt50052 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42797 to i64
%ae42797 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50052)
store volatile %struct.ScmObj* %ae42797, %struct.ScmObj** %stackaddr$makeclosure50051, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42797, %struct.ScmObj* %k40435, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42797, %struct.ScmObj* %n40190, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42797, %struct.ScmObj* %lst40189, i64 2)
%ae42798 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50053 = alloca %struct.ScmObj*, align 8
%fptrToInt50054 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42799 to i64
%ae42799 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50054)
store volatile %struct.ScmObj* %ae42799, %struct.ScmObj** %stackaddr$makeclosure50053, align 8
%args48712$ae42797$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50055 = alloca %struct.ScmObj*, align 8
%args48712$ae42797$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42799, %struct.ScmObj* %args48712$ae42797$0)
store volatile %struct.ScmObj* %args48712$ae42797$1, %struct.ScmObj** %stackaddr$prim50055, align 8
%stackaddr$prim50056 = alloca %struct.ScmObj*, align 8
%args48712$ae42797$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42798, %struct.ScmObj* %args48712$ae42797$1)
store volatile %struct.ScmObj* %args48712$ae42797$2, %struct.ScmObj** %stackaddr$prim50056, align 8
%clofunc50057 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42797)
musttail call tailcc void %clofunc50057(%struct.ScmObj* %ae42797, %struct.ScmObj* %args48712$ae42797$2)
ret void
}

define tailcc void @proc_clo$ae42797(%struct.ScmObj* %env$ae42797,%struct.ScmObj* %current_45args48694) {
%stackaddr$env-ref50058 = alloca %struct.ScmObj*, align 8
%k40435 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42797, i64 0)
store %struct.ScmObj* %k40435, %struct.ScmObj** %stackaddr$env-ref50058
%stackaddr$env-ref50059 = alloca %struct.ScmObj*, align 8
%n40190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42797, i64 1)
store %struct.ScmObj* %n40190, %struct.ScmObj** %stackaddr$env-ref50059
%stackaddr$env-ref50060 = alloca %struct.ScmObj*, align 8
%lst40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42797, i64 2)
store %struct.ScmObj* %lst40189, %struct.ScmObj** %stackaddr$env-ref50060
%stackaddr$prim50061 = alloca %struct.ScmObj*, align 8
%_95k40436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48694)
store volatile %struct.ScmObj* %_95k40436, %struct.ScmObj** %stackaddr$prim50061, align 8
%stackaddr$prim50062 = alloca %struct.ScmObj*, align 8
%current_45args48695 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48694)
store volatile %struct.ScmObj* %current_45args48695, %struct.ScmObj** %stackaddr$prim50062, align 8
%stackaddr$prim50063 = alloca %struct.ScmObj*, align 8
%anf_45bind40324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48695)
store volatile %struct.ScmObj* %anf_45bind40324, %struct.ScmObj** %stackaddr$prim50063, align 8
%stackaddr$makeclosure50064 = alloca %struct.ScmObj*, align 8
%fptrToInt50065 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42813 to i64
%ae42813 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50065)
store volatile %struct.ScmObj* %ae42813, %struct.ScmObj** %stackaddr$makeclosure50064, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42813, %struct.ScmObj* %k40435, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42813, %struct.ScmObj* %n40190, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42813, %struct.ScmObj* %lst40189, i64 2)
%stackaddr$makeclosure50066 = alloca %struct.ScmObj*, align 8
%fptrToInt50067 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42814 to i64
%ae42814 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50067)
store volatile %struct.ScmObj* %ae42814, %struct.ScmObj** %stackaddr$makeclosure50066, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42814, %struct.ScmObj* %k40435, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42814, %struct.ScmObj* %n40190, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42814, %struct.ScmObj* %lst40189, i64 2)
%args48707$anf_45bind40324$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50068 = alloca %struct.ScmObj*, align 8
%args48707$anf_45bind40324$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42814, %struct.ScmObj* %args48707$anf_45bind40324$0)
store volatile %struct.ScmObj* %args48707$anf_45bind40324$1, %struct.ScmObj** %stackaddr$prim50068, align 8
%stackaddr$prim50069 = alloca %struct.ScmObj*, align 8
%args48707$anf_45bind40324$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42813, %struct.ScmObj* %args48707$anf_45bind40324$1)
store volatile %struct.ScmObj* %args48707$anf_45bind40324$2, %struct.ScmObj** %stackaddr$prim50069, align 8
%clofunc50070 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40324)
musttail call tailcc void %clofunc50070(%struct.ScmObj* %anf_45bind40324, %struct.ScmObj* %args48707$anf_45bind40324$2)
ret void
}

define tailcc void @proc_clo$ae42813(%struct.ScmObj* %env$ae42813,%struct.ScmObj* %current_45args48697) {
%stackaddr$env-ref50071 = alloca %struct.ScmObj*, align 8
%k40435 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42813, i64 0)
store %struct.ScmObj* %k40435, %struct.ScmObj** %stackaddr$env-ref50071
%stackaddr$env-ref50072 = alloca %struct.ScmObj*, align 8
%n40190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42813, i64 1)
store %struct.ScmObj* %n40190, %struct.ScmObj** %stackaddr$env-ref50072
%stackaddr$env-ref50073 = alloca %struct.ScmObj*, align 8
%lst40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42813, i64 2)
store %struct.ScmObj* %lst40189, %struct.ScmObj** %stackaddr$env-ref50073
%stackaddr$prim50074 = alloca %struct.ScmObj*, align 8
%_95k40437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48697)
store volatile %struct.ScmObj* %_95k40437, %struct.ScmObj** %stackaddr$prim50074, align 8
%stackaddr$prim50075 = alloca %struct.ScmObj*, align 8
%current_45args48698 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48697)
store volatile %struct.ScmObj* %current_45args48698, %struct.ScmObj** %stackaddr$prim50075, align 8
%stackaddr$prim50076 = alloca %struct.ScmObj*, align 8
%cc40191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48698)
store volatile %struct.ScmObj* %cc40191, %struct.ScmObj** %stackaddr$prim50076, align 8
%ae42956 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50077 = alloca %struct.ScmObj*, align 8
%anf_45bind40325 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40190, %struct.ScmObj* %ae42956)
store volatile %struct.ScmObj* %anf_45bind40325, %struct.ScmObj** %stackaddr$prim50077, align 8
%ae42957 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50078 = alloca %struct.ScmObj*, align 8
%anf_45bind40326 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42957, %struct.ScmObj* %anf_45bind40325)
store volatile %struct.ScmObj* %anf_45bind40326, %struct.ScmObj** %stackaddr$prim50078, align 8
%truthy$cmp50079 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40326)
%cmp$cmp50079 = icmp eq i64 %truthy$cmp50079, 1
br i1 %cmp$cmp50079, label %truebranch$cmp50079, label %falsebranch$cmp50079
truebranch$cmp50079:
%ae42961 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50080 = alloca %struct.ScmObj*, align 8
%cpsprim40438 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40189, %struct.ScmObj* %ae42961)
store volatile %struct.ScmObj* %cpsprim40438, %struct.ScmObj** %stackaddr$prim50080, align 8
%ae42963 = call %struct.ScmObj* @const_init_int(i64 0)
%args48700$k40435$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50081 = alloca %struct.ScmObj*, align 8
%args48700$k40435$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40438, %struct.ScmObj* %args48700$k40435$0)
store volatile %struct.ScmObj* %args48700$k40435$1, %struct.ScmObj** %stackaddr$prim50081, align 8
%stackaddr$prim50082 = alloca %struct.ScmObj*, align 8
%args48700$k40435$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42963, %struct.ScmObj* %args48700$k40435$1)
store volatile %struct.ScmObj* %args48700$k40435$2, %struct.ScmObj** %stackaddr$prim50082, align 8
%clofunc50083 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40435)
musttail call tailcc void %clofunc50083(%struct.ScmObj* %k40435, %struct.ScmObj* %args48700$k40435$2)
ret void
falsebranch$cmp50079:
%ae42974 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50084 = alloca %struct.ScmObj*, align 8
%anf_45bind40327 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40189, %struct.ScmObj* %ae42974)
store volatile %struct.ScmObj* %anf_45bind40327, %struct.ScmObj** %stackaddr$prim50084, align 8
%stackaddr$prim50085 = alloca %struct.ScmObj*, align 8
%anf_45bind40328 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40327)
store volatile %struct.ScmObj* %anf_45bind40328, %struct.ScmObj** %stackaddr$prim50085, align 8
%ae42977 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50086 = alloca %struct.ScmObj*, align 8
%_95040194 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40189, %struct.ScmObj* %ae42977, %struct.ScmObj* %anf_45bind40328)
store volatile %struct.ScmObj* %_95040194, %struct.ScmObj** %stackaddr$prim50086, align 8
%ae42980 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50087 = alloca %struct.ScmObj*, align 8
%anf_45bind40329 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40190, %struct.ScmObj* %ae42980)
store volatile %struct.ScmObj* %anf_45bind40329, %struct.ScmObj** %stackaddr$prim50087, align 8
%ae42982 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50088 = alloca %struct.ScmObj*, align 8
%anf_45bind40330 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40329, %struct.ScmObj* %ae42982)
store volatile %struct.ScmObj* %anf_45bind40330, %struct.ScmObj** %stackaddr$prim50088, align 8
%ae42984 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50089 = alloca %struct.ScmObj*, align 8
%_95140193 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40190, %struct.ScmObj* %ae42984, %struct.ScmObj* %anf_45bind40330)
store volatile %struct.ScmObj* %_95140193, %struct.ScmObj** %stackaddr$prim50089, align 8
%args48701$cc40191$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50090 = alloca %struct.ScmObj*, align 8
%args48701$cc40191$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40191, %struct.ScmObj* %args48701$cc40191$0)
store volatile %struct.ScmObj* %args48701$cc40191$1, %struct.ScmObj** %stackaddr$prim50090, align 8
%stackaddr$prim50091 = alloca %struct.ScmObj*, align 8
%args48701$cc40191$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40435, %struct.ScmObj* %args48701$cc40191$1)
store volatile %struct.ScmObj* %args48701$cc40191$2, %struct.ScmObj** %stackaddr$prim50091, align 8
%clofunc50092 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40191)
musttail call tailcc void %clofunc50092(%struct.ScmObj* %cc40191, %struct.ScmObj* %args48701$cc40191$2)
ret void
}

define tailcc void @proc_clo$ae42814(%struct.ScmObj* %env$ae42814,%struct.ScmObj* %current_45args48702) {
%stackaddr$env-ref50093 = alloca %struct.ScmObj*, align 8
%k40435 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42814, i64 0)
store %struct.ScmObj* %k40435, %struct.ScmObj** %stackaddr$env-ref50093
%stackaddr$env-ref50094 = alloca %struct.ScmObj*, align 8
%n40190 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42814, i64 1)
store %struct.ScmObj* %n40190, %struct.ScmObj** %stackaddr$env-ref50094
%stackaddr$env-ref50095 = alloca %struct.ScmObj*, align 8
%lst40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42814, i64 2)
store %struct.ScmObj* %lst40189, %struct.ScmObj** %stackaddr$env-ref50095
%stackaddr$prim50096 = alloca %struct.ScmObj*, align 8
%_95k40437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48702)
store volatile %struct.ScmObj* %_95k40437, %struct.ScmObj** %stackaddr$prim50096, align 8
%stackaddr$prim50097 = alloca %struct.ScmObj*, align 8
%current_45args48703 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48702)
store volatile %struct.ScmObj* %current_45args48703, %struct.ScmObj** %stackaddr$prim50097, align 8
%stackaddr$prim50098 = alloca %struct.ScmObj*, align 8
%cc40191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48703)
store volatile %struct.ScmObj* %cc40191, %struct.ScmObj** %stackaddr$prim50098, align 8
%ae42816 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50099 = alloca %struct.ScmObj*, align 8
%anf_45bind40325 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40190, %struct.ScmObj* %ae42816)
store volatile %struct.ScmObj* %anf_45bind40325, %struct.ScmObj** %stackaddr$prim50099, align 8
%ae42817 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50100 = alloca %struct.ScmObj*, align 8
%anf_45bind40326 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42817, %struct.ScmObj* %anf_45bind40325)
store volatile %struct.ScmObj* %anf_45bind40326, %struct.ScmObj** %stackaddr$prim50100, align 8
%truthy$cmp50101 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40326)
%cmp$cmp50101 = icmp eq i64 %truthy$cmp50101, 1
br i1 %cmp$cmp50101, label %truebranch$cmp50101, label %falsebranch$cmp50101
truebranch$cmp50101:
%ae42821 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50102 = alloca %struct.ScmObj*, align 8
%cpsprim40438 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40189, %struct.ScmObj* %ae42821)
store volatile %struct.ScmObj* %cpsprim40438, %struct.ScmObj** %stackaddr$prim50102, align 8
%ae42823 = call %struct.ScmObj* @const_init_int(i64 0)
%args48705$k40435$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50103 = alloca %struct.ScmObj*, align 8
%args48705$k40435$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40438, %struct.ScmObj* %args48705$k40435$0)
store volatile %struct.ScmObj* %args48705$k40435$1, %struct.ScmObj** %stackaddr$prim50103, align 8
%stackaddr$prim50104 = alloca %struct.ScmObj*, align 8
%args48705$k40435$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42823, %struct.ScmObj* %args48705$k40435$1)
store volatile %struct.ScmObj* %args48705$k40435$2, %struct.ScmObj** %stackaddr$prim50104, align 8
%clofunc50105 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40435)
musttail call tailcc void %clofunc50105(%struct.ScmObj* %k40435, %struct.ScmObj* %args48705$k40435$2)
ret void
falsebranch$cmp50101:
%ae42834 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50106 = alloca %struct.ScmObj*, align 8
%anf_45bind40327 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40189, %struct.ScmObj* %ae42834)
store volatile %struct.ScmObj* %anf_45bind40327, %struct.ScmObj** %stackaddr$prim50106, align 8
%stackaddr$prim50107 = alloca %struct.ScmObj*, align 8
%anf_45bind40328 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40327)
store volatile %struct.ScmObj* %anf_45bind40328, %struct.ScmObj** %stackaddr$prim50107, align 8
%ae42837 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50108 = alloca %struct.ScmObj*, align 8
%_95040194 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40189, %struct.ScmObj* %ae42837, %struct.ScmObj* %anf_45bind40328)
store volatile %struct.ScmObj* %_95040194, %struct.ScmObj** %stackaddr$prim50108, align 8
%ae42840 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50109 = alloca %struct.ScmObj*, align 8
%anf_45bind40329 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40190, %struct.ScmObj* %ae42840)
store volatile %struct.ScmObj* %anf_45bind40329, %struct.ScmObj** %stackaddr$prim50109, align 8
%ae42842 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50110 = alloca %struct.ScmObj*, align 8
%anf_45bind40330 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40329, %struct.ScmObj* %ae42842)
store volatile %struct.ScmObj* %anf_45bind40330, %struct.ScmObj** %stackaddr$prim50110, align 8
%ae42844 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50111 = alloca %struct.ScmObj*, align 8
%_95140193 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40190, %struct.ScmObj* %ae42844, %struct.ScmObj* %anf_45bind40330)
store volatile %struct.ScmObj* %_95140193, %struct.ScmObj** %stackaddr$prim50111, align 8
%args48706$cc40191$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50112 = alloca %struct.ScmObj*, align 8
%args48706$cc40191$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40191, %struct.ScmObj* %args48706$cc40191$0)
store volatile %struct.ScmObj* %args48706$cc40191$1, %struct.ScmObj** %stackaddr$prim50112, align 8
%stackaddr$prim50113 = alloca %struct.ScmObj*, align 8
%args48706$cc40191$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40435, %struct.ScmObj* %args48706$cc40191$1)
store volatile %struct.ScmObj* %args48706$cc40191$2, %struct.ScmObj** %stackaddr$prim50113, align 8
%clofunc50114 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40191)
musttail call tailcc void %clofunc50114(%struct.ScmObj* %cc40191, %struct.ScmObj* %args48706$cc40191$2)
ret void
}

define tailcc void @proc_clo$ae42799(%struct.ScmObj* %env$ae42799,%struct.ScmObj* %current_45args48708) {
%stackaddr$prim50115 = alloca %struct.ScmObj*, align 8
%k40439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48708)
store volatile %struct.ScmObj* %k40439, %struct.ScmObj** %stackaddr$prim50115, align 8
%stackaddr$prim50116 = alloca %struct.ScmObj*, align 8
%current_45args48709 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48708)
store volatile %struct.ScmObj* %current_45args48709, %struct.ScmObj** %stackaddr$prim50116, align 8
%stackaddr$prim50117 = alloca %struct.ScmObj*, align 8
%u40192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48709)
store volatile %struct.ScmObj* %u40192, %struct.ScmObj** %stackaddr$prim50117, align 8
%args48711$u40192$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50118 = alloca %struct.ScmObj*, align 8
%args48711$u40192$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40192, %struct.ScmObj* %args48711$u40192$0)
store volatile %struct.ScmObj* %args48711$u40192$1, %struct.ScmObj** %stackaddr$prim50118, align 8
%stackaddr$prim50119 = alloca %struct.ScmObj*, align 8
%args48711$u40192$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40439, %struct.ScmObj* %args48711$u40192$1)
store volatile %struct.ScmObj* %args48711$u40192$2, %struct.ScmObj** %stackaddr$prim50119, align 8
%clofunc50120 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40192)
musttail call tailcc void %clofunc50120(%struct.ScmObj* %u40192, %struct.ScmObj* %args48711$u40192$2)
ret void
}

define tailcc void @proc_clo$ae42376(%struct.ScmObj* %env$ae42376,%struct.ScmObj* %current_45args48714) {
%stackaddr$prim50121 = alloca %struct.ScmObj*, align 8
%k40440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48714)
store volatile %struct.ScmObj* %k40440, %struct.ScmObj** %stackaddr$prim50121, align 8
%stackaddr$prim50122 = alloca %struct.ScmObj*, align 8
%current_45args48715 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48714)
store volatile %struct.ScmObj* %current_45args48715, %struct.ScmObj** %stackaddr$prim50122, align 8
%stackaddr$prim50123 = alloca %struct.ScmObj*, align 8
%a40196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48715)
store volatile %struct.ScmObj* %a40196, %struct.ScmObj** %stackaddr$prim50123, align 8
%ae42377 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50124 = alloca %struct.ScmObj*, align 8
%a40197 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42377, %struct.ScmObj* %a40196)
store volatile %struct.ScmObj* %a40197, %struct.ScmObj** %stackaddr$prim50124, align 8
%stackaddr$makeclosure50125 = alloca %struct.ScmObj*, align 8
%fptrToInt50126 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42379 to i64
%ae42379 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50126)
store volatile %struct.ScmObj* %ae42379, %struct.ScmObj** %stackaddr$makeclosure50125, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42379, %struct.ScmObj* %k40440, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42379, %struct.ScmObj* %a40197, i64 1)
%ae42380 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50127 = alloca %struct.ScmObj*, align 8
%fptrToInt50128 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42381 to i64
%ae42381 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50128)
store volatile %struct.ScmObj* %ae42381, %struct.ScmObj** %stackaddr$makeclosure50127, align 8
%args48737$ae42379$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50129 = alloca %struct.ScmObj*, align 8
%args48737$ae42379$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42381, %struct.ScmObj* %args48737$ae42379$0)
store volatile %struct.ScmObj* %args48737$ae42379$1, %struct.ScmObj** %stackaddr$prim50129, align 8
%stackaddr$prim50130 = alloca %struct.ScmObj*, align 8
%args48737$ae42379$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42380, %struct.ScmObj* %args48737$ae42379$1)
store volatile %struct.ScmObj* %args48737$ae42379$2, %struct.ScmObj** %stackaddr$prim50130, align 8
%clofunc50131 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42379)
musttail call tailcc void %clofunc50131(%struct.ScmObj* %ae42379, %struct.ScmObj* %args48737$ae42379$2)
ret void
}

define tailcc void @proc_clo$ae42379(%struct.ScmObj* %env$ae42379,%struct.ScmObj* %current_45args48717) {
%stackaddr$env-ref50132 = alloca %struct.ScmObj*, align 8
%k40440 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42379, i64 0)
store %struct.ScmObj* %k40440, %struct.ScmObj** %stackaddr$env-ref50132
%stackaddr$env-ref50133 = alloca %struct.ScmObj*, align 8
%a40197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42379, i64 1)
store %struct.ScmObj* %a40197, %struct.ScmObj** %stackaddr$env-ref50133
%stackaddr$prim50134 = alloca %struct.ScmObj*, align 8
%_95k40441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48717)
store volatile %struct.ScmObj* %_95k40441, %struct.ScmObj** %stackaddr$prim50134, align 8
%stackaddr$prim50135 = alloca %struct.ScmObj*, align 8
%current_45args48718 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48717)
store volatile %struct.ScmObj* %current_45args48718, %struct.ScmObj** %stackaddr$prim50135, align 8
%stackaddr$prim50136 = alloca %struct.ScmObj*, align 8
%anf_45bind40316 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48718)
store volatile %struct.ScmObj* %anf_45bind40316, %struct.ScmObj** %stackaddr$prim50136, align 8
%stackaddr$makeclosure50137 = alloca %struct.ScmObj*, align 8
%fptrToInt50138 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42398 to i64
%ae42398 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50138)
store volatile %struct.ScmObj* %ae42398, %struct.ScmObj** %stackaddr$makeclosure50137, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42398, %struct.ScmObj* %k40440, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42398, %struct.ScmObj* %a40197, i64 1)
%stackaddr$makeclosure50139 = alloca %struct.ScmObj*, align 8
%fptrToInt50140 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42399 to i64
%ae42399 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50140)
store volatile %struct.ScmObj* %ae42399, %struct.ScmObj** %stackaddr$makeclosure50139, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42399, %struct.ScmObj* %k40440, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42399, %struct.ScmObj* %a40197, i64 1)
%args48732$anf_45bind40316$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50141 = alloca %struct.ScmObj*, align 8
%args48732$anf_45bind40316$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42399, %struct.ScmObj* %args48732$anf_45bind40316$0)
store volatile %struct.ScmObj* %args48732$anf_45bind40316$1, %struct.ScmObj** %stackaddr$prim50141, align 8
%stackaddr$prim50142 = alloca %struct.ScmObj*, align 8
%args48732$anf_45bind40316$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42398, %struct.ScmObj* %args48732$anf_45bind40316$1)
store volatile %struct.ScmObj* %args48732$anf_45bind40316$2, %struct.ScmObj** %stackaddr$prim50142, align 8
%clofunc50143 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40316)
musttail call tailcc void %clofunc50143(%struct.ScmObj* %anf_45bind40316, %struct.ScmObj* %args48732$anf_45bind40316$2)
ret void
}

define tailcc void @proc_clo$ae42398(%struct.ScmObj* %env$ae42398,%struct.ScmObj* %current_45args48720) {
%stackaddr$env-ref50144 = alloca %struct.ScmObj*, align 8
%k40440 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42398, i64 0)
store %struct.ScmObj* %k40440, %struct.ScmObj** %stackaddr$env-ref50144
%stackaddr$env-ref50145 = alloca %struct.ScmObj*, align 8
%a40197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42398, i64 1)
store %struct.ScmObj* %a40197, %struct.ScmObj** %stackaddr$env-ref50145
%stackaddr$prim50146 = alloca %struct.ScmObj*, align 8
%_95k40442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48720)
store volatile %struct.ScmObj* %_95k40442, %struct.ScmObj** %stackaddr$prim50146, align 8
%stackaddr$prim50147 = alloca %struct.ScmObj*, align 8
%current_45args48721 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48720)
store volatile %struct.ScmObj* %current_45args48721, %struct.ScmObj** %stackaddr$prim50147, align 8
%stackaddr$prim50148 = alloca %struct.ScmObj*, align 8
%cc40198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48721)
store volatile %struct.ScmObj* %cc40198, %struct.ScmObj** %stackaddr$prim50148, align 8
%ae42514 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50149 = alloca %struct.ScmObj*, align 8
%anf_45bind40317 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40197, %struct.ScmObj* %ae42514)
store volatile %struct.ScmObj* %anf_45bind40317, %struct.ScmObj** %stackaddr$prim50149, align 8
%stackaddr$prim50150 = alloca %struct.ScmObj*, align 8
%anf_45bind40318 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40317)
store volatile %struct.ScmObj* %anf_45bind40318, %struct.ScmObj** %stackaddr$prim50150, align 8
%truthy$cmp50151 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40318)
%cmp$cmp50151 = icmp eq i64 %truthy$cmp50151, 1
br i1 %cmp$cmp50151, label %truebranch$cmp50151, label %falsebranch$cmp50151
truebranch$cmp50151:
%ae42518 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42519 = call %struct.ScmObj* @const_init_true()
%args48723$k40440$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50152 = alloca %struct.ScmObj*, align 8
%args48723$k40440$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42519, %struct.ScmObj* %args48723$k40440$0)
store volatile %struct.ScmObj* %args48723$k40440$1, %struct.ScmObj** %stackaddr$prim50152, align 8
%stackaddr$prim50153 = alloca %struct.ScmObj*, align 8
%args48723$k40440$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42518, %struct.ScmObj* %args48723$k40440$1)
store volatile %struct.ScmObj* %args48723$k40440$2, %struct.ScmObj** %stackaddr$prim50153, align 8
%clofunc50154 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40440)
musttail call tailcc void %clofunc50154(%struct.ScmObj* %k40440, %struct.ScmObj* %args48723$k40440$2)
ret void
falsebranch$cmp50151:
%ae42527 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50155 = alloca %struct.ScmObj*, align 8
%anf_45bind40319 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40197, %struct.ScmObj* %ae42527)
store volatile %struct.ScmObj* %anf_45bind40319, %struct.ScmObj** %stackaddr$prim50155, align 8
%stackaddr$prim50156 = alloca %struct.ScmObj*, align 8
%anf_45bind40320 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40319)
store volatile %struct.ScmObj* %anf_45bind40320, %struct.ScmObj** %stackaddr$prim50156, align 8
%truthy$cmp50157 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40320)
%cmp$cmp50157 = icmp eq i64 %truthy$cmp50157, 1
br i1 %cmp$cmp50157, label %truebranch$cmp50157, label %falsebranch$cmp50157
truebranch$cmp50157:
%ae42531 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50158 = alloca %struct.ScmObj*, align 8
%anf_45bind40321 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40197, %struct.ScmObj* %ae42531)
store volatile %struct.ScmObj* %anf_45bind40321, %struct.ScmObj** %stackaddr$prim50158, align 8
%stackaddr$prim50159 = alloca %struct.ScmObj*, align 8
%b40200 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40321)
store volatile %struct.ScmObj* %b40200, %struct.ScmObj** %stackaddr$prim50159, align 8
%ae42534 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50160 = alloca %struct.ScmObj*, align 8
%anf_45bind40322 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40197, %struct.ScmObj* %ae42534)
store volatile %struct.ScmObj* %anf_45bind40322, %struct.ScmObj** %stackaddr$prim50160, align 8
%stackaddr$prim50161 = alloca %struct.ScmObj*, align 8
%anf_45bind40323 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40322)
store volatile %struct.ScmObj* %anf_45bind40323, %struct.ScmObj** %stackaddr$prim50161, align 8
%ae42537 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50162 = alloca %struct.ScmObj*, align 8
%_95040201 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40197, %struct.ScmObj* %ae42537, %struct.ScmObj* %anf_45bind40323)
store volatile %struct.ScmObj* %_95040201, %struct.ScmObj** %stackaddr$prim50162, align 8
%args48724$cc40198$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50163 = alloca %struct.ScmObj*, align 8
%args48724$cc40198$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40198, %struct.ScmObj* %args48724$cc40198$0)
store volatile %struct.ScmObj* %args48724$cc40198$1, %struct.ScmObj** %stackaddr$prim50163, align 8
%stackaddr$prim50164 = alloca %struct.ScmObj*, align 8
%args48724$cc40198$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40440, %struct.ScmObj* %args48724$cc40198$1)
store volatile %struct.ScmObj* %args48724$cc40198$2, %struct.ScmObj** %stackaddr$prim50164, align 8
%clofunc50165 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40198)
musttail call tailcc void %clofunc50165(%struct.ScmObj* %cc40198, %struct.ScmObj* %args48724$cc40198$2)
ret void
falsebranch$cmp50157:
%ae42570 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42571 = call %struct.ScmObj* @const_init_false()
%args48725$k40440$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50166 = alloca %struct.ScmObj*, align 8
%args48725$k40440$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42571, %struct.ScmObj* %args48725$k40440$0)
store volatile %struct.ScmObj* %args48725$k40440$1, %struct.ScmObj** %stackaddr$prim50166, align 8
%stackaddr$prim50167 = alloca %struct.ScmObj*, align 8
%args48725$k40440$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42570, %struct.ScmObj* %args48725$k40440$1)
store volatile %struct.ScmObj* %args48725$k40440$2, %struct.ScmObj** %stackaddr$prim50167, align 8
%clofunc50168 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40440)
musttail call tailcc void %clofunc50168(%struct.ScmObj* %k40440, %struct.ScmObj* %args48725$k40440$2)
ret void
}

define tailcc void @proc_clo$ae42399(%struct.ScmObj* %env$ae42399,%struct.ScmObj* %current_45args48726) {
%stackaddr$env-ref50169 = alloca %struct.ScmObj*, align 8
%k40440 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42399, i64 0)
store %struct.ScmObj* %k40440, %struct.ScmObj** %stackaddr$env-ref50169
%stackaddr$env-ref50170 = alloca %struct.ScmObj*, align 8
%a40197 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42399, i64 1)
store %struct.ScmObj* %a40197, %struct.ScmObj** %stackaddr$env-ref50170
%stackaddr$prim50171 = alloca %struct.ScmObj*, align 8
%_95k40442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48726)
store volatile %struct.ScmObj* %_95k40442, %struct.ScmObj** %stackaddr$prim50171, align 8
%stackaddr$prim50172 = alloca %struct.ScmObj*, align 8
%current_45args48727 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48726)
store volatile %struct.ScmObj* %current_45args48727, %struct.ScmObj** %stackaddr$prim50172, align 8
%stackaddr$prim50173 = alloca %struct.ScmObj*, align 8
%cc40198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48727)
store volatile %struct.ScmObj* %cc40198, %struct.ScmObj** %stackaddr$prim50173, align 8
%ae42401 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50174 = alloca %struct.ScmObj*, align 8
%anf_45bind40317 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40197, %struct.ScmObj* %ae42401)
store volatile %struct.ScmObj* %anf_45bind40317, %struct.ScmObj** %stackaddr$prim50174, align 8
%stackaddr$prim50175 = alloca %struct.ScmObj*, align 8
%anf_45bind40318 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40317)
store volatile %struct.ScmObj* %anf_45bind40318, %struct.ScmObj** %stackaddr$prim50175, align 8
%truthy$cmp50176 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40318)
%cmp$cmp50176 = icmp eq i64 %truthy$cmp50176, 1
br i1 %cmp$cmp50176, label %truebranch$cmp50176, label %falsebranch$cmp50176
truebranch$cmp50176:
%ae42405 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42406 = call %struct.ScmObj* @const_init_true()
%args48729$k40440$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50177 = alloca %struct.ScmObj*, align 8
%args48729$k40440$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42406, %struct.ScmObj* %args48729$k40440$0)
store volatile %struct.ScmObj* %args48729$k40440$1, %struct.ScmObj** %stackaddr$prim50177, align 8
%stackaddr$prim50178 = alloca %struct.ScmObj*, align 8
%args48729$k40440$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42405, %struct.ScmObj* %args48729$k40440$1)
store volatile %struct.ScmObj* %args48729$k40440$2, %struct.ScmObj** %stackaddr$prim50178, align 8
%clofunc50179 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40440)
musttail call tailcc void %clofunc50179(%struct.ScmObj* %k40440, %struct.ScmObj* %args48729$k40440$2)
ret void
falsebranch$cmp50176:
%ae42414 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50180 = alloca %struct.ScmObj*, align 8
%anf_45bind40319 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40197, %struct.ScmObj* %ae42414)
store volatile %struct.ScmObj* %anf_45bind40319, %struct.ScmObj** %stackaddr$prim50180, align 8
%stackaddr$prim50181 = alloca %struct.ScmObj*, align 8
%anf_45bind40320 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40319)
store volatile %struct.ScmObj* %anf_45bind40320, %struct.ScmObj** %stackaddr$prim50181, align 8
%truthy$cmp50182 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40320)
%cmp$cmp50182 = icmp eq i64 %truthy$cmp50182, 1
br i1 %cmp$cmp50182, label %truebranch$cmp50182, label %falsebranch$cmp50182
truebranch$cmp50182:
%ae42418 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50183 = alloca %struct.ScmObj*, align 8
%anf_45bind40321 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40197, %struct.ScmObj* %ae42418)
store volatile %struct.ScmObj* %anf_45bind40321, %struct.ScmObj** %stackaddr$prim50183, align 8
%stackaddr$prim50184 = alloca %struct.ScmObj*, align 8
%b40200 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40321)
store volatile %struct.ScmObj* %b40200, %struct.ScmObj** %stackaddr$prim50184, align 8
%ae42421 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50185 = alloca %struct.ScmObj*, align 8
%anf_45bind40322 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40197, %struct.ScmObj* %ae42421)
store volatile %struct.ScmObj* %anf_45bind40322, %struct.ScmObj** %stackaddr$prim50185, align 8
%stackaddr$prim50186 = alloca %struct.ScmObj*, align 8
%anf_45bind40323 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40322)
store volatile %struct.ScmObj* %anf_45bind40323, %struct.ScmObj** %stackaddr$prim50186, align 8
%ae42424 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50187 = alloca %struct.ScmObj*, align 8
%_95040201 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40197, %struct.ScmObj* %ae42424, %struct.ScmObj* %anf_45bind40323)
store volatile %struct.ScmObj* %_95040201, %struct.ScmObj** %stackaddr$prim50187, align 8
%args48730$cc40198$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50188 = alloca %struct.ScmObj*, align 8
%args48730$cc40198$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40198, %struct.ScmObj* %args48730$cc40198$0)
store volatile %struct.ScmObj* %args48730$cc40198$1, %struct.ScmObj** %stackaddr$prim50188, align 8
%stackaddr$prim50189 = alloca %struct.ScmObj*, align 8
%args48730$cc40198$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40440, %struct.ScmObj* %args48730$cc40198$1)
store volatile %struct.ScmObj* %args48730$cc40198$2, %struct.ScmObj** %stackaddr$prim50189, align 8
%clofunc50190 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40198)
musttail call tailcc void %clofunc50190(%struct.ScmObj* %cc40198, %struct.ScmObj* %args48730$cc40198$2)
ret void
falsebranch$cmp50182:
%ae42457 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42458 = call %struct.ScmObj* @const_init_false()
%args48731$k40440$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50191 = alloca %struct.ScmObj*, align 8
%args48731$k40440$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42458, %struct.ScmObj* %args48731$k40440$0)
store volatile %struct.ScmObj* %args48731$k40440$1, %struct.ScmObj** %stackaddr$prim50191, align 8
%stackaddr$prim50192 = alloca %struct.ScmObj*, align 8
%args48731$k40440$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42457, %struct.ScmObj* %args48731$k40440$1)
store volatile %struct.ScmObj* %args48731$k40440$2, %struct.ScmObj** %stackaddr$prim50192, align 8
%clofunc50193 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40440)
musttail call tailcc void %clofunc50193(%struct.ScmObj* %k40440, %struct.ScmObj* %args48731$k40440$2)
ret void
}

define tailcc void @proc_clo$ae42381(%struct.ScmObj* %env$ae42381,%struct.ScmObj* %current_45args48733) {
%stackaddr$prim50194 = alloca %struct.ScmObj*, align 8
%k40443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48733)
store volatile %struct.ScmObj* %k40443, %struct.ScmObj** %stackaddr$prim50194, align 8
%stackaddr$prim50195 = alloca %struct.ScmObj*, align 8
%current_45args48734 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48733)
store volatile %struct.ScmObj* %current_45args48734, %struct.ScmObj** %stackaddr$prim50195, align 8
%stackaddr$prim50196 = alloca %struct.ScmObj*, align 8
%k40199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48734)
store volatile %struct.ScmObj* %k40199, %struct.ScmObj** %stackaddr$prim50196, align 8
%ae42383 = call %struct.ScmObj* @const_init_int(i64 0)
%args48736$k40443$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50197 = alloca %struct.ScmObj*, align 8
%args48736$k40443$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40199, %struct.ScmObj* %args48736$k40443$0)
store volatile %struct.ScmObj* %args48736$k40443$1, %struct.ScmObj** %stackaddr$prim50197, align 8
%stackaddr$prim50198 = alloca %struct.ScmObj*, align 8
%args48736$k40443$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42383, %struct.ScmObj* %args48736$k40443$1)
store volatile %struct.ScmObj* %args48736$k40443$2, %struct.ScmObj** %stackaddr$prim50198, align 8
%clofunc50199 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40443)
musttail call tailcc void %clofunc50199(%struct.ScmObj* %k40443, %struct.ScmObj* %args48736$k40443$2)
ret void
}

define tailcc void @proc_clo$ae42304(%struct.ScmObj* %env$ae42304,%struct.ScmObj* %current_45args48739) {
%stackaddr$env-ref50200 = alloca %struct.ScmObj*, align 8
%_37append40203 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42304, i64 0)
store %struct.ScmObj* %_37append40203, %struct.ScmObj** %stackaddr$env-ref50200
%stackaddr$prim50201 = alloca %struct.ScmObj*, align 8
%k40444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48739)
store volatile %struct.ScmObj* %k40444, %struct.ScmObj** %stackaddr$prim50201, align 8
%stackaddr$prim50202 = alloca %struct.ScmObj*, align 8
%current_45args48740 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48739)
store volatile %struct.ScmObj* %current_45args48740, %struct.ScmObj** %stackaddr$prim50202, align 8
%stackaddr$prim50203 = alloca %struct.ScmObj*, align 8
%ls040206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48740)
store volatile %struct.ScmObj* %ls040206, %struct.ScmObj** %stackaddr$prim50203, align 8
%stackaddr$prim50204 = alloca %struct.ScmObj*, align 8
%current_45args48741 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48740)
store volatile %struct.ScmObj* %current_45args48741, %struct.ScmObj** %stackaddr$prim50204, align 8
%stackaddr$prim50205 = alloca %struct.ScmObj*, align 8
%ls140205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48741)
store volatile %struct.ScmObj* %ls140205, %struct.ScmObj** %stackaddr$prim50205, align 8
%stackaddr$prim50206 = alloca %struct.ScmObj*, align 8
%anf_45bind40310 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls040206)
store volatile %struct.ScmObj* %anf_45bind40310, %struct.ScmObj** %stackaddr$prim50206, align 8
%truthy$cmp50207 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40310)
%cmp$cmp50207 = icmp eq i64 %truthy$cmp50207, 1
br i1 %cmp$cmp50207, label %truebranch$cmp50207, label %falsebranch$cmp50207
truebranch$cmp50207:
%ae42308 = call %struct.ScmObj* @const_init_int(i64 0)
%args48743$k40444$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50208 = alloca %struct.ScmObj*, align 8
%args48743$k40444$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140205, %struct.ScmObj* %args48743$k40444$0)
store volatile %struct.ScmObj* %args48743$k40444$1, %struct.ScmObj** %stackaddr$prim50208, align 8
%stackaddr$prim50209 = alloca %struct.ScmObj*, align 8
%args48743$k40444$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42308, %struct.ScmObj* %args48743$k40444$1)
store volatile %struct.ScmObj* %args48743$k40444$2, %struct.ScmObj** %stackaddr$prim50209, align 8
%clofunc50210 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40444)
musttail call tailcc void %clofunc50210(%struct.ScmObj* %k40444, %struct.ScmObj* %args48743$k40444$2)
ret void
falsebranch$cmp50207:
%stackaddr$prim50211 = alloca %struct.ScmObj*, align 8
%anf_45bind40311 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls040206)
store volatile %struct.ScmObj* %anf_45bind40311, %struct.ScmObj** %stackaddr$prim50211, align 8
%ae42315 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50212 = alloca %struct.ScmObj*, align 8
%anf_45bind40312 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40203, %struct.ScmObj* %ae42315)
store volatile %struct.ScmObj* %anf_45bind40312, %struct.ScmObj** %stackaddr$prim50212, align 8
%stackaddr$prim50213 = alloca %struct.ScmObj*, align 8
%anf_45bind40313 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls040206)
store volatile %struct.ScmObj* %anf_45bind40313, %struct.ScmObj** %stackaddr$prim50213, align 8
%stackaddr$makeclosure50214 = alloca %struct.ScmObj*, align 8
%fptrToInt50215 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42318 to i64
%ae42318 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50215)
store volatile %struct.ScmObj* %ae42318, %struct.ScmObj** %stackaddr$makeclosure50214, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42318, %struct.ScmObj* %anf_45bind40311, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42318, %struct.ScmObj* %k40444, i64 1)
%args48748$anf_45bind40312$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50216 = alloca %struct.ScmObj*, align 8
%args48748$anf_45bind40312$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140205, %struct.ScmObj* %args48748$anf_45bind40312$0)
store volatile %struct.ScmObj* %args48748$anf_45bind40312$1, %struct.ScmObj** %stackaddr$prim50216, align 8
%stackaddr$prim50217 = alloca %struct.ScmObj*, align 8
%args48748$anf_45bind40312$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40313, %struct.ScmObj* %args48748$anf_45bind40312$1)
store volatile %struct.ScmObj* %args48748$anf_45bind40312$2, %struct.ScmObj** %stackaddr$prim50217, align 8
%stackaddr$prim50218 = alloca %struct.ScmObj*, align 8
%args48748$anf_45bind40312$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42318, %struct.ScmObj* %args48748$anf_45bind40312$2)
store volatile %struct.ScmObj* %args48748$anf_45bind40312$3, %struct.ScmObj** %stackaddr$prim50218, align 8
%clofunc50219 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40312)
musttail call tailcc void %clofunc50219(%struct.ScmObj* %anf_45bind40312, %struct.ScmObj* %args48748$anf_45bind40312$3)
ret void
}

define tailcc void @proc_clo$ae42318(%struct.ScmObj* %env$ae42318,%struct.ScmObj* %current_45args48744) {
%stackaddr$env-ref50220 = alloca %struct.ScmObj*, align 8
%anf_45bind40311 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42318, i64 0)
store %struct.ScmObj* %anf_45bind40311, %struct.ScmObj** %stackaddr$env-ref50220
%stackaddr$env-ref50221 = alloca %struct.ScmObj*, align 8
%k40444 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42318, i64 1)
store %struct.ScmObj* %k40444, %struct.ScmObj** %stackaddr$env-ref50221
%stackaddr$prim50222 = alloca %struct.ScmObj*, align 8
%_95k40445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48744)
store volatile %struct.ScmObj* %_95k40445, %struct.ScmObj** %stackaddr$prim50222, align 8
%stackaddr$prim50223 = alloca %struct.ScmObj*, align 8
%current_45args48745 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48744)
store volatile %struct.ScmObj* %current_45args48745, %struct.ScmObj** %stackaddr$prim50223, align 8
%stackaddr$prim50224 = alloca %struct.ScmObj*, align 8
%anf_45bind40314 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48745)
store volatile %struct.ScmObj* %anf_45bind40314, %struct.ScmObj** %stackaddr$prim50224, align 8
%stackaddr$prim50225 = alloca %struct.ScmObj*, align 8
%cpsprim40446 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40311, %struct.ScmObj* %anf_45bind40314)
store volatile %struct.ScmObj* %cpsprim40446, %struct.ScmObj** %stackaddr$prim50225, align 8
%ae42324 = call %struct.ScmObj* @const_init_int(i64 0)
%args48747$k40444$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50226 = alloca %struct.ScmObj*, align 8
%args48747$k40444$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40446, %struct.ScmObj* %args48747$k40444$0)
store volatile %struct.ScmObj* %args48747$k40444$1, %struct.ScmObj** %stackaddr$prim50226, align 8
%stackaddr$prim50227 = alloca %struct.ScmObj*, align 8
%args48747$k40444$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42324, %struct.ScmObj* %args48747$k40444$1)
store volatile %struct.ScmObj* %args48747$k40444$2, %struct.ScmObj** %stackaddr$prim50227, align 8
%clofunc50228 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40444)
musttail call tailcc void %clofunc50228(%struct.ScmObj* %k40444, %struct.ScmObj* %args48747$k40444$2)
ret void
}

define tailcc void @proc_clo$ae42278(%struct.ScmObj* %env$ae42278,%struct.ScmObj* %current_45args48750) {
%stackaddr$prim50229 = alloca %struct.ScmObj*, align 8
%k40447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48750)
store volatile %struct.ScmObj* %k40447, %struct.ScmObj** %stackaddr$prim50229, align 8
%stackaddr$prim50230 = alloca %struct.ScmObj*, align 8
%current_45args48751 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48750)
store volatile %struct.ScmObj* %current_45args48751, %struct.ScmObj** %stackaddr$prim50230, align 8
%stackaddr$prim50231 = alloca %struct.ScmObj*, align 8
%a40209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48751)
store volatile %struct.ScmObj* %a40209, %struct.ScmObj** %stackaddr$prim50231, align 8
%stackaddr$prim50232 = alloca %struct.ScmObj*, align 8
%current_45args48752 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48751)
store volatile %struct.ScmObj* %current_45args48752, %struct.ScmObj** %stackaddr$prim50232, align 8
%stackaddr$prim50233 = alloca %struct.ScmObj*, align 8
%b40208 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48752)
store volatile %struct.ScmObj* %b40208, %struct.ScmObj** %stackaddr$prim50233, align 8
%stackaddr$prim50234 = alloca %struct.ScmObj*, align 8
%anf_45bind40309 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a40209, %struct.ScmObj* %b40208)
store volatile %struct.ScmObj* %anf_45bind40309, %struct.ScmObj** %stackaddr$prim50234, align 8
%stackaddr$prim50235 = alloca %struct.ScmObj*, align 8
%cpsprim40448 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40309)
store volatile %struct.ScmObj* %cpsprim40448, %struct.ScmObj** %stackaddr$prim50235, align 8
%ae42283 = call %struct.ScmObj* @const_init_int(i64 0)
%args48754$k40447$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50236 = alloca %struct.ScmObj*, align 8
%args48754$k40447$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40448, %struct.ScmObj* %args48754$k40447$0)
store volatile %struct.ScmObj* %args48754$k40447$1, %struct.ScmObj** %stackaddr$prim50236, align 8
%stackaddr$prim50237 = alloca %struct.ScmObj*, align 8
%args48754$k40447$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42283, %struct.ScmObj* %args48754$k40447$1)
store volatile %struct.ScmObj* %args48754$k40447$2, %struct.ScmObj** %stackaddr$prim50237, align 8
%clofunc50238 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40447)
musttail call tailcc void %clofunc50238(%struct.ScmObj* %k40447, %struct.ScmObj* %args48754$k40447$2)
ret void
}

define tailcc void @proc_clo$ae42254(%struct.ScmObj* %env$ae42254,%struct.ScmObj* %current_45args48756) {
%stackaddr$prim50239 = alloca %struct.ScmObj*, align 8
%k40449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48756)
store volatile %struct.ScmObj* %k40449, %struct.ScmObj** %stackaddr$prim50239, align 8
%stackaddr$prim50240 = alloca %struct.ScmObj*, align 8
%current_45args48757 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48756)
store volatile %struct.ScmObj* %current_45args48757, %struct.ScmObj** %stackaddr$prim50240, align 8
%stackaddr$prim50241 = alloca %struct.ScmObj*, align 8
%a40212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48757)
store volatile %struct.ScmObj* %a40212, %struct.ScmObj** %stackaddr$prim50241, align 8
%stackaddr$prim50242 = alloca %struct.ScmObj*, align 8
%current_45args48758 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48757)
store volatile %struct.ScmObj* %current_45args48758, %struct.ScmObj** %stackaddr$prim50242, align 8
%stackaddr$prim50243 = alloca %struct.ScmObj*, align 8
%b40211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48758)
store volatile %struct.ScmObj* %b40211, %struct.ScmObj** %stackaddr$prim50243, align 8
%stackaddr$prim50244 = alloca %struct.ScmObj*, align 8
%anf_45bind40308 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a40212, %struct.ScmObj* %b40211)
store volatile %struct.ScmObj* %anf_45bind40308, %struct.ScmObj** %stackaddr$prim50244, align 8
%stackaddr$prim50245 = alloca %struct.ScmObj*, align 8
%cpsprim40450 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40308)
store volatile %struct.ScmObj* %cpsprim40450, %struct.ScmObj** %stackaddr$prim50245, align 8
%ae42259 = call %struct.ScmObj* @const_init_int(i64 0)
%args48760$k40449$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50246 = alloca %struct.ScmObj*, align 8
%args48760$k40449$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40450, %struct.ScmObj* %args48760$k40449$0)
store volatile %struct.ScmObj* %args48760$k40449$1, %struct.ScmObj** %stackaddr$prim50246, align 8
%stackaddr$prim50247 = alloca %struct.ScmObj*, align 8
%args48760$k40449$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42259, %struct.ScmObj* %args48760$k40449$1)
store volatile %struct.ScmObj* %args48760$k40449$2, %struct.ScmObj** %stackaddr$prim50247, align 8
%clofunc50248 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40449)
musttail call tailcc void %clofunc50248(%struct.ScmObj* %k40449, %struct.ScmObj* %args48760$k40449$2)
ret void
}

define tailcc void @proc_clo$ae41860(%struct.ScmObj* %env$ae41860,%struct.ScmObj* %current_45args48763) {
%stackaddr$env-ref50249 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41860, i64 0)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref50249
%stackaddr$env-ref50250 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41860, i64 1)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref50250
%stackaddr$env-ref50251 = alloca %struct.ScmObj*, align 8
%_37map140162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41860, i64 2)
store %struct.ScmObj* %_37map140162, %struct.ScmObj** %stackaddr$env-ref50251
%stackaddr$prim50252 = alloca %struct.ScmObj*, align 8
%k40451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48763)
store volatile %struct.ScmObj* %k40451, %struct.ScmObj** %stackaddr$prim50252, align 8
%stackaddr$prim50253 = alloca %struct.ScmObj*, align 8
%current_45args48764 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48763)
store volatile %struct.ScmObj* %current_45args48764, %struct.ScmObj** %stackaddr$prim50253, align 8
%stackaddr$prim50254 = alloca %struct.ScmObj*, align 8
%_37foldl40214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48764)
store volatile %struct.ScmObj* %_37foldl40214, %struct.ScmObj** %stackaddr$prim50254, align 8
%ae41862 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50255 = alloca %struct.ScmObj*, align 8
%fptrToInt50256 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41863 to i64
%ae41863 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50256)
store volatile %struct.ScmObj* %ae41863, %struct.ScmObj** %stackaddr$makeclosure50255, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41863, %struct.ScmObj* %_37foldr40136, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41863, %struct.ScmObj* %_37foldl40214, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41863, %struct.ScmObj* %_37foldr140131, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41863, %struct.ScmObj* %_37map140162, i64 3)
%args48821$k40451$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50257 = alloca %struct.ScmObj*, align 8
%args48821$k40451$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41863, %struct.ScmObj* %args48821$k40451$0)
store volatile %struct.ScmObj* %args48821$k40451$1, %struct.ScmObj** %stackaddr$prim50257, align 8
%stackaddr$prim50258 = alloca %struct.ScmObj*, align 8
%args48821$k40451$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41862, %struct.ScmObj* %args48821$k40451$1)
store volatile %struct.ScmObj* %args48821$k40451$2, %struct.ScmObj** %stackaddr$prim50258, align 8
%clofunc50259 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40451)
musttail call tailcc void %clofunc50259(%struct.ScmObj* %k40451, %struct.ScmObj* %args48821$k40451$2)
ret void
}

define tailcc void @proc_clo$ae41863(%struct.ScmObj* %env$ae41863,%struct.ScmObj* %args4021540452) {
%stackaddr$env-ref50260 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41863, i64 0)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref50260
%stackaddr$env-ref50261 = alloca %struct.ScmObj*, align 8
%_37foldl40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41863, i64 1)
store %struct.ScmObj* %_37foldl40214, %struct.ScmObj** %stackaddr$env-ref50261
%stackaddr$env-ref50262 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41863, i64 2)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref50262
%stackaddr$env-ref50263 = alloca %struct.ScmObj*, align 8
%_37map140162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41863, i64 3)
store %struct.ScmObj* %_37map140162, %struct.ScmObj** %stackaddr$env-ref50263
%stackaddr$prim50264 = alloca %struct.ScmObj*, align 8
%k40453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4021540452)
store volatile %struct.ScmObj* %k40453, %struct.ScmObj** %stackaddr$prim50264, align 8
%stackaddr$prim50265 = alloca %struct.ScmObj*, align 8
%args40215 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4021540452)
store volatile %struct.ScmObj* %args40215, %struct.ScmObj** %stackaddr$prim50265, align 8
%stackaddr$prim50266 = alloca %struct.ScmObj*, align 8
%f40218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40215)
store volatile %struct.ScmObj* %f40218, %struct.ScmObj** %stackaddr$prim50266, align 8
%stackaddr$prim50267 = alloca %struct.ScmObj*, align 8
%anf_45bind40296 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40215)
store volatile %struct.ScmObj* %anf_45bind40296, %struct.ScmObj** %stackaddr$prim50267, align 8
%stackaddr$prim50268 = alloca %struct.ScmObj*, align 8
%acc40217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40296)
store volatile %struct.ScmObj* %acc40217, %struct.ScmObj** %stackaddr$prim50268, align 8
%stackaddr$prim50269 = alloca %struct.ScmObj*, align 8
%anf_45bind40297 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40215)
store volatile %struct.ScmObj* %anf_45bind40297, %struct.ScmObj** %stackaddr$prim50269, align 8
%stackaddr$prim50270 = alloca %struct.ScmObj*, align 8
%lsts40216 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40297)
store volatile %struct.ScmObj* %lsts40216, %struct.ScmObj** %stackaddr$prim50270, align 8
%stackaddr$makeclosure50271 = alloca %struct.ScmObj*, align 8
%fptrToInt50272 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41871 to i64
%ae41871 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt50272)
store volatile %struct.ScmObj* %ae41871, %struct.ScmObj** %stackaddr$makeclosure50271, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41871, %struct.ScmObj* %lsts40216, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41871, %struct.ScmObj* %_37foldr40136, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41871, %struct.ScmObj* %acc40217, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41871, %struct.ScmObj* %_37foldl40214, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41871, %struct.ScmObj* %k40453, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41871, %struct.ScmObj* %_37foldr140131, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41871, %struct.ScmObj* %_37map140162, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41871, %struct.ScmObj* %f40218, i64 7)
%ae41872 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50273 = alloca %struct.ScmObj*, align 8
%fptrToInt50274 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41873 to i64
%ae41873 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50274)
store volatile %struct.ScmObj* %ae41873, %struct.ScmObj** %stackaddr$makeclosure50273, align 8
%args48820$ae41871$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50275 = alloca %struct.ScmObj*, align 8
%args48820$ae41871$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41873, %struct.ScmObj* %args48820$ae41871$0)
store volatile %struct.ScmObj* %args48820$ae41871$1, %struct.ScmObj** %stackaddr$prim50275, align 8
%stackaddr$prim50276 = alloca %struct.ScmObj*, align 8
%args48820$ae41871$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41872, %struct.ScmObj* %args48820$ae41871$1)
store volatile %struct.ScmObj* %args48820$ae41871$2, %struct.ScmObj** %stackaddr$prim50276, align 8
%clofunc50277 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41871)
musttail call tailcc void %clofunc50277(%struct.ScmObj* %ae41871, %struct.ScmObj* %args48820$ae41871$2)
ret void
}

define tailcc void @proc_clo$ae41871(%struct.ScmObj* %env$ae41871,%struct.ScmObj* %current_45args48766) {
%stackaddr$env-ref50278 = alloca %struct.ScmObj*, align 8
%lsts40216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41871, i64 0)
store %struct.ScmObj* %lsts40216, %struct.ScmObj** %stackaddr$env-ref50278
%stackaddr$env-ref50279 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41871, i64 1)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref50279
%stackaddr$env-ref50280 = alloca %struct.ScmObj*, align 8
%acc40217 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41871, i64 2)
store %struct.ScmObj* %acc40217, %struct.ScmObj** %stackaddr$env-ref50280
%stackaddr$env-ref50281 = alloca %struct.ScmObj*, align 8
%_37foldl40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41871, i64 3)
store %struct.ScmObj* %_37foldl40214, %struct.ScmObj** %stackaddr$env-ref50281
%stackaddr$env-ref50282 = alloca %struct.ScmObj*, align 8
%k40453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41871, i64 4)
store %struct.ScmObj* %k40453, %struct.ScmObj** %stackaddr$env-ref50282
%stackaddr$env-ref50283 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41871, i64 5)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref50283
%stackaddr$env-ref50284 = alloca %struct.ScmObj*, align 8
%_37map140162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41871, i64 6)
store %struct.ScmObj* %_37map140162, %struct.ScmObj** %stackaddr$env-ref50284
%stackaddr$env-ref50285 = alloca %struct.ScmObj*, align 8
%f40218 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41871, i64 7)
store %struct.ScmObj* %f40218, %struct.ScmObj** %stackaddr$env-ref50285
%stackaddr$prim50286 = alloca %struct.ScmObj*, align 8
%_95k40454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48766)
store volatile %struct.ScmObj* %_95k40454, %struct.ScmObj** %stackaddr$prim50286, align 8
%stackaddr$prim50287 = alloca %struct.ScmObj*, align 8
%current_45args48767 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48766)
store volatile %struct.ScmObj* %current_45args48767, %struct.ScmObj** %stackaddr$prim50287, align 8
%stackaddr$prim50288 = alloca %struct.ScmObj*, align 8
%anf_45bind40298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48767)
store volatile %struct.ScmObj* %anf_45bind40298, %struct.ScmObj** %stackaddr$prim50288, align 8
%stackaddr$makeclosure50289 = alloca %struct.ScmObj*, align 8
%fptrToInt50290 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41903 to i64
%ae41903 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50290)
store volatile %struct.ScmObj* %ae41903, %struct.ScmObj** %stackaddr$makeclosure50289, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41903, %struct.ScmObj* %lsts40216, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41903, %struct.ScmObj* %_37foldr40136, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41903, %struct.ScmObj* %acc40217, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41903, %struct.ScmObj* %_37foldl40214, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41903, %struct.ScmObj* %k40453, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41903, %struct.ScmObj* %_37map140162, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41903, %struct.ScmObj* %f40218, i64 6)
%ae41905 = call %struct.ScmObj* @const_init_false()
%args48813$_37foldr140131$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50291 = alloca %struct.ScmObj*, align 8
%args48813$_37foldr140131$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40216, %struct.ScmObj* %args48813$_37foldr140131$0)
store volatile %struct.ScmObj* %args48813$_37foldr140131$1, %struct.ScmObj** %stackaddr$prim50291, align 8
%stackaddr$prim50292 = alloca %struct.ScmObj*, align 8
%args48813$_37foldr140131$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41905, %struct.ScmObj* %args48813$_37foldr140131$1)
store volatile %struct.ScmObj* %args48813$_37foldr140131$2, %struct.ScmObj** %stackaddr$prim50292, align 8
%stackaddr$prim50293 = alloca %struct.ScmObj*, align 8
%args48813$_37foldr140131$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40298, %struct.ScmObj* %args48813$_37foldr140131$2)
store volatile %struct.ScmObj* %args48813$_37foldr140131$3, %struct.ScmObj** %stackaddr$prim50293, align 8
%stackaddr$prim50294 = alloca %struct.ScmObj*, align 8
%args48813$_37foldr140131$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41903, %struct.ScmObj* %args48813$_37foldr140131$3)
store volatile %struct.ScmObj* %args48813$_37foldr140131$4, %struct.ScmObj** %stackaddr$prim50294, align 8
%clofunc50295 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140131)
musttail call tailcc void %clofunc50295(%struct.ScmObj* %_37foldr140131, %struct.ScmObj* %args48813$_37foldr140131$4)
ret void
}

define tailcc void @proc_clo$ae41903(%struct.ScmObj* %env$ae41903,%struct.ScmObj* %current_45args48769) {
%stackaddr$env-ref50296 = alloca %struct.ScmObj*, align 8
%lsts40216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41903, i64 0)
store %struct.ScmObj* %lsts40216, %struct.ScmObj** %stackaddr$env-ref50296
%stackaddr$env-ref50297 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41903, i64 1)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref50297
%stackaddr$env-ref50298 = alloca %struct.ScmObj*, align 8
%acc40217 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41903, i64 2)
store %struct.ScmObj* %acc40217, %struct.ScmObj** %stackaddr$env-ref50298
%stackaddr$env-ref50299 = alloca %struct.ScmObj*, align 8
%_37foldl40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41903, i64 3)
store %struct.ScmObj* %_37foldl40214, %struct.ScmObj** %stackaddr$env-ref50299
%stackaddr$env-ref50300 = alloca %struct.ScmObj*, align 8
%k40453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41903, i64 4)
store %struct.ScmObj* %k40453, %struct.ScmObj** %stackaddr$env-ref50300
%stackaddr$env-ref50301 = alloca %struct.ScmObj*, align 8
%_37map140162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41903, i64 5)
store %struct.ScmObj* %_37map140162, %struct.ScmObj** %stackaddr$env-ref50301
%stackaddr$env-ref50302 = alloca %struct.ScmObj*, align 8
%f40218 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41903, i64 6)
store %struct.ScmObj* %f40218, %struct.ScmObj** %stackaddr$env-ref50302
%stackaddr$prim50303 = alloca %struct.ScmObj*, align 8
%_95k40455 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48769)
store volatile %struct.ScmObj* %_95k40455, %struct.ScmObj** %stackaddr$prim50303, align 8
%stackaddr$prim50304 = alloca %struct.ScmObj*, align 8
%current_45args48770 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48769)
store volatile %struct.ScmObj* %current_45args48770, %struct.ScmObj** %stackaddr$prim50304, align 8
%stackaddr$prim50305 = alloca %struct.ScmObj*, align 8
%anf_45bind40299 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48770)
store volatile %struct.ScmObj* %anf_45bind40299, %struct.ScmObj** %stackaddr$prim50305, align 8
%truthy$cmp50306 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40299)
%cmp$cmp50306 = icmp eq i64 %truthy$cmp50306, 1
br i1 %cmp$cmp50306, label %truebranch$cmp50306, label %falsebranch$cmp50306
truebranch$cmp50306:
%ae41914 = call %struct.ScmObj* @const_init_int(i64 0)
%args48772$k40453$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50307 = alloca %struct.ScmObj*, align 8
%args48772$k40453$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40217, %struct.ScmObj* %args48772$k40453$0)
store volatile %struct.ScmObj* %args48772$k40453$1, %struct.ScmObj** %stackaddr$prim50307, align 8
%stackaddr$prim50308 = alloca %struct.ScmObj*, align 8
%args48772$k40453$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41914, %struct.ScmObj* %args48772$k40453$1)
store volatile %struct.ScmObj* %args48772$k40453$2, %struct.ScmObj** %stackaddr$prim50308, align 8
%clofunc50309 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40453)
musttail call tailcc void %clofunc50309(%struct.ScmObj* %k40453, %struct.ScmObj* %args48772$k40453$2)
ret void
falsebranch$cmp50306:
%stackaddr$makeclosure50310 = alloca %struct.ScmObj*, align 8
%fptrToInt50311 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41919 to i64
%ae41919 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50311)
store volatile %struct.ScmObj* %ae41919, %struct.ScmObj** %stackaddr$makeclosure50310, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41919, %struct.ScmObj* %lsts40216, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41919, %struct.ScmObj* %_37foldr40136, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41919, %struct.ScmObj* %acc40217, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41919, %struct.ScmObj* %_37foldl40214, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41919, %struct.ScmObj* %k40453, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41919, %struct.ScmObj* %_37map140162, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41919, %struct.ScmObj* %f40218, i64 6)
%ae41920 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50312 = alloca %struct.ScmObj*, align 8
%fptrToInt50313 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41921 to i64
%ae41921 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50313)
store volatile %struct.ScmObj* %ae41921, %struct.ScmObj** %stackaddr$makeclosure50312, align 8
%args48812$ae41919$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50314 = alloca %struct.ScmObj*, align 8
%args48812$ae41919$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41921, %struct.ScmObj* %args48812$ae41919$0)
store volatile %struct.ScmObj* %args48812$ae41919$1, %struct.ScmObj** %stackaddr$prim50314, align 8
%stackaddr$prim50315 = alloca %struct.ScmObj*, align 8
%args48812$ae41919$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41920, %struct.ScmObj* %args48812$ae41919$1)
store volatile %struct.ScmObj* %args48812$ae41919$2, %struct.ScmObj** %stackaddr$prim50315, align 8
%clofunc50316 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41919)
musttail call tailcc void %clofunc50316(%struct.ScmObj* %ae41919, %struct.ScmObj* %args48812$ae41919$2)
ret void
}

define tailcc void @proc_clo$ae41919(%struct.ScmObj* %env$ae41919,%struct.ScmObj* %current_45args48773) {
%stackaddr$env-ref50317 = alloca %struct.ScmObj*, align 8
%lsts40216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41919, i64 0)
store %struct.ScmObj* %lsts40216, %struct.ScmObj** %stackaddr$env-ref50317
%stackaddr$env-ref50318 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41919, i64 1)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref50318
%stackaddr$env-ref50319 = alloca %struct.ScmObj*, align 8
%acc40217 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41919, i64 2)
store %struct.ScmObj* %acc40217, %struct.ScmObj** %stackaddr$env-ref50319
%stackaddr$env-ref50320 = alloca %struct.ScmObj*, align 8
%_37foldl40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41919, i64 3)
store %struct.ScmObj* %_37foldl40214, %struct.ScmObj** %stackaddr$env-ref50320
%stackaddr$env-ref50321 = alloca %struct.ScmObj*, align 8
%k40453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41919, i64 4)
store %struct.ScmObj* %k40453, %struct.ScmObj** %stackaddr$env-ref50321
%stackaddr$env-ref50322 = alloca %struct.ScmObj*, align 8
%_37map140162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41919, i64 5)
store %struct.ScmObj* %_37map140162, %struct.ScmObj** %stackaddr$env-ref50322
%stackaddr$env-ref50323 = alloca %struct.ScmObj*, align 8
%f40218 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41919, i64 6)
store %struct.ScmObj* %f40218, %struct.ScmObj** %stackaddr$env-ref50323
%stackaddr$prim50324 = alloca %struct.ScmObj*, align 8
%_95k40456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48773)
store volatile %struct.ScmObj* %_95k40456, %struct.ScmObj** %stackaddr$prim50324, align 8
%stackaddr$prim50325 = alloca %struct.ScmObj*, align 8
%current_45args48774 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48773)
store volatile %struct.ScmObj* %current_45args48774, %struct.ScmObj** %stackaddr$prim50325, align 8
%stackaddr$prim50326 = alloca %struct.ScmObj*, align 8
%anf_45bind40300 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48774)
store volatile %struct.ScmObj* %anf_45bind40300, %struct.ScmObj** %stackaddr$prim50326, align 8
%stackaddr$makeclosure50327 = alloca %struct.ScmObj*, align 8
%fptrToInt50328 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41940 to i64
%ae41940 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50328)
store volatile %struct.ScmObj* %ae41940, %struct.ScmObj** %stackaddr$makeclosure50327, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41940, %struct.ScmObj* %lsts40216, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41940, %struct.ScmObj* %_37foldr40136, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41940, %struct.ScmObj* %acc40217, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41940, %struct.ScmObj* %_37foldl40214, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41940, %struct.ScmObj* %k40453, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41940, %struct.ScmObj* %_37map140162, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41940, %struct.ScmObj* %f40218, i64 6)
%args48807$_37map140162$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50329 = alloca %struct.ScmObj*, align 8
%args48807$_37map140162$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40216, %struct.ScmObj* %args48807$_37map140162$0)
store volatile %struct.ScmObj* %args48807$_37map140162$1, %struct.ScmObj** %stackaddr$prim50329, align 8
%stackaddr$prim50330 = alloca %struct.ScmObj*, align 8
%args48807$_37map140162$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40300, %struct.ScmObj* %args48807$_37map140162$1)
store volatile %struct.ScmObj* %args48807$_37map140162$2, %struct.ScmObj** %stackaddr$prim50330, align 8
%stackaddr$prim50331 = alloca %struct.ScmObj*, align 8
%args48807$_37map140162$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41940, %struct.ScmObj* %args48807$_37map140162$2)
store volatile %struct.ScmObj* %args48807$_37map140162$3, %struct.ScmObj** %stackaddr$prim50331, align 8
%clofunc50332 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140162)
musttail call tailcc void %clofunc50332(%struct.ScmObj* %_37map140162, %struct.ScmObj* %args48807$_37map140162$3)
ret void
}

define tailcc void @proc_clo$ae41940(%struct.ScmObj* %env$ae41940,%struct.ScmObj* %current_45args48776) {
%stackaddr$env-ref50333 = alloca %struct.ScmObj*, align 8
%lsts40216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41940, i64 0)
store %struct.ScmObj* %lsts40216, %struct.ScmObj** %stackaddr$env-ref50333
%stackaddr$env-ref50334 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41940, i64 1)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref50334
%stackaddr$env-ref50335 = alloca %struct.ScmObj*, align 8
%acc40217 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41940, i64 2)
store %struct.ScmObj* %acc40217, %struct.ScmObj** %stackaddr$env-ref50335
%stackaddr$env-ref50336 = alloca %struct.ScmObj*, align 8
%_37foldl40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41940, i64 3)
store %struct.ScmObj* %_37foldl40214, %struct.ScmObj** %stackaddr$env-ref50336
%stackaddr$env-ref50337 = alloca %struct.ScmObj*, align 8
%k40453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41940, i64 4)
store %struct.ScmObj* %k40453, %struct.ScmObj** %stackaddr$env-ref50337
%stackaddr$env-ref50338 = alloca %struct.ScmObj*, align 8
%_37map140162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41940, i64 5)
store %struct.ScmObj* %_37map140162, %struct.ScmObj** %stackaddr$env-ref50338
%stackaddr$env-ref50339 = alloca %struct.ScmObj*, align 8
%f40218 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41940, i64 6)
store %struct.ScmObj* %f40218, %struct.ScmObj** %stackaddr$env-ref50339
%stackaddr$prim50340 = alloca %struct.ScmObj*, align 8
%_95k40457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48776)
store volatile %struct.ScmObj* %_95k40457, %struct.ScmObj** %stackaddr$prim50340, align 8
%stackaddr$prim50341 = alloca %struct.ScmObj*, align 8
%current_45args48777 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48776)
store volatile %struct.ScmObj* %current_45args48777, %struct.ScmObj** %stackaddr$prim50341, align 8
%stackaddr$prim50342 = alloca %struct.ScmObj*, align 8
%lsts_4340223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48777)
store volatile %struct.ScmObj* %lsts_4340223, %struct.ScmObj** %stackaddr$prim50342, align 8
%stackaddr$makeclosure50343 = alloca %struct.ScmObj*, align 8
%fptrToInt50344 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41943 to i64
%ae41943 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt50344)
store volatile %struct.ScmObj* %ae41943, %struct.ScmObj** %stackaddr$makeclosure50343, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41943, %struct.ScmObj* %lsts40216, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41943, %struct.ScmObj* %_37foldr40136, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41943, %struct.ScmObj* %acc40217, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41943, %struct.ScmObj* %_37foldl40214, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41943, %struct.ScmObj* %k40453, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41943, %struct.ScmObj* %_37map140162, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41943, %struct.ScmObj* %lsts_4340223, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41943, %struct.ScmObj* %f40218, i64 7)
%ae41944 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50345 = alloca %struct.ScmObj*, align 8
%fptrToInt50346 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41945 to i64
%ae41945 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50346)
store volatile %struct.ScmObj* %ae41945, %struct.ScmObj** %stackaddr$makeclosure50345, align 8
%args48806$ae41943$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50347 = alloca %struct.ScmObj*, align 8
%args48806$ae41943$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41945, %struct.ScmObj* %args48806$ae41943$0)
store volatile %struct.ScmObj* %args48806$ae41943$1, %struct.ScmObj** %stackaddr$prim50347, align 8
%stackaddr$prim50348 = alloca %struct.ScmObj*, align 8
%args48806$ae41943$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41944, %struct.ScmObj* %args48806$ae41943$1)
store volatile %struct.ScmObj* %args48806$ae41943$2, %struct.ScmObj** %stackaddr$prim50348, align 8
%clofunc50349 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41943)
musttail call tailcc void %clofunc50349(%struct.ScmObj* %ae41943, %struct.ScmObj* %args48806$ae41943$2)
ret void
}

define tailcc void @proc_clo$ae41943(%struct.ScmObj* %env$ae41943,%struct.ScmObj* %current_45args48779) {
%stackaddr$env-ref50350 = alloca %struct.ScmObj*, align 8
%lsts40216 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41943, i64 0)
store %struct.ScmObj* %lsts40216, %struct.ScmObj** %stackaddr$env-ref50350
%stackaddr$env-ref50351 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41943, i64 1)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref50351
%stackaddr$env-ref50352 = alloca %struct.ScmObj*, align 8
%acc40217 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41943, i64 2)
store %struct.ScmObj* %acc40217, %struct.ScmObj** %stackaddr$env-ref50352
%stackaddr$env-ref50353 = alloca %struct.ScmObj*, align 8
%_37foldl40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41943, i64 3)
store %struct.ScmObj* %_37foldl40214, %struct.ScmObj** %stackaddr$env-ref50353
%stackaddr$env-ref50354 = alloca %struct.ScmObj*, align 8
%k40453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41943, i64 4)
store %struct.ScmObj* %k40453, %struct.ScmObj** %stackaddr$env-ref50354
%stackaddr$env-ref50355 = alloca %struct.ScmObj*, align 8
%_37map140162 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41943, i64 5)
store %struct.ScmObj* %_37map140162, %struct.ScmObj** %stackaddr$env-ref50355
%stackaddr$env-ref50356 = alloca %struct.ScmObj*, align 8
%lsts_4340223 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41943, i64 6)
store %struct.ScmObj* %lsts_4340223, %struct.ScmObj** %stackaddr$env-ref50356
%stackaddr$env-ref50357 = alloca %struct.ScmObj*, align 8
%f40218 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41943, i64 7)
store %struct.ScmObj* %f40218, %struct.ScmObj** %stackaddr$env-ref50357
%stackaddr$prim50358 = alloca %struct.ScmObj*, align 8
%_95k40458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48779)
store volatile %struct.ScmObj* %_95k40458, %struct.ScmObj** %stackaddr$prim50358, align 8
%stackaddr$prim50359 = alloca %struct.ScmObj*, align 8
%current_45args48780 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48779)
store volatile %struct.ScmObj* %current_45args48780, %struct.ScmObj** %stackaddr$prim50359, align 8
%stackaddr$prim50360 = alloca %struct.ScmObj*, align 8
%anf_45bind40301 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48780)
store volatile %struct.ScmObj* %anf_45bind40301, %struct.ScmObj** %stackaddr$prim50360, align 8
%stackaddr$makeclosure50361 = alloca %struct.ScmObj*, align 8
%fptrToInt50362 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41964 to i64
%ae41964 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt50362)
store volatile %struct.ScmObj* %ae41964, %struct.ScmObj** %stackaddr$makeclosure50361, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41964, %struct.ScmObj* %acc40217, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41964, %struct.ScmObj* %_37foldr40136, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41964, %struct.ScmObj* %_37foldl40214, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41964, %struct.ScmObj* %k40453, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41964, %struct.ScmObj* %lsts_4340223, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41964, %struct.ScmObj* %f40218, i64 5)
%args48801$_37map140162$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50363 = alloca %struct.ScmObj*, align 8
%args48801$_37map140162$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40216, %struct.ScmObj* %args48801$_37map140162$0)
store volatile %struct.ScmObj* %args48801$_37map140162$1, %struct.ScmObj** %stackaddr$prim50363, align 8
%stackaddr$prim50364 = alloca %struct.ScmObj*, align 8
%args48801$_37map140162$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40301, %struct.ScmObj* %args48801$_37map140162$1)
store volatile %struct.ScmObj* %args48801$_37map140162$2, %struct.ScmObj** %stackaddr$prim50364, align 8
%stackaddr$prim50365 = alloca %struct.ScmObj*, align 8
%args48801$_37map140162$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41964, %struct.ScmObj* %args48801$_37map140162$2)
store volatile %struct.ScmObj* %args48801$_37map140162$3, %struct.ScmObj** %stackaddr$prim50365, align 8
%clofunc50366 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140162)
musttail call tailcc void %clofunc50366(%struct.ScmObj* %_37map140162, %struct.ScmObj* %args48801$_37map140162$3)
ret void
}

define tailcc void @proc_clo$ae41964(%struct.ScmObj* %env$ae41964,%struct.ScmObj* %current_45args48782) {
%stackaddr$env-ref50367 = alloca %struct.ScmObj*, align 8
%acc40217 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41964, i64 0)
store %struct.ScmObj* %acc40217, %struct.ScmObj** %stackaddr$env-ref50367
%stackaddr$env-ref50368 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41964, i64 1)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref50368
%stackaddr$env-ref50369 = alloca %struct.ScmObj*, align 8
%_37foldl40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41964, i64 2)
store %struct.ScmObj* %_37foldl40214, %struct.ScmObj** %stackaddr$env-ref50369
%stackaddr$env-ref50370 = alloca %struct.ScmObj*, align 8
%k40453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41964, i64 3)
store %struct.ScmObj* %k40453, %struct.ScmObj** %stackaddr$env-ref50370
%stackaddr$env-ref50371 = alloca %struct.ScmObj*, align 8
%lsts_4340223 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41964, i64 4)
store %struct.ScmObj* %lsts_4340223, %struct.ScmObj** %stackaddr$env-ref50371
%stackaddr$env-ref50372 = alloca %struct.ScmObj*, align 8
%f40218 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41964, i64 5)
store %struct.ScmObj* %f40218, %struct.ScmObj** %stackaddr$env-ref50372
%stackaddr$prim50373 = alloca %struct.ScmObj*, align 8
%_95k40459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48782)
store volatile %struct.ScmObj* %_95k40459, %struct.ScmObj** %stackaddr$prim50373, align 8
%stackaddr$prim50374 = alloca %struct.ScmObj*, align 8
%current_45args48783 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48782)
store volatile %struct.ScmObj* %current_45args48783, %struct.ScmObj** %stackaddr$prim50374, align 8
%stackaddr$prim50375 = alloca %struct.ScmObj*, align 8
%vs40221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48783)
store volatile %struct.ScmObj* %vs40221, %struct.ScmObj** %stackaddr$prim50375, align 8
%stackaddr$makeclosure50376 = alloca %struct.ScmObj*, align 8
%fptrToInt50377 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41967 to i64
%ae41967 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50377)
store volatile %struct.ScmObj* %ae41967, %struct.ScmObj** %stackaddr$makeclosure50376, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41967, %struct.ScmObj* %acc40217, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41967, %struct.ScmObj* %_37foldr40136, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41967, %struct.ScmObj* %_37foldl40214, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41967, %struct.ScmObj* %k40453, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41967, %struct.ScmObj* %lsts_4340223, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41967, %struct.ScmObj* %vs40221, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41967, %struct.ScmObj* %f40218, i64 6)
%ae41968 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50378 = alloca %struct.ScmObj*, align 8
%fptrToInt50379 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41969 to i64
%ae41969 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50379)
store volatile %struct.ScmObj* %ae41969, %struct.ScmObj** %stackaddr$makeclosure50378, align 8
%args48800$ae41967$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50380 = alloca %struct.ScmObj*, align 8
%args48800$ae41967$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41969, %struct.ScmObj* %args48800$ae41967$0)
store volatile %struct.ScmObj* %args48800$ae41967$1, %struct.ScmObj** %stackaddr$prim50380, align 8
%stackaddr$prim50381 = alloca %struct.ScmObj*, align 8
%args48800$ae41967$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41968, %struct.ScmObj* %args48800$ae41967$1)
store volatile %struct.ScmObj* %args48800$ae41967$2, %struct.ScmObj** %stackaddr$prim50381, align 8
%clofunc50382 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41967)
musttail call tailcc void %clofunc50382(%struct.ScmObj* %ae41967, %struct.ScmObj* %args48800$ae41967$2)
ret void
}

define tailcc void @proc_clo$ae41967(%struct.ScmObj* %env$ae41967,%struct.ScmObj* %current_45args48785) {
%stackaddr$env-ref50383 = alloca %struct.ScmObj*, align 8
%acc40217 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41967, i64 0)
store %struct.ScmObj* %acc40217, %struct.ScmObj** %stackaddr$env-ref50383
%stackaddr$env-ref50384 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41967, i64 1)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref50384
%stackaddr$env-ref50385 = alloca %struct.ScmObj*, align 8
%_37foldl40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41967, i64 2)
store %struct.ScmObj* %_37foldl40214, %struct.ScmObj** %stackaddr$env-ref50385
%stackaddr$env-ref50386 = alloca %struct.ScmObj*, align 8
%k40453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41967, i64 3)
store %struct.ScmObj* %k40453, %struct.ScmObj** %stackaddr$env-ref50386
%stackaddr$env-ref50387 = alloca %struct.ScmObj*, align 8
%lsts_4340223 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41967, i64 4)
store %struct.ScmObj* %lsts_4340223, %struct.ScmObj** %stackaddr$env-ref50387
%stackaddr$env-ref50388 = alloca %struct.ScmObj*, align 8
%vs40221 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41967, i64 5)
store %struct.ScmObj* %vs40221, %struct.ScmObj** %stackaddr$env-ref50388
%stackaddr$env-ref50389 = alloca %struct.ScmObj*, align 8
%f40218 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41967, i64 6)
store %struct.ScmObj* %f40218, %struct.ScmObj** %stackaddr$env-ref50389
%stackaddr$prim50390 = alloca %struct.ScmObj*, align 8
%_95k40460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48785)
store volatile %struct.ScmObj* %_95k40460, %struct.ScmObj** %stackaddr$prim50390, align 8
%stackaddr$prim50391 = alloca %struct.ScmObj*, align 8
%current_45args48786 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48785)
store volatile %struct.ScmObj* %current_45args48786, %struct.ScmObj** %stackaddr$prim50391, align 8
%stackaddr$prim50392 = alloca %struct.ScmObj*, align 8
%anf_45bind40302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48786)
store volatile %struct.ScmObj* %anf_45bind40302, %struct.ScmObj** %stackaddr$prim50392, align 8
%ae41990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50393 = alloca %struct.ScmObj*, align 8
%anf_45bind40303 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40217, %struct.ScmObj* %ae41990)
store volatile %struct.ScmObj* %anf_45bind40303, %struct.ScmObj** %stackaddr$prim50393, align 8
%stackaddr$makeclosure50394 = alloca %struct.ScmObj*, align 8
%fptrToInt50395 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41992 to i64
%ae41992 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50395)
store volatile %struct.ScmObj* %ae41992, %struct.ScmObj** %stackaddr$makeclosure50394, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41992, %struct.ScmObj* %_37foldl40214, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41992, %struct.ScmObj* %k40453, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41992, %struct.ScmObj* %lsts_4340223, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41992, %struct.ScmObj* %f40218, i64 3)
%args48794$_37foldr40136$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50396 = alloca %struct.ScmObj*, align 8
%args48794$_37foldr40136$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40221, %struct.ScmObj* %args48794$_37foldr40136$0)
store volatile %struct.ScmObj* %args48794$_37foldr40136$1, %struct.ScmObj** %stackaddr$prim50396, align 8
%stackaddr$prim50397 = alloca %struct.ScmObj*, align 8
%args48794$_37foldr40136$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40303, %struct.ScmObj* %args48794$_37foldr40136$1)
store volatile %struct.ScmObj* %args48794$_37foldr40136$2, %struct.ScmObj** %stackaddr$prim50397, align 8
%stackaddr$prim50398 = alloca %struct.ScmObj*, align 8
%args48794$_37foldr40136$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40302, %struct.ScmObj* %args48794$_37foldr40136$2)
store volatile %struct.ScmObj* %args48794$_37foldr40136$3, %struct.ScmObj** %stackaddr$prim50398, align 8
%stackaddr$prim50399 = alloca %struct.ScmObj*, align 8
%args48794$_37foldr40136$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41992, %struct.ScmObj* %args48794$_37foldr40136$3)
store volatile %struct.ScmObj* %args48794$_37foldr40136$4, %struct.ScmObj** %stackaddr$prim50399, align 8
%clofunc50400 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40136)
musttail call tailcc void %clofunc50400(%struct.ScmObj* %_37foldr40136, %struct.ScmObj* %args48794$_37foldr40136$4)
ret void
}

define tailcc void @proc_clo$ae41992(%struct.ScmObj* %env$ae41992,%struct.ScmObj* %current_45args48788) {
%stackaddr$env-ref50401 = alloca %struct.ScmObj*, align 8
%_37foldl40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41992, i64 0)
store %struct.ScmObj* %_37foldl40214, %struct.ScmObj** %stackaddr$env-ref50401
%stackaddr$env-ref50402 = alloca %struct.ScmObj*, align 8
%k40453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41992, i64 1)
store %struct.ScmObj* %k40453, %struct.ScmObj** %stackaddr$env-ref50402
%stackaddr$env-ref50403 = alloca %struct.ScmObj*, align 8
%lsts_4340223 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41992, i64 2)
store %struct.ScmObj* %lsts_4340223, %struct.ScmObj** %stackaddr$env-ref50403
%stackaddr$env-ref50404 = alloca %struct.ScmObj*, align 8
%f40218 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41992, i64 3)
store %struct.ScmObj* %f40218, %struct.ScmObj** %stackaddr$env-ref50404
%stackaddr$prim50405 = alloca %struct.ScmObj*, align 8
%_95k40461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48788)
store volatile %struct.ScmObj* %_95k40461, %struct.ScmObj** %stackaddr$prim50405, align 8
%stackaddr$prim50406 = alloca %struct.ScmObj*, align 8
%current_45args48789 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48788)
store volatile %struct.ScmObj* %current_45args48789, %struct.ScmObj** %stackaddr$prim50406, align 8
%stackaddr$prim50407 = alloca %struct.ScmObj*, align 8
%anf_45bind40304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48789)
store volatile %struct.ScmObj* %anf_45bind40304, %struct.ScmObj** %stackaddr$prim50407, align 8
%stackaddr$makeclosure50408 = alloca %struct.ScmObj*, align 8
%fptrToInt50409 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41996 to i64
%ae41996 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50409)
store volatile %struct.ScmObj* %ae41996, %struct.ScmObj** %stackaddr$makeclosure50408, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41996, %struct.ScmObj* %_37foldl40214, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41996, %struct.ScmObj* %k40453, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41996, %struct.ScmObj* %lsts_4340223, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41996, %struct.ScmObj* %f40218, i64 3)
%stackaddr$prim50410 = alloca %struct.ScmObj*, align 8
%cpsargs40464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41996, %struct.ScmObj* %anf_45bind40304)
store volatile %struct.ScmObj* %cpsargs40464, %struct.ScmObj** %stackaddr$prim50410, align 8
%clofunc50411 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40218)
musttail call tailcc void %clofunc50411(%struct.ScmObj* %f40218, %struct.ScmObj* %cpsargs40464)
ret void
}

define tailcc void @proc_clo$ae41996(%struct.ScmObj* %env$ae41996,%struct.ScmObj* %current_45args48791) {
%stackaddr$env-ref50412 = alloca %struct.ScmObj*, align 8
%_37foldl40214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41996, i64 0)
store %struct.ScmObj* %_37foldl40214, %struct.ScmObj** %stackaddr$env-ref50412
%stackaddr$env-ref50413 = alloca %struct.ScmObj*, align 8
%k40453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41996, i64 1)
store %struct.ScmObj* %k40453, %struct.ScmObj** %stackaddr$env-ref50413
%stackaddr$env-ref50414 = alloca %struct.ScmObj*, align 8
%lsts_4340223 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41996, i64 2)
store %struct.ScmObj* %lsts_4340223, %struct.ScmObj** %stackaddr$env-ref50414
%stackaddr$env-ref50415 = alloca %struct.ScmObj*, align 8
%f40218 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41996, i64 3)
store %struct.ScmObj* %f40218, %struct.ScmObj** %stackaddr$env-ref50415
%stackaddr$prim50416 = alloca %struct.ScmObj*, align 8
%_95k40462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48791)
store volatile %struct.ScmObj* %_95k40462, %struct.ScmObj** %stackaddr$prim50416, align 8
%stackaddr$prim50417 = alloca %struct.ScmObj*, align 8
%current_45args48792 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48791)
store volatile %struct.ScmObj* %current_45args48792, %struct.ScmObj** %stackaddr$prim50417, align 8
%stackaddr$prim50418 = alloca %struct.ScmObj*, align 8
%acc_4340225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48792)
store volatile %struct.ScmObj* %acc_4340225, %struct.ScmObj** %stackaddr$prim50418, align 8
%stackaddr$prim50419 = alloca %struct.ScmObj*, align 8
%anf_45bind40305 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4340225, %struct.ScmObj* %lsts_4340223)
store volatile %struct.ScmObj* %anf_45bind40305, %struct.ScmObj** %stackaddr$prim50419, align 8
%stackaddr$prim50420 = alloca %struct.ScmObj*, align 8
%anf_45bind40306 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40218, %struct.ScmObj* %anf_45bind40305)
store volatile %struct.ScmObj* %anf_45bind40306, %struct.ScmObj** %stackaddr$prim50420, align 8
%stackaddr$prim50421 = alloca %struct.ScmObj*, align 8
%cpsargs40463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40453, %struct.ScmObj* %anf_45bind40306)
store volatile %struct.ScmObj* %cpsargs40463, %struct.ScmObj** %stackaddr$prim50421, align 8
%clofunc50422 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl40214)
musttail call tailcc void %clofunc50422(%struct.ScmObj* %_37foldl40214, %struct.ScmObj* %cpsargs40463)
ret void
}

define tailcc void @proc_clo$ae41969(%struct.ScmObj* %env$ae41969,%struct.ScmObj* %current_45args48795) {
%stackaddr$prim50423 = alloca %struct.ScmObj*, align 8
%k40465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48795)
store volatile %struct.ScmObj* %k40465, %struct.ScmObj** %stackaddr$prim50423, align 8
%stackaddr$prim50424 = alloca %struct.ScmObj*, align 8
%current_45args48796 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48795)
store volatile %struct.ScmObj* %current_45args48796, %struct.ScmObj** %stackaddr$prim50424, align 8
%stackaddr$prim50425 = alloca %struct.ScmObj*, align 8
%a40227 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48796)
store volatile %struct.ScmObj* %a40227, %struct.ScmObj** %stackaddr$prim50425, align 8
%stackaddr$prim50426 = alloca %struct.ScmObj*, align 8
%current_45args48797 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48796)
store volatile %struct.ScmObj* %current_45args48797, %struct.ScmObj** %stackaddr$prim50426, align 8
%stackaddr$prim50427 = alloca %struct.ScmObj*, align 8
%b40226 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48797)
store volatile %struct.ScmObj* %b40226, %struct.ScmObj** %stackaddr$prim50427, align 8
%stackaddr$prim50428 = alloca %struct.ScmObj*, align 8
%cpsprim40466 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40227, %struct.ScmObj* %b40226)
store volatile %struct.ScmObj* %cpsprim40466, %struct.ScmObj** %stackaddr$prim50428, align 8
%ae41973 = call %struct.ScmObj* @const_init_int(i64 0)
%args48799$k40465$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50429 = alloca %struct.ScmObj*, align 8
%args48799$k40465$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40466, %struct.ScmObj* %args48799$k40465$0)
store volatile %struct.ScmObj* %args48799$k40465$1, %struct.ScmObj** %stackaddr$prim50429, align 8
%stackaddr$prim50430 = alloca %struct.ScmObj*, align 8
%args48799$k40465$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41973, %struct.ScmObj* %args48799$k40465$1)
store volatile %struct.ScmObj* %args48799$k40465$2, %struct.ScmObj** %stackaddr$prim50430, align 8
%clofunc50431 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40465)
musttail call tailcc void %clofunc50431(%struct.ScmObj* %k40465, %struct.ScmObj* %args48799$k40465$2)
ret void
}

define tailcc void @proc_clo$ae41945(%struct.ScmObj* %env$ae41945,%struct.ScmObj* %current_45args48802) {
%stackaddr$prim50432 = alloca %struct.ScmObj*, align 8
%k40467 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48802)
store volatile %struct.ScmObj* %k40467, %struct.ScmObj** %stackaddr$prim50432, align 8
%stackaddr$prim50433 = alloca %struct.ScmObj*, align 8
%current_45args48803 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48802)
store volatile %struct.ScmObj* %current_45args48803, %struct.ScmObj** %stackaddr$prim50433, align 8
%stackaddr$prim50434 = alloca %struct.ScmObj*, align 8
%x40222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48803)
store volatile %struct.ScmObj* %x40222, %struct.ScmObj** %stackaddr$prim50434, align 8
%stackaddr$prim50435 = alloca %struct.ScmObj*, align 8
%cpsprim40468 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40222)
store volatile %struct.ScmObj* %cpsprim40468, %struct.ScmObj** %stackaddr$prim50435, align 8
%ae41948 = call %struct.ScmObj* @const_init_int(i64 0)
%args48805$k40467$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50436 = alloca %struct.ScmObj*, align 8
%args48805$k40467$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40468, %struct.ScmObj* %args48805$k40467$0)
store volatile %struct.ScmObj* %args48805$k40467$1, %struct.ScmObj** %stackaddr$prim50436, align 8
%stackaddr$prim50437 = alloca %struct.ScmObj*, align 8
%args48805$k40467$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41948, %struct.ScmObj* %args48805$k40467$1)
store volatile %struct.ScmObj* %args48805$k40467$2, %struct.ScmObj** %stackaddr$prim50437, align 8
%clofunc50438 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40467)
musttail call tailcc void %clofunc50438(%struct.ScmObj* %k40467, %struct.ScmObj* %args48805$k40467$2)
ret void
}

define tailcc void @proc_clo$ae41921(%struct.ScmObj* %env$ae41921,%struct.ScmObj* %current_45args48808) {
%stackaddr$prim50439 = alloca %struct.ScmObj*, align 8
%k40469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48808)
store volatile %struct.ScmObj* %k40469, %struct.ScmObj** %stackaddr$prim50439, align 8
%stackaddr$prim50440 = alloca %struct.ScmObj*, align 8
%current_45args48809 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48808)
store volatile %struct.ScmObj* %current_45args48809, %struct.ScmObj** %stackaddr$prim50440, align 8
%stackaddr$prim50441 = alloca %struct.ScmObj*, align 8
%x40224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48809)
store volatile %struct.ScmObj* %x40224, %struct.ScmObj** %stackaddr$prim50441, align 8
%stackaddr$prim50442 = alloca %struct.ScmObj*, align 8
%cpsprim40470 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40224)
store volatile %struct.ScmObj* %cpsprim40470, %struct.ScmObj** %stackaddr$prim50442, align 8
%ae41924 = call %struct.ScmObj* @const_init_int(i64 0)
%args48811$k40469$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50443 = alloca %struct.ScmObj*, align 8
%args48811$k40469$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40470, %struct.ScmObj* %args48811$k40469$0)
store volatile %struct.ScmObj* %args48811$k40469$1, %struct.ScmObj** %stackaddr$prim50443, align 8
%stackaddr$prim50444 = alloca %struct.ScmObj*, align 8
%args48811$k40469$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41924, %struct.ScmObj* %args48811$k40469$1)
store volatile %struct.ScmObj* %args48811$k40469$2, %struct.ScmObj** %stackaddr$prim50444, align 8
%clofunc50445 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40469)
musttail call tailcc void %clofunc50445(%struct.ScmObj* %k40469, %struct.ScmObj* %args48811$k40469$2)
ret void
}

define tailcc void @proc_clo$ae41873(%struct.ScmObj* %env$ae41873,%struct.ScmObj* %current_45args48814) {
%stackaddr$prim50446 = alloca %struct.ScmObj*, align 8
%k40471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48814)
store volatile %struct.ScmObj* %k40471, %struct.ScmObj** %stackaddr$prim50446, align 8
%stackaddr$prim50447 = alloca %struct.ScmObj*, align 8
%current_45args48815 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48814)
store volatile %struct.ScmObj* %current_45args48815, %struct.ScmObj** %stackaddr$prim50447, align 8
%stackaddr$prim50448 = alloca %struct.ScmObj*, align 8
%lst40220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48815)
store volatile %struct.ScmObj* %lst40220, %struct.ScmObj** %stackaddr$prim50448, align 8
%stackaddr$prim50449 = alloca %struct.ScmObj*, align 8
%current_45args48816 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48815)
store volatile %struct.ScmObj* %current_45args48816, %struct.ScmObj** %stackaddr$prim50449, align 8
%stackaddr$prim50450 = alloca %struct.ScmObj*, align 8
%b40219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48816)
store volatile %struct.ScmObj* %b40219, %struct.ScmObj** %stackaddr$prim50450, align 8
%truthy$cmp50451 = call i64 @is_truthy_value(%struct.ScmObj* %b40219)
%cmp$cmp50451 = icmp eq i64 %truthy$cmp50451, 1
br i1 %cmp$cmp50451, label %truebranch$cmp50451, label %falsebranch$cmp50451
truebranch$cmp50451:
%ae41876 = call %struct.ScmObj* @const_init_int(i64 0)
%args48818$k40471$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50452 = alloca %struct.ScmObj*, align 8
%args48818$k40471$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40219, %struct.ScmObj* %args48818$k40471$0)
store volatile %struct.ScmObj* %args48818$k40471$1, %struct.ScmObj** %stackaddr$prim50452, align 8
%stackaddr$prim50453 = alloca %struct.ScmObj*, align 8
%args48818$k40471$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41876, %struct.ScmObj* %args48818$k40471$1)
store volatile %struct.ScmObj* %args48818$k40471$2, %struct.ScmObj** %stackaddr$prim50453, align 8
%clofunc50454 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40471)
musttail call tailcc void %clofunc50454(%struct.ScmObj* %k40471, %struct.ScmObj* %args48818$k40471$2)
ret void
falsebranch$cmp50451:
%stackaddr$prim50455 = alloca %struct.ScmObj*, align 8
%cpsprim40472 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40220)
store volatile %struct.ScmObj* %cpsprim40472, %struct.ScmObj** %stackaddr$prim50455, align 8
%ae41883 = call %struct.ScmObj* @const_init_int(i64 0)
%args48819$k40471$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50456 = alloca %struct.ScmObj*, align 8
%args48819$k40471$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40472, %struct.ScmObj* %args48819$k40471$0)
store volatile %struct.ScmObj* %args48819$k40471$1, %struct.ScmObj** %stackaddr$prim50456, align 8
%stackaddr$prim50457 = alloca %struct.ScmObj*, align 8
%args48819$k40471$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41883, %struct.ScmObj* %args48819$k40471$1)
store volatile %struct.ScmObj* %args48819$k40471$2, %struct.ScmObj** %stackaddr$prim50457, align 8
%clofunc50458 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40471)
musttail call tailcc void %clofunc50458(%struct.ScmObj* %k40471, %struct.ScmObj* %args48819$k40471$2)
ret void
}

define tailcc void @proc_clo$ae41714(%struct.ScmObj* %env$ae41714,%struct.ScmObj* %args4015840473) {
%stackaddr$env-ref50459 = alloca %struct.ScmObj*, align 8
%_37last40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41714, i64 0)
store %struct.ScmObj* %_37last40153, %struct.ScmObj** %stackaddr$env-ref50459
%stackaddr$env-ref50460 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41714, i64 1)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref50460
%stackaddr$env-ref50461 = alloca %struct.ScmObj*, align 8
%_37drop_45right40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41714, i64 2)
store %struct.ScmObj* %_37drop_45right40150, %struct.ScmObj** %stackaddr$env-ref50461
%stackaddr$prim50462 = alloca %struct.ScmObj*, align 8
%k40474 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4015840473)
store volatile %struct.ScmObj* %k40474, %struct.ScmObj** %stackaddr$prim50462, align 8
%stackaddr$prim50463 = alloca %struct.ScmObj*, align 8
%args40158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4015840473)
store volatile %struct.ScmObj* %args40158, %struct.ScmObj** %stackaddr$prim50463, align 8
%stackaddr$prim50464 = alloca %struct.ScmObj*, align 8
%f40160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40158)
store volatile %struct.ScmObj* %f40160, %struct.ScmObj** %stackaddr$prim50464, align 8
%stackaddr$prim50465 = alloca %struct.ScmObj*, align 8
%lsts40159 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40158)
store volatile %struct.ScmObj* %lsts40159, %struct.ScmObj** %stackaddr$prim50465, align 8
%stackaddr$makeclosure50466 = alloca %struct.ScmObj*, align 8
%fptrToInt50467 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41719 to i64
%ae41719 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50467)
store volatile %struct.ScmObj* %ae41719, %struct.ScmObj** %stackaddr$makeclosure50466, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41719, %struct.ScmObj* %_37foldr40136, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41719, %struct.ScmObj* %lsts40159, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41719, %struct.ScmObj* %k40474, i64 2)
%ae41720 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50468 = alloca %struct.ScmObj*, align 8
%fptrToInt50469 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41721 to i64
%ae41721 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50469)
store volatile %struct.ScmObj* %ae41721, %struct.ScmObj** %stackaddr$makeclosure50468, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41721, %struct.ScmObj* %_37last40153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41721, %struct.ScmObj* %_37drop_45right40150, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41721, %struct.ScmObj* %f40160, i64 2)
%args48838$ae41719$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50470 = alloca %struct.ScmObj*, align 8
%args48838$ae41719$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41721, %struct.ScmObj* %args48838$ae41719$0)
store volatile %struct.ScmObj* %args48838$ae41719$1, %struct.ScmObj** %stackaddr$prim50470, align 8
%stackaddr$prim50471 = alloca %struct.ScmObj*, align 8
%args48838$ae41719$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41720, %struct.ScmObj* %args48838$ae41719$1)
store volatile %struct.ScmObj* %args48838$ae41719$2, %struct.ScmObj** %stackaddr$prim50471, align 8
%clofunc50472 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41719)
musttail call tailcc void %clofunc50472(%struct.ScmObj* %ae41719, %struct.ScmObj* %args48838$ae41719$2)
ret void
}

define tailcc void @proc_clo$ae41719(%struct.ScmObj* %env$ae41719,%struct.ScmObj* %current_45args48823) {
%stackaddr$env-ref50473 = alloca %struct.ScmObj*, align 8
%_37foldr40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41719, i64 0)
store %struct.ScmObj* %_37foldr40136, %struct.ScmObj** %stackaddr$env-ref50473
%stackaddr$env-ref50474 = alloca %struct.ScmObj*, align 8
%lsts40159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41719, i64 1)
store %struct.ScmObj* %lsts40159, %struct.ScmObj** %stackaddr$env-ref50474
%stackaddr$env-ref50475 = alloca %struct.ScmObj*, align 8
%k40474 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41719, i64 2)
store %struct.ScmObj* %k40474, %struct.ScmObj** %stackaddr$env-ref50475
%stackaddr$prim50476 = alloca %struct.ScmObj*, align 8
%_95k40475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48823)
store volatile %struct.ScmObj* %_95k40475, %struct.ScmObj** %stackaddr$prim50476, align 8
%stackaddr$prim50477 = alloca %struct.ScmObj*, align 8
%current_45args48824 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48823)
store volatile %struct.ScmObj* %current_45args48824, %struct.ScmObj** %stackaddr$prim50477, align 8
%stackaddr$prim50478 = alloca %struct.ScmObj*, align 8
%anf_45bind40293 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48824)
store volatile %struct.ScmObj* %anf_45bind40293, %struct.ScmObj** %stackaddr$prim50478, align 8
%ae41782 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50479 = alloca %struct.ScmObj*, align 8
%anf_45bind40294 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41782, %struct.ScmObj* %lsts40159)
store volatile %struct.ScmObj* %anf_45bind40294, %struct.ScmObj** %stackaddr$prim50479, align 8
%stackaddr$prim50480 = alloca %struct.ScmObj*, align 8
%anf_45bind40295 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40293, %struct.ScmObj* %anf_45bind40294)
store volatile %struct.ScmObj* %anf_45bind40295, %struct.ScmObj** %stackaddr$prim50480, align 8
%stackaddr$prim50481 = alloca %struct.ScmObj*, align 8
%cpsargs40476 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40474, %struct.ScmObj* %anf_45bind40295)
store volatile %struct.ScmObj* %cpsargs40476, %struct.ScmObj** %stackaddr$prim50481, align 8
%clofunc50482 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40136)
musttail call tailcc void %clofunc50482(%struct.ScmObj* %_37foldr40136, %struct.ScmObj* %cpsargs40476)
ret void
}

define tailcc void @proc_clo$ae41721(%struct.ScmObj* %env$ae41721,%struct.ScmObj* %fargs4016140477) {
%stackaddr$env-ref50483 = alloca %struct.ScmObj*, align 8
%_37last40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41721, i64 0)
store %struct.ScmObj* %_37last40153, %struct.ScmObj** %stackaddr$env-ref50483
%stackaddr$env-ref50484 = alloca %struct.ScmObj*, align 8
%_37drop_45right40150 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41721, i64 1)
store %struct.ScmObj* %_37drop_45right40150, %struct.ScmObj** %stackaddr$env-ref50484
%stackaddr$env-ref50485 = alloca %struct.ScmObj*, align 8
%f40160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41721, i64 2)
store %struct.ScmObj* %f40160, %struct.ScmObj** %stackaddr$env-ref50485
%stackaddr$prim50486 = alloca %struct.ScmObj*, align 8
%k40478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4016140477)
store volatile %struct.ScmObj* %k40478, %struct.ScmObj** %stackaddr$prim50486, align 8
%stackaddr$prim50487 = alloca %struct.ScmObj*, align 8
%fargs40161 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4016140477)
store volatile %struct.ScmObj* %fargs40161, %struct.ScmObj** %stackaddr$prim50487, align 8
%stackaddr$makeclosure50488 = alloca %struct.ScmObj*, align 8
%fptrToInt50489 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41725 to i64
%ae41725 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50489)
store volatile %struct.ScmObj* %ae41725, %struct.ScmObj** %stackaddr$makeclosure50488, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41725, %struct.ScmObj* %_37last40153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41725, %struct.ScmObj* %fargs40161, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41725, %struct.ScmObj* %f40160, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41725, %struct.ScmObj* %k40478, i64 3)
%ae41727 = call %struct.ScmObj* @const_init_int(i64 1)
%args48837$_37drop_45right40150$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50490 = alloca %struct.ScmObj*, align 8
%args48837$_37drop_45right40150$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41727, %struct.ScmObj* %args48837$_37drop_45right40150$0)
store volatile %struct.ScmObj* %args48837$_37drop_45right40150$1, %struct.ScmObj** %stackaddr$prim50490, align 8
%stackaddr$prim50491 = alloca %struct.ScmObj*, align 8
%args48837$_37drop_45right40150$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40161, %struct.ScmObj* %args48837$_37drop_45right40150$1)
store volatile %struct.ScmObj* %args48837$_37drop_45right40150$2, %struct.ScmObj** %stackaddr$prim50491, align 8
%stackaddr$prim50492 = alloca %struct.ScmObj*, align 8
%args48837$_37drop_45right40150$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41725, %struct.ScmObj* %args48837$_37drop_45right40150$2)
store volatile %struct.ScmObj* %args48837$_37drop_45right40150$3, %struct.ScmObj** %stackaddr$prim50492, align 8
%clofunc50493 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right40150)
musttail call tailcc void %clofunc50493(%struct.ScmObj* %_37drop_45right40150, %struct.ScmObj* %args48837$_37drop_45right40150$3)
ret void
}

define tailcc void @proc_clo$ae41725(%struct.ScmObj* %env$ae41725,%struct.ScmObj* %current_45args48826) {
%stackaddr$env-ref50494 = alloca %struct.ScmObj*, align 8
%_37last40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41725, i64 0)
store %struct.ScmObj* %_37last40153, %struct.ScmObj** %stackaddr$env-ref50494
%stackaddr$env-ref50495 = alloca %struct.ScmObj*, align 8
%fargs40161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41725, i64 1)
store %struct.ScmObj* %fargs40161, %struct.ScmObj** %stackaddr$env-ref50495
%stackaddr$env-ref50496 = alloca %struct.ScmObj*, align 8
%f40160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41725, i64 2)
store %struct.ScmObj* %f40160, %struct.ScmObj** %stackaddr$env-ref50496
%stackaddr$env-ref50497 = alloca %struct.ScmObj*, align 8
%k40478 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41725, i64 3)
store %struct.ScmObj* %k40478, %struct.ScmObj** %stackaddr$env-ref50497
%stackaddr$prim50498 = alloca %struct.ScmObj*, align 8
%_95k40479 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48826)
store volatile %struct.ScmObj* %_95k40479, %struct.ScmObj** %stackaddr$prim50498, align 8
%stackaddr$prim50499 = alloca %struct.ScmObj*, align 8
%current_45args48827 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48826)
store volatile %struct.ScmObj* %current_45args48827, %struct.ScmObj** %stackaddr$prim50499, align 8
%stackaddr$prim50500 = alloca %struct.ScmObj*, align 8
%anf_45bind40290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48827)
store volatile %struct.ScmObj* %anf_45bind40290, %struct.ScmObj** %stackaddr$prim50500, align 8
%stackaddr$makeclosure50501 = alloca %struct.ScmObj*, align 8
%fptrToInt50502 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41732 to i64
%ae41732 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50502)
store volatile %struct.ScmObj* %ae41732, %struct.ScmObj** %stackaddr$makeclosure50501, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41732, %struct.ScmObj* %_37last40153, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41732, %struct.ScmObj* %fargs40161, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41732, %struct.ScmObj* %k40478, i64 2)
%stackaddr$prim50503 = alloca %struct.ScmObj*, align 8
%cpsargs40483 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41732, %struct.ScmObj* %anf_45bind40290)
store volatile %struct.ScmObj* %cpsargs40483, %struct.ScmObj** %stackaddr$prim50503, align 8
%clofunc50504 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40160)
musttail call tailcc void %clofunc50504(%struct.ScmObj* %f40160, %struct.ScmObj* %cpsargs40483)
ret void
}

define tailcc void @proc_clo$ae41732(%struct.ScmObj* %env$ae41732,%struct.ScmObj* %current_45args48829) {
%stackaddr$env-ref50505 = alloca %struct.ScmObj*, align 8
%_37last40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41732, i64 0)
store %struct.ScmObj* %_37last40153, %struct.ScmObj** %stackaddr$env-ref50505
%stackaddr$env-ref50506 = alloca %struct.ScmObj*, align 8
%fargs40161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41732, i64 1)
store %struct.ScmObj* %fargs40161, %struct.ScmObj** %stackaddr$env-ref50506
%stackaddr$env-ref50507 = alloca %struct.ScmObj*, align 8
%k40478 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41732, i64 2)
store %struct.ScmObj* %k40478, %struct.ScmObj** %stackaddr$env-ref50507
%stackaddr$prim50508 = alloca %struct.ScmObj*, align 8
%_95k40480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48829)
store volatile %struct.ScmObj* %_95k40480, %struct.ScmObj** %stackaddr$prim50508, align 8
%stackaddr$prim50509 = alloca %struct.ScmObj*, align 8
%current_45args48830 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48829)
store volatile %struct.ScmObj* %current_45args48830, %struct.ScmObj** %stackaddr$prim50509, align 8
%stackaddr$prim50510 = alloca %struct.ScmObj*, align 8
%anf_45bind40291 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48830)
store volatile %struct.ScmObj* %anf_45bind40291, %struct.ScmObj** %stackaddr$prim50510, align 8
%stackaddr$makeclosure50511 = alloca %struct.ScmObj*, align 8
%fptrToInt50512 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41737 to i64
%ae41737 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50512)
store volatile %struct.ScmObj* %ae41737, %struct.ScmObj** %stackaddr$makeclosure50511, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41737, %struct.ScmObj* %anf_45bind40291, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41737, %struct.ScmObj* %k40478, i64 1)
%args48836$_37last40153$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50513 = alloca %struct.ScmObj*, align 8
%args48836$_37last40153$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40161, %struct.ScmObj* %args48836$_37last40153$0)
store volatile %struct.ScmObj* %args48836$_37last40153$1, %struct.ScmObj** %stackaddr$prim50513, align 8
%stackaddr$prim50514 = alloca %struct.ScmObj*, align 8
%args48836$_37last40153$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41737, %struct.ScmObj* %args48836$_37last40153$1)
store volatile %struct.ScmObj* %args48836$_37last40153$2, %struct.ScmObj** %stackaddr$prim50514, align 8
%clofunc50515 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last40153)
musttail call tailcc void %clofunc50515(%struct.ScmObj* %_37last40153, %struct.ScmObj* %args48836$_37last40153$2)
ret void
}

define tailcc void @proc_clo$ae41737(%struct.ScmObj* %env$ae41737,%struct.ScmObj* %current_45args48832) {
%stackaddr$env-ref50516 = alloca %struct.ScmObj*, align 8
%anf_45bind40291 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41737, i64 0)
store %struct.ScmObj* %anf_45bind40291, %struct.ScmObj** %stackaddr$env-ref50516
%stackaddr$env-ref50517 = alloca %struct.ScmObj*, align 8
%k40478 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41737, i64 1)
store %struct.ScmObj* %k40478, %struct.ScmObj** %stackaddr$env-ref50517
%stackaddr$prim50518 = alloca %struct.ScmObj*, align 8
%_95k40481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48832)
store volatile %struct.ScmObj* %_95k40481, %struct.ScmObj** %stackaddr$prim50518, align 8
%stackaddr$prim50519 = alloca %struct.ScmObj*, align 8
%current_45args48833 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48832)
store volatile %struct.ScmObj* %current_45args48833, %struct.ScmObj** %stackaddr$prim50519, align 8
%stackaddr$prim50520 = alloca %struct.ScmObj*, align 8
%anf_45bind40292 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48833)
store volatile %struct.ScmObj* %anf_45bind40292, %struct.ScmObj** %stackaddr$prim50520, align 8
%stackaddr$prim50521 = alloca %struct.ScmObj*, align 8
%cpsprim40482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40291, %struct.ScmObj* %anf_45bind40292)
store volatile %struct.ScmObj* %cpsprim40482, %struct.ScmObj** %stackaddr$prim50521, align 8
%ae41742 = call %struct.ScmObj* @const_init_int(i64 0)
%args48835$k40478$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50522 = alloca %struct.ScmObj*, align 8
%args48835$k40478$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40482, %struct.ScmObj* %args48835$k40478$0)
store volatile %struct.ScmObj* %args48835$k40478$1, %struct.ScmObj** %stackaddr$prim50522, align 8
%stackaddr$prim50523 = alloca %struct.ScmObj*, align 8
%args48835$k40478$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41742, %struct.ScmObj* %args48835$k40478$1)
store volatile %struct.ScmObj* %args48835$k40478$2, %struct.ScmObj** %stackaddr$prim50523, align 8
%clofunc50524 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40478)
musttail call tailcc void %clofunc50524(%struct.ScmObj* %k40478, %struct.ScmObj* %args48835$k40478$2)
ret void
}

define tailcc void @proc_clo$ae41637(%struct.ScmObj* %env$ae41637,%struct.ScmObj* %current_45args48840) {
%stackaddr$env-ref50525 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41637, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref50525
%stackaddr$prim50526 = alloca %struct.ScmObj*, align 8
%k40484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48840)
store volatile %struct.ScmObj* %k40484, %struct.ScmObj** %stackaddr$prim50526, align 8
%stackaddr$prim50527 = alloca %struct.ScmObj*, align 8
%current_45args48841 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48840)
store volatile %struct.ScmObj* %current_45args48841, %struct.ScmObj** %stackaddr$prim50527, align 8
%stackaddr$prim50528 = alloca %struct.ScmObj*, align 8
%f40164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48841)
store volatile %struct.ScmObj* %f40164, %struct.ScmObj** %stackaddr$prim50528, align 8
%stackaddr$prim50529 = alloca %struct.ScmObj*, align 8
%current_45args48842 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48841)
store volatile %struct.ScmObj* %current_45args48842, %struct.ScmObj** %stackaddr$prim50529, align 8
%stackaddr$prim50530 = alloca %struct.ScmObj*, align 8
%lst40163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48842)
store volatile %struct.ScmObj* %lst40163, %struct.ScmObj** %stackaddr$prim50530, align 8
%stackaddr$makeclosure50531 = alloca %struct.ScmObj*, align 8
%fptrToInt50532 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41638 to i64
%ae41638 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50532)
store volatile %struct.ScmObj* %ae41638, %struct.ScmObj** %stackaddr$makeclosure50531, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41638, %struct.ScmObj* %lst40163, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41638, %struct.ScmObj* %_37foldr140131, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41638, %struct.ScmObj* %k40484, i64 2)
%ae41639 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50533 = alloca %struct.ScmObj*, align 8
%fptrToInt50534 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41640 to i64
%ae41640 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50534)
store volatile %struct.ScmObj* %ae41640, %struct.ScmObj** %stackaddr$makeclosure50533, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41640, %struct.ScmObj* %f40164, i64 0)
%args48857$ae41638$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50535 = alloca %struct.ScmObj*, align 8
%args48857$ae41638$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41640, %struct.ScmObj* %args48857$ae41638$0)
store volatile %struct.ScmObj* %args48857$ae41638$1, %struct.ScmObj** %stackaddr$prim50535, align 8
%stackaddr$prim50536 = alloca %struct.ScmObj*, align 8
%args48857$ae41638$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41639, %struct.ScmObj* %args48857$ae41638$1)
store volatile %struct.ScmObj* %args48857$ae41638$2, %struct.ScmObj** %stackaddr$prim50536, align 8
%clofunc50537 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41638)
musttail call tailcc void %clofunc50537(%struct.ScmObj* %ae41638, %struct.ScmObj* %args48857$ae41638$2)
ret void
}

define tailcc void @proc_clo$ae41638(%struct.ScmObj* %env$ae41638,%struct.ScmObj* %current_45args48844) {
%stackaddr$env-ref50538 = alloca %struct.ScmObj*, align 8
%lst40163 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41638, i64 0)
store %struct.ScmObj* %lst40163, %struct.ScmObj** %stackaddr$env-ref50538
%stackaddr$env-ref50539 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41638, i64 1)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref50539
%stackaddr$env-ref50540 = alloca %struct.ScmObj*, align 8
%k40484 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41638, i64 2)
store %struct.ScmObj* %k40484, %struct.ScmObj** %stackaddr$env-ref50540
%stackaddr$prim50541 = alloca %struct.ScmObj*, align 8
%_95k40485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48844)
store volatile %struct.ScmObj* %_95k40485, %struct.ScmObj** %stackaddr$prim50541, align 8
%stackaddr$prim50542 = alloca %struct.ScmObj*, align 8
%current_45args48845 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48844)
store volatile %struct.ScmObj* %current_45args48845, %struct.ScmObj** %stackaddr$prim50542, align 8
%stackaddr$prim50543 = alloca %struct.ScmObj*, align 8
%anf_45bind40289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48845)
store volatile %struct.ScmObj* %anf_45bind40289, %struct.ScmObj** %stackaddr$prim50543, align 8
%ae41672 = call %struct.ScmObj* @const_init_null()
%args48847$_37foldr140131$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50544 = alloca %struct.ScmObj*, align 8
%args48847$_37foldr140131$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40163, %struct.ScmObj* %args48847$_37foldr140131$0)
store volatile %struct.ScmObj* %args48847$_37foldr140131$1, %struct.ScmObj** %stackaddr$prim50544, align 8
%stackaddr$prim50545 = alloca %struct.ScmObj*, align 8
%args48847$_37foldr140131$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41672, %struct.ScmObj* %args48847$_37foldr140131$1)
store volatile %struct.ScmObj* %args48847$_37foldr140131$2, %struct.ScmObj** %stackaddr$prim50545, align 8
%stackaddr$prim50546 = alloca %struct.ScmObj*, align 8
%args48847$_37foldr140131$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40289, %struct.ScmObj* %args48847$_37foldr140131$2)
store volatile %struct.ScmObj* %args48847$_37foldr140131$3, %struct.ScmObj** %stackaddr$prim50546, align 8
%stackaddr$prim50547 = alloca %struct.ScmObj*, align 8
%args48847$_37foldr140131$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40484, %struct.ScmObj* %args48847$_37foldr140131$3)
store volatile %struct.ScmObj* %args48847$_37foldr140131$4, %struct.ScmObj** %stackaddr$prim50547, align 8
%clofunc50548 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140131)
musttail call tailcc void %clofunc50548(%struct.ScmObj* %_37foldr140131, %struct.ScmObj* %args48847$_37foldr140131$4)
ret void
}

define tailcc void @proc_clo$ae41640(%struct.ScmObj* %env$ae41640,%struct.ScmObj* %current_45args48848) {
%stackaddr$env-ref50549 = alloca %struct.ScmObj*, align 8
%f40164 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41640, i64 0)
store %struct.ScmObj* %f40164, %struct.ScmObj** %stackaddr$env-ref50549
%stackaddr$prim50550 = alloca %struct.ScmObj*, align 8
%k40486 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48848)
store volatile %struct.ScmObj* %k40486, %struct.ScmObj** %stackaddr$prim50550, align 8
%stackaddr$prim50551 = alloca %struct.ScmObj*, align 8
%current_45args48849 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48848)
store volatile %struct.ScmObj* %current_45args48849, %struct.ScmObj** %stackaddr$prim50551, align 8
%stackaddr$prim50552 = alloca %struct.ScmObj*, align 8
%v40166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48849)
store volatile %struct.ScmObj* %v40166, %struct.ScmObj** %stackaddr$prim50552, align 8
%stackaddr$prim50553 = alloca %struct.ScmObj*, align 8
%current_45args48850 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48849)
store volatile %struct.ScmObj* %current_45args48850, %struct.ScmObj** %stackaddr$prim50553, align 8
%stackaddr$prim50554 = alloca %struct.ScmObj*, align 8
%r40165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48850)
store volatile %struct.ScmObj* %r40165, %struct.ScmObj** %stackaddr$prim50554, align 8
%stackaddr$makeclosure50555 = alloca %struct.ScmObj*, align 8
%fptrToInt50556 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41642 to i64
%ae41642 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50556)
store volatile %struct.ScmObj* %ae41642, %struct.ScmObj** %stackaddr$makeclosure50555, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41642, %struct.ScmObj* %k40486, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41642, %struct.ScmObj* %r40165, i64 1)
%args48856$f40164$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50557 = alloca %struct.ScmObj*, align 8
%args48856$f40164$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v40166, %struct.ScmObj* %args48856$f40164$0)
store volatile %struct.ScmObj* %args48856$f40164$1, %struct.ScmObj** %stackaddr$prim50557, align 8
%stackaddr$prim50558 = alloca %struct.ScmObj*, align 8
%args48856$f40164$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41642, %struct.ScmObj* %args48856$f40164$1)
store volatile %struct.ScmObj* %args48856$f40164$2, %struct.ScmObj** %stackaddr$prim50558, align 8
%clofunc50559 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40164)
musttail call tailcc void %clofunc50559(%struct.ScmObj* %f40164, %struct.ScmObj* %args48856$f40164$2)
ret void
}

define tailcc void @proc_clo$ae41642(%struct.ScmObj* %env$ae41642,%struct.ScmObj* %current_45args48852) {
%stackaddr$env-ref50560 = alloca %struct.ScmObj*, align 8
%k40486 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41642, i64 0)
store %struct.ScmObj* %k40486, %struct.ScmObj** %stackaddr$env-ref50560
%stackaddr$env-ref50561 = alloca %struct.ScmObj*, align 8
%r40165 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41642, i64 1)
store %struct.ScmObj* %r40165, %struct.ScmObj** %stackaddr$env-ref50561
%stackaddr$prim50562 = alloca %struct.ScmObj*, align 8
%_95k40487 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48852)
store volatile %struct.ScmObj* %_95k40487, %struct.ScmObj** %stackaddr$prim50562, align 8
%stackaddr$prim50563 = alloca %struct.ScmObj*, align 8
%current_45args48853 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48852)
store volatile %struct.ScmObj* %current_45args48853, %struct.ScmObj** %stackaddr$prim50563, align 8
%stackaddr$prim50564 = alloca %struct.ScmObj*, align 8
%anf_45bind40288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48853)
store volatile %struct.ScmObj* %anf_45bind40288, %struct.ScmObj** %stackaddr$prim50564, align 8
%stackaddr$prim50565 = alloca %struct.ScmObj*, align 8
%cpsprim40488 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40288, %struct.ScmObj* %r40165)
store volatile %struct.ScmObj* %cpsprim40488, %struct.ScmObj** %stackaddr$prim50565, align 8
%ae41647 = call %struct.ScmObj* @const_init_int(i64 0)
%args48855$k40486$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50566 = alloca %struct.ScmObj*, align 8
%args48855$k40486$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40488, %struct.ScmObj* %args48855$k40486$0)
store volatile %struct.ScmObj* %args48855$k40486$1, %struct.ScmObj** %stackaddr$prim50566, align 8
%stackaddr$prim50567 = alloca %struct.ScmObj*, align 8
%args48855$k40486$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41647, %struct.ScmObj* %args48855$k40486$1)
store volatile %struct.ScmObj* %args48855$k40486$2, %struct.ScmObj** %stackaddr$prim50567, align 8
%clofunc50568 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40486)
musttail call tailcc void %clofunc50568(%struct.ScmObj* %k40486, %struct.ScmObj* %args48855$k40486$2)
ret void
}

define tailcc void @proc_clo$ae41251(%struct.ScmObj* %env$ae41251,%struct.ScmObj* %current_45args48860) {
%stackaddr$env-ref50569 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41251, i64 0)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref50569
%stackaddr$env-ref50570 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41251, i64 1)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref50570
%stackaddr$prim50571 = alloca %struct.ScmObj*, align 8
%k40489 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48860)
store volatile %struct.ScmObj* %k40489, %struct.ScmObj** %stackaddr$prim50571, align 8
%stackaddr$prim50572 = alloca %struct.ScmObj*, align 8
%current_45args48861 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48860)
store volatile %struct.ScmObj* %current_45args48861, %struct.ScmObj** %stackaddr$prim50572, align 8
%stackaddr$prim50573 = alloca %struct.ScmObj*, align 8
%_37foldr40137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48861)
store volatile %struct.ScmObj* %_37foldr40137, %struct.ScmObj** %stackaddr$prim50573, align 8
%ae41253 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50574 = alloca %struct.ScmObj*, align 8
%fptrToInt50575 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41254 to i64
%ae41254 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50575)
store volatile %struct.ScmObj* %ae41254, %struct.ScmObj** %stackaddr$makeclosure50574, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41254, %struct.ScmObj* %_37foldr40137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41254, %struct.ScmObj* %_37foldr140131, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41254, %struct.ScmObj* %_37map140127, i64 2)
%args48918$k40489$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50576 = alloca %struct.ScmObj*, align 8
%args48918$k40489$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41254, %struct.ScmObj* %args48918$k40489$0)
store volatile %struct.ScmObj* %args48918$k40489$1, %struct.ScmObj** %stackaddr$prim50576, align 8
%stackaddr$prim50577 = alloca %struct.ScmObj*, align 8
%args48918$k40489$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41253, %struct.ScmObj* %args48918$k40489$1)
store volatile %struct.ScmObj* %args48918$k40489$2, %struct.ScmObj** %stackaddr$prim50577, align 8
%clofunc50578 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40489)
musttail call tailcc void %clofunc50578(%struct.ScmObj* %k40489, %struct.ScmObj* %args48918$k40489$2)
ret void
}

define tailcc void @proc_clo$ae41254(%struct.ScmObj* %env$ae41254,%struct.ScmObj* %args4013840490) {
%stackaddr$env-ref50579 = alloca %struct.ScmObj*, align 8
%_37foldr40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41254, i64 0)
store %struct.ScmObj* %_37foldr40137, %struct.ScmObj** %stackaddr$env-ref50579
%stackaddr$env-ref50580 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41254, i64 1)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref50580
%stackaddr$env-ref50581 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41254, i64 2)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref50581
%stackaddr$prim50582 = alloca %struct.ScmObj*, align 8
%k40491 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4013840490)
store volatile %struct.ScmObj* %k40491, %struct.ScmObj** %stackaddr$prim50582, align 8
%stackaddr$prim50583 = alloca %struct.ScmObj*, align 8
%args40138 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4013840490)
store volatile %struct.ScmObj* %args40138, %struct.ScmObj** %stackaddr$prim50583, align 8
%stackaddr$prim50584 = alloca %struct.ScmObj*, align 8
%f40141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40138)
store volatile %struct.ScmObj* %f40141, %struct.ScmObj** %stackaddr$prim50584, align 8
%stackaddr$prim50585 = alloca %struct.ScmObj*, align 8
%anf_45bind40275 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40138)
store volatile %struct.ScmObj* %anf_45bind40275, %struct.ScmObj** %stackaddr$prim50585, align 8
%stackaddr$prim50586 = alloca %struct.ScmObj*, align 8
%acc40140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40275)
store volatile %struct.ScmObj* %acc40140, %struct.ScmObj** %stackaddr$prim50586, align 8
%stackaddr$prim50587 = alloca %struct.ScmObj*, align 8
%anf_45bind40276 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40138)
store volatile %struct.ScmObj* %anf_45bind40276, %struct.ScmObj** %stackaddr$prim50587, align 8
%stackaddr$prim50588 = alloca %struct.ScmObj*, align 8
%lsts40139 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40276)
store volatile %struct.ScmObj* %lsts40139, %struct.ScmObj** %stackaddr$prim50588, align 8
%stackaddr$makeclosure50589 = alloca %struct.ScmObj*, align 8
%fptrToInt50590 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41262 to i64
%ae41262 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50590)
store volatile %struct.ScmObj* %ae41262, %struct.ScmObj** %stackaddr$makeclosure50589, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41262, %struct.ScmObj* %k40491, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41262, %struct.ScmObj* %lsts40139, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41262, %struct.ScmObj* %_37foldr40137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41262, %struct.ScmObj* %_37foldr140131, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41262, %struct.ScmObj* %_37map140127, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41262, %struct.ScmObj* %f40141, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41262, %struct.ScmObj* %acc40140, i64 6)
%ae41263 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50591 = alloca %struct.ScmObj*, align 8
%fptrToInt50592 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41264 to i64
%ae41264 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50592)
store volatile %struct.ScmObj* %ae41264, %struct.ScmObj** %stackaddr$makeclosure50591, align 8
%args48917$ae41262$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50593 = alloca %struct.ScmObj*, align 8
%args48917$ae41262$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41264, %struct.ScmObj* %args48917$ae41262$0)
store volatile %struct.ScmObj* %args48917$ae41262$1, %struct.ScmObj** %stackaddr$prim50593, align 8
%stackaddr$prim50594 = alloca %struct.ScmObj*, align 8
%args48917$ae41262$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41263, %struct.ScmObj* %args48917$ae41262$1)
store volatile %struct.ScmObj* %args48917$ae41262$2, %struct.ScmObj** %stackaddr$prim50594, align 8
%clofunc50595 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41262)
musttail call tailcc void %clofunc50595(%struct.ScmObj* %ae41262, %struct.ScmObj* %args48917$ae41262$2)
ret void
}

define tailcc void @proc_clo$ae41262(%struct.ScmObj* %env$ae41262,%struct.ScmObj* %current_45args48863) {
%stackaddr$env-ref50596 = alloca %struct.ScmObj*, align 8
%k40491 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41262, i64 0)
store %struct.ScmObj* %k40491, %struct.ScmObj** %stackaddr$env-ref50596
%stackaddr$env-ref50597 = alloca %struct.ScmObj*, align 8
%lsts40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41262, i64 1)
store %struct.ScmObj* %lsts40139, %struct.ScmObj** %stackaddr$env-ref50597
%stackaddr$env-ref50598 = alloca %struct.ScmObj*, align 8
%_37foldr40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41262, i64 2)
store %struct.ScmObj* %_37foldr40137, %struct.ScmObj** %stackaddr$env-ref50598
%stackaddr$env-ref50599 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41262, i64 3)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref50599
%stackaddr$env-ref50600 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41262, i64 4)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref50600
%stackaddr$env-ref50601 = alloca %struct.ScmObj*, align 8
%f40141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41262, i64 5)
store %struct.ScmObj* %f40141, %struct.ScmObj** %stackaddr$env-ref50601
%stackaddr$env-ref50602 = alloca %struct.ScmObj*, align 8
%acc40140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41262, i64 6)
store %struct.ScmObj* %acc40140, %struct.ScmObj** %stackaddr$env-ref50602
%stackaddr$prim50603 = alloca %struct.ScmObj*, align 8
%_95k40492 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48863)
store volatile %struct.ScmObj* %_95k40492, %struct.ScmObj** %stackaddr$prim50603, align 8
%stackaddr$prim50604 = alloca %struct.ScmObj*, align 8
%current_45args48864 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48863)
store volatile %struct.ScmObj* %current_45args48864, %struct.ScmObj** %stackaddr$prim50604, align 8
%stackaddr$prim50605 = alloca %struct.ScmObj*, align 8
%anf_45bind40277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48864)
store volatile %struct.ScmObj* %anf_45bind40277, %struct.ScmObj** %stackaddr$prim50605, align 8
%stackaddr$makeclosure50606 = alloca %struct.ScmObj*, align 8
%fptrToInt50607 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41294 to i64
%ae41294 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50607)
store volatile %struct.ScmObj* %ae41294, %struct.ScmObj** %stackaddr$makeclosure50606, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41294, %struct.ScmObj* %k40491, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41294, %struct.ScmObj* %lsts40139, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41294, %struct.ScmObj* %_37foldr40137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41294, %struct.ScmObj* %_37foldr140131, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41294, %struct.ScmObj* %_37map140127, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41294, %struct.ScmObj* %f40141, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41294, %struct.ScmObj* %acc40140, i64 6)
%ae41296 = call %struct.ScmObj* @const_init_false()
%args48910$_37foldr140131$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50608 = alloca %struct.ScmObj*, align 8
%args48910$_37foldr140131$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40139, %struct.ScmObj* %args48910$_37foldr140131$0)
store volatile %struct.ScmObj* %args48910$_37foldr140131$1, %struct.ScmObj** %stackaddr$prim50608, align 8
%stackaddr$prim50609 = alloca %struct.ScmObj*, align 8
%args48910$_37foldr140131$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41296, %struct.ScmObj* %args48910$_37foldr140131$1)
store volatile %struct.ScmObj* %args48910$_37foldr140131$2, %struct.ScmObj** %stackaddr$prim50609, align 8
%stackaddr$prim50610 = alloca %struct.ScmObj*, align 8
%args48910$_37foldr140131$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40277, %struct.ScmObj* %args48910$_37foldr140131$2)
store volatile %struct.ScmObj* %args48910$_37foldr140131$3, %struct.ScmObj** %stackaddr$prim50610, align 8
%stackaddr$prim50611 = alloca %struct.ScmObj*, align 8
%args48910$_37foldr140131$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41294, %struct.ScmObj* %args48910$_37foldr140131$3)
store volatile %struct.ScmObj* %args48910$_37foldr140131$4, %struct.ScmObj** %stackaddr$prim50611, align 8
%clofunc50612 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140131)
musttail call tailcc void %clofunc50612(%struct.ScmObj* %_37foldr140131, %struct.ScmObj* %args48910$_37foldr140131$4)
ret void
}

define tailcc void @proc_clo$ae41294(%struct.ScmObj* %env$ae41294,%struct.ScmObj* %current_45args48866) {
%stackaddr$env-ref50613 = alloca %struct.ScmObj*, align 8
%k40491 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41294, i64 0)
store %struct.ScmObj* %k40491, %struct.ScmObj** %stackaddr$env-ref50613
%stackaddr$env-ref50614 = alloca %struct.ScmObj*, align 8
%lsts40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41294, i64 1)
store %struct.ScmObj* %lsts40139, %struct.ScmObj** %stackaddr$env-ref50614
%stackaddr$env-ref50615 = alloca %struct.ScmObj*, align 8
%_37foldr40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41294, i64 2)
store %struct.ScmObj* %_37foldr40137, %struct.ScmObj** %stackaddr$env-ref50615
%stackaddr$env-ref50616 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41294, i64 3)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref50616
%stackaddr$env-ref50617 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41294, i64 4)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref50617
%stackaddr$env-ref50618 = alloca %struct.ScmObj*, align 8
%f40141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41294, i64 5)
store %struct.ScmObj* %f40141, %struct.ScmObj** %stackaddr$env-ref50618
%stackaddr$env-ref50619 = alloca %struct.ScmObj*, align 8
%acc40140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41294, i64 6)
store %struct.ScmObj* %acc40140, %struct.ScmObj** %stackaddr$env-ref50619
%stackaddr$prim50620 = alloca %struct.ScmObj*, align 8
%_95k40493 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48866)
store volatile %struct.ScmObj* %_95k40493, %struct.ScmObj** %stackaddr$prim50620, align 8
%stackaddr$prim50621 = alloca %struct.ScmObj*, align 8
%current_45args48867 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48866)
store volatile %struct.ScmObj* %current_45args48867, %struct.ScmObj** %stackaddr$prim50621, align 8
%stackaddr$prim50622 = alloca %struct.ScmObj*, align 8
%anf_45bind40278 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48867)
store volatile %struct.ScmObj* %anf_45bind40278, %struct.ScmObj** %stackaddr$prim50622, align 8
%truthy$cmp50623 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40278)
%cmp$cmp50623 = icmp eq i64 %truthy$cmp50623, 1
br i1 %cmp$cmp50623, label %truebranch$cmp50623, label %falsebranch$cmp50623
truebranch$cmp50623:
%ae41305 = call %struct.ScmObj* @const_init_int(i64 0)
%args48869$k40491$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50624 = alloca %struct.ScmObj*, align 8
%args48869$k40491$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40140, %struct.ScmObj* %args48869$k40491$0)
store volatile %struct.ScmObj* %args48869$k40491$1, %struct.ScmObj** %stackaddr$prim50624, align 8
%stackaddr$prim50625 = alloca %struct.ScmObj*, align 8
%args48869$k40491$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41305, %struct.ScmObj* %args48869$k40491$1)
store volatile %struct.ScmObj* %args48869$k40491$2, %struct.ScmObj** %stackaddr$prim50625, align 8
%clofunc50626 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40491)
musttail call tailcc void %clofunc50626(%struct.ScmObj* %k40491, %struct.ScmObj* %args48869$k40491$2)
ret void
falsebranch$cmp50623:
%stackaddr$makeclosure50627 = alloca %struct.ScmObj*, align 8
%fptrToInt50628 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41310 to i64
%ae41310 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50628)
store volatile %struct.ScmObj* %ae41310, %struct.ScmObj** %stackaddr$makeclosure50627, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41310, %struct.ScmObj* %k40491, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41310, %struct.ScmObj* %lsts40139, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41310, %struct.ScmObj* %_37foldr40137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41310, %struct.ScmObj* %_37foldr140131, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41310, %struct.ScmObj* %_37map140127, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41310, %struct.ScmObj* %f40141, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41310, %struct.ScmObj* %acc40140, i64 6)
%ae41311 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50629 = alloca %struct.ScmObj*, align 8
%fptrToInt50630 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41312 to i64
%ae41312 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50630)
store volatile %struct.ScmObj* %ae41312, %struct.ScmObj** %stackaddr$makeclosure50629, align 8
%args48909$ae41310$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50631 = alloca %struct.ScmObj*, align 8
%args48909$ae41310$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41312, %struct.ScmObj* %args48909$ae41310$0)
store volatile %struct.ScmObj* %args48909$ae41310$1, %struct.ScmObj** %stackaddr$prim50631, align 8
%stackaddr$prim50632 = alloca %struct.ScmObj*, align 8
%args48909$ae41310$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41311, %struct.ScmObj* %args48909$ae41310$1)
store volatile %struct.ScmObj* %args48909$ae41310$2, %struct.ScmObj** %stackaddr$prim50632, align 8
%clofunc50633 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41310)
musttail call tailcc void %clofunc50633(%struct.ScmObj* %ae41310, %struct.ScmObj* %args48909$ae41310$2)
ret void
}

define tailcc void @proc_clo$ae41310(%struct.ScmObj* %env$ae41310,%struct.ScmObj* %current_45args48870) {
%stackaddr$env-ref50634 = alloca %struct.ScmObj*, align 8
%k40491 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41310, i64 0)
store %struct.ScmObj* %k40491, %struct.ScmObj** %stackaddr$env-ref50634
%stackaddr$env-ref50635 = alloca %struct.ScmObj*, align 8
%lsts40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41310, i64 1)
store %struct.ScmObj* %lsts40139, %struct.ScmObj** %stackaddr$env-ref50635
%stackaddr$env-ref50636 = alloca %struct.ScmObj*, align 8
%_37foldr40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41310, i64 2)
store %struct.ScmObj* %_37foldr40137, %struct.ScmObj** %stackaddr$env-ref50636
%stackaddr$env-ref50637 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41310, i64 3)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref50637
%stackaddr$env-ref50638 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41310, i64 4)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref50638
%stackaddr$env-ref50639 = alloca %struct.ScmObj*, align 8
%f40141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41310, i64 5)
store %struct.ScmObj* %f40141, %struct.ScmObj** %stackaddr$env-ref50639
%stackaddr$env-ref50640 = alloca %struct.ScmObj*, align 8
%acc40140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41310, i64 6)
store %struct.ScmObj* %acc40140, %struct.ScmObj** %stackaddr$env-ref50640
%stackaddr$prim50641 = alloca %struct.ScmObj*, align 8
%_95k40494 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48870)
store volatile %struct.ScmObj* %_95k40494, %struct.ScmObj** %stackaddr$prim50641, align 8
%stackaddr$prim50642 = alloca %struct.ScmObj*, align 8
%current_45args48871 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48870)
store volatile %struct.ScmObj* %current_45args48871, %struct.ScmObj** %stackaddr$prim50642, align 8
%stackaddr$prim50643 = alloca %struct.ScmObj*, align 8
%anf_45bind40279 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48871)
store volatile %struct.ScmObj* %anf_45bind40279, %struct.ScmObj** %stackaddr$prim50643, align 8
%stackaddr$makeclosure50644 = alloca %struct.ScmObj*, align 8
%fptrToInt50645 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41331 to i64
%ae41331 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50645)
store volatile %struct.ScmObj* %ae41331, %struct.ScmObj** %stackaddr$makeclosure50644, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41331, %struct.ScmObj* %k40491, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41331, %struct.ScmObj* %lsts40139, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41331, %struct.ScmObj* %_37foldr40137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41331, %struct.ScmObj* %_37foldr140131, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41331, %struct.ScmObj* %_37map140127, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41331, %struct.ScmObj* %f40141, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41331, %struct.ScmObj* %acc40140, i64 6)
%args48904$_37map140127$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50646 = alloca %struct.ScmObj*, align 8
%args48904$_37map140127$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40139, %struct.ScmObj* %args48904$_37map140127$0)
store volatile %struct.ScmObj* %args48904$_37map140127$1, %struct.ScmObj** %stackaddr$prim50646, align 8
%stackaddr$prim50647 = alloca %struct.ScmObj*, align 8
%args48904$_37map140127$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40279, %struct.ScmObj* %args48904$_37map140127$1)
store volatile %struct.ScmObj* %args48904$_37map140127$2, %struct.ScmObj** %stackaddr$prim50647, align 8
%stackaddr$prim50648 = alloca %struct.ScmObj*, align 8
%args48904$_37map140127$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41331, %struct.ScmObj* %args48904$_37map140127$2)
store volatile %struct.ScmObj* %args48904$_37map140127$3, %struct.ScmObj** %stackaddr$prim50648, align 8
%clofunc50649 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140127)
musttail call tailcc void %clofunc50649(%struct.ScmObj* %_37map140127, %struct.ScmObj* %args48904$_37map140127$3)
ret void
}

define tailcc void @proc_clo$ae41331(%struct.ScmObj* %env$ae41331,%struct.ScmObj* %current_45args48873) {
%stackaddr$env-ref50650 = alloca %struct.ScmObj*, align 8
%k40491 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41331, i64 0)
store %struct.ScmObj* %k40491, %struct.ScmObj** %stackaddr$env-ref50650
%stackaddr$env-ref50651 = alloca %struct.ScmObj*, align 8
%lsts40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41331, i64 1)
store %struct.ScmObj* %lsts40139, %struct.ScmObj** %stackaddr$env-ref50651
%stackaddr$env-ref50652 = alloca %struct.ScmObj*, align 8
%_37foldr40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41331, i64 2)
store %struct.ScmObj* %_37foldr40137, %struct.ScmObj** %stackaddr$env-ref50652
%stackaddr$env-ref50653 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41331, i64 3)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref50653
%stackaddr$env-ref50654 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41331, i64 4)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref50654
%stackaddr$env-ref50655 = alloca %struct.ScmObj*, align 8
%f40141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41331, i64 5)
store %struct.ScmObj* %f40141, %struct.ScmObj** %stackaddr$env-ref50655
%stackaddr$env-ref50656 = alloca %struct.ScmObj*, align 8
%acc40140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41331, i64 6)
store %struct.ScmObj* %acc40140, %struct.ScmObj** %stackaddr$env-ref50656
%stackaddr$prim50657 = alloca %struct.ScmObj*, align 8
%_95k40495 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48873)
store volatile %struct.ScmObj* %_95k40495, %struct.ScmObj** %stackaddr$prim50657, align 8
%stackaddr$prim50658 = alloca %struct.ScmObj*, align 8
%current_45args48874 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48873)
store volatile %struct.ScmObj* %current_45args48874, %struct.ScmObj** %stackaddr$prim50658, align 8
%stackaddr$prim50659 = alloca %struct.ScmObj*, align 8
%lsts_4340146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48874)
store volatile %struct.ScmObj* %lsts_4340146, %struct.ScmObj** %stackaddr$prim50659, align 8
%stackaddr$makeclosure50660 = alloca %struct.ScmObj*, align 8
%fptrToInt50661 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41334 to i64
%ae41334 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt50661)
store volatile %struct.ScmObj* %ae41334, %struct.ScmObj** %stackaddr$makeclosure50660, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41334, %struct.ScmObj* %k40491, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41334, %struct.ScmObj* %lsts40139, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41334, %struct.ScmObj* %_37foldr40137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41334, %struct.ScmObj* %_37foldr140131, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41334, %struct.ScmObj* %lsts_4340146, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41334, %struct.ScmObj* %_37map140127, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41334, %struct.ScmObj* %f40141, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41334, %struct.ScmObj* %acc40140, i64 7)
%ae41335 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50662 = alloca %struct.ScmObj*, align 8
%fptrToInt50663 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41336 to i64
%ae41336 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50663)
store volatile %struct.ScmObj* %ae41336, %struct.ScmObj** %stackaddr$makeclosure50662, align 8
%args48903$ae41334$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50664 = alloca %struct.ScmObj*, align 8
%args48903$ae41334$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41336, %struct.ScmObj* %args48903$ae41334$0)
store volatile %struct.ScmObj* %args48903$ae41334$1, %struct.ScmObj** %stackaddr$prim50664, align 8
%stackaddr$prim50665 = alloca %struct.ScmObj*, align 8
%args48903$ae41334$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41335, %struct.ScmObj* %args48903$ae41334$1)
store volatile %struct.ScmObj* %args48903$ae41334$2, %struct.ScmObj** %stackaddr$prim50665, align 8
%clofunc50666 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41334)
musttail call tailcc void %clofunc50666(%struct.ScmObj* %ae41334, %struct.ScmObj* %args48903$ae41334$2)
ret void
}

define tailcc void @proc_clo$ae41334(%struct.ScmObj* %env$ae41334,%struct.ScmObj* %current_45args48876) {
%stackaddr$env-ref50667 = alloca %struct.ScmObj*, align 8
%k40491 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41334, i64 0)
store %struct.ScmObj* %k40491, %struct.ScmObj** %stackaddr$env-ref50667
%stackaddr$env-ref50668 = alloca %struct.ScmObj*, align 8
%lsts40139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41334, i64 1)
store %struct.ScmObj* %lsts40139, %struct.ScmObj** %stackaddr$env-ref50668
%stackaddr$env-ref50669 = alloca %struct.ScmObj*, align 8
%_37foldr40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41334, i64 2)
store %struct.ScmObj* %_37foldr40137, %struct.ScmObj** %stackaddr$env-ref50669
%stackaddr$env-ref50670 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41334, i64 3)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref50670
%stackaddr$env-ref50671 = alloca %struct.ScmObj*, align 8
%lsts_4340146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41334, i64 4)
store %struct.ScmObj* %lsts_4340146, %struct.ScmObj** %stackaddr$env-ref50671
%stackaddr$env-ref50672 = alloca %struct.ScmObj*, align 8
%_37map140127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41334, i64 5)
store %struct.ScmObj* %_37map140127, %struct.ScmObj** %stackaddr$env-ref50672
%stackaddr$env-ref50673 = alloca %struct.ScmObj*, align 8
%f40141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41334, i64 6)
store %struct.ScmObj* %f40141, %struct.ScmObj** %stackaddr$env-ref50673
%stackaddr$env-ref50674 = alloca %struct.ScmObj*, align 8
%acc40140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41334, i64 7)
store %struct.ScmObj* %acc40140, %struct.ScmObj** %stackaddr$env-ref50674
%stackaddr$prim50675 = alloca %struct.ScmObj*, align 8
%_95k40496 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48876)
store volatile %struct.ScmObj* %_95k40496, %struct.ScmObj** %stackaddr$prim50675, align 8
%stackaddr$prim50676 = alloca %struct.ScmObj*, align 8
%current_45args48877 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48876)
store volatile %struct.ScmObj* %current_45args48877, %struct.ScmObj** %stackaddr$prim50676, align 8
%stackaddr$prim50677 = alloca %struct.ScmObj*, align 8
%anf_45bind40280 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48877)
store volatile %struct.ScmObj* %anf_45bind40280, %struct.ScmObj** %stackaddr$prim50677, align 8
%stackaddr$makeclosure50678 = alloca %struct.ScmObj*, align 8
%fptrToInt50679 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41355 to i64
%ae41355 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt50679)
store volatile %struct.ScmObj* %ae41355, %struct.ScmObj** %stackaddr$makeclosure50678, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41355, %struct.ScmObj* %_37foldr40137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41355, %struct.ScmObj* %_37foldr140131, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41355, %struct.ScmObj* %lsts_4340146, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41355, %struct.ScmObj* %f40141, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41355, %struct.ScmObj* %acc40140, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41355, %struct.ScmObj* %k40491, i64 5)
%args48898$_37map140127$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50680 = alloca %struct.ScmObj*, align 8
%args48898$_37map140127$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40139, %struct.ScmObj* %args48898$_37map140127$0)
store volatile %struct.ScmObj* %args48898$_37map140127$1, %struct.ScmObj** %stackaddr$prim50680, align 8
%stackaddr$prim50681 = alloca %struct.ScmObj*, align 8
%args48898$_37map140127$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40280, %struct.ScmObj* %args48898$_37map140127$1)
store volatile %struct.ScmObj* %args48898$_37map140127$2, %struct.ScmObj** %stackaddr$prim50681, align 8
%stackaddr$prim50682 = alloca %struct.ScmObj*, align 8
%args48898$_37map140127$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41355, %struct.ScmObj* %args48898$_37map140127$2)
store volatile %struct.ScmObj* %args48898$_37map140127$3, %struct.ScmObj** %stackaddr$prim50682, align 8
%clofunc50683 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140127)
musttail call tailcc void %clofunc50683(%struct.ScmObj* %_37map140127, %struct.ScmObj* %args48898$_37map140127$3)
ret void
}

define tailcc void @proc_clo$ae41355(%struct.ScmObj* %env$ae41355,%struct.ScmObj* %current_45args48879) {
%stackaddr$env-ref50684 = alloca %struct.ScmObj*, align 8
%_37foldr40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41355, i64 0)
store %struct.ScmObj* %_37foldr40137, %struct.ScmObj** %stackaddr$env-ref50684
%stackaddr$env-ref50685 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41355, i64 1)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref50685
%stackaddr$env-ref50686 = alloca %struct.ScmObj*, align 8
%lsts_4340146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41355, i64 2)
store %struct.ScmObj* %lsts_4340146, %struct.ScmObj** %stackaddr$env-ref50686
%stackaddr$env-ref50687 = alloca %struct.ScmObj*, align 8
%f40141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41355, i64 3)
store %struct.ScmObj* %f40141, %struct.ScmObj** %stackaddr$env-ref50687
%stackaddr$env-ref50688 = alloca %struct.ScmObj*, align 8
%acc40140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41355, i64 4)
store %struct.ScmObj* %acc40140, %struct.ScmObj** %stackaddr$env-ref50688
%stackaddr$env-ref50689 = alloca %struct.ScmObj*, align 8
%k40491 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41355, i64 5)
store %struct.ScmObj* %k40491, %struct.ScmObj** %stackaddr$env-ref50689
%stackaddr$prim50690 = alloca %struct.ScmObj*, align 8
%_95k40497 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48879)
store volatile %struct.ScmObj* %_95k40497, %struct.ScmObj** %stackaddr$prim50690, align 8
%stackaddr$prim50691 = alloca %struct.ScmObj*, align 8
%current_45args48880 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48879)
store volatile %struct.ScmObj* %current_45args48880, %struct.ScmObj** %stackaddr$prim50691, align 8
%stackaddr$prim50692 = alloca %struct.ScmObj*, align 8
%vs40144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48880)
store volatile %struct.ScmObj* %vs40144, %struct.ScmObj** %stackaddr$prim50692, align 8
%stackaddr$makeclosure50693 = alloca %struct.ScmObj*, align 8
%fptrToInt50694 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41358 to i64
%ae41358 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt50694)
store volatile %struct.ScmObj* %ae41358, %struct.ScmObj** %stackaddr$makeclosure50693, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41358, %struct.ScmObj* %_37foldr40137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41358, %struct.ScmObj* %_37foldr140131, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41358, %struct.ScmObj* %lsts_4340146, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41358, %struct.ScmObj* %vs40144, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41358, %struct.ScmObj* %f40141, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41358, %struct.ScmObj* %acc40140, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41358, %struct.ScmObj* %k40491, i64 6)
%ae41359 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50695 = alloca %struct.ScmObj*, align 8
%fptrToInt50696 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41360 to i64
%ae41360 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50696)
store volatile %struct.ScmObj* %ae41360, %struct.ScmObj** %stackaddr$makeclosure50695, align 8
%args48897$ae41358$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50697 = alloca %struct.ScmObj*, align 8
%args48897$ae41358$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41360, %struct.ScmObj* %args48897$ae41358$0)
store volatile %struct.ScmObj* %args48897$ae41358$1, %struct.ScmObj** %stackaddr$prim50697, align 8
%stackaddr$prim50698 = alloca %struct.ScmObj*, align 8
%args48897$ae41358$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41359, %struct.ScmObj* %args48897$ae41358$1)
store volatile %struct.ScmObj* %args48897$ae41358$2, %struct.ScmObj** %stackaddr$prim50698, align 8
%clofunc50699 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41358)
musttail call tailcc void %clofunc50699(%struct.ScmObj* %ae41358, %struct.ScmObj* %args48897$ae41358$2)
ret void
}

define tailcc void @proc_clo$ae41358(%struct.ScmObj* %env$ae41358,%struct.ScmObj* %current_45args48882) {
%stackaddr$env-ref50700 = alloca %struct.ScmObj*, align 8
%_37foldr40137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41358, i64 0)
store %struct.ScmObj* %_37foldr40137, %struct.ScmObj** %stackaddr$env-ref50700
%stackaddr$env-ref50701 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41358, i64 1)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref50701
%stackaddr$env-ref50702 = alloca %struct.ScmObj*, align 8
%lsts_4340146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41358, i64 2)
store %struct.ScmObj* %lsts_4340146, %struct.ScmObj** %stackaddr$env-ref50702
%stackaddr$env-ref50703 = alloca %struct.ScmObj*, align 8
%vs40144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41358, i64 3)
store %struct.ScmObj* %vs40144, %struct.ScmObj** %stackaddr$env-ref50703
%stackaddr$env-ref50704 = alloca %struct.ScmObj*, align 8
%f40141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41358, i64 4)
store %struct.ScmObj* %f40141, %struct.ScmObj** %stackaddr$env-ref50704
%stackaddr$env-ref50705 = alloca %struct.ScmObj*, align 8
%acc40140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41358, i64 5)
store %struct.ScmObj* %acc40140, %struct.ScmObj** %stackaddr$env-ref50705
%stackaddr$env-ref50706 = alloca %struct.ScmObj*, align 8
%k40491 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41358, i64 6)
store %struct.ScmObj* %k40491, %struct.ScmObj** %stackaddr$env-ref50706
%stackaddr$prim50707 = alloca %struct.ScmObj*, align 8
%_95k40498 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48882)
store volatile %struct.ScmObj* %_95k40498, %struct.ScmObj** %stackaddr$prim50707, align 8
%stackaddr$prim50708 = alloca %struct.ScmObj*, align 8
%current_45args48883 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48882)
store volatile %struct.ScmObj* %current_45args48883, %struct.ScmObj** %stackaddr$prim50708, align 8
%stackaddr$prim50709 = alloca %struct.ScmObj*, align 8
%anf_45bind40281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48883)
store volatile %struct.ScmObj* %anf_45bind40281, %struct.ScmObj** %stackaddr$prim50709, align 8
%stackaddr$prim50710 = alloca %struct.ScmObj*, align 8
%anf_45bind40282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40140, %struct.ScmObj* %lsts_4340146)
store volatile %struct.ScmObj* %anf_45bind40282, %struct.ScmObj** %stackaddr$prim50710, align 8
%stackaddr$prim50711 = alloca %struct.ScmObj*, align 8
%anf_45bind40283 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40141, %struct.ScmObj* %anf_45bind40282)
store volatile %struct.ScmObj* %anf_45bind40283, %struct.ScmObj** %stackaddr$prim50711, align 8
%stackaddr$makeclosure50712 = alloca %struct.ScmObj*, align 8
%fptrToInt50713 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41384 to i64
%ae41384 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt50713)
store volatile %struct.ScmObj* %ae41384, %struct.ScmObj** %stackaddr$makeclosure50712, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41384, %struct.ScmObj* %anf_45bind40281, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41384, %struct.ScmObj* %_37foldr140131, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41384, %struct.ScmObj* %vs40144, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41384, %struct.ScmObj* %f40141, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41384, %struct.ScmObj* %k40491, i64 4)
%stackaddr$prim50714 = alloca %struct.ScmObj*, align 8
%cpsargs40502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41384, %struct.ScmObj* %anf_45bind40283)
store volatile %struct.ScmObj* %cpsargs40502, %struct.ScmObj** %stackaddr$prim50714, align 8
%clofunc50715 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40137)
musttail call tailcc void %clofunc50715(%struct.ScmObj* %_37foldr40137, %struct.ScmObj* %cpsargs40502)
ret void
}

define tailcc void @proc_clo$ae41384(%struct.ScmObj* %env$ae41384,%struct.ScmObj* %current_45args48885) {
%stackaddr$env-ref50716 = alloca %struct.ScmObj*, align 8
%anf_45bind40281 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41384, i64 0)
store %struct.ScmObj* %anf_45bind40281, %struct.ScmObj** %stackaddr$env-ref50716
%stackaddr$env-ref50717 = alloca %struct.ScmObj*, align 8
%_37foldr140131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41384, i64 1)
store %struct.ScmObj* %_37foldr140131, %struct.ScmObj** %stackaddr$env-ref50717
%stackaddr$env-ref50718 = alloca %struct.ScmObj*, align 8
%vs40144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41384, i64 2)
store %struct.ScmObj* %vs40144, %struct.ScmObj** %stackaddr$env-ref50718
%stackaddr$env-ref50719 = alloca %struct.ScmObj*, align 8
%f40141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41384, i64 3)
store %struct.ScmObj* %f40141, %struct.ScmObj** %stackaddr$env-ref50719
%stackaddr$env-ref50720 = alloca %struct.ScmObj*, align 8
%k40491 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41384, i64 4)
store %struct.ScmObj* %k40491, %struct.ScmObj** %stackaddr$env-ref50720
%stackaddr$prim50721 = alloca %struct.ScmObj*, align 8
%_95k40499 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48885)
store volatile %struct.ScmObj* %_95k40499, %struct.ScmObj** %stackaddr$prim50721, align 8
%stackaddr$prim50722 = alloca %struct.ScmObj*, align 8
%current_45args48886 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48885)
store volatile %struct.ScmObj* %current_45args48886, %struct.ScmObj** %stackaddr$prim50722, align 8
%stackaddr$prim50723 = alloca %struct.ScmObj*, align 8
%anf_45bind40284 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48886)
store volatile %struct.ScmObj* %anf_45bind40284, %struct.ScmObj** %stackaddr$prim50723, align 8
%ae41389 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50724 = alloca %struct.ScmObj*, align 8
%anf_45bind40285 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40284, %struct.ScmObj* %ae41389)
store volatile %struct.ScmObj* %anf_45bind40285, %struct.ScmObj** %stackaddr$prim50724, align 8
%stackaddr$makeclosure50725 = alloca %struct.ScmObj*, align 8
%fptrToInt50726 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41391 to i64
%ae41391 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50726)
store volatile %struct.ScmObj* %ae41391, %struct.ScmObj** %stackaddr$makeclosure50725, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41391, %struct.ScmObj* %f40141, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41391, %struct.ScmObj* %k40491, i64 1)
%args48891$_37foldr140131$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50727 = alloca %struct.ScmObj*, align 8
%args48891$_37foldr140131$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40144, %struct.ScmObj* %args48891$_37foldr140131$0)
store volatile %struct.ScmObj* %args48891$_37foldr140131$1, %struct.ScmObj** %stackaddr$prim50727, align 8
%stackaddr$prim50728 = alloca %struct.ScmObj*, align 8
%args48891$_37foldr140131$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40285, %struct.ScmObj* %args48891$_37foldr140131$1)
store volatile %struct.ScmObj* %args48891$_37foldr140131$2, %struct.ScmObj** %stackaddr$prim50728, align 8
%stackaddr$prim50729 = alloca %struct.ScmObj*, align 8
%args48891$_37foldr140131$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40281, %struct.ScmObj* %args48891$_37foldr140131$2)
store volatile %struct.ScmObj* %args48891$_37foldr140131$3, %struct.ScmObj** %stackaddr$prim50729, align 8
%stackaddr$prim50730 = alloca %struct.ScmObj*, align 8
%args48891$_37foldr140131$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41391, %struct.ScmObj* %args48891$_37foldr140131$3)
store volatile %struct.ScmObj* %args48891$_37foldr140131$4, %struct.ScmObj** %stackaddr$prim50730, align 8
%clofunc50731 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140131)
musttail call tailcc void %clofunc50731(%struct.ScmObj* %_37foldr140131, %struct.ScmObj* %args48891$_37foldr140131$4)
ret void
}

define tailcc void @proc_clo$ae41391(%struct.ScmObj* %env$ae41391,%struct.ScmObj* %current_45args48888) {
%stackaddr$env-ref50732 = alloca %struct.ScmObj*, align 8
%f40141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41391, i64 0)
store %struct.ScmObj* %f40141, %struct.ScmObj** %stackaddr$env-ref50732
%stackaddr$env-ref50733 = alloca %struct.ScmObj*, align 8
%k40491 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41391, i64 1)
store %struct.ScmObj* %k40491, %struct.ScmObj** %stackaddr$env-ref50733
%stackaddr$prim50734 = alloca %struct.ScmObj*, align 8
%_95k40500 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48888)
store volatile %struct.ScmObj* %_95k40500, %struct.ScmObj** %stackaddr$prim50734, align 8
%stackaddr$prim50735 = alloca %struct.ScmObj*, align 8
%current_45args48889 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48888)
store volatile %struct.ScmObj* %current_45args48889, %struct.ScmObj** %stackaddr$prim50735, align 8
%stackaddr$prim50736 = alloca %struct.ScmObj*, align 8
%anf_45bind40286 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48889)
store volatile %struct.ScmObj* %anf_45bind40286, %struct.ScmObj** %stackaddr$prim50736, align 8
%stackaddr$prim50737 = alloca %struct.ScmObj*, align 8
%cpsargs40501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40491, %struct.ScmObj* %anf_45bind40286)
store volatile %struct.ScmObj* %cpsargs40501, %struct.ScmObj** %stackaddr$prim50737, align 8
%clofunc50738 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40141)
musttail call tailcc void %clofunc50738(%struct.ScmObj* %f40141, %struct.ScmObj* %cpsargs40501)
ret void
}

define tailcc void @proc_clo$ae41360(%struct.ScmObj* %env$ae41360,%struct.ScmObj* %current_45args48892) {
%stackaddr$prim50739 = alloca %struct.ScmObj*, align 8
%k40503 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48892)
store volatile %struct.ScmObj* %k40503, %struct.ScmObj** %stackaddr$prim50739, align 8
%stackaddr$prim50740 = alloca %struct.ScmObj*, align 8
%current_45args48893 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48892)
store volatile %struct.ScmObj* %current_45args48893, %struct.ScmObj** %stackaddr$prim50740, align 8
%stackaddr$prim50741 = alloca %struct.ScmObj*, align 8
%a40149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48893)
store volatile %struct.ScmObj* %a40149, %struct.ScmObj** %stackaddr$prim50741, align 8
%stackaddr$prim50742 = alloca %struct.ScmObj*, align 8
%current_45args48894 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48893)
store volatile %struct.ScmObj* %current_45args48894, %struct.ScmObj** %stackaddr$prim50742, align 8
%stackaddr$prim50743 = alloca %struct.ScmObj*, align 8
%b40148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48894)
store volatile %struct.ScmObj* %b40148, %struct.ScmObj** %stackaddr$prim50743, align 8
%stackaddr$prim50744 = alloca %struct.ScmObj*, align 8
%cpsprim40504 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40149, %struct.ScmObj* %b40148)
store volatile %struct.ScmObj* %cpsprim40504, %struct.ScmObj** %stackaddr$prim50744, align 8
%ae41364 = call %struct.ScmObj* @const_init_int(i64 0)
%args48896$k40503$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50745 = alloca %struct.ScmObj*, align 8
%args48896$k40503$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40504, %struct.ScmObj* %args48896$k40503$0)
store volatile %struct.ScmObj* %args48896$k40503$1, %struct.ScmObj** %stackaddr$prim50745, align 8
%stackaddr$prim50746 = alloca %struct.ScmObj*, align 8
%args48896$k40503$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41364, %struct.ScmObj* %args48896$k40503$1)
store volatile %struct.ScmObj* %args48896$k40503$2, %struct.ScmObj** %stackaddr$prim50746, align 8
%clofunc50747 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40503)
musttail call tailcc void %clofunc50747(%struct.ScmObj* %k40503, %struct.ScmObj* %args48896$k40503$2)
ret void
}

define tailcc void @proc_clo$ae41336(%struct.ScmObj* %env$ae41336,%struct.ScmObj* %current_45args48899) {
%stackaddr$prim50748 = alloca %struct.ScmObj*, align 8
%k40505 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48899)
store volatile %struct.ScmObj* %k40505, %struct.ScmObj** %stackaddr$prim50748, align 8
%stackaddr$prim50749 = alloca %struct.ScmObj*, align 8
%current_45args48900 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48899)
store volatile %struct.ScmObj* %current_45args48900, %struct.ScmObj** %stackaddr$prim50749, align 8
%stackaddr$prim50750 = alloca %struct.ScmObj*, align 8
%x40145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48900)
store volatile %struct.ScmObj* %x40145, %struct.ScmObj** %stackaddr$prim50750, align 8
%stackaddr$prim50751 = alloca %struct.ScmObj*, align 8
%cpsprim40506 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40145)
store volatile %struct.ScmObj* %cpsprim40506, %struct.ScmObj** %stackaddr$prim50751, align 8
%ae41339 = call %struct.ScmObj* @const_init_int(i64 0)
%args48902$k40505$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50752 = alloca %struct.ScmObj*, align 8
%args48902$k40505$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40506, %struct.ScmObj* %args48902$k40505$0)
store volatile %struct.ScmObj* %args48902$k40505$1, %struct.ScmObj** %stackaddr$prim50752, align 8
%stackaddr$prim50753 = alloca %struct.ScmObj*, align 8
%args48902$k40505$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41339, %struct.ScmObj* %args48902$k40505$1)
store volatile %struct.ScmObj* %args48902$k40505$2, %struct.ScmObj** %stackaddr$prim50753, align 8
%clofunc50754 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40505)
musttail call tailcc void %clofunc50754(%struct.ScmObj* %k40505, %struct.ScmObj* %args48902$k40505$2)
ret void
}

define tailcc void @proc_clo$ae41312(%struct.ScmObj* %env$ae41312,%struct.ScmObj* %current_45args48905) {
%stackaddr$prim50755 = alloca %struct.ScmObj*, align 8
%k40507 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48905)
store volatile %struct.ScmObj* %k40507, %struct.ScmObj** %stackaddr$prim50755, align 8
%stackaddr$prim50756 = alloca %struct.ScmObj*, align 8
%current_45args48906 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48905)
store volatile %struct.ScmObj* %current_45args48906, %struct.ScmObj** %stackaddr$prim50756, align 8
%stackaddr$prim50757 = alloca %struct.ScmObj*, align 8
%x40147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48906)
store volatile %struct.ScmObj* %x40147, %struct.ScmObj** %stackaddr$prim50757, align 8
%stackaddr$prim50758 = alloca %struct.ScmObj*, align 8
%cpsprim40508 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40147)
store volatile %struct.ScmObj* %cpsprim40508, %struct.ScmObj** %stackaddr$prim50758, align 8
%ae41315 = call %struct.ScmObj* @const_init_int(i64 0)
%args48908$k40507$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50759 = alloca %struct.ScmObj*, align 8
%args48908$k40507$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40508, %struct.ScmObj* %args48908$k40507$0)
store volatile %struct.ScmObj* %args48908$k40507$1, %struct.ScmObj** %stackaddr$prim50759, align 8
%stackaddr$prim50760 = alloca %struct.ScmObj*, align 8
%args48908$k40507$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41315, %struct.ScmObj* %args48908$k40507$1)
store volatile %struct.ScmObj* %args48908$k40507$2, %struct.ScmObj** %stackaddr$prim50760, align 8
%clofunc50761 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40507)
musttail call tailcc void %clofunc50761(%struct.ScmObj* %k40507, %struct.ScmObj* %args48908$k40507$2)
ret void
}

define tailcc void @proc_clo$ae41264(%struct.ScmObj* %env$ae41264,%struct.ScmObj* %current_45args48911) {
%stackaddr$prim50762 = alloca %struct.ScmObj*, align 8
%k40509 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48911)
store volatile %struct.ScmObj* %k40509, %struct.ScmObj** %stackaddr$prim50762, align 8
%stackaddr$prim50763 = alloca %struct.ScmObj*, align 8
%current_45args48912 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48911)
store volatile %struct.ScmObj* %current_45args48912, %struct.ScmObj** %stackaddr$prim50763, align 8
%stackaddr$prim50764 = alloca %struct.ScmObj*, align 8
%lst40143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48912)
store volatile %struct.ScmObj* %lst40143, %struct.ScmObj** %stackaddr$prim50764, align 8
%stackaddr$prim50765 = alloca %struct.ScmObj*, align 8
%current_45args48913 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48912)
store volatile %struct.ScmObj* %current_45args48913, %struct.ScmObj** %stackaddr$prim50765, align 8
%stackaddr$prim50766 = alloca %struct.ScmObj*, align 8
%b40142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48913)
store volatile %struct.ScmObj* %b40142, %struct.ScmObj** %stackaddr$prim50766, align 8
%truthy$cmp50767 = call i64 @is_truthy_value(%struct.ScmObj* %b40142)
%cmp$cmp50767 = icmp eq i64 %truthy$cmp50767, 1
br i1 %cmp$cmp50767, label %truebranch$cmp50767, label %falsebranch$cmp50767
truebranch$cmp50767:
%ae41267 = call %struct.ScmObj* @const_init_int(i64 0)
%args48915$k40509$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50768 = alloca %struct.ScmObj*, align 8
%args48915$k40509$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40142, %struct.ScmObj* %args48915$k40509$0)
store volatile %struct.ScmObj* %args48915$k40509$1, %struct.ScmObj** %stackaddr$prim50768, align 8
%stackaddr$prim50769 = alloca %struct.ScmObj*, align 8
%args48915$k40509$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41267, %struct.ScmObj* %args48915$k40509$1)
store volatile %struct.ScmObj* %args48915$k40509$2, %struct.ScmObj** %stackaddr$prim50769, align 8
%clofunc50770 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40509)
musttail call tailcc void %clofunc50770(%struct.ScmObj* %k40509, %struct.ScmObj* %args48915$k40509$2)
ret void
falsebranch$cmp50767:
%stackaddr$prim50771 = alloca %struct.ScmObj*, align 8
%cpsprim40510 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40143)
store volatile %struct.ScmObj* %cpsprim40510, %struct.ScmObj** %stackaddr$prim50771, align 8
%ae41274 = call %struct.ScmObj* @const_init_int(i64 0)
%args48916$k40509$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50772 = alloca %struct.ScmObj*, align 8
%args48916$k40509$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40510, %struct.ScmObj* %args48916$k40509$0)
store volatile %struct.ScmObj* %args48916$k40509$1, %struct.ScmObj** %stackaddr$prim50772, align 8
%stackaddr$prim50773 = alloca %struct.ScmObj*, align 8
%args48916$k40509$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41274, %struct.ScmObj* %args48916$k40509$1)
store volatile %struct.ScmObj* %args48916$k40509$2, %struct.ScmObj** %stackaddr$prim50773, align 8
%clofunc50774 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40509)
musttail call tailcc void %clofunc50774(%struct.ScmObj* %k40509, %struct.ScmObj* %args48916$k40509$2)
ret void
}

define tailcc void @proc_clo$ae41221(%struct.ScmObj* %env$ae41221,%struct.ScmObj* %current_45args48920) {
%stackaddr$env-ref50775 = alloca %struct.ScmObj*, align 8
%_37length40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41221, i64 0)
store %struct.ScmObj* %_37length40120, %struct.ScmObj** %stackaddr$env-ref50775
%stackaddr$env-ref50776 = alloca %struct.ScmObj*, align 8
%_37take40123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41221, i64 1)
store %struct.ScmObj* %_37take40123, %struct.ScmObj** %stackaddr$env-ref50776
%stackaddr$prim50777 = alloca %struct.ScmObj*, align 8
%k40511 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48920)
store volatile %struct.ScmObj* %k40511, %struct.ScmObj** %stackaddr$prim50777, align 8
%stackaddr$prim50778 = alloca %struct.ScmObj*, align 8
%current_45args48921 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48920)
store volatile %struct.ScmObj* %current_45args48921, %struct.ScmObj** %stackaddr$prim50778, align 8
%stackaddr$prim50779 = alloca %struct.ScmObj*, align 8
%lst40152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48921)
store volatile %struct.ScmObj* %lst40152, %struct.ScmObj** %stackaddr$prim50779, align 8
%stackaddr$prim50780 = alloca %struct.ScmObj*, align 8
%current_45args48922 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48921)
store volatile %struct.ScmObj* %current_45args48922, %struct.ScmObj** %stackaddr$prim50780, align 8
%stackaddr$prim50781 = alloca %struct.ScmObj*, align 8
%n40151 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48922)
store volatile %struct.ScmObj* %n40151, %struct.ScmObj** %stackaddr$prim50781, align 8
%stackaddr$makeclosure50782 = alloca %struct.ScmObj*, align 8
%fptrToInt50783 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41223 to i64
%ae41223 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50783)
store volatile %struct.ScmObj* %ae41223, %struct.ScmObj** %stackaddr$makeclosure50782, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41223, %struct.ScmObj* %lst40152, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41223, %struct.ScmObj* %n40151, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41223, %struct.ScmObj* %k40511, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41223, %struct.ScmObj* %_37take40123, i64 3)
%args48928$_37length40120$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50784 = alloca %struct.ScmObj*, align 8
%args48928$_37length40120$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40152, %struct.ScmObj* %args48928$_37length40120$0)
store volatile %struct.ScmObj* %args48928$_37length40120$1, %struct.ScmObj** %stackaddr$prim50784, align 8
%stackaddr$prim50785 = alloca %struct.ScmObj*, align 8
%args48928$_37length40120$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41223, %struct.ScmObj* %args48928$_37length40120$1)
store volatile %struct.ScmObj* %args48928$_37length40120$2, %struct.ScmObj** %stackaddr$prim50785, align 8
%clofunc50786 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40120)
musttail call tailcc void %clofunc50786(%struct.ScmObj* %_37length40120, %struct.ScmObj* %args48928$_37length40120$2)
ret void
}

define tailcc void @proc_clo$ae41223(%struct.ScmObj* %env$ae41223,%struct.ScmObj* %current_45args48924) {
%stackaddr$env-ref50787 = alloca %struct.ScmObj*, align 8
%lst40152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41223, i64 0)
store %struct.ScmObj* %lst40152, %struct.ScmObj** %stackaddr$env-ref50787
%stackaddr$env-ref50788 = alloca %struct.ScmObj*, align 8
%n40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41223, i64 1)
store %struct.ScmObj* %n40151, %struct.ScmObj** %stackaddr$env-ref50788
%stackaddr$env-ref50789 = alloca %struct.ScmObj*, align 8
%k40511 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41223, i64 2)
store %struct.ScmObj* %k40511, %struct.ScmObj** %stackaddr$env-ref50789
%stackaddr$env-ref50790 = alloca %struct.ScmObj*, align 8
%_37take40123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41223, i64 3)
store %struct.ScmObj* %_37take40123, %struct.ScmObj** %stackaddr$env-ref50790
%stackaddr$prim50791 = alloca %struct.ScmObj*, align 8
%_95k40512 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48924)
store volatile %struct.ScmObj* %_95k40512, %struct.ScmObj** %stackaddr$prim50791, align 8
%stackaddr$prim50792 = alloca %struct.ScmObj*, align 8
%current_45args48925 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48924)
store volatile %struct.ScmObj* %current_45args48925, %struct.ScmObj** %stackaddr$prim50792, align 8
%stackaddr$prim50793 = alloca %struct.ScmObj*, align 8
%anf_45bind40273 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48925)
store volatile %struct.ScmObj* %anf_45bind40273, %struct.ScmObj** %stackaddr$prim50793, align 8
%stackaddr$prim50794 = alloca %struct.ScmObj*, align 8
%anf_45bind40274 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40273, %struct.ScmObj* %n40151)
store volatile %struct.ScmObj* %anf_45bind40274, %struct.ScmObj** %stackaddr$prim50794, align 8
%args48927$_37take40123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50795 = alloca %struct.ScmObj*, align 8
%args48927$_37take40123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40274, %struct.ScmObj* %args48927$_37take40123$0)
store volatile %struct.ScmObj* %args48927$_37take40123$1, %struct.ScmObj** %stackaddr$prim50795, align 8
%stackaddr$prim50796 = alloca %struct.ScmObj*, align 8
%args48927$_37take40123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40152, %struct.ScmObj* %args48927$_37take40123$1)
store volatile %struct.ScmObj* %args48927$_37take40123$2, %struct.ScmObj** %stackaddr$prim50796, align 8
%stackaddr$prim50797 = alloca %struct.ScmObj*, align 8
%args48927$_37take40123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40511, %struct.ScmObj* %args48927$_37take40123$2)
store volatile %struct.ScmObj* %args48927$_37take40123$3, %struct.ScmObj** %stackaddr$prim50797, align 8
%clofunc50798 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40123)
musttail call tailcc void %clofunc50798(%struct.ScmObj* %_37take40123, %struct.ScmObj* %args48927$_37take40123$3)
ret void
}

define tailcc void @proc_clo$ae41167(%struct.ScmObj* %env$ae41167,%struct.ScmObj* %current_45args48930) {
%stackaddr$env-ref50799 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41167, i64 0)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref50799
%stackaddr$prim50800 = alloca %struct.ScmObj*, align 8
%k40513 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48930)
store volatile %struct.ScmObj* %k40513, %struct.ScmObj** %stackaddr$prim50800, align 8
%stackaddr$prim50801 = alloca %struct.ScmObj*, align 8
%current_45args48931 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48930)
store volatile %struct.ScmObj* %current_45args48931, %struct.ScmObj** %stackaddr$prim50801, align 8
%stackaddr$prim50802 = alloca %struct.ScmObj*, align 8
%lst40154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48931)
store volatile %struct.ScmObj* %lst40154, %struct.ScmObj** %stackaddr$prim50802, align 8
%stackaddr$makeclosure50803 = alloca %struct.ScmObj*, align 8
%fptrToInt50804 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41168 to i64
%ae41168 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt50804)
store volatile %struct.ScmObj* %ae41168, %struct.ScmObj** %stackaddr$makeclosure50803, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41168, %struct.ScmObj* %_37foldl140115, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41168, %struct.ScmObj* %k40513, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41168, %struct.ScmObj* %lst40154, i64 2)
%ae41169 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50805 = alloca %struct.ScmObj*, align 8
%fptrToInt50806 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41170 to i64
%ae41170 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt50806)
store volatile %struct.ScmObj* %ae41170, %struct.ScmObj** %stackaddr$makeclosure50805, align 8
%args48942$ae41168$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50807 = alloca %struct.ScmObj*, align 8
%args48942$ae41168$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41170, %struct.ScmObj* %args48942$ae41168$0)
store volatile %struct.ScmObj* %args48942$ae41168$1, %struct.ScmObj** %stackaddr$prim50807, align 8
%stackaddr$prim50808 = alloca %struct.ScmObj*, align 8
%args48942$ae41168$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41169, %struct.ScmObj* %args48942$ae41168$1)
store volatile %struct.ScmObj* %args48942$ae41168$2, %struct.ScmObj** %stackaddr$prim50808, align 8
%clofunc50809 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41168)
musttail call tailcc void %clofunc50809(%struct.ScmObj* %ae41168, %struct.ScmObj* %args48942$ae41168$2)
ret void
}

define tailcc void @proc_clo$ae41168(%struct.ScmObj* %env$ae41168,%struct.ScmObj* %current_45args48933) {
%stackaddr$env-ref50810 = alloca %struct.ScmObj*, align 8
%_37foldl140115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41168, i64 0)
store %struct.ScmObj* %_37foldl140115, %struct.ScmObj** %stackaddr$env-ref50810
%stackaddr$env-ref50811 = alloca %struct.ScmObj*, align 8
%k40513 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41168, i64 1)
store %struct.ScmObj* %k40513, %struct.ScmObj** %stackaddr$env-ref50811
%stackaddr$env-ref50812 = alloca %struct.ScmObj*, align 8
%lst40154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41168, i64 2)
store %struct.ScmObj* %lst40154, %struct.ScmObj** %stackaddr$env-ref50812
%stackaddr$prim50813 = alloca %struct.ScmObj*, align 8
%_95k40514 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48933)
store volatile %struct.ScmObj* %_95k40514, %struct.ScmObj** %stackaddr$prim50813, align 8
%stackaddr$prim50814 = alloca %struct.ScmObj*, align 8
%current_45args48934 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48933)
store volatile %struct.ScmObj* %current_45args48934, %struct.ScmObj** %stackaddr$prim50814, align 8
%stackaddr$prim50815 = alloca %struct.ScmObj*, align 8
%anf_45bind40272 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48934)
store volatile %struct.ScmObj* %anf_45bind40272, %struct.ScmObj** %stackaddr$prim50815, align 8
%ae41189 = call %struct.ScmObj* @const_init_null()
%args48936$_37foldl140115$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50816 = alloca %struct.ScmObj*, align 8
%args48936$_37foldl140115$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40154, %struct.ScmObj* %args48936$_37foldl140115$0)
store volatile %struct.ScmObj* %args48936$_37foldl140115$1, %struct.ScmObj** %stackaddr$prim50816, align 8
%stackaddr$prim50817 = alloca %struct.ScmObj*, align 8
%args48936$_37foldl140115$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41189, %struct.ScmObj* %args48936$_37foldl140115$1)
store volatile %struct.ScmObj* %args48936$_37foldl140115$2, %struct.ScmObj** %stackaddr$prim50817, align 8
%stackaddr$prim50818 = alloca %struct.ScmObj*, align 8
%args48936$_37foldl140115$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40272, %struct.ScmObj* %args48936$_37foldl140115$2)
store volatile %struct.ScmObj* %args48936$_37foldl140115$3, %struct.ScmObj** %stackaddr$prim50818, align 8
%stackaddr$prim50819 = alloca %struct.ScmObj*, align 8
%args48936$_37foldl140115$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40513, %struct.ScmObj* %args48936$_37foldl140115$3)
store volatile %struct.ScmObj* %args48936$_37foldl140115$4, %struct.ScmObj** %stackaddr$prim50819, align 8
%clofunc50820 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140115)
musttail call tailcc void %clofunc50820(%struct.ScmObj* %_37foldl140115, %struct.ScmObj* %args48936$_37foldl140115$4)
ret void
}

define tailcc void @proc_clo$ae41170(%struct.ScmObj* %env$ae41170,%struct.ScmObj* %current_45args48937) {
%stackaddr$prim50821 = alloca %struct.ScmObj*, align 8
%k40515 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48937)
store volatile %struct.ScmObj* %k40515, %struct.ScmObj** %stackaddr$prim50821, align 8
%stackaddr$prim50822 = alloca %struct.ScmObj*, align 8
%current_45args48938 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48937)
store volatile %struct.ScmObj* %current_45args48938, %struct.ScmObj** %stackaddr$prim50822, align 8
%stackaddr$prim50823 = alloca %struct.ScmObj*, align 8
%x40156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48938)
store volatile %struct.ScmObj* %x40156, %struct.ScmObj** %stackaddr$prim50823, align 8
%stackaddr$prim50824 = alloca %struct.ScmObj*, align 8
%current_45args48939 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48938)
store volatile %struct.ScmObj* %current_45args48939, %struct.ScmObj** %stackaddr$prim50824, align 8
%stackaddr$prim50825 = alloca %struct.ScmObj*, align 8
%y40155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48939)
store volatile %struct.ScmObj* %y40155, %struct.ScmObj** %stackaddr$prim50825, align 8
%ae41172 = call %struct.ScmObj* @const_init_int(i64 0)
%args48941$k40515$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50826 = alloca %struct.ScmObj*, align 8
%args48941$k40515$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x40156, %struct.ScmObj* %args48941$k40515$0)
store volatile %struct.ScmObj* %args48941$k40515$1, %struct.ScmObj** %stackaddr$prim50826, align 8
%stackaddr$prim50827 = alloca %struct.ScmObj*, align 8
%args48941$k40515$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41172, %struct.ScmObj* %args48941$k40515$1)
store volatile %struct.ScmObj* %args48941$k40515$2, %struct.ScmObj** %stackaddr$prim50827, align 8
%clofunc50828 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40515)
musttail call tailcc void %clofunc50828(%struct.ScmObj* %k40515, %struct.ScmObj* %args48941$k40515$2)
ret void
}

define tailcc void @proc_clo$ae41088(%struct.ScmObj* %env$ae41088,%struct.ScmObj* %current_45args48945) {
%stackaddr$prim50829 = alloca %struct.ScmObj*, align 8
%k40516 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48945)
store volatile %struct.ScmObj* %k40516, %struct.ScmObj** %stackaddr$prim50829, align 8
%stackaddr$prim50830 = alloca %struct.ScmObj*, align 8
%current_45args48946 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48945)
store volatile %struct.ScmObj* %current_45args48946, %struct.ScmObj** %stackaddr$prim50830, align 8
%stackaddr$prim50831 = alloca %struct.ScmObj*, align 8
%_37foldl140116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48946)
store volatile %struct.ScmObj* %_37foldl140116, %struct.ScmObj** %stackaddr$prim50831, align 8
%ae41090 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50832 = alloca %struct.ScmObj*, align 8
%fptrToInt50833 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41091 to i64
%ae41091 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50833)
store volatile %struct.ScmObj* %ae41091, %struct.ScmObj** %stackaddr$makeclosure50832, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41091, %struct.ScmObj* %_37foldl140116, i64 0)
%args48959$k40516$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50834 = alloca %struct.ScmObj*, align 8
%args48959$k40516$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41091, %struct.ScmObj* %args48959$k40516$0)
store volatile %struct.ScmObj* %args48959$k40516$1, %struct.ScmObj** %stackaddr$prim50834, align 8
%stackaddr$prim50835 = alloca %struct.ScmObj*, align 8
%args48959$k40516$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41090, %struct.ScmObj* %args48959$k40516$1)
store volatile %struct.ScmObj* %args48959$k40516$2, %struct.ScmObj** %stackaddr$prim50835, align 8
%clofunc50836 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40516)
musttail call tailcc void %clofunc50836(%struct.ScmObj* %k40516, %struct.ScmObj* %args48959$k40516$2)
ret void
}

define tailcc void @proc_clo$ae41091(%struct.ScmObj* %env$ae41091,%struct.ScmObj* %current_45args48948) {
%stackaddr$env-ref50837 = alloca %struct.ScmObj*, align 8
%_37foldl140116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41091, i64 0)
store %struct.ScmObj* %_37foldl140116, %struct.ScmObj** %stackaddr$env-ref50837
%stackaddr$prim50838 = alloca %struct.ScmObj*, align 8
%k40517 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48948)
store volatile %struct.ScmObj* %k40517, %struct.ScmObj** %stackaddr$prim50838, align 8
%stackaddr$prim50839 = alloca %struct.ScmObj*, align 8
%current_45args48949 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48948)
store volatile %struct.ScmObj* %current_45args48949, %struct.ScmObj** %stackaddr$prim50839, align 8
%stackaddr$prim50840 = alloca %struct.ScmObj*, align 8
%f40119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48949)
store volatile %struct.ScmObj* %f40119, %struct.ScmObj** %stackaddr$prim50840, align 8
%stackaddr$prim50841 = alloca %struct.ScmObj*, align 8
%current_45args48950 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48949)
store volatile %struct.ScmObj* %current_45args48950, %struct.ScmObj** %stackaddr$prim50841, align 8
%stackaddr$prim50842 = alloca %struct.ScmObj*, align 8
%acc40118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48950)
store volatile %struct.ScmObj* %acc40118, %struct.ScmObj** %stackaddr$prim50842, align 8
%stackaddr$prim50843 = alloca %struct.ScmObj*, align 8
%current_45args48951 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48950)
store volatile %struct.ScmObj* %current_45args48951, %struct.ScmObj** %stackaddr$prim50843, align 8
%stackaddr$prim50844 = alloca %struct.ScmObj*, align 8
%lst40117 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48951)
store volatile %struct.ScmObj* %lst40117, %struct.ScmObj** %stackaddr$prim50844, align 8
%stackaddr$prim50845 = alloca %struct.ScmObj*, align 8
%anf_45bind40267 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40117)
store volatile %struct.ScmObj* %anf_45bind40267, %struct.ScmObj** %stackaddr$prim50845, align 8
%truthy$cmp50846 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40267)
%cmp$cmp50846 = icmp eq i64 %truthy$cmp50846, 1
br i1 %cmp$cmp50846, label %truebranch$cmp50846, label %falsebranch$cmp50846
truebranch$cmp50846:
%ae41095 = call %struct.ScmObj* @const_init_int(i64 0)
%args48953$k40517$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50847 = alloca %struct.ScmObj*, align 8
%args48953$k40517$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40118, %struct.ScmObj* %args48953$k40517$0)
store volatile %struct.ScmObj* %args48953$k40517$1, %struct.ScmObj** %stackaddr$prim50847, align 8
%stackaddr$prim50848 = alloca %struct.ScmObj*, align 8
%args48953$k40517$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41095, %struct.ScmObj* %args48953$k40517$1)
store volatile %struct.ScmObj* %args48953$k40517$2, %struct.ScmObj** %stackaddr$prim50848, align 8
%clofunc50849 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40517)
musttail call tailcc void %clofunc50849(%struct.ScmObj* %k40517, %struct.ScmObj* %args48953$k40517$2)
ret void
falsebranch$cmp50846:
%stackaddr$prim50850 = alloca %struct.ScmObj*, align 8
%anf_45bind40268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40117)
store volatile %struct.ScmObj* %anf_45bind40268, %struct.ScmObj** %stackaddr$prim50850, align 8
%stackaddr$makeclosure50851 = alloca %struct.ScmObj*, align 8
%fptrToInt50852 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41102 to i64
%ae41102 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50852)
store volatile %struct.ScmObj* %ae41102, %struct.ScmObj** %stackaddr$makeclosure50851, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41102, %struct.ScmObj* %k40517, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41102, %struct.ScmObj* %lst40117, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41102, %struct.ScmObj* %f40119, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41102, %struct.ScmObj* %_37foldl140116, i64 3)
%args48958$f40119$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50853 = alloca %struct.ScmObj*, align 8
%args48958$f40119$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40118, %struct.ScmObj* %args48958$f40119$0)
store volatile %struct.ScmObj* %args48958$f40119$1, %struct.ScmObj** %stackaddr$prim50853, align 8
%stackaddr$prim50854 = alloca %struct.ScmObj*, align 8
%args48958$f40119$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40268, %struct.ScmObj* %args48958$f40119$1)
store volatile %struct.ScmObj* %args48958$f40119$2, %struct.ScmObj** %stackaddr$prim50854, align 8
%stackaddr$prim50855 = alloca %struct.ScmObj*, align 8
%args48958$f40119$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41102, %struct.ScmObj* %args48958$f40119$2)
store volatile %struct.ScmObj* %args48958$f40119$3, %struct.ScmObj** %stackaddr$prim50855, align 8
%clofunc50856 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40119)
musttail call tailcc void %clofunc50856(%struct.ScmObj* %f40119, %struct.ScmObj* %args48958$f40119$3)
ret void
}

define tailcc void @proc_clo$ae41102(%struct.ScmObj* %env$ae41102,%struct.ScmObj* %current_45args48954) {
%stackaddr$env-ref50857 = alloca %struct.ScmObj*, align 8
%k40517 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41102, i64 0)
store %struct.ScmObj* %k40517, %struct.ScmObj** %stackaddr$env-ref50857
%stackaddr$env-ref50858 = alloca %struct.ScmObj*, align 8
%lst40117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41102, i64 1)
store %struct.ScmObj* %lst40117, %struct.ScmObj** %stackaddr$env-ref50858
%stackaddr$env-ref50859 = alloca %struct.ScmObj*, align 8
%f40119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41102, i64 2)
store %struct.ScmObj* %f40119, %struct.ScmObj** %stackaddr$env-ref50859
%stackaddr$env-ref50860 = alloca %struct.ScmObj*, align 8
%_37foldl140116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41102, i64 3)
store %struct.ScmObj* %_37foldl140116, %struct.ScmObj** %stackaddr$env-ref50860
%stackaddr$prim50861 = alloca %struct.ScmObj*, align 8
%_95k40518 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48954)
store volatile %struct.ScmObj* %_95k40518, %struct.ScmObj** %stackaddr$prim50861, align 8
%stackaddr$prim50862 = alloca %struct.ScmObj*, align 8
%current_45args48955 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48954)
store volatile %struct.ScmObj* %current_45args48955, %struct.ScmObj** %stackaddr$prim50862, align 8
%stackaddr$prim50863 = alloca %struct.ScmObj*, align 8
%anf_45bind40269 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48955)
store volatile %struct.ScmObj* %anf_45bind40269, %struct.ScmObj** %stackaddr$prim50863, align 8
%stackaddr$prim50864 = alloca %struct.ScmObj*, align 8
%anf_45bind40270 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40117)
store volatile %struct.ScmObj* %anf_45bind40270, %struct.ScmObj** %stackaddr$prim50864, align 8
%args48957$_37foldl140116$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50865 = alloca %struct.ScmObj*, align 8
%args48957$_37foldl140116$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40270, %struct.ScmObj* %args48957$_37foldl140116$0)
store volatile %struct.ScmObj* %args48957$_37foldl140116$1, %struct.ScmObj** %stackaddr$prim50865, align 8
%stackaddr$prim50866 = alloca %struct.ScmObj*, align 8
%args48957$_37foldl140116$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40269, %struct.ScmObj* %args48957$_37foldl140116$1)
store volatile %struct.ScmObj* %args48957$_37foldl140116$2, %struct.ScmObj** %stackaddr$prim50866, align 8
%stackaddr$prim50867 = alloca %struct.ScmObj*, align 8
%args48957$_37foldl140116$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40119, %struct.ScmObj* %args48957$_37foldl140116$2)
store volatile %struct.ScmObj* %args48957$_37foldl140116$3, %struct.ScmObj** %stackaddr$prim50867, align 8
%stackaddr$prim50868 = alloca %struct.ScmObj*, align 8
%args48957$_37foldl140116$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40517, %struct.ScmObj* %args48957$_37foldl140116$3)
store volatile %struct.ScmObj* %args48957$_37foldl140116$4, %struct.ScmObj** %stackaddr$prim50868, align 8
%clofunc50869 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140116)
musttail call tailcc void %clofunc50869(%struct.ScmObj* %_37foldl140116, %struct.ScmObj* %args48957$_37foldl140116$4)
ret void
}

define tailcc void @proc_clo$ae41005(%struct.ScmObj* %env$ae41005,%struct.ScmObj* %current_45args48962) {
%stackaddr$prim50870 = alloca %struct.ScmObj*, align 8
%k40519 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48962)
store volatile %struct.ScmObj* %k40519, %struct.ScmObj** %stackaddr$prim50870, align 8
%stackaddr$prim50871 = alloca %struct.ScmObj*, align 8
%current_45args48963 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48962)
store volatile %struct.ScmObj* %current_45args48963, %struct.ScmObj** %stackaddr$prim50871, align 8
%stackaddr$prim50872 = alloca %struct.ScmObj*, align 8
%_37length40121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48963)
store volatile %struct.ScmObj* %_37length40121, %struct.ScmObj** %stackaddr$prim50872, align 8
%ae41007 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50873 = alloca %struct.ScmObj*, align 8
%fptrToInt50874 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41008 to i64
%ae41008 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50874)
store volatile %struct.ScmObj* %ae41008, %struct.ScmObj** %stackaddr$makeclosure50873, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41008, %struct.ScmObj* %_37length40121, i64 0)
%args48974$k40519$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50875 = alloca %struct.ScmObj*, align 8
%args48974$k40519$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41008, %struct.ScmObj* %args48974$k40519$0)
store volatile %struct.ScmObj* %args48974$k40519$1, %struct.ScmObj** %stackaddr$prim50875, align 8
%stackaddr$prim50876 = alloca %struct.ScmObj*, align 8
%args48974$k40519$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41007, %struct.ScmObj* %args48974$k40519$1)
store volatile %struct.ScmObj* %args48974$k40519$2, %struct.ScmObj** %stackaddr$prim50876, align 8
%clofunc50877 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40519)
musttail call tailcc void %clofunc50877(%struct.ScmObj* %k40519, %struct.ScmObj* %args48974$k40519$2)
ret void
}

define tailcc void @proc_clo$ae41008(%struct.ScmObj* %env$ae41008,%struct.ScmObj* %current_45args48965) {
%stackaddr$env-ref50878 = alloca %struct.ScmObj*, align 8
%_37length40121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41008, i64 0)
store %struct.ScmObj* %_37length40121, %struct.ScmObj** %stackaddr$env-ref50878
%stackaddr$prim50879 = alloca %struct.ScmObj*, align 8
%k40520 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48965)
store volatile %struct.ScmObj* %k40520, %struct.ScmObj** %stackaddr$prim50879, align 8
%stackaddr$prim50880 = alloca %struct.ScmObj*, align 8
%current_45args48966 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48965)
store volatile %struct.ScmObj* %current_45args48966, %struct.ScmObj** %stackaddr$prim50880, align 8
%stackaddr$prim50881 = alloca %struct.ScmObj*, align 8
%lst40122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48966)
store volatile %struct.ScmObj* %lst40122, %struct.ScmObj** %stackaddr$prim50881, align 8
%stackaddr$prim50882 = alloca %struct.ScmObj*, align 8
%anf_45bind40263 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40122)
store volatile %struct.ScmObj* %anf_45bind40263, %struct.ScmObj** %stackaddr$prim50882, align 8
%truthy$cmp50883 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40263)
%cmp$cmp50883 = icmp eq i64 %truthy$cmp50883, 1
br i1 %cmp$cmp50883, label %truebranch$cmp50883, label %falsebranch$cmp50883
truebranch$cmp50883:
%ae41012 = call %struct.ScmObj* @const_init_int(i64 0)
%ae41013 = call %struct.ScmObj* @const_init_int(i64 0)
%args48968$k40520$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50884 = alloca %struct.ScmObj*, align 8
%args48968$k40520$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41013, %struct.ScmObj* %args48968$k40520$0)
store volatile %struct.ScmObj* %args48968$k40520$1, %struct.ScmObj** %stackaddr$prim50884, align 8
%stackaddr$prim50885 = alloca %struct.ScmObj*, align 8
%args48968$k40520$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41012, %struct.ScmObj* %args48968$k40520$1)
store volatile %struct.ScmObj* %args48968$k40520$2, %struct.ScmObj** %stackaddr$prim50885, align 8
%clofunc50886 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40520)
musttail call tailcc void %clofunc50886(%struct.ScmObj* %k40520, %struct.ScmObj* %args48968$k40520$2)
ret void
falsebranch$cmp50883:
%stackaddr$prim50887 = alloca %struct.ScmObj*, align 8
%anf_45bind40264 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40122)
store volatile %struct.ScmObj* %anf_45bind40264, %struct.ScmObj** %stackaddr$prim50887, align 8
%stackaddr$makeclosure50888 = alloca %struct.ScmObj*, align 8
%fptrToInt50889 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41022 to i64
%ae41022 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50889)
store volatile %struct.ScmObj* %ae41022, %struct.ScmObj** %stackaddr$makeclosure50888, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41022, %struct.ScmObj* %k40520, i64 0)
%args48973$_37length40121$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50890 = alloca %struct.ScmObj*, align 8
%args48973$_37length40121$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40264, %struct.ScmObj* %args48973$_37length40121$0)
store volatile %struct.ScmObj* %args48973$_37length40121$1, %struct.ScmObj** %stackaddr$prim50890, align 8
%stackaddr$prim50891 = alloca %struct.ScmObj*, align 8
%args48973$_37length40121$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41022, %struct.ScmObj* %args48973$_37length40121$1)
store volatile %struct.ScmObj* %args48973$_37length40121$2, %struct.ScmObj** %stackaddr$prim50891, align 8
%clofunc50892 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40121)
musttail call tailcc void %clofunc50892(%struct.ScmObj* %_37length40121, %struct.ScmObj* %args48973$_37length40121$2)
ret void
}

define tailcc void @proc_clo$ae41022(%struct.ScmObj* %env$ae41022,%struct.ScmObj* %current_45args48969) {
%stackaddr$env-ref50893 = alloca %struct.ScmObj*, align 8
%k40520 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41022, i64 0)
store %struct.ScmObj* %k40520, %struct.ScmObj** %stackaddr$env-ref50893
%stackaddr$prim50894 = alloca %struct.ScmObj*, align 8
%_95k40521 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48969)
store volatile %struct.ScmObj* %_95k40521, %struct.ScmObj** %stackaddr$prim50894, align 8
%stackaddr$prim50895 = alloca %struct.ScmObj*, align 8
%current_45args48970 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48969)
store volatile %struct.ScmObj* %current_45args48970, %struct.ScmObj** %stackaddr$prim50895, align 8
%stackaddr$prim50896 = alloca %struct.ScmObj*, align 8
%anf_45bind40265 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48970)
store volatile %struct.ScmObj* %anf_45bind40265, %struct.ScmObj** %stackaddr$prim50896, align 8
%ae41024 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50897 = alloca %struct.ScmObj*, align 8
%cpsprim40522 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae41024, %struct.ScmObj* %anf_45bind40265)
store volatile %struct.ScmObj* %cpsprim40522, %struct.ScmObj** %stackaddr$prim50897, align 8
%ae41027 = call %struct.ScmObj* @const_init_int(i64 0)
%args48972$k40520$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50898 = alloca %struct.ScmObj*, align 8
%args48972$k40520$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40522, %struct.ScmObj* %args48972$k40520$0)
store volatile %struct.ScmObj* %args48972$k40520$1, %struct.ScmObj** %stackaddr$prim50898, align 8
%stackaddr$prim50899 = alloca %struct.ScmObj*, align 8
%args48972$k40520$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41027, %struct.ScmObj* %args48972$k40520$1)
store volatile %struct.ScmObj* %args48972$k40520$2, %struct.ScmObj** %stackaddr$prim50899, align 8
%clofunc50900 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40520)
musttail call tailcc void %clofunc50900(%struct.ScmObj* %k40520, %struct.ScmObj* %args48972$k40520$2)
ret void
}

define tailcc void @proc_clo$ae40855(%struct.ScmObj* %env$ae40855,%struct.ScmObj* %current_45args48977) {
%stackaddr$prim50901 = alloca %struct.ScmObj*, align 8
%k40523 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48977)
store volatile %struct.ScmObj* %k40523, %struct.ScmObj** %stackaddr$prim50901, align 8
%stackaddr$prim50902 = alloca %struct.ScmObj*, align 8
%current_45args48978 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48977)
store volatile %struct.ScmObj* %current_45args48978, %struct.ScmObj** %stackaddr$prim50902, align 8
%stackaddr$prim50903 = alloca %struct.ScmObj*, align 8
%_37take40124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48978)
store volatile %struct.ScmObj* %_37take40124, %struct.ScmObj** %stackaddr$prim50903, align 8
%ae40857 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50904 = alloca %struct.ScmObj*, align 8
%fptrToInt50905 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40858 to i64
%ae40858 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50905)
store volatile %struct.ScmObj* %ae40858, %struct.ScmObj** %stackaddr$makeclosure50904, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40858, %struct.ScmObj* %_37take40124, i64 0)
%args48991$k40523$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50906 = alloca %struct.ScmObj*, align 8
%args48991$k40523$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40858, %struct.ScmObj* %args48991$k40523$0)
store volatile %struct.ScmObj* %args48991$k40523$1, %struct.ScmObj** %stackaddr$prim50906, align 8
%stackaddr$prim50907 = alloca %struct.ScmObj*, align 8
%args48991$k40523$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40857, %struct.ScmObj* %args48991$k40523$1)
store volatile %struct.ScmObj* %args48991$k40523$2, %struct.ScmObj** %stackaddr$prim50907, align 8
%clofunc50908 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40523)
musttail call tailcc void %clofunc50908(%struct.ScmObj* %k40523, %struct.ScmObj* %args48991$k40523$2)
ret void
}

define tailcc void @proc_clo$ae40858(%struct.ScmObj* %env$ae40858,%struct.ScmObj* %current_45args48980) {
%stackaddr$env-ref50909 = alloca %struct.ScmObj*, align 8
%_37take40124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40858, i64 0)
store %struct.ScmObj* %_37take40124, %struct.ScmObj** %stackaddr$env-ref50909
%stackaddr$prim50910 = alloca %struct.ScmObj*, align 8
%k40524 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48980)
store volatile %struct.ScmObj* %k40524, %struct.ScmObj** %stackaddr$prim50910, align 8
%stackaddr$prim50911 = alloca %struct.ScmObj*, align 8
%current_45args48981 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48980)
store volatile %struct.ScmObj* %current_45args48981, %struct.ScmObj** %stackaddr$prim50911, align 8
%stackaddr$prim50912 = alloca %struct.ScmObj*, align 8
%lst40126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48981)
store volatile %struct.ScmObj* %lst40126, %struct.ScmObj** %stackaddr$prim50912, align 8
%stackaddr$prim50913 = alloca %struct.ScmObj*, align 8
%current_45args48982 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48981)
store volatile %struct.ScmObj* %current_45args48982, %struct.ScmObj** %stackaddr$prim50913, align 8
%stackaddr$prim50914 = alloca %struct.ScmObj*, align 8
%n40125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48982)
store volatile %struct.ScmObj* %n40125, %struct.ScmObj** %stackaddr$prim50914, align 8
%ae40860 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim50915 = alloca %struct.ScmObj*, align 8
%anf_45bind40256 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n40125, %struct.ScmObj* %ae40860)
store volatile %struct.ScmObj* %anf_45bind40256, %struct.ScmObj** %stackaddr$prim50915, align 8
%truthy$cmp50916 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40256)
%cmp$cmp50916 = icmp eq i64 %truthy$cmp50916, 1
br i1 %cmp$cmp50916, label %truebranch$cmp50916, label %falsebranch$cmp50916
truebranch$cmp50916:
%ae40863 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40864 = call %struct.ScmObj* @const_init_null()
%args48984$k40524$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50917 = alloca %struct.ScmObj*, align 8
%args48984$k40524$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40864, %struct.ScmObj* %args48984$k40524$0)
store volatile %struct.ScmObj* %args48984$k40524$1, %struct.ScmObj** %stackaddr$prim50917, align 8
%stackaddr$prim50918 = alloca %struct.ScmObj*, align 8
%args48984$k40524$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40863, %struct.ScmObj* %args48984$k40524$1)
store volatile %struct.ScmObj* %args48984$k40524$2, %struct.ScmObj** %stackaddr$prim50918, align 8
%clofunc50919 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40524)
musttail call tailcc void %clofunc50919(%struct.ScmObj* %k40524, %struct.ScmObj* %args48984$k40524$2)
ret void
falsebranch$cmp50916:
%stackaddr$prim50920 = alloca %struct.ScmObj*, align 8
%anf_45bind40257 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40126)
store volatile %struct.ScmObj* %anf_45bind40257, %struct.ScmObj** %stackaddr$prim50920, align 8
%truthy$cmp50921 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40257)
%cmp$cmp50921 = icmp eq i64 %truthy$cmp50921, 1
br i1 %cmp$cmp50921, label %truebranch$cmp50921, label %falsebranch$cmp50921
truebranch$cmp50921:
%ae40874 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40875 = call %struct.ScmObj* @const_init_null()
%args48985$k40524$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50922 = alloca %struct.ScmObj*, align 8
%args48985$k40524$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40875, %struct.ScmObj* %args48985$k40524$0)
store volatile %struct.ScmObj* %args48985$k40524$1, %struct.ScmObj** %stackaddr$prim50922, align 8
%stackaddr$prim50923 = alloca %struct.ScmObj*, align 8
%args48985$k40524$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40874, %struct.ScmObj* %args48985$k40524$1)
store volatile %struct.ScmObj* %args48985$k40524$2, %struct.ScmObj** %stackaddr$prim50923, align 8
%clofunc50924 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40524)
musttail call tailcc void %clofunc50924(%struct.ScmObj* %k40524, %struct.ScmObj* %args48985$k40524$2)
ret void
falsebranch$cmp50921:
%stackaddr$prim50925 = alloca %struct.ScmObj*, align 8
%anf_45bind40258 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40126)
store volatile %struct.ScmObj* %anf_45bind40258, %struct.ScmObj** %stackaddr$prim50925, align 8
%stackaddr$prim50926 = alloca %struct.ScmObj*, align 8
%anf_45bind40259 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40126)
store volatile %struct.ScmObj* %anf_45bind40259, %struct.ScmObj** %stackaddr$prim50926, align 8
%ae40885 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim50927 = alloca %struct.ScmObj*, align 8
%anf_45bind40260 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n40125, %struct.ScmObj* %ae40885)
store volatile %struct.ScmObj* %anf_45bind40260, %struct.ScmObj** %stackaddr$prim50927, align 8
%stackaddr$makeclosure50928 = alloca %struct.ScmObj*, align 8
%fptrToInt50929 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40887 to i64
%ae40887 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50929)
store volatile %struct.ScmObj* %ae40887, %struct.ScmObj** %stackaddr$makeclosure50928, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40887, %struct.ScmObj* %anf_45bind40258, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40887, %struct.ScmObj* %k40524, i64 1)
%args48990$_37take40124$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50930 = alloca %struct.ScmObj*, align 8
%args48990$_37take40124$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40260, %struct.ScmObj* %args48990$_37take40124$0)
store volatile %struct.ScmObj* %args48990$_37take40124$1, %struct.ScmObj** %stackaddr$prim50930, align 8
%stackaddr$prim50931 = alloca %struct.ScmObj*, align 8
%args48990$_37take40124$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40259, %struct.ScmObj* %args48990$_37take40124$1)
store volatile %struct.ScmObj* %args48990$_37take40124$2, %struct.ScmObj** %stackaddr$prim50931, align 8
%stackaddr$prim50932 = alloca %struct.ScmObj*, align 8
%args48990$_37take40124$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40887, %struct.ScmObj* %args48990$_37take40124$2)
store volatile %struct.ScmObj* %args48990$_37take40124$3, %struct.ScmObj** %stackaddr$prim50932, align 8
%clofunc50933 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40124)
musttail call tailcc void %clofunc50933(%struct.ScmObj* %_37take40124, %struct.ScmObj* %args48990$_37take40124$3)
ret void
}

define tailcc void @proc_clo$ae40887(%struct.ScmObj* %env$ae40887,%struct.ScmObj* %current_45args48986) {
%stackaddr$env-ref50934 = alloca %struct.ScmObj*, align 8
%anf_45bind40258 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40887, i64 0)
store %struct.ScmObj* %anf_45bind40258, %struct.ScmObj** %stackaddr$env-ref50934
%stackaddr$env-ref50935 = alloca %struct.ScmObj*, align 8
%k40524 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40887, i64 1)
store %struct.ScmObj* %k40524, %struct.ScmObj** %stackaddr$env-ref50935
%stackaddr$prim50936 = alloca %struct.ScmObj*, align 8
%_95k40525 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48986)
store volatile %struct.ScmObj* %_95k40525, %struct.ScmObj** %stackaddr$prim50936, align 8
%stackaddr$prim50937 = alloca %struct.ScmObj*, align 8
%current_45args48987 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48986)
store volatile %struct.ScmObj* %current_45args48987, %struct.ScmObj** %stackaddr$prim50937, align 8
%stackaddr$prim50938 = alloca %struct.ScmObj*, align 8
%anf_45bind40261 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48987)
store volatile %struct.ScmObj* %anf_45bind40261, %struct.ScmObj** %stackaddr$prim50938, align 8
%stackaddr$prim50939 = alloca %struct.ScmObj*, align 8
%cpsprim40526 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40258, %struct.ScmObj* %anf_45bind40261)
store volatile %struct.ScmObj* %cpsprim40526, %struct.ScmObj** %stackaddr$prim50939, align 8
%ae40893 = call %struct.ScmObj* @const_init_int(i64 0)
%args48989$k40524$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50940 = alloca %struct.ScmObj*, align 8
%args48989$k40524$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40526, %struct.ScmObj* %args48989$k40524$0)
store volatile %struct.ScmObj* %args48989$k40524$1, %struct.ScmObj** %stackaddr$prim50940, align 8
%stackaddr$prim50941 = alloca %struct.ScmObj*, align 8
%args48989$k40524$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40893, %struct.ScmObj* %args48989$k40524$1)
store volatile %struct.ScmObj* %args48989$k40524$2, %struct.ScmObj** %stackaddr$prim50941, align 8
%clofunc50942 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40524)
musttail call tailcc void %clofunc50942(%struct.ScmObj* %k40524, %struct.ScmObj* %args48989$k40524$2)
ret void
}

define tailcc void @proc_clo$ae40758(%struct.ScmObj* %env$ae40758,%struct.ScmObj* %current_45args48994) {
%stackaddr$prim50943 = alloca %struct.ScmObj*, align 8
%k40527 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48994)
store volatile %struct.ScmObj* %k40527, %struct.ScmObj** %stackaddr$prim50943, align 8
%stackaddr$prim50944 = alloca %struct.ScmObj*, align 8
%current_45args48995 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48994)
store volatile %struct.ScmObj* %current_45args48995, %struct.ScmObj** %stackaddr$prim50944, align 8
%stackaddr$prim50945 = alloca %struct.ScmObj*, align 8
%_37map40128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48995)
store volatile %struct.ScmObj* %_37map40128, %struct.ScmObj** %stackaddr$prim50945, align 8
%ae40760 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50946 = alloca %struct.ScmObj*, align 8
%fptrToInt50947 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40761 to i64
%ae40761 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50947)
store volatile %struct.ScmObj* %ae40761, %struct.ScmObj** %stackaddr$makeclosure50946, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40761, %struct.ScmObj* %_37map40128, i64 0)
%args49011$k40527$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50948 = alloca %struct.ScmObj*, align 8
%args49011$k40527$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40761, %struct.ScmObj* %args49011$k40527$0)
store volatile %struct.ScmObj* %args49011$k40527$1, %struct.ScmObj** %stackaddr$prim50948, align 8
%stackaddr$prim50949 = alloca %struct.ScmObj*, align 8
%args49011$k40527$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40760, %struct.ScmObj* %args49011$k40527$1)
store volatile %struct.ScmObj* %args49011$k40527$2, %struct.ScmObj** %stackaddr$prim50949, align 8
%clofunc50950 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40527)
musttail call tailcc void %clofunc50950(%struct.ScmObj* %k40527, %struct.ScmObj* %args49011$k40527$2)
ret void
}

define tailcc void @proc_clo$ae40761(%struct.ScmObj* %env$ae40761,%struct.ScmObj* %current_45args48997) {
%stackaddr$env-ref50951 = alloca %struct.ScmObj*, align 8
%_37map40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40761, i64 0)
store %struct.ScmObj* %_37map40128, %struct.ScmObj** %stackaddr$env-ref50951
%stackaddr$prim50952 = alloca %struct.ScmObj*, align 8
%k40528 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48997)
store volatile %struct.ScmObj* %k40528, %struct.ScmObj** %stackaddr$prim50952, align 8
%stackaddr$prim50953 = alloca %struct.ScmObj*, align 8
%current_45args48998 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48997)
store volatile %struct.ScmObj* %current_45args48998, %struct.ScmObj** %stackaddr$prim50953, align 8
%stackaddr$prim50954 = alloca %struct.ScmObj*, align 8
%f40130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48998)
store volatile %struct.ScmObj* %f40130, %struct.ScmObj** %stackaddr$prim50954, align 8
%stackaddr$prim50955 = alloca %struct.ScmObj*, align 8
%current_45args48999 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args48998)
store volatile %struct.ScmObj* %current_45args48999, %struct.ScmObj** %stackaddr$prim50955, align 8
%stackaddr$prim50956 = alloca %struct.ScmObj*, align 8
%lst40129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args48999)
store volatile %struct.ScmObj* %lst40129, %struct.ScmObj** %stackaddr$prim50956, align 8
%stackaddr$prim50957 = alloca %struct.ScmObj*, align 8
%anf_45bind40250 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40129)
store volatile %struct.ScmObj* %anf_45bind40250, %struct.ScmObj** %stackaddr$prim50957, align 8
%truthy$cmp50958 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40250)
%cmp$cmp50958 = icmp eq i64 %truthy$cmp50958, 1
br i1 %cmp$cmp50958, label %truebranch$cmp50958, label %falsebranch$cmp50958
truebranch$cmp50958:
%ae40765 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40766 = call %struct.ScmObj* @const_init_null()
%args49001$k40528$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50959 = alloca %struct.ScmObj*, align 8
%args49001$k40528$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40766, %struct.ScmObj* %args49001$k40528$0)
store volatile %struct.ScmObj* %args49001$k40528$1, %struct.ScmObj** %stackaddr$prim50959, align 8
%stackaddr$prim50960 = alloca %struct.ScmObj*, align 8
%args49001$k40528$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40765, %struct.ScmObj* %args49001$k40528$1)
store volatile %struct.ScmObj* %args49001$k40528$2, %struct.ScmObj** %stackaddr$prim50960, align 8
%clofunc50961 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40528)
musttail call tailcc void %clofunc50961(%struct.ScmObj* %k40528, %struct.ScmObj* %args49001$k40528$2)
ret void
falsebranch$cmp50958:
%stackaddr$prim50962 = alloca %struct.ScmObj*, align 8
%anf_45bind40251 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40129)
store volatile %struct.ScmObj* %anf_45bind40251, %struct.ScmObj** %stackaddr$prim50962, align 8
%stackaddr$makeclosure50963 = alloca %struct.ScmObj*, align 8
%fptrToInt50964 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40775 to i64
%ae40775 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt50964)
store volatile %struct.ScmObj* %ae40775, %struct.ScmObj** %stackaddr$makeclosure50963, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40775, %struct.ScmObj* %k40528, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40775, %struct.ScmObj* %_37map40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40775, %struct.ScmObj* %f40130, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae40775, %struct.ScmObj* %lst40129, i64 3)
%args49010$f40130$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50965 = alloca %struct.ScmObj*, align 8
%args49010$f40130$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40251, %struct.ScmObj* %args49010$f40130$0)
store volatile %struct.ScmObj* %args49010$f40130$1, %struct.ScmObj** %stackaddr$prim50965, align 8
%stackaddr$prim50966 = alloca %struct.ScmObj*, align 8
%args49010$f40130$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40775, %struct.ScmObj* %args49010$f40130$1)
store volatile %struct.ScmObj* %args49010$f40130$2, %struct.ScmObj** %stackaddr$prim50966, align 8
%clofunc50967 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40130)
musttail call tailcc void %clofunc50967(%struct.ScmObj* %f40130, %struct.ScmObj* %args49010$f40130$2)
ret void
}

define tailcc void @proc_clo$ae40775(%struct.ScmObj* %env$ae40775,%struct.ScmObj* %current_45args49002) {
%stackaddr$env-ref50968 = alloca %struct.ScmObj*, align 8
%k40528 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40775, i64 0)
store %struct.ScmObj* %k40528, %struct.ScmObj** %stackaddr$env-ref50968
%stackaddr$env-ref50969 = alloca %struct.ScmObj*, align 8
%_37map40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40775, i64 1)
store %struct.ScmObj* %_37map40128, %struct.ScmObj** %stackaddr$env-ref50969
%stackaddr$env-ref50970 = alloca %struct.ScmObj*, align 8
%f40130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40775, i64 2)
store %struct.ScmObj* %f40130, %struct.ScmObj** %stackaddr$env-ref50970
%stackaddr$env-ref50971 = alloca %struct.ScmObj*, align 8
%lst40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40775, i64 3)
store %struct.ScmObj* %lst40129, %struct.ScmObj** %stackaddr$env-ref50971
%stackaddr$prim50972 = alloca %struct.ScmObj*, align 8
%_95k40529 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49002)
store volatile %struct.ScmObj* %_95k40529, %struct.ScmObj** %stackaddr$prim50972, align 8
%stackaddr$prim50973 = alloca %struct.ScmObj*, align 8
%current_45args49003 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49002)
store volatile %struct.ScmObj* %current_45args49003, %struct.ScmObj** %stackaddr$prim50973, align 8
%stackaddr$prim50974 = alloca %struct.ScmObj*, align 8
%anf_45bind40252 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49003)
store volatile %struct.ScmObj* %anf_45bind40252, %struct.ScmObj** %stackaddr$prim50974, align 8
%stackaddr$prim50975 = alloca %struct.ScmObj*, align 8
%anf_45bind40253 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40129)
store volatile %struct.ScmObj* %anf_45bind40253, %struct.ScmObj** %stackaddr$prim50975, align 8
%stackaddr$makeclosure50976 = alloca %struct.ScmObj*, align 8
%fptrToInt50977 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40779 to i64
%ae40779 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt50977)
store volatile %struct.ScmObj* %ae40779, %struct.ScmObj** %stackaddr$makeclosure50976, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40779, %struct.ScmObj* %k40528, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40779, %struct.ScmObj* %anf_45bind40252, i64 1)
%args49009$_37map40128$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50978 = alloca %struct.ScmObj*, align 8
%args49009$_37map40128$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40253, %struct.ScmObj* %args49009$_37map40128$0)
store volatile %struct.ScmObj* %args49009$_37map40128$1, %struct.ScmObj** %stackaddr$prim50978, align 8
%stackaddr$prim50979 = alloca %struct.ScmObj*, align 8
%args49009$_37map40128$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40130, %struct.ScmObj* %args49009$_37map40128$1)
store volatile %struct.ScmObj* %args49009$_37map40128$2, %struct.ScmObj** %stackaddr$prim50979, align 8
%stackaddr$prim50980 = alloca %struct.ScmObj*, align 8
%args49009$_37map40128$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40779, %struct.ScmObj* %args49009$_37map40128$2)
store volatile %struct.ScmObj* %args49009$_37map40128$3, %struct.ScmObj** %stackaddr$prim50980, align 8
%clofunc50981 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map40128)
musttail call tailcc void %clofunc50981(%struct.ScmObj* %_37map40128, %struct.ScmObj* %args49009$_37map40128$3)
ret void
}

define tailcc void @proc_clo$ae40779(%struct.ScmObj* %env$ae40779,%struct.ScmObj* %current_45args49005) {
%stackaddr$env-ref50982 = alloca %struct.ScmObj*, align 8
%k40528 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40779, i64 0)
store %struct.ScmObj* %k40528, %struct.ScmObj** %stackaddr$env-ref50982
%stackaddr$env-ref50983 = alloca %struct.ScmObj*, align 8
%anf_45bind40252 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40779, i64 1)
store %struct.ScmObj* %anf_45bind40252, %struct.ScmObj** %stackaddr$env-ref50983
%stackaddr$prim50984 = alloca %struct.ScmObj*, align 8
%_95k40530 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49005)
store volatile %struct.ScmObj* %_95k40530, %struct.ScmObj** %stackaddr$prim50984, align 8
%stackaddr$prim50985 = alloca %struct.ScmObj*, align 8
%current_45args49006 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49005)
store volatile %struct.ScmObj* %current_45args49006, %struct.ScmObj** %stackaddr$prim50985, align 8
%stackaddr$prim50986 = alloca %struct.ScmObj*, align 8
%anf_45bind40254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49006)
store volatile %struct.ScmObj* %anf_45bind40254, %struct.ScmObj** %stackaddr$prim50986, align 8
%stackaddr$prim50987 = alloca %struct.ScmObj*, align 8
%cpsprim40531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40252, %struct.ScmObj* %anf_45bind40254)
store volatile %struct.ScmObj* %cpsprim40531, %struct.ScmObj** %stackaddr$prim50987, align 8
%ae40785 = call %struct.ScmObj* @const_init_int(i64 0)
%args49008$k40528$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50988 = alloca %struct.ScmObj*, align 8
%args49008$k40528$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40531, %struct.ScmObj* %args49008$k40528$0)
store volatile %struct.ScmObj* %args49008$k40528$1, %struct.ScmObj** %stackaddr$prim50988, align 8
%stackaddr$prim50989 = alloca %struct.ScmObj*, align 8
%args49008$k40528$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40785, %struct.ScmObj* %args49008$k40528$1)
store volatile %struct.ScmObj* %args49008$k40528$2, %struct.ScmObj** %stackaddr$prim50989, align 8
%clofunc50990 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40528)
musttail call tailcc void %clofunc50990(%struct.ScmObj* %k40528, %struct.ScmObj* %args49008$k40528$2)
ret void
}

define tailcc void @proc_clo$ae40678(%struct.ScmObj* %env$ae40678,%struct.ScmObj* %current_45args49014) {
%stackaddr$prim50991 = alloca %struct.ScmObj*, align 8
%k40532 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49014)
store volatile %struct.ScmObj* %k40532, %struct.ScmObj** %stackaddr$prim50991, align 8
%stackaddr$prim50992 = alloca %struct.ScmObj*, align 8
%current_45args49015 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49014)
store volatile %struct.ScmObj* %current_45args49015, %struct.ScmObj** %stackaddr$prim50992, align 8
%stackaddr$prim50993 = alloca %struct.ScmObj*, align 8
%_37foldr140132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49015)
store volatile %struct.ScmObj* %_37foldr140132, %struct.ScmObj** %stackaddr$prim50993, align 8
%ae40680 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure50994 = alloca %struct.ScmObj*, align 8
%fptrToInt50995 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40681 to i64
%ae40681 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt50995)
store volatile %struct.ScmObj* %ae40681, %struct.ScmObj** %stackaddr$makeclosure50994, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40681, %struct.ScmObj* %_37foldr140132, i64 0)
%args49028$k40532$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim50996 = alloca %struct.ScmObj*, align 8
%args49028$k40532$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40681, %struct.ScmObj* %args49028$k40532$0)
store volatile %struct.ScmObj* %args49028$k40532$1, %struct.ScmObj** %stackaddr$prim50996, align 8
%stackaddr$prim50997 = alloca %struct.ScmObj*, align 8
%args49028$k40532$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40680, %struct.ScmObj* %args49028$k40532$1)
store volatile %struct.ScmObj* %args49028$k40532$2, %struct.ScmObj** %stackaddr$prim50997, align 8
%clofunc50998 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40532)
musttail call tailcc void %clofunc50998(%struct.ScmObj* %k40532, %struct.ScmObj* %args49028$k40532$2)
ret void
}

define tailcc void @proc_clo$ae40681(%struct.ScmObj* %env$ae40681,%struct.ScmObj* %current_45args49017) {
%stackaddr$env-ref50999 = alloca %struct.ScmObj*, align 8
%_37foldr140132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40681, i64 0)
store %struct.ScmObj* %_37foldr140132, %struct.ScmObj** %stackaddr$env-ref50999
%stackaddr$prim51000 = alloca %struct.ScmObj*, align 8
%k40533 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49017)
store volatile %struct.ScmObj* %k40533, %struct.ScmObj** %stackaddr$prim51000, align 8
%stackaddr$prim51001 = alloca %struct.ScmObj*, align 8
%current_45args49018 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49017)
store volatile %struct.ScmObj* %current_45args49018, %struct.ScmObj** %stackaddr$prim51001, align 8
%stackaddr$prim51002 = alloca %struct.ScmObj*, align 8
%f40135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49018)
store volatile %struct.ScmObj* %f40135, %struct.ScmObj** %stackaddr$prim51002, align 8
%stackaddr$prim51003 = alloca %struct.ScmObj*, align 8
%current_45args49019 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49018)
store volatile %struct.ScmObj* %current_45args49019, %struct.ScmObj** %stackaddr$prim51003, align 8
%stackaddr$prim51004 = alloca %struct.ScmObj*, align 8
%acc40134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49019)
store volatile %struct.ScmObj* %acc40134, %struct.ScmObj** %stackaddr$prim51004, align 8
%stackaddr$prim51005 = alloca %struct.ScmObj*, align 8
%current_45args49020 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49019)
store volatile %struct.ScmObj* %current_45args49020, %struct.ScmObj** %stackaddr$prim51005, align 8
%stackaddr$prim51006 = alloca %struct.ScmObj*, align 8
%lst40133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49020)
store volatile %struct.ScmObj* %lst40133, %struct.ScmObj** %stackaddr$prim51006, align 8
%stackaddr$prim51007 = alloca %struct.ScmObj*, align 8
%anf_45bind40245 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40133)
store volatile %struct.ScmObj* %anf_45bind40245, %struct.ScmObj** %stackaddr$prim51007, align 8
%truthy$cmp51008 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40245)
%cmp$cmp51008 = icmp eq i64 %truthy$cmp51008, 1
br i1 %cmp$cmp51008, label %truebranch$cmp51008, label %falsebranch$cmp51008
truebranch$cmp51008:
%ae40685 = call %struct.ScmObj* @const_init_int(i64 0)
%args49022$k40533$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51009 = alloca %struct.ScmObj*, align 8
%args49022$k40533$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40134, %struct.ScmObj* %args49022$k40533$0)
store volatile %struct.ScmObj* %args49022$k40533$1, %struct.ScmObj** %stackaddr$prim51009, align 8
%stackaddr$prim51010 = alloca %struct.ScmObj*, align 8
%args49022$k40533$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40685, %struct.ScmObj* %args49022$k40533$1)
store volatile %struct.ScmObj* %args49022$k40533$2, %struct.ScmObj** %stackaddr$prim51010, align 8
%clofunc51011 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40533)
musttail call tailcc void %clofunc51011(%struct.ScmObj* %k40533, %struct.ScmObj* %args49022$k40533$2)
ret void
falsebranch$cmp51008:
%stackaddr$prim51012 = alloca %struct.ScmObj*, align 8
%anf_45bind40246 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40133)
store volatile %struct.ScmObj* %anf_45bind40246, %struct.ScmObj** %stackaddr$prim51012, align 8
%stackaddr$prim51013 = alloca %struct.ScmObj*, align 8
%anf_45bind40247 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40133)
store volatile %struct.ScmObj* %anf_45bind40247, %struct.ScmObj** %stackaddr$prim51013, align 8
%stackaddr$makeclosure51014 = alloca %struct.ScmObj*, align 8
%fptrToInt51015 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40693 to i64
%ae40693 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt51015)
store volatile %struct.ScmObj* %ae40693, %struct.ScmObj** %stackaddr$makeclosure51014, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40693, %struct.ScmObj* %f40135, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40693, %struct.ScmObj* %anf_45bind40246, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40693, %struct.ScmObj* %k40533, i64 2)
%args49027$_37foldr140132$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51016 = alloca %struct.ScmObj*, align 8
%args49027$_37foldr140132$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40247, %struct.ScmObj* %args49027$_37foldr140132$0)
store volatile %struct.ScmObj* %args49027$_37foldr140132$1, %struct.ScmObj** %stackaddr$prim51016, align 8
%stackaddr$prim51017 = alloca %struct.ScmObj*, align 8
%args49027$_37foldr140132$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40134, %struct.ScmObj* %args49027$_37foldr140132$1)
store volatile %struct.ScmObj* %args49027$_37foldr140132$2, %struct.ScmObj** %stackaddr$prim51017, align 8
%stackaddr$prim51018 = alloca %struct.ScmObj*, align 8
%args49027$_37foldr140132$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40135, %struct.ScmObj* %args49027$_37foldr140132$2)
store volatile %struct.ScmObj* %args49027$_37foldr140132$3, %struct.ScmObj** %stackaddr$prim51018, align 8
%stackaddr$prim51019 = alloca %struct.ScmObj*, align 8
%args49027$_37foldr140132$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40693, %struct.ScmObj* %args49027$_37foldr140132$3)
store volatile %struct.ScmObj* %args49027$_37foldr140132$4, %struct.ScmObj** %stackaddr$prim51019, align 8
%clofunc51020 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140132)
musttail call tailcc void %clofunc51020(%struct.ScmObj* %_37foldr140132, %struct.ScmObj* %args49027$_37foldr140132$4)
ret void
}

define tailcc void @proc_clo$ae40693(%struct.ScmObj* %env$ae40693,%struct.ScmObj* %current_45args49023) {
%stackaddr$env-ref51021 = alloca %struct.ScmObj*, align 8
%f40135 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40693, i64 0)
store %struct.ScmObj* %f40135, %struct.ScmObj** %stackaddr$env-ref51021
%stackaddr$env-ref51022 = alloca %struct.ScmObj*, align 8
%anf_45bind40246 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40693, i64 1)
store %struct.ScmObj* %anf_45bind40246, %struct.ScmObj** %stackaddr$env-ref51022
%stackaddr$env-ref51023 = alloca %struct.ScmObj*, align 8
%k40533 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40693, i64 2)
store %struct.ScmObj* %k40533, %struct.ScmObj** %stackaddr$env-ref51023
%stackaddr$prim51024 = alloca %struct.ScmObj*, align 8
%_95k40534 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49023)
store volatile %struct.ScmObj* %_95k40534, %struct.ScmObj** %stackaddr$prim51024, align 8
%stackaddr$prim51025 = alloca %struct.ScmObj*, align 8
%current_45args49024 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49023)
store volatile %struct.ScmObj* %current_45args49024, %struct.ScmObj** %stackaddr$prim51025, align 8
%stackaddr$prim51026 = alloca %struct.ScmObj*, align 8
%anf_45bind40248 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49024)
store volatile %struct.ScmObj* %anf_45bind40248, %struct.ScmObj** %stackaddr$prim51026, align 8
%args49026$f40135$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51027 = alloca %struct.ScmObj*, align 8
%args49026$f40135$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40248, %struct.ScmObj* %args49026$f40135$0)
store volatile %struct.ScmObj* %args49026$f40135$1, %struct.ScmObj** %stackaddr$prim51027, align 8
%stackaddr$prim51028 = alloca %struct.ScmObj*, align 8
%args49026$f40135$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40246, %struct.ScmObj* %args49026$f40135$1)
store volatile %struct.ScmObj* %args49026$f40135$2, %struct.ScmObj** %stackaddr$prim51028, align 8
%stackaddr$prim51029 = alloca %struct.ScmObj*, align 8
%args49026$f40135$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40533, %struct.ScmObj* %args49026$f40135$2)
store volatile %struct.ScmObj* %args49026$f40135$3, %struct.ScmObj** %stackaddr$prim51029, align 8
%clofunc51030 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40135)
musttail call tailcc void %clofunc51030(%struct.ScmObj* %f40135, %struct.ScmObj* %args49026$f40135$3)
ret void
}

define tailcc void @proc_clo$ae40561(%struct.ScmObj* %env$ae40561,%struct.ScmObj* %current_45args49031) {
%stackaddr$prim51031 = alloca %struct.ScmObj*, align 8
%k40535 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49031)
store volatile %struct.ScmObj* %k40535, %struct.ScmObj** %stackaddr$prim51031, align 8
%stackaddr$prim51032 = alloca %struct.ScmObj*, align 8
%current_45args49032 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49031)
store volatile %struct.ScmObj* %current_45args49032, %struct.ScmObj** %stackaddr$prim51032, align 8
%stackaddr$prim51033 = alloca %struct.ScmObj*, align 8
%y40112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49032)
store volatile %struct.ScmObj* %y40112, %struct.ScmObj** %stackaddr$prim51033, align 8
%ae40563 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51034 = alloca %struct.ScmObj*, align 8
%fptrToInt51035 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40564 to i64
%ae40564 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt51035)
store volatile %struct.ScmObj* %ae40564, %struct.ScmObj** %stackaddr$makeclosure51034, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40564, %struct.ScmObj* %y40112, i64 0)
%args49050$k40535$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51036 = alloca %struct.ScmObj*, align 8
%args49050$k40535$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40564, %struct.ScmObj* %args49050$k40535$0)
store volatile %struct.ScmObj* %args49050$k40535$1, %struct.ScmObj** %stackaddr$prim51036, align 8
%stackaddr$prim51037 = alloca %struct.ScmObj*, align 8
%args49050$k40535$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40563, %struct.ScmObj* %args49050$k40535$1)
store volatile %struct.ScmObj* %args49050$k40535$2, %struct.ScmObj** %stackaddr$prim51037, align 8
%clofunc51038 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40535)
musttail call tailcc void %clofunc51038(%struct.ScmObj* %k40535, %struct.ScmObj* %args49050$k40535$2)
ret void
}

define tailcc void @proc_clo$ae40564(%struct.ScmObj* %env$ae40564,%struct.ScmObj* %current_45args49034) {
%stackaddr$env-ref51039 = alloca %struct.ScmObj*, align 8
%y40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40564, i64 0)
store %struct.ScmObj* %y40112, %struct.ScmObj** %stackaddr$env-ref51039
%stackaddr$prim51040 = alloca %struct.ScmObj*, align 8
%k40536 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49034)
store volatile %struct.ScmObj* %k40536, %struct.ScmObj** %stackaddr$prim51040, align 8
%stackaddr$prim51041 = alloca %struct.ScmObj*, align 8
%current_45args49035 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49034)
store volatile %struct.ScmObj* %current_45args49035, %struct.ScmObj** %stackaddr$prim51041, align 8
%stackaddr$prim51042 = alloca %struct.ScmObj*, align 8
%f40113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49035)
store volatile %struct.ScmObj* %f40113, %struct.ScmObj** %stackaddr$prim51042, align 8
%stackaddr$makeclosure51043 = alloca %struct.ScmObj*, align 8
%fptrToInt51044 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40565 to i64
%ae40565 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt51044)
store volatile %struct.ScmObj* %ae40565, %struct.ScmObj** %stackaddr$makeclosure51043, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40565, %struct.ScmObj* %k40536, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40565, %struct.ScmObj* %f40113, i64 1)
%ae40566 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure51045 = alloca %struct.ScmObj*, align 8
%fptrToInt51046 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40567 to i64
%ae40567 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt51046)
store volatile %struct.ScmObj* %ae40567, %struct.ScmObj** %stackaddr$makeclosure51045, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40567, %struct.ScmObj* %f40113, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40567, %struct.ScmObj* %y40112, i64 1)
%args49049$ae40565$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51047 = alloca %struct.ScmObj*, align 8
%args49049$ae40565$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40567, %struct.ScmObj* %args49049$ae40565$0)
store volatile %struct.ScmObj* %args49049$ae40565$1, %struct.ScmObj** %stackaddr$prim51047, align 8
%stackaddr$prim51048 = alloca %struct.ScmObj*, align 8
%args49049$ae40565$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40566, %struct.ScmObj* %args49049$ae40565$1)
store volatile %struct.ScmObj* %args49049$ae40565$2, %struct.ScmObj** %stackaddr$prim51048, align 8
%clofunc51049 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40565)
musttail call tailcc void %clofunc51049(%struct.ScmObj* %ae40565, %struct.ScmObj* %args49049$ae40565$2)
ret void
}

define tailcc void @proc_clo$ae40565(%struct.ScmObj* %env$ae40565,%struct.ScmObj* %current_45args49037) {
%stackaddr$env-ref51050 = alloca %struct.ScmObj*, align 8
%k40536 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40565, i64 0)
store %struct.ScmObj* %k40536, %struct.ScmObj** %stackaddr$env-ref51050
%stackaddr$env-ref51051 = alloca %struct.ScmObj*, align 8
%f40113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40565, i64 1)
store %struct.ScmObj* %f40113, %struct.ScmObj** %stackaddr$env-ref51051
%stackaddr$prim51052 = alloca %struct.ScmObj*, align 8
%_95k40537 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49037)
store volatile %struct.ScmObj* %_95k40537, %struct.ScmObj** %stackaddr$prim51052, align 8
%stackaddr$prim51053 = alloca %struct.ScmObj*, align 8
%current_45args49038 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49037)
store volatile %struct.ScmObj* %current_45args49038, %struct.ScmObj** %stackaddr$prim51053, align 8
%stackaddr$prim51054 = alloca %struct.ScmObj*, align 8
%anf_45bind40243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49038)
store volatile %struct.ScmObj* %anf_45bind40243, %struct.ScmObj** %stackaddr$prim51054, align 8
%args49040$f40113$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51055 = alloca %struct.ScmObj*, align 8
%args49040$f40113$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40243, %struct.ScmObj* %args49040$f40113$0)
store volatile %struct.ScmObj* %args49040$f40113$1, %struct.ScmObj** %stackaddr$prim51055, align 8
%stackaddr$prim51056 = alloca %struct.ScmObj*, align 8
%args49040$f40113$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40536, %struct.ScmObj* %args49040$f40113$1)
store volatile %struct.ScmObj* %args49040$f40113$2, %struct.ScmObj** %stackaddr$prim51056, align 8
%clofunc51057 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40113)
musttail call tailcc void %clofunc51057(%struct.ScmObj* %f40113, %struct.ScmObj* %args49040$f40113$2)
ret void
}

define tailcc void @proc_clo$ae40567(%struct.ScmObj* %env$ae40567,%struct.ScmObj* %args4011440538) {
%stackaddr$env-ref51058 = alloca %struct.ScmObj*, align 8
%f40113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40567, i64 0)
store %struct.ScmObj* %f40113, %struct.ScmObj** %stackaddr$env-ref51058
%stackaddr$env-ref51059 = alloca %struct.ScmObj*, align 8
%y40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40567, i64 1)
store %struct.ScmObj* %y40112, %struct.ScmObj** %stackaddr$env-ref51059
%stackaddr$prim51060 = alloca %struct.ScmObj*, align 8
%k40539 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4011440538)
store volatile %struct.ScmObj* %k40539, %struct.ScmObj** %stackaddr$prim51060, align 8
%stackaddr$prim51061 = alloca %struct.ScmObj*, align 8
%args40114 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4011440538)
store volatile %struct.ScmObj* %args40114, %struct.ScmObj** %stackaddr$prim51061, align 8
%stackaddr$makeclosure51062 = alloca %struct.ScmObj*, align 8
%fptrToInt51063 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40571 to i64
%ae40571 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt51063)
store volatile %struct.ScmObj* %ae40571, %struct.ScmObj** %stackaddr$makeclosure51062, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40571, %struct.ScmObj* %args40114, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40571, %struct.ScmObj* %f40113, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40571, %struct.ScmObj* %k40539, i64 2)
%args49048$y40112$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51064 = alloca %struct.ScmObj*, align 8
%args49048$y40112$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y40112, %struct.ScmObj* %args49048$y40112$0)
store volatile %struct.ScmObj* %args49048$y40112$1, %struct.ScmObj** %stackaddr$prim51064, align 8
%stackaddr$prim51065 = alloca %struct.ScmObj*, align 8
%args49048$y40112$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40571, %struct.ScmObj* %args49048$y40112$1)
store volatile %struct.ScmObj* %args49048$y40112$2, %struct.ScmObj** %stackaddr$prim51065, align 8
%clofunc51066 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y40112)
musttail call tailcc void %clofunc51066(%struct.ScmObj* %y40112, %struct.ScmObj* %args49048$y40112$2)
ret void
}

define tailcc void @proc_clo$ae40571(%struct.ScmObj* %env$ae40571,%struct.ScmObj* %current_45args49041) {
%stackaddr$env-ref51067 = alloca %struct.ScmObj*, align 8
%args40114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40571, i64 0)
store %struct.ScmObj* %args40114, %struct.ScmObj** %stackaddr$env-ref51067
%stackaddr$env-ref51068 = alloca %struct.ScmObj*, align 8
%f40113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40571, i64 1)
store %struct.ScmObj* %f40113, %struct.ScmObj** %stackaddr$env-ref51068
%stackaddr$env-ref51069 = alloca %struct.ScmObj*, align 8
%k40539 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40571, i64 2)
store %struct.ScmObj* %k40539, %struct.ScmObj** %stackaddr$env-ref51069
%stackaddr$prim51070 = alloca %struct.ScmObj*, align 8
%_95k40540 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49041)
store volatile %struct.ScmObj* %_95k40540, %struct.ScmObj** %stackaddr$prim51070, align 8
%stackaddr$prim51071 = alloca %struct.ScmObj*, align 8
%current_45args49042 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49041)
store volatile %struct.ScmObj* %current_45args49042, %struct.ScmObj** %stackaddr$prim51071, align 8
%stackaddr$prim51072 = alloca %struct.ScmObj*, align 8
%anf_45bind40241 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49042)
store volatile %struct.ScmObj* %anf_45bind40241, %struct.ScmObj** %stackaddr$prim51072, align 8
%stackaddr$makeclosure51073 = alloca %struct.ScmObj*, align 8
%fptrToInt51074 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40574 to i64
%ae40574 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt51074)
store volatile %struct.ScmObj* %ae40574, %struct.ScmObj** %stackaddr$makeclosure51073, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40574, %struct.ScmObj* %args40114, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40574, %struct.ScmObj* %k40539, i64 1)
%args49047$anf_45bind40241$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51075 = alloca %struct.ScmObj*, align 8
%args49047$anf_45bind40241$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40113, %struct.ScmObj* %args49047$anf_45bind40241$0)
store volatile %struct.ScmObj* %args49047$anf_45bind40241$1, %struct.ScmObj** %stackaddr$prim51075, align 8
%stackaddr$prim51076 = alloca %struct.ScmObj*, align 8
%args49047$anf_45bind40241$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40574, %struct.ScmObj* %args49047$anf_45bind40241$1)
store volatile %struct.ScmObj* %args49047$anf_45bind40241$2, %struct.ScmObj** %stackaddr$prim51076, align 8
%clofunc51077 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40241)
musttail call tailcc void %clofunc51077(%struct.ScmObj* %anf_45bind40241, %struct.ScmObj* %args49047$anf_45bind40241$2)
ret void
}

define tailcc void @proc_clo$ae40574(%struct.ScmObj* %env$ae40574,%struct.ScmObj* %current_45args49044) {
%stackaddr$env-ref51078 = alloca %struct.ScmObj*, align 8
%args40114 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40574, i64 0)
store %struct.ScmObj* %args40114, %struct.ScmObj** %stackaddr$env-ref51078
%stackaddr$env-ref51079 = alloca %struct.ScmObj*, align 8
%k40539 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40574, i64 1)
store %struct.ScmObj* %k40539, %struct.ScmObj** %stackaddr$env-ref51079
%stackaddr$prim51080 = alloca %struct.ScmObj*, align 8
%_95k40541 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49044)
store volatile %struct.ScmObj* %_95k40541, %struct.ScmObj** %stackaddr$prim51080, align 8
%stackaddr$prim51081 = alloca %struct.ScmObj*, align 8
%current_45args49045 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49044)
store volatile %struct.ScmObj* %current_45args49045, %struct.ScmObj** %stackaddr$prim51081, align 8
%stackaddr$prim51082 = alloca %struct.ScmObj*, align 8
%anf_45bind40242 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49045)
store volatile %struct.ScmObj* %anf_45bind40242, %struct.ScmObj** %stackaddr$prim51082, align 8
%stackaddr$prim51083 = alloca %struct.ScmObj*, align 8
%cpsargs40542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40539, %struct.ScmObj* %args40114)
store volatile %struct.ScmObj* %cpsargs40542, %struct.ScmObj** %stackaddr$prim51083, align 8
%clofunc51084 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40242)
musttail call tailcc void %clofunc51084(%struct.ScmObj* %anf_45bind40242, %struct.ScmObj* %cpsargs40542)
ret void
}

define tailcc void @proc_clo$ae40546(%struct.ScmObj* %env$ae40546,%struct.ScmObj* %current_45args49052) {
%stackaddr$prim51085 = alloca %struct.ScmObj*, align 8
%k40543 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49052)
store volatile %struct.ScmObj* %k40543, %struct.ScmObj** %stackaddr$prim51085, align 8
%stackaddr$prim51086 = alloca %struct.ScmObj*, align 8
%current_45args49053 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args49052)
store volatile %struct.ScmObj* %current_45args49053, %struct.ScmObj** %stackaddr$prim51086, align 8
%stackaddr$prim51087 = alloca %struct.ScmObj*, align 8
%yu40111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args49053)
store volatile %struct.ScmObj* %yu40111, %struct.ScmObj** %stackaddr$prim51087, align 8
%args49055$yu40111$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim51088 = alloca %struct.ScmObj*, align 8
%args49055$yu40111$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu40111, %struct.ScmObj* %args49055$yu40111$0)
store volatile %struct.ScmObj* %args49055$yu40111$1, %struct.ScmObj** %stackaddr$prim51088, align 8
%stackaddr$prim51089 = alloca %struct.ScmObj*, align 8
%args49055$yu40111$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40543, %struct.ScmObj* %args49055$yu40111$1)
store volatile %struct.ScmObj* %args49055$yu40111$2, %struct.ScmObj** %stackaddr$prim51089, align 8
%clofunc51090 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu40111)
musttail call tailcc void %clofunc51090(%struct.ScmObj* %yu40111, %struct.ScmObj* %args49055$yu40111$2)
ret void
}