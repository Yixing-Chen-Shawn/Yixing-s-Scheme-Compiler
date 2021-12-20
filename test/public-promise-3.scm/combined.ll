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

@global$sym$ae5094257443 = private unnamed_addr constant [8 x i8] c"promise\00", align 8
@global$sym$ae5144657485 = private unnamed_addr constant [4 x i8] c"get\00", align 8
@global$str$ae5266057498 = private unnamed_addr constant [12 x i8] c"not promise\00", align 8
@global$sym$ae5118057538 = private unnamed_addr constant [4 x i8] c"get\00", align 8
@global$str$ae5132357551 = private unnamed_addr constant [12 x i8] c"not promise\00", align 8
@global$sym$ae5106957599 = private unnamed_addr constant [8 x i8] c"promise\00", align 8
@global$sym$ae5158457657 = private unnamed_addr constant [4 x i8] c"get\00", align 8
@global$str$ae5172757670 = private unnamed_addr constant [12 x i8] c"not promise\00", align 8
@global$sym$ae5147357718 = private unnamed_addr constant [8 x i8] c"promise\00", align 8
@global$sym$ae5247457765 = private unnamed_addr constant [4 x i8] c"get\00", align 8
@global$str$ae5261757778 = private unnamed_addr constant [12 x i8] c"not promise\00", align 8
@global$sym$ae5236357826 = private unnamed_addr constant [8 x i8] c"promise\00", align 8
@global$sym$ae5279057873 = private unnamed_addr constant [4 x i8] c"get\00", align 8
@global$str$ae5293357886 = private unnamed_addr constant [12 x i8] c"not promise\00", align 8
@global$sym$ae5267957934 = private unnamed_addr constant [8 x i8] c"promise\00", align 8
@global$sym$ae5096757954 = private unnamed_addr constant [8 x i8] c"promise\00", align 8

define ccc i32 @main() {
%mainenv57038 = call %struct.ScmObj* @const_init_null()
%mainargs57039 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv57038, %struct.ScmObj* %mainargs57039)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv57036,%struct.ScmObj* %mainargs57037) {
%stackaddr$makeclosure57040 = alloca %struct.ScmObj*, align 8
%fptrToInt57041 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47519 to i64
%ae47519 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57041)
store volatile %struct.ScmObj* %ae47519, %struct.ScmObj** %stackaddr$makeclosure57040, align 8
%ae47520 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57042 = alloca %struct.ScmObj*, align 8
%fptrToInt57043 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47521 to i64
%ae47521 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57043)
store volatile %struct.ScmObj* %ae47521, %struct.ScmObj** %stackaddr$makeclosure57042, align 8
%argslist57035$ae475190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57044 = alloca %struct.ScmObj*, align 8
%argslist57035$ae475191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47521, %struct.ScmObj* %argslist57035$ae475190)
store volatile %struct.ScmObj* %argslist57035$ae475191, %struct.ScmObj** %stackaddr$prim57044, align 8
%stackaddr$prim57045 = alloca %struct.ScmObj*, align 8
%argslist57035$ae475192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47520, %struct.ScmObj* %argslist57035$ae475191)
store volatile %struct.ScmObj* %argslist57035$ae475192, %struct.ScmObj** %stackaddr$prim57045, align 8
%clofunc57046 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47519)
musttail call tailcc void %clofunc57046(%struct.ScmObj* %ae47519, %struct.ScmObj* %argslist57035$ae475192)
ret void
}

define tailcc void @proc_clo$ae47519(%struct.ScmObj* %env$ae47519,%struct.ScmObj* %current_45args56311) {
%stackaddr$prim57047 = alloca %struct.ScmObj*, align 8
%_95k47337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56311)
store volatile %struct.ScmObj* %_95k47337, %struct.ScmObj** %stackaddr$prim57047, align 8
%stackaddr$prim57048 = alloca %struct.ScmObj*, align 8
%current_45args56312 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56311)
store volatile %struct.ScmObj* %current_45args56312, %struct.ScmObj** %stackaddr$prim57048, align 8
%stackaddr$prim57049 = alloca %struct.ScmObj*, align 8
%anf_45bind47206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56312)
store volatile %struct.ScmObj* %anf_45bind47206, %struct.ScmObj** %stackaddr$prim57049, align 8
%stackaddr$makeclosure57050 = alloca %struct.ScmObj*, align 8
%fptrToInt57051 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47534 to i64
%ae47534 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57051)
store volatile %struct.ScmObj* %ae47534, %struct.ScmObj** %stackaddr$makeclosure57050, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47534, %struct.ScmObj* %anf_45bind47206, i64 0)
%ae47535 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57052 = alloca %struct.ScmObj*, align 8
%fptrToInt57053 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47536 to i64
%ae47536 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57053)
store volatile %struct.ScmObj* %ae47536, %struct.ScmObj** %stackaddr$makeclosure57052, align 8
%argslist57030$ae475340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57054 = alloca %struct.ScmObj*, align 8
%argslist57030$ae475341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47536, %struct.ScmObj* %argslist57030$ae475340)
store volatile %struct.ScmObj* %argslist57030$ae475341, %struct.ScmObj** %stackaddr$prim57054, align 8
%stackaddr$prim57055 = alloca %struct.ScmObj*, align 8
%argslist57030$ae475342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47535, %struct.ScmObj* %argslist57030$ae475341)
store volatile %struct.ScmObj* %argslist57030$ae475342, %struct.ScmObj** %stackaddr$prim57055, align 8
%clofunc57056 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47534)
musttail call tailcc void %clofunc57056(%struct.ScmObj* %ae47534, %struct.ScmObj* %argslist57030$ae475342)
ret void
}

define tailcc void @proc_clo$ae47534(%struct.ScmObj* %env$ae47534,%struct.ScmObj* %current_45args56314) {
%stackaddr$env-ref57057 = alloca %struct.ScmObj*, align 8
%anf_45bind47206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47534, i64 0)
store %struct.ScmObj* %anf_45bind47206, %struct.ScmObj** %stackaddr$env-ref57057
%stackaddr$prim57058 = alloca %struct.ScmObj*, align 8
%_95k47338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56314)
store volatile %struct.ScmObj* %_95k47338, %struct.ScmObj** %stackaddr$prim57058, align 8
%stackaddr$prim57059 = alloca %struct.ScmObj*, align 8
%current_45args56315 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56314)
store volatile %struct.ScmObj* %current_45args56315, %struct.ScmObj** %stackaddr$prim57059, align 8
%stackaddr$prim57060 = alloca %struct.ScmObj*, align 8
%anf_45bind47210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56315)
store volatile %struct.ScmObj* %anf_45bind47210, %struct.ScmObj** %stackaddr$prim57060, align 8
%stackaddr$makeclosure57061 = alloca %struct.ScmObj*, align 8
%fptrToInt57062 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47649 to i64
%ae47649 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57062)
store volatile %struct.ScmObj* %ae47649, %struct.ScmObj** %stackaddr$makeclosure57061, align 8
%argslist57009$anf_45bind472060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57063 = alloca %struct.ScmObj*, align 8
%argslist57009$anf_45bind472061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47210, %struct.ScmObj* %argslist57009$anf_45bind472060)
store volatile %struct.ScmObj* %argslist57009$anf_45bind472061, %struct.ScmObj** %stackaddr$prim57063, align 8
%stackaddr$prim57064 = alloca %struct.ScmObj*, align 8
%argslist57009$anf_45bind472062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47649, %struct.ScmObj* %argslist57009$anf_45bind472061)
store volatile %struct.ScmObj* %argslist57009$anf_45bind472062, %struct.ScmObj** %stackaddr$prim57064, align 8
%clofunc57065 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47206)
musttail call tailcc void %clofunc57065(%struct.ScmObj* %anf_45bind47206, %struct.ScmObj* %argslist57009$anf_45bind472062)
ret void
}

define tailcc void @proc_clo$ae47649(%struct.ScmObj* %env$ae47649,%struct.ScmObj* %current_45args56317) {
%stackaddr$prim57066 = alloca %struct.ScmObj*, align 8
%_95k47339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56317)
store volatile %struct.ScmObj* %_95k47339, %struct.ScmObj** %stackaddr$prim57066, align 8
%stackaddr$prim57067 = alloca %struct.ScmObj*, align 8
%current_45args56318 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56317)
store volatile %struct.ScmObj* %current_45args56318, %struct.ScmObj** %stackaddr$prim57067, align 8
%stackaddr$prim57068 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56318)
store volatile %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$prim57068, align 8
%stackaddr$makeclosure57069 = alloca %struct.ScmObj*, align 8
%fptrToInt57070 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47651 to i64
%ae47651 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57070)
store volatile %struct.ScmObj* %ae47651, %struct.ScmObj** %stackaddr$makeclosure57069, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47651, %struct.ScmObj* %Ycmb47076, i64 0)
%ae47652 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57071 = alloca %struct.ScmObj*, align 8
%fptrToInt57072 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47653 to i64
%ae47653 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57072)
store volatile %struct.ScmObj* %ae47653, %struct.ScmObj** %stackaddr$makeclosure57071, align 8
%argslist57008$ae476510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57073 = alloca %struct.ScmObj*, align 8
%argslist57008$ae476511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47653, %struct.ScmObj* %argslist57008$ae476510)
store volatile %struct.ScmObj* %argslist57008$ae476511, %struct.ScmObj** %stackaddr$prim57073, align 8
%stackaddr$prim57074 = alloca %struct.ScmObj*, align 8
%argslist57008$ae476512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47652, %struct.ScmObj* %argslist57008$ae476511)
store volatile %struct.ScmObj* %argslist57008$ae476512, %struct.ScmObj** %stackaddr$prim57074, align 8
%clofunc57075 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47651)
musttail call tailcc void %clofunc57075(%struct.ScmObj* %ae47651, %struct.ScmObj* %argslist57008$ae476512)
ret void
}

define tailcc void @proc_clo$ae47651(%struct.ScmObj* %env$ae47651,%struct.ScmObj* %current_45args56320) {
%stackaddr$env-ref57076 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47651, i64 0)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref57076
%stackaddr$prim57077 = alloca %struct.ScmObj*, align 8
%_95k47340 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56320)
store volatile %struct.ScmObj* %_95k47340, %struct.ScmObj** %stackaddr$prim57077, align 8
%stackaddr$prim57078 = alloca %struct.ScmObj*, align 8
%current_45args56321 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56320)
store volatile %struct.ScmObj* %current_45args56321, %struct.ScmObj** %stackaddr$prim57078, align 8
%stackaddr$prim57079 = alloca %struct.ScmObj*, align 8
%anf_45bind47215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56321)
store volatile %struct.ScmObj* %anf_45bind47215, %struct.ScmObj** %stackaddr$prim57079, align 8
%stackaddr$makeclosure57080 = alloca %struct.ScmObj*, align 8
%fptrToInt57081 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47729 to i64
%ae47729 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57081)
store volatile %struct.ScmObj* %ae47729, %struct.ScmObj** %stackaddr$makeclosure57080, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47729, %struct.ScmObj* %Ycmb47076, i64 0)
%argslist56992$Ycmb470760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57082 = alloca %struct.ScmObj*, align 8
%argslist56992$Ycmb470761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47215, %struct.ScmObj* %argslist56992$Ycmb470760)
store volatile %struct.ScmObj* %argslist56992$Ycmb470761, %struct.ScmObj** %stackaddr$prim57082, align 8
%stackaddr$prim57083 = alloca %struct.ScmObj*, align 8
%argslist56992$Ycmb470762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47729, %struct.ScmObj* %argslist56992$Ycmb470761)
store volatile %struct.ScmObj* %argslist56992$Ycmb470762, %struct.ScmObj** %stackaddr$prim57083, align 8
%clofunc57084 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47076)
musttail call tailcc void %clofunc57084(%struct.ScmObj* %Ycmb47076, %struct.ScmObj* %argslist56992$Ycmb470762)
ret void
}

define tailcc void @proc_clo$ae47729(%struct.ScmObj* %env$ae47729,%struct.ScmObj* %current_45args56323) {
%stackaddr$env-ref57085 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47729, i64 0)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref57085
%stackaddr$prim57086 = alloca %struct.ScmObj*, align 8
%_95k47341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56323)
store volatile %struct.ScmObj* %_95k47341, %struct.ScmObj** %stackaddr$prim57086, align 8
%stackaddr$prim57087 = alloca %struct.ScmObj*, align 8
%current_45args56324 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56323)
store volatile %struct.ScmObj* %current_45args56324, %struct.ScmObj** %stackaddr$prim57087, align 8
%stackaddr$prim57088 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56324)
store volatile %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$prim57088, align 8
%stackaddr$makeclosure57089 = alloca %struct.ScmObj*, align 8
%fptrToInt57090 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47731 to i64
%ae47731 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57090)
store volatile %struct.ScmObj* %ae47731, %struct.ScmObj** %stackaddr$makeclosure57089, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47731, %struct.ScmObj* %Ycmb47076, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47731, %struct.ScmObj* %_37foldr147097, i64 1)
%ae47732 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57091 = alloca %struct.ScmObj*, align 8
%fptrToInt57092 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47733 to i64
%ae47733 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57092)
store volatile %struct.ScmObj* %ae47733, %struct.ScmObj** %stackaddr$makeclosure57091, align 8
%argslist56991$ae477310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57093 = alloca %struct.ScmObj*, align 8
%argslist56991$ae477311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47733, %struct.ScmObj* %argslist56991$ae477310)
store volatile %struct.ScmObj* %argslist56991$ae477311, %struct.ScmObj** %stackaddr$prim57093, align 8
%stackaddr$prim57094 = alloca %struct.ScmObj*, align 8
%argslist56991$ae477312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47732, %struct.ScmObj* %argslist56991$ae477311)
store volatile %struct.ScmObj* %argslist56991$ae477312, %struct.ScmObj** %stackaddr$prim57094, align 8
%clofunc57095 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47731)
musttail call tailcc void %clofunc57095(%struct.ScmObj* %ae47731, %struct.ScmObj* %argslist56991$ae477312)
ret void
}

define tailcc void @proc_clo$ae47731(%struct.ScmObj* %env$ae47731,%struct.ScmObj* %current_45args56326) {
%stackaddr$env-ref57096 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47731, i64 0)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref57096
%stackaddr$env-ref57097 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47731, i64 1)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref57097
%stackaddr$prim57098 = alloca %struct.ScmObj*, align 8
%_95k47342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56326)
store volatile %struct.ScmObj* %_95k47342, %struct.ScmObj** %stackaddr$prim57098, align 8
%stackaddr$prim57099 = alloca %struct.ScmObj*, align 8
%current_45args56327 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56326)
store volatile %struct.ScmObj* %current_45args56327, %struct.ScmObj** %stackaddr$prim57099, align 8
%stackaddr$prim57100 = alloca %struct.ScmObj*, align 8
%anf_45bind47221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56327)
store volatile %struct.ScmObj* %anf_45bind47221, %struct.ScmObj** %stackaddr$prim57100, align 8
%stackaddr$makeclosure57101 = alloca %struct.ScmObj*, align 8
%fptrToInt57102 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47826 to i64
%ae47826 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57102)
store volatile %struct.ScmObj* %ae47826, %struct.ScmObj** %stackaddr$makeclosure57101, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47826, %struct.ScmObj* %Ycmb47076, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47826, %struct.ScmObj* %_37foldr147097, i64 1)
%argslist56972$Ycmb470760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57103 = alloca %struct.ScmObj*, align 8
%argslist56972$Ycmb470761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47221, %struct.ScmObj* %argslist56972$Ycmb470760)
store volatile %struct.ScmObj* %argslist56972$Ycmb470761, %struct.ScmObj** %stackaddr$prim57103, align 8
%stackaddr$prim57104 = alloca %struct.ScmObj*, align 8
%argslist56972$Ycmb470762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47826, %struct.ScmObj* %argslist56972$Ycmb470761)
store volatile %struct.ScmObj* %argslist56972$Ycmb470762, %struct.ScmObj** %stackaddr$prim57104, align 8
%clofunc57105 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47076)
musttail call tailcc void %clofunc57105(%struct.ScmObj* %Ycmb47076, %struct.ScmObj* %argslist56972$Ycmb470762)
ret void
}

define tailcc void @proc_clo$ae47826(%struct.ScmObj* %env$ae47826,%struct.ScmObj* %current_45args56329) {
%stackaddr$env-ref57106 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47826, i64 0)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref57106
%stackaddr$env-ref57107 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47826, i64 1)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref57107
%stackaddr$prim57108 = alloca %struct.ScmObj*, align 8
%_95k47343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56329)
store volatile %struct.ScmObj* %_95k47343, %struct.ScmObj** %stackaddr$prim57108, align 8
%stackaddr$prim57109 = alloca %struct.ScmObj*, align 8
%current_45args56330 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56329)
store volatile %struct.ScmObj* %current_45args56330, %struct.ScmObj** %stackaddr$prim57109, align 8
%stackaddr$prim57110 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56330)
store volatile %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$prim57110, align 8
%stackaddr$makeclosure57111 = alloca %struct.ScmObj*, align 8
%fptrToInt57112 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47828 to i64
%ae47828 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57112)
store volatile %struct.ScmObj* %ae47828, %struct.ScmObj** %stackaddr$makeclosure57111, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47828, %struct.ScmObj* %_37map147093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47828, %struct.ScmObj* %Ycmb47076, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47828, %struct.ScmObj* %_37foldr147097, i64 2)
%ae47829 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57113 = alloca %struct.ScmObj*, align 8
%fptrToInt57114 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47830 to i64
%ae47830 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57114)
store volatile %struct.ScmObj* %ae47830, %struct.ScmObj** %stackaddr$makeclosure57113, align 8
%argslist56971$ae478280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57115 = alloca %struct.ScmObj*, align 8
%argslist56971$ae478281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47830, %struct.ScmObj* %argslist56971$ae478280)
store volatile %struct.ScmObj* %argslist56971$ae478281, %struct.ScmObj** %stackaddr$prim57115, align 8
%stackaddr$prim57116 = alloca %struct.ScmObj*, align 8
%argslist56971$ae478282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47829, %struct.ScmObj* %argslist56971$ae478281)
store volatile %struct.ScmObj* %argslist56971$ae478282, %struct.ScmObj** %stackaddr$prim57116, align 8
%clofunc57117 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47828)
musttail call tailcc void %clofunc57117(%struct.ScmObj* %ae47828, %struct.ScmObj* %argslist56971$ae478282)
ret void
}

define tailcc void @proc_clo$ae47828(%struct.ScmObj* %env$ae47828,%struct.ScmObj* %current_45args56332) {
%stackaddr$env-ref57118 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47828, i64 0)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref57118
%stackaddr$env-ref57119 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47828, i64 1)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref57119
%stackaddr$env-ref57120 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47828, i64 2)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref57120
%stackaddr$prim57121 = alloca %struct.ScmObj*, align 8
%_95k47344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56332)
store volatile %struct.ScmObj* %_95k47344, %struct.ScmObj** %stackaddr$prim57121, align 8
%stackaddr$prim57122 = alloca %struct.ScmObj*, align 8
%current_45args56333 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56332)
store volatile %struct.ScmObj* %current_45args56333, %struct.ScmObj** %stackaddr$prim57122, align 8
%stackaddr$prim57123 = alloca %struct.ScmObj*, align 8
%anf_45bind47228 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56333)
store volatile %struct.ScmObj* %anf_45bind47228, %struct.ScmObj** %stackaddr$prim57123, align 8
%stackaddr$makeclosure57124 = alloca %struct.ScmObj*, align 8
%fptrToInt57125 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47976 to i64
%ae47976 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57125)
store volatile %struct.ScmObj* %ae47976, %struct.ScmObj** %stackaddr$makeclosure57124, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47976, %struct.ScmObj* %_37map147093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47976, %struct.ScmObj* %Ycmb47076, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47976, %struct.ScmObj* %_37foldr147097, i64 2)
%argslist56955$Ycmb470760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57126 = alloca %struct.ScmObj*, align 8
%argslist56955$Ycmb470761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47228, %struct.ScmObj* %argslist56955$Ycmb470760)
store volatile %struct.ScmObj* %argslist56955$Ycmb470761, %struct.ScmObj** %stackaddr$prim57126, align 8
%stackaddr$prim57127 = alloca %struct.ScmObj*, align 8
%argslist56955$Ycmb470762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47976, %struct.ScmObj* %argslist56955$Ycmb470761)
store volatile %struct.ScmObj* %argslist56955$Ycmb470762, %struct.ScmObj** %stackaddr$prim57127, align 8
%clofunc57128 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47076)
musttail call tailcc void %clofunc57128(%struct.ScmObj* %Ycmb47076, %struct.ScmObj* %argslist56955$Ycmb470762)
ret void
}

define tailcc void @proc_clo$ae47976(%struct.ScmObj* %env$ae47976,%struct.ScmObj* %current_45args56335) {
%stackaddr$env-ref57129 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47976, i64 0)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref57129
%stackaddr$env-ref57130 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47976, i64 1)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref57130
%stackaddr$env-ref57131 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47976, i64 2)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref57131
%stackaddr$prim57132 = alloca %struct.ScmObj*, align 8
%_95k47345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56335)
store volatile %struct.ScmObj* %_95k47345, %struct.ScmObj** %stackaddr$prim57132, align 8
%stackaddr$prim57133 = alloca %struct.ScmObj*, align 8
%current_45args56336 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56335)
store volatile %struct.ScmObj* %current_45args56336, %struct.ScmObj** %stackaddr$prim57133, align 8
%stackaddr$prim57134 = alloca %struct.ScmObj*, align 8
%_37take47089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56336)
store volatile %struct.ScmObj* %_37take47089, %struct.ScmObj** %stackaddr$prim57134, align 8
%stackaddr$makeclosure57135 = alloca %struct.ScmObj*, align 8
%fptrToInt57136 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47978 to i64
%ae47978 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57136)
store volatile %struct.ScmObj* %ae47978, %struct.ScmObj** %stackaddr$makeclosure57135, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47978, %struct.ScmObj* %_37map147093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47978, %struct.ScmObj* %Ycmb47076, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47978, %struct.ScmObj* %_37take47089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47978, %struct.ScmObj* %_37foldr147097, i64 3)
%ae47979 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57137 = alloca %struct.ScmObj*, align 8
%fptrToInt57138 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47980 to i64
%ae47980 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57138)
store volatile %struct.ScmObj* %ae47980, %struct.ScmObj** %stackaddr$makeclosure57137, align 8
%argslist56954$ae479780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57139 = alloca %struct.ScmObj*, align 8
%argslist56954$ae479781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47980, %struct.ScmObj* %argslist56954$ae479780)
store volatile %struct.ScmObj* %argslist56954$ae479781, %struct.ScmObj** %stackaddr$prim57139, align 8
%stackaddr$prim57140 = alloca %struct.ScmObj*, align 8
%argslist56954$ae479782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47979, %struct.ScmObj* %argslist56954$ae479781)
store volatile %struct.ScmObj* %argslist56954$ae479782, %struct.ScmObj** %stackaddr$prim57140, align 8
%clofunc57141 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47978)
musttail call tailcc void %clofunc57141(%struct.ScmObj* %ae47978, %struct.ScmObj* %argslist56954$ae479782)
ret void
}

define tailcc void @proc_clo$ae47978(%struct.ScmObj* %env$ae47978,%struct.ScmObj* %current_45args56338) {
%stackaddr$env-ref57142 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47978, i64 0)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref57142
%stackaddr$env-ref57143 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47978, i64 1)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref57143
%stackaddr$env-ref57144 = alloca %struct.ScmObj*, align 8
%_37take47089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47978, i64 2)
store %struct.ScmObj* %_37take47089, %struct.ScmObj** %stackaddr$env-ref57144
%stackaddr$env-ref57145 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47978, i64 3)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref57145
%stackaddr$prim57146 = alloca %struct.ScmObj*, align 8
%_95k47346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56338)
store volatile %struct.ScmObj* %_95k47346, %struct.ScmObj** %stackaddr$prim57146, align 8
%stackaddr$prim57147 = alloca %struct.ScmObj*, align 8
%current_45args56339 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56338)
store volatile %struct.ScmObj* %current_45args56339, %struct.ScmObj** %stackaddr$prim57147, align 8
%stackaddr$prim57148 = alloca %struct.ScmObj*, align 8
%anf_45bind47232 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56339)
store volatile %struct.ScmObj* %anf_45bind47232, %struct.ScmObj** %stackaddr$prim57148, align 8
%stackaddr$makeclosure57149 = alloca %struct.ScmObj*, align 8
%fptrToInt57150 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48059 to i64
%ae48059 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57150)
store volatile %struct.ScmObj* %ae48059, %struct.ScmObj** %stackaddr$makeclosure57149, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48059, %struct.ScmObj* %_37map147093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48059, %struct.ScmObj* %Ycmb47076, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48059, %struct.ScmObj* %_37take47089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48059, %struct.ScmObj* %_37foldr147097, i64 3)
%argslist56940$Ycmb470760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57151 = alloca %struct.ScmObj*, align 8
%argslist56940$Ycmb470761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47232, %struct.ScmObj* %argslist56940$Ycmb470760)
store volatile %struct.ScmObj* %argslist56940$Ycmb470761, %struct.ScmObj** %stackaddr$prim57151, align 8
%stackaddr$prim57152 = alloca %struct.ScmObj*, align 8
%argslist56940$Ycmb470762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48059, %struct.ScmObj* %argslist56940$Ycmb470761)
store volatile %struct.ScmObj* %argslist56940$Ycmb470762, %struct.ScmObj** %stackaddr$prim57152, align 8
%clofunc57153 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47076)
musttail call tailcc void %clofunc57153(%struct.ScmObj* %Ycmb47076, %struct.ScmObj* %argslist56940$Ycmb470762)
ret void
}

define tailcc void @proc_clo$ae48059(%struct.ScmObj* %env$ae48059,%struct.ScmObj* %current_45args56341) {
%stackaddr$env-ref57154 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48059, i64 0)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref57154
%stackaddr$env-ref57155 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48059, i64 1)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref57155
%stackaddr$env-ref57156 = alloca %struct.ScmObj*, align 8
%_37take47089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48059, i64 2)
store %struct.ScmObj* %_37take47089, %struct.ScmObj** %stackaddr$env-ref57156
%stackaddr$env-ref57157 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48059, i64 3)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref57157
%stackaddr$prim57158 = alloca %struct.ScmObj*, align 8
%_95k47347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56341)
store volatile %struct.ScmObj* %_95k47347, %struct.ScmObj** %stackaddr$prim57158, align 8
%stackaddr$prim57159 = alloca %struct.ScmObj*, align 8
%current_45args56342 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56341)
store volatile %struct.ScmObj* %current_45args56342, %struct.ScmObj** %stackaddr$prim57159, align 8
%stackaddr$prim57160 = alloca %struct.ScmObj*, align 8
%_37length47086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56342)
store volatile %struct.ScmObj* %_37length47086, %struct.ScmObj** %stackaddr$prim57160, align 8
%stackaddr$makeclosure57161 = alloca %struct.ScmObj*, align 8
%fptrToInt57162 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48061 to i64
%ae48061 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57162)
store volatile %struct.ScmObj* %ae48061, %struct.ScmObj** %stackaddr$makeclosure57161, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48061, %struct.ScmObj* %_37map147093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48061, %struct.ScmObj* %Ycmb47076, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48061, %struct.ScmObj* %_37take47089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48061, %struct.ScmObj* %_37length47086, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48061, %struct.ScmObj* %_37foldr147097, i64 4)
%ae48062 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57163 = alloca %struct.ScmObj*, align 8
%fptrToInt57164 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48063 to i64
%ae48063 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57164)
store volatile %struct.ScmObj* %ae48063, %struct.ScmObj** %stackaddr$makeclosure57163, align 8
%argslist56939$ae480610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57165 = alloca %struct.ScmObj*, align 8
%argslist56939$ae480611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48063, %struct.ScmObj* %argslist56939$ae480610)
store volatile %struct.ScmObj* %argslist56939$ae480611, %struct.ScmObj** %stackaddr$prim57165, align 8
%stackaddr$prim57166 = alloca %struct.ScmObj*, align 8
%argslist56939$ae480612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48062, %struct.ScmObj* %argslist56939$ae480611)
store volatile %struct.ScmObj* %argslist56939$ae480612, %struct.ScmObj** %stackaddr$prim57166, align 8
%clofunc57167 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48061)
musttail call tailcc void %clofunc57167(%struct.ScmObj* %ae48061, %struct.ScmObj* %argslist56939$ae480612)
ret void
}

define tailcc void @proc_clo$ae48061(%struct.ScmObj* %env$ae48061,%struct.ScmObj* %current_45args56344) {
%stackaddr$env-ref57168 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48061, i64 0)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref57168
%stackaddr$env-ref57169 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48061, i64 1)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref57169
%stackaddr$env-ref57170 = alloca %struct.ScmObj*, align 8
%_37take47089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48061, i64 2)
store %struct.ScmObj* %_37take47089, %struct.ScmObj** %stackaddr$env-ref57170
%stackaddr$env-ref57171 = alloca %struct.ScmObj*, align 8
%_37length47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48061, i64 3)
store %struct.ScmObj* %_37length47086, %struct.ScmObj** %stackaddr$env-ref57171
%stackaddr$env-ref57172 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48061, i64 4)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref57172
%stackaddr$prim57173 = alloca %struct.ScmObj*, align 8
%_95k47348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56344)
store volatile %struct.ScmObj* %_95k47348, %struct.ScmObj** %stackaddr$prim57173, align 8
%stackaddr$prim57174 = alloca %struct.ScmObj*, align 8
%current_45args56345 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56344)
store volatile %struct.ScmObj* %current_45args56345, %struct.ScmObj** %stackaddr$prim57174, align 8
%stackaddr$prim57175 = alloca %struct.ScmObj*, align 8
%anf_45bind47237 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56345)
store volatile %struct.ScmObj* %anf_45bind47237, %struct.ScmObj** %stackaddr$prim57175, align 8
%stackaddr$makeclosure57176 = alloca %struct.ScmObj*, align 8
%fptrToInt57177 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48138 to i64
%ae48138 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57177)
store volatile %struct.ScmObj* %ae48138, %struct.ScmObj** %stackaddr$makeclosure57176, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48138, %struct.ScmObj* %_37map147093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48138, %struct.ScmObj* %Ycmb47076, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48138, %struct.ScmObj* %_37take47089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48138, %struct.ScmObj* %_37length47086, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48138, %struct.ScmObj* %_37foldr147097, i64 4)
%argslist56923$Ycmb470760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57178 = alloca %struct.ScmObj*, align 8
%argslist56923$Ycmb470761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47237, %struct.ScmObj* %argslist56923$Ycmb470760)
store volatile %struct.ScmObj* %argslist56923$Ycmb470761, %struct.ScmObj** %stackaddr$prim57178, align 8
%stackaddr$prim57179 = alloca %struct.ScmObj*, align 8
%argslist56923$Ycmb470762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48138, %struct.ScmObj* %argslist56923$Ycmb470761)
store volatile %struct.ScmObj* %argslist56923$Ycmb470762, %struct.ScmObj** %stackaddr$prim57179, align 8
%clofunc57180 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47076)
musttail call tailcc void %clofunc57180(%struct.ScmObj* %Ycmb47076, %struct.ScmObj* %argslist56923$Ycmb470762)
ret void
}

define tailcc void @proc_clo$ae48138(%struct.ScmObj* %env$ae48138,%struct.ScmObj* %current_45args56347) {
%stackaddr$env-ref57181 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48138, i64 0)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref57181
%stackaddr$env-ref57182 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48138, i64 1)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref57182
%stackaddr$env-ref57183 = alloca %struct.ScmObj*, align 8
%_37take47089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48138, i64 2)
store %struct.ScmObj* %_37take47089, %struct.ScmObj** %stackaddr$env-ref57183
%stackaddr$env-ref57184 = alloca %struct.ScmObj*, align 8
%_37length47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48138, i64 3)
store %struct.ScmObj* %_37length47086, %struct.ScmObj** %stackaddr$env-ref57184
%stackaddr$env-ref57185 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48138, i64 4)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref57185
%stackaddr$prim57186 = alloca %struct.ScmObj*, align 8
%_95k47349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56347)
store volatile %struct.ScmObj* %_95k47349, %struct.ScmObj** %stackaddr$prim57186, align 8
%stackaddr$prim57187 = alloca %struct.ScmObj*, align 8
%current_45args56348 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56347)
store volatile %struct.ScmObj* %current_45args56348, %struct.ScmObj** %stackaddr$prim57187, align 8
%stackaddr$prim57188 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56348)
store volatile %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$prim57188, align 8
%stackaddr$makeclosure57189 = alloca %struct.ScmObj*, align 8
%fptrToInt57190 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48140 to i64
%ae48140 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt57190)
store volatile %struct.ScmObj* %ae48140, %struct.ScmObj** %stackaddr$makeclosure57189, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48140, %struct.ScmObj* %_37foldr147097, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48140, %struct.ScmObj* %_37foldl147081, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48140, %struct.ScmObj* %_37map147093, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48140, %struct.ScmObj* %Ycmb47076, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48140, %struct.ScmObj* %_37take47089, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48140, %struct.ScmObj* %_37length47086, i64 5)
%ae48141 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57191 = alloca %struct.ScmObj*, align 8
%fptrToInt57192 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48142 to i64
%ae48142 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57192)
store volatile %struct.ScmObj* %ae48142, %struct.ScmObj** %stackaddr$makeclosure57191, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48142, %struct.ScmObj* %_37foldl147081, i64 0)
%argslist56922$ae481400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57193 = alloca %struct.ScmObj*, align 8
%argslist56922$ae481401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48142, %struct.ScmObj* %argslist56922$ae481400)
store volatile %struct.ScmObj* %argslist56922$ae481401, %struct.ScmObj** %stackaddr$prim57193, align 8
%stackaddr$prim57194 = alloca %struct.ScmObj*, align 8
%argslist56922$ae481402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48141, %struct.ScmObj* %argslist56922$ae481401)
store volatile %struct.ScmObj* %argslist56922$ae481402, %struct.ScmObj** %stackaddr$prim57194, align 8
%clofunc57195 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48140)
musttail call tailcc void %clofunc57195(%struct.ScmObj* %ae48140, %struct.ScmObj* %argslist56922$ae481402)
ret void
}

define tailcc void @proc_clo$ae48140(%struct.ScmObj* %env$ae48140,%struct.ScmObj* %current_45args56350) {
%stackaddr$env-ref57196 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48140, i64 0)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref57196
%stackaddr$env-ref57197 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48140, i64 1)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref57197
%stackaddr$env-ref57198 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48140, i64 2)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref57198
%stackaddr$env-ref57199 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48140, i64 3)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref57199
%stackaddr$env-ref57200 = alloca %struct.ScmObj*, align 8
%_37take47089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48140, i64 4)
store %struct.ScmObj* %_37take47089, %struct.ScmObj** %stackaddr$env-ref57200
%stackaddr$env-ref57201 = alloca %struct.ScmObj*, align 8
%_37length47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48140, i64 5)
store %struct.ScmObj* %_37length47086, %struct.ScmObj** %stackaddr$env-ref57201
%stackaddr$prim57202 = alloca %struct.ScmObj*, align 8
%_95k47350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56350)
store volatile %struct.ScmObj* %_95k47350, %struct.ScmObj** %stackaddr$prim57202, align 8
%stackaddr$prim57203 = alloca %struct.ScmObj*, align 8
%current_45args56351 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56350)
store volatile %struct.ScmObj* %current_45args56351, %struct.ScmObj** %stackaddr$prim57203, align 8
%stackaddr$prim57204 = alloca %struct.ScmObj*, align 8
%_37last47119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56351)
store volatile %struct.ScmObj* %_37last47119, %struct.ScmObj** %stackaddr$prim57204, align 8
%stackaddr$makeclosure57205 = alloca %struct.ScmObj*, align 8
%fptrToInt57206 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48194 to i64
%ae48194 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57206)
store volatile %struct.ScmObj* %ae48194, %struct.ScmObj** %stackaddr$makeclosure57205, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48194, %struct.ScmObj* %_37foldr147097, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48194, %struct.ScmObj* %_37foldl147081, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48194, %struct.ScmObj* %_37map147093, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48194, %struct.ScmObj* %Ycmb47076, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48194, %struct.ScmObj* %_37last47119, i64 4)
%ae48195 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57207 = alloca %struct.ScmObj*, align 8
%fptrToInt57208 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48196 to i64
%ae48196 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57208)
store volatile %struct.ScmObj* %ae48196, %struct.ScmObj** %stackaddr$makeclosure57207, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48196, %struct.ScmObj* %_37take47089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48196, %struct.ScmObj* %_37length47086, i64 1)
%argslist56908$ae481940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57209 = alloca %struct.ScmObj*, align 8
%argslist56908$ae481941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48196, %struct.ScmObj* %argslist56908$ae481940)
store volatile %struct.ScmObj* %argslist56908$ae481941, %struct.ScmObj** %stackaddr$prim57209, align 8
%stackaddr$prim57210 = alloca %struct.ScmObj*, align 8
%argslist56908$ae481942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48195, %struct.ScmObj* %argslist56908$ae481941)
store volatile %struct.ScmObj* %argslist56908$ae481942, %struct.ScmObj** %stackaddr$prim57210, align 8
%clofunc57211 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48194)
musttail call tailcc void %clofunc57211(%struct.ScmObj* %ae48194, %struct.ScmObj* %argslist56908$ae481942)
ret void
}

define tailcc void @proc_clo$ae48194(%struct.ScmObj* %env$ae48194,%struct.ScmObj* %current_45args56353) {
%stackaddr$env-ref57212 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48194, i64 0)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref57212
%stackaddr$env-ref57213 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48194, i64 1)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref57213
%stackaddr$env-ref57214 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48194, i64 2)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref57214
%stackaddr$env-ref57215 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48194, i64 3)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref57215
%stackaddr$env-ref57216 = alloca %struct.ScmObj*, align 8
%_37last47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48194, i64 4)
store %struct.ScmObj* %_37last47119, %struct.ScmObj** %stackaddr$env-ref57216
%stackaddr$prim57217 = alloca %struct.ScmObj*, align 8
%_95k47351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56353)
store volatile %struct.ScmObj* %_95k47351, %struct.ScmObj** %stackaddr$prim57217, align 8
%stackaddr$prim57218 = alloca %struct.ScmObj*, align 8
%current_45args56354 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56353)
store volatile %struct.ScmObj* %current_45args56354, %struct.ScmObj** %stackaddr$prim57218, align 8
%stackaddr$prim57219 = alloca %struct.ScmObj*, align 8
%_37drop_45right47116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56354)
store volatile %struct.ScmObj* %_37drop_45right47116, %struct.ScmObj** %stackaddr$prim57219, align 8
%stackaddr$makeclosure57220 = alloca %struct.ScmObj*, align 8
%fptrToInt57221 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48224 to i64
%ae48224 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57221)
store volatile %struct.ScmObj* %ae48224, %struct.ScmObj** %stackaddr$makeclosure57220, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48224, %struct.ScmObj* %_37foldr147097, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48224, %struct.ScmObj* %_37foldl147081, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48224, %struct.ScmObj* %Ycmb47076, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48224, %struct.ScmObj* %_37last47119, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48224, %struct.ScmObj* %_37drop_45right47116, i64 4)
%ae48225 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57222 = alloca %struct.ScmObj*, align 8
%fptrToInt57223 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48226 to i64
%ae48226 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57223)
store volatile %struct.ScmObj* %ae48226, %struct.ScmObj** %stackaddr$makeclosure57222, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48226, %struct.ScmObj* %_37map147093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48226, %struct.ScmObj* %_37foldr147097, i64 1)
%argslist56898$ae482240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57224 = alloca %struct.ScmObj*, align 8
%argslist56898$ae482241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48226, %struct.ScmObj* %argslist56898$ae482240)
store volatile %struct.ScmObj* %argslist56898$ae482241, %struct.ScmObj** %stackaddr$prim57224, align 8
%stackaddr$prim57225 = alloca %struct.ScmObj*, align 8
%argslist56898$ae482242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48225, %struct.ScmObj* %argslist56898$ae482241)
store volatile %struct.ScmObj* %argslist56898$ae482242, %struct.ScmObj** %stackaddr$prim57225, align 8
%clofunc57226 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48224)
musttail call tailcc void %clofunc57226(%struct.ScmObj* %ae48224, %struct.ScmObj* %argslist56898$ae482242)
ret void
}

define tailcc void @proc_clo$ae48224(%struct.ScmObj* %env$ae48224,%struct.ScmObj* %current_45args56356) {
%stackaddr$env-ref57227 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48224, i64 0)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref57227
%stackaddr$env-ref57228 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48224, i64 1)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref57228
%stackaddr$env-ref57229 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48224, i64 2)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref57229
%stackaddr$env-ref57230 = alloca %struct.ScmObj*, align 8
%_37last47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48224, i64 3)
store %struct.ScmObj* %_37last47119, %struct.ScmObj** %stackaddr$env-ref57230
%stackaddr$env-ref57231 = alloca %struct.ScmObj*, align 8
%_37drop_45right47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48224, i64 4)
store %struct.ScmObj* %_37drop_45right47116, %struct.ScmObj** %stackaddr$env-ref57231
%stackaddr$prim57232 = alloca %struct.ScmObj*, align 8
%_95k47352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56356)
store volatile %struct.ScmObj* %_95k47352, %struct.ScmObj** %stackaddr$prim57232, align 8
%stackaddr$prim57233 = alloca %struct.ScmObj*, align 8
%current_45args56357 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56356)
store volatile %struct.ScmObj* %current_45args56357, %struct.ScmObj** %stackaddr$prim57233, align 8
%stackaddr$prim57234 = alloca %struct.ScmObj*, align 8
%anf_45bind47253 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56357)
store volatile %struct.ScmObj* %anf_45bind47253, %struct.ScmObj** %stackaddr$prim57234, align 8
%stackaddr$makeclosure57235 = alloca %struct.ScmObj*, align 8
%fptrToInt57236 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48608 to i64
%ae48608 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57236)
store volatile %struct.ScmObj* %ae48608, %struct.ScmObj** %stackaddr$makeclosure57235, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48608, %struct.ScmObj* %_37foldr147097, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48608, %struct.ScmObj* %_37foldl147081, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48608, %struct.ScmObj* %Ycmb47076, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48608, %struct.ScmObj* %_37last47119, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48608, %struct.ScmObj* %_37drop_45right47116, i64 4)
%argslist56838$Ycmb470760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57237 = alloca %struct.ScmObj*, align 8
%argslist56838$Ycmb470761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47253, %struct.ScmObj* %argslist56838$Ycmb470760)
store volatile %struct.ScmObj* %argslist56838$Ycmb470761, %struct.ScmObj** %stackaddr$prim57237, align 8
%stackaddr$prim57238 = alloca %struct.ScmObj*, align 8
%argslist56838$Ycmb470762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48608, %struct.ScmObj* %argslist56838$Ycmb470761)
store volatile %struct.ScmObj* %argslist56838$Ycmb470762, %struct.ScmObj** %stackaddr$prim57238, align 8
%clofunc57239 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47076)
musttail call tailcc void %clofunc57239(%struct.ScmObj* %Ycmb47076, %struct.ScmObj* %argslist56838$Ycmb470762)
ret void
}

define tailcc void @proc_clo$ae48608(%struct.ScmObj* %env$ae48608,%struct.ScmObj* %current_45args56359) {
%stackaddr$env-ref57240 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48608, i64 0)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref57240
%stackaddr$env-ref57241 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48608, i64 1)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref57241
%stackaddr$env-ref57242 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48608, i64 2)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref57242
%stackaddr$env-ref57243 = alloca %struct.ScmObj*, align 8
%_37last47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48608, i64 3)
store %struct.ScmObj* %_37last47119, %struct.ScmObj** %stackaddr$env-ref57243
%stackaddr$env-ref57244 = alloca %struct.ScmObj*, align 8
%_37drop_45right47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48608, i64 4)
store %struct.ScmObj* %_37drop_45right47116, %struct.ScmObj** %stackaddr$env-ref57244
%stackaddr$prim57245 = alloca %struct.ScmObj*, align 8
%_95k47353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56359)
store volatile %struct.ScmObj* %_95k47353, %struct.ScmObj** %stackaddr$prim57245, align 8
%stackaddr$prim57246 = alloca %struct.ScmObj*, align 8
%current_45args56360 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56359)
store volatile %struct.ScmObj* %current_45args56360, %struct.ScmObj** %stackaddr$prim57246, align 8
%stackaddr$prim57247 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56360)
store volatile %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$prim57247, align 8
%stackaddr$makeclosure57248 = alloca %struct.ScmObj*, align 8
%fptrToInt57249 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48610 to i64
%ae48610 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt57249)
store volatile %struct.ScmObj* %ae48610, %struct.ScmObj** %stackaddr$makeclosure57248, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48610, %struct.ScmObj* %_37foldr147097, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48610, %struct.ScmObj* %_37foldl147081, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48610, %struct.ScmObj* %Ycmb47076, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48610, %struct.ScmObj* %_37last47119, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48610, %struct.ScmObj* %_37foldr47102, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48610, %struct.ScmObj* %_37drop_45right47116, i64 5)
%ae48611 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57250 = alloca %struct.ScmObj*, align 8
%fptrToInt57251 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48612 to i64
%ae48612 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57251)
store volatile %struct.ScmObj* %ae48612, %struct.ScmObj** %stackaddr$makeclosure57250, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48612, %struct.ScmObj* %_37foldr147097, i64 0)
%argslist56837$ae486100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57252 = alloca %struct.ScmObj*, align 8
%argslist56837$ae486101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48612, %struct.ScmObj* %argslist56837$ae486100)
store volatile %struct.ScmObj* %argslist56837$ae486101, %struct.ScmObj** %stackaddr$prim57252, align 8
%stackaddr$prim57253 = alloca %struct.ScmObj*, align 8
%argslist56837$ae486102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48611, %struct.ScmObj* %argslist56837$ae486101)
store volatile %struct.ScmObj* %argslist56837$ae486102, %struct.ScmObj** %stackaddr$prim57253, align 8
%clofunc57254 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48610)
musttail call tailcc void %clofunc57254(%struct.ScmObj* %ae48610, %struct.ScmObj* %argslist56837$ae486102)
ret void
}

define tailcc void @proc_clo$ae48610(%struct.ScmObj* %env$ae48610,%struct.ScmObj* %current_45args56362) {
%stackaddr$env-ref57255 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48610, i64 0)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref57255
%stackaddr$env-ref57256 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48610, i64 1)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref57256
%stackaddr$env-ref57257 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48610, i64 2)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref57257
%stackaddr$env-ref57258 = alloca %struct.ScmObj*, align 8
%_37last47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48610, i64 3)
store %struct.ScmObj* %_37last47119, %struct.ScmObj** %stackaddr$env-ref57258
%stackaddr$env-ref57259 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48610, i64 4)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref57259
%stackaddr$env-ref57260 = alloca %struct.ScmObj*, align 8
%_37drop_45right47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48610, i64 5)
store %struct.ScmObj* %_37drop_45right47116, %struct.ScmObj** %stackaddr$env-ref57260
%stackaddr$prim57261 = alloca %struct.ScmObj*, align 8
%_95k47354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56362)
store volatile %struct.ScmObj* %_95k47354, %struct.ScmObj** %stackaddr$prim57261, align 8
%stackaddr$prim57262 = alloca %struct.ScmObj*, align 8
%current_45args56363 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56362)
store volatile %struct.ScmObj* %current_45args56363, %struct.ScmObj** %stackaddr$prim57262, align 8
%stackaddr$prim57263 = alloca %struct.ScmObj*, align 8
%_37map147128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56363)
store volatile %struct.ScmObj* %_37map147128, %struct.ScmObj** %stackaddr$prim57263, align 8
%stackaddr$makeclosure57264 = alloca %struct.ScmObj*, align 8
%fptrToInt57265 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48687 to i64
%ae48687 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt57265)
store volatile %struct.ScmObj* %ae48687, %struct.ScmObj** %stackaddr$makeclosure57264, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48687, %struct.ScmObj* %_37foldr147097, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48687, %struct.ScmObj* %_37foldl147081, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48687, %struct.ScmObj* %Ycmb47076, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48687, %struct.ScmObj* %_37foldr47102, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48687, %struct.ScmObj* %_37map147128, i64 4)
%ae48688 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57266 = alloca %struct.ScmObj*, align 8
%fptrToInt57267 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48689 to i64
%ae48689 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57267)
store volatile %struct.ScmObj* %ae48689, %struct.ScmObj** %stackaddr$makeclosure57266, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48689, %struct.ScmObj* %_37last47119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48689, %struct.ScmObj* %_37foldr47102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48689, %struct.ScmObj* %_37drop_45right47116, i64 2)
%argslist56818$ae486870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57268 = alloca %struct.ScmObj*, align 8
%argslist56818$ae486871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48689, %struct.ScmObj* %argslist56818$ae486870)
store volatile %struct.ScmObj* %argslist56818$ae486871, %struct.ScmObj** %stackaddr$prim57268, align 8
%stackaddr$prim57269 = alloca %struct.ScmObj*, align 8
%argslist56818$ae486872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48688, %struct.ScmObj* %argslist56818$ae486871)
store volatile %struct.ScmObj* %argslist56818$ae486872, %struct.ScmObj** %stackaddr$prim57269, align 8
%clofunc57270 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48687)
musttail call tailcc void %clofunc57270(%struct.ScmObj* %ae48687, %struct.ScmObj* %argslist56818$ae486872)
ret void
}

define tailcc void @proc_clo$ae48687(%struct.ScmObj* %env$ae48687,%struct.ScmObj* %current_45args56365) {
%stackaddr$env-ref57271 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48687, i64 0)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref57271
%stackaddr$env-ref57272 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48687, i64 1)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref57272
%stackaddr$env-ref57273 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48687, i64 2)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref57273
%stackaddr$env-ref57274 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48687, i64 3)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref57274
%stackaddr$env-ref57275 = alloca %struct.ScmObj*, align 8
%_37map147128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48687, i64 4)
store %struct.ScmObj* %_37map147128, %struct.ScmObj** %stackaddr$env-ref57275
%stackaddr$prim57276 = alloca %struct.ScmObj*, align 8
%_95k47355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56365)
store volatile %struct.ScmObj* %_95k47355, %struct.ScmObj** %stackaddr$prim57276, align 8
%stackaddr$prim57277 = alloca %struct.ScmObj*, align 8
%current_45args56366 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56365)
store volatile %struct.ScmObj* %current_45args56366, %struct.ScmObj** %stackaddr$prim57277, align 8
%stackaddr$prim57278 = alloca %struct.ScmObj*, align 8
%_37map47123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56366)
store volatile %struct.ScmObj* %_37map47123, %struct.ScmObj** %stackaddr$prim57278, align 8
%stackaddr$makeclosure57279 = alloca %struct.ScmObj*, align 8
%fptrToInt57280 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48833 to i64
%ae48833 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57280)
store volatile %struct.ScmObj* %ae48833, %struct.ScmObj** %stackaddr$makeclosure57279, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48833, %struct.ScmObj* %Ycmb47076, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48833, %struct.ScmObj* %_37foldl147081, i64 1)
%ae48834 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57281 = alloca %struct.ScmObj*, align 8
%fptrToInt57282 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48835 to i64
%ae48835 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57282)
store volatile %struct.ScmObj* %ae48835, %struct.ScmObj** %stackaddr$makeclosure57281, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48835, %struct.ScmObj* %_37foldr47102, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48835, %struct.ScmObj* %_37foldr147097, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48835, %struct.ScmObj* %_37map147128, i64 2)
%argslist56801$ae488330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57283 = alloca %struct.ScmObj*, align 8
%argslist56801$ae488331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48835, %struct.ScmObj* %argslist56801$ae488330)
store volatile %struct.ScmObj* %argslist56801$ae488331, %struct.ScmObj** %stackaddr$prim57283, align 8
%stackaddr$prim57284 = alloca %struct.ScmObj*, align 8
%argslist56801$ae488332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48834, %struct.ScmObj* %argslist56801$ae488331)
store volatile %struct.ScmObj* %argslist56801$ae488332, %struct.ScmObj** %stackaddr$prim57284, align 8
%clofunc57285 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48833)
musttail call tailcc void %clofunc57285(%struct.ScmObj* %ae48833, %struct.ScmObj* %argslist56801$ae488332)
ret void
}

define tailcc void @proc_clo$ae48833(%struct.ScmObj* %env$ae48833,%struct.ScmObj* %current_45args56368) {
%stackaddr$env-ref57286 = alloca %struct.ScmObj*, align 8
%Ycmb47076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48833, i64 0)
store %struct.ScmObj* %Ycmb47076, %struct.ScmObj** %stackaddr$env-ref57286
%stackaddr$env-ref57287 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48833, i64 1)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref57287
%stackaddr$prim57288 = alloca %struct.ScmObj*, align 8
%_95k47356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56368)
store volatile %struct.ScmObj* %_95k47356, %struct.ScmObj** %stackaddr$prim57288, align 8
%stackaddr$prim57289 = alloca %struct.ScmObj*, align 8
%current_45args56369 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56368)
store volatile %struct.ScmObj* %current_45args56369, %struct.ScmObj** %stackaddr$prim57289, align 8
%stackaddr$prim57290 = alloca %struct.ScmObj*, align 8
%anf_45bind47273 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56369)
store volatile %struct.ScmObj* %anf_45bind47273, %struct.ScmObj** %stackaddr$prim57290, align 8
%stackaddr$makeclosure57291 = alloca %struct.ScmObj*, align 8
%fptrToInt57292 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49225 to i64
%ae49225 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57292)
store volatile %struct.ScmObj* %ae49225, %struct.ScmObj** %stackaddr$makeclosure57291, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49225, %struct.ScmObj* %_37foldl147081, i64 0)
%argslist56741$Ycmb470760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57293 = alloca %struct.ScmObj*, align 8
%argslist56741$Ycmb470761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47273, %struct.ScmObj* %argslist56741$Ycmb470760)
store volatile %struct.ScmObj* %argslist56741$Ycmb470761, %struct.ScmObj** %stackaddr$prim57293, align 8
%stackaddr$prim57294 = alloca %struct.ScmObj*, align 8
%argslist56741$Ycmb470762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49225, %struct.ScmObj* %argslist56741$Ycmb470761)
store volatile %struct.ScmObj* %argslist56741$Ycmb470762, %struct.ScmObj** %stackaddr$prim57294, align 8
%clofunc57295 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47076)
musttail call tailcc void %clofunc57295(%struct.ScmObj* %Ycmb47076, %struct.ScmObj* %argslist56741$Ycmb470762)
ret void
}

define tailcc void @proc_clo$ae49225(%struct.ScmObj* %env$ae49225,%struct.ScmObj* %current_45args56371) {
%stackaddr$env-ref57296 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49225, i64 0)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref57296
%stackaddr$prim57297 = alloca %struct.ScmObj*, align 8
%_95k47357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56371)
store volatile %struct.ScmObj* %_95k47357, %struct.ScmObj** %stackaddr$prim57297, align 8
%stackaddr$prim57298 = alloca %struct.ScmObj*, align 8
%current_45args56372 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56371)
store volatile %struct.ScmObj* %current_45args56372, %struct.ScmObj** %stackaddr$prim57298, align 8
%stackaddr$prim57299 = alloca %struct.ScmObj*, align 8
%_37foldl47179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56372)
store volatile %struct.ScmObj* %_37foldl47179, %struct.ScmObj** %stackaddr$prim57299, align 8
%stackaddr$makeclosure57300 = alloca %struct.ScmObj*, align 8
%fptrToInt57301 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49227 to i64
%ae49227 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57301)
store volatile %struct.ScmObj* %ae49227, %struct.ScmObj** %stackaddr$makeclosure57300, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49227, %struct.ScmObj* %_37foldl147081, i64 0)
%ae49228 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57302 = alloca %struct.ScmObj*, align 8
%fptrToInt57303 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49229 to i64
%ae49229 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57303)
store volatile %struct.ScmObj* %ae49229, %struct.ScmObj** %stackaddr$makeclosure57302, align 8
%argslist56740$ae492270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57304 = alloca %struct.ScmObj*, align 8
%argslist56740$ae492271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49229, %struct.ScmObj* %argslist56740$ae492270)
store volatile %struct.ScmObj* %argslist56740$ae492271, %struct.ScmObj** %stackaddr$prim57304, align 8
%stackaddr$prim57305 = alloca %struct.ScmObj*, align 8
%argslist56740$ae492272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49228, %struct.ScmObj* %argslist56740$ae492271)
store volatile %struct.ScmObj* %argslist56740$ae492272, %struct.ScmObj** %stackaddr$prim57305, align 8
%clofunc57306 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49227)
musttail call tailcc void %clofunc57306(%struct.ScmObj* %ae49227, %struct.ScmObj* %argslist56740$ae492272)
ret void
}

define tailcc void @proc_clo$ae49227(%struct.ScmObj* %env$ae49227,%struct.ScmObj* %current_45args56374) {
%stackaddr$env-ref57307 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49227, i64 0)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref57307
%stackaddr$prim57308 = alloca %struct.ScmObj*, align 8
%_95k47358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56374)
store volatile %struct.ScmObj* %_95k47358, %struct.ScmObj** %stackaddr$prim57308, align 8
%stackaddr$prim57309 = alloca %struct.ScmObj*, align 8
%current_45args56375 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56374)
store volatile %struct.ScmObj* %current_45args56375, %struct.ScmObj** %stackaddr$prim57309, align 8
%stackaddr$prim57310 = alloca %struct.ScmObj*, align 8
%_37_6247176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56375)
store volatile %struct.ScmObj* %_37_6247176, %struct.ScmObj** %stackaddr$prim57310, align 8
%stackaddr$makeclosure57311 = alloca %struct.ScmObj*, align 8
%fptrToInt57312 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49251 to i64
%ae49251 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57312)
store volatile %struct.ScmObj* %ae49251, %struct.ScmObj** %stackaddr$makeclosure57311, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49251, %struct.ScmObj* %_37foldl147081, i64 0)
%ae49252 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57313 = alloca %struct.ScmObj*, align 8
%fptrToInt57314 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49253 to i64
%ae49253 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57314)
store volatile %struct.ScmObj* %ae49253, %struct.ScmObj** %stackaddr$makeclosure57313, align 8
%argslist56734$ae492510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57315 = alloca %struct.ScmObj*, align 8
%argslist56734$ae492511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49253, %struct.ScmObj* %argslist56734$ae492510)
store volatile %struct.ScmObj* %argslist56734$ae492511, %struct.ScmObj** %stackaddr$prim57315, align 8
%stackaddr$prim57316 = alloca %struct.ScmObj*, align 8
%argslist56734$ae492512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49252, %struct.ScmObj* %argslist56734$ae492511)
store volatile %struct.ScmObj* %argslist56734$ae492512, %struct.ScmObj** %stackaddr$prim57316, align 8
%clofunc57317 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49251)
musttail call tailcc void %clofunc57317(%struct.ScmObj* %ae49251, %struct.ScmObj* %argslist56734$ae492512)
ret void
}

define tailcc void @proc_clo$ae49251(%struct.ScmObj* %env$ae49251,%struct.ScmObj* %current_45args56377) {
%stackaddr$env-ref57318 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49251, i64 0)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref57318
%stackaddr$prim57319 = alloca %struct.ScmObj*, align 8
%_95k47359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56377)
store volatile %struct.ScmObj* %_95k47359, %struct.ScmObj** %stackaddr$prim57319, align 8
%stackaddr$prim57320 = alloca %struct.ScmObj*, align 8
%current_45args56378 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56377)
store volatile %struct.ScmObj* %current_45args56378, %struct.ScmObj** %stackaddr$prim57320, align 8
%stackaddr$prim57321 = alloca %struct.ScmObj*, align 8
%_37_62_6147173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56378)
store volatile %struct.ScmObj* %_37_62_6147173, %struct.ScmObj** %stackaddr$prim57321, align 8
%ae49275 = call %struct.ScmObj* @const_init_int(i64 1)
%ae49276 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57322 = alloca %struct.ScmObj*, align 8
%_37append47169 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49275, %struct.ScmObj* %ae49276)
store volatile %struct.ScmObj* %_37append47169, %struct.ScmObj** %stackaddr$prim57322, align 8
%stackaddr$makeclosure57323 = alloca %struct.ScmObj*, align 8
%fptrToInt57324 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49277 to i64
%ae49277 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57324)
store volatile %struct.ScmObj* %ae49277, %struct.ScmObj** %stackaddr$makeclosure57323, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49277, %struct.ScmObj* %_37append47169, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49277, %struct.ScmObj* %_37foldl147081, i64 1)
%ae49278 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57325 = alloca %struct.ScmObj*, align 8
%fptrToInt57326 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49279 to i64
%ae49279 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57326)
store volatile %struct.ScmObj* %ae49279, %struct.ScmObj** %stackaddr$makeclosure57325, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49279, %struct.ScmObj* %_37append47169, i64 0)
%argslist56728$ae492770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57327 = alloca %struct.ScmObj*, align 8
%argslist56728$ae492771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49279, %struct.ScmObj* %argslist56728$ae492770)
store volatile %struct.ScmObj* %argslist56728$ae492771, %struct.ScmObj** %stackaddr$prim57327, align 8
%stackaddr$prim57328 = alloca %struct.ScmObj*, align 8
%argslist56728$ae492772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49278, %struct.ScmObj* %argslist56728$ae492771)
store volatile %struct.ScmObj* %argslist56728$ae492772, %struct.ScmObj** %stackaddr$prim57328, align 8
%clofunc57329 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49277)
musttail call tailcc void %clofunc57329(%struct.ScmObj* %ae49277, %struct.ScmObj* %argslist56728$ae492772)
ret void
}

define tailcc void @proc_clo$ae49277(%struct.ScmObj* %env$ae49277,%struct.ScmObj* %current_45args56380) {
%stackaddr$env-ref57330 = alloca %struct.ScmObj*, align 8
%_37append47169 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49277, i64 0)
store %struct.ScmObj* %_37append47169, %struct.ScmObj** %stackaddr$env-ref57330
%stackaddr$env-ref57331 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49277, i64 1)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref57331
%stackaddr$prim57332 = alloca %struct.ScmObj*, align 8
%_95k47360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56380)
store volatile %struct.ScmObj* %_95k47360, %struct.ScmObj** %stackaddr$prim57332, align 8
%stackaddr$prim57333 = alloca %struct.ScmObj*, align 8
%current_45args56381 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56380)
store volatile %struct.ScmObj* %current_45args56381, %struct.ScmObj** %stackaddr$prim57333, align 8
%stackaddr$prim57334 = alloca %struct.ScmObj*, align 8
%anf_45bind47281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56381)
store volatile %struct.ScmObj* %anf_45bind47281, %struct.ScmObj** %stackaddr$prim57334, align 8
%ae49345 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57335 = alloca %struct.ScmObj*, align 8
%_95047170 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append47169, %struct.ScmObj* %ae49345, %struct.ScmObj* %anf_45bind47281)
store volatile %struct.ScmObj* %_95047170, %struct.ScmObj** %stackaddr$prim57335, align 8
%ae49348 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57336 = alloca %struct.ScmObj*, align 8
%_37append47168 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47169, %struct.ScmObj* %ae49348)
store volatile %struct.ScmObj* %_37append47168, %struct.ScmObj** %stackaddr$prim57336, align 8
%stackaddr$makeclosure57337 = alloca %struct.ScmObj*, align 8
%fptrToInt57338 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49349 to i64
%ae49349 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57338)
store volatile %struct.ScmObj* %ae49349, %struct.ScmObj** %stackaddr$makeclosure57337, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49349, %struct.ScmObj* %_37foldl147081, i64 0)
%ae49350 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57339 = alloca %struct.ScmObj*, align 8
%fptrToInt57340 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49351 to i64
%ae49351 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57340)
store volatile %struct.ScmObj* %ae49351, %struct.ScmObj** %stackaddr$makeclosure57339, align 8
%argslist56717$ae493490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57341 = alloca %struct.ScmObj*, align 8
%argslist56717$ae493491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49351, %struct.ScmObj* %argslist56717$ae493490)
store volatile %struct.ScmObj* %argslist56717$ae493491, %struct.ScmObj** %stackaddr$prim57341, align 8
%stackaddr$prim57342 = alloca %struct.ScmObj*, align 8
%argslist56717$ae493492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49350, %struct.ScmObj* %argslist56717$ae493491)
store volatile %struct.ScmObj* %argslist56717$ae493492, %struct.ScmObj** %stackaddr$prim57342, align 8
%clofunc57343 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49349)
musttail call tailcc void %clofunc57343(%struct.ScmObj* %ae49349, %struct.ScmObj* %argslist56717$ae493492)
ret void
}

define tailcc void @proc_clo$ae49349(%struct.ScmObj* %env$ae49349,%struct.ScmObj* %current_45args56383) {
%stackaddr$env-ref57344 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49349, i64 0)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref57344
%stackaddr$prim57345 = alloca %struct.ScmObj*, align 8
%_95k47361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56383)
store volatile %struct.ScmObj* %_95k47361, %struct.ScmObj** %stackaddr$prim57345, align 8
%stackaddr$prim57346 = alloca %struct.ScmObj*, align 8
%current_45args56384 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56383)
store volatile %struct.ScmObj* %current_45args56384, %struct.ScmObj** %stackaddr$prim57346, align 8
%stackaddr$prim57347 = alloca %struct.ScmObj*, align 8
%_37list_6347161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56384)
store volatile %struct.ScmObj* %_37list_6347161, %struct.ScmObj** %stackaddr$prim57347, align 8
%stackaddr$makeclosure57348 = alloca %struct.ScmObj*, align 8
%fptrToInt57349 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49765 to i64
%ae49765 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57349)
store volatile %struct.ScmObj* %ae49765, %struct.ScmObj** %stackaddr$makeclosure57348, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49765, %struct.ScmObj* %_37foldl147081, i64 0)
%ae49766 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57350 = alloca %struct.ScmObj*, align 8
%fptrToInt57351 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49767 to i64
%ae49767 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57351)
store volatile %struct.ScmObj* %ae49767, %struct.ScmObj** %stackaddr$makeclosure57350, align 8
%argslist56692$ae497650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57352 = alloca %struct.ScmObj*, align 8
%argslist56692$ae497651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49767, %struct.ScmObj* %argslist56692$ae497650)
store volatile %struct.ScmObj* %argslist56692$ae497651, %struct.ScmObj** %stackaddr$prim57352, align 8
%stackaddr$prim57353 = alloca %struct.ScmObj*, align 8
%argslist56692$ae497652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49766, %struct.ScmObj* %argslist56692$ae497651)
store volatile %struct.ScmObj* %argslist56692$ae497652, %struct.ScmObj** %stackaddr$prim57353, align 8
%clofunc57354 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49765)
musttail call tailcc void %clofunc57354(%struct.ScmObj* %ae49765, %struct.ScmObj* %argslist56692$ae497652)
ret void
}

define tailcc void @proc_clo$ae49765(%struct.ScmObj* %env$ae49765,%struct.ScmObj* %current_45args56386) {
%stackaddr$env-ref57355 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49765, i64 0)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref57355
%stackaddr$prim57356 = alloca %struct.ScmObj*, align 8
%_95k47362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56386)
store volatile %struct.ScmObj* %_95k47362, %struct.ScmObj** %stackaddr$prim57356, align 8
%stackaddr$prim57357 = alloca %struct.ScmObj*, align 8
%current_45args56387 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56386)
store volatile %struct.ScmObj* %current_45args56387, %struct.ScmObj** %stackaddr$prim57357, align 8
%stackaddr$prim57358 = alloca %struct.ScmObj*, align 8
%_37drop47152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56387)
store volatile %struct.ScmObj* %_37drop47152, %struct.ScmObj** %stackaddr$prim57358, align 8
%stackaddr$makeclosure57359 = alloca %struct.ScmObj*, align 8
%fptrToInt57360 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50301 to i64
%ae50301 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57360)
store volatile %struct.ScmObj* %ae50301, %struct.ScmObj** %stackaddr$makeclosure57359, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50301, %struct.ScmObj* %_37foldl147081, i64 0)
%ae50302 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57361 = alloca %struct.ScmObj*, align 8
%fptrToInt57362 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50303 to i64
%ae50303 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57362)
store volatile %struct.ScmObj* %ae50303, %struct.ScmObj** %stackaddr$makeclosure57361, align 8
%argslist56668$ae503010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57363 = alloca %struct.ScmObj*, align 8
%argslist56668$ae503011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50303, %struct.ScmObj* %argslist56668$ae503010)
store volatile %struct.ScmObj* %argslist56668$ae503011, %struct.ScmObj** %stackaddr$prim57363, align 8
%stackaddr$prim57364 = alloca %struct.ScmObj*, align 8
%argslist56668$ae503012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50302, %struct.ScmObj* %argslist56668$ae503011)
store volatile %struct.ScmObj* %argslist56668$ae503012, %struct.ScmObj** %stackaddr$prim57364, align 8
%clofunc57365 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50301)
musttail call tailcc void %clofunc57365(%struct.ScmObj* %ae50301, %struct.ScmObj* %argslist56668$ae503012)
ret void
}

define tailcc void @proc_clo$ae50301(%struct.ScmObj* %env$ae50301,%struct.ScmObj* %current_45args56389) {
%stackaddr$env-ref57366 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50301, i64 0)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref57366
%stackaddr$prim57367 = alloca %struct.ScmObj*, align 8
%_95k47363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56389)
store volatile %struct.ScmObj* %_95k47363, %struct.ScmObj** %stackaddr$prim57367, align 8
%stackaddr$prim57368 = alloca %struct.ScmObj*, align 8
%current_45args56390 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56389)
store volatile %struct.ScmObj* %current_45args56390, %struct.ScmObj** %stackaddr$prim57368, align 8
%stackaddr$prim57369 = alloca %struct.ScmObj*, align 8
%_37memv47145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56390)
store volatile %struct.ScmObj* %_37memv47145, %struct.ScmObj** %stackaddr$prim57369, align 8
%stackaddr$makeclosure57370 = alloca %struct.ScmObj*, align 8
%fptrToInt57371 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50705 to i64
%ae50705 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57371)
store volatile %struct.ScmObj* %ae50705, %struct.ScmObj** %stackaddr$makeclosure57370, align 8
%ae50706 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57372 = alloca %struct.ScmObj*, align 8
%fptrToInt57373 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50707 to i64
%ae50707 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57373)
store volatile %struct.ScmObj* %ae50707, %struct.ScmObj** %stackaddr$makeclosure57372, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50707, %struct.ScmObj* %_37foldl147081, i64 0)
%argslist56642$ae507050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57374 = alloca %struct.ScmObj*, align 8
%argslist56642$ae507051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50707, %struct.ScmObj* %argslist56642$ae507050)
store volatile %struct.ScmObj* %argslist56642$ae507051, %struct.ScmObj** %stackaddr$prim57374, align 8
%stackaddr$prim57375 = alloca %struct.ScmObj*, align 8
%argslist56642$ae507052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50706, %struct.ScmObj* %argslist56642$ae507051)
store volatile %struct.ScmObj* %argslist56642$ae507052, %struct.ScmObj** %stackaddr$prim57375, align 8
%clofunc57376 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50705)
musttail call tailcc void %clofunc57376(%struct.ScmObj* %ae50705, %struct.ScmObj* %argslist56642$ae507052)
ret void
}

define tailcc void @proc_clo$ae50705(%struct.ScmObj* %env$ae50705,%struct.ScmObj* %current_45args56392) {
%stackaddr$prim57377 = alloca %struct.ScmObj*, align 8
%_95k47364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56392)
store volatile %struct.ScmObj* %_95k47364, %struct.ScmObj** %stackaddr$prim57377, align 8
%stackaddr$prim57378 = alloca %struct.ScmObj*, align 8
%current_45args56393 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56392)
store volatile %struct.ScmObj* %current_45args56393, %struct.ScmObj** %stackaddr$prim57378, align 8
%stackaddr$prim57379 = alloca %struct.ScmObj*, align 8
%_37_4747141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56393)
store volatile %struct.ScmObj* %_37_4747141, %struct.ScmObj** %stackaddr$prim57379, align 8
%stackaddr$makeclosure57380 = alloca %struct.ScmObj*, align 8
%fptrToInt57381 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50803 to i64
%ae50803 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57381)
store volatile %struct.ScmObj* %ae50803, %struct.ScmObj** %stackaddr$makeclosure57380, align 8
%ae50804 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57382 = alloca %struct.ScmObj*, align 8
%fptrToInt57383 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50805 to i64
%ae50805 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57383)
store volatile %struct.ScmObj* %ae50805, %struct.ScmObj** %stackaddr$makeclosure57382, align 8
%argslist56629$ae508030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57384 = alloca %struct.ScmObj*, align 8
%argslist56629$ae508031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50805, %struct.ScmObj* %argslist56629$ae508030)
store volatile %struct.ScmObj* %argslist56629$ae508031, %struct.ScmObj** %stackaddr$prim57384, align 8
%stackaddr$prim57385 = alloca %struct.ScmObj*, align 8
%argslist56629$ae508032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50804, %struct.ScmObj* %argslist56629$ae508031)
store volatile %struct.ScmObj* %argslist56629$ae508032, %struct.ScmObj** %stackaddr$prim57385, align 8
%clofunc57386 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50803)
musttail call tailcc void %clofunc57386(%struct.ScmObj* %ae50803, %struct.ScmObj* %argslist56629$ae508032)
ret void
}

define tailcc void @proc_clo$ae50803(%struct.ScmObj* %env$ae50803,%struct.ScmObj* %current_45args56395) {
%stackaddr$prim57387 = alloca %struct.ScmObj*, align 8
%_95k47365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56395)
store volatile %struct.ScmObj* %_95k47365, %struct.ScmObj** %stackaddr$prim57387, align 8
%stackaddr$prim57388 = alloca %struct.ScmObj*, align 8
%current_45args56396 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56395)
store volatile %struct.ScmObj* %current_45args56396, %struct.ScmObj** %stackaddr$prim57388, align 8
%stackaddr$prim57389 = alloca %struct.ScmObj*, align 8
%_37first47139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56396)
store volatile %struct.ScmObj* %_37first47139, %struct.ScmObj** %stackaddr$prim57389, align 8
%stackaddr$makeclosure57390 = alloca %struct.ScmObj*, align 8
%fptrToInt57391 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50823 to i64
%ae50823 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57391)
store volatile %struct.ScmObj* %ae50823, %struct.ScmObj** %stackaddr$makeclosure57390, align 8
%ae50824 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57392 = alloca %struct.ScmObj*, align 8
%fptrToInt57393 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50825 to i64
%ae50825 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57393)
store volatile %struct.ScmObj* %ae50825, %struct.ScmObj** %stackaddr$makeclosure57392, align 8
%argslist56624$ae508230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57394 = alloca %struct.ScmObj*, align 8
%argslist56624$ae508231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50825, %struct.ScmObj* %argslist56624$ae508230)
store volatile %struct.ScmObj* %argslist56624$ae508231, %struct.ScmObj** %stackaddr$prim57394, align 8
%stackaddr$prim57395 = alloca %struct.ScmObj*, align 8
%argslist56624$ae508232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50824, %struct.ScmObj* %argslist56624$ae508231)
store volatile %struct.ScmObj* %argslist56624$ae508232, %struct.ScmObj** %stackaddr$prim57395, align 8
%clofunc57396 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50823)
musttail call tailcc void %clofunc57396(%struct.ScmObj* %ae50823, %struct.ScmObj* %argslist56624$ae508232)
ret void
}

define tailcc void @proc_clo$ae50823(%struct.ScmObj* %env$ae50823,%struct.ScmObj* %current_45args56398) {
%stackaddr$prim57397 = alloca %struct.ScmObj*, align 8
%_95k47366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56398)
store volatile %struct.ScmObj* %_95k47366, %struct.ScmObj** %stackaddr$prim57397, align 8
%stackaddr$prim57398 = alloca %struct.ScmObj*, align 8
%current_45args56399 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56398)
store volatile %struct.ScmObj* %current_45args56399, %struct.ScmObj** %stackaddr$prim57398, align 8
%stackaddr$prim57399 = alloca %struct.ScmObj*, align 8
%_37second47137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56399)
store volatile %struct.ScmObj* %_37second47137, %struct.ScmObj** %stackaddr$prim57399, align 8
%stackaddr$makeclosure57400 = alloca %struct.ScmObj*, align 8
%fptrToInt57401 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50845 to i64
%ae50845 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57401)
store volatile %struct.ScmObj* %ae50845, %struct.ScmObj** %stackaddr$makeclosure57400, align 8
%ae50846 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57402 = alloca %struct.ScmObj*, align 8
%fptrToInt57403 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50847 to i64
%ae50847 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57403)
store volatile %struct.ScmObj* %ae50847, %struct.ScmObj** %stackaddr$makeclosure57402, align 8
%argslist56619$ae508450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57404 = alloca %struct.ScmObj*, align 8
%argslist56619$ae508451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50847, %struct.ScmObj* %argslist56619$ae508450)
store volatile %struct.ScmObj* %argslist56619$ae508451, %struct.ScmObj** %stackaddr$prim57404, align 8
%stackaddr$prim57405 = alloca %struct.ScmObj*, align 8
%argslist56619$ae508452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50846, %struct.ScmObj* %argslist56619$ae508451)
store volatile %struct.ScmObj* %argslist56619$ae508452, %struct.ScmObj** %stackaddr$prim57405, align 8
%clofunc57406 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50845)
musttail call tailcc void %clofunc57406(%struct.ScmObj* %ae50845, %struct.ScmObj* %argslist56619$ae508452)
ret void
}

define tailcc void @proc_clo$ae50845(%struct.ScmObj* %env$ae50845,%struct.ScmObj* %current_45args56401) {
%stackaddr$prim57407 = alloca %struct.ScmObj*, align 8
%_95k47367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56401)
store volatile %struct.ScmObj* %_95k47367, %struct.ScmObj** %stackaddr$prim57407, align 8
%stackaddr$prim57408 = alloca %struct.ScmObj*, align 8
%current_45args56402 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56401)
store volatile %struct.ScmObj* %current_45args56402, %struct.ScmObj** %stackaddr$prim57408, align 8
%stackaddr$prim57409 = alloca %struct.ScmObj*, align 8
%_37third47135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56402)
store volatile %struct.ScmObj* %_37third47135, %struct.ScmObj** %stackaddr$prim57409, align 8
%stackaddr$makeclosure57410 = alloca %struct.ScmObj*, align 8
%fptrToInt57411 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50869 to i64
%ae50869 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57411)
store volatile %struct.ScmObj* %ae50869, %struct.ScmObj** %stackaddr$makeclosure57410, align 8
%ae50870 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57412 = alloca %struct.ScmObj*, align 8
%fptrToInt57413 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50871 to i64
%ae50871 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57413)
store volatile %struct.ScmObj* %ae50871, %struct.ScmObj** %stackaddr$makeclosure57412, align 8
%argslist56614$ae508690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57414 = alloca %struct.ScmObj*, align 8
%argslist56614$ae508691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50871, %struct.ScmObj* %argslist56614$ae508690)
store volatile %struct.ScmObj* %argslist56614$ae508691, %struct.ScmObj** %stackaddr$prim57414, align 8
%stackaddr$prim57415 = alloca %struct.ScmObj*, align 8
%argslist56614$ae508692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50870, %struct.ScmObj* %argslist56614$ae508691)
store volatile %struct.ScmObj* %argslist56614$ae508692, %struct.ScmObj** %stackaddr$prim57415, align 8
%clofunc57416 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50869)
musttail call tailcc void %clofunc57416(%struct.ScmObj* %ae50869, %struct.ScmObj* %argslist56614$ae508692)
ret void
}

define tailcc void @proc_clo$ae50869(%struct.ScmObj* %env$ae50869,%struct.ScmObj* %current_45args56404) {
%stackaddr$prim57417 = alloca %struct.ScmObj*, align 8
%_95k47368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56404)
store volatile %struct.ScmObj* %_95k47368, %struct.ScmObj** %stackaddr$prim57417, align 8
%stackaddr$prim57418 = alloca %struct.ScmObj*, align 8
%current_45args56405 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56404)
store volatile %struct.ScmObj* %current_45args56405, %struct.ScmObj** %stackaddr$prim57418, align 8
%stackaddr$prim57419 = alloca %struct.ScmObj*, align 8
%_37fourth47133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56405)
store volatile %struct.ScmObj* %_37fourth47133, %struct.ScmObj** %stackaddr$prim57419, align 8
%stackaddr$makeclosure57420 = alloca %struct.ScmObj*, align 8
%fptrToInt57421 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50895 to i64
%ae50895 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57421)
store volatile %struct.ScmObj* %ae50895, %struct.ScmObj** %stackaddr$makeclosure57420, align 8
%ae50896 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57422 = alloca %struct.ScmObj*, align 8
%fptrToInt57423 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50897 to i64
%ae50897 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57423)
store volatile %struct.ScmObj* %ae50897, %struct.ScmObj** %stackaddr$makeclosure57422, align 8
%argslist56609$ae508950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57424 = alloca %struct.ScmObj*, align 8
%argslist56609$ae508951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50897, %struct.ScmObj* %argslist56609$ae508950)
store volatile %struct.ScmObj* %argslist56609$ae508951, %struct.ScmObj** %stackaddr$prim57424, align 8
%stackaddr$prim57425 = alloca %struct.ScmObj*, align 8
%argslist56609$ae508952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50896, %struct.ScmObj* %argslist56609$ae508951)
store volatile %struct.ScmObj* %argslist56609$ae508952, %struct.ScmObj** %stackaddr$prim57425, align 8
%clofunc57426 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50895)
musttail call tailcc void %clofunc57426(%struct.ScmObj* %ae50895, %struct.ScmObj* %argslist56609$ae508952)
ret void
}

define tailcc void @proc_clo$ae50895(%struct.ScmObj* %env$ae50895,%struct.ScmObj* %current_45args56407) {
%stackaddr$prim57427 = alloca %struct.ScmObj*, align 8
%_95k47369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56407)
store volatile %struct.ScmObj* %_95k47369, %struct.ScmObj** %stackaddr$prim57427, align 8
%stackaddr$prim57428 = alloca %struct.ScmObj*, align 8
%current_45args56408 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56407)
store volatile %struct.ScmObj* %current_45args56408, %struct.ScmObj** %stackaddr$prim57428, align 8
%stackaddr$prim57429 = alloca %struct.ScmObj*, align 8
%anf_45bind47317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56408)
store volatile %struct.ScmObj* %anf_45bind47317, %struct.ScmObj** %stackaddr$prim57429, align 8
%stackaddr$makeclosure57430 = alloca %struct.ScmObj*, align 8
%fptrToInt57431 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50919 to i64
%ae50919 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57431)
store volatile %struct.ScmObj* %ae50919, %struct.ScmObj** %stackaddr$makeclosure57430, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50919, %struct.ScmObj* %anf_45bind47317, i64 0)
%ae50920 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57432 = alloca %struct.ScmObj*, align 8
%fptrToInt57433 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50921 to i64
%ae50921 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57433)
store volatile %struct.ScmObj* %ae50921, %struct.ScmObj** %stackaddr$makeclosure57432, align 8
%argslist56607$ae509190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57434 = alloca %struct.ScmObj*, align 8
%argslist56607$ae509191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50921, %struct.ScmObj* %argslist56607$ae509190)
store volatile %struct.ScmObj* %argslist56607$ae509191, %struct.ScmObj** %stackaddr$prim57434, align 8
%stackaddr$prim57435 = alloca %struct.ScmObj*, align 8
%argslist56607$ae509192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50920, %struct.ScmObj* %argslist56607$ae509191)
store volatile %struct.ScmObj* %argslist56607$ae509192, %struct.ScmObj** %stackaddr$prim57435, align 8
%clofunc57436 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50919)
musttail call tailcc void %clofunc57436(%struct.ScmObj* %ae50919, %struct.ScmObj* %argslist56607$ae509192)
ret void
}

define tailcc void @proc_clo$ae50919(%struct.ScmObj* %env$ae50919,%struct.ScmObj* %current_45args56410) {
%stackaddr$env-ref57437 = alloca %struct.ScmObj*, align 8
%anf_45bind47317 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50919, i64 0)
store %struct.ScmObj* %anf_45bind47317, %struct.ScmObj** %stackaddr$env-ref57437
%stackaddr$prim57438 = alloca %struct.ScmObj*, align 8
%_95k47370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56410)
store volatile %struct.ScmObj* %_95k47370, %struct.ScmObj** %stackaddr$prim57438, align 8
%stackaddr$prim57439 = alloca %struct.ScmObj*, align 8
%current_45args56411 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56410)
store volatile %struct.ScmObj* %current_45args56411, %struct.ScmObj** %stackaddr$prim57439, align 8
%stackaddr$prim57440 = alloca %struct.ScmObj*, align 8
%anf_45bind47318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56411)
store volatile %struct.ScmObj* %anf_45bind47318, %struct.ScmObj** %stackaddr$prim57440, align 8
%stackaddr$makeclosure57441 = alloca %struct.ScmObj*, align 8
%fptrToInt57442 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50941 to i64
%ae50941 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57442)
store volatile %struct.ScmObj* %ae50941, %struct.ScmObj** %stackaddr$makeclosure57441, align 8
%ae50942 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae5094257443, i32 0, i32 0))
%ae50943 = call %struct.ScmObj* @const_init_false()
%argslist56602$anf_45bind473170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57444 = alloca %struct.ScmObj*, align 8
%argslist56602$anf_45bind473171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47318, %struct.ScmObj* %argslist56602$anf_45bind473170)
store volatile %struct.ScmObj* %argslist56602$anf_45bind473171, %struct.ScmObj** %stackaddr$prim57444, align 8
%stackaddr$prim57445 = alloca %struct.ScmObj*, align 8
%argslist56602$anf_45bind473172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50943, %struct.ScmObj* %argslist56602$anf_45bind473171)
store volatile %struct.ScmObj* %argslist56602$anf_45bind473172, %struct.ScmObj** %stackaddr$prim57445, align 8
%stackaddr$prim57446 = alloca %struct.ScmObj*, align 8
%argslist56602$anf_45bind473173 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50942, %struct.ScmObj* %argslist56602$anf_45bind473172)
store volatile %struct.ScmObj* %argslist56602$anf_45bind473173, %struct.ScmObj** %stackaddr$prim57446, align 8
%stackaddr$prim57447 = alloca %struct.ScmObj*, align 8
%argslist56602$anf_45bind473174 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50941, %struct.ScmObj* %argslist56602$anf_45bind473173)
store volatile %struct.ScmObj* %argslist56602$anf_45bind473174, %struct.ScmObj** %stackaddr$prim57447, align 8
%clofunc57448 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47317)
musttail call tailcc void %clofunc57448(%struct.ScmObj* %anf_45bind47317, %struct.ScmObj* %argslist56602$anf_45bind473174)
ret void
}

define tailcc void @proc_clo$ae50941(%struct.ScmObj* %env$ae50941,%struct.ScmObj* %current_45args56413) {
%stackaddr$prim57449 = alloca %struct.ScmObj*, align 8
%_95k47371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56413)
store volatile %struct.ScmObj* %_95k47371, %struct.ScmObj** %stackaddr$prim57449, align 8
%stackaddr$prim57450 = alloca %struct.ScmObj*, align 8
%current_45args56414 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56413)
store volatile %struct.ScmObj* %current_45args56414, %struct.ScmObj** %stackaddr$prim57450, align 8
%stackaddr$prim57451 = alloca %struct.ScmObj*, align 8
%thunk4707047195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56414)
store volatile %struct.ScmObj* %thunk4707047195, %struct.ScmObj** %stackaddr$prim57451, align 8
%stackaddr$makeclosure57452 = alloca %struct.ScmObj*, align 8
%fptrToInt57453 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50955 to i64
%ae50955 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57453)
store volatile %struct.ScmObj* %ae50955, %struct.ScmObj** %stackaddr$makeclosure57452, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50955, %struct.ScmObj* %thunk4707047195, i64 0)
%ae50956 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57454 = alloca %struct.ScmObj*, align 8
%fptrToInt57455 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50957 to i64
%ae50957 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57455)
store volatile %struct.ScmObj* %ae50957, %struct.ScmObj** %stackaddr$makeclosure57454, align 8
%argslist56601$ae509550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57456 = alloca %struct.ScmObj*, align 8
%argslist56601$ae509551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50957, %struct.ScmObj* %argslist56601$ae509550)
store volatile %struct.ScmObj* %argslist56601$ae509551, %struct.ScmObj** %stackaddr$prim57456, align 8
%stackaddr$prim57457 = alloca %struct.ScmObj*, align 8
%argslist56601$ae509552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50956, %struct.ScmObj* %argslist56601$ae509551)
store volatile %struct.ScmObj* %argslist56601$ae509552, %struct.ScmObj** %stackaddr$prim57457, align 8
%clofunc57458 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50955)
musttail call tailcc void %clofunc57458(%struct.ScmObj* %ae50955, %struct.ScmObj* %argslist56601$ae509552)
ret void
}

define tailcc void @proc_clo$ae50955(%struct.ScmObj* %env$ae50955,%struct.ScmObj* %current_45args56416) {
%stackaddr$env-ref57459 = alloca %struct.ScmObj*, align 8
%thunk4707047195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50955, i64 0)
store %struct.ScmObj* %thunk4707047195, %struct.ScmObj** %stackaddr$env-ref57459
%stackaddr$prim57460 = alloca %struct.ScmObj*, align 8
%_95k47372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56416)
store volatile %struct.ScmObj* %_95k47372, %struct.ScmObj** %stackaddr$prim57460, align 8
%stackaddr$prim57461 = alloca %struct.ScmObj*, align 8
%current_45args56417 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56416)
store volatile %struct.ScmObj* %current_45args56417, %struct.ScmObj** %stackaddr$prim57461, align 8
%stackaddr$prim57462 = alloca %struct.ScmObj*, align 8
%anf_45bind47323 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56417)
store volatile %struct.ScmObj* %anf_45bind47323, %struct.ScmObj** %stackaddr$prim57462, align 8
%stackaddr$makeclosure57463 = alloca %struct.ScmObj*, align 8
%fptrToInt57464 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51043 to i64
%ae51043 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57464)
store volatile %struct.ScmObj* %ae51043, %struct.ScmObj** %stackaddr$makeclosure57463, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51043, %struct.ScmObj* %thunk4707047195, i64 0)
%argslist56594$anf_45bind473230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57465 = alloca %struct.ScmObj*, align 8
%argslist56594$anf_45bind473231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4707047195, %struct.ScmObj* %argslist56594$anf_45bind473230)
store volatile %struct.ScmObj* %argslist56594$anf_45bind473231, %struct.ScmObj** %stackaddr$prim57465, align 8
%stackaddr$prim57466 = alloca %struct.ScmObj*, align 8
%argslist56594$anf_45bind473232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51043, %struct.ScmObj* %argslist56594$anf_45bind473231)
store volatile %struct.ScmObj* %argslist56594$anf_45bind473232, %struct.ScmObj** %stackaddr$prim57466, align 8
%clofunc57467 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47323)
musttail call tailcc void %clofunc57467(%struct.ScmObj* %anf_45bind47323, %struct.ScmObj* %argslist56594$anf_45bind473232)
ret void
}

define tailcc void @proc_clo$ae51043(%struct.ScmObj* %env$ae51043,%struct.ScmObj* %current_45args56419) {
%stackaddr$env-ref57468 = alloca %struct.ScmObj*, align 8
%thunk4707047195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51043, i64 0)
store %struct.ScmObj* %thunk4707047195, %struct.ScmObj** %stackaddr$env-ref57468
%stackaddr$prim57469 = alloca %struct.ScmObj*, align 8
%_95k47373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56419)
store volatile %struct.ScmObj* %_95k47373, %struct.ScmObj** %stackaddr$prim57469, align 8
%stackaddr$prim57470 = alloca %struct.ScmObj*, align 8
%current_45args56420 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56419)
store volatile %struct.ScmObj* %current_45args56420, %struct.ScmObj** %stackaddr$prim57470, align 8
%stackaddr$prim57471 = alloca %struct.ScmObj*, align 8
%anf_45bind47324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56420)
store volatile %struct.ScmObj* %anf_45bind47324, %struct.ScmObj** %stackaddr$prim57471, align 8
%truthy$cmp57472 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47324)
%cmp$cmp57472 = icmp eq i64 %truthy$cmp57472, 1
br i1 %cmp$cmp57472, label %truebranch$cmp57472, label %falsebranch$cmp57472
truebranch$cmp57472:
%ae51047 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57473 = alloca %struct.ScmObj*, align 8
%anf_45bind47325 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4707047195, %struct.ScmObj* %ae51047)
store volatile %struct.ScmObj* %anf_45bind47325, %struct.ScmObj** %stackaddr$prim57473, align 8
%truthy$cmp57474 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47325)
%cmp$cmp57474 = icmp eq i64 %truthy$cmp57474, 1
br i1 %cmp$cmp57474, label %truebranch$cmp57474, label %falsebranch$cmp57474
truebranch$cmp57474:
%ae51050 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim57475 = alloca %struct.ScmObj*, align 8
%cpsprim47382 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4707047195, %struct.ScmObj* %ae51050)
store volatile %struct.ScmObj* %cpsprim47382, %struct.ScmObj** %stackaddr$prim57475, align 8
%stackaddr$makeclosure57476 = alloca %struct.ScmObj*, align 8
%fptrToInt57477 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51051 to i64
%ae51051 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57477)
store volatile %struct.ScmObj* %ae51051, %struct.ScmObj** %stackaddr$makeclosure57476, align 8
%ae51052 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56463$ae510510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57478 = alloca %struct.ScmObj*, align 8
%argslist56463$ae510511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47382, %struct.ScmObj* %argslist56463$ae510510)
store volatile %struct.ScmObj* %argslist56463$ae510511, %struct.ScmObj** %stackaddr$prim57478, align 8
%stackaddr$prim57479 = alloca %struct.ScmObj*, align 8
%argslist56463$ae510512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51052, %struct.ScmObj* %argslist56463$ae510511)
store volatile %struct.ScmObj* %argslist56463$ae510512, %struct.ScmObj** %stackaddr$prim57479, align 8
%clofunc57480 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51051)
musttail call tailcc void %clofunc57480(%struct.ScmObj* %ae51051, %struct.ScmObj* %argslist56463$ae510512)
ret void
falsebranch$cmp57474:
%ae51440 = call %struct.ScmObj* @const_init_int(i64 1)
%ae51441 = call %struct.ScmObj* @const_init_true()
%stackaddr$prim57481 = alloca %struct.ScmObj*, align 8
%t4707247199 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4707047195, %struct.ScmObj* %ae51440, %struct.ScmObj* %ae51441)
store volatile %struct.ScmObj* %t4707247199, %struct.ScmObj** %stackaddr$prim57481, align 8
%ae51443 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim57482 = alloca %struct.ScmObj*, align 8
%anf_45bind47326 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4707047195, %struct.ScmObj* %ae51443)
store volatile %struct.ScmObj* %anf_45bind47326, %struct.ScmObj** %stackaddr$prim57482, align 8
%stackaddr$makeclosure57483 = alloca %struct.ScmObj*, align 8
%fptrToInt57484 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51445 to i64
%ae51445 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57484)
store volatile %struct.ScmObj* %ae51445, %struct.ScmObj** %stackaddr$makeclosure57483, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51445, %struct.ScmObj* %thunk4707047195, i64 0)
%ae51446 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @global$sym$ae5144657485, i32 0, i32 0))
%argslist56509$anf_45bind473260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57486 = alloca %struct.ScmObj*, align 8
%argslist56509$anf_45bind473261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51446, %struct.ScmObj* %argslist56509$anf_45bind473260)
store volatile %struct.ScmObj* %argslist56509$anf_45bind473261, %struct.ScmObj** %stackaddr$prim57486, align 8
%stackaddr$prim57487 = alloca %struct.ScmObj*, align 8
%argslist56509$anf_45bind473262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51445, %struct.ScmObj* %argslist56509$anf_45bind473261)
store volatile %struct.ScmObj* %argslist56509$anf_45bind473262, %struct.ScmObj** %stackaddr$prim57487, align 8
%clofunc57488 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47326)
musttail call tailcc void %clofunc57488(%struct.ScmObj* %anf_45bind47326, %struct.ScmObj* %argslist56509$anf_45bind473262)
ret void
falsebranch$cmp57472:
%stackaddr$prim57489 = alloca %struct.ScmObj*, align 8
%anf_45bind47327 = call %struct.ScmObj* @prim_number_63(%struct.ScmObj* %thunk4707047195)
store volatile %struct.ScmObj* %anf_45bind47327, %struct.ScmObj** %stackaddr$prim57489, align 8
%truthy$cmp57490 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47327)
%cmp$cmp57490 = icmp eq i64 %truthy$cmp57490, 1
br i1 %cmp$cmp57490, label %truebranch$cmp57490, label %falsebranch$cmp57490
truebranch$cmp57490:
%stackaddr$makeclosure57491 = alloca %struct.ScmObj*, align 8
%fptrToInt57492 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52345 to i64
%ae52345 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57492)
store volatile %struct.ScmObj* %ae52345, %struct.ScmObj** %stackaddr$makeclosure57491, align 8
%ae52346 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56551$ae523450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57493 = alloca %struct.ScmObj*, align 8
%argslist56551$ae523451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4707047195, %struct.ScmObj* %argslist56551$ae523450)
store volatile %struct.ScmObj* %argslist56551$ae523451, %struct.ScmObj** %stackaddr$prim57493, align 8
%stackaddr$prim57494 = alloca %struct.ScmObj*, align 8
%argslist56551$ae523452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52346, %struct.ScmObj* %argslist56551$ae523451)
store volatile %struct.ScmObj* %argslist56551$ae523452, %struct.ScmObj** %stackaddr$prim57494, align 8
%clofunc57495 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52345)
musttail call tailcc void %clofunc57495(%struct.ScmObj* %ae52345, %struct.ScmObj* %argslist56551$ae523452)
ret void
falsebranch$cmp57490:
%stackaddr$makeclosure57496 = alloca %struct.ScmObj*, align 8
%fptrToInt57497 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52658 to i64
%ae52658 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57497)
store volatile %struct.ScmObj* %ae52658, %struct.ScmObj** %stackaddr$makeclosure57496, align 8
%ae52659 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52660 = call %struct.ScmObj* @const_init_string(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @global$str$ae5266057498, i32 0, i32 0))
%argslist56593$ae526580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57499 = alloca %struct.ScmObj*, align 8
%argslist56593$ae526581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52660, %struct.ScmObj* %argslist56593$ae526580)
store volatile %struct.ScmObj* %argslist56593$ae526581, %struct.ScmObj** %stackaddr$prim57499, align 8
%stackaddr$prim57500 = alloca %struct.ScmObj*, align 8
%argslist56593$ae526582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52659, %struct.ScmObj* %argslist56593$ae526581)
store volatile %struct.ScmObj* %argslist56593$ae526582, %struct.ScmObj** %stackaddr$prim57500, align 8
%clofunc57501 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52658)
musttail call tailcc void %clofunc57501(%struct.ScmObj* %ae52658, %struct.ScmObj* %argslist56593$ae526582)
ret void
}

define tailcc void @proc_clo$ae51051(%struct.ScmObj* %env$ae51051,%struct.ScmObj* %current_45args56422) {
%stackaddr$prim57502 = alloca %struct.ScmObj*, align 8
%_95k47374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56422)
store volatile %struct.ScmObj* %_95k47374, %struct.ScmObj** %stackaddr$prim57502, align 8
%stackaddr$prim57503 = alloca %struct.ScmObj*, align 8
%current_45args56423 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56422)
store volatile %struct.ScmObj* %current_45args56423, %struct.ScmObj** %stackaddr$prim57503, align 8
%stackaddr$prim57504 = alloca %struct.ScmObj*, align 8
%thunk4706847194 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56423)
store volatile %struct.ScmObj* %thunk4706847194, %struct.ScmObj** %stackaddr$prim57504, align 8
%stackaddr$makeclosure57505 = alloca %struct.ScmObj*, align 8
%fptrToInt57506 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51057 to i64
%ae51057 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57506)
store volatile %struct.ScmObj* %ae51057, %struct.ScmObj** %stackaddr$makeclosure57505, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51057, %struct.ScmObj* %thunk4706847194, i64 0)
%ae51058 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57507 = alloca %struct.ScmObj*, align 8
%fptrToInt57508 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51059 to i64
%ae51059 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57508)
store volatile %struct.ScmObj* %ae51059, %struct.ScmObj** %stackaddr$makeclosure57507, align 8
%argslist56462$ae510570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57509 = alloca %struct.ScmObj*, align 8
%argslist56462$ae510571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51059, %struct.ScmObj* %argslist56462$ae510570)
store volatile %struct.ScmObj* %argslist56462$ae510571, %struct.ScmObj** %stackaddr$prim57509, align 8
%stackaddr$prim57510 = alloca %struct.ScmObj*, align 8
%argslist56462$ae510572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51058, %struct.ScmObj* %argslist56462$ae510571)
store volatile %struct.ScmObj* %argslist56462$ae510572, %struct.ScmObj** %stackaddr$prim57510, align 8
%clofunc57511 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51057)
musttail call tailcc void %clofunc57511(%struct.ScmObj* %ae51057, %struct.ScmObj* %argslist56462$ae510572)
ret void
}

define tailcc void @proc_clo$ae51057(%struct.ScmObj* %env$ae51057,%struct.ScmObj* %current_45args56425) {
%stackaddr$env-ref57512 = alloca %struct.ScmObj*, align 8
%thunk4706847194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51057, i64 0)
store %struct.ScmObj* %thunk4706847194, %struct.ScmObj** %stackaddr$env-ref57512
%stackaddr$prim57513 = alloca %struct.ScmObj*, align 8
%_95k47375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56425)
store volatile %struct.ScmObj* %_95k47375, %struct.ScmObj** %stackaddr$prim57513, align 8
%stackaddr$prim57514 = alloca %struct.ScmObj*, align 8
%current_45args56426 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56425)
store volatile %struct.ScmObj* %current_45args56426, %struct.ScmObj** %stackaddr$prim57514, align 8
%stackaddr$prim57515 = alloca %struct.ScmObj*, align 8
%anf_45bind47332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56426)
store volatile %struct.ScmObj* %anf_45bind47332, %struct.ScmObj** %stackaddr$prim57515, align 8
%stackaddr$makeclosure57516 = alloca %struct.ScmObj*, align 8
%fptrToInt57517 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51145 to i64
%ae51145 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57517)
store volatile %struct.ScmObj* %ae51145, %struct.ScmObj** %stackaddr$makeclosure57516, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51145, %struct.ScmObj* %thunk4706847194, i64 0)
%argslist56455$anf_45bind473320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57518 = alloca %struct.ScmObj*, align 8
%argslist56455$anf_45bind473321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %argslist56455$anf_45bind473320)
store volatile %struct.ScmObj* %argslist56455$anf_45bind473321, %struct.ScmObj** %stackaddr$prim57518, align 8
%stackaddr$prim57519 = alloca %struct.ScmObj*, align 8
%argslist56455$anf_45bind473322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51145, %struct.ScmObj* %argslist56455$anf_45bind473321)
store volatile %struct.ScmObj* %argslist56455$anf_45bind473322, %struct.ScmObj** %stackaddr$prim57519, align 8
%clofunc57520 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47332)
musttail call tailcc void %clofunc57520(%struct.ScmObj* %anf_45bind47332, %struct.ScmObj* %argslist56455$anf_45bind473322)
ret void
}

define tailcc void @proc_clo$ae51145(%struct.ScmObj* %env$ae51145,%struct.ScmObj* %current_45args56428) {
%stackaddr$env-ref57521 = alloca %struct.ScmObj*, align 8
%thunk4706847194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51145, i64 0)
store %struct.ScmObj* %thunk4706847194, %struct.ScmObj** %stackaddr$env-ref57521
%stackaddr$prim57522 = alloca %struct.ScmObj*, align 8
%_95k47376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56428)
store volatile %struct.ScmObj* %_95k47376, %struct.ScmObj** %stackaddr$prim57522, align 8
%stackaddr$prim57523 = alloca %struct.ScmObj*, align 8
%current_45args56429 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56428)
store volatile %struct.ScmObj* %current_45args56429, %struct.ScmObj** %stackaddr$prim57523, align 8
%stackaddr$prim57524 = alloca %struct.ScmObj*, align 8
%anf_45bind47333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56429)
store volatile %struct.ScmObj* %anf_45bind47333, %struct.ScmObj** %stackaddr$prim57524, align 8
%truthy$cmp57525 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47333)
%cmp$cmp57525 = icmp eq i64 %truthy$cmp57525, 1
br i1 %cmp$cmp57525, label %truebranch$cmp57525, label %falsebranch$cmp57525
truebranch$cmp57525:
%ae51149 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57526 = alloca %struct.ScmObj*, align 8
%anf_45bind47334 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %ae51149)
store volatile %struct.ScmObj* %anf_45bind47334, %struct.ScmObj** %stackaddr$prim57526, align 8
%truthy$cmp57527 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47334)
%cmp$cmp57527 = icmp eq i64 %truthy$cmp57527, 1
br i1 %cmp$cmp57527, label %truebranch$cmp57527, label %falsebranch$cmp57527
truebranch$cmp57527:
%ae51152 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim57528 = alloca %struct.ScmObj*, align 8
%cpsprim47377 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %ae51152)
store volatile %struct.ScmObj* %cpsprim47377, %struct.ScmObj** %stackaddr$prim57528, align 8
%stackaddr$makeclosure57529 = alloca %struct.ScmObj*, align 8
%fptrToInt57530 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51153 to i64
%ae51153 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57530)
store volatile %struct.ScmObj* %ae51153, %struct.ScmObj** %stackaddr$makeclosure57529, align 8
%ae51154 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56435$ae511530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57531 = alloca %struct.ScmObj*, align 8
%argslist56435$ae511531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47377, %struct.ScmObj* %argslist56435$ae511530)
store volatile %struct.ScmObj* %argslist56435$ae511531, %struct.ScmObj** %stackaddr$prim57531, align 8
%stackaddr$prim57532 = alloca %struct.ScmObj*, align 8
%argslist56435$ae511532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51154, %struct.ScmObj* %argslist56435$ae511531)
store volatile %struct.ScmObj* %argslist56435$ae511532, %struct.ScmObj** %stackaddr$prim57532, align 8
%clofunc57533 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51153)
musttail call tailcc void %clofunc57533(%struct.ScmObj* %ae51153, %struct.ScmObj* %argslist56435$ae511532)
ret void
falsebranch$cmp57527:
%ae51174 = call %struct.ScmObj* @const_init_int(i64 1)
%ae51175 = call %struct.ScmObj* @const_init_true()
%stackaddr$prim57534 = alloca %struct.ScmObj*, align 8
%t4707447203 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %ae51174, %struct.ScmObj* %ae51175)
store volatile %struct.ScmObj* %t4707447203, %struct.ScmObj** %stackaddr$prim57534, align 8
%ae51177 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim57535 = alloca %struct.ScmObj*, align 8
%anf_45bind47335 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %ae51177)
store volatile %struct.ScmObj* %anf_45bind47335, %struct.ScmObj** %stackaddr$prim57535, align 8
%stackaddr$makeclosure57536 = alloca %struct.ScmObj*, align 8
%fptrToInt57537 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51179 to i64
%ae51179 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57537)
store volatile %struct.ScmObj* %ae51179, %struct.ScmObj** %stackaddr$makeclosure57536, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51179, %struct.ScmObj* %thunk4706847194, i64 0)
%ae51180 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @global$sym$ae5118057538, i32 0, i32 0))
%argslist56444$anf_45bind473350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57539 = alloca %struct.ScmObj*, align 8
%argslist56444$anf_45bind473351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51180, %struct.ScmObj* %argslist56444$anf_45bind473350)
store volatile %struct.ScmObj* %argslist56444$anf_45bind473351, %struct.ScmObj** %stackaddr$prim57539, align 8
%stackaddr$prim57540 = alloca %struct.ScmObj*, align 8
%argslist56444$anf_45bind473352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51179, %struct.ScmObj* %argslist56444$anf_45bind473351)
store volatile %struct.ScmObj* %argslist56444$anf_45bind473352, %struct.ScmObj** %stackaddr$prim57540, align 8
%clofunc57541 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47335)
musttail call tailcc void %clofunc57541(%struct.ScmObj* %anf_45bind47335, %struct.ScmObj* %argslist56444$anf_45bind473352)
ret void
falsebranch$cmp57525:
%stackaddr$prim57542 = alloca %struct.ScmObj*, align 8
%anf_45bind47336 = call %struct.ScmObj* @prim_number_63(%struct.ScmObj* %thunk4706847194)
store volatile %struct.ScmObj* %anf_45bind47336, %struct.ScmObj** %stackaddr$prim57542, align 8
%truthy$cmp57543 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47336)
%cmp$cmp57543 = icmp eq i64 %truthy$cmp57543, 1
br i1 %cmp$cmp57543, label %truebranch$cmp57543, label %falsebranch$cmp57543
truebranch$cmp57543:
%stackaddr$makeclosure57544 = alloca %struct.ScmObj*, align 8
%fptrToInt57545 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51309 to i64
%ae51309 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57545)
store volatile %struct.ScmObj* %ae51309, %struct.ScmObj** %stackaddr$makeclosure57544, align 8
%ae51310 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56449$ae513090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57546 = alloca %struct.ScmObj*, align 8
%argslist56449$ae513091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %argslist56449$ae513090)
store volatile %struct.ScmObj* %argslist56449$ae513091, %struct.ScmObj** %stackaddr$prim57546, align 8
%stackaddr$prim57547 = alloca %struct.ScmObj*, align 8
%argslist56449$ae513092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51310, %struct.ScmObj* %argslist56449$ae513091)
store volatile %struct.ScmObj* %argslist56449$ae513092, %struct.ScmObj** %stackaddr$prim57547, align 8
%clofunc57548 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51309)
musttail call tailcc void %clofunc57548(%struct.ScmObj* %ae51309, %struct.ScmObj* %argslist56449$ae513092)
ret void
falsebranch$cmp57543:
%stackaddr$makeclosure57549 = alloca %struct.ScmObj*, align 8
%fptrToInt57550 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51321 to i64
%ae51321 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57550)
store volatile %struct.ScmObj* %ae51321, %struct.ScmObj** %stackaddr$makeclosure57549, align 8
%ae51322 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51323 = call %struct.ScmObj* @const_init_string(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @global$str$ae5132357551, i32 0, i32 0))
%argslist56454$ae513210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57552 = alloca %struct.ScmObj*, align 8
%argslist56454$ae513211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51323, %struct.ScmObj* %argslist56454$ae513210)
store volatile %struct.ScmObj* %argslist56454$ae513211, %struct.ScmObj** %stackaddr$prim57552, align 8
%stackaddr$prim57553 = alloca %struct.ScmObj*, align 8
%argslist56454$ae513212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51322, %struct.ScmObj* %argslist56454$ae513211)
store volatile %struct.ScmObj* %argslist56454$ae513212, %struct.ScmObj** %stackaddr$prim57553, align 8
%clofunc57554 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51321)
musttail call tailcc void %clofunc57554(%struct.ScmObj* %ae51321, %struct.ScmObj* %argslist56454$ae513212)
ret void
}

define tailcc void @proc_clo$ae51153(%struct.ScmObj* %env$ae51153,%struct.ScmObj* %current_45args56431) {
%stackaddr$prim57555 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56431)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim57555, align 8
%stackaddr$prim57556 = alloca %struct.ScmObj*, align 8
%current_45args56432 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56431)
store volatile %struct.ScmObj* %current_45args56432, %struct.ScmObj** %stackaddr$prim57556, align 8
%stackaddr$prim57557 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56432)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim57557, align 8
%stackaddr$prim57558 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim57558, align 8
%argslist56434$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57559 = alloca %struct.ScmObj*, align 8
%argslist56434$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist56434$k0)
store volatile %struct.ScmObj* %argslist56434$k1, %struct.ScmObj** %stackaddr$prim57559, align 8
%clofunc57560 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc57560(%struct.ScmObj* %k, %struct.ScmObj* %argslist56434$k1)
ret void
}

define tailcc void @proc_clo$ae51179(%struct.ScmObj* %env$ae51179,%struct.ScmObj* %current_45args56436) {
%stackaddr$env-ref57561 = alloca %struct.ScmObj*, align 8
%thunk4706847194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51179, i64 0)
store %struct.ScmObj* %thunk4706847194, %struct.ScmObj** %stackaddr$env-ref57561
%stackaddr$prim57562 = alloca %struct.ScmObj*, align 8
%_95k47378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56436)
store volatile %struct.ScmObj* %_95k47378, %struct.ScmObj** %stackaddr$prim57562, align 8
%stackaddr$prim57563 = alloca %struct.ScmObj*, align 8
%current_45args56437 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56436)
store volatile %struct.ScmObj* %current_45args56437, %struct.ScmObj** %stackaddr$prim57563, align 8
%stackaddr$prim57564 = alloca %struct.ScmObj*, align 8
%val4706947205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56437)
store volatile %struct.ScmObj* %val4706947205, %struct.ScmObj** %stackaddr$prim57564, align 8
%ae51185 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim57565 = alloca %struct.ScmObj*, align 8
%t4707547204 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %ae51185, %struct.ScmObj* %val4706947205)
store volatile %struct.ScmObj* %t4707547204, %struct.ScmObj** %stackaddr$prim57565, align 8
%ae51188 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim57566 = alloca %struct.ScmObj*, align 8
%cpsprim47379 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %ae51188)
store volatile %struct.ScmObj* %cpsprim47379, %struct.ScmObj** %stackaddr$prim57566, align 8
%stackaddr$makeclosure57567 = alloca %struct.ScmObj*, align 8
%fptrToInt57568 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51189 to i64
%ae51189 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57568)
store volatile %struct.ScmObj* %ae51189, %struct.ScmObj** %stackaddr$makeclosure57567, align 8
%ae51190 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56443$ae511890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57569 = alloca %struct.ScmObj*, align 8
%argslist56443$ae511891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47379, %struct.ScmObj* %argslist56443$ae511890)
store volatile %struct.ScmObj* %argslist56443$ae511891, %struct.ScmObj** %stackaddr$prim57569, align 8
%stackaddr$prim57570 = alloca %struct.ScmObj*, align 8
%argslist56443$ae511892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51190, %struct.ScmObj* %argslist56443$ae511891)
store volatile %struct.ScmObj* %argslist56443$ae511892, %struct.ScmObj** %stackaddr$prim57570, align 8
%clofunc57571 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51189)
musttail call tailcc void %clofunc57571(%struct.ScmObj* %ae51189, %struct.ScmObj* %argslist56443$ae511892)
ret void
}

define tailcc void @proc_clo$ae51189(%struct.ScmObj* %env$ae51189,%struct.ScmObj* %current_45args56439) {
%stackaddr$prim57572 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56439)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim57572, align 8
%stackaddr$prim57573 = alloca %struct.ScmObj*, align 8
%current_45args56440 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56439)
store volatile %struct.ScmObj* %current_45args56440, %struct.ScmObj** %stackaddr$prim57573, align 8
%stackaddr$prim57574 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56440)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim57574, align 8
%stackaddr$prim57575 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim57575, align 8
%argslist56442$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57576 = alloca %struct.ScmObj*, align 8
%argslist56442$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist56442$k0)
store volatile %struct.ScmObj* %argslist56442$k1, %struct.ScmObj** %stackaddr$prim57576, align 8
%clofunc57577 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc57577(%struct.ScmObj* %k, %struct.ScmObj* %argslist56442$k1)
ret void
}

define tailcc void @proc_clo$ae51309(%struct.ScmObj* %env$ae51309,%struct.ScmObj* %current_45args56445) {
%stackaddr$prim57578 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56445)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim57578, align 8
%stackaddr$prim57579 = alloca %struct.ScmObj*, align 8
%current_45args56446 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56445)
store volatile %struct.ScmObj* %current_45args56446, %struct.ScmObj** %stackaddr$prim57579, align 8
%stackaddr$prim57580 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56446)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim57580, align 8
%stackaddr$prim57581 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim57581, align 8
%argslist56448$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57582 = alloca %struct.ScmObj*, align 8
%argslist56448$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist56448$k0)
store volatile %struct.ScmObj* %argslist56448$k1, %struct.ScmObj** %stackaddr$prim57582, align 8
%clofunc57583 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc57583(%struct.ScmObj* %k, %struct.ScmObj* %argslist56448$k1)
ret void
}

define tailcc void @proc_clo$ae51321(%struct.ScmObj* %env$ae51321,%struct.ScmObj* %current_45args56450) {
%stackaddr$prim57584 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56450)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim57584, align 8
%stackaddr$prim57585 = alloca %struct.ScmObj*, align 8
%current_45args56451 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56450)
store volatile %struct.ScmObj* %current_45args56451, %struct.ScmObj** %stackaddr$prim57585, align 8
%stackaddr$prim57586 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56451)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim57586, align 8
%stackaddr$prim57587 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim57587, align 8
%argslist56453$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57588 = alloca %struct.ScmObj*, align 8
%argslist56453$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist56453$k0)
store volatile %struct.ScmObj* %argslist56453$k1, %struct.ScmObj** %stackaddr$prim57588, align 8
%clofunc57589 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc57589(%struct.ScmObj* %k, %struct.ScmObj* %argslist56453$k1)
ret void
}

define tailcc void @proc_clo$ae51059(%struct.ScmObj* %env$ae51059,%struct.ScmObj* %current_45args56456) {
%stackaddr$prim57590 = alloca %struct.ScmObj*, align 8
%k47380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56456)
store volatile %struct.ScmObj* %k47380, %struct.ScmObj** %stackaddr$prim57590, align 8
%stackaddr$prim57591 = alloca %struct.ScmObj*, align 8
%current_45args56457 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56456)
store volatile %struct.ScmObj* %current_45args56457, %struct.ScmObj** %stackaddr$prim57591, align 8
%stackaddr$prim57592 = alloca %struct.ScmObj*, align 8
%thunk47202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56457)
store volatile %struct.ScmObj* %thunk47202, %struct.ScmObj** %stackaddr$prim57592, align 8
%stackaddr$prim57593 = alloca %struct.ScmObj*, align 8
%anf_45bind47328 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk47202)
store volatile %struct.ScmObj* %anf_45bind47328, %struct.ScmObj** %stackaddr$prim57593, align 8
%truthy$cmp57594 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47328)
%cmp$cmp57594 = icmp eq i64 %truthy$cmp57594, 1
br i1 %cmp$cmp57594, label %truebranch$cmp57594, label %falsebranch$cmp57594
truebranch$cmp57594:
%stackaddr$prim57595 = alloca %struct.ScmObj*, align 8
%anf_45bind47329 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk47202)
store volatile %struct.ScmObj* %anf_45bind47329, %struct.ScmObj** %stackaddr$prim57595, align 8
%ae51064 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim57596 = alloca %struct.ScmObj*, align 8
%anf_45bind47330 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind47329, %struct.ScmObj* %ae51064)
store volatile %struct.ScmObj* %anf_45bind47330, %struct.ScmObj** %stackaddr$prim57596, align 8
%truthy$cmp57597 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47330)
%cmp$cmp57597 = icmp eq i64 %truthy$cmp57597, 1
br i1 %cmp$cmp57597, label %truebranch$cmp57597, label %falsebranch$cmp57597
truebranch$cmp57597:
%ae51067 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57598 = alloca %struct.ScmObj*, align 8
%anf_45bind47331 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk47202, %struct.ScmObj* %ae51067)
store volatile %struct.ScmObj* %anf_45bind47331, %struct.ScmObj** %stackaddr$prim57598, align 8
%ae51069 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae5106957599, i32 0, i32 0))
%stackaddr$prim57600 = alloca %struct.ScmObj*, align 8
%cpsprim47381 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind47331, %struct.ScmObj* %ae51069)
store volatile %struct.ScmObj* %cpsprim47381, %struct.ScmObj** %stackaddr$prim57600, align 8
%ae51071 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56459$k473800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57601 = alloca %struct.ScmObj*, align 8
%argslist56459$k473801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47381, %struct.ScmObj* %argslist56459$k473800)
store volatile %struct.ScmObj* %argslist56459$k473801, %struct.ScmObj** %stackaddr$prim57601, align 8
%stackaddr$prim57602 = alloca %struct.ScmObj*, align 8
%argslist56459$k473802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51071, %struct.ScmObj* %argslist56459$k473801)
store volatile %struct.ScmObj* %argslist56459$k473802, %struct.ScmObj** %stackaddr$prim57602, align 8
%clofunc57603 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47380)
musttail call tailcc void %clofunc57603(%struct.ScmObj* %k47380, %struct.ScmObj* %argslist56459$k473802)
ret void
falsebranch$cmp57597:
%ae51089 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51090 = call %struct.ScmObj* @const_init_false()
%argslist56460$k473800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57604 = alloca %struct.ScmObj*, align 8
%argslist56460$k473801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51090, %struct.ScmObj* %argslist56460$k473800)
store volatile %struct.ScmObj* %argslist56460$k473801, %struct.ScmObj** %stackaddr$prim57604, align 8
%stackaddr$prim57605 = alloca %struct.ScmObj*, align 8
%argslist56460$k473802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51089, %struct.ScmObj* %argslist56460$k473801)
store volatile %struct.ScmObj* %argslist56460$k473802, %struct.ScmObj** %stackaddr$prim57605, align 8
%clofunc57606 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47380)
musttail call tailcc void %clofunc57606(%struct.ScmObj* %k47380, %struct.ScmObj* %argslist56460$k473802)
ret void
falsebranch$cmp57594:
%ae51111 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51112 = call %struct.ScmObj* @const_init_false()
%argslist56461$k473800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57607 = alloca %struct.ScmObj*, align 8
%argslist56461$k473801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51112, %struct.ScmObj* %argslist56461$k473800)
store volatile %struct.ScmObj* %argslist56461$k473801, %struct.ScmObj** %stackaddr$prim57607, align 8
%stackaddr$prim57608 = alloca %struct.ScmObj*, align 8
%argslist56461$k473802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51111, %struct.ScmObj* %argslist56461$k473801)
store volatile %struct.ScmObj* %argslist56461$k473802, %struct.ScmObj** %stackaddr$prim57608, align 8
%clofunc57609 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47380)
musttail call tailcc void %clofunc57609(%struct.ScmObj* %k47380, %struct.ScmObj* %argslist56461$k473802)
ret void
}

define tailcc void @proc_clo$ae51445(%struct.ScmObj* %env$ae51445,%struct.ScmObj* %current_45args56464) {
%stackaddr$env-ref57610 = alloca %struct.ScmObj*, align 8
%thunk4707047195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51445, i64 0)
store %struct.ScmObj* %thunk4707047195, %struct.ScmObj** %stackaddr$env-ref57610
%stackaddr$prim57611 = alloca %struct.ScmObj*, align 8
%_95k47383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56464)
store volatile %struct.ScmObj* %_95k47383, %struct.ScmObj** %stackaddr$prim57611, align 8
%stackaddr$prim57612 = alloca %struct.ScmObj*, align 8
%current_45args56465 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56464)
store volatile %struct.ScmObj* %current_45args56465, %struct.ScmObj** %stackaddr$prim57612, align 8
%stackaddr$prim57613 = alloca %struct.ScmObj*, align 8
%val4707147201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56465)
store volatile %struct.ScmObj* %val4707147201, %struct.ScmObj** %stackaddr$prim57613, align 8
%ae51451 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim57614 = alloca %struct.ScmObj*, align 8
%t4707347200 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4707047195, %struct.ScmObj* %ae51451, %struct.ScmObj* %val4707147201)
store volatile %struct.ScmObj* %t4707347200, %struct.ScmObj** %stackaddr$prim57614, align 8
%ae51454 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim57615 = alloca %struct.ScmObj*, align 8
%cpsprim47384 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4707047195, %struct.ScmObj* %ae51454)
store volatile %struct.ScmObj* %cpsprim47384, %struct.ScmObj** %stackaddr$prim57615, align 8
%stackaddr$makeclosure57616 = alloca %struct.ScmObj*, align 8
%fptrToInt57617 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51455 to i64
%ae51455 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57617)
store volatile %struct.ScmObj* %ae51455, %struct.ScmObj** %stackaddr$makeclosure57616, align 8
%ae51456 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56508$ae514550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57618 = alloca %struct.ScmObj*, align 8
%argslist56508$ae514551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47384, %struct.ScmObj* %argslist56508$ae514550)
store volatile %struct.ScmObj* %argslist56508$ae514551, %struct.ScmObj** %stackaddr$prim57618, align 8
%stackaddr$prim57619 = alloca %struct.ScmObj*, align 8
%argslist56508$ae514552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51456, %struct.ScmObj* %argslist56508$ae514551)
store volatile %struct.ScmObj* %argslist56508$ae514552, %struct.ScmObj** %stackaddr$prim57619, align 8
%clofunc57620 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51455)
musttail call tailcc void %clofunc57620(%struct.ScmObj* %ae51455, %struct.ScmObj* %argslist56508$ae514552)
ret void
}

define tailcc void @proc_clo$ae51455(%struct.ScmObj* %env$ae51455,%struct.ScmObj* %current_45args56467) {
%stackaddr$prim57621 = alloca %struct.ScmObj*, align 8
%_95k47374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56467)
store volatile %struct.ScmObj* %_95k47374, %struct.ScmObj** %stackaddr$prim57621, align 8
%stackaddr$prim57622 = alloca %struct.ScmObj*, align 8
%current_45args56468 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56467)
store volatile %struct.ScmObj* %current_45args56468, %struct.ScmObj** %stackaddr$prim57622, align 8
%stackaddr$prim57623 = alloca %struct.ScmObj*, align 8
%thunk4706847194 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56468)
store volatile %struct.ScmObj* %thunk4706847194, %struct.ScmObj** %stackaddr$prim57623, align 8
%stackaddr$makeclosure57624 = alloca %struct.ScmObj*, align 8
%fptrToInt57625 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51461 to i64
%ae51461 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57625)
store volatile %struct.ScmObj* %ae51461, %struct.ScmObj** %stackaddr$makeclosure57624, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51461, %struct.ScmObj* %thunk4706847194, i64 0)
%ae51462 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57626 = alloca %struct.ScmObj*, align 8
%fptrToInt57627 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51463 to i64
%ae51463 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57627)
store volatile %struct.ScmObj* %ae51463, %struct.ScmObj** %stackaddr$makeclosure57626, align 8
%argslist56507$ae514610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57628 = alloca %struct.ScmObj*, align 8
%argslist56507$ae514611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51463, %struct.ScmObj* %argslist56507$ae514610)
store volatile %struct.ScmObj* %argslist56507$ae514611, %struct.ScmObj** %stackaddr$prim57628, align 8
%stackaddr$prim57629 = alloca %struct.ScmObj*, align 8
%argslist56507$ae514612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51462, %struct.ScmObj* %argslist56507$ae514611)
store volatile %struct.ScmObj* %argslist56507$ae514612, %struct.ScmObj** %stackaddr$prim57629, align 8
%clofunc57630 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51461)
musttail call tailcc void %clofunc57630(%struct.ScmObj* %ae51461, %struct.ScmObj* %argslist56507$ae514612)
ret void
}

define tailcc void @proc_clo$ae51461(%struct.ScmObj* %env$ae51461,%struct.ScmObj* %current_45args56470) {
%stackaddr$env-ref57631 = alloca %struct.ScmObj*, align 8
%thunk4706847194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51461, i64 0)
store %struct.ScmObj* %thunk4706847194, %struct.ScmObj** %stackaddr$env-ref57631
%stackaddr$prim57632 = alloca %struct.ScmObj*, align 8
%_95k47375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56470)
store volatile %struct.ScmObj* %_95k47375, %struct.ScmObj** %stackaddr$prim57632, align 8
%stackaddr$prim57633 = alloca %struct.ScmObj*, align 8
%current_45args56471 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56470)
store volatile %struct.ScmObj* %current_45args56471, %struct.ScmObj** %stackaddr$prim57633, align 8
%stackaddr$prim57634 = alloca %struct.ScmObj*, align 8
%anf_45bind47332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56471)
store volatile %struct.ScmObj* %anf_45bind47332, %struct.ScmObj** %stackaddr$prim57634, align 8
%stackaddr$makeclosure57635 = alloca %struct.ScmObj*, align 8
%fptrToInt57636 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51549 to i64
%ae51549 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57636)
store volatile %struct.ScmObj* %ae51549, %struct.ScmObj** %stackaddr$makeclosure57635, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51549, %struct.ScmObj* %thunk4706847194, i64 0)
%argslist56500$anf_45bind473320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57637 = alloca %struct.ScmObj*, align 8
%argslist56500$anf_45bind473321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %argslist56500$anf_45bind473320)
store volatile %struct.ScmObj* %argslist56500$anf_45bind473321, %struct.ScmObj** %stackaddr$prim57637, align 8
%stackaddr$prim57638 = alloca %struct.ScmObj*, align 8
%argslist56500$anf_45bind473322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51549, %struct.ScmObj* %argslist56500$anf_45bind473321)
store volatile %struct.ScmObj* %argslist56500$anf_45bind473322, %struct.ScmObj** %stackaddr$prim57638, align 8
%clofunc57639 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47332)
musttail call tailcc void %clofunc57639(%struct.ScmObj* %anf_45bind47332, %struct.ScmObj* %argslist56500$anf_45bind473322)
ret void
}

define tailcc void @proc_clo$ae51549(%struct.ScmObj* %env$ae51549,%struct.ScmObj* %current_45args56473) {
%stackaddr$env-ref57640 = alloca %struct.ScmObj*, align 8
%thunk4706847194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51549, i64 0)
store %struct.ScmObj* %thunk4706847194, %struct.ScmObj** %stackaddr$env-ref57640
%stackaddr$prim57641 = alloca %struct.ScmObj*, align 8
%_95k47376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56473)
store volatile %struct.ScmObj* %_95k47376, %struct.ScmObj** %stackaddr$prim57641, align 8
%stackaddr$prim57642 = alloca %struct.ScmObj*, align 8
%current_45args56474 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56473)
store volatile %struct.ScmObj* %current_45args56474, %struct.ScmObj** %stackaddr$prim57642, align 8
%stackaddr$prim57643 = alloca %struct.ScmObj*, align 8
%anf_45bind47333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56474)
store volatile %struct.ScmObj* %anf_45bind47333, %struct.ScmObj** %stackaddr$prim57643, align 8
%truthy$cmp57644 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47333)
%cmp$cmp57644 = icmp eq i64 %truthy$cmp57644, 1
br i1 %cmp$cmp57644, label %truebranch$cmp57644, label %falsebranch$cmp57644
truebranch$cmp57644:
%ae51553 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57645 = alloca %struct.ScmObj*, align 8
%anf_45bind47334 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %ae51553)
store volatile %struct.ScmObj* %anf_45bind47334, %struct.ScmObj** %stackaddr$prim57645, align 8
%truthy$cmp57646 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47334)
%cmp$cmp57646 = icmp eq i64 %truthy$cmp57646, 1
br i1 %cmp$cmp57646, label %truebranch$cmp57646, label %falsebranch$cmp57646
truebranch$cmp57646:
%ae51556 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim57647 = alloca %struct.ScmObj*, align 8
%cpsprim47377 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %ae51556)
store volatile %struct.ScmObj* %cpsprim47377, %struct.ScmObj** %stackaddr$prim57647, align 8
%stackaddr$makeclosure57648 = alloca %struct.ScmObj*, align 8
%fptrToInt57649 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51557 to i64
%ae51557 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57649)
store volatile %struct.ScmObj* %ae51557, %struct.ScmObj** %stackaddr$makeclosure57648, align 8
%ae51558 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56480$ae515570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57650 = alloca %struct.ScmObj*, align 8
%argslist56480$ae515571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47377, %struct.ScmObj* %argslist56480$ae515570)
store volatile %struct.ScmObj* %argslist56480$ae515571, %struct.ScmObj** %stackaddr$prim57650, align 8
%stackaddr$prim57651 = alloca %struct.ScmObj*, align 8
%argslist56480$ae515572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51558, %struct.ScmObj* %argslist56480$ae515571)
store volatile %struct.ScmObj* %argslist56480$ae515572, %struct.ScmObj** %stackaddr$prim57651, align 8
%clofunc57652 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51557)
musttail call tailcc void %clofunc57652(%struct.ScmObj* %ae51557, %struct.ScmObj* %argslist56480$ae515572)
ret void
falsebranch$cmp57646:
%ae51578 = call %struct.ScmObj* @const_init_int(i64 1)
%ae51579 = call %struct.ScmObj* @const_init_true()
%stackaddr$prim57653 = alloca %struct.ScmObj*, align 8
%t4707447203 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %ae51578, %struct.ScmObj* %ae51579)
store volatile %struct.ScmObj* %t4707447203, %struct.ScmObj** %stackaddr$prim57653, align 8
%ae51581 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim57654 = alloca %struct.ScmObj*, align 8
%anf_45bind47335 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %ae51581)
store volatile %struct.ScmObj* %anf_45bind47335, %struct.ScmObj** %stackaddr$prim57654, align 8
%stackaddr$makeclosure57655 = alloca %struct.ScmObj*, align 8
%fptrToInt57656 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51583 to i64
%ae51583 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57656)
store volatile %struct.ScmObj* %ae51583, %struct.ScmObj** %stackaddr$makeclosure57655, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51583, %struct.ScmObj* %thunk4706847194, i64 0)
%ae51584 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @global$sym$ae5158457657, i32 0, i32 0))
%argslist56489$anf_45bind473350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57658 = alloca %struct.ScmObj*, align 8
%argslist56489$anf_45bind473351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51584, %struct.ScmObj* %argslist56489$anf_45bind473350)
store volatile %struct.ScmObj* %argslist56489$anf_45bind473351, %struct.ScmObj** %stackaddr$prim57658, align 8
%stackaddr$prim57659 = alloca %struct.ScmObj*, align 8
%argslist56489$anf_45bind473352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51583, %struct.ScmObj* %argslist56489$anf_45bind473351)
store volatile %struct.ScmObj* %argslist56489$anf_45bind473352, %struct.ScmObj** %stackaddr$prim57659, align 8
%clofunc57660 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47335)
musttail call tailcc void %clofunc57660(%struct.ScmObj* %anf_45bind47335, %struct.ScmObj* %argslist56489$anf_45bind473352)
ret void
falsebranch$cmp57644:
%stackaddr$prim57661 = alloca %struct.ScmObj*, align 8
%anf_45bind47336 = call %struct.ScmObj* @prim_number_63(%struct.ScmObj* %thunk4706847194)
store volatile %struct.ScmObj* %anf_45bind47336, %struct.ScmObj** %stackaddr$prim57661, align 8
%truthy$cmp57662 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47336)
%cmp$cmp57662 = icmp eq i64 %truthy$cmp57662, 1
br i1 %cmp$cmp57662, label %truebranch$cmp57662, label %falsebranch$cmp57662
truebranch$cmp57662:
%stackaddr$makeclosure57663 = alloca %struct.ScmObj*, align 8
%fptrToInt57664 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51713 to i64
%ae51713 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57664)
store volatile %struct.ScmObj* %ae51713, %struct.ScmObj** %stackaddr$makeclosure57663, align 8
%ae51714 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56494$ae517130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57665 = alloca %struct.ScmObj*, align 8
%argslist56494$ae517131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %argslist56494$ae517130)
store volatile %struct.ScmObj* %argslist56494$ae517131, %struct.ScmObj** %stackaddr$prim57665, align 8
%stackaddr$prim57666 = alloca %struct.ScmObj*, align 8
%argslist56494$ae517132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51714, %struct.ScmObj* %argslist56494$ae517131)
store volatile %struct.ScmObj* %argslist56494$ae517132, %struct.ScmObj** %stackaddr$prim57666, align 8
%clofunc57667 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51713)
musttail call tailcc void %clofunc57667(%struct.ScmObj* %ae51713, %struct.ScmObj* %argslist56494$ae517132)
ret void
falsebranch$cmp57662:
%stackaddr$makeclosure57668 = alloca %struct.ScmObj*, align 8
%fptrToInt57669 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51725 to i64
%ae51725 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57669)
store volatile %struct.ScmObj* %ae51725, %struct.ScmObj** %stackaddr$makeclosure57668, align 8
%ae51726 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51727 = call %struct.ScmObj* @const_init_string(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @global$str$ae5172757670, i32 0, i32 0))
%argslist56499$ae517250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57671 = alloca %struct.ScmObj*, align 8
%argslist56499$ae517251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51727, %struct.ScmObj* %argslist56499$ae517250)
store volatile %struct.ScmObj* %argslist56499$ae517251, %struct.ScmObj** %stackaddr$prim57671, align 8
%stackaddr$prim57672 = alloca %struct.ScmObj*, align 8
%argslist56499$ae517252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51726, %struct.ScmObj* %argslist56499$ae517251)
store volatile %struct.ScmObj* %argslist56499$ae517252, %struct.ScmObj** %stackaddr$prim57672, align 8
%clofunc57673 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51725)
musttail call tailcc void %clofunc57673(%struct.ScmObj* %ae51725, %struct.ScmObj* %argslist56499$ae517252)
ret void
}

define tailcc void @proc_clo$ae51557(%struct.ScmObj* %env$ae51557,%struct.ScmObj* %current_45args56476) {
%stackaddr$prim57674 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56476)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim57674, align 8
%stackaddr$prim57675 = alloca %struct.ScmObj*, align 8
%current_45args56477 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56476)
store volatile %struct.ScmObj* %current_45args56477, %struct.ScmObj** %stackaddr$prim57675, align 8
%stackaddr$prim57676 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56477)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim57676, align 8
%stackaddr$prim57677 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim57677, align 8
%argslist56479$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57678 = alloca %struct.ScmObj*, align 8
%argslist56479$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist56479$k0)
store volatile %struct.ScmObj* %argslist56479$k1, %struct.ScmObj** %stackaddr$prim57678, align 8
%clofunc57679 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc57679(%struct.ScmObj* %k, %struct.ScmObj* %argslist56479$k1)
ret void
}

define tailcc void @proc_clo$ae51583(%struct.ScmObj* %env$ae51583,%struct.ScmObj* %current_45args56481) {
%stackaddr$env-ref57680 = alloca %struct.ScmObj*, align 8
%thunk4706847194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51583, i64 0)
store %struct.ScmObj* %thunk4706847194, %struct.ScmObj** %stackaddr$env-ref57680
%stackaddr$prim57681 = alloca %struct.ScmObj*, align 8
%_95k47378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56481)
store volatile %struct.ScmObj* %_95k47378, %struct.ScmObj** %stackaddr$prim57681, align 8
%stackaddr$prim57682 = alloca %struct.ScmObj*, align 8
%current_45args56482 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56481)
store volatile %struct.ScmObj* %current_45args56482, %struct.ScmObj** %stackaddr$prim57682, align 8
%stackaddr$prim57683 = alloca %struct.ScmObj*, align 8
%val4706947205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56482)
store volatile %struct.ScmObj* %val4706947205, %struct.ScmObj** %stackaddr$prim57683, align 8
%ae51589 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim57684 = alloca %struct.ScmObj*, align 8
%t4707547204 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %ae51589, %struct.ScmObj* %val4706947205)
store volatile %struct.ScmObj* %t4707547204, %struct.ScmObj** %stackaddr$prim57684, align 8
%ae51592 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim57685 = alloca %struct.ScmObj*, align 8
%cpsprim47379 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %ae51592)
store volatile %struct.ScmObj* %cpsprim47379, %struct.ScmObj** %stackaddr$prim57685, align 8
%stackaddr$makeclosure57686 = alloca %struct.ScmObj*, align 8
%fptrToInt57687 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51593 to i64
%ae51593 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57687)
store volatile %struct.ScmObj* %ae51593, %struct.ScmObj** %stackaddr$makeclosure57686, align 8
%ae51594 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56488$ae515930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57688 = alloca %struct.ScmObj*, align 8
%argslist56488$ae515931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47379, %struct.ScmObj* %argslist56488$ae515930)
store volatile %struct.ScmObj* %argslist56488$ae515931, %struct.ScmObj** %stackaddr$prim57688, align 8
%stackaddr$prim57689 = alloca %struct.ScmObj*, align 8
%argslist56488$ae515932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51594, %struct.ScmObj* %argslist56488$ae515931)
store volatile %struct.ScmObj* %argslist56488$ae515932, %struct.ScmObj** %stackaddr$prim57689, align 8
%clofunc57690 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51593)
musttail call tailcc void %clofunc57690(%struct.ScmObj* %ae51593, %struct.ScmObj* %argslist56488$ae515932)
ret void
}

define tailcc void @proc_clo$ae51593(%struct.ScmObj* %env$ae51593,%struct.ScmObj* %current_45args56484) {
%stackaddr$prim57691 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56484)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim57691, align 8
%stackaddr$prim57692 = alloca %struct.ScmObj*, align 8
%current_45args56485 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56484)
store volatile %struct.ScmObj* %current_45args56485, %struct.ScmObj** %stackaddr$prim57692, align 8
%stackaddr$prim57693 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56485)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim57693, align 8
%stackaddr$prim57694 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim57694, align 8
%argslist56487$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57695 = alloca %struct.ScmObj*, align 8
%argslist56487$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist56487$k0)
store volatile %struct.ScmObj* %argslist56487$k1, %struct.ScmObj** %stackaddr$prim57695, align 8
%clofunc57696 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc57696(%struct.ScmObj* %k, %struct.ScmObj* %argslist56487$k1)
ret void
}

define tailcc void @proc_clo$ae51713(%struct.ScmObj* %env$ae51713,%struct.ScmObj* %current_45args56490) {
%stackaddr$prim57697 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56490)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim57697, align 8
%stackaddr$prim57698 = alloca %struct.ScmObj*, align 8
%current_45args56491 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56490)
store volatile %struct.ScmObj* %current_45args56491, %struct.ScmObj** %stackaddr$prim57698, align 8
%stackaddr$prim57699 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56491)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim57699, align 8
%stackaddr$prim57700 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim57700, align 8
%argslist56493$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57701 = alloca %struct.ScmObj*, align 8
%argslist56493$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist56493$k0)
store volatile %struct.ScmObj* %argslist56493$k1, %struct.ScmObj** %stackaddr$prim57701, align 8
%clofunc57702 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc57702(%struct.ScmObj* %k, %struct.ScmObj* %argslist56493$k1)
ret void
}

define tailcc void @proc_clo$ae51725(%struct.ScmObj* %env$ae51725,%struct.ScmObj* %current_45args56495) {
%stackaddr$prim57703 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56495)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim57703, align 8
%stackaddr$prim57704 = alloca %struct.ScmObj*, align 8
%current_45args56496 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56495)
store volatile %struct.ScmObj* %current_45args56496, %struct.ScmObj** %stackaddr$prim57704, align 8
%stackaddr$prim57705 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56496)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim57705, align 8
%stackaddr$prim57706 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim57706, align 8
%argslist56498$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57707 = alloca %struct.ScmObj*, align 8
%argslist56498$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist56498$k0)
store volatile %struct.ScmObj* %argslist56498$k1, %struct.ScmObj** %stackaddr$prim57707, align 8
%clofunc57708 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc57708(%struct.ScmObj* %k, %struct.ScmObj* %argslist56498$k1)
ret void
}

define tailcc void @proc_clo$ae51463(%struct.ScmObj* %env$ae51463,%struct.ScmObj* %current_45args56501) {
%stackaddr$prim57709 = alloca %struct.ScmObj*, align 8
%k47380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56501)
store volatile %struct.ScmObj* %k47380, %struct.ScmObj** %stackaddr$prim57709, align 8
%stackaddr$prim57710 = alloca %struct.ScmObj*, align 8
%current_45args56502 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56501)
store volatile %struct.ScmObj* %current_45args56502, %struct.ScmObj** %stackaddr$prim57710, align 8
%stackaddr$prim57711 = alloca %struct.ScmObj*, align 8
%thunk47202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56502)
store volatile %struct.ScmObj* %thunk47202, %struct.ScmObj** %stackaddr$prim57711, align 8
%stackaddr$prim57712 = alloca %struct.ScmObj*, align 8
%anf_45bind47328 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk47202)
store volatile %struct.ScmObj* %anf_45bind47328, %struct.ScmObj** %stackaddr$prim57712, align 8
%truthy$cmp57713 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47328)
%cmp$cmp57713 = icmp eq i64 %truthy$cmp57713, 1
br i1 %cmp$cmp57713, label %truebranch$cmp57713, label %falsebranch$cmp57713
truebranch$cmp57713:
%stackaddr$prim57714 = alloca %struct.ScmObj*, align 8
%anf_45bind47329 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk47202)
store volatile %struct.ScmObj* %anf_45bind47329, %struct.ScmObj** %stackaddr$prim57714, align 8
%ae51468 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim57715 = alloca %struct.ScmObj*, align 8
%anf_45bind47330 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind47329, %struct.ScmObj* %ae51468)
store volatile %struct.ScmObj* %anf_45bind47330, %struct.ScmObj** %stackaddr$prim57715, align 8
%truthy$cmp57716 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47330)
%cmp$cmp57716 = icmp eq i64 %truthy$cmp57716, 1
br i1 %cmp$cmp57716, label %truebranch$cmp57716, label %falsebranch$cmp57716
truebranch$cmp57716:
%ae51471 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57717 = alloca %struct.ScmObj*, align 8
%anf_45bind47331 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk47202, %struct.ScmObj* %ae51471)
store volatile %struct.ScmObj* %anf_45bind47331, %struct.ScmObj** %stackaddr$prim57717, align 8
%ae51473 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae5147357718, i32 0, i32 0))
%stackaddr$prim57719 = alloca %struct.ScmObj*, align 8
%cpsprim47381 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind47331, %struct.ScmObj* %ae51473)
store volatile %struct.ScmObj* %cpsprim47381, %struct.ScmObj** %stackaddr$prim57719, align 8
%ae51475 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56504$k473800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57720 = alloca %struct.ScmObj*, align 8
%argslist56504$k473801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47381, %struct.ScmObj* %argslist56504$k473800)
store volatile %struct.ScmObj* %argslist56504$k473801, %struct.ScmObj** %stackaddr$prim57720, align 8
%stackaddr$prim57721 = alloca %struct.ScmObj*, align 8
%argslist56504$k473802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51475, %struct.ScmObj* %argslist56504$k473801)
store volatile %struct.ScmObj* %argslist56504$k473802, %struct.ScmObj** %stackaddr$prim57721, align 8
%clofunc57722 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47380)
musttail call tailcc void %clofunc57722(%struct.ScmObj* %k47380, %struct.ScmObj* %argslist56504$k473802)
ret void
falsebranch$cmp57716:
%ae51493 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51494 = call %struct.ScmObj* @const_init_false()
%argslist56505$k473800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57723 = alloca %struct.ScmObj*, align 8
%argslist56505$k473801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51494, %struct.ScmObj* %argslist56505$k473800)
store volatile %struct.ScmObj* %argslist56505$k473801, %struct.ScmObj** %stackaddr$prim57723, align 8
%stackaddr$prim57724 = alloca %struct.ScmObj*, align 8
%argslist56505$k473802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51493, %struct.ScmObj* %argslist56505$k473801)
store volatile %struct.ScmObj* %argslist56505$k473802, %struct.ScmObj** %stackaddr$prim57724, align 8
%clofunc57725 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47380)
musttail call tailcc void %clofunc57725(%struct.ScmObj* %k47380, %struct.ScmObj* %argslist56505$k473802)
ret void
falsebranch$cmp57713:
%ae51515 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51516 = call %struct.ScmObj* @const_init_false()
%argslist56506$k473800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57726 = alloca %struct.ScmObj*, align 8
%argslist56506$k473801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51516, %struct.ScmObj* %argslist56506$k473800)
store volatile %struct.ScmObj* %argslist56506$k473801, %struct.ScmObj** %stackaddr$prim57726, align 8
%stackaddr$prim57727 = alloca %struct.ScmObj*, align 8
%argslist56506$k473802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51515, %struct.ScmObj* %argslist56506$k473801)
store volatile %struct.ScmObj* %argslist56506$k473802, %struct.ScmObj** %stackaddr$prim57727, align 8
%clofunc57728 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47380)
musttail call tailcc void %clofunc57728(%struct.ScmObj* %k47380, %struct.ScmObj* %argslist56506$k473802)
ret void
}

define tailcc void @proc_clo$ae52345(%struct.ScmObj* %env$ae52345,%struct.ScmObj* %current_45args56510) {
%stackaddr$prim57729 = alloca %struct.ScmObj*, align 8
%_95k47374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56510)
store volatile %struct.ScmObj* %_95k47374, %struct.ScmObj** %stackaddr$prim57729, align 8
%stackaddr$prim57730 = alloca %struct.ScmObj*, align 8
%current_45args56511 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56510)
store volatile %struct.ScmObj* %current_45args56511, %struct.ScmObj** %stackaddr$prim57730, align 8
%stackaddr$prim57731 = alloca %struct.ScmObj*, align 8
%thunk4706847194 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56511)
store volatile %struct.ScmObj* %thunk4706847194, %struct.ScmObj** %stackaddr$prim57731, align 8
%stackaddr$makeclosure57732 = alloca %struct.ScmObj*, align 8
%fptrToInt57733 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52351 to i64
%ae52351 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57733)
store volatile %struct.ScmObj* %ae52351, %struct.ScmObj** %stackaddr$makeclosure57732, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52351, %struct.ScmObj* %thunk4706847194, i64 0)
%ae52352 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57734 = alloca %struct.ScmObj*, align 8
%fptrToInt57735 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52353 to i64
%ae52353 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57735)
store volatile %struct.ScmObj* %ae52353, %struct.ScmObj** %stackaddr$makeclosure57734, align 8
%argslist56550$ae523510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57736 = alloca %struct.ScmObj*, align 8
%argslist56550$ae523511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52353, %struct.ScmObj* %argslist56550$ae523510)
store volatile %struct.ScmObj* %argslist56550$ae523511, %struct.ScmObj** %stackaddr$prim57736, align 8
%stackaddr$prim57737 = alloca %struct.ScmObj*, align 8
%argslist56550$ae523512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52352, %struct.ScmObj* %argslist56550$ae523511)
store volatile %struct.ScmObj* %argslist56550$ae523512, %struct.ScmObj** %stackaddr$prim57737, align 8
%clofunc57738 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52351)
musttail call tailcc void %clofunc57738(%struct.ScmObj* %ae52351, %struct.ScmObj* %argslist56550$ae523512)
ret void
}

define tailcc void @proc_clo$ae52351(%struct.ScmObj* %env$ae52351,%struct.ScmObj* %current_45args56513) {
%stackaddr$env-ref57739 = alloca %struct.ScmObj*, align 8
%thunk4706847194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52351, i64 0)
store %struct.ScmObj* %thunk4706847194, %struct.ScmObj** %stackaddr$env-ref57739
%stackaddr$prim57740 = alloca %struct.ScmObj*, align 8
%_95k47375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56513)
store volatile %struct.ScmObj* %_95k47375, %struct.ScmObj** %stackaddr$prim57740, align 8
%stackaddr$prim57741 = alloca %struct.ScmObj*, align 8
%current_45args56514 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56513)
store volatile %struct.ScmObj* %current_45args56514, %struct.ScmObj** %stackaddr$prim57741, align 8
%stackaddr$prim57742 = alloca %struct.ScmObj*, align 8
%anf_45bind47332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56514)
store volatile %struct.ScmObj* %anf_45bind47332, %struct.ScmObj** %stackaddr$prim57742, align 8
%stackaddr$makeclosure57743 = alloca %struct.ScmObj*, align 8
%fptrToInt57744 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52439 to i64
%ae52439 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57744)
store volatile %struct.ScmObj* %ae52439, %struct.ScmObj** %stackaddr$makeclosure57743, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52439, %struct.ScmObj* %thunk4706847194, i64 0)
%argslist56543$anf_45bind473320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57745 = alloca %struct.ScmObj*, align 8
%argslist56543$anf_45bind473321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %argslist56543$anf_45bind473320)
store volatile %struct.ScmObj* %argslist56543$anf_45bind473321, %struct.ScmObj** %stackaddr$prim57745, align 8
%stackaddr$prim57746 = alloca %struct.ScmObj*, align 8
%argslist56543$anf_45bind473322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52439, %struct.ScmObj* %argslist56543$anf_45bind473321)
store volatile %struct.ScmObj* %argslist56543$anf_45bind473322, %struct.ScmObj** %stackaddr$prim57746, align 8
%clofunc57747 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47332)
musttail call tailcc void %clofunc57747(%struct.ScmObj* %anf_45bind47332, %struct.ScmObj* %argslist56543$anf_45bind473322)
ret void
}

define tailcc void @proc_clo$ae52439(%struct.ScmObj* %env$ae52439,%struct.ScmObj* %current_45args56516) {
%stackaddr$env-ref57748 = alloca %struct.ScmObj*, align 8
%thunk4706847194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52439, i64 0)
store %struct.ScmObj* %thunk4706847194, %struct.ScmObj** %stackaddr$env-ref57748
%stackaddr$prim57749 = alloca %struct.ScmObj*, align 8
%_95k47376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56516)
store volatile %struct.ScmObj* %_95k47376, %struct.ScmObj** %stackaddr$prim57749, align 8
%stackaddr$prim57750 = alloca %struct.ScmObj*, align 8
%current_45args56517 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56516)
store volatile %struct.ScmObj* %current_45args56517, %struct.ScmObj** %stackaddr$prim57750, align 8
%stackaddr$prim57751 = alloca %struct.ScmObj*, align 8
%anf_45bind47333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56517)
store volatile %struct.ScmObj* %anf_45bind47333, %struct.ScmObj** %stackaddr$prim57751, align 8
%truthy$cmp57752 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47333)
%cmp$cmp57752 = icmp eq i64 %truthy$cmp57752, 1
br i1 %cmp$cmp57752, label %truebranch$cmp57752, label %falsebranch$cmp57752
truebranch$cmp57752:
%ae52443 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57753 = alloca %struct.ScmObj*, align 8
%anf_45bind47334 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %ae52443)
store volatile %struct.ScmObj* %anf_45bind47334, %struct.ScmObj** %stackaddr$prim57753, align 8
%truthy$cmp57754 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47334)
%cmp$cmp57754 = icmp eq i64 %truthy$cmp57754, 1
br i1 %cmp$cmp57754, label %truebranch$cmp57754, label %falsebranch$cmp57754
truebranch$cmp57754:
%ae52446 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim57755 = alloca %struct.ScmObj*, align 8
%cpsprim47377 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %ae52446)
store volatile %struct.ScmObj* %cpsprim47377, %struct.ScmObj** %stackaddr$prim57755, align 8
%stackaddr$makeclosure57756 = alloca %struct.ScmObj*, align 8
%fptrToInt57757 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52447 to i64
%ae52447 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57757)
store volatile %struct.ScmObj* %ae52447, %struct.ScmObj** %stackaddr$makeclosure57756, align 8
%ae52448 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56523$ae524470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57758 = alloca %struct.ScmObj*, align 8
%argslist56523$ae524471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47377, %struct.ScmObj* %argslist56523$ae524470)
store volatile %struct.ScmObj* %argslist56523$ae524471, %struct.ScmObj** %stackaddr$prim57758, align 8
%stackaddr$prim57759 = alloca %struct.ScmObj*, align 8
%argslist56523$ae524472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52448, %struct.ScmObj* %argslist56523$ae524471)
store volatile %struct.ScmObj* %argslist56523$ae524472, %struct.ScmObj** %stackaddr$prim57759, align 8
%clofunc57760 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52447)
musttail call tailcc void %clofunc57760(%struct.ScmObj* %ae52447, %struct.ScmObj* %argslist56523$ae524472)
ret void
falsebranch$cmp57754:
%ae52468 = call %struct.ScmObj* @const_init_int(i64 1)
%ae52469 = call %struct.ScmObj* @const_init_true()
%stackaddr$prim57761 = alloca %struct.ScmObj*, align 8
%t4707447203 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %ae52468, %struct.ScmObj* %ae52469)
store volatile %struct.ScmObj* %t4707447203, %struct.ScmObj** %stackaddr$prim57761, align 8
%ae52471 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim57762 = alloca %struct.ScmObj*, align 8
%anf_45bind47335 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %ae52471)
store volatile %struct.ScmObj* %anf_45bind47335, %struct.ScmObj** %stackaddr$prim57762, align 8
%stackaddr$makeclosure57763 = alloca %struct.ScmObj*, align 8
%fptrToInt57764 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52473 to i64
%ae52473 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57764)
store volatile %struct.ScmObj* %ae52473, %struct.ScmObj** %stackaddr$makeclosure57763, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52473, %struct.ScmObj* %thunk4706847194, i64 0)
%ae52474 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @global$sym$ae5247457765, i32 0, i32 0))
%argslist56532$anf_45bind473350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57766 = alloca %struct.ScmObj*, align 8
%argslist56532$anf_45bind473351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52474, %struct.ScmObj* %argslist56532$anf_45bind473350)
store volatile %struct.ScmObj* %argslist56532$anf_45bind473351, %struct.ScmObj** %stackaddr$prim57766, align 8
%stackaddr$prim57767 = alloca %struct.ScmObj*, align 8
%argslist56532$anf_45bind473352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52473, %struct.ScmObj* %argslist56532$anf_45bind473351)
store volatile %struct.ScmObj* %argslist56532$anf_45bind473352, %struct.ScmObj** %stackaddr$prim57767, align 8
%clofunc57768 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47335)
musttail call tailcc void %clofunc57768(%struct.ScmObj* %anf_45bind47335, %struct.ScmObj* %argslist56532$anf_45bind473352)
ret void
falsebranch$cmp57752:
%stackaddr$prim57769 = alloca %struct.ScmObj*, align 8
%anf_45bind47336 = call %struct.ScmObj* @prim_number_63(%struct.ScmObj* %thunk4706847194)
store volatile %struct.ScmObj* %anf_45bind47336, %struct.ScmObj** %stackaddr$prim57769, align 8
%truthy$cmp57770 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47336)
%cmp$cmp57770 = icmp eq i64 %truthy$cmp57770, 1
br i1 %cmp$cmp57770, label %truebranch$cmp57770, label %falsebranch$cmp57770
truebranch$cmp57770:
%stackaddr$makeclosure57771 = alloca %struct.ScmObj*, align 8
%fptrToInt57772 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52603 to i64
%ae52603 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57772)
store volatile %struct.ScmObj* %ae52603, %struct.ScmObj** %stackaddr$makeclosure57771, align 8
%ae52604 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56537$ae526030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57773 = alloca %struct.ScmObj*, align 8
%argslist56537$ae526031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %argslist56537$ae526030)
store volatile %struct.ScmObj* %argslist56537$ae526031, %struct.ScmObj** %stackaddr$prim57773, align 8
%stackaddr$prim57774 = alloca %struct.ScmObj*, align 8
%argslist56537$ae526032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52604, %struct.ScmObj* %argslist56537$ae526031)
store volatile %struct.ScmObj* %argslist56537$ae526032, %struct.ScmObj** %stackaddr$prim57774, align 8
%clofunc57775 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52603)
musttail call tailcc void %clofunc57775(%struct.ScmObj* %ae52603, %struct.ScmObj* %argslist56537$ae526032)
ret void
falsebranch$cmp57770:
%stackaddr$makeclosure57776 = alloca %struct.ScmObj*, align 8
%fptrToInt57777 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52615 to i64
%ae52615 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57777)
store volatile %struct.ScmObj* %ae52615, %struct.ScmObj** %stackaddr$makeclosure57776, align 8
%ae52616 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52617 = call %struct.ScmObj* @const_init_string(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @global$str$ae5261757778, i32 0, i32 0))
%argslist56542$ae526150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57779 = alloca %struct.ScmObj*, align 8
%argslist56542$ae526151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52617, %struct.ScmObj* %argslist56542$ae526150)
store volatile %struct.ScmObj* %argslist56542$ae526151, %struct.ScmObj** %stackaddr$prim57779, align 8
%stackaddr$prim57780 = alloca %struct.ScmObj*, align 8
%argslist56542$ae526152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52616, %struct.ScmObj* %argslist56542$ae526151)
store volatile %struct.ScmObj* %argslist56542$ae526152, %struct.ScmObj** %stackaddr$prim57780, align 8
%clofunc57781 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52615)
musttail call tailcc void %clofunc57781(%struct.ScmObj* %ae52615, %struct.ScmObj* %argslist56542$ae526152)
ret void
}

define tailcc void @proc_clo$ae52447(%struct.ScmObj* %env$ae52447,%struct.ScmObj* %current_45args56519) {
%stackaddr$prim57782 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56519)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim57782, align 8
%stackaddr$prim57783 = alloca %struct.ScmObj*, align 8
%current_45args56520 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56519)
store volatile %struct.ScmObj* %current_45args56520, %struct.ScmObj** %stackaddr$prim57783, align 8
%stackaddr$prim57784 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56520)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim57784, align 8
%stackaddr$prim57785 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim57785, align 8
%argslist56522$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57786 = alloca %struct.ScmObj*, align 8
%argslist56522$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist56522$k0)
store volatile %struct.ScmObj* %argslist56522$k1, %struct.ScmObj** %stackaddr$prim57786, align 8
%clofunc57787 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc57787(%struct.ScmObj* %k, %struct.ScmObj* %argslist56522$k1)
ret void
}

define tailcc void @proc_clo$ae52473(%struct.ScmObj* %env$ae52473,%struct.ScmObj* %current_45args56524) {
%stackaddr$env-ref57788 = alloca %struct.ScmObj*, align 8
%thunk4706847194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52473, i64 0)
store %struct.ScmObj* %thunk4706847194, %struct.ScmObj** %stackaddr$env-ref57788
%stackaddr$prim57789 = alloca %struct.ScmObj*, align 8
%_95k47378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56524)
store volatile %struct.ScmObj* %_95k47378, %struct.ScmObj** %stackaddr$prim57789, align 8
%stackaddr$prim57790 = alloca %struct.ScmObj*, align 8
%current_45args56525 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56524)
store volatile %struct.ScmObj* %current_45args56525, %struct.ScmObj** %stackaddr$prim57790, align 8
%stackaddr$prim57791 = alloca %struct.ScmObj*, align 8
%val4706947205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56525)
store volatile %struct.ScmObj* %val4706947205, %struct.ScmObj** %stackaddr$prim57791, align 8
%ae52479 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim57792 = alloca %struct.ScmObj*, align 8
%t4707547204 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %ae52479, %struct.ScmObj* %val4706947205)
store volatile %struct.ScmObj* %t4707547204, %struct.ScmObj** %stackaddr$prim57792, align 8
%ae52482 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim57793 = alloca %struct.ScmObj*, align 8
%cpsprim47379 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %ae52482)
store volatile %struct.ScmObj* %cpsprim47379, %struct.ScmObj** %stackaddr$prim57793, align 8
%stackaddr$makeclosure57794 = alloca %struct.ScmObj*, align 8
%fptrToInt57795 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52483 to i64
%ae52483 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57795)
store volatile %struct.ScmObj* %ae52483, %struct.ScmObj** %stackaddr$makeclosure57794, align 8
%ae52484 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56531$ae524830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57796 = alloca %struct.ScmObj*, align 8
%argslist56531$ae524831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47379, %struct.ScmObj* %argslist56531$ae524830)
store volatile %struct.ScmObj* %argslist56531$ae524831, %struct.ScmObj** %stackaddr$prim57796, align 8
%stackaddr$prim57797 = alloca %struct.ScmObj*, align 8
%argslist56531$ae524832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52484, %struct.ScmObj* %argslist56531$ae524831)
store volatile %struct.ScmObj* %argslist56531$ae524832, %struct.ScmObj** %stackaddr$prim57797, align 8
%clofunc57798 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52483)
musttail call tailcc void %clofunc57798(%struct.ScmObj* %ae52483, %struct.ScmObj* %argslist56531$ae524832)
ret void
}

define tailcc void @proc_clo$ae52483(%struct.ScmObj* %env$ae52483,%struct.ScmObj* %current_45args56527) {
%stackaddr$prim57799 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56527)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim57799, align 8
%stackaddr$prim57800 = alloca %struct.ScmObj*, align 8
%current_45args56528 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56527)
store volatile %struct.ScmObj* %current_45args56528, %struct.ScmObj** %stackaddr$prim57800, align 8
%stackaddr$prim57801 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56528)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim57801, align 8
%stackaddr$prim57802 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim57802, align 8
%argslist56530$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57803 = alloca %struct.ScmObj*, align 8
%argslist56530$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist56530$k0)
store volatile %struct.ScmObj* %argslist56530$k1, %struct.ScmObj** %stackaddr$prim57803, align 8
%clofunc57804 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc57804(%struct.ScmObj* %k, %struct.ScmObj* %argslist56530$k1)
ret void
}

define tailcc void @proc_clo$ae52603(%struct.ScmObj* %env$ae52603,%struct.ScmObj* %current_45args56533) {
%stackaddr$prim57805 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56533)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim57805, align 8
%stackaddr$prim57806 = alloca %struct.ScmObj*, align 8
%current_45args56534 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56533)
store volatile %struct.ScmObj* %current_45args56534, %struct.ScmObj** %stackaddr$prim57806, align 8
%stackaddr$prim57807 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56534)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim57807, align 8
%stackaddr$prim57808 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim57808, align 8
%argslist56536$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57809 = alloca %struct.ScmObj*, align 8
%argslist56536$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist56536$k0)
store volatile %struct.ScmObj* %argslist56536$k1, %struct.ScmObj** %stackaddr$prim57809, align 8
%clofunc57810 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc57810(%struct.ScmObj* %k, %struct.ScmObj* %argslist56536$k1)
ret void
}

define tailcc void @proc_clo$ae52615(%struct.ScmObj* %env$ae52615,%struct.ScmObj* %current_45args56538) {
%stackaddr$prim57811 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56538)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim57811, align 8
%stackaddr$prim57812 = alloca %struct.ScmObj*, align 8
%current_45args56539 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56538)
store volatile %struct.ScmObj* %current_45args56539, %struct.ScmObj** %stackaddr$prim57812, align 8
%stackaddr$prim57813 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56539)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim57813, align 8
%stackaddr$prim57814 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim57814, align 8
%argslist56541$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57815 = alloca %struct.ScmObj*, align 8
%argslist56541$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist56541$k0)
store volatile %struct.ScmObj* %argslist56541$k1, %struct.ScmObj** %stackaddr$prim57815, align 8
%clofunc57816 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc57816(%struct.ScmObj* %k, %struct.ScmObj* %argslist56541$k1)
ret void
}

define tailcc void @proc_clo$ae52353(%struct.ScmObj* %env$ae52353,%struct.ScmObj* %current_45args56544) {
%stackaddr$prim57817 = alloca %struct.ScmObj*, align 8
%k47380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56544)
store volatile %struct.ScmObj* %k47380, %struct.ScmObj** %stackaddr$prim57817, align 8
%stackaddr$prim57818 = alloca %struct.ScmObj*, align 8
%current_45args56545 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56544)
store volatile %struct.ScmObj* %current_45args56545, %struct.ScmObj** %stackaddr$prim57818, align 8
%stackaddr$prim57819 = alloca %struct.ScmObj*, align 8
%thunk47202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56545)
store volatile %struct.ScmObj* %thunk47202, %struct.ScmObj** %stackaddr$prim57819, align 8
%stackaddr$prim57820 = alloca %struct.ScmObj*, align 8
%anf_45bind47328 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk47202)
store volatile %struct.ScmObj* %anf_45bind47328, %struct.ScmObj** %stackaddr$prim57820, align 8
%truthy$cmp57821 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47328)
%cmp$cmp57821 = icmp eq i64 %truthy$cmp57821, 1
br i1 %cmp$cmp57821, label %truebranch$cmp57821, label %falsebranch$cmp57821
truebranch$cmp57821:
%stackaddr$prim57822 = alloca %struct.ScmObj*, align 8
%anf_45bind47329 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk47202)
store volatile %struct.ScmObj* %anf_45bind47329, %struct.ScmObj** %stackaddr$prim57822, align 8
%ae52358 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim57823 = alloca %struct.ScmObj*, align 8
%anf_45bind47330 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind47329, %struct.ScmObj* %ae52358)
store volatile %struct.ScmObj* %anf_45bind47330, %struct.ScmObj** %stackaddr$prim57823, align 8
%truthy$cmp57824 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47330)
%cmp$cmp57824 = icmp eq i64 %truthy$cmp57824, 1
br i1 %cmp$cmp57824, label %truebranch$cmp57824, label %falsebranch$cmp57824
truebranch$cmp57824:
%ae52361 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57825 = alloca %struct.ScmObj*, align 8
%anf_45bind47331 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk47202, %struct.ScmObj* %ae52361)
store volatile %struct.ScmObj* %anf_45bind47331, %struct.ScmObj** %stackaddr$prim57825, align 8
%ae52363 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae5236357826, i32 0, i32 0))
%stackaddr$prim57827 = alloca %struct.ScmObj*, align 8
%cpsprim47381 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind47331, %struct.ScmObj* %ae52363)
store volatile %struct.ScmObj* %cpsprim47381, %struct.ScmObj** %stackaddr$prim57827, align 8
%ae52365 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56547$k473800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57828 = alloca %struct.ScmObj*, align 8
%argslist56547$k473801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47381, %struct.ScmObj* %argslist56547$k473800)
store volatile %struct.ScmObj* %argslist56547$k473801, %struct.ScmObj** %stackaddr$prim57828, align 8
%stackaddr$prim57829 = alloca %struct.ScmObj*, align 8
%argslist56547$k473802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52365, %struct.ScmObj* %argslist56547$k473801)
store volatile %struct.ScmObj* %argslist56547$k473802, %struct.ScmObj** %stackaddr$prim57829, align 8
%clofunc57830 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47380)
musttail call tailcc void %clofunc57830(%struct.ScmObj* %k47380, %struct.ScmObj* %argslist56547$k473802)
ret void
falsebranch$cmp57824:
%ae52383 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52384 = call %struct.ScmObj* @const_init_false()
%argslist56548$k473800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57831 = alloca %struct.ScmObj*, align 8
%argslist56548$k473801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52384, %struct.ScmObj* %argslist56548$k473800)
store volatile %struct.ScmObj* %argslist56548$k473801, %struct.ScmObj** %stackaddr$prim57831, align 8
%stackaddr$prim57832 = alloca %struct.ScmObj*, align 8
%argslist56548$k473802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52383, %struct.ScmObj* %argslist56548$k473801)
store volatile %struct.ScmObj* %argslist56548$k473802, %struct.ScmObj** %stackaddr$prim57832, align 8
%clofunc57833 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47380)
musttail call tailcc void %clofunc57833(%struct.ScmObj* %k47380, %struct.ScmObj* %argslist56548$k473802)
ret void
falsebranch$cmp57821:
%ae52405 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52406 = call %struct.ScmObj* @const_init_false()
%argslist56549$k473800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57834 = alloca %struct.ScmObj*, align 8
%argslist56549$k473801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52406, %struct.ScmObj* %argslist56549$k473800)
store volatile %struct.ScmObj* %argslist56549$k473801, %struct.ScmObj** %stackaddr$prim57834, align 8
%stackaddr$prim57835 = alloca %struct.ScmObj*, align 8
%argslist56549$k473802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52405, %struct.ScmObj* %argslist56549$k473801)
store volatile %struct.ScmObj* %argslist56549$k473802, %struct.ScmObj** %stackaddr$prim57835, align 8
%clofunc57836 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47380)
musttail call tailcc void %clofunc57836(%struct.ScmObj* %k47380, %struct.ScmObj* %argslist56549$k473802)
ret void
}

define tailcc void @proc_clo$ae52658(%struct.ScmObj* %env$ae52658,%struct.ScmObj* %current_45args56552) {
%stackaddr$prim57837 = alloca %struct.ScmObj*, align 8
%_95k47374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56552)
store volatile %struct.ScmObj* %_95k47374, %struct.ScmObj** %stackaddr$prim57837, align 8
%stackaddr$prim57838 = alloca %struct.ScmObj*, align 8
%current_45args56553 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56552)
store volatile %struct.ScmObj* %current_45args56553, %struct.ScmObj** %stackaddr$prim57838, align 8
%stackaddr$prim57839 = alloca %struct.ScmObj*, align 8
%thunk4706847194 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56553)
store volatile %struct.ScmObj* %thunk4706847194, %struct.ScmObj** %stackaddr$prim57839, align 8
%stackaddr$makeclosure57840 = alloca %struct.ScmObj*, align 8
%fptrToInt57841 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52667 to i64
%ae52667 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57841)
store volatile %struct.ScmObj* %ae52667, %struct.ScmObj** %stackaddr$makeclosure57840, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52667, %struct.ScmObj* %thunk4706847194, i64 0)
%ae52668 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57842 = alloca %struct.ScmObj*, align 8
%fptrToInt57843 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52669 to i64
%ae52669 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57843)
store volatile %struct.ScmObj* %ae52669, %struct.ScmObj** %stackaddr$makeclosure57842, align 8
%argslist56592$ae526670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57844 = alloca %struct.ScmObj*, align 8
%argslist56592$ae526671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52669, %struct.ScmObj* %argslist56592$ae526670)
store volatile %struct.ScmObj* %argslist56592$ae526671, %struct.ScmObj** %stackaddr$prim57844, align 8
%stackaddr$prim57845 = alloca %struct.ScmObj*, align 8
%argslist56592$ae526672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52668, %struct.ScmObj* %argslist56592$ae526671)
store volatile %struct.ScmObj* %argslist56592$ae526672, %struct.ScmObj** %stackaddr$prim57845, align 8
%clofunc57846 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52667)
musttail call tailcc void %clofunc57846(%struct.ScmObj* %ae52667, %struct.ScmObj* %argslist56592$ae526672)
ret void
}

define tailcc void @proc_clo$ae52667(%struct.ScmObj* %env$ae52667,%struct.ScmObj* %current_45args56555) {
%stackaddr$env-ref57847 = alloca %struct.ScmObj*, align 8
%thunk4706847194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52667, i64 0)
store %struct.ScmObj* %thunk4706847194, %struct.ScmObj** %stackaddr$env-ref57847
%stackaddr$prim57848 = alloca %struct.ScmObj*, align 8
%_95k47375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56555)
store volatile %struct.ScmObj* %_95k47375, %struct.ScmObj** %stackaddr$prim57848, align 8
%stackaddr$prim57849 = alloca %struct.ScmObj*, align 8
%current_45args56556 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56555)
store volatile %struct.ScmObj* %current_45args56556, %struct.ScmObj** %stackaddr$prim57849, align 8
%stackaddr$prim57850 = alloca %struct.ScmObj*, align 8
%anf_45bind47332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56556)
store volatile %struct.ScmObj* %anf_45bind47332, %struct.ScmObj** %stackaddr$prim57850, align 8
%stackaddr$makeclosure57851 = alloca %struct.ScmObj*, align 8
%fptrToInt57852 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52755 to i64
%ae52755 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57852)
store volatile %struct.ScmObj* %ae52755, %struct.ScmObj** %stackaddr$makeclosure57851, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52755, %struct.ScmObj* %thunk4706847194, i64 0)
%argslist56585$anf_45bind473320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57853 = alloca %struct.ScmObj*, align 8
%argslist56585$anf_45bind473321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %argslist56585$anf_45bind473320)
store volatile %struct.ScmObj* %argslist56585$anf_45bind473321, %struct.ScmObj** %stackaddr$prim57853, align 8
%stackaddr$prim57854 = alloca %struct.ScmObj*, align 8
%argslist56585$anf_45bind473322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52755, %struct.ScmObj* %argslist56585$anf_45bind473321)
store volatile %struct.ScmObj* %argslist56585$anf_45bind473322, %struct.ScmObj** %stackaddr$prim57854, align 8
%clofunc57855 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47332)
musttail call tailcc void %clofunc57855(%struct.ScmObj* %anf_45bind47332, %struct.ScmObj* %argslist56585$anf_45bind473322)
ret void
}

define tailcc void @proc_clo$ae52755(%struct.ScmObj* %env$ae52755,%struct.ScmObj* %current_45args56558) {
%stackaddr$env-ref57856 = alloca %struct.ScmObj*, align 8
%thunk4706847194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52755, i64 0)
store %struct.ScmObj* %thunk4706847194, %struct.ScmObj** %stackaddr$env-ref57856
%stackaddr$prim57857 = alloca %struct.ScmObj*, align 8
%_95k47376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56558)
store volatile %struct.ScmObj* %_95k47376, %struct.ScmObj** %stackaddr$prim57857, align 8
%stackaddr$prim57858 = alloca %struct.ScmObj*, align 8
%current_45args56559 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56558)
store volatile %struct.ScmObj* %current_45args56559, %struct.ScmObj** %stackaddr$prim57858, align 8
%stackaddr$prim57859 = alloca %struct.ScmObj*, align 8
%anf_45bind47333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56559)
store volatile %struct.ScmObj* %anf_45bind47333, %struct.ScmObj** %stackaddr$prim57859, align 8
%truthy$cmp57860 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47333)
%cmp$cmp57860 = icmp eq i64 %truthy$cmp57860, 1
br i1 %cmp$cmp57860, label %truebranch$cmp57860, label %falsebranch$cmp57860
truebranch$cmp57860:
%ae52759 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57861 = alloca %struct.ScmObj*, align 8
%anf_45bind47334 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %ae52759)
store volatile %struct.ScmObj* %anf_45bind47334, %struct.ScmObj** %stackaddr$prim57861, align 8
%truthy$cmp57862 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47334)
%cmp$cmp57862 = icmp eq i64 %truthy$cmp57862, 1
br i1 %cmp$cmp57862, label %truebranch$cmp57862, label %falsebranch$cmp57862
truebranch$cmp57862:
%ae52762 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim57863 = alloca %struct.ScmObj*, align 8
%cpsprim47377 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %ae52762)
store volatile %struct.ScmObj* %cpsprim47377, %struct.ScmObj** %stackaddr$prim57863, align 8
%stackaddr$makeclosure57864 = alloca %struct.ScmObj*, align 8
%fptrToInt57865 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52763 to i64
%ae52763 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57865)
store volatile %struct.ScmObj* %ae52763, %struct.ScmObj** %stackaddr$makeclosure57864, align 8
%ae52764 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56565$ae527630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57866 = alloca %struct.ScmObj*, align 8
%argslist56565$ae527631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47377, %struct.ScmObj* %argslist56565$ae527630)
store volatile %struct.ScmObj* %argslist56565$ae527631, %struct.ScmObj** %stackaddr$prim57866, align 8
%stackaddr$prim57867 = alloca %struct.ScmObj*, align 8
%argslist56565$ae527632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52764, %struct.ScmObj* %argslist56565$ae527631)
store volatile %struct.ScmObj* %argslist56565$ae527632, %struct.ScmObj** %stackaddr$prim57867, align 8
%clofunc57868 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52763)
musttail call tailcc void %clofunc57868(%struct.ScmObj* %ae52763, %struct.ScmObj* %argslist56565$ae527632)
ret void
falsebranch$cmp57862:
%ae52784 = call %struct.ScmObj* @const_init_int(i64 1)
%ae52785 = call %struct.ScmObj* @const_init_true()
%stackaddr$prim57869 = alloca %struct.ScmObj*, align 8
%t4707447203 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %ae52784, %struct.ScmObj* %ae52785)
store volatile %struct.ScmObj* %t4707447203, %struct.ScmObj** %stackaddr$prim57869, align 8
%ae52787 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim57870 = alloca %struct.ScmObj*, align 8
%anf_45bind47335 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %ae52787)
store volatile %struct.ScmObj* %anf_45bind47335, %struct.ScmObj** %stackaddr$prim57870, align 8
%stackaddr$makeclosure57871 = alloca %struct.ScmObj*, align 8
%fptrToInt57872 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52789 to i64
%ae52789 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57872)
store volatile %struct.ScmObj* %ae52789, %struct.ScmObj** %stackaddr$makeclosure57871, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52789, %struct.ScmObj* %thunk4706847194, i64 0)
%ae52790 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @global$sym$ae5279057873, i32 0, i32 0))
%argslist56574$anf_45bind473350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57874 = alloca %struct.ScmObj*, align 8
%argslist56574$anf_45bind473351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52790, %struct.ScmObj* %argslist56574$anf_45bind473350)
store volatile %struct.ScmObj* %argslist56574$anf_45bind473351, %struct.ScmObj** %stackaddr$prim57874, align 8
%stackaddr$prim57875 = alloca %struct.ScmObj*, align 8
%argslist56574$anf_45bind473352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52789, %struct.ScmObj* %argslist56574$anf_45bind473351)
store volatile %struct.ScmObj* %argslist56574$anf_45bind473352, %struct.ScmObj** %stackaddr$prim57875, align 8
%clofunc57876 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47335)
musttail call tailcc void %clofunc57876(%struct.ScmObj* %anf_45bind47335, %struct.ScmObj* %argslist56574$anf_45bind473352)
ret void
falsebranch$cmp57860:
%stackaddr$prim57877 = alloca %struct.ScmObj*, align 8
%anf_45bind47336 = call %struct.ScmObj* @prim_number_63(%struct.ScmObj* %thunk4706847194)
store volatile %struct.ScmObj* %anf_45bind47336, %struct.ScmObj** %stackaddr$prim57877, align 8
%truthy$cmp57878 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47336)
%cmp$cmp57878 = icmp eq i64 %truthy$cmp57878, 1
br i1 %cmp$cmp57878, label %truebranch$cmp57878, label %falsebranch$cmp57878
truebranch$cmp57878:
%stackaddr$makeclosure57879 = alloca %struct.ScmObj*, align 8
%fptrToInt57880 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52919 to i64
%ae52919 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57880)
store volatile %struct.ScmObj* %ae52919, %struct.ScmObj** %stackaddr$makeclosure57879, align 8
%ae52920 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56579$ae529190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57881 = alloca %struct.ScmObj*, align 8
%argslist56579$ae529191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %argslist56579$ae529190)
store volatile %struct.ScmObj* %argslist56579$ae529191, %struct.ScmObj** %stackaddr$prim57881, align 8
%stackaddr$prim57882 = alloca %struct.ScmObj*, align 8
%argslist56579$ae529192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52920, %struct.ScmObj* %argslist56579$ae529191)
store volatile %struct.ScmObj* %argslist56579$ae529192, %struct.ScmObj** %stackaddr$prim57882, align 8
%clofunc57883 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52919)
musttail call tailcc void %clofunc57883(%struct.ScmObj* %ae52919, %struct.ScmObj* %argslist56579$ae529192)
ret void
falsebranch$cmp57878:
%stackaddr$makeclosure57884 = alloca %struct.ScmObj*, align 8
%fptrToInt57885 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52931 to i64
%ae52931 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57885)
store volatile %struct.ScmObj* %ae52931, %struct.ScmObj** %stackaddr$makeclosure57884, align 8
%ae52932 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52933 = call %struct.ScmObj* @const_init_string(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @global$str$ae5293357886, i32 0, i32 0))
%argslist56584$ae529310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57887 = alloca %struct.ScmObj*, align 8
%argslist56584$ae529311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52933, %struct.ScmObj* %argslist56584$ae529310)
store volatile %struct.ScmObj* %argslist56584$ae529311, %struct.ScmObj** %stackaddr$prim57887, align 8
%stackaddr$prim57888 = alloca %struct.ScmObj*, align 8
%argslist56584$ae529312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52932, %struct.ScmObj* %argslist56584$ae529311)
store volatile %struct.ScmObj* %argslist56584$ae529312, %struct.ScmObj** %stackaddr$prim57888, align 8
%clofunc57889 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52931)
musttail call tailcc void %clofunc57889(%struct.ScmObj* %ae52931, %struct.ScmObj* %argslist56584$ae529312)
ret void
}

define tailcc void @proc_clo$ae52763(%struct.ScmObj* %env$ae52763,%struct.ScmObj* %current_45args56561) {
%stackaddr$prim57890 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56561)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim57890, align 8
%stackaddr$prim57891 = alloca %struct.ScmObj*, align 8
%current_45args56562 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56561)
store volatile %struct.ScmObj* %current_45args56562, %struct.ScmObj** %stackaddr$prim57891, align 8
%stackaddr$prim57892 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56562)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim57892, align 8
%stackaddr$prim57893 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim57893, align 8
%argslist56564$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57894 = alloca %struct.ScmObj*, align 8
%argslist56564$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist56564$k0)
store volatile %struct.ScmObj* %argslist56564$k1, %struct.ScmObj** %stackaddr$prim57894, align 8
%clofunc57895 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc57895(%struct.ScmObj* %k, %struct.ScmObj* %argslist56564$k1)
ret void
}

define tailcc void @proc_clo$ae52789(%struct.ScmObj* %env$ae52789,%struct.ScmObj* %current_45args56566) {
%stackaddr$env-ref57896 = alloca %struct.ScmObj*, align 8
%thunk4706847194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52789, i64 0)
store %struct.ScmObj* %thunk4706847194, %struct.ScmObj** %stackaddr$env-ref57896
%stackaddr$prim57897 = alloca %struct.ScmObj*, align 8
%_95k47378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56566)
store volatile %struct.ScmObj* %_95k47378, %struct.ScmObj** %stackaddr$prim57897, align 8
%stackaddr$prim57898 = alloca %struct.ScmObj*, align 8
%current_45args56567 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56566)
store volatile %struct.ScmObj* %current_45args56567, %struct.ScmObj** %stackaddr$prim57898, align 8
%stackaddr$prim57899 = alloca %struct.ScmObj*, align 8
%val4706947205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56567)
store volatile %struct.ScmObj* %val4706947205, %struct.ScmObj** %stackaddr$prim57899, align 8
%ae52795 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim57900 = alloca %struct.ScmObj*, align 8
%t4707547204 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %ae52795, %struct.ScmObj* %val4706947205)
store volatile %struct.ScmObj* %t4707547204, %struct.ScmObj** %stackaddr$prim57900, align 8
%ae52798 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim57901 = alloca %struct.ScmObj*, align 8
%cpsprim47379 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4706847194, %struct.ScmObj* %ae52798)
store volatile %struct.ScmObj* %cpsprim47379, %struct.ScmObj** %stackaddr$prim57901, align 8
%stackaddr$makeclosure57902 = alloca %struct.ScmObj*, align 8
%fptrToInt57903 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52799 to i64
%ae52799 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57903)
store volatile %struct.ScmObj* %ae52799, %struct.ScmObj** %stackaddr$makeclosure57902, align 8
%ae52800 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56573$ae527990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57904 = alloca %struct.ScmObj*, align 8
%argslist56573$ae527991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47379, %struct.ScmObj* %argslist56573$ae527990)
store volatile %struct.ScmObj* %argslist56573$ae527991, %struct.ScmObj** %stackaddr$prim57904, align 8
%stackaddr$prim57905 = alloca %struct.ScmObj*, align 8
%argslist56573$ae527992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52800, %struct.ScmObj* %argslist56573$ae527991)
store volatile %struct.ScmObj* %argslist56573$ae527992, %struct.ScmObj** %stackaddr$prim57905, align 8
%clofunc57906 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52799)
musttail call tailcc void %clofunc57906(%struct.ScmObj* %ae52799, %struct.ScmObj* %argslist56573$ae527992)
ret void
}

define tailcc void @proc_clo$ae52799(%struct.ScmObj* %env$ae52799,%struct.ScmObj* %current_45args56569) {
%stackaddr$prim57907 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56569)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim57907, align 8
%stackaddr$prim57908 = alloca %struct.ScmObj*, align 8
%current_45args56570 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56569)
store volatile %struct.ScmObj* %current_45args56570, %struct.ScmObj** %stackaddr$prim57908, align 8
%stackaddr$prim57909 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56570)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim57909, align 8
%stackaddr$prim57910 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim57910, align 8
%argslist56572$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57911 = alloca %struct.ScmObj*, align 8
%argslist56572$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist56572$k0)
store volatile %struct.ScmObj* %argslist56572$k1, %struct.ScmObj** %stackaddr$prim57911, align 8
%clofunc57912 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc57912(%struct.ScmObj* %k, %struct.ScmObj* %argslist56572$k1)
ret void
}

define tailcc void @proc_clo$ae52919(%struct.ScmObj* %env$ae52919,%struct.ScmObj* %current_45args56575) {
%stackaddr$prim57913 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56575)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim57913, align 8
%stackaddr$prim57914 = alloca %struct.ScmObj*, align 8
%current_45args56576 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56575)
store volatile %struct.ScmObj* %current_45args56576, %struct.ScmObj** %stackaddr$prim57914, align 8
%stackaddr$prim57915 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56576)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim57915, align 8
%stackaddr$prim57916 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim57916, align 8
%argslist56578$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57917 = alloca %struct.ScmObj*, align 8
%argslist56578$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist56578$k0)
store volatile %struct.ScmObj* %argslist56578$k1, %struct.ScmObj** %stackaddr$prim57917, align 8
%clofunc57918 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc57918(%struct.ScmObj* %k, %struct.ScmObj* %argslist56578$k1)
ret void
}

define tailcc void @proc_clo$ae52931(%struct.ScmObj* %env$ae52931,%struct.ScmObj* %current_45args56580) {
%stackaddr$prim57919 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56580)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim57919, align 8
%stackaddr$prim57920 = alloca %struct.ScmObj*, align 8
%current_45args56581 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56580)
store volatile %struct.ScmObj* %current_45args56581, %struct.ScmObj** %stackaddr$prim57920, align 8
%stackaddr$prim57921 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56581)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim57921, align 8
%stackaddr$prim57922 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim57922, align 8
%argslist56583$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57923 = alloca %struct.ScmObj*, align 8
%argslist56583$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist56583$k0)
store volatile %struct.ScmObj* %argslist56583$k1, %struct.ScmObj** %stackaddr$prim57923, align 8
%clofunc57924 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc57924(%struct.ScmObj* %k, %struct.ScmObj* %argslist56583$k1)
ret void
}

define tailcc void @proc_clo$ae52669(%struct.ScmObj* %env$ae52669,%struct.ScmObj* %current_45args56586) {
%stackaddr$prim57925 = alloca %struct.ScmObj*, align 8
%k47380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56586)
store volatile %struct.ScmObj* %k47380, %struct.ScmObj** %stackaddr$prim57925, align 8
%stackaddr$prim57926 = alloca %struct.ScmObj*, align 8
%current_45args56587 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56586)
store volatile %struct.ScmObj* %current_45args56587, %struct.ScmObj** %stackaddr$prim57926, align 8
%stackaddr$prim57927 = alloca %struct.ScmObj*, align 8
%thunk47202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56587)
store volatile %struct.ScmObj* %thunk47202, %struct.ScmObj** %stackaddr$prim57927, align 8
%stackaddr$prim57928 = alloca %struct.ScmObj*, align 8
%anf_45bind47328 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk47202)
store volatile %struct.ScmObj* %anf_45bind47328, %struct.ScmObj** %stackaddr$prim57928, align 8
%truthy$cmp57929 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47328)
%cmp$cmp57929 = icmp eq i64 %truthy$cmp57929, 1
br i1 %cmp$cmp57929, label %truebranch$cmp57929, label %falsebranch$cmp57929
truebranch$cmp57929:
%stackaddr$prim57930 = alloca %struct.ScmObj*, align 8
%anf_45bind47329 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk47202)
store volatile %struct.ScmObj* %anf_45bind47329, %struct.ScmObj** %stackaddr$prim57930, align 8
%ae52674 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim57931 = alloca %struct.ScmObj*, align 8
%anf_45bind47330 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind47329, %struct.ScmObj* %ae52674)
store volatile %struct.ScmObj* %anf_45bind47330, %struct.ScmObj** %stackaddr$prim57931, align 8
%truthy$cmp57932 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47330)
%cmp$cmp57932 = icmp eq i64 %truthy$cmp57932, 1
br i1 %cmp$cmp57932, label %truebranch$cmp57932, label %falsebranch$cmp57932
truebranch$cmp57932:
%ae52677 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57933 = alloca %struct.ScmObj*, align 8
%anf_45bind47331 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk47202, %struct.ScmObj* %ae52677)
store volatile %struct.ScmObj* %anf_45bind47331, %struct.ScmObj** %stackaddr$prim57933, align 8
%ae52679 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae5267957934, i32 0, i32 0))
%stackaddr$prim57935 = alloca %struct.ScmObj*, align 8
%cpsprim47381 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind47331, %struct.ScmObj* %ae52679)
store volatile %struct.ScmObj* %cpsprim47381, %struct.ScmObj** %stackaddr$prim57935, align 8
%ae52681 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56589$k473800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57936 = alloca %struct.ScmObj*, align 8
%argslist56589$k473801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47381, %struct.ScmObj* %argslist56589$k473800)
store volatile %struct.ScmObj* %argslist56589$k473801, %struct.ScmObj** %stackaddr$prim57936, align 8
%stackaddr$prim57937 = alloca %struct.ScmObj*, align 8
%argslist56589$k473802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52681, %struct.ScmObj* %argslist56589$k473801)
store volatile %struct.ScmObj* %argslist56589$k473802, %struct.ScmObj** %stackaddr$prim57937, align 8
%clofunc57938 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47380)
musttail call tailcc void %clofunc57938(%struct.ScmObj* %k47380, %struct.ScmObj* %argslist56589$k473802)
ret void
falsebranch$cmp57932:
%ae52699 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52700 = call %struct.ScmObj* @const_init_false()
%argslist56590$k473800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57939 = alloca %struct.ScmObj*, align 8
%argslist56590$k473801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52700, %struct.ScmObj* %argslist56590$k473800)
store volatile %struct.ScmObj* %argslist56590$k473801, %struct.ScmObj** %stackaddr$prim57939, align 8
%stackaddr$prim57940 = alloca %struct.ScmObj*, align 8
%argslist56590$k473802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52699, %struct.ScmObj* %argslist56590$k473801)
store volatile %struct.ScmObj* %argslist56590$k473802, %struct.ScmObj** %stackaddr$prim57940, align 8
%clofunc57941 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47380)
musttail call tailcc void %clofunc57941(%struct.ScmObj* %k47380, %struct.ScmObj* %argslist56590$k473802)
ret void
falsebranch$cmp57929:
%ae52721 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52722 = call %struct.ScmObj* @const_init_false()
%argslist56591$k473800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57942 = alloca %struct.ScmObj*, align 8
%argslist56591$k473801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52722, %struct.ScmObj* %argslist56591$k473800)
store volatile %struct.ScmObj* %argslist56591$k473801, %struct.ScmObj** %stackaddr$prim57942, align 8
%stackaddr$prim57943 = alloca %struct.ScmObj*, align 8
%argslist56591$k473802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52721, %struct.ScmObj* %argslist56591$k473801)
store volatile %struct.ScmObj* %argslist56591$k473802, %struct.ScmObj** %stackaddr$prim57943, align 8
%clofunc57944 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47380)
musttail call tailcc void %clofunc57944(%struct.ScmObj* %k47380, %struct.ScmObj* %argslist56591$k473802)
ret void
}

define tailcc void @proc_clo$ae50957(%struct.ScmObj* %env$ae50957,%struct.ScmObj* %current_45args56595) {
%stackaddr$prim57945 = alloca %struct.ScmObj*, align 8
%k47385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56595)
store volatile %struct.ScmObj* %k47385, %struct.ScmObj** %stackaddr$prim57945, align 8
%stackaddr$prim57946 = alloca %struct.ScmObj*, align 8
%current_45args56596 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56595)
store volatile %struct.ScmObj* %current_45args56596, %struct.ScmObj** %stackaddr$prim57946, align 8
%stackaddr$prim57947 = alloca %struct.ScmObj*, align 8
%thunk47198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56596)
store volatile %struct.ScmObj* %thunk47198, %struct.ScmObj** %stackaddr$prim57947, align 8
%stackaddr$prim57948 = alloca %struct.ScmObj*, align 8
%anf_45bind47319 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk47198)
store volatile %struct.ScmObj* %anf_45bind47319, %struct.ScmObj** %stackaddr$prim57948, align 8
%truthy$cmp57949 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47319)
%cmp$cmp57949 = icmp eq i64 %truthy$cmp57949, 1
br i1 %cmp$cmp57949, label %truebranch$cmp57949, label %falsebranch$cmp57949
truebranch$cmp57949:
%stackaddr$prim57950 = alloca %struct.ScmObj*, align 8
%anf_45bind47320 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk47198)
store volatile %struct.ScmObj* %anf_45bind47320, %struct.ScmObj** %stackaddr$prim57950, align 8
%ae50962 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim57951 = alloca %struct.ScmObj*, align 8
%anf_45bind47321 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind47320, %struct.ScmObj* %ae50962)
store volatile %struct.ScmObj* %anf_45bind47321, %struct.ScmObj** %stackaddr$prim57951, align 8
%truthy$cmp57952 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47321)
%cmp$cmp57952 = icmp eq i64 %truthy$cmp57952, 1
br i1 %cmp$cmp57952, label %truebranch$cmp57952, label %falsebranch$cmp57952
truebranch$cmp57952:
%ae50965 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57953 = alloca %struct.ScmObj*, align 8
%anf_45bind47322 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk47198, %struct.ScmObj* %ae50965)
store volatile %struct.ScmObj* %anf_45bind47322, %struct.ScmObj** %stackaddr$prim57953, align 8
%ae50967 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae5096757954, i32 0, i32 0))
%stackaddr$prim57955 = alloca %struct.ScmObj*, align 8
%cpsprim47386 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind47322, %struct.ScmObj* %ae50967)
store volatile %struct.ScmObj* %cpsprim47386, %struct.ScmObj** %stackaddr$prim57955, align 8
%ae50969 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56598$k473850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57956 = alloca %struct.ScmObj*, align 8
%argslist56598$k473851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47386, %struct.ScmObj* %argslist56598$k473850)
store volatile %struct.ScmObj* %argslist56598$k473851, %struct.ScmObj** %stackaddr$prim57956, align 8
%stackaddr$prim57957 = alloca %struct.ScmObj*, align 8
%argslist56598$k473852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50969, %struct.ScmObj* %argslist56598$k473851)
store volatile %struct.ScmObj* %argslist56598$k473852, %struct.ScmObj** %stackaddr$prim57957, align 8
%clofunc57958 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47385)
musttail call tailcc void %clofunc57958(%struct.ScmObj* %k47385, %struct.ScmObj* %argslist56598$k473852)
ret void
falsebranch$cmp57952:
%ae50987 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50988 = call %struct.ScmObj* @const_init_false()
%argslist56599$k473850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57959 = alloca %struct.ScmObj*, align 8
%argslist56599$k473851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50988, %struct.ScmObj* %argslist56599$k473850)
store volatile %struct.ScmObj* %argslist56599$k473851, %struct.ScmObj** %stackaddr$prim57959, align 8
%stackaddr$prim57960 = alloca %struct.ScmObj*, align 8
%argslist56599$k473852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50987, %struct.ScmObj* %argslist56599$k473851)
store volatile %struct.ScmObj* %argslist56599$k473852, %struct.ScmObj** %stackaddr$prim57960, align 8
%clofunc57961 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47385)
musttail call tailcc void %clofunc57961(%struct.ScmObj* %k47385, %struct.ScmObj* %argslist56599$k473852)
ret void
falsebranch$cmp57949:
%ae51009 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51010 = call %struct.ScmObj* @const_init_false()
%argslist56600$k473850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57962 = alloca %struct.ScmObj*, align 8
%argslist56600$k473851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51010, %struct.ScmObj* %argslist56600$k473850)
store volatile %struct.ScmObj* %argslist56600$k473851, %struct.ScmObj** %stackaddr$prim57962, align 8
%stackaddr$prim57963 = alloca %struct.ScmObj*, align 8
%argslist56600$k473852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51009, %struct.ScmObj* %argslist56600$k473851)
store volatile %struct.ScmObj* %argslist56600$k473852, %struct.ScmObj** %stackaddr$prim57963, align 8
%clofunc57964 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47385)
musttail call tailcc void %clofunc57964(%struct.ScmObj* %k47385, %struct.ScmObj* %argslist56600$k473852)
ret void
}

define tailcc void @proc_clo$ae50921(%struct.ScmObj* %env$ae50921,%struct.ScmObj* %current_45args56603) {
%stackaddr$prim57965 = alloca %struct.ScmObj*, align 8
%k47387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56603)
store volatile %struct.ScmObj* %k47387, %struct.ScmObj** %stackaddr$prim57965, align 8
%stackaddr$prim57966 = alloca %struct.ScmObj*, align 8
%current_45args56604 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56603)
store volatile %struct.ScmObj* %current_45args56604, %struct.ScmObj** %stackaddr$prim57966, align 8
%stackaddr$prim57967 = alloca %struct.ScmObj*, align 8
%_9547197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56604)
store volatile %struct.ScmObj* %_9547197, %struct.ScmObj** %stackaddr$prim57967, align 8
%ae50923 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50924 = call %struct.ScmObj* @const_init_int(i64 29)
%argslist56606$k473870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57968 = alloca %struct.ScmObj*, align 8
%argslist56606$k473871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50924, %struct.ScmObj* %argslist56606$k473870)
store volatile %struct.ScmObj* %argslist56606$k473871, %struct.ScmObj** %stackaddr$prim57968, align 8
%stackaddr$prim57969 = alloca %struct.ScmObj*, align 8
%argslist56606$k473872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50923, %struct.ScmObj* %argslist56606$k473871)
store volatile %struct.ScmObj* %argslist56606$k473872, %struct.ScmObj** %stackaddr$prim57969, align 8
%clofunc57970 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47387)
musttail call tailcc void %clofunc57970(%struct.ScmObj* %k47387, %struct.ScmObj* %argslist56606$k473872)
ret void
}

define tailcc void @proc_clo$ae50897(%struct.ScmObj* %env$ae50897,%struct.ScmObj* %el4719647388) {
%stackaddr$prim57971 = alloca %struct.ScmObj*, align 8
%k47389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %el4719647388)
store volatile %struct.ScmObj* %k47389, %struct.ScmObj** %stackaddr$prim57971, align 8
%stackaddr$prim57972 = alloca %struct.ScmObj*, align 8
%el47196 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %el4719647388)
store volatile %struct.ScmObj* %el47196, %struct.ScmObj** %stackaddr$prim57972, align 8
%stackaddr$applyprim57973 = alloca %struct.ScmObj*, align 8
%cpsaprim47390 = call %struct.ScmObj* @applyprim_vector(%struct.ScmObj* %el47196)
store volatile %struct.ScmObj* %cpsaprim47390, %struct.ScmObj** %stackaddr$applyprim57973, align 8
%ae50902 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56608$k473890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57974 = alloca %struct.ScmObj*, align 8
%argslist56608$k473891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim47390, %struct.ScmObj* %argslist56608$k473890)
store volatile %struct.ScmObj* %argslist56608$k473891, %struct.ScmObj** %stackaddr$prim57974, align 8
%stackaddr$prim57975 = alloca %struct.ScmObj*, align 8
%argslist56608$k473892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50902, %struct.ScmObj* %argslist56608$k473891)
store volatile %struct.ScmObj* %argslist56608$k473892, %struct.ScmObj** %stackaddr$prim57975, align 8
%clofunc57976 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47389)
musttail call tailcc void %clofunc57976(%struct.ScmObj* %k47389, %struct.ScmObj* %argslist56608$k473892)
ret void
}

define tailcc void @proc_clo$ae50871(%struct.ScmObj* %env$ae50871,%struct.ScmObj* %current_45args56610) {
%stackaddr$prim57977 = alloca %struct.ScmObj*, align 8
%k47391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56610)
store volatile %struct.ScmObj* %k47391, %struct.ScmObj** %stackaddr$prim57977, align 8
%stackaddr$prim57978 = alloca %struct.ScmObj*, align 8
%current_45args56611 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56610)
store volatile %struct.ScmObj* %current_45args56611, %struct.ScmObj** %stackaddr$prim57978, align 8
%stackaddr$prim57979 = alloca %struct.ScmObj*, align 8
%x47134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56611)
store volatile %struct.ScmObj* %x47134, %struct.ScmObj** %stackaddr$prim57979, align 8
%stackaddr$prim57980 = alloca %struct.ScmObj*, align 8
%anf_45bind47314 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47134)
store volatile %struct.ScmObj* %anf_45bind47314, %struct.ScmObj** %stackaddr$prim57980, align 8
%stackaddr$prim57981 = alloca %struct.ScmObj*, align 8
%anf_45bind47315 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47314)
store volatile %struct.ScmObj* %anf_45bind47315, %struct.ScmObj** %stackaddr$prim57981, align 8
%stackaddr$prim57982 = alloca %struct.ScmObj*, align 8
%anf_45bind47316 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47315)
store volatile %struct.ScmObj* %anf_45bind47316, %struct.ScmObj** %stackaddr$prim57982, align 8
%stackaddr$prim57983 = alloca %struct.ScmObj*, align 8
%cpsprim47392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47316)
store volatile %struct.ScmObj* %cpsprim47392, %struct.ScmObj** %stackaddr$prim57983, align 8
%ae50877 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56613$k473910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57984 = alloca %struct.ScmObj*, align 8
%argslist56613$k473911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47392, %struct.ScmObj* %argslist56613$k473910)
store volatile %struct.ScmObj* %argslist56613$k473911, %struct.ScmObj** %stackaddr$prim57984, align 8
%stackaddr$prim57985 = alloca %struct.ScmObj*, align 8
%argslist56613$k473912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50877, %struct.ScmObj* %argslist56613$k473911)
store volatile %struct.ScmObj* %argslist56613$k473912, %struct.ScmObj** %stackaddr$prim57985, align 8
%clofunc57986 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47391)
musttail call tailcc void %clofunc57986(%struct.ScmObj* %k47391, %struct.ScmObj* %argslist56613$k473912)
ret void
}

define tailcc void @proc_clo$ae50847(%struct.ScmObj* %env$ae50847,%struct.ScmObj* %current_45args56615) {
%stackaddr$prim57987 = alloca %struct.ScmObj*, align 8
%k47393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56615)
store volatile %struct.ScmObj* %k47393, %struct.ScmObj** %stackaddr$prim57987, align 8
%stackaddr$prim57988 = alloca %struct.ScmObj*, align 8
%current_45args56616 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56615)
store volatile %struct.ScmObj* %current_45args56616, %struct.ScmObj** %stackaddr$prim57988, align 8
%stackaddr$prim57989 = alloca %struct.ScmObj*, align 8
%x47136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56616)
store volatile %struct.ScmObj* %x47136, %struct.ScmObj** %stackaddr$prim57989, align 8
%stackaddr$prim57990 = alloca %struct.ScmObj*, align 8
%anf_45bind47312 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47136)
store volatile %struct.ScmObj* %anf_45bind47312, %struct.ScmObj** %stackaddr$prim57990, align 8
%stackaddr$prim57991 = alloca %struct.ScmObj*, align 8
%anf_45bind47313 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47312)
store volatile %struct.ScmObj* %anf_45bind47313, %struct.ScmObj** %stackaddr$prim57991, align 8
%stackaddr$prim57992 = alloca %struct.ScmObj*, align 8
%cpsprim47394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47313)
store volatile %struct.ScmObj* %cpsprim47394, %struct.ScmObj** %stackaddr$prim57992, align 8
%ae50852 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56618$k473930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57993 = alloca %struct.ScmObj*, align 8
%argslist56618$k473931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47394, %struct.ScmObj* %argslist56618$k473930)
store volatile %struct.ScmObj* %argslist56618$k473931, %struct.ScmObj** %stackaddr$prim57993, align 8
%stackaddr$prim57994 = alloca %struct.ScmObj*, align 8
%argslist56618$k473932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50852, %struct.ScmObj* %argslist56618$k473931)
store volatile %struct.ScmObj* %argslist56618$k473932, %struct.ScmObj** %stackaddr$prim57994, align 8
%clofunc57995 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47393)
musttail call tailcc void %clofunc57995(%struct.ScmObj* %k47393, %struct.ScmObj* %argslist56618$k473932)
ret void
}

define tailcc void @proc_clo$ae50825(%struct.ScmObj* %env$ae50825,%struct.ScmObj* %current_45args56620) {
%stackaddr$prim57996 = alloca %struct.ScmObj*, align 8
%k47395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56620)
store volatile %struct.ScmObj* %k47395, %struct.ScmObj** %stackaddr$prim57996, align 8
%stackaddr$prim57997 = alloca %struct.ScmObj*, align 8
%current_45args56621 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56620)
store volatile %struct.ScmObj* %current_45args56621, %struct.ScmObj** %stackaddr$prim57997, align 8
%stackaddr$prim57998 = alloca %struct.ScmObj*, align 8
%x47138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56621)
store volatile %struct.ScmObj* %x47138, %struct.ScmObj** %stackaddr$prim57998, align 8
%stackaddr$prim57999 = alloca %struct.ScmObj*, align 8
%anf_45bind47311 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47138)
store volatile %struct.ScmObj* %anf_45bind47311, %struct.ScmObj** %stackaddr$prim57999, align 8
%stackaddr$prim58000 = alloca %struct.ScmObj*, align 8
%cpsprim47396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47311)
store volatile %struct.ScmObj* %cpsprim47396, %struct.ScmObj** %stackaddr$prim58000, align 8
%ae50829 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56623$k473950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58001 = alloca %struct.ScmObj*, align 8
%argslist56623$k473951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47396, %struct.ScmObj* %argslist56623$k473950)
store volatile %struct.ScmObj* %argslist56623$k473951, %struct.ScmObj** %stackaddr$prim58001, align 8
%stackaddr$prim58002 = alloca %struct.ScmObj*, align 8
%argslist56623$k473952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50829, %struct.ScmObj* %argslist56623$k473951)
store volatile %struct.ScmObj* %argslist56623$k473952, %struct.ScmObj** %stackaddr$prim58002, align 8
%clofunc58003 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47395)
musttail call tailcc void %clofunc58003(%struct.ScmObj* %k47395, %struct.ScmObj* %argslist56623$k473952)
ret void
}

define tailcc void @proc_clo$ae50805(%struct.ScmObj* %env$ae50805,%struct.ScmObj* %current_45args56625) {
%stackaddr$prim58004 = alloca %struct.ScmObj*, align 8
%k47397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56625)
store volatile %struct.ScmObj* %k47397, %struct.ScmObj** %stackaddr$prim58004, align 8
%stackaddr$prim58005 = alloca %struct.ScmObj*, align 8
%current_45args56626 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56625)
store volatile %struct.ScmObj* %current_45args56626, %struct.ScmObj** %stackaddr$prim58005, align 8
%stackaddr$prim58006 = alloca %struct.ScmObj*, align 8
%x47140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56626)
store volatile %struct.ScmObj* %x47140, %struct.ScmObj** %stackaddr$prim58006, align 8
%stackaddr$prim58007 = alloca %struct.ScmObj*, align 8
%cpsprim47398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47140)
store volatile %struct.ScmObj* %cpsprim47398, %struct.ScmObj** %stackaddr$prim58007, align 8
%ae50808 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56628$k473970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58008 = alloca %struct.ScmObj*, align 8
%argslist56628$k473971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47398, %struct.ScmObj* %argslist56628$k473970)
store volatile %struct.ScmObj* %argslist56628$k473971, %struct.ScmObj** %stackaddr$prim58008, align 8
%stackaddr$prim58009 = alloca %struct.ScmObj*, align 8
%argslist56628$k473972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50808, %struct.ScmObj* %argslist56628$k473971)
store volatile %struct.ScmObj* %argslist56628$k473972, %struct.ScmObj** %stackaddr$prim58009, align 8
%clofunc58010 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47397)
musttail call tailcc void %clofunc58010(%struct.ScmObj* %k47397, %struct.ScmObj* %argslist56628$k473972)
ret void
}

define tailcc void @proc_clo$ae50707(%struct.ScmObj* %env$ae50707,%struct.ScmObj* %args4714247399) {
%stackaddr$env-ref58011 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50707, i64 0)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref58011
%stackaddr$prim58012 = alloca %struct.ScmObj*, align 8
%k47400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4714247399)
store volatile %struct.ScmObj* %k47400, %struct.ScmObj** %stackaddr$prim58012, align 8
%stackaddr$prim58013 = alloca %struct.ScmObj*, align 8
%args47142 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4714247399)
store volatile %struct.ScmObj* %args47142, %struct.ScmObj** %stackaddr$prim58013, align 8
%stackaddr$prim58014 = alloca %struct.ScmObj*, align 8
%anf_45bind47305 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args47142)
store volatile %struct.ScmObj* %anf_45bind47305, %struct.ScmObj** %stackaddr$prim58014, align 8
%truthy$cmp58015 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47305)
%cmp$cmp58015 = icmp eq i64 %truthy$cmp58015, 1
br i1 %cmp$cmp58015, label %truebranch$cmp58015, label %falsebranch$cmp58015
truebranch$cmp58015:
%ae50713 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50714 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist56630$k474000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58016 = alloca %struct.ScmObj*, align 8
%argslist56630$k474001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50714, %struct.ScmObj* %argslist56630$k474000)
store volatile %struct.ScmObj* %argslist56630$k474001, %struct.ScmObj** %stackaddr$prim58016, align 8
%stackaddr$prim58017 = alloca %struct.ScmObj*, align 8
%argslist56630$k474002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50713, %struct.ScmObj* %argslist56630$k474001)
store volatile %struct.ScmObj* %argslist56630$k474002, %struct.ScmObj** %stackaddr$prim58017, align 8
%clofunc58018 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47400)
musttail call tailcc void %clofunc58018(%struct.ScmObj* %k47400, %struct.ScmObj* %argslist56630$k474002)
ret void
falsebranch$cmp58015:
%stackaddr$prim58019 = alloca %struct.ScmObj*, align 8
%anf_45bind47306 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47142)
store volatile %struct.ScmObj* %anf_45bind47306, %struct.ScmObj** %stackaddr$prim58019, align 8
%stackaddr$prim58020 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47306)
store volatile %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$prim58020, align 8
%truthy$cmp58021 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47307)
%cmp$cmp58021 = icmp eq i64 %truthy$cmp58021, 1
br i1 %cmp$cmp58021, label %truebranch$cmp58021, label %falsebranch$cmp58021
truebranch$cmp58021:
%stackaddr$prim58022 = alloca %struct.ScmObj*, align 8
%cpsprim47401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47142)
store volatile %struct.ScmObj* %cpsprim47401, %struct.ScmObj** %stackaddr$prim58022, align 8
%ae50726 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56631$k474000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58023 = alloca %struct.ScmObj*, align 8
%argslist56631$k474001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47401, %struct.ScmObj* %argslist56631$k474000)
store volatile %struct.ScmObj* %argslist56631$k474001, %struct.ScmObj** %stackaddr$prim58023, align 8
%stackaddr$prim58024 = alloca %struct.ScmObj*, align 8
%argslist56631$k474002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50726, %struct.ScmObj* %argslist56631$k474001)
store volatile %struct.ScmObj* %argslist56631$k474002, %struct.ScmObj** %stackaddr$prim58024, align 8
%clofunc58025 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47400)
musttail call tailcc void %clofunc58025(%struct.ScmObj* %k47400, %struct.ScmObj* %argslist56631$k474002)
ret void
falsebranch$cmp58021:
%stackaddr$makeclosure58026 = alloca %struct.ScmObj*, align 8
%fptrToInt58027 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50731 to i64
%ae50731 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58027)
store volatile %struct.ScmObj* %ae50731, %struct.ScmObj** %stackaddr$makeclosure58026, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50731, %struct.ScmObj* %args47142, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50731, %struct.ScmObj* %_37foldl147081, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50731, %struct.ScmObj* %k47400, i64 2)
%ae50732 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58028 = alloca %struct.ScmObj*, align 8
%fptrToInt58029 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50733 to i64
%ae50733 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58029)
store volatile %struct.ScmObj* %ae50733, %struct.ScmObj** %stackaddr$makeclosure58028, align 8
%argslist56641$ae507310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58030 = alloca %struct.ScmObj*, align 8
%argslist56641$ae507311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50733, %struct.ScmObj* %argslist56641$ae507310)
store volatile %struct.ScmObj* %argslist56641$ae507311, %struct.ScmObj** %stackaddr$prim58030, align 8
%stackaddr$prim58031 = alloca %struct.ScmObj*, align 8
%argslist56641$ae507312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50732, %struct.ScmObj* %argslist56641$ae507311)
store volatile %struct.ScmObj* %argslist56641$ae507312, %struct.ScmObj** %stackaddr$prim58031, align 8
%clofunc58032 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50731)
musttail call tailcc void %clofunc58032(%struct.ScmObj* %ae50731, %struct.ScmObj* %argslist56641$ae507312)
ret void
}

define tailcc void @proc_clo$ae50731(%struct.ScmObj* %env$ae50731,%struct.ScmObj* %current_45args56632) {
%stackaddr$env-ref58033 = alloca %struct.ScmObj*, align 8
%args47142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50731, i64 0)
store %struct.ScmObj* %args47142, %struct.ScmObj** %stackaddr$env-ref58033
%stackaddr$env-ref58034 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50731, i64 1)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref58034
%stackaddr$env-ref58035 = alloca %struct.ScmObj*, align 8
%k47400 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50731, i64 2)
store %struct.ScmObj* %k47400, %struct.ScmObj** %stackaddr$env-ref58035
%stackaddr$prim58036 = alloca %struct.ScmObj*, align 8
%_95k47402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56632)
store volatile %struct.ScmObj* %_95k47402, %struct.ScmObj** %stackaddr$prim58036, align 8
%stackaddr$prim58037 = alloca %struct.ScmObj*, align 8
%current_45args56633 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56632)
store volatile %struct.ScmObj* %current_45args56633, %struct.ScmObj** %stackaddr$prim58037, align 8
%stackaddr$prim58038 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56633)
store volatile %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$prim58038, align 8
%stackaddr$prim58039 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47142)
store volatile %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$prim58039, align 8
%stackaddr$prim58040 = alloca %struct.ScmObj*, align 8
%anf_45bind47310 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47142)
store volatile %struct.ScmObj* %anf_45bind47310, %struct.ScmObj** %stackaddr$prim58040, align 8
%argslist56635$_37foldl1470810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58041 = alloca %struct.ScmObj*, align 8
%argslist56635$_37foldl1470811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47310, %struct.ScmObj* %argslist56635$_37foldl1470810)
store volatile %struct.ScmObj* %argslist56635$_37foldl1470811, %struct.ScmObj** %stackaddr$prim58041, align 8
%stackaddr$prim58042 = alloca %struct.ScmObj*, align 8
%argslist56635$_37foldl1470812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47309, %struct.ScmObj* %argslist56635$_37foldl1470811)
store volatile %struct.ScmObj* %argslist56635$_37foldl1470812, %struct.ScmObj** %stackaddr$prim58042, align 8
%stackaddr$prim58043 = alloca %struct.ScmObj*, align 8
%argslist56635$_37foldl1470813 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47308, %struct.ScmObj* %argslist56635$_37foldl1470812)
store volatile %struct.ScmObj* %argslist56635$_37foldl1470813, %struct.ScmObj** %stackaddr$prim58043, align 8
%stackaddr$prim58044 = alloca %struct.ScmObj*, align 8
%argslist56635$_37foldl1470814 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47400, %struct.ScmObj* %argslist56635$_37foldl1470813)
store volatile %struct.ScmObj* %argslist56635$_37foldl1470814, %struct.ScmObj** %stackaddr$prim58044, align 8
%clofunc58045 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147081)
musttail call tailcc void %clofunc58045(%struct.ScmObj* %_37foldl147081, %struct.ScmObj* %argslist56635$_37foldl1470814)
ret void
}

define tailcc void @proc_clo$ae50733(%struct.ScmObj* %env$ae50733,%struct.ScmObj* %current_45args56636) {
%stackaddr$prim58046 = alloca %struct.ScmObj*, align 8
%k47403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56636)
store volatile %struct.ScmObj* %k47403, %struct.ScmObj** %stackaddr$prim58046, align 8
%stackaddr$prim58047 = alloca %struct.ScmObj*, align 8
%current_45args56637 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56636)
store volatile %struct.ScmObj* %current_45args56637, %struct.ScmObj** %stackaddr$prim58047, align 8
%stackaddr$prim58048 = alloca %struct.ScmObj*, align 8
%n47144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56637)
store volatile %struct.ScmObj* %n47144, %struct.ScmObj** %stackaddr$prim58048, align 8
%stackaddr$prim58049 = alloca %struct.ScmObj*, align 8
%current_45args56638 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56637)
store volatile %struct.ScmObj* %current_45args56638, %struct.ScmObj** %stackaddr$prim58049, align 8
%stackaddr$prim58050 = alloca %struct.ScmObj*, align 8
%v47143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56638)
store volatile %struct.ScmObj* %v47143, %struct.ScmObj** %stackaddr$prim58050, align 8
%stackaddr$prim58051 = alloca %struct.ScmObj*, align 8
%cpsprim47404 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v47143, %struct.ScmObj* %n47144)
store volatile %struct.ScmObj* %cpsprim47404, %struct.ScmObj** %stackaddr$prim58051, align 8
%ae50737 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56640$k474030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58052 = alloca %struct.ScmObj*, align 8
%argslist56640$k474031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47404, %struct.ScmObj* %argslist56640$k474030)
store volatile %struct.ScmObj* %argslist56640$k474031, %struct.ScmObj** %stackaddr$prim58052, align 8
%stackaddr$prim58053 = alloca %struct.ScmObj*, align 8
%argslist56640$k474032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50737, %struct.ScmObj* %argslist56640$k474031)
store volatile %struct.ScmObj* %argslist56640$k474032, %struct.ScmObj** %stackaddr$prim58053, align 8
%clofunc58054 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47403)
musttail call tailcc void %clofunc58054(%struct.ScmObj* %k47403, %struct.ScmObj* %argslist56640$k474032)
ret void
}

define tailcc void @proc_clo$ae50303(%struct.ScmObj* %env$ae50303,%struct.ScmObj* %current_45args56643) {
%stackaddr$prim58055 = alloca %struct.ScmObj*, align 8
%k47405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56643)
store volatile %struct.ScmObj* %k47405, %struct.ScmObj** %stackaddr$prim58055, align 8
%stackaddr$prim58056 = alloca %struct.ScmObj*, align 8
%current_45args56644 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56643)
store volatile %struct.ScmObj* %current_45args56644, %struct.ScmObj** %stackaddr$prim58056, align 8
%stackaddr$prim58057 = alloca %struct.ScmObj*, align 8
%v47147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56644)
store volatile %struct.ScmObj* %v47147, %struct.ScmObj** %stackaddr$prim58057, align 8
%stackaddr$prim58058 = alloca %struct.ScmObj*, align 8
%current_45args56645 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56644)
store volatile %struct.ScmObj* %current_45args56645, %struct.ScmObj** %stackaddr$prim58058, align 8
%stackaddr$prim58059 = alloca %struct.ScmObj*, align 8
%lst47146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56645)
store volatile %struct.ScmObj* %lst47146, %struct.ScmObj** %stackaddr$prim58059, align 8
%ae50304 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58060 = alloca %struct.ScmObj*, align 8
%lst47148 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50304, %struct.ScmObj* %lst47146)
store volatile %struct.ScmObj* %lst47148, %struct.ScmObj** %stackaddr$prim58060, align 8
%stackaddr$makeclosure58061 = alloca %struct.ScmObj*, align 8
%fptrToInt58062 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50306 to i64
%ae50306 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58062)
store volatile %struct.ScmObj* %ae50306, %struct.ScmObj** %stackaddr$makeclosure58061, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50306, %struct.ScmObj* %k47405, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50306, %struct.ScmObj* %lst47148, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50306, %struct.ScmObj* %v47147, i64 2)
%ae50307 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58063 = alloca %struct.ScmObj*, align 8
%fptrToInt58064 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50308 to i64
%ae50308 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58064)
store volatile %struct.ScmObj* %ae50308, %struct.ScmObj** %stackaddr$makeclosure58063, align 8
%argslist56667$ae503060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58065 = alloca %struct.ScmObj*, align 8
%argslist56667$ae503061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50308, %struct.ScmObj* %argslist56667$ae503060)
store volatile %struct.ScmObj* %argslist56667$ae503061, %struct.ScmObj** %stackaddr$prim58065, align 8
%stackaddr$prim58066 = alloca %struct.ScmObj*, align 8
%argslist56667$ae503062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50307, %struct.ScmObj* %argslist56667$ae503061)
store volatile %struct.ScmObj* %argslist56667$ae503062, %struct.ScmObj** %stackaddr$prim58066, align 8
%clofunc58067 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50306)
musttail call tailcc void %clofunc58067(%struct.ScmObj* %ae50306, %struct.ScmObj* %argslist56667$ae503062)
ret void
}

define tailcc void @proc_clo$ae50306(%struct.ScmObj* %env$ae50306,%struct.ScmObj* %current_45args56647) {
%stackaddr$env-ref58068 = alloca %struct.ScmObj*, align 8
%k47405 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50306, i64 0)
store %struct.ScmObj* %k47405, %struct.ScmObj** %stackaddr$env-ref58068
%stackaddr$env-ref58069 = alloca %struct.ScmObj*, align 8
%lst47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50306, i64 1)
store %struct.ScmObj* %lst47148, %struct.ScmObj** %stackaddr$env-ref58069
%stackaddr$env-ref58070 = alloca %struct.ScmObj*, align 8
%v47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50306, i64 2)
store %struct.ScmObj* %v47147, %struct.ScmObj** %stackaddr$env-ref58070
%stackaddr$prim58071 = alloca %struct.ScmObj*, align 8
%_95k47406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56647)
store volatile %struct.ScmObj* %_95k47406, %struct.ScmObj** %stackaddr$prim58071, align 8
%stackaddr$prim58072 = alloca %struct.ScmObj*, align 8
%current_45args56648 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56647)
store volatile %struct.ScmObj* %current_45args56648, %struct.ScmObj** %stackaddr$prim58072, align 8
%stackaddr$prim58073 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56648)
store volatile %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$prim58073, align 8
%stackaddr$makeclosure58074 = alloca %struct.ScmObj*, align 8
%fptrToInt58075 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50322 to i64
%ae50322 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58075)
store volatile %struct.ScmObj* %ae50322, %struct.ScmObj** %stackaddr$makeclosure58074, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50322, %struct.ScmObj* %k47405, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50322, %struct.ScmObj* %lst47148, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50322, %struct.ScmObj* %v47147, i64 2)
%stackaddr$makeclosure58076 = alloca %struct.ScmObj*, align 8
%fptrToInt58077 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50323 to i64
%ae50323 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58077)
store volatile %struct.ScmObj* %ae50323, %struct.ScmObj** %stackaddr$makeclosure58076, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50323, %struct.ScmObj* %k47405, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50323, %struct.ScmObj* %lst47148, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50323, %struct.ScmObj* %v47147, i64 2)
%argslist56662$anf_45bind472970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58078 = alloca %struct.ScmObj*, align 8
%argslist56662$anf_45bind472971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50323, %struct.ScmObj* %argslist56662$anf_45bind472970)
store volatile %struct.ScmObj* %argslist56662$anf_45bind472971, %struct.ScmObj** %stackaddr$prim58078, align 8
%stackaddr$prim58079 = alloca %struct.ScmObj*, align 8
%argslist56662$anf_45bind472972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50322, %struct.ScmObj* %argslist56662$anf_45bind472971)
store volatile %struct.ScmObj* %argslist56662$anf_45bind472972, %struct.ScmObj** %stackaddr$prim58079, align 8
%clofunc58080 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47297)
musttail call tailcc void %clofunc58080(%struct.ScmObj* %anf_45bind47297, %struct.ScmObj* %argslist56662$anf_45bind472972)
ret void
}

define tailcc void @proc_clo$ae50322(%struct.ScmObj* %env$ae50322,%struct.ScmObj* %current_45args56650) {
%stackaddr$env-ref58081 = alloca %struct.ScmObj*, align 8
%k47405 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50322, i64 0)
store %struct.ScmObj* %k47405, %struct.ScmObj** %stackaddr$env-ref58081
%stackaddr$env-ref58082 = alloca %struct.ScmObj*, align 8
%lst47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50322, i64 1)
store %struct.ScmObj* %lst47148, %struct.ScmObj** %stackaddr$env-ref58082
%stackaddr$env-ref58083 = alloca %struct.ScmObj*, align 8
%v47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50322, i64 2)
store %struct.ScmObj* %v47147, %struct.ScmObj** %stackaddr$env-ref58083
%stackaddr$prim58084 = alloca %struct.ScmObj*, align 8
%_95k47407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56650)
store volatile %struct.ScmObj* %_95k47407, %struct.ScmObj** %stackaddr$prim58084, align 8
%stackaddr$prim58085 = alloca %struct.ScmObj*, align 8
%current_45args56651 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56650)
store volatile %struct.ScmObj* %current_45args56651, %struct.ScmObj** %stackaddr$prim58085, align 8
%stackaddr$prim58086 = alloca %struct.ScmObj*, align 8
%cc47149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56651)
store volatile %struct.ScmObj* %cc47149, %struct.ScmObj** %stackaddr$prim58086, align 8
%ae50431 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58087 = alloca %struct.ScmObj*, align 8
%anf_45bind47298 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae50431)
store volatile %struct.ScmObj* %anf_45bind47298, %struct.ScmObj** %stackaddr$prim58087, align 8
%stackaddr$prim58088 = alloca %struct.ScmObj*, align 8
%anf_45bind47299 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47298)
store volatile %struct.ScmObj* %anf_45bind47299, %struct.ScmObj** %stackaddr$prim58088, align 8
%truthy$cmp58089 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47299)
%cmp$cmp58089 = icmp eq i64 %truthy$cmp58089, 1
br i1 %cmp$cmp58089, label %truebranch$cmp58089, label %falsebranch$cmp58089
truebranch$cmp58089:
%ae50435 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50436 = call %struct.ScmObj* @const_init_false()
%argslist56653$k474050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58090 = alloca %struct.ScmObj*, align 8
%argslist56653$k474051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50436, %struct.ScmObj* %argslist56653$k474050)
store volatile %struct.ScmObj* %argslist56653$k474051, %struct.ScmObj** %stackaddr$prim58090, align 8
%stackaddr$prim58091 = alloca %struct.ScmObj*, align 8
%argslist56653$k474052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50435, %struct.ScmObj* %argslist56653$k474051)
store volatile %struct.ScmObj* %argslist56653$k474052, %struct.ScmObj** %stackaddr$prim58091, align 8
%clofunc58092 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47405)
musttail call tailcc void %clofunc58092(%struct.ScmObj* %k47405, %struct.ScmObj* %argslist56653$k474052)
ret void
falsebranch$cmp58089:
%ae50444 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58093 = alloca %struct.ScmObj*, align 8
%anf_45bind47300 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae50444)
store volatile %struct.ScmObj* %anf_45bind47300, %struct.ScmObj** %stackaddr$prim58093, align 8
%stackaddr$prim58094 = alloca %struct.ScmObj*, align 8
%anf_45bind47301 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47300)
store volatile %struct.ScmObj* %anf_45bind47301, %struct.ScmObj** %stackaddr$prim58094, align 8
%stackaddr$prim58095 = alloca %struct.ScmObj*, align 8
%anf_45bind47302 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47301, %struct.ScmObj* %v47147)
store volatile %struct.ScmObj* %anf_45bind47302, %struct.ScmObj** %stackaddr$prim58095, align 8
%truthy$cmp58096 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47302)
%cmp$cmp58096 = icmp eq i64 %truthy$cmp58096, 1
br i1 %cmp$cmp58096, label %truebranch$cmp58096, label %falsebranch$cmp58096
truebranch$cmp58096:
%ae50450 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58097 = alloca %struct.ScmObj*, align 8
%cpsprim47408 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae50450)
store volatile %struct.ScmObj* %cpsprim47408, %struct.ScmObj** %stackaddr$prim58097, align 8
%ae50452 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56654$k474050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58098 = alloca %struct.ScmObj*, align 8
%argslist56654$k474051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47408, %struct.ScmObj* %argslist56654$k474050)
store volatile %struct.ScmObj* %argslist56654$k474051, %struct.ScmObj** %stackaddr$prim58098, align 8
%stackaddr$prim58099 = alloca %struct.ScmObj*, align 8
%argslist56654$k474052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50452, %struct.ScmObj* %argslist56654$k474051)
store volatile %struct.ScmObj* %argslist56654$k474052, %struct.ScmObj** %stackaddr$prim58099, align 8
%clofunc58100 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47405)
musttail call tailcc void %clofunc58100(%struct.ScmObj* %k47405, %struct.ScmObj* %argslist56654$k474052)
ret void
falsebranch$cmp58096:
%ae50463 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58101 = alloca %struct.ScmObj*, align 8
%anf_45bind47303 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae50463)
store volatile %struct.ScmObj* %anf_45bind47303, %struct.ScmObj** %stackaddr$prim58101, align 8
%stackaddr$prim58102 = alloca %struct.ScmObj*, align 8
%anf_45bind47304 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47303)
store volatile %struct.ScmObj* %anf_45bind47304, %struct.ScmObj** %stackaddr$prim58102, align 8
%ae50466 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58103 = alloca %struct.ScmObj*, align 8
%_95047151 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae50466, %struct.ScmObj* %anf_45bind47304)
store volatile %struct.ScmObj* %_95047151, %struct.ScmObj** %stackaddr$prim58103, align 8
%argslist56655$cc471490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58104 = alloca %struct.ScmObj*, align 8
%argslist56655$cc471491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist56655$cc471490)
store volatile %struct.ScmObj* %argslist56655$cc471491, %struct.ScmObj** %stackaddr$prim58104, align 8
%stackaddr$prim58105 = alloca %struct.ScmObj*, align 8
%argslist56655$cc471492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47405, %struct.ScmObj* %argslist56655$cc471491)
store volatile %struct.ScmObj* %argslist56655$cc471492, %struct.ScmObj** %stackaddr$prim58105, align 8
%clofunc58106 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47149)
musttail call tailcc void %clofunc58106(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist56655$cc471492)
ret void
}

define tailcc void @proc_clo$ae50323(%struct.ScmObj* %env$ae50323,%struct.ScmObj* %current_45args56656) {
%stackaddr$env-ref58107 = alloca %struct.ScmObj*, align 8
%k47405 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50323, i64 0)
store %struct.ScmObj* %k47405, %struct.ScmObj** %stackaddr$env-ref58107
%stackaddr$env-ref58108 = alloca %struct.ScmObj*, align 8
%lst47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50323, i64 1)
store %struct.ScmObj* %lst47148, %struct.ScmObj** %stackaddr$env-ref58108
%stackaddr$env-ref58109 = alloca %struct.ScmObj*, align 8
%v47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50323, i64 2)
store %struct.ScmObj* %v47147, %struct.ScmObj** %stackaddr$env-ref58109
%stackaddr$prim58110 = alloca %struct.ScmObj*, align 8
%_95k47407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56656)
store volatile %struct.ScmObj* %_95k47407, %struct.ScmObj** %stackaddr$prim58110, align 8
%stackaddr$prim58111 = alloca %struct.ScmObj*, align 8
%current_45args56657 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56656)
store volatile %struct.ScmObj* %current_45args56657, %struct.ScmObj** %stackaddr$prim58111, align 8
%stackaddr$prim58112 = alloca %struct.ScmObj*, align 8
%cc47149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56657)
store volatile %struct.ScmObj* %cc47149, %struct.ScmObj** %stackaddr$prim58112, align 8
%ae50325 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58113 = alloca %struct.ScmObj*, align 8
%anf_45bind47298 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae50325)
store volatile %struct.ScmObj* %anf_45bind47298, %struct.ScmObj** %stackaddr$prim58113, align 8
%stackaddr$prim58114 = alloca %struct.ScmObj*, align 8
%anf_45bind47299 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47298)
store volatile %struct.ScmObj* %anf_45bind47299, %struct.ScmObj** %stackaddr$prim58114, align 8
%truthy$cmp58115 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47299)
%cmp$cmp58115 = icmp eq i64 %truthy$cmp58115, 1
br i1 %cmp$cmp58115, label %truebranch$cmp58115, label %falsebranch$cmp58115
truebranch$cmp58115:
%ae50329 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50330 = call %struct.ScmObj* @const_init_false()
%argslist56659$k474050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58116 = alloca %struct.ScmObj*, align 8
%argslist56659$k474051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50330, %struct.ScmObj* %argslist56659$k474050)
store volatile %struct.ScmObj* %argslist56659$k474051, %struct.ScmObj** %stackaddr$prim58116, align 8
%stackaddr$prim58117 = alloca %struct.ScmObj*, align 8
%argslist56659$k474052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50329, %struct.ScmObj* %argslist56659$k474051)
store volatile %struct.ScmObj* %argslist56659$k474052, %struct.ScmObj** %stackaddr$prim58117, align 8
%clofunc58118 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47405)
musttail call tailcc void %clofunc58118(%struct.ScmObj* %k47405, %struct.ScmObj* %argslist56659$k474052)
ret void
falsebranch$cmp58115:
%ae50338 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58119 = alloca %struct.ScmObj*, align 8
%anf_45bind47300 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae50338)
store volatile %struct.ScmObj* %anf_45bind47300, %struct.ScmObj** %stackaddr$prim58119, align 8
%stackaddr$prim58120 = alloca %struct.ScmObj*, align 8
%anf_45bind47301 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47300)
store volatile %struct.ScmObj* %anf_45bind47301, %struct.ScmObj** %stackaddr$prim58120, align 8
%stackaddr$prim58121 = alloca %struct.ScmObj*, align 8
%anf_45bind47302 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47301, %struct.ScmObj* %v47147)
store volatile %struct.ScmObj* %anf_45bind47302, %struct.ScmObj** %stackaddr$prim58121, align 8
%truthy$cmp58122 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47302)
%cmp$cmp58122 = icmp eq i64 %truthy$cmp58122, 1
br i1 %cmp$cmp58122, label %truebranch$cmp58122, label %falsebranch$cmp58122
truebranch$cmp58122:
%ae50344 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58123 = alloca %struct.ScmObj*, align 8
%cpsprim47408 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae50344)
store volatile %struct.ScmObj* %cpsprim47408, %struct.ScmObj** %stackaddr$prim58123, align 8
%ae50346 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56660$k474050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58124 = alloca %struct.ScmObj*, align 8
%argslist56660$k474051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47408, %struct.ScmObj* %argslist56660$k474050)
store volatile %struct.ScmObj* %argslist56660$k474051, %struct.ScmObj** %stackaddr$prim58124, align 8
%stackaddr$prim58125 = alloca %struct.ScmObj*, align 8
%argslist56660$k474052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50346, %struct.ScmObj* %argslist56660$k474051)
store volatile %struct.ScmObj* %argslist56660$k474052, %struct.ScmObj** %stackaddr$prim58125, align 8
%clofunc58126 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47405)
musttail call tailcc void %clofunc58126(%struct.ScmObj* %k47405, %struct.ScmObj* %argslist56660$k474052)
ret void
falsebranch$cmp58122:
%ae50357 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58127 = alloca %struct.ScmObj*, align 8
%anf_45bind47303 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae50357)
store volatile %struct.ScmObj* %anf_45bind47303, %struct.ScmObj** %stackaddr$prim58127, align 8
%stackaddr$prim58128 = alloca %struct.ScmObj*, align 8
%anf_45bind47304 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47303)
store volatile %struct.ScmObj* %anf_45bind47304, %struct.ScmObj** %stackaddr$prim58128, align 8
%ae50360 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58129 = alloca %struct.ScmObj*, align 8
%_95047151 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47148, %struct.ScmObj* %ae50360, %struct.ScmObj* %anf_45bind47304)
store volatile %struct.ScmObj* %_95047151, %struct.ScmObj** %stackaddr$prim58129, align 8
%argslist56661$cc471490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58130 = alloca %struct.ScmObj*, align 8
%argslist56661$cc471491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist56661$cc471490)
store volatile %struct.ScmObj* %argslist56661$cc471491, %struct.ScmObj** %stackaddr$prim58130, align 8
%stackaddr$prim58131 = alloca %struct.ScmObj*, align 8
%argslist56661$cc471492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47405, %struct.ScmObj* %argslist56661$cc471491)
store volatile %struct.ScmObj* %argslist56661$cc471492, %struct.ScmObj** %stackaddr$prim58131, align 8
%clofunc58132 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47149)
musttail call tailcc void %clofunc58132(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist56661$cc471492)
ret void
}

define tailcc void @proc_clo$ae50308(%struct.ScmObj* %env$ae50308,%struct.ScmObj* %current_45args56663) {
%stackaddr$prim58133 = alloca %struct.ScmObj*, align 8
%k47409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56663)
store volatile %struct.ScmObj* %k47409, %struct.ScmObj** %stackaddr$prim58133, align 8
%stackaddr$prim58134 = alloca %struct.ScmObj*, align 8
%current_45args56664 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56663)
store volatile %struct.ScmObj* %current_45args56664, %struct.ScmObj** %stackaddr$prim58134, align 8
%stackaddr$prim58135 = alloca %struct.ScmObj*, align 8
%u47150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56664)
store volatile %struct.ScmObj* %u47150, %struct.ScmObj** %stackaddr$prim58135, align 8
%argslist56666$u471500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58136 = alloca %struct.ScmObj*, align 8
%argslist56666$u471501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47150, %struct.ScmObj* %argslist56666$u471500)
store volatile %struct.ScmObj* %argslist56666$u471501, %struct.ScmObj** %stackaddr$prim58136, align 8
%stackaddr$prim58137 = alloca %struct.ScmObj*, align 8
%argslist56666$u471502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47409, %struct.ScmObj* %argslist56666$u471501)
store volatile %struct.ScmObj* %argslist56666$u471502, %struct.ScmObj** %stackaddr$prim58137, align 8
%clofunc58138 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47150)
musttail call tailcc void %clofunc58138(%struct.ScmObj* %u47150, %struct.ScmObj* %argslist56666$u471502)
ret void
}

define tailcc void @proc_clo$ae49767(%struct.ScmObj* %env$ae49767,%struct.ScmObj* %current_45args56669) {
%stackaddr$prim58139 = alloca %struct.ScmObj*, align 8
%k47410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56669)
store volatile %struct.ScmObj* %k47410, %struct.ScmObj** %stackaddr$prim58139, align 8
%stackaddr$prim58140 = alloca %struct.ScmObj*, align 8
%current_45args56670 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56669)
store volatile %struct.ScmObj* %current_45args56670, %struct.ScmObj** %stackaddr$prim58140, align 8
%stackaddr$prim58141 = alloca %struct.ScmObj*, align 8
%lst47154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56670)
store volatile %struct.ScmObj* %lst47154, %struct.ScmObj** %stackaddr$prim58141, align 8
%stackaddr$prim58142 = alloca %struct.ScmObj*, align 8
%current_45args56671 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56670)
store volatile %struct.ScmObj* %current_45args56671, %struct.ScmObj** %stackaddr$prim58142, align 8
%stackaddr$prim58143 = alloca %struct.ScmObj*, align 8
%n47153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56671)
store volatile %struct.ScmObj* %n47153, %struct.ScmObj** %stackaddr$prim58143, align 8
%ae49768 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58144 = alloca %struct.ScmObj*, align 8
%n47156 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49768, %struct.ScmObj* %n47153)
store volatile %struct.ScmObj* %n47156, %struct.ScmObj** %stackaddr$prim58144, align 8
%ae49770 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58145 = alloca %struct.ScmObj*, align 8
%lst47155 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49770, %struct.ScmObj* %lst47154)
store volatile %struct.ScmObj* %lst47155, %struct.ScmObj** %stackaddr$prim58145, align 8
%stackaddr$makeclosure58146 = alloca %struct.ScmObj*, align 8
%fptrToInt58147 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49772 to i64
%ae49772 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58147)
store volatile %struct.ScmObj* %ae49772, %struct.ScmObj** %stackaddr$makeclosure58146, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49772, %struct.ScmObj* %n47156, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49772, %struct.ScmObj* %lst47155, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49772, %struct.ScmObj* %k47410, i64 2)
%ae49773 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58148 = alloca %struct.ScmObj*, align 8
%fptrToInt58149 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49774 to i64
%ae49774 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58149)
store volatile %struct.ScmObj* %ae49774, %struct.ScmObj** %stackaddr$makeclosure58148, align 8
%argslist56691$ae497720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58150 = alloca %struct.ScmObj*, align 8
%argslist56691$ae497721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49774, %struct.ScmObj* %argslist56691$ae497720)
store volatile %struct.ScmObj* %argslist56691$ae497721, %struct.ScmObj** %stackaddr$prim58150, align 8
%stackaddr$prim58151 = alloca %struct.ScmObj*, align 8
%argslist56691$ae497722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49773, %struct.ScmObj* %argslist56691$ae497721)
store volatile %struct.ScmObj* %argslist56691$ae497722, %struct.ScmObj** %stackaddr$prim58151, align 8
%clofunc58152 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49772)
musttail call tailcc void %clofunc58152(%struct.ScmObj* %ae49772, %struct.ScmObj* %argslist56691$ae497722)
ret void
}

define tailcc void @proc_clo$ae49772(%struct.ScmObj* %env$ae49772,%struct.ScmObj* %current_45args56673) {
%stackaddr$env-ref58153 = alloca %struct.ScmObj*, align 8
%n47156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49772, i64 0)
store %struct.ScmObj* %n47156, %struct.ScmObj** %stackaddr$env-ref58153
%stackaddr$env-ref58154 = alloca %struct.ScmObj*, align 8
%lst47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49772, i64 1)
store %struct.ScmObj* %lst47155, %struct.ScmObj** %stackaddr$env-ref58154
%stackaddr$env-ref58155 = alloca %struct.ScmObj*, align 8
%k47410 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49772, i64 2)
store %struct.ScmObj* %k47410, %struct.ScmObj** %stackaddr$env-ref58155
%stackaddr$prim58156 = alloca %struct.ScmObj*, align 8
%_95k47411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56673)
store volatile %struct.ScmObj* %_95k47411, %struct.ScmObj** %stackaddr$prim58156, align 8
%stackaddr$prim58157 = alloca %struct.ScmObj*, align 8
%current_45args56674 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56673)
store volatile %struct.ScmObj* %current_45args56674, %struct.ScmObj** %stackaddr$prim58157, align 8
%stackaddr$prim58158 = alloca %struct.ScmObj*, align 8
%anf_45bind47290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56674)
store volatile %struct.ScmObj* %anf_45bind47290, %struct.ScmObj** %stackaddr$prim58158, align 8
%stackaddr$makeclosure58159 = alloca %struct.ScmObj*, align 8
%fptrToInt58160 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49788 to i64
%ae49788 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58160)
store volatile %struct.ScmObj* %ae49788, %struct.ScmObj** %stackaddr$makeclosure58159, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49788, %struct.ScmObj* %n47156, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49788, %struct.ScmObj* %lst47155, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49788, %struct.ScmObj* %k47410, i64 2)
%stackaddr$makeclosure58161 = alloca %struct.ScmObj*, align 8
%fptrToInt58162 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49789 to i64
%ae49789 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58162)
store volatile %struct.ScmObj* %ae49789, %struct.ScmObj** %stackaddr$makeclosure58161, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49789, %struct.ScmObj* %n47156, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49789, %struct.ScmObj* %lst47155, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49789, %struct.ScmObj* %k47410, i64 2)
%argslist56686$anf_45bind472900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58163 = alloca %struct.ScmObj*, align 8
%argslist56686$anf_45bind472901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49789, %struct.ScmObj* %argslist56686$anf_45bind472900)
store volatile %struct.ScmObj* %argslist56686$anf_45bind472901, %struct.ScmObj** %stackaddr$prim58163, align 8
%stackaddr$prim58164 = alloca %struct.ScmObj*, align 8
%argslist56686$anf_45bind472902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49788, %struct.ScmObj* %argslist56686$anf_45bind472901)
store volatile %struct.ScmObj* %argslist56686$anf_45bind472902, %struct.ScmObj** %stackaddr$prim58164, align 8
%clofunc58165 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47290)
musttail call tailcc void %clofunc58165(%struct.ScmObj* %anf_45bind47290, %struct.ScmObj* %argslist56686$anf_45bind472902)
ret void
}

define tailcc void @proc_clo$ae49788(%struct.ScmObj* %env$ae49788,%struct.ScmObj* %current_45args56676) {
%stackaddr$env-ref58166 = alloca %struct.ScmObj*, align 8
%n47156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49788, i64 0)
store %struct.ScmObj* %n47156, %struct.ScmObj** %stackaddr$env-ref58166
%stackaddr$env-ref58167 = alloca %struct.ScmObj*, align 8
%lst47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49788, i64 1)
store %struct.ScmObj* %lst47155, %struct.ScmObj** %stackaddr$env-ref58167
%stackaddr$env-ref58168 = alloca %struct.ScmObj*, align 8
%k47410 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49788, i64 2)
store %struct.ScmObj* %k47410, %struct.ScmObj** %stackaddr$env-ref58168
%stackaddr$prim58169 = alloca %struct.ScmObj*, align 8
%_95k47412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56676)
store volatile %struct.ScmObj* %_95k47412, %struct.ScmObj** %stackaddr$prim58169, align 8
%stackaddr$prim58170 = alloca %struct.ScmObj*, align 8
%current_45args56677 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56676)
store volatile %struct.ScmObj* %current_45args56677, %struct.ScmObj** %stackaddr$prim58170, align 8
%stackaddr$prim58171 = alloca %struct.ScmObj*, align 8
%cc47157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56677)
store volatile %struct.ScmObj* %cc47157, %struct.ScmObj** %stackaddr$prim58171, align 8
%ae49931 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58172 = alloca %struct.ScmObj*, align 8
%anf_45bind47291 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47156, %struct.ScmObj* %ae49931)
store volatile %struct.ScmObj* %anf_45bind47291, %struct.ScmObj** %stackaddr$prim58172, align 8
%ae49932 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58173 = alloca %struct.ScmObj*, align 8
%anf_45bind47292 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49932, %struct.ScmObj* %anf_45bind47291)
store volatile %struct.ScmObj* %anf_45bind47292, %struct.ScmObj** %stackaddr$prim58173, align 8
%truthy$cmp58174 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47292)
%cmp$cmp58174 = icmp eq i64 %truthy$cmp58174, 1
br i1 %cmp$cmp58174, label %truebranch$cmp58174, label %falsebranch$cmp58174
truebranch$cmp58174:
%ae49936 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58175 = alloca %struct.ScmObj*, align 8
%cpsprim47413 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47155, %struct.ScmObj* %ae49936)
store volatile %struct.ScmObj* %cpsprim47413, %struct.ScmObj** %stackaddr$prim58175, align 8
%ae49938 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56679$k474100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58176 = alloca %struct.ScmObj*, align 8
%argslist56679$k474101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47413, %struct.ScmObj* %argslist56679$k474100)
store volatile %struct.ScmObj* %argslist56679$k474101, %struct.ScmObj** %stackaddr$prim58176, align 8
%stackaddr$prim58177 = alloca %struct.ScmObj*, align 8
%argslist56679$k474102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49938, %struct.ScmObj* %argslist56679$k474101)
store volatile %struct.ScmObj* %argslist56679$k474102, %struct.ScmObj** %stackaddr$prim58177, align 8
%clofunc58178 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47410)
musttail call tailcc void %clofunc58178(%struct.ScmObj* %k47410, %struct.ScmObj* %argslist56679$k474102)
ret void
falsebranch$cmp58174:
%ae49949 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58179 = alloca %struct.ScmObj*, align 8
%anf_45bind47293 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47155, %struct.ScmObj* %ae49949)
store volatile %struct.ScmObj* %anf_45bind47293, %struct.ScmObj** %stackaddr$prim58179, align 8
%stackaddr$prim58180 = alloca %struct.ScmObj*, align 8
%anf_45bind47294 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47293)
store volatile %struct.ScmObj* %anf_45bind47294, %struct.ScmObj** %stackaddr$prim58180, align 8
%ae49952 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58181 = alloca %struct.ScmObj*, align 8
%_95047160 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47155, %struct.ScmObj* %ae49952, %struct.ScmObj* %anf_45bind47294)
store volatile %struct.ScmObj* %_95047160, %struct.ScmObj** %stackaddr$prim58181, align 8
%ae49955 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58182 = alloca %struct.ScmObj*, align 8
%anf_45bind47295 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47156, %struct.ScmObj* %ae49955)
store volatile %struct.ScmObj* %anf_45bind47295, %struct.ScmObj** %stackaddr$prim58182, align 8
%ae49957 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58183 = alloca %struct.ScmObj*, align 8
%anf_45bind47296 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47295, %struct.ScmObj* %ae49957)
store volatile %struct.ScmObj* %anf_45bind47296, %struct.ScmObj** %stackaddr$prim58183, align 8
%ae49959 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58184 = alloca %struct.ScmObj*, align 8
%_95147159 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47156, %struct.ScmObj* %ae49959, %struct.ScmObj* %anf_45bind47296)
store volatile %struct.ScmObj* %_95147159, %struct.ScmObj** %stackaddr$prim58184, align 8
%argslist56680$cc471570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58185 = alloca %struct.ScmObj*, align 8
%argslist56680$cc471571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47157, %struct.ScmObj* %argslist56680$cc471570)
store volatile %struct.ScmObj* %argslist56680$cc471571, %struct.ScmObj** %stackaddr$prim58185, align 8
%stackaddr$prim58186 = alloca %struct.ScmObj*, align 8
%argslist56680$cc471572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47410, %struct.ScmObj* %argslist56680$cc471571)
store volatile %struct.ScmObj* %argslist56680$cc471572, %struct.ScmObj** %stackaddr$prim58186, align 8
%clofunc58187 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47157)
musttail call tailcc void %clofunc58187(%struct.ScmObj* %cc47157, %struct.ScmObj* %argslist56680$cc471572)
ret void
}

define tailcc void @proc_clo$ae49789(%struct.ScmObj* %env$ae49789,%struct.ScmObj* %current_45args56681) {
%stackaddr$env-ref58188 = alloca %struct.ScmObj*, align 8
%n47156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49789, i64 0)
store %struct.ScmObj* %n47156, %struct.ScmObj** %stackaddr$env-ref58188
%stackaddr$env-ref58189 = alloca %struct.ScmObj*, align 8
%lst47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49789, i64 1)
store %struct.ScmObj* %lst47155, %struct.ScmObj** %stackaddr$env-ref58189
%stackaddr$env-ref58190 = alloca %struct.ScmObj*, align 8
%k47410 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49789, i64 2)
store %struct.ScmObj* %k47410, %struct.ScmObj** %stackaddr$env-ref58190
%stackaddr$prim58191 = alloca %struct.ScmObj*, align 8
%_95k47412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56681)
store volatile %struct.ScmObj* %_95k47412, %struct.ScmObj** %stackaddr$prim58191, align 8
%stackaddr$prim58192 = alloca %struct.ScmObj*, align 8
%current_45args56682 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56681)
store volatile %struct.ScmObj* %current_45args56682, %struct.ScmObj** %stackaddr$prim58192, align 8
%stackaddr$prim58193 = alloca %struct.ScmObj*, align 8
%cc47157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56682)
store volatile %struct.ScmObj* %cc47157, %struct.ScmObj** %stackaddr$prim58193, align 8
%ae49791 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58194 = alloca %struct.ScmObj*, align 8
%anf_45bind47291 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47156, %struct.ScmObj* %ae49791)
store volatile %struct.ScmObj* %anf_45bind47291, %struct.ScmObj** %stackaddr$prim58194, align 8
%ae49792 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58195 = alloca %struct.ScmObj*, align 8
%anf_45bind47292 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49792, %struct.ScmObj* %anf_45bind47291)
store volatile %struct.ScmObj* %anf_45bind47292, %struct.ScmObj** %stackaddr$prim58195, align 8
%truthy$cmp58196 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47292)
%cmp$cmp58196 = icmp eq i64 %truthy$cmp58196, 1
br i1 %cmp$cmp58196, label %truebranch$cmp58196, label %falsebranch$cmp58196
truebranch$cmp58196:
%ae49796 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58197 = alloca %struct.ScmObj*, align 8
%cpsprim47413 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47155, %struct.ScmObj* %ae49796)
store volatile %struct.ScmObj* %cpsprim47413, %struct.ScmObj** %stackaddr$prim58197, align 8
%ae49798 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56684$k474100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58198 = alloca %struct.ScmObj*, align 8
%argslist56684$k474101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47413, %struct.ScmObj* %argslist56684$k474100)
store volatile %struct.ScmObj* %argslist56684$k474101, %struct.ScmObj** %stackaddr$prim58198, align 8
%stackaddr$prim58199 = alloca %struct.ScmObj*, align 8
%argslist56684$k474102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49798, %struct.ScmObj* %argslist56684$k474101)
store volatile %struct.ScmObj* %argslist56684$k474102, %struct.ScmObj** %stackaddr$prim58199, align 8
%clofunc58200 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47410)
musttail call tailcc void %clofunc58200(%struct.ScmObj* %k47410, %struct.ScmObj* %argslist56684$k474102)
ret void
falsebranch$cmp58196:
%ae49809 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58201 = alloca %struct.ScmObj*, align 8
%anf_45bind47293 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47155, %struct.ScmObj* %ae49809)
store volatile %struct.ScmObj* %anf_45bind47293, %struct.ScmObj** %stackaddr$prim58201, align 8
%stackaddr$prim58202 = alloca %struct.ScmObj*, align 8
%anf_45bind47294 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47293)
store volatile %struct.ScmObj* %anf_45bind47294, %struct.ScmObj** %stackaddr$prim58202, align 8
%ae49812 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58203 = alloca %struct.ScmObj*, align 8
%_95047160 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47155, %struct.ScmObj* %ae49812, %struct.ScmObj* %anf_45bind47294)
store volatile %struct.ScmObj* %_95047160, %struct.ScmObj** %stackaddr$prim58203, align 8
%ae49815 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58204 = alloca %struct.ScmObj*, align 8
%anf_45bind47295 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47156, %struct.ScmObj* %ae49815)
store volatile %struct.ScmObj* %anf_45bind47295, %struct.ScmObj** %stackaddr$prim58204, align 8
%ae49817 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58205 = alloca %struct.ScmObj*, align 8
%anf_45bind47296 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47295, %struct.ScmObj* %ae49817)
store volatile %struct.ScmObj* %anf_45bind47296, %struct.ScmObj** %stackaddr$prim58205, align 8
%ae49819 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58206 = alloca %struct.ScmObj*, align 8
%_95147159 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47156, %struct.ScmObj* %ae49819, %struct.ScmObj* %anf_45bind47296)
store volatile %struct.ScmObj* %_95147159, %struct.ScmObj** %stackaddr$prim58206, align 8
%argslist56685$cc471570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58207 = alloca %struct.ScmObj*, align 8
%argslist56685$cc471571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47157, %struct.ScmObj* %argslist56685$cc471570)
store volatile %struct.ScmObj* %argslist56685$cc471571, %struct.ScmObj** %stackaddr$prim58207, align 8
%stackaddr$prim58208 = alloca %struct.ScmObj*, align 8
%argslist56685$cc471572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47410, %struct.ScmObj* %argslist56685$cc471571)
store volatile %struct.ScmObj* %argslist56685$cc471572, %struct.ScmObj** %stackaddr$prim58208, align 8
%clofunc58209 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47157)
musttail call tailcc void %clofunc58209(%struct.ScmObj* %cc47157, %struct.ScmObj* %argslist56685$cc471572)
ret void
}

define tailcc void @proc_clo$ae49774(%struct.ScmObj* %env$ae49774,%struct.ScmObj* %current_45args56687) {
%stackaddr$prim58210 = alloca %struct.ScmObj*, align 8
%k47414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56687)
store volatile %struct.ScmObj* %k47414, %struct.ScmObj** %stackaddr$prim58210, align 8
%stackaddr$prim58211 = alloca %struct.ScmObj*, align 8
%current_45args56688 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56687)
store volatile %struct.ScmObj* %current_45args56688, %struct.ScmObj** %stackaddr$prim58211, align 8
%stackaddr$prim58212 = alloca %struct.ScmObj*, align 8
%u47158 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56688)
store volatile %struct.ScmObj* %u47158, %struct.ScmObj** %stackaddr$prim58212, align 8
%argslist56690$u471580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58213 = alloca %struct.ScmObj*, align 8
%argslist56690$u471581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47158, %struct.ScmObj* %argslist56690$u471580)
store volatile %struct.ScmObj* %argslist56690$u471581, %struct.ScmObj** %stackaddr$prim58213, align 8
%stackaddr$prim58214 = alloca %struct.ScmObj*, align 8
%argslist56690$u471582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47414, %struct.ScmObj* %argslist56690$u471581)
store volatile %struct.ScmObj* %argslist56690$u471582, %struct.ScmObj** %stackaddr$prim58214, align 8
%clofunc58215 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47158)
musttail call tailcc void %clofunc58215(%struct.ScmObj* %u47158, %struct.ScmObj* %argslist56690$u471582)
ret void
}

define tailcc void @proc_clo$ae49351(%struct.ScmObj* %env$ae49351,%struct.ScmObj* %current_45args56693) {
%stackaddr$prim58216 = alloca %struct.ScmObj*, align 8
%k47415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56693)
store volatile %struct.ScmObj* %k47415, %struct.ScmObj** %stackaddr$prim58216, align 8
%stackaddr$prim58217 = alloca %struct.ScmObj*, align 8
%current_45args56694 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56693)
store volatile %struct.ScmObj* %current_45args56694, %struct.ScmObj** %stackaddr$prim58217, align 8
%stackaddr$prim58218 = alloca %struct.ScmObj*, align 8
%a47162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56694)
store volatile %struct.ScmObj* %a47162, %struct.ScmObj** %stackaddr$prim58218, align 8
%ae49352 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58219 = alloca %struct.ScmObj*, align 8
%a47163 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49352, %struct.ScmObj* %a47162)
store volatile %struct.ScmObj* %a47163, %struct.ScmObj** %stackaddr$prim58219, align 8
%stackaddr$makeclosure58220 = alloca %struct.ScmObj*, align 8
%fptrToInt58221 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49354 to i64
%ae49354 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58221)
store volatile %struct.ScmObj* %ae49354, %struct.ScmObj** %stackaddr$makeclosure58220, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49354, %struct.ScmObj* %a47163, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49354, %struct.ScmObj* %k47415, i64 1)
%ae49355 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58222 = alloca %struct.ScmObj*, align 8
%fptrToInt58223 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49356 to i64
%ae49356 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58223)
store volatile %struct.ScmObj* %ae49356, %struct.ScmObj** %stackaddr$makeclosure58222, align 8
%argslist56716$ae493540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58224 = alloca %struct.ScmObj*, align 8
%argslist56716$ae493541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49356, %struct.ScmObj* %argslist56716$ae493540)
store volatile %struct.ScmObj* %argslist56716$ae493541, %struct.ScmObj** %stackaddr$prim58224, align 8
%stackaddr$prim58225 = alloca %struct.ScmObj*, align 8
%argslist56716$ae493542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49355, %struct.ScmObj* %argslist56716$ae493541)
store volatile %struct.ScmObj* %argslist56716$ae493542, %struct.ScmObj** %stackaddr$prim58225, align 8
%clofunc58226 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49354)
musttail call tailcc void %clofunc58226(%struct.ScmObj* %ae49354, %struct.ScmObj* %argslist56716$ae493542)
ret void
}

define tailcc void @proc_clo$ae49354(%struct.ScmObj* %env$ae49354,%struct.ScmObj* %current_45args56696) {
%stackaddr$env-ref58227 = alloca %struct.ScmObj*, align 8
%a47163 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49354, i64 0)
store %struct.ScmObj* %a47163, %struct.ScmObj** %stackaddr$env-ref58227
%stackaddr$env-ref58228 = alloca %struct.ScmObj*, align 8
%k47415 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49354, i64 1)
store %struct.ScmObj* %k47415, %struct.ScmObj** %stackaddr$env-ref58228
%stackaddr$prim58229 = alloca %struct.ScmObj*, align 8
%_95k47416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56696)
store volatile %struct.ScmObj* %_95k47416, %struct.ScmObj** %stackaddr$prim58229, align 8
%stackaddr$prim58230 = alloca %struct.ScmObj*, align 8
%current_45args56697 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56696)
store volatile %struct.ScmObj* %current_45args56697, %struct.ScmObj** %stackaddr$prim58230, align 8
%stackaddr$prim58231 = alloca %struct.ScmObj*, align 8
%anf_45bind47282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56697)
store volatile %struct.ScmObj* %anf_45bind47282, %struct.ScmObj** %stackaddr$prim58231, align 8
%stackaddr$makeclosure58232 = alloca %struct.ScmObj*, align 8
%fptrToInt58233 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49373 to i64
%ae49373 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58233)
store volatile %struct.ScmObj* %ae49373, %struct.ScmObj** %stackaddr$makeclosure58232, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49373, %struct.ScmObj* %a47163, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49373, %struct.ScmObj* %k47415, i64 1)
%stackaddr$makeclosure58234 = alloca %struct.ScmObj*, align 8
%fptrToInt58235 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49374 to i64
%ae49374 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58235)
store volatile %struct.ScmObj* %ae49374, %struct.ScmObj** %stackaddr$makeclosure58234, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49374, %struct.ScmObj* %a47163, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49374, %struct.ScmObj* %k47415, i64 1)
%argslist56711$anf_45bind472820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58236 = alloca %struct.ScmObj*, align 8
%argslist56711$anf_45bind472821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49374, %struct.ScmObj* %argslist56711$anf_45bind472820)
store volatile %struct.ScmObj* %argslist56711$anf_45bind472821, %struct.ScmObj** %stackaddr$prim58236, align 8
%stackaddr$prim58237 = alloca %struct.ScmObj*, align 8
%argslist56711$anf_45bind472822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49373, %struct.ScmObj* %argslist56711$anf_45bind472821)
store volatile %struct.ScmObj* %argslist56711$anf_45bind472822, %struct.ScmObj** %stackaddr$prim58237, align 8
%clofunc58238 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47282)
musttail call tailcc void %clofunc58238(%struct.ScmObj* %anf_45bind47282, %struct.ScmObj* %argslist56711$anf_45bind472822)
ret void
}

define tailcc void @proc_clo$ae49373(%struct.ScmObj* %env$ae49373,%struct.ScmObj* %current_45args56699) {
%stackaddr$env-ref58239 = alloca %struct.ScmObj*, align 8
%a47163 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49373, i64 0)
store %struct.ScmObj* %a47163, %struct.ScmObj** %stackaddr$env-ref58239
%stackaddr$env-ref58240 = alloca %struct.ScmObj*, align 8
%k47415 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49373, i64 1)
store %struct.ScmObj* %k47415, %struct.ScmObj** %stackaddr$env-ref58240
%stackaddr$prim58241 = alloca %struct.ScmObj*, align 8
%_95k47417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56699)
store volatile %struct.ScmObj* %_95k47417, %struct.ScmObj** %stackaddr$prim58241, align 8
%stackaddr$prim58242 = alloca %struct.ScmObj*, align 8
%current_45args56700 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56699)
store volatile %struct.ScmObj* %current_45args56700, %struct.ScmObj** %stackaddr$prim58242, align 8
%stackaddr$prim58243 = alloca %struct.ScmObj*, align 8
%cc47164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56700)
store volatile %struct.ScmObj* %cc47164, %struct.ScmObj** %stackaddr$prim58243, align 8
%ae49489 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58244 = alloca %struct.ScmObj*, align 8
%anf_45bind47283 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47163, %struct.ScmObj* %ae49489)
store volatile %struct.ScmObj* %anf_45bind47283, %struct.ScmObj** %stackaddr$prim58244, align 8
%stackaddr$prim58245 = alloca %struct.ScmObj*, align 8
%anf_45bind47284 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47283)
store volatile %struct.ScmObj* %anf_45bind47284, %struct.ScmObj** %stackaddr$prim58245, align 8
%truthy$cmp58246 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47284)
%cmp$cmp58246 = icmp eq i64 %truthy$cmp58246, 1
br i1 %cmp$cmp58246, label %truebranch$cmp58246, label %falsebranch$cmp58246
truebranch$cmp58246:
%ae49493 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49494 = call %struct.ScmObj* @const_init_true()
%argslist56702$k474150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58247 = alloca %struct.ScmObj*, align 8
%argslist56702$k474151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49494, %struct.ScmObj* %argslist56702$k474150)
store volatile %struct.ScmObj* %argslist56702$k474151, %struct.ScmObj** %stackaddr$prim58247, align 8
%stackaddr$prim58248 = alloca %struct.ScmObj*, align 8
%argslist56702$k474152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49493, %struct.ScmObj* %argslist56702$k474151)
store volatile %struct.ScmObj* %argslist56702$k474152, %struct.ScmObj** %stackaddr$prim58248, align 8
%clofunc58249 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47415)
musttail call tailcc void %clofunc58249(%struct.ScmObj* %k47415, %struct.ScmObj* %argslist56702$k474152)
ret void
falsebranch$cmp58246:
%ae49502 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58250 = alloca %struct.ScmObj*, align 8
%anf_45bind47285 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47163, %struct.ScmObj* %ae49502)
store volatile %struct.ScmObj* %anf_45bind47285, %struct.ScmObj** %stackaddr$prim58250, align 8
%stackaddr$prim58251 = alloca %struct.ScmObj*, align 8
%anf_45bind47286 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47285)
store volatile %struct.ScmObj* %anf_45bind47286, %struct.ScmObj** %stackaddr$prim58251, align 8
%truthy$cmp58252 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47286)
%cmp$cmp58252 = icmp eq i64 %truthy$cmp58252, 1
br i1 %cmp$cmp58252, label %truebranch$cmp58252, label %falsebranch$cmp58252
truebranch$cmp58252:
%ae49506 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58253 = alloca %struct.ScmObj*, align 8
%anf_45bind47287 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47163, %struct.ScmObj* %ae49506)
store volatile %struct.ScmObj* %anf_45bind47287, %struct.ScmObj** %stackaddr$prim58253, align 8
%stackaddr$prim58254 = alloca %struct.ScmObj*, align 8
%b47166 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47287)
store volatile %struct.ScmObj* %b47166, %struct.ScmObj** %stackaddr$prim58254, align 8
%ae49509 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58255 = alloca %struct.ScmObj*, align 8
%anf_45bind47288 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47163, %struct.ScmObj* %ae49509)
store volatile %struct.ScmObj* %anf_45bind47288, %struct.ScmObj** %stackaddr$prim58255, align 8
%stackaddr$prim58256 = alloca %struct.ScmObj*, align 8
%anf_45bind47289 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47288)
store volatile %struct.ScmObj* %anf_45bind47289, %struct.ScmObj** %stackaddr$prim58256, align 8
%ae49512 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58257 = alloca %struct.ScmObj*, align 8
%_95047167 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47163, %struct.ScmObj* %ae49512, %struct.ScmObj* %anf_45bind47289)
store volatile %struct.ScmObj* %_95047167, %struct.ScmObj** %stackaddr$prim58257, align 8
%argslist56703$cc471640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58258 = alloca %struct.ScmObj*, align 8
%argslist56703$cc471641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47164, %struct.ScmObj* %argslist56703$cc471640)
store volatile %struct.ScmObj* %argslist56703$cc471641, %struct.ScmObj** %stackaddr$prim58258, align 8
%stackaddr$prim58259 = alloca %struct.ScmObj*, align 8
%argslist56703$cc471642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47415, %struct.ScmObj* %argslist56703$cc471641)
store volatile %struct.ScmObj* %argslist56703$cc471642, %struct.ScmObj** %stackaddr$prim58259, align 8
%clofunc58260 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47164)
musttail call tailcc void %clofunc58260(%struct.ScmObj* %cc47164, %struct.ScmObj* %argslist56703$cc471642)
ret void
falsebranch$cmp58252:
%ae49545 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49546 = call %struct.ScmObj* @const_init_false()
%argslist56704$k474150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58261 = alloca %struct.ScmObj*, align 8
%argslist56704$k474151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49546, %struct.ScmObj* %argslist56704$k474150)
store volatile %struct.ScmObj* %argslist56704$k474151, %struct.ScmObj** %stackaddr$prim58261, align 8
%stackaddr$prim58262 = alloca %struct.ScmObj*, align 8
%argslist56704$k474152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49545, %struct.ScmObj* %argslist56704$k474151)
store volatile %struct.ScmObj* %argslist56704$k474152, %struct.ScmObj** %stackaddr$prim58262, align 8
%clofunc58263 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47415)
musttail call tailcc void %clofunc58263(%struct.ScmObj* %k47415, %struct.ScmObj* %argslist56704$k474152)
ret void
}

define tailcc void @proc_clo$ae49374(%struct.ScmObj* %env$ae49374,%struct.ScmObj* %current_45args56705) {
%stackaddr$env-ref58264 = alloca %struct.ScmObj*, align 8
%a47163 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49374, i64 0)
store %struct.ScmObj* %a47163, %struct.ScmObj** %stackaddr$env-ref58264
%stackaddr$env-ref58265 = alloca %struct.ScmObj*, align 8
%k47415 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49374, i64 1)
store %struct.ScmObj* %k47415, %struct.ScmObj** %stackaddr$env-ref58265
%stackaddr$prim58266 = alloca %struct.ScmObj*, align 8
%_95k47417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56705)
store volatile %struct.ScmObj* %_95k47417, %struct.ScmObj** %stackaddr$prim58266, align 8
%stackaddr$prim58267 = alloca %struct.ScmObj*, align 8
%current_45args56706 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56705)
store volatile %struct.ScmObj* %current_45args56706, %struct.ScmObj** %stackaddr$prim58267, align 8
%stackaddr$prim58268 = alloca %struct.ScmObj*, align 8
%cc47164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56706)
store volatile %struct.ScmObj* %cc47164, %struct.ScmObj** %stackaddr$prim58268, align 8
%ae49376 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58269 = alloca %struct.ScmObj*, align 8
%anf_45bind47283 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47163, %struct.ScmObj* %ae49376)
store volatile %struct.ScmObj* %anf_45bind47283, %struct.ScmObj** %stackaddr$prim58269, align 8
%stackaddr$prim58270 = alloca %struct.ScmObj*, align 8
%anf_45bind47284 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47283)
store volatile %struct.ScmObj* %anf_45bind47284, %struct.ScmObj** %stackaddr$prim58270, align 8
%truthy$cmp58271 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47284)
%cmp$cmp58271 = icmp eq i64 %truthy$cmp58271, 1
br i1 %cmp$cmp58271, label %truebranch$cmp58271, label %falsebranch$cmp58271
truebranch$cmp58271:
%ae49380 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49381 = call %struct.ScmObj* @const_init_true()
%argslist56708$k474150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58272 = alloca %struct.ScmObj*, align 8
%argslist56708$k474151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49381, %struct.ScmObj* %argslist56708$k474150)
store volatile %struct.ScmObj* %argslist56708$k474151, %struct.ScmObj** %stackaddr$prim58272, align 8
%stackaddr$prim58273 = alloca %struct.ScmObj*, align 8
%argslist56708$k474152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49380, %struct.ScmObj* %argslist56708$k474151)
store volatile %struct.ScmObj* %argslist56708$k474152, %struct.ScmObj** %stackaddr$prim58273, align 8
%clofunc58274 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47415)
musttail call tailcc void %clofunc58274(%struct.ScmObj* %k47415, %struct.ScmObj* %argslist56708$k474152)
ret void
falsebranch$cmp58271:
%ae49389 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58275 = alloca %struct.ScmObj*, align 8
%anf_45bind47285 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47163, %struct.ScmObj* %ae49389)
store volatile %struct.ScmObj* %anf_45bind47285, %struct.ScmObj** %stackaddr$prim58275, align 8
%stackaddr$prim58276 = alloca %struct.ScmObj*, align 8
%anf_45bind47286 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47285)
store volatile %struct.ScmObj* %anf_45bind47286, %struct.ScmObj** %stackaddr$prim58276, align 8
%truthy$cmp58277 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47286)
%cmp$cmp58277 = icmp eq i64 %truthy$cmp58277, 1
br i1 %cmp$cmp58277, label %truebranch$cmp58277, label %falsebranch$cmp58277
truebranch$cmp58277:
%ae49393 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58278 = alloca %struct.ScmObj*, align 8
%anf_45bind47287 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47163, %struct.ScmObj* %ae49393)
store volatile %struct.ScmObj* %anf_45bind47287, %struct.ScmObj** %stackaddr$prim58278, align 8
%stackaddr$prim58279 = alloca %struct.ScmObj*, align 8
%b47166 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47287)
store volatile %struct.ScmObj* %b47166, %struct.ScmObj** %stackaddr$prim58279, align 8
%ae49396 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58280 = alloca %struct.ScmObj*, align 8
%anf_45bind47288 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47163, %struct.ScmObj* %ae49396)
store volatile %struct.ScmObj* %anf_45bind47288, %struct.ScmObj** %stackaddr$prim58280, align 8
%stackaddr$prim58281 = alloca %struct.ScmObj*, align 8
%anf_45bind47289 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47288)
store volatile %struct.ScmObj* %anf_45bind47289, %struct.ScmObj** %stackaddr$prim58281, align 8
%ae49399 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58282 = alloca %struct.ScmObj*, align 8
%_95047167 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47163, %struct.ScmObj* %ae49399, %struct.ScmObj* %anf_45bind47289)
store volatile %struct.ScmObj* %_95047167, %struct.ScmObj** %stackaddr$prim58282, align 8
%argslist56709$cc471640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58283 = alloca %struct.ScmObj*, align 8
%argslist56709$cc471641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47164, %struct.ScmObj* %argslist56709$cc471640)
store volatile %struct.ScmObj* %argslist56709$cc471641, %struct.ScmObj** %stackaddr$prim58283, align 8
%stackaddr$prim58284 = alloca %struct.ScmObj*, align 8
%argslist56709$cc471642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47415, %struct.ScmObj* %argslist56709$cc471641)
store volatile %struct.ScmObj* %argslist56709$cc471642, %struct.ScmObj** %stackaddr$prim58284, align 8
%clofunc58285 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47164)
musttail call tailcc void %clofunc58285(%struct.ScmObj* %cc47164, %struct.ScmObj* %argslist56709$cc471642)
ret void
falsebranch$cmp58277:
%ae49432 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49433 = call %struct.ScmObj* @const_init_false()
%argslist56710$k474150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58286 = alloca %struct.ScmObj*, align 8
%argslist56710$k474151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49433, %struct.ScmObj* %argslist56710$k474150)
store volatile %struct.ScmObj* %argslist56710$k474151, %struct.ScmObj** %stackaddr$prim58286, align 8
%stackaddr$prim58287 = alloca %struct.ScmObj*, align 8
%argslist56710$k474152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49432, %struct.ScmObj* %argslist56710$k474151)
store volatile %struct.ScmObj* %argslist56710$k474152, %struct.ScmObj** %stackaddr$prim58287, align 8
%clofunc58288 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47415)
musttail call tailcc void %clofunc58288(%struct.ScmObj* %k47415, %struct.ScmObj* %argslist56710$k474152)
ret void
}

define tailcc void @proc_clo$ae49356(%struct.ScmObj* %env$ae49356,%struct.ScmObj* %current_45args56712) {
%stackaddr$prim58289 = alloca %struct.ScmObj*, align 8
%k47418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56712)
store volatile %struct.ScmObj* %k47418, %struct.ScmObj** %stackaddr$prim58289, align 8
%stackaddr$prim58290 = alloca %struct.ScmObj*, align 8
%current_45args56713 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56712)
store volatile %struct.ScmObj* %current_45args56713, %struct.ScmObj** %stackaddr$prim58290, align 8
%stackaddr$prim58291 = alloca %struct.ScmObj*, align 8
%k47165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56713)
store volatile %struct.ScmObj* %k47165, %struct.ScmObj** %stackaddr$prim58291, align 8
%ae49358 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56715$k474180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58292 = alloca %struct.ScmObj*, align 8
%argslist56715$k474181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47165, %struct.ScmObj* %argslist56715$k474180)
store volatile %struct.ScmObj* %argslist56715$k474181, %struct.ScmObj** %stackaddr$prim58292, align 8
%stackaddr$prim58293 = alloca %struct.ScmObj*, align 8
%argslist56715$k474182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49358, %struct.ScmObj* %argslist56715$k474181)
store volatile %struct.ScmObj* %argslist56715$k474182, %struct.ScmObj** %stackaddr$prim58293, align 8
%clofunc58294 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47418)
musttail call tailcc void %clofunc58294(%struct.ScmObj* %k47418, %struct.ScmObj* %argslist56715$k474182)
ret void
}

define tailcc void @proc_clo$ae49279(%struct.ScmObj* %env$ae49279,%struct.ScmObj* %current_45args56718) {
%stackaddr$env-ref58295 = alloca %struct.ScmObj*, align 8
%_37append47169 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49279, i64 0)
store %struct.ScmObj* %_37append47169, %struct.ScmObj** %stackaddr$env-ref58295
%stackaddr$prim58296 = alloca %struct.ScmObj*, align 8
%k47419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56718)
store volatile %struct.ScmObj* %k47419, %struct.ScmObj** %stackaddr$prim58296, align 8
%stackaddr$prim58297 = alloca %struct.ScmObj*, align 8
%current_45args56719 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56718)
store volatile %struct.ScmObj* %current_45args56719, %struct.ScmObj** %stackaddr$prim58297, align 8
%stackaddr$prim58298 = alloca %struct.ScmObj*, align 8
%ls047172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56719)
store volatile %struct.ScmObj* %ls047172, %struct.ScmObj** %stackaddr$prim58298, align 8
%stackaddr$prim58299 = alloca %struct.ScmObj*, align 8
%current_45args56720 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56719)
store volatile %struct.ScmObj* %current_45args56720, %struct.ScmObj** %stackaddr$prim58299, align 8
%stackaddr$prim58300 = alloca %struct.ScmObj*, align 8
%ls147171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56720)
store volatile %struct.ScmObj* %ls147171, %struct.ScmObj** %stackaddr$prim58300, align 8
%stackaddr$prim58301 = alloca %struct.ScmObj*, align 8
%anf_45bind47276 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls047172)
store volatile %struct.ScmObj* %anf_45bind47276, %struct.ScmObj** %stackaddr$prim58301, align 8
%truthy$cmp58302 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47276)
%cmp$cmp58302 = icmp eq i64 %truthy$cmp58302, 1
br i1 %cmp$cmp58302, label %truebranch$cmp58302, label %falsebranch$cmp58302
truebranch$cmp58302:
%ae49283 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56722$k474190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58303 = alloca %struct.ScmObj*, align 8
%argslist56722$k474191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147171, %struct.ScmObj* %argslist56722$k474190)
store volatile %struct.ScmObj* %argslist56722$k474191, %struct.ScmObj** %stackaddr$prim58303, align 8
%stackaddr$prim58304 = alloca %struct.ScmObj*, align 8
%argslist56722$k474192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49283, %struct.ScmObj* %argslist56722$k474191)
store volatile %struct.ScmObj* %argslist56722$k474192, %struct.ScmObj** %stackaddr$prim58304, align 8
%clofunc58305 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47419)
musttail call tailcc void %clofunc58305(%struct.ScmObj* %k47419, %struct.ScmObj* %argslist56722$k474192)
ret void
falsebranch$cmp58302:
%stackaddr$prim58306 = alloca %struct.ScmObj*, align 8
%anf_45bind47277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls047172)
store volatile %struct.ScmObj* %anf_45bind47277, %struct.ScmObj** %stackaddr$prim58306, align 8
%ae49290 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58307 = alloca %struct.ScmObj*, align 8
%anf_45bind47278 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47169, %struct.ScmObj* %ae49290)
store volatile %struct.ScmObj* %anf_45bind47278, %struct.ScmObj** %stackaddr$prim58307, align 8
%stackaddr$prim58308 = alloca %struct.ScmObj*, align 8
%anf_45bind47279 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls047172)
store volatile %struct.ScmObj* %anf_45bind47279, %struct.ScmObj** %stackaddr$prim58308, align 8
%stackaddr$makeclosure58309 = alloca %struct.ScmObj*, align 8
%fptrToInt58310 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49293 to i64
%ae49293 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58310)
store volatile %struct.ScmObj* %ae49293, %struct.ScmObj** %stackaddr$makeclosure58309, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49293, %struct.ScmObj* %anf_45bind47277, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49293, %struct.ScmObj* %k47419, i64 1)
%argslist56727$anf_45bind472780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58311 = alloca %struct.ScmObj*, align 8
%argslist56727$anf_45bind472781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147171, %struct.ScmObj* %argslist56727$anf_45bind472780)
store volatile %struct.ScmObj* %argslist56727$anf_45bind472781, %struct.ScmObj** %stackaddr$prim58311, align 8
%stackaddr$prim58312 = alloca %struct.ScmObj*, align 8
%argslist56727$anf_45bind472782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47279, %struct.ScmObj* %argslist56727$anf_45bind472781)
store volatile %struct.ScmObj* %argslist56727$anf_45bind472782, %struct.ScmObj** %stackaddr$prim58312, align 8
%stackaddr$prim58313 = alloca %struct.ScmObj*, align 8
%argslist56727$anf_45bind472783 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49293, %struct.ScmObj* %argslist56727$anf_45bind472782)
store volatile %struct.ScmObj* %argslist56727$anf_45bind472783, %struct.ScmObj** %stackaddr$prim58313, align 8
%clofunc58314 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47278)
musttail call tailcc void %clofunc58314(%struct.ScmObj* %anf_45bind47278, %struct.ScmObj* %argslist56727$anf_45bind472783)
ret void
}

define tailcc void @proc_clo$ae49293(%struct.ScmObj* %env$ae49293,%struct.ScmObj* %current_45args56723) {
%stackaddr$env-ref58315 = alloca %struct.ScmObj*, align 8
%anf_45bind47277 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49293, i64 0)
store %struct.ScmObj* %anf_45bind47277, %struct.ScmObj** %stackaddr$env-ref58315
%stackaddr$env-ref58316 = alloca %struct.ScmObj*, align 8
%k47419 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49293, i64 1)
store %struct.ScmObj* %k47419, %struct.ScmObj** %stackaddr$env-ref58316
%stackaddr$prim58317 = alloca %struct.ScmObj*, align 8
%_95k47420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56723)
store volatile %struct.ScmObj* %_95k47420, %struct.ScmObj** %stackaddr$prim58317, align 8
%stackaddr$prim58318 = alloca %struct.ScmObj*, align 8
%current_45args56724 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56723)
store volatile %struct.ScmObj* %current_45args56724, %struct.ScmObj** %stackaddr$prim58318, align 8
%stackaddr$prim58319 = alloca %struct.ScmObj*, align 8
%anf_45bind47280 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56724)
store volatile %struct.ScmObj* %anf_45bind47280, %struct.ScmObj** %stackaddr$prim58319, align 8
%stackaddr$prim58320 = alloca %struct.ScmObj*, align 8
%cpsprim47421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47277, %struct.ScmObj* %anf_45bind47280)
store volatile %struct.ScmObj* %cpsprim47421, %struct.ScmObj** %stackaddr$prim58320, align 8
%ae49299 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56726$k474190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58321 = alloca %struct.ScmObj*, align 8
%argslist56726$k474191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47421, %struct.ScmObj* %argslist56726$k474190)
store volatile %struct.ScmObj* %argslist56726$k474191, %struct.ScmObj** %stackaddr$prim58321, align 8
%stackaddr$prim58322 = alloca %struct.ScmObj*, align 8
%argslist56726$k474192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49299, %struct.ScmObj* %argslist56726$k474191)
store volatile %struct.ScmObj* %argslist56726$k474192, %struct.ScmObj** %stackaddr$prim58322, align 8
%clofunc58323 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47419)
musttail call tailcc void %clofunc58323(%struct.ScmObj* %k47419, %struct.ScmObj* %argslist56726$k474192)
ret void
}

define tailcc void @proc_clo$ae49253(%struct.ScmObj* %env$ae49253,%struct.ScmObj* %current_45args56729) {
%stackaddr$prim58324 = alloca %struct.ScmObj*, align 8
%k47422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56729)
store volatile %struct.ScmObj* %k47422, %struct.ScmObj** %stackaddr$prim58324, align 8
%stackaddr$prim58325 = alloca %struct.ScmObj*, align 8
%current_45args56730 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56729)
store volatile %struct.ScmObj* %current_45args56730, %struct.ScmObj** %stackaddr$prim58325, align 8
%stackaddr$prim58326 = alloca %struct.ScmObj*, align 8
%a47175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56730)
store volatile %struct.ScmObj* %a47175, %struct.ScmObj** %stackaddr$prim58326, align 8
%stackaddr$prim58327 = alloca %struct.ScmObj*, align 8
%current_45args56731 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56730)
store volatile %struct.ScmObj* %current_45args56731, %struct.ScmObj** %stackaddr$prim58327, align 8
%stackaddr$prim58328 = alloca %struct.ScmObj*, align 8
%b47174 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56731)
store volatile %struct.ScmObj* %b47174, %struct.ScmObj** %stackaddr$prim58328, align 8
%stackaddr$prim58329 = alloca %struct.ScmObj*, align 8
%anf_45bind47275 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a47175, %struct.ScmObj* %b47174)
store volatile %struct.ScmObj* %anf_45bind47275, %struct.ScmObj** %stackaddr$prim58329, align 8
%stackaddr$prim58330 = alloca %struct.ScmObj*, align 8
%cpsprim47423 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47275)
store volatile %struct.ScmObj* %cpsprim47423, %struct.ScmObj** %stackaddr$prim58330, align 8
%ae49258 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56733$k474220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58331 = alloca %struct.ScmObj*, align 8
%argslist56733$k474221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47423, %struct.ScmObj* %argslist56733$k474220)
store volatile %struct.ScmObj* %argslist56733$k474221, %struct.ScmObj** %stackaddr$prim58331, align 8
%stackaddr$prim58332 = alloca %struct.ScmObj*, align 8
%argslist56733$k474222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49258, %struct.ScmObj* %argslist56733$k474221)
store volatile %struct.ScmObj* %argslist56733$k474222, %struct.ScmObj** %stackaddr$prim58332, align 8
%clofunc58333 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47422)
musttail call tailcc void %clofunc58333(%struct.ScmObj* %k47422, %struct.ScmObj* %argslist56733$k474222)
ret void
}

define tailcc void @proc_clo$ae49229(%struct.ScmObj* %env$ae49229,%struct.ScmObj* %current_45args56735) {
%stackaddr$prim58334 = alloca %struct.ScmObj*, align 8
%k47424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56735)
store volatile %struct.ScmObj* %k47424, %struct.ScmObj** %stackaddr$prim58334, align 8
%stackaddr$prim58335 = alloca %struct.ScmObj*, align 8
%current_45args56736 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56735)
store volatile %struct.ScmObj* %current_45args56736, %struct.ScmObj** %stackaddr$prim58335, align 8
%stackaddr$prim58336 = alloca %struct.ScmObj*, align 8
%a47178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56736)
store volatile %struct.ScmObj* %a47178, %struct.ScmObj** %stackaddr$prim58336, align 8
%stackaddr$prim58337 = alloca %struct.ScmObj*, align 8
%current_45args56737 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56736)
store volatile %struct.ScmObj* %current_45args56737, %struct.ScmObj** %stackaddr$prim58337, align 8
%stackaddr$prim58338 = alloca %struct.ScmObj*, align 8
%b47177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56737)
store volatile %struct.ScmObj* %b47177, %struct.ScmObj** %stackaddr$prim58338, align 8
%stackaddr$prim58339 = alloca %struct.ScmObj*, align 8
%anf_45bind47274 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a47178, %struct.ScmObj* %b47177)
store volatile %struct.ScmObj* %anf_45bind47274, %struct.ScmObj** %stackaddr$prim58339, align 8
%stackaddr$prim58340 = alloca %struct.ScmObj*, align 8
%cpsprim47425 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47274)
store volatile %struct.ScmObj* %cpsprim47425, %struct.ScmObj** %stackaddr$prim58340, align 8
%ae49234 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56739$k474240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58341 = alloca %struct.ScmObj*, align 8
%argslist56739$k474241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47425, %struct.ScmObj* %argslist56739$k474240)
store volatile %struct.ScmObj* %argslist56739$k474241, %struct.ScmObj** %stackaddr$prim58341, align 8
%stackaddr$prim58342 = alloca %struct.ScmObj*, align 8
%argslist56739$k474242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49234, %struct.ScmObj* %argslist56739$k474241)
store volatile %struct.ScmObj* %argslist56739$k474242, %struct.ScmObj** %stackaddr$prim58342, align 8
%clofunc58343 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47424)
musttail call tailcc void %clofunc58343(%struct.ScmObj* %k47424, %struct.ScmObj* %argslist56739$k474242)
ret void
}

define tailcc void @proc_clo$ae48835(%struct.ScmObj* %env$ae48835,%struct.ScmObj* %current_45args56742) {
%stackaddr$env-ref58344 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48835, i64 0)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref58344
%stackaddr$env-ref58345 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48835, i64 1)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref58345
%stackaddr$env-ref58346 = alloca %struct.ScmObj*, align 8
%_37map147128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48835, i64 2)
store %struct.ScmObj* %_37map147128, %struct.ScmObj** %stackaddr$env-ref58346
%stackaddr$prim58347 = alloca %struct.ScmObj*, align 8
%k47426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56742)
store volatile %struct.ScmObj* %k47426, %struct.ScmObj** %stackaddr$prim58347, align 8
%stackaddr$prim58348 = alloca %struct.ScmObj*, align 8
%current_45args56743 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56742)
store volatile %struct.ScmObj* %current_45args56743, %struct.ScmObj** %stackaddr$prim58348, align 8
%stackaddr$prim58349 = alloca %struct.ScmObj*, align 8
%_37foldl47180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56743)
store volatile %struct.ScmObj* %_37foldl47180, %struct.ScmObj** %stackaddr$prim58349, align 8
%ae48837 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58350 = alloca %struct.ScmObj*, align 8
%fptrToInt58351 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48838 to i64
%ae48838 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58351)
store volatile %struct.ScmObj* %ae48838, %struct.ScmObj** %stackaddr$makeclosure58350, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48838, %struct.ScmObj* %_37foldr47102, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48838, %struct.ScmObj* %_37foldl47180, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48838, %struct.ScmObj* %_37foldr147097, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48838, %struct.ScmObj* %_37map147128, i64 3)
%argslist56800$k474260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58352 = alloca %struct.ScmObj*, align 8
%argslist56800$k474261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48838, %struct.ScmObj* %argslist56800$k474260)
store volatile %struct.ScmObj* %argslist56800$k474261, %struct.ScmObj** %stackaddr$prim58352, align 8
%stackaddr$prim58353 = alloca %struct.ScmObj*, align 8
%argslist56800$k474262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48837, %struct.ScmObj* %argslist56800$k474261)
store volatile %struct.ScmObj* %argslist56800$k474262, %struct.ScmObj** %stackaddr$prim58353, align 8
%clofunc58354 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47426)
musttail call tailcc void %clofunc58354(%struct.ScmObj* %k47426, %struct.ScmObj* %argslist56800$k474262)
ret void
}

define tailcc void @proc_clo$ae48838(%struct.ScmObj* %env$ae48838,%struct.ScmObj* %args4718147427) {
%stackaddr$env-ref58355 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48838, i64 0)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref58355
%stackaddr$env-ref58356 = alloca %struct.ScmObj*, align 8
%_37foldl47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48838, i64 1)
store %struct.ScmObj* %_37foldl47180, %struct.ScmObj** %stackaddr$env-ref58356
%stackaddr$env-ref58357 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48838, i64 2)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref58357
%stackaddr$env-ref58358 = alloca %struct.ScmObj*, align 8
%_37map147128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48838, i64 3)
store %struct.ScmObj* %_37map147128, %struct.ScmObj** %stackaddr$env-ref58358
%stackaddr$prim58359 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4718147427)
store volatile %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$prim58359, align 8
%stackaddr$prim58360 = alloca %struct.ScmObj*, align 8
%args47181 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4718147427)
store volatile %struct.ScmObj* %args47181, %struct.ScmObj** %stackaddr$prim58360, align 8
%stackaddr$prim58361 = alloca %struct.ScmObj*, align 8
%f47184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47181)
store volatile %struct.ScmObj* %f47184, %struct.ScmObj** %stackaddr$prim58361, align 8
%stackaddr$prim58362 = alloca %struct.ScmObj*, align 8
%anf_45bind47262 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47181)
store volatile %struct.ScmObj* %anf_45bind47262, %struct.ScmObj** %stackaddr$prim58362, align 8
%stackaddr$prim58363 = alloca %struct.ScmObj*, align 8
%acc47183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47262)
store volatile %struct.ScmObj* %acc47183, %struct.ScmObj** %stackaddr$prim58363, align 8
%stackaddr$prim58364 = alloca %struct.ScmObj*, align 8
%anf_45bind47263 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47181)
store volatile %struct.ScmObj* %anf_45bind47263, %struct.ScmObj** %stackaddr$prim58364, align 8
%stackaddr$prim58365 = alloca %struct.ScmObj*, align 8
%lsts47182 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47263)
store volatile %struct.ScmObj* %lsts47182, %struct.ScmObj** %stackaddr$prim58365, align 8
%stackaddr$makeclosure58366 = alloca %struct.ScmObj*, align 8
%fptrToInt58367 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48846 to i64
%ae48846 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt58367)
store volatile %struct.ScmObj* %ae48846, %struct.ScmObj** %stackaddr$makeclosure58366, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48846, %struct.ScmObj* %lsts47182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48846, %struct.ScmObj* %_37foldr47102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48846, %struct.ScmObj* %k47428, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48846, %struct.ScmObj* %f47184, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48846, %struct.ScmObj* %acc47183, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48846, %struct.ScmObj* %_37foldl47180, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48846, %struct.ScmObj* %_37foldr147097, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48846, %struct.ScmObj* %_37map147128, i64 7)
%ae48847 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58368 = alloca %struct.ScmObj*, align 8
%fptrToInt58369 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48848 to i64
%ae48848 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58369)
store volatile %struct.ScmObj* %ae48848, %struct.ScmObj** %stackaddr$makeclosure58368, align 8
%argslist56799$ae488460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58370 = alloca %struct.ScmObj*, align 8
%argslist56799$ae488461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48848, %struct.ScmObj* %argslist56799$ae488460)
store volatile %struct.ScmObj* %argslist56799$ae488461, %struct.ScmObj** %stackaddr$prim58370, align 8
%stackaddr$prim58371 = alloca %struct.ScmObj*, align 8
%argslist56799$ae488462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48847, %struct.ScmObj* %argslist56799$ae488461)
store volatile %struct.ScmObj* %argslist56799$ae488462, %struct.ScmObj** %stackaddr$prim58371, align 8
%clofunc58372 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48846)
musttail call tailcc void %clofunc58372(%struct.ScmObj* %ae48846, %struct.ScmObj* %argslist56799$ae488462)
ret void
}

define tailcc void @proc_clo$ae48846(%struct.ScmObj* %env$ae48846,%struct.ScmObj* %current_45args56745) {
%stackaddr$env-ref58373 = alloca %struct.ScmObj*, align 8
%lsts47182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48846, i64 0)
store %struct.ScmObj* %lsts47182, %struct.ScmObj** %stackaddr$env-ref58373
%stackaddr$env-ref58374 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48846, i64 1)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref58374
%stackaddr$env-ref58375 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48846, i64 2)
store %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$env-ref58375
%stackaddr$env-ref58376 = alloca %struct.ScmObj*, align 8
%f47184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48846, i64 3)
store %struct.ScmObj* %f47184, %struct.ScmObj** %stackaddr$env-ref58376
%stackaddr$env-ref58377 = alloca %struct.ScmObj*, align 8
%acc47183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48846, i64 4)
store %struct.ScmObj* %acc47183, %struct.ScmObj** %stackaddr$env-ref58377
%stackaddr$env-ref58378 = alloca %struct.ScmObj*, align 8
%_37foldl47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48846, i64 5)
store %struct.ScmObj* %_37foldl47180, %struct.ScmObj** %stackaddr$env-ref58378
%stackaddr$env-ref58379 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48846, i64 6)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref58379
%stackaddr$env-ref58380 = alloca %struct.ScmObj*, align 8
%_37map147128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48846, i64 7)
store %struct.ScmObj* %_37map147128, %struct.ScmObj** %stackaddr$env-ref58380
%stackaddr$prim58381 = alloca %struct.ScmObj*, align 8
%_95k47429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56745)
store volatile %struct.ScmObj* %_95k47429, %struct.ScmObj** %stackaddr$prim58381, align 8
%stackaddr$prim58382 = alloca %struct.ScmObj*, align 8
%current_45args56746 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56745)
store volatile %struct.ScmObj* %current_45args56746, %struct.ScmObj** %stackaddr$prim58382, align 8
%stackaddr$prim58383 = alloca %struct.ScmObj*, align 8
%anf_45bind47264 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56746)
store volatile %struct.ScmObj* %anf_45bind47264, %struct.ScmObj** %stackaddr$prim58383, align 8
%stackaddr$makeclosure58384 = alloca %struct.ScmObj*, align 8
%fptrToInt58385 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48878 to i64
%ae48878 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58385)
store volatile %struct.ScmObj* %ae48878, %struct.ScmObj** %stackaddr$makeclosure58384, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48878, %struct.ScmObj* %lsts47182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48878, %struct.ScmObj* %_37foldr47102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48878, %struct.ScmObj* %k47428, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48878, %struct.ScmObj* %f47184, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48878, %struct.ScmObj* %acc47183, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48878, %struct.ScmObj* %_37foldl47180, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48878, %struct.ScmObj* %_37map147128, i64 6)
%ae48880 = call %struct.ScmObj* @const_init_false()
%argslist56792$_37foldr1470970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58386 = alloca %struct.ScmObj*, align 8
%argslist56792$_37foldr1470971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47182, %struct.ScmObj* %argslist56792$_37foldr1470970)
store volatile %struct.ScmObj* %argslist56792$_37foldr1470971, %struct.ScmObj** %stackaddr$prim58386, align 8
%stackaddr$prim58387 = alloca %struct.ScmObj*, align 8
%argslist56792$_37foldr1470972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48880, %struct.ScmObj* %argslist56792$_37foldr1470971)
store volatile %struct.ScmObj* %argslist56792$_37foldr1470972, %struct.ScmObj** %stackaddr$prim58387, align 8
%stackaddr$prim58388 = alloca %struct.ScmObj*, align 8
%argslist56792$_37foldr1470973 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47264, %struct.ScmObj* %argslist56792$_37foldr1470972)
store volatile %struct.ScmObj* %argslist56792$_37foldr1470973, %struct.ScmObj** %stackaddr$prim58388, align 8
%stackaddr$prim58389 = alloca %struct.ScmObj*, align 8
%argslist56792$_37foldr1470974 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48878, %struct.ScmObj* %argslist56792$_37foldr1470973)
store volatile %struct.ScmObj* %argslist56792$_37foldr1470974, %struct.ScmObj** %stackaddr$prim58389, align 8
%clofunc58390 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147097)
musttail call tailcc void %clofunc58390(%struct.ScmObj* %_37foldr147097, %struct.ScmObj* %argslist56792$_37foldr1470974)
ret void
}

define tailcc void @proc_clo$ae48878(%struct.ScmObj* %env$ae48878,%struct.ScmObj* %current_45args56748) {
%stackaddr$env-ref58391 = alloca %struct.ScmObj*, align 8
%lsts47182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48878, i64 0)
store %struct.ScmObj* %lsts47182, %struct.ScmObj** %stackaddr$env-ref58391
%stackaddr$env-ref58392 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48878, i64 1)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref58392
%stackaddr$env-ref58393 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48878, i64 2)
store %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$env-ref58393
%stackaddr$env-ref58394 = alloca %struct.ScmObj*, align 8
%f47184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48878, i64 3)
store %struct.ScmObj* %f47184, %struct.ScmObj** %stackaddr$env-ref58394
%stackaddr$env-ref58395 = alloca %struct.ScmObj*, align 8
%acc47183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48878, i64 4)
store %struct.ScmObj* %acc47183, %struct.ScmObj** %stackaddr$env-ref58395
%stackaddr$env-ref58396 = alloca %struct.ScmObj*, align 8
%_37foldl47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48878, i64 5)
store %struct.ScmObj* %_37foldl47180, %struct.ScmObj** %stackaddr$env-ref58396
%stackaddr$env-ref58397 = alloca %struct.ScmObj*, align 8
%_37map147128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48878, i64 6)
store %struct.ScmObj* %_37map147128, %struct.ScmObj** %stackaddr$env-ref58397
%stackaddr$prim58398 = alloca %struct.ScmObj*, align 8
%_95k47430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56748)
store volatile %struct.ScmObj* %_95k47430, %struct.ScmObj** %stackaddr$prim58398, align 8
%stackaddr$prim58399 = alloca %struct.ScmObj*, align 8
%current_45args56749 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56748)
store volatile %struct.ScmObj* %current_45args56749, %struct.ScmObj** %stackaddr$prim58399, align 8
%stackaddr$prim58400 = alloca %struct.ScmObj*, align 8
%anf_45bind47265 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56749)
store volatile %struct.ScmObj* %anf_45bind47265, %struct.ScmObj** %stackaddr$prim58400, align 8
%truthy$cmp58401 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47265)
%cmp$cmp58401 = icmp eq i64 %truthy$cmp58401, 1
br i1 %cmp$cmp58401, label %truebranch$cmp58401, label %falsebranch$cmp58401
truebranch$cmp58401:
%ae48889 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56751$k474280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58402 = alloca %struct.ScmObj*, align 8
%argslist56751$k474281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47183, %struct.ScmObj* %argslist56751$k474280)
store volatile %struct.ScmObj* %argslist56751$k474281, %struct.ScmObj** %stackaddr$prim58402, align 8
%stackaddr$prim58403 = alloca %struct.ScmObj*, align 8
%argslist56751$k474282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48889, %struct.ScmObj* %argslist56751$k474281)
store volatile %struct.ScmObj* %argslist56751$k474282, %struct.ScmObj** %stackaddr$prim58403, align 8
%clofunc58404 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47428)
musttail call tailcc void %clofunc58404(%struct.ScmObj* %k47428, %struct.ScmObj* %argslist56751$k474282)
ret void
falsebranch$cmp58401:
%stackaddr$makeclosure58405 = alloca %struct.ScmObj*, align 8
%fptrToInt58406 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48894 to i64
%ae48894 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58406)
store volatile %struct.ScmObj* %ae48894, %struct.ScmObj** %stackaddr$makeclosure58405, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48894, %struct.ScmObj* %lsts47182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48894, %struct.ScmObj* %_37foldr47102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48894, %struct.ScmObj* %k47428, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48894, %struct.ScmObj* %f47184, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48894, %struct.ScmObj* %acc47183, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48894, %struct.ScmObj* %_37foldl47180, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48894, %struct.ScmObj* %_37map147128, i64 6)
%ae48895 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58407 = alloca %struct.ScmObj*, align 8
%fptrToInt58408 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48896 to i64
%ae48896 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58408)
store volatile %struct.ScmObj* %ae48896, %struct.ScmObj** %stackaddr$makeclosure58407, align 8
%argslist56791$ae488940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58409 = alloca %struct.ScmObj*, align 8
%argslist56791$ae488941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48896, %struct.ScmObj* %argslist56791$ae488940)
store volatile %struct.ScmObj* %argslist56791$ae488941, %struct.ScmObj** %stackaddr$prim58409, align 8
%stackaddr$prim58410 = alloca %struct.ScmObj*, align 8
%argslist56791$ae488942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48895, %struct.ScmObj* %argslist56791$ae488941)
store volatile %struct.ScmObj* %argslist56791$ae488942, %struct.ScmObj** %stackaddr$prim58410, align 8
%clofunc58411 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48894)
musttail call tailcc void %clofunc58411(%struct.ScmObj* %ae48894, %struct.ScmObj* %argslist56791$ae488942)
ret void
}

define tailcc void @proc_clo$ae48894(%struct.ScmObj* %env$ae48894,%struct.ScmObj* %current_45args56752) {
%stackaddr$env-ref58412 = alloca %struct.ScmObj*, align 8
%lsts47182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48894, i64 0)
store %struct.ScmObj* %lsts47182, %struct.ScmObj** %stackaddr$env-ref58412
%stackaddr$env-ref58413 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48894, i64 1)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref58413
%stackaddr$env-ref58414 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48894, i64 2)
store %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$env-ref58414
%stackaddr$env-ref58415 = alloca %struct.ScmObj*, align 8
%f47184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48894, i64 3)
store %struct.ScmObj* %f47184, %struct.ScmObj** %stackaddr$env-ref58415
%stackaddr$env-ref58416 = alloca %struct.ScmObj*, align 8
%acc47183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48894, i64 4)
store %struct.ScmObj* %acc47183, %struct.ScmObj** %stackaddr$env-ref58416
%stackaddr$env-ref58417 = alloca %struct.ScmObj*, align 8
%_37foldl47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48894, i64 5)
store %struct.ScmObj* %_37foldl47180, %struct.ScmObj** %stackaddr$env-ref58417
%stackaddr$env-ref58418 = alloca %struct.ScmObj*, align 8
%_37map147128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48894, i64 6)
store %struct.ScmObj* %_37map147128, %struct.ScmObj** %stackaddr$env-ref58418
%stackaddr$prim58419 = alloca %struct.ScmObj*, align 8
%_95k47431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56752)
store volatile %struct.ScmObj* %_95k47431, %struct.ScmObj** %stackaddr$prim58419, align 8
%stackaddr$prim58420 = alloca %struct.ScmObj*, align 8
%current_45args56753 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56752)
store volatile %struct.ScmObj* %current_45args56753, %struct.ScmObj** %stackaddr$prim58420, align 8
%stackaddr$prim58421 = alloca %struct.ScmObj*, align 8
%anf_45bind47266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56753)
store volatile %struct.ScmObj* %anf_45bind47266, %struct.ScmObj** %stackaddr$prim58421, align 8
%stackaddr$makeclosure58422 = alloca %struct.ScmObj*, align 8
%fptrToInt58423 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48915 to i64
%ae48915 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58423)
store volatile %struct.ScmObj* %ae48915, %struct.ScmObj** %stackaddr$makeclosure58422, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48915, %struct.ScmObj* %lsts47182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48915, %struct.ScmObj* %_37foldr47102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48915, %struct.ScmObj* %k47428, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48915, %struct.ScmObj* %f47184, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48915, %struct.ScmObj* %acc47183, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48915, %struct.ScmObj* %_37foldl47180, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48915, %struct.ScmObj* %_37map147128, i64 6)
%argslist56786$_37map1471280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58424 = alloca %struct.ScmObj*, align 8
%argslist56786$_37map1471281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47182, %struct.ScmObj* %argslist56786$_37map1471280)
store volatile %struct.ScmObj* %argslist56786$_37map1471281, %struct.ScmObj** %stackaddr$prim58424, align 8
%stackaddr$prim58425 = alloca %struct.ScmObj*, align 8
%argslist56786$_37map1471282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47266, %struct.ScmObj* %argslist56786$_37map1471281)
store volatile %struct.ScmObj* %argslist56786$_37map1471282, %struct.ScmObj** %stackaddr$prim58425, align 8
%stackaddr$prim58426 = alloca %struct.ScmObj*, align 8
%argslist56786$_37map1471283 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48915, %struct.ScmObj* %argslist56786$_37map1471282)
store volatile %struct.ScmObj* %argslist56786$_37map1471283, %struct.ScmObj** %stackaddr$prim58426, align 8
%clofunc58427 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147128)
musttail call tailcc void %clofunc58427(%struct.ScmObj* %_37map147128, %struct.ScmObj* %argslist56786$_37map1471283)
ret void
}

define tailcc void @proc_clo$ae48915(%struct.ScmObj* %env$ae48915,%struct.ScmObj* %current_45args56755) {
%stackaddr$env-ref58428 = alloca %struct.ScmObj*, align 8
%lsts47182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48915, i64 0)
store %struct.ScmObj* %lsts47182, %struct.ScmObj** %stackaddr$env-ref58428
%stackaddr$env-ref58429 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48915, i64 1)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref58429
%stackaddr$env-ref58430 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48915, i64 2)
store %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$env-ref58430
%stackaddr$env-ref58431 = alloca %struct.ScmObj*, align 8
%f47184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48915, i64 3)
store %struct.ScmObj* %f47184, %struct.ScmObj** %stackaddr$env-ref58431
%stackaddr$env-ref58432 = alloca %struct.ScmObj*, align 8
%acc47183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48915, i64 4)
store %struct.ScmObj* %acc47183, %struct.ScmObj** %stackaddr$env-ref58432
%stackaddr$env-ref58433 = alloca %struct.ScmObj*, align 8
%_37foldl47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48915, i64 5)
store %struct.ScmObj* %_37foldl47180, %struct.ScmObj** %stackaddr$env-ref58433
%stackaddr$env-ref58434 = alloca %struct.ScmObj*, align 8
%_37map147128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48915, i64 6)
store %struct.ScmObj* %_37map147128, %struct.ScmObj** %stackaddr$env-ref58434
%stackaddr$prim58435 = alloca %struct.ScmObj*, align 8
%_95k47432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56755)
store volatile %struct.ScmObj* %_95k47432, %struct.ScmObj** %stackaddr$prim58435, align 8
%stackaddr$prim58436 = alloca %struct.ScmObj*, align 8
%current_45args56756 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56755)
store volatile %struct.ScmObj* %current_45args56756, %struct.ScmObj** %stackaddr$prim58436, align 8
%stackaddr$prim58437 = alloca %struct.ScmObj*, align 8
%lsts_4347189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56756)
store volatile %struct.ScmObj* %lsts_4347189, %struct.ScmObj** %stackaddr$prim58437, align 8
%stackaddr$makeclosure58438 = alloca %struct.ScmObj*, align 8
%fptrToInt58439 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48918 to i64
%ae48918 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt58439)
store volatile %struct.ScmObj* %ae48918, %struct.ScmObj** %stackaddr$makeclosure58438, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48918, %struct.ScmObj* %lsts47182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48918, %struct.ScmObj* %_37foldr47102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48918, %struct.ScmObj* %lsts_4347189, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48918, %struct.ScmObj* %k47428, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48918, %struct.ScmObj* %f47184, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48918, %struct.ScmObj* %acc47183, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48918, %struct.ScmObj* %_37foldl47180, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48918, %struct.ScmObj* %_37map147128, i64 7)
%ae48919 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58440 = alloca %struct.ScmObj*, align 8
%fptrToInt58441 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48920 to i64
%ae48920 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58441)
store volatile %struct.ScmObj* %ae48920, %struct.ScmObj** %stackaddr$makeclosure58440, align 8
%argslist56785$ae489180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58442 = alloca %struct.ScmObj*, align 8
%argslist56785$ae489181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48920, %struct.ScmObj* %argslist56785$ae489180)
store volatile %struct.ScmObj* %argslist56785$ae489181, %struct.ScmObj** %stackaddr$prim58442, align 8
%stackaddr$prim58443 = alloca %struct.ScmObj*, align 8
%argslist56785$ae489182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48919, %struct.ScmObj* %argslist56785$ae489181)
store volatile %struct.ScmObj* %argslist56785$ae489182, %struct.ScmObj** %stackaddr$prim58443, align 8
%clofunc58444 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48918)
musttail call tailcc void %clofunc58444(%struct.ScmObj* %ae48918, %struct.ScmObj* %argslist56785$ae489182)
ret void
}

define tailcc void @proc_clo$ae48918(%struct.ScmObj* %env$ae48918,%struct.ScmObj* %current_45args56758) {
%stackaddr$env-ref58445 = alloca %struct.ScmObj*, align 8
%lsts47182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48918, i64 0)
store %struct.ScmObj* %lsts47182, %struct.ScmObj** %stackaddr$env-ref58445
%stackaddr$env-ref58446 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48918, i64 1)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref58446
%stackaddr$env-ref58447 = alloca %struct.ScmObj*, align 8
%lsts_4347189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48918, i64 2)
store %struct.ScmObj* %lsts_4347189, %struct.ScmObj** %stackaddr$env-ref58447
%stackaddr$env-ref58448 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48918, i64 3)
store %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$env-ref58448
%stackaddr$env-ref58449 = alloca %struct.ScmObj*, align 8
%f47184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48918, i64 4)
store %struct.ScmObj* %f47184, %struct.ScmObj** %stackaddr$env-ref58449
%stackaddr$env-ref58450 = alloca %struct.ScmObj*, align 8
%acc47183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48918, i64 5)
store %struct.ScmObj* %acc47183, %struct.ScmObj** %stackaddr$env-ref58450
%stackaddr$env-ref58451 = alloca %struct.ScmObj*, align 8
%_37foldl47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48918, i64 6)
store %struct.ScmObj* %_37foldl47180, %struct.ScmObj** %stackaddr$env-ref58451
%stackaddr$env-ref58452 = alloca %struct.ScmObj*, align 8
%_37map147128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48918, i64 7)
store %struct.ScmObj* %_37map147128, %struct.ScmObj** %stackaddr$env-ref58452
%stackaddr$prim58453 = alloca %struct.ScmObj*, align 8
%_95k47433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56758)
store volatile %struct.ScmObj* %_95k47433, %struct.ScmObj** %stackaddr$prim58453, align 8
%stackaddr$prim58454 = alloca %struct.ScmObj*, align 8
%current_45args56759 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56758)
store volatile %struct.ScmObj* %current_45args56759, %struct.ScmObj** %stackaddr$prim58454, align 8
%stackaddr$prim58455 = alloca %struct.ScmObj*, align 8
%anf_45bind47267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56759)
store volatile %struct.ScmObj* %anf_45bind47267, %struct.ScmObj** %stackaddr$prim58455, align 8
%stackaddr$makeclosure58456 = alloca %struct.ScmObj*, align 8
%fptrToInt58457 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48939 to i64
%ae48939 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt58457)
store volatile %struct.ScmObj* %ae48939, %struct.ScmObj** %stackaddr$makeclosure58456, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48939, %struct.ScmObj* %lsts_4347189, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48939, %struct.ScmObj* %k47428, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48939, %struct.ScmObj* %f47184, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48939, %struct.ScmObj* %acc47183, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48939, %struct.ScmObj* %_37foldr47102, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48939, %struct.ScmObj* %_37foldl47180, i64 5)
%argslist56780$_37map1471280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58458 = alloca %struct.ScmObj*, align 8
%argslist56780$_37map1471281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47182, %struct.ScmObj* %argslist56780$_37map1471280)
store volatile %struct.ScmObj* %argslist56780$_37map1471281, %struct.ScmObj** %stackaddr$prim58458, align 8
%stackaddr$prim58459 = alloca %struct.ScmObj*, align 8
%argslist56780$_37map1471282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47267, %struct.ScmObj* %argslist56780$_37map1471281)
store volatile %struct.ScmObj* %argslist56780$_37map1471282, %struct.ScmObj** %stackaddr$prim58459, align 8
%stackaddr$prim58460 = alloca %struct.ScmObj*, align 8
%argslist56780$_37map1471283 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48939, %struct.ScmObj* %argslist56780$_37map1471282)
store volatile %struct.ScmObj* %argslist56780$_37map1471283, %struct.ScmObj** %stackaddr$prim58460, align 8
%clofunc58461 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147128)
musttail call tailcc void %clofunc58461(%struct.ScmObj* %_37map147128, %struct.ScmObj* %argslist56780$_37map1471283)
ret void
}

define tailcc void @proc_clo$ae48939(%struct.ScmObj* %env$ae48939,%struct.ScmObj* %current_45args56761) {
%stackaddr$env-ref58462 = alloca %struct.ScmObj*, align 8
%lsts_4347189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48939, i64 0)
store %struct.ScmObj* %lsts_4347189, %struct.ScmObj** %stackaddr$env-ref58462
%stackaddr$env-ref58463 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48939, i64 1)
store %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$env-ref58463
%stackaddr$env-ref58464 = alloca %struct.ScmObj*, align 8
%f47184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48939, i64 2)
store %struct.ScmObj* %f47184, %struct.ScmObj** %stackaddr$env-ref58464
%stackaddr$env-ref58465 = alloca %struct.ScmObj*, align 8
%acc47183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48939, i64 3)
store %struct.ScmObj* %acc47183, %struct.ScmObj** %stackaddr$env-ref58465
%stackaddr$env-ref58466 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48939, i64 4)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref58466
%stackaddr$env-ref58467 = alloca %struct.ScmObj*, align 8
%_37foldl47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48939, i64 5)
store %struct.ScmObj* %_37foldl47180, %struct.ScmObj** %stackaddr$env-ref58467
%stackaddr$prim58468 = alloca %struct.ScmObj*, align 8
%_95k47434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56761)
store volatile %struct.ScmObj* %_95k47434, %struct.ScmObj** %stackaddr$prim58468, align 8
%stackaddr$prim58469 = alloca %struct.ScmObj*, align 8
%current_45args56762 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56761)
store volatile %struct.ScmObj* %current_45args56762, %struct.ScmObj** %stackaddr$prim58469, align 8
%stackaddr$prim58470 = alloca %struct.ScmObj*, align 8
%vs47187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56762)
store volatile %struct.ScmObj* %vs47187, %struct.ScmObj** %stackaddr$prim58470, align 8
%stackaddr$makeclosure58471 = alloca %struct.ScmObj*, align 8
%fptrToInt58472 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48942 to i64
%ae48942 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58472)
store volatile %struct.ScmObj* %ae48942, %struct.ScmObj** %stackaddr$makeclosure58471, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48942, %struct.ScmObj* %lsts_4347189, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48942, %struct.ScmObj* %k47428, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48942, %struct.ScmObj* %vs47187, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48942, %struct.ScmObj* %f47184, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48942, %struct.ScmObj* %acc47183, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48942, %struct.ScmObj* %_37foldr47102, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48942, %struct.ScmObj* %_37foldl47180, i64 6)
%ae48943 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58473 = alloca %struct.ScmObj*, align 8
%fptrToInt58474 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48944 to i64
%ae48944 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58474)
store volatile %struct.ScmObj* %ae48944, %struct.ScmObj** %stackaddr$makeclosure58473, align 8
%argslist56779$ae489420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58475 = alloca %struct.ScmObj*, align 8
%argslist56779$ae489421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48944, %struct.ScmObj* %argslist56779$ae489420)
store volatile %struct.ScmObj* %argslist56779$ae489421, %struct.ScmObj** %stackaddr$prim58475, align 8
%stackaddr$prim58476 = alloca %struct.ScmObj*, align 8
%argslist56779$ae489422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48943, %struct.ScmObj* %argslist56779$ae489421)
store volatile %struct.ScmObj* %argslist56779$ae489422, %struct.ScmObj** %stackaddr$prim58476, align 8
%clofunc58477 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48942)
musttail call tailcc void %clofunc58477(%struct.ScmObj* %ae48942, %struct.ScmObj* %argslist56779$ae489422)
ret void
}

define tailcc void @proc_clo$ae48942(%struct.ScmObj* %env$ae48942,%struct.ScmObj* %current_45args56764) {
%stackaddr$env-ref58478 = alloca %struct.ScmObj*, align 8
%lsts_4347189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48942, i64 0)
store %struct.ScmObj* %lsts_4347189, %struct.ScmObj** %stackaddr$env-ref58478
%stackaddr$env-ref58479 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48942, i64 1)
store %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$env-ref58479
%stackaddr$env-ref58480 = alloca %struct.ScmObj*, align 8
%vs47187 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48942, i64 2)
store %struct.ScmObj* %vs47187, %struct.ScmObj** %stackaddr$env-ref58480
%stackaddr$env-ref58481 = alloca %struct.ScmObj*, align 8
%f47184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48942, i64 3)
store %struct.ScmObj* %f47184, %struct.ScmObj** %stackaddr$env-ref58481
%stackaddr$env-ref58482 = alloca %struct.ScmObj*, align 8
%acc47183 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48942, i64 4)
store %struct.ScmObj* %acc47183, %struct.ScmObj** %stackaddr$env-ref58482
%stackaddr$env-ref58483 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48942, i64 5)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref58483
%stackaddr$env-ref58484 = alloca %struct.ScmObj*, align 8
%_37foldl47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48942, i64 6)
store %struct.ScmObj* %_37foldl47180, %struct.ScmObj** %stackaddr$env-ref58484
%stackaddr$prim58485 = alloca %struct.ScmObj*, align 8
%_95k47435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56764)
store volatile %struct.ScmObj* %_95k47435, %struct.ScmObj** %stackaddr$prim58485, align 8
%stackaddr$prim58486 = alloca %struct.ScmObj*, align 8
%current_45args56765 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56764)
store volatile %struct.ScmObj* %current_45args56765, %struct.ScmObj** %stackaddr$prim58486, align 8
%stackaddr$prim58487 = alloca %struct.ScmObj*, align 8
%anf_45bind47268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56765)
store volatile %struct.ScmObj* %anf_45bind47268, %struct.ScmObj** %stackaddr$prim58487, align 8
%ae48965 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58488 = alloca %struct.ScmObj*, align 8
%anf_45bind47269 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47183, %struct.ScmObj* %ae48965)
store volatile %struct.ScmObj* %anf_45bind47269, %struct.ScmObj** %stackaddr$prim58488, align 8
%stackaddr$makeclosure58489 = alloca %struct.ScmObj*, align 8
%fptrToInt58490 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48967 to i64
%ae48967 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58490)
store volatile %struct.ScmObj* %ae48967, %struct.ScmObj** %stackaddr$makeclosure58489, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48967, %struct.ScmObj* %lsts_4347189, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48967, %struct.ScmObj* %k47428, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48967, %struct.ScmObj* %f47184, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48967, %struct.ScmObj* %_37foldl47180, i64 3)
%argslist56773$_37foldr471020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58491 = alloca %struct.ScmObj*, align 8
%argslist56773$_37foldr471021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47187, %struct.ScmObj* %argslist56773$_37foldr471020)
store volatile %struct.ScmObj* %argslist56773$_37foldr471021, %struct.ScmObj** %stackaddr$prim58491, align 8
%stackaddr$prim58492 = alloca %struct.ScmObj*, align 8
%argslist56773$_37foldr471022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47269, %struct.ScmObj* %argslist56773$_37foldr471021)
store volatile %struct.ScmObj* %argslist56773$_37foldr471022, %struct.ScmObj** %stackaddr$prim58492, align 8
%stackaddr$prim58493 = alloca %struct.ScmObj*, align 8
%argslist56773$_37foldr471023 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47268, %struct.ScmObj* %argslist56773$_37foldr471022)
store volatile %struct.ScmObj* %argslist56773$_37foldr471023, %struct.ScmObj** %stackaddr$prim58493, align 8
%stackaddr$prim58494 = alloca %struct.ScmObj*, align 8
%argslist56773$_37foldr471024 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48967, %struct.ScmObj* %argslist56773$_37foldr471023)
store volatile %struct.ScmObj* %argslist56773$_37foldr471024, %struct.ScmObj** %stackaddr$prim58494, align 8
%clofunc58495 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47102)
musttail call tailcc void %clofunc58495(%struct.ScmObj* %_37foldr47102, %struct.ScmObj* %argslist56773$_37foldr471024)
ret void
}

define tailcc void @proc_clo$ae48967(%struct.ScmObj* %env$ae48967,%struct.ScmObj* %current_45args56767) {
%stackaddr$env-ref58496 = alloca %struct.ScmObj*, align 8
%lsts_4347189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48967, i64 0)
store %struct.ScmObj* %lsts_4347189, %struct.ScmObj** %stackaddr$env-ref58496
%stackaddr$env-ref58497 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48967, i64 1)
store %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$env-ref58497
%stackaddr$env-ref58498 = alloca %struct.ScmObj*, align 8
%f47184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48967, i64 2)
store %struct.ScmObj* %f47184, %struct.ScmObj** %stackaddr$env-ref58498
%stackaddr$env-ref58499 = alloca %struct.ScmObj*, align 8
%_37foldl47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48967, i64 3)
store %struct.ScmObj* %_37foldl47180, %struct.ScmObj** %stackaddr$env-ref58499
%stackaddr$prim58500 = alloca %struct.ScmObj*, align 8
%_95k47436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56767)
store volatile %struct.ScmObj* %_95k47436, %struct.ScmObj** %stackaddr$prim58500, align 8
%stackaddr$prim58501 = alloca %struct.ScmObj*, align 8
%current_45args56768 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56767)
store volatile %struct.ScmObj* %current_45args56768, %struct.ScmObj** %stackaddr$prim58501, align 8
%stackaddr$prim58502 = alloca %struct.ScmObj*, align 8
%anf_45bind47270 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56768)
store volatile %struct.ScmObj* %anf_45bind47270, %struct.ScmObj** %stackaddr$prim58502, align 8
%stackaddr$makeclosure58503 = alloca %struct.ScmObj*, align 8
%fptrToInt58504 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48971 to i64
%ae48971 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58504)
store volatile %struct.ScmObj* %ae48971, %struct.ScmObj** %stackaddr$makeclosure58503, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48971, %struct.ScmObj* %lsts_4347189, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48971, %struct.ScmObj* %k47428, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48971, %struct.ScmObj* %f47184, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48971, %struct.ScmObj* %_37foldl47180, i64 3)
%stackaddr$prim58505 = alloca %struct.ScmObj*, align 8
%cpsargs47439 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48971, %struct.ScmObj* %anf_45bind47270)
store volatile %struct.ScmObj* %cpsargs47439, %struct.ScmObj** %stackaddr$prim58505, align 8
%clofunc58506 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47184)
musttail call tailcc void %clofunc58506(%struct.ScmObj* %f47184, %struct.ScmObj* %cpsargs47439)
ret void
}

define tailcc void @proc_clo$ae48971(%struct.ScmObj* %env$ae48971,%struct.ScmObj* %current_45args56770) {
%stackaddr$env-ref58507 = alloca %struct.ScmObj*, align 8
%lsts_4347189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48971, i64 0)
store %struct.ScmObj* %lsts_4347189, %struct.ScmObj** %stackaddr$env-ref58507
%stackaddr$env-ref58508 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48971, i64 1)
store %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$env-ref58508
%stackaddr$env-ref58509 = alloca %struct.ScmObj*, align 8
%f47184 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48971, i64 2)
store %struct.ScmObj* %f47184, %struct.ScmObj** %stackaddr$env-ref58509
%stackaddr$env-ref58510 = alloca %struct.ScmObj*, align 8
%_37foldl47180 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48971, i64 3)
store %struct.ScmObj* %_37foldl47180, %struct.ScmObj** %stackaddr$env-ref58510
%stackaddr$prim58511 = alloca %struct.ScmObj*, align 8
%_95k47437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56770)
store volatile %struct.ScmObj* %_95k47437, %struct.ScmObj** %stackaddr$prim58511, align 8
%stackaddr$prim58512 = alloca %struct.ScmObj*, align 8
%current_45args56771 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56770)
store volatile %struct.ScmObj* %current_45args56771, %struct.ScmObj** %stackaddr$prim58512, align 8
%stackaddr$prim58513 = alloca %struct.ScmObj*, align 8
%acc_4347191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56771)
store volatile %struct.ScmObj* %acc_4347191, %struct.ScmObj** %stackaddr$prim58513, align 8
%stackaddr$prim58514 = alloca %struct.ScmObj*, align 8
%anf_45bind47271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4347191, %struct.ScmObj* %lsts_4347189)
store volatile %struct.ScmObj* %anf_45bind47271, %struct.ScmObj** %stackaddr$prim58514, align 8
%stackaddr$prim58515 = alloca %struct.ScmObj*, align 8
%anf_45bind47272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47184, %struct.ScmObj* %anf_45bind47271)
store volatile %struct.ScmObj* %anf_45bind47272, %struct.ScmObj** %stackaddr$prim58515, align 8
%stackaddr$prim58516 = alloca %struct.ScmObj*, align 8
%cpsargs47438 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47428, %struct.ScmObj* %anf_45bind47272)
store volatile %struct.ScmObj* %cpsargs47438, %struct.ScmObj** %stackaddr$prim58516, align 8
%clofunc58517 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl47180)
musttail call tailcc void %clofunc58517(%struct.ScmObj* %_37foldl47180, %struct.ScmObj* %cpsargs47438)
ret void
}

define tailcc void @proc_clo$ae48944(%struct.ScmObj* %env$ae48944,%struct.ScmObj* %current_45args56774) {
%stackaddr$prim58518 = alloca %struct.ScmObj*, align 8
%k47440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56774)
store volatile %struct.ScmObj* %k47440, %struct.ScmObj** %stackaddr$prim58518, align 8
%stackaddr$prim58519 = alloca %struct.ScmObj*, align 8
%current_45args56775 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56774)
store volatile %struct.ScmObj* %current_45args56775, %struct.ScmObj** %stackaddr$prim58519, align 8
%stackaddr$prim58520 = alloca %struct.ScmObj*, align 8
%a47193 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56775)
store volatile %struct.ScmObj* %a47193, %struct.ScmObj** %stackaddr$prim58520, align 8
%stackaddr$prim58521 = alloca %struct.ScmObj*, align 8
%current_45args56776 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56775)
store volatile %struct.ScmObj* %current_45args56776, %struct.ScmObj** %stackaddr$prim58521, align 8
%stackaddr$prim58522 = alloca %struct.ScmObj*, align 8
%b47192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56776)
store volatile %struct.ScmObj* %b47192, %struct.ScmObj** %stackaddr$prim58522, align 8
%stackaddr$prim58523 = alloca %struct.ScmObj*, align 8
%cpsprim47441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47193, %struct.ScmObj* %b47192)
store volatile %struct.ScmObj* %cpsprim47441, %struct.ScmObj** %stackaddr$prim58523, align 8
%ae48948 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56778$k474400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58524 = alloca %struct.ScmObj*, align 8
%argslist56778$k474401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47441, %struct.ScmObj* %argslist56778$k474400)
store volatile %struct.ScmObj* %argslist56778$k474401, %struct.ScmObj** %stackaddr$prim58524, align 8
%stackaddr$prim58525 = alloca %struct.ScmObj*, align 8
%argslist56778$k474402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48948, %struct.ScmObj* %argslist56778$k474401)
store volatile %struct.ScmObj* %argslist56778$k474402, %struct.ScmObj** %stackaddr$prim58525, align 8
%clofunc58526 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47440)
musttail call tailcc void %clofunc58526(%struct.ScmObj* %k47440, %struct.ScmObj* %argslist56778$k474402)
ret void
}

define tailcc void @proc_clo$ae48920(%struct.ScmObj* %env$ae48920,%struct.ScmObj* %current_45args56781) {
%stackaddr$prim58527 = alloca %struct.ScmObj*, align 8
%k47442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56781)
store volatile %struct.ScmObj* %k47442, %struct.ScmObj** %stackaddr$prim58527, align 8
%stackaddr$prim58528 = alloca %struct.ScmObj*, align 8
%current_45args56782 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56781)
store volatile %struct.ScmObj* %current_45args56782, %struct.ScmObj** %stackaddr$prim58528, align 8
%stackaddr$prim58529 = alloca %struct.ScmObj*, align 8
%x47188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56782)
store volatile %struct.ScmObj* %x47188, %struct.ScmObj** %stackaddr$prim58529, align 8
%stackaddr$prim58530 = alloca %struct.ScmObj*, align 8
%cpsprim47443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47188)
store volatile %struct.ScmObj* %cpsprim47443, %struct.ScmObj** %stackaddr$prim58530, align 8
%ae48923 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56784$k474420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58531 = alloca %struct.ScmObj*, align 8
%argslist56784$k474421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47443, %struct.ScmObj* %argslist56784$k474420)
store volatile %struct.ScmObj* %argslist56784$k474421, %struct.ScmObj** %stackaddr$prim58531, align 8
%stackaddr$prim58532 = alloca %struct.ScmObj*, align 8
%argslist56784$k474422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48923, %struct.ScmObj* %argslist56784$k474421)
store volatile %struct.ScmObj* %argslist56784$k474422, %struct.ScmObj** %stackaddr$prim58532, align 8
%clofunc58533 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47442)
musttail call tailcc void %clofunc58533(%struct.ScmObj* %k47442, %struct.ScmObj* %argslist56784$k474422)
ret void
}

define tailcc void @proc_clo$ae48896(%struct.ScmObj* %env$ae48896,%struct.ScmObj* %current_45args56787) {
%stackaddr$prim58534 = alloca %struct.ScmObj*, align 8
%k47444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56787)
store volatile %struct.ScmObj* %k47444, %struct.ScmObj** %stackaddr$prim58534, align 8
%stackaddr$prim58535 = alloca %struct.ScmObj*, align 8
%current_45args56788 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56787)
store volatile %struct.ScmObj* %current_45args56788, %struct.ScmObj** %stackaddr$prim58535, align 8
%stackaddr$prim58536 = alloca %struct.ScmObj*, align 8
%x47190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56788)
store volatile %struct.ScmObj* %x47190, %struct.ScmObj** %stackaddr$prim58536, align 8
%stackaddr$prim58537 = alloca %struct.ScmObj*, align 8
%cpsprim47445 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47190)
store volatile %struct.ScmObj* %cpsprim47445, %struct.ScmObj** %stackaddr$prim58537, align 8
%ae48899 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56790$k474440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58538 = alloca %struct.ScmObj*, align 8
%argslist56790$k474441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47445, %struct.ScmObj* %argslist56790$k474440)
store volatile %struct.ScmObj* %argslist56790$k474441, %struct.ScmObj** %stackaddr$prim58538, align 8
%stackaddr$prim58539 = alloca %struct.ScmObj*, align 8
%argslist56790$k474442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48899, %struct.ScmObj* %argslist56790$k474441)
store volatile %struct.ScmObj* %argslist56790$k474442, %struct.ScmObj** %stackaddr$prim58539, align 8
%clofunc58540 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47444)
musttail call tailcc void %clofunc58540(%struct.ScmObj* %k47444, %struct.ScmObj* %argslist56790$k474442)
ret void
}

define tailcc void @proc_clo$ae48848(%struct.ScmObj* %env$ae48848,%struct.ScmObj* %current_45args56793) {
%stackaddr$prim58541 = alloca %struct.ScmObj*, align 8
%k47446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56793)
store volatile %struct.ScmObj* %k47446, %struct.ScmObj** %stackaddr$prim58541, align 8
%stackaddr$prim58542 = alloca %struct.ScmObj*, align 8
%current_45args56794 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56793)
store volatile %struct.ScmObj* %current_45args56794, %struct.ScmObj** %stackaddr$prim58542, align 8
%stackaddr$prim58543 = alloca %struct.ScmObj*, align 8
%lst47186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56794)
store volatile %struct.ScmObj* %lst47186, %struct.ScmObj** %stackaddr$prim58543, align 8
%stackaddr$prim58544 = alloca %struct.ScmObj*, align 8
%current_45args56795 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56794)
store volatile %struct.ScmObj* %current_45args56795, %struct.ScmObj** %stackaddr$prim58544, align 8
%stackaddr$prim58545 = alloca %struct.ScmObj*, align 8
%b47185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56795)
store volatile %struct.ScmObj* %b47185, %struct.ScmObj** %stackaddr$prim58545, align 8
%truthy$cmp58546 = call i64 @is_truthy_value(%struct.ScmObj* %b47185)
%cmp$cmp58546 = icmp eq i64 %truthy$cmp58546, 1
br i1 %cmp$cmp58546, label %truebranch$cmp58546, label %falsebranch$cmp58546
truebranch$cmp58546:
%ae48851 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56797$k474460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58547 = alloca %struct.ScmObj*, align 8
%argslist56797$k474461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47185, %struct.ScmObj* %argslist56797$k474460)
store volatile %struct.ScmObj* %argslist56797$k474461, %struct.ScmObj** %stackaddr$prim58547, align 8
%stackaddr$prim58548 = alloca %struct.ScmObj*, align 8
%argslist56797$k474462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48851, %struct.ScmObj* %argslist56797$k474461)
store volatile %struct.ScmObj* %argslist56797$k474462, %struct.ScmObj** %stackaddr$prim58548, align 8
%clofunc58549 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47446)
musttail call tailcc void %clofunc58549(%struct.ScmObj* %k47446, %struct.ScmObj* %argslist56797$k474462)
ret void
falsebranch$cmp58546:
%stackaddr$prim58550 = alloca %struct.ScmObj*, align 8
%cpsprim47447 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47186)
store volatile %struct.ScmObj* %cpsprim47447, %struct.ScmObj** %stackaddr$prim58550, align 8
%ae48858 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56798$k474460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58551 = alloca %struct.ScmObj*, align 8
%argslist56798$k474461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47447, %struct.ScmObj* %argslist56798$k474460)
store volatile %struct.ScmObj* %argslist56798$k474461, %struct.ScmObj** %stackaddr$prim58551, align 8
%stackaddr$prim58552 = alloca %struct.ScmObj*, align 8
%argslist56798$k474462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48858, %struct.ScmObj* %argslist56798$k474461)
store volatile %struct.ScmObj* %argslist56798$k474462, %struct.ScmObj** %stackaddr$prim58552, align 8
%clofunc58553 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47446)
musttail call tailcc void %clofunc58553(%struct.ScmObj* %k47446, %struct.ScmObj* %argslist56798$k474462)
ret void
}

define tailcc void @proc_clo$ae48689(%struct.ScmObj* %env$ae48689,%struct.ScmObj* %args4712447448) {
%stackaddr$env-ref58554 = alloca %struct.ScmObj*, align 8
%_37last47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48689, i64 0)
store %struct.ScmObj* %_37last47119, %struct.ScmObj** %stackaddr$env-ref58554
%stackaddr$env-ref58555 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48689, i64 1)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref58555
%stackaddr$env-ref58556 = alloca %struct.ScmObj*, align 8
%_37drop_45right47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48689, i64 2)
store %struct.ScmObj* %_37drop_45right47116, %struct.ScmObj** %stackaddr$env-ref58556
%stackaddr$prim58557 = alloca %struct.ScmObj*, align 8
%k47449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4712447448)
store volatile %struct.ScmObj* %k47449, %struct.ScmObj** %stackaddr$prim58557, align 8
%stackaddr$prim58558 = alloca %struct.ScmObj*, align 8
%args47124 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4712447448)
store volatile %struct.ScmObj* %args47124, %struct.ScmObj** %stackaddr$prim58558, align 8
%stackaddr$prim58559 = alloca %struct.ScmObj*, align 8
%f47126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47124)
store volatile %struct.ScmObj* %f47126, %struct.ScmObj** %stackaddr$prim58559, align 8
%stackaddr$prim58560 = alloca %struct.ScmObj*, align 8
%lsts47125 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47124)
store volatile %struct.ScmObj* %lsts47125, %struct.ScmObj** %stackaddr$prim58560, align 8
%stackaddr$makeclosure58561 = alloca %struct.ScmObj*, align 8
%fptrToInt58562 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48694 to i64
%ae48694 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58562)
store volatile %struct.ScmObj* %ae48694, %struct.ScmObj** %stackaddr$makeclosure58561, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48694, %struct.ScmObj* %lsts47125, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48694, %struct.ScmObj* %_37foldr47102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48694, %struct.ScmObj* %k47449, i64 2)
%ae48695 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58563 = alloca %struct.ScmObj*, align 8
%fptrToInt58564 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48696 to i64
%ae48696 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58564)
store volatile %struct.ScmObj* %ae48696, %struct.ScmObj** %stackaddr$makeclosure58563, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48696, %struct.ScmObj* %f47126, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48696, %struct.ScmObj* %_37last47119, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48696, %struct.ScmObj* %_37drop_45right47116, i64 2)
%argslist56817$ae486940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58565 = alloca %struct.ScmObj*, align 8
%argslist56817$ae486941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48696, %struct.ScmObj* %argslist56817$ae486940)
store volatile %struct.ScmObj* %argslist56817$ae486941, %struct.ScmObj** %stackaddr$prim58565, align 8
%stackaddr$prim58566 = alloca %struct.ScmObj*, align 8
%argslist56817$ae486942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48695, %struct.ScmObj* %argslist56817$ae486941)
store volatile %struct.ScmObj* %argslist56817$ae486942, %struct.ScmObj** %stackaddr$prim58566, align 8
%clofunc58567 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48694)
musttail call tailcc void %clofunc58567(%struct.ScmObj* %ae48694, %struct.ScmObj* %argslist56817$ae486942)
ret void
}

define tailcc void @proc_clo$ae48694(%struct.ScmObj* %env$ae48694,%struct.ScmObj* %current_45args56802) {
%stackaddr$env-ref58568 = alloca %struct.ScmObj*, align 8
%lsts47125 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48694, i64 0)
store %struct.ScmObj* %lsts47125, %struct.ScmObj** %stackaddr$env-ref58568
%stackaddr$env-ref58569 = alloca %struct.ScmObj*, align 8
%_37foldr47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48694, i64 1)
store %struct.ScmObj* %_37foldr47102, %struct.ScmObj** %stackaddr$env-ref58569
%stackaddr$env-ref58570 = alloca %struct.ScmObj*, align 8
%k47449 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48694, i64 2)
store %struct.ScmObj* %k47449, %struct.ScmObj** %stackaddr$env-ref58570
%stackaddr$prim58571 = alloca %struct.ScmObj*, align 8
%_95k47450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56802)
store volatile %struct.ScmObj* %_95k47450, %struct.ScmObj** %stackaddr$prim58571, align 8
%stackaddr$prim58572 = alloca %struct.ScmObj*, align 8
%current_45args56803 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56802)
store volatile %struct.ScmObj* %current_45args56803, %struct.ScmObj** %stackaddr$prim58572, align 8
%stackaddr$prim58573 = alloca %struct.ScmObj*, align 8
%anf_45bind47259 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56803)
store volatile %struct.ScmObj* %anf_45bind47259, %struct.ScmObj** %stackaddr$prim58573, align 8
%ae48757 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58574 = alloca %struct.ScmObj*, align 8
%anf_45bind47260 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48757, %struct.ScmObj* %lsts47125)
store volatile %struct.ScmObj* %anf_45bind47260, %struct.ScmObj** %stackaddr$prim58574, align 8
%stackaddr$prim58575 = alloca %struct.ScmObj*, align 8
%anf_45bind47261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47259, %struct.ScmObj* %anf_45bind47260)
store volatile %struct.ScmObj* %anf_45bind47261, %struct.ScmObj** %stackaddr$prim58575, align 8
%stackaddr$prim58576 = alloca %struct.ScmObj*, align 8
%cpsargs47451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47449, %struct.ScmObj* %anf_45bind47261)
store volatile %struct.ScmObj* %cpsargs47451, %struct.ScmObj** %stackaddr$prim58576, align 8
%clofunc58577 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47102)
musttail call tailcc void %clofunc58577(%struct.ScmObj* %_37foldr47102, %struct.ScmObj* %cpsargs47451)
ret void
}

define tailcc void @proc_clo$ae48696(%struct.ScmObj* %env$ae48696,%struct.ScmObj* %fargs4712747452) {
%stackaddr$env-ref58578 = alloca %struct.ScmObj*, align 8
%f47126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48696, i64 0)
store %struct.ScmObj* %f47126, %struct.ScmObj** %stackaddr$env-ref58578
%stackaddr$env-ref58579 = alloca %struct.ScmObj*, align 8
%_37last47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48696, i64 1)
store %struct.ScmObj* %_37last47119, %struct.ScmObj** %stackaddr$env-ref58579
%stackaddr$env-ref58580 = alloca %struct.ScmObj*, align 8
%_37drop_45right47116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48696, i64 2)
store %struct.ScmObj* %_37drop_45right47116, %struct.ScmObj** %stackaddr$env-ref58580
%stackaddr$prim58581 = alloca %struct.ScmObj*, align 8
%k47453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4712747452)
store volatile %struct.ScmObj* %k47453, %struct.ScmObj** %stackaddr$prim58581, align 8
%stackaddr$prim58582 = alloca %struct.ScmObj*, align 8
%fargs47127 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4712747452)
store volatile %struct.ScmObj* %fargs47127, %struct.ScmObj** %stackaddr$prim58582, align 8
%stackaddr$makeclosure58583 = alloca %struct.ScmObj*, align 8
%fptrToInt58584 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48700 to i64
%ae48700 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58584)
store volatile %struct.ScmObj* %ae48700, %struct.ScmObj** %stackaddr$makeclosure58583, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48700, %struct.ScmObj* %f47126, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48700, %struct.ScmObj* %_37last47119, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48700, %struct.ScmObj* %k47453, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48700, %struct.ScmObj* %fargs47127, i64 3)
%ae48702 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist56816$_37drop_45right471160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58585 = alloca %struct.ScmObj*, align 8
%argslist56816$_37drop_45right471161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48702, %struct.ScmObj* %argslist56816$_37drop_45right471160)
store volatile %struct.ScmObj* %argslist56816$_37drop_45right471161, %struct.ScmObj** %stackaddr$prim58585, align 8
%stackaddr$prim58586 = alloca %struct.ScmObj*, align 8
%argslist56816$_37drop_45right471162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47127, %struct.ScmObj* %argslist56816$_37drop_45right471161)
store volatile %struct.ScmObj* %argslist56816$_37drop_45right471162, %struct.ScmObj** %stackaddr$prim58586, align 8
%stackaddr$prim58587 = alloca %struct.ScmObj*, align 8
%argslist56816$_37drop_45right471163 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48700, %struct.ScmObj* %argslist56816$_37drop_45right471162)
store volatile %struct.ScmObj* %argslist56816$_37drop_45right471163, %struct.ScmObj** %stackaddr$prim58587, align 8
%clofunc58588 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right47116)
musttail call tailcc void %clofunc58588(%struct.ScmObj* %_37drop_45right47116, %struct.ScmObj* %argslist56816$_37drop_45right471163)
ret void
}

define tailcc void @proc_clo$ae48700(%struct.ScmObj* %env$ae48700,%struct.ScmObj* %current_45args56805) {
%stackaddr$env-ref58589 = alloca %struct.ScmObj*, align 8
%f47126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48700, i64 0)
store %struct.ScmObj* %f47126, %struct.ScmObj** %stackaddr$env-ref58589
%stackaddr$env-ref58590 = alloca %struct.ScmObj*, align 8
%_37last47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48700, i64 1)
store %struct.ScmObj* %_37last47119, %struct.ScmObj** %stackaddr$env-ref58590
%stackaddr$env-ref58591 = alloca %struct.ScmObj*, align 8
%k47453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48700, i64 2)
store %struct.ScmObj* %k47453, %struct.ScmObj** %stackaddr$env-ref58591
%stackaddr$env-ref58592 = alloca %struct.ScmObj*, align 8
%fargs47127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48700, i64 3)
store %struct.ScmObj* %fargs47127, %struct.ScmObj** %stackaddr$env-ref58592
%stackaddr$prim58593 = alloca %struct.ScmObj*, align 8
%_95k47454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56805)
store volatile %struct.ScmObj* %_95k47454, %struct.ScmObj** %stackaddr$prim58593, align 8
%stackaddr$prim58594 = alloca %struct.ScmObj*, align 8
%current_45args56806 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56805)
store volatile %struct.ScmObj* %current_45args56806, %struct.ScmObj** %stackaddr$prim58594, align 8
%stackaddr$prim58595 = alloca %struct.ScmObj*, align 8
%anf_45bind47256 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56806)
store volatile %struct.ScmObj* %anf_45bind47256, %struct.ScmObj** %stackaddr$prim58595, align 8
%stackaddr$makeclosure58596 = alloca %struct.ScmObj*, align 8
%fptrToInt58597 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48707 to i64
%ae48707 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58597)
store volatile %struct.ScmObj* %ae48707, %struct.ScmObj** %stackaddr$makeclosure58596, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48707, %struct.ScmObj* %_37last47119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48707, %struct.ScmObj* %k47453, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48707, %struct.ScmObj* %fargs47127, i64 2)
%stackaddr$prim58598 = alloca %struct.ScmObj*, align 8
%cpsargs47458 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48707, %struct.ScmObj* %anf_45bind47256)
store volatile %struct.ScmObj* %cpsargs47458, %struct.ScmObj** %stackaddr$prim58598, align 8
%clofunc58599 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47126)
musttail call tailcc void %clofunc58599(%struct.ScmObj* %f47126, %struct.ScmObj* %cpsargs47458)
ret void
}

define tailcc void @proc_clo$ae48707(%struct.ScmObj* %env$ae48707,%struct.ScmObj* %current_45args56808) {
%stackaddr$env-ref58600 = alloca %struct.ScmObj*, align 8
%_37last47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48707, i64 0)
store %struct.ScmObj* %_37last47119, %struct.ScmObj** %stackaddr$env-ref58600
%stackaddr$env-ref58601 = alloca %struct.ScmObj*, align 8
%k47453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48707, i64 1)
store %struct.ScmObj* %k47453, %struct.ScmObj** %stackaddr$env-ref58601
%stackaddr$env-ref58602 = alloca %struct.ScmObj*, align 8
%fargs47127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48707, i64 2)
store %struct.ScmObj* %fargs47127, %struct.ScmObj** %stackaddr$env-ref58602
%stackaddr$prim58603 = alloca %struct.ScmObj*, align 8
%_95k47455 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56808)
store volatile %struct.ScmObj* %_95k47455, %struct.ScmObj** %stackaddr$prim58603, align 8
%stackaddr$prim58604 = alloca %struct.ScmObj*, align 8
%current_45args56809 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56808)
store volatile %struct.ScmObj* %current_45args56809, %struct.ScmObj** %stackaddr$prim58604, align 8
%stackaddr$prim58605 = alloca %struct.ScmObj*, align 8
%anf_45bind47257 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56809)
store volatile %struct.ScmObj* %anf_45bind47257, %struct.ScmObj** %stackaddr$prim58605, align 8
%stackaddr$makeclosure58606 = alloca %struct.ScmObj*, align 8
%fptrToInt58607 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48712 to i64
%ae48712 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58607)
store volatile %struct.ScmObj* %ae48712, %struct.ScmObj** %stackaddr$makeclosure58606, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48712, %struct.ScmObj* %k47453, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48712, %struct.ScmObj* %anf_45bind47257, i64 1)
%argslist56815$_37last471190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58608 = alloca %struct.ScmObj*, align 8
%argslist56815$_37last471191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47127, %struct.ScmObj* %argslist56815$_37last471190)
store volatile %struct.ScmObj* %argslist56815$_37last471191, %struct.ScmObj** %stackaddr$prim58608, align 8
%stackaddr$prim58609 = alloca %struct.ScmObj*, align 8
%argslist56815$_37last471192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48712, %struct.ScmObj* %argslist56815$_37last471191)
store volatile %struct.ScmObj* %argslist56815$_37last471192, %struct.ScmObj** %stackaddr$prim58609, align 8
%clofunc58610 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last47119)
musttail call tailcc void %clofunc58610(%struct.ScmObj* %_37last47119, %struct.ScmObj* %argslist56815$_37last471192)
ret void
}

define tailcc void @proc_clo$ae48712(%struct.ScmObj* %env$ae48712,%struct.ScmObj* %current_45args56811) {
%stackaddr$env-ref58611 = alloca %struct.ScmObj*, align 8
%k47453 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48712, i64 0)
store %struct.ScmObj* %k47453, %struct.ScmObj** %stackaddr$env-ref58611
%stackaddr$env-ref58612 = alloca %struct.ScmObj*, align 8
%anf_45bind47257 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48712, i64 1)
store %struct.ScmObj* %anf_45bind47257, %struct.ScmObj** %stackaddr$env-ref58612
%stackaddr$prim58613 = alloca %struct.ScmObj*, align 8
%_95k47456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56811)
store volatile %struct.ScmObj* %_95k47456, %struct.ScmObj** %stackaddr$prim58613, align 8
%stackaddr$prim58614 = alloca %struct.ScmObj*, align 8
%current_45args56812 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56811)
store volatile %struct.ScmObj* %current_45args56812, %struct.ScmObj** %stackaddr$prim58614, align 8
%stackaddr$prim58615 = alloca %struct.ScmObj*, align 8
%anf_45bind47258 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56812)
store volatile %struct.ScmObj* %anf_45bind47258, %struct.ScmObj** %stackaddr$prim58615, align 8
%stackaddr$prim58616 = alloca %struct.ScmObj*, align 8
%cpsprim47457 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47257, %struct.ScmObj* %anf_45bind47258)
store volatile %struct.ScmObj* %cpsprim47457, %struct.ScmObj** %stackaddr$prim58616, align 8
%ae48717 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56814$k474530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58617 = alloca %struct.ScmObj*, align 8
%argslist56814$k474531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47457, %struct.ScmObj* %argslist56814$k474530)
store volatile %struct.ScmObj* %argslist56814$k474531, %struct.ScmObj** %stackaddr$prim58617, align 8
%stackaddr$prim58618 = alloca %struct.ScmObj*, align 8
%argslist56814$k474532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48717, %struct.ScmObj* %argslist56814$k474531)
store volatile %struct.ScmObj* %argslist56814$k474532, %struct.ScmObj** %stackaddr$prim58618, align 8
%clofunc58619 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47453)
musttail call tailcc void %clofunc58619(%struct.ScmObj* %k47453, %struct.ScmObj* %argslist56814$k474532)
ret void
}

define tailcc void @proc_clo$ae48612(%struct.ScmObj* %env$ae48612,%struct.ScmObj* %current_45args56819) {
%stackaddr$env-ref58620 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48612, i64 0)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref58620
%stackaddr$prim58621 = alloca %struct.ScmObj*, align 8
%k47459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56819)
store volatile %struct.ScmObj* %k47459, %struct.ScmObj** %stackaddr$prim58621, align 8
%stackaddr$prim58622 = alloca %struct.ScmObj*, align 8
%current_45args56820 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56819)
store volatile %struct.ScmObj* %current_45args56820, %struct.ScmObj** %stackaddr$prim58622, align 8
%stackaddr$prim58623 = alloca %struct.ScmObj*, align 8
%f47130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56820)
store volatile %struct.ScmObj* %f47130, %struct.ScmObj** %stackaddr$prim58623, align 8
%stackaddr$prim58624 = alloca %struct.ScmObj*, align 8
%current_45args56821 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56820)
store volatile %struct.ScmObj* %current_45args56821, %struct.ScmObj** %stackaddr$prim58624, align 8
%stackaddr$prim58625 = alloca %struct.ScmObj*, align 8
%lst47129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56821)
store volatile %struct.ScmObj* %lst47129, %struct.ScmObj** %stackaddr$prim58625, align 8
%stackaddr$makeclosure58626 = alloca %struct.ScmObj*, align 8
%fptrToInt58627 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48613 to i64
%ae48613 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58627)
store volatile %struct.ScmObj* %ae48613, %struct.ScmObj** %stackaddr$makeclosure58626, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48613, %struct.ScmObj* %lst47129, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48613, %struct.ScmObj* %_37foldr147097, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48613, %struct.ScmObj* %k47459, i64 2)
%ae48614 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58628 = alloca %struct.ScmObj*, align 8
%fptrToInt58629 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48615 to i64
%ae48615 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58629)
store volatile %struct.ScmObj* %ae48615, %struct.ScmObj** %stackaddr$makeclosure58628, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48615, %struct.ScmObj* %f47130, i64 0)
%argslist56836$ae486130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58630 = alloca %struct.ScmObj*, align 8
%argslist56836$ae486131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48615, %struct.ScmObj* %argslist56836$ae486130)
store volatile %struct.ScmObj* %argslist56836$ae486131, %struct.ScmObj** %stackaddr$prim58630, align 8
%stackaddr$prim58631 = alloca %struct.ScmObj*, align 8
%argslist56836$ae486132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48614, %struct.ScmObj* %argslist56836$ae486131)
store volatile %struct.ScmObj* %argslist56836$ae486132, %struct.ScmObj** %stackaddr$prim58631, align 8
%clofunc58632 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48613)
musttail call tailcc void %clofunc58632(%struct.ScmObj* %ae48613, %struct.ScmObj* %argslist56836$ae486132)
ret void
}

define tailcc void @proc_clo$ae48613(%struct.ScmObj* %env$ae48613,%struct.ScmObj* %current_45args56823) {
%stackaddr$env-ref58633 = alloca %struct.ScmObj*, align 8
%lst47129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48613, i64 0)
store %struct.ScmObj* %lst47129, %struct.ScmObj** %stackaddr$env-ref58633
%stackaddr$env-ref58634 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48613, i64 1)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref58634
%stackaddr$env-ref58635 = alloca %struct.ScmObj*, align 8
%k47459 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48613, i64 2)
store %struct.ScmObj* %k47459, %struct.ScmObj** %stackaddr$env-ref58635
%stackaddr$prim58636 = alloca %struct.ScmObj*, align 8
%_95k47460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56823)
store volatile %struct.ScmObj* %_95k47460, %struct.ScmObj** %stackaddr$prim58636, align 8
%stackaddr$prim58637 = alloca %struct.ScmObj*, align 8
%current_45args56824 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56823)
store volatile %struct.ScmObj* %current_45args56824, %struct.ScmObj** %stackaddr$prim58637, align 8
%stackaddr$prim58638 = alloca %struct.ScmObj*, align 8
%anf_45bind47255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56824)
store volatile %struct.ScmObj* %anf_45bind47255, %struct.ScmObj** %stackaddr$prim58638, align 8
%ae48647 = call %struct.ScmObj* @const_init_null()
%argslist56826$_37foldr1470970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58639 = alloca %struct.ScmObj*, align 8
%argslist56826$_37foldr1470971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47129, %struct.ScmObj* %argslist56826$_37foldr1470970)
store volatile %struct.ScmObj* %argslist56826$_37foldr1470971, %struct.ScmObj** %stackaddr$prim58639, align 8
%stackaddr$prim58640 = alloca %struct.ScmObj*, align 8
%argslist56826$_37foldr1470972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48647, %struct.ScmObj* %argslist56826$_37foldr1470971)
store volatile %struct.ScmObj* %argslist56826$_37foldr1470972, %struct.ScmObj** %stackaddr$prim58640, align 8
%stackaddr$prim58641 = alloca %struct.ScmObj*, align 8
%argslist56826$_37foldr1470973 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47255, %struct.ScmObj* %argslist56826$_37foldr1470972)
store volatile %struct.ScmObj* %argslist56826$_37foldr1470973, %struct.ScmObj** %stackaddr$prim58641, align 8
%stackaddr$prim58642 = alloca %struct.ScmObj*, align 8
%argslist56826$_37foldr1470974 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47459, %struct.ScmObj* %argslist56826$_37foldr1470973)
store volatile %struct.ScmObj* %argslist56826$_37foldr1470974, %struct.ScmObj** %stackaddr$prim58642, align 8
%clofunc58643 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147097)
musttail call tailcc void %clofunc58643(%struct.ScmObj* %_37foldr147097, %struct.ScmObj* %argslist56826$_37foldr1470974)
ret void
}

define tailcc void @proc_clo$ae48615(%struct.ScmObj* %env$ae48615,%struct.ScmObj* %current_45args56827) {
%stackaddr$env-ref58644 = alloca %struct.ScmObj*, align 8
%f47130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48615, i64 0)
store %struct.ScmObj* %f47130, %struct.ScmObj** %stackaddr$env-ref58644
%stackaddr$prim58645 = alloca %struct.ScmObj*, align 8
%k47461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56827)
store volatile %struct.ScmObj* %k47461, %struct.ScmObj** %stackaddr$prim58645, align 8
%stackaddr$prim58646 = alloca %struct.ScmObj*, align 8
%current_45args56828 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56827)
store volatile %struct.ScmObj* %current_45args56828, %struct.ScmObj** %stackaddr$prim58646, align 8
%stackaddr$prim58647 = alloca %struct.ScmObj*, align 8
%v47132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56828)
store volatile %struct.ScmObj* %v47132, %struct.ScmObj** %stackaddr$prim58647, align 8
%stackaddr$prim58648 = alloca %struct.ScmObj*, align 8
%current_45args56829 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56828)
store volatile %struct.ScmObj* %current_45args56829, %struct.ScmObj** %stackaddr$prim58648, align 8
%stackaddr$prim58649 = alloca %struct.ScmObj*, align 8
%r47131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56829)
store volatile %struct.ScmObj* %r47131, %struct.ScmObj** %stackaddr$prim58649, align 8
%stackaddr$makeclosure58650 = alloca %struct.ScmObj*, align 8
%fptrToInt58651 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48617 to i64
%ae48617 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58651)
store volatile %struct.ScmObj* %ae48617, %struct.ScmObj** %stackaddr$makeclosure58650, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48617, %struct.ScmObj* %k47461, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48617, %struct.ScmObj* %r47131, i64 1)
%argslist56835$f471300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58652 = alloca %struct.ScmObj*, align 8
%argslist56835$f471301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v47132, %struct.ScmObj* %argslist56835$f471300)
store volatile %struct.ScmObj* %argslist56835$f471301, %struct.ScmObj** %stackaddr$prim58652, align 8
%stackaddr$prim58653 = alloca %struct.ScmObj*, align 8
%argslist56835$f471302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48617, %struct.ScmObj* %argslist56835$f471301)
store volatile %struct.ScmObj* %argslist56835$f471302, %struct.ScmObj** %stackaddr$prim58653, align 8
%clofunc58654 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47130)
musttail call tailcc void %clofunc58654(%struct.ScmObj* %f47130, %struct.ScmObj* %argslist56835$f471302)
ret void
}

define tailcc void @proc_clo$ae48617(%struct.ScmObj* %env$ae48617,%struct.ScmObj* %current_45args56831) {
%stackaddr$env-ref58655 = alloca %struct.ScmObj*, align 8
%k47461 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48617, i64 0)
store %struct.ScmObj* %k47461, %struct.ScmObj** %stackaddr$env-ref58655
%stackaddr$env-ref58656 = alloca %struct.ScmObj*, align 8
%r47131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48617, i64 1)
store %struct.ScmObj* %r47131, %struct.ScmObj** %stackaddr$env-ref58656
%stackaddr$prim58657 = alloca %struct.ScmObj*, align 8
%_95k47462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56831)
store volatile %struct.ScmObj* %_95k47462, %struct.ScmObj** %stackaddr$prim58657, align 8
%stackaddr$prim58658 = alloca %struct.ScmObj*, align 8
%current_45args56832 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56831)
store volatile %struct.ScmObj* %current_45args56832, %struct.ScmObj** %stackaddr$prim58658, align 8
%stackaddr$prim58659 = alloca %struct.ScmObj*, align 8
%anf_45bind47254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56832)
store volatile %struct.ScmObj* %anf_45bind47254, %struct.ScmObj** %stackaddr$prim58659, align 8
%stackaddr$prim58660 = alloca %struct.ScmObj*, align 8
%cpsprim47463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47254, %struct.ScmObj* %r47131)
store volatile %struct.ScmObj* %cpsprim47463, %struct.ScmObj** %stackaddr$prim58660, align 8
%ae48622 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56834$k474610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58661 = alloca %struct.ScmObj*, align 8
%argslist56834$k474611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47463, %struct.ScmObj* %argslist56834$k474610)
store volatile %struct.ScmObj* %argslist56834$k474611, %struct.ScmObj** %stackaddr$prim58661, align 8
%stackaddr$prim58662 = alloca %struct.ScmObj*, align 8
%argslist56834$k474612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48622, %struct.ScmObj* %argslist56834$k474611)
store volatile %struct.ScmObj* %argslist56834$k474612, %struct.ScmObj** %stackaddr$prim58662, align 8
%clofunc58663 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47461)
musttail call tailcc void %clofunc58663(%struct.ScmObj* %k47461, %struct.ScmObj* %argslist56834$k474612)
ret void
}

define tailcc void @proc_clo$ae48226(%struct.ScmObj* %env$ae48226,%struct.ScmObj* %current_45args56839) {
%stackaddr$env-ref58664 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48226, i64 0)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref58664
%stackaddr$env-ref58665 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48226, i64 1)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref58665
%stackaddr$prim58666 = alloca %struct.ScmObj*, align 8
%k47464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56839)
store volatile %struct.ScmObj* %k47464, %struct.ScmObj** %stackaddr$prim58666, align 8
%stackaddr$prim58667 = alloca %struct.ScmObj*, align 8
%current_45args56840 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56839)
store volatile %struct.ScmObj* %current_45args56840, %struct.ScmObj** %stackaddr$prim58667, align 8
%stackaddr$prim58668 = alloca %struct.ScmObj*, align 8
%_37foldr47103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56840)
store volatile %struct.ScmObj* %_37foldr47103, %struct.ScmObj** %stackaddr$prim58668, align 8
%ae48228 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58669 = alloca %struct.ScmObj*, align 8
%fptrToInt58670 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48229 to i64
%ae48229 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58670)
store volatile %struct.ScmObj* %ae48229, %struct.ScmObj** %stackaddr$makeclosure58669, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48229, %struct.ScmObj* %_37map147093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48229, %struct.ScmObj* %_37foldr47103, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48229, %struct.ScmObj* %_37foldr147097, i64 2)
%argslist56897$k474640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58671 = alloca %struct.ScmObj*, align 8
%argslist56897$k474641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48229, %struct.ScmObj* %argslist56897$k474640)
store volatile %struct.ScmObj* %argslist56897$k474641, %struct.ScmObj** %stackaddr$prim58671, align 8
%stackaddr$prim58672 = alloca %struct.ScmObj*, align 8
%argslist56897$k474642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48228, %struct.ScmObj* %argslist56897$k474641)
store volatile %struct.ScmObj* %argslist56897$k474642, %struct.ScmObj** %stackaddr$prim58672, align 8
%clofunc58673 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47464)
musttail call tailcc void %clofunc58673(%struct.ScmObj* %k47464, %struct.ScmObj* %argslist56897$k474642)
ret void
}

define tailcc void @proc_clo$ae48229(%struct.ScmObj* %env$ae48229,%struct.ScmObj* %args4710447465) {
%stackaddr$env-ref58674 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48229, i64 0)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref58674
%stackaddr$env-ref58675 = alloca %struct.ScmObj*, align 8
%_37foldr47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48229, i64 1)
store %struct.ScmObj* %_37foldr47103, %struct.ScmObj** %stackaddr$env-ref58675
%stackaddr$env-ref58676 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48229, i64 2)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref58676
%stackaddr$prim58677 = alloca %struct.ScmObj*, align 8
%k47466 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4710447465)
store volatile %struct.ScmObj* %k47466, %struct.ScmObj** %stackaddr$prim58677, align 8
%stackaddr$prim58678 = alloca %struct.ScmObj*, align 8
%args47104 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4710447465)
store volatile %struct.ScmObj* %args47104, %struct.ScmObj** %stackaddr$prim58678, align 8
%stackaddr$prim58679 = alloca %struct.ScmObj*, align 8
%f47107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47104)
store volatile %struct.ScmObj* %f47107, %struct.ScmObj** %stackaddr$prim58679, align 8
%stackaddr$prim58680 = alloca %struct.ScmObj*, align 8
%anf_45bind47241 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47104)
store volatile %struct.ScmObj* %anf_45bind47241, %struct.ScmObj** %stackaddr$prim58680, align 8
%stackaddr$prim58681 = alloca %struct.ScmObj*, align 8
%acc47106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47241)
store volatile %struct.ScmObj* %acc47106, %struct.ScmObj** %stackaddr$prim58681, align 8
%stackaddr$prim58682 = alloca %struct.ScmObj*, align 8
%anf_45bind47242 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47104)
store volatile %struct.ScmObj* %anf_45bind47242, %struct.ScmObj** %stackaddr$prim58682, align 8
%stackaddr$prim58683 = alloca %struct.ScmObj*, align 8
%lsts47105 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47242)
store volatile %struct.ScmObj* %lsts47105, %struct.ScmObj** %stackaddr$prim58683, align 8
%stackaddr$makeclosure58684 = alloca %struct.ScmObj*, align 8
%fptrToInt58685 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48237 to i64
%ae48237 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58685)
store volatile %struct.ScmObj* %ae48237, %struct.ScmObj** %stackaddr$makeclosure58684, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48237, %struct.ScmObj* %_37map147093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48237, %struct.ScmObj* %f47107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48237, %struct.ScmObj* %acc47106, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48237, %struct.ScmObj* %lsts47105, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48237, %struct.ScmObj* %_37foldr47103, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48237, %struct.ScmObj* %k47466, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48237, %struct.ScmObj* %_37foldr147097, i64 6)
%ae48238 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58686 = alloca %struct.ScmObj*, align 8
%fptrToInt58687 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48239 to i64
%ae48239 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58687)
store volatile %struct.ScmObj* %ae48239, %struct.ScmObj** %stackaddr$makeclosure58686, align 8
%argslist56896$ae482370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58688 = alloca %struct.ScmObj*, align 8
%argslist56896$ae482371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48239, %struct.ScmObj* %argslist56896$ae482370)
store volatile %struct.ScmObj* %argslist56896$ae482371, %struct.ScmObj** %stackaddr$prim58688, align 8
%stackaddr$prim58689 = alloca %struct.ScmObj*, align 8
%argslist56896$ae482372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48238, %struct.ScmObj* %argslist56896$ae482371)
store volatile %struct.ScmObj* %argslist56896$ae482372, %struct.ScmObj** %stackaddr$prim58689, align 8
%clofunc58690 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48237)
musttail call tailcc void %clofunc58690(%struct.ScmObj* %ae48237, %struct.ScmObj* %argslist56896$ae482372)
ret void
}

define tailcc void @proc_clo$ae48237(%struct.ScmObj* %env$ae48237,%struct.ScmObj* %current_45args56842) {
%stackaddr$env-ref58691 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48237, i64 0)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref58691
%stackaddr$env-ref58692 = alloca %struct.ScmObj*, align 8
%f47107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48237, i64 1)
store %struct.ScmObj* %f47107, %struct.ScmObj** %stackaddr$env-ref58692
%stackaddr$env-ref58693 = alloca %struct.ScmObj*, align 8
%acc47106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48237, i64 2)
store %struct.ScmObj* %acc47106, %struct.ScmObj** %stackaddr$env-ref58693
%stackaddr$env-ref58694 = alloca %struct.ScmObj*, align 8
%lsts47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48237, i64 3)
store %struct.ScmObj* %lsts47105, %struct.ScmObj** %stackaddr$env-ref58694
%stackaddr$env-ref58695 = alloca %struct.ScmObj*, align 8
%_37foldr47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48237, i64 4)
store %struct.ScmObj* %_37foldr47103, %struct.ScmObj** %stackaddr$env-ref58695
%stackaddr$env-ref58696 = alloca %struct.ScmObj*, align 8
%k47466 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48237, i64 5)
store %struct.ScmObj* %k47466, %struct.ScmObj** %stackaddr$env-ref58696
%stackaddr$env-ref58697 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48237, i64 6)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref58697
%stackaddr$prim58698 = alloca %struct.ScmObj*, align 8
%_95k47467 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56842)
store volatile %struct.ScmObj* %_95k47467, %struct.ScmObj** %stackaddr$prim58698, align 8
%stackaddr$prim58699 = alloca %struct.ScmObj*, align 8
%current_45args56843 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56842)
store volatile %struct.ScmObj* %current_45args56843, %struct.ScmObj** %stackaddr$prim58699, align 8
%stackaddr$prim58700 = alloca %struct.ScmObj*, align 8
%anf_45bind47243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56843)
store volatile %struct.ScmObj* %anf_45bind47243, %struct.ScmObj** %stackaddr$prim58700, align 8
%stackaddr$makeclosure58701 = alloca %struct.ScmObj*, align 8
%fptrToInt58702 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48269 to i64
%ae48269 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58702)
store volatile %struct.ScmObj* %ae48269, %struct.ScmObj** %stackaddr$makeclosure58701, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48269, %struct.ScmObj* %_37map147093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48269, %struct.ScmObj* %f47107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48269, %struct.ScmObj* %acc47106, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48269, %struct.ScmObj* %lsts47105, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48269, %struct.ScmObj* %_37foldr47103, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48269, %struct.ScmObj* %k47466, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48269, %struct.ScmObj* %_37foldr147097, i64 6)
%ae48271 = call %struct.ScmObj* @const_init_false()
%argslist56889$_37foldr1470970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58703 = alloca %struct.ScmObj*, align 8
%argslist56889$_37foldr1470971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47105, %struct.ScmObj* %argslist56889$_37foldr1470970)
store volatile %struct.ScmObj* %argslist56889$_37foldr1470971, %struct.ScmObj** %stackaddr$prim58703, align 8
%stackaddr$prim58704 = alloca %struct.ScmObj*, align 8
%argslist56889$_37foldr1470972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48271, %struct.ScmObj* %argslist56889$_37foldr1470971)
store volatile %struct.ScmObj* %argslist56889$_37foldr1470972, %struct.ScmObj** %stackaddr$prim58704, align 8
%stackaddr$prim58705 = alloca %struct.ScmObj*, align 8
%argslist56889$_37foldr1470973 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47243, %struct.ScmObj* %argslist56889$_37foldr1470972)
store volatile %struct.ScmObj* %argslist56889$_37foldr1470973, %struct.ScmObj** %stackaddr$prim58705, align 8
%stackaddr$prim58706 = alloca %struct.ScmObj*, align 8
%argslist56889$_37foldr1470974 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48269, %struct.ScmObj* %argslist56889$_37foldr1470973)
store volatile %struct.ScmObj* %argslist56889$_37foldr1470974, %struct.ScmObj** %stackaddr$prim58706, align 8
%clofunc58707 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147097)
musttail call tailcc void %clofunc58707(%struct.ScmObj* %_37foldr147097, %struct.ScmObj* %argslist56889$_37foldr1470974)
ret void
}

define tailcc void @proc_clo$ae48269(%struct.ScmObj* %env$ae48269,%struct.ScmObj* %current_45args56845) {
%stackaddr$env-ref58708 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48269, i64 0)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref58708
%stackaddr$env-ref58709 = alloca %struct.ScmObj*, align 8
%f47107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48269, i64 1)
store %struct.ScmObj* %f47107, %struct.ScmObj** %stackaddr$env-ref58709
%stackaddr$env-ref58710 = alloca %struct.ScmObj*, align 8
%acc47106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48269, i64 2)
store %struct.ScmObj* %acc47106, %struct.ScmObj** %stackaddr$env-ref58710
%stackaddr$env-ref58711 = alloca %struct.ScmObj*, align 8
%lsts47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48269, i64 3)
store %struct.ScmObj* %lsts47105, %struct.ScmObj** %stackaddr$env-ref58711
%stackaddr$env-ref58712 = alloca %struct.ScmObj*, align 8
%_37foldr47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48269, i64 4)
store %struct.ScmObj* %_37foldr47103, %struct.ScmObj** %stackaddr$env-ref58712
%stackaddr$env-ref58713 = alloca %struct.ScmObj*, align 8
%k47466 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48269, i64 5)
store %struct.ScmObj* %k47466, %struct.ScmObj** %stackaddr$env-ref58713
%stackaddr$env-ref58714 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48269, i64 6)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref58714
%stackaddr$prim58715 = alloca %struct.ScmObj*, align 8
%_95k47468 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56845)
store volatile %struct.ScmObj* %_95k47468, %struct.ScmObj** %stackaddr$prim58715, align 8
%stackaddr$prim58716 = alloca %struct.ScmObj*, align 8
%current_45args56846 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56845)
store volatile %struct.ScmObj* %current_45args56846, %struct.ScmObj** %stackaddr$prim58716, align 8
%stackaddr$prim58717 = alloca %struct.ScmObj*, align 8
%anf_45bind47244 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56846)
store volatile %struct.ScmObj* %anf_45bind47244, %struct.ScmObj** %stackaddr$prim58717, align 8
%truthy$cmp58718 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47244)
%cmp$cmp58718 = icmp eq i64 %truthy$cmp58718, 1
br i1 %cmp$cmp58718, label %truebranch$cmp58718, label %falsebranch$cmp58718
truebranch$cmp58718:
%ae48280 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56848$k474660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58719 = alloca %struct.ScmObj*, align 8
%argslist56848$k474661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47106, %struct.ScmObj* %argslist56848$k474660)
store volatile %struct.ScmObj* %argslist56848$k474661, %struct.ScmObj** %stackaddr$prim58719, align 8
%stackaddr$prim58720 = alloca %struct.ScmObj*, align 8
%argslist56848$k474662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48280, %struct.ScmObj* %argslist56848$k474661)
store volatile %struct.ScmObj* %argslist56848$k474662, %struct.ScmObj** %stackaddr$prim58720, align 8
%clofunc58721 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47466)
musttail call tailcc void %clofunc58721(%struct.ScmObj* %k47466, %struct.ScmObj* %argslist56848$k474662)
ret void
falsebranch$cmp58718:
%stackaddr$makeclosure58722 = alloca %struct.ScmObj*, align 8
%fptrToInt58723 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48285 to i64
%ae48285 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58723)
store volatile %struct.ScmObj* %ae48285, %struct.ScmObj** %stackaddr$makeclosure58722, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48285, %struct.ScmObj* %_37map147093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48285, %struct.ScmObj* %f47107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48285, %struct.ScmObj* %acc47106, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48285, %struct.ScmObj* %lsts47105, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48285, %struct.ScmObj* %_37foldr47103, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48285, %struct.ScmObj* %k47466, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48285, %struct.ScmObj* %_37foldr147097, i64 6)
%ae48286 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58724 = alloca %struct.ScmObj*, align 8
%fptrToInt58725 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48287 to i64
%ae48287 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58725)
store volatile %struct.ScmObj* %ae48287, %struct.ScmObj** %stackaddr$makeclosure58724, align 8
%argslist56888$ae482850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58726 = alloca %struct.ScmObj*, align 8
%argslist56888$ae482851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48287, %struct.ScmObj* %argslist56888$ae482850)
store volatile %struct.ScmObj* %argslist56888$ae482851, %struct.ScmObj** %stackaddr$prim58726, align 8
%stackaddr$prim58727 = alloca %struct.ScmObj*, align 8
%argslist56888$ae482852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48286, %struct.ScmObj* %argslist56888$ae482851)
store volatile %struct.ScmObj* %argslist56888$ae482852, %struct.ScmObj** %stackaddr$prim58727, align 8
%clofunc58728 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48285)
musttail call tailcc void %clofunc58728(%struct.ScmObj* %ae48285, %struct.ScmObj* %argslist56888$ae482852)
ret void
}

define tailcc void @proc_clo$ae48285(%struct.ScmObj* %env$ae48285,%struct.ScmObj* %current_45args56849) {
%stackaddr$env-ref58729 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48285, i64 0)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref58729
%stackaddr$env-ref58730 = alloca %struct.ScmObj*, align 8
%f47107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48285, i64 1)
store %struct.ScmObj* %f47107, %struct.ScmObj** %stackaddr$env-ref58730
%stackaddr$env-ref58731 = alloca %struct.ScmObj*, align 8
%acc47106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48285, i64 2)
store %struct.ScmObj* %acc47106, %struct.ScmObj** %stackaddr$env-ref58731
%stackaddr$env-ref58732 = alloca %struct.ScmObj*, align 8
%lsts47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48285, i64 3)
store %struct.ScmObj* %lsts47105, %struct.ScmObj** %stackaddr$env-ref58732
%stackaddr$env-ref58733 = alloca %struct.ScmObj*, align 8
%_37foldr47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48285, i64 4)
store %struct.ScmObj* %_37foldr47103, %struct.ScmObj** %stackaddr$env-ref58733
%stackaddr$env-ref58734 = alloca %struct.ScmObj*, align 8
%k47466 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48285, i64 5)
store %struct.ScmObj* %k47466, %struct.ScmObj** %stackaddr$env-ref58734
%stackaddr$env-ref58735 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48285, i64 6)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref58735
%stackaddr$prim58736 = alloca %struct.ScmObj*, align 8
%_95k47469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56849)
store volatile %struct.ScmObj* %_95k47469, %struct.ScmObj** %stackaddr$prim58736, align 8
%stackaddr$prim58737 = alloca %struct.ScmObj*, align 8
%current_45args56850 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56849)
store volatile %struct.ScmObj* %current_45args56850, %struct.ScmObj** %stackaddr$prim58737, align 8
%stackaddr$prim58738 = alloca %struct.ScmObj*, align 8
%anf_45bind47245 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56850)
store volatile %struct.ScmObj* %anf_45bind47245, %struct.ScmObj** %stackaddr$prim58738, align 8
%stackaddr$makeclosure58739 = alloca %struct.ScmObj*, align 8
%fptrToInt58740 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48306 to i64
%ae48306 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58740)
store volatile %struct.ScmObj* %ae48306, %struct.ScmObj** %stackaddr$makeclosure58739, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48306, %struct.ScmObj* %_37map147093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48306, %struct.ScmObj* %f47107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48306, %struct.ScmObj* %acc47106, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48306, %struct.ScmObj* %lsts47105, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48306, %struct.ScmObj* %_37foldr47103, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48306, %struct.ScmObj* %k47466, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48306, %struct.ScmObj* %_37foldr147097, i64 6)
%argslist56883$_37map1470930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58741 = alloca %struct.ScmObj*, align 8
%argslist56883$_37map1470931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47105, %struct.ScmObj* %argslist56883$_37map1470930)
store volatile %struct.ScmObj* %argslist56883$_37map1470931, %struct.ScmObj** %stackaddr$prim58741, align 8
%stackaddr$prim58742 = alloca %struct.ScmObj*, align 8
%argslist56883$_37map1470932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47245, %struct.ScmObj* %argslist56883$_37map1470931)
store volatile %struct.ScmObj* %argslist56883$_37map1470932, %struct.ScmObj** %stackaddr$prim58742, align 8
%stackaddr$prim58743 = alloca %struct.ScmObj*, align 8
%argslist56883$_37map1470933 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48306, %struct.ScmObj* %argslist56883$_37map1470932)
store volatile %struct.ScmObj* %argslist56883$_37map1470933, %struct.ScmObj** %stackaddr$prim58743, align 8
%clofunc58744 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147093)
musttail call tailcc void %clofunc58744(%struct.ScmObj* %_37map147093, %struct.ScmObj* %argslist56883$_37map1470933)
ret void
}

define tailcc void @proc_clo$ae48306(%struct.ScmObj* %env$ae48306,%struct.ScmObj* %current_45args56852) {
%stackaddr$env-ref58745 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48306, i64 0)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref58745
%stackaddr$env-ref58746 = alloca %struct.ScmObj*, align 8
%f47107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48306, i64 1)
store %struct.ScmObj* %f47107, %struct.ScmObj** %stackaddr$env-ref58746
%stackaddr$env-ref58747 = alloca %struct.ScmObj*, align 8
%acc47106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48306, i64 2)
store %struct.ScmObj* %acc47106, %struct.ScmObj** %stackaddr$env-ref58747
%stackaddr$env-ref58748 = alloca %struct.ScmObj*, align 8
%lsts47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48306, i64 3)
store %struct.ScmObj* %lsts47105, %struct.ScmObj** %stackaddr$env-ref58748
%stackaddr$env-ref58749 = alloca %struct.ScmObj*, align 8
%_37foldr47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48306, i64 4)
store %struct.ScmObj* %_37foldr47103, %struct.ScmObj** %stackaddr$env-ref58749
%stackaddr$env-ref58750 = alloca %struct.ScmObj*, align 8
%k47466 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48306, i64 5)
store %struct.ScmObj* %k47466, %struct.ScmObj** %stackaddr$env-ref58750
%stackaddr$env-ref58751 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48306, i64 6)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref58751
%stackaddr$prim58752 = alloca %struct.ScmObj*, align 8
%_95k47470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56852)
store volatile %struct.ScmObj* %_95k47470, %struct.ScmObj** %stackaddr$prim58752, align 8
%stackaddr$prim58753 = alloca %struct.ScmObj*, align 8
%current_45args56853 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56852)
store volatile %struct.ScmObj* %current_45args56853, %struct.ScmObj** %stackaddr$prim58753, align 8
%stackaddr$prim58754 = alloca %struct.ScmObj*, align 8
%lsts_4347112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56853)
store volatile %struct.ScmObj* %lsts_4347112, %struct.ScmObj** %stackaddr$prim58754, align 8
%stackaddr$makeclosure58755 = alloca %struct.ScmObj*, align 8
%fptrToInt58756 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48309 to i64
%ae48309 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt58756)
store volatile %struct.ScmObj* %ae48309, %struct.ScmObj** %stackaddr$makeclosure58755, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48309, %struct.ScmObj* %_37map147093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48309, %struct.ScmObj* %f47107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48309, %struct.ScmObj* %acc47106, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48309, %struct.ScmObj* %lsts47105, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48309, %struct.ScmObj* %_37foldr47103, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48309, %struct.ScmObj* %k47466, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48309, %struct.ScmObj* %_37foldr147097, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48309, %struct.ScmObj* %lsts_4347112, i64 7)
%ae48310 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58757 = alloca %struct.ScmObj*, align 8
%fptrToInt58758 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48311 to i64
%ae48311 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58758)
store volatile %struct.ScmObj* %ae48311, %struct.ScmObj** %stackaddr$makeclosure58757, align 8
%argslist56882$ae483090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58759 = alloca %struct.ScmObj*, align 8
%argslist56882$ae483091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48311, %struct.ScmObj* %argslist56882$ae483090)
store volatile %struct.ScmObj* %argslist56882$ae483091, %struct.ScmObj** %stackaddr$prim58759, align 8
%stackaddr$prim58760 = alloca %struct.ScmObj*, align 8
%argslist56882$ae483092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48310, %struct.ScmObj* %argslist56882$ae483091)
store volatile %struct.ScmObj* %argslist56882$ae483092, %struct.ScmObj** %stackaddr$prim58760, align 8
%clofunc58761 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48309)
musttail call tailcc void %clofunc58761(%struct.ScmObj* %ae48309, %struct.ScmObj* %argslist56882$ae483092)
ret void
}

define tailcc void @proc_clo$ae48309(%struct.ScmObj* %env$ae48309,%struct.ScmObj* %current_45args56855) {
%stackaddr$env-ref58762 = alloca %struct.ScmObj*, align 8
%_37map147093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48309, i64 0)
store %struct.ScmObj* %_37map147093, %struct.ScmObj** %stackaddr$env-ref58762
%stackaddr$env-ref58763 = alloca %struct.ScmObj*, align 8
%f47107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48309, i64 1)
store %struct.ScmObj* %f47107, %struct.ScmObj** %stackaddr$env-ref58763
%stackaddr$env-ref58764 = alloca %struct.ScmObj*, align 8
%acc47106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48309, i64 2)
store %struct.ScmObj* %acc47106, %struct.ScmObj** %stackaddr$env-ref58764
%stackaddr$env-ref58765 = alloca %struct.ScmObj*, align 8
%lsts47105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48309, i64 3)
store %struct.ScmObj* %lsts47105, %struct.ScmObj** %stackaddr$env-ref58765
%stackaddr$env-ref58766 = alloca %struct.ScmObj*, align 8
%_37foldr47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48309, i64 4)
store %struct.ScmObj* %_37foldr47103, %struct.ScmObj** %stackaddr$env-ref58766
%stackaddr$env-ref58767 = alloca %struct.ScmObj*, align 8
%k47466 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48309, i64 5)
store %struct.ScmObj* %k47466, %struct.ScmObj** %stackaddr$env-ref58767
%stackaddr$env-ref58768 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48309, i64 6)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref58768
%stackaddr$env-ref58769 = alloca %struct.ScmObj*, align 8
%lsts_4347112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48309, i64 7)
store %struct.ScmObj* %lsts_4347112, %struct.ScmObj** %stackaddr$env-ref58769
%stackaddr$prim58770 = alloca %struct.ScmObj*, align 8
%_95k47471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56855)
store volatile %struct.ScmObj* %_95k47471, %struct.ScmObj** %stackaddr$prim58770, align 8
%stackaddr$prim58771 = alloca %struct.ScmObj*, align 8
%current_45args56856 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56855)
store volatile %struct.ScmObj* %current_45args56856, %struct.ScmObj** %stackaddr$prim58771, align 8
%stackaddr$prim58772 = alloca %struct.ScmObj*, align 8
%anf_45bind47246 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56856)
store volatile %struct.ScmObj* %anf_45bind47246, %struct.ScmObj** %stackaddr$prim58772, align 8
%stackaddr$makeclosure58773 = alloca %struct.ScmObj*, align 8
%fptrToInt58774 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48330 to i64
%ae48330 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt58774)
store volatile %struct.ScmObj* %ae48330, %struct.ScmObj** %stackaddr$makeclosure58773, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48330, %struct.ScmObj* %f47107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48330, %struct.ScmObj* %acc47106, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48330, %struct.ScmObj* %_37foldr47103, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48330, %struct.ScmObj* %k47466, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48330, %struct.ScmObj* %_37foldr147097, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48330, %struct.ScmObj* %lsts_4347112, i64 5)
%argslist56877$_37map1470930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58775 = alloca %struct.ScmObj*, align 8
%argslist56877$_37map1470931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47105, %struct.ScmObj* %argslist56877$_37map1470930)
store volatile %struct.ScmObj* %argslist56877$_37map1470931, %struct.ScmObj** %stackaddr$prim58775, align 8
%stackaddr$prim58776 = alloca %struct.ScmObj*, align 8
%argslist56877$_37map1470932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47246, %struct.ScmObj* %argslist56877$_37map1470931)
store volatile %struct.ScmObj* %argslist56877$_37map1470932, %struct.ScmObj** %stackaddr$prim58776, align 8
%stackaddr$prim58777 = alloca %struct.ScmObj*, align 8
%argslist56877$_37map1470933 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48330, %struct.ScmObj* %argslist56877$_37map1470932)
store volatile %struct.ScmObj* %argslist56877$_37map1470933, %struct.ScmObj** %stackaddr$prim58777, align 8
%clofunc58778 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147093)
musttail call tailcc void %clofunc58778(%struct.ScmObj* %_37map147093, %struct.ScmObj* %argslist56877$_37map1470933)
ret void
}

define tailcc void @proc_clo$ae48330(%struct.ScmObj* %env$ae48330,%struct.ScmObj* %current_45args56858) {
%stackaddr$env-ref58779 = alloca %struct.ScmObj*, align 8
%f47107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48330, i64 0)
store %struct.ScmObj* %f47107, %struct.ScmObj** %stackaddr$env-ref58779
%stackaddr$env-ref58780 = alloca %struct.ScmObj*, align 8
%acc47106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48330, i64 1)
store %struct.ScmObj* %acc47106, %struct.ScmObj** %stackaddr$env-ref58780
%stackaddr$env-ref58781 = alloca %struct.ScmObj*, align 8
%_37foldr47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48330, i64 2)
store %struct.ScmObj* %_37foldr47103, %struct.ScmObj** %stackaddr$env-ref58781
%stackaddr$env-ref58782 = alloca %struct.ScmObj*, align 8
%k47466 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48330, i64 3)
store %struct.ScmObj* %k47466, %struct.ScmObj** %stackaddr$env-ref58782
%stackaddr$env-ref58783 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48330, i64 4)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref58783
%stackaddr$env-ref58784 = alloca %struct.ScmObj*, align 8
%lsts_4347112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48330, i64 5)
store %struct.ScmObj* %lsts_4347112, %struct.ScmObj** %stackaddr$env-ref58784
%stackaddr$prim58785 = alloca %struct.ScmObj*, align 8
%_95k47472 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56858)
store volatile %struct.ScmObj* %_95k47472, %struct.ScmObj** %stackaddr$prim58785, align 8
%stackaddr$prim58786 = alloca %struct.ScmObj*, align 8
%current_45args56859 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56858)
store volatile %struct.ScmObj* %current_45args56859, %struct.ScmObj** %stackaddr$prim58786, align 8
%stackaddr$prim58787 = alloca %struct.ScmObj*, align 8
%vs47110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56859)
store volatile %struct.ScmObj* %vs47110, %struct.ScmObj** %stackaddr$prim58787, align 8
%stackaddr$makeclosure58788 = alloca %struct.ScmObj*, align 8
%fptrToInt58789 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48333 to i64
%ae48333 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt58789)
store volatile %struct.ScmObj* %ae48333, %struct.ScmObj** %stackaddr$makeclosure58788, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48333, %struct.ScmObj* %vs47110, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48333, %struct.ScmObj* %f47107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48333, %struct.ScmObj* %acc47106, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48333, %struct.ScmObj* %_37foldr47103, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48333, %struct.ScmObj* %k47466, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48333, %struct.ScmObj* %_37foldr147097, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48333, %struct.ScmObj* %lsts_4347112, i64 6)
%ae48334 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58790 = alloca %struct.ScmObj*, align 8
%fptrToInt58791 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48335 to i64
%ae48335 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58791)
store volatile %struct.ScmObj* %ae48335, %struct.ScmObj** %stackaddr$makeclosure58790, align 8
%argslist56876$ae483330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58792 = alloca %struct.ScmObj*, align 8
%argslist56876$ae483331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48335, %struct.ScmObj* %argslist56876$ae483330)
store volatile %struct.ScmObj* %argslist56876$ae483331, %struct.ScmObj** %stackaddr$prim58792, align 8
%stackaddr$prim58793 = alloca %struct.ScmObj*, align 8
%argslist56876$ae483332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48334, %struct.ScmObj* %argslist56876$ae483331)
store volatile %struct.ScmObj* %argslist56876$ae483332, %struct.ScmObj** %stackaddr$prim58793, align 8
%clofunc58794 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48333)
musttail call tailcc void %clofunc58794(%struct.ScmObj* %ae48333, %struct.ScmObj* %argslist56876$ae483332)
ret void
}

define tailcc void @proc_clo$ae48333(%struct.ScmObj* %env$ae48333,%struct.ScmObj* %current_45args56861) {
%stackaddr$env-ref58795 = alloca %struct.ScmObj*, align 8
%vs47110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48333, i64 0)
store %struct.ScmObj* %vs47110, %struct.ScmObj** %stackaddr$env-ref58795
%stackaddr$env-ref58796 = alloca %struct.ScmObj*, align 8
%f47107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48333, i64 1)
store %struct.ScmObj* %f47107, %struct.ScmObj** %stackaddr$env-ref58796
%stackaddr$env-ref58797 = alloca %struct.ScmObj*, align 8
%acc47106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48333, i64 2)
store %struct.ScmObj* %acc47106, %struct.ScmObj** %stackaddr$env-ref58797
%stackaddr$env-ref58798 = alloca %struct.ScmObj*, align 8
%_37foldr47103 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48333, i64 3)
store %struct.ScmObj* %_37foldr47103, %struct.ScmObj** %stackaddr$env-ref58798
%stackaddr$env-ref58799 = alloca %struct.ScmObj*, align 8
%k47466 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48333, i64 4)
store %struct.ScmObj* %k47466, %struct.ScmObj** %stackaddr$env-ref58799
%stackaddr$env-ref58800 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48333, i64 5)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref58800
%stackaddr$env-ref58801 = alloca %struct.ScmObj*, align 8
%lsts_4347112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48333, i64 6)
store %struct.ScmObj* %lsts_4347112, %struct.ScmObj** %stackaddr$env-ref58801
%stackaddr$prim58802 = alloca %struct.ScmObj*, align 8
%_95k47473 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56861)
store volatile %struct.ScmObj* %_95k47473, %struct.ScmObj** %stackaddr$prim58802, align 8
%stackaddr$prim58803 = alloca %struct.ScmObj*, align 8
%current_45args56862 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56861)
store volatile %struct.ScmObj* %current_45args56862, %struct.ScmObj** %stackaddr$prim58803, align 8
%stackaddr$prim58804 = alloca %struct.ScmObj*, align 8
%anf_45bind47247 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56862)
store volatile %struct.ScmObj* %anf_45bind47247, %struct.ScmObj** %stackaddr$prim58804, align 8
%stackaddr$prim58805 = alloca %struct.ScmObj*, align 8
%anf_45bind47248 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47106, %struct.ScmObj* %lsts_4347112)
store volatile %struct.ScmObj* %anf_45bind47248, %struct.ScmObj** %stackaddr$prim58805, align 8
%stackaddr$prim58806 = alloca %struct.ScmObj*, align 8
%anf_45bind47249 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47107, %struct.ScmObj* %anf_45bind47248)
store volatile %struct.ScmObj* %anf_45bind47249, %struct.ScmObj** %stackaddr$prim58806, align 8
%stackaddr$makeclosure58807 = alloca %struct.ScmObj*, align 8
%fptrToInt58808 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48359 to i64
%ae48359 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt58808)
store volatile %struct.ScmObj* %ae48359, %struct.ScmObj** %stackaddr$makeclosure58807, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48359, %struct.ScmObj* %vs47110, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48359, %struct.ScmObj* %f47107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48359, %struct.ScmObj* %anf_45bind47247, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48359, %struct.ScmObj* %k47466, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48359, %struct.ScmObj* %_37foldr147097, i64 4)
%stackaddr$prim58809 = alloca %struct.ScmObj*, align 8
%cpsargs47477 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48359, %struct.ScmObj* %anf_45bind47249)
store volatile %struct.ScmObj* %cpsargs47477, %struct.ScmObj** %stackaddr$prim58809, align 8
%clofunc58810 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47103)
musttail call tailcc void %clofunc58810(%struct.ScmObj* %_37foldr47103, %struct.ScmObj* %cpsargs47477)
ret void
}

define tailcc void @proc_clo$ae48359(%struct.ScmObj* %env$ae48359,%struct.ScmObj* %current_45args56864) {
%stackaddr$env-ref58811 = alloca %struct.ScmObj*, align 8
%vs47110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48359, i64 0)
store %struct.ScmObj* %vs47110, %struct.ScmObj** %stackaddr$env-ref58811
%stackaddr$env-ref58812 = alloca %struct.ScmObj*, align 8
%f47107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48359, i64 1)
store %struct.ScmObj* %f47107, %struct.ScmObj** %stackaddr$env-ref58812
%stackaddr$env-ref58813 = alloca %struct.ScmObj*, align 8
%anf_45bind47247 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48359, i64 2)
store %struct.ScmObj* %anf_45bind47247, %struct.ScmObj** %stackaddr$env-ref58813
%stackaddr$env-ref58814 = alloca %struct.ScmObj*, align 8
%k47466 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48359, i64 3)
store %struct.ScmObj* %k47466, %struct.ScmObj** %stackaddr$env-ref58814
%stackaddr$env-ref58815 = alloca %struct.ScmObj*, align 8
%_37foldr147097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48359, i64 4)
store %struct.ScmObj* %_37foldr147097, %struct.ScmObj** %stackaddr$env-ref58815
%stackaddr$prim58816 = alloca %struct.ScmObj*, align 8
%_95k47474 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56864)
store volatile %struct.ScmObj* %_95k47474, %struct.ScmObj** %stackaddr$prim58816, align 8
%stackaddr$prim58817 = alloca %struct.ScmObj*, align 8
%current_45args56865 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56864)
store volatile %struct.ScmObj* %current_45args56865, %struct.ScmObj** %stackaddr$prim58817, align 8
%stackaddr$prim58818 = alloca %struct.ScmObj*, align 8
%anf_45bind47250 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56865)
store volatile %struct.ScmObj* %anf_45bind47250, %struct.ScmObj** %stackaddr$prim58818, align 8
%ae48364 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58819 = alloca %struct.ScmObj*, align 8
%anf_45bind47251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47250, %struct.ScmObj* %ae48364)
store volatile %struct.ScmObj* %anf_45bind47251, %struct.ScmObj** %stackaddr$prim58819, align 8
%stackaddr$makeclosure58820 = alloca %struct.ScmObj*, align 8
%fptrToInt58821 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48366 to i64
%ae48366 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58821)
store volatile %struct.ScmObj* %ae48366, %struct.ScmObj** %stackaddr$makeclosure58820, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48366, %struct.ScmObj* %f47107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48366, %struct.ScmObj* %k47466, i64 1)
%argslist56870$_37foldr1470970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58822 = alloca %struct.ScmObj*, align 8
%argslist56870$_37foldr1470971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47110, %struct.ScmObj* %argslist56870$_37foldr1470970)
store volatile %struct.ScmObj* %argslist56870$_37foldr1470971, %struct.ScmObj** %stackaddr$prim58822, align 8
%stackaddr$prim58823 = alloca %struct.ScmObj*, align 8
%argslist56870$_37foldr1470972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47251, %struct.ScmObj* %argslist56870$_37foldr1470971)
store volatile %struct.ScmObj* %argslist56870$_37foldr1470972, %struct.ScmObj** %stackaddr$prim58823, align 8
%stackaddr$prim58824 = alloca %struct.ScmObj*, align 8
%argslist56870$_37foldr1470973 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47247, %struct.ScmObj* %argslist56870$_37foldr1470972)
store volatile %struct.ScmObj* %argslist56870$_37foldr1470973, %struct.ScmObj** %stackaddr$prim58824, align 8
%stackaddr$prim58825 = alloca %struct.ScmObj*, align 8
%argslist56870$_37foldr1470974 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48366, %struct.ScmObj* %argslist56870$_37foldr1470973)
store volatile %struct.ScmObj* %argslist56870$_37foldr1470974, %struct.ScmObj** %stackaddr$prim58825, align 8
%clofunc58826 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147097)
musttail call tailcc void %clofunc58826(%struct.ScmObj* %_37foldr147097, %struct.ScmObj* %argslist56870$_37foldr1470974)
ret void
}

define tailcc void @proc_clo$ae48366(%struct.ScmObj* %env$ae48366,%struct.ScmObj* %current_45args56867) {
%stackaddr$env-ref58827 = alloca %struct.ScmObj*, align 8
%f47107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48366, i64 0)
store %struct.ScmObj* %f47107, %struct.ScmObj** %stackaddr$env-ref58827
%stackaddr$env-ref58828 = alloca %struct.ScmObj*, align 8
%k47466 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48366, i64 1)
store %struct.ScmObj* %k47466, %struct.ScmObj** %stackaddr$env-ref58828
%stackaddr$prim58829 = alloca %struct.ScmObj*, align 8
%_95k47475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56867)
store volatile %struct.ScmObj* %_95k47475, %struct.ScmObj** %stackaddr$prim58829, align 8
%stackaddr$prim58830 = alloca %struct.ScmObj*, align 8
%current_45args56868 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56867)
store volatile %struct.ScmObj* %current_45args56868, %struct.ScmObj** %stackaddr$prim58830, align 8
%stackaddr$prim58831 = alloca %struct.ScmObj*, align 8
%anf_45bind47252 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56868)
store volatile %struct.ScmObj* %anf_45bind47252, %struct.ScmObj** %stackaddr$prim58831, align 8
%stackaddr$prim58832 = alloca %struct.ScmObj*, align 8
%cpsargs47476 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47466, %struct.ScmObj* %anf_45bind47252)
store volatile %struct.ScmObj* %cpsargs47476, %struct.ScmObj** %stackaddr$prim58832, align 8
%clofunc58833 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47107)
musttail call tailcc void %clofunc58833(%struct.ScmObj* %f47107, %struct.ScmObj* %cpsargs47476)
ret void
}

define tailcc void @proc_clo$ae48335(%struct.ScmObj* %env$ae48335,%struct.ScmObj* %current_45args56871) {
%stackaddr$prim58834 = alloca %struct.ScmObj*, align 8
%k47478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56871)
store volatile %struct.ScmObj* %k47478, %struct.ScmObj** %stackaddr$prim58834, align 8
%stackaddr$prim58835 = alloca %struct.ScmObj*, align 8
%current_45args56872 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56871)
store volatile %struct.ScmObj* %current_45args56872, %struct.ScmObj** %stackaddr$prim58835, align 8
%stackaddr$prim58836 = alloca %struct.ScmObj*, align 8
%a47115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56872)
store volatile %struct.ScmObj* %a47115, %struct.ScmObj** %stackaddr$prim58836, align 8
%stackaddr$prim58837 = alloca %struct.ScmObj*, align 8
%current_45args56873 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56872)
store volatile %struct.ScmObj* %current_45args56873, %struct.ScmObj** %stackaddr$prim58837, align 8
%stackaddr$prim58838 = alloca %struct.ScmObj*, align 8
%b47114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56873)
store volatile %struct.ScmObj* %b47114, %struct.ScmObj** %stackaddr$prim58838, align 8
%stackaddr$prim58839 = alloca %struct.ScmObj*, align 8
%cpsprim47479 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47115, %struct.ScmObj* %b47114)
store volatile %struct.ScmObj* %cpsprim47479, %struct.ScmObj** %stackaddr$prim58839, align 8
%ae48339 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56875$k474780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58840 = alloca %struct.ScmObj*, align 8
%argslist56875$k474781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47479, %struct.ScmObj* %argslist56875$k474780)
store volatile %struct.ScmObj* %argslist56875$k474781, %struct.ScmObj** %stackaddr$prim58840, align 8
%stackaddr$prim58841 = alloca %struct.ScmObj*, align 8
%argslist56875$k474782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48339, %struct.ScmObj* %argslist56875$k474781)
store volatile %struct.ScmObj* %argslist56875$k474782, %struct.ScmObj** %stackaddr$prim58841, align 8
%clofunc58842 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47478)
musttail call tailcc void %clofunc58842(%struct.ScmObj* %k47478, %struct.ScmObj* %argslist56875$k474782)
ret void
}

define tailcc void @proc_clo$ae48311(%struct.ScmObj* %env$ae48311,%struct.ScmObj* %current_45args56878) {
%stackaddr$prim58843 = alloca %struct.ScmObj*, align 8
%k47480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56878)
store volatile %struct.ScmObj* %k47480, %struct.ScmObj** %stackaddr$prim58843, align 8
%stackaddr$prim58844 = alloca %struct.ScmObj*, align 8
%current_45args56879 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56878)
store volatile %struct.ScmObj* %current_45args56879, %struct.ScmObj** %stackaddr$prim58844, align 8
%stackaddr$prim58845 = alloca %struct.ScmObj*, align 8
%x47111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56879)
store volatile %struct.ScmObj* %x47111, %struct.ScmObj** %stackaddr$prim58845, align 8
%stackaddr$prim58846 = alloca %struct.ScmObj*, align 8
%cpsprim47481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47111)
store volatile %struct.ScmObj* %cpsprim47481, %struct.ScmObj** %stackaddr$prim58846, align 8
%ae48314 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56881$k474800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58847 = alloca %struct.ScmObj*, align 8
%argslist56881$k474801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47481, %struct.ScmObj* %argslist56881$k474800)
store volatile %struct.ScmObj* %argslist56881$k474801, %struct.ScmObj** %stackaddr$prim58847, align 8
%stackaddr$prim58848 = alloca %struct.ScmObj*, align 8
%argslist56881$k474802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48314, %struct.ScmObj* %argslist56881$k474801)
store volatile %struct.ScmObj* %argslist56881$k474802, %struct.ScmObj** %stackaddr$prim58848, align 8
%clofunc58849 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47480)
musttail call tailcc void %clofunc58849(%struct.ScmObj* %k47480, %struct.ScmObj* %argslist56881$k474802)
ret void
}

define tailcc void @proc_clo$ae48287(%struct.ScmObj* %env$ae48287,%struct.ScmObj* %current_45args56884) {
%stackaddr$prim58850 = alloca %struct.ScmObj*, align 8
%k47482 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56884)
store volatile %struct.ScmObj* %k47482, %struct.ScmObj** %stackaddr$prim58850, align 8
%stackaddr$prim58851 = alloca %struct.ScmObj*, align 8
%current_45args56885 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56884)
store volatile %struct.ScmObj* %current_45args56885, %struct.ScmObj** %stackaddr$prim58851, align 8
%stackaddr$prim58852 = alloca %struct.ScmObj*, align 8
%x47113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56885)
store volatile %struct.ScmObj* %x47113, %struct.ScmObj** %stackaddr$prim58852, align 8
%stackaddr$prim58853 = alloca %struct.ScmObj*, align 8
%cpsprim47483 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47113)
store volatile %struct.ScmObj* %cpsprim47483, %struct.ScmObj** %stackaddr$prim58853, align 8
%ae48290 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56887$k474820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58854 = alloca %struct.ScmObj*, align 8
%argslist56887$k474821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47483, %struct.ScmObj* %argslist56887$k474820)
store volatile %struct.ScmObj* %argslist56887$k474821, %struct.ScmObj** %stackaddr$prim58854, align 8
%stackaddr$prim58855 = alloca %struct.ScmObj*, align 8
%argslist56887$k474822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48290, %struct.ScmObj* %argslist56887$k474821)
store volatile %struct.ScmObj* %argslist56887$k474822, %struct.ScmObj** %stackaddr$prim58855, align 8
%clofunc58856 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47482)
musttail call tailcc void %clofunc58856(%struct.ScmObj* %k47482, %struct.ScmObj* %argslist56887$k474822)
ret void
}

define tailcc void @proc_clo$ae48239(%struct.ScmObj* %env$ae48239,%struct.ScmObj* %current_45args56890) {
%stackaddr$prim58857 = alloca %struct.ScmObj*, align 8
%k47484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56890)
store volatile %struct.ScmObj* %k47484, %struct.ScmObj** %stackaddr$prim58857, align 8
%stackaddr$prim58858 = alloca %struct.ScmObj*, align 8
%current_45args56891 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56890)
store volatile %struct.ScmObj* %current_45args56891, %struct.ScmObj** %stackaddr$prim58858, align 8
%stackaddr$prim58859 = alloca %struct.ScmObj*, align 8
%lst47109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56891)
store volatile %struct.ScmObj* %lst47109, %struct.ScmObj** %stackaddr$prim58859, align 8
%stackaddr$prim58860 = alloca %struct.ScmObj*, align 8
%current_45args56892 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56891)
store volatile %struct.ScmObj* %current_45args56892, %struct.ScmObj** %stackaddr$prim58860, align 8
%stackaddr$prim58861 = alloca %struct.ScmObj*, align 8
%b47108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56892)
store volatile %struct.ScmObj* %b47108, %struct.ScmObj** %stackaddr$prim58861, align 8
%truthy$cmp58862 = call i64 @is_truthy_value(%struct.ScmObj* %b47108)
%cmp$cmp58862 = icmp eq i64 %truthy$cmp58862, 1
br i1 %cmp$cmp58862, label %truebranch$cmp58862, label %falsebranch$cmp58862
truebranch$cmp58862:
%ae48242 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56894$k474840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58863 = alloca %struct.ScmObj*, align 8
%argslist56894$k474841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47108, %struct.ScmObj* %argslist56894$k474840)
store volatile %struct.ScmObj* %argslist56894$k474841, %struct.ScmObj** %stackaddr$prim58863, align 8
%stackaddr$prim58864 = alloca %struct.ScmObj*, align 8
%argslist56894$k474842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48242, %struct.ScmObj* %argslist56894$k474841)
store volatile %struct.ScmObj* %argslist56894$k474842, %struct.ScmObj** %stackaddr$prim58864, align 8
%clofunc58865 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47484)
musttail call tailcc void %clofunc58865(%struct.ScmObj* %k47484, %struct.ScmObj* %argslist56894$k474842)
ret void
falsebranch$cmp58862:
%stackaddr$prim58866 = alloca %struct.ScmObj*, align 8
%cpsprim47485 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47109)
store volatile %struct.ScmObj* %cpsprim47485, %struct.ScmObj** %stackaddr$prim58866, align 8
%ae48249 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56895$k474840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58867 = alloca %struct.ScmObj*, align 8
%argslist56895$k474841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47485, %struct.ScmObj* %argslist56895$k474840)
store volatile %struct.ScmObj* %argslist56895$k474841, %struct.ScmObj** %stackaddr$prim58867, align 8
%stackaddr$prim58868 = alloca %struct.ScmObj*, align 8
%argslist56895$k474842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48249, %struct.ScmObj* %argslist56895$k474841)
store volatile %struct.ScmObj* %argslist56895$k474842, %struct.ScmObj** %stackaddr$prim58868, align 8
%clofunc58869 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47484)
musttail call tailcc void %clofunc58869(%struct.ScmObj* %k47484, %struct.ScmObj* %argslist56895$k474842)
ret void
}

define tailcc void @proc_clo$ae48196(%struct.ScmObj* %env$ae48196,%struct.ScmObj* %current_45args56899) {
%stackaddr$env-ref58870 = alloca %struct.ScmObj*, align 8
%_37take47089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48196, i64 0)
store %struct.ScmObj* %_37take47089, %struct.ScmObj** %stackaddr$env-ref58870
%stackaddr$env-ref58871 = alloca %struct.ScmObj*, align 8
%_37length47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48196, i64 1)
store %struct.ScmObj* %_37length47086, %struct.ScmObj** %stackaddr$env-ref58871
%stackaddr$prim58872 = alloca %struct.ScmObj*, align 8
%k47486 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56899)
store volatile %struct.ScmObj* %k47486, %struct.ScmObj** %stackaddr$prim58872, align 8
%stackaddr$prim58873 = alloca %struct.ScmObj*, align 8
%current_45args56900 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56899)
store volatile %struct.ScmObj* %current_45args56900, %struct.ScmObj** %stackaddr$prim58873, align 8
%stackaddr$prim58874 = alloca %struct.ScmObj*, align 8
%lst47118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56900)
store volatile %struct.ScmObj* %lst47118, %struct.ScmObj** %stackaddr$prim58874, align 8
%stackaddr$prim58875 = alloca %struct.ScmObj*, align 8
%current_45args56901 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56900)
store volatile %struct.ScmObj* %current_45args56901, %struct.ScmObj** %stackaddr$prim58875, align 8
%stackaddr$prim58876 = alloca %struct.ScmObj*, align 8
%n47117 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56901)
store volatile %struct.ScmObj* %n47117, %struct.ScmObj** %stackaddr$prim58876, align 8
%stackaddr$makeclosure58877 = alloca %struct.ScmObj*, align 8
%fptrToInt58878 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48198 to i64
%ae48198 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58878)
store volatile %struct.ScmObj* %ae48198, %struct.ScmObj** %stackaddr$makeclosure58877, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48198, %struct.ScmObj* %k47486, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48198, %struct.ScmObj* %lst47118, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48198, %struct.ScmObj* %_37take47089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48198, %struct.ScmObj* %n47117, i64 3)
%argslist56907$_37length470860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58879 = alloca %struct.ScmObj*, align 8
%argslist56907$_37length470861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47118, %struct.ScmObj* %argslist56907$_37length470860)
store volatile %struct.ScmObj* %argslist56907$_37length470861, %struct.ScmObj** %stackaddr$prim58879, align 8
%stackaddr$prim58880 = alloca %struct.ScmObj*, align 8
%argslist56907$_37length470862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48198, %struct.ScmObj* %argslist56907$_37length470861)
store volatile %struct.ScmObj* %argslist56907$_37length470862, %struct.ScmObj** %stackaddr$prim58880, align 8
%clofunc58881 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47086)
musttail call tailcc void %clofunc58881(%struct.ScmObj* %_37length47086, %struct.ScmObj* %argslist56907$_37length470862)
ret void
}

define tailcc void @proc_clo$ae48198(%struct.ScmObj* %env$ae48198,%struct.ScmObj* %current_45args56903) {
%stackaddr$env-ref58882 = alloca %struct.ScmObj*, align 8
%k47486 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48198, i64 0)
store %struct.ScmObj* %k47486, %struct.ScmObj** %stackaddr$env-ref58882
%stackaddr$env-ref58883 = alloca %struct.ScmObj*, align 8
%lst47118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48198, i64 1)
store %struct.ScmObj* %lst47118, %struct.ScmObj** %stackaddr$env-ref58883
%stackaddr$env-ref58884 = alloca %struct.ScmObj*, align 8
%_37take47089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48198, i64 2)
store %struct.ScmObj* %_37take47089, %struct.ScmObj** %stackaddr$env-ref58884
%stackaddr$env-ref58885 = alloca %struct.ScmObj*, align 8
%n47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48198, i64 3)
store %struct.ScmObj* %n47117, %struct.ScmObj** %stackaddr$env-ref58885
%stackaddr$prim58886 = alloca %struct.ScmObj*, align 8
%_95k47487 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56903)
store volatile %struct.ScmObj* %_95k47487, %struct.ScmObj** %stackaddr$prim58886, align 8
%stackaddr$prim58887 = alloca %struct.ScmObj*, align 8
%current_45args56904 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56903)
store volatile %struct.ScmObj* %current_45args56904, %struct.ScmObj** %stackaddr$prim58887, align 8
%stackaddr$prim58888 = alloca %struct.ScmObj*, align 8
%anf_45bind47239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56904)
store volatile %struct.ScmObj* %anf_45bind47239, %struct.ScmObj** %stackaddr$prim58888, align 8
%stackaddr$prim58889 = alloca %struct.ScmObj*, align 8
%anf_45bind47240 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47239, %struct.ScmObj* %n47117)
store volatile %struct.ScmObj* %anf_45bind47240, %struct.ScmObj** %stackaddr$prim58889, align 8
%argslist56906$_37take470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58890 = alloca %struct.ScmObj*, align 8
%argslist56906$_37take470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47240, %struct.ScmObj* %argslist56906$_37take470890)
store volatile %struct.ScmObj* %argslist56906$_37take470891, %struct.ScmObj** %stackaddr$prim58890, align 8
%stackaddr$prim58891 = alloca %struct.ScmObj*, align 8
%argslist56906$_37take470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47118, %struct.ScmObj* %argslist56906$_37take470891)
store volatile %struct.ScmObj* %argslist56906$_37take470892, %struct.ScmObj** %stackaddr$prim58891, align 8
%stackaddr$prim58892 = alloca %struct.ScmObj*, align 8
%argslist56906$_37take470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47486, %struct.ScmObj* %argslist56906$_37take470892)
store volatile %struct.ScmObj* %argslist56906$_37take470893, %struct.ScmObj** %stackaddr$prim58892, align 8
%clofunc58893 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47089)
musttail call tailcc void %clofunc58893(%struct.ScmObj* %_37take47089, %struct.ScmObj* %argslist56906$_37take470893)
ret void
}

define tailcc void @proc_clo$ae48142(%struct.ScmObj* %env$ae48142,%struct.ScmObj* %current_45args56909) {
%stackaddr$env-ref58894 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48142, i64 0)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref58894
%stackaddr$prim58895 = alloca %struct.ScmObj*, align 8
%k47488 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56909)
store volatile %struct.ScmObj* %k47488, %struct.ScmObj** %stackaddr$prim58895, align 8
%stackaddr$prim58896 = alloca %struct.ScmObj*, align 8
%current_45args56910 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56909)
store volatile %struct.ScmObj* %current_45args56910, %struct.ScmObj** %stackaddr$prim58896, align 8
%stackaddr$prim58897 = alloca %struct.ScmObj*, align 8
%lst47120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56910)
store volatile %struct.ScmObj* %lst47120, %struct.ScmObj** %stackaddr$prim58897, align 8
%stackaddr$makeclosure58898 = alloca %struct.ScmObj*, align 8
%fptrToInt58899 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48143 to i64
%ae48143 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58899)
store volatile %struct.ScmObj* %ae48143, %struct.ScmObj** %stackaddr$makeclosure58898, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48143, %struct.ScmObj* %k47488, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48143, %struct.ScmObj* %lst47120, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48143, %struct.ScmObj* %_37foldl147081, i64 2)
%ae48144 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58900 = alloca %struct.ScmObj*, align 8
%fptrToInt58901 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48145 to i64
%ae48145 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58901)
store volatile %struct.ScmObj* %ae48145, %struct.ScmObj** %stackaddr$makeclosure58900, align 8
%argslist56921$ae481430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58902 = alloca %struct.ScmObj*, align 8
%argslist56921$ae481431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48145, %struct.ScmObj* %argslist56921$ae481430)
store volatile %struct.ScmObj* %argslist56921$ae481431, %struct.ScmObj** %stackaddr$prim58902, align 8
%stackaddr$prim58903 = alloca %struct.ScmObj*, align 8
%argslist56921$ae481432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48144, %struct.ScmObj* %argslist56921$ae481431)
store volatile %struct.ScmObj* %argslist56921$ae481432, %struct.ScmObj** %stackaddr$prim58903, align 8
%clofunc58904 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48143)
musttail call tailcc void %clofunc58904(%struct.ScmObj* %ae48143, %struct.ScmObj* %argslist56921$ae481432)
ret void
}

define tailcc void @proc_clo$ae48143(%struct.ScmObj* %env$ae48143,%struct.ScmObj* %current_45args56912) {
%stackaddr$env-ref58905 = alloca %struct.ScmObj*, align 8
%k47488 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48143, i64 0)
store %struct.ScmObj* %k47488, %struct.ScmObj** %stackaddr$env-ref58905
%stackaddr$env-ref58906 = alloca %struct.ScmObj*, align 8
%lst47120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48143, i64 1)
store %struct.ScmObj* %lst47120, %struct.ScmObj** %stackaddr$env-ref58906
%stackaddr$env-ref58907 = alloca %struct.ScmObj*, align 8
%_37foldl147081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48143, i64 2)
store %struct.ScmObj* %_37foldl147081, %struct.ScmObj** %stackaddr$env-ref58907
%stackaddr$prim58908 = alloca %struct.ScmObj*, align 8
%_95k47489 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56912)
store volatile %struct.ScmObj* %_95k47489, %struct.ScmObj** %stackaddr$prim58908, align 8
%stackaddr$prim58909 = alloca %struct.ScmObj*, align 8
%current_45args56913 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56912)
store volatile %struct.ScmObj* %current_45args56913, %struct.ScmObj** %stackaddr$prim58909, align 8
%stackaddr$prim58910 = alloca %struct.ScmObj*, align 8
%anf_45bind47238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56913)
store volatile %struct.ScmObj* %anf_45bind47238, %struct.ScmObj** %stackaddr$prim58910, align 8
%ae48164 = call %struct.ScmObj* @const_init_null()
%argslist56915$_37foldl1470810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58911 = alloca %struct.ScmObj*, align 8
%argslist56915$_37foldl1470811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47120, %struct.ScmObj* %argslist56915$_37foldl1470810)
store volatile %struct.ScmObj* %argslist56915$_37foldl1470811, %struct.ScmObj** %stackaddr$prim58911, align 8
%stackaddr$prim58912 = alloca %struct.ScmObj*, align 8
%argslist56915$_37foldl1470812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48164, %struct.ScmObj* %argslist56915$_37foldl1470811)
store volatile %struct.ScmObj* %argslist56915$_37foldl1470812, %struct.ScmObj** %stackaddr$prim58912, align 8
%stackaddr$prim58913 = alloca %struct.ScmObj*, align 8
%argslist56915$_37foldl1470813 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47238, %struct.ScmObj* %argslist56915$_37foldl1470812)
store volatile %struct.ScmObj* %argslist56915$_37foldl1470813, %struct.ScmObj** %stackaddr$prim58913, align 8
%stackaddr$prim58914 = alloca %struct.ScmObj*, align 8
%argslist56915$_37foldl1470814 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47488, %struct.ScmObj* %argslist56915$_37foldl1470813)
store volatile %struct.ScmObj* %argslist56915$_37foldl1470814, %struct.ScmObj** %stackaddr$prim58914, align 8
%clofunc58915 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147081)
musttail call tailcc void %clofunc58915(%struct.ScmObj* %_37foldl147081, %struct.ScmObj* %argslist56915$_37foldl1470814)
ret void
}

define tailcc void @proc_clo$ae48145(%struct.ScmObj* %env$ae48145,%struct.ScmObj* %current_45args56916) {
%stackaddr$prim58916 = alloca %struct.ScmObj*, align 8
%k47490 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56916)
store volatile %struct.ScmObj* %k47490, %struct.ScmObj** %stackaddr$prim58916, align 8
%stackaddr$prim58917 = alloca %struct.ScmObj*, align 8
%current_45args56917 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56916)
store volatile %struct.ScmObj* %current_45args56917, %struct.ScmObj** %stackaddr$prim58917, align 8
%stackaddr$prim58918 = alloca %struct.ScmObj*, align 8
%x47122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56917)
store volatile %struct.ScmObj* %x47122, %struct.ScmObj** %stackaddr$prim58918, align 8
%stackaddr$prim58919 = alloca %struct.ScmObj*, align 8
%current_45args56918 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56917)
store volatile %struct.ScmObj* %current_45args56918, %struct.ScmObj** %stackaddr$prim58919, align 8
%stackaddr$prim58920 = alloca %struct.ScmObj*, align 8
%y47121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56918)
store volatile %struct.ScmObj* %y47121, %struct.ScmObj** %stackaddr$prim58920, align 8
%ae48147 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56920$k474900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58921 = alloca %struct.ScmObj*, align 8
%argslist56920$k474901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x47122, %struct.ScmObj* %argslist56920$k474900)
store volatile %struct.ScmObj* %argslist56920$k474901, %struct.ScmObj** %stackaddr$prim58921, align 8
%stackaddr$prim58922 = alloca %struct.ScmObj*, align 8
%argslist56920$k474902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48147, %struct.ScmObj* %argslist56920$k474901)
store volatile %struct.ScmObj* %argslist56920$k474902, %struct.ScmObj** %stackaddr$prim58922, align 8
%clofunc58923 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47490)
musttail call tailcc void %clofunc58923(%struct.ScmObj* %k47490, %struct.ScmObj* %argslist56920$k474902)
ret void
}

define tailcc void @proc_clo$ae48063(%struct.ScmObj* %env$ae48063,%struct.ScmObj* %current_45args56924) {
%stackaddr$prim58924 = alloca %struct.ScmObj*, align 8
%k47491 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56924)
store volatile %struct.ScmObj* %k47491, %struct.ScmObj** %stackaddr$prim58924, align 8
%stackaddr$prim58925 = alloca %struct.ScmObj*, align 8
%current_45args56925 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56924)
store volatile %struct.ScmObj* %current_45args56925, %struct.ScmObj** %stackaddr$prim58925, align 8
%stackaddr$prim58926 = alloca %struct.ScmObj*, align 8
%_37foldl147082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56925)
store volatile %struct.ScmObj* %_37foldl147082, %struct.ScmObj** %stackaddr$prim58926, align 8
%ae48065 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58927 = alloca %struct.ScmObj*, align 8
%fptrToInt58928 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48066 to i64
%ae48066 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58928)
store volatile %struct.ScmObj* %ae48066, %struct.ScmObj** %stackaddr$makeclosure58927, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48066, %struct.ScmObj* %_37foldl147082, i64 0)
%argslist56938$k474910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58929 = alloca %struct.ScmObj*, align 8
%argslist56938$k474911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48066, %struct.ScmObj* %argslist56938$k474910)
store volatile %struct.ScmObj* %argslist56938$k474911, %struct.ScmObj** %stackaddr$prim58929, align 8
%stackaddr$prim58930 = alloca %struct.ScmObj*, align 8
%argslist56938$k474912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48065, %struct.ScmObj* %argslist56938$k474911)
store volatile %struct.ScmObj* %argslist56938$k474912, %struct.ScmObj** %stackaddr$prim58930, align 8
%clofunc58931 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47491)
musttail call tailcc void %clofunc58931(%struct.ScmObj* %k47491, %struct.ScmObj* %argslist56938$k474912)
ret void
}

define tailcc void @proc_clo$ae48066(%struct.ScmObj* %env$ae48066,%struct.ScmObj* %current_45args56927) {
%stackaddr$env-ref58932 = alloca %struct.ScmObj*, align 8
%_37foldl147082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48066, i64 0)
store %struct.ScmObj* %_37foldl147082, %struct.ScmObj** %stackaddr$env-ref58932
%stackaddr$prim58933 = alloca %struct.ScmObj*, align 8
%k47492 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56927)
store volatile %struct.ScmObj* %k47492, %struct.ScmObj** %stackaddr$prim58933, align 8
%stackaddr$prim58934 = alloca %struct.ScmObj*, align 8
%current_45args56928 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56927)
store volatile %struct.ScmObj* %current_45args56928, %struct.ScmObj** %stackaddr$prim58934, align 8
%stackaddr$prim58935 = alloca %struct.ScmObj*, align 8
%f47085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56928)
store volatile %struct.ScmObj* %f47085, %struct.ScmObj** %stackaddr$prim58935, align 8
%stackaddr$prim58936 = alloca %struct.ScmObj*, align 8
%current_45args56929 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56928)
store volatile %struct.ScmObj* %current_45args56929, %struct.ScmObj** %stackaddr$prim58936, align 8
%stackaddr$prim58937 = alloca %struct.ScmObj*, align 8
%acc47084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56929)
store volatile %struct.ScmObj* %acc47084, %struct.ScmObj** %stackaddr$prim58937, align 8
%stackaddr$prim58938 = alloca %struct.ScmObj*, align 8
%current_45args56930 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56929)
store volatile %struct.ScmObj* %current_45args56930, %struct.ScmObj** %stackaddr$prim58938, align 8
%stackaddr$prim58939 = alloca %struct.ScmObj*, align 8
%lst47083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56930)
store volatile %struct.ScmObj* %lst47083, %struct.ScmObj** %stackaddr$prim58939, align 8
%stackaddr$prim58940 = alloca %struct.ScmObj*, align 8
%anf_45bind47233 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47083)
store volatile %struct.ScmObj* %anf_45bind47233, %struct.ScmObj** %stackaddr$prim58940, align 8
%truthy$cmp58941 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47233)
%cmp$cmp58941 = icmp eq i64 %truthy$cmp58941, 1
br i1 %cmp$cmp58941, label %truebranch$cmp58941, label %falsebranch$cmp58941
truebranch$cmp58941:
%ae48070 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56932$k474920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58942 = alloca %struct.ScmObj*, align 8
%argslist56932$k474921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47084, %struct.ScmObj* %argslist56932$k474920)
store volatile %struct.ScmObj* %argslist56932$k474921, %struct.ScmObj** %stackaddr$prim58942, align 8
%stackaddr$prim58943 = alloca %struct.ScmObj*, align 8
%argslist56932$k474922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48070, %struct.ScmObj* %argslist56932$k474921)
store volatile %struct.ScmObj* %argslist56932$k474922, %struct.ScmObj** %stackaddr$prim58943, align 8
%clofunc58944 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47492)
musttail call tailcc void %clofunc58944(%struct.ScmObj* %k47492, %struct.ScmObj* %argslist56932$k474922)
ret void
falsebranch$cmp58941:
%stackaddr$prim58945 = alloca %struct.ScmObj*, align 8
%anf_45bind47234 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47083)
store volatile %struct.ScmObj* %anf_45bind47234, %struct.ScmObj** %stackaddr$prim58945, align 8
%stackaddr$makeclosure58946 = alloca %struct.ScmObj*, align 8
%fptrToInt58947 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48077 to i64
%ae48077 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58947)
store volatile %struct.ScmObj* %ae48077, %struct.ScmObj** %stackaddr$makeclosure58946, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48077, %struct.ScmObj* %k47492, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48077, %struct.ScmObj* %f47085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48077, %struct.ScmObj* %lst47083, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48077, %struct.ScmObj* %_37foldl147082, i64 3)
%argslist56937$f470850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58948 = alloca %struct.ScmObj*, align 8
%argslist56937$f470851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47084, %struct.ScmObj* %argslist56937$f470850)
store volatile %struct.ScmObj* %argslist56937$f470851, %struct.ScmObj** %stackaddr$prim58948, align 8
%stackaddr$prim58949 = alloca %struct.ScmObj*, align 8
%argslist56937$f470852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47234, %struct.ScmObj* %argslist56937$f470851)
store volatile %struct.ScmObj* %argslist56937$f470852, %struct.ScmObj** %stackaddr$prim58949, align 8
%stackaddr$prim58950 = alloca %struct.ScmObj*, align 8
%argslist56937$f470853 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48077, %struct.ScmObj* %argslist56937$f470852)
store volatile %struct.ScmObj* %argslist56937$f470853, %struct.ScmObj** %stackaddr$prim58950, align 8
%clofunc58951 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47085)
musttail call tailcc void %clofunc58951(%struct.ScmObj* %f47085, %struct.ScmObj* %argslist56937$f470853)
ret void
}

define tailcc void @proc_clo$ae48077(%struct.ScmObj* %env$ae48077,%struct.ScmObj* %current_45args56933) {
%stackaddr$env-ref58952 = alloca %struct.ScmObj*, align 8
%k47492 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48077, i64 0)
store %struct.ScmObj* %k47492, %struct.ScmObj** %stackaddr$env-ref58952
%stackaddr$env-ref58953 = alloca %struct.ScmObj*, align 8
%f47085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48077, i64 1)
store %struct.ScmObj* %f47085, %struct.ScmObj** %stackaddr$env-ref58953
%stackaddr$env-ref58954 = alloca %struct.ScmObj*, align 8
%lst47083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48077, i64 2)
store %struct.ScmObj* %lst47083, %struct.ScmObj** %stackaddr$env-ref58954
%stackaddr$env-ref58955 = alloca %struct.ScmObj*, align 8
%_37foldl147082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48077, i64 3)
store %struct.ScmObj* %_37foldl147082, %struct.ScmObj** %stackaddr$env-ref58955
%stackaddr$prim58956 = alloca %struct.ScmObj*, align 8
%_95k47493 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56933)
store volatile %struct.ScmObj* %_95k47493, %struct.ScmObj** %stackaddr$prim58956, align 8
%stackaddr$prim58957 = alloca %struct.ScmObj*, align 8
%current_45args56934 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56933)
store volatile %struct.ScmObj* %current_45args56934, %struct.ScmObj** %stackaddr$prim58957, align 8
%stackaddr$prim58958 = alloca %struct.ScmObj*, align 8
%anf_45bind47235 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56934)
store volatile %struct.ScmObj* %anf_45bind47235, %struct.ScmObj** %stackaddr$prim58958, align 8
%stackaddr$prim58959 = alloca %struct.ScmObj*, align 8
%anf_45bind47236 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47083)
store volatile %struct.ScmObj* %anf_45bind47236, %struct.ScmObj** %stackaddr$prim58959, align 8
%argslist56936$_37foldl1470820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58960 = alloca %struct.ScmObj*, align 8
%argslist56936$_37foldl1470821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47236, %struct.ScmObj* %argslist56936$_37foldl1470820)
store volatile %struct.ScmObj* %argslist56936$_37foldl1470821, %struct.ScmObj** %stackaddr$prim58960, align 8
%stackaddr$prim58961 = alloca %struct.ScmObj*, align 8
%argslist56936$_37foldl1470822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47235, %struct.ScmObj* %argslist56936$_37foldl1470821)
store volatile %struct.ScmObj* %argslist56936$_37foldl1470822, %struct.ScmObj** %stackaddr$prim58961, align 8
%stackaddr$prim58962 = alloca %struct.ScmObj*, align 8
%argslist56936$_37foldl1470823 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47085, %struct.ScmObj* %argslist56936$_37foldl1470822)
store volatile %struct.ScmObj* %argslist56936$_37foldl1470823, %struct.ScmObj** %stackaddr$prim58962, align 8
%stackaddr$prim58963 = alloca %struct.ScmObj*, align 8
%argslist56936$_37foldl1470824 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47492, %struct.ScmObj* %argslist56936$_37foldl1470823)
store volatile %struct.ScmObj* %argslist56936$_37foldl1470824, %struct.ScmObj** %stackaddr$prim58963, align 8
%clofunc58964 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147082)
musttail call tailcc void %clofunc58964(%struct.ScmObj* %_37foldl147082, %struct.ScmObj* %argslist56936$_37foldl1470824)
ret void
}

define tailcc void @proc_clo$ae47980(%struct.ScmObj* %env$ae47980,%struct.ScmObj* %current_45args56941) {
%stackaddr$prim58965 = alloca %struct.ScmObj*, align 8
%k47494 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56941)
store volatile %struct.ScmObj* %k47494, %struct.ScmObj** %stackaddr$prim58965, align 8
%stackaddr$prim58966 = alloca %struct.ScmObj*, align 8
%current_45args56942 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56941)
store volatile %struct.ScmObj* %current_45args56942, %struct.ScmObj** %stackaddr$prim58966, align 8
%stackaddr$prim58967 = alloca %struct.ScmObj*, align 8
%_37length47087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56942)
store volatile %struct.ScmObj* %_37length47087, %struct.ScmObj** %stackaddr$prim58967, align 8
%ae47982 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58968 = alloca %struct.ScmObj*, align 8
%fptrToInt58969 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47983 to i64
%ae47983 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58969)
store volatile %struct.ScmObj* %ae47983, %struct.ScmObj** %stackaddr$makeclosure58968, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47983, %struct.ScmObj* %_37length47087, i64 0)
%argslist56953$k474940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58970 = alloca %struct.ScmObj*, align 8
%argslist56953$k474941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47983, %struct.ScmObj* %argslist56953$k474940)
store volatile %struct.ScmObj* %argslist56953$k474941, %struct.ScmObj** %stackaddr$prim58970, align 8
%stackaddr$prim58971 = alloca %struct.ScmObj*, align 8
%argslist56953$k474942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47982, %struct.ScmObj* %argslist56953$k474941)
store volatile %struct.ScmObj* %argslist56953$k474942, %struct.ScmObj** %stackaddr$prim58971, align 8
%clofunc58972 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47494)
musttail call tailcc void %clofunc58972(%struct.ScmObj* %k47494, %struct.ScmObj* %argslist56953$k474942)
ret void
}

define tailcc void @proc_clo$ae47983(%struct.ScmObj* %env$ae47983,%struct.ScmObj* %current_45args56944) {
%stackaddr$env-ref58973 = alloca %struct.ScmObj*, align 8
%_37length47087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47983, i64 0)
store %struct.ScmObj* %_37length47087, %struct.ScmObj** %stackaddr$env-ref58973
%stackaddr$prim58974 = alloca %struct.ScmObj*, align 8
%k47495 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56944)
store volatile %struct.ScmObj* %k47495, %struct.ScmObj** %stackaddr$prim58974, align 8
%stackaddr$prim58975 = alloca %struct.ScmObj*, align 8
%current_45args56945 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56944)
store volatile %struct.ScmObj* %current_45args56945, %struct.ScmObj** %stackaddr$prim58975, align 8
%stackaddr$prim58976 = alloca %struct.ScmObj*, align 8
%lst47088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56945)
store volatile %struct.ScmObj* %lst47088, %struct.ScmObj** %stackaddr$prim58976, align 8
%stackaddr$prim58977 = alloca %struct.ScmObj*, align 8
%anf_45bind47229 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47088)
store volatile %struct.ScmObj* %anf_45bind47229, %struct.ScmObj** %stackaddr$prim58977, align 8
%truthy$cmp58978 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47229)
%cmp$cmp58978 = icmp eq i64 %truthy$cmp58978, 1
br i1 %cmp$cmp58978, label %truebranch$cmp58978, label %falsebranch$cmp58978
truebranch$cmp58978:
%ae47987 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47988 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56947$k474950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58979 = alloca %struct.ScmObj*, align 8
%argslist56947$k474951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47988, %struct.ScmObj* %argslist56947$k474950)
store volatile %struct.ScmObj* %argslist56947$k474951, %struct.ScmObj** %stackaddr$prim58979, align 8
%stackaddr$prim58980 = alloca %struct.ScmObj*, align 8
%argslist56947$k474952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47987, %struct.ScmObj* %argslist56947$k474951)
store volatile %struct.ScmObj* %argslist56947$k474952, %struct.ScmObj** %stackaddr$prim58980, align 8
%clofunc58981 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47495)
musttail call tailcc void %clofunc58981(%struct.ScmObj* %k47495, %struct.ScmObj* %argslist56947$k474952)
ret void
falsebranch$cmp58978:
%stackaddr$prim58982 = alloca %struct.ScmObj*, align 8
%anf_45bind47230 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47088)
store volatile %struct.ScmObj* %anf_45bind47230, %struct.ScmObj** %stackaddr$prim58982, align 8
%stackaddr$makeclosure58983 = alloca %struct.ScmObj*, align 8
%fptrToInt58984 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47997 to i64
%ae47997 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58984)
store volatile %struct.ScmObj* %ae47997, %struct.ScmObj** %stackaddr$makeclosure58983, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47997, %struct.ScmObj* %k47495, i64 0)
%argslist56952$_37length470870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58985 = alloca %struct.ScmObj*, align 8
%argslist56952$_37length470871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47230, %struct.ScmObj* %argslist56952$_37length470870)
store volatile %struct.ScmObj* %argslist56952$_37length470871, %struct.ScmObj** %stackaddr$prim58985, align 8
%stackaddr$prim58986 = alloca %struct.ScmObj*, align 8
%argslist56952$_37length470872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47997, %struct.ScmObj* %argslist56952$_37length470871)
store volatile %struct.ScmObj* %argslist56952$_37length470872, %struct.ScmObj** %stackaddr$prim58986, align 8
%clofunc58987 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47087)
musttail call tailcc void %clofunc58987(%struct.ScmObj* %_37length47087, %struct.ScmObj* %argslist56952$_37length470872)
ret void
}

define tailcc void @proc_clo$ae47997(%struct.ScmObj* %env$ae47997,%struct.ScmObj* %current_45args56948) {
%stackaddr$env-ref58988 = alloca %struct.ScmObj*, align 8
%k47495 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47997, i64 0)
store %struct.ScmObj* %k47495, %struct.ScmObj** %stackaddr$env-ref58988
%stackaddr$prim58989 = alloca %struct.ScmObj*, align 8
%_95k47496 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56948)
store volatile %struct.ScmObj* %_95k47496, %struct.ScmObj** %stackaddr$prim58989, align 8
%stackaddr$prim58990 = alloca %struct.ScmObj*, align 8
%current_45args56949 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56948)
store volatile %struct.ScmObj* %current_45args56949, %struct.ScmObj** %stackaddr$prim58990, align 8
%stackaddr$prim58991 = alloca %struct.ScmObj*, align 8
%anf_45bind47231 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56949)
store volatile %struct.ScmObj* %anf_45bind47231, %struct.ScmObj** %stackaddr$prim58991, align 8
%ae47999 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58992 = alloca %struct.ScmObj*, align 8
%cpsprim47497 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae47999, %struct.ScmObj* %anf_45bind47231)
store volatile %struct.ScmObj* %cpsprim47497, %struct.ScmObj** %stackaddr$prim58992, align 8
%ae48002 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56951$k474950 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58993 = alloca %struct.ScmObj*, align 8
%argslist56951$k474951 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47497, %struct.ScmObj* %argslist56951$k474950)
store volatile %struct.ScmObj* %argslist56951$k474951, %struct.ScmObj** %stackaddr$prim58993, align 8
%stackaddr$prim58994 = alloca %struct.ScmObj*, align 8
%argslist56951$k474952 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48002, %struct.ScmObj* %argslist56951$k474951)
store volatile %struct.ScmObj* %argslist56951$k474952, %struct.ScmObj** %stackaddr$prim58994, align 8
%clofunc58995 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47495)
musttail call tailcc void %clofunc58995(%struct.ScmObj* %k47495, %struct.ScmObj* %argslist56951$k474952)
ret void
}

define tailcc void @proc_clo$ae47830(%struct.ScmObj* %env$ae47830,%struct.ScmObj* %current_45args56956) {
%stackaddr$prim58996 = alloca %struct.ScmObj*, align 8
%k47498 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56956)
store volatile %struct.ScmObj* %k47498, %struct.ScmObj** %stackaddr$prim58996, align 8
%stackaddr$prim58997 = alloca %struct.ScmObj*, align 8
%current_45args56957 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56956)
store volatile %struct.ScmObj* %current_45args56957, %struct.ScmObj** %stackaddr$prim58997, align 8
%stackaddr$prim58998 = alloca %struct.ScmObj*, align 8
%_37take47090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56957)
store volatile %struct.ScmObj* %_37take47090, %struct.ScmObj** %stackaddr$prim58998, align 8
%ae47832 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58999 = alloca %struct.ScmObj*, align 8
%fptrToInt59000 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47833 to i64
%ae47833 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt59000)
store volatile %struct.ScmObj* %ae47833, %struct.ScmObj** %stackaddr$makeclosure58999, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47833, %struct.ScmObj* %_37take47090, i64 0)
%argslist56970$k474980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59001 = alloca %struct.ScmObj*, align 8
%argslist56970$k474981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47833, %struct.ScmObj* %argslist56970$k474980)
store volatile %struct.ScmObj* %argslist56970$k474981, %struct.ScmObj** %stackaddr$prim59001, align 8
%stackaddr$prim59002 = alloca %struct.ScmObj*, align 8
%argslist56970$k474982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47832, %struct.ScmObj* %argslist56970$k474981)
store volatile %struct.ScmObj* %argslist56970$k474982, %struct.ScmObj** %stackaddr$prim59002, align 8
%clofunc59003 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47498)
musttail call tailcc void %clofunc59003(%struct.ScmObj* %k47498, %struct.ScmObj* %argslist56970$k474982)
ret void
}

define tailcc void @proc_clo$ae47833(%struct.ScmObj* %env$ae47833,%struct.ScmObj* %current_45args56959) {
%stackaddr$env-ref59004 = alloca %struct.ScmObj*, align 8
%_37take47090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47833, i64 0)
store %struct.ScmObj* %_37take47090, %struct.ScmObj** %stackaddr$env-ref59004
%stackaddr$prim59005 = alloca %struct.ScmObj*, align 8
%k47499 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56959)
store volatile %struct.ScmObj* %k47499, %struct.ScmObj** %stackaddr$prim59005, align 8
%stackaddr$prim59006 = alloca %struct.ScmObj*, align 8
%current_45args56960 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56959)
store volatile %struct.ScmObj* %current_45args56960, %struct.ScmObj** %stackaddr$prim59006, align 8
%stackaddr$prim59007 = alloca %struct.ScmObj*, align 8
%lst47092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56960)
store volatile %struct.ScmObj* %lst47092, %struct.ScmObj** %stackaddr$prim59007, align 8
%stackaddr$prim59008 = alloca %struct.ScmObj*, align 8
%current_45args56961 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56960)
store volatile %struct.ScmObj* %current_45args56961, %struct.ScmObj** %stackaddr$prim59008, align 8
%stackaddr$prim59009 = alloca %struct.ScmObj*, align 8
%n47091 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56961)
store volatile %struct.ScmObj* %n47091, %struct.ScmObj** %stackaddr$prim59009, align 8
%ae47835 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59010 = alloca %struct.ScmObj*, align 8
%anf_45bind47222 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n47091, %struct.ScmObj* %ae47835)
store volatile %struct.ScmObj* %anf_45bind47222, %struct.ScmObj** %stackaddr$prim59010, align 8
%truthy$cmp59011 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47222)
%cmp$cmp59011 = icmp eq i64 %truthy$cmp59011, 1
br i1 %cmp$cmp59011, label %truebranch$cmp59011, label %falsebranch$cmp59011
truebranch$cmp59011:
%ae47838 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47839 = call %struct.ScmObj* @const_init_null()
%argslist56963$k474990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59012 = alloca %struct.ScmObj*, align 8
%argslist56963$k474991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47839, %struct.ScmObj* %argslist56963$k474990)
store volatile %struct.ScmObj* %argslist56963$k474991, %struct.ScmObj** %stackaddr$prim59012, align 8
%stackaddr$prim59013 = alloca %struct.ScmObj*, align 8
%argslist56963$k474992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47838, %struct.ScmObj* %argslist56963$k474991)
store volatile %struct.ScmObj* %argslist56963$k474992, %struct.ScmObj** %stackaddr$prim59013, align 8
%clofunc59014 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47499)
musttail call tailcc void %clofunc59014(%struct.ScmObj* %k47499, %struct.ScmObj* %argslist56963$k474992)
ret void
falsebranch$cmp59011:
%stackaddr$prim59015 = alloca %struct.ScmObj*, align 8
%anf_45bind47223 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47092)
store volatile %struct.ScmObj* %anf_45bind47223, %struct.ScmObj** %stackaddr$prim59015, align 8
%truthy$cmp59016 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47223)
%cmp$cmp59016 = icmp eq i64 %truthy$cmp59016, 1
br i1 %cmp$cmp59016, label %truebranch$cmp59016, label %falsebranch$cmp59016
truebranch$cmp59016:
%ae47849 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47850 = call %struct.ScmObj* @const_init_null()
%argslist56964$k474990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59017 = alloca %struct.ScmObj*, align 8
%argslist56964$k474991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47850, %struct.ScmObj* %argslist56964$k474990)
store volatile %struct.ScmObj* %argslist56964$k474991, %struct.ScmObj** %stackaddr$prim59017, align 8
%stackaddr$prim59018 = alloca %struct.ScmObj*, align 8
%argslist56964$k474992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47849, %struct.ScmObj* %argslist56964$k474991)
store volatile %struct.ScmObj* %argslist56964$k474992, %struct.ScmObj** %stackaddr$prim59018, align 8
%clofunc59019 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47499)
musttail call tailcc void %clofunc59019(%struct.ScmObj* %k47499, %struct.ScmObj* %argslist56964$k474992)
ret void
falsebranch$cmp59016:
%stackaddr$prim59020 = alloca %struct.ScmObj*, align 8
%anf_45bind47224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47092)
store volatile %struct.ScmObj* %anf_45bind47224, %struct.ScmObj** %stackaddr$prim59020, align 8
%stackaddr$prim59021 = alloca %struct.ScmObj*, align 8
%anf_45bind47225 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47092)
store volatile %struct.ScmObj* %anf_45bind47225, %struct.ScmObj** %stackaddr$prim59021, align 8
%ae47860 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim59022 = alloca %struct.ScmObj*, align 8
%anf_45bind47226 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n47091, %struct.ScmObj* %ae47860)
store volatile %struct.ScmObj* %anf_45bind47226, %struct.ScmObj** %stackaddr$prim59022, align 8
%stackaddr$makeclosure59023 = alloca %struct.ScmObj*, align 8
%fptrToInt59024 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47862 to i64
%ae47862 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59024)
store volatile %struct.ScmObj* %ae47862, %struct.ScmObj** %stackaddr$makeclosure59023, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47862, %struct.ScmObj* %k47499, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47862, %struct.ScmObj* %anf_45bind47224, i64 1)
%argslist56969$_37take470900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59025 = alloca %struct.ScmObj*, align 8
%argslist56969$_37take470901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47226, %struct.ScmObj* %argslist56969$_37take470900)
store volatile %struct.ScmObj* %argslist56969$_37take470901, %struct.ScmObj** %stackaddr$prim59025, align 8
%stackaddr$prim59026 = alloca %struct.ScmObj*, align 8
%argslist56969$_37take470902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47225, %struct.ScmObj* %argslist56969$_37take470901)
store volatile %struct.ScmObj* %argslist56969$_37take470902, %struct.ScmObj** %stackaddr$prim59026, align 8
%stackaddr$prim59027 = alloca %struct.ScmObj*, align 8
%argslist56969$_37take470903 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47862, %struct.ScmObj* %argslist56969$_37take470902)
store volatile %struct.ScmObj* %argslist56969$_37take470903, %struct.ScmObj** %stackaddr$prim59027, align 8
%clofunc59028 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47090)
musttail call tailcc void %clofunc59028(%struct.ScmObj* %_37take47090, %struct.ScmObj* %argslist56969$_37take470903)
ret void
}

define tailcc void @proc_clo$ae47862(%struct.ScmObj* %env$ae47862,%struct.ScmObj* %current_45args56965) {
%stackaddr$env-ref59029 = alloca %struct.ScmObj*, align 8
%k47499 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47862, i64 0)
store %struct.ScmObj* %k47499, %struct.ScmObj** %stackaddr$env-ref59029
%stackaddr$env-ref59030 = alloca %struct.ScmObj*, align 8
%anf_45bind47224 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47862, i64 1)
store %struct.ScmObj* %anf_45bind47224, %struct.ScmObj** %stackaddr$env-ref59030
%stackaddr$prim59031 = alloca %struct.ScmObj*, align 8
%_95k47500 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56965)
store volatile %struct.ScmObj* %_95k47500, %struct.ScmObj** %stackaddr$prim59031, align 8
%stackaddr$prim59032 = alloca %struct.ScmObj*, align 8
%current_45args56966 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56965)
store volatile %struct.ScmObj* %current_45args56966, %struct.ScmObj** %stackaddr$prim59032, align 8
%stackaddr$prim59033 = alloca %struct.ScmObj*, align 8
%anf_45bind47227 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56966)
store volatile %struct.ScmObj* %anf_45bind47227, %struct.ScmObj** %stackaddr$prim59033, align 8
%stackaddr$prim59034 = alloca %struct.ScmObj*, align 8
%cpsprim47501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47224, %struct.ScmObj* %anf_45bind47227)
store volatile %struct.ScmObj* %cpsprim47501, %struct.ScmObj** %stackaddr$prim59034, align 8
%ae47868 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56968$k474990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59035 = alloca %struct.ScmObj*, align 8
%argslist56968$k474991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47501, %struct.ScmObj* %argslist56968$k474990)
store volatile %struct.ScmObj* %argslist56968$k474991, %struct.ScmObj** %stackaddr$prim59035, align 8
%stackaddr$prim59036 = alloca %struct.ScmObj*, align 8
%argslist56968$k474992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47868, %struct.ScmObj* %argslist56968$k474991)
store volatile %struct.ScmObj* %argslist56968$k474992, %struct.ScmObj** %stackaddr$prim59036, align 8
%clofunc59037 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47499)
musttail call tailcc void %clofunc59037(%struct.ScmObj* %k47499, %struct.ScmObj* %argslist56968$k474992)
ret void
}

define tailcc void @proc_clo$ae47733(%struct.ScmObj* %env$ae47733,%struct.ScmObj* %current_45args56973) {
%stackaddr$prim59038 = alloca %struct.ScmObj*, align 8
%k47502 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56973)
store volatile %struct.ScmObj* %k47502, %struct.ScmObj** %stackaddr$prim59038, align 8
%stackaddr$prim59039 = alloca %struct.ScmObj*, align 8
%current_45args56974 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56973)
store volatile %struct.ScmObj* %current_45args56974, %struct.ScmObj** %stackaddr$prim59039, align 8
%stackaddr$prim59040 = alloca %struct.ScmObj*, align 8
%_37map47094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56974)
store volatile %struct.ScmObj* %_37map47094, %struct.ScmObj** %stackaddr$prim59040, align 8
%ae47735 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59041 = alloca %struct.ScmObj*, align 8
%fptrToInt59042 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47736 to i64
%ae47736 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt59042)
store volatile %struct.ScmObj* %ae47736, %struct.ScmObj** %stackaddr$makeclosure59041, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47736, %struct.ScmObj* %_37map47094, i64 0)
%argslist56990$k475020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59043 = alloca %struct.ScmObj*, align 8
%argslist56990$k475021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47736, %struct.ScmObj* %argslist56990$k475020)
store volatile %struct.ScmObj* %argslist56990$k475021, %struct.ScmObj** %stackaddr$prim59043, align 8
%stackaddr$prim59044 = alloca %struct.ScmObj*, align 8
%argslist56990$k475022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47735, %struct.ScmObj* %argslist56990$k475021)
store volatile %struct.ScmObj* %argslist56990$k475022, %struct.ScmObj** %stackaddr$prim59044, align 8
%clofunc59045 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47502)
musttail call tailcc void %clofunc59045(%struct.ScmObj* %k47502, %struct.ScmObj* %argslist56990$k475022)
ret void
}

define tailcc void @proc_clo$ae47736(%struct.ScmObj* %env$ae47736,%struct.ScmObj* %current_45args56976) {
%stackaddr$env-ref59046 = alloca %struct.ScmObj*, align 8
%_37map47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47736, i64 0)
store %struct.ScmObj* %_37map47094, %struct.ScmObj** %stackaddr$env-ref59046
%stackaddr$prim59047 = alloca %struct.ScmObj*, align 8
%k47503 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56976)
store volatile %struct.ScmObj* %k47503, %struct.ScmObj** %stackaddr$prim59047, align 8
%stackaddr$prim59048 = alloca %struct.ScmObj*, align 8
%current_45args56977 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56976)
store volatile %struct.ScmObj* %current_45args56977, %struct.ScmObj** %stackaddr$prim59048, align 8
%stackaddr$prim59049 = alloca %struct.ScmObj*, align 8
%f47096 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56977)
store volatile %struct.ScmObj* %f47096, %struct.ScmObj** %stackaddr$prim59049, align 8
%stackaddr$prim59050 = alloca %struct.ScmObj*, align 8
%current_45args56978 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56977)
store volatile %struct.ScmObj* %current_45args56978, %struct.ScmObj** %stackaddr$prim59050, align 8
%stackaddr$prim59051 = alloca %struct.ScmObj*, align 8
%lst47095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56978)
store volatile %struct.ScmObj* %lst47095, %struct.ScmObj** %stackaddr$prim59051, align 8
%stackaddr$prim59052 = alloca %struct.ScmObj*, align 8
%anf_45bind47216 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47095)
store volatile %struct.ScmObj* %anf_45bind47216, %struct.ScmObj** %stackaddr$prim59052, align 8
%truthy$cmp59053 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47216)
%cmp$cmp59053 = icmp eq i64 %truthy$cmp59053, 1
br i1 %cmp$cmp59053, label %truebranch$cmp59053, label %falsebranch$cmp59053
truebranch$cmp59053:
%ae47740 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47741 = call %struct.ScmObj* @const_init_null()
%argslist56980$k475030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59054 = alloca %struct.ScmObj*, align 8
%argslist56980$k475031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47741, %struct.ScmObj* %argslist56980$k475030)
store volatile %struct.ScmObj* %argslist56980$k475031, %struct.ScmObj** %stackaddr$prim59054, align 8
%stackaddr$prim59055 = alloca %struct.ScmObj*, align 8
%argslist56980$k475032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47740, %struct.ScmObj* %argslist56980$k475031)
store volatile %struct.ScmObj* %argslist56980$k475032, %struct.ScmObj** %stackaddr$prim59055, align 8
%clofunc59056 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47503)
musttail call tailcc void %clofunc59056(%struct.ScmObj* %k47503, %struct.ScmObj* %argslist56980$k475032)
ret void
falsebranch$cmp59053:
%stackaddr$prim59057 = alloca %struct.ScmObj*, align 8
%anf_45bind47217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47095)
store volatile %struct.ScmObj* %anf_45bind47217, %struct.ScmObj** %stackaddr$prim59057, align 8
%stackaddr$makeclosure59058 = alloca %struct.ScmObj*, align 8
%fptrToInt59059 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47750 to i64
%ae47750 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt59059)
store volatile %struct.ScmObj* %ae47750, %struct.ScmObj** %stackaddr$makeclosure59058, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47750, %struct.ScmObj* %_37map47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47750, %struct.ScmObj* %k47503, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47750, %struct.ScmObj* %f47096, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47750, %struct.ScmObj* %lst47095, i64 3)
%argslist56989$f470960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59060 = alloca %struct.ScmObj*, align 8
%argslist56989$f470961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47217, %struct.ScmObj* %argslist56989$f470960)
store volatile %struct.ScmObj* %argslist56989$f470961, %struct.ScmObj** %stackaddr$prim59060, align 8
%stackaddr$prim59061 = alloca %struct.ScmObj*, align 8
%argslist56989$f470962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47750, %struct.ScmObj* %argslist56989$f470961)
store volatile %struct.ScmObj* %argslist56989$f470962, %struct.ScmObj** %stackaddr$prim59061, align 8
%clofunc59062 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47096)
musttail call tailcc void %clofunc59062(%struct.ScmObj* %f47096, %struct.ScmObj* %argslist56989$f470962)
ret void
}

define tailcc void @proc_clo$ae47750(%struct.ScmObj* %env$ae47750,%struct.ScmObj* %current_45args56981) {
%stackaddr$env-ref59063 = alloca %struct.ScmObj*, align 8
%_37map47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47750, i64 0)
store %struct.ScmObj* %_37map47094, %struct.ScmObj** %stackaddr$env-ref59063
%stackaddr$env-ref59064 = alloca %struct.ScmObj*, align 8
%k47503 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47750, i64 1)
store %struct.ScmObj* %k47503, %struct.ScmObj** %stackaddr$env-ref59064
%stackaddr$env-ref59065 = alloca %struct.ScmObj*, align 8
%f47096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47750, i64 2)
store %struct.ScmObj* %f47096, %struct.ScmObj** %stackaddr$env-ref59065
%stackaddr$env-ref59066 = alloca %struct.ScmObj*, align 8
%lst47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47750, i64 3)
store %struct.ScmObj* %lst47095, %struct.ScmObj** %stackaddr$env-ref59066
%stackaddr$prim59067 = alloca %struct.ScmObj*, align 8
%_95k47504 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56981)
store volatile %struct.ScmObj* %_95k47504, %struct.ScmObj** %stackaddr$prim59067, align 8
%stackaddr$prim59068 = alloca %struct.ScmObj*, align 8
%current_45args56982 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56981)
store volatile %struct.ScmObj* %current_45args56982, %struct.ScmObj** %stackaddr$prim59068, align 8
%stackaddr$prim59069 = alloca %struct.ScmObj*, align 8
%anf_45bind47218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56982)
store volatile %struct.ScmObj* %anf_45bind47218, %struct.ScmObj** %stackaddr$prim59069, align 8
%stackaddr$prim59070 = alloca %struct.ScmObj*, align 8
%anf_45bind47219 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47095)
store volatile %struct.ScmObj* %anf_45bind47219, %struct.ScmObj** %stackaddr$prim59070, align 8
%stackaddr$makeclosure59071 = alloca %struct.ScmObj*, align 8
%fptrToInt59072 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47754 to i64
%ae47754 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59072)
store volatile %struct.ScmObj* %ae47754, %struct.ScmObj** %stackaddr$makeclosure59071, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47754, %struct.ScmObj* %anf_45bind47218, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47754, %struct.ScmObj* %k47503, i64 1)
%argslist56988$_37map470940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59073 = alloca %struct.ScmObj*, align 8
%argslist56988$_37map470941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47219, %struct.ScmObj* %argslist56988$_37map470940)
store volatile %struct.ScmObj* %argslist56988$_37map470941, %struct.ScmObj** %stackaddr$prim59073, align 8
%stackaddr$prim59074 = alloca %struct.ScmObj*, align 8
%argslist56988$_37map470942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47096, %struct.ScmObj* %argslist56988$_37map470941)
store volatile %struct.ScmObj* %argslist56988$_37map470942, %struct.ScmObj** %stackaddr$prim59074, align 8
%stackaddr$prim59075 = alloca %struct.ScmObj*, align 8
%argslist56988$_37map470943 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47754, %struct.ScmObj* %argslist56988$_37map470942)
store volatile %struct.ScmObj* %argslist56988$_37map470943, %struct.ScmObj** %stackaddr$prim59075, align 8
%clofunc59076 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map47094)
musttail call tailcc void %clofunc59076(%struct.ScmObj* %_37map47094, %struct.ScmObj* %argslist56988$_37map470943)
ret void
}

define tailcc void @proc_clo$ae47754(%struct.ScmObj* %env$ae47754,%struct.ScmObj* %current_45args56984) {
%stackaddr$env-ref59077 = alloca %struct.ScmObj*, align 8
%anf_45bind47218 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47754, i64 0)
store %struct.ScmObj* %anf_45bind47218, %struct.ScmObj** %stackaddr$env-ref59077
%stackaddr$env-ref59078 = alloca %struct.ScmObj*, align 8
%k47503 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47754, i64 1)
store %struct.ScmObj* %k47503, %struct.ScmObj** %stackaddr$env-ref59078
%stackaddr$prim59079 = alloca %struct.ScmObj*, align 8
%_95k47505 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56984)
store volatile %struct.ScmObj* %_95k47505, %struct.ScmObj** %stackaddr$prim59079, align 8
%stackaddr$prim59080 = alloca %struct.ScmObj*, align 8
%current_45args56985 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56984)
store volatile %struct.ScmObj* %current_45args56985, %struct.ScmObj** %stackaddr$prim59080, align 8
%stackaddr$prim59081 = alloca %struct.ScmObj*, align 8
%anf_45bind47220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56985)
store volatile %struct.ScmObj* %anf_45bind47220, %struct.ScmObj** %stackaddr$prim59081, align 8
%stackaddr$prim59082 = alloca %struct.ScmObj*, align 8
%cpsprim47506 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47218, %struct.ScmObj* %anf_45bind47220)
store volatile %struct.ScmObj* %cpsprim47506, %struct.ScmObj** %stackaddr$prim59082, align 8
%ae47760 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist56987$k475030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59083 = alloca %struct.ScmObj*, align 8
%argslist56987$k475031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47506, %struct.ScmObj* %argslist56987$k475030)
store volatile %struct.ScmObj* %argslist56987$k475031, %struct.ScmObj** %stackaddr$prim59083, align 8
%stackaddr$prim59084 = alloca %struct.ScmObj*, align 8
%argslist56987$k475032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47760, %struct.ScmObj* %argslist56987$k475031)
store volatile %struct.ScmObj* %argslist56987$k475032, %struct.ScmObj** %stackaddr$prim59084, align 8
%clofunc59085 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47503)
musttail call tailcc void %clofunc59085(%struct.ScmObj* %k47503, %struct.ScmObj* %argslist56987$k475032)
ret void
}

define tailcc void @proc_clo$ae47653(%struct.ScmObj* %env$ae47653,%struct.ScmObj* %current_45args56993) {
%stackaddr$prim59086 = alloca %struct.ScmObj*, align 8
%k47507 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56993)
store volatile %struct.ScmObj* %k47507, %struct.ScmObj** %stackaddr$prim59086, align 8
%stackaddr$prim59087 = alloca %struct.ScmObj*, align 8
%current_45args56994 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56993)
store volatile %struct.ScmObj* %current_45args56994, %struct.ScmObj** %stackaddr$prim59087, align 8
%stackaddr$prim59088 = alloca %struct.ScmObj*, align 8
%_37foldr147098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56994)
store volatile %struct.ScmObj* %_37foldr147098, %struct.ScmObj** %stackaddr$prim59088, align 8
%ae47655 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59089 = alloca %struct.ScmObj*, align 8
%fptrToInt59090 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47656 to i64
%ae47656 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt59090)
store volatile %struct.ScmObj* %ae47656, %struct.ScmObj** %stackaddr$makeclosure59089, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47656, %struct.ScmObj* %_37foldr147098, i64 0)
%argslist57007$k475070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59091 = alloca %struct.ScmObj*, align 8
%argslist57007$k475071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47656, %struct.ScmObj* %argslist57007$k475070)
store volatile %struct.ScmObj* %argslist57007$k475071, %struct.ScmObj** %stackaddr$prim59091, align 8
%stackaddr$prim59092 = alloca %struct.ScmObj*, align 8
%argslist57007$k475072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47655, %struct.ScmObj* %argslist57007$k475071)
store volatile %struct.ScmObj* %argslist57007$k475072, %struct.ScmObj** %stackaddr$prim59092, align 8
%clofunc59093 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47507)
musttail call tailcc void %clofunc59093(%struct.ScmObj* %k47507, %struct.ScmObj* %argslist57007$k475072)
ret void
}

define tailcc void @proc_clo$ae47656(%struct.ScmObj* %env$ae47656,%struct.ScmObj* %current_45args56996) {
%stackaddr$env-ref59094 = alloca %struct.ScmObj*, align 8
%_37foldr147098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47656, i64 0)
store %struct.ScmObj* %_37foldr147098, %struct.ScmObj** %stackaddr$env-ref59094
%stackaddr$prim59095 = alloca %struct.ScmObj*, align 8
%k47508 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56996)
store volatile %struct.ScmObj* %k47508, %struct.ScmObj** %stackaddr$prim59095, align 8
%stackaddr$prim59096 = alloca %struct.ScmObj*, align 8
%current_45args56997 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56996)
store volatile %struct.ScmObj* %current_45args56997, %struct.ScmObj** %stackaddr$prim59096, align 8
%stackaddr$prim59097 = alloca %struct.ScmObj*, align 8
%f47101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56997)
store volatile %struct.ScmObj* %f47101, %struct.ScmObj** %stackaddr$prim59097, align 8
%stackaddr$prim59098 = alloca %struct.ScmObj*, align 8
%current_45args56998 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56997)
store volatile %struct.ScmObj* %current_45args56998, %struct.ScmObj** %stackaddr$prim59098, align 8
%stackaddr$prim59099 = alloca %struct.ScmObj*, align 8
%acc47100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56998)
store volatile %struct.ScmObj* %acc47100, %struct.ScmObj** %stackaddr$prim59099, align 8
%stackaddr$prim59100 = alloca %struct.ScmObj*, align 8
%current_45args56999 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args56998)
store volatile %struct.ScmObj* %current_45args56999, %struct.ScmObj** %stackaddr$prim59100, align 8
%stackaddr$prim59101 = alloca %struct.ScmObj*, align 8
%lst47099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args56999)
store volatile %struct.ScmObj* %lst47099, %struct.ScmObj** %stackaddr$prim59101, align 8
%stackaddr$prim59102 = alloca %struct.ScmObj*, align 8
%anf_45bind47211 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47099)
store volatile %struct.ScmObj* %anf_45bind47211, %struct.ScmObj** %stackaddr$prim59102, align 8
%truthy$cmp59103 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47211)
%cmp$cmp59103 = icmp eq i64 %truthy$cmp59103, 1
br i1 %cmp$cmp59103, label %truebranch$cmp59103, label %falsebranch$cmp59103
truebranch$cmp59103:
%ae47660 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57001$k475080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59104 = alloca %struct.ScmObj*, align 8
%argslist57001$k475081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47100, %struct.ScmObj* %argslist57001$k475080)
store volatile %struct.ScmObj* %argslist57001$k475081, %struct.ScmObj** %stackaddr$prim59104, align 8
%stackaddr$prim59105 = alloca %struct.ScmObj*, align 8
%argslist57001$k475082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47660, %struct.ScmObj* %argslist57001$k475081)
store volatile %struct.ScmObj* %argslist57001$k475082, %struct.ScmObj** %stackaddr$prim59105, align 8
%clofunc59106 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47508)
musttail call tailcc void %clofunc59106(%struct.ScmObj* %k47508, %struct.ScmObj* %argslist57001$k475082)
ret void
falsebranch$cmp59103:
%stackaddr$prim59107 = alloca %struct.ScmObj*, align 8
%anf_45bind47212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47099)
store volatile %struct.ScmObj* %anf_45bind47212, %struct.ScmObj** %stackaddr$prim59107, align 8
%stackaddr$prim59108 = alloca %struct.ScmObj*, align 8
%anf_45bind47213 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47099)
store volatile %struct.ScmObj* %anf_45bind47213, %struct.ScmObj** %stackaddr$prim59108, align 8
%stackaddr$makeclosure59109 = alloca %struct.ScmObj*, align 8
%fptrToInt59110 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47668 to i64
%ae47668 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt59110)
store volatile %struct.ScmObj* %ae47668, %struct.ScmObj** %stackaddr$makeclosure59109, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47668, %struct.ScmObj* %k47508, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47668, %struct.ScmObj* %f47101, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47668, %struct.ScmObj* %anf_45bind47212, i64 2)
%argslist57006$_37foldr1470980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59111 = alloca %struct.ScmObj*, align 8
%argslist57006$_37foldr1470981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47213, %struct.ScmObj* %argslist57006$_37foldr1470980)
store volatile %struct.ScmObj* %argslist57006$_37foldr1470981, %struct.ScmObj** %stackaddr$prim59111, align 8
%stackaddr$prim59112 = alloca %struct.ScmObj*, align 8
%argslist57006$_37foldr1470982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47100, %struct.ScmObj* %argslist57006$_37foldr1470981)
store volatile %struct.ScmObj* %argslist57006$_37foldr1470982, %struct.ScmObj** %stackaddr$prim59112, align 8
%stackaddr$prim59113 = alloca %struct.ScmObj*, align 8
%argslist57006$_37foldr1470983 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47101, %struct.ScmObj* %argslist57006$_37foldr1470982)
store volatile %struct.ScmObj* %argslist57006$_37foldr1470983, %struct.ScmObj** %stackaddr$prim59113, align 8
%stackaddr$prim59114 = alloca %struct.ScmObj*, align 8
%argslist57006$_37foldr1470984 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47668, %struct.ScmObj* %argslist57006$_37foldr1470983)
store volatile %struct.ScmObj* %argslist57006$_37foldr1470984, %struct.ScmObj** %stackaddr$prim59114, align 8
%clofunc59115 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147098)
musttail call tailcc void %clofunc59115(%struct.ScmObj* %_37foldr147098, %struct.ScmObj* %argslist57006$_37foldr1470984)
ret void
}

define tailcc void @proc_clo$ae47668(%struct.ScmObj* %env$ae47668,%struct.ScmObj* %current_45args57002) {
%stackaddr$env-ref59116 = alloca %struct.ScmObj*, align 8
%k47508 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47668, i64 0)
store %struct.ScmObj* %k47508, %struct.ScmObj** %stackaddr$env-ref59116
%stackaddr$env-ref59117 = alloca %struct.ScmObj*, align 8
%f47101 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47668, i64 1)
store %struct.ScmObj* %f47101, %struct.ScmObj** %stackaddr$env-ref59117
%stackaddr$env-ref59118 = alloca %struct.ScmObj*, align 8
%anf_45bind47212 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47668, i64 2)
store %struct.ScmObj* %anf_45bind47212, %struct.ScmObj** %stackaddr$env-ref59118
%stackaddr$prim59119 = alloca %struct.ScmObj*, align 8
%_95k47509 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57002)
store volatile %struct.ScmObj* %_95k47509, %struct.ScmObj** %stackaddr$prim59119, align 8
%stackaddr$prim59120 = alloca %struct.ScmObj*, align 8
%current_45args57003 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57002)
store volatile %struct.ScmObj* %current_45args57003, %struct.ScmObj** %stackaddr$prim59120, align 8
%stackaddr$prim59121 = alloca %struct.ScmObj*, align 8
%anf_45bind47214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57003)
store volatile %struct.ScmObj* %anf_45bind47214, %struct.ScmObj** %stackaddr$prim59121, align 8
%argslist57005$f471010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59122 = alloca %struct.ScmObj*, align 8
%argslist57005$f471011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47214, %struct.ScmObj* %argslist57005$f471010)
store volatile %struct.ScmObj* %argslist57005$f471011, %struct.ScmObj** %stackaddr$prim59122, align 8
%stackaddr$prim59123 = alloca %struct.ScmObj*, align 8
%argslist57005$f471012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47212, %struct.ScmObj* %argslist57005$f471011)
store volatile %struct.ScmObj* %argslist57005$f471012, %struct.ScmObj** %stackaddr$prim59123, align 8
%stackaddr$prim59124 = alloca %struct.ScmObj*, align 8
%argslist57005$f471013 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47508, %struct.ScmObj* %argslist57005$f471012)
store volatile %struct.ScmObj* %argslist57005$f471013, %struct.ScmObj** %stackaddr$prim59124, align 8
%clofunc59125 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47101)
musttail call tailcc void %clofunc59125(%struct.ScmObj* %f47101, %struct.ScmObj* %argslist57005$f471013)
ret void
}

define tailcc void @proc_clo$ae47536(%struct.ScmObj* %env$ae47536,%struct.ScmObj* %current_45args57010) {
%stackaddr$prim59126 = alloca %struct.ScmObj*, align 8
%k47510 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57010)
store volatile %struct.ScmObj* %k47510, %struct.ScmObj** %stackaddr$prim59126, align 8
%stackaddr$prim59127 = alloca %struct.ScmObj*, align 8
%current_45args57011 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57010)
store volatile %struct.ScmObj* %current_45args57011, %struct.ScmObj** %stackaddr$prim59127, align 8
%stackaddr$prim59128 = alloca %struct.ScmObj*, align 8
%y47078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57011)
store volatile %struct.ScmObj* %y47078, %struct.ScmObj** %stackaddr$prim59128, align 8
%ae47538 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59129 = alloca %struct.ScmObj*, align 8
%fptrToInt59130 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47539 to i64
%ae47539 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt59130)
store volatile %struct.ScmObj* %ae47539, %struct.ScmObj** %stackaddr$makeclosure59129, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47539, %struct.ScmObj* %y47078, i64 0)
%argslist57029$k475100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59131 = alloca %struct.ScmObj*, align 8
%argslist57029$k475101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47539, %struct.ScmObj* %argslist57029$k475100)
store volatile %struct.ScmObj* %argslist57029$k475101, %struct.ScmObj** %stackaddr$prim59131, align 8
%stackaddr$prim59132 = alloca %struct.ScmObj*, align 8
%argslist57029$k475102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47538, %struct.ScmObj* %argslist57029$k475101)
store volatile %struct.ScmObj* %argslist57029$k475102, %struct.ScmObj** %stackaddr$prim59132, align 8
%clofunc59133 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47510)
musttail call tailcc void %clofunc59133(%struct.ScmObj* %k47510, %struct.ScmObj* %argslist57029$k475102)
ret void
}

define tailcc void @proc_clo$ae47539(%struct.ScmObj* %env$ae47539,%struct.ScmObj* %current_45args57013) {
%stackaddr$env-ref59134 = alloca %struct.ScmObj*, align 8
%y47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47539, i64 0)
store %struct.ScmObj* %y47078, %struct.ScmObj** %stackaddr$env-ref59134
%stackaddr$prim59135 = alloca %struct.ScmObj*, align 8
%k47511 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57013)
store volatile %struct.ScmObj* %k47511, %struct.ScmObj** %stackaddr$prim59135, align 8
%stackaddr$prim59136 = alloca %struct.ScmObj*, align 8
%current_45args57014 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57013)
store volatile %struct.ScmObj* %current_45args57014, %struct.ScmObj** %stackaddr$prim59136, align 8
%stackaddr$prim59137 = alloca %struct.ScmObj*, align 8
%f47079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57014)
store volatile %struct.ScmObj* %f47079, %struct.ScmObj** %stackaddr$prim59137, align 8
%stackaddr$makeclosure59138 = alloca %struct.ScmObj*, align 8
%fptrToInt59139 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47540 to i64
%ae47540 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59139)
store volatile %struct.ScmObj* %ae47540, %struct.ScmObj** %stackaddr$makeclosure59138, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47540, %struct.ScmObj* %f47079, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47540, %struct.ScmObj* %k47511, i64 1)
%ae47541 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59140 = alloca %struct.ScmObj*, align 8
%fptrToInt59141 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47542 to i64
%ae47542 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59141)
store volatile %struct.ScmObj* %ae47542, %struct.ScmObj** %stackaddr$makeclosure59140, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47542, %struct.ScmObj* %y47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47542, %struct.ScmObj* %f47079, i64 1)
%argslist57028$ae475400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59142 = alloca %struct.ScmObj*, align 8
%argslist57028$ae475401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47542, %struct.ScmObj* %argslist57028$ae475400)
store volatile %struct.ScmObj* %argslist57028$ae475401, %struct.ScmObj** %stackaddr$prim59142, align 8
%stackaddr$prim59143 = alloca %struct.ScmObj*, align 8
%argslist57028$ae475402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47541, %struct.ScmObj* %argslist57028$ae475401)
store volatile %struct.ScmObj* %argslist57028$ae475402, %struct.ScmObj** %stackaddr$prim59143, align 8
%clofunc59144 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47540)
musttail call tailcc void %clofunc59144(%struct.ScmObj* %ae47540, %struct.ScmObj* %argslist57028$ae475402)
ret void
}

define tailcc void @proc_clo$ae47540(%struct.ScmObj* %env$ae47540,%struct.ScmObj* %current_45args57016) {
%stackaddr$env-ref59145 = alloca %struct.ScmObj*, align 8
%f47079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47540, i64 0)
store %struct.ScmObj* %f47079, %struct.ScmObj** %stackaddr$env-ref59145
%stackaddr$env-ref59146 = alloca %struct.ScmObj*, align 8
%k47511 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47540, i64 1)
store %struct.ScmObj* %k47511, %struct.ScmObj** %stackaddr$env-ref59146
%stackaddr$prim59147 = alloca %struct.ScmObj*, align 8
%_95k47512 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57016)
store volatile %struct.ScmObj* %_95k47512, %struct.ScmObj** %stackaddr$prim59147, align 8
%stackaddr$prim59148 = alloca %struct.ScmObj*, align 8
%current_45args57017 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57016)
store volatile %struct.ScmObj* %current_45args57017, %struct.ScmObj** %stackaddr$prim59148, align 8
%stackaddr$prim59149 = alloca %struct.ScmObj*, align 8
%anf_45bind47209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57017)
store volatile %struct.ScmObj* %anf_45bind47209, %struct.ScmObj** %stackaddr$prim59149, align 8
%argslist57019$f470790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59150 = alloca %struct.ScmObj*, align 8
%argslist57019$f470791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47209, %struct.ScmObj* %argslist57019$f470790)
store volatile %struct.ScmObj* %argslist57019$f470791, %struct.ScmObj** %stackaddr$prim59150, align 8
%stackaddr$prim59151 = alloca %struct.ScmObj*, align 8
%argslist57019$f470792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47511, %struct.ScmObj* %argslist57019$f470791)
store volatile %struct.ScmObj* %argslist57019$f470792, %struct.ScmObj** %stackaddr$prim59151, align 8
%clofunc59152 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47079)
musttail call tailcc void %clofunc59152(%struct.ScmObj* %f47079, %struct.ScmObj* %argslist57019$f470792)
ret void
}

define tailcc void @proc_clo$ae47542(%struct.ScmObj* %env$ae47542,%struct.ScmObj* %args4708047513) {
%stackaddr$env-ref59153 = alloca %struct.ScmObj*, align 8
%y47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47542, i64 0)
store %struct.ScmObj* %y47078, %struct.ScmObj** %stackaddr$env-ref59153
%stackaddr$env-ref59154 = alloca %struct.ScmObj*, align 8
%f47079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47542, i64 1)
store %struct.ScmObj* %f47079, %struct.ScmObj** %stackaddr$env-ref59154
%stackaddr$prim59155 = alloca %struct.ScmObj*, align 8
%k47514 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4708047513)
store volatile %struct.ScmObj* %k47514, %struct.ScmObj** %stackaddr$prim59155, align 8
%stackaddr$prim59156 = alloca %struct.ScmObj*, align 8
%args47080 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4708047513)
store volatile %struct.ScmObj* %args47080, %struct.ScmObj** %stackaddr$prim59156, align 8
%stackaddr$makeclosure59157 = alloca %struct.ScmObj*, align 8
%fptrToInt59158 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47546 to i64
%ae47546 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt59158)
store volatile %struct.ScmObj* %ae47546, %struct.ScmObj** %stackaddr$makeclosure59157, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47546, %struct.ScmObj* %k47514, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47546, %struct.ScmObj* %args47080, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47546, %struct.ScmObj* %f47079, i64 2)
%argslist57027$y470780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59159 = alloca %struct.ScmObj*, align 8
%argslist57027$y470781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y47078, %struct.ScmObj* %argslist57027$y470780)
store volatile %struct.ScmObj* %argslist57027$y470781, %struct.ScmObj** %stackaddr$prim59159, align 8
%stackaddr$prim59160 = alloca %struct.ScmObj*, align 8
%argslist57027$y470782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47546, %struct.ScmObj* %argslist57027$y470781)
store volatile %struct.ScmObj* %argslist57027$y470782, %struct.ScmObj** %stackaddr$prim59160, align 8
%clofunc59161 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y47078)
musttail call tailcc void %clofunc59161(%struct.ScmObj* %y47078, %struct.ScmObj* %argslist57027$y470782)
ret void
}

define tailcc void @proc_clo$ae47546(%struct.ScmObj* %env$ae47546,%struct.ScmObj* %current_45args57020) {
%stackaddr$env-ref59162 = alloca %struct.ScmObj*, align 8
%k47514 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47546, i64 0)
store %struct.ScmObj* %k47514, %struct.ScmObj** %stackaddr$env-ref59162
%stackaddr$env-ref59163 = alloca %struct.ScmObj*, align 8
%args47080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47546, i64 1)
store %struct.ScmObj* %args47080, %struct.ScmObj** %stackaddr$env-ref59163
%stackaddr$env-ref59164 = alloca %struct.ScmObj*, align 8
%f47079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47546, i64 2)
store %struct.ScmObj* %f47079, %struct.ScmObj** %stackaddr$env-ref59164
%stackaddr$prim59165 = alloca %struct.ScmObj*, align 8
%_95k47515 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57020)
store volatile %struct.ScmObj* %_95k47515, %struct.ScmObj** %stackaddr$prim59165, align 8
%stackaddr$prim59166 = alloca %struct.ScmObj*, align 8
%current_45args57021 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57020)
store volatile %struct.ScmObj* %current_45args57021, %struct.ScmObj** %stackaddr$prim59166, align 8
%stackaddr$prim59167 = alloca %struct.ScmObj*, align 8
%anf_45bind47207 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57021)
store volatile %struct.ScmObj* %anf_45bind47207, %struct.ScmObj** %stackaddr$prim59167, align 8
%stackaddr$makeclosure59168 = alloca %struct.ScmObj*, align 8
%fptrToInt59169 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47549 to i64
%ae47549 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59169)
store volatile %struct.ScmObj* %ae47549, %struct.ScmObj** %stackaddr$makeclosure59168, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47549, %struct.ScmObj* %k47514, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47549, %struct.ScmObj* %args47080, i64 1)
%argslist57026$anf_45bind472070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59170 = alloca %struct.ScmObj*, align 8
%argslist57026$anf_45bind472071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47079, %struct.ScmObj* %argslist57026$anf_45bind472070)
store volatile %struct.ScmObj* %argslist57026$anf_45bind472071, %struct.ScmObj** %stackaddr$prim59170, align 8
%stackaddr$prim59171 = alloca %struct.ScmObj*, align 8
%argslist57026$anf_45bind472072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47549, %struct.ScmObj* %argslist57026$anf_45bind472071)
store volatile %struct.ScmObj* %argslist57026$anf_45bind472072, %struct.ScmObj** %stackaddr$prim59171, align 8
%clofunc59172 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47207)
musttail call tailcc void %clofunc59172(%struct.ScmObj* %anf_45bind47207, %struct.ScmObj* %argslist57026$anf_45bind472072)
ret void
}

define tailcc void @proc_clo$ae47549(%struct.ScmObj* %env$ae47549,%struct.ScmObj* %current_45args57023) {
%stackaddr$env-ref59173 = alloca %struct.ScmObj*, align 8
%k47514 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47549, i64 0)
store %struct.ScmObj* %k47514, %struct.ScmObj** %stackaddr$env-ref59173
%stackaddr$env-ref59174 = alloca %struct.ScmObj*, align 8
%args47080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47549, i64 1)
store %struct.ScmObj* %args47080, %struct.ScmObj** %stackaddr$env-ref59174
%stackaddr$prim59175 = alloca %struct.ScmObj*, align 8
%_95k47516 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57023)
store volatile %struct.ScmObj* %_95k47516, %struct.ScmObj** %stackaddr$prim59175, align 8
%stackaddr$prim59176 = alloca %struct.ScmObj*, align 8
%current_45args57024 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57023)
store volatile %struct.ScmObj* %current_45args57024, %struct.ScmObj** %stackaddr$prim59176, align 8
%stackaddr$prim59177 = alloca %struct.ScmObj*, align 8
%anf_45bind47208 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57024)
store volatile %struct.ScmObj* %anf_45bind47208, %struct.ScmObj** %stackaddr$prim59177, align 8
%stackaddr$prim59178 = alloca %struct.ScmObj*, align 8
%cpsargs47517 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47514, %struct.ScmObj* %args47080)
store volatile %struct.ScmObj* %cpsargs47517, %struct.ScmObj** %stackaddr$prim59178, align 8
%clofunc59179 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47208)
musttail call tailcc void %clofunc59179(%struct.ScmObj* %anf_45bind47208, %struct.ScmObj* %cpsargs47517)
ret void
}

define tailcc void @proc_clo$ae47521(%struct.ScmObj* %env$ae47521,%struct.ScmObj* %current_45args57031) {
%stackaddr$prim59180 = alloca %struct.ScmObj*, align 8
%k47518 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57031)
store volatile %struct.ScmObj* %k47518, %struct.ScmObj** %stackaddr$prim59180, align 8
%stackaddr$prim59181 = alloca %struct.ScmObj*, align 8
%current_45args57032 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57031)
store volatile %struct.ScmObj* %current_45args57032, %struct.ScmObj** %stackaddr$prim59181, align 8
%stackaddr$prim59182 = alloca %struct.ScmObj*, align 8
%yu47077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57032)
store volatile %struct.ScmObj* %yu47077, %struct.ScmObj** %stackaddr$prim59182, align 8
%argslist57034$yu470770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59183 = alloca %struct.ScmObj*, align 8
%argslist57034$yu470771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu47077, %struct.ScmObj* %argslist57034$yu470770)
store volatile %struct.ScmObj* %argslist57034$yu470771, %struct.ScmObj** %stackaddr$prim59183, align 8
%stackaddr$prim59184 = alloca %struct.ScmObj*, align 8
%argslist57034$yu470772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47518, %struct.ScmObj* %argslist57034$yu470771)
store volatile %struct.ScmObj* %argslist57034$yu470772, %struct.ScmObj** %stackaddr$prim59184, align 8
%clofunc59185 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu47077)
musttail call tailcc void %clofunc59185(%struct.ScmObj* %yu47077, %struct.ScmObj* %argslist57034$yu470772)
ret void
}