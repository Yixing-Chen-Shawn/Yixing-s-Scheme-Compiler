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

@global$sym$ae5189958400 = private unnamed_addr constant [8 x i8] c"promise\00", align 8
@global$sym$ae5240358442 = private unnamed_addr constant [4 x i8] c"get\00", align 8
@global$str$ae5361758455 = private unnamed_addr constant [12 x i8] c"not promise\00", align 8
@global$sym$ae5213758495 = private unnamed_addr constant [4 x i8] c"get\00", align 8
@global$str$ae5228058508 = private unnamed_addr constant [12 x i8] c"not promise\00", align 8
@global$sym$ae5202658556 = private unnamed_addr constant [8 x i8] c"promise\00", align 8
@global$sym$ae5254158614 = private unnamed_addr constant [4 x i8] c"get\00", align 8
@global$str$ae5268458627 = private unnamed_addr constant [12 x i8] c"not promise\00", align 8
@global$sym$ae5243058675 = private unnamed_addr constant [8 x i8] c"promise\00", align 8
@global$sym$ae5343158722 = private unnamed_addr constant [4 x i8] c"get\00", align 8
@global$str$ae5357458735 = private unnamed_addr constant [12 x i8] c"not promise\00", align 8
@global$sym$ae5332058783 = private unnamed_addr constant [8 x i8] c"promise\00", align 8
@global$sym$ae5374758830 = private unnamed_addr constant [4 x i8] c"get\00", align 8
@global$str$ae5389058843 = private unnamed_addr constant [12 x i8] c"not promise\00", align 8
@global$sym$ae5363658891 = private unnamed_addr constant [8 x i8] c"promise\00", align 8
@global$sym$ae5192458911 = private unnamed_addr constant [8 x i8] c"promise\00", align 8

define ccc i32 @main() {
%mainenv57995 = call %struct.ScmObj* @const_init_null()
%mainargs57996 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv57995, %struct.ScmObj* %mainargs57996)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv57993,%struct.ScmObj* %mainargs57994) {
%stackaddr$makeclosure57997 = alloca %struct.ScmObj*, align 8
%fptrToInt57998 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48476 to i64
%ae48476 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57998)
store volatile %struct.ScmObj* %ae48476, %struct.ScmObj** %stackaddr$makeclosure57997, align 8
%ae48477 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57999 = alloca %struct.ScmObj*, align 8
%fptrToInt58000 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48478 to i64
%ae48478 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58000)
store volatile %struct.ScmObj* %ae48478, %struct.ScmObj** %stackaddr$makeclosure57999, align 8
%argslist57992$ae484760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58001 = alloca %struct.ScmObj*, align 8
%argslist57992$ae484761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48478, %struct.ScmObj* %argslist57992$ae484760)
store volatile %struct.ScmObj* %argslist57992$ae484761, %struct.ScmObj** %stackaddr$prim58001, align 8
%stackaddr$prim58002 = alloca %struct.ScmObj*, align 8
%argslist57992$ae484762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48477, %struct.ScmObj* %argslist57992$ae484761)
store volatile %struct.ScmObj* %argslist57992$ae484762, %struct.ScmObj** %stackaddr$prim58002, align 8
%clofunc58003 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48476)
musttail call tailcc void %clofunc58003(%struct.ScmObj* %ae48476, %struct.ScmObj* %argslist57992$ae484762)
ret void
}

define tailcc void @proc_clo$ae48476(%struct.ScmObj* %env$ae48476,%struct.ScmObj* %current_45args57268) {
%stackaddr$prim58004 = alloca %struct.ScmObj*, align 8
%_95k48294 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57268)
store volatile %struct.ScmObj* %_95k48294, %struct.ScmObj** %stackaddr$prim58004, align 8
%stackaddr$prim58005 = alloca %struct.ScmObj*, align 8
%current_45args57269 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57268)
store volatile %struct.ScmObj* %current_45args57269, %struct.ScmObj** %stackaddr$prim58005, align 8
%stackaddr$prim58006 = alloca %struct.ScmObj*, align 8
%anf_45bind48163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57269)
store volatile %struct.ScmObj* %anf_45bind48163, %struct.ScmObj** %stackaddr$prim58006, align 8
%stackaddr$makeclosure58007 = alloca %struct.ScmObj*, align 8
%fptrToInt58008 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48491 to i64
%ae48491 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58008)
store volatile %struct.ScmObj* %ae48491, %struct.ScmObj** %stackaddr$makeclosure58007, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48491, %struct.ScmObj* %anf_45bind48163, i64 0)
%ae48492 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58009 = alloca %struct.ScmObj*, align 8
%fptrToInt58010 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48493 to i64
%ae48493 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58010)
store volatile %struct.ScmObj* %ae48493, %struct.ScmObj** %stackaddr$makeclosure58009, align 8
%argslist57987$ae484910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58011 = alloca %struct.ScmObj*, align 8
%argslist57987$ae484911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48493, %struct.ScmObj* %argslist57987$ae484910)
store volatile %struct.ScmObj* %argslist57987$ae484911, %struct.ScmObj** %stackaddr$prim58011, align 8
%stackaddr$prim58012 = alloca %struct.ScmObj*, align 8
%argslist57987$ae484912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48492, %struct.ScmObj* %argslist57987$ae484911)
store volatile %struct.ScmObj* %argslist57987$ae484912, %struct.ScmObj** %stackaddr$prim58012, align 8
%clofunc58013 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48491)
musttail call tailcc void %clofunc58013(%struct.ScmObj* %ae48491, %struct.ScmObj* %argslist57987$ae484912)
ret void
}

define tailcc void @proc_clo$ae48491(%struct.ScmObj* %env$ae48491,%struct.ScmObj* %current_45args57271) {
%stackaddr$env-ref58014 = alloca %struct.ScmObj*, align 8
%anf_45bind48163 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48491, i64 0)
store %struct.ScmObj* %anf_45bind48163, %struct.ScmObj** %stackaddr$env-ref58014
%stackaddr$prim58015 = alloca %struct.ScmObj*, align 8
%_95k48295 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57271)
store volatile %struct.ScmObj* %_95k48295, %struct.ScmObj** %stackaddr$prim58015, align 8
%stackaddr$prim58016 = alloca %struct.ScmObj*, align 8
%current_45args57272 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57271)
store volatile %struct.ScmObj* %current_45args57272, %struct.ScmObj** %stackaddr$prim58016, align 8
%stackaddr$prim58017 = alloca %struct.ScmObj*, align 8
%anf_45bind48167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57272)
store volatile %struct.ScmObj* %anf_45bind48167, %struct.ScmObj** %stackaddr$prim58017, align 8
%stackaddr$makeclosure58018 = alloca %struct.ScmObj*, align 8
%fptrToInt58019 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48606 to i64
%ae48606 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58019)
store volatile %struct.ScmObj* %ae48606, %struct.ScmObj** %stackaddr$makeclosure58018, align 8
%argslist57966$anf_45bind481630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58020 = alloca %struct.ScmObj*, align 8
%argslist57966$anf_45bind481631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48167, %struct.ScmObj* %argslist57966$anf_45bind481630)
store volatile %struct.ScmObj* %argslist57966$anf_45bind481631, %struct.ScmObj** %stackaddr$prim58020, align 8
%stackaddr$prim58021 = alloca %struct.ScmObj*, align 8
%argslist57966$anf_45bind481632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48606, %struct.ScmObj* %argslist57966$anf_45bind481631)
store volatile %struct.ScmObj* %argslist57966$anf_45bind481632, %struct.ScmObj** %stackaddr$prim58021, align 8
%clofunc58022 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48163)
musttail call tailcc void %clofunc58022(%struct.ScmObj* %anf_45bind48163, %struct.ScmObj* %argslist57966$anf_45bind481632)
ret void
}

define tailcc void @proc_clo$ae48606(%struct.ScmObj* %env$ae48606,%struct.ScmObj* %current_45args57274) {
%stackaddr$prim58023 = alloca %struct.ScmObj*, align 8
%_95k48296 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57274)
store volatile %struct.ScmObj* %_95k48296, %struct.ScmObj** %stackaddr$prim58023, align 8
%stackaddr$prim58024 = alloca %struct.ScmObj*, align 8
%current_45args57275 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57274)
store volatile %struct.ScmObj* %current_45args57275, %struct.ScmObj** %stackaddr$prim58024, align 8
%stackaddr$prim58025 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57275)
store volatile %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$prim58025, align 8
%stackaddr$makeclosure58026 = alloca %struct.ScmObj*, align 8
%fptrToInt58027 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48608 to i64
%ae48608 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58027)
store volatile %struct.ScmObj* %ae48608, %struct.ScmObj** %stackaddr$makeclosure58026, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48608, %struct.ScmObj* %Ycmb48033, i64 0)
%ae48609 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58028 = alloca %struct.ScmObj*, align 8
%fptrToInt58029 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48610 to i64
%ae48610 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58029)
store volatile %struct.ScmObj* %ae48610, %struct.ScmObj** %stackaddr$makeclosure58028, align 8
%argslist57965$ae486080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58030 = alloca %struct.ScmObj*, align 8
%argslist57965$ae486081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48610, %struct.ScmObj* %argslist57965$ae486080)
store volatile %struct.ScmObj* %argslist57965$ae486081, %struct.ScmObj** %stackaddr$prim58030, align 8
%stackaddr$prim58031 = alloca %struct.ScmObj*, align 8
%argslist57965$ae486082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48609, %struct.ScmObj* %argslist57965$ae486081)
store volatile %struct.ScmObj* %argslist57965$ae486082, %struct.ScmObj** %stackaddr$prim58031, align 8
%clofunc58032 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48608)
musttail call tailcc void %clofunc58032(%struct.ScmObj* %ae48608, %struct.ScmObj* %argslist57965$ae486082)
ret void
}

define tailcc void @proc_clo$ae48608(%struct.ScmObj* %env$ae48608,%struct.ScmObj* %current_45args57277) {
%stackaddr$env-ref58033 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48608, i64 0)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref58033
%stackaddr$prim58034 = alloca %struct.ScmObj*, align 8
%_95k48297 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57277)
store volatile %struct.ScmObj* %_95k48297, %struct.ScmObj** %stackaddr$prim58034, align 8
%stackaddr$prim58035 = alloca %struct.ScmObj*, align 8
%current_45args57278 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57277)
store volatile %struct.ScmObj* %current_45args57278, %struct.ScmObj** %stackaddr$prim58035, align 8
%stackaddr$prim58036 = alloca %struct.ScmObj*, align 8
%anf_45bind48172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57278)
store volatile %struct.ScmObj* %anf_45bind48172, %struct.ScmObj** %stackaddr$prim58036, align 8
%stackaddr$makeclosure58037 = alloca %struct.ScmObj*, align 8
%fptrToInt58038 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48686 to i64
%ae48686 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58038)
store volatile %struct.ScmObj* %ae48686, %struct.ScmObj** %stackaddr$makeclosure58037, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48686, %struct.ScmObj* %Ycmb48033, i64 0)
%argslist57949$Ycmb480330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58039 = alloca %struct.ScmObj*, align 8
%argslist57949$Ycmb480331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48172, %struct.ScmObj* %argslist57949$Ycmb480330)
store volatile %struct.ScmObj* %argslist57949$Ycmb480331, %struct.ScmObj** %stackaddr$prim58039, align 8
%stackaddr$prim58040 = alloca %struct.ScmObj*, align 8
%argslist57949$Ycmb480332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48686, %struct.ScmObj* %argslist57949$Ycmb480331)
store volatile %struct.ScmObj* %argslist57949$Ycmb480332, %struct.ScmObj** %stackaddr$prim58040, align 8
%clofunc58041 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48033)
musttail call tailcc void %clofunc58041(%struct.ScmObj* %Ycmb48033, %struct.ScmObj* %argslist57949$Ycmb480332)
ret void
}

define tailcc void @proc_clo$ae48686(%struct.ScmObj* %env$ae48686,%struct.ScmObj* %current_45args57280) {
%stackaddr$env-ref58042 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48686, i64 0)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref58042
%stackaddr$prim58043 = alloca %struct.ScmObj*, align 8
%_95k48298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57280)
store volatile %struct.ScmObj* %_95k48298, %struct.ScmObj** %stackaddr$prim58043, align 8
%stackaddr$prim58044 = alloca %struct.ScmObj*, align 8
%current_45args57281 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57280)
store volatile %struct.ScmObj* %current_45args57281, %struct.ScmObj** %stackaddr$prim58044, align 8
%stackaddr$prim58045 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57281)
store volatile %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$prim58045, align 8
%stackaddr$makeclosure58046 = alloca %struct.ScmObj*, align 8
%fptrToInt58047 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48688 to i64
%ae48688 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58047)
store volatile %struct.ScmObj* %ae48688, %struct.ScmObj** %stackaddr$makeclosure58046, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48688, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48688, %struct.ScmObj* %Ycmb48033, i64 1)
%ae48689 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58048 = alloca %struct.ScmObj*, align 8
%fptrToInt58049 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48690 to i64
%ae48690 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58049)
store volatile %struct.ScmObj* %ae48690, %struct.ScmObj** %stackaddr$makeclosure58048, align 8
%argslist57948$ae486880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58050 = alloca %struct.ScmObj*, align 8
%argslist57948$ae486881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48690, %struct.ScmObj* %argslist57948$ae486880)
store volatile %struct.ScmObj* %argslist57948$ae486881, %struct.ScmObj** %stackaddr$prim58050, align 8
%stackaddr$prim58051 = alloca %struct.ScmObj*, align 8
%argslist57948$ae486882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48689, %struct.ScmObj* %argslist57948$ae486881)
store volatile %struct.ScmObj* %argslist57948$ae486882, %struct.ScmObj** %stackaddr$prim58051, align 8
%clofunc58052 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48688)
musttail call tailcc void %clofunc58052(%struct.ScmObj* %ae48688, %struct.ScmObj* %argslist57948$ae486882)
ret void
}

define tailcc void @proc_clo$ae48688(%struct.ScmObj* %env$ae48688,%struct.ScmObj* %current_45args57283) {
%stackaddr$env-ref58053 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48688, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref58053
%stackaddr$env-ref58054 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48688, i64 1)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref58054
%stackaddr$prim58055 = alloca %struct.ScmObj*, align 8
%_95k48299 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57283)
store volatile %struct.ScmObj* %_95k48299, %struct.ScmObj** %stackaddr$prim58055, align 8
%stackaddr$prim58056 = alloca %struct.ScmObj*, align 8
%current_45args57284 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57283)
store volatile %struct.ScmObj* %current_45args57284, %struct.ScmObj** %stackaddr$prim58056, align 8
%stackaddr$prim58057 = alloca %struct.ScmObj*, align 8
%anf_45bind48178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57284)
store volatile %struct.ScmObj* %anf_45bind48178, %struct.ScmObj** %stackaddr$prim58057, align 8
%stackaddr$makeclosure58058 = alloca %struct.ScmObj*, align 8
%fptrToInt58059 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48783 to i64
%ae48783 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58059)
store volatile %struct.ScmObj* %ae48783, %struct.ScmObj** %stackaddr$makeclosure58058, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48783, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48783, %struct.ScmObj* %Ycmb48033, i64 1)
%argslist57929$Ycmb480330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58060 = alloca %struct.ScmObj*, align 8
%argslist57929$Ycmb480331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48178, %struct.ScmObj* %argslist57929$Ycmb480330)
store volatile %struct.ScmObj* %argslist57929$Ycmb480331, %struct.ScmObj** %stackaddr$prim58060, align 8
%stackaddr$prim58061 = alloca %struct.ScmObj*, align 8
%argslist57929$Ycmb480332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48783, %struct.ScmObj* %argslist57929$Ycmb480331)
store volatile %struct.ScmObj* %argslist57929$Ycmb480332, %struct.ScmObj** %stackaddr$prim58061, align 8
%clofunc58062 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48033)
musttail call tailcc void %clofunc58062(%struct.ScmObj* %Ycmb48033, %struct.ScmObj* %argslist57929$Ycmb480332)
ret void
}

define tailcc void @proc_clo$ae48783(%struct.ScmObj* %env$ae48783,%struct.ScmObj* %current_45args57286) {
%stackaddr$env-ref58063 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48783, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref58063
%stackaddr$env-ref58064 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48783, i64 1)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref58064
%stackaddr$prim58065 = alloca %struct.ScmObj*, align 8
%_95k48300 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57286)
store volatile %struct.ScmObj* %_95k48300, %struct.ScmObj** %stackaddr$prim58065, align 8
%stackaddr$prim58066 = alloca %struct.ScmObj*, align 8
%current_45args57287 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57286)
store volatile %struct.ScmObj* %current_45args57287, %struct.ScmObj** %stackaddr$prim58066, align 8
%stackaddr$prim58067 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57287)
store volatile %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$prim58067, align 8
%stackaddr$makeclosure58068 = alloca %struct.ScmObj*, align 8
%fptrToInt58069 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48785 to i64
%ae48785 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58069)
store volatile %struct.ScmObj* %ae48785, %struct.ScmObj** %stackaddr$makeclosure58068, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48785, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48785, %struct.ScmObj* %_37map148050, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48785, %struct.ScmObj* %Ycmb48033, i64 2)
%ae48786 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58070 = alloca %struct.ScmObj*, align 8
%fptrToInt58071 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48787 to i64
%ae48787 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58071)
store volatile %struct.ScmObj* %ae48787, %struct.ScmObj** %stackaddr$makeclosure58070, align 8
%argslist57928$ae487850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58072 = alloca %struct.ScmObj*, align 8
%argslist57928$ae487851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48787, %struct.ScmObj* %argslist57928$ae487850)
store volatile %struct.ScmObj* %argslist57928$ae487851, %struct.ScmObj** %stackaddr$prim58072, align 8
%stackaddr$prim58073 = alloca %struct.ScmObj*, align 8
%argslist57928$ae487852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48786, %struct.ScmObj* %argslist57928$ae487851)
store volatile %struct.ScmObj* %argslist57928$ae487852, %struct.ScmObj** %stackaddr$prim58073, align 8
%clofunc58074 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48785)
musttail call tailcc void %clofunc58074(%struct.ScmObj* %ae48785, %struct.ScmObj* %argslist57928$ae487852)
ret void
}

define tailcc void @proc_clo$ae48785(%struct.ScmObj* %env$ae48785,%struct.ScmObj* %current_45args57289) {
%stackaddr$env-ref58075 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48785, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref58075
%stackaddr$env-ref58076 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48785, i64 1)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref58076
%stackaddr$env-ref58077 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48785, i64 2)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref58077
%stackaddr$prim58078 = alloca %struct.ScmObj*, align 8
%_95k48301 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57289)
store volatile %struct.ScmObj* %_95k48301, %struct.ScmObj** %stackaddr$prim58078, align 8
%stackaddr$prim58079 = alloca %struct.ScmObj*, align 8
%current_45args57290 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57289)
store volatile %struct.ScmObj* %current_45args57290, %struct.ScmObj** %stackaddr$prim58079, align 8
%stackaddr$prim58080 = alloca %struct.ScmObj*, align 8
%anf_45bind48185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57290)
store volatile %struct.ScmObj* %anf_45bind48185, %struct.ScmObj** %stackaddr$prim58080, align 8
%stackaddr$makeclosure58081 = alloca %struct.ScmObj*, align 8
%fptrToInt58082 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48933 to i64
%ae48933 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58082)
store volatile %struct.ScmObj* %ae48933, %struct.ScmObj** %stackaddr$makeclosure58081, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48933, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48933, %struct.ScmObj* %_37map148050, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48933, %struct.ScmObj* %Ycmb48033, i64 2)
%argslist57912$Ycmb480330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58083 = alloca %struct.ScmObj*, align 8
%argslist57912$Ycmb480331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48185, %struct.ScmObj* %argslist57912$Ycmb480330)
store volatile %struct.ScmObj* %argslist57912$Ycmb480331, %struct.ScmObj** %stackaddr$prim58083, align 8
%stackaddr$prim58084 = alloca %struct.ScmObj*, align 8
%argslist57912$Ycmb480332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48933, %struct.ScmObj* %argslist57912$Ycmb480331)
store volatile %struct.ScmObj* %argslist57912$Ycmb480332, %struct.ScmObj** %stackaddr$prim58084, align 8
%clofunc58085 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48033)
musttail call tailcc void %clofunc58085(%struct.ScmObj* %Ycmb48033, %struct.ScmObj* %argslist57912$Ycmb480332)
ret void
}

define tailcc void @proc_clo$ae48933(%struct.ScmObj* %env$ae48933,%struct.ScmObj* %current_45args57292) {
%stackaddr$env-ref58086 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48933, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref58086
%stackaddr$env-ref58087 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48933, i64 1)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref58087
%stackaddr$env-ref58088 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48933, i64 2)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref58088
%stackaddr$prim58089 = alloca %struct.ScmObj*, align 8
%_95k48302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57292)
store volatile %struct.ScmObj* %_95k48302, %struct.ScmObj** %stackaddr$prim58089, align 8
%stackaddr$prim58090 = alloca %struct.ScmObj*, align 8
%current_45args57293 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57292)
store volatile %struct.ScmObj* %current_45args57293, %struct.ScmObj** %stackaddr$prim58090, align 8
%stackaddr$prim58091 = alloca %struct.ScmObj*, align 8
%_37take48046 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57293)
store volatile %struct.ScmObj* %_37take48046, %struct.ScmObj** %stackaddr$prim58091, align 8
%stackaddr$makeclosure58092 = alloca %struct.ScmObj*, align 8
%fptrToInt58093 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48935 to i64
%ae48935 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58093)
store volatile %struct.ScmObj* %ae48935, %struct.ScmObj** %stackaddr$makeclosure58092, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48935, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48935, %struct.ScmObj* %_37map148050, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48935, %struct.ScmObj* %Ycmb48033, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48935, %struct.ScmObj* %_37take48046, i64 3)
%ae48936 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58094 = alloca %struct.ScmObj*, align 8
%fptrToInt58095 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48937 to i64
%ae48937 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58095)
store volatile %struct.ScmObj* %ae48937, %struct.ScmObj** %stackaddr$makeclosure58094, align 8
%argslist57911$ae489350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58096 = alloca %struct.ScmObj*, align 8
%argslist57911$ae489351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48937, %struct.ScmObj* %argslist57911$ae489350)
store volatile %struct.ScmObj* %argslist57911$ae489351, %struct.ScmObj** %stackaddr$prim58096, align 8
%stackaddr$prim58097 = alloca %struct.ScmObj*, align 8
%argslist57911$ae489352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48936, %struct.ScmObj* %argslist57911$ae489351)
store volatile %struct.ScmObj* %argslist57911$ae489352, %struct.ScmObj** %stackaddr$prim58097, align 8
%clofunc58098 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48935)
musttail call tailcc void %clofunc58098(%struct.ScmObj* %ae48935, %struct.ScmObj* %argslist57911$ae489352)
ret void
}

define tailcc void @proc_clo$ae48935(%struct.ScmObj* %env$ae48935,%struct.ScmObj* %current_45args57295) {
%stackaddr$env-ref58099 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48935, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref58099
%stackaddr$env-ref58100 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48935, i64 1)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref58100
%stackaddr$env-ref58101 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48935, i64 2)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref58101
%stackaddr$env-ref58102 = alloca %struct.ScmObj*, align 8
%_37take48046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48935, i64 3)
store %struct.ScmObj* %_37take48046, %struct.ScmObj** %stackaddr$env-ref58102
%stackaddr$prim58103 = alloca %struct.ScmObj*, align 8
%_95k48303 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57295)
store volatile %struct.ScmObj* %_95k48303, %struct.ScmObj** %stackaddr$prim58103, align 8
%stackaddr$prim58104 = alloca %struct.ScmObj*, align 8
%current_45args57296 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57295)
store volatile %struct.ScmObj* %current_45args57296, %struct.ScmObj** %stackaddr$prim58104, align 8
%stackaddr$prim58105 = alloca %struct.ScmObj*, align 8
%anf_45bind48189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57296)
store volatile %struct.ScmObj* %anf_45bind48189, %struct.ScmObj** %stackaddr$prim58105, align 8
%stackaddr$makeclosure58106 = alloca %struct.ScmObj*, align 8
%fptrToInt58107 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49016 to i64
%ae49016 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt58107)
store volatile %struct.ScmObj* %ae49016, %struct.ScmObj** %stackaddr$makeclosure58106, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49016, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49016, %struct.ScmObj* %_37map148050, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49016, %struct.ScmObj* %Ycmb48033, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49016, %struct.ScmObj* %_37take48046, i64 3)
%argslist57897$Ycmb480330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58108 = alloca %struct.ScmObj*, align 8
%argslist57897$Ycmb480331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48189, %struct.ScmObj* %argslist57897$Ycmb480330)
store volatile %struct.ScmObj* %argslist57897$Ycmb480331, %struct.ScmObj** %stackaddr$prim58108, align 8
%stackaddr$prim58109 = alloca %struct.ScmObj*, align 8
%argslist57897$Ycmb480332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49016, %struct.ScmObj* %argslist57897$Ycmb480331)
store volatile %struct.ScmObj* %argslist57897$Ycmb480332, %struct.ScmObj** %stackaddr$prim58109, align 8
%clofunc58110 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48033)
musttail call tailcc void %clofunc58110(%struct.ScmObj* %Ycmb48033, %struct.ScmObj* %argslist57897$Ycmb480332)
ret void
}

define tailcc void @proc_clo$ae49016(%struct.ScmObj* %env$ae49016,%struct.ScmObj* %current_45args57298) {
%stackaddr$env-ref58111 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49016, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref58111
%stackaddr$env-ref58112 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49016, i64 1)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref58112
%stackaddr$env-ref58113 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49016, i64 2)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref58113
%stackaddr$env-ref58114 = alloca %struct.ScmObj*, align 8
%_37take48046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49016, i64 3)
store %struct.ScmObj* %_37take48046, %struct.ScmObj** %stackaddr$env-ref58114
%stackaddr$prim58115 = alloca %struct.ScmObj*, align 8
%_95k48304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57298)
store volatile %struct.ScmObj* %_95k48304, %struct.ScmObj** %stackaddr$prim58115, align 8
%stackaddr$prim58116 = alloca %struct.ScmObj*, align 8
%current_45args57299 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57298)
store volatile %struct.ScmObj* %current_45args57299, %struct.ScmObj** %stackaddr$prim58116, align 8
%stackaddr$prim58117 = alloca %struct.ScmObj*, align 8
%_37length48043 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57299)
store volatile %struct.ScmObj* %_37length48043, %struct.ScmObj** %stackaddr$prim58117, align 8
%stackaddr$makeclosure58118 = alloca %struct.ScmObj*, align 8
%fptrToInt58119 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49018 to i64
%ae49018 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt58119)
store volatile %struct.ScmObj* %ae49018, %struct.ScmObj** %stackaddr$makeclosure58118, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49018, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49018, %struct.ScmObj* %_37map148050, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49018, %struct.ScmObj* %Ycmb48033, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49018, %struct.ScmObj* %_37take48046, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49018, %struct.ScmObj* %_37length48043, i64 4)
%ae49019 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58120 = alloca %struct.ScmObj*, align 8
%fptrToInt58121 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49020 to i64
%ae49020 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58121)
store volatile %struct.ScmObj* %ae49020, %struct.ScmObj** %stackaddr$makeclosure58120, align 8
%argslist57896$ae490180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58122 = alloca %struct.ScmObj*, align 8
%argslist57896$ae490181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49020, %struct.ScmObj* %argslist57896$ae490180)
store volatile %struct.ScmObj* %argslist57896$ae490181, %struct.ScmObj** %stackaddr$prim58122, align 8
%stackaddr$prim58123 = alloca %struct.ScmObj*, align 8
%argslist57896$ae490182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49019, %struct.ScmObj* %argslist57896$ae490181)
store volatile %struct.ScmObj* %argslist57896$ae490182, %struct.ScmObj** %stackaddr$prim58123, align 8
%clofunc58124 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49018)
musttail call tailcc void %clofunc58124(%struct.ScmObj* %ae49018, %struct.ScmObj* %argslist57896$ae490182)
ret void
}

define tailcc void @proc_clo$ae49018(%struct.ScmObj* %env$ae49018,%struct.ScmObj* %current_45args57301) {
%stackaddr$env-ref58125 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49018, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref58125
%stackaddr$env-ref58126 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49018, i64 1)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref58126
%stackaddr$env-ref58127 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49018, i64 2)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref58127
%stackaddr$env-ref58128 = alloca %struct.ScmObj*, align 8
%_37take48046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49018, i64 3)
store %struct.ScmObj* %_37take48046, %struct.ScmObj** %stackaddr$env-ref58128
%stackaddr$env-ref58129 = alloca %struct.ScmObj*, align 8
%_37length48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49018, i64 4)
store %struct.ScmObj* %_37length48043, %struct.ScmObj** %stackaddr$env-ref58129
%stackaddr$prim58130 = alloca %struct.ScmObj*, align 8
%_95k48305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57301)
store volatile %struct.ScmObj* %_95k48305, %struct.ScmObj** %stackaddr$prim58130, align 8
%stackaddr$prim58131 = alloca %struct.ScmObj*, align 8
%current_45args57302 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57301)
store volatile %struct.ScmObj* %current_45args57302, %struct.ScmObj** %stackaddr$prim58131, align 8
%stackaddr$prim58132 = alloca %struct.ScmObj*, align 8
%anf_45bind48194 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57302)
store volatile %struct.ScmObj* %anf_45bind48194, %struct.ScmObj** %stackaddr$prim58132, align 8
%stackaddr$makeclosure58133 = alloca %struct.ScmObj*, align 8
%fptrToInt58134 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49095 to i64
%ae49095 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt58134)
store volatile %struct.ScmObj* %ae49095, %struct.ScmObj** %stackaddr$makeclosure58133, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49095, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49095, %struct.ScmObj* %_37map148050, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49095, %struct.ScmObj* %Ycmb48033, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49095, %struct.ScmObj* %_37take48046, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49095, %struct.ScmObj* %_37length48043, i64 4)
%argslist57880$Ycmb480330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58135 = alloca %struct.ScmObj*, align 8
%argslist57880$Ycmb480331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48194, %struct.ScmObj* %argslist57880$Ycmb480330)
store volatile %struct.ScmObj* %argslist57880$Ycmb480331, %struct.ScmObj** %stackaddr$prim58135, align 8
%stackaddr$prim58136 = alloca %struct.ScmObj*, align 8
%argslist57880$Ycmb480332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49095, %struct.ScmObj* %argslist57880$Ycmb480331)
store volatile %struct.ScmObj* %argslist57880$Ycmb480332, %struct.ScmObj** %stackaddr$prim58136, align 8
%clofunc58137 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48033)
musttail call tailcc void %clofunc58137(%struct.ScmObj* %Ycmb48033, %struct.ScmObj* %argslist57880$Ycmb480332)
ret void
}

define tailcc void @proc_clo$ae49095(%struct.ScmObj* %env$ae49095,%struct.ScmObj* %current_45args57304) {
%stackaddr$env-ref58138 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49095, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref58138
%stackaddr$env-ref58139 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49095, i64 1)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref58139
%stackaddr$env-ref58140 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49095, i64 2)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref58140
%stackaddr$env-ref58141 = alloca %struct.ScmObj*, align 8
%_37take48046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49095, i64 3)
store %struct.ScmObj* %_37take48046, %struct.ScmObj** %stackaddr$env-ref58141
%stackaddr$env-ref58142 = alloca %struct.ScmObj*, align 8
%_37length48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49095, i64 4)
store %struct.ScmObj* %_37length48043, %struct.ScmObj** %stackaddr$env-ref58142
%stackaddr$prim58143 = alloca %struct.ScmObj*, align 8
%_95k48306 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57304)
store volatile %struct.ScmObj* %_95k48306, %struct.ScmObj** %stackaddr$prim58143, align 8
%stackaddr$prim58144 = alloca %struct.ScmObj*, align 8
%current_45args57305 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57304)
store volatile %struct.ScmObj* %current_45args57305, %struct.ScmObj** %stackaddr$prim58144, align 8
%stackaddr$prim58145 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57305)
store volatile %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$prim58145, align 8
%stackaddr$makeclosure58146 = alloca %struct.ScmObj*, align 8
%fptrToInt58147 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49097 to i64
%ae49097 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt58147)
store volatile %struct.ScmObj* %ae49097, %struct.ScmObj** %stackaddr$makeclosure58146, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49097, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49097, %struct.ScmObj* %_37foldl148038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49097, %struct.ScmObj* %_37map148050, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49097, %struct.ScmObj* %Ycmb48033, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49097, %struct.ScmObj* %_37take48046, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49097, %struct.ScmObj* %_37length48043, i64 5)
%ae49098 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58148 = alloca %struct.ScmObj*, align 8
%fptrToInt58149 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49099 to i64
%ae49099 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58149)
store volatile %struct.ScmObj* %ae49099, %struct.ScmObj** %stackaddr$makeclosure58148, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49099, %struct.ScmObj* %_37foldl148038, i64 0)
%argslist57879$ae490970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58150 = alloca %struct.ScmObj*, align 8
%argslist57879$ae490971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49099, %struct.ScmObj* %argslist57879$ae490970)
store volatile %struct.ScmObj* %argslist57879$ae490971, %struct.ScmObj** %stackaddr$prim58150, align 8
%stackaddr$prim58151 = alloca %struct.ScmObj*, align 8
%argslist57879$ae490972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49098, %struct.ScmObj* %argslist57879$ae490971)
store volatile %struct.ScmObj* %argslist57879$ae490972, %struct.ScmObj** %stackaddr$prim58151, align 8
%clofunc58152 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49097)
musttail call tailcc void %clofunc58152(%struct.ScmObj* %ae49097, %struct.ScmObj* %argslist57879$ae490972)
ret void
}

define tailcc void @proc_clo$ae49097(%struct.ScmObj* %env$ae49097,%struct.ScmObj* %current_45args57307) {
%stackaddr$env-ref58153 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49097, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref58153
%stackaddr$env-ref58154 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49097, i64 1)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref58154
%stackaddr$env-ref58155 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49097, i64 2)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref58155
%stackaddr$env-ref58156 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49097, i64 3)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref58156
%stackaddr$env-ref58157 = alloca %struct.ScmObj*, align 8
%_37take48046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49097, i64 4)
store %struct.ScmObj* %_37take48046, %struct.ScmObj** %stackaddr$env-ref58157
%stackaddr$env-ref58158 = alloca %struct.ScmObj*, align 8
%_37length48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49097, i64 5)
store %struct.ScmObj* %_37length48043, %struct.ScmObj** %stackaddr$env-ref58158
%stackaddr$prim58159 = alloca %struct.ScmObj*, align 8
%_95k48307 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57307)
store volatile %struct.ScmObj* %_95k48307, %struct.ScmObj** %stackaddr$prim58159, align 8
%stackaddr$prim58160 = alloca %struct.ScmObj*, align 8
%current_45args57308 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57307)
store volatile %struct.ScmObj* %current_45args57308, %struct.ScmObj** %stackaddr$prim58160, align 8
%stackaddr$prim58161 = alloca %struct.ScmObj*, align 8
%_37last48076 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57308)
store volatile %struct.ScmObj* %_37last48076, %struct.ScmObj** %stackaddr$prim58161, align 8
%stackaddr$makeclosure58162 = alloca %struct.ScmObj*, align 8
%fptrToInt58163 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49151 to i64
%ae49151 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt58163)
store volatile %struct.ScmObj* %ae49151, %struct.ScmObj** %stackaddr$makeclosure58162, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49151, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49151, %struct.ScmObj* %_37foldl148038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49151, %struct.ScmObj* %_37map148050, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49151, %struct.ScmObj* %Ycmb48033, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49151, %struct.ScmObj* %_37last48076, i64 4)
%ae49152 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58164 = alloca %struct.ScmObj*, align 8
%fptrToInt58165 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49153 to i64
%ae49153 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58165)
store volatile %struct.ScmObj* %ae49153, %struct.ScmObj** %stackaddr$makeclosure58164, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49153, %struct.ScmObj* %_37take48046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49153, %struct.ScmObj* %_37length48043, i64 1)
%argslist57865$ae491510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58166 = alloca %struct.ScmObj*, align 8
%argslist57865$ae491511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49153, %struct.ScmObj* %argslist57865$ae491510)
store volatile %struct.ScmObj* %argslist57865$ae491511, %struct.ScmObj** %stackaddr$prim58166, align 8
%stackaddr$prim58167 = alloca %struct.ScmObj*, align 8
%argslist57865$ae491512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49152, %struct.ScmObj* %argslist57865$ae491511)
store volatile %struct.ScmObj* %argslist57865$ae491512, %struct.ScmObj** %stackaddr$prim58167, align 8
%clofunc58168 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49151)
musttail call tailcc void %clofunc58168(%struct.ScmObj* %ae49151, %struct.ScmObj* %argslist57865$ae491512)
ret void
}

define tailcc void @proc_clo$ae49151(%struct.ScmObj* %env$ae49151,%struct.ScmObj* %current_45args57310) {
%stackaddr$env-ref58169 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49151, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref58169
%stackaddr$env-ref58170 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49151, i64 1)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref58170
%stackaddr$env-ref58171 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49151, i64 2)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref58171
%stackaddr$env-ref58172 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49151, i64 3)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref58172
%stackaddr$env-ref58173 = alloca %struct.ScmObj*, align 8
%_37last48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49151, i64 4)
store %struct.ScmObj* %_37last48076, %struct.ScmObj** %stackaddr$env-ref58173
%stackaddr$prim58174 = alloca %struct.ScmObj*, align 8
%_95k48308 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57310)
store volatile %struct.ScmObj* %_95k48308, %struct.ScmObj** %stackaddr$prim58174, align 8
%stackaddr$prim58175 = alloca %struct.ScmObj*, align 8
%current_45args57311 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57310)
store volatile %struct.ScmObj* %current_45args57311, %struct.ScmObj** %stackaddr$prim58175, align 8
%stackaddr$prim58176 = alloca %struct.ScmObj*, align 8
%_37drop_45right48073 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57311)
store volatile %struct.ScmObj* %_37drop_45right48073, %struct.ScmObj** %stackaddr$prim58176, align 8
%stackaddr$makeclosure58177 = alloca %struct.ScmObj*, align 8
%fptrToInt58178 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49181 to i64
%ae49181 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt58178)
store volatile %struct.ScmObj* %ae49181, %struct.ScmObj** %stackaddr$makeclosure58177, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49181, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49181, %struct.ScmObj* %_37foldl148038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49181, %struct.ScmObj* %_37drop_45right48073, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49181, %struct.ScmObj* %Ycmb48033, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49181, %struct.ScmObj* %_37last48076, i64 4)
%ae49182 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58179 = alloca %struct.ScmObj*, align 8
%fptrToInt58180 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49183 to i64
%ae49183 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58180)
store volatile %struct.ScmObj* %ae49183, %struct.ScmObj** %stackaddr$makeclosure58179, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49183, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49183, %struct.ScmObj* %_37map148050, i64 1)
%argslist57855$ae491810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58181 = alloca %struct.ScmObj*, align 8
%argslist57855$ae491811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49183, %struct.ScmObj* %argslist57855$ae491810)
store volatile %struct.ScmObj* %argslist57855$ae491811, %struct.ScmObj** %stackaddr$prim58181, align 8
%stackaddr$prim58182 = alloca %struct.ScmObj*, align 8
%argslist57855$ae491812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49182, %struct.ScmObj* %argslist57855$ae491811)
store volatile %struct.ScmObj* %argslist57855$ae491812, %struct.ScmObj** %stackaddr$prim58182, align 8
%clofunc58183 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49181)
musttail call tailcc void %clofunc58183(%struct.ScmObj* %ae49181, %struct.ScmObj* %argslist57855$ae491812)
ret void
}

define tailcc void @proc_clo$ae49181(%struct.ScmObj* %env$ae49181,%struct.ScmObj* %current_45args57313) {
%stackaddr$env-ref58184 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49181, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref58184
%stackaddr$env-ref58185 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49181, i64 1)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref58185
%stackaddr$env-ref58186 = alloca %struct.ScmObj*, align 8
%_37drop_45right48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49181, i64 2)
store %struct.ScmObj* %_37drop_45right48073, %struct.ScmObj** %stackaddr$env-ref58186
%stackaddr$env-ref58187 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49181, i64 3)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref58187
%stackaddr$env-ref58188 = alloca %struct.ScmObj*, align 8
%_37last48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49181, i64 4)
store %struct.ScmObj* %_37last48076, %struct.ScmObj** %stackaddr$env-ref58188
%stackaddr$prim58189 = alloca %struct.ScmObj*, align 8
%_95k48309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57313)
store volatile %struct.ScmObj* %_95k48309, %struct.ScmObj** %stackaddr$prim58189, align 8
%stackaddr$prim58190 = alloca %struct.ScmObj*, align 8
%current_45args57314 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57313)
store volatile %struct.ScmObj* %current_45args57314, %struct.ScmObj** %stackaddr$prim58190, align 8
%stackaddr$prim58191 = alloca %struct.ScmObj*, align 8
%anf_45bind48210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57314)
store volatile %struct.ScmObj* %anf_45bind48210, %struct.ScmObj** %stackaddr$prim58191, align 8
%stackaddr$makeclosure58192 = alloca %struct.ScmObj*, align 8
%fptrToInt58193 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49565 to i64
%ae49565 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt58193)
store volatile %struct.ScmObj* %ae49565, %struct.ScmObj** %stackaddr$makeclosure58192, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49565, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49565, %struct.ScmObj* %_37foldl148038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49565, %struct.ScmObj* %_37drop_45right48073, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49565, %struct.ScmObj* %Ycmb48033, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49565, %struct.ScmObj* %_37last48076, i64 4)
%argslist57795$Ycmb480330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58194 = alloca %struct.ScmObj*, align 8
%argslist57795$Ycmb480331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48210, %struct.ScmObj* %argslist57795$Ycmb480330)
store volatile %struct.ScmObj* %argslist57795$Ycmb480331, %struct.ScmObj** %stackaddr$prim58194, align 8
%stackaddr$prim58195 = alloca %struct.ScmObj*, align 8
%argslist57795$Ycmb480332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49565, %struct.ScmObj* %argslist57795$Ycmb480331)
store volatile %struct.ScmObj* %argslist57795$Ycmb480332, %struct.ScmObj** %stackaddr$prim58195, align 8
%clofunc58196 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48033)
musttail call tailcc void %clofunc58196(%struct.ScmObj* %Ycmb48033, %struct.ScmObj* %argslist57795$Ycmb480332)
ret void
}

define tailcc void @proc_clo$ae49565(%struct.ScmObj* %env$ae49565,%struct.ScmObj* %current_45args57316) {
%stackaddr$env-ref58197 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49565, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref58197
%stackaddr$env-ref58198 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49565, i64 1)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref58198
%stackaddr$env-ref58199 = alloca %struct.ScmObj*, align 8
%_37drop_45right48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49565, i64 2)
store %struct.ScmObj* %_37drop_45right48073, %struct.ScmObj** %stackaddr$env-ref58199
%stackaddr$env-ref58200 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49565, i64 3)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref58200
%stackaddr$env-ref58201 = alloca %struct.ScmObj*, align 8
%_37last48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49565, i64 4)
store %struct.ScmObj* %_37last48076, %struct.ScmObj** %stackaddr$env-ref58201
%stackaddr$prim58202 = alloca %struct.ScmObj*, align 8
%_95k48310 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57316)
store volatile %struct.ScmObj* %_95k48310, %struct.ScmObj** %stackaddr$prim58202, align 8
%stackaddr$prim58203 = alloca %struct.ScmObj*, align 8
%current_45args57317 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57316)
store volatile %struct.ScmObj* %current_45args57317, %struct.ScmObj** %stackaddr$prim58203, align 8
%stackaddr$prim58204 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57317)
store volatile %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$prim58204, align 8
%stackaddr$makeclosure58205 = alloca %struct.ScmObj*, align 8
%fptrToInt58206 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49567 to i64
%ae49567 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt58206)
store volatile %struct.ScmObj* %ae49567, %struct.ScmObj** %stackaddr$makeclosure58205, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49567, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49567, %struct.ScmObj* %_37foldl148038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49567, %struct.ScmObj* %_37drop_45right48073, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49567, %struct.ScmObj* %Ycmb48033, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49567, %struct.ScmObj* %_37last48076, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49567, %struct.ScmObj* %_37foldr48059, i64 5)
%ae49568 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58207 = alloca %struct.ScmObj*, align 8
%fptrToInt58208 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49569 to i64
%ae49569 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58208)
store volatile %struct.ScmObj* %ae49569, %struct.ScmObj** %stackaddr$makeclosure58207, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49569, %struct.ScmObj* %_37foldr148054, i64 0)
%argslist57794$ae495670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58209 = alloca %struct.ScmObj*, align 8
%argslist57794$ae495671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49569, %struct.ScmObj* %argslist57794$ae495670)
store volatile %struct.ScmObj* %argslist57794$ae495671, %struct.ScmObj** %stackaddr$prim58209, align 8
%stackaddr$prim58210 = alloca %struct.ScmObj*, align 8
%argslist57794$ae495672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49568, %struct.ScmObj* %argslist57794$ae495671)
store volatile %struct.ScmObj* %argslist57794$ae495672, %struct.ScmObj** %stackaddr$prim58210, align 8
%clofunc58211 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49567)
musttail call tailcc void %clofunc58211(%struct.ScmObj* %ae49567, %struct.ScmObj* %argslist57794$ae495672)
ret void
}

define tailcc void @proc_clo$ae49567(%struct.ScmObj* %env$ae49567,%struct.ScmObj* %current_45args57319) {
%stackaddr$env-ref58212 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49567, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref58212
%stackaddr$env-ref58213 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49567, i64 1)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref58213
%stackaddr$env-ref58214 = alloca %struct.ScmObj*, align 8
%_37drop_45right48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49567, i64 2)
store %struct.ScmObj* %_37drop_45right48073, %struct.ScmObj** %stackaddr$env-ref58214
%stackaddr$env-ref58215 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49567, i64 3)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref58215
%stackaddr$env-ref58216 = alloca %struct.ScmObj*, align 8
%_37last48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49567, i64 4)
store %struct.ScmObj* %_37last48076, %struct.ScmObj** %stackaddr$env-ref58216
%stackaddr$env-ref58217 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49567, i64 5)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref58217
%stackaddr$prim58218 = alloca %struct.ScmObj*, align 8
%_95k48311 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57319)
store volatile %struct.ScmObj* %_95k48311, %struct.ScmObj** %stackaddr$prim58218, align 8
%stackaddr$prim58219 = alloca %struct.ScmObj*, align 8
%current_45args57320 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57319)
store volatile %struct.ScmObj* %current_45args57320, %struct.ScmObj** %stackaddr$prim58219, align 8
%stackaddr$prim58220 = alloca %struct.ScmObj*, align 8
%_37map148085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57320)
store volatile %struct.ScmObj* %_37map148085, %struct.ScmObj** %stackaddr$prim58220, align 8
%stackaddr$makeclosure58221 = alloca %struct.ScmObj*, align 8
%fptrToInt58222 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49644 to i64
%ae49644 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt58222)
store volatile %struct.ScmObj* %ae49644, %struct.ScmObj** %stackaddr$makeclosure58221, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49644, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49644, %struct.ScmObj* %_37foldl148038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49644, %struct.ScmObj* %_37map148085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49644, %struct.ScmObj* %Ycmb48033, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49644, %struct.ScmObj* %_37foldr48059, i64 4)
%ae49645 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58223 = alloca %struct.ScmObj*, align 8
%fptrToInt58224 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49646 to i64
%ae49646 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58224)
store volatile %struct.ScmObj* %ae49646, %struct.ScmObj** %stackaddr$makeclosure58223, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49646, %struct.ScmObj* %_37drop_45right48073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49646, %struct.ScmObj* %_37last48076, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49646, %struct.ScmObj* %_37foldr48059, i64 2)
%argslist57775$ae496440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58225 = alloca %struct.ScmObj*, align 8
%argslist57775$ae496441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49646, %struct.ScmObj* %argslist57775$ae496440)
store volatile %struct.ScmObj* %argslist57775$ae496441, %struct.ScmObj** %stackaddr$prim58225, align 8
%stackaddr$prim58226 = alloca %struct.ScmObj*, align 8
%argslist57775$ae496442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49645, %struct.ScmObj* %argslist57775$ae496441)
store volatile %struct.ScmObj* %argslist57775$ae496442, %struct.ScmObj** %stackaddr$prim58226, align 8
%clofunc58227 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49644)
musttail call tailcc void %clofunc58227(%struct.ScmObj* %ae49644, %struct.ScmObj* %argslist57775$ae496442)
ret void
}

define tailcc void @proc_clo$ae49644(%struct.ScmObj* %env$ae49644,%struct.ScmObj* %current_45args57322) {
%stackaddr$env-ref58228 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49644, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref58228
%stackaddr$env-ref58229 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49644, i64 1)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref58229
%stackaddr$env-ref58230 = alloca %struct.ScmObj*, align 8
%_37map148085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49644, i64 2)
store %struct.ScmObj* %_37map148085, %struct.ScmObj** %stackaddr$env-ref58230
%stackaddr$env-ref58231 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49644, i64 3)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref58231
%stackaddr$env-ref58232 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49644, i64 4)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref58232
%stackaddr$prim58233 = alloca %struct.ScmObj*, align 8
%_95k48312 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57322)
store volatile %struct.ScmObj* %_95k48312, %struct.ScmObj** %stackaddr$prim58233, align 8
%stackaddr$prim58234 = alloca %struct.ScmObj*, align 8
%current_45args57323 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57322)
store volatile %struct.ScmObj* %current_45args57323, %struct.ScmObj** %stackaddr$prim58234, align 8
%stackaddr$prim58235 = alloca %struct.ScmObj*, align 8
%_37map48080 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57323)
store volatile %struct.ScmObj* %_37map48080, %struct.ScmObj** %stackaddr$prim58235, align 8
%stackaddr$makeclosure58236 = alloca %struct.ScmObj*, align 8
%fptrToInt58237 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49790 to i64
%ae49790 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58237)
store volatile %struct.ScmObj* %ae49790, %struct.ScmObj** %stackaddr$makeclosure58236, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49790, %struct.ScmObj* %_37foldl148038, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49790, %struct.ScmObj* %Ycmb48033, i64 1)
%ae49791 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58238 = alloca %struct.ScmObj*, align 8
%fptrToInt58239 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49792 to i64
%ae49792 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58239)
store volatile %struct.ScmObj* %ae49792, %struct.ScmObj** %stackaddr$makeclosure58238, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49792, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49792, %struct.ScmObj* %_37map148085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49792, %struct.ScmObj* %_37foldr48059, i64 2)
%argslist57758$ae497900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58240 = alloca %struct.ScmObj*, align 8
%argslist57758$ae497901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49792, %struct.ScmObj* %argslist57758$ae497900)
store volatile %struct.ScmObj* %argslist57758$ae497901, %struct.ScmObj** %stackaddr$prim58240, align 8
%stackaddr$prim58241 = alloca %struct.ScmObj*, align 8
%argslist57758$ae497902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49791, %struct.ScmObj* %argslist57758$ae497901)
store volatile %struct.ScmObj* %argslist57758$ae497902, %struct.ScmObj** %stackaddr$prim58241, align 8
%clofunc58242 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49790)
musttail call tailcc void %clofunc58242(%struct.ScmObj* %ae49790, %struct.ScmObj* %argslist57758$ae497902)
ret void
}

define tailcc void @proc_clo$ae49790(%struct.ScmObj* %env$ae49790,%struct.ScmObj* %current_45args57325) {
%stackaddr$env-ref58243 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49790, i64 0)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref58243
%stackaddr$env-ref58244 = alloca %struct.ScmObj*, align 8
%Ycmb48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49790, i64 1)
store %struct.ScmObj* %Ycmb48033, %struct.ScmObj** %stackaddr$env-ref58244
%stackaddr$prim58245 = alloca %struct.ScmObj*, align 8
%_95k48313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57325)
store volatile %struct.ScmObj* %_95k48313, %struct.ScmObj** %stackaddr$prim58245, align 8
%stackaddr$prim58246 = alloca %struct.ScmObj*, align 8
%current_45args57326 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57325)
store volatile %struct.ScmObj* %current_45args57326, %struct.ScmObj** %stackaddr$prim58246, align 8
%stackaddr$prim58247 = alloca %struct.ScmObj*, align 8
%anf_45bind48230 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57326)
store volatile %struct.ScmObj* %anf_45bind48230, %struct.ScmObj** %stackaddr$prim58247, align 8
%stackaddr$makeclosure58248 = alloca %struct.ScmObj*, align 8
%fptrToInt58249 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50182 to i64
%ae50182 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58249)
store volatile %struct.ScmObj* %ae50182, %struct.ScmObj** %stackaddr$makeclosure58248, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50182, %struct.ScmObj* %_37foldl148038, i64 0)
%argslist57698$Ycmb480330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58250 = alloca %struct.ScmObj*, align 8
%argslist57698$Ycmb480331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48230, %struct.ScmObj* %argslist57698$Ycmb480330)
store volatile %struct.ScmObj* %argslist57698$Ycmb480331, %struct.ScmObj** %stackaddr$prim58250, align 8
%stackaddr$prim58251 = alloca %struct.ScmObj*, align 8
%argslist57698$Ycmb480332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50182, %struct.ScmObj* %argslist57698$Ycmb480331)
store volatile %struct.ScmObj* %argslist57698$Ycmb480332, %struct.ScmObj** %stackaddr$prim58251, align 8
%clofunc58252 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48033)
musttail call tailcc void %clofunc58252(%struct.ScmObj* %Ycmb48033, %struct.ScmObj* %argslist57698$Ycmb480332)
ret void
}

define tailcc void @proc_clo$ae50182(%struct.ScmObj* %env$ae50182,%struct.ScmObj* %current_45args57328) {
%stackaddr$env-ref58253 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50182, i64 0)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref58253
%stackaddr$prim58254 = alloca %struct.ScmObj*, align 8
%_95k48314 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57328)
store volatile %struct.ScmObj* %_95k48314, %struct.ScmObj** %stackaddr$prim58254, align 8
%stackaddr$prim58255 = alloca %struct.ScmObj*, align 8
%current_45args57329 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57328)
store volatile %struct.ScmObj* %current_45args57329, %struct.ScmObj** %stackaddr$prim58255, align 8
%stackaddr$prim58256 = alloca %struct.ScmObj*, align 8
%_37foldl48136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57329)
store volatile %struct.ScmObj* %_37foldl48136, %struct.ScmObj** %stackaddr$prim58256, align 8
%stackaddr$makeclosure58257 = alloca %struct.ScmObj*, align 8
%fptrToInt58258 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50184 to i64
%ae50184 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58258)
store volatile %struct.ScmObj* %ae50184, %struct.ScmObj** %stackaddr$makeclosure58257, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50184, %struct.ScmObj* %_37foldl148038, i64 0)
%ae50185 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58259 = alloca %struct.ScmObj*, align 8
%fptrToInt58260 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50186 to i64
%ae50186 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58260)
store volatile %struct.ScmObj* %ae50186, %struct.ScmObj** %stackaddr$makeclosure58259, align 8
%argslist57697$ae501840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58261 = alloca %struct.ScmObj*, align 8
%argslist57697$ae501841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50186, %struct.ScmObj* %argslist57697$ae501840)
store volatile %struct.ScmObj* %argslist57697$ae501841, %struct.ScmObj** %stackaddr$prim58261, align 8
%stackaddr$prim58262 = alloca %struct.ScmObj*, align 8
%argslist57697$ae501842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50185, %struct.ScmObj* %argslist57697$ae501841)
store volatile %struct.ScmObj* %argslist57697$ae501842, %struct.ScmObj** %stackaddr$prim58262, align 8
%clofunc58263 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50184)
musttail call tailcc void %clofunc58263(%struct.ScmObj* %ae50184, %struct.ScmObj* %argslist57697$ae501842)
ret void
}

define tailcc void @proc_clo$ae50184(%struct.ScmObj* %env$ae50184,%struct.ScmObj* %current_45args57331) {
%stackaddr$env-ref58264 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50184, i64 0)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref58264
%stackaddr$prim58265 = alloca %struct.ScmObj*, align 8
%_95k48315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57331)
store volatile %struct.ScmObj* %_95k48315, %struct.ScmObj** %stackaddr$prim58265, align 8
%stackaddr$prim58266 = alloca %struct.ScmObj*, align 8
%current_45args57332 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57331)
store volatile %struct.ScmObj* %current_45args57332, %struct.ScmObj** %stackaddr$prim58266, align 8
%stackaddr$prim58267 = alloca %struct.ScmObj*, align 8
%_37_6248133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57332)
store volatile %struct.ScmObj* %_37_6248133, %struct.ScmObj** %stackaddr$prim58267, align 8
%stackaddr$makeclosure58268 = alloca %struct.ScmObj*, align 8
%fptrToInt58269 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50208 to i64
%ae50208 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58269)
store volatile %struct.ScmObj* %ae50208, %struct.ScmObj** %stackaddr$makeclosure58268, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50208, %struct.ScmObj* %_37foldl148038, i64 0)
%ae50209 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58270 = alloca %struct.ScmObj*, align 8
%fptrToInt58271 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50210 to i64
%ae50210 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58271)
store volatile %struct.ScmObj* %ae50210, %struct.ScmObj** %stackaddr$makeclosure58270, align 8
%argslist57691$ae502080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58272 = alloca %struct.ScmObj*, align 8
%argslist57691$ae502081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50210, %struct.ScmObj* %argslist57691$ae502080)
store volatile %struct.ScmObj* %argslist57691$ae502081, %struct.ScmObj** %stackaddr$prim58272, align 8
%stackaddr$prim58273 = alloca %struct.ScmObj*, align 8
%argslist57691$ae502082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50209, %struct.ScmObj* %argslist57691$ae502081)
store volatile %struct.ScmObj* %argslist57691$ae502082, %struct.ScmObj** %stackaddr$prim58273, align 8
%clofunc58274 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50208)
musttail call tailcc void %clofunc58274(%struct.ScmObj* %ae50208, %struct.ScmObj* %argslist57691$ae502082)
ret void
}

define tailcc void @proc_clo$ae50208(%struct.ScmObj* %env$ae50208,%struct.ScmObj* %current_45args57334) {
%stackaddr$env-ref58275 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50208, i64 0)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref58275
%stackaddr$prim58276 = alloca %struct.ScmObj*, align 8
%_95k48316 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57334)
store volatile %struct.ScmObj* %_95k48316, %struct.ScmObj** %stackaddr$prim58276, align 8
%stackaddr$prim58277 = alloca %struct.ScmObj*, align 8
%current_45args57335 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57334)
store volatile %struct.ScmObj* %current_45args57335, %struct.ScmObj** %stackaddr$prim58277, align 8
%stackaddr$prim58278 = alloca %struct.ScmObj*, align 8
%_37_62_6148130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57335)
store volatile %struct.ScmObj* %_37_62_6148130, %struct.ScmObj** %stackaddr$prim58278, align 8
%ae50232 = call %struct.ScmObj* @const_init_int(i64 1)
%ae50233 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58279 = alloca %struct.ScmObj*, align 8
%_37append48126 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50232, %struct.ScmObj* %ae50233)
store volatile %struct.ScmObj* %_37append48126, %struct.ScmObj** %stackaddr$prim58279, align 8
%stackaddr$makeclosure58280 = alloca %struct.ScmObj*, align 8
%fptrToInt58281 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50234 to i64
%ae50234 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt58281)
store volatile %struct.ScmObj* %ae50234, %struct.ScmObj** %stackaddr$makeclosure58280, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50234, %struct.ScmObj* %_37foldl148038, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50234, %struct.ScmObj* %_37append48126, i64 1)
%ae50235 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58282 = alloca %struct.ScmObj*, align 8
%fptrToInt58283 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50236 to i64
%ae50236 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58283)
store volatile %struct.ScmObj* %ae50236, %struct.ScmObj** %stackaddr$makeclosure58282, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50236, %struct.ScmObj* %_37append48126, i64 0)
%argslist57685$ae502340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58284 = alloca %struct.ScmObj*, align 8
%argslist57685$ae502341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50236, %struct.ScmObj* %argslist57685$ae502340)
store volatile %struct.ScmObj* %argslist57685$ae502341, %struct.ScmObj** %stackaddr$prim58284, align 8
%stackaddr$prim58285 = alloca %struct.ScmObj*, align 8
%argslist57685$ae502342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50235, %struct.ScmObj* %argslist57685$ae502341)
store volatile %struct.ScmObj* %argslist57685$ae502342, %struct.ScmObj** %stackaddr$prim58285, align 8
%clofunc58286 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50234)
musttail call tailcc void %clofunc58286(%struct.ScmObj* %ae50234, %struct.ScmObj* %argslist57685$ae502342)
ret void
}

define tailcc void @proc_clo$ae50234(%struct.ScmObj* %env$ae50234,%struct.ScmObj* %current_45args57337) {
%stackaddr$env-ref58287 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50234, i64 0)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref58287
%stackaddr$env-ref58288 = alloca %struct.ScmObj*, align 8
%_37append48126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50234, i64 1)
store %struct.ScmObj* %_37append48126, %struct.ScmObj** %stackaddr$env-ref58288
%stackaddr$prim58289 = alloca %struct.ScmObj*, align 8
%_95k48317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57337)
store volatile %struct.ScmObj* %_95k48317, %struct.ScmObj** %stackaddr$prim58289, align 8
%stackaddr$prim58290 = alloca %struct.ScmObj*, align 8
%current_45args57338 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57337)
store volatile %struct.ScmObj* %current_45args57338, %struct.ScmObj** %stackaddr$prim58290, align 8
%stackaddr$prim58291 = alloca %struct.ScmObj*, align 8
%anf_45bind48238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57338)
store volatile %struct.ScmObj* %anf_45bind48238, %struct.ScmObj** %stackaddr$prim58291, align 8
%ae50302 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58292 = alloca %struct.ScmObj*, align 8
%_95048127 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append48126, %struct.ScmObj* %ae50302, %struct.ScmObj* %anf_45bind48238)
store volatile %struct.ScmObj* %_95048127, %struct.ScmObj** %stackaddr$prim58292, align 8
%ae50305 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58293 = alloca %struct.ScmObj*, align 8
%_37append48125 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48126, %struct.ScmObj* %ae50305)
store volatile %struct.ScmObj* %_37append48125, %struct.ScmObj** %stackaddr$prim58293, align 8
%stackaddr$makeclosure58294 = alloca %struct.ScmObj*, align 8
%fptrToInt58295 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50306 to i64
%ae50306 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58295)
store volatile %struct.ScmObj* %ae50306, %struct.ScmObj** %stackaddr$makeclosure58294, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50306, %struct.ScmObj* %_37foldl148038, i64 0)
%ae50307 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58296 = alloca %struct.ScmObj*, align 8
%fptrToInt58297 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50308 to i64
%ae50308 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58297)
store volatile %struct.ScmObj* %ae50308, %struct.ScmObj** %stackaddr$makeclosure58296, align 8
%argslist57674$ae503060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58298 = alloca %struct.ScmObj*, align 8
%argslist57674$ae503061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50308, %struct.ScmObj* %argslist57674$ae503060)
store volatile %struct.ScmObj* %argslist57674$ae503061, %struct.ScmObj** %stackaddr$prim58298, align 8
%stackaddr$prim58299 = alloca %struct.ScmObj*, align 8
%argslist57674$ae503062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50307, %struct.ScmObj* %argslist57674$ae503061)
store volatile %struct.ScmObj* %argslist57674$ae503062, %struct.ScmObj** %stackaddr$prim58299, align 8
%clofunc58300 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50306)
musttail call tailcc void %clofunc58300(%struct.ScmObj* %ae50306, %struct.ScmObj* %argslist57674$ae503062)
ret void
}

define tailcc void @proc_clo$ae50306(%struct.ScmObj* %env$ae50306,%struct.ScmObj* %current_45args57340) {
%stackaddr$env-ref58301 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50306, i64 0)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref58301
%stackaddr$prim58302 = alloca %struct.ScmObj*, align 8
%_95k48318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57340)
store volatile %struct.ScmObj* %_95k48318, %struct.ScmObj** %stackaddr$prim58302, align 8
%stackaddr$prim58303 = alloca %struct.ScmObj*, align 8
%current_45args57341 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57340)
store volatile %struct.ScmObj* %current_45args57341, %struct.ScmObj** %stackaddr$prim58303, align 8
%stackaddr$prim58304 = alloca %struct.ScmObj*, align 8
%_37list_6348118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57341)
store volatile %struct.ScmObj* %_37list_6348118, %struct.ScmObj** %stackaddr$prim58304, align 8
%stackaddr$makeclosure58305 = alloca %struct.ScmObj*, align 8
%fptrToInt58306 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50722 to i64
%ae50722 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58306)
store volatile %struct.ScmObj* %ae50722, %struct.ScmObj** %stackaddr$makeclosure58305, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50722, %struct.ScmObj* %_37foldl148038, i64 0)
%ae50723 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58307 = alloca %struct.ScmObj*, align 8
%fptrToInt58308 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50724 to i64
%ae50724 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58308)
store volatile %struct.ScmObj* %ae50724, %struct.ScmObj** %stackaddr$makeclosure58307, align 8
%argslist57649$ae507220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58309 = alloca %struct.ScmObj*, align 8
%argslist57649$ae507221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50724, %struct.ScmObj* %argslist57649$ae507220)
store volatile %struct.ScmObj* %argslist57649$ae507221, %struct.ScmObj** %stackaddr$prim58309, align 8
%stackaddr$prim58310 = alloca %struct.ScmObj*, align 8
%argslist57649$ae507222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50723, %struct.ScmObj* %argslist57649$ae507221)
store volatile %struct.ScmObj* %argslist57649$ae507222, %struct.ScmObj** %stackaddr$prim58310, align 8
%clofunc58311 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50722)
musttail call tailcc void %clofunc58311(%struct.ScmObj* %ae50722, %struct.ScmObj* %argslist57649$ae507222)
ret void
}

define tailcc void @proc_clo$ae50722(%struct.ScmObj* %env$ae50722,%struct.ScmObj* %current_45args57343) {
%stackaddr$env-ref58312 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50722, i64 0)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref58312
%stackaddr$prim58313 = alloca %struct.ScmObj*, align 8
%_95k48319 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57343)
store volatile %struct.ScmObj* %_95k48319, %struct.ScmObj** %stackaddr$prim58313, align 8
%stackaddr$prim58314 = alloca %struct.ScmObj*, align 8
%current_45args57344 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57343)
store volatile %struct.ScmObj* %current_45args57344, %struct.ScmObj** %stackaddr$prim58314, align 8
%stackaddr$prim58315 = alloca %struct.ScmObj*, align 8
%_37drop48109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57344)
store volatile %struct.ScmObj* %_37drop48109, %struct.ScmObj** %stackaddr$prim58315, align 8
%stackaddr$makeclosure58316 = alloca %struct.ScmObj*, align 8
%fptrToInt58317 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51258 to i64
%ae51258 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58317)
store volatile %struct.ScmObj* %ae51258, %struct.ScmObj** %stackaddr$makeclosure58316, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51258, %struct.ScmObj* %_37foldl148038, i64 0)
%ae51259 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58318 = alloca %struct.ScmObj*, align 8
%fptrToInt58319 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51260 to i64
%ae51260 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58319)
store volatile %struct.ScmObj* %ae51260, %struct.ScmObj** %stackaddr$makeclosure58318, align 8
%argslist57625$ae512580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58320 = alloca %struct.ScmObj*, align 8
%argslist57625$ae512581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51260, %struct.ScmObj* %argslist57625$ae512580)
store volatile %struct.ScmObj* %argslist57625$ae512581, %struct.ScmObj** %stackaddr$prim58320, align 8
%stackaddr$prim58321 = alloca %struct.ScmObj*, align 8
%argslist57625$ae512582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51259, %struct.ScmObj* %argslist57625$ae512581)
store volatile %struct.ScmObj* %argslist57625$ae512582, %struct.ScmObj** %stackaddr$prim58321, align 8
%clofunc58322 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51258)
musttail call tailcc void %clofunc58322(%struct.ScmObj* %ae51258, %struct.ScmObj* %argslist57625$ae512582)
ret void
}

define tailcc void @proc_clo$ae51258(%struct.ScmObj* %env$ae51258,%struct.ScmObj* %current_45args57346) {
%stackaddr$env-ref58323 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51258, i64 0)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref58323
%stackaddr$prim58324 = alloca %struct.ScmObj*, align 8
%_95k48320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57346)
store volatile %struct.ScmObj* %_95k48320, %struct.ScmObj** %stackaddr$prim58324, align 8
%stackaddr$prim58325 = alloca %struct.ScmObj*, align 8
%current_45args57347 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57346)
store volatile %struct.ScmObj* %current_45args57347, %struct.ScmObj** %stackaddr$prim58325, align 8
%stackaddr$prim58326 = alloca %struct.ScmObj*, align 8
%_37memv48102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57347)
store volatile %struct.ScmObj* %_37memv48102, %struct.ScmObj** %stackaddr$prim58326, align 8
%stackaddr$makeclosure58327 = alloca %struct.ScmObj*, align 8
%fptrToInt58328 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51662 to i64
%ae51662 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58328)
store volatile %struct.ScmObj* %ae51662, %struct.ScmObj** %stackaddr$makeclosure58327, align 8
%ae51663 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58329 = alloca %struct.ScmObj*, align 8
%fptrToInt58330 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51664 to i64
%ae51664 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58330)
store volatile %struct.ScmObj* %ae51664, %struct.ScmObj** %stackaddr$makeclosure58329, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51664, %struct.ScmObj* %_37foldl148038, i64 0)
%argslist57599$ae516620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58331 = alloca %struct.ScmObj*, align 8
%argslist57599$ae516621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51664, %struct.ScmObj* %argslist57599$ae516620)
store volatile %struct.ScmObj* %argslist57599$ae516621, %struct.ScmObj** %stackaddr$prim58331, align 8
%stackaddr$prim58332 = alloca %struct.ScmObj*, align 8
%argslist57599$ae516622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51663, %struct.ScmObj* %argslist57599$ae516621)
store volatile %struct.ScmObj* %argslist57599$ae516622, %struct.ScmObj** %stackaddr$prim58332, align 8
%clofunc58333 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51662)
musttail call tailcc void %clofunc58333(%struct.ScmObj* %ae51662, %struct.ScmObj* %argslist57599$ae516622)
ret void
}

define tailcc void @proc_clo$ae51662(%struct.ScmObj* %env$ae51662,%struct.ScmObj* %current_45args57349) {
%stackaddr$prim58334 = alloca %struct.ScmObj*, align 8
%_95k48321 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57349)
store volatile %struct.ScmObj* %_95k48321, %struct.ScmObj** %stackaddr$prim58334, align 8
%stackaddr$prim58335 = alloca %struct.ScmObj*, align 8
%current_45args57350 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57349)
store volatile %struct.ScmObj* %current_45args57350, %struct.ScmObj** %stackaddr$prim58335, align 8
%stackaddr$prim58336 = alloca %struct.ScmObj*, align 8
%_37_4748098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57350)
store volatile %struct.ScmObj* %_37_4748098, %struct.ScmObj** %stackaddr$prim58336, align 8
%stackaddr$makeclosure58337 = alloca %struct.ScmObj*, align 8
%fptrToInt58338 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51760 to i64
%ae51760 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58338)
store volatile %struct.ScmObj* %ae51760, %struct.ScmObj** %stackaddr$makeclosure58337, align 8
%ae51761 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58339 = alloca %struct.ScmObj*, align 8
%fptrToInt58340 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51762 to i64
%ae51762 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58340)
store volatile %struct.ScmObj* %ae51762, %struct.ScmObj** %stackaddr$makeclosure58339, align 8
%argslist57586$ae517600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58341 = alloca %struct.ScmObj*, align 8
%argslist57586$ae517601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51762, %struct.ScmObj* %argslist57586$ae517600)
store volatile %struct.ScmObj* %argslist57586$ae517601, %struct.ScmObj** %stackaddr$prim58341, align 8
%stackaddr$prim58342 = alloca %struct.ScmObj*, align 8
%argslist57586$ae517602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51761, %struct.ScmObj* %argslist57586$ae517601)
store volatile %struct.ScmObj* %argslist57586$ae517602, %struct.ScmObj** %stackaddr$prim58342, align 8
%clofunc58343 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51760)
musttail call tailcc void %clofunc58343(%struct.ScmObj* %ae51760, %struct.ScmObj* %argslist57586$ae517602)
ret void
}

define tailcc void @proc_clo$ae51760(%struct.ScmObj* %env$ae51760,%struct.ScmObj* %current_45args57352) {
%stackaddr$prim58344 = alloca %struct.ScmObj*, align 8
%_95k48322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57352)
store volatile %struct.ScmObj* %_95k48322, %struct.ScmObj** %stackaddr$prim58344, align 8
%stackaddr$prim58345 = alloca %struct.ScmObj*, align 8
%current_45args57353 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57352)
store volatile %struct.ScmObj* %current_45args57353, %struct.ScmObj** %stackaddr$prim58345, align 8
%stackaddr$prim58346 = alloca %struct.ScmObj*, align 8
%_37first48096 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57353)
store volatile %struct.ScmObj* %_37first48096, %struct.ScmObj** %stackaddr$prim58346, align 8
%stackaddr$makeclosure58347 = alloca %struct.ScmObj*, align 8
%fptrToInt58348 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51780 to i64
%ae51780 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58348)
store volatile %struct.ScmObj* %ae51780, %struct.ScmObj** %stackaddr$makeclosure58347, align 8
%ae51781 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58349 = alloca %struct.ScmObj*, align 8
%fptrToInt58350 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51782 to i64
%ae51782 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58350)
store volatile %struct.ScmObj* %ae51782, %struct.ScmObj** %stackaddr$makeclosure58349, align 8
%argslist57581$ae517800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58351 = alloca %struct.ScmObj*, align 8
%argslist57581$ae517801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51782, %struct.ScmObj* %argslist57581$ae517800)
store volatile %struct.ScmObj* %argslist57581$ae517801, %struct.ScmObj** %stackaddr$prim58351, align 8
%stackaddr$prim58352 = alloca %struct.ScmObj*, align 8
%argslist57581$ae517802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51781, %struct.ScmObj* %argslist57581$ae517801)
store volatile %struct.ScmObj* %argslist57581$ae517802, %struct.ScmObj** %stackaddr$prim58352, align 8
%clofunc58353 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51780)
musttail call tailcc void %clofunc58353(%struct.ScmObj* %ae51780, %struct.ScmObj* %argslist57581$ae517802)
ret void
}

define tailcc void @proc_clo$ae51780(%struct.ScmObj* %env$ae51780,%struct.ScmObj* %current_45args57355) {
%stackaddr$prim58354 = alloca %struct.ScmObj*, align 8
%_95k48323 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57355)
store volatile %struct.ScmObj* %_95k48323, %struct.ScmObj** %stackaddr$prim58354, align 8
%stackaddr$prim58355 = alloca %struct.ScmObj*, align 8
%current_45args57356 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57355)
store volatile %struct.ScmObj* %current_45args57356, %struct.ScmObj** %stackaddr$prim58355, align 8
%stackaddr$prim58356 = alloca %struct.ScmObj*, align 8
%_37second48094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57356)
store volatile %struct.ScmObj* %_37second48094, %struct.ScmObj** %stackaddr$prim58356, align 8
%stackaddr$makeclosure58357 = alloca %struct.ScmObj*, align 8
%fptrToInt58358 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51802 to i64
%ae51802 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58358)
store volatile %struct.ScmObj* %ae51802, %struct.ScmObj** %stackaddr$makeclosure58357, align 8
%ae51803 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58359 = alloca %struct.ScmObj*, align 8
%fptrToInt58360 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51804 to i64
%ae51804 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58360)
store volatile %struct.ScmObj* %ae51804, %struct.ScmObj** %stackaddr$makeclosure58359, align 8
%argslist57576$ae518020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58361 = alloca %struct.ScmObj*, align 8
%argslist57576$ae518021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51804, %struct.ScmObj* %argslist57576$ae518020)
store volatile %struct.ScmObj* %argslist57576$ae518021, %struct.ScmObj** %stackaddr$prim58361, align 8
%stackaddr$prim58362 = alloca %struct.ScmObj*, align 8
%argslist57576$ae518022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51803, %struct.ScmObj* %argslist57576$ae518021)
store volatile %struct.ScmObj* %argslist57576$ae518022, %struct.ScmObj** %stackaddr$prim58362, align 8
%clofunc58363 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51802)
musttail call tailcc void %clofunc58363(%struct.ScmObj* %ae51802, %struct.ScmObj* %argslist57576$ae518022)
ret void
}

define tailcc void @proc_clo$ae51802(%struct.ScmObj* %env$ae51802,%struct.ScmObj* %current_45args57358) {
%stackaddr$prim58364 = alloca %struct.ScmObj*, align 8
%_95k48324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57358)
store volatile %struct.ScmObj* %_95k48324, %struct.ScmObj** %stackaddr$prim58364, align 8
%stackaddr$prim58365 = alloca %struct.ScmObj*, align 8
%current_45args57359 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57358)
store volatile %struct.ScmObj* %current_45args57359, %struct.ScmObj** %stackaddr$prim58365, align 8
%stackaddr$prim58366 = alloca %struct.ScmObj*, align 8
%_37third48092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57359)
store volatile %struct.ScmObj* %_37third48092, %struct.ScmObj** %stackaddr$prim58366, align 8
%stackaddr$makeclosure58367 = alloca %struct.ScmObj*, align 8
%fptrToInt58368 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51826 to i64
%ae51826 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58368)
store volatile %struct.ScmObj* %ae51826, %struct.ScmObj** %stackaddr$makeclosure58367, align 8
%ae51827 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58369 = alloca %struct.ScmObj*, align 8
%fptrToInt58370 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51828 to i64
%ae51828 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58370)
store volatile %struct.ScmObj* %ae51828, %struct.ScmObj** %stackaddr$makeclosure58369, align 8
%argslist57571$ae518260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58371 = alloca %struct.ScmObj*, align 8
%argslist57571$ae518261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51828, %struct.ScmObj* %argslist57571$ae518260)
store volatile %struct.ScmObj* %argslist57571$ae518261, %struct.ScmObj** %stackaddr$prim58371, align 8
%stackaddr$prim58372 = alloca %struct.ScmObj*, align 8
%argslist57571$ae518262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51827, %struct.ScmObj* %argslist57571$ae518261)
store volatile %struct.ScmObj* %argslist57571$ae518262, %struct.ScmObj** %stackaddr$prim58372, align 8
%clofunc58373 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51826)
musttail call tailcc void %clofunc58373(%struct.ScmObj* %ae51826, %struct.ScmObj* %argslist57571$ae518262)
ret void
}

define tailcc void @proc_clo$ae51826(%struct.ScmObj* %env$ae51826,%struct.ScmObj* %current_45args57361) {
%stackaddr$prim58374 = alloca %struct.ScmObj*, align 8
%_95k48325 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57361)
store volatile %struct.ScmObj* %_95k48325, %struct.ScmObj** %stackaddr$prim58374, align 8
%stackaddr$prim58375 = alloca %struct.ScmObj*, align 8
%current_45args57362 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57361)
store volatile %struct.ScmObj* %current_45args57362, %struct.ScmObj** %stackaddr$prim58375, align 8
%stackaddr$prim58376 = alloca %struct.ScmObj*, align 8
%_37fourth48090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57362)
store volatile %struct.ScmObj* %_37fourth48090, %struct.ScmObj** %stackaddr$prim58376, align 8
%stackaddr$makeclosure58377 = alloca %struct.ScmObj*, align 8
%fptrToInt58378 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51852 to i64
%ae51852 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58378)
store volatile %struct.ScmObj* %ae51852, %struct.ScmObj** %stackaddr$makeclosure58377, align 8
%ae51853 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58379 = alloca %struct.ScmObj*, align 8
%fptrToInt58380 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51854 to i64
%ae51854 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58380)
store volatile %struct.ScmObj* %ae51854, %struct.ScmObj** %stackaddr$makeclosure58379, align 8
%argslist57566$ae518520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58381 = alloca %struct.ScmObj*, align 8
%argslist57566$ae518521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51854, %struct.ScmObj* %argslist57566$ae518520)
store volatile %struct.ScmObj* %argslist57566$ae518521, %struct.ScmObj** %stackaddr$prim58381, align 8
%stackaddr$prim58382 = alloca %struct.ScmObj*, align 8
%argslist57566$ae518522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51853, %struct.ScmObj* %argslist57566$ae518521)
store volatile %struct.ScmObj* %argslist57566$ae518522, %struct.ScmObj** %stackaddr$prim58382, align 8
%clofunc58383 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51852)
musttail call tailcc void %clofunc58383(%struct.ScmObj* %ae51852, %struct.ScmObj* %argslist57566$ae518522)
ret void
}

define tailcc void @proc_clo$ae51852(%struct.ScmObj* %env$ae51852,%struct.ScmObj* %current_45args57364) {
%stackaddr$prim58384 = alloca %struct.ScmObj*, align 8
%_95k48326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57364)
store volatile %struct.ScmObj* %_95k48326, %struct.ScmObj** %stackaddr$prim58384, align 8
%stackaddr$prim58385 = alloca %struct.ScmObj*, align 8
%current_45args57365 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57364)
store volatile %struct.ScmObj* %current_45args57365, %struct.ScmObj** %stackaddr$prim58385, align 8
%stackaddr$prim58386 = alloca %struct.ScmObj*, align 8
%anf_45bind48274 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57365)
store volatile %struct.ScmObj* %anf_45bind48274, %struct.ScmObj** %stackaddr$prim58386, align 8
%stackaddr$makeclosure58387 = alloca %struct.ScmObj*, align 8
%fptrToInt58388 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51876 to i64
%ae51876 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58388)
store volatile %struct.ScmObj* %ae51876, %struct.ScmObj** %stackaddr$makeclosure58387, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51876, %struct.ScmObj* %anf_45bind48274, i64 0)
%ae51877 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58389 = alloca %struct.ScmObj*, align 8
%fptrToInt58390 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51878 to i64
%ae51878 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58390)
store volatile %struct.ScmObj* %ae51878, %struct.ScmObj** %stackaddr$makeclosure58389, align 8
%argslist57564$ae518760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58391 = alloca %struct.ScmObj*, align 8
%argslist57564$ae518761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51878, %struct.ScmObj* %argslist57564$ae518760)
store volatile %struct.ScmObj* %argslist57564$ae518761, %struct.ScmObj** %stackaddr$prim58391, align 8
%stackaddr$prim58392 = alloca %struct.ScmObj*, align 8
%argslist57564$ae518762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51877, %struct.ScmObj* %argslist57564$ae518761)
store volatile %struct.ScmObj* %argslist57564$ae518762, %struct.ScmObj** %stackaddr$prim58392, align 8
%clofunc58393 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51876)
musttail call tailcc void %clofunc58393(%struct.ScmObj* %ae51876, %struct.ScmObj* %argslist57564$ae518762)
ret void
}

define tailcc void @proc_clo$ae51876(%struct.ScmObj* %env$ae51876,%struct.ScmObj* %current_45args57367) {
%stackaddr$env-ref58394 = alloca %struct.ScmObj*, align 8
%anf_45bind48274 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51876, i64 0)
store %struct.ScmObj* %anf_45bind48274, %struct.ScmObj** %stackaddr$env-ref58394
%stackaddr$prim58395 = alloca %struct.ScmObj*, align 8
%_95k48327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57367)
store volatile %struct.ScmObj* %_95k48327, %struct.ScmObj** %stackaddr$prim58395, align 8
%stackaddr$prim58396 = alloca %struct.ScmObj*, align 8
%current_45args57368 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57367)
store volatile %struct.ScmObj* %current_45args57368, %struct.ScmObj** %stackaddr$prim58396, align 8
%stackaddr$prim58397 = alloca %struct.ScmObj*, align 8
%anf_45bind48275 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57368)
store volatile %struct.ScmObj* %anf_45bind48275, %struct.ScmObj** %stackaddr$prim58397, align 8
%stackaddr$makeclosure58398 = alloca %struct.ScmObj*, align 8
%fptrToInt58399 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51898 to i64
%ae51898 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58399)
store volatile %struct.ScmObj* %ae51898, %struct.ScmObj** %stackaddr$makeclosure58398, align 8
%ae51899 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae5189958400, i32 0, i32 0))
%ae51900 = call %struct.ScmObj* @const_init_false()
%argslist57559$anf_45bind482740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58401 = alloca %struct.ScmObj*, align 8
%argslist57559$anf_45bind482741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48275, %struct.ScmObj* %argslist57559$anf_45bind482740)
store volatile %struct.ScmObj* %argslist57559$anf_45bind482741, %struct.ScmObj** %stackaddr$prim58401, align 8
%stackaddr$prim58402 = alloca %struct.ScmObj*, align 8
%argslist57559$anf_45bind482742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51900, %struct.ScmObj* %argslist57559$anf_45bind482741)
store volatile %struct.ScmObj* %argslist57559$anf_45bind482742, %struct.ScmObj** %stackaddr$prim58402, align 8
%stackaddr$prim58403 = alloca %struct.ScmObj*, align 8
%argslist57559$anf_45bind482743 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51899, %struct.ScmObj* %argslist57559$anf_45bind482742)
store volatile %struct.ScmObj* %argslist57559$anf_45bind482743, %struct.ScmObj** %stackaddr$prim58403, align 8
%stackaddr$prim58404 = alloca %struct.ScmObj*, align 8
%argslist57559$anf_45bind482744 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51898, %struct.ScmObj* %argslist57559$anf_45bind482743)
store volatile %struct.ScmObj* %argslist57559$anf_45bind482744, %struct.ScmObj** %stackaddr$prim58404, align 8
%clofunc58405 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48274)
musttail call tailcc void %clofunc58405(%struct.ScmObj* %anf_45bind48274, %struct.ScmObj* %argslist57559$anf_45bind482744)
ret void
}

define tailcc void @proc_clo$ae51898(%struct.ScmObj* %env$ae51898,%struct.ScmObj* %current_45args57370) {
%stackaddr$prim58406 = alloca %struct.ScmObj*, align 8
%_95k48328 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57370)
store volatile %struct.ScmObj* %_95k48328, %struct.ScmObj** %stackaddr$prim58406, align 8
%stackaddr$prim58407 = alloca %struct.ScmObj*, align 8
%current_45args57371 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57370)
store volatile %struct.ScmObj* %current_45args57371, %struct.ScmObj** %stackaddr$prim58407, align 8
%stackaddr$prim58408 = alloca %struct.ScmObj*, align 8
%thunk4802748152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57371)
store volatile %struct.ScmObj* %thunk4802748152, %struct.ScmObj** %stackaddr$prim58408, align 8
%stackaddr$makeclosure58409 = alloca %struct.ScmObj*, align 8
%fptrToInt58410 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51912 to i64
%ae51912 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58410)
store volatile %struct.ScmObj* %ae51912, %struct.ScmObj** %stackaddr$makeclosure58409, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51912, %struct.ScmObj* %thunk4802748152, i64 0)
%ae51913 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58411 = alloca %struct.ScmObj*, align 8
%fptrToInt58412 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51914 to i64
%ae51914 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58412)
store volatile %struct.ScmObj* %ae51914, %struct.ScmObj** %stackaddr$makeclosure58411, align 8
%argslist57558$ae519120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58413 = alloca %struct.ScmObj*, align 8
%argslist57558$ae519121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51914, %struct.ScmObj* %argslist57558$ae519120)
store volatile %struct.ScmObj* %argslist57558$ae519121, %struct.ScmObj** %stackaddr$prim58413, align 8
%stackaddr$prim58414 = alloca %struct.ScmObj*, align 8
%argslist57558$ae519122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51913, %struct.ScmObj* %argslist57558$ae519121)
store volatile %struct.ScmObj* %argslist57558$ae519122, %struct.ScmObj** %stackaddr$prim58414, align 8
%clofunc58415 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51912)
musttail call tailcc void %clofunc58415(%struct.ScmObj* %ae51912, %struct.ScmObj* %argslist57558$ae519122)
ret void
}

define tailcc void @proc_clo$ae51912(%struct.ScmObj* %env$ae51912,%struct.ScmObj* %current_45args57373) {
%stackaddr$env-ref58416 = alloca %struct.ScmObj*, align 8
%thunk4802748152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51912, i64 0)
store %struct.ScmObj* %thunk4802748152, %struct.ScmObj** %stackaddr$env-ref58416
%stackaddr$prim58417 = alloca %struct.ScmObj*, align 8
%_95k48329 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57373)
store volatile %struct.ScmObj* %_95k48329, %struct.ScmObj** %stackaddr$prim58417, align 8
%stackaddr$prim58418 = alloca %struct.ScmObj*, align 8
%current_45args57374 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57373)
store volatile %struct.ScmObj* %current_45args57374, %struct.ScmObj** %stackaddr$prim58418, align 8
%stackaddr$prim58419 = alloca %struct.ScmObj*, align 8
%anf_45bind48280 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57374)
store volatile %struct.ScmObj* %anf_45bind48280, %struct.ScmObj** %stackaddr$prim58419, align 8
%stackaddr$makeclosure58420 = alloca %struct.ScmObj*, align 8
%fptrToInt58421 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52000 to i64
%ae52000 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58421)
store volatile %struct.ScmObj* %ae52000, %struct.ScmObj** %stackaddr$makeclosure58420, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52000, %struct.ScmObj* %thunk4802748152, i64 0)
%argslist57551$anf_45bind482800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58422 = alloca %struct.ScmObj*, align 8
%argslist57551$anf_45bind482801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4802748152, %struct.ScmObj* %argslist57551$anf_45bind482800)
store volatile %struct.ScmObj* %argslist57551$anf_45bind482801, %struct.ScmObj** %stackaddr$prim58422, align 8
%stackaddr$prim58423 = alloca %struct.ScmObj*, align 8
%argslist57551$anf_45bind482802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52000, %struct.ScmObj* %argslist57551$anf_45bind482801)
store volatile %struct.ScmObj* %argslist57551$anf_45bind482802, %struct.ScmObj** %stackaddr$prim58423, align 8
%clofunc58424 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48280)
musttail call tailcc void %clofunc58424(%struct.ScmObj* %anf_45bind48280, %struct.ScmObj* %argslist57551$anf_45bind482802)
ret void
}

define tailcc void @proc_clo$ae52000(%struct.ScmObj* %env$ae52000,%struct.ScmObj* %current_45args57376) {
%stackaddr$env-ref58425 = alloca %struct.ScmObj*, align 8
%thunk4802748152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52000, i64 0)
store %struct.ScmObj* %thunk4802748152, %struct.ScmObj** %stackaddr$env-ref58425
%stackaddr$prim58426 = alloca %struct.ScmObj*, align 8
%_95k48330 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57376)
store volatile %struct.ScmObj* %_95k48330, %struct.ScmObj** %stackaddr$prim58426, align 8
%stackaddr$prim58427 = alloca %struct.ScmObj*, align 8
%current_45args57377 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57376)
store volatile %struct.ScmObj* %current_45args57377, %struct.ScmObj** %stackaddr$prim58427, align 8
%stackaddr$prim58428 = alloca %struct.ScmObj*, align 8
%anf_45bind48281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57377)
store volatile %struct.ScmObj* %anf_45bind48281, %struct.ScmObj** %stackaddr$prim58428, align 8
%truthy$cmp58429 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48281)
%cmp$cmp58429 = icmp eq i64 %truthy$cmp58429, 1
br i1 %cmp$cmp58429, label %truebranch$cmp58429, label %falsebranch$cmp58429
truebranch$cmp58429:
%ae52004 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58430 = alloca %struct.ScmObj*, align 8
%anf_45bind48282 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4802748152, %struct.ScmObj* %ae52004)
store volatile %struct.ScmObj* %anf_45bind48282, %struct.ScmObj** %stackaddr$prim58430, align 8
%truthy$cmp58431 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48282)
%cmp$cmp58431 = icmp eq i64 %truthy$cmp58431, 1
br i1 %cmp$cmp58431, label %truebranch$cmp58431, label %falsebranch$cmp58431
truebranch$cmp58431:
%ae52007 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim58432 = alloca %struct.ScmObj*, align 8
%cpsprim48339 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4802748152, %struct.ScmObj* %ae52007)
store volatile %struct.ScmObj* %cpsprim48339, %struct.ScmObj** %stackaddr$prim58432, align 8
%stackaddr$makeclosure58433 = alloca %struct.ScmObj*, align 8
%fptrToInt58434 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52008 to i64
%ae52008 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58434)
store volatile %struct.ScmObj* %ae52008, %struct.ScmObj** %stackaddr$makeclosure58433, align 8
%ae52009 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57420$ae520080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58435 = alloca %struct.ScmObj*, align 8
%argslist57420$ae520081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48339, %struct.ScmObj* %argslist57420$ae520080)
store volatile %struct.ScmObj* %argslist57420$ae520081, %struct.ScmObj** %stackaddr$prim58435, align 8
%stackaddr$prim58436 = alloca %struct.ScmObj*, align 8
%argslist57420$ae520082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52009, %struct.ScmObj* %argslist57420$ae520081)
store volatile %struct.ScmObj* %argslist57420$ae520082, %struct.ScmObj** %stackaddr$prim58436, align 8
%clofunc58437 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52008)
musttail call tailcc void %clofunc58437(%struct.ScmObj* %ae52008, %struct.ScmObj* %argslist57420$ae520082)
ret void
falsebranch$cmp58431:
%ae52397 = call %struct.ScmObj* @const_init_int(i64 1)
%ae52398 = call %struct.ScmObj* @const_init_true()
%stackaddr$prim58438 = alloca %struct.ScmObj*, align 8
%t4802948156 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4802748152, %struct.ScmObj* %ae52397, %struct.ScmObj* %ae52398)
store volatile %struct.ScmObj* %t4802948156, %struct.ScmObj** %stackaddr$prim58438, align 8
%ae52400 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim58439 = alloca %struct.ScmObj*, align 8
%anf_45bind48283 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4802748152, %struct.ScmObj* %ae52400)
store volatile %struct.ScmObj* %anf_45bind48283, %struct.ScmObj** %stackaddr$prim58439, align 8
%stackaddr$makeclosure58440 = alloca %struct.ScmObj*, align 8
%fptrToInt58441 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52402 to i64
%ae52402 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58441)
store volatile %struct.ScmObj* %ae52402, %struct.ScmObj** %stackaddr$makeclosure58440, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52402, %struct.ScmObj* %thunk4802748152, i64 0)
%ae52403 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @global$sym$ae5240358442, i32 0, i32 0))
%argslist57466$anf_45bind482830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58443 = alloca %struct.ScmObj*, align 8
%argslist57466$anf_45bind482831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52403, %struct.ScmObj* %argslist57466$anf_45bind482830)
store volatile %struct.ScmObj* %argslist57466$anf_45bind482831, %struct.ScmObj** %stackaddr$prim58443, align 8
%stackaddr$prim58444 = alloca %struct.ScmObj*, align 8
%argslist57466$anf_45bind482832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52402, %struct.ScmObj* %argslist57466$anf_45bind482831)
store volatile %struct.ScmObj* %argslist57466$anf_45bind482832, %struct.ScmObj** %stackaddr$prim58444, align 8
%clofunc58445 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48283)
musttail call tailcc void %clofunc58445(%struct.ScmObj* %anf_45bind48283, %struct.ScmObj* %argslist57466$anf_45bind482832)
ret void
falsebranch$cmp58429:
%stackaddr$prim58446 = alloca %struct.ScmObj*, align 8
%anf_45bind48284 = call %struct.ScmObj* @prim_number_63(%struct.ScmObj* %thunk4802748152)
store volatile %struct.ScmObj* %anf_45bind48284, %struct.ScmObj** %stackaddr$prim58446, align 8
%truthy$cmp58447 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48284)
%cmp$cmp58447 = icmp eq i64 %truthy$cmp58447, 1
br i1 %cmp$cmp58447, label %truebranch$cmp58447, label %falsebranch$cmp58447
truebranch$cmp58447:
%stackaddr$makeclosure58448 = alloca %struct.ScmObj*, align 8
%fptrToInt58449 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53302 to i64
%ae53302 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58449)
store volatile %struct.ScmObj* %ae53302, %struct.ScmObj** %stackaddr$makeclosure58448, align 8
%ae53303 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57508$ae533020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58450 = alloca %struct.ScmObj*, align 8
%argslist57508$ae533021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4802748152, %struct.ScmObj* %argslist57508$ae533020)
store volatile %struct.ScmObj* %argslist57508$ae533021, %struct.ScmObj** %stackaddr$prim58450, align 8
%stackaddr$prim58451 = alloca %struct.ScmObj*, align 8
%argslist57508$ae533022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53303, %struct.ScmObj* %argslist57508$ae533021)
store volatile %struct.ScmObj* %argslist57508$ae533022, %struct.ScmObj** %stackaddr$prim58451, align 8
%clofunc58452 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53302)
musttail call tailcc void %clofunc58452(%struct.ScmObj* %ae53302, %struct.ScmObj* %argslist57508$ae533022)
ret void
falsebranch$cmp58447:
%stackaddr$makeclosure58453 = alloca %struct.ScmObj*, align 8
%fptrToInt58454 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53615 to i64
%ae53615 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58454)
store volatile %struct.ScmObj* %ae53615, %struct.ScmObj** %stackaddr$makeclosure58453, align 8
%ae53616 = call %struct.ScmObj* @const_init_int(i64 0)
%ae53617 = call %struct.ScmObj* @const_init_string(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @global$str$ae5361758455, i32 0, i32 0))
%argslist57550$ae536150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58456 = alloca %struct.ScmObj*, align 8
%argslist57550$ae536151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53617, %struct.ScmObj* %argslist57550$ae536150)
store volatile %struct.ScmObj* %argslist57550$ae536151, %struct.ScmObj** %stackaddr$prim58456, align 8
%stackaddr$prim58457 = alloca %struct.ScmObj*, align 8
%argslist57550$ae536152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53616, %struct.ScmObj* %argslist57550$ae536151)
store volatile %struct.ScmObj* %argslist57550$ae536152, %struct.ScmObj** %stackaddr$prim58457, align 8
%clofunc58458 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53615)
musttail call tailcc void %clofunc58458(%struct.ScmObj* %ae53615, %struct.ScmObj* %argslist57550$ae536152)
ret void
}

define tailcc void @proc_clo$ae52008(%struct.ScmObj* %env$ae52008,%struct.ScmObj* %current_45args57379) {
%stackaddr$prim58459 = alloca %struct.ScmObj*, align 8
%_95k48331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57379)
store volatile %struct.ScmObj* %_95k48331, %struct.ScmObj** %stackaddr$prim58459, align 8
%stackaddr$prim58460 = alloca %struct.ScmObj*, align 8
%current_45args57380 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57379)
store volatile %struct.ScmObj* %current_45args57380, %struct.ScmObj** %stackaddr$prim58460, align 8
%stackaddr$prim58461 = alloca %struct.ScmObj*, align 8
%thunk4802548151 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57380)
store volatile %struct.ScmObj* %thunk4802548151, %struct.ScmObj** %stackaddr$prim58461, align 8
%stackaddr$makeclosure58462 = alloca %struct.ScmObj*, align 8
%fptrToInt58463 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52014 to i64
%ae52014 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58463)
store volatile %struct.ScmObj* %ae52014, %struct.ScmObj** %stackaddr$makeclosure58462, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52014, %struct.ScmObj* %thunk4802548151, i64 0)
%ae52015 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58464 = alloca %struct.ScmObj*, align 8
%fptrToInt58465 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52016 to i64
%ae52016 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58465)
store volatile %struct.ScmObj* %ae52016, %struct.ScmObj** %stackaddr$makeclosure58464, align 8
%argslist57419$ae520140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58466 = alloca %struct.ScmObj*, align 8
%argslist57419$ae520141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52016, %struct.ScmObj* %argslist57419$ae520140)
store volatile %struct.ScmObj* %argslist57419$ae520141, %struct.ScmObj** %stackaddr$prim58466, align 8
%stackaddr$prim58467 = alloca %struct.ScmObj*, align 8
%argslist57419$ae520142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52015, %struct.ScmObj* %argslist57419$ae520141)
store volatile %struct.ScmObj* %argslist57419$ae520142, %struct.ScmObj** %stackaddr$prim58467, align 8
%clofunc58468 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52014)
musttail call tailcc void %clofunc58468(%struct.ScmObj* %ae52014, %struct.ScmObj* %argslist57419$ae520142)
ret void
}

define tailcc void @proc_clo$ae52014(%struct.ScmObj* %env$ae52014,%struct.ScmObj* %current_45args57382) {
%stackaddr$env-ref58469 = alloca %struct.ScmObj*, align 8
%thunk4802548151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52014, i64 0)
store %struct.ScmObj* %thunk4802548151, %struct.ScmObj** %stackaddr$env-ref58469
%stackaddr$prim58470 = alloca %struct.ScmObj*, align 8
%_95k48332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57382)
store volatile %struct.ScmObj* %_95k48332, %struct.ScmObj** %stackaddr$prim58470, align 8
%stackaddr$prim58471 = alloca %struct.ScmObj*, align 8
%current_45args57383 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57382)
store volatile %struct.ScmObj* %current_45args57383, %struct.ScmObj** %stackaddr$prim58471, align 8
%stackaddr$prim58472 = alloca %struct.ScmObj*, align 8
%anf_45bind48289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57383)
store volatile %struct.ScmObj* %anf_45bind48289, %struct.ScmObj** %stackaddr$prim58472, align 8
%stackaddr$makeclosure58473 = alloca %struct.ScmObj*, align 8
%fptrToInt58474 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52102 to i64
%ae52102 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58474)
store volatile %struct.ScmObj* %ae52102, %struct.ScmObj** %stackaddr$makeclosure58473, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52102, %struct.ScmObj* %thunk4802548151, i64 0)
%argslist57412$anf_45bind482890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58475 = alloca %struct.ScmObj*, align 8
%argslist57412$anf_45bind482891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %argslist57412$anf_45bind482890)
store volatile %struct.ScmObj* %argslist57412$anf_45bind482891, %struct.ScmObj** %stackaddr$prim58475, align 8
%stackaddr$prim58476 = alloca %struct.ScmObj*, align 8
%argslist57412$anf_45bind482892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52102, %struct.ScmObj* %argslist57412$anf_45bind482891)
store volatile %struct.ScmObj* %argslist57412$anf_45bind482892, %struct.ScmObj** %stackaddr$prim58476, align 8
%clofunc58477 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48289)
musttail call tailcc void %clofunc58477(%struct.ScmObj* %anf_45bind48289, %struct.ScmObj* %argslist57412$anf_45bind482892)
ret void
}

define tailcc void @proc_clo$ae52102(%struct.ScmObj* %env$ae52102,%struct.ScmObj* %current_45args57385) {
%stackaddr$env-ref58478 = alloca %struct.ScmObj*, align 8
%thunk4802548151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52102, i64 0)
store %struct.ScmObj* %thunk4802548151, %struct.ScmObj** %stackaddr$env-ref58478
%stackaddr$prim58479 = alloca %struct.ScmObj*, align 8
%_95k48333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57385)
store volatile %struct.ScmObj* %_95k48333, %struct.ScmObj** %stackaddr$prim58479, align 8
%stackaddr$prim58480 = alloca %struct.ScmObj*, align 8
%current_45args57386 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57385)
store volatile %struct.ScmObj* %current_45args57386, %struct.ScmObj** %stackaddr$prim58480, align 8
%stackaddr$prim58481 = alloca %struct.ScmObj*, align 8
%anf_45bind48290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57386)
store volatile %struct.ScmObj* %anf_45bind48290, %struct.ScmObj** %stackaddr$prim58481, align 8
%truthy$cmp58482 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48290)
%cmp$cmp58482 = icmp eq i64 %truthy$cmp58482, 1
br i1 %cmp$cmp58482, label %truebranch$cmp58482, label %falsebranch$cmp58482
truebranch$cmp58482:
%ae52106 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58483 = alloca %struct.ScmObj*, align 8
%anf_45bind48291 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %ae52106)
store volatile %struct.ScmObj* %anf_45bind48291, %struct.ScmObj** %stackaddr$prim58483, align 8
%truthy$cmp58484 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48291)
%cmp$cmp58484 = icmp eq i64 %truthy$cmp58484, 1
br i1 %cmp$cmp58484, label %truebranch$cmp58484, label %falsebranch$cmp58484
truebranch$cmp58484:
%ae52109 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim58485 = alloca %struct.ScmObj*, align 8
%cpsprim48334 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %ae52109)
store volatile %struct.ScmObj* %cpsprim48334, %struct.ScmObj** %stackaddr$prim58485, align 8
%stackaddr$makeclosure58486 = alloca %struct.ScmObj*, align 8
%fptrToInt58487 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52110 to i64
%ae52110 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58487)
store volatile %struct.ScmObj* %ae52110, %struct.ScmObj** %stackaddr$makeclosure58486, align 8
%ae52111 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57392$ae521100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58488 = alloca %struct.ScmObj*, align 8
%argslist57392$ae521101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48334, %struct.ScmObj* %argslist57392$ae521100)
store volatile %struct.ScmObj* %argslist57392$ae521101, %struct.ScmObj** %stackaddr$prim58488, align 8
%stackaddr$prim58489 = alloca %struct.ScmObj*, align 8
%argslist57392$ae521102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52111, %struct.ScmObj* %argslist57392$ae521101)
store volatile %struct.ScmObj* %argslist57392$ae521102, %struct.ScmObj** %stackaddr$prim58489, align 8
%clofunc58490 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52110)
musttail call tailcc void %clofunc58490(%struct.ScmObj* %ae52110, %struct.ScmObj* %argslist57392$ae521102)
ret void
falsebranch$cmp58484:
%ae52131 = call %struct.ScmObj* @const_init_int(i64 1)
%ae52132 = call %struct.ScmObj* @const_init_true()
%stackaddr$prim58491 = alloca %struct.ScmObj*, align 8
%t4803148160 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %ae52131, %struct.ScmObj* %ae52132)
store volatile %struct.ScmObj* %t4803148160, %struct.ScmObj** %stackaddr$prim58491, align 8
%ae52134 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim58492 = alloca %struct.ScmObj*, align 8
%anf_45bind48292 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %ae52134)
store volatile %struct.ScmObj* %anf_45bind48292, %struct.ScmObj** %stackaddr$prim58492, align 8
%stackaddr$makeclosure58493 = alloca %struct.ScmObj*, align 8
%fptrToInt58494 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52136 to i64
%ae52136 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58494)
store volatile %struct.ScmObj* %ae52136, %struct.ScmObj** %stackaddr$makeclosure58493, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52136, %struct.ScmObj* %thunk4802548151, i64 0)
%ae52137 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @global$sym$ae5213758495, i32 0, i32 0))
%argslist57401$anf_45bind482920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58496 = alloca %struct.ScmObj*, align 8
%argslist57401$anf_45bind482921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52137, %struct.ScmObj* %argslist57401$anf_45bind482920)
store volatile %struct.ScmObj* %argslist57401$anf_45bind482921, %struct.ScmObj** %stackaddr$prim58496, align 8
%stackaddr$prim58497 = alloca %struct.ScmObj*, align 8
%argslist57401$anf_45bind482922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52136, %struct.ScmObj* %argslist57401$anf_45bind482921)
store volatile %struct.ScmObj* %argslist57401$anf_45bind482922, %struct.ScmObj** %stackaddr$prim58497, align 8
%clofunc58498 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48292)
musttail call tailcc void %clofunc58498(%struct.ScmObj* %anf_45bind48292, %struct.ScmObj* %argslist57401$anf_45bind482922)
ret void
falsebranch$cmp58482:
%stackaddr$prim58499 = alloca %struct.ScmObj*, align 8
%anf_45bind48293 = call %struct.ScmObj* @prim_number_63(%struct.ScmObj* %thunk4802548151)
store volatile %struct.ScmObj* %anf_45bind48293, %struct.ScmObj** %stackaddr$prim58499, align 8
%truthy$cmp58500 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48293)
%cmp$cmp58500 = icmp eq i64 %truthy$cmp58500, 1
br i1 %cmp$cmp58500, label %truebranch$cmp58500, label %falsebranch$cmp58500
truebranch$cmp58500:
%stackaddr$makeclosure58501 = alloca %struct.ScmObj*, align 8
%fptrToInt58502 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52266 to i64
%ae52266 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58502)
store volatile %struct.ScmObj* %ae52266, %struct.ScmObj** %stackaddr$makeclosure58501, align 8
%ae52267 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57406$ae522660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58503 = alloca %struct.ScmObj*, align 8
%argslist57406$ae522661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %argslist57406$ae522660)
store volatile %struct.ScmObj* %argslist57406$ae522661, %struct.ScmObj** %stackaddr$prim58503, align 8
%stackaddr$prim58504 = alloca %struct.ScmObj*, align 8
%argslist57406$ae522662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52267, %struct.ScmObj* %argslist57406$ae522661)
store volatile %struct.ScmObj* %argslist57406$ae522662, %struct.ScmObj** %stackaddr$prim58504, align 8
%clofunc58505 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52266)
musttail call tailcc void %clofunc58505(%struct.ScmObj* %ae52266, %struct.ScmObj* %argslist57406$ae522662)
ret void
falsebranch$cmp58500:
%stackaddr$makeclosure58506 = alloca %struct.ScmObj*, align 8
%fptrToInt58507 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52278 to i64
%ae52278 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58507)
store volatile %struct.ScmObj* %ae52278, %struct.ScmObj** %stackaddr$makeclosure58506, align 8
%ae52279 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52280 = call %struct.ScmObj* @const_init_string(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @global$str$ae5228058508, i32 0, i32 0))
%argslist57411$ae522780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58509 = alloca %struct.ScmObj*, align 8
%argslist57411$ae522781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52280, %struct.ScmObj* %argslist57411$ae522780)
store volatile %struct.ScmObj* %argslist57411$ae522781, %struct.ScmObj** %stackaddr$prim58509, align 8
%stackaddr$prim58510 = alloca %struct.ScmObj*, align 8
%argslist57411$ae522782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52279, %struct.ScmObj* %argslist57411$ae522781)
store volatile %struct.ScmObj* %argslist57411$ae522782, %struct.ScmObj** %stackaddr$prim58510, align 8
%clofunc58511 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52278)
musttail call tailcc void %clofunc58511(%struct.ScmObj* %ae52278, %struct.ScmObj* %argslist57411$ae522782)
ret void
}

define tailcc void @proc_clo$ae52110(%struct.ScmObj* %env$ae52110,%struct.ScmObj* %current_45args57388) {
%stackaddr$prim58512 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57388)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim58512, align 8
%stackaddr$prim58513 = alloca %struct.ScmObj*, align 8
%current_45args57389 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57388)
store volatile %struct.ScmObj* %current_45args57389, %struct.ScmObj** %stackaddr$prim58513, align 8
%stackaddr$prim58514 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57389)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim58514, align 8
%stackaddr$prim58515 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim58515, align 8
%argslist57391$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58516 = alloca %struct.ScmObj*, align 8
%argslist57391$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist57391$k0)
store volatile %struct.ScmObj* %argslist57391$k1, %struct.ScmObj** %stackaddr$prim58516, align 8
%clofunc58517 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc58517(%struct.ScmObj* %k, %struct.ScmObj* %argslist57391$k1)
ret void
}

define tailcc void @proc_clo$ae52136(%struct.ScmObj* %env$ae52136,%struct.ScmObj* %current_45args57393) {
%stackaddr$env-ref58518 = alloca %struct.ScmObj*, align 8
%thunk4802548151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52136, i64 0)
store %struct.ScmObj* %thunk4802548151, %struct.ScmObj** %stackaddr$env-ref58518
%stackaddr$prim58519 = alloca %struct.ScmObj*, align 8
%_95k48335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57393)
store volatile %struct.ScmObj* %_95k48335, %struct.ScmObj** %stackaddr$prim58519, align 8
%stackaddr$prim58520 = alloca %struct.ScmObj*, align 8
%current_45args57394 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57393)
store volatile %struct.ScmObj* %current_45args57394, %struct.ScmObj** %stackaddr$prim58520, align 8
%stackaddr$prim58521 = alloca %struct.ScmObj*, align 8
%val4802648162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57394)
store volatile %struct.ScmObj* %val4802648162, %struct.ScmObj** %stackaddr$prim58521, align 8
%ae52142 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim58522 = alloca %struct.ScmObj*, align 8
%t4803248161 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %ae52142, %struct.ScmObj* %val4802648162)
store volatile %struct.ScmObj* %t4803248161, %struct.ScmObj** %stackaddr$prim58522, align 8
%ae52145 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim58523 = alloca %struct.ScmObj*, align 8
%cpsprim48336 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %ae52145)
store volatile %struct.ScmObj* %cpsprim48336, %struct.ScmObj** %stackaddr$prim58523, align 8
%stackaddr$makeclosure58524 = alloca %struct.ScmObj*, align 8
%fptrToInt58525 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52146 to i64
%ae52146 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58525)
store volatile %struct.ScmObj* %ae52146, %struct.ScmObj** %stackaddr$makeclosure58524, align 8
%ae52147 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57400$ae521460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58526 = alloca %struct.ScmObj*, align 8
%argslist57400$ae521461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48336, %struct.ScmObj* %argslist57400$ae521460)
store volatile %struct.ScmObj* %argslist57400$ae521461, %struct.ScmObj** %stackaddr$prim58526, align 8
%stackaddr$prim58527 = alloca %struct.ScmObj*, align 8
%argslist57400$ae521462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52147, %struct.ScmObj* %argslist57400$ae521461)
store volatile %struct.ScmObj* %argslist57400$ae521462, %struct.ScmObj** %stackaddr$prim58527, align 8
%clofunc58528 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52146)
musttail call tailcc void %clofunc58528(%struct.ScmObj* %ae52146, %struct.ScmObj* %argslist57400$ae521462)
ret void
}

define tailcc void @proc_clo$ae52146(%struct.ScmObj* %env$ae52146,%struct.ScmObj* %current_45args57396) {
%stackaddr$prim58529 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57396)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim58529, align 8
%stackaddr$prim58530 = alloca %struct.ScmObj*, align 8
%current_45args57397 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57396)
store volatile %struct.ScmObj* %current_45args57397, %struct.ScmObj** %stackaddr$prim58530, align 8
%stackaddr$prim58531 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57397)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim58531, align 8
%stackaddr$prim58532 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim58532, align 8
%argslist57399$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58533 = alloca %struct.ScmObj*, align 8
%argslist57399$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist57399$k0)
store volatile %struct.ScmObj* %argslist57399$k1, %struct.ScmObj** %stackaddr$prim58533, align 8
%clofunc58534 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc58534(%struct.ScmObj* %k, %struct.ScmObj* %argslist57399$k1)
ret void
}

define tailcc void @proc_clo$ae52266(%struct.ScmObj* %env$ae52266,%struct.ScmObj* %current_45args57402) {
%stackaddr$prim58535 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57402)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim58535, align 8
%stackaddr$prim58536 = alloca %struct.ScmObj*, align 8
%current_45args57403 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57402)
store volatile %struct.ScmObj* %current_45args57403, %struct.ScmObj** %stackaddr$prim58536, align 8
%stackaddr$prim58537 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57403)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim58537, align 8
%stackaddr$prim58538 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim58538, align 8
%argslist57405$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58539 = alloca %struct.ScmObj*, align 8
%argslist57405$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist57405$k0)
store volatile %struct.ScmObj* %argslist57405$k1, %struct.ScmObj** %stackaddr$prim58539, align 8
%clofunc58540 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc58540(%struct.ScmObj* %k, %struct.ScmObj* %argslist57405$k1)
ret void
}

define tailcc void @proc_clo$ae52278(%struct.ScmObj* %env$ae52278,%struct.ScmObj* %current_45args57407) {
%stackaddr$prim58541 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57407)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim58541, align 8
%stackaddr$prim58542 = alloca %struct.ScmObj*, align 8
%current_45args57408 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57407)
store volatile %struct.ScmObj* %current_45args57408, %struct.ScmObj** %stackaddr$prim58542, align 8
%stackaddr$prim58543 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57408)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim58543, align 8
%stackaddr$prim58544 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim58544, align 8
%argslist57410$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58545 = alloca %struct.ScmObj*, align 8
%argslist57410$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist57410$k0)
store volatile %struct.ScmObj* %argslist57410$k1, %struct.ScmObj** %stackaddr$prim58545, align 8
%clofunc58546 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc58546(%struct.ScmObj* %k, %struct.ScmObj* %argslist57410$k1)
ret void
}

define tailcc void @proc_clo$ae52016(%struct.ScmObj* %env$ae52016,%struct.ScmObj* %current_45args57413) {
%stackaddr$prim58547 = alloca %struct.ScmObj*, align 8
%k48337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57413)
store volatile %struct.ScmObj* %k48337, %struct.ScmObj** %stackaddr$prim58547, align 8
%stackaddr$prim58548 = alloca %struct.ScmObj*, align 8
%current_45args57414 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57413)
store volatile %struct.ScmObj* %current_45args57414, %struct.ScmObj** %stackaddr$prim58548, align 8
%stackaddr$prim58549 = alloca %struct.ScmObj*, align 8
%thunk48159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57414)
store volatile %struct.ScmObj* %thunk48159, %struct.ScmObj** %stackaddr$prim58549, align 8
%stackaddr$prim58550 = alloca %struct.ScmObj*, align 8
%anf_45bind48285 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk48159)
store volatile %struct.ScmObj* %anf_45bind48285, %struct.ScmObj** %stackaddr$prim58550, align 8
%truthy$cmp58551 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48285)
%cmp$cmp58551 = icmp eq i64 %truthy$cmp58551, 1
br i1 %cmp$cmp58551, label %truebranch$cmp58551, label %falsebranch$cmp58551
truebranch$cmp58551:
%stackaddr$prim58552 = alloca %struct.ScmObj*, align 8
%anf_45bind48286 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk48159)
store volatile %struct.ScmObj* %anf_45bind48286, %struct.ScmObj** %stackaddr$prim58552, align 8
%ae52021 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim58553 = alloca %struct.ScmObj*, align 8
%anf_45bind48287 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind48286, %struct.ScmObj* %ae52021)
store volatile %struct.ScmObj* %anf_45bind48287, %struct.ScmObj** %stackaddr$prim58553, align 8
%truthy$cmp58554 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48287)
%cmp$cmp58554 = icmp eq i64 %truthy$cmp58554, 1
br i1 %cmp$cmp58554, label %truebranch$cmp58554, label %falsebranch$cmp58554
truebranch$cmp58554:
%ae52024 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58555 = alloca %struct.ScmObj*, align 8
%anf_45bind48288 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk48159, %struct.ScmObj* %ae52024)
store volatile %struct.ScmObj* %anf_45bind48288, %struct.ScmObj** %stackaddr$prim58555, align 8
%ae52026 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae5202658556, i32 0, i32 0))
%stackaddr$prim58557 = alloca %struct.ScmObj*, align 8
%cpsprim48338 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind48288, %struct.ScmObj* %ae52026)
store volatile %struct.ScmObj* %cpsprim48338, %struct.ScmObj** %stackaddr$prim58557, align 8
%ae52028 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57416$k483370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58558 = alloca %struct.ScmObj*, align 8
%argslist57416$k483371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48338, %struct.ScmObj* %argslist57416$k483370)
store volatile %struct.ScmObj* %argslist57416$k483371, %struct.ScmObj** %stackaddr$prim58558, align 8
%stackaddr$prim58559 = alloca %struct.ScmObj*, align 8
%argslist57416$k483372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52028, %struct.ScmObj* %argslist57416$k483371)
store volatile %struct.ScmObj* %argslist57416$k483372, %struct.ScmObj** %stackaddr$prim58559, align 8
%clofunc58560 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48337)
musttail call tailcc void %clofunc58560(%struct.ScmObj* %k48337, %struct.ScmObj* %argslist57416$k483372)
ret void
falsebranch$cmp58554:
%ae52046 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52047 = call %struct.ScmObj* @const_init_false()
%argslist57417$k483370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58561 = alloca %struct.ScmObj*, align 8
%argslist57417$k483371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52047, %struct.ScmObj* %argslist57417$k483370)
store volatile %struct.ScmObj* %argslist57417$k483371, %struct.ScmObj** %stackaddr$prim58561, align 8
%stackaddr$prim58562 = alloca %struct.ScmObj*, align 8
%argslist57417$k483372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52046, %struct.ScmObj* %argslist57417$k483371)
store volatile %struct.ScmObj* %argslist57417$k483372, %struct.ScmObj** %stackaddr$prim58562, align 8
%clofunc58563 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48337)
musttail call tailcc void %clofunc58563(%struct.ScmObj* %k48337, %struct.ScmObj* %argslist57417$k483372)
ret void
falsebranch$cmp58551:
%ae52068 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52069 = call %struct.ScmObj* @const_init_false()
%argslist57418$k483370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58564 = alloca %struct.ScmObj*, align 8
%argslist57418$k483371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52069, %struct.ScmObj* %argslist57418$k483370)
store volatile %struct.ScmObj* %argslist57418$k483371, %struct.ScmObj** %stackaddr$prim58564, align 8
%stackaddr$prim58565 = alloca %struct.ScmObj*, align 8
%argslist57418$k483372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52068, %struct.ScmObj* %argslist57418$k483371)
store volatile %struct.ScmObj* %argslist57418$k483372, %struct.ScmObj** %stackaddr$prim58565, align 8
%clofunc58566 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48337)
musttail call tailcc void %clofunc58566(%struct.ScmObj* %k48337, %struct.ScmObj* %argslist57418$k483372)
ret void
}

define tailcc void @proc_clo$ae52402(%struct.ScmObj* %env$ae52402,%struct.ScmObj* %current_45args57421) {
%stackaddr$env-ref58567 = alloca %struct.ScmObj*, align 8
%thunk4802748152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52402, i64 0)
store %struct.ScmObj* %thunk4802748152, %struct.ScmObj** %stackaddr$env-ref58567
%stackaddr$prim58568 = alloca %struct.ScmObj*, align 8
%_95k48340 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57421)
store volatile %struct.ScmObj* %_95k48340, %struct.ScmObj** %stackaddr$prim58568, align 8
%stackaddr$prim58569 = alloca %struct.ScmObj*, align 8
%current_45args57422 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57421)
store volatile %struct.ScmObj* %current_45args57422, %struct.ScmObj** %stackaddr$prim58569, align 8
%stackaddr$prim58570 = alloca %struct.ScmObj*, align 8
%val4802848158 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57422)
store volatile %struct.ScmObj* %val4802848158, %struct.ScmObj** %stackaddr$prim58570, align 8
%ae52408 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim58571 = alloca %struct.ScmObj*, align 8
%t4803048157 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4802748152, %struct.ScmObj* %ae52408, %struct.ScmObj* %val4802848158)
store volatile %struct.ScmObj* %t4803048157, %struct.ScmObj** %stackaddr$prim58571, align 8
%ae52411 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim58572 = alloca %struct.ScmObj*, align 8
%cpsprim48341 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4802748152, %struct.ScmObj* %ae52411)
store volatile %struct.ScmObj* %cpsprim48341, %struct.ScmObj** %stackaddr$prim58572, align 8
%stackaddr$makeclosure58573 = alloca %struct.ScmObj*, align 8
%fptrToInt58574 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52412 to i64
%ae52412 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58574)
store volatile %struct.ScmObj* %ae52412, %struct.ScmObj** %stackaddr$makeclosure58573, align 8
%ae52413 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57465$ae524120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58575 = alloca %struct.ScmObj*, align 8
%argslist57465$ae524121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48341, %struct.ScmObj* %argslist57465$ae524120)
store volatile %struct.ScmObj* %argslist57465$ae524121, %struct.ScmObj** %stackaddr$prim58575, align 8
%stackaddr$prim58576 = alloca %struct.ScmObj*, align 8
%argslist57465$ae524122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52413, %struct.ScmObj* %argslist57465$ae524121)
store volatile %struct.ScmObj* %argslist57465$ae524122, %struct.ScmObj** %stackaddr$prim58576, align 8
%clofunc58577 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52412)
musttail call tailcc void %clofunc58577(%struct.ScmObj* %ae52412, %struct.ScmObj* %argslist57465$ae524122)
ret void
}

define tailcc void @proc_clo$ae52412(%struct.ScmObj* %env$ae52412,%struct.ScmObj* %current_45args57424) {
%stackaddr$prim58578 = alloca %struct.ScmObj*, align 8
%_95k48331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57424)
store volatile %struct.ScmObj* %_95k48331, %struct.ScmObj** %stackaddr$prim58578, align 8
%stackaddr$prim58579 = alloca %struct.ScmObj*, align 8
%current_45args57425 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57424)
store volatile %struct.ScmObj* %current_45args57425, %struct.ScmObj** %stackaddr$prim58579, align 8
%stackaddr$prim58580 = alloca %struct.ScmObj*, align 8
%thunk4802548151 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57425)
store volatile %struct.ScmObj* %thunk4802548151, %struct.ScmObj** %stackaddr$prim58580, align 8
%stackaddr$makeclosure58581 = alloca %struct.ScmObj*, align 8
%fptrToInt58582 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52418 to i64
%ae52418 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58582)
store volatile %struct.ScmObj* %ae52418, %struct.ScmObj** %stackaddr$makeclosure58581, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52418, %struct.ScmObj* %thunk4802548151, i64 0)
%ae52419 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58583 = alloca %struct.ScmObj*, align 8
%fptrToInt58584 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52420 to i64
%ae52420 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58584)
store volatile %struct.ScmObj* %ae52420, %struct.ScmObj** %stackaddr$makeclosure58583, align 8
%argslist57464$ae524180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58585 = alloca %struct.ScmObj*, align 8
%argslist57464$ae524181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52420, %struct.ScmObj* %argslist57464$ae524180)
store volatile %struct.ScmObj* %argslist57464$ae524181, %struct.ScmObj** %stackaddr$prim58585, align 8
%stackaddr$prim58586 = alloca %struct.ScmObj*, align 8
%argslist57464$ae524182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52419, %struct.ScmObj* %argslist57464$ae524181)
store volatile %struct.ScmObj* %argslist57464$ae524182, %struct.ScmObj** %stackaddr$prim58586, align 8
%clofunc58587 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52418)
musttail call tailcc void %clofunc58587(%struct.ScmObj* %ae52418, %struct.ScmObj* %argslist57464$ae524182)
ret void
}

define tailcc void @proc_clo$ae52418(%struct.ScmObj* %env$ae52418,%struct.ScmObj* %current_45args57427) {
%stackaddr$env-ref58588 = alloca %struct.ScmObj*, align 8
%thunk4802548151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52418, i64 0)
store %struct.ScmObj* %thunk4802548151, %struct.ScmObj** %stackaddr$env-ref58588
%stackaddr$prim58589 = alloca %struct.ScmObj*, align 8
%_95k48332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57427)
store volatile %struct.ScmObj* %_95k48332, %struct.ScmObj** %stackaddr$prim58589, align 8
%stackaddr$prim58590 = alloca %struct.ScmObj*, align 8
%current_45args57428 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57427)
store volatile %struct.ScmObj* %current_45args57428, %struct.ScmObj** %stackaddr$prim58590, align 8
%stackaddr$prim58591 = alloca %struct.ScmObj*, align 8
%anf_45bind48289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57428)
store volatile %struct.ScmObj* %anf_45bind48289, %struct.ScmObj** %stackaddr$prim58591, align 8
%stackaddr$makeclosure58592 = alloca %struct.ScmObj*, align 8
%fptrToInt58593 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52506 to i64
%ae52506 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58593)
store volatile %struct.ScmObj* %ae52506, %struct.ScmObj** %stackaddr$makeclosure58592, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52506, %struct.ScmObj* %thunk4802548151, i64 0)
%argslist57457$anf_45bind482890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58594 = alloca %struct.ScmObj*, align 8
%argslist57457$anf_45bind482891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %argslist57457$anf_45bind482890)
store volatile %struct.ScmObj* %argslist57457$anf_45bind482891, %struct.ScmObj** %stackaddr$prim58594, align 8
%stackaddr$prim58595 = alloca %struct.ScmObj*, align 8
%argslist57457$anf_45bind482892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52506, %struct.ScmObj* %argslist57457$anf_45bind482891)
store volatile %struct.ScmObj* %argslist57457$anf_45bind482892, %struct.ScmObj** %stackaddr$prim58595, align 8
%clofunc58596 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48289)
musttail call tailcc void %clofunc58596(%struct.ScmObj* %anf_45bind48289, %struct.ScmObj* %argslist57457$anf_45bind482892)
ret void
}

define tailcc void @proc_clo$ae52506(%struct.ScmObj* %env$ae52506,%struct.ScmObj* %current_45args57430) {
%stackaddr$env-ref58597 = alloca %struct.ScmObj*, align 8
%thunk4802548151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52506, i64 0)
store %struct.ScmObj* %thunk4802548151, %struct.ScmObj** %stackaddr$env-ref58597
%stackaddr$prim58598 = alloca %struct.ScmObj*, align 8
%_95k48333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57430)
store volatile %struct.ScmObj* %_95k48333, %struct.ScmObj** %stackaddr$prim58598, align 8
%stackaddr$prim58599 = alloca %struct.ScmObj*, align 8
%current_45args57431 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57430)
store volatile %struct.ScmObj* %current_45args57431, %struct.ScmObj** %stackaddr$prim58599, align 8
%stackaddr$prim58600 = alloca %struct.ScmObj*, align 8
%anf_45bind48290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57431)
store volatile %struct.ScmObj* %anf_45bind48290, %struct.ScmObj** %stackaddr$prim58600, align 8
%truthy$cmp58601 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48290)
%cmp$cmp58601 = icmp eq i64 %truthy$cmp58601, 1
br i1 %cmp$cmp58601, label %truebranch$cmp58601, label %falsebranch$cmp58601
truebranch$cmp58601:
%ae52510 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58602 = alloca %struct.ScmObj*, align 8
%anf_45bind48291 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %ae52510)
store volatile %struct.ScmObj* %anf_45bind48291, %struct.ScmObj** %stackaddr$prim58602, align 8
%truthy$cmp58603 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48291)
%cmp$cmp58603 = icmp eq i64 %truthy$cmp58603, 1
br i1 %cmp$cmp58603, label %truebranch$cmp58603, label %falsebranch$cmp58603
truebranch$cmp58603:
%ae52513 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim58604 = alloca %struct.ScmObj*, align 8
%cpsprim48334 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %ae52513)
store volatile %struct.ScmObj* %cpsprim48334, %struct.ScmObj** %stackaddr$prim58604, align 8
%stackaddr$makeclosure58605 = alloca %struct.ScmObj*, align 8
%fptrToInt58606 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52514 to i64
%ae52514 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58606)
store volatile %struct.ScmObj* %ae52514, %struct.ScmObj** %stackaddr$makeclosure58605, align 8
%ae52515 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57437$ae525140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58607 = alloca %struct.ScmObj*, align 8
%argslist57437$ae525141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48334, %struct.ScmObj* %argslist57437$ae525140)
store volatile %struct.ScmObj* %argslist57437$ae525141, %struct.ScmObj** %stackaddr$prim58607, align 8
%stackaddr$prim58608 = alloca %struct.ScmObj*, align 8
%argslist57437$ae525142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52515, %struct.ScmObj* %argslist57437$ae525141)
store volatile %struct.ScmObj* %argslist57437$ae525142, %struct.ScmObj** %stackaddr$prim58608, align 8
%clofunc58609 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52514)
musttail call tailcc void %clofunc58609(%struct.ScmObj* %ae52514, %struct.ScmObj* %argslist57437$ae525142)
ret void
falsebranch$cmp58603:
%ae52535 = call %struct.ScmObj* @const_init_int(i64 1)
%ae52536 = call %struct.ScmObj* @const_init_true()
%stackaddr$prim58610 = alloca %struct.ScmObj*, align 8
%t4803148160 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %ae52535, %struct.ScmObj* %ae52536)
store volatile %struct.ScmObj* %t4803148160, %struct.ScmObj** %stackaddr$prim58610, align 8
%ae52538 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim58611 = alloca %struct.ScmObj*, align 8
%anf_45bind48292 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %ae52538)
store volatile %struct.ScmObj* %anf_45bind48292, %struct.ScmObj** %stackaddr$prim58611, align 8
%stackaddr$makeclosure58612 = alloca %struct.ScmObj*, align 8
%fptrToInt58613 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52540 to i64
%ae52540 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58613)
store volatile %struct.ScmObj* %ae52540, %struct.ScmObj** %stackaddr$makeclosure58612, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52540, %struct.ScmObj* %thunk4802548151, i64 0)
%ae52541 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @global$sym$ae5254158614, i32 0, i32 0))
%argslist57446$anf_45bind482920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58615 = alloca %struct.ScmObj*, align 8
%argslist57446$anf_45bind482921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52541, %struct.ScmObj* %argslist57446$anf_45bind482920)
store volatile %struct.ScmObj* %argslist57446$anf_45bind482921, %struct.ScmObj** %stackaddr$prim58615, align 8
%stackaddr$prim58616 = alloca %struct.ScmObj*, align 8
%argslist57446$anf_45bind482922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52540, %struct.ScmObj* %argslist57446$anf_45bind482921)
store volatile %struct.ScmObj* %argslist57446$anf_45bind482922, %struct.ScmObj** %stackaddr$prim58616, align 8
%clofunc58617 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48292)
musttail call tailcc void %clofunc58617(%struct.ScmObj* %anf_45bind48292, %struct.ScmObj* %argslist57446$anf_45bind482922)
ret void
falsebranch$cmp58601:
%stackaddr$prim58618 = alloca %struct.ScmObj*, align 8
%anf_45bind48293 = call %struct.ScmObj* @prim_number_63(%struct.ScmObj* %thunk4802548151)
store volatile %struct.ScmObj* %anf_45bind48293, %struct.ScmObj** %stackaddr$prim58618, align 8
%truthy$cmp58619 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48293)
%cmp$cmp58619 = icmp eq i64 %truthy$cmp58619, 1
br i1 %cmp$cmp58619, label %truebranch$cmp58619, label %falsebranch$cmp58619
truebranch$cmp58619:
%stackaddr$makeclosure58620 = alloca %struct.ScmObj*, align 8
%fptrToInt58621 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52670 to i64
%ae52670 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58621)
store volatile %struct.ScmObj* %ae52670, %struct.ScmObj** %stackaddr$makeclosure58620, align 8
%ae52671 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57451$ae526700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58622 = alloca %struct.ScmObj*, align 8
%argslist57451$ae526701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %argslist57451$ae526700)
store volatile %struct.ScmObj* %argslist57451$ae526701, %struct.ScmObj** %stackaddr$prim58622, align 8
%stackaddr$prim58623 = alloca %struct.ScmObj*, align 8
%argslist57451$ae526702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52671, %struct.ScmObj* %argslist57451$ae526701)
store volatile %struct.ScmObj* %argslist57451$ae526702, %struct.ScmObj** %stackaddr$prim58623, align 8
%clofunc58624 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52670)
musttail call tailcc void %clofunc58624(%struct.ScmObj* %ae52670, %struct.ScmObj* %argslist57451$ae526702)
ret void
falsebranch$cmp58619:
%stackaddr$makeclosure58625 = alloca %struct.ScmObj*, align 8
%fptrToInt58626 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52682 to i64
%ae52682 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58626)
store volatile %struct.ScmObj* %ae52682, %struct.ScmObj** %stackaddr$makeclosure58625, align 8
%ae52683 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52684 = call %struct.ScmObj* @const_init_string(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @global$str$ae5268458627, i32 0, i32 0))
%argslist57456$ae526820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58628 = alloca %struct.ScmObj*, align 8
%argslist57456$ae526821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52684, %struct.ScmObj* %argslist57456$ae526820)
store volatile %struct.ScmObj* %argslist57456$ae526821, %struct.ScmObj** %stackaddr$prim58628, align 8
%stackaddr$prim58629 = alloca %struct.ScmObj*, align 8
%argslist57456$ae526822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52683, %struct.ScmObj* %argslist57456$ae526821)
store volatile %struct.ScmObj* %argslist57456$ae526822, %struct.ScmObj** %stackaddr$prim58629, align 8
%clofunc58630 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52682)
musttail call tailcc void %clofunc58630(%struct.ScmObj* %ae52682, %struct.ScmObj* %argslist57456$ae526822)
ret void
}

define tailcc void @proc_clo$ae52514(%struct.ScmObj* %env$ae52514,%struct.ScmObj* %current_45args57433) {
%stackaddr$prim58631 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57433)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim58631, align 8
%stackaddr$prim58632 = alloca %struct.ScmObj*, align 8
%current_45args57434 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57433)
store volatile %struct.ScmObj* %current_45args57434, %struct.ScmObj** %stackaddr$prim58632, align 8
%stackaddr$prim58633 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57434)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim58633, align 8
%stackaddr$prim58634 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim58634, align 8
%argslist57436$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58635 = alloca %struct.ScmObj*, align 8
%argslist57436$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist57436$k0)
store volatile %struct.ScmObj* %argslist57436$k1, %struct.ScmObj** %stackaddr$prim58635, align 8
%clofunc58636 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc58636(%struct.ScmObj* %k, %struct.ScmObj* %argslist57436$k1)
ret void
}

define tailcc void @proc_clo$ae52540(%struct.ScmObj* %env$ae52540,%struct.ScmObj* %current_45args57438) {
%stackaddr$env-ref58637 = alloca %struct.ScmObj*, align 8
%thunk4802548151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52540, i64 0)
store %struct.ScmObj* %thunk4802548151, %struct.ScmObj** %stackaddr$env-ref58637
%stackaddr$prim58638 = alloca %struct.ScmObj*, align 8
%_95k48335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57438)
store volatile %struct.ScmObj* %_95k48335, %struct.ScmObj** %stackaddr$prim58638, align 8
%stackaddr$prim58639 = alloca %struct.ScmObj*, align 8
%current_45args57439 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57438)
store volatile %struct.ScmObj* %current_45args57439, %struct.ScmObj** %stackaddr$prim58639, align 8
%stackaddr$prim58640 = alloca %struct.ScmObj*, align 8
%val4802648162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57439)
store volatile %struct.ScmObj* %val4802648162, %struct.ScmObj** %stackaddr$prim58640, align 8
%ae52546 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim58641 = alloca %struct.ScmObj*, align 8
%t4803248161 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %ae52546, %struct.ScmObj* %val4802648162)
store volatile %struct.ScmObj* %t4803248161, %struct.ScmObj** %stackaddr$prim58641, align 8
%ae52549 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim58642 = alloca %struct.ScmObj*, align 8
%cpsprim48336 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %ae52549)
store volatile %struct.ScmObj* %cpsprim48336, %struct.ScmObj** %stackaddr$prim58642, align 8
%stackaddr$makeclosure58643 = alloca %struct.ScmObj*, align 8
%fptrToInt58644 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52550 to i64
%ae52550 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58644)
store volatile %struct.ScmObj* %ae52550, %struct.ScmObj** %stackaddr$makeclosure58643, align 8
%ae52551 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57445$ae525500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58645 = alloca %struct.ScmObj*, align 8
%argslist57445$ae525501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48336, %struct.ScmObj* %argslist57445$ae525500)
store volatile %struct.ScmObj* %argslist57445$ae525501, %struct.ScmObj** %stackaddr$prim58645, align 8
%stackaddr$prim58646 = alloca %struct.ScmObj*, align 8
%argslist57445$ae525502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52551, %struct.ScmObj* %argslist57445$ae525501)
store volatile %struct.ScmObj* %argslist57445$ae525502, %struct.ScmObj** %stackaddr$prim58646, align 8
%clofunc58647 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52550)
musttail call tailcc void %clofunc58647(%struct.ScmObj* %ae52550, %struct.ScmObj* %argslist57445$ae525502)
ret void
}

define tailcc void @proc_clo$ae52550(%struct.ScmObj* %env$ae52550,%struct.ScmObj* %current_45args57441) {
%stackaddr$prim58648 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57441)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim58648, align 8
%stackaddr$prim58649 = alloca %struct.ScmObj*, align 8
%current_45args57442 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57441)
store volatile %struct.ScmObj* %current_45args57442, %struct.ScmObj** %stackaddr$prim58649, align 8
%stackaddr$prim58650 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57442)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim58650, align 8
%stackaddr$prim58651 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim58651, align 8
%argslist57444$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58652 = alloca %struct.ScmObj*, align 8
%argslist57444$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist57444$k0)
store volatile %struct.ScmObj* %argslist57444$k1, %struct.ScmObj** %stackaddr$prim58652, align 8
%clofunc58653 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc58653(%struct.ScmObj* %k, %struct.ScmObj* %argslist57444$k1)
ret void
}

define tailcc void @proc_clo$ae52670(%struct.ScmObj* %env$ae52670,%struct.ScmObj* %current_45args57447) {
%stackaddr$prim58654 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57447)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim58654, align 8
%stackaddr$prim58655 = alloca %struct.ScmObj*, align 8
%current_45args57448 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57447)
store volatile %struct.ScmObj* %current_45args57448, %struct.ScmObj** %stackaddr$prim58655, align 8
%stackaddr$prim58656 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57448)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim58656, align 8
%stackaddr$prim58657 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim58657, align 8
%argslist57450$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58658 = alloca %struct.ScmObj*, align 8
%argslist57450$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist57450$k0)
store volatile %struct.ScmObj* %argslist57450$k1, %struct.ScmObj** %stackaddr$prim58658, align 8
%clofunc58659 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc58659(%struct.ScmObj* %k, %struct.ScmObj* %argslist57450$k1)
ret void
}

define tailcc void @proc_clo$ae52682(%struct.ScmObj* %env$ae52682,%struct.ScmObj* %current_45args57452) {
%stackaddr$prim58660 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57452)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim58660, align 8
%stackaddr$prim58661 = alloca %struct.ScmObj*, align 8
%current_45args57453 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57452)
store volatile %struct.ScmObj* %current_45args57453, %struct.ScmObj** %stackaddr$prim58661, align 8
%stackaddr$prim58662 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57453)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim58662, align 8
%stackaddr$prim58663 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim58663, align 8
%argslist57455$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58664 = alloca %struct.ScmObj*, align 8
%argslist57455$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist57455$k0)
store volatile %struct.ScmObj* %argslist57455$k1, %struct.ScmObj** %stackaddr$prim58664, align 8
%clofunc58665 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc58665(%struct.ScmObj* %k, %struct.ScmObj* %argslist57455$k1)
ret void
}

define tailcc void @proc_clo$ae52420(%struct.ScmObj* %env$ae52420,%struct.ScmObj* %current_45args57458) {
%stackaddr$prim58666 = alloca %struct.ScmObj*, align 8
%k48337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57458)
store volatile %struct.ScmObj* %k48337, %struct.ScmObj** %stackaddr$prim58666, align 8
%stackaddr$prim58667 = alloca %struct.ScmObj*, align 8
%current_45args57459 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57458)
store volatile %struct.ScmObj* %current_45args57459, %struct.ScmObj** %stackaddr$prim58667, align 8
%stackaddr$prim58668 = alloca %struct.ScmObj*, align 8
%thunk48159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57459)
store volatile %struct.ScmObj* %thunk48159, %struct.ScmObj** %stackaddr$prim58668, align 8
%stackaddr$prim58669 = alloca %struct.ScmObj*, align 8
%anf_45bind48285 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk48159)
store volatile %struct.ScmObj* %anf_45bind48285, %struct.ScmObj** %stackaddr$prim58669, align 8
%truthy$cmp58670 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48285)
%cmp$cmp58670 = icmp eq i64 %truthy$cmp58670, 1
br i1 %cmp$cmp58670, label %truebranch$cmp58670, label %falsebranch$cmp58670
truebranch$cmp58670:
%stackaddr$prim58671 = alloca %struct.ScmObj*, align 8
%anf_45bind48286 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk48159)
store volatile %struct.ScmObj* %anf_45bind48286, %struct.ScmObj** %stackaddr$prim58671, align 8
%ae52425 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim58672 = alloca %struct.ScmObj*, align 8
%anf_45bind48287 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind48286, %struct.ScmObj* %ae52425)
store volatile %struct.ScmObj* %anf_45bind48287, %struct.ScmObj** %stackaddr$prim58672, align 8
%truthy$cmp58673 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48287)
%cmp$cmp58673 = icmp eq i64 %truthy$cmp58673, 1
br i1 %cmp$cmp58673, label %truebranch$cmp58673, label %falsebranch$cmp58673
truebranch$cmp58673:
%ae52428 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58674 = alloca %struct.ScmObj*, align 8
%anf_45bind48288 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk48159, %struct.ScmObj* %ae52428)
store volatile %struct.ScmObj* %anf_45bind48288, %struct.ScmObj** %stackaddr$prim58674, align 8
%ae52430 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae5243058675, i32 0, i32 0))
%stackaddr$prim58676 = alloca %struct.ScmObj*, align 8
%cpsprim48338 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind48288, %struct.ScmObj* %ae52430)
store volatile %struct.ScmObj* %cpsprim48338, %struct.ScmObj** %stackaddr$prim58676, align 8
%ae52432 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57461$k483370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58677 = alloca %struct.ScmObj*, align 8
%argslist57461$k483371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48338, %struct.ScmObj* %argslist57461$k483370)
store volatile %struct.ScmObj* %argslist57461$k483371, %struct.ScmObj** %stackaddr$prim58677, align 8
%stackaddr$prim58678 = alloca %struct.ScmObj*, align 8
%argslist57461$k483372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52432, %struct.ScmObj* %argslist57461$k483371)
store volatile %struct.ScmObj* %argslist57461$k483372, %struct.ScmObj** %stackaddr$prim58678, align 8
%clofunc58679 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48337)
musttail call tailcc void %clofunc58679(%struct.ScmObj* %k48337, %struct.ScmObj* %argslist57461$k483372)
ret void
falsebranch$cmp58673:
%ae52450 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52451 = call %struct.ScmObj* @const_init_false()
%argslist57462$k483370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58680 = alloca %struct.ScmObj*, align 8
%argslist57462$k483371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52451, %struct.ScmObj* %argslist57462$k483370)
store volatile %struct.ScmObj* %argslist57462$k483371, %struct.ScmObj** %stackaddr$prim58680, align 8
%stackaddr$prim58681 = alloca %struct.ScmObj*, align 8
%argslist57462$k483372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52450, %struct.ScmObj* %argslist57462$k483371)
store volatile %struct.ScmObj* %argslist57462$k483372, %struct.ScmObj** %stackaddr$prim58681, align 8
%clofunc58682 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48337)
musttail call tailcc void %clofunc58682(%struct.ScmObj* %k48337, %struct.ScmObj* %argslist57462$k483372)
ret void
falsebranch$cmp58670:
%ae52472 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52473 = call %struct.ScmObj* @const_init_false()
%argslist57463$k483370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58683 = alloca %struct.ScmObj*, align 8
%argslist57463$k483371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52473, %struct.ScmObj* %argslist57463$k483370)
store volatile %struct.ScmObj* %argslist57463$k483371, %struct.ScmObj** %stackaddr$prim58683, align 8
%stackaddr$prim58684 = alloca %struct.ScmObj*, align 8
%argslist57463$k483372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52472, %struct.ScmObj* %argslist57463$k483371)
store volatile %struct.ScmObj* %argslist57463$k483372, %struct.ScmObj** %stackaddr$prim58684, align 8
%clofunc58685 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48337)
musttail call tailcc void %clofunc58685(%struct.ScmObj* %k48337, %struct.ScmObj* %argslist57463$k483372)
ret void
}

define tailcc void @proc_clo$ae53302(%struct.ScmObj* %env$ae53302,%struct.ScmObj* %current_45args57467) {
%stackaddr$prim58686 = alloca %struct.ScmObj*, align 8
%_95k48331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57467)
store volatile %struct.ScmObj* %_95k48331, %struct.ScmObj** %stackaddr$prim58686, align 8
%stackaddr$prim58687 = alloca %struct.ScmObj*, align 8
%current_45args57468 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57467)
store volatile %struct.ScmObj* %current_45args57468, %struct.ScmObj** %stackaddr$prim58687, align 8
%stackaddr$prim58688 = alloca %struct.ScmObj*, align 8
%thunk4802548151 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57468)
store volatile %struct.ScmObj* %thunk4802548151, %struct.ScmObj** %stackaddr$prim58688, align 8
%stackaddr$makeclosure58689 = alloca %struct.ScmObj*, align 8
%fptrToInt58690 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53308 to i64
%ae53308 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58690)
store volatile %struct.ScmObj* %ae53308, %struct.ScmObj** %stackaddr$makeclosure58689, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae53308, %struct.ScmObj* %thunk4802548151, i64 0)
%ae53309 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58691 = alloca %struct.ScmObj*, align 8
%fptrToInt58692 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53310 to i64
%ae53310 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58692)
store volatile %struct.ScmObj* %ae53310, %struct.ScmObj** %stackaddr$makeclosure58691, align 8
%argslist57507$ae533080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58693 = alloca %struct.ScmObj*, align 8
%argslist57507$ae533081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53310, %struct.ScmObj* %argslist57507$ae533080)
store volatile %struct.ScmObj* %argslist57507$ae533081, %struct.ScmObj** %stackaddr$prim58693, align 8
%stackaddr$prim58694 = alloca %struct.ScmObj*, align 8
%argslist57507$ae533082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53309, %struct.ScmObj* %argslist57507$ae533081)
store volatile %struct.ScmObj* %argslist57507$ae533082, %struct.ScmObj** %stackaddr$prim58694, align 8
%clofunc58695 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53308)
musttail call tailcc void %clofunc58695(%struct.ScmObj* %ae53308, %struct.ScmObj* %argslist57507$ae533082)
ret void
}

define tailcc void @proc_clo$ae53308(%struct.ScmObj* %env$ae53308,%struct.ScmObj* %current_45args57470) {
%stackaddr$env-ref58696 = alloca %struct.ScmObj*, align 8
%thunk4802548151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53308, i64 0)
store %struct.ScmObj* %thunk4802548151, %struct.ScmObj** %stackaddr$env-ref58696
%stackaddr$prim58697 = alloca %struct.ScmObj*, align 8
%_95k48332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57470)
store volatile %struct.ScmObj* %_95k48332, %struct.ScmObj** %stackaddr$prim58697, align 8
%stackaddr$prim58698 = alloca %struct.ScmObj*, align 8
%current_45args57471 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57470)
store volatile %struct.ScmObj* %current_45args57471, %struct.ScmObj** %stackaddr$prim58698, align 8
%stackaddr$prim58699 = alloca %struct.ScmObj*, align 8
%anf_45bind48289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57471)
store volatile %struct.ScmObj* %anf_45bind48289, %struct.ScmObj** %stackaddr$prim58699, align 8
%stackaddr$makeclosure58700 = alloca %struct.ScmObj*, align 8
%fptrToInt58701 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53396 to i64
%ae53396 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58701)
store volatile %struct.ScmObj* %ae53396, %struct.ScmObj** %stackaddr$makeclosure58700, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae53396, %struct.ScmObj* %thunk4802548151, i64 0)
%argslist57500$anf_45bind482890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58702 = alloca %struct.ScmObj*, align 8
%argslist57500$anf_45bind482891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %argslist57500$anf_45bind482890)
store volatile %struct.ScmObj* %argslist57500$anf_45bind482891, %struct.ScmObj** %stackaddr$prim58702, align 8
%stackaddr$prim58703 = alloca %struct.ScmObj*, align 8
%argslist57500$anf_45bind482892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53396, %struct.ScmObj* %argslist57500$anf_45bind482891)
store volatile %struct.ScmObj* %argslist57500$anf_45bind482892, %struct.ScmObj** %stackaddr$prim58703, align 8
%clofunc58704 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48289)
musttail call tailcc void %clofunc58704(%struct.ScmObj* %anf_45bind48289, %struct.ScmObj* %argslist57500$anf_45bind482892)
ret void
}

define tailcc void @proc_clo$ae53396(%struct.ScmObj* %env$ae53396,%struct.ScmObj* %current_45args57473) {
%stackaddr$env-ref58705 = alloca %struct.ScmObj*, align 8
%thunk4802548151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53396, i64 0)
store %struct.ScmObj* %thunk4802548151, %struct.ScmObj** %stackaddr$env-ref58705
%stackaddr$prim58706 = alloca %struct.ScmObj*, align 8
%_95k48333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57473)
store volatile %struct.ScmObj* %_95k48333, %struct.ScmObj** %stackaddr$prim58706, align 8
%stackaddr$prim58707 = alloca %struct.ScmObj*, align 8
%current_45args57474 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57473)
store volatile %struct.ScmObj* %current_45args57474, %struct.ScmObj** %stackaddr$prim58707, align 8
%stackaddr$prim58708 = alloca %struct.ScmObj*, align 8
%anf_45bind48290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57474)
store volatile %struct.ScmObj* %anf_45bind48290, %struct.ScmObj** %stackaddr$prim58708, align 8
%truthy$cmp58709 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48290)
%cmp$cmp58709 = icmp eq i64 %truthy$cmp58709, 1
br i1 %cmp$cmp58709, label %truebranch$cmp58709, label %falsebranch$cmp58709
truebranch$cmp58709:
%ae53400 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58710 = alloca %struct.ScmObj*, align 8
%anf_45bind48291 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %ae53400)
store volatile %struct.ScmObj* %anf_45bind48291, %struct.ScmObj** %stackaddr$prim58710, align 8
%truthy$cmp58711 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48291)
%cmp$cmp58711 = icmp eq i64 %truthy$cmp58711, 1
br i1 %cmp$cmp58711, label %truebranch$cmp58711, label %falsebranch$cmp58711
truebranch$cmp58711:
%ae53403 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim58712 = alloca %struct.ScmObj*, align 8
%cpsprim48334 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %ae53403)
store volatile %struct.ScmObj* %cpsprim48334, %struct.ScmObj** %stackaddr$prim58712, align 8
%stackaddr$makeclosure58713 = alloca %struct.ScmObj*, align 8
%fptrToInt58714 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53404 to i64
%ae53404 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58714)
store volatile %struct.ScmObj* %ae53404, %struct.ScmObj** %stackaddr$makeclosure58713, align 8
%ae53405 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57480$ae534040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58715 = alloca %struct.ScmObj*, align 8
%argslist57480$ae534041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48334, %struct.ScmObj* %argslist57480$ae534040)
store volatile %struct.ScmObj* %argslist57480$ae534041, %struct.ScmObj** %stackaddr$prim58715, align 8
%stackaddr$prim58716 = alloca %struct.ScmObj*, align 8
%argslist57480$ae534042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53405, %struct.ScmObj* %argslist57480$ae534041)
store volatile %struct.ScmObj* %argslist57480$ae534042, %struct.ScmObj** %stackaddr$prim58716, align 8
%clofunc58717 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53404)
musttail call tailcc void %clofunc58717(%struct.ScmObj* %ae53404, %struct.ScmObj* %argslist57480$ae534042)
ret void
falsebranch$cmp58711:
%ae53425 = call %struct.ScmObj* @const_init_int(i64 1)
%ae53426 = call %struct.ScmObj* @const_init_true()
%stackaddr$prim58718 = alloca %struct.ScmObj*, align 8
%t4803148160 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %ae53425, %struct.ScmObj* %ae53426)
store volatile %struct.ScmObj* %t4803148160, %struct.ScmObj** %stackaddr$prim58718, align 8
%ae53428 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim58719 = alloca %struct.ScmObj*, align 8
%anf_45bind48292 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %ae53428)
store volatile %struct.ScmObj* %anf_45bind48292, %struct.ScmObj** %stackaddr$prim58719, align 8
%stackaddr$makeclosure58720 = alloca %struct.ScmObj*, align 8
%fptrToInt58721 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53430 to i64
%ae53430 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58721)
store volatile %struct.ScmObj* %ae53430, %struct.ScmObj** %stackaddr$makeclosure58720, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae53430, %struct.ScmObj* %thunk4802548151, i64 0)
%ae53431 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @global$sym$ae5343158722, i32 0, i32 0))
%argslist57489$anf_45bind482920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58723 = alloca %struct.ScmObj*, align 8
%argslist57489$anf_45bind482921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53431, %struct.ScmObj* %argslist57489$anf_45bind482920)
store volatile %struct.ScmObj* %argslist57489$anf_45bind482921, %struct.ScmObj** %stackaddr$prim58723, align 8
%stackaddr$prim58724 = alloca %struct.ScmObj*, align 8
%argslist57489$anf_45bind482922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53430, %struct.ScmObj* %argslist57489$anf_45bind482921)
store volatile %struct.ScmObj* %argslist57489$anf_45bind482922, %struct.ScmObj** %stackaddr$prim58724, align 8
%clofunc58725 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48292)
musttail call tailcc void %clofunc58725(%struct.ScmObj* %anf_45bind48292, %struct.ScmObj* %argslist57489$anf_45bind482922)
ret void
falsebranch$cmp58709:
%stackaddr$prim58726 = alloca %struct.ScmObj*, align 8
%anf_45bind48293 = call %struct.ScmObj* @prim_number_63(%struct.ScmObj* %thunk4802548151)
store volatile %struct.ScmObj* %anf_45bind48293, %struct.ScmObj** %stackaddr$prim58726, align 8
%truthy$cmp58727 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48293)
%cmp$cmp58727 = icmp eq i64 %truthy$cmp58727, 1
br i1 %cmp$cmp58727, label %truebranch$cmp58727, label %falsebranch$cmp58727
truebranch$cmp58727:
%stackaddr$makeclosure58728 = alloca %struct.ScmObj*, align 8
%fptrToInt58729 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53560 to i64
%ae53560 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58729)
store volatile %struct.ScmObj* %ae53560, %struct.ScmObj** %stackaddr$makeclosure58728, align 8
%ae53561 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57494$ae535600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58730 = alloca %struct.ScmObj*, align 8
%argslist57494$ae535601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %argslist57494$ae535600)
store volatile %struct.ScmObj* %argslist57494$ae535601, %struct.ScmObj** %stackaddr$prim58730, align 8
%stackaddr$prim58731 = alloca %struct.ScmObj*, align 8
%argslist57494$ae535602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53561, %struct.ScmObj* %argslist57494$ae535601)
store volatile %struct.ScmObj* %argslist57494$ae535602, %struct.ScmObj** %stackaddr$prim58731, align 8
%clofunc58732 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53560)
musttail call tailcc void %clofunc58732(%struct.ScmObj* %ae53560, %struct.ScmObj* %argslist57494$ae535602)
ret void
falsebranch$cmp58727:
%stackaddr$makeclosure58733 = alloca %struct.ScmObj*, align 8
%fptrToInt58734 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53572 to i64
%ae53572 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58734)
store volatile %struct.ScmObj* %ae53572, %struct.ScmObj** %stackaddr$makeclosure58733, align 8
%ae53573 = call %struct.ScmObj* @const_init_int(i64 0)
%ae53574 = call %struct.ScmObj* @const_init_string(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @global$str$ae5357458735, i32 0, i32 0))
%argslist57499$ae535720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58736 = alloca %struct.ScmObj*, align 8
%argslist57499$ae535721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53574, %struct.ScmObj* %argslist57499$ae535720)
store volatile %struct.ScmObj* %argslist57499$ae535721, %struct.ScmObj** %stackaddr$prim58736, align 8
%stackaddr$prim58737 = alloca %struct.ScmObj*, align 8
%argslist57499$ae535722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53573, %struct.ScmObj* %argslist57499$ae535721)
store volatile %struct.ScmObj* %argslist57499$ae535722, %struct.ScmObj** %stackaddr$prim58737, align 8
%clofunc58738 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53572)
musttail call tailcc void %clofunc58738(%struct.ScmObj* %ae53572, %struct.ScmObj* %argslist57499$ae535722)
ret void
}

define tailcc void @proc_clo$ae53404(%struct.ScmObj* %env$ae53404,%struct.ScmObj* %current_45args57476) {
%stackaddr$prim58739 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57476)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim58739, align 8
%stackaddr$prim58740 = alloca %struct.ScmObj*, align 8
%current_45args57477 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57476)
store volatile %struct.ScmObj* %current_45args57477, %struct.ScmObj** %stackaddr$prim58740, align 8
%stackaddr$prim58741 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57477)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim58741, align 8
%stackaddr$prim58742 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim58742, align 8
%argslist57479$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58743 = alloca %struct.ScmObj*, align 8
%argslist57479$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist57479$k0)
store volatile %struct.ScmObj* %argslist57479$k1, %struct.ScmObj** %stackaddr$prim58743, align 8
%clofunc58744 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc58744(%struct.ScmObj* %k, %struct.ScmObj* %argslist57479$k1)
ret void
}

define tailcc void @proc_clo$ae53430(%struct.ScmObj* %env$ae53430,%struct.ScmObj* %current_45args57481) {
%stackaddr$env-ref58745 = alloca %struct.ScmObj*, align 8
%thunk4802548151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53430, i64 0)
store %struct.ScmObj* %thunk4802548151, %struct.ScmObj** %stackaddr$env-ref58745
%stackaddr$prim58746 = alloca %struct.ScmObj*, align 8
%_95k48335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57481)
store volatile %struct.ScmObj* %_95k48335, %struct.ScmObj** %stackaddr$prim58746, align 8
%stackaddr$prim58747 = alloca %struct.ScmObj*, align 8
%current_45args57482 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57481)
store volatile %struct.ScmObj* %current_45args57482, %struct.ScmObj** %stackaddr$prim58747, align 8
%stackaddr$prim58748 = alloca %struct.ScmObj*, align 8
%val4802648162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57482)
store volatile %struct.ScmObj* %val4802648162, %struct.ScmObj** %stackaddr$prim58748, align 8
%ae53436 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim58749 = alloca %struct.ScmObj*, align 8
%t4803248161 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %ae53436, %struct.ScmObj* %val4802648162)
store volatile %struct.ScmObj* %t4803248161, %struct.ScmObj** %stackaddr$prim58749, align 8
%ae53439 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim58750 = alloca %struct.ScmObj*, align 8
%cpsprim48336 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %ae53439)
store volatile %struct.ScmObj* %cpsprim48336, %struct.ScmObj** %stackaddr$prim58750, align 8
%stackaddr$makeclosure58751 = alloca %struct.ScmObj*, align 8
%fptrToInt58752 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53440 to i64
%ae53440 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58752)
store volatile %struct.ScmObj* %ae53440, %struct.ScmObj** %stackaddr$makeclosure58751, align 8
%ae53441 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57488$ae534400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58753 = alloca %struct.ScmObj*, align 8
%argslist57488$ae534401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48336, %struct.ScmObj* %argslist57488$ae534400)
store volatile %struct.ScmObj* %argslist57488$ae534401, %struct.ScmObj** %stackaddr$prim58753, align 8
%stackaddr$prim58754 = alloca %struct.ScmObj*, align 8
%argslist57488$ae534402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53441, %struct.ScmObj* %argslist57488$ae534401)
store volatile %struct.ScmObj* %argslist57488$ae534402, %struct.ScmObj** %stackaddr$prim58754, align 8
%clofunc58755 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53440)
musttail call tailcc void %clofunc58755(%struct.ScmObj* %ae53440, %struct.ScmObj* %argslist57488$ae534402)
ret void
}

define tailcc void @proc_clo$ae53440(%struct.ScmObj* %env$ae53440,%struct.ScmObj* %current_45args57484) {
%stackaddr$prim58756 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57484)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim58756, align 8
%stackaddr$prim58757 = alloca %struct.ScmObj*, align 8
%current_45args57485 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57484)
store volatile %struct.ScmObj* %current_45args57485, %struct.ScmObj** %stackaddr$prim58757, align 8
%stackaddr$prim58758 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57485)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim58758, align 8
%stackaddr$prim58759 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim58759, align 8
%argslist57487$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58760 = alloca %struct.ScmObj*, align 8
%argslist57487$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist57487$k0)
store volatile %struct.ScmObj* %argslist57487$k1, %struct.ScmObj** %stackaddr$prim58760, align 8
%clofunc58761 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc58761(%struct.ScmObj* %k, %struct.ScmObj* %argslist57487$k1)
ret void
}

define tailcc void @proc_clo$ae53560(%struct.ScmObj* %env$ae53560,%struct.ScmObj* %current_45args57490) {
%stackaddr$prim58762 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57490)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim58762, align 8
%stackaddr$prim58763 = alloca %struct.ScmObj*, align 8
%current_45args57491 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57490)
store volatile %struct.ScmObj* %current_45args57491, %struct.ScmObj** %stackaddr$prim58763, align 8
%stackaddr$prim58764 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57491)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim58764, align 8
%stackaddr$prim58765 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim58765, align 8
%argslist57493$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58766 = alloca %struct.ScmObj*, align 8
%argslist57493$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist57493$k0)
store volatile %struct.ScmObj* %argslist57493$k1, %struct.ScmObj** %stackaddr$prim58766, align 8
%clofunc58767 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc58767(%struct.ScmObj* %k, %struct.ScmObj* %argslist57493$k1)
ret void
}

define tailcc void @proc_clo$ae53572(%struct.ScmObj* %env$ae53572,%struct.ScmObj* %current_45args57495) {
%stackaddr$prim58768 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57495)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim58768, align 8
%stackaddr$prim58769 = alloca %struct.ScmObj*, align 8
%current_45args57496 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57495)
store volatile %struct.ScmObj* %current_45args57496, %struct.ScmObj** %stackaddr$prim58769, align 8
%stackaddr$prim58770 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57496)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim58770, align 8
%stackaddr$prim58771 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim58771, align 8
%argslist57498$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58772 = alloca %struct.ScmObj*, align 8
%argslist57498$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist57498$k0)
store volatile %struct.ScmObj* %argslist57498$k1, %struct.ScmObj** %stackaddr$prim58772, align 8
%clofunc58773 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc58773(%struct.ScmObj* %k, %struct.ScmObj* %argslist57498$k1)
ret void
}

define tailcc void @proc_clo$ae53310(%struct.ScmObj* %env$ae53310,%struct.ScmObj* %current_45args57501) {
%stackaddr$prim58774 = alloca %struct.ScmObj*, align 8
%k48337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57501)
store volatile %struct.ScmObj* %k48337, %struct.ScmObj** %stackaddr$prim58774, align 8
%stackaddr$prim58775 = alloca %struct.ScmObj*, align 8
%current_45args57502 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57501)
store volatile %struct.ScmObj* %current_45args57502, %struct.ScmObj** %stackaddr$prim58775, align 8
%stackaddr$prim58776 = alloca %struct.ScmObj*, align 8
%thunk48159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57502)
store volatile %struct.ScmObj* %thunk48159, %struct.ScmObj** %stackaddr$prim58776, align 8
%stackaddr$prim58777 = alloca %struct.ScmObj*, align 8
%anf_45bind48285 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk48159)
store volatile %struct.ScmObj* %anf_45bind48285, %struct.ScmObj** %stackaddr$prim58777, align 8
%truthy$cmp58778 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48285)
%cmp$cmp58778 = icmp eq i64 %truthy$cmp58778, 1
br i1 %cmp$cmp58778, label %truebranch$cmp58778, label %falsebranch$cmp58778
truebranch$cmp58778:
%stackaddr$prim58779 = alloca %struct.ScmObj*, align 8
%anf_45bind48286 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk48159)
store volatile %struct.ScmObj* %anf_45bind48286, %struct.ScmObj** %stackaddr$prim58779, align 8
%ae53315 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim58780 = alloca %struct.ScmObj*, align 8
%anf_45bind48287 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind48286, %struct.ScmObj* %ae53315)
store volatile %struct.ScmObj* %anf_45bind48287, %struct.ScmObj** %stackaddr$prim58780, align 8
%truthy$cmp58781 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48287)
%cmp$cmp58781 = icmp eq i64 %truthy$cmp58781, 1
br i1 %cmp$cmp58781, label %truebranch$cmp58781, label %falsebranch$cmp58781
truebranch$cmp58781:
%ae53318 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58782 = alloca %struct.ScmObj*, align 8
%anf_45bind48288 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk48159, %struct.ScmObj* %ae53318)
store volatile %struct.ScmObj* %anf_45bind48288, %struct.ScmObj** %stackaddr$prim58782, align 8
%ae53320 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae5332058783, i32 0, i32 0))
%stackaddr$prim58784 = alloca %struct.ScmObj*, align 8
%cpsprim48338 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind48288, %struct.ScmObj* %ae53320)
store volatile %struct.ScmObj* %cpsprim48338, %struct.ScmObj** %stackaddr$prim58784, align 8
%ae53322 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57504$k483370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58785 = alloca %struct.ScmObj*, align 8
%argslist57504$k483371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48338, %struct.ScmObj* %argslist57504$k483370)
store volatile %struct.ScmObj* %argslist57504$k483371, %struct.ScmObj** %stackaddr$prim58785, align 8
%stackaddr$prim58786 = alloca %struct.ScmObj*, align 8
%argslist57504$k483372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53322, %struct.ScmObj* %argslist57504$k483371)
store volatile %struct.ScmObj* %argslist57504$k483372, %struct.ScmObj** %stackaddr$prim58786, align 8
%clofunc58787 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48337)
musttail call tailcc void %clofunc58787(%struct.ScmObj* %k48337, %struct.ScmObj* %argslist57504$k483372)
ret void
falsebranch$cmp58781:
%ae53340 = call %struct.ScmObj* @const_init_int(i64 0)
%ae53341 = call %struct.ScmObj* @const_init_false()
%argslist57505$k483370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58788 = alloca %struct.ScmObj*, align 8
%argslist57505$k483371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53341, %struct.ScmObj* %argslist57505$k483370)
store volatile %struct.ScmObj* %argslist57505$k483371, %struct.ScmObj** %stackaddr$prim58788, align 8
%stackaddr$prim58789 = alloca %struct.ScmObj*, align 8
%argslist57505$k483372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53340, %struct.ScmObj* %argslist57505$k483371)
store volatile %struct.ScmObj* %argslist57505$k483372, %struct.ScmObj** %stackaddr$prim58789, align 8
%clofunc58790 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48337)
musttail call tailcc void %clofunc58790(%struct.ScmObj* %k48337, %struct.ScmObj* %argslist57505$k483372)
ret void
falsebranch$cmp58778:
%ae53362 = call %struct.ScmObj* @const_init_int(i64 0)
%ae53363 = call %struct.ScmObj* @const_init_false()
%argslist57506$k483370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58791 = alloca %struct.ScmObj*, align 8
%argslist57506$k483371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53363, %struct.ScmObj* %argslist57506$k483370)
store volatile %struct.ScmObj* %argslist57506$k483371, %struct.ScmObj** %stackaddr$prim58791, align 8
%stackaddr$prim58792 = alloca %struct.ScmObj*, align 8
%argslist57506$k483372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53362, %struct.ScmObj* %argslist57506$k483371)
store volatile %struct.ScmObj* %argslist57506$k483372, %struct.ScmObj** %stackaddr$prim58792, align 8
%clofunc58793 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48337)
musttail call tailcc void %clofunc58793(%struct.ScmObj* %k48337, %struct.ScmObj* %argslist57506$k483372)
ret void
}

define tailcc void @proc_clo$ae53615(%struct.ScmObj* %env$ae53615,%struct.ScmObj* %current_45args57509) {
%stackaddr$prim58794 = alloca %struct.ScmObj*, align 8
%_95k48331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57509)
store volatile %struct.ScmObj* %_95k48331, %struct.ScmObj** %stackaddr$prim58794, align 8
%stackaddr$prim58795 = alloca %struct.ScmObj*, align 8
%current_45args57510 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57509)
store volatile %struct.ScmObj* %current_45args57510, %struct.ScmObj** %stackaddr$prim58795, align 8
%stackaddr$prim58796 = alloca %struct.ScmObj*, align 8
%thunk4802548151 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57510)
store volatile %struct.ScmObj* %thunk4802548151, %struct.ScmObj** %stackaddr$prim58796, align 8
%stackaddr$makeclosure58797 = alloca %struct.ScmObj*, align 8
%fptrToInt58798 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53624 to i64
%ae53624 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58798)
store volatile %struct.ScmObj* %ae53624, %struct.ScmObj** %stackaddr$makeclosure58797, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae53624, %struct.ScmObj* %thunk4802548151, i64 0)
%ae53625 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58799 = alloca %struct.ScmObj*, align 8
%fptrToInt58800 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53626 to i64
%ae53626 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58800)
store volatile %struct.ScmObj* %ae53626, %struct.ScmObj** %stackaddr$makeclosure58799, align 8
%argslist57549$ae536240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58801 = alloca %struct.ScmObj*, align 8
%argslist57549$ae536241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53626, %struct.ScmObj* %argslist57549$ae536240)
store volatile %struct.ScmObj* %argslist57549$ae536241, %struct.ScmObj** %stackaddr$prim58801, align 8
%stackaddr$prim58802 = alloca %struct.ScmObj*, align 8
%argslist57549$ae536242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53625, %struct.ScmObj* %argslist57549$ae536241)
store volatile %struct.ScmObj* %argslist57549$ae536242, %struct.ScmObj** %stackaddr$prim58802, align 8
%clofunc58803 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53624)
musttail call tailcc void %clofunc58803(%struct.ScmObj* %ae53624, %struct.ScmObj* %argslist57549$ae536242)
ret void
}

define tailcc void @proc_clo$ae53624(%struct.ScmObj* %env$ae53624,%struct.ScmObj* %current_45args57512) {
%stackaddr$env-ref58804 = alloca %struct.ScmObj*, align 8
%thunk4802548151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53624, i64 0)
store %struct.ScmObj* %thunk4802548151, %struct.ScmObj** %stackaddr$env-ref58804
%stackaddr$prim58805 = alloca %struct.ScmObj*, align 8
%_95k48332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57512)
store volatile %struct.ScmObj* %_95k48332, %struct.ScmObj** %stackaddr$prim58805, align 8
%stackaddr$prim58806 = alloca %struct.ScmObj*, align 8
%current_45args57513 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57512)
store volatile %struct.ScmObj* %current_45args57513, %struct.ScmObj** %stackaddr$prim58806, align 8
%stackaddr$prim58807 = alloca %struct.ScmObj*, align 8
%anf_45bind48289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57513)
store volatile %struct.ScmObj* %anf_45bind48289, %struct.ScmObj** %stackaddr$prim58807, align 8
%stackaddr$makeclosure58808 = alloca %struct.ScmObj*, align 8
%fptrToInt58809 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53712 to i64
%ae53712 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58809)
store volatile %struct.ScmObj* %ae53712, %struct.ScmObj** %stackaddr$makeclosure58808, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae53712, %struct.ScmObj* %thunk4802548151, i64 0)
%argslist57542$anf_45bind482890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58810 = alloca %struct.ScmObj*, align 8
%argslist57542$anf_45bind482891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %argslist57542$anf_45bind482890)
store volatile %struct.ScmObj* %argslist57542$anf_45bind482891, %struct.ScmObj** %stackaddr$prim58810, align 8
%stackaddr$prim58811 = alloca %struct.ScmObj*, align 8
%argslist57542$anf_45bind482892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53712, %struct.ScmObj* %argslist57542$anf_45bind482891)
store volatile %struct.ScmObj* %argslist57542$anf_45bind482892, %struct.ScmObj** %stackaddr$prim58811, align 8
%clofunc58812 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48289)
musttail call tailcc void %clofunc58812(%struct.ScmObj* %anf_45bind48289, %struct.ScmObj* %argslist57542$anf_45bind482892)
ret void
}

define tailcc void @proc_clo$ae53712(%struct.ScmObj* %env$ae53712,%struct.ScmObj* %current_45args57515) {
%stackaddr$env-ref58813 = alloca %struct.ScmObj*, align 8
%thunk4802548151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53712, i64 0)
store %struct.ScmObj* %thunk4802548151, %struct.ScmObj** %stackaddr$env-ref58813
%stackaddr$prim58814 = alloca %struct.ScmObj*, align 8
%_95k48333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57515)
store volatile %struct.ScmObj* %_95k48333, %struct.ScmObj** %stackaddr$prim58814, align 8
%stackaddr$prim58815 = alloca %struct.ScmObj*, align 8
%current_45args57516 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57515)
store volatile %struct.ScmObj* %current_45args57516, %struct.ScmObj** %stackaddr$prim58815, align 8
%stackaddr$prim58816 = alloca %struct.ScmObj*, align 8
%anf_45bind48290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57516)
store volatile %struct.ScmObj* %anf_45bind48290, %struct.ScmObj** %stackaddr$prim58816, align 8
%truthy$cmp58817 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48290)
%cmp$cmp58817 = icmp eq i64 %truthy$cmp58817, 1
br i1 %cmp$cmp58817, label %truebranch$cmp58817, label %falsebranch$cmp58817
truebranch$cmp58817:
%ae53716 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim58818 = alloca %struct.ScmObj*, align 8
%anf_45bind48291 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %ae53716)
store volatile %struct.ScmObj* %anf_45bind48291, %struct.ScmObj** %stackaddr$prim58818, align 8
%truthy$cmp58819 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48291)
%cmp$cmp58819 = icmp eq i64 %truthy$cmp58819, 1
br i1 %cmp$cmp58819, label %truebranch$cmp58819, label %falsebranch$cmp58819
truebranch$cmp58819:
%ae53719 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim58820 = alloca %struct.ScmObj*, align 8
%cpsprim48334 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %ae53719)
store volatile %struct.ScmObj* %cpsprim48334, %struct.ScmObj** %stackaddr$prim58820, align 8
%stackaddr$makeclosure58821 = alloca %struct.ScmObj*, align 8
%fptrToInt58822 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53720 to i64
%ae53720 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58822)
store volatile %struct.ScmObj* %ae53720, %struct.ScmObj** %stackaddr$makeclosure58821, align 8
%ae53721 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57522$ae537200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58823 = alloca %struct.ScmObj*, align 8
%argslist57522$ae537201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48334, %struct.ScmObj* %argslist57522$ae537200)
store volatile %struct.ScmObj* %argslist57522$ae537201, %struct.ScmObj** %stackaddr$prim58823, align 8
%stackaddr$prim58824 = alloca %struct.ScmObj*, align 8
%argslist57522$ae537202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53721, %struct.ScmObj* %argslist57522$ae537201)
store volatile %struct.ScmObj* %argslist57522$ae537202, %struct.ScmObj** %stackaddr$prim58824, align 8
%clofunc58825 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53720)
musttail call tailcc void %clofunc58825(%struct.ScmObj* %ae53720, %struct.ScmObj* %argslist57522$ae537202)
ret void
falsebranch$cmp58819:
%ae53741 = call %struct.ScmObj* @const_init_int(i64 1)
%ae53742 = call %struct.ScmObj* @const_init_true()
%stackaddr$prim58826 = alloca %struct.ScmObj*, align 8
%t4803148160 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %ae53741, %struct.ScmObj* %ae53742)
store volatile %struct.ScmObj* %t4803148160, %struct.ScmObj** %stackaddr$prim58826, align 8
%ae53744 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim58827 = alloca %struct.ScmObj*, align 8
%anf_45bind48292 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %ae53744)
store volatile %struct.ScmObj* %anf_45bind48292, %struct.ScmObj** %stackaddr$prim58827, align 8
%stackaddr$makeclosure58828 = alloca %struct.ScmObj*, align 8
%fptrToInt58829 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53746 to i64
%ae53746 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt58829)
store volatile %struct.ScmObj* %ae53746, %struct.ScmObj** %stackaddr$makeclosure58828, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae53746, %struct.ScmObj* %thunk4802548151, i64 0)
%ae53747 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @global$sym$ae5374758830, i32 0, i32 0))
%argslist57531$anf_45bind482920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58831 = alloca %struct.ScmObj*, align 8
%argslist57531$anf_45bind482921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53747, %struct.ScmObj* %argslist57531$anf_45bind482920)
store volatile %struct.ScmObj* %argslist57531$anf_45bind482921, %struct.ScmObj** %stackaddr$prim58831, align 8
%stackaddr$prim58832 = alloca %struct.ScmObj*, align 8
%argslist57531$anf_45bind482922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53746, %struct.ScmObj* %argslist57531$anf_45bind482921)
store volatile %struct.ScmObj* %argslist57531$anf_45bind482922, %struct.ScmObj** %stackaddr$prim58832, align 8
%clofunc58833 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48292)
musttail call tailcc void %clofunc58833(%struct.ScmObj* %anf_45bind48292, %struct.ScmObj* %argslist57531$anf_45bind482922)
ret void
falsebranch$cmp58817:
%stackaddr$prim58834 = alloca %struct.ScmObj*, align 8
%anf_45bind48293 = call %struct.ScmObj* @prim_number_63(%struct.ScmObj* %thunk4802548151)
store volatile %struct.ScmObj* %anf_45bind48293, %struct.ScmObj** %stackaddr$prim58834, align 8
%truthy$cmp58835 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48293)
%cmp$cmp58835 = icmp eq i64 %truthy$cmp58835, 1
br i1 %cmp$cmp58835, label %truebranch$cmp58835, label %falsebranch$cmp58835
truebranch$cmp58835:
%stackaddr$makeclosure58836 = alloca %struct.ScmObj*, align 8
%fptrToInt58837 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53876 to i64
%ae53876 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58837)
store volatile %struct.ScmObj* %ae53876, %struct.ScmObj** %stackaddr$makeclosure58836, align 8
%ae53877 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57536$ae538760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58838 = alloca %struct.ScmObj*, align 8
%argslist57536$ae538761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %argslist57536$ae538760)
store volatile %struct.ScmObj* %argslist57536$ae538761, %struct.ScmObj** %stackaddr$prim58838, align 8
%stackaddr$prim58839 = alloca %struct.ScmObj*, align 8
%argslist57536$ae538762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53877, %struct.ScmObj* %argslist57536$ae538761)
store volatile %struct.ScmObj* %argslist57536$ae538762, %struct.ScmObj** %stackaddr$prim58839, align 8
%clofunc58840 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53876)
musttail call tailcc void %clofunc58840(%struct.ScmObj* %ae53876, %struct.ScmObj* %argslist57536$ae538762)
ret void
falsebranch$cmp58835:
%stackaddr$makeclosure58841 = alloca %struct.ScmObj*, align 8
%fptrToInt58842 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53888 to i64
%ae53888 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58842)
store volatile %struct.ScmObj* %ae53888, %struct.ScmObj** %stackaddr$makeclosure58841, align 8
%ae53889 = call %struct.ScmObj* @const_init_int(i64 0)
%ae53890 = call %struct.ScmObj* @const_init_string(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @global$str$ae5389058843, i32 0, i32 0))
%argslist57541$ae538880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58844 = alloca %struct.ScmObj*, align 8
%argslist57541$ae538881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53890, %struct.ScmObj* %argslist57541$ae538880)
store volatile %struct.ScmObj* %argslist57541$ae538881, %struct.ScmObj** %stackaddr$prim58844, align 8
%stackaddr$prim58845 = alloca %struct.ScmObj*, align 8
%argslist57541$ae538882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53889, %struct.ScmObj* %argslist57541$ae538881)
store volatile %struct.ScmObj* %argslist57541$ae538882, %struct.ScmObj** %stackaddr$prim58845, align 8
%clofunc58846 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53888)
musttail call tailcc void %clofunc58846(%struct.ScmObj* %ae53888, %struct.ScmObj* %argslist57541$ae538882)
ret void
}

define tailcc void @proc_clo$ae53720(%struct.ScmObj* %env$ae53720,%struct.ScmObj* %current_45args57518) {
%stackaddr$prim58847 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57518)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim58847, align 8
%stackaddr$prim58848 = alloca %struct.ScmObj*, align 8
%current_45args57519 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57518)
store volatile %struct.ScmObj* %current_45args57519, %struct.ScmObj** %stackaddr$prim58848, align 8
%stackaddr$prim58849 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57519)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim58849, align 8
%stackaddr$prim58850 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim58850, align 8
%argslist57521$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58851 = alloca %struct.ScmObj*, align 8
%argslist57521$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist57521$k0)
store volatile %struct.ScmObj* %argslist57521$k1, %struct.ScmObj** %stackaddr$prim58851, align 8
%clofunc58852 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc58852(%struct.ScmObj* %k, %struct.ScmObj* %argslist57521$k1)
ret void
}

define tailcc void @proc_clo$ae53746(%struct.ScmObj* %env$ae53746,%struct.ScmObj* %current_45args57523) {
%stackaddr$env-ref58853 = alloca %struct.ScmObj*, align 8
%thunk4802548151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae53746, i64 0)
store %struct.ScmObj* %thunk4802548151, %struct.ScmObj** %stackaddr$env-ref58853
%stackaddr$prim58854 = alloca %struct.ScmObj*, align 8
%_95k48335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57523)
store volatile %struct.ScmObj* %_95k48335, %struct.ScmObj** %stackaddr$prim58854, align 8
%stackaddr$prim58855 = alloca %struct.ScmObj*, align 8
%current_45args57524 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57523)
store volatile %struct.ScmObj* %current_45args57524, %struct.ScmObj** %stackaddr$prim58855, align 8
%stackaddr$prim58856 = alloca %struct.ScmObj*, align 8
%val4802648162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57524)
store volatile %struct.ScmObj* %val4802648162, %struct.ScmObj** %stackaddr$prim58856, align 8
%ae53752 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim58857 = alloca %struct.ScmObj*, align 8
%t4803248161 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %ae53752, %struct.ScmObj* %val4802648162)
store volatile %struct.ScmObj* %t4803248161, %struct.ScmObj** %stackaddr$prim58857, align 8
%ae53755 = call %struct.ScmObj* @const_init_int(i64 2)
%stackaddr$prim58858 = alloca %struct.ScmObj*, align 8
%cpsprim48336 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk4802548151, %struct.ScmObj* %ae53755)
store volatile %struct.ScmObj* %cpsprim48336, %struct.ScmObj** %stackaddr$prim58858, align 8
%stackaddr$makeclosure58859 = alloca %struct.ScmObj*, align 8
%fptrToInt58860 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae53756 to i64
%ae53756 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58860)
store volatile %struct.ScmObj* %ae53756, %struct.ScmObj** %stackaddr$makeclosure58859, align 8
%ae53757 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57530$ae537560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58861 = alloca %struct.ScmObj*, align 8
%argslist57530$ae537561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48336, %struct.ScmObj* %argslist57530$ae537560)
store volatile %struct.ScmObj* %argslist57530$ae537561, %struct.ScmObj** %stackaddr$prim58861, align 8
%stackaddr$prim58862 = alloca %struct.ScmObj*, align 8
%argslist57530$ae537562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53757, %struct.ScmObj* %argslist57530$ae537561)
store volatile %struct.ScmObj* %argslist57530$ae537562, %struct.ScmObj** %stackaddr$prim58862, align 8
%clofunc58863 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae53756)
musttail call tailcc void %clofunc58863(%struct.ScmObj* %ae53756, %struct.ScmObj* %argslist57530$ae537562)
ret void
}

define tailcc void @proc_clo$ae53756(%struct.ScmObj* %env$ae53756,%struct.ScmObj* %current_45args57526) {
%stackaddr$prim58864 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57526)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim58864, align 8
%stackaddr$prim58865 = alloca %struct.ScmObj*, align 8
%current_45args57527 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57526)
store volatile %struct.ScmObj* %current_45args57527, %struct.ScmObj** %stackaddr$prim58865, align 8
%stackaddr$prim58866 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57527)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim58866, align 8
%stackaddr$prim58867 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim58867, align 8
%argslist57529$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58868 = alloca %struct.ScmObj*, align 8
%argslist57529$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist57529$k0)
store volatile %struct.ScmObj* %argslist57529$k1, %struct.ScmObj** %stackaddr$prim58868, align 8
%clofunc58869 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc58869(%struct.ScmObj* %k, %struct.ScmObj* %argslist57529$k1)
ret void
}

define tailcc void @proc_clo$ae53876(%struct.ScmObj* %env$ae53876,%struct.ScmObj* %current_45args57532) {
%stackaddr$prim58870 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57532)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim58870, align 8
%stackaddr$prim58871 = alloca %struct.ScmObj*, align 8
%current_45args57533 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57532)
store volatile %struct.ScmObj* %current_45args57533, %struct.ScmObj** %stackaddr$prim58871, align 8
%stackaddr$prim58872 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57533)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim58872, align 8
%stackaddr$prim58873 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim58873, align 8
%argslist57535$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58874 = alloca %struct.ScmObj*, align 8
%argslist57535$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist57535$k0)
store volatile %struct.ScmObj* %argslist57535$k1, %struct.ScmObj** %stackaddr$prim58874, align 8
%clofunc58875 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc58875(%struct.ScmObj* %k, %struct.ScmObj* %argslist57535$k1)
ret void
}

define tailcc void @proc_clo$ae53888(%struct.ScmObj* %env$ae53888,%struct.ScmObj* %current_45args57537) {
%stackaddr$prim58876 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57537)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim58876, align 8
%stackaddr$prim58877 = alloca %struct.ScmObj*, align 8
%current_45args57538 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57537)
store volatile %struct.ScmObj* %current_45args57538, %struct.ScmObj** %stackaddr$prim58877, align 8
%stackaddr$prim58878 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57538)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim58878, align 8
%stackaddr$prim58879 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim58879, align 8
%argslist57540$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58880 = alloca %struct.ScmObj*, align 8
%argslist57540$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist57540$k0)
store volatile %struct.ScmObj* %argslist57540$k1, %struct.ScmObj** %stackaddr$prim58880, align 8
%clofunc58881 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc58881(%struct.ScmObj* %k, %struct.ScmObj* %argslist57540$k1)
ret void
}

define tailcc void @proc_clo$ae53626(%struct.ScmObj* %env$ae53626,%struct.ScmObj* %current_45args57543) {
%stackaddr$prim58882 = alloca %struct.ScmObj*, align 8
%k48337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57543)
store volatile %struct.ScmObj* %k48337, %struct.ScmObj** %stackaddr$prim58882, align 8
%stackaddr$prim58883 = alloca %struct.ScmObj*, align 8
%current_45args57544 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57543)
store volatile %struct.ScmObj* %current_45args57544, %struct.ScmObj** %stackaddr$prim58883, align 8
%stackaddr$prim58884 = alloca %struct.ScmObj*, align 8
%thunk48159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57544)
store volatile %struct.ScmObj* %thunk48159, %struct.ScmObj** %stackaddr$prim58884, align 8
%stackaddr$prim58885 = alloca %struct.ScmObj*, align 8
%anf_45bind48285 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk48159)
store volatile %struct.ScmObj* %anf_45bind48285, %struct.ScmObj** %stackaddr$prim58885, align 8
%truthy$cmp58886 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48285)
%cmp$cmp58886 = icmp eq i64 %truthy$cmp58886, 1
br i1 %cmp$cmp58886, label %truebranch$cmp58886, label %falsebranch$cmp58886
truebranch$cmp58886:
%stackaddr$prim58887 = alloca %struct.ScmObj*, align 8
%anf_45bind48286 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk48159)
store volatile %struct.ScmObj* %anf_45bind48286, %struct.ScmObj** %stackaddr$prim58887, align 8
%ae53631 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim58888 = alloca %struct.ScmObj*, align 8
%anf_45bind48287 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind48286, %struct.ScmObj* %ae53631)
store volatile %struct.ScmObj* %anf_45bind48287, %struct.ScmObj** %stackaddr$prim58888, align 8
%truthy$cmp58889 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48287)
%cmp$cmp58889 = icmp eq i64 %truthy$cmp58889, 1
br i1 %cmp$cmp58889, label %truebranch$cmp58889, label %falsebranch$cmp58889
truebranch$cmp58889:
%ae53634 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58890 = alloca %struct.ScmObj*, align 8
%anf_45bind48288 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk48159, %struct.ScmObj* %ae53634)
store volatile %struct.ScmObj* %anf_45bind48288, %struct.ScmObj** %stackaddr$prim58890, align 8
%ae53636 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae5363658891, i32 0, i32 0))
%stackaddr$prim58892 = alloca %struct.ScmObj*, align 8
%cpsprim48338 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind48288, %struct.ScmObj* %ae53636)
store volatile %struct.ScmObj* %cpsprim48338, %struct.ScmObj** %stackaddr$prim58892, align 8
%ae53638 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57546$k483370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58893 = alloca %struct.ScmObj*, align 8
%argslist57546$k483371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48338, %struct.ScmObj* %argslist57546$k483370)
store volatile %struct.ScmObj* %argslist57546$k483371, %struct.ScmObj** %stackaddr$prim58893, align 8
%stackaddr$prim58894 = alloca %struct.ScmObj*, align 8
%argslist57546$k483372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53638, %struct.ScmObj* %argslist57546$k483371)
store volatile %struct.ScmObj* %argslist57546$k483372, %struct.ScmObj** %stackaddr$prim58894, align 8
%clofunc58895 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48337)
musttail call tailcc void %clofunc58895(%struct.ScmObj* %k48337, %struct.ScmObj* %argslist57546$k483372)
ret void
falsebranch$cmp58889:
%ae53656 = call %struct.ScmObj* @const_init_int(i64 0)
%ae53657 = call %struct.ScmObj* @const_init_false()
%argslist57547$k483370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58896 = alloca %struct.ScmObj*, align 8
%argslist57547$k483371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53657, %struct.ScmObj* %argslist57547$k483370)
store volatile %struct.ScmObj* %argslist57547$k483371, %struct.ScmObj** %stackaddr$prim58896, align 8
%stackaddr$prim58897 = alloca %struct.ScmObj*, align 8
%argslist57547$k483372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53656, %struct.ScmObj* %argslist57547$k483371)
store volatile %struct.ScmObj* %argslist57547$k483372, %struct.ScmObj** %stackaddr$prim58897, align 8
%clofunc58898 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48337)
musttail call tailcc void %clofunc58898(%struct.ScmObj* %k48337, %struct.ScmObj* %argslist57547$k483372)
ret void
falsebranch$cmp58886:
%ae53678 = call %struct.ScmObj* @const_init_int(i64 0)
%ae53679 = call %struct.ScmObj* @const_init_false()
%argslist57548$k483370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58899 = alloca %struct.ScmObj*, align 8
%argslist57548$k483371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53679, %struct.ScmObj* %argslist57548$k483370)
store volatile %struct.ScmObj* %argslist57548$k483371, %struct.ScmObj** %stackaddr$prim58899, align 8
%stackaddr$prim58900 = alloca %struct.ScmObj*, align 8
%argslist57548$k483372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae53678, %struct.ScmObj* %argslist57548$k483371)
store volatile %struct.ScmObj* %argslist57548$k483372, %struct.ScmObj** %stackaddr$prim58900, align 8
%clofunc58901 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48337)
musttail call tailcc void %clofunc58901(%struct.ScmObj* %k48337, %struct.ScmObj* %argslist57548$k483372)
ret void
}

define tailcc void @proc_clo$ae51914(%struct.ScmObj* %env$ae51914,%struct.ScmObj* %current_45args57552) {
%stackaddr$prim58902 = alloca %struct.ScmObj*, align 8
%k48342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57552)
store volatile %struct.ScmObj* %k48342, %struct.ScmObj** %stackaddr$prim58902, align 8
%stackaddr$prim58903 = alloca %struct.ScmObj*, align 8
%current_45args57553 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57552)
store volatile %struct.ScmObj* %current_45args57553, %struct.ScmObj** %stackaddr$prim58903, align 8
%stackaddr$prim58904 = alloca %struct.ScmObj*, align 8
%thunk48155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57553)
store volatile %struct.ScmObj* %thunk48155, %struct.ScmObj** %stackaddr$prim58904, align 8
%stackaddr$prim58905 = alloca %struct.ScmObj*, align 8
%anf_45bind48276 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk48155)
store volatile %struct.ScmObj* %anf_45bind48276, %struct.ScmObj** %stackaddr$prim58905, align 8
%truthy$cmp58906 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48276)
%cmp$cmp58906 = icmp eq i64 %truthy$cmp58906, 1
br i1 %cmp$cmp58906, label %truebranch$cmp58906, label %falsebranch$cmp58906
truebranch$cmp58906:
%stackaddr$prim58907 = alloca %struct.ScmObj*, align 8
%anf_45bind48277 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk48155)
store volatile %struct.ScmObj* %anf_45bind48277, %struct.ScmObj** %stackaddr$prim58907, align 8
%ae51919 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim58908 = alloca %struct.ScmObj*, align 8
%anf_45bind48278 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind48277, %struct.ScmObj* %ae51919)
store volatile %struct.ScmObj* %anf_45bind48278, %struct.ScmObj** %stackaddr$prim58908, align 8
%truthy$cmp58909 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48278)
%cmp$cmp58909 = icmp eq i64 %truthy$cmp58909, 1
br i1 %cmp$cmp58909, label %truebranch$cmp58909, label %falsebranch$cmp58909
truebranch$cmp58909:
%ae51922 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim58910 = alloca %struct.ScmObj*, align 8
%anf_45bind48279 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk48155, %struct.ScmObj* %ae51922)
store volatile %struct.ScmObj* %anf_45bind48279, %struct.ScmObj** %stackaddr$prim58910, align 8
%ae51924 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae5192458911, i32 0, i32 0))
%stackaddr$prim58912 = alloca %struct.ScmObj*, align 8
%cpsprim48343 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind48279, %struct.ScmObj* %ae51924)
store volatile %struct.ScmObj* %cpsprim48343, %struct.ScmObj** %stackaddr$prim58912, align 8
%ae51926 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57555$k483420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58913 = alloca %struct.ScmObj*, align 8
%argslist57555$k483421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48343, %struct.ScmObj* %argslist57555$k483420)
store volatile %struct.ScmObj* %argslist57555$k483421, %struct.ScmObj** %stackaddr$prim58913, align 8
%stackaddr$prim58914 = alloca %struct.ScmObj*, align 8
%argslist57555$k483422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51926, %struct.ScmObj* %argslist57555$k483421)
store volatile %struct.ScmObj* %argslist57555$k483422, %struct.ScmObj** %stackaddr$prim58914, align 8
%clofunc58915 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48342)
musttail call tailcc void %clofunc58915(%struct.ScmObj* %k48342, %struct.ScmObj* %argslist57555$k483422)
ret void
falsebranch$cmp58909:
%ae51944 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51945 = call %struct.ScmObj* @const_init_false()
%argslist57556$k483420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58916 = alloca %struct.ScmObj*, align 8
%argslist57556$k483421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51945, %struct.ScmObj* %argslist57556$k483420)
store volatile %struct.ScmObj* %argslist57556$k483421, %struct.ScmObj** %stackaddr$prim58916, align 8
%stackaddr$prim58917 = alloca %struct.ScmObj*, align 8
%argslist57556$k483422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51944, %struct.ScmObj* %argslist57556$k483421)
store volatile %struct.ScmObj* %argslist57556$k483422, %struct.ScmObj** %stackaddr$prim58917, align 8
%clofunc58918 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48342)
musttail call tailcc void %clofunc58918(%struct.ScmObj* %k48342, %struct.ScmObj* %argslist57556$k483422)
ret void
falsebranch$cmp58906:
%ae51966 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51967 = call %struct.ScmObj* @const_init_false()
%argslist57557$k483420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58919 = alloca %struct.ScmObj*, align 8
%argslist57557$k483421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51967, %struct.ScmObj* %argslist57557$k483420)
store volatile %struct.ScmObj* %argslist57557$k483421, %struct.ScmObj** %stackaddr$prim58919, align 8
%stackaddr$prim58920 = alloca %struct.ScmObj*, align 8
%argslist57557$k483422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51966, %struct.ScmObj* %argslist57557$k483421)
store volatile %struct.ScmObj* %argslist57557$k483422, %struct.ScmObj** %stackaddr$prim58920, align 8
%clofunc58921 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48342)
musttail call tailcc void %clofunc58921(%struct.ScmObj* %k48342, %struct.ScmObj* %argslist57557$k483422)
ret void
}

define tailcc void @proc_clo$ae51878(%struct.ScmObj* %env$ae51878,%struct.ScmObj* %current_45args57560) {
%stackaddr$prim58922 = alloca %struct.ScmObj*, align 8
%k48344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57560)
store volatile %struct.ScmObj* %k48344, %struct.ScmObj** %stackaddr$prim58922, align 8
%stackaddr$prim58923 = alloca %struct.ScmObj*, align 8
%current_45args57561 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57560)
store volatile %struct.ScmObj* %current_45args57561, %struct.ScmObj** %stackaddr$prim58923, align 8
%stackaddr$prim58924 = alloca %struct.ScmObj*, align 8
%_9548154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57561)
store volatile %struct.ScmObj* %_9548154, %struct.ScmObj** %stackaddr$prim58924, align 8
%ae51880 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51881 = call %struct.ScmObj* @const_init_int(i64 29)
%argslist57563$k483440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58925 = alloca %struct.ScmObj*, align 8
%argslist57563$k483441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51881, %struct.ScmObj* %argslist57563$k483440)
store volatile %struct.ScmObj* %argslist57563$k483441, %struct.ScmObj** %stackaddr$prim58925, align 8
%stackaddr$prim58926 = alloca %struct.ScmObj*, align 8
%argslist57563$k483442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51880, %struct.ScmObj* %argslist57563$k483441)
store volatile %struct.ScmObj* %argslist57563$k483442, %struct.ScmObj** %stackaddr$prim58926, align 8
%clofunc58927 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48344)
musttail call tailcc void %clofunc58927(%struct.ScmObj* %k48344, %struct.ScmObj* %argslist57563$k483442)
ret void
}

define tailcc void @proc_clo$ae51854(%struct.ScmObj* %env$ae51854,%struct.ScmObj* %el4815348345) {
%stackaddr$prim58928 = alloca %struct.ScmObj*, align 8
%k48346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %el4815348345)
store volatile %struct.ScmObj* %k48346, %struct.ScmObj** %stackaddr$prim58928, align 8
%stackaddr$prim58929 = alloca %struct.ScmObj*, align 8
%el48153 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %el4815348345)
store volatile %struct.ScmObj* %el48153, %struct.ScmObj** %stackaddr$prim58929, align 8
%stackaddr$applyprim58930 = alloca %struct.ScmObj*, align 8
%cpsaprim48347 = call %struct.ScmObj* @applyprim_vector(%struct.ScmObj* %el48153)
store volatile %struct.ScmObj* %cpsaprim48347, %struct.ScmObj** %stackaddr$applyprim58930, align 8
%ae51859 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57565$k483460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58931 = alloca %struct.ScmObj*, align 8
%argslist57565$k483461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim48347, %struct.ScmObj* %argslist57565$k483460)
store volatile %struct.ScmObj* %argslist57565$k483461, %struct.ScmObj** %stackaddr$prim58931, align 8
%stackaddr$prim58932 = alloca %struct.ScmObj*, align 8
%argslist57565$k483462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51859, %struct.ScmObj* %argslist57565$k483461)
store volatile %struct.ScmObj* %argslist57565$k483462, %struct.ScmObj** %stackaddr$prim58932, align 8
%clofunc58933 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48346)
musttail call tailcc void %clofunc58933(%struct.ScmObj* %k48346, %struct.ScmObj* %argslist57565$k483462)
ret void
}

define tailcc void @proc_clo$ae51828(%struct.ScmObj* %env$ae51828,%struct.ScmObj* %current_45args57567) {
%stackaddr$prim58934 = alloca %struct.ScmObj*, align 8
%k48348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57567)
store volatile %struct.ScmObj* %k48348, %struct.ScmObj** %stackaddr$prim58934, align 8
%stackaddr$prim58935 = alloca %struct.ScmObj*, align 8
%current_45args57568 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57567)
store volatile %struct.ScmObj* %current_45args57568, %struct.ScmObj** %stackaddr$prim58935, align 8
%stackaddr$prim58936 = alloca %struct.ScmObj*, align 8
%x48091 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57568)
store volatile %struct.ScmObj* %x48091, %struct.ScmObj** %stackaddr$prim58936, align 8
%stackaddr$prim58937 = alloca %struct.ScmObj*, align 8
%anf_45bind48271 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48091)
store volatile %struct.ScmObj* %anf_45bind48271, %struct.ScmObj** %stackaddr$prim58937, align 8
%stackaddr$prim58938 = alloca %struct.ScmObj*, align 8
%anf_45bind48272 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48271)
store volatile %struct.ScmObj* %anf_45bind48272, %struct.ScmObj** %stackaddr$prim58938, align 8
%stackaddr$prim58939 = alloca %struct.ScmObj*, align 8
%anf_45bind48273 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48272)
store volatile %struct.ScmObj* %anf_45bind48273, %struct.ScmObj** %stackaddr$prim58939, align 8
%stackaddr$prim58940 = alloca %struct.ScmObj*, align 8
%cpsprim48349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48273)
store volatile %struct.ScmObj* %cpsprim48349, %struct.ScmObj** %stackaddr$prim58940, align 8
%ae51834 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57570$k483480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58941 = alloca %struct.ScmObj*, align 8
%argslist57570$k483481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48349, %struct.ScmObj* %argslist57570$k483480)
store volatile %struct.ScmObj* %argslist57570$k483481, %struct.ScmObj** %stackaddr$prim58941, align 8
%stackaddr$prim58942 = alloca %struct.ScmObj*, align 8
%argslist57570$k483482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51834, %struct.ScmObj* %argslist57570$k483481)
store volatile %struct.ScmObj* %argslist57570$k483482, %struct.ScmObj** %stackaddr$prim58942, align 8
%clofunc58943 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48348)
musttail call tailcc void %clofunc58943(%struct.ScmObj* %k48348, %struct.ScmObj* %argslist57570$k483482)
ret void
}

define tailcc void @proc_clo$ae51804(%struct.ScmObj* %env$ae51804,%struct.ScmObj* %current_45args57572) {
%stackaddr$prim58944 = alloca %struct.ScmObj*, align 8
%k48350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57572)
store volatile %struct.ScmObj* %k48350, %struct.ScmObj** %stackaddr$prim58944, align 8
%stackaddr$prim58945 = alloca %struct.ScmObj*, align 8
%current_45args57573 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57572)
store volatile %struct.ScmObj* %current_45args57573, %struct.ScmObj** %stackaddr$prim58945, align 8
%stackaddr$prim58946 = alloca %struct.ScmObj*, align 8
%x48093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57573)
store volatile %struct.ScmObj* %x48093, %struct.ScmObj** %stackaddr$prim58946, align 8
%stackaddr$prim58947 = alloca %struct.ScmObj*, align 8
%anf_45bind48269 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48093)
store volatile %struct.ScmObj* %anf_45bind48269, %struct.ScmObj** %stackaddr$prim58947, align 8
%stackaddr$prim58948 = alloca %struct.ScmObj*, align 8
%anf_45bind48270 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48269)
store volatile %struct.ScmObj* %anf_45bind48270, %struct.ScmObj** %stackaddr$prim58948, align 8
%stackaddr$prim58949 = alloca %struct.ScmObj*, align 8
%cpsprim48351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48270)
store volatile %struct.ScmObj* %cpsprim48351, %struct.ScmObj** %stackaddr$prim58949, align 8
%ae51809 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57575$k483500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58950 = alloca %struct.ScmObj*, align 8
%argslist57575$k483501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48351, %struct.ScmObj* %argslist57575$k483500)
store volatile %struct.ScmObj* %argslist57575$k483501, %struct.ScmObj** %stackaddr$prim58950, align 8
%stackaddr$prim58951 = alloca %struct.ScmObj*, align 8
%argslist57575$k483502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51809, %struct.ScmObj* %argslist57575$k483501)
store volatile %struct.ScmObj* %argslist57575$k483502, %struct.ScmObj** %stackaddr$prim58951, align 8
%clofunc58952 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48350)
musttail call tailcc void %clofunc58952(%struct.ScmObj* %k48350, %struct.ScmObj* %argslist57575$k483502)
ret void
}

define tailcc void @proc_clo$ae51782(%struct.ScmObj* %env$ae51782,%struct.ScmObj* %current_45args57577) {
%stackaddr$prim58953 = alloca %struct.ScmObj*, align 8
%k48352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57577)
store volatile %struct.ScmObj* %k48352, %struct.ScmObj** %stackaddr$prim58953, align 8
%stackaddr$prim58954 = alloca %struct.ScmObj*, align 8
%current_45args57578 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57577)
store volatile %struct.ScmObj* %current_45args57578, %struct.ScmObj** %stackaddr$prim58954, align 8
%stackaddr$prim58955 = alloca %struct.ScmObj*, align 8
%x48095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57578)
store volatile %struct.ScmObj* %x48095, %struct.ScmObj** %stackaddr$prim58955, align 8
%stackaddr$prim58956 = alloca %struct.ScmObj*, align 8
%anf_45bind48268 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48095)
store volatile %struct.ScmObj* %anf_45bind48268, %struct.ScmObj** %stackaddr$prim58956, align 8
%stackaddr$prim58957 = alloca %struct.ScmObj*, align 8
%cpsprim48353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48268)
store volatile %struct.ScmObj* %cpsprim48353, %struct.ScmObj** %stackaddr$prim58957, align 8
%ae51786 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57580$k483520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58958 = alloca %struct.ScmObj*, align 8
%argslist57580$k483521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48353, %struct.ScmObj* %argslist57580$k483520)
store volatile %struct.ScmObj* %argslist57580$k483521, %struct.ScmObj** %stackaddr$prim58958, align 8
%stackaddr$prim58959 = alloca %struct.ScmObj*, align 8
%argslist57580$k483522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51786, %struct.ScmObj* %argslist57580$k483521)
store volatile %struct.ScmObj* %argslist57580$k483522, %struct.ScmObj** %stackaddr$prim58959, align 8
%clofunc58960 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48352)
musttail call tailcc void %clofunc58960(%struct.ScmObj* %k48352, %struct.ScmObj* %argslist57580$k483522)
ret void
}

define tailcc void @proc_clo$ae51762(%struct.ScmObj* %env$ae51762,%struct.ScmObj* %current_45args57582) {
%stackaddr$prim58961 = alloca %struct.ScmObj*, align 8
%k48354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57582)
store volatile %struct.ScmObj* %k48354, %struct.ScmObj** %stackaddr$prim58961, align 8
%stackaddr$prim58962 = alloca %struct.ScmObj*, align 8
%current_45args57583 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57582)
store volatile %struct.ScmObj* %current_45args57583, %struct.ScmObj** %stackaddr$prim58962, align 8
%stackaddr$prim58963 = alloca %struct.ScmObj*, align 8
%x48097 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57583)
store volatile %struct.ScmObj* %x48097, %struct.ScmObj** %stackaddr$prim58963, align 8
%stackaddr$prim58964 = alloca %struct.ScmObj*, align 8
%cpsprim48355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48097)
store volatile %struct.ScmObj* %cpsprim48355, %struct.ScmObj** %stackaddr$prim58964, align 8
%ae51765 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57585$k483540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58965 = alloca %struct.ScmObj*, align 8
%argslist57585$k483541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48355, %struct.ScmObj* %argslist57585$k483540)
store volatile %struct.ScmObj* %argslist57585$k483541, %struct.ScmObj** %stackaddr$prim58965, align 8
%stackaddr$prim58966 = alloca %struct.ScmObj*, align 8
%argslist57585$k483542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51765, %struct.ScmObj* %argslist57585$k483541)
store volatile %struct.ScmObj* %argslist57585$k483542, %struct.ScmObj** %stackaddr$prim58966, align 8
%clofunc58967 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48354)
musttail call tailcc void %clofunc58967(%struct.ScmObj* %k48354, %struct.ScmObj* %argslist57585$k483542)
ret void
}

define tailcc void @proc_clo$ae51664(%struct.ScmObj* %env$ae51664,%struct.ScmObj* %args4809948356) {
%stackaddr$env-ref58968 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51664, i64 0)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref58968
%stackaddr$prim58969 = alloca %struct.ScmObj*, align 8
%k48357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4809948356)
store volatile %struct.ScmObj* %k48357, %struct.ScmObj** %stackaddr$prim58969, align 8
%stackaddr$prim58970 = alloca %struct.ScmObj*, align 8
%args48099 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4809948356)
store volatile %struct.ScmObj* %args48099, %struct.ScmObj** %stackaddr$prim58970, align 8
%stackaddr$prim58971 = alloca %struct.ScmObj*, align 8
%anf_45bind48262 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args48099)
store volatile %struct.ScmObj* %anf_45bind48262, %struct.ScmObj** %stackaddr$prim58971, align 8
%truthy$cmp58972 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48262)
%cmp$cmp58972 = icmp eq i64 %truthy$cmp58972, 1
br i1 %cmp$cmp58972, label %truebranch$cmp58972, label %falsebranch$cmp58972
truebranch$cmp58972:
%ae51670 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51671 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist57587$k483570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58973 = alloca %struct.ScmObj*, align 8
%argslist57587$k483571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51671, %struct.ScmObj* %argslist57587$k483570)
store volatile %struct.ScmObj* %argslist57587$k483571, %struct.ScmObj** %stackaddr$prim58973, align 8
%stackaddr$prim58974 = alloca %struct.ScmObj*, align 8
%argslist57587$k483572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51670, %struct.ScmObj* %argslist57587$k483571)
store volatile %struct.ScmObj* %argslist57587$k483572, %struct.ScmObj** %stackaddr$prim58974, align 8
%clofunc58975 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48357)
musttail call tailcc void %clofunc58975(%struct.ScmObj* %k48357, %struct.ScmObj* %argslist57587$k483572)
ret void
falsebranch$cmp58972:
%stackaddr$prim58976 = alloca %struct.ScmObj*, align 8
%anf_45bind48263 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48099)
store volatile %struct.ScmObj* %anf_45bind48263, %struct.ScmObj** %stackaddr$prim58976, align 8
%stackaddr$prim58977 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48263)
store volatile %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$prim58977, align 8
%truthy$cmp58978 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48264)
%cmp$cmp58978 = icmp eq i64 %truthy$cmp58978, 1
br i1 %cmp$cmp58978, label %truebranch$cmp58978, label %falsebranch$cmp58978
truebranch$cmp58978:
%stackaddr$prim58979 = alloca %struct.ScmObj*, align 8
%cpsprim48358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48099)
store volatile %struct.ScmObj* %cpsprim48358, %struct.ScmObj** %stackaddr$prim58979, align 8
%ae51683 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57588$k483570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58980 = alloca %struct.ScmObj*, align 8
%argslist57588$k483571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48358, %struct.ScmObj* %argslist57588$k483570)
store volatile %struct.ScmObj* %argslist57588$k483571, %struct.ScmObj** %stackaddr$prim58980, align 8
%stackaddr$prim58981 = alloca %struct.ScmObj*, align 8
%argslist57588$k483572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51683, %struct.ScmObj* %argslist57588$k483571)
store volatile %struct.ScmObj* %argslist57588$k483572, %struct.ScmObj** %stackaddr$prim58981, align 8
%clofunc58982 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48357)
musttail call tailcc void %clofunc58982(%struct.ScmObj* %k48357, %struct.ScmObj* %argslist57588$k483572)
ret void
falsebranch$cmp58978:
%stackaddr$makeclosure58983 = alloca %struct.ScmObj*, align 8
%fptrToInt58984 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51688 to i64
%ae51688 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt58984)
store volatile %struct.ScmObj* %ae51688, %struct.ScmObj** %stackaddr$makeclosure58983, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51688, %struct.ScmObj* %_37foldl148038, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51688, %struct.ScmObj* %k48357, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51688, %struct.ScmObj* %args48099, i64 2)
%ae51689 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure58985 = alloca %struct.ScmObj*, align 8
%fptrToInt58986 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51690 to i64
%ae51690 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt58986)
store volatile %struct.ScmObj* %ae51690, %struct.ScmObj** %stackaddr$makeclosure58985, align 8
%argslist57598$ae516880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58987 = alloca %struct.ScmObj*, align 8
%argslist57598$ae516881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51690, %struct.ScmObj* %argslist57598$ae516880)
store volatile %struct.ScmObj* %argslist57598$ae516881, %struct.ScmObj** %stackaddr$prim58987, align 8
%stackaddr$prim58988 = alloca %struct.ScmObj*, align 8
%argslist57598$ae516882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51689, %struct.ScmObj* %argslist57598$ae516881)
store volatile %struct.ScmObj* %argslist57598$ae516882, %struct.ScmObj** %stackaddr$prim58988, align 8
%clofunc58989 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51688)
musttail call tailcc void %clofunc58989(%struct.ScmObj* %ae51688, %struct.ScmObj* %argslist57598$ae516882)
ret void
}

define tailcc void @proc_clo$ae51688(%struct.ScmObj* %env$ae51688,%struct.ScmObj* %current_45args57589) {
%stackaddr$env-ref58990 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51688, i64 0)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref58990
%stackaddr$env-ref58991 = alloca %struct.ScmObj*, align 8
%k48357 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51688, i64 1)
store %struct.ScmObj* %k48357, %struct.ScmObj** %stackaddr$env-ref58991
%stackaddr$env-ref58992 = alloca %struct.ScmObj*, align 8
%args48099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51688, i64 2)
store %struct.ScmObj* %args48099, %struct.ScmObj** %stackaddr$env-ref58992
%stackaddr$prim58993 = alloca %struct.ScmObj*, align 8
%_95k48359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57589)
store volatile %struct.ScmObj* %_95k48359, %struct.ScmObj** %stackaddr$prim58993, align 8
%stackaddr$prim58994 = alloca %struct.ScmObj*, align 8
%current_45args57590 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57589)
store volatile %struct.ScmObj* %current_45args57590, %struct.ScmObj** %stackaddr$prim58994, align 8
%stackaddr$prim58995 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57590)
store volatile %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$prim58995, align 8
%stackaddr$prim58996 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48099)
store volatile %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$prim58996, align 8
%stackaddr$prim58997 = alloca %struct.ScmObj*, align 8
%anf_45bind48267 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48099)
store volatile %struct.ScmObj* %anf_45bind48267, %struct.ScmObj** %stackaddr$prim58997, align 8
%argslist57592$_37foldl1480380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim58998 = alloca %struct.ScmObj*, align 8
%argslist57592$_37foldl1480381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48267, %struct.ScmObj* %argslist57592$_37foldl1480380)
store volatile %struct.ScmObj* %argslist57592$_37foldl1480381, %struct.ScmObj** %stackaddr$prim58998, align 8
%stackaddr$prim58999 = alloca %struct.ScmObj*, align 8
%argslist57592$_37foldl1480382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48266, %struct.ScmObj* %argslist57592$_37foldl1480381)
store volatile %struct.ScmObj* %argslist57592$_37foldl1480382, %struct.ScmObj** %stackaddr$prim58999, align 8
%stackaddr$prim59000 = alloca %struct.ScmObj*, align 8
%argslist57592$_37foldl1480383 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48265, %struct.ScmObj* %argslist57592$_37foldl1480382)
store volatile %struct.ScmObj* %argslist57592$_37foldl1480383, %struct.ScmObj** %stackaddr$prim59000, align 8
%stackaddr$prim59001 = alloca %struct.ScmObj*, align 8
%argslist57592$_37foldl1480384 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48357, %struct.ScmObj* %argslist57592$_37foldl1480383)
store volatile %struct.ScmObj* %argslist57592$_37foldl1480384, %struct.ScmObj** %stackaddr$prim59001, align 8
%clofunc59002 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148038)
musttail call tailcc void %clofunc59002(%struct.ScmObj* %_37foldl148038, %struct.ScmObj* %argslist57592$_37foldl1480384)
ret void
}

define tailcc void @proc_clo$ae51690(%struct.ScmObj* %env$ae51690,%struct.ScmObj* %current_45args57593) {
%stackaddr$prim59003 = alloca %struct.ScmObj*, align 8
%k48360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57593)
store volatile %struct.ScmObj* %k48360, %struct.ScmObj** %stackaddr$prim59003, align 8
%stackaddr$prim59004 = alloca %struct.ScmObj*, align 8
%current_45args57594 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57593)
store volatile %struct.ScmObj* %current_45args57594, %struct.ScmObj** %stackaddr$prim59004, align 8
%stackaddr$prim59005 = alloca %struct.ScmObj*, align 8
%n48101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57594)
store volatile %struct.ScmObj* %n48101, %struct.ScmObj** %stackaddr$prim59005, align 8
%stackaddr$prim59006 = alloca %struct.ScmObj*, align 8
%current_45args57595 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57594)
store volatile %struct.ScmObj* %current_45args57595, %struct.ScmObj** %stackaddr$prim59006, align 8
%stackaddr$prim59007 = alloca %struct.ScmObj*, align 8
%v48100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57595)
store volatile %struct.ScmObj* %v48100, %struct.ScmObj** %stackaddr$prim59007, align 8
%stackaddr$prim59008 = alloca %struct.ScmObj*, align 8
%cpsprim48361 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v48100, %struct.ScmObj* %n48101)
store volatile %struct.ScmObj* %cpsprim48361, %struct.ScmObj** %stackaddr$prim59008, align 8
%ae51694 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57597$k483600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59009 = alloca %struct.ScmObj*, align 8
%argslist57597$k483601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48361, %struct.ScmObj* %argslist57597$k483600)
store volatile %struct.ScmObj* %argslist57597$k483601, %struct.ScmObj** %stackaddr$prim59009, align 8
%stackaddr$prim59010 = alloca %struct.ScmObj*, align 8
%argslist57597$k483602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51694, %struct.ScmObj* %argslist57597$k483601)
store volatile %struct.ScmObj* %argslist57597$k483602, %struct.ScmObj** %stackaddr$prim59010, align 8
%clofunc59011 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48360)
musttail call tailcc void %clofunc59011(%struct.ScmObj* %k48360, %struct.ScmObj* %argslist57597$k483602)
ret void
}

define tailcc void @proc_clo$ae51260(%struct.ScmObj* %env$ae51260,%struct.ScmObj* %current_45args57600) {
%stackaddr$prim59012 = alloca %struct.ScmObj*, align 8
%k48362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57600)
store volatile %struct.ScmObj* %k48362, %struct.ScmObj** %stackaddr$prim59012, align 8
%stackaddr$prim59013 = alloca %struct.ScmObj*, align 8
%current_45args57601 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57600)
store volatile %struct.ScmObj* %current_45args57601, %struct.ScmObj** %stackaddr$prim59013, align 8
%stackaddr$prim59014 = alloca %struct.ScmObj*, align 8
%v48104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57601)
store volatile %struct.ScmObj* %v48104, %struct.ScmObj** %stackaddr$prim59014, align 8
%stackaddr$prim59015 = alloca %struct.ScmObj*, align 8
%current_45args57602 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57601)
store volatile %struct.ScmObj* %current_45args57602, %struct.ScmObj** %stackaddr$prim59015, align 8
%stackaddr$prim59016 = alloca %struct.ScmObj*, align 8
%lst48103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57602)
store volatile %struct.ScmObj* %lst48103, %struct.ScmObj** %stackaddr$prim59016, align 8
%ae51261 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim59017 = alloca %struct.ScmObj*, align 8
%lst48105 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51261, %struct.ScmObj* %lst48103)
store volatile %struct.ScmObj* %lst48105, %struct.ScmObj** %stackaddr$prim59017, align 8
%stackaddr$makeclosure59018 = alloca %struct.ScmObj*, align 8
%fptrToInt59019 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51263 to i64
%ae51263 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt59019)
store volatile %struct.ScmObj* %ae51263, %struct.ScmObj** %stackaddr$makeclosure59018, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51263, %struct.ScmObj* %lst48105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51263, %struct.ScmObj* %v48104, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51263, %struct.ScmObj* %k48362, i64 2)
%ae51264 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59020 = alloca %struct.ScmObj*, align 8
%fptrToInt59021 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51265 to i64
%ae51265 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt59021)
store volatile %struct.ScmObj* %ae51265, %struct.ScmObj** %stackaddr$makeclosure59020, align 8
%argslist57624$ae512630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59022 = alloca %struct.ScmObj*, align 8
%argslist57624$ae512631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51265, %struct.ScmObj* %argslist57624$ae512630)
store volatile %struct.ScmObj* %argslist57624$ae512631, %struct.ScmObj** %stackaddr$prim59022, align 8
%stackaddr$prim59023 = alloca %struct.ScmObj*, align 8
%argslist57624$ae512632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51264, %struct.ScmObj* %argslist57624$ae512631)
store volatile %struct.ScmObj* %argslist57624$ae512632, %struct.ScmObj** %stackaddr$prim59023, align 8
%clofunc59024 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51263)
musttail call tailcc void %clofunc59024(%struct.ScmObj* %ae51263, %struct.ScmObj* %argslist57624$ae512632)
ret void
}

define tailcc void @proc_clo$ae51263(%struct.ScmObj* %env$ae51263,%struct.ScmObj* %current_45args57604) {
%stackaddr$env-ref59025 = alloca %struct.ScmObj*, align 8
%lst48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51263, i64 0)
store %struct.ScmObj* %lst48105, %struct.ScmObj** %stackaddr$env-ref59025
%stackaddr$env-ref59026 = alloca %struct.ScmObj*, align 8
%v48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51263, i64 1)
store %struct.ScmObj* %v48104, %struct.ScmObj** %stackaddr$env-ref59026
%stackaddr$env-ref59027 = alloca %struct.ScmObj*, align 8
%k48362 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51263, i64 2)
store %struct.ScmObj* %k48362, %struct.ScmObj** %stackaddr$env-ref59027
%stackaddr$prim59028 = alloca %struct.ScmObj*, align 8
%_95k48363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57604)
store volatile %struct.ScmObj* %_95k48363, %struct.ScmObj** %stackaddr$prim59028, align 8
%stackaddr$prim59029 = alloca %struct.ScmObj*, align 8
%current_45args57605 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57604)
store volatile %struct.ScmObj* %current_45args57605, %struct.ScmObj** %stackaddr$prim59029, align 8
%stackaddr$prim59030 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57605)
store volatile %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$prim59030, align 8
%stackaddr$makeclosure59031 = alloca %struct.ScmObj*, align 8
%fptrToInt59032 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51279 to i64
%ae51279 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt59032)
store volatile %struct.ScmObj* %ae51279, %struct.ScmObj** %stackaddr$makeclosure59031, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51279, %struct.ScmObj* %lst48105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51279, %struct.ScmObj* %v48104, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51279, %struct.ScmObj* %k48362, i64 2)
%stackaddr$makeclosure59033 = alloca %struct.ScmObj*, align 8
%fptrToInt59034 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51280 to i64
%ae51280 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt59034)
store volatile %struct.ScmObj* %ae51280, %struct.ScmObj** %stackaddr$makeclosure59033, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51280, %struct.ScmObj* %lst48105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51280, %struct.ScmObj* %v48104, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51280, %struct.ScmObj* %k48362, i64 2)
%argslist57619$anf_45bind482540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59035 = alloca %struct.ScmObj*, align 8
%argslist57619$anf_45bind482541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51280, %struct.ScmObj* %argslist57619$anf_45bind482540)
store volatile %struct.ScmObj* %argslist57619$anf_45bind482541, %struct.ScmObj** %stackaddr$prim59035, align 8
%stackaddr$prim59036 = alloca %struct.ScmObj*, align 8
%argslist57619$anf_45bind482542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51279, %struct.ScmObj* %argslist57619$anf_45bind482541)
store volatile %struct.ScmObj* %argslist57619$anf_45bind482542, %struct.ScmObj** %stackaddr$prim59036, align 8
%clofunc59037 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48254)
musttail call tailcc void %clofunc59037(%struct.ScmObj* %anf_45bind48254, %struct.ScmObj* %argslist57619$anf_45bind482542)
ret void
}

define tailcc void @proc_clo$ae51279(%struct.ScmObj* %env$ae51279,%struct.ScmObj* %current_45args57607) {
%stackaddr$env-ref59038 = alloca %struct.ScmObj*, align 8
%lst48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51279, i64 0)
store %struct.ScmObj* %lst48105, %struct.ScmObj** %stackaddr$env-ref59038
%stackaddr$env-ref59039 = alloca %struct.ScmObj*, align 8
%v48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51279, i64 1)
store %struct.ScmObj* %v48104, %struct.ScmObj** %stackaddr$env-ref59039
%stackaddr$env-ref59040 = alloca %struct.ScmObj*, align 8
%k48362 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51279, i64 2)
store %struct.ScmObj* %k48362, %struct.ScmObj** %stackaddr$env-ref59040
%stackaddr$prim59041 = alloca %struct.ScmObj*, align 8
%_95k48364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57607)
store volatile %struct.ScmObj* %_95k48364, %struct.ScmObj** %stackaddr$prim59041, align 8
%stackaddr$prim59042 = alloca %struct.ScmObj*, align 8
%current_45args57608 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57607)
store volatile %struct.ScmObj* %current_45args57608, %struct.ScmObj** %stackaddr$prim59042, align 8
%stackaddr$prim59043 = alloca %struct.ScmObj*, align 8
%cc48106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57608)
store volatile %struct.ScmObj* %cc48106, %struct.ScmObj** %stackaddr$prim59043, align 8
%ae51388 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59044 = alloca %struct.ScmObj*, align 8
%anf_45bind48255 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae51388)
store volatile %struct.ScmObj* %anf_45bind48255, %struct.ScmObj** %stackaddr$prim59044, align 8
%stackaddr$prim59045 = alloca %struct.ScmObj*, align 8
%anf_45bind48256 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48255)
store volatile %struct.ScmObj* %anf_45bind48256, %struct.ScmObj** %stackaddr$prim59045, align 8
%truthy$cmp59046 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48256)
%cmp$cmp59046 = icmp eq i64 %truthy$cmp59046, 1
br i1 %cmp$cmp59046, label %truebranch$cmp59046, label %falsebranch$cmp59046
truebranch$cmp59046:
%ae51392 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51393 = call %struct.ScmObj* @const_init_false()
%argslist57610$k483620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59047 = alloca %struct.ScmObj*, align 8
%argslist57610$k483621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51393, %struct.ScmObj* %argslist57610$k483620)
store volatile %struct.ScmObj* %argslist57610$k483621, %struct.ScmObj** %stackaddr$prim59047, align 8
%stackaddr$prim59048 = alloca %struct.ScmObj*, align 8
%argslist57610$k483622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51392, %struct.ScmObj* %argslist57610$k483621)
store volatile %struct.ScmObj* %argslist57610$k483622, %struct.ScmObj** %stackaddr$prim59048, align 8
%clofunc59049 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48362)
musttail call tailcc void %clofunc59049(%struct.ScmObj* %k48362, %struct.ScmObj* %argslist57610$k483622)
ret void
falsebranch$cmp59046:
%ae51401 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59050 = alloca %struct.ScmObj*, align 8
%anf_45bind48257 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae51401)
store volatile %struct.ScmObj* %anf_45bind48257, %struct.ScmObj** %stackaddr$prim59050, align 8
%stackaddr$prim59051 = alloca %struct.ScmObj*, align 8
%anf_45bind48258 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48257)
store volatile %struct.ScmObj* %anf_45bind48258, %struct.ScmObj** %stackaddr$prim59051, align 8
%stackaddr$prim59052 = alloca %struct.ScmObj*, align 8
%anf_45bind48259 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48258, %struct.ScmObj* %v48104)
store volatile %struct.ScmObj* %anf_45bind48259, %struct.ScmObj** %stackaddr$prim59052, align 8
%truthy$cmp59053 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48259)
%cmp$cmp59053 = icmp eq i64 %truthy$cmp59053, 1
br i1 %cmp$cmp59053, label %truebranch$cmp59053, label %falsebranch$cmp59053
truebranch$cmp59053:
%ae51407 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59054 = alloca %struct.ScmObj*, align 8
%cpsprim48365 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae51407)
store volatile %struct.ScmObj* %cpsprim48365, %struct.ScmObj** %stackaddr$prim59054, align 8
%ae51409 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57611$k483620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59055 = alloca %struct.ScmObj*, align 8
%argslist57611$k483621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48365, %struct.ScmObj* %argslist57611$k483620)
store volatile %struct.ScmObj* %argslist57611$k483621, %struct.ScmObj** %stackaddr$prim59055, align 8
%stackaddr$prim59056 = alloca %struct.ScmObj*, align 8
%argslist57611$k483622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51409, %struct.ScmObj* %argslist57611$k483621)
store volatile %struct.ScmObj* %argslist57611$k483622, %struct.ScmObj** %stackaddr$prim59056, align 8
%clofunc59057 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48362)
musttail call tailcc void %clofunc59057(%struct.ScmObj* %k48362, %struct.ScmObj* %argslist57611$k483622)
ret void
falsebranch$cmp59053:
%ae51420 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59058 = alloca %struct.ScmObj*, align 8
%anf_45bind48260 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae51420)
store volatile %struct.ScmObj* %anf_45bind48260, %struct.ScmObj** %stackaddr$prim59058, align 8
%stackaddr$prim59059 = alloca %struct.ScmObj*, align 8
%anf_45bind48261 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48260)
store volatile %struct.ScmObj* %anf_45bind48261, %struct.ScmObj** %stackaddr$prim59059, align 8
%ae51423 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59060 = alloca %struct.ScmObj*, align 8
%_95048108 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae51423, %struct.ScmObj* %anf_45bind48261)
store volatile %struct.ScmObj* %_95048108, %struct.ScmObj** %stackaddr$prim59060, align 8
%argslist57612$cc481060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59061 = alloca %struct.ScmObj*, align 8
%argslist57612$cc481061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist57612$cc481060)
store volatile %struct.ScmObj* %argslist57612$cc481061, %struct.ScmObj** %stackaddr$prim59061, align 8
%stackaddr$prim59062 = alloca %struct.ScmObj*, align 8
%argslist57612$cc481062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48362, %struct.ScmObj* %argslist57612$cc481061)
store volatile %struct.ScmObj* %argslist57612$cc481062, %struct.ScmObj** %stackaddr$prim59062, align 8
%clofunc59063 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48106)
musttail call tailcc void %clofunc59063(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist57612$cc481062)
ret void
}

define tailcc void @proc_clo$ae51280(%struct.ScmObj* %env$ae51280,%struct.ScmObj* %current_45args57613) {
%stackaddr$env-ref59064 = alloca %struct.ScmObj*, align 8
%lst48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51280, i64 0)
store %struct.ScmObj* %lst48105, %struct.ScmObj** %stackaddr$env-ref59064
%stackaddr$env-ref59065 = alloca %struct.ScmObj*, align 8
%v48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51280, i64 1)
store %struct.ScmObj* %v48104, %struct.ScmObj** %stackaddr$env-ref59065
%stackaddr$env-ref59066 = alloca %struct.ScmObj*, align 8
%k48362 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51280, i64 2)
store %struct.ScmObj* %k48362, %struct.ScmObj** %stackaddr$env-ref59066
%stackaddr$prim59067 = alloca %struct.ScmObj*, align 8
%_95k48364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57613)
store volatile %struct.ScmObj* %_95k48364, %struct.ScmObj** %stackaddr$prim59067, align 8
%stackaddr$prim59068 = alloca %struct.ScmObj*, align 8
%current_45args57614 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57613)
store volatile %struct.ScmObj* %current_45args57614, %struct.ScmObj** %stackaddr$prim59068, align 8
%stackaddr$prim59069 = alloca %struct.ScmObj*, align 8
%cc48106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57614)
store volatile %struct.ScmObj* %cc48106, %struct.ScmObj** %stackaddr$prim59069, align 8
%ae51282 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59070 = alloca %struct.ScmObj*, align 8
%anf_45bind48255 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae51282)
store volatile %struct.ScmObj* %anf_45bind48255, %struct.ScmObj** %stackaddr$prim59070, align 8
%stackaddr$prim59071 = alloca %struct.ScmObj*, align 8
%anf_45bind48256 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48255)
store volatile %struct.ScmObj* %anf_45bind48256, %struct.ScmObj** %stackaddr$prim59071, align 8
%truthy$cmp59072 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48256)
%cmp$cmp59072 = icmp eq i64 %truthy$cmp59072, 1
br i1 %cmp$cmp59072, label %truebranch$cmp59072, label %falsebranch$cmp59072
truebranch$cmp59072:
%ae51286 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51287 = call %struct.ScmObj* @const_init_false()
%argslist57616$k483620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59073 = alloca %struct.ScmObj*, align 8
%argslist57616$k483621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51287, %struct.ScmObj* %argslist57616$k483620)
store volatile %struct.ScmObj* %argslist57616$k483621, %struct.ScmObj** %stackaddr$prim59073, align 8
%stackaddr$prim59074 = alloca %struct.ScmObj*, align 8
%argslist57616$k483622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51286, %struct.ScmObj* %argslist57616$k483621)
store volatile %struct.ScmObj* %argslist57616$k483622, %struct.ScmObj** %stackaddr$prim59074, align 8
%clofunc59075 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48362)
musttail call tailcc void %clofunc59075(%struct.ScmObj* %k48362, %struct.ScmObj* %argslist57616$k483622)
ret void
falsebranch$cmp59072:
%ae51295 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59076 = alloca %struct.ScmObj*, align 8
%anf_45bind48257 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae51295)
store volatile %struct.ScmObj* %anf_45bind48257, %struct.ScmObj** %stackaddr$prim59076, align 8
%stackaddr$prim59077 = alloca %struct.ScmObj*, align 8
%anf_45bind48258 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48257)
store volatile %struct.ScmObj* %anf_45bind48258, %struct.ScmObj** %stackaddr$prim59077, align 8
%stackaddr$prim59078 = alloca %struct.ScmObj*, align 8
%anf_45bind48259 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48258, %struct.ScmObj* %v48104)
store volatile %struct.ScmObj* %anf_45bind48259, %struct.ScmObj** %stackaddr$prim59078, align 8
%truthy$cmp59079 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48259)
%cmp$cmp59079 = icmp eq i64 %truthy$cmp59079, 1
br i1 %cmp$cmp59079, label %truebranch$cmp59079, label %falsebranch$cmp59079
truebranch$cmp59079:
%ae51301 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59080 = alloca %struct.ScmObj*, align 8
%cpsprim48365 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae51301)
store volatile %struct.ScmObj* %cpsprim48365, %struct.ScmObj** %stackaddr$prim59080, align 8
%ae51303 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57617$k483620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59081 = alloca %struct.ScmObj*, align 8
%argslist57617$k483621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48365, %struct.ScmObj* %argslist57617$k483620)
store volatile %struct.ScmObj* %argslist57617$k483621, %struct.ScmObj** %stackaddr$prim59081, align 8
%stackaddr$prim59082 = alloca %struct.ScmObj*, align 8
%argslist57617$k483622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51303, %struct.ScmObj* %argslist57617$k483621)
store volatile %struct.ScmObj* %argslist57617$k483622, %struct.ScmObj** %stackaddr$prim59082, align 8
%clofunc59083 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48362)
musttail call tailcc void %clofunc59083(%struct.ScmObj* %k48362, %struct.ScmObj* %argslist57617$k483622)
ret void
falsebranch$cmp59079:
%ae51314 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59084 = alloca %struct.ScmObj*, align 8
%anf_45bind48260 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae51314)
store volatile %struct.ScmObj* %anf_45bind48260, %struct.ScmObj** %stackaddr$prim59084, align 8
%stackaddr$prim59085 = alloca %struct.ScmObj*, align 8
%anf_45bind48261 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48260)
store volatile %struct.ScmObj* %anf_45bind48261, %struct.ScmObj** %stackaddr$prim59085, align 8
%ae51317 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59086 = alloca %struct.ScmObj*, align 8
%_95048108 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae51317, %struct.ScmObj* %anf_45bind48261)
store volatile %struct.ScmObj* %_95048108, %struct.ScmObj** %stackaddr$prim59086, align 8
%argslist57618$cc481060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59087 = alloca %struct.ScmObj*, align 8
%argslist57618$cc481061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist57618$cc481060)
store volatile %struct.ScmObj* %argslist57618$cc481061, %struct.ScmObj** %stackaddr$prim59087, align 8
%stackaddr$prim59088 = alloca %struct.ScmObj*, align 8
%argslist57618$cc481062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48362, %struct.ScmObj* %argslist57618$cc481061)
store volatile %struct.ScmObj* %argslist57618$cc481062, %struct.ScmObj** %stackaddr$prim59088, align 8
%clofunc59089 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48106)
musttail call tailcc void %clofunc59089(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist57618$cc481062)
ret void
}

define tailcc void @proc_clo$ae51265(%struct.ScmObj* %env$ae51265,%struct.ScmObj* %current_45args57620) {
%stackaddr$prim59090 = alloca %struct.ScmObj*, align 8
%k48366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57620)
store volatile %struct.ScmObj* %k48366, %struct.ScmObj** %stackaddr$prim59090, align 8
%stackaddr$prim59091 = alloca %struct.ScmObj*, align 8
%current_45args57621 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57620)
store volatile %struct.ScmObj* %current_45args57621, %struct.ScmObj** %stackaddr$prim59091, align 8
%stackaddr$prim59092 = alloca %struct.ScmObj*, align 8
%u48107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57621)
store volatile %struct.ScmObj* %u48107, %struct.ScmObj** %stackaddr$prim59092, align 8
%argslist57623$u481070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59093 = alloca %struct.ScmObj*, align 8
%argslist57623$u481071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48107, %struct.ScmObj* %argslist57623$u481070)
store volatile %struct.ScmObj* %argslist57623$u481071, %struct.ScmObj** %stackaddr$prim59093, align 8
%stackaddr$prim59094 = alloca %struct.ScmObj*, align 8
%argslist57623$u481072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48366, %struct.ScmObj* %argslist57623$u481071)
store volatile %struct.ScmObj* %argslist57623$u481072, %struct.ScmObj** %stackaddr$prim59094, align 8
%clofunc59095 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48107)
musttail call tailcc void %clofunc59095(%struct.ScmObj* %u48107, %struct.ScmObj* %argslist57623$u481072)
ret void
}

define tailcc void @proc_clo$ae50724(%struct.ScmObj* %env$ae50724,%struct.ScmObj* %current_45args57626) {
%stackaddr$prim59096 = alloca %struct.ScmObj*, align 8
%k48367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57626)
store volatile %struct.ScmObj* %k48367, %struct.ScmObj** %stackaddr$prim59096, align 8
%stackaddr$prim59097 = alloca %struct.ScmObj*, align 8
%current_45args57627 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57626)
store volatile %struct.ScmObj* %current_45args57627, %struct.ScmObj** %stackaddr$prim59097, align 8
%stackaddr$prim59098 = alloca %struct.ScmObj*, align 8
%lst48111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57627)
store volatile %struct.ScmObj* %lst48111, %struct.ScmObj** %stackaddr$prim59098, align 8
%stackaddr$prim59099 = alloca %struct.ScmObj*, align 8
%current_45args57628 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57627)
store volatile %struct.ScmObj* %current_45args57628, %struct.ScmObj** %stackaddr$prim59099, align 8
%stackaddr$prim59100 = alloca %struct.ScmObj*, align 8
%n48110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57628)
store volatile %struct.ScmObj* %n48110, %struct.ScmObj** %stackaddr$prim59100, align 8
%ae50725 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim59101 = alloca %struct.ScmObj*, align 8
%n48113 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50725, %struct.ScmObj* %n48110)
store volatile %struct.ScmObj* %n48113, %struct.ScmObj** %stackaddr$prim59101, align 8
%ae50727 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim59102 = alloca %struct.ScmObj*, align 8
%lst48112 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50727, %struct.ScmObj* %lst48111)
store volatile %struct.ScmObj* %lst48112, %struct.ScmObj** %stackaddr$prim59102, align 8
%stackaddr$makeclosure59103 = alloca %struct.ScmObj*, align 8
%fptrToInt59104 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50729 to i64
%ae50729 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt59104)
store volatile %struct.ScmObj* %ae50729, %struct.ScmObj** %stackaddr$makeclosure59103, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50729, %struct.ScmObj* %n48113, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50729, %struct.ScmObj* %lst48112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50729, %struct.ScmObj* %k48367, i64 2)
%ae50730 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59105 = alloca %struct.ScmObj*, align 8
%fptrToInt59106 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50731 to i64
%ae50731 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt59106)
store volatile %struct.ScmObj* %ae50731, %struct.ScmObj** %stackaddr$makeclosure59105, align 8
%argslist57648$ae507290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59107 = alloca %struct.ScmObj*, align 8
%argslist57648$ae507291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50731, %struct.ScmObj* %argslist57648$ae507290)
store volatile %struct.ScmObj* %argslist57648$ae507291, %struct.ScmObj** %stackaddr$prim59107, align 8
%stackaddr$prim59108 = alloca %struct.ScmObj*, align 8
%argslist57648$ae507292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50730, %struct.ScmObj* %argslist57648$ae507291)
store volatile %struct.ScmObj* %argslist57648$ae507292, %struct.ScmObj** %stackaddr$prim59108, align 8
%clofunc59109 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50729)
musttail call tailcc void %clofunc59109(%struct.ScmObj* %ae50729, %struct.ScmObj* %argslist57648$ae507292)
ret void
}

define tailcc void @proc_clo$ae50729(%struct.ScmObj* %env$ae50729,%struct.ScmObj* %current_45args57630) {
%stackaddr$env-ref59110 = alloca %struct.ScmObj*, align 8
%n48113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50729, i64 0)
store %struct.ScmObj* %n48113, %struct.ScmObj** %stackaddr$env-ref59110
%stackaddr$env-ref59111 = alloca %struct.ScmObj*, align 8
%lst48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50729, i64 1)
store %struct.ScmObj* %lst48112, %struct.ScmObj** %stackaddr$env-ref59111
%stackaddr$env-ref59112 = alloca %struct.ScmObj*, align 8
%k48367 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50729, i64 2)
store %struct.ScmObj* %k48367, %struct.ScmObj** %stackaddr$env-ref59112
%stackaddr$prim59113 = alloca %struct.ScmObj*, align 8
%_95k48368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57630)
store volatile %struct.ScmObj* %_95k48368, %struct.ScmObj** %stackaddr$prim59113, align 8
%stackaddr$prim59114 = alloca %struct.ScmObj*, align 8
%current_45args57631 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57630)
store volatile %struct.ScmObj* %current_45args57631, %struct.ScmObj** %stackaddr$prim59114, align 8
%stackaddr$prim59115 = alloca %struct.ScmObj*, align 8
%anf_45bind48247 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57631)
store volatile %struct.ScmObj* %anf_45bind48247, %struct.ScmObj** %stackaddr$prim59115, align 8
%stackaddr$makeclosure59116 = alloca %struct.ScmObj*, align 8
%fptrToInt59117 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50745 to i64
%ae50745 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt59117)
store volatile %struct.ScmObj* %ae50745, %struct.ScmObj** %stackaddr$makeclosure59116, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50745, %struct.ScmObj* %n48113, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50745, %struct.ScmObj* %lst48112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50745, %struct.ScmObj* %k48367, i64 2)
%stackaddr$makeclosure59118 = alloca %struct.ScmObj*, align 8
%fptrToInt59119 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50746 to i64
%ae50746 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt59119)
store volatile %struct.ScmObj* %ae50746, %struct.ScmObj** %stackaddr$makeclosure59118, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50746, %struct.ScmObj* %n48113, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50746, %struct.ScmObj* %lst48112, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50746, %struct.ScmObj* %k48367, i64 2)
%argslist57643$anf_45bind482470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59120 = alloca %struct.ScmObj*, align 8
%argslist57643$anf_45bind482471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50746, %struct.ScmObj* %argslist57643$anf_45bind482470)
store volatile %struct.ScmObj* %argslist57643$anf_45bind482471, %struct.ScmObj** %stackaddr$prim59120, align 8
%stackaddr$prim59121 = alloca %struct.ScmObj*, align 8
%argslist57643$anf_45bind482472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50745, %struct.ScmObj* %argslist57643$anf_45bind482471)
store volatile %struct.ScmObj* %argslist57643$anf_45bind482472, %struct.ScmObj** %stackaddr$prim59121, align 8
%clofunc59122 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48247)
musttail call tailcc void %clofunc59122(%struct.ScmObj* %anf_45bind48247, %struct.ScmObj* %argslist57643$anf_45bind482472)
ret void
}

define tailcc void @proc_clo$ae50745(%struct.ScmObj* %env$ae50745,%struct.ScmObj* %current_45args57633) {
%stackaddr$env-ref59123 = alloca %struct.ScmObj*, align 8
%n48113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50745, i64 0)
store %struct.ScmObj* %n48113, %struct.ScmObj** %stackaddr$env-ref59123
%stackaddr$env-ref59124 = alloca %struct.ScmObj*, align 8
%lst48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50745, i64 1)
store %struct.ScmObj* %lst48112, %struct.ScmObj** %stackaddr$env-ref59124
%stackaddr$env-ref59125 = alloca %struct.ScmObj*, align 8
%k48367 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50745, i64 2)
store %struct.ScmObj* %k48367, %struct.ScmObj** %stackaddr$env-ref59125
%stackaddr$prim59126 = alloca %struct.ScmObj*, align 8
%_95k48369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57633)
store volatile %struct.ScmObj* %_95k48369, %struct.ScmObj** %stackaddr$prim59126, align 8
%stackaddr$prim59127 = alloca %struct.ScmObj*, align 8
%current_45args57634 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57633)
store volatile %struct.ScmObj* %current_45args57634, %struct.ScmObj** %stackaddr$prim59127, align 8
%stackaddr$prim59128 = alloca %struct.ScmObj*, align 8
%cc48114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57634)
store volatile %struct.ScmObj* %cc48114, %struct.ScmObj** %stackaddr$prim59128, align 8
%ae50888 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59129 = alloca %struct.ScmObj*, align 8
%anf_45bind48248 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48113, %struct.ScmObj* %ae50888)
store volatile %struct.ScmObj* %anf_45bind48248, %struct.ScmObj** %stackaddr$prim59129, align 8
%ae50889 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59130 = alloca %struct.ScmObj*, align 8
%anf_45bind48249 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50889, %struct.ScmObj* %anf_45bind48248)
store volatile %struct.ScmObj* %anf_45bind48249, %struct.ScmObj** %stackaddr$prim59130, align 8
%truthy$cmp59131 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48249)
%cmp$cmp59131 = icmp eq i64 %truthy$cmp59131, 1
br i1 %cmp$cmp59131, label %truebranch$cmp59131, label %falsebranch$cmp59131
truebranch$cmp59131:
%ae50893 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59132 = alloca %struct.ScmObj*, align 8
%cpsprim48370 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48112, %struct.ScmObj* %ae50893)
store volatile %struct.ScmObj* %cpsprim48370, %struct.ScmObj** %stackaddr$prim59132, align 8
%ae50895 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57636$k483670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59133 = alloca %struct.ScmObj*, align 8
%argslist57636$k483671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48370, %struct.ScmObj* %argslist57636$k483670)
store volatile %struct.ScmObj* %argslist57636$k483671, %struct.ScmObj** %stackaddr$prim59133, align 8
%stackaddr$prim59134 = alloca %struct.ScmObj*, align 8
%argslist57636$k483672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50895, %struct.ScmObj* %argslist57636$k483671)
store volatile %struct.ScmObj* %argslist57636$k483672, %struct.ScmObj** %stackaddr$prim59134, align 8
%clofunc59135 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48367)
musttail call tailcc void %clofunc59135(%struct.ScmObj* %k48367, %struct.ScmObj* %argslist57636$k483672)
ret void
falsebranch$cmp59131:
%ae50906 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59136 = alloca %struct.ScmObj*, align 8
%anf_45bind48250 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48112, %struct.ScmObj* %ae50906)
store volatile %struct.ScmObj* %anf_45bind48250, %struct.ScmObj** %stackaddr$prim59136, align 8
%stackaddr$prim59137 = alloca %struct.ScmObj*, align 8
%anf_45bind48251 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48250)
store volatile %struct.ScmObj* %anf_45bind48251, %struct.ScmObj** %stackaddr$prim59137, align 8
%ae50909 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59138 = alloca %struct.ScmObj*, align 8
%_95048117 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48112, %struct.ScmObj* %ae50909, %struct.ScmObj* %anf_45bind48251)
store volatile %struct.ScmObj* %_95048117, %struct.ScmObj** %stackaddr$prim59138, align 8
%ae50912 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59139 = alloca %struct.ScmObj*, align 8
%anf_45bind48252 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48113, %struct.ScmObj* %ae50912)
store volatile %struct.ScmObj* %anf_45bind48252, %struct.ScmObj** %stackaddr$prim59139, align 8
%ae50914 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim59140 = alloca %struct.ScmObj*, align 8
%anf_45bind48253 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48252, %struct.ScmObj* %ae50914)
store volatile %struct.ScmObj* %anf_45bind48253, %struct.ScmObj** %stackaddr$prim59140, align 8
%ae50916 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59141 = alloca %struct.ScmObj*, align 8
%_95148116 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48113, %struct.ScmObj* %ae50916, %struct.ScmObj* %anf_45bind48253)
store volatile %struct.ScmObj* %_95148116, %struct.ScmObj** %stackaddr$prim59141, align 8
%argslist57637$cc481140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59142 = alloca %struct.ScmObj*, align 8
%argslist57637$cc481141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48114, %struct.ScmObj* %argslist57637$cc481140)
store volatile %struct.ScmObj* %argslist57637$cc481141, %struct.ScmObj** %stackaddr$prim59142, align 8
%stackaddr$prim59143 = alloca %struct.ScmObj*, align 8
%argslist57637$cc481142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48367, %struct.ScmObj* %argslist57637$cc481141)
store volatile %struct.ScmObj* %argslist57637$cc481142, %struct.ScmObj** %stackaddr$prim59143, align 8
%clofunc59144 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48114)
musttail call tailcc void %clofunc59144(%struct.ScmObj* %cc48114, %struct.ScmObj* %argslist57637$cc481142)
ret void
}

define tailcc void @proc_clo$ae50746(%struct.ScmObj* %env$ae50746,%struct.ScmObj* %current_45args57638) {
%stackaddr$env-ref59145 = alloca %struct.ScmObj*, align 8
%n48113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50746, i64 0)
store %struct.ScmObj* %n48113, %struct.ScmObj** %stackaddr$env-ref59145
%stackaddr$env-ref59146 = alloca %struct.ScmObj*, align 8
%lst48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50746, i64 1)
store %struct.ScmObj* %lst48112, %struct.ScmObj** %stackaddr$env-ref59146
%stackaddr$env-ref59147 = alloca %struct.ScmObj*, align 8
%k48367 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50746, i64 2)
store %struct.ScmObj* %k48367, %struct.ScmObj** %stackaddr$env-ref59147
%stackaddr$prim59148 = alloca %struct.ScmObj*, align 8
%_95k48369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57638)
store volatile %struct.ScmObj* %_95k48369, %struct.ScmObj** %stackaddr$prim59148, align 8
%stackaddr$prim59149 = alloca %struct.ScmObj*, align 8
%current_45args57639 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57638)
store volatile %struct.ScmObj* %current_45args57639, %struct.ScmObj** %stackaddr$prim59149, align 8
%stackaddr$prim59150 = alloca %struct.ScmObj*, align 8
%cc48114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57639)
store volatile %struct.ScmObj* %cc48114, %struct.ScmObj** %stackaddr$prim59150, align 8
%ae50748 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59151 = alloca %struct.ScmObj*, align 8
%anf_45bind48248 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48113, %struct.ScmObj* %ae50748)
store volatile %struct.ScmObj* %anf_45bind48248, %struct.ScmObj** %stackaddr$prim59151, align 8
%ae50749 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59152 = alloca %struct.ScmObj*, align 8
%anf_45bind48249 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50749, %struct.ScmObj* %anf_45bind48248)
store volatile %struct.ScmObj* %anf_45bind48249, %struct.ScmObj** %stackaddr$prim59152, align 8
%truthy$cmp59153 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48249)
%cmp$cmp59153 = icmp eq i64 %truthy$cmp59153, 1
br i1 %cmp$cmp59153, label %truebranch$cmp59153, label %falsebranch$cmp59153
truebranch$cmp59153:
%ae50753 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59154 = alloca %struct.ScmObj*, align 8
%cpsprim48370 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48112, %struct.ScmObj* %ae50753)
store volatile %struct.ScmObj* %cpsprim48370, %struct.ScmObj** %stackaddr$prim59154, align 8
%ae50755 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57641$k483670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59155 = alloca %struct.ScmObj*, align 8
%argslist57641$k483671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48370, %struct.ScmObj* %argslist57641$k483670)
store volatile %struct.ScmObj* %argslist57641$k483671, %struct.ScmObj** %stackaddr$prim59155, align 8
%stackaddr$prim59156 = alloca %struct.ScmObj*, align 8
%argslist57641$k483672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50755, %struct.ScmObj* %argslist57641$k483671)
store volatile %struct.ScmObj* %argslist57641$k483672, %struct.ScmObj** %stackaddr$prim59156, align 8
%clofunc59157 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48367)
musttail call tailcc void %clofunc59157(%struct.ScmObj* %k48367, %struct.ScmObj* %argslist57641$k483672)
ret void
falsebranch$cmp59153:
%ae50766 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59158 = alloca %struct.ScmObj*, align 8
%anf_45bind48250 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48112, %struct.ScmObj* %ae50766)
store volatile %struct.ScmObj* %anf_45bind48250, %struct.ScmObj** %stackaddr$prim59158, align 8
%stackaddr$prim59159 = alloca %struct.ScmObj*, align 8
%anf_45bind48251 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48250)
store volatile %struct.ScmObj* %anf_45bind48251, %struct.ScmObj** %stackaddr$prim59159, align 8
%ae50769 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59160 = alloca %struct.ScmObj*, align 8
%_95048117 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48112, %struct.ScmObj* %ae50769, %struct.ScmObj* %anf_45bind48251)
store volatile %struct.ScmObj* %_95048117, %struct.ScmObj** %stackaddr$prim59160, align 8
%ae50772 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59161 = alloca %struct.ScmObj*, align 8
%anf_45bind48252 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48113, %struct.ScmObj* %ae50772)
store volatile %struct.ScmObj* %anf_45bind48252, %struct.ScmObj** %stackaddr$prim59161, align 8
%ae50774 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim59162 = alloca %struct.ScmObj*, align 8
%anf_45bind48253 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48252, %struct.ScmObj* %ae50774)
store volatile %struct.ScmObj* %anf_45bind48253, %struct.ScmObj** %stackaddr$prim59162, align 8
%ae50776 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59163 = alloca %struct.ScmObj*, align 8
%_95148116 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48113, %struct.ScmObj* %ae50776, %struct.ScmObj* %anf_45bind48253)
store volatile %struct.ScmObj* %_95148116, %struct.ScmObj** %stackaddr$prim59163, align 8
%argslist57642$cc481140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59164 = alloca %struct.ScmObj*, align 8
%argslist57642$cc481141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48114, %struct.ScmObj* %argslist57642$cc481140)
store volatile %struct.ScmObj* %argslist57642$cc481141, %struct.ScmObj** %stackaddr$prim59164, align 8
%stackaddr$prim59165 = alloca %struct.ScmObj*, align 8
%argslist57642$cc481142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48367, %struct.ScmObj* %argslist57642$cc481141)
store volatile %struct.ScmObj* %argslist57642$cc481142, %struct.ScmObj** %stackaddr$prim59165, align 8
%clofunc59166 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48114)
musttail call tailcc void %clofunc59166(%struct.ScmObj* %cc48114, %struct.ScmObj* %argslist57642$cc481142)
ret void
}

define tailcc void @proc_clo$ae50731(%struct.ScmObj* %env$ae50731,%struct.ScmObj* %current_45args57644) {
%stackaddr$prim59167 = alloca %struct.ScmObj*, align 8
%k48371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57644)
store volatile %struct.ScmObj* %k48371, %struct.ScmObj** %stackaddr$prim59167, align 8
%stackaddr$prim59168 = alloca %struct.ScmObj*, align 8
%current_45args57645 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57644)
store volatile %struct.ScmObj* %current_45args57645, %struct.ScmObj** %stackaddr$prim59168, align 8
%stackaddr$prim59169 = alloca %struct.ScmObj*, align 8
%u48115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57645)
store volatile %struct.ScmObj* %u48115, %struct.ScmObj** %stackaddr$prim59169, align 8
%argslist57647$u481150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59170 = alloca %struct.ScmObj*, align 8
%argslist57647$u481151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48115, %struct.ScmObj* %argslist57647$u481150)
store volatile %struct.ScmObj* %argslist57647$u481151, %struct.ScmObj** %stackaddr$prim59170, align 8
%stackaddr$prim59171 = alloca %struct.ScmObj*, align 8
%argslist57647$u481152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48371, %struct.ScmObj* %argslist57647$u481151)
store volatile %struct.ScmObj* %argslist57647$u481152, %struct.ScmObj** %stackaddr$prim59171, align 8
%clofunc59172 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48115)
musttail call tailcc void %clofunc59172(%struct.ScmObj* %u48115, %struct.ScmObj* %argslist57647$u481152)
ret void
}

define tailcc void @proc_clo$ae50308(%struct.ScmObj* %env$ae50308,%struct.ScmObj* %current_45args57650) {
%stackaddr$prim59173 = alloca %struct.ScmObj*, align 8
%k48372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57650)
store volatile %struct.ScmObj* %k48372, %struct.ScmObj** %stackaddr$prim59173, align 8
%stackaddr$prim59174 = alloca %struct.ScmObj*, align 8
%current_45args57651 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57650)
store volatile %struct.ScmObj* %current_45args57651, %struct.ScmObj** %stackaddr$prim59174, align 8
%stackaddr$prim59175 = alloca %struct.ScmObj*, align 8
%a48119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57651)
store volatile %struct.ScmObj* %a48119, %struct.ScmObj** %stackaddr$prim59175, align 8
%ae50309 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim59176 = alloca %struct.ScmObj*, align 8
%a48120 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50309, %struct.ScmObj* %a48119)
store volatile %struct.ScmObj* %a48120, %struct.ScmObj** %stackaddr$prim59176, align 8
%stackaddr$makeclosure59177 = alloca %struct.ScmObj*, align 8
%fptrToInt59178 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50311 to i64
%ae50311 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59178)
store volatile %struct.ScmObj* %ae50311, %struct.ScmObj** %stackaddr$makeclosure59177, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50311, %struct.ScmObj* %a48120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50311, %struct.ScmObj* %k48372, i64 1)
%ae50312 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59179 = alloca %struct.ScmObj*, align 8
%fptrToInt59180 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50313 to i64
%ae50313 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt59180)
store volatile %struct.ScmObj* %ae50313, %struct.ScmObj** %stackaddr$makeclosure59179, align 8
%argslist57673$ae503110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59181 = alloca %struct.ScmObj*, align 8
%argslist57673$ae503111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50313, %struct.ScmObj* %argslist57673$ae503110)
store volatile %struct.ScmObj* %argslist57673$ae503111, %struct.ScmObj** %stackaddr$prim59181, align 8
%stackaddr$prim59182 = alloca %struct.ScmObj*, align 8
%argslist57673$ae503112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50312, %struct.ScmObj* %argslist57673$ae503111)
store volatile %struct.ScmObj* %argslist57673$ae503112, %struct.ScmObj** %stackaddr$prim59182, align 8
%clofunc59183 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50311)
musttail call tailcc void %clofunc59183(%struct.ScmObj* %ae50311, %struct.ScmObj* %argslist57673$ae503112)
ret void
}

define tailcc void @proc_clo$ae50311(%struct.ScmObj* %env$ae50311,%struct.ScmObj* %current_45args57653) {
%stackaddr$env-ref59184 = alloca %struct.ScmObj*, align 8
%a48120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50311, i64 0)
store %struct.ScmObj* %a48120, %struct.ScmObj** %stackaddr$env-ref59184
%stackaddr$env-ref59185 = alloca %struct.ScmObj*, align 8
%k48372 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50311, i64 1)
store %struct.ScmObj* %k48372, %struct.ScmObj** %stackaddr$env-ref59185
%stackaddr$prim59186 = alloca %struct.ScmObj*, align 8
%_95k48373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57653)
store volatile %struct.ScmObj* %_95k48373, %struct.ScmObj** %stackaddr$prim59186, align 8
%stackaddr$prim59187 = alloca %struct.ScmObj*, align 8
%current_45args57654 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57653)
store volatile %struct.ScmObj* %current_45args57654, %struct.ScmObj** %stackaddr$prim59187, align 8
%stackaddr$prim59188 = alloca %struct.ScmObj*, align 8
%anf_45bind48239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57654)
store volatile %struct.ScmObj* %anf_45bind48239, %struct.ScmObj** %stackaddr$prim59188, align 8
%stackaddr$makeclosure59189 = alloca %struct.ScmObj*, align 8
%fptrToInt59190 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50330 to i64
%ae50330 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59190)
store volatile %struct.ScmObj* %ae50330, %struct.ScmObj** %stackaddr$makeclosure59189, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50330, %struct.ScmObj* %a48120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50330, %struct.ScmObj* %k48372, i64 1)
%stackaddr$makeclosure59191 = alloca %struct.ScmObj*, align 8
%fptrToInt59192 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50331 to i64
%ae50331 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59192)
store volatile %struct.ScmObj* %ae50331, %struct.ScmObj** %stackaddr$makeclosure59191, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50331, %struct.ScmObj* %a48120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50331, %struct.ScmObj* %k48372, i64 1)
%argslist57668$anf_45bind482390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59193 = alloca %struct.ScmObj*, align 8
%argslist57668$anf_45bind482391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50331, %struct.ScmObj* %argslist57668$anf_45bind482390)
store volatile %struct.ScmObj* %argslist57668$anf_45bind482391, %struct.ScmObj** %stackaddr$prim59193, align 8
%stackaddr$prim59194 = alloca %struct.ScmObj*, align 8
%argslist57668$anf_45bind482392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50330, %struct.ScmObj* %argslist57668$anf_45bind482391)
store volatile %struct.ScmObj* %argslist57668$anf_45bind482392, %struct.ScmObj** %stackaddr$prim59194, align 8
%clofunc59195 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48239)
musttail call tailcc void %clofunc59195(%struct.ScmObj* %anf_45bind48239, %struct.ScmObj* %argslist57668$anf_45bind482392)
ret void
}

define tailcc void @proc_clo$ae50330(%struct.ScmObj* %env$ae50330,%struct.ScmObj* %current_45args57656) {
%stackaddr$env-ref59196 = alloca %struct.ScmObj*, align 8
%a48120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50330, i64 0)
store %struct.ScmObj* %a48120, %struct.ScmObj** %stackaddr$env-ref59196
%stackaddr$env-ref59197 = alloca %struct.ScmObj*, align 8
%k48372 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50330, i64 1)
store %struct.ScmObj* %k48372, %struct.ScmObj** %stackaddr$env-ref59197
%stackaddr$prim59198 = alloca %struct.ScmObj*, align 8
%_95k48374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57656)
store volatile %struct.ScmObj* %_95k48374, %struct.ScmObj** %stackaddr$prim59198, align 8
%stackaddr$prim59199 = alloca %struct.ScmObj*, align 8
%current_45args57657 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57656)
store volatile %struct.ScmObj* %current_45args57657, %struct.ScmObj** %stackaddr$prim59199, align 8
%stackaddr$prim59200 = alloca %struct.ScmObj*, align 8
%cc48121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57657)
store volatile %struct.ScmObj* %cc48121, %struct.ScmObj** %stackaddr$prim59200, align 8
%ae50446 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59201 = alloca %struct.ScmObj*, align 8
%anf_45bind48240 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48120, %struct.ScmObj* %ae50446)
store volatile %struct.ScmObj* %anf_45bind48240, %struct.ScmObj** %stackaddr$prim59201, align 8
%stackaddr$prim59202 = alloca %struct.ScmObj*, align 8
%anf_45bind48241 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48240)
store volatile %struct.ScmObj* %anf_45bind48241, %struct.ScmObj** %stackaddr$prim59202, align 8
%truthy$cmp59203 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48241)
%cmp$cmp59203 = icmp eq i64 %truthy$cmp59203, 1
br i1 %cmp$cmp59203, label %truebranch$cmp59203, label %falsebranch$cmp59203
truebranch$cmp59203:
%ae50450 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50451 = call %struct.ScmObj* @const_init_true()
%argslist57659$k483720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59204 = alloca %struct.ScmObj*, align 8
%argslist57659$k483721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50451, %struct.ScmObj* %argslist57659$k483720)
store volatile %struct.ScmObj* %argslist57659$k483721, %struct.ScmObj** %stackaddr$prim59204, align 8
%stackaddr$prim59205 = alloca %struct.ScmObj*, align 8
%argslist57659$k483722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50450, %struct.ScmObj* %argslist57659$k483721)
store volatile %struct.ScmObj* %argslist57659$k483722, %struct.ScmObj** %stackaddr$prim59205, align 8
%clofunc59206 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48372)
musttail call tailcc void %clofunc59206(%struct.ScmObj* %k48372, %struct.ScmObj* %argslist57659$k483722)
ret void
falsebranch$cmp59203:
%ae50459 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59207 = alloca %struct.ScmObj*, align 8
%anf_45bind48242 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48120, %struct.ScmObj* %ae50459)
store volatile %struct.ScmObj* %anf_45bind48242, %struct.ScmObj** %stackaddr$prim59207, align 8
%stackaddr$prim59208 = alloca %struct.ScmObj*, align 8
%anf_45bind48243 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48242)
store volatile %struct.ScmObj* %anf_45bind48243, %struct.ScmObj** %stackaddr$prim59208, align 8
%truthy$cmp59209 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48243)
%cmp$cmp59209 = icmp eq i64 %truthy$cmp59209, 1
br i1 %cmp$cmp59209, label %truebranch$cmp59209, label %falsebranch$cmp59209
truebranch$cmp59209:
%ae50463 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59210 = alloca %struct.ScmObj*, align 8
%anf_45bind48244 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48120, %struct.ScmObj* %ae50463)
store volatile %struct.ScmObj* %anf_45bind48244, %struct.ScmObj** %stackaddr$prim59210, align 8
%stackaddr$prim59211 = alloca %struct.ScmObj*, align 8
%b48123 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48244)
store volatile %struct.ScmObj* %b48123, %struct.ScmObj** %stackaddr$prim59211, align 8
%ae50466 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59212 = alloca %struct.ScmObj*, align 8
%anf_45bind48245 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48120, %struct.ScmObj* %ae50466)
store volatile %struct.ScmObj* %anf_45bind48245, %struct.ScmObj** %stackaddr$prim59212, align 8
%stackaddr$prim59213 = alloca %struct.ScmObj*, align 8
%anf_45bind48246 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48245)
store volatile %struct.ScmObj* %anf_45bind48246, %struct.ScmObj** %stackaddr$prim59213, align 8
%ae50469 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59214 = alloca %struct.ScmObj*, align 8
%_95048124 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48120, %struct.ScmObj* %ae50469, %struct.ScmObj* %anf_45bind48246)
store volatile %struct.ScmObj* %_95048124, %struct.ScmObj** %stackaddr$prim59214, align 8
%argslist57660$cc481210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59215 = alloca %struct.ScmObj*, align 8
%argslist57660$cc481211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48121, %struct.ScmObj* %argslist57660$cc481210)
store volatile %struct.ScmObj* %argslist57660$cc481211, %struct.ScmObj** %stackaddr$prim59215, align 8
%stackaddr$prim59216 = alloca %struct.ScmObj*, align 8
%argslist57660$cc481212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48372, %struct.ScmObj* %argslist57660$cc481211)
store volatile %struct.ScmObj* %argslist57660$cc481212, %struct.ScmObj** %stackaddr$prim59216, align 8
%clofunc59217 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48121)
musttail call tailcc void %clofunc59217(%struct.ScmObj* %cc48121, %struct.ScmObj* %argslist57660$cc481212)
ret void
falsebranch$cmp59209:
%ae50502 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50503 = call %struct.ScmObj* @const_init_false()
%argslist57661$k483720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59218 = alloca %struct.ScmObj*, align 8
%argslist57661$k483721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50503, %struct.ScmObj* %argslist57661$k483720)
store volatile %struct.ScmObj* %argslist57661$k483721, %struct.ScmObj** %stackaddr$prim59218, align 8
%stackaddr$prim59219 = alloca %struct.ScmObj*, align 8
%argslist57661$k483722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50502, %struct.ScmObj* %argslist57661$k483721)
store volatile %struct.ScmObj* %argslist57661$k483722, %struct.ScmObj** %stackaddr$prim59219, align 8
%clofunc59220 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48372)
musttail call tailcc void %clofunc59220(%struct.ScmObj* %k48372, %struct.ScmObj* %argslist57661$k483722)
ret void
}

define tailcc void @proc_clo$ae50331(%struct.ScmObj* %env$ae50331,%struct.ScmObj* %current_45args57662) {
%stackaddr$env-ref59221 = alloca %struct.ScmObj*, align 8
%a48120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50331, i64 0)
store %struct.ScmObj* %a48120, %struct.ScmObj** %stackaddr$env-ref59221
%stackaddr$env-ref59222 = alloca %struct.ScmObj*, align 8
%k48372 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50331, i64 1)
store %struct.ScmObj* %k48372, %struct.ScmObj** %stackaddr$env-ref59222
%stackaddr$prim59223 = alloca %struct.ScmObj*, align 8
%_95k48374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57662)
store volatile %struct.ScmObj* %_95k48374, %struct.ScmObj** %stackaddr$prim59223, align 8
%stackaddr$prim59224 = alloca %struct.ScmObj*, align 8
%current_45args57663 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57662)
store volatile %struct.ScmObj* %current_45args57663, %struct.ScmObj** %stackaddr$prim59224, align 8
%stackaddr$prim59225 = alloca %struct.ScmObj*, align 8
%cc48121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57663)
store volatile %struct.ScmObj* %cc48121, %struct.ScmObj** %stackaddr$prim59225, align 8
%ae50333 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59226 = alloca %struct.ScmObj*, align 8
%anf_45bind48240 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48120, %struct.ScmObj* %ae50333)
store volatile %struct.ScmObj* %anf_45bind48240, %struct.ScmObj** %stackaddr$prim59226, align 8
%stackaddr$prim59227 = alloca %struct.ScmObj*, align 8
%anf_45bind48241 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48240)
store volatile %struct.ScmObj* %anf_45bind48241, %struct.ScmObj** %stackaddr$prim59227, align 8
%truthy$cmp59228 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48241)
%cmp$cmp59228 = icmp eq i64 %truthy$cmp59228, 1
br i1 %cmp$cmp59228, label %truebranch$cmp59228, label %falsebranch$cmp59228
truebranch$cmp59228:
%ae50337 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50338 = call %struct.ScmObj* @const_init_true()
%argslist57665$k483720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59229 = alloca %struct.ScmObj*, align 8
%argslist57665$k483721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50338, %struct.ScmObj* %argslist57665$k483720)
store volatile %struct.ScmObj* %argslist57665$k483721, %struct.ScmObj** %stackaddr$prim59229, align 8
%stackaddr$prim59230 = alloca %struct.ScmObj*, align 8
%argslist57665$k483722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50337, %struct.ScmObj* %argslist57665$k483721)
store volatile %struct.ScmObj* %argslist57665$k483722, %struct.ScmObj** %stackaddr$prim59230, align 8
%clofunc59231 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48372)
musttail call tailcc void %clofunc59231(%struct.ScmObj* %k48372, %struct.ScmObj* %argslist57665$k483722)
ret void
falsebranch$cmp59228:
%ae50346 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59232 = alloca %struct.ScmObj*, align 8
%anf_45bind48242 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48120, %struct.ScmObj* %ae50346)
store volatile %struct.ScmObj* %anf_45bind48242, %struct.ScmObj** %stackaddr$prim59232, align 8
%stackaddr$prim59233 = alloca %struct.ScmObj*, align 8
%anf_45bind48243 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48242)
store volatile %struct.ScmObj* %anf_45bind48243, %struct.ScmObj** %stackaddr$prim59233, align 8
%truthy$cmp59234 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48243)
%cmp$cmp59234 = icmp eq i64 %truthy$cmp59234, 1
br i1 %cmp$cmp59234, label %truebranch$cmp59234, label %falsebranch$cmp59234
truebranch$cmp59234:
%ae50350 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59235 = alloca %struct.ScmObj*, align 8
%anf_45bind48244 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48120, %struct.ScmObj* %ae50350)
store volatile %struct.ScmObj* %anf_45bind48244, %struct.ScmObj** %stackaddr$prim59235, align 8
%stackaddr$prim59236 = alloca %struct.ScmObj*, align 8
%b48123 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48244)
store volatile %struct.ScmObj* %b48123, %struct.ScmObj** %stackaddr$prim59236, align 8
%ae50353 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59237 = alloca %struct.ScmObj*, align 8
%anf_45bind48245 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48120, %struct.ScmObj* %ae50353)
store volatile %struct.ScmObj* %anf_45bind48245, %struct.ScmObj** %stackaddr$prim59237, align 8
%stackaddr$prim59238 = alloca %struct.ScmObj*, align 8
%anf_45bind48246 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48245)
store volatile %struct.ScmObj* %anf_45bind48246, %struct.ScmObj** %stackaddr$prim59238, align 8
%ae50356 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59239 = alloca %struct.ScmObj*, align 8
%_95048124 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48120, %struct.ScmObj* %ae50356, %struct.ScmObj* %anf_45bind48246)
store volatile %struct.ScmObj* %_95048124, %struct.ScmObj** %stackaddr$prim59239, align 8
%argslist57666$cc481210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59240 = alloca %struct.ScmObj*, align 8
%argslist57666$cc481211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48121, %struct.ScmObj* %argslist57666$cc481210)
store volatile %struct.ScmObj* %argslist57666$cc481211, %struct.ScmObj** %stackaddr$prim59240, align 8
%stackaddr$prim59241 = alloca %struct.ScmObj*, align 8
%argslist57666$cc481212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48372, %struct.ScmObj* %argslist57666$cc481211)
store volatile %struct.ScmObj* %argslist57666$cc481212, %struct.ScmObj** %stackaddr$prim59241, align 8
%clofunc59242 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48121)
musttail call tailcc void %clofunc59242(%struct.ScmObj* %cc48121, %struct.ScmObj* %argslist57666$cc481212)
ret void
falsebranch$cmp59234:
%ae50389 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50390 = call %struct.ScmObj* @const_init_false()
%argslist57667$k483720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59243 = alloca %struct.ScmObj*, align 8
%argslist57667$k483721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50390, %struct.ScmObj* %argslist57667$k483720)
store volatile %struct.ScmObj* %argslist57667$k483721, %struct.ScmObj** %stackaddr$prim59243, align 8
%stackaddr$prim59244 = alloca %struct.ScmObj*, align 8
%argslist57667$k483722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50389, %struct.ScmObj* %argslist57667$k483721)
store volatile %struct.ScmObj* %argslist57667$k483722, %struct.ScmObj** %stackaddr$prim59244, align 8
%clofunc59245 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48372)
musttail call tailcc void %clofunc59245(%struct.ScmObj* %k48372, %struct.ScmObj* %argslist57667$k483722)
ret void
}

define tailcc void @proc_clo$ae50313(%struct.ScmObj* %env$ae50313,%struct.ScmObj* %current_45args57669) {
%stackaddr$prim59246 = alloca %struct.ScmObj*, align 8
%k48375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57669)
store volatile %struct.ScmObj* %k48375, %struct.ScmObj** %stackaddr$prim59246, align 8
%stackaddr$prim59247 = alloca %struct.ScmObj*, align 8
%current_45args57670 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57669)
store volatile %struct.ScmObj* %current_45args57670, %struct.ScmObj** %stackaddr$prim59247, align 8
%stackaddr$prim59248 = alloca %struct.ScmObj*, align 8
%k48122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57670)
store volatile %struct.ScmObj* %k48122, %struct.ScmObj** %stackaddr$prim59248, align 8
%ae50315 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57672$k483750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59249 = alloca %struct.ScmObj*, align 8
%argslist57672$k483751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48122, %struct.ScmObj* %argslist57672$k483750)
store volatile %struct.ScmObj* %argslist57672$k483751, %struct.ScmObj** %stackaddr$prim59249, align 8
%stackaddr$prim59250 = alloca %struct.ScmObj*, align 8
%argslist57672$k483752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50315, %struct.ScmObj* %argslist57672$k483751)
store volatile %struct.ScmObj* %argslist57672$k483752, %struct.ScmObj** %stackaddr$prim59250, align 8
%clofunc59251 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48375)
musttail call tailcc void %clofunc59251(%struct.ScmObj* %k48375, %struct.ScmObj* %argslist57672$k483752)
ret void
}

define tailcc void @proc_clo$ae50236(%struct.ScmObj* %env$ae50236,%struct.ScmObj* %current_45args57675) {
%stackaddr$env-ref59252 = alloca %struct.ScmObj*, align 8
%_37append48126 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50236, i64 0)
store %struct.ScmObj* %_37append48126, %struct.ScmObj** %stackaddr$env-ref59252
%stackaddr$prim59253 = alloca %struct.ScmObj*, align 8
%k48376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57675)
store volatile %struct.ScmObj* %k48376, %struct.ScmObj** %stackaddr$prim59253, align 8
%stackaddr$prim59254 = alloca %struct.ScmObj*, align 8
%current_45args57676 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57675)
store volatile %struct.ScmObj* %current_45args57676, %struct.ScmObj** %stackaddr$prim59254, align 8
%stackaddr$prim59255 = alloca %struct.ScmObj*, align 8
%ls048129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57676)
store volatile %struct.ScmObj* %ls048129, %struct.ScmObj** %stackaddr$prim59255, align 8
%stackaddr$prim59256 = alloca %struct.ScmObj*, align 8
%current_45args57677 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57676)
store volatile %struct.ScmObj* %current_45args57677, %struct.ScmObj** %stackaddr$prim59256, align 8
%stackaddr$prim59257 = alloca %struct.ScmObj*, align 8
%ls148128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57677)
store volatile %struct.ScmObj* %ls148128, %struct.ScmObj** %stackaddr$prim59257, align 8
%stackaddr$prim59258 = alloca %struct.ScmObj*, align 8
%anf_45bind48233 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls048129)
store volatile %struct.ScmObj* %anf_45bind48233, %struct.ScmObj** %stackaddr$prim59258, align 8
%truthy$cmp59259 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48233)
%cmp$cmp59259 = icmp eq i64 %truthy$cmp59259, 1
br i1 %cmp$cmp59259, label %truebranch$cmp59259, label %falsebranch$cmp59259
truebranch$cmp59259:
%ae50240 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57679$k483760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59260 = alloca %struct.ScmObj*, align 8
%argslist57679$k483761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148128, %struct.ScmObj* %argslist57679$k483760)
store volatile %struct.ScmObj* %argslist57679$k483761, %struct.ScmObj** %stackaddr$prim59260, align 8
%stackaddr$prim59261 = alloca %struct.ScmObj*, align 8
%argslist57679$k483762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50240, %struct.ScmObj* %argslist57679$k483761)
store volatile %struct.ScmObj* %argslist57679$k483762, %struct.ScmObj** %stackaddr$prim59261, align 8
%clofunc59262 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48376)
musttail call tailcc void %clofunc59262(%struct.ScmObj* %k48376, %struct.ScmObj* %argslist57679$k483762)
ret void
falsebranch$cmp59259:
%stackaddr$prim59263 = alloca %struct.ScmObj*, align 8
%anf_45bind48234 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls048129)
store volatile %struct.ScmObj* %anf_45bind48234, %struct.ScmObj** %stackaddr$prim59263, align 8
%ae50247 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59264 = alloca %struct.ScmObj*, align 8
%anf_45bind48235 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48126, %struct.ScmObj* %ae50247)
store volatile %struct.ScmObj* %anf_45bind48235, %struct.ScmObj** %stackaddr$prim59264, align 8
%stackaddr$prim59265 = alloca %struct.ScmObj*, align 8
%anf_45bind48236 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls048129)
store volatile %struct.ScmObj* %anf_45bind48236, %struct.ScmObj** %stackaddr$prim59265, align 8
%stackaddr$makeclosure59266 = alloca %struct.ScmObj*, align 8
%fptrToInt59267 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50250 to i64
%ae50250 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59267)
store volatile %struct.ScmObj* %ae50250, %struct.ScmObj** %stackaddr$makeclosure59266, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50250, %struct.ScmObj* %k48376, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50250, %struct.ScmObj* %anf_45bind48234, i64 1)
%argslist57684$anf_45bind482350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59268 = alloca %struct.ScmObj*, align 8
%argslist57684$anf_45bind482351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148128, %struct.ScmObj* %argslist57684$anf_45bind482350)
store volatile %struct.ScmObj* %argslist57684$anf_45bind482351, %struct.ScmObj** %stackaddr$prim59268, align 8
%stackaddr$prim59269 = alloca %struct.ScmObj*, align 8
%argslist57684$anf_45bind482352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48236, %struct.ScmObj* %argslist57684$anf_45bind482351)
store volatile %struct.ScmObj* %argslist57684$anf_45bind482352, %struct.ScmObj** %stackaddr$prim59269, align 8
%stackaddr$prim59270 = alloca %struct.ScmObj*, align 8
%argslist57684$anf_45bind482353 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50250, %struct.ScmObj* %argslist57684$anf_45bind482352)
store volatile %struct.ScmObj* %argslist57684$anf_45bind482353, %struct.ScmObj** %stackaddr$prim59270, align 8
%clofunc59271 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48235)
musttail call tailcc void %clofunc59271(%struct.ScmObj* %anf_45bind48235, %struct.ScmObj* %argslist57684$anf_45bind482353)
ret void
}

define tailcc void @proc_clo$ae50250(%struct.ScmObj* %env$ae50250,%struct.ScmObj* %current_45args57680) {
%stackaddr$env-ref59272 = alloca %struct.ScmObj*, align 8
%k48376 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50250, i64 0)
store %struct.ScmObj* %k48376, %struct.ScmObj** %stackaddr$env-ref59272
%stackaddr$env-ref59273 = alloca %struct.ScmObj*, align 8
%anf_45bind48234 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50250, i64 1)
store %struct.ScmObj* %anf_45bind48234, %struct.ScmObj** %stackaddr$env-ref59273
%stackaddr$prim59274 = alloca %struct.ScmObj*, align 8
%_95k48377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57680)
store volatile %struct.ScmObj* %_95k48377, %struct.ScmObj** %stackaddr$prim59274, align 8
%stackaddr$prim59275 = alloca %struct.ScmObj*, align 8
%current_45args57681 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57680)
store volatile %struct.ScmObj* %current_45args57681, %struct.ScmObj** %stackaddr$prim59275, align 8
%stackaddr$prim59276 = alloca %struct.ScmObj*, align 8
%anf_45bind48237 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57681)
store volatile %struct.ScmObj* %anf_45bind48237, %struct.ScmObj** %stackaddr$prim59276, align 8
%stackaddr$prim59277 = alloca %struct.ScmObj*, align 8
%cpsprim48378 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48234, %struct.ScmObj* %anf_45bind48237)
store volatile %struct.ScmObj* %cpsprim48378, %struct.ScmObj** %stackaddr$prim59277, align 8
%ae50256 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57683$k483760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59278 = alloca %struct.ScmObj*, align 8
%argslist57683$k483761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48378, %struct.ScmObj* %argslist57683$k483760)
store volatile %struct.ScmObj* %argslist57683$k483761, %struct.ScmObj** %stackaddr$prim59278, align 8
%stackaddr$prim59279 = alloca %struct.ScmObj*, align 8
%argslist57683$k483762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50256, %struct.ScmObj* %argslist57683$k483761)
store volatile %struct.ScmObj* %argslist57683$k483762, %struct.ScmObj** %stackaddr$prim59279, align 8
%clofunc59280 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48376)
musttail call tailcc void %clofunc59280(%struct.ScmObj* %k48376, %struct.ScmObj* %argslist57683$k483762)
ret void
}

define tailcc void @proc_clo$ae50210(%struct.ScmObj* %env$ae50210,%struct.ScmObj* %current_45args57686) {
%stackaddr$prim59281 = alloca %struct.ScmObj*, align 8
%k48379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57686)
store volatile %struct.ScmObj* %k48379, %struct.ScmObj** %stackaddr$prim59281, align 8
%stackaddr$prim59282 = alloca %struct.ScmObj*, align 8
%current_45args57687 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57686)
store volatile %struct.ScmObj* %current_45args57687, %struct.ScmObj** %stackaddr$prim59282, align 8
%stackaddr$prim59283 = alloca %struct.ScmObj*, align 8
%a48132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57687)
store volatile %struct.ScmObj* %a48132, %struct.ScmObj** %stackaddr$prim59283, align 8
%stackaddr$prim59284 = alloca %struct.ScmObj*, align 8
%current_45args57688 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57687)
store volatile %struct.ScmObj* %current_45args57688, %struct.ScmObj** %stackaddr$prim59284, align 8
%stackaddr$prim59285 = alloca %struct.ScmObj*, align 8
%b48131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57688)
store volatile %struct.ScmObj* %b48131, %struct.ScmObj** %stackaddr$prim59285, align 8
%stackaddr$prim59286 = alloca %struct.ScmObj*, align 8
%anf_45bind48232 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a48132, %struct.ScmObj* %b48131)
store volatile %struct.ScmObj* %anf_45bind48232, %struct.ScmObj** %stackaddr$prim59286, align 8
%stackaddr$prim59287 = alloca %struct.ScmObj*, align 8
%cpsprim48380 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48232)
store volatile %struct.ScmObj* %cpsprim48380, %struct.ScmObj** %stackaddr$prim59287, align 8
%ae50215 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57690$k483790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59288 = alloca %struct.ScmObj*, align 8
%argslist57690$k483791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48380, %struct.ScmObj* %argslist57690$k483790)
store volatile %struct.ScmObj* %argslist57690$k483791, %struct.ScmObj** %stackaddr$prim59288, align 8
%stackaddr$prim59289 = alloca %struct.ScmObj*, align 8
%argslist57690$k483792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50215, %struct.ScmObj* %argslist57690$k483791)
store volatile %struct.ScmObj* %argslist57690$k483792, %struct.ScmObj** %stackaddr$prim59289, align 8
%clofunc59290 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48379)
musttail call tailcc void %clofunc59290(%struct.ScmObj* %k48379, %struct.ScmObj* %argslist57690$k483792)
ret void
}

define tailcc void @proc_clo$ae50186(%struct.ScmObj* %env$ae50186,%struct.ScmObj* %current_45args57692) {
%stackaddr$prim59291 = alloca %struct.ScmObj*, align 8
%k48381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57692)
store volatile %struct.ScmObj* %k48381, %struct.ScmObj** %stackaddr$prim59291, align 8
%stackaddr$prim59292 = alloca %struct.ScmObj*, align 8
%current_45args57693 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57692)
store volatile %struct.ScmObj* %current_45args57693, %struct.ScmObj** %stackaddr$prim59292, align 8
%stackaddr$prim59293 = alloca %struct.ScmObj*, align 8
%a48135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57693)
store volatile %struct.ScmObj* %a48135, %struct.ScmObj** %stackaddr$prim59293, align 8
%stackaddr$prim59294 = alloca %struct.ScmObj*, align 8
%current_45args57694 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57693)
store volatile %struct.ScmObj* %current_45args57694, %struct.ScmObj** %stackaddr$prim59294, align 8
%stackaddr$prim59295 = alloca %struct.ScmObj*, align 8
%b48134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57694)
store volatile %struct.ScmObj* %b48134, %struct.ScmObj** %stackaddr$prim59295, align 8
%stackaddr$prim59296 = alloca %struct.ScmObj*, align 8
%anf_45bind48231 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a48135, %struct.ScmObj* %b48134)
store volatile %struct.ScmObj* %anf_45bind48231, %struct.ScmObj** %stackaddr$prim59296, align 8
%stackaddr$prim59297 = alloca %struct.ScmObj*, align 8
%cpsprim48382 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48231)
store volatile %struct.ScmObj* %cpsprim48382, %struct.ScmObj** %stackaddr$prim59297, align 8
%ae50191 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57696$k483810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59298 = alloca %struct.ScmObj*, align 8
%argslist57696$k483811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48382, %struct.ScmObj* %argslist57696$k483810)
store volatile %struct.ScmObj* %argslist57696$k483811, %struct.ScmObj** %stackaddr$prim59298, align 8
%stackaddr$prim59299 = alloca %struct.ScmObj*, align 8
%argslist57696$k483812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50191, %struct.ScmObj* %argslist57696$k483811)
store volatile %struct.ScmObj* %argslist57696$k483812, %struct.ScmObj** %stackaddr$prim59299, align 8
%clofunc59300 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48381)
musttail call tailcc void %clofunc59300(%struct.ScmObj* %k48381, %struct.ScmObj* %argslist57696$k483812)
ret void
}

define tailcc void @proc_clo$ae49792(%struct.ScmObj* %env$ae49792,%struct.ScmObj* %current_45args57699) {
%stackaddr$env-ref59301 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49792, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref59301
%stackaddr$env-ref59302 = alloca %struct.ScmObj*, align 8
%_37map148085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49792, i64 1)
store %struct.ScmObj* %_37map148085, %struct.ScmObj** %stackaddr$env-ref59302
%stackaddr$env-ref59303 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49792, i64 2)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref59303
%stackaddr$prim59304 = alloca %struct.ScmObj*, align 8
%k48383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57699)
store volatile %struct.ScmObj* %k48383, %struct.ScmObj** %stackaddr$prim59304, align 8
%stackaddr$prim59305 = alloca %struct.ScmObj*, align 8
%current_45args57700 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57699)
store volatile %struct.ScmObj* %current_45args57700, %struct.ScmObj** %stackaddr$prim59305, align 8
%stackaddr$prim59306 = alloca %struct.ScmObj*, align 8
%_37foldl48137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57700)
store volatile %struct.ScmObj* %_37foldl48137, %struct.ScmObj** %stackaddr$prim59306, align 8
%ae49794 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59307 = alloca %struct.ScmObj*, align 8
%fptrToInt59308 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49795 to i64
%ae49795 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt59308)
store volatile %struct.ScmObj* %ae49795, %struct.ScmObj** %stackaddr$makeclosure59307, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49795, %struct.ScmObj* %_37foldl48137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49795, %struct.ScmObj* %_37foldr148054, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49795, %struct.ScmObj* %_37map148085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49795, %struct.ScmObj* %_37foldr48059, i64 3)
%argslist57757$k483830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59309 = alloca %struct.ScmObj*, align 8
%argslist57757$k483831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49795, %struct.ScmObj* %argslist57757$k483830)
store volatile %struct.ScmObj* %argslist57757$k483831, %struct.ScmObj** %stackaddr$prim59309, align 8
%stackaddr$prim59310 = alloca %struct.ScmObj*, align 8
%argslist57757$k483832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49794, %struct.ScmObj* %argslist57757$k483831)
store volatile %struct.ScmObj* %argslist57757$k483832, %struct.ScmObj** %stackaddr$prim59310, align 8
%clofunc59311 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48383)
musttail call tailcc void %clofunc59311(%struct.ScmObj* %k48383, %struct.ScmObj* %argslist57757$k483832)
ret void
}

define tailcc void @proc_clo$ae49795(%struct.ScmObj* %env$ae49795,%struct.ScmObj* %args4813848384) {
%stackaddr$env-ref59312 = alloca %struct.ScmObj*, align 8
%_37foldl48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49795, i64 0)
store %struct.ScmObj* %_37foldl48137, %struct.ScmObj** %stackaddr$env-ref59312
%stackaddr$env-ref59313 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49795, i64 1)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref59313
%stackaddr$env-ref59314 = alloca %struct.ScmObj*, align 8
%_37map148085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49795, i64 2)
store %struct.ScmObj* %_37map148085, %struct.ScmObj** %stackaddr$env-ref59314
%stackaddr$env-ref59315 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49795, i64 3)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref59315
%stackaddr$prim59316 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4813848384)
store volatile %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$prim59316, align 8
%stackaddr$prim59317 = alloca %struct.ScmObj*, align 8
%args48138 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4813848384)
store volatile %struct.ScmObj* %args48138, %struct.ScmObj** %stackaddr$prim59317, align 8
%stackaddr$prim59318 = alloca %struct.ScmObj*, align 8
%f48141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48138)
store volatile %struct.ScmObj* %f48141, %struct.ScmObj** %stackaddr$prim59318, align 8
%stackaddr$prim59319 = alloca %struct.ScmObj*, align 8
%anf_45bind48219 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48138)
store volatile %struct.ScmObj* %anf_45bind48219, %struct.ScmObj** %stackaddr$prim59319, align 8
%stackaddr$prim59320 = alloca %struct.ScmObj*, align 8
%acc48140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48219)
store volatile %struct.ScmObj* %acc48140, %struct.ScmObj** %stackaddr$prim59320, align 8
%stackaddr$prim59321 = alloca %struct.ScmObj*, align 8
%anf_45bind48220 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48138)
store volatile %struct.ScmObj* %anf_45bind48220, %struct.ScmObj** %stackaddr$prim59321, align 8
%stackaddr$prim59322 = alloca %struct.ScmObj*, align 8
%lsts48139 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48220)
store volatile %struct.ScmObj* %lsts48139, %struct.ScmObj** %stackaddr$prim59322, align 8
%stackaddr$makeclosure59323 = alloca %struct.ScmObj*, align 8
%fptrToInt59324 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49803 to i64
%ae49803 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt59324)
store volatile %struct.ScmObj* %ae49803, %struct.ScmObj** %stackaddr$makeclosure59323, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49803, %struct.ScmObj* %lsts48139, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49803, %struct.ScmObj* %_37foldr48059, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49803, %struct.ScmObj* %_37foldl48137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49803, %struct.ScmObj* %_37foldr148054, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49803, %struct.ScmObj* %_37map148085, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49803, %struct.ScmObj* %k48385, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49803, %struct.ScmObj* %f48141, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49803, %struct.ScmObj* %acc48140, i64 7)
%ae49804 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59325 = alloca %struct.ScmObj*, align 8
%fptrToInt59326 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49805 to i64
%ae49805 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt59326)
store volatile %struct.ScmObj* %ae49805, %struct.ScmObj** %stackaddr$makeclosure59325, align 8
%argslist57756$ae498030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59327 = alloca %struct.ScmObj*, align 8
%argslist57756$ae498031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49805, %struct.ScmObj* %argslist57756$ae498030)
store volatile %struct.ScmObj* %argslist57756$ae498031, %struct.ScmObj** %stackaddr$prim59327, align 8
%stackaddr$prim59328 = alloca %struct.ScmObj*, align 8
%argslist57756$ae498032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49804, %struct.ScmObj* %argslist57756$ae498031)
store volatile %struct.ScmObj* %argslist57756$ae498032, %struct.ScmObj** %stackaddr$prim59328, align 8
%clofunc59329 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49803)
musttail call tailcc void %clofunc59329(%struct.ScmObj* %ae49803, %struct.ScmObj* %argslist57756$ae498032)
ret void
}

define tailcc void @proc_clo$ae49803(%struct.ScmObj* %env$ae49803,%struct.ScmObj* %current_45args57702) {
%stackaddr$env-ref59330 = alloca %struct.ScmObj*, align 8
%lsts48139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49803, i64 0)
store %struct.ScmObj* %lsts48139, %struct.ScmObj** %stackaddr$env-ref59330
%stackaddr$env-ref59331 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49803, i64 1)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref59331
%stackaddr$env-ref59332 = alloca %struct.ScmObj*, align 8
%_37foldl48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49803, i64 2)
store %struct.ScmObj* %_37foldl48137, %struct.ScmObj** %stackaddr$env-ref59332
%stackaddr$env-ref59333 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49803, i64 3)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref59333
%stackaddr$env-ref59334 = alloca %struct.ScmObj*, align 8
%_37map148085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49803, i64 4)
store %struct.ScmObj* %_37map148085, %struct.ScmObj** %stackaddr$env-ref59334
%stackaddr$env-ref59335 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49803, i64 5)
store %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$env-ref59335
%stackaddr$env-ref59336 = alloca %struct.ScmObj*, align 8
%f48141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49803, i64 6)
store %struct.ScmObj* %f48141, %struct.ScmObj** %stackaddr$env-ref59336
%stackaddr$env-ref59337 = alloca %struct.ScmObj*, align 8
%acc48140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49803, i64 7)
store %struct.ScmObj* %acc48140, %struct.ScmObj** %stackaddr$env-ref59337
%stackaddr$prim59338 = alloca %struct.ScmObj*, align 8
%_95k48386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57702)
store volatile %struct.ScmObj* %_95k48386, %struct.ScmObj** %stackaddr$prim59338, align 8
%stackaddr$prim59339 = alloca %struct.ScmObj*, align 8
%current_45args57703 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57702)
store volatile %struct.ScmObj* %current_45args57703, %struct.ScmObj** %stackaddr$prim59339, align 8
%stackaddr$prim59340 = alloca %struct.ScmObj*, align 8
%anf_45bind48221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57703)
store volatile %struct.ScmObj* %anf_45bind48221, %struct.ScmObj** %stackaddr$prim59340, align 8
%stackaddr$makeclosure59341 = alloca %struct.ScmObj*, align 8
%fptrToInt59342 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49835 to i64
%ae49835 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt59342)
store volatile %struct.ScmObj* %ae49835, %struct.ScmObj** %stackaddr$makeclosure59341, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49835, %struct.ScmObj* %lsts48139, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49835, %struct.ScmObj* %_37foldr48059, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49835, %struct.ScmObj* %_37foldl48137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49835, %struct.ScmObj* %_37map148085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49835, %struct.ScmObj* %k48385, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49835, %struct.ScmObj* %f48141, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49835, %struct.ScmObj* %acc48140, i64 6)
%ae49837 = call %struct.ScmObj* @const_init_false()
%argslist57749$_37foldr1480540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59343 = alloca %struct.ScmObj*, align 8
%argslist57749$_37foldr1480541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48139, %struct.ScmObj* %argslist57749$_37foldr1480540)
store volatile %struct.ScmObj* %argslist57749$_37foldr1480541, %struct.ScmObj** %stackaddr$prim59343, align 8
%stackaddr$prim59344 = alloca %struct.ScmObj*, align 8
%argslist57749$_37foldr1480542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49837, %struct.ScmObj* %argslist57749$_37foldr1480541)
store volatile %struct.ScmObj* %argslist57749$_37foldr1480542, %struct.ScmObj** %stackaddr$prim59344, align 8
%stackaddr$prim59345 = alloca %struct.ScmObj*, align 8
%argslist57749$_37foldr1480543 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48221, %struct.ScmObj* %argslist57749$_37foldr1480542)
store volatile %struct.ScmObj* %argslist57749$_37foldr1480543, %struct.ScmObj** %stackaddr$prim59345, align 8
%stackaddr$prim59346 = alloca %struct.ScmObj*, align 8
%argslist57749$_37foldr1480544 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49835, %struct.ScmObj* %argslist57749$_37foldr1480543)
store volatile %struct.ScmObj* %argslist57749$_37foldr1480544, %struct.ScmObj** %stackaddr$prim59346, align 8
%clofunc59347 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148054)
musttail call tailcc void %clofunc59347(%struct.ScmObj* %_37foldr148054, %struct.ScmObj* %argslist57749$_37foldr1480544)
ret void
}

define tailcc void @proc_clo$ae49835(%struct.ScmObj* %env$ae49835,%struct.ScmObj* %current_45args57705) {
%stackaddr$env-ref59348 = alloca %struct.ScmObj*, align 8
%lsts48139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49835, i64 0)
store %struct.ScmObj* %lsts48139, %struct.ScmObj** %stackaddr$env-ref59348
%stackaddr$env-ref59349 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49835, i64 1)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref59349
%stackaddr$env-ref59350 = alloca %struct.ScmObj*, align 8
%_37foldl48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49835, i64 2)
store %struct.ScmObj* %_37foldl48137, %struct.ScmObj** %stackaddr$env-ref59350
%stackaddr$env-ref59351 = alloca %struct.ScmObj*, align 8
%_37map148085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49835, i64 3)
store %struct.ScmObj* %_37map148085, %struct.ScmObj** %stackaddr$env-ref59351
%stackaddr$env-ref59352 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49835, i64 4)
store %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$env-ref59352
%stackaddr$env-ref59353 = alloca %struct.ScmObj*, align 8
%f48141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49835, i64 5)
store %struct.ScmObj* %f48141, %struct.ScmObj** %stackaddr$env-ref59353
%stackaddr$env-ref59354 = alloca %struct.ScmObj*, align 8
%acc48140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49835, i64 6)
store %struct.ScmObj* %acc48140, %struct.ScmObj** %stackaddr$env-ref59354
%stackaddr$prim59355 = alloca %struct.ScmObj*, align 8
%_95k48387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57705)
store volatile %struct.ScmObj* %_95k48387, %struct.ScmObj** %stackaddr$prim59355, align 8
%stackaddr$prim59356 = alloca %struct.ScmObj*, align 8
%current_45args57706 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57705)
store volatile %struct.ScmObj* %current_45args57706, %struct.ScmObj** %stackaddr$prim59356, align 8
%stackaddr$prim59357 = alloca %struct.ScmObj*, align 8
%anf_45bind48222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57706)
store volatile %struct.ScmObj* %anf_45bind48222, %struct.ScmObj** %stackaddr$prim59357, align 8
%truthy$cmp59358 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48222)
%cmp$cmp59358 = icmp eq i64 %truthy$cmp59358, 1
br i1 %cmp$cmp59358, label %truebranch$cmp59358, label %falsebranch$cmp59358
truebranch$cmp59358:
%ae49846 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57708$k483850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59359 = alloca %struct.ScmObj*, align 8
%argslist57708$k483851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48140, %struct.ScmObj* %argslist57708$k483850)
store volatile %struct.ScmObj* %argslist57708$k483851, %struct.ScmObj** %stackaddr$prim59359, align 8
%stackaddr$prim59360 = alloca %struct.ScmObj*, align 8
%argslist57708$k483852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49846, %struct.ScmObj* %argslist57708$k483851)
store volatile %struct.ScmObj* %argslist57708$k483852, %struct.ScmObj** %stackaddr$prim59360, align 8
%clofunc59361 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48385)
musttail call tailcc void %clofunc59361(%struct.ScmObj* %k48385, %struct.ScmObj* %argslist57708$k483852)
ret void
falsebranch$cmp59358:
%stackaddr$makeclosure59362 = alloca %struct.ScmObj*, align 8
%fptrToInt59363 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49851 to i64
%ae49851 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt59363)
store volatile %struct.ScmObj* %ae49851, %struct.ScmObj** %stackaddr$makeclosure59362, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49851, %struct.ScmObj* %lsts48139, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49851, %struct.ScmObj* %_37foldr48059, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49851, %struct.ScmObj* %_37foldl48137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49851, %struct.ScmObj* %_37map148085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49851, %struct.ScmObj* %k48385, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49851, %struct.ScmObj* %f48141, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49851, %struct.ScmObj* %acc48140, i64 6)
%ae49852 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59364 = alloca %struct.ScmObj*, align 8
%fptrToInt59365 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49853 to i64
%ae49853 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt59365)
store volatile %struct.ScmObj* %ae49853, %struct.ScmObj** %stackaddr$makeclosure59364, align 8
%argslist57748$ae498510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59366 = alloca %struct.ScmObj*, align 8
%argslist57748$ae498511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49853, %struct.ScmObj* %argslist57748$ae498510)
store volatile %struct.ScmObj* %argslist57748$ae498511, %struct.ScmObj** %stackaddr$prim59366, align 8
%stackaddr$prim59367 = alloca %struct.ScmObj*, align 8
%argslist57748$ae498512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49852, %struct.ScmObj* %argslist57748$ae498511)
store volatile %struct.ScmObj* %argslist57748$ae498512, %struct.ScmObj** %stackaddr$prim59367, align 8
%clofunc59368 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49851)
musttail call tailcc void %clofunc59368(%struct.ScmObj* %ae49851, %struct.ScmObj* %argslist57748$ae498512)
ret void
}

define tailcc void @proc_clo$ae49851(%struct.ScmObj* %env$ae49851,%struct.ScmObj* %current_45args57709) {
%stackaddr$env-ref59369 = alloca %struct.ScmObj*, align 8
%lsts48139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49851, i64 0)
store %struct.ScmObj* %lsts48139, %struct.ScmObj** %stackaddr$env-ref59369
%stackaddr$env-ref59370 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49851, i64 1)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref59370
%stackaddr$env-ref59371 = alloca %struct.ScmObj*, align 8
%_37foldl48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49851, i64 2)
store %struct.ScmObj* %_37foldl48137, %struct.ScmObj** %stackaddr$env-ref59371
%stackaddr$env-ref59372 = alloca %struct.ScmObj*, align 8
%_37map148085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49851, i64 3)
store %struct.ScmObj* %_37map148085, %struct.ScmObj** %stackaddr$env-ref59372
%stackaddr$env-ref59373 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49851, i64 4)
store %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$env-ref59373
%stackaddr$env-ref59374 = alloca %struct.ScmObj*, align 8
%f48141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49851, i64 5)
store %struct.ScmObj* %f48141, %struct.ScmObj** %stackaddr$env-ref59374
%stackaddr$env-ref59375 = alloca %struct.ScmObj*, align 8
%acc48140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49851, i64 6)
store %struct.ScmObj* %acc48140, %struct.ScmObj** %stackaddr$env-ref59375
%stackaddr$prim59376 = alloca %struct.ScmObj*, align 8
%_95k48388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57709)
store volatile %struct.ScmObj* %_95k48388, %struct.ScmObj** %stackaddr$prim59376, align 8
%stackaddr$prim59377 = alloca %struct.ScmObj*, align 8
%current_45args57710 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57709)
store volatile %struct.ScmObj* %current_45args57710, %struct.ScmObj** %stackaddr$prim59377, align 8
%stackaddr$prim59378 = alloca %struct.ScmObj*, align 8
%anf_45bind48223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57710)
store volatile %struct.ScmObj* %anf_45bind48223, %struct.ScmObj** %stackaddr$prim59378, align 8
%stackaddr$makeclosure59379 = alloca %struct.ScmObj*, align 8
%fptrToInt59380 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49872 to i64
%ae49872 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt59380)
store volatile %struct.ScmObj* %ae49872, %struct.ScmObj** %stackaddr$makeclosure59379, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49872, %struct.ScmObj* %lsts48139, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49872, %struct.ScmObj* %_37foldr48059, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49872, %struct.ScmObj* %_37foldl48137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49872, %struct.ScmObj* %_37map148085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49872, %struct.ScmObj* %k48385, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49872, %struct.ScmObj* %f48141, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49872, %struct.ScmObj* %acc48140, i64 6)
%argslist57743$_37map1480850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59381 = alloca %struct.ScmObj*, align 8
%argslist57743$_37map1480851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48139, %struct.ScmObj* %argslist57743$_37map1480850)
store volatile %struct.ScmObj* %argslist57743$_37map1480851, %struct.ScmObj** %stackaddr$prim59381, align 8
%stackaddr$prim59382 = alloca %struct.ScmObj*, align 8
%argslist57743$_37map1480852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48223, %struct.ScmObj* %argslist57743$_37map1480851)
store volatile %struct.ScmObj* %argslist57743$_37map1480852, %struct.ScmObj** %stackaddr$prim59382, align 8
%stackaddr$prim59383 = alloca %struct.ScmObj*, align 8
%argslist57743$_37map1480853 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49872, %struct.ScmObj* %argslist57743$_37map1480852)
store volatile %struct.ScmObj* %argslist57743$_37map1480853, %struct.ScmObj** %stackaddr$prim59383, align 8
%clofunc59384 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148085)
musttail call tailcc void %clofunc59384(%struct.ScmObj* %_37map148085, %struct.ScmObj* %argslist57743$_37map1480853)
ret void
}

define tailcc void @proc_clo$ae49872(%struct.ScmObj* %env$ae49872,%struct.ScmObj* %current_45args57712) {
%stackaddr$env-ref59385 = alloca %struct.ScmObj*, align 8
%lsts48139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49872, i64 0)
store %struct.ScmObj* %lsts48139, %struct.ScmObj** %stackaddr$env-ref59385
%stackaddr$env-ref59386 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49872, i64 1)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref59386
%stackaddr$env-ref59387 = alloca %struct.ScmObj*, align 8
%_37foldl48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49872, i64 2)
store %struct.ScmObj* %_37foldl48137, %struct.ScmObj** %stackaddr$env-ref59387
%stackaddr$env-ref59388 = alloca %struct.ScmObj*, align 8
%_37map148085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49872, i64 3)
store %struct.ScmObj* %_37map148085, %struct.ScmObj** %stackaddr$env-ref59388
%stackaddr$env-ref59389 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49872, i64 4)
store %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$env-ref59389
%stackaddr$env-ref59390 = alloca %struct.ScmObj*, align 8
%f48141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49872, i64 5)
store %struct.ScmObj* %f48141, %struct.ScmObj** %stackaddr$env-ref59390
%stackaddr$env-ref59391 = alloca %struct.ScmObj*, align 8
%acc48140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49872, i64 6)
store %struct.ScmObj* %acc48140, %struct.ScmObj** %stackaddr$env-ref59391
%stackaddr$prim59392 = alloca %struct.ScmObj*, align 8
%_95k48389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57712)
store volatile %struct.ScmObj* %_95k48389, %struct.ScmObj** %stackaddr$prim59392, align 8
%stackaddr$prim59393 = alloca %struct.ScmObj*, align 8
%current_45args57713 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57712)
store volatile %struct.ScmObj* %current_45args57713, %struct.ScmObj** %stackaddr$prim59393, align 8
%stackaddr$prim59394 = alloca %struct.ScmObj*, align 8
%lsts_4348146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57713)
store volatile %struct.ScmObj* %lsts_4348146, %struct.ScmObj** %stackaddr$prim59394, align 8
%stackaddr$makeclosure59395 = alloca %struct.ScmObj*, align 8
%fptrToInt59396 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49875 to i64
%ae49875 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt59396)
store volatile %struct.ScmObj* %ae49875, %struct.ScmObj** %stackaddr$makeclosure59395, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49875, %struct.ScmObj* %lsts48139, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49875, %struct.ScmObj* %_37foldr48059, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49875, %struct.ScmObj* %_37foldl48137, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49875, %struct.ScmObj* %_37map148085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49875, %struct.ScmObj* %lsts_4348146, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49875, %struct.ScmObj* %k48385, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49875, %struct.ScmObj* %f48141, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49875, %struct.ScmObj* %acc48140, i64 7)
%ae49876 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59397 = alloca %struct.ScmObj*, align 8
%fptrToInt59398 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49877 to i64
%ae49877 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt59398)
store volatile %struct.ScmObj* %ae49877, %struct.ScmObj** %stackaddr$makeclosure59397, align 8
%argslist57742$ae498750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59399 = alloca %struct.ScmObj*, align 8
%argslist57742$ae498751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49877, %struct.ScmObj* %argslist57742$ae498750)
store volatile %struct.ScmObj* %argslist57742$ae498751, %struct.ScmObj** %stackaddr$prim59399, align 8
%stackaddr$prim59400 = alloca %struct.ScmObj*, align 8
%argslist57742$ae498752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49876, %struct.ScmObj* %argslist57742$ae498751)
store volatile %struct.ScmObj* %argslist57742$ae498752, %struct.ScmObj** %stackaddr$prim59400, align 8
%clofunc59401 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49875)
musttail call tailcc void %clofunc59401(%struct.ScmObj* %ae49875, %struct.ScmObj* %argslist57742$ae498752)
ret void
}

define tailcc void @proc_clo$ae49875(%struct.ScmObj* %env$ae49875,%struct.ScmObj* %current_45args57715) {
%stackaddr$env-ref59402 = alloca %struct.ScmObj*, align 8
%lsts48139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49875, i64 0)
store %struct.ScmObj* %lsts48139, %struct.ScmObj** %stackaddr$env-ref59402
%stackaddr$env-ref59403 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49875, i64 1)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref59403
%stackaddr$env-ref59404 = alloca %struct.ScmObj*, align 8
%_37foldl48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49875, i64 2)
store %struct.ScmObj* %_37foldl48137, %struct.ScmObj** %stackaddr$env-ref59404
%stackaddr$env-ref59405 = alloca %struct.ScmObj*, align 8
%_37map148085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49875, i64 3)
store %struct.ScmObj* %_37map148085, %struct.ScmObj** %stackaddr$env-ref59405
%stackaddr$env-ref59406 = alloca %struct.ScmObj*, align 8
%lsts_4348146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49875, i64 4)
store %struct.ScmObj* %lsts_4348146, %struct.ScmObj** %stackaddr$env-ref59406
%stackaddr$env-ref59407 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49875, i64 5)
store %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$env-ref59407
%stackaddr$env-ref59408 = alloca %struct.ScmObj*, align 8
%f48141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49875, i64 6)
store %struct.ScmObj* %f48141, %struct.ScmObj** %stackaddr$env-ref59408
%stackaddr$env-ref59409 = alloca %struct.ScmObj*, align 8
%acc48140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49875, i64 7)
store %struct.ScmObj* %acc48140, %struct.ScmObj** %stackaddr$env-ref59409
%stackaddr$prim59410 = alloca %struct.ScmObj*, align 8
%_95k48390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57715)
store volatile %struct.ScmObj* %_95k48390, %struct.ScmObj** %stackaddr$prim59410, align 8
%stackaddr$prim59411 = alloca %struct.ScmObj*, align 8
%current_45args57716 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57715)
store volatile %struct.ScmObj* %current_45args57716, %struct.ScmObj** %stackaddr$prim59411, align 8
%stackaddr$prim59412 = alloca %struct.ScmObj*, align 8
%anf_45bind48224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57716)
store volatile %struct.ScmObj* %anf_45bind48224, %struct.ScmObj** %stackaddr$prim59412, align 8
%stackaddr$makeclosure59413 = alloca %struct.ScmObj*, align 8
%fptrToInt59414 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49896 to i64
%ae49896 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt59414)
store volatile %struct.ScmObj* %ae49896, %struct.ScmObj** %stackaddr$makeclosure59413, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49896, %struct.ScmObj* %_37foldl48137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49896, %struct.ScmObj* %lsts_4348146, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49896, %struct.ScmObj* %k48385, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49896, %struct.ScmObj* %f48141, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49896, %struct.ScmObj* %acc48140, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49896, %struct.ScmObj* %_37foldr48059, i64 5)
%argslist57737$_37map1480850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59415 = alloca %struct.ScmObj*, align 8
%argslist57737$_37map1480851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48139, %struct.ScmObj* %argslist57737$_37map1480850)
store volatile %struct.ScmObj* %argslist57737$_37map1480851, %struct.ScmObj** %stackaddr$prim59415, align 8
%stackaddr$prim59416 = alloca %struct.ScmObj*, align 8
%argslist57737$_37map1480852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48224, %struct.ScmObj* %argslist57737$_37map1480851)
store volatile %struct.ScmObj* %argslist57737$_37map1480852, %struct.ScmObj** %stackaddr$prim59416, align 8
%stackaddr$prim59417 = alloca %struct.ScmObj*, align 8
%argslist57737$_37map1480853 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49896, %struct.ScmObj* %argslist57737$_37map1480852)
store volatile %struct.ScmObj* %argslist57737$_37map1480853, %struct.ScmObj** %stackaddr$prim59417, align 8
%clofunc59418 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148085)
musttail call tailcc void %clofunc59418(%struct.ScmObj* %_37map148085, %struct.ScmObj* %argslist57737$_37map1480853)
ret void
}

define tailcc void @proc_clo$ae49896(%struct.ScmObj* %env$ae49896,%struct.ScmObj* %current_45args57718) {
%stackaddr$env-ref59419 = alloca %struct.ScmObj*, align 8
%_37foldl48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49896, i64 0)
store %struct.ScmObj* %_37foldl48137, %struct.ScmObj** %stackaddr$env-ref59419
%stackaddr$env-ref59420 = alloca %struct.ScmObj*, align 8
%lsts_4348146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49896, i64 1)
store %struct.ScmObj* %lsts_4348146, %struct.ScmObj** %stackaddr$env-ref59420
%stackaddr$env-ref59421 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49896, i64 2)
store %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$env-ref59421
%stackaddr$env-ref59422 = alloca %struct.ScmObj*, align 8
%f48141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49896, i64 3)
store %struct.ScmObj* %f48141, %struct.ScmObj** %stackaddr$env-ref59422
%stackaddr$env-ref59423 = alloca %struct.ScmObj*, align 8
%acc48140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49896, i64 4)
store %struct.ScmObj* %acc48140, %struct.ScmObj** %stackaddr$env-ref59423
%stackaddr$env-ref59424 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49896, i64 5)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref59424
%stackaddr$prim59425 = alloca %struct.ScmObj*, align 8
%_95k48391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57718)
store volatile %struct.ScmObj* %_95k48391, %struct.ScmObj** %stackaddr$prim59425, align 8
%stackaddr$prim59426 = alloca %struct.ScmObj*, align 8
%current_45args57719 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57718)
store volatile %struct.ScmObj* %current_45args57719, %struct.ScmObj** %stackaddr$prim59426, align 8
%stackaddr$prim59427 = alloca %struct.ScmObj*, align 8
%vs48144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57719)
store volatile %struct.ScmObj* %vs48144, %struct.ScmObj** %stackaddr$prim59427, align 8
%stackaddr$makeclosure59428 = alloca %struct.ScmObj*, align 8
%fptrToInt59429 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49899 to i64
%ae49899 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt59429)
store volatile %struct.ScmObj* %ae49899, %struct.ScmObj** %stackaddr$makeclosure59428, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49899, %struct.ScmObj* %_37foldl48137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49899, %struct.ScmObj* %lsts_4348146, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49899, %struct.ScmObj* %k48385, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49899, %struct.ScmObj* %vs48144, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49899, %struct.ScmObj* %f48141, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49899, %struct.ScmObj* %acc48140, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49899, %struct.ScmObj* %_37foldr48059, i64 6)
%ae49900 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59430 = alloca %struct.ScmObj*, align 8
%fptrToInt59431 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49901 to i64
%ae49901 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt59431)
store volatile %struct.ScmObj* %ae49901, %struct.ScmObj** %stackaddr$makeclosure59430, align 8
%argslist57736$ae498990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59432 = alloca %struct.ScmObj*, align 8
%argslist57736$ae498991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49901, %struct.ScmObj* %argslist57736$ae498990)
store volatile %struct.ScmObj* %argslist57736$ae498991, %struct.ScmObj** %stackaddr$prim59432, align 8
%stackaddr$prim59433 = alloca %struct.ScmObj*, align 8
%argslist57736$ae498992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49900, %struct.ScmObj* %argslist57736$ae498991)
store volatile %struct.ScmObj* %argslist57736$ae498992, %struct.ScmObj** %stackaddr$prim59433, align 8
%clofunc59434 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49899)
musttail call tailcc void %clofunc59434(%struct.ScmObj* %ae49899, %struct.ScmObj* %argslist57736$ae498992)
ret void
}

define tailcc void @proc_clo$ae49899(%struct.ScmObj* %env$ae49899,%struct.ScmObj* %current_45args57721) {
%stackaddr$env-ref59435 = alloca %struct.ScmObj*, align 8
%_37foldl48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49899, i64 0)
store %struct.ScmObj* %_37foldl48137, %struct.ScmObj** %stackaddr$env-ref59435
%stackaddr$env-ref59436 = alloca %struct.ScmObj*, align 8
%lsts_4348146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49899, i64 1)
store %struct.ScmObj* %lsts_4348146, %struct.ScmObj** %stackaddr$env-ref59436
%stackaddr$env-ref59437 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49899, i64 2)
store %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$env-ref59437
%stackaddr$env-ref59438 = alloca %struct.ScmObj*, align 8
%vs48144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49899, i64 3)
store %struct.ScmObj* %vs48144, %struct.ScmObj** %stackaddr$env-ref59438
%stackaddr$env-ref59439 = alloca %struct.ScmObj*, align 8
%f48141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49899, i64 4)
store %struct.ScmObj* %f48141, %struct.ScmObj** %stackaddr$env-ref59439
%stackaddr$env-ref59440 = alloca %struct.ScmObj*, align 8
%acc48140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49899, i64 5)
store %struct.ScmObj* %acc48140, %struct.ScmObj** %stackaddr$env-ref59440
%stackaddr$env-ref59441 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49899, i64 6)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref59441
%stackaddr$prim59442 = alloca %struct.ScmObj*, align 8
%_95k48392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57721)
store volatile %struct.ScmObj* %_95k48392, %struct.ScmObj** %stackaddr$prim59442, align 8
%stackaddr$prim59443 = alloca %struct.ScmObj*, align 8
%current_45args57722 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57721)
store volatile %struct.ScmObj* %current_45args57722, %struct.ScmObj** %stackaddr$prim59443, align 8
%stackaddr$prim59444 = alloca %struct.ScmObj*, align 8
%anf_45bind48225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57722)
store volatile %struct.ScmObj* %anf_45bind48225, %struct.ScmObj** %stackaddr$prim59444, align 8
%ae49922 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59445 = alloca %struct.ScmObj*, align 8
%anf_45bind48226 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48140, %struct.ScmObj* %ae49922)
store volatile %struct.ScmObj* %anf_45bind48226, %struct.ScmObj** %stackaddr$prim59445, align 8
%stackaddr$makeclosure59446 = alloca %struct.ScmObj*, align 8
%fptrToInt59447 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49924 to i64
%ae49924 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt59447)
store volatile %struct.ScmObj* %ae49924, %struct.ScmObj** %stackaddr$makeclosure59446, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49924, %struct.ScmObj* %_37foldl48137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49924, %struct.ScmObj* %lsts_4348146, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49924, %struct.ScmObj* %k48385, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49924, %struct.ScmObj* %f48141, i64 3)
%argslist57730$_37foldr480590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59448 = alloca %struct.ScmObj*, align 8
%argslist57730$_37foldr480591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48144, %struct.ScmObj* %argslist57730$_37foldr480590)
store volatile %struct.ScmObj* %argslist57730$_37foldr480591, %struct.ScmObj** %stackaddr$prim59448, align 8
%stackaddr$prim59449 = alloca %struct.ScmObj*, align 8
%argslist57730$_37foldr480592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48226, %struct.ScmObj* %argslist57730$_37foldr480591)
store volatile %struct.ScmObj* %argslist57730$_37foldr480592, %struct.ScmObj** %stackaddr$prim59449, align 8
%stackaddr$prim59450 = alloca %struct.ScmObj*, align 8
%argslist57730$_37foldr480593 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48225, %struct.ScmObj* %argslist57730$_37foldr480592)
store volatile %struct.ScmObj* %argslist57730$_37foldr480593, %struct.ScmObj** %stackaddr$prim59450, align 8
%stackaddr$prim59451 = alloca %struct.ScmObj*, align 8
%argslist57730$_37foldr480594 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49924, %struct.ScmObj* %argslist57730$_37foldr480593)
store volatile %struct.ScmObj* %argslist57730$_37foldr480594, %struct.ScmObj** %stackaddr$prim59451, align 8
%clofunc59452 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48059)
musttail call tailcc void %clofunc59452(%struct.ScmObj* %_37foldr48059, %struct.ScmObj* %argslist57730$_37foldr480594)
ret void
}

define tailcc void @proc_clo$ae49924(%struct.ScmObj* %env$ae49924,%struct.ScmObj* %current_45args57724) {
%stackaddr$env-ref59453 = alloca %struct.ScmObj*, align 8
%_37foldl48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49924, i64 0)
store %struct.ScmObj* %_37foldl48137, %struct.ScmObj** %stackaddr$env-ref59453
%stackaddr$env-ref59454 = alloca %struct.ScmObj*, align 8
%lsts_4348146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49924, i64 1)
store %struct.ScmObj* %lsts_4348146, %struct.ScmObj** %stackaddr$env-ref59454
%stackaddr$env-ref59455 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49924, i64 2)
store %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$env-ref59455
%stackaddr$env-ref59456 = alloca %struct.ScmObj*, align 8
%f48141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49924, i64 3)
store %struct.ScmObj* %f48141, %struct.ScmObj** %stackaddr$env-ref59456
%stackaddr$prim59457 = alloca %struct.ScmObj*, align 8
%_95k48393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57724)
store volatile %struct.ScmObj* %_95k48393, %struct.ScmObj** %stackaddr$prim59457, align 8
%stackaddr$prim59458 = alloca %struct.ScmObj*, align 8
%current_45args57725 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57724)
store volatile %struct.ScmObj* %current_45args57725, %struct.ScmObj** %stackaddr$prim59458, align 8
%stackaddr$prim59459 = alloca %struct.ScmObj*, align 8
%anf_45bind48227 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57725)
store volatile %struct.ScmObj* %anf_45bind48227, %struct.ScmObj** %stackaddr$prim59459, align 8
%stackaddr$makeclosure59460 = alloca %struct.ScmObj*, align 8
%fptrToInt59461 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49928 to i64
%ae49928 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt59461)
store volatile %struct.ScmObj* %ae49928, %struct.ScmObj** %stackaddr$makeclosure59460, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49928, %struct.ScmObj* %_37foldl48137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49928, %struct.ScmObj* %lsts_4348146, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49928, %struct.ScmObj* %k48385, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49928, %struct.ScmObj* %f48141, i64 3)
%stackaddr$prim59462 = alloca %struct.ScmObj*, align 8
%cpsargs48396 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49928, %struct.ScmObj* %anf_45bind48227)
store volatile %struct.ScmObj* %cpsargs48396, %struct.ScmObj** %stackaddr$prim59462, align 8
%clofunc59463 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48141)
musttail call tailcc void %clofunc59463(%struct.ScmObj* %f48141, %struct.ScmObj* %cpsargs48396)
ret void
}

define tailcc void @proc_clo$ae49928(%struct.ScmObj* %env$ae49928,%struct.ScmObj* %current_45args57727) {
%stackaddr$env-ref59464 = alloca %struct.ScmObj*, align 8
%_37foldl48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49928, i64 0)
store %struct.ScmObj* %_37foldl48137, %struct.ScmObj** %stackaddr$env-ref59464
%stackaddr$env-ref59465 = alloca %struct.ScmObj*, align 8
%lsts_4348146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49928, i64 1)
store %struct.ScmObj* %lsts_4348146, %struct.ScmObj** %stackaddr$env-ref59465
%stackaddr$env-ref59466 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49928, i64 2)
store %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$env-ref59466
%stackaddr$env-ref59467 = alloca %struct.ScmObj*, align 8
%f48141 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49928, i64 3)
store %struct.ScmObj* %f48141, %struct.ScmObj** %stackaddr$env-ref59467
%stackaddr$prim59468 = alloca %struct.ScmObj*, align 8
%_95k48394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57727)
store volatile %struct.ScmObj* %_95k48394, %struct.ScmObj** %stackaddr$prim59468, align 8
%stackaddr$prim59469 = alloca %struct.ScmObj*, align 8
%current_45args57728 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57727)
store volatile %struct.ScmObj* %current_45args57728, %struct.ScmObj** %stackaddr$prim59469, align 8
%stackaddr$prim59470 = alloca %struct.ScmObj*, align 8
%acc_4348148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57728)
store volatile %struct.ScmObj* %acc_4348148, %struct.ScmObj** %stackaddr$prim59470, align 8
%stackaddr$prim59471 = alloca %struct.ScmObj*, align 8
%anf_45bind48228 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4348148, %struct.ScmObj* %lsts_4348146)
store volatile %struct.ScmObj* %anf_45bind48228, %struct.ScmObj** %stackaddr$prim59471, align 8
%stackaddr$prim59472 = alloca %struct.ScmObj*, align 8
%anf_45bind48229 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48141, %struct.ScmObj* %anf_45bind48228)
store volatile %struct.ScmObj* %anf_45bind48229, %struct.ScmObj** %stackaddr$prim59472, align 8
%stackaddr$prim59473 = alloca %struct.ScmObj*, align 8
%cpsargs48395 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48385, %struct.ScmObj* %anf_45bind48229)
store volatile %struct.ScmObj* %cpsargs48395, %struct.ScmObj** %stackaddr$prim59473, align 8
%clofunc59474 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl48137)
musttail call tailcc void %clofunc59474(%struct.ScmObj* %_37foldl48137, %struct.ScmObj* %cpsargs48395)
ret void
}

define tailcc void @proc_clo$ae49901(%struct.ScmObj* %env$ae49901,%struct.ScmObj* %current_45args57731) {
%stackaddr$prim59475 = alloca %struct.ScmObj*, align 8
%k48397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57731)
store volatile %struct.ScmObj* %k48397, %struct.ScmObj** %stackaddr$prim59475, align 8
%stackaddr$prim59476 = alloca %struct.ScmObj*, align 8
%current_45args57732 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57731)
store volatile %struct.ScmObj* %current_45args57732, %struct.ScmObj** %stackaddr$prim59476, align 8
%stackaddr$prim59477 = alloca %struct.ScmObj*, align 8
%a48150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57732)
store volatile %struct.ScmObj* %a48150, %struct.ScmObj** %stackaddr$prim59477, align 8
%stackaddr$prim59478 = alloca %struct.ScmObj*, align 8
%current_45args57733 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57732)
store volatile %struct.ScmObj* %current_45args57733, %struct.ScmObj** %stackaddr$prim59478, align 8
%stackaddr$prim59479 = alloca %struct.ScmObj*, align 8
%b48149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57733)
store volatile %struct.ScmObj* %b48149, %struct.ScmObj** %stackaddr$prim59479, align 8
%stackaddr$prim59480 = alloca %struct.ScmObj*, align 8
%cpsprim48398 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48150, %struct.ScmObj* %b48149)
store volatile %struct.ScmObj* %cpsprim48398, %struct.ScmObj** %stackaddr$prim59480, align 8
%ae49905 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57735$k483970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59481 = alloca %struct.ScmObj*, align 8
%argslist57735$k483971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48398, %struct.ScmObj* %argslist57735$k483970)
store volatile %struct.ScmObj* %argslist57735$k483971, %struct.ScmObj** %stackaddr$prim59481, align 8
%stackaddr$prim59482 = alloca %struct.ScmObj*, align 8
%argslist57735$k483972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49905, %struct.ScmObj* %argslist57735$k483971)
store volatile %struct.ScmObj* %argslist57735$k483972, %struct.ScmObj** %stackaddr$prim59482, align 8
%clofunc59483 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48397)
musttail call tailcc void %clofunc59483(%struct.ScmObj* %k48397, %struct.ScmObj* %argslist57735$k483972)
ret void
}

define tailcc void @proc_clo$ae49877(%struct.ScmObj* %env$ae49877,%struct.ScmObj* %current_45args57738) {
%stackaddr$prim59484 = alloca %struct.ScmObj*, align 8
%k48399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57738)
store volatile %struct.ScmObj* %k48399, %struct.ScmObj** %stackaddr$prim59484, align 8
%stackaddr$prim59485 = alloca %struct.ScmObj*, align 8
%current_45args57739 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57738)
store volatile %struct.ScmObj* %current_45args57739, %struct.ScmObj** %stackaddr$prim59485, align 8
%stackaddr$prim59486 = alloca %struct.ScmObj*, align 8
%x48145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57739)
store volatile %struct.ScmObj* %x48145, %struct.ScmObj** %stackaddr$prim59486, align 8
%stackaddr$prim59487 = alloca %struct.ScmObj*, align 8
%cpsprim48400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48145)
store volatile %struct.ScmObj* %cpsprim48400, %struct.ScmObj** %stackaddr$prim59487, align 8
%ae49880 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57741$k483990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59488 = alloca %struct.ScmObj*, align 8
%argslist57741$k483991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48400, %struct.ScmObj* %argslist57741$k483990)
store volatile %struct.ScmObj* %argslist57741$k483991, %struct.ScmObj** %stackaddr$prim59488, align 8
%stackaddr$prim59489 = alloca %struct.ScmObj*, align 8
%argslist57741$k483992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49880, %struct.ScmObj* %argslist57741$k483991)
store volatile %struct.ScmObj* %argslist57741$k483992, %struct.ScmObj** %stackaddr$prim59489, align 8
%clofunc59490 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48399)
musttail call tailcc void %clofunc59490(%struct.ScmObj* %k48399, %struct.ScmObj* %argslist57741$k483992)
ret void
}

define tailcc void @proc_clo$ae49853(%struct.ScmObj* %env$ae49853,%struct.ScmObj* %current_45args57744) {
%stackaddr$prim59491 = alloca %struct.ScmObj*, align 8
%k48401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57744)
store volatile %struct.ScmObj* %k48401, %struct.ScmObj** %stackaddr$prim59491, align 8
%stackaddr$prim59492 = alloca %struct.ScmObj*, align 8
%current_45args57745 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57744)
store volatile %struct.ScmObj* %current_45args57745, %struct.ScmObj** %stackaddr$prim59492, align 8
%stackaddr$prim59493 = alloca %struct.ScmObj*, align 8
%x48147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57745)
store volatile %struct.ScmObj* %x48147, %struct.ScmObj** %stackaddr$prim59493, align 8
%stackaddr$prim59494 = alloca %struct.ScmObj*, align 8
%cpsprim48402 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48147)
store volatile %struct.ScmObj* %cpsprim48402, %struct.ScmObj** %stackaddr$prim59494, align 8
%ae49856 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57747$k484010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59495 = alloca %struct.ScmObj*, align 8
%argslist57747$k484011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48402, %struct.ScmObj* %argslist57747$k484010)
store volatile %struct.ScmObj* %argslist57747$k484011, %struct.ScmObj** %stackaddr$prim59495, align 8
%stackaddr$prim59496 = alloca %struct.ScmObj*, align 8
%argslist57747$k484012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49856, %struct.ScmObj* %argslist57747$k484011)
store volatile %struct.ScmObj* %argslist57747$k484012, %struct.ScmObj** %stackaddr$prim59496, align 8
%clofunc59497 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48401)
musttail call tailcc void %clofunc59497(%struct.ScmObj* %k48401, %struct.ScmObj* %argslist57747$k484012)
ret void
}

define tailcc void @proc_clo$ae49805(%struct.ScmObj* %env$ae49805,%struct.ScmObj* %current_45args57750) {
%stackaddr$prim59498 = alloca %struct.ScmObj*, align 8
%k48403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57750)
store volatile %struct.ScmObj* %k48403, %struct.ScmObj** %stackaddr$prim59498, align 8
%stackaddr$prim59499 = alloca %struct.ScmObj*, align 8
%current_45args57751 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57750)
store volatile %struct.ScmObj* %current_45args57751, %struct.ScmObj** %stackaddr$prim59499, align 8
%stackaddr$prim59500 = alloca %struct.ScmObj*, align 8
%lst48143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57751)
store volatile %struct.ScmObj* %lst48143, %struct.ScmObj** %stackaddr$prim59500, align 8
%stackaddr$prim59501 = alloca %struct.ScmObj*, align 8
%current_45args57752 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57751)
store volatile %struct.ScmObj* %current_45args57752, %struct.ScmObj** %stackaddr$prim59501, align 8
%stackaddr$prim59502 = alloca %struct.ScmObj*, align 8
%b48142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57752)
store volatile %struct.ScmObj* %b48142, %struct.ScmObj** %stackaddr$prim59502, align 8
%truthy$cmp59503 = call i64 @is_truthy_value(%struct.ScmObj* %b48142)
%cmp$cmp59503 = icmp eq i64 %truthy$cmp59503, 1
br i1 %cmp$cmp59503, label %truebranch$cmp59503, label %falsebranch$cmp59503
truebranch$cmp59503:
%ae49808 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57754$k484030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59504 = alloca %struct.ScmObj*, align 8
%argslist57754$k484031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48142, %struct.ScmObj* %argslist57754$k484030)
store volatile %struct.ScmObj* %argslist57754$k484031, %struct.ScmObj** %stackaddr$prim59504, align 8
%stackaddr$prim59505 = alloca %struct.ScmObj*, align 8
%argslist57754$k484032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49808, %struct.ScmObj* %argslist57754$k484031)
store volatile %struct.ScmObj* %argslist57754$k484032, %struct.ScmObj** %stackaddr$prim59505, align 8
%clofunc59506 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48403)
musttail call tailcc void %clofunc59506(%struct.ScmObj* %k48403, %struct.ScmObj* %argslist57754$k484032)
ret void
falsebranch$cmp59503:
%stackaddr$prim59507 = alloca %struct.ScmObj*, align 8
%cpsprim48404 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48143)
store volatile %struct.ScmObj* %cpsprim48404, %struct.ScmObj** %stackaddr$prim59507, align 8
%ae49815 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57755$k484030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59508 = alloca %struct.ScmObj*, align 8
%argslist57755$k484031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48404, %struct.ScmObj* %argslist57755$k484030)
store volatile %struct.ScmObj* %argslist57755$k484031, %struct.ScmObj** %stackaddr$prim59508, align 8
%stackaddr$prim59509 = alloca %struct.ScmObj*, align 8
%argslist57755$k484032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49815, %struct.ScmObj* %argslist57755$k484031)
store volatile %struct.ScmObj* %argslist57755$k484032, %struct.ScmObj** %stackaddr$prim59509, align 8
%clofunc59510 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48403)
musttail call tailcc void %clofunc59510(%struct.ScmObj* %k48403, %struct.ScmObj* %argslist57755$k484032)
ret void
}

define tailcc void @proc_clo$ae49646(%struct.ScmObj* %env$ae49646,%struct.ScmObj* %args4808148405) {
%stackaddr$env-ref59511 = alloca %struct.ScmObj*, align 8
%_37drop_45right48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49646, i64 0)
store %struct.ScmObj* %_37drop_45right48073, %struct.ScmObj** %stackaddr$env-ref59511
%stackaddr$env-ref59512 = alloca %struct.ScmObj*, align 8
%_37last48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49646, i64 1)
store %struct.ScmObj* %_37last48076, %struct.ScmObj** %stackaddr$env-ref59512
%stackaddr$env-ref59513 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49646, i64 2)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref59513
%stackaddr$prim59514 = alloca %struct.ScmObj*, align 8
%k48406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4808148405)
store volatile %struct.ScmObj* %k48406, %struct.ScmObj** %stackaddr$prim59514, align 8
%stackaddr$prim59515 = alloca %struct.ScmObj*, align 8
%args48081 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4808148405)
store volatile %struct.ScmObj* %args48081, %struct.ScmObj** %stackaddr$prim59515, align 8
%stackaddr$prim59516 = alloca %struct.ScmObj*, align 8
%f48083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48081)
store volatile %struct.ScmObj* %f48083, %struct.ScmObj** %stackaddr$prim59516, align 8
%stackaddr$prim59517 = alloca %struct.ScmObj*, align 8
%lsts48082 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48081)
store volatile %struct.ScmObj* %lsts48082, %struct.ScmObj** %stackaddr$prim59517, align 8
%stackaddr$makeclosure59518 = alloca %struct.ScmObj*, align 8
%fptrToInt59519 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49651 to i64
%ae49651 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt59519)
store volatile %struct.ScmObj* %ae49651, %struct.ScmObj** %stackaddr$makeclosure59518, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49651, %struct.ScmObj* %k48406, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49651, %struct.ScmObj* %lsts48082, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49651, %struct.ScmObj* %_37foldr48059, i64 2)
%ae49652 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59520 = alloca %struct.ScmObj*, align 8
%fptrToInt59521 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49653 to i64
%ae49653 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt59521)
store volatile %struct.ScmObj* %ae49653, %struct.ScmObj** %stackaddr$makeclosure59520, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49653, %struct.ScmObj* %_37drop_45right48073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49653, %struct.ScmObj* %f48083, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49653, %struct.ScmObj* %_37last48076, i64 2)
%argslist57774$ae496510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59522 = alloca %struct.ScmObj*, align 8
%argslist57774$ae496511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49653, %struct.ScmObj* %argslist57774$ae496510)
store volatile %struct.ScmObj* %argslist57774$ae496511, %struct.ScmObj** %stackaddr$prim59522, align 8
%stackaddr$prim59523 = alloca %struct.ScmObj*, align 8
%argslist57774$ae496512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49652, %struct.ScmObj* %argslist57774$ae496511)
store volatile %struct.ScmObj* %argslist57774$ae496512, %struct.ScmObj** %stackaddr$prim59523, align 8
%clofunc59524 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49651)
musttail call tailcc void %clofunc59524(%struct.ScmObj* %ae49651, %struct.ScmObj* %argslist57774$ae496512)
ret void
}

define tailcc void @proc_clo$ae49651(%struct.ScmObj* %env$ae49651,%struct.ScmObj* %current_45args57759) {
%stackaddr$env-ref59525 = alloca %struct.ScmObj*, align 8
%k48406 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49651, i64 0)
store %struct.ScmObj* %k48406, %struct.ScmObj** %stackaddr$env-ref59525
%stackaddr$env-ref59526 = alloca %struct.ScmObj*, align 8
%lsts48082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49651, i64 1)
store %struct.ScmObj* %lsts48082, %struct.ScmObj** %stackaddr$env-ref59526
%stackaddr$env-ref59527 = alloca %struct.ScmObj*, align 8
%_37foldr48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49651, i64 2)
store %struct.ScmObj* %_37foldr48059, %struct.ScmObj** %stackaddr$env-ref59527
%stackaddr$prim59528 = alloca %struct.ScmObj*, align 8
%_95k48407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57759)
store volatile %struct.ScmObj* %_95k48407, %struct.ScmObj** %stackaddr$prim59528, align 8
%stackaddr$prim59529 = alloca %struct.ScmObj*, align 8
%current_45args57760 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57759)
store volatile %struct.ScmObj* %current_45args57760, %struct.ScmObj** %stackaddr$prim59529, align 8
%stackaddr$prim59530 = alloca %struct.ScmObj*, align 8
%anf_45bind48216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57760)
store volatile %struct.ScmObj* %anf_45bind48216, %struct.ScmObj** %stackaddr$prim59530, align 8
%ae49714 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59531 = alloca %struct.ScmObj*, align 8
%anf_45bind48217 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49714, %struct.ScmObj* %lsts48082)
store volatile %struct.ScmObj* %anf_45bind48217, %struct.ScmObj** %stackaddr$prim59531, align 8
%stackaddr$prim59532 = alloca %struct.ScmObj*, align 8
%anf_45bind48218 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48216, %struct.ScmObj* %anf_45bind48217)
store volatile %struct.ScmObj* %anf_45bind48218, %struct.ScmObj** %stackaddr$prim59532, align 8
%stackaddr$prim59533 = alloca %struct.ScmObj*, align 8
%cpsargs48408 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48406, %struct.ScmObj* %anf_45bind48218)
store volatile %struct.ScmObj* %cpsargs48408, %struct.ScmObj** %stackaddr$prim59533, align 8
%clofunc59534 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48059)
musttail call tailcc void %clofunc59534(%struct.ScmObj* %_37foldr48059, %struct.ScmObj* %cpsargs48408)
ret void
}

define tailcc void @proc_clo$ae49653(%struct.ScmObj* %env$ae49653,%struct.ScmObj* %fargs4808448409) {
%stackaddr$env-ref59535 = alloca %struct.ScmObj*, align 8
%_37drop_45right48073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49653, i64 0)
store %struct.ScmObj* %_37drop_45right48073, %struct.ScmObj** %stackaddr$env-ref59535
%stackaddr$env-ref59536 = alloca %struct.ScmObj*, align 8
%f48083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49653, i64 1)
store %struct.ScmObj* %f48083, %struct.ScmObj** %stackaddr$env-ref59536
%stackaddr$env-ref59537 = alloca %struct.ScmObj*, align 8
%_37last48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49653, i64 2)
store %struct.ScmObj* %_37last48076, %struct.ScmObj** %stackaddr$env-ref59537
%stackaddr$prim59538 = alloca %struct.ScmObj*, align 8
%k48410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4808448409)
store volatile %struct.ScmObj* %k48410, %struct.ScmObj** %stackaddr$prim59538, align 8
%stackaddr$prim59539 = alloca %struct.ScmObj*, align 8
%fargs48084 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4808448409)
store volatile %struct.ScmObj* %fargs48084, %struct.ScmObj** %stackaddr$prim59539, align 8
%stackaddr$makeclosure59540 = alloca %struct.ScmObj*, align 8
%fptrToInt59541 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49657 to i64
%ae49657 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt59541)
store volatile %struct.ScmObj* %ae49657, %struct.ScmObj** %stackaddr$makeclosure59540, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49657, %struct.ScmObj* %fargs48084, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49657, %struct.ScmObj* %f48083, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49657, %struct.ScmObj* %_37last48076, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49657, %struct.ScmObj* %k48410, i64 3)
%ae49659 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist57773$_37drop_45right480730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59542 = alloca %struct.ScmObj*, align 8
%argslist57773$_37drop_45right480731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49659, %struct.ScmObj* %argslist57773$_37drop_45right480730)
store volatile %struct.ScmObj* %argslist57773$_37drop_45right480731, %struct.ScmObj** %stackaddr$prim59542, align 8
%stackaddr$prim59543 = alloca %struct.ScmObj*, align 8
%argslist57773$_37drop_45right480732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48084, %struct.ScmObj* %argslist57773$_37drop_45right480731)
store volatile %struct.ScmObj* %argslist57773$_37drop_45right480732, %struct.ScmObj** %stackaddr$prim59543, align 8
%stackaddr$prim59544 = alloca %struct.ScmObj*, align 8
%argslist57773$_37drop_45right480733 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49657, %struct.ScmObj* %argslist57773$_37drop_45right480732)
store volatile %struct.ScmObj* %argslist57773$_37drop_45right480733, %struct.ScmObj** %stackaddr$prim59544, align 8
%clofunc59545 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right48073)
musttail call tailcc void %clofunc59545(%struct.ScmObj* %_37drop_45right48073, %struct.ScmObj* %argslist57773$_37drop_45right480733)
ret void
}

define tailcc void @proc_clo$ae49657(%struct.ScmObj* %env$ae49657,%struct.ScmObj* %current_45args57762) {
%stackaddr$env-ref59546 = alloca %struct.ScmObj*, align 8
%fargs48084 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49657, i64 0)
store %struct.ScmObj* %fargs48084, %struct.ScmObj** %stackaddr$env-ref59546
%stackaddr$env-ref59547 = alloca %struct.ScmObj*, align 8
%f48083 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49657, i64 1)
store %struct.ScmObj* %f48083, %struct.ScmObj** %stackaddr$env-ref59547
%stackaddr$env-ref59548 = alloca %struct.ScmObj*, align 8
%_37last48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49657, i64 2)
store %struct.ScmObj* %_37last48076, %struct.ScmObj** %stackaddr$env-ref59548
%stackaddr$env-ref59549 = alloca %struct.ScmObj*, align 8
%k48410 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49657, i64 3)
store %struct.ScmObj* %k48410, %struct.ScmObj** %stackaddr$env-ref59549
%stackaddr$prim59550 = alloca %struct.ScmObj*, align 8
%_95k48411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57762)
store volatile %struct.ScmObj* %_95k48411, %struct.ScmObj** %stackaddr$prim59550, align 8
%stackaddr$prim59551 = alloca %struct.ScmObj*, align 8
%current_45args57763 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57762)
store volatile %struct.ScmObj* %current_45args57763, %struct.ScmObj** %stackaddr$prim59551, align 8
%stackaddr$prim59552 = alloca %struct.ScmObj*, align 8
%anf_45bind48213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57763)
store volatile %struct.ScmObj* %anf_45bind48213, %struct.ScmObj** %stackaddr$prim59552, align 8
%stackaddr$makeclosure59553 = alloca %struct.ScmObj*, align 8
%fptrToInt59554 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49664 to i64
%ae49664 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt59554)
store volatile %struct.ScmObj* %ae49664, %struct.ScmObj** %stackaddr$makeclosure59553, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49664, %struct.ScmObj* %fargs48084, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49664, %struct.ScmObj* %_37last48076, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49664, %struct.ScmObj* %k48410, i64 2)
%stackaddr$prim59555 = alloca %struct.ScmObj*, align 8
%cpsargs48415 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49664, %struct.ScmObj* %anf_45bind48213)
store volatile %struct.ScmObj* %cpsargs48415, %struct.ScmObj** %stackaddr$prim59555, align 8
%clofunc59556 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48083)
musttail call tailcc void %clofunc59556(%struct.ScmObj* %f48083, %struct.ScmObj* %cpsargs48415)
ret void
}

define tailcc void @proc_clo$ae49664(%struct.ScmObj* %env$ae49664,%struct.ScmObj* %current_45args57765) {
%stackaddr$env-ref59557 = alloca %struct.ScmObj*, align 8
%fargs48084 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49664, i64 0)
store %struct.ScmObj* %fargs48084, %struct.ScmObj** %stackaddr$env-ref59557
%stackaddr$env-ref59558 = alloca %struct.ScmObj*, align 8
%_37last48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49664, i64 1)
store %struct.ScmObj* %_37last48076, %struct.ScmObj** %stackaddr$env-ref59558
%stackaddr$env-ref59559 = alloca %struct.ScmObj*, align 8
%k48410 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49664, i64 2)
store %struct.ScmObj* %k48410, %struct.ScmObj** %stackaddr$env-ref59559
%stackaddr$prim59560 = alloca %struct.ScmObj*, align 8
%_95k48412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57765)
store volatile %struct.ScmObj* %_95k48412, %struct.ScmObj** %stackaddr$prim59560, align 8
%stackaddr$prim59561 = alloca %struct.ScmObj*, align 8
%current_45args57766 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57765)
store volatile %struct.ScmObj* %current_45args57766, %struct.ScmObj** %stackaddr$prim59561, align 8
%stackaddr$prim59562 = alloca %struct.ScmObj*, align 8
%anf_45bind48214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57766)
store volatile %struct.ScmObj* %anf_45bind48214, %struct.ScmObj** %stackaddr$prim59562, align 8
%stackaddr$makeclosure59563 = alloca %struct.ScmObj*, align 8
%fptrToInt59564 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49669 to i64
%ae49669 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59564)
store volatile %struct.ScmObj* %ae49669, %struct.ScmObj** %stackaddr$makeclosure59563, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49669, %struct.ScmObj* %anf_45bind48214, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49669, %struct.ScmObj* %k48410, i64 1)
%argslist57772$_37last480760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59565 = alloca %struct.ScmObj*, align 8
%argslist57772$_37last480761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48084, %struct.ScmObj* %argslist57772$_37last480760)
store volatile %struct.ScmObj* %argslist57772$_37last480761, %struct.ScmObj** %stackaddr$prim59565, align 8
%stackaddr$prim59566 = alloca %struct.ScmObj*, align 8
%argslist57772$_37last480762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49669, %struct.ScmObj* %argslist57772$_37last480761)
store volatile %struct.ScmObj* %argslist57772$_37last480762, %struct.ScmObj** %stackaddr$prim59566, align 8
%clofunc59567 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last48076)
musttail call tailcc void %clofunc59567(%struct.ScmObj* %_37last48076, %struct.ScmObj* %argslist57772$_37last480762)
ret void
}

define tailcc void @proc_clo$ae49669(%struct.ScmObj* %env$ae49669,%struct.ScmObj* %current_45args57768) {
%stackaddr$env-ref59568 = alloca %struct.ScmObj*, align 8
%anf_45bind48214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49669, i64 0)
store %struct.ScmObj* %anf_45bind48214, %struct.ScmObj** %stackaddr$env-ref59568
%stackaddr$env-ref59569 = alloca %struct.ScmObj*, align 8
%k48410 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49669, i64 1)
store %struct.ScmObj* %k48410, %struct.ScmObj** %stackaddr$env-ref59569
%stackaddr$prim59570 = alloca %struct.ScmObj*, align 8
%_95k48413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57768)
store volatile %struct.ScmObj* %_95k48413, %struct.ScmObj** %stackaddr$prim59570, align 8
%stackaddr$prim59571 = alloca %struct.ScmObj*, align 8
%current_45args57769 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57768)
store volatile %struct.ScmObj* %current_45args57769, %struct.ScmObj** %stackaddr$prim59571, align 8
%stackaddr$prim59572 = alloca %struct.ScmObj*, align 8
%anf_45bind48215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57769)
store volatile %struct.ScmObj* %anf_45bind48215, %struct.ScmObj** %stackaddr$prim59572, align 8
%stackaddr$prim59573 = alloca %struct.ScmObj*, align 8
%cpsprim48414 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48214, %struct.ScmObj* %anf_45bind48215)
store volatile %struct.ScmObj* %cpsprim48414, %struct.ScmObj** %stackaddr$prim59573, align 8
%ae49674 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57771$k484100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59574 = alloca %struct.ScmObj*, align 8
%argslist57771$k484101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48414, %struct.ScmObj* %argslist57771$k484100)
store volatile %struct.ScmObj* %argslist57771$k484101, %struct.ScmObj** %stackaddr$prim59574, align 8
%stackaddr$prim59575 = alloca %struct.ScmObj*, align 8
%argslist57771$k484102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49674, %struct.ScmObj* %argslist57771$k484101)
store volatile %struct.ScmObj* %argslist57771$k484102, %struct.ScmObj** %stackaddr$prim59575, align 8
%clofunc59576 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48410)
musttail call tailcc void %clofunc59576(%struct.ScmObj* %k48410, %struct.ScmObj* %argslist57771$k484102)
ret void
}

define tailcc void @proc_clo$ae49569(%struct.ScmObj* %env$ae49569,%struct.ScmObj* %current_45args57776) {
%stackaddr$env-ref59577 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49569, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref59577
%stackaddr$prim59578 = alloca %struct.ScmObj*, align 8
%k48416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57776)
store volatile %struct.ScmObj* %k48416, %struct.ScmObj** %stackaddr$prim59578, align 8
%stackaddr$prim59579 = alloca %struct.ScmObj*, align 8
%current_45args57777 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57776)
store volatile %struct.ScmObj* %current_45args57777, %struct.ScmObj** %stackaddr$prim59579, align 8
%stackaddr$prim59580 = alloca %struct.ScmObj*, align 8
%f48087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57777)
store volatile %struct.ScmObj* %f48087, %struct.ScmObj** %stackaddr$prim59580, align 8
%stackaddr$prim59581 = alloca %struct.ScmObj*, align 8
%current_45args57778 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57777)
store volatile %struct.ScmObj* %current_45args57778, %struct.ScmObj** %stackaddr$prim59581, align 8
%stackaddr$prim59582 = alloca %struct.ScmObj*, align 8
%lst48086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57778)
store volatile %struct.ScmObj* %lst48086, %struct.ScmObj** %stackaddr$prim59582, align 8
%stackaddr$makeclosure59583 = alloca %struct.ScmObj*, align 8
%fptrToInt59584 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49570 to i64
%ae49570 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt59584)
store volatile %struct.ScmObj* %ae49570, %struct.ScmObj** %stackaddr$makeclosure59583, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49570, %struct.ScmObj* %lst48086, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49570, %struct.ScmObj* %_37foldr148054, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49570, %struct.ScmObj* %k48416, i64 2)
%ae49571 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59585 = alloca %struct.ScmObj*, align 8
%fptrToInt59586 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49572 to i64
%ae49572 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt59586)
store volatile %struct.ScmObj* %ae49572, %struct.ScmObj** %stackaddr$makeclosure59585, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49572, %struct.ScmObj* %f48087, i64 0)
%argslist57793$ae495700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59587 = alloca %struct.ScmObj*, align 8
%argslist57793$ae495701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49572, %struct.ScmObj* %argslist57793$ae495700)
store volatile %struct.ScmObj* %argslist57793$ae495701, %struct.ScmObj** %stackaddr$prim59587, align 8
%stackaddr$prim59588 = alloca %struct.ScmObj*, align 8
%argslist57793$ae495702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49571, %struct.ScmObj* %argslist57793$ae495701)
store volatile %struct.ScmObj* %argslist57793$ae495702, %struct.ScmObj** %stackaddr$prim59588, align 8
%clofunc59589 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49570)
musttail call tailcc void %clofunc59589(%struct.ScmObj* %ae49570, %struct.ScmObj* %argslist57793$ae495702)
ret void
}

define tailcc void @proc_clo$ae49570(%struct.ScmObj* %env$ae49570,%struct.ScmObj* %current_45args57780) {
%stackaddr$env-ref59590 = alloca %struct.ScmObj*, align 8
%lst48086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49570, i64 0)
store %struct.ScmObj* %lst48086, %struct.ScmObj** %stackaddr$env-ref59590
%stackaddr$env-ref59591 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49570, i64 1)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref59591
%stackaddr$env-ref59592 = alloca %struct.ScmObj*, align 8
%k48416 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49570, i64 2)
store %struct.ScmObj* %k48416, %struct.ScmObj** %stackaddr$env-ref59592
%stackaddr$prim59593 = alloca %struct.ScmObj*, align 8
%_95k48417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57780)
store volatile %struct.ScmObj* %_95k48417, %struct.ScmObj** %stackaddr$prim59593, align 8
%stackaddr$prim59594 = alloca %struct.ScmObj*, align 8
%current_45args57781 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57780)
store volatile %struct.ScmObj* %current_45args57781, %struct.ScmObj** %stackaddr$prim59594, align 8
%stackaddr$prim59595 = alloca %struct.ScmObj*, align 8
%anf_45bind48212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57781)
store volatile %struct.ScmObj* %anf_45bind48212, %struct.ScmObj** %stackaddr$prim59595, align 8
%ae49604 = call %struct.ScmObj* @const_init_null()
%argslist57783$_37foldr1480540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59596 = alloca %struct.ScmObj*, align 8
%argslist57783$_37foldr1480541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48086, %struct.ScmObj* %argslist57783$_37foldr1480540)
store volatile %struct.ScmObj* %argslist57783$_37foldr1480541, %struct.ScmObj** %stackaddr$prim59596, align 8
%stackaddr$prim59597 = alloca %struct.ScmObj*, align 8
%argslist57783$_37foldr1480542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49604, %struct.ScmObj* %argslist57783$_37foldr1480541)
store volatile %struct.ScmObj* %argslist57783$_37foldr1480542, %struct.ScmObj** %stackaddr$prim59597, align 8
%stackaddr$prim59598 = alloca %struct.ScmObj*, align 8
%argslist57783$_37foldr1480543 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48212, %struct.ScmObj* %argslist57783$_37foldr1480542)
store volatile %struct.ScmObj* %argslist57783$_37foldr1480543, %struct.ScmObj** %stackaddr$prim59598, align 8
%stackaddr$prim59599 = alloca %struct.ScmObj*, align 8
%argslist57783$_37foldr1480544 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48416, %struct.ScmObj* %argslist57783$_37foldr1480543)
store volatile %struct.ScmObj* %argslist57783$_37foldr1480544, %struct.ScmObj** %stackaddr$prim59599, align 8
%clofunc59600 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148054)
musttail call tailcc void %clofunc59600(%struct.ScmObj* %_37foldr148054, %struct.ScmObj* %argslist57783$_37foldr1480544)
ret void
}

define tailcc void @proc_clo$ae49572(%struct.ScmObj* %env$ae49572,%struct.ScmObj* %current_45args57784) {
%stackaddr$env-ref59601 = alloca %struct.ScmObj*, align 8
%f48087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49572, i64 0)
store %struct.ScmObj* %f48087, %struct.ScmObj** %stackaddr$env-ref59601
%stackaddr$prim59602 = alloca %struct.ScmObj*, align 8
%k48418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57784)
store volatile %struct.ScmObj* %k48418, %struct.ScmObj** %stackaddr$prim59602, align 8
%stackaddr$prim59603 = alloca %struct.ScmObj*, align 8
%current_45args57785 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57784)
store volatile %struct.ScmObj* %current_45args57785, %struct.ScmObj** %stackaddr$prim59603, align 8
%stackaddr$prim59604 = alloca %struct.ScmObj*, align 8
%v48089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57785)
store volatile %struct.ScmObj* %v48089, %struct.ScmObj** %stackaddr$prim59604, align 8
%stackaddr$prim59605 = alloca %struct.ScmObj*, align 8
%current_45args57786 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57785)
store volatile %struct.ScmObj* %current_45args57786, %struct.ScmObj** %stackaddr$prim59605, align 8
%stackaddr$prim59606 = alloca %struct.ScmObj*, align 8
%r48088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57786)
store volatile %struct.ScmObj* %r48088, %struct.ScmObj** %stackaddr$prim59606, align 8
%stackaddr$makeclosure59607 = alloca %struct.ScmObj*, align 8
%fptrToInt59608 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49574 to i64
%ae49574 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59608)
store volatile %struct.ScmObj* %ae49574, %struct.ScmObj** %stackaddr$makeclosure59607, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49574, %struct.ScmObj* %r48088, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49574, %struct.ScmObj* %k48418, i64 1)
%argslist57792$f480870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59609 = alloca %struct.ScmObj*, align 8
%argslist57792$f480871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v48089, %struct.ScmObj* %argslist57792$f480870)
store volatile %struct.ScmObj* %argslist57792$f480871, %struct.ScmObj** %stackaddr$prim59609, align 8
%stackaddr$prim59610 = alloca %struct.ScmObj*, align 8
%argslist57792$f480872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49574, %struct.ScmObj* %argslist57792$f480871)
store volatile %struct.ScmObj* %argslist57792$f480872, %struct.ScmObj** %stackaddr$prim59610, align 8
%clofunc59611 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48087)
musttail call tailcc void %clofunc59611(%struct.ScmObj* %f48087, %struct.ScmObj* %argslist57792$f480872)
ret void
}

define tailcc void @proc_clo$ae49574(%struct.ScmObj* %env$ae49574,%struct.ScmObj* %current_45args57788) {
%stackaddr$env-ref59612 = alloca %struct.ScmObj*, align 8
%r48088 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49574, i64 0)
store %struct.ScmObj* %r48088, %struct.ScmObj** %stackaddr$env-ref59612
%stackaddr$env-ref59613 = alloca %struct.ScmObj*, align 8
%k48418 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49574, i64 1)
store %struct.ScmObj* %k48418, %struct.ScmObj** %stackaddr$env-ref59613
%stackaddr$prim59614 = alloca %struct.ScmObj*, align 8
%_95k48419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57788)
store volatile %struct.ScmObj* %_95k48419, %struct.ScmObj** %stackaddr$prim59614, align 8
%stackaddr$prim59615 = alloca %struct.ScmObj*, align 8
%current_45args57789 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57788)
store volatile %struct.ScmObj* %current_45args57789, %struct.ScmObj** %stackaddr$prim59615, align 8
%stackaddr$prim59616 = alloca %struct.ScmObj*, align 8
%anf_45bind48211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57789)
store volatile %struct.ScmObj* %anf_45bind48211, %struct.ScmObj** %stackaddr$prim59616, align 8
%stackaddr$prim59617 = alloca %struct.ScmObj*, align 8
%cpsprim48420 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48211, %struct.ScmObj* %r48088)
store volatile %struct.ScmObj* %cpsprim48420, %struct.ScmObj** %stackaddr$prim59617, align 8
%ae49579 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57791$k484180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59618 = alloca %struct.ScmObj*, align 8
%argslist57791$k484181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48420, %struct.ScmObj* %argslist57791$k484180)
store volatile %struct.ScmObj* %argslist57791$k484181, %struct.ScmObj** %stackaddr$prim59618, align 8
%stackaddr$prim59619 = alloca %struct.ScmObj*, align 8
%argslist57791$k484182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49579, %struct.ScmObj* %argslist57791$k484181)
store volatile %struct.ScmObj* %argslist57791$k484182, %struct.ScmObj** %stackaddr$prim59619, align 8
%clofunc59620 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48418)
musttail call tailcc void %clofunc59620(%struct.ScmObj* %k48418, %struct.ScmObj* %argslist57791$k484182)
ret void
}

define tailcc void @proc_clo$ae49183(%struct.ScmObj* %env$ae49183,%struct.ScmObj* %current_45args57796) {
%stackaddr$env-ref59621 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49183, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref59621
%stackaddr$env-ref59622 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49183, i64 1)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref59622
%stackaddr$prim59623 = alloca %struct.ScmObj*, align 8
%k48421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57796)
store volatile %struct.ScmObj* %k48421, %struct.ScmObj** %stackaddr$prim59623, align 8
%stackaddr$prim59624 = alloca %struct.ScmObj*, align 8
%current_45args57797 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57796)
store volatile %struct.ScmObj* %current_45args57797, %struct.ScmObj** %stackaddr$prim59624, align 8
%stackaddr$prim59625 = alloca %struct.ScmObj*, align 8
%_37foldr48060 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57797)
store volatile %struct.ScmObj* %_37foldr48060, %struct.ScmObj** %stackaddr$prim59625, align 8
%ae49185 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59626 = alloca %struct.ScmObj*, align 8
%fptrToInt59627 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49186 to i64
%ae49186 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt59627)
store volatile %struct.ScmObj* %ae49186, %struct.ScmObj** %stackaddr$makeclosure59626, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49186, %struct.ScmObj* %_37foldr148054, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49186, %struct.ScmObj* %_37map148050, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49186, %struct.ScmObj* %_37foldr48060, i64 2)
%argslist57854$k484210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59628 = alloca %struct.ScmObj*, align 8
%argslist57854$k484211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49186, %struct.ScmObj* %argslist57854$k484210)
store volatile %struct.ScmObj* %argslist57854$k484211, %struct.ScmObj** %stackaddr$prim59628, align 8
%stackaddr$prim59629 = alloca %struct.ScmObj*, align 8
%argslist57854$k484212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49185, %struct.ScmObj* %argslist57854$k484211)
store volatile %struct.ScmObj* %argslist57854$k484212, %struct.ScmObj** %stackaddr$prim59629, align 8
%clofunc59630 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48421)
musttail call tailcc void %clofunc59630(%struct.ScmObj* %k48421, %struct.ScmObj* %argslist57854$k484212)
ret void
}

define tailcc void @proc_clo$ae49186(%struct.ScmObj* %env$ae49186,%struct.ScmObj* %args4806148422) {
%stackaddr$env-ref59631 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49186, i64 0)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref59631
%stackaddr$env-ref59632 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49186, i64 1)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref59632
%stackaddr$env-ref59633 = alloca %struct.ScmObj*, align 8
%_37foldr48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49186, i64 2)
store %struct.ScmObj* %_37foldr48060, %struct.ScmObj** %stackaddr$env-ref59633
%stackaddr$prim59634 = alloca %struct.ScmObj*, align 8
%k48423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4806148422)
store volatile %struct.ScmObj* %k48423, %struct.ScmObj** %stackaddr$prim59634, align 8
%stackaddr$prim59635 = alloca %struct.ScmObj*, align 8
%args48061 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4806148422)
store volatile %struct.ScmObj* %args48061, %struct.ScmObj** %stackaddr$prim59635, align 8
%stackaddr$prim59636 = alloca %struct.ScmObj*, align 8
%f48064 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48061)
store volatile %struct.ScmObj* %f48064, %struct.ScmObj** %stackaddr$prim59636, align 8
%stackaddr$prim59637 = alloca %struct.ScmObj*, align 8
%anf_45bind48198 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48061)
store volatile %struct.ScmObj* %anf_45bind48198, %struct.ScmObj** %stackaddr$prim59637, align 8
%stackaddr$prim59638 = alloca %struct.ScmObj*, align 8
%acc48063 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48198)
store volatile %struct.ScmObj* %acc48063, %struct.ScmObj** %stackaddr$prim59638, align 8
%stackaddr$prim59639 = alloca %struct.ScmObj*, align 8
%anf_45bind48199 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48061)
store volatile %struct.ScmObj* %anf_45bind48199, %struct.ScmObj** %stackaddr$prim59639, align 8
%stackaddr$prim59640 = alloca %struct.ScmObj*, align 8
%lsts48062 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48199)
store volatile %struct.ScmObj* %lsts48062, %struct.ScmObj** %stackaddr$prim59640, align 8
%stackaddr$makeclosure59641 = alloca %struct.ScmObj*, align 8
%fptrToInt59642 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49194 to i64
%ae49194 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt59642)
store volatile %struct.ScmObj* %ae49194, %struct.ScmObj** %stackaddr$makeclosure59641, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49194, %struct.ScmObj* %k48423, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49194, %struct.ScmObj* %_37foldr148054, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49194, %struct.ScmObj* %_37map148050, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49194, %struct.ScmObj* %f48064, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49194, %struct.ScmObj* %acc48063, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49194, %struct.ScmObj* %lsts48062, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49194, %struct.ScmObj* %_37foldr48060, i64 6)
%ae49195 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59643 = alloca %struct.ScmObj*, align 8
%fptrToInt59644 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49196 to i64
%ae49196 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt59644)
store volatile %struct.ScmObj* %ae49196, %struct.ScmObj** %stackaddr$makeclosure59643, align 8
%argslist57853$ae491940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59645 = alloca %struct.ScmObj*, align 8
%argslist57853$ae491941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49196, %struct.ScmObj* %argslist57853$ae491940)
store volatile %struct.ScmObj* %argslist57853$ae491941, %struct.ScmObj** %stackaddr$prim59645, align 8
%stackaddr$prim59646 = alloca %struct.ScmObj*, align 8
%argslist57853$ae491942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49195, %struct.ScmObj* %argslist57853$ae491941)
store volatile %struct.ScmObj* %argslist57853$ae491942, %struct.ScmObj** %stackaddr$prim59646, align 8
%clofunc59647 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49194)
musttail call tailcc void %clofunc59647(%struct.ScmObj* %ae49194, %struct.ScmObj* %argslist57853$ae491942)
ret void
}

define tailcc void @proc_clo$ae49194(%struct.ScmObj* %env$ae49194,%struct.ScmObj* %current_45args57799) {
%stackaddr$env-ref59648 = alloca %struct.ScmObj*, align 8
%k48423 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49194, i64 0)
store %struct.ScmObj* %k48423, %struct.ScmObj** %stackaddr$env-ref59648
%stackaddr$env-ref59649 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49194, i64 1)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref59649
%stackaddr$env-ref59650 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49194, i64 2)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref59650
%stackaddr$env-ref59651 = alloca %struct.ScmObj*, align 8
%f48064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49194, i64 3)
store %struct.ScmObj* %f48064, %struct.ScmObj** %stackaddr$env-ref59651
%stackaddr$env-ref59652 = alloca %struct.ScmObj*, align 8
%acc48063 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49194, i64 4)
store %struct.ScmObj* %acc48063, %struct.ScmObj** %stackaddr$env-ref59652
%stackaddr$env-ref59653 = alloca %struct.ScmObj*, align 8
%lsts48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49194, i64 5)
store %struct.ScmObj* %lsts48062, %struct.ScmObj** %stackaddr$env-ref59653
%stackaddr$env-ref59654 = alloca %struct.ScmObj*, align 8
%_37foldr48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49194, i64 6)
store %struct.ScmObj* %_37foldr48060, %struct.ScmObj** %stackaddr$env-ref59654
%stackaddr$prim59655 = alloca %struct.ScmObj*, align 8
%_95k48424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57799)
store volatile %struct.ScmObj* %_95k48424, %struct.ScmObj** %stackaddr$prim59655, align 8
%stackaddr$prim59656 = alloca %struct.ScmObj*, align 8
%current_45args57800 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57799)
store volatile %struct.ScmObj* %current_45args57800, %struct.ScmObj** %stackaddr$prim59656, align 8
%stackaddr$prim59657 = alloca %struct.ScmObj*, align 8
%anf_45bind48200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57800)
store volatile %struct.ScmObj* %anf_45bind48200, %struct.ScmObj** %stackaddr$prim59657, align 8
%stackaddr$makeclosure59658 = alloca %struct.ScmObj*, align 8
%fptrToInt59659 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49226 to i64
%ae49226 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt59659)
store volatile %struct.ScmObj* %ae49226, %struct.ScmObj** %stackaddr$makeclosure59658, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49226, %struct.ScmObj* %k48423, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49226, %struct.ScmObj* %_37foldr148054, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49226, %struct.ScmObj* %_37map148050, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49226, %struct.ScmObj* %f48064, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49226, %struct.ScmObj* %acc48063, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49226, %struct.ScmObj* %lsts48062, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49226, %struct.ScmObj* %_37foldr48060, i64 6)
%ae49228 = call %struct.ScmObj* @const_init_false()
%argslist57846$_37foldr1480540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59660 = alloca %struct.ScmObj*, align 8
%argslist57846$_37foldr1480541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48062, %struct.ScmObj* %argslist57846$_37foldr1480540)
store volatile %struct.ScmObj* %argslist57846$_37foldr1480541, %struct.ScmObj** %stackaddr$prim59660, align 8
%stackaddr$prim59661 = alloca %struct.ScmObj*, align 8
%argslist57846$_37foldr1480542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49228, %struct.ScmObj* %argslist57846$_37foldr1480541)
store volatile %struct.ScmObj* %argslist57846$_37foldr1480542, %struct.ScmObj** %stackaddr$prim59661, align 8
%stackaddr$prim59662 = alloca %struct.ScmObj*, align 8
%argslist57846$_37foldr1480543 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48200, %struct.ScmObj* %argslist57846$_37foldr1480542)
store volatile %struct.ScmObj* %argslist57846$_37foldr1480543, %struct.ScmObj** %stackaddr$prim59662, align 8
%stackaddr$prim59663 = alloca %struct.ScmObj*, align 8
%argslist57846$_37foldr1480544 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49226, %struct.ScmObj* %argslist57846$_37foldr1480543)
store volatile %struct.ScmObj* %argslist57846$_37foldr1480544, %struct.ScmObj** %stackaddr$prim59663, align 8
%clofunc59664 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148054)
musttail call tailcc void %clofunc59664(%struct.ScmObj* %_37foldr148054, %struct.ScmObj* %argslist57846$_37foldr1480544)
ret void
}

define tailcc void @proc_clo$ae49226(%struct.ScmObj* %env$ae49226,%struct.ScmObj* %current_45args57802) {
%stackaddr$env-ref59665 = alloca %struct.ScmObj*, align 8
%k48423 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49226, i64 0)
store %struct.ScmObj* %k48423, %struct.ScmObj** %stackaddr$env-ref59665
%stackaddr$env-ref59666 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49226, i64 1)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref59666
%stackaddr$env-ref59667 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49226, i64 2)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref59667
%stackaddr$env-ref59668 = alloca %struct.ScmObj*, align 8
%f48064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49226, i64 3)
store %struct.ScmObj* %f48064, %struct.ScmObj** %stackaddr$env-ref59668
%stackaddr$env-ref59669 = alloca %struct.ScmObj*, align 8
%acc48063 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49226, i64 4)
store %struct.ScmObj* %acc48063, %struct.ScmObj** %stackaddr$env-ref59669
%stackaddr$env-ref59670 = alloca %struct.ScmObj*, align 8
%lsts48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49226, i64 5)
store %struct.ScmObj* %lsts48062, %struct.ScmObj** %stackaddr$env-ref59670
%stackaddr$env-ref59671 = alloca %struct.ScmObj*, align 8
%_37foldr48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49226, i64 6)
store %struct.ScmObj* %_37foldr48060, %struct.ScmObj** %stackaddr$env-ref59671
%stackaddr$prim59672 = alloca %struct.ScmObj*, align 8
%_95k48425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57802)
store volatile %struct.ScmObj* %_95k48425, %struct.ScmObj** %stackaddr$prim59672, align 8
%stackaddr$prim59673 = alloca %struct.ScmObj*, align 8
%current_45args57803 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57802)
store volatile %struct.ScmObj* %current_45args57803, %struct.ScmObj** %stackaddr$prim59673, align 8
%stackaddr$prim59674 = alloca %struct.ScmObj*, align 8
%anf_45bind48201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57803)
store volatile %struct.ScmObj* %anf_45bind48201, %struct.ScmObj** %stackaddr$prim59674, align 8
%truthy$cmp59675 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48201)
%cmp$cmp59675 = icmp eq i64 %truthy$cmp59675, 1
br i1 %cmp$cmp59675, label %truebranch$cmp59675, label %falsebranch$cmp59675
truebranch$cmp59675:
%ae49237 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57805$k484230 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59676 = alloca %struct.ScmObj*, align 8
%argslist57805$k484231 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48063, %struct.ScmObj* %argslist57805$k484230)
store volatile %struct.ScmObj* %argslist57805$k484231, %struct.ScmObj** %stackaddr$prim59676, align 8
%stackaddr$prim59677 = alloca %struct.ScmObj*, align 8
%argslist57805$k484232 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49237, %struct.ScmObj* %argslist57805$k484231)
store volatile %struct.ScmObj* %argslist57805$k484232, %struct.ScmObj** %stackaddr$prim59677, align 8
%clofunc59678 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48423)
musttail call tailcc void %clofunc59678(%struct.ScmObj* %k48423, %struct.ScmObj* %argslist57805$k484232)
ret void
falsebranch$cmp59675:
%stackaddr$makeclosure59679 = alloca %struct.ScmObj*, align 8
%fptrToInt59680 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49242 to i64
%ae49242 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt59680)
store volatile %struct.ScmObj* %ae49242, %struct.ScmObj** %stackaddr$makeclosure59679, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49242, %struct.ScmObj* %k48423, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49242, %struct.ScmObj* %_37foldr148054, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49242, %struct.ScmObj* %_37map148050, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49242, %struct.ScmObj* %f48064, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49242, %struct.ScmObj* %acc48063, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49242, %struct.ScmObj* %lsts48062, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49242, %struct.ScmObj* %_37foldr48060, i64 6)
%ae49243 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59681 = alloca %struct.ScmObj*, align 8
%fptrToInt59682 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49244 to i64
%ae49244 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt59682)
store volatile %struct.ScmObj* %ae49244, %struct.ScmObj** %stackaddr$makeclosure59681, align 8
%argslist57845$ae492420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59683 = alloca %struct.ScmObj*, align 8
%argslist57845$ae492421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49244, %struct.ScmObj* %argslist57845$ae492420)
store volatile %struct.ScmObj* %argslist57845$ae492421, %struct.ScmObj** %stackaddr$prim59683, align 8
%stackaddr$prim59684 = alloca %struct.ScmObj*, align 8
%argslist57845$ae492422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49243, %struct.ScmObj* %argslist57845$ae492421)
store volatile %struct.ScmObj* %argslist57845$ae492422, %struct.ScmObj** %stackaddr$prim59684, align 8
%clofunc59685 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49242)
musttail call tailcc void %clofunc59685(%struct.ScmObj* %ae49242, %struct.ScmObj* %argslist57845$ae492422)
ret void
}

define tailcc void @proc_clo$ae49242(%struct.ScmObj* %env$ae49242,%struct.ScmObj* %current_45args57806) {
%stackaddr$env-ref59686 = alloca %struct.ScmObj*, align 8
%k48423 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49242, i64 0)
store %struct.ScmObj* %k48423, %struct.ScmObj** %stackaddr$env-ref59686
%stackaddr$env-ref59687 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49242, i64 1)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref59687
%stackaddr$env-ref59688 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49242, i64 2)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref59688
%stackaddr$env-ref59689 = alloca %struct.ScmObj*, align 8
%f48064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49242, i64 3)
store %struct.ScmObj* %f48064, %struct.ScmObj** %stackaddr$env-ref59689
%stackaddr$env-ref59690 = alloca %struct.ScmObj*, align 8
%acc48063 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49242, i64 4)
store %struct.ScmObj* %acc48063, %struct.ScmObj** %stackaddr$env-ref59690
%stackaddr$env-ref59691 = alloca %struct.ScmObj*, align 8
%lsts48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49242, i64 5)
store %struct.ScmObj* %lsts48062, %struct.ScmObj** %stackaddr$env-ref59691
%stackaddr$env-ref59692 = alloca %struct.ScmObj*, align 8
%_37foldr48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49242, i64 6)
store %struct.ScmObj* %_37foldr48060, %struct.ScmObj** %stackaddr$env-ref59692
%stackaddr$prim59693 = alloca %struct.ScmObj*, align 8
%_95k48426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57806)
store volatile %struct.ScmObj* %_95k48426, %struct.ScmObj** %stackaddr$prim59693, align 8
%stackaddr$prim59694 = alloca %struct.ScmObj*, align 8
%current_45args57807 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57806)
store volatile %struct.ScmObj* %current_45args57807, %struct.ScmObj** %stackaddr$prim59694, align 8
%stackaddr$prim59695 = alloca %struct.ScmObj*, align 8
%anf_45bind48202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57807)
store volatile %struct.ScmObj* %anf_45bind48202, %struct.ScmObj** %stackaddr$prim59695, align 8
%stackaddr$makeclosure59696 = alloca %struct.ScmObj*, align 8
%fptrToInt59697 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49263 to i64
%ae49263 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt59697)
store volatile %struct.ScmObj* %ae49263, %struct.ScmObj** %stackaddr$makeclosure59696, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49263, %struct.ScmObj* %k48423, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49263, %struct.ScmObj* %_37foldr148054, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49263, %struct.ScmObj* %_37map148050, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49263, %struct.ScmObj* %f48064, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49263, %struct.ScmObj* %acc48063, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49263, %struct.ScmObj* %lsts48062, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49263, %struct.ScmObj* %_37foldr48060, i64 6)
%argslist57840$_37map1480500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59698 = alloca %struct.ScmObj*, align 8
%argslist57840$_37map1480501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48062, %struct.ScmObj* %argslist57840$_37map1480500)
store volatile %struct.ScmObj* %argslist57840$_37map1480501, %struct.ScmObj** %stackaddr$prim59698, align 8
%stackaddr$prim59699 = alloca %struct.ScmObj*, align 8
%argslist57840$_37map1480502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48202, %struct.ScmObj* %argslist57840$_37map1480501)
store volatile %struct.ScmObj* %argslist57840$_37map1480502, %struct.ScmObj** %stackaddr$prim59699, align 8
%stackaddr$prim59700 = alloca %struct.ScmObj*, align 8
%argslist57840$_37map1480503 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49263, %struct.ScmObj* %argslist57840$_37map1480502)
store volatile %struct.ScmObj* %argslist57840$_37map1480503, %struct.ScmObj** %stackaddr$prim59700, align 8
%clofunc59701 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148050)
musttail call tailcc void %clofunc59701(%struct.ScmObj* %_37map148050, %struct.ScmObj* %argslist57840$_37map1480503)
ret void
}

define tailcc void @proc_clo$ae49263(%struct.ScmObj* %env$ae49263,%struct.ScmObj* %current_45args57809) {
%stackaddr$env-ref59702 = alloca %struct.ScmObj*, align 8
%k48423 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49263, i64 0)
store %struct.ScmObj* %k48423, %struct.ScmObj** %stackaddr$env-ref59702
%stackaddr$env-ref59703 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49263, i64 1)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref59703
%stackaddr$env-ref59704 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49263, i64 2)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref59704
%stackaddr$env-ref59705 = alloca %struct.ScmObj*, align 8
%f48064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49263, i64 3)
store %struct.ScmObj* %f48064, %struct.ScmObj** %stackaddr$env-ref59705
%stackaddr$env-ref59706 = alloca %struct.ScmObj*, align 8
%acc48063 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49263, i64 4)
store %struct.ScmObj* %acc48063, %struct.ScmObj** %stackaddr$env-ref59706
%stackaddr$env-ref59707 = alloca %struct.ScmObj*, align 8
%lsts48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49263, i64 5)
store %struct.ScmObj* %lsts48062, %struct.ScmObj** %stackaddr$env-ref59707
%stackaddr$env-ref59708 = alloca %struct.ScmObj*, align 8
%_37foldr48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49263, i64 6)
store %struct.ScmObj* %_37foldr48060, %struct.ScmObj** %stackaddr$env-ref59708
%stackaddr$prim59709 = alloca %struct.ScmObj*, align 8
%_95k48427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57809)
store volatile %struct.ScmObj* %_95k48427, %struct.ScmObj** %stackaddr$prim59709, align 8
%stackaddr$prim59710 = alloca %struct.ScmObj*, align 8
%current_45args57810 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57809)
store volatile %struct.ScmObj* %current_45args57810, %struct.ScmObj** %stackaddr$prim59710, align 8
%stackaddr$prim59711 = alloca %struct.ScmObj*, align 8
%lsts_4348069 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57810)
store volatile %struct.ScmObj* %lsts_4348069, %struct.ScmObj** %stackaddr$prim59711, align 8
%stackaddr$makeclosure59712 = alloca %struct.ScmObj*, align 8
%fptrToInt59713 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49266 to i64
%ae49266 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt59713)
store volatile %struct.ScmObj* %ae49266, %struct.ScmObj** %stackaddr$makeclosure59712, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49266, %struct.ScmObj* %k48423, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49266, %struct.ScmObj* %_37foldr148054, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49266, %struct.ScmObj* %lsts_4348069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49266, %struct.ScmObj* %_37map148050, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49266, %struct.ScmObj* %f48064, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49266, %struct.ScmObj* %acc48063, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49266, %struct.ScmObj* %lsts48062, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49266, %struct.ScmObj* %_37foldr48060, i64 7)
%ae49267 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59714 = alloca %struct.ScmObj*, align 8
%fptrToInt59715 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49268 to i64
%ae49268 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt59715)
store volatile %struct.ScmObj* %ae49268, %struct.ScmObj** %stackaddr$makeclosure59714, align 8
%argslist57839$ae492660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59716 = alloca %struct.ScmObj*, align 8
%argslist57839$ae492661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49268, %struct.ScmObj* %argslist57839$ae492660)
store volatile %struct.ScmObj* %argslist57839$ae492661, %struct.ScmObj** %stackaddr$prim59716, align 8
%stackaddr$prim59717 = alloca %struct.ScmObj*, align 8
%argslist57839$ae492662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49267, %struct.ScmObj* %argslist57839$ae492661)
store volatile %struct.ScmObj* %argslist57839$ae492662, %struct.ScmObj** %stackaddr$prim59717, align 8
%clofunc59718 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49266)
musttail call tailcc void %clofunc59718(%struct.ScmObj* %ae49266, %struct.ScmObj* %argslist57839$ae492662)
ret void
}

define tailcc void @proc_clo$ae49266(%struct.ScmObj* %env$ae49266,%struct.ScmObj* %current_45args57812) {
%stackaddr$env-ref59719 = alloca %struct.ScmObj*, align 8
%k48423 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49266, i64 0)
store %struct.ScmObj* %k48423, %struct.ScmObj** %stackaddr$env-ref59719
%stackaddr$env-ref59720 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49266, i64 1)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref59720
%stackaddr$env-ref59721 = alloca %struct.ScmObj*, align 8
%lsts_4348069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49266, i64 2)
store %struct.ScmObj* %lsts_4348069, %struct.ScmObj** %stackaddr$env-ref59721
%stackaddr$env-ref59722 = alloca %struct.ScmObj*, align 8
%_37map148050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49266, i64 3)
store %struct.ScmObj* %_37map148050, %struct.ScmObj** %stackaddr$env-ref59722
%stackaddr$env-ref59723 = alloca %struct.ScmObj*, align 8
%f48064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49266, i64 4)
store %struct.ScmObj* %f48064, %struct.ScmObj** %stackaddr$env-ref59723
%stackaddr$env-ref59724 = alloca %struct.ScmObj*, align 8
%acc48063 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49266, i64 5)
store %struct.ScmObj* %acc48063, %struct.ScmObj** %stackaddr$env-ref59724
%stackaddr$env-ref59725 = alloca %struct.ScmObj*, align 8
%lsts48062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49266, i64 6)
store %struct.ScmObj* %lsts48062, %struct.ScmObj** %stackaddr$env-ref59725
%stackaddr$env-ref59726 = alloca %struct.ScmObj*, align 8
%_37foldr48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49266, i64 7)
store %struct.ScmObj* %_37foldr48060, %struct.ScmObj** %stackaddr$env-ref59726
%stackaddr$prim59727 = alloca %struct.ScmObj*, align 8
%_95k48428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57812)
store volatile %struct.ScmObj* %_95k48428, %struct.ScmObj** %stackaddr$prim59727, align 8
%stackaddr$prim59728 = alloca %struct.ScmObj*, align 8
%current_45args57813 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57812)
store volatile %struct.ScmObj* %current_45args57813, %struct.ScmObj** %stackaddr$prim59728, align 8
%stackaddr$prim59729 = alloca %struct.ScmObj*, align 8
%anf_45bind48203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57813)
store volatile %struct.ScmObj* %anf_45bind48203, %struct.ScmObj** %stackaddr$prim59729, align 8
%stackaddr$makeclosure59730 = alloca %struct.ScmObj*, align 8
%fptrToInt59731 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49287 to i64
%ae49287 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt59731)
store volatile %struct.ScmObj* %ae49287, %struct.ScmObj** %stackaddr$makeclosure59730, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49287, %struct.ScmObj* %k48423, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49287, %struct.ScmObj* %_37foldr148054, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49287, %struct.ScmObj* %lsts_4348069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49287, %struct.ScmObj* %f48064, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49287, %struct.ScmObj* %acc48063, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49287, %struct.ScmObj* %_37foldr48060, i64 5)
%argslist57834$_37map1480500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59732 = alloca %struct.ScmObj*, align 8
%argslist57834$_37map1480501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48062, %struct.ScmObj* %argslist57834$_37map1480500)
store volatile %struct.ScmObj* %argslist57834$_37map1480501, %struct.ScmObj** %stackaddr$prim59732, align 8
%stackaddr$prim59733 = alloca %struct.ScmObj*, align 8
%argslist57834$_37map1480502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48203, %struct.ScmObj* %argslist57834$_37map1480501)
store volatile %struct.ScmObj* %argslist57834$_37map1480502, %struct.ScmObj** %stackaddr$prim59733, align 8
%stackaddr$prim59734 = alloca %struct.ScmObj*, align 8
%argslist57834$_37map1480503 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49287, %struct.ScmObj* %argslist57834$_37map1480502)
store volatile %struct.ScmObj* %argslist57834$_37map1480503, %struct.ScmObj** %stackaddr$prim59734, align 8
%clofunc59735 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148050)
musttail call tailcc void %clofunc59735(%struct.ScmObj* %_37map148050, %struct.ScmObj* %argslist57834$_37map1480503)
ret void
}

define tailcc void @proc_clo$ae49287(%struct.ScmObj* %env$ae49287,%struct.ScmObj* %current_45args57815) {
%stackaddr$env-ref59736 = alloca %struct.ScmObj*, align 8
%k48423 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49287, i64 0)
store %struct.ScmObj* %k48423, %struct.ScmObj** %stackaddr$env-ref59736
%stackaddr$env-ref59737 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49287, i64 1)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref59737
%stackaddr$env-ref59738 = alloca %struct.ScmObj*, align 8
%lsts_4348069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49287, i64 2)
store %struct.ScmObj* %lsts_4348069, %struct.ScmObj** %stackaddr$env-ref59738
%stackaddr$env-ref59739 = alloca %struct.ScmObj*, align 8
%f48064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49287, i64 3)
store %struct.ScmObj* %f48064, %struct.ScmObj** %stackaddr$env-ref59739
%stackaddr$env-ref59740 = alloca %struct.ScmObj*, align 8
%acc48063 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49287, i64 4)
store %struct.ScmObj* %acc48063, %struct.ScmObj** %stackaddr$env-ref59740
%stackaddr$env-ref59741 = alloca %struct.ScmObj*, align 8
%_37foldr48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49287, i64 5)
store %struct.ScmObj* %_37foldr48060, %struct.ScmObj** %stackaddr$env-ref59741
%stackaddr$prim59742 = alloca %struct.ScmObj*, align 8
%_95k48429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57815)
store volatile %struct.ScmObj* %_95k48429, %struct.ScmObj** %stackaddr$prim59742, align 8
%stackaddr$prim59743 = alloca %struct.ScmObj*, align 8
%current_45args57816 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57815)
store volatile %struct.ScmObj* %current_45args57816, %struct.ScmObj** %stackaddr$prim59743, align 8
%stackaddr$prim59744 = alloca %struct.ScmObj*, align 8
%vs48067 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57816)
store volatile %struct.ScmObj* %vs48067, %struct.ScmObj** %stackaddr$prim59744, align 8
%stackaddr$makeclosure59745 = alloca %struct.ScmObj*, align 8
%fptrToInt59746 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49290 to i64
%ae49290 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt59746)
store volatile %struct.ScmObj* %ae49290, %struct.ScmObj** %stackaddr$makeclosure59745, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49290, %struct.ScmObj* %k48423, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49290, %struct.ScmObj* %_37foldr148054, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49290, %struct.ScmObj* %lsts_4348069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49290, %struct.ScmObj* %vs48067, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49290, %struct.ScmObj* %f48064, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49290, %struct.ScmObj* %acc48063, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49290, %struct.ScmObj* %_37foldr48060, i64 6)
%ae49291 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59747 = alloca %struct.ScmObj*, align 8
%fptrToInt59748 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49292 to i64
%ae49292 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt59748)
store volatile %struct.ScmObj* %ae49292, %struct.ScmObj** %stackaddr$makeclosure59747, align 8
%argslist57833$ae492900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59749 = alloca %struct.ScmObj*, align 8
%argslist57833$ae492901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49292, %struct.ScmObj* %argslist57833$ae492900)
store volatile %struct.ScmObj* %argslist57833$ae492901, %struct.ScmObj** %stackaddr$prim59749, align 8
%stackaddr$prim59750 = alloca %struct.ScmObj*, align 8
%argslist57833$ae492902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49291, %struct.ScmObj* %argslist57833$ae492901)
store volatile %struct.ScmObj* %argslist57833$ae492902, %struct.ScmObj** %stackaddr$prim59750, align 8
%clofunc59751 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49290)
musttail call tailcc void %clofunc59751(%struct.ScmObj* %ae49290, %struct.ScmObj* %argslist57833$ae492902)
ret void
}

define tailcc void @proc_clo$ae49290(%struct.ScmObj* %env$ae49290,%struct.ScmObj* %current_45args57818) {
%stackaddr$env-ref59752 = alloca %struct.ScmObj*, align 8
%k48423 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49290, i64 0)
store %struct.ScmObj* %k48423, %struct.ScmObj** %stackaddr$env-ref59752
%stackaddr$env-ref59753 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49290, i64 1)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref59753
%stackaddr$env-ref59754 = alloca %struct.ScmObj*, align 8
%lsts_4348069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49290, i64 2)
store %struct.ScmObj* %lsts_4348069, %struct.ScmObj** %stackaddr$env-ref59754
%stackaddr$env-ref59755 = alloca %struct.ScmObj*, align 8
%vs48067 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49290, i64 3)
store %struct.ScmObj* %vs48067, %struct.ScmObj** %stackaddr$env-ref59755
%stackaddr$env-ref59756 = alloca %struct.ScmObj*, align 8
%f48064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49290, i64 4)
store %struct.ScmObj* %f48064, %struct.ScmObj** %stackaddr$env-ref59756
%stackaddr$env-ref59757 = alloca %struct.ScmObj*, align 8
%acc48063 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49290, i64 5)
store %struct.ScmObj* %acc48063, %struct.ScmObj** %stackaddr$env-ref59757
%stackaddr$env-ref59758 = alloca %struct.ScmObj*, align 8
%_37foldr48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49290, i64 6)
store %struct.ScmObj* %_37foldr48060, %struct.ScmObj** %stackaddr$env-ref59758
%stackaddr$prim59759 = alloca %struct.ScmObj*, align 8
%_95k48430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57818)
store volatile %struct.ScmObj* %_95k48430, %struct.ScmObj** %stackaddr$prim59759, align 8
%stackaddr$prim59760 = alloca %struct.ScmObj*, align 8
%current_45args57819 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57818)
store volatile %struct.ScmObj* %current_45args57819, %struct.ScmObj** %stackaddr$prim59760, align 8
%stackaddr$prim59761 = alloca %struct.ScmObj*, align 8
%anf_45bind48204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57819)
store volatile %struct.ScmObj* %anf_45bind48204, %struct.ScmObj** %stackaddr$prim59761, align 8
%stackaddr$prim59762 = alloca %struct.ScmObj*, align 8
%anf_45bind48205 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48063, %struct.ScmObj* %lsts_4348069)
store volatile %struct.ScmObj* %anf_45bind48205, %struct.ScmObj** %stackaddr$prim59762, align 8
%stackaddr$prim59763 = alloca %struct.ScmObj*, align 8
%anf_45bind48206 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48064, %struct.ScmObj* %anf_45bind48205)
store volatile %struct.ScmObj* %anf_45bind48206, %struct.ScmObj** %stackaddr$prim59763, align 8
%stackaddr$makeclosure59764 = alloca %struct.ScmObj*, align 8
%fptrToInt59765 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49316 to i64
%ae49316 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt59765)
store volatile %struct.ScmObj* %ae49316, %struct.ScmObj** %stackaddr$makeclosure59764, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49316, %struct.ScmObj* %k48423, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49316, %struct.ScmObj* %_37foldr148054, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49316, %struct.ScmObj* %vs48067, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49316, %struct.ScmObj* %f48064, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49316, %struct.ScmObj* %anf_45bind48204, i64 4)
%stackaddr$prim59766 = alloca %struct.ScmObj*, align 8
%cpsargs48434 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49316, %struct.ScmObj* %anf_45bind48206)
store volatile %struct.ScmObj* %cpsargs48434, %struct.ScmObj** %stackaddr$prim59766, align 8
%clofunc59767 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48060)
musttail call tailcc void %clofunc59767(%struct.ScmObj* %_37foldr48060, %struct.ScmObj* %cpsargs48434)
ret void
}

define tailcc void @proc_clo$ae49316(%struct.ScmObj* %env$ae49316,%struct.ScmObj* %current_45args57821) {
%stackaddr$env-ref59768 = alloca %struct.ScmObj*, align 8
%k48423 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49316, i64 0)
store %struct.ScmObj* %k48423, %struct.ScmObj** %stackaddr$env-ref59768
%stackaddr$env-ref59769 = alloca %struct.ScmObj*, align 8
%_37foldr148054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49316, i64 1)
store %struct.ScmObj* %_37foldr148054, %struct.ScmObj** %stackaddr$env-ref59769
%stackaddr$env-ref59770 = alloca %struct.ScmObj*, align 8
%vs48067 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49316, i64 2)
store %struct.ScmObj* %vs48067, %struct.ScmObj** %stackaddr$env-ref59770
%stackaddr$env-ref59771 = alloca %struct.ScmObj*, align 8
%f48064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49316, i64 3)
store %struct.ScmObj* %f48064, %struct.ScmObj** %stackaddr$env-ref59771
%stackaddr$env-ref59772 = alloca %struct.ScmObj*, align 8
%anf_45bind48204 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49316, i64 4)
store %struct.ScmObj* %anf_45bind48204, %struct.ScmObj** %stackaddr$env-ref59772
%stackaddr$prim59773 = alloca %struct.ScmObj*, align 8
%_95k48431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57821)
store volatile %struct.ScmObj* %_95k48431, %struct.ScmObj** %stackaddr$prim59773, align 8
%stackaddr$prim59774 = alloca %struct.ScmObj*, align 8
%current_45args57822 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57821)
store volatile %struct.ScmObj* %current_45args57822, %struct.ScmObj** %stackaddr$prim59774, align 8
%stackaddr$prim59775 = alloca %struct.ScmObj*, align 8
%anf_45bind48207 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57822)
store volatile %struct.ScmObj* %anf_45bind48207, %struct.ScmObj** %stackaddr$prim59775, align 8
%ae49321 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59776 = alloca %struct.ScmObj*, align 8
%anf_45bind48208 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48207, %struct.ScmObj* %ae49321)
store volatile %struct.ScmObj* %anf_45bind48208, %struct.ScmObj** %stackaddr$prim59776, align 8
%stackaddr$makeclosure59777 = alloca %struct.ScmObj*, align 8
%fptrToInt59778 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49323 to i64
%ae49323 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59778)
store volatile %struct.ScmObj* %ae49323, %struct.ScmObj** %stackaddr$makeclosure59777, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49323, %struct.ScmObj* %k48423, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49323, %struct.ScmObj* %f48064, i64 1)
%argslist57827$_37foldr1480540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59779 = alloca %struct.ScmObj*, align 8
%argslist57827$_37foldr1480541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48067, %struct.ScmObj* %argslist57827$_37foldr1480540)
store volatile %struct.ScmObj* %argslist57827$_37foldr1480541, %struct.ScmObj** %stackaddr$prim59779, align 8
%stackaddr$prim59780 = alloca %struct.ScmObj*, align 8
%argslist57827$_37foldr1480542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48208, %struct.ScmObj* %argslist57827$_37foldr1480541)
store volatile %struct.ScmObj* %argslist57827$_37foldr1480542, %struct.ScmObj** %stackaddr$prim59780, align 8
%stackaddr$prim59781 = alloca %struct.ScmObj*, align 8
%argslist57827$_37foldr1480543 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48204, %struct.ScmObj* %argslist57827$_37foldr1480542)
store volatile %struct.ScmObj* %argslist57827$_37foldr1480543, %struct.ScmObj** %stackaddr$prim59781, align 8
%stackaddr$prim59782 = alloca %struct.ScmObj*, align 8
%argslist57827$_37foldr1480544 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49323, %struct.ScmObj* %argslist57827$_37foldr1480543)
store volatile %struct.ScmObj* %argslist57827$_37foldr1480544, %struct.ScmObj** %stackaddr$prim59782, align 8
%clofunc59783 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148054)
musttail call tailcc void %clofunc59783(%struct.ScmObj* %_37foldr148054, %struct.ScmObj* %argslist57827$_37foldr1480544)
ret void
}

define tailcc void @proc_clo$ae49323(%struct.ScmObj* %env$ae49323,%struct.ScmObj* %current_45args57824) {
%stackaddr$env-ref59784 = alloca %struct.ScmObj*, align 8
%k48423 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49323, i64 0)
store %struct.ScmObj* %k48423, %struct.ScmObj** %stackaddr$env-ref59784
%stackaddr$env-ref59785 = alloca %struct.ScmObj*, align 8
%f48064 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49323, i64 1)
store %struct.ScmObj* %f48064, %struct.ScmObj** %stackaddr$env-ref59785
%stackaddr$prim59786 = alloca %struct.ScmObj*, align 8
%_95k48432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57824)
store volatile %struct.ScmObj* %_95k48432, %struct.ScmObj** %stackaddr$prim59786, align 8
%stackaddr$prim59787 = alloca %struct.ScmObj*, align 8
%current_45args57825 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57824)
store volatile %struct.ScmObj* %current_45args57825, %struct.ScmObj** %stackaddr$prim59787, align 8
%stackaddr$prim59788 = alloca %struct.ScmObj*, align 8
%anf_45bind48209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57825)
store volatile %struct.ScmObj* %anf_45bind48209, %struct.ScmObj** %stackaddr$prim59788, align 8
%stackaddr$prim59789 = alloca %struct.ScmObj*, align 8
%cpsargs48433 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48423, %struct.ScmObj* %anf_45bind48209)
store volatile %struct.ScmObj* %cpsargs48433, %struct.ScmObj** %stackaddr$prim59789, align 8
%clofunc59790 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48064)
musttail call tailcc void %clofunc59790(%struct.ScmObj* %f48064, %struct.ScmObj* %cpsargs48433)
ret void
}

define tailcc void @proc_clo$ae49292(%struct.ScmObj* %env$ae49292,%struct.ScmObj* %current_45args57828) {
%stackaddr$prim59791 = alloca %struct.ScmObj*, align 8
%k48435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57828)
store volatile %struct.ScmObj* %k48435, %struct.ScmObj** %stackaddr$prim59791, align 8
%stackaddr$prim59792 = alloca %struct.ScmObj*, align 8
%current_45args57829 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57828)
store volatile %struct.ScmObj* %current_45args57829, %struct.ScmObj** %stackaddr$prim59792, align 8
%stackaddr$prim59793 = alloca %struct.ScmObj*, align 8
%a48072 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57829)
store volatile %struct.ScmObj* %a48072, %struct.ScmObj** %stackaddr$prim59793, align 8
%stackaddr$prim59794 = alloca %struct.ScmObj*, align 8
%current_45args57830 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57829)
store volatile %struct.ScmObj* %current_45args57830, %struct.ScmObj** %stackaddr$prim59794, align 8
%stackaddr$prim59795 = alloca %struct.ScmObj*, align 8
%b48071 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57830)
store volatile %struct.ScmObj* %b48071, %struct.ScmObj** %stackaddr$prim59795, align 8
%stackaddr$prim59796 = alloca %struct.ScmObj*, align 8
%cpsprim48436 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48072, %struct.ScmObj* %b48071)
store volatile %struct.ScmObj* %cpsprim48436, %struct.ScmObj** %stackaddr$prim59796, align 8
%ae49296 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57832$k484350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59797 = alloca %struct.ScmObj*, align 8
%argslist57832$k484351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48436, %struct.ScmObj* %argslist57832$k484350)
store volatile %struct.ScmObj* %argslist57832$k484351, %struct.ScmObj** %stackaddr$prim59797, align 8
%stackaddr$prim59798 = alloca %struct.ScmObj*, align 8
%argslist57832$k484352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49296, %struct.ScmObj* %argslist57832$k484351)
store volatile %struct.ScmObj* %argslist57832$k484352, %struct.ScmObj** %stackaddr$prim59798, align 8
%clofunc59799 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48435)
musttail call tailcc void %clofunc59799(%struct.ScmObj* %k48435, %struct.ScmObj* %argslist57832$k484352)
ret void
}

define tailcc void @proc_clo$ae49268(%struct.ScmObj* %env$ae49268,%struct.ScmObj* %current_45args57835) {
%stackaddr$prim59800 = alloca %struct.ScmObj*, align 8
%k48437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57835)
store volatile %struct.ScmObj* %k48437, %struct.ScmObj** %stackaddr$prim59800, align 8
%stackaddr$prim59801 = alloca %struct.ScmObj*, align 8
%current_45args57836 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57835)
store volatile %struct.ScmObj* %current_45args57836, %struct.ScmObj** %stackaddr$prim59801, align 8
%stackaddr$prim59802 = alloca %struct.ScmObj*, align 8
%x48068 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57836)
store volatile %struct.ScmObj* %x48068, %struct.ScmObj** %stackaddr$prim59802, align 8
%stackaddr$prim59803 = alloca %struct.ScmObj*, align 8
%cpsprim48438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48068)
store volatile %struct.ScmObj* %cpsprim48438, %struct.ScmObj** %stackaddr$prim59803, align 8
%ae49271 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57838$k484370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59804 = alloca %struct.ScmObj*, align 8
%argslist57838$k484371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48438, %struct.ScmObj* %argslist57838$k484370)
store volatile %struct.ScmObj* %argslist57838$k484371, %struct.ScmObj** %stackaddr$prim59804, align 8
%stackaddr$prim59805 = alloca %struct.ScmObj*, align 8
%argslist57838$k484372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49271, %struct.ScmObj* %argslist57838$k484371)
store volatile %struct.ScmObj* %argslist57838$k484372, %struct.ScmObj** %stackaddr$prim59805, align 8
%clofunc59806 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48437)
musttail call tailcc void %clofunc59806(%struct.ScmObj* %k48437, %struct.ScmObj* %argslist57838$k484372)
ret void
}

define tailcc void @proc_clo$ae49244(%struct.ScmObj* %env$ae49244,%struct.ScmObj* %current_45args57841) {
%stackaddr$prim59807 = alloca %struct.ScmObj*, align 8
%k48439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57841)
store volatile %struct.ScmObj* %k48439, %struct.ScmObj** %stackaddr$prim59807, align 8
%stackaddr$prim59808 = alloca %struct.ScmObj*, align 8
%current_45args57842 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57841)
store volatile %struct.ScmObj* %current_45args57842, %struct.ScmObj** %stackaddr$prim59808, align 8
%stackaddr$prim59809 = alloca %struct.ScmObj*, align 8
%x48070 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57842)
store volatile %struct.ScmObj* %x48070, %struct.ScmObj** %stackaddr$prim59809, align 8
%stackaddr$prim59810 = alloca %struct.ScmObj*, align 8
%cpsprim48440 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48070)
store volatile %struct.ScmObj* %cpsprim48440, %struct.ScmObj** %stackaddr$prim59810, align 8
%ae49247 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57844$k484390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59811 = alloca %struct.ScmObj*, align 8
%argslist57844$k484391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48440, %struct.ScmObj* %argslist57844$k484390)
store volatile %struct.ScmObj* %argslist57844$k484391, %struct.ScmObj** %stackaddr$prim59811, align 8
%stackaddr$prim59812 = alloca %struct.ScmObj*, align 8
%argslist57844$k484392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49247, %struct.ScmObj* %argslist57844$k484391)
store volatile %struct.ScmObj* %argslist57844$k484392, %struct.ScmObj** %stackaddr$prim59812, align 8
%clofunc59813 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48439)
musttail call tailcc void %clofunc59813(%struct.ScmObj* %k48439, %struct.ScmObj* %argslist57844$k484392)
ret void
}

define tailcc void @proc_clo$ae49196(%struct.ScmObj* %env$ae49196,%struct.ScmObj* %current_45args57847) {
%stackaddr$prim59814 = alloca %struct.ScmObj*, align 8
%k48441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57847)
store volatile %struct.ScmObj* %k48441, %struct.ScmObj** %stackaddr$prim59814, align 8
%stackaddr$prim59815 = alloca %struct.ScmObj*, align 8
%current_45args57848 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57847)
store volatile %struct.ScmObj* %current_45args57848, %struct.ScmObj** %stackaddr$prim59815, align 8
%stackaddr$prim59816 = alloca %struct.ScmObj*, align 8
%lst48066 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57848)
store volatile %struct.ScmObj* %lst48066, %struct.ScmObj** %stackaddr$prim59816, align 8
%stackaddr$prim59817 = alloca %struct.ScmObj*, align 8
%current_45args57849 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57848)
store volatile %struct.ScmObj* %current_45args57849, %struct.ScmObj** %stackaddr$prim59817, align 8
%stackaddr$prim59818 = alloca %struct.ScmObj*, align 8
%b48065 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57849)
store volatile %struct.ScmObj* %b48065, %struct.ScmObj** %stackaddr$prim59818, align 8
%truthy$cmp59819 = call i64 @is_truthy_value(%struct.ScmObj* %b48065)
%cmp$cmp59819 = icmp eq i64 %truthy$cmp59819, 1
br i1 %cmp$cmp59819, label %truebranch$cmp59819, label %falsebranch$cmp59819
truebranch$cmp59819:
%ae49199 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57851$k484410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59820 = alloca %struct.ScmObj*, align 8
%argslist57851$k484411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48065, %struct.ScmObj* %argslist57851$k484410)
store volatile %struct.ScmObj* %argslist57851$k484411, %struct.ScmObj** %stackaddr$prim59820, align 8
%stackaddr$prim59821 = alloca %struct.ScmObj*, align 8
%argslist57851$k484412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49199, %struct.ScmObj* %argslist57851$k484411)
store volatile %struct.ScmObj* %argslist57851$k484412, %struct.ScmObj** %stackaddr$prim59821, align 8
%clofunc59822 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48441)
musttail call tailcc void %clofunc59822(%struct.ScmObj* %k48441, %struct.ScmObj* %argslist57851$k484412)
ret void
falsebranch$cmp59819:
%stackaddr$prim59823 = alloca %struct.ScmObj*, align 8
%cpsprim48442 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48066)
store volatile %struct.ScmObj* %cpsprim48442, %struct.ScmObj** %stackaddr$prim59823, align 8
%ae49206 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57852$k484410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59824 = alloca %struct.ScmObj*, align 8
%argslist57852$k484411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48442, %struct.ScmObj* %argslist57852$k484410)
store volatile %struct.ScmObj* %argslist57852$k484411, %struct.ScmObj** %stackaddr$prim59824, align 8
%stackaddr$prim59825 = alloca %struct.ScmObj*, align 8
%argslist57852$k484412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49206, %struct.ScmObj* %argslist57852$k484411)
store volatile %struct.ScmObj* %argslist57852$k484412, %struct.ScmObj** %stackaddr$prim59825, align 8
%clofunc59826 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48441)
musttail call tailcc void %clofunc59826(%struct.ScmObj* %k48441, %struct.ScmObj* %argslist57852$k484412)
ret void
}

define tailcc void @proc_clo$ae49153(%struct.ScmObj* %env$ae49153,%struct.ScmObj* %current_45args57856) {
%stackaddr$env-ref59827 = alloca %struct.ScmObj*, align 8
%_37take48046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49153, i64 0)
store %struct.ScmObj* %_37take48046, %struct.ScmObj** %stackaddr$env-ref59827
%stackaddr$env-ref59828 = alloca %struct.ScmObj*, align 8
%_37length48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49153, i64 1)
store %struct.ScmObj* %_37length48043, %struct.ScmObj** %stackaddr$env-ref59828
%stackaddr$prim59829 = alloca %struct.ScmObj*, align 8
%k48443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57856)
store volatile %struct.ScmObj* %k48443, %struct.ScmObj** %stackaddr$prim59829, align 8
%stackaddr$prim59830 = alloca %struct.ScmObj*, align 8
%current_45args57857 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57856)
store volatile %struct.ScmObj* %current_45args57857, %struct.ScmObj** %stackaddr$prim59830, align 8
%stackaddr$prim59831 = alloca %struct.ScmObj*, align 8
%lst48075 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57857)
store volatile %struct.ScmObj* %lst48075, %struct.ScmObj** %stackaddr$prim59831, align 8
%stackaddr$prim59832 = alloca %struct.ScmObj*, align 8
%current_45args57858 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57857)
store volatile %struct.ScmObj* %current_45args57858, %struct.ScmObj** %stackaddr$prim59832, align 8
%stackaddr$prim59833 = alloca %struct.ScmObj*, align 8
%n48074 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57858)
store volatile %struct.ScmObj* %n48074, %struct.ScmObj** %stackaddr$prim59833, align 8
%stackaddr$makeclosure59834 = alloca %struct.ScmObj*, align 8
%fptrToInt59835 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49155 to i64
%ae49155 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt59835)
store volatile %struct.ScmObj* %ae49155, %struct.ScmObj** %stackaddr$makeclosure59834, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49155, %struct.ScmObj* %lst48075, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49155, %struct.ScmObj* %k48443, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49155, %struct.ScmObj* %_37take48046, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49155, %struct.ScmObj* %n48074, i64 3)
%argslist57864$_37length480430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59836 = alloca %struct.ScmObj*, align 8
%argslist57864$_37length480431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48075, %struct.ScmObj* %argslist57864$_37length480430)
store volatile %struct.ScmObj* %argslist57864$_37length480431, %struct.ScmObj** %stackaddr$prim59836, align 8
%stackaddr$prim59837 = alloca %struct.ScmObj*, align 8
%argslist57864$_37length480432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49155, %struct.ScmObj* %argslist57864$_37length480431)
store volatile %struct.ScmObj* %argslist57864$_37length480432, %struct.ScmObj** %stackaddr$prim59837, align 8
%clofunc59838 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48043)
musttail call tailcc void %clofunc59838(%struct.ScmObj* %_37length48043, %struct.ScmObj* %argslist57864$_37length480432)
ret void
}

define tailcc void @proc_clo$ae49155(%struct.ScmObj* %env$ae49155,%struct.ScmObj* %current_45args57860) {
%stackaddr$env-ref59839 = alloca %struct.ScmObj*, align 8
%lst48075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49155, i64 0)
store %struct.ScmObj* %lst48075, %struct.ScmObj** %stackaddr$env-ref59839
%stackaddr$env-ref59840 = alloca %struct.ScmObj*, align 8
%k48443 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49155, i64 1)
store %struct.ScmObj* %k48443, %struct.ScmObj** %stackaddr$env-ref59840
%stackaddr$env-ref59841 = alloca %struct.ScmObj*, align 8
%_37take48046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49155, i64 2)
store %struct.ScmObj* %_37take48046, %struct.ScmObj** %stackaddr$env-ref59841
%stackaddr$env-ref59842 = alloca %struct.ScmObj*, align 8
%n48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49155, i64 3)
store %struct.ScmObj* %n48074, %struct.ScmObj** %stackaddr$env-ref59842
%stackaddr$prim59843 = alloca %struct.ScmObj*, align 8
%_95k48444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57860)
store volatile %struct.ScmObj* %_95k48444, %struct.ScmObj** %stackaddr$prim59843, align 8
%stackaddr$prim59844 = alloca %struct.ScmObj*, align 8
%current_45args57861 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57860)
store volatile %struct.ScmObj* %current_45args57861, %struct.ScmObj** %stackaddr$prim59844, align 8
%stackaddr$prim59845 = alloca %struct.ScmObj*, align 8
%anf_45bind48196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57861)
store volatile %struct.ScmObj* %anf_45bind48196, %struct.ScmObj** %stackaddr$prim59845, align 8
%stackaddr$prim59846 = alloca %struct.ScmObj*, align 8
%anf_45bind48197 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48196, %struct.ScmObj* %n48074)
store volatile %struct.ScmObj* %anf_45bind48197, %struct.ScmObj** %stackaddr$prim59846, align 8
%argslist57863$_37take480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59847 = alloca %struct.ScmObj*, align 8
%argslist57863$_37take480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48197, %struct.ScmObj* %argslist57863$_37take480460)
store volatile %struct.ScmObj* %argslist57863$_37take480461, %struct.ScmObj** %stackaddr$prim59847, align 8
%stackaddr$prim59848 = alloca %struct.ScmObj*, align 8
%argslist57863$_37take480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48075, %struct.ScmObj* %argslist57863$_37take480461)
store volatile %struct.ScmObj* %argslist57863$_37take480462, %struct.ScmObj** %stackaddr$prim59848, align 8
%stackaddr$prim59849 = alloca %struct.ScmObj*, align 8
%argslist57863$_37take480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48443, %struct.ScmObj* %argslist57863$_37take480462)
store volatile %struct.ScmObj* %argslist57863$_37take480463, %struct.ScmObj** %stackaddr$prim59849, align 8
%clofunc59850 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48046)
musttail call tailcc void %clofunc59850(%struct.ScmObj* %_37take48046, %struct.ScmObj* %argslist57863$_37take480463)
ret void
}

define tailcc void @proc_clo$ae49099(%struct.ScmObj* %env$ae49099,%struct.ScmObj* %current_45args57866) {
%stackaddr$env-ref59851 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49099, i64 0)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref59851
%stackaddr$prim59852 = alloca %struct.ScmObj*, align 8
%k48445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57866)
store volatile %struct.ScmObj* %k48445, %struct.ScmObj** %stackaddr$prim59852, align 8
%stackaddr$prim59853 = alloca %struct.ScmObj*, align 8
%current_45args57867 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57866)
store volatile %struct.ScmObj* %current_45args57867, %struct.ScmObj** %stackaddr$prim59853, align 8
%stackaddr$prim59854 = alloca %struct.ScmObj*, align 8
%lst48077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57867)
store volatile %struct.ScmObj* %lst48077, %struct.ScmObj** %stackaddr$prim59854, align 8
%stackaddr$makeclosure59855 = alloca %struct.ScmObj*, align 8
%fptrToInt59856 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49100 to i64
%ae49100 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt59856)
store volatile %struct.ScmObj* %ae49100, %struct.ScmObj** %stackaddr$makeclosure59855, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49100, %struct.ScmObj* %lst48077, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49100, %struct.ScmObj* %k48445, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49100, %struct.ScmObj* %_37foldl148038, i64 2)
%ae49101 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59857 = alloca %struct.ScmObj*, align 8
%fptrToInt59858 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49102 to i64
%ae49102 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt59858)
store volatile %struct.ScmObj* %ae49102, %struct.ScmObj** %stackaddr$makeclosure59857, align 8
%argslist57878$ae491000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59859 = alloca %struct.ScmObj*, align 8
%argslist57878$ae491001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49102, %struct.ScmObj* %argslist57878$ae491000)
store volatile %struct.ScmObj* %argslist57878$ae491001, %struct.ScmObj** %stackaddr$prim59859, align 8
%stackaddr$prim59860 = alloca %struct.ScmObj*, align 8
%argslist57878$ae491002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49101, %struct.ScmObj* %argslist57878$ae491001)
store volatile %struct.ScmObj* %argslist57878$ae491002, %struct.ScmObj** %stackaddr$prim59860, align 8
%clofunc59861 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49100)
musttail call tailcc void %clofunc59861(%struct.ScmObj* %ae49100, %struct.ScmObj* %argslist57878$ae491002)
ret void
}

define tailcc void @proc_clo$ae49100(%struct.ScmObj* %env$ae49100,%struct.ScmObj* %current_45args57869) {
%stackaddr$env-ref59862 = alloca %struct.ScmObj*, align 8
%lst48077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49100, i64 0)
store %struct.ScmObj* %lst48077, %struct.ScmObj** %stackaddr$env-ref59862
%stackaddr$env-ref59863 = alloca %struct.ScmObj*, align 8
%k48445 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49100, i64 1)
store %struct.ScmObj* %k48445, %struct.ScmObj** %stackaddr$env-ref59863
%stackaddr$env-ref59864 = alloca %struct.ScmObj*, align 8
%_37foldl148038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49100, i64 2)
store %struct.ScmObj* %_37foldl148038, %struct.ScmObj** %stackaddr$env-ref59864
%stackaddr$prim59865 = alloca %struct.ScmObj*, align 8
%_95k48446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57869)
store volatile %struct.ScmObj* %_95k48446, %struct.ScmObj** %stackaddr$prim59865, align 8
%stackaddr$prim59866 = alloca %struct.ScmObj*, align 8
%current_45args57870 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57869)
store volatile %struct.ScmObj* %current_45args57870, %struct.ScmObj** %stackaddr$prim59866, align 8
%stackaddr$prim59867 = alloca %struct.ScmObj*, align 8
%anf_45bind48195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57870)
store volatile %struct.ScmObj* %anf_45bind48195, %struct.ScmObj** %stackaddr$prim59867, align 8
%ae49121 = call %struct.ScmObj* @const_init_null()
%argslist57872$_37foldl1480380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59868 = alloca %struct.ScmObj*, align 8
%argslist57872$_37foldl1480381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48077, %struct.ScmObj* %argslist57872$_37foldl1480380)
store volatile %struct.ScmObj* %argslist57872$_37foldl1480381, %struct.ScmObj** %stackaddr$prim59868, align 8
%stackaddr$prim59869 = alloca %struct.ScmObj*, align 8
%argslist57872$_37foldl1480382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49121, %struct.ScmObj* %argslist57872$_37foldl1480381)
store volatile %struct.ScmObj* %argslist57872$_37foldl1480382, %struct.ScmObj** %stackaddr$prim59869, align 8
%stackaddr$prim59870 = alloca %struct.ScmObj*, align 8
%argslist57872$_37foldl1480383 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48195, %struct.ScmObj* %argslist57872$_37foldl1480382)
store volatile %struct.ScmObj* %argslist57872$_37foldl1480383, %struct.ScmObj** %stackaddr$prim59870, align 8
%stackaddr$prim59871 = alloca %struct.ScmObj*, align 8
%argslist57872$_37foldl1480384 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48445, %struct.ScmObj* %argslist57872$_37foldl1480383)
store volatile %struct.ScmObj* %argslist57872$_37foldl1480384, %struct.ScmObj** %stackaddr$prim59871, align 8
%clofunc59872 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148038)
musttail call tailcc void %clofunc59872(%struct.ScmObj* %_37foldl148038, %struct.ScmObj* %argslist57872$_37foldl1480384)
ret void
}

define tailcc void @proc_clo$ae49102(%struct.ScmObj* %env$ae49102,%struct.ScmObj* %current_45args57873) {
%stackaddr$prim59873 = alloca %struct.ScmObj*, align 8
%k48447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57873)
store volatile %struct.ScmObj* %k48447, %struct.ScmObj** %stackaddr$prim59873, align 8
%stackaddr$prim59874 = alloca %struct.ScmObj*, align 8
%current_45args57874 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57873)
store volatile %struct.ScmObj* %current_45args57874, %struct.ScmObj** %stackaddr$prim59874, align 8
%stackaddr$prim59875 = alloca %struct.ScmObj*, align 8
%x48079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57874)
store volatile %struct.ScmObj* %x48079, %struct.ScmObj** %stackaddr$prim59875, align 8
%stackaddr$prim59876 = alloca %struct.ScmObj*, align 8
%current_45args57875 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57874)
store volatile %struct.ScmObj* %current_45args57875, %struct.ScmObj** %stackaddr$prim59876, align 8
%stackaddr$prim59877 = alloca %struct.ScmObj*, align 8
%y48078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57875)
store volatile %struct.ScmObj* %y48078, %struct.ScmObj** %stackaddr$prim59877, align 8
%ae49104 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57877$k484470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59878 = alloca %struct.ScmObj*, align 8
%argslist57877$k484471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x48079, %struct.ScmObj* %argslist57877$k484470)
store volatile %struct.ScmObj* %argslist57877$k484471, %struct.ScmObj** %stackaddr$prim59878, align 8
%stackaddr$prim59879 = alloca %struct.ScmObj*, align 8
%argslist57877$k484472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49104, %struct.ScmObj* %argslist57877$k484471)
store volatile %struct.ScmObj* %argslist57877$k484472, %struct.ScmObj** %stackaddr$prim59879, align 8
%clofunc59880 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48447)
musttail call tailcc void %clofunc59880(%struct.ScmObj* %k48447, %struct.ScmObj* %argslist57877$k484472)
ret void
}

define tailcc void @proc_clo$ae49020(%struct.ScmObj* %env$ae49020,%struct.ScmObj* %current_45args57881) {
%stackaddr$prim59881 = alloca %struct.ScmObj*, align 8
%k48448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57881)
store volatile %struct.ScmObj* %k48448, %struct.ScmObj** %stackaddr$prim59881, align 8
%stackaddr$prim59882 = alloca %struct.ScmObj*, align 8
%current_45args57882 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57881)
store volatile %struct.ScmObj* %current_45args57882, %struct.ScmObj** %stackaddr$prim59882, align 8
%stackaddr$prim59883 = alloca %struct.ScmObj*, align 8
%_37foldl148039 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57882)
store volatile %struct.ScmObj* %_37foldl148039, %struct.ScmObj** %stackaddr$prim59883, align 8
%ae49022 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59884 = alloca %struct.ScmObj*, align 8
%fptrToInt59885 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49023 to i64
%ae49023 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt59885)
store volatile %struct.ScmObj* %ae49023, %struct.ScmObj** %stackaddr$makeclosure59884, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49023, %struct.ScmObj* %_37foldl148039, i64 0)
%argslist57895$k484480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59886 = alloca %struct.ScmObj*, align 8
%argslist57895$k484481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49023, %struct.ScmObj* %argslist57895$k484480)
store volatile %struct.ScmObj* %argslist57895$k484481, %struct.ScmObj** %stackaddr$prim59886, align 8
%stackaddr$prim59887 = alloca %struct.ScmObj*, align 8
%argslist57895$k484482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49022, %struct.ScmObj* %argslist57895$k484481)
store volatile %struct.ScmObj* %argslist57895$k484482, %struct.ScmObj** %stackaddr$prim59887, align 8
%clofunc59888 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48448)
musttail call tailcc void %clofunc59888(%struct.ScmObj* %k48448, %struct.ScmObj* %argslist57895$k484482)
ret void
}

define tailcc void @proc_clo$ae49023(%struct.ScmObj* %env$ae49023,%struct.ScmObj* %current_45args57884) {
%stackaddr$env-ref59889 = alloca %struct.ScmObj*, align 8
%_37foldl148039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49023, i64 0)
store %struct.ScmObj* %_37foldl148039, %struct.ScmObj** %stackaddr$env-ref59889
%stackaddr$prim59890 = alloca %struct.ScmObj*, align 8
%k48449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57884)
store volatile %struct.ScmObj* %k48449, %struct.ScmObj** %stackaddr$prim59890, align 8
%stackaddr$prim59891 = alloca %struct.ScmObj*, align 8
%current_45args57885 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57884)
store volatile %struct.ScmObj* %current_45args57885, %struct.ScmObj** %stackaddr$prim59891, align 8
%stackaddr$prim59892 = alloca %struct.ScmObj*, align 8
%f48042 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57885)
store volatile %struct.ScmObj* %f48042, %struct.ScmObj** %stackaddr$prim59892, align 8
%stackaddr$prim59893 = alloca %struct.ScmObj*, align 8
%current_45args57886 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57885)
store volatile %struct.ScmObj* %current_45args57886, %struct.ScmObj** %stackaddr$prim59893, align 8
%stackaddr$prim59894 = alloca %struct.ScmObj*, align 8
%acc48041 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57886)
store volatile %struct.ScmObj* %acc48041, %struct.ScmObj** %stackaddr$prim59894, align 8
%stackaddr$prim59895 = alloca %struct.ScmObj*, align 8
%current_45args57887 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57886)
store volatile %struct.ScmObj* %current_45args57887, %struct.ScmObj** %stackaddr$prim59895, align 8
%stackaddr$prim59896 = alloca %struct.ScmObj*, align 8
%lst48040 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57887)
store volatile %struct.ScmObj* %lst48040, %struct.ScmObj** %stackaddr$prim59896, align 8
%stackaddr$prim59897 = alloca %struct.ScmObj*, align 8
%anf_45bind48190 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48040)
store volatile %struct.ScmObj* %anf_45bind48190, %struct.ScmObj** %stackaddr$prim59897, align 8
%truthy$cmp59898 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48190)
%cmp$cmp59898 = icmp eq i64 %truthy$cmp59898, 1
br i1 %cmp$cmp59898, label %truebranch$cmp59898, label %falsebranch$cmp59898
truebranch$cmp59898:
%ae49027 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57889$k484490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59899 = alloca %struct.ScmObj*, align 8
%argslist57889$k484491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48041, %struct.ScmObj* %argslist57889$k484490)
store volatile %struct.ScmObj* %argslist57889$k484491, %struct.ScmObj** %stackaddr$prim59899, align 8
%stackaddr$prim59900 = alloca %struct.ScmObj*, align 8
%argslist57889$k484492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49027, %struct.ScmObj* %argslist57889$k484491)
store volatile %struct.ScmObj* %argslist57889$k484492, %struct.ScmObj** %stackaddr$prim59900, align 8
%clofunc59901 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48449)
musttail call tailcc void %clofunc59901(%struct.ScmObj* %k48449, %struct.ScmObj* %argslist57889$k484492)
ret void
falsebranch$cmp59898:
%stackaddr$prim59902 = alloca %struct.ScmObj*, align 8
%anf_45bind48191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48040)
store volatile %struct.ScmObj* %anf_45bind48191, %struct.ScmObj** %stackaddr$prim59902, align 8
%stackaddr$makeclosure59903 = alloca %struct.ScmObj*, align 8
%fptrToInt59904 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49034 to i64
%ae49034 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt59904)
store volatile %struct.ScmObj* %ae49034, %struct.ScmObj** %stackaddr$makeclosure59903, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49034, %struct.ScmObj* %lst48040, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49034, %struct.ScmObj* %_37foldl148039, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49034, %struct.ScmObj* %k48449, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49034, %struct.ScmObj* %f48042, i64 3)
%argslist57894$f480420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59905 = alloca %struct.ScmObj*, align 8
%argslist57894$f480421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48041, %struct.ScmObj* %argslist57894$f480420)
store volatile %struct.ScmObj* %argslist57894$f480421, %struct.ScmObj** %stackaddr$prim59905, align 8
%stackaddr$prim59906 = alloca %struct.ScmObj*, align 8
%argslist57894$f480422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48191, %struct.ScmObj* %argslist57894$f480421)
store volatile %struct.ScmObj* %argslist57894$f480422, %struct.ScmObj** %stackaddr$prim59906, align 8
%stackaddr$prim59907 = alloca %struct.ScmObj*, align 8
%argslist57894$f480423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49034, %struct.ScmObj* %argslist57894$f480422)
store volatile %struct.ScmObj* %argslist57894$f480423, %struct.ScmObj** %stackaddr$prim59907, align 8
%clofunc59908 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48042)
musttail call tailcc void %clofunc59908(%struct.ScmObj* %f48042, %struct.ScmObj* %argslist57894$f480423)
ret void
}

define tailcc void @proc_clo$ae49034(%struct.ScmObj* %env$ae49034,%struct.ScmObj* %current_45args57890) {
%stackaddr$env-ref59909 = alloca %struct.ScmObj*, align 8
%lst48040 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49034, i64 0)
store %struct.ScmObj* %lst48040, %struct.ScmObj** %stackaddr$env-ref59909
%stackaddr$env-ref59910 = alloca %struct.ScmObj*, align 8
%_37foldl148039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49034, i64 1)
store %struct.ScmObj* %_37foldl148039, %struct.ScmObj** %stackaddr$env-ref59910
%stackaddr$env-ref59911 = alloca %struct.ScmObj*, align 8
%k48449 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49034, i64 2)
store %struct.ScmObj* %k48449, %struct.ScmObj** %stackaddr$env-ref59911
%stackaddr$env-ref59912 = alloca %struct.ScmObj*, align 8
%f48042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49034, i64 3)
store %struct.ScmObj* %f48042, %struct.ScmObj** %stackaddr$env-ref59912
%stackaddr$prim59913 = alloca %struct.ScmObj*, align 8
%_95k48450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57890)
store volatile %struct.ScmObj* %_95k48450, %struct.ScmObj** %stackaddr$prim59913, align 8
%stackaddr$prim59914 = alloca %struct.ScmObj*, align 8
%current_45args57891 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57890)
store volatile %struct.ScmObj* %current_45args57891, %struct.ScmObj** %stackaddr$prim59914, align 8
%stackaddr$prim59915 = alloca %struct.ScmObj*, align 8
%anf_45bind48192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57891)
store volatile %struct.ScmObj* %anf_45bind48192, %struct.ScmObj** %stackaddr$prim59915, align 8
%stackaddr$prim59916 = alloca %struct.ScmObj*, align 8
%anf_45bind48193 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48040)
store volatile %struct.ScmObj* %anf_45bind48193, %struct.ScmObj** %stackaddr$prim59916, align 8
%argslist57893$_37foldl1480390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59917 = alloca %struct.ScmObj*, align 8
%argslist57893$_37foldl1480391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48193, %struct.ScmObj* %argslist57893$_37foldl1480390)
store volatile %struct.ScmObj* %argslist57893$_37foldl1480391, %struct.ScmObj** %stackaddr$prim59917, align 8
%stackaddr$prim59918 = alloca %struct.ScmObj*, align 8
%argslist57893$_37foldl1480392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48192, %struct.ScmObj* %argslist57893$_37foldl1480391)
store volatile %struct.ScmObj* %argslist57893$_37foldl1480392, %struct.ScmObj** %stackaddr$prim59918, align 8
%stackaddr$prim59919 = alloca %struct.ScmObj*, align 8
%argslist57893$_37foldl1480393 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48042, %struct.ScmObj* %argslist57893$_37foldl1480392)
store volatile %struct.ScmObj* %argslist57893$_37foldl1480393, %struct.ScmObj** %stackaddr$prim59919, align 8
%stackaddr$prim59920 = alloca %struct.ScmObj*, align 8
%argslist57893$_37foldl1480394 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48449, %struct.ScmObj* %argslist57893$_37foldl1480393)
store volatile %struct.ScmObj* %argslist57893$_37foldl1480394, %struct.ScmObj** %stackaddr$prim59920, align 8
%clofunc59921 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148039)
musttail call tailcc void %clofunc59921(%struct.ScmObj* %_37foldl148039, %struct.ScmObj* %argslist57893$_37foldl1480394)
ret void
}

define tailcc void @proc_clo$ae48937(%struct.ScmObj* %env$ae48937,%struct.ScmObj* %current_45args57898) {
%stackaddr$prim59922 = alloca %struct.ScmObj*, align 8
%k48451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57898)
store volatile %struct.ScmObj* %k48451, %struct.ScmObj** %stackaddr$prim59922, align 8
%stackaddr$prim59923 = alloca %struct.ScmObj*, align 8
%current_45args57899 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57898)
store volatile %struct.ScmObj* %current_45args57899, %struct.ScmObj** %stackaddr$prim59923, align 8
%stackaddr$prim59924 = alloca %struct.ScmObj*, align 8
%_37length48044 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57899)
store volatile %struct.ScmObj* %_37length48044, %struct.ScmObj** %stackaddr$prim59924, align 8
%ae48939 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59925 = alloca %struct.ScmObj*, align 8
%fptrToInt59926 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48940 to i64
%ae48940 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt59926)
store volatile %struct.ScmObj* %ae48940, %struct.ScmObj** %stackaddr$makeclosure59925, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48940, %struct.ScmObj* %_37length48044, i64 0)
%argslist57910$k484510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59927 = alloca %struct.ScmObj*, align 8
%argslist57910$k484511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48940, %struct.ScmObj* %argslist57910$k484510)
store volatile %struct.ScmObj* %argslist57910$k484511, %struct.ScmObj** %stackaddr$prim59927, align 8
%stackaddr$prim59928 = alloca %struct.ScmObj*, align 8
%argslist57910$k484512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48939, %struct.ScmObj* %argslist57910$k484511)
store volatile %struct.ScmObj* %argslist57910$k484512, %struct.ScmObj** %stackaddr$prim59928, align 8
%clofunc59929 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48451)
musttail call tailcc void %clofunc59929(%struct.ScmObj* %k48451, %struct.ScmObj* %argslist57910$k484512)
ret void
}

define tailcc void @proc_clo$ae48940(%struct.ScmObj* %env$ae48940,%struct.ScmObj* %current_45args57901) {
%stackaddr$env-ref59930 = alloca %struct.ScmObj*, align 8
%_37length48044 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48940, i64 0)
store %struct.ScmObj* %_37length48044, %struct.ScmObj** %stackaddr$env-ref59930
%stackaddr$prim59931 = alloca %struct.ScmObj*, align 8
%k48452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57901)
store volatile %struct.ScmObj* %k48452, %struct.ScmObj** %stackaddr$prim59931, align 8
%stackaddr$prim59932 = alloca %struct.ScmObj*, align 8
%current_45args57902 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57901)
store volatile %struct.ScmObj* %current_45args57902, %struct.ScmObj** %stackaddr$prim59932, align 8
%stackaddr$prim59933 = alloca %struct.ScmObj*, align 8
%lst48045 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57902)
store volatile %struct.ScmObj* %lst48045, %struct.ScmObj** %stackaddr$prim59933, align 8
%stackaddr$prim59934 = alloca %struct.ScmObj*, align 8
%anf_45bind48186 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48045)
store volatile %struct.ScmObj* %anf_45bind48186, %struct.ScmObj** %stackaddr$prim59934, align 8
%truthy$cmp59935 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48186)
%cmp$cmp59935 = icmp eq i64 %truthy$cmp59935, 1
br i1 %cmp$cmp59935, label %truebranch$cmp59935, label %falsebranch$cmp59935
truebranch$cmp59935:
%ae48944 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48945 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57904$k484520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59936 = alloca %struct.ScmObj*, align 8
%argslist57904$k484521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48945, %struct.ScmObj* %argslist57904$k484520)
store volatile %struct.ScmObj* %argslist57904$k484521, %struct.ScmObj** %stackaddr$prim59936, align 8
%stackaddr$prim59937 = alloca %struct.ScmObj*, align 8
%argslist57904$k484522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48944, %struct.ScmObj* %argslist57904$k484521)
store volatile %struct.ScmObj* %argslist57904$k484522, %struct.ScmObj** %stackaddr$prim59937, align 8
%clofunc59938 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48452)
musttail call tailcc void %clofunc59938(%struct.ScmObj* %k48452, %struct.ScmObj* %argslist57904$k484522)
ret void
falsebranch$cmp59935:
%stackaddr$prim59939 = alloca %struct.ScmObj*, align 8
%anf_45bind48187 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48045)
store volatile %struct.ScmObj* %anf_45bind48187, %struct.ScmObj** %stackaddr$prim59939, align 8
%stackaddr$makeclosure59940 = alloca %struct.ScmObj*, align 8
%fptrToInt59941 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48954 to i64
%ae48954 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt59941)
store volatile %struct.ScmObj* %ae48954, %struct.ScmObj** %stackaddr$makeclosure59940, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48954, %struct.ScmObj* %k48452, i64 0)
%argslist57909$_37length480440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59942 = alloca %struct.ScmObj*, align 8
%argslist57909$_37length480441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48187, %struct.ScmObj* %argslist57909$_37length480440)
store volatile %struct.ScmObj* %argslist57909$_37length480441, %struct.ScmObj** %stackaddr$prim59942, align 8
%stackaddr$prim59943 = alloca %struct.ScmObj*, align 8
%argslist57909$_37length480442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48954, %struct.ScmObj* %argslist57909$_37length480441)
store volatile %struct.ScmObj* %argslist57909$_37length480442, %struct.ScmObj** %stackaddr$prim59943, align 8
%clofunc59944 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48044)
musttail call tailcc void %clofunc59944(%struct.ScmObj* %_37length48044, %struct.ScmObj* %argslist57909$_37length480442)
ret void
}

define tailcc void @proc_clo$ae48954(%struct.ScmObj* %env$ae48954,%struct.ScmObj* %current_45args57905) {
%stackaddr$env-ref59945 = alloca %struct.ScmObj*, align 8
%k48452 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48954, i64 0)
store %struct.ScmObj* %k48452, %struct.ScmObj** %stackaddr$env-ref59945
%stackaddr$prim59946 = alloca %struct.ScmObj*, align 8
%_95k48453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57905)
store volatile %struct.ScmObj* %_95k48453, %struct.ScmObj** %stackaddr$prim59946, align 8
%stackaddr$prim59947 = alloca %struct.ScmObj*, align 8
%current_45args57906 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57905)
store volatile %struct.ScmObj* %current_45args57906, %struct.ScmObj** %stackaddr$prim59947, align 8
%stackaddr$prim59948 = alloca %struct.ScmObj*, align 8
%anf_45bind48188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57906)
store volatile %struct.ScmObj* %anf_45bind48188, %struct.ScmObj** %stackaddr$prim59948, align 8
%ae48956 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim59949 = alloca %struct.ScmObj*, align 8
%cpsprim48454 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae48956, %struct.ScmObj* %anf_45bind48188)
store volatile %struct.ScmObj* %cpsprim48454, %struct.ScmObj** %stackaddr$prim59949, align 8
%ae48959 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57908$k484520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59950 = alloca %struct.ScmObj*, align 8
%argslist57908$k484521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48454, %struct.ScmObj* %argslist57908$k484520)
store volatile %struct.ScmObj* %argslist57908$k484521, %struct.ScmObj** %stackaddr$prim59950, align 8
%stackaddr$prim59951 = alloca %struct.ScmObj*, align 8
%argslist57908$k484522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48959, %struct.ScmObj* %argslist57908$k484521)
store volatile %struct.ScmObj* %argslist57908$k484522, %struct.ScmObj** %stackaddr$prim59951, align 8
%clofunc59952 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48452)
musttail call tailcc void %clofunc59952(%struct.ScmObj* %k48452, %struct.ScmObj* %argslist57908$k484522)
ret void
}

define tailcc void @proc_clo$ae48787(%struct.ScmObj* %env$ae48787,%struct.ScmObj* %current_45args57913) {
%stackaddr$prim59953 = alloca %struct.ScmObj*, align 8
%k48455 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57913)
store volatile %struct.ScmObj* %k48455, %struct.ScmObj** %stackaddr$prim59953, align 8
%stackaddr$prim59954 = alloca %struct.ScmObj*, align 8
%current_45args57914 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57913)
store volatile %struct.ScmObj* %current_45args57914, %struct.ScmObj** %stackaddr$prim59954, align 8
%stackaddr$prim59955 = alloca %struct.ScmObj*, align 8
%_37take48047 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57914)
store volatile %struct.ScmObj* %_37take48047, %struct.ScmObj** %stackaddr$prim59955, align 8
%ae48789 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59956 = alloca %struct.ScmObj*, align 8
%fptrToInt59957 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48790 to i64
%ae48790 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt59957)
store volatile %struct.ScmObj* %ae48790, %struct.ScmObj** %stackaddr$makeclosure59956, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48790, %struct.ScmObj* %_37take48047, i64 0)
%argslist57927$k484550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59958 = alloca %struct.ScmObj*, align 8
%argslist57927$k484551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48790, %struct.ScmObj* %argslist57927$k484550)
store volatile %struct.ScmObj* %argslist57927$k484551, %struct.ScmObj** %stackaddr$prim59958, align 8
%stackaddr$prim59959 = alloca %struct.ScmObj*, align 8
%argslist57927$k484552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48789, %struct.ScmObj* %argslist57927$k484551)
store volatile %struct.ScmObj* %argslist57927$k484552, %struct.ScmObj** %stackaddr$prim59959, align 8
%clofunc59960 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48455)
musttail call tailcc void %clofunc59960(%struct.ScmObj* %k48455, %struct.ScmObj* %argslist57927$k484552)
ret void
}

define tailcc void @proc_clo$ae48790(%struct.ScmObj* %env$ae48790,%struct.ScmObj* %current_45args57916) {
%stackaddr$env-ref59961 = alloca %struct.ScmObj*, align 8
%_37take48047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48790, i64 0)
store %struct.ScmObj* %_37take48047, %struct.ScmObj** %stackaddr$env-ref59961
%stackaddr$prim59962 = alloca %struct.ScmObj*, align 8
%k48456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57916)
store volatile %struct.ScmObj* %k48456, %struct.ScmObj** %stackaddr$prim59962, align 8
%stackaddr$prim59963 = alloca %struct.ScmObj*, align 8
%current_45args57917 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57916)
store volatile %struct.ScmObj* %current_45args57917, %struct.ScmObj** %stackaddr$prim59963, align 8
%stackaddr$prim59964 = alloca %struct.ScmObj*, align 8
%lst48049 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57917)
store volatile %struct.ScmObj* %lst48049, %struct.ScmObj** %stackaddr$prim59964, align 8
%stackaddr$prim59965 = alloca %struct.ScmObj*, align 8
%current_45args57918 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57917)
store volatile %struct.ScmObj* %current_45args57918, %struct.ScmObj** %stackaddr$prim59965, align 8
%stackaddr$prim59966 = alloca %struct.ScmObj*, align 8
%n48048 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57918)
store volatile %struct.ScmObj* %n48048, %struct.ScmObj** %stackaddr$prim59966, align 8
%ae48792 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim59967 = alloca %struct.ScmObj*, align 8
%anf_45bind48179 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n48048, %struct.ScmObj* %ae48792)
store volatile %struct.ScmObj* %anf_45bind48179, %struct.ScmObj** %stackaddr$prim59967, align 8
%truthy$cmp59968 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48179)
%cmp$cmp59968 = icmp eq i64 %truthy$cmp59968, 1
br i1 %cmp$cmp59968, label %truebranch$cmp59968, label %falsebranch$cmp59968
truebranch$cmp59968:
%ae48795 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48796 = call %struct.ScmObj* @const_init_null()
%argslist57920$k484560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59969 = alloca %struct.ScmObj*, align 8
%argslist57920$k484561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48796, %struct.ScmObj* %argslist57920$k484560)
store volatile %struct.ScmObj* %argslist57920$k484561, %struct.ScmObj** %stackaddr$prim59969, align 8
%stackaddr$prim59970 = alloca %struct.ScmObj*, align 8
%argslist57920$k484562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48795, %struct.ScmObj* %argslist57920$k484561)
store volatile %struct.ScmObj* %argslist57920$k484562, %struct.ScmObj** %stackaddr$prim59970, align 8
%clofunc59971 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48456)
musttail call tailcc void %clofunc59971(%struct.ScmObj* %k48456, %struct.ScmObj* %argslist57920$k484562)
ret void
falsebranch$cmp59968:
%stackaddr$prim59972 = alloca %struct.ScmObj*, align 8
%anf_45bind48180 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48049)
store volatile %struct.ScmObj* %anf_45bind48180, %struct.ScmObj** %stackaddr$prim59972, align 8
%truthy$cmp59973 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48180)
%cmp$cmp59973 = icmp eq i64 %truthy$cmp59973, 1
br i1 %cmp$cmp59973, label %truebranch$cmp59973, label %falsebranch$cmp59973
truebranch$cmp59973:
%ae48806 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48807 = call %struct.ScmObj* @const_init_null()
%argslist57921$k484560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59974 = alloca %struct.ScmObj*, align 8
%argslist57921$k484561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48807, %struct.ScmObj* %argslist57921$k484560)
store volatile %struct.ScmObj* %argslist57921$k484561, %struct.ScmObj** %stackaddr$prim59974, align 8
%stackaddr$prim59975 = alloca %struct.ScmObj*, align 8
%argslist57921$k484562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48806, %struct.ScmObj* %argslist57921$k484561)
store volatile %struct.ScmObj* %argslist57921$k484562, %struct.ScmObj** %stackaddr$prim59975, align 8
%clofunc59976 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48456)
musttail call tailcc void %clofunc59976(%struct.ScmObj* %k48456, %struct.ScmObj* %argslist57921$k484562)
ret void
falsebranch$cmp59973:
%stackaddr$prim59977 = alloca %struct.ScmObj*, align 8
%anf_45bind48181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48049)
store volatile %struct.ScmObj* %anf_45bind48181, %struct.ScmObj** %stackaddr$prim59977, align 8
%stackaddr$prim59978 = alloca %struct.ScmObj*, align 8
%anf_45bind48182 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48049)
store volatile %struct.ScmObj* %anf_45bind48182, %struct.ScmObj** %stackaddr$prim59978, align 8
%ae48817 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim59979 = alloca %struct.ScmObj*, align 8
%anf_45bind48183 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n48048, %struct.ScmObj* %ae48817)
store volatile %struct.ScmObj* %anf_45bind48183, %struct.ScmObj** %stackaddr$prim59979, align 8
%stackaddr$makeclosure59980 = alloca %struct.ScmObj*, align 8
%fptrToInt59981 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48819 to i64
%ae48819 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt59981)
store volatile %struct.ScmObj* %ae48819, %struct.ScmObj** %stackaddr$makeclosure59980, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48819, %struct.ScmObj* %k48456, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48819, %struct.ScmObj* %anf_45bind48181, i64 1)
%argslist57926$_37take480470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59982 = alloca %struct.ScmObj*, align 8
%argslist57926$_37take480471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48183, %struct.ScmObj* %argslist57926$_37take480470)
store volatile %struct.ScmObj* %argslist57926$_37take480471, %struct.ScmObj** %stackaddr$prim59982, align 8
%stackaddr$prim59983 = alloca %struct.ScmObj*, align 8
%argslist57926$_37take480472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48182, %struct.ScmObj* %argslist57926$_37take480471)
store volatile %struct.ScmObj* %argslist57926$_37take480472, %struct.ScmObj** %stackaddr$prim59983, align 8
%stackaddr$prim59984 = alloca %struct.ScmObj*, align 8
%argslist57926$_37take480473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48819, %struct.ScmObj* %argslist57926$_37take480472)
store volatile %struct.ScmObj* %argslist57926$_37take480473, %struct.ScmObj** %stackaddr$prim59984, align 8
%clofunc59985 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48047)
musttail call tailcc void %clofunc59985(%struct.ScmObj* %_37take48047, %struct.ScmObj* %argslist57926$_37take480473)
ret void
}

define tailcc void @proc_clo$ae48819(%struct.ScmObj* %env$ae48819,%struct.ScmObj* %current_45args57922) {
%stackaddr$env-ref59986 = alloca %struct.ScmObj*, align 8
%k48456 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48819, i64 0)
store %struct.ScmObj* %k48456, %struct.ScmObj** %stackaddr$env-ref59986
%stackaddr$env-ref59987 = alloca %struct.ScmObj*, align 8
%anf_45bind48181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48819, i64 1)
store %struct.ScmObj* %anf_45bind48181, %struct.ScmObj** %stackaddr$env-ref59987
%stackaddr$prim59988 = alloca %struct.ScmObj*, align 8
%_95k48457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57922)
store volatile %struct.ScmObj* %_95k48457, %struct.ScmObj** %stackaddr$prim59988, align 8
%stackaddr$prim59989 = alloca %struct.ScmObj*, align 8
%current_45args57923 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57922)
store volatile %struct.ScmObj* %current_45args57923, %struct.ScmObj** %stackaddr$prim59989, align 8
%stackaddr$prim59990 = alloca %struct.ScmObj*, align 8
%anf_45bind48184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57923)
store volatile %struct.ScmObj* %anf_45bind48184, %struct.ScmObj** %stackaddr$prim59990, align 8
%stackaddr$prim59991 = alloca %struct.ScmObj*, align 8
%cpsprim48458 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48181, %struct.ScmObj* %anf_45bind48184)
store volatile %struct.ScmObj* %cpsprim48458, %struct.ScmObj** %stackaddr$prim59991, align 8
%ae48825 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57925$k484560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim59992 = alloca %struct.ScmObj*, align 8
%argslist57925$k484561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48458, %struct.ScmObj* %argslist57925$k484560)
store volatile %struct.ScmObj* %argslist57925$k484561, %struct.ScmObj** %stackaddr$prim59992, align 8
%stackaddr$prim59993 = alloca %struct.ScmObj*, align 8
%argslist57925$k484562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48825, %struct.ScmObj* %argslist57925$k484561)
store volatile %struct.ScmObj* %argslist57925$k484562, %struct.ScmObj** %stackaddr$prim59993, align 8
%clofunc59994 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48456)
musttail call tailcc void %clofunc59994(%struct.ScmObj* %k48456, %struct.ScmObj* %argslist57925$k484562)
ret void
}

define tailcc void @proc_clo$ae48690(%struct.ScmObj* %env$ae48690,%struct.ScmObj* %current_45args57930) {
%stackaddr$prim59995 = alloca %struct.ScmObj*, align 8
%k48459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57930)
store volatile %struct.ScmObj* %k48459, %struct.ScmObj** %stackaddr$prim59995, align 8
%stackaddr$prim59996 = alloca %struct.ScmObj*, align 8
%current_45args57931 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57930)
store volatile %struct.ScmObj* %current_45args57931, %struct.ScmObj** %stackaddr$prim59996, align 8
%stackaddr$prim59997 = alloca %struct.ScmObj*, align 8
%_37map48051 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57931)
store volatile %struct.ScmObj* %_37map48051, %struct.ScmObj** %stackaddr$prim59997, align 8
%ae48692 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure59998 = alloca %struct.ScmObj*, align 8
%fptrToInt59999 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48693 to i64
%ae48693 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt59999)
store volatile %struct.ScmObj* %ae48693, %struct.ScmObj** %stackaddr$makeclosure59998, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48693, %struct.ScmObj* %_37map48051, i64 0)
%argslist57947$k484590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60000 = alloca %struct.ScmObj*, align 8
%argslist57947$k484591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48693, %struct.ScmObj* %argslist57947$k484590)
store volatile %struct.ScmObj* %argslist57947$k484591, %struct.ScmObj** %stackaddr$prim60000, align 8
%stackaddr$prim60001 = alloca %struct.ScmObj*, align 8
%argslist57947$k484592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48692, %struct.ScmObj* %argslist57947$k484591)
store volatile %struct.ScmObj* %argslist57947$k484592, %struct.ScmObj** %stackaddr$prim60001, align 8
%clofunc60002 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48459)
musttail call tailcc void %clofunc60002(%struct.ScmObj* %k48459, %struct.ScmObj* %argslist57947$k484592)
ret void
}

define tailcc void @proc_clo$ae48693(%struct.ScmObj* %env$ae48693,%struct.ScmObj* %current_45args57933) {
%stackaddr$env-ref60003 = alloca %struct.ScmObj*, align 8
%_37map48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48693, i64 0)
store %struct.ScmObj* %_37map48051, %struct.ScmObj** %stackaddr$env-ref60003
%stackaddr$prim60004 = alloca %struct.ScmObj*, align 8
%k48460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57933)
store volatile %struct.ScmObj* %k48460, %struct.ScmObj** %stackaddr$prim60004, align 8
%stackaddr$prim60005 = alloca %struct.ScmObj*, align 8
%current_45args57934 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57933)
store volatile %struct.ScmObj* %current_45args57934, %struct.ScmObj** %stackaddr$prim60005, align 8
%stackaddr$prim60006 = alloca %struct.ScmObj*, align 8
%f48053 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57934)
store volatile %struct.ScmObj* %f48053, %struct.ScmObj** %stackaddr$prim60006, align 8
%stackaddr$prim60007 = alloca %struct.ScmObj*, align 8
%current_45args57935 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57934)
store volatile %struct.ScmObj* %current_45args57935, %struct.ScmObj** %stackaddr$prim60007, align 8
%stackaddr$prim60008 = alloca %struct.ScmObj*, align 8
%lst48052 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57935)
store volatile %struct.ScmObj* %lst48052, %struct.ScmObj** %stackaddr$prim60008, align 8
%stackaddr$prim60009 = alloca %struct.ScmObj*, align 8
%anf_45bind48173 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48052)
store volatile %struct.ScmObj* %anf_45bind48173, %struct.ScmObj** %stackaddr$prim60009, align 8
%truthy$cmp60010 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48173)
%cmp$cmp60010 = icmp eq i64 %truthy$cmp60010, 1
br i1 %cmp$cmp60010, label %truebranch$cmp60010, label %falsebranch$cmp60010
truebranch$cmp60010:
%ae48697 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48698 = call %struct.ScmObj* @const_init_null()
%argslist57937$k484600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60011 = alloca %struct.ScmObj*, align 8
%argslist57937$k484601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48698, %struct.ScmObj* %argslist57937$k484600)
store volatile %struct.ScmObj* %argslist57937$k484601, %struct.ScmObj** %stackaddr$prim60011, align 8
%stackaddr$prim60012 = alloca %struct.ScmObj*, align 8
%argslist57937$k484602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48697, %struct.ScmObj* %argslist57937$k484601)
store volatile %struct.ScmObj* %argslist57937$k484602, %struct.ScmObj** %stackaddr$prim60012, align 8
%clofunc60013 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48460)
musttail call tailcc void %clofunc60013(%struct.ScmObj* %k48460, %struct.ScmObj* %argslist57937$k484602)
ret void
falsebranch$cmp60010:
%stackaddr$prim60014 = alloca %struct.ScmObj*, align 8
%anf_45bind48174 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48052)
store volatile %struct.ScmObj* %anf_45bind48174, %struct.ScmObj** %stackaddr$prim60014, align 8
%stackaddr$makeclosure60015 = alloca %struct.ScmObj*, align 8
%fptrToInt60016 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48707 to i64
%ae48707 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt60016)
store volatile %struct.ScmObj* %ae48707, %struct.ScmObj** %stackaddr$makeclosure60015, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48707, %struct.ScmObj* %f48053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48707, %struct.ScmObj* %lst48052, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48707, %struct.ScmObj* %_37map48051, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48707, %struct.ScmObj* %k48460, i64 3)
%argslist57946$f480530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60017 = alloca %struct.ScmObj*, align 8
%argslist57946$f480531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48174, %struct.ScmObj* %argslist57946$f480530)
store volatile %struct.ScmObj* %argslist57946$f480531, %struct.ScmObj** %stackaddr$prim60017, align 8
%stackaddr$prim60018 = alloca %struct.ScmObj*, align 8
%argslist57946$f480532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48707, %struct.ScmObj* %argslist57946$f480531)
store volatile %struct.ScmObj* %argslist57946$f480532, %struct.ScmObj** %stackaddr$prim60018, align 8
%clofunc60019 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48053)
musttail call tailcc void %clofunc60019(%struct.ScmObj* %f48053, %struct.ScmObj* %argslist57946$f480532)
ret void
}

define tailcc void @proc_clo$ae48707(%struct.ScmObj* %env$ae48707,%struct.ScmObj* %current_45args57938) {
%stackaddr$env-ref60020 = alloca %struct.ScmObj*, align 8
%f48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48707, i64 0)
store %struct.ScmObj* %f48053, %struct.ScmObj** %stackaddr$env-ref60020
%stackaddr$env-ref60021 = alloca %struct.ScmObj*, align 8
%lst48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48707, i64 1)
store %struct.ScmObj* %lst48052, %struct.ScmObj** %stackaddr$env-ref60021
%stackaddr$env-ref60022 = alloca %struct.ScmObj*, align 8
%_37map48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48707, i64 2)
store %struct.ScmObj* %_37map48051, %struct.ScmObj** %stackaddr$env-ref60022
%stackaddr$env-ref60023 = alloca %struct.ScmObj*, align 8
%k48460 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48707, i64 3)
store %struct.ScmObj* %k48460, %struct.ScmObj** %stackaddr$env-ref60023
%stackaddr$prim60024 = alloca %struct.ScmObj*, align 8
%_95k48461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57938)
store volatile %struct.ScmObj* %_95k48461, %struct.ScmObj** %stackaddr$prim60024, align 8
%stackaddr$prim60025 = alloca %struct.ScmObj*, align 8
%current_45args57939 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57938)
store volatile %struct.ScmObj* %current_45args57939, %struct.ScmObj** %stackaddr$prim60025, align 8
%stackaddr$prim60026 = alloca %struct.ScmObj*, align 8
%anf_45bind48175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57939)
store volatile %struct.ScmObj* %anf_45bind48175, %struct.ScmObj** %stackaddr$prim60026, align 8
%stackaddr$prim60027 = alloca %struct.ScmObj*, align 8
%anf_45bind48176 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48052)
store volatile %struct.ScmObj* %anf_45bind48176, %struct.ScmObj** %stackaddr$prim60027, align 8
%stackaddr$makeclosure60028 = alloca %struct.ScmObj*, align 8
%fptrToInt60029 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48711 to i64
%ae48711 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt60029)
store volatile %struct.ScmObj* %ae48711, %struct.ScmObj** %stackaddr$makeclosure60028, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48711, %struct.ScmObj* %anf_45bind48175, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48711, %struct.ScmObj* %k48460, i64 1)
%argslist57945$_37map480510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60030 = alloca %struct.ScmObj*, align 8
%argslist57945$_37map480511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48176, %struct.ScmObj* %argslist57945$_37map480510)
store volatile %struct.ScmObj* %argslist57945$_37map480511, %struct.ScmObj** %stackaddr$prim60030, align 8
%stackaddr$prim60031 = alloca %struct.ScmObj*, align 8
%argslist57945$_37map480512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48053, %struct.ScmObj* %argslist57945$_37map480511)
store volatile %struct.ScmObj* %argslist57945$_37map480512, %struct.ScmObj** %stackaddr$prim60031, align 8
%stackaddr$prim60032 = alloca %struct.ScmObj*, align 8
%argslist57945$_37map480513 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48711, %struct.ScmObj* %argslist57945$_37map480512)
store volatile %struct.ScmObj* %argslist57945$_37map480513, %struct.ScmObj** %stackaddr$prim60032, align 8
%clofunc60033 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map48051)
musttail call tailcc void %clofunc60033(%struct.ScmObj* %_37map48051, %struct.ScmObj* %argslist57945$_37map480513)
ret void
}

define tailcc void @proc_clo$ae48711(%struct.ScmObj* %env$ae48711,%struct.ScmObj* %current_45args57941) {
%stackaddr$env-ref60034 = alloca %struct.ScmObj*, align 8
%anf_45bind48175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48711, i64 0)
store %struct.ScmObj* %anf_45bind48175, %struct.ScmObj** %stackaddr$env-ref60034
%stackaddr$env-ref60035 = alloca %struct.ScmObj*, align 8
%k48460 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48711, i64 1)
store %struct.ScmObj* %k48460, %struct.ScmObj** %stackaddr$env-ref60035
%stackaddr$prim60036 = alloca %struct.ScmObj*, align 8
%_95k48462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57941)
store volatile %struct.ScmObj* %_95k48462, %struct.ScmObj** %stackaddr$prim60036, align 8
%stackaddr$prim60037 = alloca %struct.ScmObj*, align 8
%current_45args57942 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57941)
store volatile %struct.ScmObj* %current_45args57942, %struct.ScmObj** %stackaddr$prim60037, align 8
%stackaddr$prim60038 = alloca %struct.ScmObj*, align 8
%anf_45bind48177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57942)
store volatile %struct.ScmObj* %anf_45bind48177, %struct.ScmObj** %stackaddr$prim60038, align 8
%stackaddr$prim60039 = alloca %struct.ScmObj*, align 8
%cpsprim48463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48175, %struct.ScmObj* %anf_45bind48177)
store volatile %struct.ScmObj* %cpsprim48463, %struct.ScmObj** %stackaddr$prim60039, align 8
%ae48717 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57944$k484600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60040 = alloca %struct.ScmObj*, align 8
%argslist57944$k484601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48463, %struct.ScmObj* %argslist57944$k484600)
store volatile %struct.ScmObj* %argslist57944$k484601, %struct.ScmObj** %stackaddr$prim60040, align 8
%stackaddr$prim60041 = alloca %struct.ScmObj*, align 8
%argslist57944$k484602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48717, %struct.ScmObj* %argslist57944$k484601)
store volatile %struct.ScmObj* %argslist57944$k484602, %struct.ScmObj** %stackaddr$prim60041, align 8
%clofunc60042 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48460)
musttail call tailcc void %clofunc60042(%struct.ScmObj* %k48460, %struct.ScmObj* %argslist57944$k484602)
ret void
}

define tailcc void @proc_clo$ae48610(%struct.ScmObj* %env$ae48610,%struct.ScmObj* %current_45args57950) {
%stackaddr$prim60043 = alloca %struct.ScmObj*, align 8
%k48464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57950)
store volatile %struct.ScmObj* %k48464, %struct.ScmObj** %stackaddr$prim60043, align 8
%stackaddr$prim60044 = alloca %struct.ScmObj*, align 8
%current_45args57951 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57950)
store volatile %struct.ScmObj* %current_45args57951, %struct.ScmObj** %stackaddr$prim60044, align 8
%stackaddr$prim60045 = alloca %struct.ScmObj*, align 8
%_37foldr148055 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57951)
store volatile %struct.ScmObj* %_37foldr148055, %struct.ScmObj** %stackaddr$prim60045, align 8
%ae48612 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60046 = alloca %struct.ScmObj*, align 8
%fptrToInt60047 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48613 to i64
%ae48613 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt60047)
store volatile %struct.ScmObj* %ae48613, %struct.ScmObj** %stackaddr$makeclosure60046, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48613, %struct.ScmObj* %_37foldr148055, i64 0)
%argslist57964$k484640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60048 = alloca %struct.ScmObj*, align 8
%argslist57964$k484641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48613, %struct.ScmObj* %argslist57964$k484640)
store volatile %struct.ScmObj* %argslist57964$k484641, %struct.ScmObj** %stackaddr$prim60048, align 8
%stackaddr$prim60049 = alloca %struct.ScmObj*, align 8
%argslist57964$k484642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48612, %struct.ScmObj* %argslist57964$k484641)
store volatile %struct.ScmObj* %argslist57964$k484642, %struct.ScmObj** %stackaddr$prim60049, align 8
%clofunc60050 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48464)
musttail call tailcc void %clofunc60050(%struct.ScmObj* %k48464, %struct.ScmObj* %argslist57964$k484642)
ret void
}

define tailcc void @proc_clo$ae48613(%struct.ScmObj* %env$ae48613,%struct.ScmObj* %current_45args57953) {
%stackaddr$env-ref60051 = alloca %struct.ScmObj*, align 8
%_37foldr148055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48613, i64 0)
store %struct.ScmObj* %_37foldr148055, %struct.ScmObj** %stackaddr$env-ref60051
%stackaddr$prim60052 = alloca %struct.ScmObj*, align 8
%k48465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57953)
store volatile %struct.ScmObj* %k48465, %struct.ScmObj** %stackaddr$prim60052, align 8
%stackaddr$prim60053 = alloca %struct.ScmObj*, align 8
%current_45args57954 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57953)
store volatile %struct.ScmObj* %current_45args57954, %struct.ScmObj** %stackaddr$prim60053, align 8
%stackaddr$prim60054 = alloca %struct.ScmObj*, align 8
%f48058 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57954)
store volatile %struct.ScmObj* %f48058, %struct.ScmObj** %stackaddr$prim60054, align 8
%stackaddr$prim60055 = alloca %struct.ScmObj*, align 8
%current_45args57955 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57954)
store volatile %struct.ScmObj* %current_45args57955, %struct.ScmObj** %stackaddr$prim60055, align 8
%stackaddr$prim60056 = alloca %struct.ScmObj*, align 8
%acc48057 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57955)
store volatile %struct.ScmObj* %acc48057, %struct.ScmObj** %stackaddr$prim60056, align 8
%stackaddr$prim60057 = alloca %struct.ScmObj*, align 8
%current_45args57956 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57955)
store volatile %struct.ScmObj* %current_45args57956, %struct.ScmObj** %stackaddr$prim60057, align 8
%stackaddr$prim60058 = alloca %struct.ScmObj*, align 8
%lst48056 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57956)
store volatile %struct.ScmObj* %lst48056, %struct.ScmObj** %stackaddr$prim60058, align 8
%stackaddr$prim60059 = alloca %struct.ScmObj*, align 8
%anf_45bind48168 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48056)
store volatile %struct.ScmObj* %anf_45bind48168, %struct.ScmObj** %stackaddr$prim60059, align 8
%truthy$cmp60060 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48168)
%cmp$cmp60060 = icmp eq i64 %truthy$cmp60060, 1
br i1 %cmp$cmp60060, label %truebranch$cmp60060, label %falsebranch$cmp60060
truebranch$cmp60060:
%ae48617 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist57958$k484650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60061 = alloca %struct.ScmObj*, align 8
%argslist57958$k484651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48057, %struct.ScmObj* %argslist57958$k484650)
store volatile %struct.ScmObj* %argslist57958$k484651, %struct.ScmObj** %stackaddr$prim60061, align 8
%stackaddr$prim60062 = alloca %struct.ScmObj*, align 8
%argslist57958$k484652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48617, %struct.ScmObj* %argslist57958$k484651)
store volatile %struct.ScmObj* %argslist57958$k484652, %struct.ScmObj** %stackaddr$prim60062, align 8
%clofunc60063 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48465)
musttail call tailcc void %clofunc60063(%struct.ScmObj* %k48465, %struct.ScmObj* %argslist57958$k484652)
ret void
falsebranch$cmp60060:
%stackaddr$prim60064 = alloca %struct.ScmObj*, align 8
%anf_45bind48169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48056)
store volatile %struct.ScmObj* %anf_45bind48169, %struct.ScmObj** %stackaddr$prim60064, align 8
%stackaddr$prim60065 = alloca %struct.ScmObj*, align 8
%anf_45bind48170 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48056)
store volatile %struct.ScmObj* %anf_45bind48170, %struct.ScmObj** %stackaddr$prim60065, align 8
%stackaddr$makeclosure60066 = alloca %struct.ScmObj*, align 8
%fptrToInt60067 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48625 to i64
%ae48625 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt60067)
store volatile %struct.ScmObj* %ae48625, %struct.ScmObj** %stackaddr$makeclosure60066, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48625, %struct.ScmObj* %anf_45bind48169, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48625, %struct.ScmObj* %k48465, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48625, %struct.ScmObj* %f48058, i64 2)
%argslist57963$_37foldr1480550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60068 = alloca %struct.ScmObj*, align 8
%argslist57963$_37foldr1480551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48170, %struct.ScmObj* %argslist57963$_37foldr1480550)
store volatile %struct.ScmObj* %argslist57963$_37foldr1480551, %struct.ScmObj** %stackaddr$prim60068, align 8
%stackaddr$prim60069 = alloca %struct.ScmObj*, align 8
%argslist57963$_37foldr1480552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48057, %struct.ScmObj* %argslist57963$_37foldr1480551)
store volatile %struct.ScmObj* %argslist57963$_37foldr1480552, %struct.ScmObj** %stackaddr$prim60069, align 8
%stackaddr$prim60070 = alloca %struct.ScmObj*, align 8
%argslist57963$_37foldr1480553 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48058, %struct.ScmObj* %argslist57963$_37foldr1480552)
store volatile %struct.ScmObj* %argslist57963$_37foldr1480553, %struct.ScmObj** %stackaddr$prim60070, align 8
%stackaddr$prim60071 = alloca %struct.ScmObj*, align 8
%argslist57963$_37foldr1480554 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48625, %struct.ScmObj* %argslist57963$_37foldr1480553)
store volatile %struct.ScmObj* %argslist57963$_37foldr1480554, %struct.ScmObj** %stackaddr$prim60071, align 8
%clofunc60072 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148055)
musttail call tailcc void %clofunc60072(%struct.ScmObj* %_37foldr148055, %struct.ScmObj* %argslist57963$_37foldr1480554)
ret void
}

define tailcc void @proc_clo$ae48625(%struct.ScmObj* %env$ae48625,%struct.ScmObj* %current_45args57959) {
%stackaddr$env-ref60073 = alloca %struct.ScmObj*, align 8
%anf_45bind48169 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48625, i64 0)
store %struct.ScmObj* %anf_45bind48169, %struct.ScmObj** %stackaddr$env-ref60073
%stackaddr$env-ref60074 = alloca %struct.ScmObj*, align 8
%k48465 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48625, i64 1)
store %struct.ScmObj* %k48465, %struct.ScmObj** %stackaddr$env-ref60074
%stackaddr$env-ref60075 = alloca %struct.ScmObj*, align 8
%f48058 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48625, i64 2)
store %struct.ScmObj* %f48058, %struct.ScmObj** %stackaddr$env-ref60075
%stackaddr$prim60076 = alloca %struct.ScmObj*, align 8
%_95k48466 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57959)
store volatile %struct.ScmObj* %_95k48466, %struct.ScmObj** %stackaddr$prim60076, align 8
%stackaddr$prim60077 = alloca %struct.ScmObj*, align 8
%current_45args57960 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57959)
store volatile %struct.ScmObj* %current_45args57960, %struct.ScmObj** %stackaddr$prim60077, align 8
%stackaddr$prim60078 = alloca %struct.ScmObj*, align 8
%anf_45bind48171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57960)
store volatile %struct.ScmObj* %anf_45bind48171, %struct.ScmObj** %stackaddr$prim60078, align 8
%argslist57962$f480580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60079 = alloca %struct.ScmObj*, align 8
%argslist57962$f480581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48171, %struct.ScmObj* %argslist57962$f480580)
store volatile %struct.ScmObj* %argslist57962$f480581, %struct.ScmObj** %stackaddr$prim60079, align 8
%stackaddr$prim60080 = alloca %struct.ScmObj*, align 8
%argslist57962$f480582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48169, %struct.ScmObj* %argslist57962$f480581)
store volatile %struct.ScmObj* %argslist57962$f480582, %struct.ScmObj** %stackaddr$prim60080, align 8
%stackaddr$prim60081 = alloca %struct.ScmObj*, align 8
%argslist57962$f480583 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48465, %struct.ScmObj* %argslist57962$f480582)
store volatile %struct.ScmObj* %argslist57962$f480583, %struct.ScmObj** %stackaddr$prim60081, align 8
%clofunc60082 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48058)
musttail call tailcc void %clofunc60082(%struct.ScmObj* %f48058, %struct.ScmObj* %argslist57962$f480583)
ret void
}

define tailcc void @proc_clo$ae48493(%struct.ScmObj* %env$ae48493,%struct.ScmObj* %current_45args57967) {
%stackaddr$prim60083 = alloca %struct.ScmObj*, align 8
%k48467 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57967)
store volatile %struct.ScmObj* %k48467, %struct.ScmObj** %stackaddr$prim60083, align 8
%stackaddr$prim60084 = alloca %struct.ScmObj*, align 8
%current_45args57968 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57967)
store volatile %struct.ScmObj* %current_45args57968, %struct.ScmObj** %stackaddr$prim60084, align 8
%stackaddr$prim60085 = alloca %struct.ScmObj*, align 8
%y48035 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57968)
store volatile %struct.ScmObj* %y48035, %struct.ScmObj** %stackaddr$prim60085, align 8
%ae48495 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60086 = alloca %struct.ScmObj*, align 8
%fptrToInt60087 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48496 to i64
%ae48496 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt60087)
store volatile %struct.ScmObj* %ae48496, %struct.ScmObj** %stackaddr$makeclosure60086, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48496, %struct.ScmObj* %y48035, i64 0)
%argslist57986$k484670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60088 = alloca %struct.ScmObj*, align 8
%argslist57986$k484671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48496, %struct.ScmObj* %argslist57986$k484670)
store volatile %struct.ScmObj* %argslist57986$k484671, %struct.ScmObj** %stackaddr$prim60088, align 8
%stackaddr$prim60089 = alloca %struct.ScmObj*, align 8
%argslist57986$k484672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48495, %struct.ScmObj* %argslist57986$k484671)
store volatile %struct.ScmObj* %argslist57986$k484672, %struct.ScmObj** %stackaddr$prim60089, align 8
%clofunc60090 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48467)
musttail call tailcc void %clofunc60090(%struct.ScmObj* %k48467, %struct.ScmObj* %argslist57986$k484672)
ret void
}

define tailcc void @proc_clo$ae48496(%struct.ScmObj* %env$ae48496,%struct.ScmObj* %current_45args57970) {
%stackaddr$env-ref60091 = alloca %struct.ScmObj*, align 8
%y48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48496, i64 0)
store %struct.ScmObj* %y48035, %struct.ScmObj** %stackaddr$env-ref60091
%stackaddr$prim60092 = alloca %struct.ScmObj*, align 8
%k48468 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57970)
store volatile %struct.ScmObj* %k48468, %struct.ScmObj** %stackaddr$prim60092, align 8
%stackaddr$prim60093 = alloca %struct.ScmObj*, align 8
%current_45args57971 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57970)
store volatile %struct.ScmObj* %current_45args57971, %struct.ScmObj** %stackaddr$prim60093, align 8
%stackaddr$prim60094 = alloca %struct.ScmObj*, align 8
%f48036 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57971)
store volatile %struct.ScmObj* %f48036, %struct.ScmObj** %stackaddr$prim60094, align 8
%stackaddr$makeclosure60095 = alloca %struct.ScmObj*, align 8
%fptrToInt60096 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48497 to i64
%ae48497 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt60096)
store volatile %struct.ScmObj* %ae48497, %struct.ScmObj** %stackaddr$makeclosure60095, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48497, %struct.ScmObj* %f48036, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48497, %struct.ScmObj* %k48468, i64 1)
%ae48498 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure60097 = alloca %struct.ScmObj*, align 8
%fptrToInt60098 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48499 to i64
%ae48499 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt60098)
store volatile %struct.ScmObj* %ae48499, %struct.ScmObj** %stackaddr$makeclosure60097, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48499, %struct.ScmObj* %f48036, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48499, %struct.ScmObj* %y48035, i64 1)
%argslist57985$ae484970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60099 = alloca %struct.ScmObj*, align 8
%argslist57985$ae484971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48499, %struct.ScmObj* %argslist57985$ae484970)
store volatile %struct.ScmObj* %argslist57985$ae484971, %struct.ScmObj** %stackaddr$prim60099, align 8
%stackaddr$prim60100 = alloca %struct.ScmObj*, align 8
%argslist57985$ae484972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48498, %struct.ScmObj* %argslist57985$ae484971)
store volatile %struct.ScmObj* %argslist57985$ae484972, %struct.ScmObj** %stackaddr$prim60100, align 8
%clofunc60101 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48497)
musttail call tailcc void %clofunc60101(%struct.ScmObj* %ae48497, %struct.ScmObj* %argslist57985$ae484972)
ret void
}

define tailcc void @proc_clo$ae48497(%struct.ScmObj* %env$ae48497,%struct.ScmObj* %current_45args57973) {
%stackaddr$env-ref60102 = alloca %struct.ScmObj*, align 8
%f48036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48497, i64 0)
store %struct.ScmObj* %f48036, %struct.ScmObj** %stackaddr$env-ref60102
%stackaddr$env-ref60103 = alloca %struct.ScmObj*, align 8
%k48468 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48497, i64 1)
store %struct.ScmObj* %k48468, %struct.ScmObj** %stackaddr$env-ref60103
%stackaddr$prim60104 = alloca %struct.ScmObj*, align 8
%_95k48469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57973)
store volatile %struct.ScmObj* %_95k48469, %struct.ScmObj** %stackaddr$prim60104, align 8
%stackaddr$prim60105 = alloca %struct.ScmObj*, align 8
%current_45args57974 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57973)
store volatile %struct.ScmObj* %current_45args57974, %struct.ScmObj** %stackaddr$prim60105, align 8
%stackaddr$prim60106 = alloca %struct.ScmObj*, align 8
%anf_45bind48166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57974)
store volatile %struct.ScmObj* %anf_45bind48166, %struct.ScmObj** %stackaddr$prim60106, align 8
%argslist57976$f480360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60107 = alloca %struct.ScmObj*, align 8
%argslist57976$f480361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48166, %struct.ScmObj* %argslist57976$f480360)
store volatile %struct.ScmObj* %argslist57976$f480361, %struct.ScmObj** %stackaddr$prim60107, align 8
%stackaddr$prim60108 = alloca %struct.ScmObj*, align 8
%argslist57976$f480362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48468, %struct.ScmObj* %argslist57976$f480361)
store volatile %struct.ScmObj* %argslist57976$f480362, %struct.ScmObj** %stackaddr$prim60108, align 8
%clofunc60109 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48036)
musttail call tailcc void %clofunc60109(%struct.ScmObj* %f48036, %struct.ScmObj* %argslist57976$f480362)
ret void
}

define tailcc void @proc_clo$ae48499(%struct.ScmObj* %env$ae48499,%struct.ScmObj* %args4803748470) {
%stackaddr$env-ref60110 = alloca %struct.ScmObj*, align 8
%f48036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48499, i64 0)
store %struct.ScmObj* %f48036, %struct.ScmObj** %stackaddr$env-ref60110
%stackaddr$env-ref60111 = alloca %struct.ScmObj*, align 8
%y48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48499, i64 1)
store %struct.ScmObj* %y48035, %struct.ScmObj** %stackaddr$env-ref60111
%stackaddr$prim60112 = alloca %struct.ScmObj*, align 8
%k48471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4803748470)
store volatile %struct.ScmObj* %k48471, %struct.ScmObj** %stackaddr$prim60112, align 8
%stackaddr$prim60113 = alloca %struct.ScmObj*, align 8
%args48037 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4803748470)
store volatile %struct.ScmObj* %args48037, %struct.ScmObj** %stackaddr$prim60113, align 8
%stackaddr$makeclosure60114 = alloca %struct.ScmObj*, align 8
%fptrToInt60115 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48503 to i64
%ae48503 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt60115)
store volatile %struct.ScmObj* %ae48503, %struct.ScmObj** %stackaddr$makeclosure60114, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48503, %struct.ScmObj* %k48471, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48503, %struct.ScmObj* %args48037, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48503, %struct.ScmObj* %f48036, i64 2)
%argslist57984$y480350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60116 = alloca %struct.ScmObj*, align 8
%argslist57984$y480351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y48035, %struct.ScmObj* %argslist57984$y480350)
store volatile %struct.ScmObj* %argslist57984$y480351, %struct.ScmObj** %stackaddr$prim60116, align 8
%stackaddr$prim60117 = alloca %struct.ScmObj*, align 8
%argslist57984$y480352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48503, %struct.ScmObj* %argslist57984$y480351)
store volatile %struct.ScmObj* %argslist57984$y480352, %struct.ScmObj** %stackaddr$prim60117, align 8
%clofunc60118 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y48035)
musttail call tailcc void %clofunc60118(%struct.ScmObj* %y48035, %struct.ScmObj* %argslist57984$y480352)
ret void
}

define tailcc void @proc_clo$ae48503(%struct.ScmObj* %env$ae48503,%struct.ScmObj* %current_45args57977) {
%stackaddr$env-ref60119 = alloca %struct.ScmObj*, align 8
%k48471 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48503, i64 0)
store %struct.ScmObj* %k48471, %struct.ScmObj** %stackaddr$env-ref60119
%stackaddr$env-ref60120 = alloca %struct.ScmObj*, align 8
%args48037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48503, i64 1)
store %struct.ScmObj* %args48037, %struct.ScmObj** %stackaddr$env-ref60120
%stackaddr$env-ref60121 = alloca %struct.ScmObj*, align 8
%f48036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48503, i64 2)
store %struct.ScmObj* %f48036, %struct.ScmObj** %stackaddr$env-ref60121
%stackaddr$prim60122 = alloca %struct.ScmObj*, align 8
%_95k48472 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57977)
store volatile %struct.ScmObj* %_95k48472, %struct.ScmObj** %stackaddr$prim60122, align 8
%stackaddr$prim60123 = alloca %struct.ScmObj*, align 8
%current_45args57978 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57977)
store volatile %struct.ScmObj* %current_45args57978, %struct.ScmObj** %stackaddr$prim60123, align 8
%stackaddr$prim60124 = alloca %struct.ScmObj*, align 8
%anf_45bind48164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57978)
store volatile %struct.ScmObj* %anf_45bind48164, %struct.ScmObj** %stackaddr$prim60124, align 8
%stackaddr$makeclosure60125 = alloca %struct.ScmObj*, align 8
%fptrToInt60126 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48506 to i64
%ae48506 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt60126)
store volatile %struct.ScmObj* %ae48506, %struct.ScmObj** %stackaddr$makeclosure60125, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48506, %struct.ScmObj* %k48471, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48506, %struct.ScmObj* %args48037, i64 1)
%argslist57983$anf_45bind481640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60127 = alloca %struct.ScmObj*, align 8
%argslist57983$anf_45bind481641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48036, %struct.ScmObj* %argslist57983$anf_45bind481640)
store volatile %struct.ScmObj* %argslist57983$anf_45bind481641, %struct.ScmObj** %stackaddr$prim60127, align 8
%stackaddr$prim60128 = alloca %struct.ScmObj*, align 8
%argslist57983$anf_45bind481642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48506, %struct.ScmObj* %argslist57983$anf_45bind481641)
store volatile %struct.ScmObj* %argslist57983$anf_45bind481642, %struct.ScmObj** %stackaddr$prim60128, align 8
%clofunc60129 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48164)
musttail call tailcc void %clofunc60129(%struct.ScmObj* %anf_45bind48164, %struct.ScmObj* %argslist57983$anf_45bind481642)
ret void
}

define tailcc void @proc_clo$ae48506(%struct.ScmObj* %env$ae48506,%struct.ScmObj* %current_45args57980) {
%stackaddr$env-ref60130 = alloca %struct.ScmObj*, align 8
%k48471 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48506, i64 0)
store %struct.ScmObj* %k48471, %struct.ScmObj** %stackaddr$env-ref60130
%stackaddr$env-ref60131 = alloca %struct.ScmObj*, align 8
%args48037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48506, i64 1)
store %struct.ScmObj* %args48037, %struct.ScmObj** %stackaddr$env-ref60131
%stackaddr$prim60132 = alloca %struct.ScmObj*, align 8
%_95k48473 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57980)
store volatile %struct.ScmObj* %_95k48473, %struct.ScmObj** %stackaddr$prim60132, align 8
%stackaddr$prim60133 = alloca %struct.ScmObj*, align 8
%current_45args57981 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57980)
store volatile %struct.ScmObj* %current_45args57981, %struct.ScmObj** %stackaddr$prim60133, align 8
%stackaddr$prim60134 = alloca %struct.ScmObj*, align 8
%anf_45bind48165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57981)
store volatile %struct.ScmObj* %anf_45bind48165, %struct.ScmObj** %stackaddr$prim60134, align 8
%stackaddr$prim60135 = alloca %struct.ScmObj*, align 8
%cpsargs48474 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48471, %struct.ScmObj* %args48037)
store volatile %struct.ScmObj* %cpsargs48474, %struct.ScmObj** %stackaddr$prim60135, align 8
%clofunc60136 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48165)
musttail call tailcc void %clofunc60136(%struct.ScmObj* %anf_45bind48165, %struct.ScmObj* %cpsargs48474)
ret void
}

define tailcc void @proc_clo$ae48478(%struct.ScmObj* %env$ae48478,%struct.ScmObj* %current_45args57988) {
%stackaddr$prim60137 = alloca %struct.ScmObj*, align 8
%k48475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57988)
store volatile %struct.ScmObj* %k48475, %struct.ScmObj** %stackaddr$prim60137, align 8
%stackaddr$prim60138 = alloca %struct.ScmObj*, align 8
%current_45args57989 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args57988)
store volatile %struct.ScmObj* %current_45args57989, %struct.ScmObj** %stackaddr$prim60138, align 8
%stackaddr$prim60139 = alloca %struct.ScmObj*, align 8
%yu48034 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args57989)
store volatile %struct.ScmObj* %yu48034, %struct.ScmObj** %stackaddr$prim60139, align 8
%argslist57991$yu480340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim60140 = alloca %struct.ScmObj*, align 8
%argslist57991$yu480341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu48034, %struct.ScmObj* %argslist57991$yu480340)
store volatile %struct.ScmObj* %argslist57991$yu480341, %struct.ScmObj** %stackaddr$prim60140, align 8
%stackaddr$prim60141 = alloca %struct.ScmObj*, align 8
%argslist57991$yu480342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48475, %struct.ScmObj* %argslist57991$yu480341)
store volatile %struct.ScmObj* %argslist57991$yu480342, %struct.ScmObj** %stackaddr$prim60141, align 8
%clofunc60142 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu48034)
musttail call tailcc void %clofunc60142(%struct.ScmObj* %yu48034, %struct.ScmObj* %argslist57991$yu480342)
ret void
}