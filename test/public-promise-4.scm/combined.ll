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

@global$sym$ae4430848274 = private unnamed_addr constant [4 x i8] c"yes\00", align 8
@global$sym$ae4395948398 = private unnamed_addr constant [8 x i8] c"promise\00", align 8

define ccc i32 @main() {
%mainenv47606 = call %struct.ScmObj* @const_init_null()
%mainargs47607 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv47606, %struct.ScmObj* %mainargs47607)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv47604,%struct.ScmObj* %mainargs47605) {
%stackaddr$makeclosure47608 = alloca %struct.ScmObj*, align 8
%fptrToInt47609 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40571 to i64
%ae40571 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47609)
store volatile %struct.ScmObj* %ae40571, %struct.ScmObj** %stackaddr$makeclosure47608, align 8
%ae40572 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47610 = alloca %struct.ScmObj*, align 8
%fptrToInt47611 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40573 to i64
%ae40573 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47611)
store volatile %struct.ScmObj* %ae40573, %struct.ScmObj** %stackaddr$makeclosure47610, align 8
%args47603$ae40571$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47612 = alloca %struct.ScmObj*, align 8
%args47603$ae40571$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40573, %struct.ScmObj* %args47603$ae40571$0)
store volatile %struct.ScmObj* %args47603$ae40571$1, %struct.ScmObj** %stackaddr$prim47612, align 8
%stackaddr$prim47613 = alloca %struct.ScmObj*, align 8
%args47603$ae40571$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40572, %struct.ScmObj* %args47603$ae40571$1)
store volatile %struct.ScmObj* %args47603$ae40571$2, %struct.ScmObj** %stackaddr$prim47613, align 8
%clofunc47614 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40571)
musttail call tailcc void %clofunc47614(%struct.ScmObj* %ae40571, %struct.ScmObj* %args47603$ae40571$2)
ret void
}

define tailcc void @proc_clo$ae40571(%struct.ScmObj* %env$ae40571,%struct.ScmObj* %current_45args46977) {
%stackaddr$prim47615 = alloca %struct.ScmObj*, align 8
%_95k40366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46977)
store volatile %struct.ScmObj* %_95k40366, %struct.ScmObj** %stackaddr$prim47615, align 8
%stackaddr$prim47616 = alloca %struct.ScmObj*, align 8
%current_45args46978 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46977)
store volatile %struct.ScmObj* %current_45args46978, %struct.ScmObj** %stackaddr$prim47616, align 8
%stackaddr$prim47617 = alloca %struct.ScmObj*, align 8
%anf_45bind40232 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46978)
store volatile %struct.ScmObj* %anf_45bind40232, %struct.ScmObj** %stackaddr$prim47617, align 8
%stackaddr$makeclosure47618 = alloca %struct.ScmObj*, align 8
%fptrToInt47619 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40586 to i64
%ae40586 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47619)
store volatile %struct.ScmObj* %ae40586, %struct.ScmObj** %stackaddr$makeclosure47618, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40586, %struct.ScmObj* %anf_45bind40232, i64 0)
%ae40587 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47620 = alloca %struct.ScmObj*, align 8
%fptrToInt47621 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40588 to i64
%ae40588 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47621)
store volatile %struct.ScmObj* %ae40588, %struct.ScmObj** %stackaddr$makeclosure47620, align 8
%args47598$ae40586$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47622 = alloca %struct.ScmObj*, align 8
%args47598$ae40586$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40588, %struct.ScmObj* %args47598$ae40586$0)
store volatile %struct.ScmObj* %args47598$ae40586$1, %struct.ScmObj** %stackaddr$prim47622, align 8
%stackaddr$prim47623 = alloca %struct.ScmObj*, align 8
%args47598$ae40586$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40587, %struct.ScmObj* %args47598$ae40586$1)
store volatile %struct.ScmObj* %args47598$ae40586$2, %struct.ScmObj** %stackaddr$prim47623, align 8
%clofunc47624 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40586)
musttail call tailcc void %clofunc47624(%struct.ScmObj* %ae40586, %struct.ScmObj* %args47598$ae40586$2)
ret void
}

define tailcc void @proc_clo$ae40586(%struct.ScmObj* %env$ae40586,%struct.ScmObj* %current_45args46980) {
%stackaddr$env-ref47625 = alloca %struct.ScmObj*, align 8
%anf_45bind40232 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40586, i64 0)
store %struct.ScmObj* %anf_45bind40232, %struct.ScmObj** %stackaddr$env-ref47625
%stackaddr$prim47626 = alloca %struct.ScmObj*, align 8
%_95k40367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46980)
store volatile %struct.ScmObj* %_95k40367, %struct.ScmObj** %stackaddr$prim47626, align 8
%stackaddr$prim47627 = alloca %struct.ScmObj*, align 8
%current_45args46981 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46980)
store volatile %struct.ScmObj* %current_45args46981, %struct.ScmObj** %stackaddr$prim47627, align 8
%stackaddr$prim47628 = alloca %struct.ScmObj*, align 8
%anf_45bind40236 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46981)
store volatile %struct.ScmObj* %anf_45bind40236, %struct.ScmObj** %stackaddr$prim47628, align 8
%stackaddr$makeclosure47629 = alloca %struct.ScmObj*, align 8
%fptrToInt47630 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40701 to i64
%ae40701 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47630)
store volatile %struct.ScmObj* %ae40701, %struct.ScmObj** %stackaddr$makeclosure47629, align 8
%args47577$anf_45bind40232$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47631 = alloca %struct.ScmObj*, align 8
%args47577$anf_45bind40232$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40236, %struct.ScmObj* %args47577$anf_45bind40232$0)
store volatile %struct.ScmObj* %args47577$anf_45bind40232$1, %struct.ScmObj** %stackaddr$prim47631, align 8
%stackaddr$prim47632 = alloca %struct.ScmObj*, align 8
%args47577$anf_45bind40232$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40701, %struct.ScmObj* %args47577$anf_45bind40232$1)
store volatile %struct.ScmObj* %args47577$anf_45bind40232$2, %struct.ScmObj** %stackaddr$prim47632, align 8
%clofunc47633 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40232)
musttail call tailcc void %clofunc47633(%struct.ScmObj* %anf_45bind40232, %struct.ScmObj* %args47577$anf_45bind40232$2)
ret void
}

define tailcc void @proc_clo$ae40701(%struct.ScmObj* %env$ae40701,%struct.ScmObj* %current_45args46983) {
%stackaddr$prim47634 = alloca %struct.ScmObj*, align 8
%_95k40368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46983)
store volatile %struct.ScmObj* %_95k40368, %struct.ScmObj** %stackaddr$prim47634, align 8
%stackaddr$prim47635 = alloca %struct.ScmObj*, align 8
%current_45args46984 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46983)
store volatile %struct.ScmObj* %current_45args46984, %struct.ScmObj** %stackaddr$prim47635, align 8
%stackaddr$prim47636 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46984)
store volatile %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$prim47636, align 8
%stackaddr$makeclosure47637 = alloca %struct.ScmObj*, align 8
%fptrToInt47638 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40703 to i64
%ae40703 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47638)
store volatile %struct.ScmObj* %ae40703, %struct.ScmObj** %stackaddr$makeclosure47637, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40703, %struct.ScmObj* %Ycmb40102, i64 0)
%ae40704 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47639 = alloca %struct.ScmObj*, align 8
%fptrToInt47640 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40705 to i64
%ae40705 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47640)
store volatile %struct.ScmObj* %ae40705, %struct.ScmObj** %stackaddr$makeclosure47639, align 8
%args47576$ae40703$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47641 = alloca %struct.ScmObj*, align 8
%args47576$ae40703$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40705, %struct.ScmObj* %args47576$ae40703$0)
store volatile %struct.ScmObj* %args47576$ae40703$1, %struct.ScmObj** %stackaddr$prim47641, align 8
%stackaddr$prim47642 = alloca %struct.ScmObj*, align 8
%args47576$ae40703$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40704, %struct.ScmObj* %args47576$ae40703$1)
store volatile %struct.ScmObj* %args47576$ae40703$2, %struct.ScmObj** %stackaddr$prim47642, align 8
%clofunc47643 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40703)
musttail call tailcc void %clofunc47643(%struct.ScmObj* %ae40703, %struct.ScmObj* %args47576$ae40703$2)
ret void
}

define tailcc void @proc_clo$ae40703(%struct.ScmObj* %env$ae40703,%struct.ScmObj* %current_45args46986) {
%stackaddr$env-ref47644 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40703, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47644
%stackaddr$prim47645 = alloca %struct.ScmObj*, align 8
%_95k40369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46986)
store volatile %struct.ScmObj* %_95k40369, %struct.ScmObj** %stackaddr$prim47645, align 8
%stackaddr$prim47646 = alloca %struct.ScmObj*, align 8
%current_45args46987 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46986)
store volatile %struct.ScmObj* %current_45args46987, %struct.ScmObj** %stackaddr$prim47646, align 8
%stackaddr$prim47647 = alloca %struct.ScmObj*, align 8
%anf_45bind40241 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46987)
store volatile %struct.ScmObj* %anf_45bind40241, %struct.ScmObj** %stackaddr$prim47647, align 8
%stackaddr$makeclosure47648 = alloca %struct.ScmObj*, align 8
%fptrToInt47649 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40781 to i64
%ae40781 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47649)
store volatile %struct.ScmObj* %ae40781, %struct.ScmObj** %stackaddr$makeclosure47648, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40781, %struct.ScmObj* %Ycmb40102, i64 0)
%args47560$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47650 = alloca %struct.ScmObj*, align 8
%args47560$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40241, %struct.ScmObj* %args47560$Ycmb40102$0)
store volatile %struct.ScmObj* %args47560$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47650, align 8
%stackaddr$prim47651 = alloca %struct.ScmObj*, align 8
%args47560$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40781, %struct.ScmObj* %args47560$Ycmb40102$1)
store volatile %struct.ScmObj* %args47560$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47651, align 8
%clofunc47652 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47652(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args47560$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae40781(%struct.ScmObj* %env$ae40781,%struct.ScmObj* %current_45args46989) {
%stackaddr$env-ref47653 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40781, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47653
%stackaddr$prim47654 = alloca %struct.ScmObj*, align 8
%_95k40370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46989)
store volatile %struct.ScmObj* %_95k40370, %struct.ScmObj** %stackaddr$prim47654, align 8
%stackaddr$prim47655 = alloca %struct.ScmObj*, align 8
%current_45args46990 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46989)
store volatile %struct.ScmObj* %current_45args46990, %struct.ScmObj** %stackaddr$prim47655, align 8
%stackaddr$prim47656 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46990)
store volatile %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$prim47656, align 8
%stackaddr$makeclosure47657 = alloca %struct.ScmObj*, align 8
%fptrToInt47658 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40783 to i64
%ae40783 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47658)
store volatile %struct.ScmObj* %ae40783, %struct.ScmObj** %stackaddr$makeclosure47657, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40783, %struct.ScmObj* %Ycmb40102, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40783, %struct.ScmObj* %_37foldr140123, i64 1)
%ae40784 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47659 = alloca %struct.ScmObj*, align 8
%fptrToInt47660 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40785 to i64
%ae40785 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47660)
store volatile %struct.ScmObj* %ae40785, %struct.ScmObj** %stackaddr$makeclosure47659, align 8
%args47559$ae40783$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47661 = alloca %struct.ScmObj*, align 8
%args47559$ae40783$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40785, %struct.ScmObj* %args47559$ae40783$0)
store volatile %struct.ScmObj* %args47559$ae40783$1, %struct.ScmObj** %stackaddr$prim47661, align 8
%stackaddr$prim47662 = alloca %struct.ScmObj*, align 8
%args47559$ae40783$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40784, %struct.ScmObj* %args47559$ae40783$1)
store volatile %struct.ScmObj* %args47559$ae40783$2, %struct.ScmObj** %stackaddr$prim47662, align 8
%clofunc47663 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40783)
musttail call tailcc void %clofunc47663(%struct.ScmObj* %ae40783, %struct.ScmObj* %args47559$ae40783$2)
ret void
}

define tailcc void @proc_clo$ae40783(%struct.ScmObj* %env$ae40783,%struct.ScmObj* %current_45args46992) {
%stackaddr$env-ref47664 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40783, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47664
%stackaddr$env-ref47665 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40783, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47665
%stackaddr$prim47666 = alloca %struct.ScmObj*, align 8
%_95k40371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46992)
store volatile %struct.ScmObj* %_95k40371, %struct.ScmObj** %stackaddr$prim47666, align 8
%stackaddr$prim47667 = alloca %struct.ScmObj*, align 8
%current_45args46993 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46992)
store volatile %struct.ScmObj* %current_45args46993, %struct.ScmObj** %stackaddr$prim47667, align 8
%stackaddr$prim47668 = alloca %struct.ScmObj*, align 8
%anf_45bind40247 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46993)
store volatile %struct.ScmObj* %anf_45bind40247, %struct.ScmObj** %stackaddr$prim47668, align 8
%stackaddr$makeclosure47669 = alloca %struct.ScmObj*, align 8
%fptrToInt47670 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40878 to i64
%ae40878 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47670)
store volatile %struct.ScmObj* %ae40878, %struct.ScmObj** %stackaddr$makeclosure47669, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40878, %struct.ScmObj* %Ycmb40102, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40878, %struct.ScmObj* %_37foldr140123, i64 1)
%args47540$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47671 = alloca %struct.ScmObj*, align 8
%args47540$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40247, %struct.ScmObj* %args47540$Ycmb40102$0)
store volatile %struct.ScmObj* %args47540$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47671, align 8
%stackaddr$prim47672 = alloca %struct.ScmObj*, align 8
%args47540$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40878, %struct.ScmObj* %args47540$Ycmb40102$1)
store volatile %struct.ScmObj* %args47540$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47672, align 8
%clofunc47673 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47673(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args47540$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae40878(%struct.ScmObj* %env$ae40878,%struct.ScmObj* %current_45args46995) {
%stackaddr$env-ref47674 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40878, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47674
%stackaddr$env-ref47675 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40878, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47675
%stackaddr$prim47676 = alloca %struct.ScmObj*, align 8
%_95k40372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46995)
store volatile %struct.ScmObj* %_95k40372, %struct.ScmObj** %stackaddr$prim47676, align 8
%stackaddr$prim47677 = alloca %struct.ScmObj*, align 8
%current_45args46996 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46995)
store volatile %struct.ScmObj* %current_45args46996, %struct.ScmObj** %stackaddr$prim47677, align 8
%stackaddr$prim47678 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46996)
store volatile %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$prim47678, align 8
%stackaddr$makeclosure47679 = alloca %struct.ScmObj*, align 8
%fptrToInt47680 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40880 to i64
%ae40880 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47680)
store volatile %struct.ScmObj* %ae40880, %struct.ScmObj** %stackaddr$makeclosure47679, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40880, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40880, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40880, %struct.ScmObj* %_37foldr140123, i64 2)
%ae40881 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47681 = alloca %struct.ScmObj*, align 8
%fptrToInt47682 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40882 to i64
%ae40882 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47682)
store volatile %struct.ScmObj* %ae40882, %struct.ScmObj** %stackaddr$makeclosure47681, align 8
%args47539$ae40880$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47683 = alloca %struct.ScmObj*, align 8
%args47539$ae40880$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40882, %struct.ScmObj* %args47539$ae40880$0)
store volatile %struct.ScmObj* %args47539$ae40880$1, %struct.ScmObj** %stackaddr$prim47683, align 8
%stackaddr$prim47684 = alloca %struct.ScmObj*, align 8
%args47539$ae40880$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40881, %struct.ScmObj* %args47539$ae40880$1)
store volatile %struct.ScmObj* %args47539$ae40880$2, %struct.ScmObj** %stackaddr$prim47684, align 8
%clofunc47685 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40880)
musttail call tailcc void %clofunc47685(%struct.ScmObj* %ae40880, %struct.ScmObj* %args47539$ae40880$2)
ret void
}

define tailcc void @proc_clo$ae40880(%struct.ScmObj* %env$ae40880,%struct.ScmObj* %current_45args46998) {
%stackaddr$env-ref47686 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40880, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47686
%stackaddr$env-ref47687 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40880, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47687
%stackaddr$env-ref47688 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40880, i64 2)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47688
%stackaddr$prim47689 = alloca %struct.ScmObj*, align 8
%_95k40373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46998)
store volatile %struct.ScmObj* %_95k40373, %struct.ScmObj** %stackaddr$prim47689, align 8
%stackaddr$prim47690 = alloca %struct.ScmObj*, align 8
%current_45args46999 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args46998)
store volatile %struct.ScmObj* %current_45args46999, %struct.ScmObj** %stackaddr$prim47690, align 8
%stackaddr$prim47691 = alloca %struct.ScmObj*, align 8
%anf_45bind40254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args46999)
store volatile %struct.ScmObj* %anf_45bind40254, %struct.ScmObj** %stackaddr$prim47691, align 8
%stackaddr$makeclosure47692 = alloca %struct.ScmObj*, align 8
%fptrToInt47693 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41028 to i64
%ae41028 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47693)
store volatile %struct.ScmObj* %ae41028, %struct.ScmObj** %stackaddr$makeclosure47692, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41028, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41028, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41028, %struct.ScmObj* %_37foldr140123, i64 2)
%args47523$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47694 = alloca %struct.ScmObj*, align 8
%args47523$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40254, %struct.ScmObj* %args47523$Ycmb40102$0)
store volatile %struct.ScmObj* %args47523$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47694, align 8
%stackaddr$prim47695 = alloca %struct.ScmObj*, align 8
%args47523$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41028, %struct.ScmObj* %args47523$Ycmb40102$1)
store volatile %struct.ScmObj* %args47523$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47695, align 8
%clofunc47696 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47696(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args47523$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae41028(%struct.ScmObj* %env$ae41028,%struct.ScmObj* %current_45args47001) {
%stackaddr$env-ref47697 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41028, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47697
%stackaddr$env-ref47698 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41028, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47698
%stackaddr$env-ref47699 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41028, i64 2)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47699
%stackaddr$prim47700 = alloca %struct.ScmObj*, align 8
%_95k40374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47001)
store volatile %struct.ScmObj* %_95k40374, %struct.ScmObj** %stackaddr$prim47700, align 8
%stackaddr$prim47701 = alloca %struct.ScmObj*, align 8
%current_45args47002 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47001)
store volatile %struct.ScmObj* %current_45args47002, %struct.ScmObj** %stackaddr$prim47701, align 8
%stackaddr$prim47702 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47002)
store volatile %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$prim47702, align 8
%stackaddr$makeclosure47703 = alloca %struct.ScmObj*, align 8
%fptrToInt47704 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41030 to i64
%ae41030 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47704)
store volatile %struct.ScmObj* %ae41030, %struct.ScmObj** %stackaddr$makeclosure47703, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41030, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41030, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41030, %struct.ScmObj* %_37take40115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41030, %struct.ScmObj* %_37foldr140123, i64 3)
%ae41031 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47705 = alloca %struct.ScmObj*, align 8
%fptrToInt47706 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41032 to i64
%ae41032 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47706)
store volatile %struct.ScmObj* %ae41032, %struct.ScmObj** %stackaddr$makeclosure47705, align 8
%args47522$ae41030$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47707 = alloca %struct.ScmObj*, align 8
%args47522$ae41030$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41032, %struct.ScmObj* %args47522$ae41030$0)
store volatile %struct.ScmObj* %args47522$ae41030$1, %struct.ScmObj** %stackaddr$prim47707, align 8
%stackaddr$prim47708 = alloca %struct.ScmObj*, align 8
%args47522$ae41030$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41031, %struct.ScmObj* %args47522$ae41030$1)
store volatile %struct.ScmObj* %args47522$ae41030$2, %struct.ScmObj** %stackaddr$prim47708, align 8
%clofunc47709 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41030)
musttail call tailcc void %clofunc47709(%struct.ScmObj* %ae41030, %struct.ScmObj* %args47522$ae41030$2)
ret void
}

define tailcc void @proc_clo$ae41030(%struct.ScmObj* %env$ae41030,%struct.ScmObj* %current_45args47004) {
%stackaddr$env-ref47710 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41030, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47710
%stackaddr$env-ref47711 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41030, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47711
%stackaddr$env-ref47712 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41030, i64 2)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47712
%stackaddr$env-ref47713 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41030, i64 3)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47713
%stackaddr$prim47714 = alloca %struct.ScmObj*, align 8
%_95k40375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47004)
store volatile %struct.ScmObj* %_95k40375, %struct.ScmObj** %stackaddr$prim47714, align 8
%stackaddr$prim47715 = alloca %struct.ScmObj*, align 8
%current_45args47005 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47004)
store volatile %struct.ScmObj* %current_45args47005, %struct.ScmObj** %stackaddr$prim47715, align 8
%stackaddr$prim47716 = alloca %struct.ScmObj*, align 8
%anf_45bind40258 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47005)
store volatile %struct.ScmObj* %anf_45bind40258, %struct.ScmObj** %stackaddr$prim47716, align 8
%stackaddr$makeclosure47717 = alloca %struct.ScmObj*, align 8
%fptrToInt47718 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41111 to i64
%ae41111 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt47718)
store volatile %struct.ScmObj* %ae41111, %struct.ScmObj** %stackaddr$makeclosure47717, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41111, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41111, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41111, %struct.ScmObj* %_37take40115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41111, %struct.ScmObj* %_37foldr140123, i64 3)
%args47508$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47719 = alloca %struct.ScmObj*, align 8
%args47508$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40258, %struct.ScmObj* %args47508$Ycmb40102$0)
store volatile %struct.ScmObj* %args47508$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47719, align 8
%stackaddr$prim47720 = alloca %struct.ScmObj*, align 8
%args47508$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41111, %struct.ScmObj* %args47508$Ycmb40102$1)
store volatile %struct.ScmObj* %args47508$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47720, align 8
%clofunc47721 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47721(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args47508$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae41111(%struct.ScmObj* %env$ae41111,%struct.ScmObj* %current_45args47007) {
%stackaddr$env-ref47722 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41111, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47722
%stackaddr$env-ref47723 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41111, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47723
%stackaddr$env-ref47724 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41111, i64 2)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47724
%stackaddr$env-ref47725 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41111, i64 3)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47725
%stackaddr$prim47726 = alloca %struct.ScmObj*, align 8
%_95k40376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47007)
store volatile %struct.ScmObj* %_95k40376, %struct.ScmObj** %stackaddr$prim47726, align 8
%stackaddr$prim47727 = alloca %struct.ScmObj*, align 8
%current_45args47008 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47007)
store volatile %struct.ScmObj* %current_45args47008, %struct.ScmObj** %stackaddr$prim47727, align 8
%stackaddr$prim47728 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47008)
store volatile %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$prim47728, align 8
%stackaddr$makeclosure47729 = alloca %struct.ScmObj*, align 8
%fptrToInt47730 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41113 to i64
%ae41113 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47730)
store volatile %struct.ScmObj* %ae41113, %struct.ScmObj** %stackaddr$makeclosure47729, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41113, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41113, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41113, %struct.ScmObj* %_37take40115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41113, %struct.ScmObj* %_37length40112, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41113, %struct.ScmObj* %_37foldr140123, i64 4)
%ae41114 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47731 = alloca %struct.ScmObj*, align 8
%fptrToInt47732 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41115 to i64
%ae41115 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47732)
store volatile %struct.ScmObj* %ae41115, %struct.ScmObj** %stackaddr$makeclosure47731, align 8
%args47507$ae41113$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47733 = alloca %struct.ScmObj*, align 8
%args47507$ae41113$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41115, %struct.ScmObj* %args47507$ae41113$0)
store volatile %struct.ScmObj* %args47507$ae41113$1, %struct.ScmObj** %stackaddr$prim47733, align 8
%stackaddr$prim47734 = alloca %struct.ScmObj*, align 8
%args47507$ae41113$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41114, %struct.ScmObj* %args47507$ae41113$1)
store volatile %struct.ScmObj* %args47507$ae41113$2, %struct.ScmObj** %stackaddr$prim47734, align 8
%clofunc47735 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41113)
musttail call tailcc void %clofunc47735(%struct.ScmObj* %ae41113, %struct.ScmObj* %args47507$ae41113$2)
ret void
}

define tailcc void @proc_clo$ae41113(%struct.ScmObj* %env$ae41113,%struct.ScmObj* %current_45args47010) {
%stackaddr$env-ref47736 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41113, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47736
%stackaddr$env-ref47737 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41113, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47737
%stackaddr$env-ref47738 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41113, i64 2)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47738
%stackaddr$env-ref47739 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41113, i64 3)
store %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$env-ref47739
%stackaddr$env-ref47740 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41113, i64 4)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47740
%stackaddr$prim47741 = alloca %struct.ScmObj*, align 8
%_95k40377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47010)
store volatile %struct.ScmObj* %_95k40377, %struct.ScmObj** %stackaddr$prim47741, align 8
%stackaddr$prim47742 = alloca %struct.ScmObj*, align 8
%current_45args47011 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47010)
store volatile %struct.ScmObj* %current_45args47011, %struct.ScmObj** %stackaddr$prim47742, align 8
%stackaddr$prim47743 = alloca %struct.ScmObj*, align 8
%anf_45bind40263 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47011)
store volatile %struct.ScmObj* %anf_45bind40263, %struct.ScmObj** %stackaddr$prim47743, align 8
%stackaddr$makeclosure47744 = alloca %struct.ScmObj*, align 8
%fptrToInt47745 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41190 to i64
%ae41190 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47745)
store volatile %struct.ScmObj* %ae41190, %struct.ScmObj** %stackaddr$makeclosure47744, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41190, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41190, %struct.ScmObj* %Ycmb40102, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41190, %struct.ScmObj* %_37take40115, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41190, %struct.ScmObj* %_37length40112, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41190, %struct.ScmObj* %_37foldr140123, i64 4)
%args47491$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47746 = alloca %struct.ScmObj*, align 8
%args47491$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40263, %struct.ScmObj* %args47491$Ycmb40102$0)
store volatile %struct.ScmObj* %args47491$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47746, align 8
%stackaddr$prim47747 = alloca %struct.ScmObj*, align 8
%args47491$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41190, %struct.ScmObj* %args47491$Ycmb40102$1)
store volatile %struct.ScmObj* %args47491$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47747, align 8
%clofunc47748 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47748(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args47491$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae41190(%struct.ScmObj* %env$ae41190,%struct.ScmObj* %current_45args47013) {
%stackaddr$env-ref47749 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41190, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47749
%stackaddr$env-ref47750 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41190, i64 1)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47750
%stackaddr$env-ref47751 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41190, i64 2)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47751
%stackaddr$env-ref47752 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41190, i64 3)
store %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$env-ref47752
%stackaddr$env-ref47753 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41190, i64 4)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47753
%stackaddr$prim47754 = alloca %struct.ScmObj*, align 8
%_95k40378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47013)
store volatile %struct.ScmObj* %_95k40378, %struct.ScmObj** %stackaddr$prim47754, align 8
%stackaddr$prim47755 = alloca %struct.ScmObj*, align 8
%current_45args47014 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47013)
store volatile %struct.ScmObj* %current_45args47014, %struct.ScmObj** %stackaddr$prim47755, align 8
%stackaddr$prim47756 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47014)
store volatile %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$prim47756, align 8
%stackaddr$makeclosure47757 = alloca %struct.ScmObj*, align 8
%fptrToInt47758 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41192 to i64
%ae41192 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt47758)
store volatile %struct.ScmObj* %ae41192, %struct.ScmObj** %stackaddr$makeclosure47757, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41192, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41192, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41192, %struct.ScmObj* %_37map140119, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41192, %struct.ScmObj* %Ycmb40102, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41192, %struct.ScmObj* %_37take40115, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41192, %struct.ScmObj* %_37length40112, i64 5)
%ae41193 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47759 = alloca %struct.ScmObj*, align 8
%fptrToInt47760 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41194 to i64
%ae41194 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47760)
store volatile %struct.ScmObj* %ae41194, %struct.ScmObj** %stackaddr$makeclosure47759, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41194, %struct.ScmObj* %_37foldl140107, i64 0)
%args47490$ae41192$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47761 = alloca %struct.ScmObj*, align 8
%args47490$ae41192$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41194, %struct.ScmObj* %args47490$ae41192$0)
store volatile %struct.ScmObj* %args47490$ae41192$1, %struct.ScmObj** %stackaddr$prim47761, align 8
%stackaddr$prim47762 = alloca %struct.ScmObj*, align 8
%args47490$ae41192$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41193, %struct.ScmObj* %args47490$ae41192$1)
store volatile %struct.ScmObj* %args47490$ae41192$2, %struct.ScmObj** %stackaddr$prim47762, align 8
%clofunc47763 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41192)
musttail call tailcc void %clofunc47763(%struct.ScmObj* %ae41192, %struct.ScmObj* %args47490$ae41192$2)
ret void
}

define tailcc void @proc_clo$ae41192(%struct.ScmObj* %env$ae41192,%struct.ScmObj* %current_45args47016) {
%stackaddr$env-ref47764 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41192, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47764
%stackaddr$env-ref47765 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41192, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47765
%stackaddr$env-ref47766 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41192, i64 2)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47766
%stackaddr$env-ref47767 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41192, i64 3)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47767
%stackaddr$env-ref47768 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41192, i64 4)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref47768
%stackaddr$env-ref47769 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41192, i64 5)
store %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$env-ref47769
%stackaddr$prim47770 = alloca %struct.ScmObj*, align 8
%_95k40379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47016)
store volatile %struct.ScmObj* %_95k40379, %struct.ScmObj** %stackaddr$prim47770, align 8
%stackaddr$prim47771 = alloca %struct.ScmObj*, align 8
%current_45args47017 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47016)
store volatile %struct.ScmObj* %current_45args47017, %struct.ScmObj** %stackaddr$prim47771, align 8
%stackaddr$prim47772 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47017)
store volatile %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$prim47772, align 8
%stackaddr$makeclosure47773 = alloca %struct.ScmObj*, align 8
%fptrToInt47774 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41246 to i64
%ae41246 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47774)
store volatile %struct.ScmObj* %ae41246, %struct.ScmObj** %stackaddr$makeclosure47773, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41246, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41246, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41246, %struct.ScmObj* %_37map140119, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41246, %struct.ScmObj* %Ycmb40102, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41246, %struct.ScmObj* %_37last40145, i64 4)
%ae41247 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47775 = alloca %struct.ScmObj*, align 8
%fptrToInt47776 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41248 to i64
%ae41248 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47776)
store volatile %struct.ScmObj* %ae41248, %struct.ScmObj** %stackaddr$makeclosure47775, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41248, %struct.ScmObj* %_37take40115, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41248, %struct.ScmObj* %_37length40112, i64 1)
%args47476$ae41246$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47777 = alloca %struct.ScmObj*, align 8
%args47476$ae41246$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41248, %struct.ScmObj* %args47476$ae41246$0)
store volatile %struct.ScmObj* %args47476$ae41246$1, %struct.ScmObj** %stackaddr$prim47777, align 8
%stackaddr$prim47778 = alloca %struct.ScmObj*, align 8
%args47476$ae41246$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41247, %struct.ScmObj* %args47476$ae41246$1)
store volatile %struct.ScmObj* %args47476$ae41246$2, %struct.ScmObj** %stackaddr$prim47778, align 8
%clofunc47779 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41246)
musttail call tailcc void %clofunc47779(%struct.ScmObj* %ae41246, %struct.ScmObj* %args47476$ae41246$2)
ret void
}

define tailcc void @proc_clo$ae41246(%struct.ScmObj* %env$ae41246,%struct.ScmObj* %current_45args47019) {
%stackaddr$env-ref47780 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41246, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47780
%stackaddr$env-ref47781 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41246, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47781
%stackaddr$env-ref47782 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41246, i64 2)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref47782
%stackaddr$env-ref47783 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41246, i64 3)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47783
%stackaddr$env-ref47784 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41246, i64 4)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref47784
%stackaddr$prim47785 = alloca %struct.ScmObj*, align 8
%_95k40380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47019)
store volatile %struct.ScmObj* %_95k40380, %struct.ScmObj** %stackaddr$prim47785, align 8
%stackaddr$prim47786 = alloca %struct.ScmObj*, align 8
%current_45args47020 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47019)
store volatile %struct.ScmObj* %current_45args47020, %struct.ScmObj** %stackaddr$prim47786, align 8
%stackaddr$prim47787 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47020)
store volatile %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$prim47787, align 8
%stackaddr$makeclosure47788 = alloca %struct.ScmObj*, align 8
%fptrToInt47789 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41276 to i64
%ae41276 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47789)
store volatile %struct.ScmObj* %ae41276, %struct.ScmObj** %stackaddr$makeclosure47788, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41276, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41276, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41276, %struct.ScmObj* %Ycmb40102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41276, %struct.ScmObj* %_37last40145, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41276, %struct.ScmObj* %_37drop_45right40142, i64 4)
%ae41277 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47790 = alloca %struct.ScmObj*, align 8
%fptrToInt47791 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41278 to i64
%ae41278 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47791)
store volatile %struct.ScmObj* %ae41278, %struct.ScmObj** %stackaddr$makeclosure47790, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41278, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41278, %struct.ScmObj* %_37foldr140123, i64 1)
%args47466$ae41276$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47792 = alloca %struct.ScmObj*, align 8
%args47466$ae41276$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41278, %struct.ScmObj* %args47466$ae41276$0)
store volatile %struct.ScmObj* %args47466$ae41276$1, %struct.ScmObj** %stackaddr$prim47792, align 8
%stackaddr$prim47793 = alloca %struct.ScmObj*, align 8
%args47466$ae41276$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41277, %struct.ScmObj* %args47466$ae41276$1)
store volatile %struct.ScmObj* %args47466$ae41276$2, %struct.ScmObj** %stackaddr$prim47793, align 8
%clofunc47794 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41276)
musttail call tailcc void %clofunc47794(%struct.ScmObj* %ae41276, %struct.ScmObj* %args47466$ae41276$2)
ret void
}

define tailcc void @proc_clo$ae41276(%struct.ScmObj* %env$ae41276,%struct.ScmObj* %current_45args47022) {
%stackaddr$env-ref47795 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41276, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47795
%stackaddr$env-ref47796 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41276, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47796
%stackaddr$env-ref47797 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41276, i64 2)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47797
%stackaddr$env-ref47798 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41276, i64 3)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref47798
%stackaddr$env-ref47799 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41276, i64 4)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref47799
%stackaddr$prim47800 = alloca %struct.ScmObj*, align 8
%_95k40381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47022)
store volatile %struct.ScmObj* %_95k40381, %struct.ScmObj** %stackaddr$prim47800, align 8
%stackaddr$prim47801 = alloca %struct.ScmObj*, align 8
%current_45args47023 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47022)
store volatile %struct.ScmObj* %current_45args47023, %struct.ScmObj** %stackaddr$prim47801, align 8
%stackaddr$prim47802 = alloca %struct.ScmObj*, align 8
%anf_45bind40279 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47023)
store volatile %struct.ScmObj* %anf_45bind40279, %struct.ScmObj** %stackaddr$prim47802, align 8
%stackaddr$makeclosure47803 = alloca %struct.ScmObj*, align 8
%fptrToInt47804 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41660 to i64
%ae41660 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47804)
store volatile %struct.ScmObj* %ae41660, %struct.ScmObj** %stackaddr$makeclosure47803, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41660, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41660, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41660, %struct.ScmObj* %Ycmb40102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41660, %struct.ScmObj* %_37last40145, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41660, %struct.ScmObj* %_37drop_45right40142, i64 4)
%args47406$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47805 = alloca %struct.ScmObj*, align 8
%args47406$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40279, %struct.ScmObj* %args47406$Ycmb40102$0)
store volatile %struct.ScmObj* %args47406$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47805, align 8
%stackaddr$prim47806 = alloca %struct.ScmObj*, align 8
%args47406$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41660, %struct.ScmObj* %args47406$Ycmb40102$1)
store volatile %struct.ScmObj* %args47406$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47806, align 8
%clofunc47807 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47807(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args47406$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae41660(%struct.ScmObj* %env$ae41660,%struct.ScmObj* %current_45args47025) {
%stackaddr$env-ref47808 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41660, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47808
%stackaddr$env-ref47809 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41660, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47809
%stackaddr$env-ref47810 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41660, i64 2)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47810
%stackaddr$env-ref47811 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41660, i64 3)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref47811
%stackaddr$env-ref47812 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41660, i64 4)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref47812
%stackaddr$prim47813 = alloca %struct.ScmObj*, align 8
%_95k40382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47025)
store volatile %struct.ScmObj* %_95k40382, %struct.ScmObj** %stackaddr$prim47813, align 8
%stackaddr$prim47814 = alloca %struct.ScmObj*, align 8
%current_45args47026 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47025)
store volatile %struct.ScmObj* %current_45args47026, %struct.ScmObj** %stackaddr$prim47814, align 8
%stackaddr$prim47815 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47026)
store volatile %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$prim47815, align 8
%stackaddr$makeclosure47816 = alloca %struct.ScmObj*, align 8
%fptrToInt47817 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41662 to i64
%ae41662 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt47817)
store volatile %struct.ScmObj* %ae41662, %struct.ScmObj** %stackaddr$makeclosure47816, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41662, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41662, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41662, %struct.ScmObj* %Ycmb40102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41662, %struct.ScmObj* %_37last40145, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41662, %struct.ScmObj* %_37foldr40128, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41662, %struct.ScmObj* %_37drop_45right40142, i64 5)
%ae41663 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47818 = alloca %struct.ScmObj*, align 8
%fptrToInt47819 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41664 to i64
%ae41664 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47819)
store volatile %struct.ScmObj* %ae41664, %struct.ScmObj** %stackaddr$makeclosure47818, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41664, %struct.ScmObj* %_37foldr140123, i64 0)
%args47405$ae41662$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47820 = alloca %struct.ScmObj*, align 8
%args47405$ae41662$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41664, %struct.ScmObj* %args47405$ae41662$0)
store volatile %struct.ScmObj* %args47405$ae41662$1, %struct.ScmObj** %stackaddr$prim47820, align 8
%stackaddr$prim47821 = alloca %struct.ScmObj*, align 8
%args47405$ae41662$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41663, %struct.ScmObj* %args47405$ae41662$1)
store volatile %struct.ScmObj* %args47405$ae41662$2, %struct.ScmObj** %stackaddr$prim47821, align 8
%clofunc47822 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41662)
musttail call tailcc void %clofunc47822(%struct.ScmObj* %ae41662, %struct.ScmObj* %args47405$ae41662$2)
ret void
}

define tailcc void @proc_clo$ae41662(%struct.ScmObj* %env$ae41662,%struct.ScmObj* %current_45args47028) {
%stackaddr$env-ref47823 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41662, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47823
%stackaddr$env-ref47824 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41662, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47824
%stackaddr$env-ref47825 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41662, i64 2)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47825
%stackaddr$env-ref47826 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41662, i64 3)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref47826
%stackaddr$env-ref47827 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41662, i64 4)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47827
%stackaddr$env-ref47828 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41662, i64 5)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref47828
%stackaddr$prim47829 = alloca %struct.ScmObj*, align 8
%_95k40383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47028)
store volatile %struct.ScmObj* %_95k40383, %struct.ScmObj** %stackaddr$prim47829, align 8
%stackaddr$prim47830 = alloca %struct.ScmObj*, align 8
%current_45args47029 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47028)
store volatile %struct.ScmObj* %current_45args47029, %struct.ScmObj** %stackaddr$prim47830, align 8
%stackaddr$prim47831 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47029)
store volatile %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$prim47831, align 8
%stackaddr$makeclosure47832 = alloca %struct.ScmObj*, align 8
%fptrToInt47833 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41739 to i64
%ae41739 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt47833)
store volatile %struct.ScmObj* %ae41739, %struct.ScmObj** %stackaddr$makeclosure47832, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41739, %struct.ScmObj* %_37foldr140123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41739, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41739, %struct.ScmObj* %Ycmb40102, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41739, %struct.ScmObj* %_37foldr40128, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41739, %struct.ScmObj* %_37map140154, i64 4)
%ae41740 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47834 = alloca %struct.ScmObj*, align 8
%fptrToInt47835 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41741 to i64
%ae41741 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47835)
store volatile %struct.ScmObj* %ae41741, %struct.ScmObj** %stackaddr$makeclosure47834, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41741, %struct.ScmObj* %_37last40145, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41741, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41741, %struct.ScmObj* %_37drop_45right40142, i64 2)
%args47386$ae41739$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47836 = alloca %struct.ScmObj*, align 8
%args47386$ae41739$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41741, %struct.ScmObj* %args47386$ae41739$0)
store volatile %struct.ScmObj* %args47386$ae41739$1, %struct.ScmObj** %stackaddr$prim47836, align 8
%stackaddr$prim47837 = alloca %struct.ScmObj*, align 8
%args47386$ae41739$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41740, %struct.ScmObj* %args47386$ae41739$1)
store volatile %struct.ScmObj* %args47386$ae41739$2, %struct.ScmObj** %stackaddr$prim47837, align 8
%clofunc47838 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41739)
musttail call tailcc void %clofunc47838(%struct.ScmObj* %ae41739, %struct.ScmObj* %args47386$ae41739$2)
ret void
}

define tailcc void @proc_clo$ae41739(%struct.ScmObj* %env$ae41739,%struct.ScmObj* %current_45args47031) {
%stackaddr$env-ref47839 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41739, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref47839
%stackaddr$env-ref47840 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41739, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47840
%stackaddr$env-ref47841 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41739, i64 2)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47841
%stackaddr$env-ref47842 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41739, i64 3)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref47842
%stackaddr$env-ref47843 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41739, i64 4)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47843
%stackaddr$prim47844 = alloca %struct.ScmObj*, align 8
%_95k40384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47031)
store volatile %struct.ScmObj* %_95k40384, %struct.ScmObj** %stackaddr$prim47844, align 8
%stackaddr$prim47845 = alloca %struct.ScmObj*, align 8
%current_45args47032 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47031)
store volatile %struct.ScmObj* %current_45args47032, %struct.ScmObj** %stackaddr$prim47845, align 8
%stackaddr$prim47846 = alloca %struct.ScmObj*, align 8
%_37map40149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47032)
store volatile %struct.ScmObj* %_37map40149, %struct.ScmObj** %stackaddr$prim47846, align 8
%stackaddr$makeclosure47847 = alloca %struct.ScmObj*, align 8
%fptrToInt47848 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41885 to i64
%ae41885 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47848)
store volatile %struct.ScmObj* %ae41885, %struct.ScmObj** %stackaddr$makeclosure47847, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41885, %struct.ScmObj* %Ycmb40102, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41885, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41885, %struct.ScmObj* %_37map140154, i64 2)
%ae41886 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47849 = alloca %struct.ScmObj*, align 8
%fptrToInt47850 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41887 to i64
%ae41887 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47850)
store volatile %struct.ScmObj* %ae41887, %struct.ScmObj** %stackaddr$makeclosure47849, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41887, %struct.ScmObj* %_37foldr40128, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41887, %struct.ScmObj* %_37foldr140123, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41887, %struct.ScmObj* %_37map140154, i64 2)
%args47369$ae41885$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47851 = alloca %struct.ScmObj*, align 8
%args47369$ae41885$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41887, %struct.ScmObj* %args47369$ae41885$0)
store volatile %struct.ScmObj* %args47369$ae41885$1, %struct.ScmObj** %stackaddr$prim47851, align 8
%stackaddr$prim47852 = alloca %struct.ScmObj*, align 8
%args47369$ae41885$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41886, %struct.ScmObj* %args47369$ae41885$1)
store volatile %struct.ScmObj* %args47369$ae41885$2, %struct.ScmObj** %stackaddr$prim47852, align 8
%clofunc47853 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41885)
musttail call tailcc void %clofunc47853(%struct.ScmObj* %ae41885, %struct.ScmObj* %args47369$ae41885$2)
ret void
}

define tailcc void @proc_clo$ae41885(%struct.ScmObj* %env$ae41885,%struct.ScmObj* %current_45args47034) {
%stackaddr$env-ref47854 = alloca %struct.ScmObj*, align 8
%Ycmb40102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41885, i64 0)
store %struct.ScmObj* %Ycmb40102, %struct.ScmObj** %stackaddr$env-ref47854
%stackaddr$env-ref47855 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41885, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47855
%stackaddr$env-ref47856 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41885, i64 2)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47856
%stackaddr$prim47857 = alloca %struct.ScmObj*, align 8
%_95k40385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47034)
store volatile %struct.ScmObj* %_95k40385, %struct.ScmObj** %stackaddr$prim47857, align 8
%stackaddr$prim47858 = alloca %struct.ScmObj*, align 8
%current_45args47035 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47034)
store volatile %struct.ScmObj* %current_45args47035, %struct.ScmObj** %stackaddr$prim47858, align 8
%stackaddr$prim47859 = alloca %struct.ScmObj*, align 8
%anf_45bind40299 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47035)
store volatile %struct.ScmObj* %anf_45bind40299, %struct.ScmObj** %stackaddr$prim47859, align 8
%stackaddr$makeclosure47860 = alloca %struct.ScmObj*, align 8
%fptrToInt47861 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42277 to i64
%ae42277 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47861)
store volatile %struct.ScmObj* %ae42277, %struct.ScmObj** %stackaddr$makeclosure47860, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42277, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42277, %struct.ScmObj* %_37map140154, i64 1)
%args47309$Ycmb40102$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47862 = alloca %struct.ScmObj*, align 8
%args47309$Ycmb40102$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40299, %struct.ScmObj* %args47309$Ycmb40102$0)
store volatile %struct.ScmObj* %args47309$Ycmb40102$1, %struct.ScmObj** %stackaddr$prim47862, align 8
%stackaddr$prim47863 = alloca %struct.ScmObj*, align 8
%args47309$Ycmb40102$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42277, %struct.ScmObj* %args47309$Ycmb40102$1)
store volatile %struct.ScmObj* %args47309$Ycmb40102$2, %struct.ScmObj** %stackaddr$prim47863, align 8
%clofunc47864 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb40102)
musttail call tailcc void %clofunc47864(%struct.ScmObj* %Ycmb40102, %struct.ScmObj* %args47309$Ycmb40102$2)
ret void
}

define tailcc void @proc_clo$ae42277(%struct.ScmObj* %env$ae42277,%struct.ScmObj* %current_45args47037) {
%stackaddr$env-ref47865 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42277, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47865
%stackaddr$env-ref47866 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42277, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47866
%stackaddr$prim47867 = alloca %struct.ScmObj*, align 8
%_95k40386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47037)
store volatile %struct.ScmObj* %_95k40386, %struct.ScmObj** %stackaddr$prim47867, align 8
%stackaddr$prim47868 = alloca %struct.ScmObj*, align 8
%current_45args47038 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47037)
store volatile %struct.ScmObj* %current_45args47038, %struct.ScmObj** %stackaddr$prim47868, align 8
%stackaddr$prim47869 = alloca %struct.ScmObj*, align 8
%_37foldl40205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47038)
store volatile %struct.ScmObj* %_37foldl40205, %struct.ScmObj** %stackaddr$prim47869, align 8
%stackaddr$makeclosure47870 = alloca %struct.ScmObj*, align 8
%fptrToInt47871 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42279 to i64
%ae42279 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47871)
store volatile %struct.ScmObj* %ae42279, %struct.ScmObj** %stackaddr$makeclosure47870, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42279, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42279, %struct.ScmObj* %_37map140154, i64 1)
%ae42280 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47872 = alloca %struct.ScmObj*, align 8
%fptrToInt47873 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42281 to i64
%ae42281 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47873)
store volatile %struct.ScmObj* %ae42281, %struct.ScmObj** %stackaddr$makeclosure47872, align 8
%args47308$ae42279$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47874 = alloca %struct.ScmObj*, align 8
%args47308$ae42279$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42281, %struct.ScmObj* %args47308$ae42279$0)
store volatile %struct.ScmObj* %args47308$ae42279$1, %struct.ScmObj** %stackaddr$prim47874, align 8
%stackaddr$prim47875 = alloca %struct.ScmObj*, align 8
%args47308$ae42279$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42280, %struct.ScmObj* %args47308$ae42279$1)
store volatile %struct.ScmObj* %args47308$ae42279$2, %struct.ScmObj** %stackaddr$prim47875, align 8
%clofunc47876 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42279)
musttail call tailcc void %clofunc47876(%struct.ScmObj* %ae42279, %struct.ScmObj* %args47308$ae42279$2)
ret void
}

define tailcc void @proc_clo$ae42279(%struct.ScmObj* %env$ae42279,%struct.ScmObj* %current_45args47040) {
%stackaddr$env-ref47877 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42279, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47877
%stackaddr$env-ref47878 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42279, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47878
%stackaddr$prim47879 = alloca %struct.ScmObj*, align 8
%_95k40387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47040)
store volatile %struct.ScmObj* %_95k40387, %struct.ScmObj** %stackaddr$prim47879, align 8
%stackaddr$prim47880 = alloca %struct.ScmObj*, align 8
%current_45args47041 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47040)
store volatile %struct.ScmObj* %current_45args47041, %struct.ScmObj** %stackaddr$prim47880, align 8
%stackaddr$prim47881 = alloca %struct.ScmObj*, align 8
%_37_6240202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47041)
store volatile %struct.ScmObj* %_37_6240202, %struct.ScmObj** %stackaddr$prim47881, align 8
%stackaddr$makeclosure47882 = alloca %struct.ScmObj*, align 8
%fptrToInt47883 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42303 to i64
%ae42303 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47883)
store volatile %struct.ScmObj* %ae42303, %struct.ScmObj** %stackaddr$makeclosure47882, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42303, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42303, %struct.ScmObj* %_37map140154, i64 1)
%ae42304 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47884 = alloca %struct.ScmObj*, align 8
%fptrToInt47885 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42305 to i64
%ae42305 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47885)
store volatile %struct.ScmObj* %ae42305, %struct.ScmObj** %stackaddr$makeclosure47884, align 8
%args47302$ae42303$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47886 = alloca %struct.ScmObj*, align 8
%args47302$ae42303$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42305, %struct.ScmObj* %args47302$ae42303$0)
store volatile %struct.ScmObj* %args47302$ae42303$1, %struct.ScmObj** %stackaddr$prim47886, align 8
%stackaddr$prim47887 = alloca %struct.ScmObj*, align 8
%args47302$ae42303$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42304, %struct.ScmObj* %args47302$ae42303$1)
store volatile %struct.ScmObj* %args47302$ae42303$2, %struct.ScmObj** %stackaddr$prim47887, align 8
%clofunc47888 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42303)
musttail call tailcc void %clofunc47888(%struct.ScmObj* %ae42303, %struct.ScmObj* %args47302$ae42303$2)
ret void
}

define tailcc void @proc_clo$ae42303(%struct.ScmObj* %env$ae42303,%struct.ScmObj* %current_45args47043) {
%stackaddr$env-ref47889 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42303, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47889
%stackaddr$env-ref47890 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42303, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47890
%stackaddr$prim47891 = alloca %struct.ScmObj*, align 8
%_95k40388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47043)
store volatile %struct.ScmObj* %_95k40388, %struct.ScmObj** %stackaddr$prim47891, align 8
%stackaddr$prim47892 = alloca %struct.ScmObj*, align 8
%current_45args47044 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47043)
store volatile %struct.ScmObj* %current_45args47044, %struct.ScmObj** %stackaddr$prim47892, align 8
%stackaddr$prim47893 = alloca %struct.ScmObj*, align 8
%_37_62_6140199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47044)
store volatile %struct.ScmObj* %_37_62_6140199, %struct.ScmObj** %stackaddr$prim47893, align 8
%ae42327 = call %struct.ScmObj* @const_init_int(i64 1)
%ae42328 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47894 = alloca %struct.ScmObj*, align 8
%_37append40195 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42327, %struct.ScmObj* %ae42328)
store volatile %struct.ScmObj* %_37append40195, %struct.ScmObj** %stackaddr$prim47894, align 8
%stackaddr$makeclosure47895 = alloca %struct.ScmObj*, align 8
%fptrToInt47896 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42329 to i64
%ae42329 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt47896)
store volatile %struct.ScmObj* %ae42329, %struct.ScmObj** %stackaddr$makeclosure47895, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42329, %struct.ScmObj* %_37append40195, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42329, %struct.ScmObj* %_37foldl140107, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42329, %struct.ScmObj* %_37map140154, i64 2)
%ae42330 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47897 = alloca %struct.ScmObj*, align 8
%fptrToInt47898 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42331 to i64
%ae42331 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47898)
store volatile %struct.ScmObj* %ae42331, %struct.ScmObj** %stackaddr$makeclosure47897, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42331, %struct.ScmObj* %_37append40195, i64 0)
%args47296$ae42329$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47899 = alloca %struct.ScmObj*, align 8
%args47296$ae42329$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42331, %struct.ScmObj* %args47296$ae42329$0)
store volatile %struct.ScmObj* %args47296$ae42329$1, %struct.ScmObj** %stackaddr$prim47899, align 8
%stackaddr$prim47900 = alloca %struct.ScmObj*, align 8
%args47296$ae42329$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42330, %struct.ScmObj* %args47296$ae42329$1)
store volatile %struct.ScmObj* %args47296$ae42329$2, %struct.ScmObj** %stackaddr$prim47900, align 8
%clofunc47901 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42329)
musttail call tailcc void %clofunc47901(%struct.ScmObj* %ae42329, %struct.ScmObj* %args47296$ae42329$2)
ret void
}

define tailcc void @proc_clo$ae42329(%struct.ScmObj* %env$ae42329,%struct.ScmObj* %current_45args47046) {
%stackaddr$env-ref47902 = alloca %struct.ScmObj*, align 8
%_37append40195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42329, i64 0)
store %struct.ScmObj* %_37append40195, %struct.ScmObj** %stackaddr$env-ref47902
%stackaddr$env-ref47903 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42329, i64 1)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47903
%stackaddr$env-ref47904 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42329, i64 2)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47904
%stackaddr$prim47905 = alloca %struct.ScmObj*, align 8
%_95k40389 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47046)
store volatile %struct.ScmObj* %_95k40389, %struct.ScmObj** %stackaddr$prim47905, align 8
%stackaddr$prim47906 = alloca %struct.ScmObj*, align 8
%current_45args47047 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47046)
store volatile %struct.ScmObj* %current_45args47047, %struct.ScmObj** %stackaddr$prim47906, align 8
%stackaddr$prim47907 = alloca %struct.ScmObj*, align 8
%anf_45bind40307 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47047)
store volatile %struct.ScmObj* %anf_45bind40307, %struct.ScmObj** %stackaddr$prim47907, align 8
%ae42397 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47908 = alloca %struct.ScmObj*, align 8
%_95040196 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append40195, %struct.ScmObj* %ae42397, %struct.ScmObj* %anf_45bind40307)
store volatile %struct.ScmObj* %_95040196, %struct.ScmObj** %stackaddr$prim47908, align 8
%ae42400 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim47909 = alloca %struct.ScmObj*, align 8
%_37append40194 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40195, %struct.ScmObj* %ae42400)
store volatile %struct.ScmObj* %_37append40194, %struct.ScmObj** %stackaddr$prim47909, align 8
%stackaddr$makeclosure47910 = alloca %struct.ScmObj*, align 8
%fptrToInt47911 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42401 to i64
%ae42401 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47911)
store volatile %struct.ScmObj* %ae42401, %struct.ScmObj** %stackaddr$makeclosure47910, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42401, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42401, %struct.ScmObj* %_37map140154, i64 1)
%ae42402 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47912 = alloca %struct.ScmObj*, align 8
%fptrToInt47913 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42403 to i64
%ae42403 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47913)
store volatile %struct.ScmObj* %ae42403, %struct.ScmObj** %stackaddr$makeclosure47912, align 8
%args47285$ae42401$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47914 = alloca %struct.ScmObj*, align 8
%args47285$ae42401$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42403, %struct.ScmObj* %args47285$ae42401$0)
store volatile %struct.ScmObj* %args47285$ae42401$1, %struct.ScmObj** %stackaddr$prim47914, align 8
%stackaddr$prim47915 = alloca %struct.ScmObj*, align 8
%args47285$ae42401$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42402, %struct.ScmObj* %args47285$ae42401$1)
store volatile %struct.ScmObj* %args47285$ae42401$2, %struct.ScmObj** %stackaddr$prim47915, align 8
%clofunc47916 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42401)
musttail call tailcc void %clofunc47916(%struct.ScmObj* %ae42401, %struct.ScmObj* %args47285$ae42401$2)
ret void
}

define tailcc void @proc_clo$ae42401(%struct.ScmObj* %env$ae42401,%struct.ScmObj* %current_45args47049) {
%stackaddr$env-ref47917 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42401, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47917
%stackaddr$env-ref47918 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42401, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47918
%stackaddr$prim47919 = alloca %struct.ScmObj*, align 8
%_95k40390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47049)
store volatile %struct.ScmObj* %_95k40390, %struct.ScmObj** %stackaddr$prim47919, align 8
%stackaddr$prim47920 = alloca %struct.ScmObj*, align 8
%current_45args47050 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47049)
store volatile %struct.ScmObj* %current_45args47050, %struct.ScmObj** %stackaddr$prim47920, align 8
%stackaddr$prim47921 = alloca %struct.ScmObj*, align 8
%_37list_6340187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47050)
store volatile %struct.ScmObj* %_37list_6340187, %struct.ScmObj** %stackaddr$prim47921, align 8
%stackaddr$makeclosure47922 = alloca %struct.ScmObj*, align 8
%fptrToInt47923 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42817 to i64
%ae42817 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47923)
store volatile %struct.ScmObj* %ae42817, %struct.ScmObj** %stackaddr$makeclosure47922, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42817, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42817, %struct.ScmObj* %_37map140154, i64 1)
%ae42818 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47924 = alloca %struct.ScmObj*, align 8
%fptrToInt47925 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42819 to i64
%ae42819 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47925)
store volatile %struct.ScmObj* %ae42819, %struct.ScmObj** %stackaddr$makeclosure47924, align 8
%args47260$ae42817$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47926 = alloca %struct.ScmObj*, align 8
%args47260$ae42817$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42819, %struct.ScmObj* %args47260$ae42817$0)
store volatile %struct.ScmObj* %args47260$ae42817$1, %struct.ScmObj** %stackaddr$prim47926, align 8
%stackaddr$prim47927 = alloca %struct.ScmObj*, align 8
%args47260$ae42817$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42818, %struct.ScmObj* %args47260$ae42817$1)
store volatile %struct.ScmObj* %args47260$ae42817$2, %struct.ScmObj** %stackaddr$prim47927, align 8
%clofunc47928 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42817)
musttail call tailcc void %clofunc47928(%struct.ScmObj* %ae42817, %struct.ScmObj* %args47260$ae42817$2)
ret void
}

define tailcc void @proc_clo$ae42817(%struct.ScmObj* %env$ae42817,%struct.ScmObj* %current_45args47052) {
%stackaddr$env-ref47929 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42817, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47929
%stackaddr$env-ref47930 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42817, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47930
%stackaddr$prim47931 = alloca %struct.ScmObj*, align 8
%_95k40391 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47052)
store volatile %struct.ScmObj* %_95k40391, %struct.ScmObj** %stackaddr$prim47931, align 8
%stackaddr$prim47932 = alloca %struct.ScmObj*, align 8
%current_45args47053 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47052)
store volatile %struct.ScmObj* %current_45args47053, %struct.ScmObj** %stackaddr$prim47932, align 8
%stackaddr$prim47933 = alloca %struct.ScmObj*, align 8
%_37drop40178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47053)
store volatile %struct.ScmObj* %_37drop40178, %struct.ScmObj** %stackaddr$prim47933, align 8
%stackaddr$makeclosure47934 = alloca %struct.ScmObj*, align 8
%fptrToInt47935 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43353 to i64
%ae43353 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47935)
store volatile %struct.ScmObj* %ae43353, %struct.ScmObj** %stackaddr$makeclosure47934, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43353, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43353, %struct.ScmObj* %_37map140154, i64 1)
%ae43354 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47936 = alloca %struct.ScmObj*, align 8
%fptrToInt47937 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43355 to i64
%ae43355 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47937)
store volatile %struct.ScmObj* %ae43355, %struct.ScmObj** %stackaddr$makeclosure47936, align 8
%args47236$ae43353$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47938 = alloca %struct.ScmObj*, align 8
%args47236$ae43353$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43355, %struct.ScmObj* %args47236$ae43353$0)
store volatile %struct.ScmObj* %args47236$ae43353$1, %struct.ScmObj** %stackaddr$prim47938, align 8
%stackaddr$prim47939 = alloca %struct.ScmObj*, align 8
%args47236$ae43353$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43354, %struct.ScmObj* %args47236$ae43353$1)
store volatile %struct.ScmObj* %args47236$ae43353$2, %struct.ScmObj** %stackaddr$prim47939, align 8
%clofunc47940 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43353)
musttail call tailcc void %clofunc47940(%struct.ScmObj* %ae43353, %struct.ScmObj* %args47236$ae43353$2)
ret void
}

define tailcc void @proc_clo$ae43353(%struct.ScmObj* %env$ae43353,%struct.ScmObj* %current_45args47055) {
%stackaddr$env-ref47941 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43353, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47941
%stackaddr$env-ref47942 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43353, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47942
%stackaddr$prim47943 = alloca %struct.ScmObj*, align 8
%_95k40392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47055)
store volatile %struct.ScmObj* %_95k40392, %struct.ScmObj** %stackaddr$prim47943, align 8
%stackaddr$prim47944 = alloca %struct.ScmObj*, align 8
%current_45args47056 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47055)
store volatile %struct.ScmObj* %current_45args47056, %struct.ScmObj** %stackaddr$prim47944, align 8
%stackaddr$prim47945 = alloca %struct.ScmObj*, align 8
%_37memv40171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47056)
store volatile %struct.ScmObj* %_37memv40171, %struct.ScmObj** %stackaddr$prim47945, align 8
%stackaddr$makeclosure47946 = alloca %struct.ScmObj*, align 8
%fptrToInt47947 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43757 to i64
%ae43757 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47947)
store volatile %struct.ScmObj* %ae43757, %struct.ScmObj** %stackaddr$makeclosure47946, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43757, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43757, %struct.ScmObj* %_37map140154, i64 1)
%ae43758 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47948 = alloca %struct.ScmObj*, align 8
%fptrToInt47949 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43759 to i64
%ae43759 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt47949)
store volatile %struct.ScmObj* %ae43759, %struct.ScmObj** %stackaddr$makeclosure47948, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43759, %struct.ScmObj* %_37foldl140107, i64 0)
%args47210$ae43757$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47950 = alloca %struct.ScmObj*, align 8
%args47210$ae43757$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43759, %struct.ScmObj* %args47210$ae43757$0)
store volatile %struct.ScmObj* %args47210$ae43757$1, %struct.ScmObj** %stackaddr$prim47950, align 8
%stackaddr$prim47951 = alloca %struct.ScmObj*, align 8
%args47210$ae43757$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43758, %struct.ScmObj* %args47210$ae43757$1)
store volatile %struct.ScmObj* %args47210$ae43757$2, %struct.ScmObj** %stackaddr$prim47951, align 8
%clofunc47952 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43757)
musttail call tailcc void %clofunc47952(%struct.ScmObj* %ae43757, %struct.ScmObj* %args47210$ae43757$2)
ret void
}

define tailcc void @proc_clo$ae43757(%struct.ScmObj* %env$ae43757,%struct.ScmObj* %current_45args47058) {
%stackaddr$env-ref47953 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43757, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47953
%stackaddr$env-ref47954 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43757, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47954
%stackaddr$prim47955 = alloca %struct.ScmObj*, align 8
%_95k40393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47058)
store volatile %struct.ScmObj* %_95k40393, %struct.ScmObj** %stackaddr$prim47955, align 8
%stackaddr$prim47956 = alloca %struct.ScmObj*, align 8
%current_45args47059 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47058)
store volatile %struct.ScmObj* %current_45args47059, %struct.ScmObj** %stackaddr$prim47956, align 8
%stackaddr$prim47957 = alloca %struct.ScmObj*, align 8
%_37_4740167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47059)
store volatile %struct.ScmObj* %_37_4740167, %struct.ScmObj** %stackaddr$prim47957, align 8
%stackaddr$makeclosure47958 = alloca %struct.ScmObj*, align 8
%fptrToInt47959 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43855 to i64
%ae43855 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47959)
store volatile %struct.ScmObj* %ae43855, %struct.ScmObj** %stackaddr$makeclosure47958, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43855, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43855, %struct.ScmObj* %_37map140154, i64 1)
%ae43856 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47960 = alloca %struct.ScmObj*, align 8
%fptrToInt47961 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43857 to i64
%ae43857 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47961)
store volatile %struct.ScmObj* %ae43857, %struct.ScmObj** %stackaddr$makeclosure47960, align 8
%args47197$ae43855$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47962 = alloca %struct.ScmObj*, align 8
%args47197$ae43855$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43857, %struct.ScmObj* %args47197$ae43855$0)
store volatile %struct.ScmObj* %args47197$ae43855$1, %struct.ScmObj** %stackaddr$prim47962, align 8
%stackaddr$prim47963 = alloca %struct.ScmObj*, align 8
%args47197$ae43855$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43856, %struct.ScmObj* %args47197$ae43855$1)
store volatile %struct.ScmObj* %args47197$ae43855$2, %struct.ScmObj** %stackaddr$prim47963, align 8
%clofunc47964 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43855)
musttail call tailcc void %clofunc47964(%struct.ScmObj* %ae43855, %struct.ScmObj* %args47197$ae43855$2)
ret void
}

define tailcc void @proc_clo$ae43855(%struct.ScmObj* %env$ae43855,%struct.ScmObj* %current_45args47061) {
%stackaddr$env-ref47965 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43855, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47965
%stackaddr$env-ref47966 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43855, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47966
%stackaddr$prim47967 = alloca %struct.ScmObj*, align 8
%_95k40394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47061)
store volatile %struct.ScmObj* %_95k40394, %struct.ScmObj** %stackaddr$prim47967, align 8
%stackaddr$prim47968 = alloca %struct.ScmObj*, align 8
%current_45args47062 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47061)
store volatile %struct.ScmObj* %current_45args47062, %struct.ScmObj** %stackaddr$prim47968, align 8
%stackaddr$prim47969 = alloca %struct.ScmObj*, align 8
%_37first40165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47062)
store volatile %struct.ScmObj* %_37first40165, %struct.ScmObj** %stackaddr$prim47969, align 8
%stackaddr$makeclosure47970 = alloca %struct.ScmObj*, align 8
%fptrToInt47971 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43875 to i64
%ae43875 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47971)
store volatile %struct.ScmObj* %ae43875, %struct.ScmObj** %stackaddr$makeclosure47970, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43875, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43875, %struct.ScmObj* %_37map140154, i64 1)
%ae43876 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47972 = alloca %struct.ScmObj*, align 8
%fptrToInt47973 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43877 to i64
%ae43877 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47973)
store volatile %struct.ScmObj* %ae43877, %struct.ScmObj** %stackaddr$makeclosure47972, align 8
%args47192$ae43875$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47974 = alloca %struct.ScmObj*, align 8
%args47192$ae43875$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43877, %struct.ScmObj* %args47192$ae43875$0)
store volatile %struct.ScmObj* %args47192$ae43875$1, %struct.ScmObj** %stackaddr$prim47974, align 8
%stackaddr$prim47975 = alloca %struct.ScmObj*, align 8
%args47192$ae43875$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43876, %struct.ScmObj* %args47192$ae43875$1)
store volatile %struct.ScmObj* %args47192$ae43875$2, %struct.ScmObj** %stackaddr$prim47975, align 8
%clofunc47976 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43875)
musttail call tailcc void %clofunc47976(%struct.ScmObj* %ae43875, %struct.ScmObj* %args47192$ae43875$2)
ret void
}

define tailcc void @proc_clo$ae43875(%struct.ScmObj* %env$ae43875,%struct.ScmObj* %current_45args47064) {
%stackaddr$env-ref47977 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43875, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47977
%stackaddr$env-ref47978 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43875, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47978
%stackaddr$prim47979 = alloca %struct.ScmObj*, align 8
%_95k40395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47064)
store volatile %struct.ScmObj* %_95k40395, %struct.ScmObj** %stackaddr$prim47979, align 8
%stackaddr$prim47980 = alloca %struct.ScmObj*, align 8
%current_45args47065 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47064)
store volatile %struct.ScmObj* %current_45args47065, %struct.ScmObj** %stackaddr$prim47980, align 8
%stackaddr$prim47981 = alloca %struct.ScmObj*, align 8
%_37second40163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47065)
store volatile %struct.ScmObj* %_37second40163, %struct.ScmObj** %stackaddr$prim47981, align 8
%stackaddr$makeclosure47982 = alloca %struct.ScmObj*, align 8
%fptrToInt47983 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43897 to i64
%ae43897 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47983)
store volatile %struct.ScmObj* %ae43897, %struct.ScmObj** %stackaddr$makeclosure47982, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43897, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43897, %struct.ScmObj* %_37map140154, i64 1)
%ae43898 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47984 = alloca %struct.ScmObj*, align 8
%fptrToInt47985 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43899 to i64
%ae43899 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47985)
store volatile %struct.ScmObj* %ae43899, %struct.ScmObj** %stackaddr$makeclosure47984, align 8
%args47187$ae43897$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47986 = alloca %struct.ScmObj*, align 8
%args47187$ae43897$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43899, %struct.ScmObj* %args47187$ae43897$0)
store volatile %struct.ScmObj* %args47187$ae43897$1, %struct.ScmObj** %stackaddr$prim47986, align 8
%stackaddr$prim47987 = alloca %struct.ScmObj*, align 8
%args47187$ae43897$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43898, %struct.ScmObj* %args47187$ae43897$1)
store volatile %struct.ScmObj* %args47187$ae43897$2, %struct.ScmObj** %stackaddr$prim47987, align 8
%clofunc47988 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43897)
musttail call tailcc void %clofunc47988(%struct.ScmObj* %ae43897, %struct.ScmObj* %args47187$ae43897$2)
ret void
}

define tailcc void @proc_clo$ae43897(%struct.ScmObj* %env$ae43897,%struct.ScmObj* %current_45args47067) {
%stackaddr$env-ref47989 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43897, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref47989
%stackaddr$env-ref47990 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43897, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref47990
%stackaddr$prim47991 = alloca %struct.ScmObj*, align 8
%_95k40396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47067)
store volatile %struct.ScmObj* %_95k40396, %struct.ScmObj** %stackaddr$prim47991, align 8
%stackaddr$prim47992 = alloca %struct.ScmObj*, align 8
%current_45args47068 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47067)
store volatile %struct.ScmObj* %current_45args47068, %struct.ScmObj** %stackaddr$prim47992, align 8
%stackaddr$prim47993 = alloca %struct.ScmObj*, align 8
%_37third40161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47068)
store volatile %struct.ScmObj* %_37third40161, %struct.ScmObj** %stackaddr$prim47993, align 8
%stackaddr$makeclosure47994 = alloca %struct.ScmObj*, align 8
%fptrToInt47995 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43921 to i64
%ae43921 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt47995)
store volatile %struct.ScmObj* %ae43921, %struct.ScmObj** %stackaddr$makeclosure47994, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43921, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43921, %struct.ScmObj* %_37map140154, i64 1)
%ae43922 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure47996 = alloca %struct.ScmObj*, align 8
%fptrToInt47997 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43923 to i64
%ae43923 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt47997)
store volatile %struct.ScmObj* %ae43923, %struct.ScmObj** %stackaddr$makeclosure47996, align 8
%args47182$ae43921$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim47998 = alloca %struct.ScmObj*, align 8
%args47182$ae43921$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43923, %struct.ScmObj* %args47182$ae43921$0)
store volatile %struct.ScmObj* %args47182$ae43921$1, %struct.ScmObj** %stackaddr$prim47998, align 8
%stackaddr$prim47999 = alloca %struct.ScmObj*, align 8
%args47182$ae43921$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43922, %struct.ScmObj* %args47182$ae43921$1)
store volatile %struct.ScmObj* %args47182$ae43921$2, %struct.ScmObj** %stackaddr$prim47999, align 8
%clofunc48000 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43921)
musttail call tailcc void %clofunc48000(%struct.ScmObj* %ae43921, %struct.ScmObj* %args47182$ae43921$2)
ret void
}

define tailcc void @proc_clo$ae43921(%struct.ScmObj* %env$ae43921,%struct.ScmObj* %current_45args47070) {
%stackaddr$env-ref48001 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43921, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48001
%stackaddr$env-ref48002 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43921, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48002
%stackaddr$prim48003 = alloca %struct.ScmObj*, align 8
%_95k40397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47070)
store volatile %struct.ScmObj* %_95k40397, %struct.ScmObj** %stackaddr$prim48003, align 8
%stackaddr$prim48004 = alloca %struct.ScmObj*, align 8
%current_45args47071 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47070)
store volatile %struct.ScmObj* %current_45args47071, %struct.ScmObj** %stackaddr$prim48004, align 8
%stackaddr$prim48005 = alloca %struct.ScmObj*, align 8
%_37fourth40159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47071)
store volatile %struct.ScmObj* %_37fourth40159, %struct.ScmObj** %stackaddr$prim48005, align 8
%stackaddr$makeclosure48006 = alloca %struct.ScmObj*, align 8
%fptrToInt48007 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43947 to i64
%ae43947 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48007)
store volatile %struct.ScmObj* %ae43947, %struct.ScmObj** %stackaddr$makeclosure48006, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43947, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43947, %struct.ScmObj* %_37map140154, i64 1)
%ae43948 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48008 = alloca %struct.ScmObj*, align 8
%fptrToInt48009 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43949 to i64
%ae43949 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48009)
store volatile %struct.ScmObj* %ae43949, %struct.ScmObj** %stackaddr$makeclosure48008, align 8
%args47177$ae43947$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48010 = alloca %struct.ScmObj*, align 8
%args47177$ae43947$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43949, %struct.ScmObj* %args47177$ae43947$0)
store volatile %struct.ScmObj* %args47177$ae43947$1, %struct.ScmObj** %stackaddr$prim48010, align 8
%stackaddr$prim48011 = alloca %struct.ScmObj*, align 8
%args47177$ae43947$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43948, %struct.ScmObj* %args47177$ae43947$1)
store volatile %struct.ScmObj* %args47177$ae43947$2, %struct.ScmObj** %stackaddr$prim48011, align 8
%clofunc48012 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43947)
musttail call tailcc void %clofunc48012(%struct.ScmObj* %ae43947, %struct.ScmObj* %args47177$ae43947$2)
ret void
}

define tailcc void @proc_clo$ae43947(%struct.ScmObj* %env$ae43947,%struct.ScmObj* %current_45args47073) {
%stackaddr$env-ref48013 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43947, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48013
%stackaddr$env-ref48014 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43947, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48014
%stackaddr$prim48015 = alloca %struct.ScmObj*, align 8
%_95k40398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47073)
store volatile %struct.ScmObj* %_95k40398, %struct.ScmObj** %stackaddr$prim48015, align 8
%stackaddr$prim48016 = alloca %struct.ScmObj*, align 8
%current_45args47074 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47073)
store volatile %struct.ScmObj* %current_45args47074, %struct.ScmObj** %stackaddr$prim48016, align 8
%stackaddr$prim48017 = alloca %struct.ScmObj*, align 8
%promise_6340220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47074)
store volatile %struct.ScmObj* %promise_6340220, %struct.ScmObj** %stackaddr$prim48017, align 8
%stackaddr$makeclosure48018 = alloca %struct.ScmObj*, align 8
%fptrToInt48019 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44034 to i64
%ae44034 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48019)
store volatile %struct.ScmObj* %ae44034, %struct.ScmObj** %stackaddr$makeclosure48018, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44034, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44034, %struct.ScmObj* %_37map140154, i64 1)
%ae44035 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48020 = alloca %struct.ScmObj*, align 8
%fptrToInt48021 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44036 to i64
%ae44036 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48021)
store volatile %struct.ScmObj* %ae44036, %struct.ScmObj** %stackaddr$makeclosure48020, align 8
%args47170$ae44034$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48022 = alloca %struct.ScmObj*, align 8
%args47170$ae44034$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44036, %struct.ScmObj* %args47170$ae44034$0)
store volatile %struct.ScmObj* %args47170$ae44034$1, %struct.ScmObj** %stackaddr$prim48022, align 8
%stackaddr$prim48023 = alloca %struct.ScmObj*, align 8
%args47170$ae44034$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44035, %struct.ScmObj* %args47170$ae44034$1)
store volatile %struct.ScmObj* %args47170$ae44034$2, %struct.ScmObj** %stackaddr$prim48023, align 8
%clofunc48024 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44034)
musttail call tailcc void %clofunc48024(%struct.ScmObj* %ae44034, %struct.ScmObj* %args47170$ae44034$2)
ret void
}

define tailcc void @proc_clo$ae44034(%struct.ScmObj* %env$ae44034,%struct.ScmObj* %current_45args47076) {
%stackaddr$env-ref48025 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44034, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48025
%stackaddr$env-ref48026 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44034, i64 1)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48026
%stackaddr$prim48027 = alloca %struct.ScmObj*, align 8
%_95k40399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47076)
store volatile %struct.ScmObj* %_95k40399, %struct.ScmObj** %stackaddr$prim48027, align 8
%stackaddr$prim48028 = alloca %struct.ScmObj*, align 8
%current_45args47077 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47076)
store volatile %struct.ScmObj* %current_45args47077, %struct.ScmObj** %stackaddr$prim48028, align 8
%stackaddr$prim48029 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47077)
store volatile %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$prim48029, align 8
%stackaddr$makeclosure48030 = alloca %struct.ScmObj*, align 8
%fptrToInt48031 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44058 to i64
%ae44058 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48031)
store volatile %struct.ScmObj* %ae44058, %struct.ScmObj** %stackaddr$makeclosure48030, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44058, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44058, %struct.ScmObj* %anf_45bind40347, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44058, %struct.ScmObj* %_37map140154, i64 2)
%ae44059 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48032 = alloca %struct.ScmObj*, align 8
%fptrToInt48033 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44060 to i64
%ae44060 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48033)
store volatile %struct.ScmObj* %ae44060, %struct.ScmObj** %stackaddr$makeclosure48032, align 8
%args47168$ae44058$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48034 = alloca %struct.ScmObj*, align 8
%args47168$ae44058$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44060, %struct.ScmObj* %args47168$ae44058$0)
store volatile %struct.ScmObj* %args47168$ae44058$1, %struct.ScmObj** %stackaddr$prim48034, align 8
%stackaddr$prim48035 = alloca %struct.ScmObj*, align 8
%args47168$ae44058$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44059, %struct.ScmObj* %args47168$ae44058$1)
store volatile %struct.ScmObj* %args47168$ae44058$2, %struct.ScmObj** %stackaddr$prim48035, align 8
%clofunc48036 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44058)
musttail call tailcc void %clofunc48036(%struct.ScmObj* %ae44058, %struct.ScmObj* %args47168$ae44058$2)
ret void
}

define tailcc void @proc_clo$ae44058(%struct.ScmObj* %env$ae44058,%struct.ScmObj* %current_45args47079) {
%stackaddr$env-ref48037 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44058, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48037
%stackaddr$env-ref48038 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44058, i64 1)
store %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$env-ref48038
%stackaddr$env-ref48039 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44058, i64 2)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48039
%stackaddr$prim48040 = alloca %struct.ScmObj*, align 8
%_95k40400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47079)
store volatile %struct.ScmObj* %_95k40400, %struct.ScmObj** %stackaddr$prim48040, align 8
%stackaddr$prim48041 = alloca %struct.ScmObj*, align 8
%current_45args47080 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47079)
store volatile %struct.ScmObj* %current_45args47080, %struct.ScmObj** %stackaddr$prim48041, align 8
%stackaddr$prim48042 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47080)
store volatile %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$prim48042, align 8
%stackaddr$makeclosure48043 = alloca %struct.ScmObj*, align 8
%fptrToInt48044 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44093 to i64
%ae44093 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48044)
store volatile %struct.ScmObj* %ae44093, %struct.ScmObj** %stackaddr$makeclosure48043, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44093, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44093, %struct.ScmObj* %anf_45bind40347, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44093, %struct.ScmObj* %anf_45bind40348, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44093, %struct.ScmObj* %_37map140154, i64 3)
%ae44094 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48045 = alloca %struct.ScmObj*, align 8
%fptrToInt48046 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44095 to i64
%ae44095 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48046)
store volatile %struct.ScmObj* %ae44095, %struct.ScmObj** %stackaddr$makeclosure48045, align 8
%args47162$ae44093$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48047 = alloca %struct.ScmObj*, align 8
%args47162$ae44093$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44095, %struct.ScmObj* %args47162$ae44093$0)
store volatile %struct.ScmObj* %args47162$ae44093$1, %struct.ScmObj** %stackaddr$prim48047, align 8
%stackaddr$prim48048 = alloca %struct.ScmObj*, align 8
%args47162$ae44093$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44094, %struct.ScmObj* %args47162$ae44093$1)
store volatile %struct.ScmObj* %args47162$ae44093$2, %struct.ScmObj** %stackaddr$prim48048, align 8
%clofunc48049 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44093)
musttail call tailcc void %clofunc48049(%struct.ScmObj* %ae44093, %struct.ScmObj* %args47162$ae44093$2)
ret void
}

define tailcc void @proc_clo$ae44093(%struct.ScmObj* %env$ae44093,%struct.ScmObj* %current_45args47082) {
%stackaddr$env-ref48050 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44093, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48050
%stackaddr$env-ref48051 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44093, i64 1)
store %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$env-ref48051
%stackaddr$env-ref48052 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44093, i64 2)
store %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$env-ref48052
%stackaddr$env-ref48053 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44093, i64 3)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48053
%stackaddr$prim48054 = alloca %struct.ScmObj*, align 8
%_95k40401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47082)
store volatile %struct.ScmObj* %_95k40401, %struct.ScmObj** %stackaddr$prim48054, align 8
%stackaddr$prim48055 = alloca %struct.ScmObj*, align 8
%current_45args47083 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47082)
store volatile %struct.ScmObj* %current_45args47083, %struct.ScmObj** %stackaddr$prim48055, align 8
%stackaddr$prim48056 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47083)
store volatile %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$prim48056, align 8
%stackaddr$makeclosure48057 = alloca %struct.ScmObj*, align 8
%fptrToInt48058 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44117 to i64
%ae44117 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt48058)
store volatile %struct.ScmObj* %ae44117, %struct.ScmObj** %stackaddr$makeclosure48057, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44117, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44117, %struct.ScmObj* %anf_45bind40347, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44117, %struct.ScmObj* %anf_45bind40349, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44117, %struct.ScmObj* %anf_45bind40348, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44117, %struct.ScmObj* %_37map140154, i64 4)
%ae44118 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48059 = alloca %struct.ScmObj*, align 8
%fptrToInt48060 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44119 to i64
%ae44119 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48060)
store volatile %struct.ScmObj* %ae44119, %struct.ScmObj** %stackaddr$makeclosure48059, align 8
%args47160$ae44117$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48061 = alloca %struct.ScmObj*, align 8
%args47160$ae44117$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44119, %struct.ScmObj* %args47160$ae44117$0)
store volatile %struct.ScmObj* %args47160$ae44117$1, %struct.ScmObj** %stackaddr$prim48061, align 8
%stackaddr$prim48062 = alloca %struct.ScmObj*, align 8
%args47160$ae44117$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44118, %struct.ScmObj* %args47160$ae44117$1)
store volatile %struct.ScmObj* %args47160$ae44117$2, %struct.ScmObj** %stackaddr$prim48062, align 8
%clofunc48063 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44117)
musttail call tailcc void %clofunc48063(%struct.ScmObj* %ae44117, %struct.ScmObj* %args47160$ae44117$2)
ret void
}

define tailcc void @proc_clo$ae44117(%struct.ScmObj* %env$ae44117,%struct.ScmObj* %current_45args47085) {
%stackaddr$env-ref48064 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44117, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48064
%stackaddr$env-ref48065 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44117, i64 1)
store %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$env-ref48065
%stackaddr$env-ref48066 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44117, i64 2)
store %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$env-ref48066
%stackaddr$env-ref48067 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44117, i64 3)
store %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$env-ref48067
%stackaddr$env-ref48068 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44117, i64 4)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48068
%stackaddr$prim48069 = alloca %struct.ScmObj*, align 8
%_95k40402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47085)
store volatile %struct.ScmObj* %_95k40402, %struct.ScmObj** %stackaddr$prim48069, align 8
%stackaddr$prim48070 = alloca %struct.ScmObj*, align 8
%current_45args47086 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47085)
store volatile %struct.ScmObj* %current_45args47086, %struct.ScmObj** %stackaddr$prim48070, align 8
%stackaddr$prim48071 = alloca %struct.ScmObj*, align 8
%anf_45bind40350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47086)
store volatile %struct.ScmObj* %anf_45bind40350, %struct.ScmObj** %stackaddr$prim48071, align 8
%stackaddr$makeclosure48072 = alloca %struct.ScmObj*, align 8
%fptrToInt48073 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44139 to i64
%ae44139 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt48073)
store volatile %struct.ScmObj* %ae44139, %struct.ScmObj** %stackaddr$makeclosure48072, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44139, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44139, %struct.ScmObj* %anf_45bind40347, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44139, %struct.ScmObj* %anf_45bind40350, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44139, %struct.ScmObj* %anf_45bind40349, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44139, %struct.ScmObj* %anf_45bind40348, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44139, %struct.ScmObj* %_37map140154, i64 5)
%ae44140 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48074 = alloca %struct.ScmObj*, align 8
%fptrToInt48075 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44141 to i64
%ae44141 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48075)
store volatile %struct.ScmObj* %ae44141, %struct.ScmObj** %stackaddr$makeclosure48074, align 8
%args47158$ae44139$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48076 = alloca %struct.ScmObj*, align 8
%args47158$ae44139$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44141, %struct.ScmObj* %args47158$ae44139$0)
store volatile %struct.ScmObj* %args47158$ae44139$1, %struct.ScmObj** %stackaddr$prim48076, align 8
%stackaddr$prim48077 = alloca %struct.ScmObj*, align 8
%args47158$ae44139$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44140, %struct.ScmObj* %args47158$ae44139$1)
store volatile %struct.ScmObj* %args47158$ae44139$2, %struct.ScmObj** %stackaddr$prim48077, align 8
%clofunc48078 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44139)
musttail call tailcc void %clofunc48078(%struct.ScmObj* %ae44139, %struct.ScmObj* %args47158$ae44139$2)
ret void
}

define tailcc void @proc_clo$ae44139(%struct.ScmObj* %env$ae44139,%struct.ScmObj* %current_45args47088) {
%stackaddr$env-ref48079 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44139, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48079
%stackaddr$env-ref48080 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44139, i64 1)
store %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$env-ref48080
%stackaddr$env-ref48081 = alloca %struct.ScmObj*, align 8
%anf_45bind40350 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44139, i64 2)
store %struct.ScmObj* %anf_45bind40350, %struct.ScmObj** %stackaddr$env-ref48081
%stackaddr$env-ref48082 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44139, i64 3)
store %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$env-ref48082
%stackaddr$env-ref48083 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44139, i64 4)
store %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$env-ref48083
%stackaddr$env-ref48084 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44139, i64 5)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48084
%stackaddr$prim48085 = alloca %struct.ScmObj*, align 8
%_95k40403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47088)
store volatile %struct.ScmObj* %_95k40403, %struct.ScmObj** %stackaddr$prim48085, align 8
%stackaddr$prim48086 = alloca %struct.ScmObj*, align 8
%current_45args47089 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47088)
store volatile %struct.ScmObj* %current_45args47089, %struct.ScmObj** %stackaddr$prim48086, align 8
%stackaddr$prim48087 = alloca %struct.ScmObj*, align 8
%anf_45bind40351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47089)
store volatile %struct.ScmObj* %anf_45bind40351, %struct.ScmObj** %stackaddr$prim48087, align 8
%stackaddr$makeclosure48088 = alloca %struct.ScmObj*, align 8
%fptrToInt48089 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44162 to i64
%ae44162 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt48089)
store volatile %struct.ScmObj* %ae44162, %struct.ScmObj** %stackaddr$makeclosure48088, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44162, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44162, %struct.ScmObj* %anf_45bind40347, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44162, %struct.ScmObj* %anf_45bind40350, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44162, %struct.ScmObj* %anf_45bind40349, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44162, %struct.ScmObj* %anf_45bind40348, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44162, %struct.ScmObj* %_37map140154, i64 5)
%args47156$anf_45bind40351$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48090 = alloca %struct.ScmObj*, align 8
%args47156$anf_45bind40351$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44162, %struct.ScmObj* %args47156$anf_45bind40351$0)
store volatile %struct.ScmObj* %args47156$anf_45bind40351$1, %struct.ScmObj** %stackaddr$prim48090, align 8
%clofunc48091 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40351)
musttail call tailcc void %clofunc48091(%struct.ScmObj* %anf_45bind40351, %struct.ScmObj* %args47156$anf_45bind40351$1)
ret void
}

define tailcc void @proc_clo$ae44162(%struct.ScmObj* %env$ae44162,%struct.ScmObj* %current_45args47091) {
%stackaddr$env-ref48092 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44162, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48092
%stackaddr$env-ref48093 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44162, i64 1)
store %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$env-ref48093
%stackaddr$env-ref48094 = alloca %struct.ScmObj*, align 8
%anf_45bind40350 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44162, i64 2)
store %struct.ScmObj* %anf_45bind40350, %struct.ScmObj** %stackaddr$env-ref48094
%stackaddr$env-ref48095 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44162, i64 3)
store %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$env-ref48095
%stackaddr$env-ref48096 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44162, i64 4)
store %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$env-ref48096
%stackaddr$env-ref48097 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44162, i64 5)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48097
%stackaddr$prim48098 = alloca %struct.ScmObj*, align 8
%_95k40404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47091)
store volatile %struct.ScmObj* %_95k40404, %struct.ScmObj** %stackaddr$prim48098, align 8
%stackaddr$prim48099 = alloca %struct.ScmObj*, align 8
%current_45args47092 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47091)
store volatile %struct.ScmObj* %current_45args47092, %struct.ScmObj** %stackaddr$prim48099, align 8
%stackaddr$prim48100 = alloca %struct.ScmObj*, align 8
%anf_45bind40352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47092)
store volatile %struct.ScmObj* %anf_45bind40352, %struct.ScmObj** %stackaddr$prim48100, align 8
%stackaddr$makeclosure48101 = alloca %struct.ScmObj*, align 8
%fptrToInt48102 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44163 to i64
%ae44163 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48102)
store volatile %struct.ScmObj* %ae44163, %struct.ScmObj** %stackaddr$makeclosure48101, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44163, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44163, %struct.ScmObj* %anf_45bind40347, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44163, %struct.ScmObj* %anf_45bind40352, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44163, %struct.ScmObj* %anf_45bind40350, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44163, %struct.ScmObj* %anf_45bind40349, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44163, %struct.ScmObj* %anf_45bind40348, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44163, %struct.ScmObj* %_37map140154, i64 6)
%ae44164 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48103 = alloca %struct.ScmObj*, align 8
%fptrToInt48104 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44165 to i64
%ae44165 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48104)
store volatile %struct.ScmObj* %ae44165, %struct.ScmObj** %stackaddr$makeclosure48103, align 8
%args47155$ae44163$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48105 = alloca %struct.ScmObj*, align 8
%args47155$ae44163$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44165, %struct.ScmObj* %args47155$ae44163$0)
store volatile %struct.ScmObj* %args47155$ae44163$1, %struct.ScmObj** %stackaddr$prim48105, align 8
%stackaddr$prim48106 = alloca %struct.ScmObj*, align 8
%args47155$ae44163$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44164, %struct.ScmObj* %args47155$ae44163$1)
store volatile %struct.ScmObj* %args47155$ae44163$2, %struct.ScmObj** %stackaddr$prim48106, align 8
%clofunc48107 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44163)
musttail call tailcc void %clofunc48107(%struct.ScmObj* %ae44163, %struct.ScmObj* %args47155$ae44163$2)
ret void
}

define tailcc void @proc_clo$ae44163(%struct.ScmObj* %env$ae44163,%struct.ScmObj* %current_45args47094) {
%stackaddr$env-ref48108 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44163, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48108
%stackaddr$env-ref48109 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44163, i64 1)
store %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$env-ref48109
%stackaddr$env-ref48110 = alloca %struct.ScmObj*, align 8
%anf_45bind40352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44163, i64 2)
store %struct.ScmObj* %anf_45bind40352, %struct.ScmObj** %stackaddr$env-ref48110
%stackaddr$env-ref48111 = alloca %struct.ScmObj*, align 8
%anf_45bind40350 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44163, i64 3)
store %struct.ScmObj* %anf_45bind40350, %struct.ScmObj** %stackaddr$env-ref48111
%stackaddr$env-ref48112 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44163, i64 4)
store %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$env-ref48112
%stackaddr$env-ref48113 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44163, i64 5)
store %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$env-ref48113
%stackaddr$env-ref48114 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44163, i64 6)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48114
%stackaddr$prim48115 = alloca %struct.ScmObj*, align 8
%_95k40405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47094)
store volatile %struct.ScmObj* %_95k40405, %struct.ScmObj** %stackaddr$prim48115, align 8
%stackaddr$prim48116 = alloca %struct.ScmObj*, align 8
%current_45args47095 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47094)
store volatile %struct.ScmObj* %current_45args47095, %struct.ScmObj** %stackaddr$prim48116, align 8
%stackaddr$prim48117 = alloca %struct.ScmObj*, align 8
%anf_45bind40353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47095)
store volatile %struct.ScmObj* %anf_45bind40353, %struct.ScmObj** %stackaddr$prim48117, align 8
%stackaddr$makeclosure48118 = alloca %struct.ScmObj*, align 8
%fptrToInt48119 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44185 to i64
%ae44185 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt48119)
store volatile %struct.ScmObj* %ae44185, %struct.ScmObj** %stackaddr$makeclosure48118, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44185, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44185, %struct.ScmObj* %anf_45bind40347, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44185, %struct.ScmObj* %anf_45bind40353, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44185, %struct.ScmObj* %anf_45bind40352, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44185, %struct.ScmObj* %anf_45bind40350, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44185, %struct.ScmObj* %anf_45bind40349, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44185, %struct.ScmObj* %anf_45bind40348, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae44185, %struct.ScmObj* %_37map140154, i64 7)
%ae44186 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48120 = alloca %struct.ScmObj*, align 8
%fptrToInt48121 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44187 to i64
%ae44187 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48121)
store volatile %struct.ScmObj* %ae44187, %struct.ScmObj** %stackaddr$makeclosure48120, align 8
%args47153$ae44185$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48122 = alloca %struct.ScmObj*, align 8
%args47153$ae44185$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44187, %struct.ScmObj* %args47153$ae44185$0)
store volatile %struct.ScmObj* %args47153$ae44185$1, %struct.ScmObj** %stackaddr$prim48122, align 8
%stackaddr$prim48123 = alloca %struct.ScmObj*, align 8
%args47153$ae44185$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44186, %struct.ScmObj* %args47153$ae44185$1)
store volatile %struct.ScmObj* %args47153$ae44185$2, %struct.ScmObj** %stackaddr$prim48123, align 8
%clofunc48124 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44185)
musttail call tailcc void %clofunc48124(%struct.ScmObj* %ae44185, %struct.ScmObj* %args47153$ae44185$2)
ret void
}

define tailcc void @proc_clo$ae44185(%struct.ScmObj* %env$ae44185,%struct.ScmObj* %current_45args47097) {
%stackaddr$env-ref48125 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44185, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48125
%stackaddr$env-ref48126 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44185, i64 1)
store %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$env-ref48126
%stackaddr$env-ref48127 = alloca %struct.ScmObj*, align 8
%anf_45bind40353 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44185, i64 2)
store %struct.ScmObj* %anf_45bind40353, %struct.ScmObj** %stackaddr$env-ref48127
%stackaddr$env-ref48128 = alloca %struct.ScmObj*, align 8
%anf_45bind40352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44185, i64 3)
store %struct.ScmObj* %anf_45bind40352, %struct.ScmObj** %stackaddr$env-ref48128
%stackaddr$env-ref48129 = alloca %struct.ScmObj*, align 8
%anf_45bind40350 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44185, i64 4)
store %struct.ScmObj* %anf_45bind40350, %struct.ScmObj** %stackaddr$env-ref48129
%stackaddr$env-ref48130 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44185, i64 5)
store %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$env-ref48130
%stackaddr$env-ref48131 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44185, i64 6)
store %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$env-ref48131
%stackaddr$env-ref48132 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44185, i64 7)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48132
%stackaddr$prim48133 = alloca %struct.ScmObj*, align 8
%_95k40406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47097)
store volatile %struct.ScmObj* %_95k40406, %struct.ScmObj** %stackaddr$prim48133, align 8
%stackaddr$prim48134 = alloca %struct.ScmObj*, align 8
%current_45args47098 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47097)
store volatile %struct.ScmObj* %current_45args47098, %struct.ScmObj** %stackaddr$prim48134, align 8
%stackaddr$prim48135 = alloca %struct.ScmObj*, align 8
%anf_45bind40354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47098)
store volatile %struct.ScmObj* %anf_45bind40354, %struct.ScmObj** %stackaddr$prim48135, align 8
%stackaddr$makeclosure48136 = alloca %struct.ScmObj*, align 8
%fptrToInt48137 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44208 to i64
%ae44208 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt48137)
store volatile %struct.ScmObj* %ae44208, %struct.ScmObj** %stackaddr$makeclosure48136, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44208, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44208, %struct.ScmObj* %anf_45bind40347, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44208, %struct.ScmObj* %anf_45bind40353, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44208, %struct.ScmObj* %anf_45bind40352, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44208, %struct.ScmObj* %anf_45bind40350, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44208, %struct.ScmObj* %anf_45bind40349, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44208, %struct.ScmObj* %anf_45bind40348, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae44208, %struct.ScmObj* %_37map140154, i64 7)
%args47151$anf_45bind40354$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48138 = alloca %struct.ScmObj*, align 8
%args47151$anf_45bind40354$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44208, %struct.ScmObj* %args47151$anf_45bind40354$0)
store volatile %struct.ScmObj* %args47151$anf_45bind40354$1, %struct.ScmObj** %stackaddr$prim48138, align 8
%clofunc48139 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40354)
musttail call tailcc void %clofunc48139(%struct.ScmObj* %anf_45bind40354, %struct.ScmObj* %args47151$anf_45bind40354$1)
ret void
}

define tailcc void @proc_clo$ae44208(%struct.ScmObj* %env$ae44208,%struct.ScmObj* %current_45args47100) {
%stackaddr$env-ref48140 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44208, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48140
%stackaddr$env-ref48141 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44208, i64 1)
store %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$env-ref48141
%stackaddr$env-ref48142 = alloca %struct.ScmObj*, align 8
%anf_45bind40353 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44208, i64 2)
store %struct.ScmObj* %anf_45bind40353, %struct.ScmObj** %stackaddr$env-ref48142
%stackaddr$env-ref48143 = alloca %struct.ScmObj*, align 8
%anf_45bind40352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44208, i64 3)
store %struct.ScmObj* %anf_45bind40352, %struct.ScmObj** %stackaddr$env-ref48143
%stackaddr$env-ref48144 = alloca %struct.ScmObj*, align 8
%anf_45bind40350 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44208, i64 4)
store %struct.ScmObj* %anf_45bind40350, %struct.ScmObj** %stackaddr$env-ref48144
%stackaddr$env-ref48145 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44208, i64 5)
store %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$env-ref48145
%stackaddr$env-ref48146 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44208, i64 6)
store %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$env-ref48146
%stackaddr$env-ref48147 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44208, i64 7)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48147
%stackaddr$prim48148 = alloca %struct.ScmObj*, align 8
%_95k40407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47100)
store volatile %struct.ScmObj* %_95k40407, %struct.ScmObj** %stackaddr$prim48148, align 8
%stackaddr$prim48149 = alloca %struct.ScmObj*, align 8
%current_45args47101 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47100)
store volatile %struct.ScmObj* %current_45args47101, %struct.ScmObj** %stackaddr$prim48149, align 8
%stackaddr$prim48150 = alloca %struct.ScmObj*, align 8
%anf_45bind40355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47101)
store volatile %struct.ScmObj* %anf_45bind40355, %struct.ScmObj** %stackaddr$prim48150, align 8
%stackaddr$makeclosure48151 = alloca %struct.ScmObj*, align 8
%fptrToInt48152 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44209 to i64
%ae44209 = call %struct.ScmObj* @closure_alloc(i64 9, i64 %fptrToInt48152)
store volatile %struct.ScmObj* %ae44209, %struct.ScmObj** %stackaddr$makeclosure48151, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44209, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44209, %struct.ScmObj* %anf_45bind40347, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44209, %struct.ScmObj* %anf_45bind40355, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44209, %struct.ScmObj* %anf_45bind40353, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44209, %struct.ScmObj* %anf_45bind40352, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44209, %struct.ScmObj* %anf_45bind40350, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44209, %struct.ScmObj* %anf_45bind40349, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae44209, %struct.ScmObj* %anf_45bind40348, i64 7)
call void @closure_place_freevar(%struct.ScmObj* %ae44209, %struct.ScmObj* %_37map140154, i64 8)
%ae44210 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48153 = alloca %struct.ScmObj*, align 8
%fptrToInt48154 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44211 to i64
%ae44211 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48154)
store volatile %struct.ScmObj* %ae44211, %struct.ScmObj** %stackaddr$makeclosure48153, align 8
%args47150$ae44209$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48155 = alloca %struct.ScmObj*, align 8
%args47150$ae44209$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44211, %struct.ScmObj* %args47150$ae44209$0)
store volatile %struct.ScmObj* %args47150$ae44209$1, %struct.ScmObj** %stackaddr$prim48155, align 8
%stackaddr$prim48156 = alloca %struct.ScmObj*, align 8
%args47150$ae44209$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44210, %struct.ScmObj* %args47150$ae44209$1)
store volatile %struct.ScmObj* %args47150$ae44209$2, %struct.ScmObj** %stackaddr$prim48156, align 8
%clofunc48157 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44209)
musttail call tailcc void %clofunc48157(%struct.ScmObj* %ae44209, %struct.ScmObj* %args47150$ae44209$2)
ret void
}

define tailcc void @proc_clo$ae44209(%struct.ScmObj* %env$ae44209,%struct.ScmObj* %current_45args47103) {
%stackaddr$env-ref48158 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44209, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48158
%stackaddr$env-ref48159 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44209, i64 1)
store %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$env-ref48159
%stackaddr$env-ref48160 = alloca %struct.ScmObj*, align 8
%anf_45bind40355 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44209, i64 2)
store %struct.ScmObj* %anf_45bind40355, %struct.ScmObj** %stackaddr$env-ref48160
%stackaddr$env-ref48161 = alloca %struct.ScmObj*, align 8
%anf_45bind40353 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44209, i64 3)
store %struct.ScmObj* %anf_45bind40353, %struct.ScmObj** %stackaddr$env-ref48161
%stackaddr$env-ref48162 = alloca %struct.ScmObj*, align 8
%anf_45bind40352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44209, i64 4)
store %struct.ScmObj* %anf_45bind40352, %struct.ScmObj** %stackaddr$env-ref48162
%stackaddr$env-ref48163 = alloca %struct.ScmObj*, align 8
%anf_45bind40350 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44209, i64 5)
store %struct.ScmObj* %anf_45bind40350, %struct.ScmObj** %stackaddr$env-ref48163
%stackaddr$env-ref48164 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44209, i64 6)
store %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$env-ref48164
%stackaddr$env-ref48165 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44209, i64 7)
store %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$env-ref48165
%stackaddr$env-ref48166 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44209, i64 8)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48166
%stackaddr$prim48167 = alloca %struct.ScmObj*, align 8
%_95k40408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47103)
store volatile %struct.ScmObj* %_95k40408, %struct.ScmObj** %stackaddr$prim48167, align 8
%stackaddr$prim48168 = alloca %struct.ScmObj*, align 8
%current_45args47104 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47103)
store volatile %struct.ScmObj* %current_45args47104, %struct.ScmObj** %stackaddr$prim48168, align 8
%stackaddr$prim48169 = alloca %struct.ScmObj*, align 8
%anf_45bind40356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47104)
store volatile %struct.ScmObj* %anf_45bind40356, %struct.ScmObj** %stackaddr$prim48169, align 8
%stackaddr$makeclosure48170 = alloca %struct.ScmObj*, align 8
%fptrToInt48171 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44232 to i64
%ae44232 = call %struct.ScmObj* @closure_alloc(i64 9, i64 %fptrToInt48171)
store volatile %struct.ScmObj* %ae44232, %struct.ScmObj** %stackaddr$makeclosure48170, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44232, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44232, %struct.ScmObj* %anf_45bind40347, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44232, %struct.ScmObj* %anf_45bind40355, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44232, %struct.ScmObj* %anf_45bind40353, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44232, %struct.ScmObj* %anf_45bind40352, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44232, %struct.ScmObj* %anf_45bind40350, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44232, %struct.ScmObj* %anf_45bind40349, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae44232, %struct.ScmObj* %anf_45bind40348, i64 7)
call void @closure_place_freevar(%struct.ScmObj* %ae44232, %struct.ScmObj* %_37map140154, i64 8)
%args47148$anf_45bind40356$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48172 = alloca %struct.ScmObj*, align 8
%args47148$anf_45bind40356$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44232, %struct.ScmObj* %args47148$anf_45bind40356$0)
store volatile %struct.ScmObj* %args47148$anf_45bind40356$1, %struct.ScmObj** %stackaddr$prim48172, align 8
%clofunc48173 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40356)
musttail call tailcc void %clofunc48173(%struct.ScmObj* %anf_45bind40356, %struct.ScmObj* %args47148$anf_45bind40356$1)
ret void
}

define tailcc void @proc_clo$ae44232(%struct.ScmObj* %env$ae44232,%struct.ScmObj* %current_45args47106) {
%stackaddr$env-ref48174 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44232, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48174
%stackaddr$env-ref48175 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44232, i64 1)
store %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$env-ref48175
%stackaddr$env-ref48176 = alloca %struct.ScmObj*, align 8
%anf_45bind40355 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44232, i64 2)
store %struct.ScmObj* %anf_45bind40355, %struct.ScmObj** %stackaddr$env-ref48176
%stackaddr$env-ref48177 = alloca %struct.ScmObj*, align 8
%anf_45bind40353 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44232, i64 3)
store %struct.ScmObj* %anf_45bind40353, %struct.ScmObj** %stackaddr$env-ref48177
%stackaddr$env-ref48178 = alloca %struct.ScmObj*, align 8
%anf_45bind40352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44232, i64 4)
store %struct.ScmObj* %anf_45bind40352, %struct.ScmObj** %stackaddr$env-ref48178
%stackaddr$env-ref48179 = alloca %struct.ScmObj*, align 8
%anf_45bind40350 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44232, i64 5)
store %struct.ScmObj* %anf_45bind40350, %struct.ScmObj** %stackaddr$env-ref48179
%stackaddr$env-ref48180 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44232, i64 6)
store %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$env-ref48180
%stackaddr$env-ref48181 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44232, i64 7)
store %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$env-ref48181
%stackaddr$env-ref48182 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44232, i64 8)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48182
%stackaddr$prim48183 = alloca %struct.ScmObj*, align 8
%_95k40409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47106)
store volatile %struct.ScmObj* %_95k40409, %struct.ScmObj** %stackaddr$prim48183, align 8
%stackaddr$prim48184 = alloca %struct.ScmObj*, align 8
%current_45args47107 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47106)
store volatile %struct.ScmObj* %current_45args47107, %struct.ScmObj** %stackaddr$prim48184, align 8
%stackaddr$prim48185 = alloca %struct.ScmObj*, align 8
%anf_45bind40357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47107)
store volatile %struct.ScmObj* %anf_45bind40357, %struct.ScmObj** %stackaddr$prim48185, align 8
%stackaddr$makeclosure48186 = alloca %struct.ScmObj*, align 8
%fptrToInt48187 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44234 to i64
%ae44234 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48187)
store volatile %struct.ScmObj* %ae44234, %struct.ScmObj** %stackaddr$makeclosure48186, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44234, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44234, %struct.ScmObj* %anf_45bind40347, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44234, %struct.ScmObj* %anf_45bind40352, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44234, %struct.ScmObj* %anf_45bind40350, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44234, %struct.ScmObj* %anf_45bind40349, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44234, %struct.ScmObj* %anf_45bind40348, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44234, %struct.ScmObj* %_37map140154, i64 6)
%args47147$anf_45bind40353$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48188 = alloca %struct.ScmObj*, align 8
%args47147$anf_45bind40353$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40357, %struct.ScmObj* %args47147$anf_45bind40353$0)
store volatile %struct.ScmObj* %args47147$anf_45bind40353$1, %struct.ScmObj** %stackaddr$prim48188, align 8
%stackaddr$prim48189 = alloca %struct.ScmObj*, align 8
%args47147$anf_45bind40353$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40355, %struct.ScmObj* %args47147$anf_45bind40353$1)
store volatile %struct.ScmObj* %args47147$anf_45bind40353$2, %struct.ScmObj** %stackaddr$prim48189, align 8
%stackaddr$prim48190 = alloca %struct.ScmObj*, align 8
%args47147$anf_45bind40353$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44234, %struct.ScmObj* %args47147$anf_45bind40353$2)
store volatile %struct.ScmObj* %args47147$anf_45bind40353$3, %struct.ScmObj** %stackaddr$prim48190, align 8
%clofunc48191 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40353)
musttail call tailcc void %clofunc48191(%struct.ScmObj* %anf_45bind40353, %struct.ScmObj* %args47147$anf_45bind40353$3)
ret void
}

define tailcc void @proc_clo$ae44234(%struct.ScmObj* %env$ae44234,%struct.ScmObj* %current_45args47109) {
%stackaddr$env-ref48192 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44234, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48192
%stackaddr$env-ref48193 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44234, i64 1)
store %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$env-ref48193
%stackaddr$env-ref48194 = alloca %struct.ScmObj*, align 8
%anf_45bind40352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44234, i64 2)
store %struct.ScmObj* %anf_45bind40352, %struct.ScmObj** %stackaddr$env-ref48194
%stackaddr$env-ref48195 = alloca %struct.ScmObj*, align 8
%anf_45bind40350 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44234, i64 3)
store %struct.ScmObj* %anf_45bind40350, %struct.ScmObj** %stackaddr$env-ref48195
%stackaddr$env-ref48196 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44234, i64 4)
store %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$env-ref48196
%stackaddr$env-ref48197 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44234, i64 5)
store %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$env-ref48197
%stackaddr$env-ref48198 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44234, i64 6)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48198
%stackaddr$prim48199 = alloca %struct.ScmObj*, align 8
%_95k40410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47109)
store volatile %struct.ScmObj* %_95k40410, %struct.ScmObj** %stackaddr$prim48199, align 8
%stackaddr$prim48200 = alloca %struct.ScmObj*, align 8
%current_45args47110 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47109)
store volatile %struct.ScmObj* %current_45args47110, %struct.ScmObj** %stackaddr$prim48200, align 8
%stackaddr$prim48201 = alloca %struct.ScmObj*, align 8
%anf_45bind40358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47110)
store volatile %struct.ScmObj* %anf_45bind40358, %struct.ScmObj** %stackaddr$prim48201, align 8
%stackaddr$makeclosure48202 = alloca %struct.ScmObj*, align 8
%fptrToInt48203 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44237 to i64
%ae44237 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt48203)
store volatile %struct.ScmObj* %ae44237, %struct.ScmObj** %stackaddr$makeclosure48202, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44237, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44237, %struct.ScmObj* %anf_45bind40347, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44237, %struct.ScmObj* %anf_45bind40358, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44237, %struct.ScmObj* %anf_45bind40352, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44237, %struct.ScmObj* %anf_45bind40350, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44237, %struct.ScmObj* %anf_45bind40349, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44237, %struct.ScmObj* %anf_45bind40348, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae44237, %struct.ScmObj* %_37map140154, i64 7)
%ae44238 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48204 = alloca %struct.ScmObj*, align 8
%fptrToInt48205 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44239 to i64
%ae44239 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48205)
store volatile %struct.ScmObj* %ae44239, %struct.ScmObj** %stackaddr$makeclosure48204, align 8
%args47146$ae44237$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48206 = alloca %struct.ScmObj*, align 8
%args47146$ae44237$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44239, %struct.ScmObj* %args47146$ae44237$0)
store volatile %struct.ScmObj* %args47146$ae44237$1, %struct.ScmObj** %stackaddr$prim48206, align 8
%stackaddr$prim48207 = alloca %struct.ScmObj*, align 8
%args47146$ae44237$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44238, %struct.ScmObj* %args47146$ae44237$1)
store volatile %struct.ScmObj* %args47146$ae44237$2, %struct.ScmObj** %stackaddr$prim48207, align 8
%clofunc48208 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44237)
musttail call tailcc void %clofunc48208(%struct.ScmObj* %ae44237, %struct.ScmObj* %args47146$ae44237$2)
ret void
}

define tailcc void @proc_clo$ae44237(%struct.ScmObj* %env$ae44237,%struct.ScmObj* %current_45args47112) {
%stackaddr$env-ref48209 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44237, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48209
%stackaddr$env-ref48210 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44237, i64 1)
store %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$env-ref48210
%stackaddr$env-ref48211 = alloca %struct.ScmObj*, align 8
%anf_45bind40358 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44237, i64 2)
store %struct.ScmObj* %anf_45bind40358, %struct.ScmObj** %stackaddr$env-ref48211
%stackaddr$env-ref48212 = alloca %struct.ScmObj*, align 8
%anf_45bind40352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44237, i64 3)
store %struct.ScmObj* %anf_45bind40352, %struct.ScmObj** %stackaddr$env-ref48212
%stackaddr$env-ref48213 = alloca %struct.ScmObj*, align 8
%anf_45bind40350 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44237, i64 4)
store %struct.ScmObj* %anf_45bind40350, %struct.ScmObj** %stackaddr$env-ref48213
%stackaddr$env-ref48214 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44237, i64 5)
store %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$env-ref48214
%stackaddr$env-ref48215 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44237, i64 6)
store %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$env-ref48215
%stackaddr$env-ref48216 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44237, i64 7)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48216
%stackaddr$prim48217 = alloca %struct.ScmObj*, align 8
%_95k40411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47112)
store volatile %struct.ScmObj* %_95k40411, %struct.ScmObj** %stackaddr$prim48217, align 8
%stackaddr$prim48218 = alloca %struct.ScmObj*, align 8
%current_45args47113 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47112)
store volatile %struct.ScmObj* %current_45args47113, %struct.ScmObj** %stackaddr$prim48218, align 8
%stackaddr$prim48219 = alloca %struct.ScmObj*, align 8
%anf_45bind40359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47113)
store volatile %struct.ScmObj* %anf_45bind40359, %struct.ScmObj** %stackaddr$prim48219, align 8
%stackaddr$makeclosure48220 = alloca %struct.ScmObj*, align 8
%fptrToInt48221 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44262 to i64
%ae44262 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt48221)
store volatile %struct.ScmObj* %ae44262, %struct.ScmObj** %stackaddr$makeclosure48220, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44262, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44262, %struct.ScmObj* %anf_45bind40347, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44262, %struct.ScmObj* %anf_45bind40358, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44262, %struct.ScmObj* %anf_45bind40352, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44262, %struct.ScmObj* %anf_45bind40350, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44262, %struct.ScmObj* %anf_45bind40349, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44262, %struct.ScmObj* %anf_45bind40348, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae44262, %struct.ScmObj* %_37map140154, i64 7)
%args47144$anf_45bind40359$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48222 = alloca %struct.ScmObj*, align 8
%args47144$anf_45bind40359$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44262, %struct.ScmObj* %args47144$anf_45bind40359$0)
store volatile %struct.ScmObj* %args47144$anf_45bind40359$1, %struct.ScmObj** %stackaddr$prim48222, align 8
%clofunc48223 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40359)
musttail call tailcc void %clofunc48223(%struct.ScmObj* %anf_45bind40359, %struct.ScmObj* %args47144$anf_45bind40359$1)
ret void
}

define tailcc void @proc_clo$ae44262(%struct.ScmObj* %env$ae44262,%struct.ScmObj* %current_45args47115) {
%stackaddr$env-ref48224 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44262, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48224
%stackaddr$env-ref48225 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44262, i64 1)
store %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$env-ref48225
%stackaddr$env-ref48226 = alloca %struct.ScmObj*, align 8
%anf_45bind40358 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44262, i64 2)
store %struct.ScmObj* %anf_45bind40358, %struct.ScmObj** %stackaddr$env-ref48226
%stackaddr$env-ref48227 = alloca %struct.ScmObj*, align 8
%anf_45bind40352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44262, i64 3)
store %struct.ScmObj* %anf_45bind40352, %struct.ScmObj** %stackaddr$env-ref48227
%stackaddr$env-ref48228 = alloca %struct.ScmObj*, align 8
%anf_45bind40350 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44262, i64 4)
store %struct.ScmObj* %anf_45bind40350, %struct.ScmObj** %stackaddr$env-ref48228
%stackaddr$env-ref48229 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44262, i64 5)
store %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$env-ref48229
%stackaddr$env-ref48230 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44262, i64 6)
store %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$env-ref48230
%stackaddr$env-ref48231 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44262, i64 7)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48231
%stackaddr$prim48232 = alloca %struct.ScmObj*, align 8
%_95k40412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47115)
store volatile %struct.ScmObj* %_95k40412, %struct.ScmObj** %stackaddr$prim48232, align 8
%stackaddr$prim48233 = alloca %struct.ScmObj*, align 8
%current_45args47116 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47115)
store volatile %struct.ScmObj* %current_45args47116, %struct.ScmObj** %stackaddr$prim48233, align 8
%stackaddr$prim48234 = alloca %struct.ScmObj*, align 8
%anf_45bind40360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47116)
store volatile %struct.ScmObj* %anf_45bind40360, %struct.ScmObj** %stackaddr$prim48234, align 8
%stackaddr$makeclosure48235 = alloca %struct.ScmObj*, align 8
%fptrToInt48236 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44263 to i64
%ae44263 = call %struct.ScmObj* @closure_alloc(i64 9, i64 %fptrToInt48236)
store volatile %struct.ScmObj* %ae44263, %struct.ScmObj** %stackaddr$makeclosure48235, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44263, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44263, %struct.ScmObj* %anf_45bind40347, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44263, %struct.ScmObj* %anf_45bind40360, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44263, %struct.ScmObj* %anf_45bind40358, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44263, %struct.ScmObj* %anf_45bind40352, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44263, %struct.ScmObj* %anf_45bind40350, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44263, %struct.ScmObj* %anf_45bind40349, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae44263, %struct.ScmObj* %anf_45bind40348, i64 7)
call void @closure_place_freevar(%struct.ScmObj* %ae44263, %struct.ScmObj* %_37map140154, i64 8)
%ae44264 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48237 = alloca %struct.ScmObj*, align 8
%fptrToInt48238 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44265 to i64
%ae44265 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48238)
store volatile %struct.ScmObj* %ae44265, %struct.ScmObj** %stackaddr$makeclosure48237, align 8
%args47143$ae44263$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48239 = alloca %struct.ScmObj*, align 8
%args47143$ae44263$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44265, %struct.ScmObj* %args47143$ae44263$0)
store volatile %struct.ScmObj* %args47143$ae44263$1, %struct.ScmObj** %stackaddr$prim48239, align 8
%stackaddr$prim48240 = alloca %struct.ScmObj*, align 8
%args47143$ae44263$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44264, %struct.ScmObj* %args47143$ae44263$1)
store volatile %struct.ScmObj* %args47143$ae44263$2, %struct.ScmObj** %stackaddr$prim48240, align 8
%clofunc48241 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae44263)
musttail call tailcc void %clofunc48241(%struct.ScmObj* %ae44263, %struct.ScmObj* %args47143$ae44263$2)
ret void
}

define tailcc void @proc_clo$ae44263(%struct.ScmObj* %env$ae44263,%struct.ScmObj* %current_45args47118) {
%stackaddr$env-ref48242 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44263, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48242
%stackaddr$env-ref48243 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44263, i64 1)
store %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$env-ref48243
%stackaddr$env-ref48244 = alloca %struct.ScmObj*, align 8
%anf_45bind40360 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44263, i64 2)
store %struct.ScmObj* %anf_45bind40360, %struct.ScmObj** %stackaddr$env-ref48244
%stackaddr$env-ref48245 = alloca %struct.ScmObj*, align 8
%anf_45bind40358 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44263, i64 3)
store %struct.ScmObj* %anf_45bind40358, %struct.ScmObj** %stackaddr$env-ref48245
%stackaddr$env-ref48246 = alloca %struct.ScmObj*, align 8
%anf_45bind40352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44263, i64 4)
store %struct.ScmObj* %anf_45bind40352, %struct.ScmObj** %stackaddr$env-ref48246
%stackaddr$env-ref48247 = alloca %struct.ScmObj*, align 8
%anf_45bind40350 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44263, i64 5)
store %struct.ScmObj* %anf_45bind40350, %struct.ScmObj** %stackaddr$env-ref48247
%stackaddr$env-ref48248 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44263, i64 6)
store %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$env-ref48248
%stackaddr$env-ref48249 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44263, i64 7)
store %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$env-ref48249
%stackaddr$env-ref48250 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44263, i64 8)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48250
%stackaddr$prim48251 = alloca %struct.ScmObj*, align 8
%_95k40413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47118)
store volatile %struct.ScmObj* %_95k40413, %struct.ScmObj** %stackaddr$prim48251, align 8
%stackaddr$prim48252 = alloca %struct.ScmObj*, align 8
%current_45args47119 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47118)
store volatile %struct.ScmObj* %current_45args47119, %struct.ScmObj** %stackaddr$prim48252, align 8
%stackaddr$prim48253 = alloca %struct.ScmObj*, align 8
%anf_45bind40361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47119)
store volatile %struct.ScmObj* %anf_45bind40361, %struct.ScmObj** %stackaddr$prim48253, align 8
%stackaddr$makeclosure48254 = alloca %struct.ScmObj*, align 8
%fptrToInt48255 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44288 to i64
%ae44288 = call %struct.ScmObj* @closure_alloc(i64 9, i64 %fptrToInt48255)
store volatile %struct.ScmObj* %ae44288, %struct.ScmObj** %stackaddr$makeclosure48254, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44288, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44288, %struct.ScmObj* %anf_45bind40347, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44288, %struct.ScmObj* %anf_45bind40360, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44288, %struct.ScmObj* %anf_45bind40358, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44288, %struct.ScmObj* %anf_45bind40352, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae44288, %struct.ScmObj* %anf_45bind40350, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae44288, %struct.ScmObj* %anf_45bind40349, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae44288, %struct.ScmObj* %anf_45bind40348, i64 7)
call void @closure_place_freevar(%struct.ScmObj* %ae44288, %struct.ScmObj* %_37map140154, i64 8)
%ae44289 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44290 = call %struct.ScmObj* @const_init_int(i64 1)
%args47141$anf_45bind40361$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48256 = alloca %struct.ScmObj*, align 8
%args47141$anf_45bind40361$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44290, %struct.ScmObj* %args47141$anf_45bind40361$0)
store volatile %struct.ScmObj* %args47141$anf_45bind40361$1, %struct.ScmObj** %stackaddr$prim48256, align 8
%stackaddr$prim48257 = alloca %struct.ScmObj*, align 8
%args47141$anf_45bind40361$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44289, %struct.ScmObj* %args47141$anf_45bind40361$1)
store volatile %struct.ScmObj* %args47141$anf_45bind40361$2, %struct.ScmObj** %stackaddr$prim48257, align 8
%stackaddr$prim48258 = alloca %struct.ScmObj*, align 8
%args47141$anf_45bind40361$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44288, %struct.ScmObj* %args47141$anf_45bind40361$2)
store volatile %struct.ScmObj* %args47141$anf_45bind40361$3, %struct.ScmObj** %stackaddr$prim48258, align 8
%clofunc48259 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40361)
musttail call tailcc void %clofunc48259(%struct.ScmObj* %anf_45bind40361, %struct.ScmObj* %args47141$anf_45bind40361$3)
ret void
}

define tailcc void @proc_clo$ae44288(%struct.ScmObj* %env$ae44288,%struct.ScmObj* %current_45args47121) {
%stackaddr$env-ref48260 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44288, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48260
%stackaddr$env-ref48261 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44288, i64 1)
store %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$env-ref48261
%stackaddr$env-ref48262 = alloca %struct.ScmObj*, align 8
%anf_45bind40360 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44288, i64 2)
store %struct.ScmObj* %anf_45bind40360, %struct.ScmObj** %stackaddr$env-ref48262
%stackaddr$env-ref48263 = alloca %struct.ScmObj*, align 8
%anf_45bind40358 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44288, i64 3)
store %struct.ScmObj* %anf_45bind40358, %struct.ScmObj** %stackaddr$env-ref48263
%stackaddr$env-ref48264 = alloca %struct.ScmObj*, align 8
%anf_45bind40352 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44288, i64 4)
store %struct.ScmObj* %anf_45bind40352, %struct.ScmObj** %stackaddr$env-ref48264
%stackaddr$env-ref48265 = alloca %struct.ScmObj*, align 8
%anf_45bind40350 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44288, i64 5)
store %struct.ScmObj* %anf_45bind40350, %struct.ScmObj** %stackaddr$env-ref48265
%stackaddr$env-ref48266 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44288, i64 6)
store %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$env-ref48266
%stackaddr$env-ref48267 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44288, i64 7)
store %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$env-ref48267
%stackaddr$env-ref48268 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44288, i64 8)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48268
%stackaddr$prim48269 = alloca %struct.ScmObj*, align 8
%_95k40414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47121)
store volatile %struct.ScmObj* %_95k40414, %struct.ScmObj** %stackaddr$prim48269, align 8
%stackaddr$prim48270 = alloca %struct.ScmObj*, align 8
%current_45args47122 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47121)
store volatile %struct.ScmObj* %current_45args47122, %struct.ScmObj** %stackaddr$prim48270, align 8
%stackaddr$prim48271 = alloca %struct.ScmObj*, align 8
%anf_45bind40362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47122)
store volatile %struct.ScmObj* %anf_45bind40362, %struct.ScmObj** %stackaddr$prim48271, align 8
%stackaddr$makeclosure48272 = alloca %struct.ScmObj*, align 8
%fptrToInt48273 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44300 to i64
%ae44300 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt48273)
store volatile %struct.ScmObj* %ae44300, %struct.ScmObj** %stackaddr$makeclosure48272, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44300, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44300, %struct.ScmObj* %anf_45bind40347, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44300, %struct.ScmObj* %anf_45bind40349, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44300, %struct.ScmObj* %anf_45bind40348, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae44300, %struct.ScmObj* %_37map140154, i64 4)
%ae44301 = call %struct.ScmObj* @const_init_false()
%ae44302 = call %struct.ScmObj* @const_init_true()
%ae44303 = call %struct.ScmObj* @const_init_int(i64 7)
%ae44308 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @global$sym$ae4430848274, i32 0, i32 0))
%args47140$anf_45bind40350$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48275 = alloca %struct.ScmObj*, align 8
%args47140$anf_45bind40350$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44308, %struct.ScmObj* %args47140$anf_45bind40350$0)
store volatile %struct.ScmObj* %args47140$anf_45bind40350$1, %struct.ScmObj** %stackaddr$prim48275, align 8
%stackaddr$prim48276 = alloca %struct.ScmObj*, align 8
%args47140$anf_45bind40350$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40362, %struct.ScmObj* %args47140$anf_45bind40350$1)
store volatile %struct.ScmObj* %args47140$anf_45bind40350$2, %struct.ScmObj** %stackaddr$prim48276, align 8
%stackaddr$prim48277 = alloca %struct.ScmObj*, align 8
%args47140$anf_45bind40350$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40360, %struct.ScmObj* %args47140$anf_45bind40350$2)
store volatile %struct.ScmObj* %args47140$anf_45bind40350$3, %struct.ScmObj** %stackaddr$prim48277, align 8
%stackaddr$prim48278 = alloca %struct.ScmObj*, align 8
%args47140$anf_45bind40350$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40358, %struct.ScmObj* %args47140$anf_45bind40350$3)
store volatile %struct.ScmObj* %args47140$anf_45bind40350$4, %struct.ScmObj** %stackaddr$prim48278, align 8
%stackaddr$prim48279 = alloca %struct.ScmObj*, align 8
%args47140$anf_45bind40350$5 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40352, %struct.ScmObj* %args47140$anf_45bind40350$4)
store volatile %struct.ScmObj* %args47140$anf_45bind40350$5, %struct.ScmObj** %stackaddr$prim48279, align 8
%stackaddr$prim48280 = alloca %struct.ScmObj*, align 8
%args47140$anf_45bind40350$6 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44303, %struct.ScmObj* %args47140$anf_45bind40350$5)
store volatile %struct.ScmObj* %args47140$anf_45bind40350$6, %struct.ScmObj** %stackaddr$prim48280, align 8
%stackaddr$prim48281 = alloca %struct.ScmObj*, align 8
%args47140$anf_45bind40350$7 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44302, %struct.ScmObj* %args47140$anf_45bind40350$6)
store volatile %struct.ScmObj* %args47140$anf_45bind40350$7, %struct.ScmObj** %stackaddr$prim48281, align 8
%stackaddr$prim48282 = alloca %struct.ScmObj*, align 8
%args47140$anf_45bind40350$8 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44301, %struct.ScmObj* %args47140$anf_45bind40350$7)
store volatile %struct.ScmObj* %args47140$anf_45bind40350$8, %struct.ScmObj** %stackaddr$prim48282, align 8
%stackaddr$prim48283 = alloca %struct.ScmObj*, align 8
%args47140$anf_45bind40350$9 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44300, %struct.ScmObj* %args47140$anf_45bind40350$8)
store volatile %struct.ScmObj* %args47140$anf_45bind40350$9, %struct.ScmObj** %stackaddr$prim48283, align 8
%clofunc48284 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40350)
musttail call tailcc void %clofunc48284(%struct.ScmObj* %anf_45bind40350, %struct.ScmObj* %args47140$anf_45bind40350$9)
ret void
}

define tailcc void @proc_clo$ae44300(%struct.ScmObj* %env$ae44300,%struct.ScmObj* %current_45args47124) {
%stackaddr$env-ref48285 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44300, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48285
%stackaddr$env-ref48286 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44300, i64 1)
store %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$env-ref48286
%stackaddr$env-ref48287 = alloca %struct.ScmObj*, align 8
%anf_45bind40349 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44300, i64 2)
store %struct.ScmObj* %anf_45bind40349, %struct.ScmObj** %stackaddr$env-ref48287
%stackaddr$env-ref48288 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44300, i64 3)
store %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$env-ref48288
%stackaddr$env-ref48289 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44300, i64 4)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48289
%stackaddr$prim48290 = alloca %struct.ScmObj*, align 8
%_95k40415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47124)
store volatile %struct.ScmObj* %_95k40415, %struct.ScmObj** %stackaddr$prim48290, align 8
%stackaddr$prim48291 = alloca %struct.ScmObj*, align 8
%current_45args47125 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47124)
store volatile %struct.ScmObj* %current_45args47125, %struct.ScmObj** %stackaddr$prim48291, align 8
%stackaddr$prim48292 = alloca %struct.ScmObj*, align 8
%anf_45bind40363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47125)
store volatile %struct.ScmObj* %anf_45bind40363, %struct.ScmObj** %stackaddr$prim48292, align 8
%stackaddr$makeclosure48293 = alloca %struct.ScmObj*, align 8
%fptrToInt48294 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44350 to i64
%ae44350 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48294)
store volatile %struct.ScmObj* %ae44350, %struct.ScmObj** %stackaddr$makeclosure48293, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44350, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44350, %struct.ScmObj* %anf_45bind40347, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae44350, %struct.ScmObj* %anf_45bind40348, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae44350, %struct.ScmObj* %_37map140154, i64 3)
%args47139$_37map140154$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48295 = alloca %struct.ScmObj*, align 8
%args47139$_37map140154$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40363, %struct.ScmObj* %args47139$_37map140154$0)
store volatile %struct.ScmObj* %args47139$_37map140154$1, %struct.ScmObj** %stackaddr$prim48295, align 8
%stackaddr$prim48296 = alloca %struct.ScmObj*, align 8
%args47139$_37map140154$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40349, %struct.ScmObj* %args47139$_37map140154$1)
store volatile %struct.ScmObj* %args47139$_37map140154$2, %struct.ScmObj** %stackaddr$prim48296, align 8
%stackaddr$prim48297 = alloca %struct.ScmObj*, align 8
%args47139$_37map140154$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44350, %struct.ScmObj* %args47139$_37map140154$2)
store volatile %struct.ScmObj* %args47139$_37map140154$3, %struct.ScmObj** %stackaddr$prim48297, align 8
%clofunc48298 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140154)
musttail call tailcc void %clofunc48298(%struct.ScmObj* %_37map140154, %struct.ScmObj* %args47139$_37map140154$3)
ret void
}

define tailcc void @proc_clo$ae44350(%struct.ScmObj* %env$ae44350,%struct.ScmObj* %current_45args47127) {
%stackaddr$env-ref48299 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44350, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48299
%stackaddr$env-ref48300 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44350, i64 1)
store %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$env-ref48300
%stackaddr$env-ref48301 = alloca %struct.ScmObj*, align 8
%anf_45bind40348 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44350, i64 2)
store %struct.ScmObj* %anf_45bind40348, %struct.ScmObj** %stackaddr$env-ref48301
%stackaddr$env-ref48302 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44350, i64 3)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48302
%stackaddr$prim48303 = alloca %struct.ScmObj*, align 8
%_95k40416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47127)
store volatile %struct.ScmObj* %_95k40416, %struct.ScmObj** %stackaddr$prim48303, align 8
%stackaddr$prim48304 = alloca %struct.ScmObj*, align 8
%current_45args47128 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47127)
store volatile %struct.ScmObj* %current_45args47128, %struct.ScmObj** %stackaddr$prim48304, align 8
%stackaddr$prim48305 = alloca %struct.ScmObj*, align 8
%anf_45bind40364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47128)
store volatile %struct.ScmObj* %anf_45bind40364, %struct.ScmObj** %stackaddr$prim48305, align 8
%stackaddr$makeclosure48306 = alloca %struct.ScmObj*, align 8
%fptrToInt48307 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44354 to i64
%ae44354 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48307)
store volatile %struct.ScmObj* %ae44354, %struct.ScmObj** %stackaddr$makeclosure48306, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae44354, %struct.ScmObj* %_37foldl140107, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae44354, %struct.ScmObj* %anf_45bind40347, i64 1)
%args47138$_37map140154$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48308 = alloca %struct.ScmObj*, align 8
%args47138$_37map140154$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40364, %struct.ScmObj* %args47138$_37map140154$0)
store volatile %struct.ScmObj* %args47138$_37map140154$1, %struct.ScmObj** %stackaddr$prim48308, align 8
%stackaddr$prim48309 = alloca %struct.ScmObj*, align 8
%args47138$_37map140154$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40348, %struct.ScmObj* %args47138$_37map140154$1)
store volatile %struct.ScmObj* %args47138$_37map140154$2, %struct.ScmObj** %stackaddr$prim48309, align 8
%stackaddr$prim48310 = alloca %struct.ScmObj*, align 8
%args47138$_37map140154$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44354, %struct.ScmObj* %args47138$_37map140154$2)
store volatile %struct.ScmObj* %args47138$_37map140154$3, %struct.ScmObj** %stackaddr$prim48310, align 8
%clofunc48311 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140154)
musttail call tailcc void %clofunc48311(%struct.ScmObj* %_37map140154, %struct.ScmObj* %args47138$_37map140154$3)
ret void
}

define tailcc void @proc_clo$ae44354(%struct.ScmObj* %env$ae44354,%struct.ScmObj* %current_45args47130) {
%stackaddr$env-ref48312 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44354, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48312
%stackaddr$env-ref48313 = alloca %struct.ScmObj*, align 8
%anf_45bind40347 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae44354, i64 1)
store %struct.ScmObj* %anf_45bind40347, %struct.ScmObj** %stackaddr$env-ref48313
%stackaddr$prim48314 = alloca %struct.ScmObj*, align 8
%_95k40417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47130)
store volatile %struct.ScmObj* %_95k40417, %struct.ScmObj** %stackaddr$prim48314, align 8
%stackaddr$prim48315 = alloca %struct.ScmObj*, align 8
%current_45args47131 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47130)
store volatile %struct.ScmObj* %current_45args47131, %struct.ScmObj** %stackaddr$prim48315, align 8
%stackaddr$prim48316 = alloca %struct.ScmObj*, align 8
%anf_45bind40365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47131)
store volatile %struct.ScmObj* %anf_45bind40365, %struct.ScmObj** %stackaddr$prim48316, align 8
%stackaddr$makeclosure48317 = alloca %struct.ScmObj*, align 8
%fptrToInt48318 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae44358 to i64
%ae44358 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48318)
store volatile %struct.ScmObj* %ae44358, %struct.ScmObj** %stackaddr$makeclosure48317, align 8
%ae44360 = call %struct.ScmObj* @const_init_int(i64 0)
%args47137$_37foldl140107$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48319 = alloca %struct.ScmObj*, align 8
%args47137$_37foldl140107$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40365, %struct.ScmObj* %args47137$_37foldl140107$0)
store volatile %struct.ScmObj* %args47137$_37foldl140107$1, %struct.ScmObj** %stackaddr$prim48319, align 8
%stackaddr$prim48320 = alloca %struct.ScmObj*, align 8
%args47137$_37foldl140107$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44360, %struct.ScmObj* %args47137$_37foldl140107$1)
store volatile %struct.ScmObj* %args47137$_37foldl140107$2, %struct.ScmObj** %stackaddr$prim48320, align 8
%stackaddr$prim48321 = alloca %struct.ScmObj*, align 8
%args47137$_37foldl140107$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40347, %struct.ScmObj* %args47137$_37foldl140107$2)
store volatile %struct.ScmObj* %args47137$_37foldl140107$3, %struct.ScmObj** %stackaddr$prim48321, align 8
%stackaddr$prim48322 = alloca %struct.ScmObj*, align 8
%args47137$_37foldl140107$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44358, %struct.ScmObj* %args47137$_37foldl140107$3)
store volatile %struct.ScmObj* %args47137$_37foldl140107$4, %struct.ScmObj** %stackaddr$prim48322, align 8
%clofunc48323 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140107)
musttail call tailcc void %clofunc48323(%struct.ScmObj* %_37foldl140107, %struct.ScmObj* %args47137$_37foldl140107$4)
ret void
}

define tailcc void @proc_clo$ae44358(%struct.ScmObj* %env$ae44358,%struct.ScmObj* %current_45args47133) {
%stackaddr$prim48324 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47133)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim48324, align 8
%stackaddr$prim48325 = alloca %struct.ScmObj*, align 8
%current_45args47134 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47133)
store volatile %struct.ScmObj* %current_45args47134, %struct.ScmObj** %stackaddr$prim48325, align 8
%stackaddr$prim48326 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47134)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim48326, align 8
%stackaddr$prim48327 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim48327, align 8
%args47136$k$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48328 = alloca %struct.ScmObj*, align 8
%args47136$k$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %args47136$k$0)
store volatile %struct.ScmObj* %args47136$k$1, %struct.ScmObj** %stackaddr$prim48328, align 8
%clofunc48329 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc48329(%struct.ScmObj* %k, %struct.ScmObj* %args47136$k$1)
ret void
}

define tailcc void @proc_clo$ae44265(%struct.ScmObj* %env$ae44265,%struct.ScmObj* %el4023140418) {
%stackaddr$prim48330 = alloca %struct.ScmObj*, align 8
%k40419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %el4023140418)
store volatile %struct.ScmObj* %k40419, %struct.ScmObj** %stackaddr$prim48330, align 8
%stackaddr$prim48331 = alloca %struct.ScmObj*, align 8
%el40231 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %el4023140418)
store volatile %struct.ScmObj* %el40231, %struct.ScmObj** %stackaddr$prim48331, align 8
%stackaddr$applyprim48332 = alloca %struct.ScmObj*, align 8
%cpsaprim40420 = call %struct.ScmObj* @applyprim_vector(%struct.ScmObj* %el40231)
store volatile %struct.ScmObj* %cpsaprim40420, %struct.ScmObj** %stackaddr$applyprim48332, align 8
%ae44270 = call %struct.ScmObj* @const_init_int(i64 0)
%args47142$k40419$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48333 = alloca %struct.ScmObj*, align 8
%args47142$k40419$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim40420, %struct.ScmObj* %args47142$k40419$0)
store volatile %struct.ScmObj* %args47142$k40419$1, %struct.ScmObj** %stackaddr$prim48333, align 8
%stackaddr$prim48334 = alloca %struct.ScmObj*, align 8
%args47142$k40419$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44270, %struct.ScmObj* %args47142$k40419$1)
store volatile %struct.ScmObj* %args47142$k40419$2, %struct.ScmObj** %stackaddr$prim48334, align 8
%clofunc48335 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40419)
musttail call tailcc void %clofunc48335(%struct.ScmObj* %k40419, %struct.ScmObj* %args47142$k40419$2)
ret void
}

define tailcc void @proc_clo$ae44239(%struct.ScmObj* %env$ae44239,%struct.ScmObj* %el4023040421) {
%stackaddr$prim48336 = alloca %struct.ScmObj*, align 8
%k40422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %el4023040421)
store volatile %struct.ScmObj* %k40422, %struct.ScmObj** %stackaddr$prim48336, align 8
%stackaddr$prim48337 = alloca %struct.ScmObj*, align 8
%el40230 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %el4023040421)
store volatile %struct.ScmObj* %el40230, %struct.ScmObj** %stackaddr$prim48337, align 8
%stackaddr$applyprim48338 = alloca %struct.ScmObj*, align 8
%cpsaprim40423 = call %struct.ScmObj* @applyprim_vector(%struct.ScmObj* %el40230)
store volatile %struct.ScmObj* %cpsaprim40423, %struct.ScmObj** %stackaddr$applyprim48338, align 8
%ae44244 = call %struct.ScmObj* @const_init_int(i64 0)
%args47145$k40422$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48339 = alloca %struct.ScmObj*, align 8
%args47145$k40422$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim40423, %struct.ScmObj* %args47145$k40422$0)
store volatile %struct.ScmObj* %args47145$k40422$1, %struct.ScmObj** %stackaddr$prim48339, align 8
%stackaddr$prim48340 = alloca %struct.ScmObj*, align 8
%args47145$k40422$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44244, %struct.ScmObj* %args47145$k40422$1)
store volatile %struct.ScmObj* %args47145$k40422$2, %struct.ScmObj** %stackaddr$prim48340, align 8
%clofunc48341 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40422)
musttail call tailcc void %clofunc48341(%struct.ScmObj* %k40422, %struct.ScmObj* %args47145$k40422$2)
ret void
}

define tailcc void @proc_clo$ae44211(%struct.ScmObj* %env$ae44211,%struct.ScmObj* %lst4022940424) {
%stackaddr$prim48342 = alloca %struct.ScmObj*, align 8
%k40425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4022940424)
store volatile %struct.ScmObj* %k40425, %struct.ScmObj** %stackaddr$prim48342, align 8
%stackaddr$prim48343 = alloca %struct.ScmObj*, align 8
%lst40229 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4022940424)
store volatile %struct.ScmObj* %lst40229, %struct.ScmObj** %stackaddr$prim48343, align 8
%ae44215 = call %struct.ScmObj* @const_init_int(i64 0)
%args47149$k40425$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48344 = alloca %struct.ScmObj*, align 8
%args47149$k40425$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40229, %struct.ScmObj* %args47149$k40425$0)
store volatile %struct.ScmObj* %args47149$k40425$1, %struct.ScmObj** %stackaddr$prim48344, align 8
%stackaddr$prim48345 = alloca %struct.ScmObj*, align 8
%args47149$k40425$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44215, %struct.ScmObj* %args47149$k40425$1)
store volatile %struct.ScmObj* %args47149$k40425$2, %struct.ScmObj** %stackaddr$prim48345, align 8
%clofunc48346 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40425)
musttail call tailcc void %clofunc48346(%struct.ScmObj* %k40425, %struct.ScmObj* %args47149$k40425$2)
ret void
}

define tailcc void @proc_clo$ae44187(%struct.ScmObj* %env$ae44187,%struct.ScmObj* %lst4022840426) {
%stackaddr$prim48347 = alloca %struct.ScmObj*, align 8
%k40427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4022840426)
store volatile %struct.ScmObj* %k40427, %struct.ScmObj** %stackaddr$prim48347, align 8
%stackaddr$prim48348 = alloca %struct.ScmObj*, align 8
%lst40228 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4022840426)
store volatile %struct.ScmObj* %lst40228, %struct.ScmObj** %stackaddr$prim48348, align 8
%ae44191 = call %struct.ScmObj* @const_init_int(i64 0)
%args47152$k40427$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48349 = alloca %struct.ScmObj*, align 8
%args47152$k40427$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40228, %struct.ScmObj* %args47152$k40427$0)
store volatile %struct.ScmObj* %args47152$k40427$1, %struct.ScmObj** %stackaddr$prim48349, align 8
%stackaddr$prim48350 = alloca %struct.ScmObj*, align 8
%args47152$k40427$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44191, %struct.ScmObj* %args47152$k40427$1)
store volatile %struct.ScmObj* %args47152$k40427$2, %struct.ScmObj** %stackaddr$prim48350, align 8
%clofunc48351 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40427)
musttail call tailcc void %clofunc48351(%struct.ScmObj* %k40427, %struct.ScmObj* %args47152$k40427$2)
ret void
}

define tailcc void @proc_clo$ae44165(%struct.ScmObj* %env$ae44165,%struct.ScmObj* %lst4022740428) {
%stackaddr$prim48352 = alloca %struct.ScmObj*, align 8
%k40429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4022740428)
store volatile %struct.ScmObj* %k40429, %struct.ScmObj** %stackaddr$prim48352, align 8
%stackaddr$prim48353 = alloca %struct.ScmObj*, align 8
%lst40227 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4022740428)
store volatile %struct.ScmObj* %lst40227, %struct.ScmObj** %stackaddr$prim48353, align 8
%ae44169 = call %struct.ScmObj* @const_init_int(i64 0)
%args47154$k40429$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48354 = alloca %struct.ScmObj*, align 8
%args47154$k40429$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40227, %struct.ScmObj* %args47154$k40429$0)
store volatile %struct.ScmObj* %args47154$k40429$1, %struct.ScmObj** %stackaddr$prim48354, align 8
%stackaddr$prim48355 = alloca %struct.ScmObj*, align 8
%args47154$k40429$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44169, %struct.ScmObj* %args47154$k40429$1)
store volatile %struct.ScmObj* %args47154$k40429$2, %struct.ScmObj** %stackaddr$prim48355, align 8
%clofunc48356 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40429)
musttail call tailcc void %clofunc48356(%struct.ScmObj* %k40429, %struct.ScmObj* %args47154$k40429$2)
ret void
}

define tailcc void @proc_clo$ae44141(%struct.ScmObj* %env$ae44141,%struct.ScmObj* %lst4022640430) {
%stackaddr$prim48357 = alloca %struct.ScmObj*, align 8
%k40431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4022640430)
store volatile %struct.ScmObj* %k40431, %struct.ScmObj** %stackaddr$prim48357, align 8
%stackaddr$prim48358 = alloca %struct.ScmObj*, align 8
%lst40226 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4022640430)
store volatile %struct.ScmObj* %lst40226, %struct.ScmObj** %stackaddr$prim48358, align 8
%ae44145 = call %struct.ScmObj* @const_init_int(i64 0)
%args47157$k40431$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48359 = alloca %struct.ScmObj*, align 8
%args47157$k40431$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40226, %struct.ScmObj* %args47157$k40431$0)
store volatile %struct.ScmObj* %args47157$k40431$1, %struct.ScmObj** %stackaddr$prim48359, align 8
%stackaddr$prim48360 = alloca %struct.ScmObj*, align 8
%args47157$k40431$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44145, %struct.ScmObj* %args47157$k40431$1)
store volatile %struct.ScmObj* %args47157$k40431$2, %struct.ScmObj** %stackaddr$prim48360, align 8
%clofunc48361 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40431)
musttail call tailcc void %clofunc48361(%struct.ScmObj* %k40431, %struct.ScmObj* %args47157$k40431$2)
ret void
}

define tailcc void @proc_clo$ae44119(%struct.ScmObj* %env$ae44119,%struct.ScmObj* %lst4022540432) {
%stackaddr$prim48362 = alloca %struct.ScmObj*, align 8
%k40433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4022540432)
store volatile %struct.ScmObj* %k40433, %struct.ScmObj** %stackaddr$prim48362, align 8
%stackaddr$prim48363 = alloca %struct.ScmObj*, align 8
%lst40225 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4022540432)
store volatile %struct.ScmObj* %lst40225, %struct.ScmObj** %stackaddr$prim48363, align 8
%ae44123 = call %struct.ScmObj* @const_init_int(i64 0)
%args47159$k40433$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48364 = alloca %struct.ScmObj*, align 8
%args47159$k40433$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40225, %struct.ScmObj* %args47159$k40433$0)
store volatile %struct.ScmObj* %args47159$k40433$1, %struct.ScmObj** %stackaddr$prim48364, align 8
%stackaddr$prim48365 = alloca %struct.ScmObj*, align 8
%args47159$k40433$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44123, %struct.ScmObj* %args47159$k40433$1)
store volatile %struct.ScmObj* %args47159$k40433$2, %struct.ScmObj** %stackaddr$prim48365, align 8
%clofunc48366 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40433)
musttail call tailcc void %clofunc48366(%struct.ScmObj* %k40433, %struct.ScmObj* %args47159$k40433$2)
ret void
}

define tailcc void @proc_clo$ae44095(%struct.ScmObj* %env$ae44095,%struct.ScmObj* %arg4022440434) {
%stackaddr$prim48367 = alloca %struct.ScmObj*, align 8
%k40435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %arg4022440434)
store volatile %struct.ScmObj* %k40435, %struct.ScmObj** %stackaddr$prim48367, align 8
%stackaddr$prim48368 = alloca %struct.ScmObj*, align 8
%arg40224 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %arg4022440434)
store volatile %struct.ScmObj* %arg40224, %struct.ScmObj** %stackaddr$prim48368, align 8
%stackaddr$applyprim48369 = alloca %struct.ScmObj*, align 8
%cpsaprim40436 = call %struct.ScmObj* @applyprim_number_63(%struct.ScmObj* %arg40224)
store volatile %struct.ScmObj* %cpsaprim40436, %struct.ScmObj** %stackaddr$applyprim48369, align 8
%ae44100 = call %struct.ScmObj* @const_init_int(i64 0)
%args47161$k40435$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48370 = alloca %struct.ScmObj*, align 8
%args47161$k40435$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim40436, %struct.ScmObj* %args47161$k40435$0)
store volatile %struct.ScmObj* %args47161$k40435$1, %struct.ScmObj** %stackaddr$prim48370, align 8
%stackaddr$prim48371 = alloca %struct.ScmObj*, align 8
%args47161$k40435$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44100, %struct.ScmObj* %args47161$k40435$1)
store volatile %struct.ScmObj* %args47161$k40435$2, %struct.ScmObj** %stackaddr$prim48371, align 8
%clofunc48372 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40435)
musttail call tailcc void %clofunc48372(%struct.ScmObj* %k40435, %struct.ScmObj* %args47161$k40435$2)
ret void
}

define tailcc void @proc_clo$ae44060(%struct.ScmObj* %env$ae44060,%struct.ScmObj* %current_45args47163) {
%stackaddr$prim48373 = alloca %struct.ScmObj*, align 8
%k40437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47163)
store volatile %struct.ScmObj* %k40437, %struct.ScmObj** %stackaddr$prim48373, align 8
%stackaddr$prim48374 = alloca %struct.ScmObj*, align 8
%current_45args47164 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47163)
store volatile %struct.ScmObj* %current_45args47164, %struct.ScmObj** %stackaddr$prim48374, align 8
%stackaddr$prim48375 = alloca %struct.ScmObj*, align 8
%b40223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47164)
store volatile %struct.ScmObj* %b40223, %struct.ScmObj** %stackaddr$prim48375, align 8
%truthy$cmp48376 = call i64 @is_truthy_value(%struct.ScmObj* %b40223)
%cmp$cmp48376 = icmp eq i64 %truthy$cmp48376, 1
br i1 %cmp$cmp48376, label %truebranch$cmp48376, label %falsebranch$cmp48376
truebranch$cmp48376:
%ae44063 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44064 = call %struct.ScmObj* @const_init_int(i64 1)
%args47166$k40437$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48377 = alloca %struct.ScmObj*, align 8
%args47166$k40437$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44064, %struct.ScmObj* %args47166$k40437$0)
store volatile %struct.ScmObj* %args47166$k40437$1, %struct.ScmObj** %stackaddr$prim48377, align 8
%stackaddr$prim48378 = alloca %struct.ScmObj*, align 8
%args47166$k40437$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44063, %struct.ScmObj* %args47166$k40437$1)
store volatile %struct.ScmObj* %args47166$k40437$2, %struct.ScmObj** %stackaddr$prim48378, align 8
%clofunc48379 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40437)
musttail call tailcc void %clofunc48379(%struct.ScmObj* %k40437, %struct.ScmObj* %args47166$k40437$2)
ret void
falsebranch$cmp48376:
%ae44072 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44073 = call %struct.ScmObj* @const_init_int(i64 2)
%args47167$k40437$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48380 = alloca %struct.ScmObj*, align 8
%args47167$k40437$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44073, %struct.ScmObj* %args47167$k40437$0)
store volatile %struct.ScmObj* %args47167$k40437$1, %struct.ScmObj** %stackaddr$prim48380, align 8
%stackaddr$prim48381 = alloca %struct.ScmObj*, align 8
%args47167$k40437$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44072, %struct.ScmObj* %args47167$k40437$1)
store volatile %struct.ScmObj* %args47167$k40437$2, %struct.ScmObj** %stackaddr$prim48381, align 8
%clofunc48382 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40437)
musttail call tailcc void %clofunc48382(%struct.ScmObj* %k40437, %struct.ScmObj* %args47167$k40437$2)
ret void
}

define tailcc void @proc_clo$ae44036(%struct.ScmObj* %env$ae44036,%struct.ScmObj* %args4022240438) {
%stackaddr$prim48383 = alloca %struct.ScmObj*, align 8
%k40439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4022240438)
store volatile %struct.ScmObj* %k40439, %struct.ScmObj** %stackaddr$prim48383, align 8
%stackaddr$prim48384 = alloca %struct.ScmObj*, align 8
%args40222 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4022240438)
store volatile %struct.ScmObj* %args40222, %struct.ScmObj** %stackaddr$prim48384, align 8
%stackaddr$applyprim48385 = alloca %struct.ScmObj*, align 8
%cpsaprim40440 = call %struct.ScmObj* @applyprim__43(%struct.ScmObj* %args40222)
store volatile %struct.ScmObj* %cpsaprim40440, %struct.ScmObj** %stackaddr$applyprim48385, align 8
%ae44041 = call %struct.ScmObj* @const_init_int(i64 0)
%args47169$k40439$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48386 = alloca %struct.ScmObj*, align 8
%args47169$k40439$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim40440, %struct.ScmObj* %args47169$k40439$0)
store volatile %struct.ScmObj* %args47169$k40439$1, %struct.ScmObj** %stackaddr$prim48386, align 8
%stackaddr$prim48387 = alloca %struct.ScmObj*, align 8
%args47169$k40439$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44041, %struct.ScmObj* %args47169$k40439$1)
store volatile %struct.ScmObj* %args47169$k40439$2, %struct.ScmObj** %stackaddr$prim48387, align 8
%clofunc48388 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40439)
musttail call tailcc void %clofunc48388(%struct.ScmObj* %k40439, %struct.ScmObj* %args47169$k40439$2)
ret void
}

define tailcc void @proc_clo$ae43949(%struct.ScmObj* %env$ae43949,%struct.ScmObj* %current_45args47171) {
%stackaddr$prim48389 = alloca %struct.ScmObj*, align 8
%k40441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47171)
store volatile %struct.ScmObj* %k40441, %struct.ScmObj** %stackaddr$prim48389, align 8
%stackaddr$prim48390 = alloca %struct.ScmObj*, align 8
%current_45args47172 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47171)
store volatile %struct.ScmObj* %current_45args47172, %struct.ScmObj** %stackaddr$prim48390, align 8
%stackaddr$prim48391 = alloca %struct.ScmObj*, align 8
%thunk40221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47172)
store volatile %struct.ScmObj* %thunk40221, %struct.ScmObj** %stackaddr$prim48391, align 8
%stackaddr$prim48392 = alloca %struct.ScmObj*, align 8
%anf_45bind40343 = call %struct.ScmObj* @prim_vector_63(%struct.ScmObj* %thunk40221)
store volatile %struct.ScmObj* %anf_45bind40343, %struct.ScmObj** %stackaddr$prim48392, align 8
%truthy$cmp48393 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40343)
%cmp$cmp48393 = icmp eq i64 %truthy$cmp48393, 1
br i1 %cmp$cmp48393, label %truebranch$cmp48393, label %falsebranch$cmp48393
truebranch$cmp48393:
%stackaddr$prim48394 = alloca %struct.ScmObj*, align 8
%anf_45bind40344 = call %struct.ScmObj* @prim_vector_45length(%struct.ScmObj* %thunk40221)
store volatile %struct.ScmObj* %anf_45bind40344, %struct.ScmObj** %stackaddr$prim48394, align 8
%ae43954 = call %struct.ScmObj* @const_init_int(i64 3)
%stackaddr$prim48395 = alloca %struct.ScmObj*, align 8
%anf_45bind40345 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40344, %struct.ScmObj* %ae43954)
store volatile %struct.ScmObj* %anf_45bind40345, %struct.ScmObj** %stackaddr$prim48395, align 8
%truthy$cmp48396 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40345)
%cmp$cmp48396 = icmp eq i64 %truthy$cmp48396, 1
br i1 %cmp$cmp48396, label %truebranch$cmp48396, label %falsebranch$cmp48396
truebranch$cmp48396:
%ae43957 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48397 = alloca %struct.ScmObj*, align 8
%anf_45bind40346 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %thunk40221, %struct.ScmObj* %ae43957)
store volatile %struct.ScmObj* %anf_45bind40346, %struct.ScmObj** %stackaddr$prim48397, align 8
%ae43959 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @global$sym$ae4395948398, i32 0, i32 0))
%stackaddr$prim48399 = alloca %struct.ScmObj*, align 8
%cpsprim40442 = call %struct.ScmObj* @prim_equal_63(%struct.ScmObj* %anf_45bind40346, %struct.ScmObj* %ae43959)
store volatile %struct.ScmObj* %cpsprim40442, %struct.ScmObj** %stackaddr$prim48399, align 8
%ae43961 = call %struct.ScmObj* @const_init_int(i64 0)
%args47174$k40441$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48400 = alloca %struct.ScmObj*, align 8
%args47174$k40441$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40442, %struct.ScmObj* %args47174$k40441$0)
store volatile %struct.ScmObj* %args47174$k40441$1, %struct.ScmObj** %stackaddr$prim48400, align 8
%stackaddr$prim48401 = alloca %struct.ScmObj*, align 8
%args47174$k40441$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43961, %struct.ScmObj* %args47174$k40441$1)
store volatile %struct.ScmObj* %args47174$k40441$2, %struct.ScmObj** %stackaddr$prim48401, align 8
%clofunc48402 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40441)
musttail call tailcc void %clofunc48402(%struct.ScmObj* %k40441, %struct.ScmObj* %args47174$k40441$2)
ret void
falsebranch$cmp48396:
%ae43979 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43980 = call %struct.ScmObj* @const_init_false()
%args47175$k40441$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48403 = alloca %struct.ScmObj*, align 8
%args47175$k40441$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43980, %struct.ScmObj* %args47175$k40441$0)
store volatile %struct.ScmObj* %args47175$k40441$1, %struct.ScmObj** %stackaddr$prim48403, align 8
%stackaddr$prim48404 = alloca %struct.ScmObj*, align 8
%args47175$k40441$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43979, %struct.ScmObj* %args47175$k40441$1)
store volatile %struct.ScmObj* %args47175$k40441$2, %struct.ScmObj** %stackaddr$prim48404, align 8
%clofunc48405 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40441)
musttail call tailcc void %clofunc48405(%struct.ScmObj* %k40441, %struct.ScmObj* %args47175$k40441$2)
ret void
falsebranch$cmp48393:
%ae44001 = call %struct.ScmObj* @const_init_int(i64 0)
%ae44002 = call %struct.ScmObj* @const_init_false()
%args47176$k40441$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48406 = alloca %struct.ScmObj*, align 8
%args47176$k40441$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44002, %struct.ScmObj* %args47176$k40441$0)
store volatile %struct.ScmObj* %args47176$k40441$1, %struct.ScmObj** %stackaddr$prim48406, align 8
%stackaddr$prim48407 = alloca %struct.ScmObj*, align 8
%args47176$k40441$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae44001, %struct.ScmObj* %args47176$k40441$1)
store volatile %struct.ScmObj* %args47176$k40441$2, %struct.ScmObj** %stackaddr$prim48407, align 8
%clofunc48408 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40441)
musttail call tailcc void %clofunc48408(%struct.ScmObj* %k40441, %struct.ScmObj* %args47176$k40441$2)
ret void
}

define tailcc void @proc_clo$ae43923(%struct.ScmObj* %env$ae43923,%struct.ScmObj* %current_45args47178) {
%stackaddr$prim48409 = alloca %struct.ScmObj*, align 8
%k40443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47178)
store volatile %struct.ScmObj* %k40443, %struct.ScmObj** %stackaddr$prim48409, align 8
%stackaddr$prim48410 = alloca %struct.ScmObj*, align 8
%current_45args47179 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47178)
store volatile %struct.ScmObj* %current_45args47179, %struct.ScmObj** %stackaddr$prim48410, align 8
%stackaddr$prim48411 = alloca %struct.ScmObj*, align 8
%x40160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47179)
store volatile %struct.ScmObj* %x40160, %struct.ScmObj** %stackaddr$prim48411, align 8
%stackaddr$prim48412 = alloca %struct.ScmObj*, align 8
%anf_45bind40340 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40160)
store volatile %struct.ScmObj* %anf_45bind40340, %struct.ScmObj** %stackaddr$prim48412, align 8
%stackaddr$prim48413 = alloca %struct.ScmObj*, align 8
%anf_45bind40341 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40340)
store volatile %struct.ScmObj* %anf_45bind40341, %struct.ScmObj** %stackaddr$prim48413, align 8
%stackaddr$prim48414 = alloca %struct.ScmObj*, align 8
%anf_45bind40342 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40341)
store volatile %struct.ScmObj* %anf_45bind40342, %struct.ScmObj** %stackaddr$prim48414, align 8
%stackaddr$prim48415 = alloca %struct.ScmObj*, align 8
%cpsprim40444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40342)
store volatile %struct.ScmObj* %cpsprim40444, %struct.ScmObj** %stackaddr$prim48415, align 8
%ae43929 = call %struct.ScmObj* @const_init_int(i64 0)
%args47181$k40443$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48416 = alloca %struct.ScmObj*, align 8
%args47181$k40443$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40444, %struct.ScmObj* %args47181$k40443$0)
store volatile %struct.ScmObj* %args47181$k40443$1, %struct.ScmObj** %stackaddr$prim48416, align 8
%stackaddr$prim48417 = alloca %struct.ScmObj*, align 8
%args47181$k40443$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43929, %struct.ScmObj* %args47181$k40443$1)
store volatile %struct.ScmObj* %args47181$k40443$2, %struct.ScmObj** %stackaddr$prim48417, align 8
%clofunc48418 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40443)
musttail call tailcc void %clofunc48418(%struct.ScmObj* %k40443, %struct.ScmObj* %args47181$k40443$2)
ret void
}

define tailcc void @proc_clo$ae43899(%struct.ScmObj* %env$ae43899,%struct.ScmObj* %current_45args47183) {
%stackaddr$prim48419 = alloca %struct.ScmObj*, align 8
%k40445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47183)
store volatile %struct.ScmObj* %k40445, %struct.ScmObj** %stackaddr$prim48419, align 8
%stackaddr$prim48420 = alloca %struct.ScmObj*, align 8
%current_45args47184 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47183)
store volatile %struct.ScmObj* %current_45args47184, %struct.ScmObj** %stackaddr$prim48420, align 8
%stackaddr$prim48421 = alloca %struct.ScmObj*, align 8
%x40162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47184)
store volatile %struct.ScmObj* %x40162, %struct.ScmObj** %stackaddr$prim48421, align 8
%stackaddr$prim48422 = alloca %struct.ScmObj*, align 8
%anf_45bind40338 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40162)
store volatile %struct.ScmObj* %anf_45bind40338, %struct.ScmObj** %stackaddr$prim48422, align 8
%stackaddr$prim48423 = alloca %struct.ScmObj*, align 8
%anf_45bind40339 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40338)
store volatile %struct.ScmObj* %anf_45bind40339, %struct.ScmObj** %stackaddr$prim48423, align 8
%stackaddr$prim48424 = alloca %struct.ScmObj*, align 8
%cpsprim40446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40339)
store volatile %struct.ScmObj* %cpsprim40446, %struct.ScmObj** %stackaddr$prim48424, align 8
%ae43904 = call %struct.ScmObj* @const_init_int(i64 0)
%args47186$k40445$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48425 = alloca %struct.ScmObj*, align 8
%args47186$k40445$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40446, %struct.ScmObj* %args47186$k40445$0)
store volatile %struct.ScmObj* %args47186$k40445$1, %struct.ScmObj** %stackaddr$prim48425, align 8
%stackaddr$prim48426 = alloca %struct.ScmObj*, align 8
%args47186$k40445$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43904, %struct.ScmObj* %args47186$k40445$1)
store volatile %struct.ScmObj* %args47186$k40445$2, %struct.ScmObj** %stackaddr$prim48426, align 8
%clofunc48427 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40445)
musttail call tailcc void %clofunc48427(%struct.ScmObj* %k40445, %struct.ScmObj* %args47186$k40445$2)
ret void
}

define tailcc void @proc_clo$ae43877(%struct.ScmObj* %env$ae43877,%struct.ScmObj* %current_45args47188) {
%stackaddr$prim48428 = alloca %struct.ScmObj*, align 8
%k40447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47188)
store volatile %struct.ScmObj* %k40447, %struct.ScmObj** %stackaddr$prim48428, align 8
%stackaddr$prim48429 = alloca %struct.ScmObj*, align 8
%current_45args47189 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47188)
store volatile %struct.ScmObj* %current_45args47189, %struct.ScmObj** %stackaddr$prim48429, align 8
%stackaddr$prim48430 = alloca %struct.ScmObj*, align 8
%x40164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47189)
store volatile %struct.ScmObj* %x40164, %struct.ScmObj** %stackaddr$prim48430, align 8
%stackaddr$prim48431 = alloca %struct.ScmObj*, align 8
%anf_45bind40337 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40164)
store volatile %struct.ScmObj* %anf_45bind40337, %struct.ScmObj** %stackaddr$prim48431, align 8
%stackaddr$prim48432 = alloca %struct.ScmObj*, align 8
%cpsprim40448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40337)
store volatile %struct.ScmObj* %cpsprim40448, %struct.ScmObj** %stackaddr$prim48432, align 8
%ae43881 = call %struct.ScmObj* @const_init_int(i64 0)
%args47191$k40447$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48433 = alloca %struct.ScmObj*, align 8
%args47191$k40447$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40448, %struct.ScmObj* %args47191$k40447$0)
store volatile %struct.ScmObj* %args47191$k40447$1, %struct.ScmObj** %stackaddr$prim48433, align 8
%stackaddr$prim48434 = alloca %struct.ScmObj*, align 8
%args47191$k40447$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43881, %struct.ScmObj* %args47191$k40447$1)
store volatile %struct.ScmObj* %args47191$k40447$2, %struct.ScmObj** %stackaddr$prim48434, align 8
%clofunc48435 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40447)
musttail call tailcc void %clofunc48435(%struct.ScmObj* %k40447, %struct.ScmObj* %args47191$k40447$2)
ret void
}

define tailcc void @proc_clo$ae43857(%struct.ScmObj* %env$ae43857,%struct.ScmObj* %current_45args47193) {
%stackaddr$prim48436 = alloca %struct.ScmObj*, align 8
%k40449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47193)
store volatile %struct.ScmObj* %k40449, %struct.ScmObj** %stackaddr$prim48436, align 8
%stackaddr$prim48437 = alloca %struct.ScmObj*, align 8
%current_45args47194 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47193)
store volatile %struct.ScmObj* %current_45args47194, %struct.ScmObj** %stackaddr$prim48437, align 8
%stackaddr$prim48438 = alloca %struct.ScmObj*, align 8
%x40166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47194)
store volatile %struct.ScmObj* %x40166, %struct.ScmObj** %stackaddr$prim48438, align 8
%stackaddr$prim48439 = alloca %struct.ScmObj*, align 8
%cpsprim40450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40166)
store volatile %struct.ScmObj* %cpsprim40450, %struct.ScmObj** %stackaddr$prim48439, align 8
%ae43860 = call %struct.ScmObj* @const_init_int(i64 0)
%args47196$k40449$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48440 = alloca %struct.ScmObj*, align 8
%args47196$k40449$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40450, %struct.ScmObj* %args47196$k40449$0)
store volatile %struct.ScmObj* %args47196$k40449$1, %struct.ScmObj** %stackaddr$prim48440, align 8
%stackaddr$prim48441 = alloca %struct.ScmObj*, align 8
%args47196$k40449$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43860, %struct.ScmObj* %args47196$k40449$1)
store volatile %struct.ScmObj* %args47196$k40449$2, %struct.ScmObj** %stackaddr$prim48441, align 8
%clofunc48442 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40449)
musttail call tailcc void %clofunc48442(%struct.ScmObj* %k40449, %struct.ScmObj* %args47196$k40449$2)
ret void
}

define tailcc void @proc_clo$ae43759(%struct.ScmObj* %env$ae43759,%struct.ScmObj* %args4016840451) {
%stackaddr$env-ref48443 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43759, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48443
%stackaddr$prim48444 = alloca %struct.ScmObj*, align 8
%k40452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4016840451)
store volatile %struct.ScmObj* %k40452, %struct.ScmObj** %stackaddr$prim48444, align 8
%stackaddr$prim48445 = alloca %struct.ScmObj*, align 8
%args40168 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4016840451)
store volatile %struct.ScmObj* %args40168, %struct.ScmObj** %stackaddr$prim48445, align 8
%stackaddr$prim48446 = alloca %struct.ScmObj*, align 8
%anf_45bind40331 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %anf_45bind40331, %struct.ScmObj** %stackaddr$prim48446, align 8
%truthy$cmp48447 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40331)
%cmp$cmp48447 = icmp eq i64 %truthy$cmp48447, 1
br i1 %cmp$cmp48447, label %truebranch$cmp48447, label %falsebranch$cmp48447
truebranch$cmp48447:
%ae43765 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43766 = call %struct.ScmObj* @const_init_int(i64 1)
%args47198$k40452$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48448 = alloca %struct.ScmObj*, align 8
%args47198$k40452$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43766, %struct.ScmObj* %args47198$k40452$0)
store volatile %struct.ScmObj* %args47198$k40452$1, %struct.ScmObj** %stackaddr$prim48448, align 8
%stackaddr$prim48449 = alloca %struct.ScmObj*, align 8
%args47198$k40452$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43765, %struct.ScmObj* %args47198$k40452$1)
store volatile %struct.ScmObj* %args47198$k40452$2, %struct.ScmObj** %stackaddr$prim48449, align 8
%clofunc48450 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40452)
musttail call tailcc void %clofunc48450(%struct.ScmObj* %k40452, %struct.ScmObj* %args47198$k40452$2)
ret void
falsebranch$cmp48447:
%stackaddr$prim48451 = alloca %struct.ScmObj*, align 8
%anf_45bind40332 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %anf_45bind40332, %struct.ScmObj** %stackaddr$prim48451, align 8
%stackaddr$prim48452 = alloca %struct.ScmObj*, align 8
%anf_45bind40333 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40332)
store volatile %struct.ScmObj* %anf_45bind40333, %struct.ScmObj** %stackaddr$prim48452, align 8
%truthy$cmp48453 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40333)
%cmp$cmp48453 = icmp eq i64 %truthy$cmp48453, 1
br i1 %cmp$cmp48453, label %truebranch$cmp48453, label %falsebranch$cmp48453
truebranch$cmp48453:
%stackaddr$prim48454 = alloca %struct.ScmObj*, align 8
%cpsprim40453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %cpsprim40453, %struct.ScmObj** %stackaddr$prim48454, align 8
%ae43778 = call %struct.ScmObj* @const_init_int(i64 0)
%args47199$k40452$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48455 = alloca %struct.ScmObj*, align 8
%args47199$k40452$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40453, %struct.ScmObj* %args47199$k40452$0)
store volatile %struct.ScmObj* %args47199$k40452$1, %struct.ScmObj** %stackaddr$prim48455, align 8
%stackaddr$prim48456 = alloca %struct.ScmObj*, align 8
%args47199$k40452$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43778, %struct.ScmObj* %args47199$k40452$1)
store volatile %struct.ScmObj* %args47199$k40452$2, %struct.ScmObj** %stackaddr$prim48456, align 8
%clofunc48457 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40452)
musttail call tailcc void %clofunc48457(%struct.ScmObj* %k40452, %struct.ScmObj* %args47199$k40452$2)
ret void
falsebranch$cmp48453:
%stackaddr$makeclosure48458 = alloca %struct.ScmObj*, align 8
%fptrToInt48459 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43783 to i64
%ae43783 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48459)
store volatile %struct.ScmObj* %ae43783, %struct.ScmObj** %stackaddr$makeclosure48458, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43783, %struct.ScmObj* %args40168, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43783, %struct.ScmObj* %k40452, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43783, %struct.ScmObj* %_37foldl140107, i64 2)
%ae43784 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48460 = alloca %struct.ScmObj*, align 8
%fptrToInt48461 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43785 to i64
%ae43785 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48461)
store volatile %struct.ScmObj* %ae43785, %struct.ScmObj** %stackaddr$makeclosure48460, align 8
%args47209$ae43783$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48462 = alloca %struct.ScmObj*, align 8
%args47209$ae43783$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43785, %struct.ScmObj* %args47209$ae43783$0)
store volatile %struct.ScmObj* %args47209$ae43783$1, %struct.ScmObj** %stackaddr$prim48462, align 8
%stackaddr$prim48463 = alloca %struct.ScmObj*, align 8
%args47209$ae43783$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43784, %struct.ScmObj* %args47209$ae43783$1)
store volatile %struct.ScmObj* %args47209$ae43783$2, %struct.ScmObj** %stackaddr$prim48463, align 8
%clofunc48464 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43783)
musttail call tailcc void %clofunc48464(%struct.ScmObj* %ae43783, %struct.ScmObj* %args47209$ae43783$2)
ret void
}

define tailcc void @proc_clo$ae43783(%struct.ScmObj* %env$ae43783,%struct.ScmObj* %current_45args47200) {
%stackaddr$env-ref48465 = alloca %struct.ScmObj*, align 8
%args40168 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43783, i64 0)
store %struct.ScmObj* %args40168, %struct.ScmObj** %stackaddr$env-ref48465
%stackaddr$env-ref48466 = alloca %struct.ScmObj*, align 8
%k40452 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43783, i64 1)
store %struct.ScmObj* %k40452, %struct.ScmObj** %stackaddr$env-ref48466
%stackaddr$env-ref48467 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43783, i64 2)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref48467
%stackaddr$prim48468 = alloca %struct.ScmObj*, align 8
%_95k40454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47200)
store volatile %struct.ScmObj* %_95k40454, %struct.ScmObj** %stackaddr$prim48468, align 8
%stackaddr$prim48469 = alloca %struct.ScmObj*, align 8
%current_45args47201 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47200)
store volatile %struct.ScmObj* %current_45args47201, %struct.ScmObj** %stackaddr$prim48469, align 8
%stackaddr$prim48470 = alloca %struct.ScmObj*, align 8
%anf_45bind40334 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47201)
store volatile %struct.ScmObj* %anf_45bind40334, %struct.ScmObj** %stackaddr$prim48470, align 8
%stackaddr$prim48471 = alloca %struct.ScmObj*, align 8
%anf_45bind40335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %anf_45bind40335, %struct.ScmObj** %stackaddr$prim48471, align 8
%stackaddr$prim48472 = alloca %struct.ScmObj*, align 8
%anf_45bind40336 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40168)
store volatile %struct.ScmObj* %anf_45bind40336, %struct.ScmObj** %stackaddr$prim48472, align 8
%args47203$_37foldl140107$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48473 = alloca %struct.ScmObj*, align 8
%args47203$_37foldl140107$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40336, %struct.ScmObj* %args47203$_37foldl140107$0)
store volatile %struct.ScmObj* %args47203$_37foldl140107$1, %struct.ScmObj** %stackaddr$prim48473, align 8
%stackaddr$prim48474 = alloca %struct.ScmObj*, align 8
%args47203$_37foldl140107$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40335, %struct.ScmObj* %args47203$_37foldl140107$1)
store volatile %struct.ScmObj* %args47203$_37foldl140107$2, %struct.ScmObj** %stackaddr$prim48474, align 8
%stackaddr$prim48475 = alloca %struct.ScmObj*, align 8
%args47203$_37foldl140107$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40334, %struct.ScmObj* %args47203$_37foldl140107$2)
store volatile %struct.ScmObj* %args47203$_37foldl140107$3, %struct.ScmObj** %stackaddr$prim48475, align 8
%stackaddr$prim48476 = alloca %struct.ScmObj*, align 8
%args47203$_37foldl140107$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40452, %struct.ScmObj* %args47203$_37foldl140107$3)
store volatile %struct.ScmObj* %args47203$_37foldl140107$4, %struct.ScmObj** %stackaddr$prim48476, align 8
%clofunc48477 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140107)
musttail call tailcc void %clofunc48477(%struct.ScmObj* %_37foldl140107, %struct.ScmObj* %args47203$_37foldl140107$4)
ret void
}

define tailcc void @proc_clo$ae43785(%struct.ScmObj* %env$ae43785,%struct.ScmObj* %current_45args47204) {
%stackaddr$prim48478 = alloca %struct.ScmObj*, align 8
%k40455 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47204)
store volatile %struct.ScmObj* %k40455, %struct.ScmObj** %stackaddr$prim48478, align 8
%stackaddr$prim48479 = alloca %struct.ScmObj*, align 8
%current_45args47205 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47204)
store volatile %struct.ScmObj* %current_45args47205, %struct.ScmObj** %stackaddr$prim48479, align 8
%stackaddr$prim48480 = alloca %struct.ScmObj*, align 8
%n40170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47205)
store volatile %struct.ScmObj* %n40170, %struct.ScmObj** %stackaddr$prim48480, align 8
%stackaddr$prim48481 = alloca %struct.ScmObj*, align 8
%current_45args47206 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47205)
store volatile %struct.ScmObj* %current_45args47206, %struct.ScmObj** %stackaddr$prim48481, align 8
%stackaddr$prim48482 = alloca %struct.ScmObj*, align 8
%v40169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47206)
store volatile %struct.ScmObj* %v40169, %struct.ScmObj** %stackaddr$prim48482, align 8
%stackaddr$prim48483 = alloca %struct.ScmObj*, align 8
%cpsprim40456 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v40169, %struct.ScmObj* %n40170)
store volatile %struct.ScmObj* %cpsprim40456, %struct.ScmObj** %stackaddr$prim48483, align 8
%ae43789 = call %struct.ScmObj* @const_init_int(i64 0)
%args47208$k40455$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48484 = alloca %struct.ScmObj*, align 8
%args47208$k40455$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40456, %struct.ScmObj* %args47208$k40455$0)
store volatile %struct.ScmObj* %args47208$k40455$1, %struct.ScmObj** %stackaddr$prim48484, align 8
%stackaddr$prim48485 = alloca %struct.ScmObj*, align 8
%args47208$k40455$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43789, %struct.ScmObj* %args47208$k40455$1)
store volatile %struct.ScmObj* %args47208$k40455$2, %struct.ScmObj** %stackaddr$prim48485, align 8
%clofunc48486 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40455)
musttail call tailcc void %clofunc48486(%struct.ScmObj* %k40455, %struct.ScmObj* %args47208$k40455$2)
ret void
}

define tailcc void @proc_clo$ae43355(%struct.ScmObj* %env$ae43355,%struct.ScmObj* %current_45args47211) {
%stackaddr$prim48487 = alloca %struct.ScmObj*, align 8
%k40457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47211)
store volatile %struct.ScmObj* %k40457, %struct.ScmObj** %stackaddr$prim48487, align 8
%stackaddr$prim48488 = alloca %struct.ScmObj*, align 8
%current_45args47212 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47211)
store volatile %struct.ScmObj* %current_45args47212, %struct.ScmObj** %stackaddr$prim48488, align 8
%stackaddr$prim48489 = alloca %struct.ScmObj*, align 8
%v40173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47212)
store volatile %struct.ScmObj* %v40173, %struct.ScmObj** %stackaddr$prim48489, align 8
%stackaddr$prim48490 = alloca %struct.ScmObj*, align 8
%current_45args47213 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47212)
store volatile %struct.ScmObj* %current_45args47213, %struct.ScmObj** %stackaddr$prim48490, align 8
%stackaddr$prim48491 = alloca %struct.ScmObj*, align 8
%lst40172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47213)
store volatile %struct.ScmObj* %lst40172, %struct.ScmObj** %stackaddr$prim48491, align 8
%ae43356 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48492 = alloca %struct.ScmObj*, align 8
%lst40174 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae43356, %struct.ScmObj* %lst40172)
store volatile %struct.ScmObj* %lst40174, %struct.ScmObj** %stackaddr$prim48492, align 8
%stackaddr$makeclosure48493 = alloca %struct.ScmObj*, align 8
%fptrToInt48494 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43358 to i64
%ae43358 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48494)
store volatile %struct.ScmObj* %ae43358, %struct.ScmObj** %stackaddr$makeclosure48493, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43358, %struct.ScmObj* %k40457, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43358, %struct.ScmObj* %lst40174, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43358, %struct.ScmObj* %v40173, i64 2)
%ae43359 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48495 = alloca %struct.ScmObj*, align 8
%fptrToInt48496 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43360 to i64
%ae43360 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48496)
store volatile %struct.ScmObj* %ae43360, %struct.ScmObj** %stackaddr$makeclosure48495, align 8
%args47235$ae43358$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48497 = alloca %struct.ScmObj*, align 8
%args47235$ae43358$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43360, %struct.ScmObj* %args47235$ae43358$0)
store volatile %struct.ScmObj* %args47235$ae43358$1, %struct.ScmObj** %stackaddr$prim48497, align 8
%stackaddr$prim48498 = alloca %struct.ScmObj*, align 8
%args47235$ae43358$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43359, %struct.ScmObj* %args47235$ae43358$1)
store volatile %struct.ScmObj* %args47235$ae43358$2, %struct.ScmObj** %stackaddr$prim48498, align 8
%clofunc48499 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae43358)
musttail call tailcc void %clofunc48499(%struct.ScmObj* %ae43358, %struct.ScmObj* %args47235$ae43358$2)
ret void
}

define tailcc void @proc_clo$ae43358(%struct.ScmObj* %env$ae43358,%struct.ScmObj* %current_45args47215) {
%stackaddr$env-ref48500 = alloca %struct.ScmObj*, align 8
%k40457 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43358, i64 0)
store %struct.ScmObj* %k40457, %struct.ScmObj** %stackaddr$env-ref48500
%stackaddr$env-ref48501 = alloca %struct.ScmObj*, align 8
%lst40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43358, i64 1)
store %struct.ScmObj* %lst40174, %struct.ScmObj** %stackaddr$env-ref48501
%stackaddr$env-ref48502 = alloca %struct.ScmObj*, align 8
%v40173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43358, i64 2)
store %struct.ScmObj* %v40173, %struct.ScmObj** %stackaddr$env-ref48502
%stackaddr$prim48503 = alloca %struct.ScmObj*, align 8
%_95k40458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47215)
store volatile %struct.ScmObj* %_95k40458, %struct.ScmObj** %stackaddr$prim48503, align 8
%stackaddr$prim48504 = alloca %struct.ScmObj*, align 8
%current_45args47216 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47215)
store volatile %struct.ScmObj* %current_45args47216, %struct.ScmObj** %stackaddr$prim48504, align 8
%stackaddr$prim48505 = alloca %struct.ScmObj*, align 8
%anf_45bind40323 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47216)
store volatile %struct.ScmObj* %anf_45bind40323, %struct.ScmObj** %stackaddr$prim48505, align 8
%stackaddr$makeclosure48506 = alloca %struct.ScmObj*, align 8
%fptrToInt48507 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43374 to i64
%ae43374 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48507)
store volatile %struct.ScmObj* %ae43374, %struct.ScmObj** %stackaddr$makeclosure48506, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43374, %struct.ScmObj* %k40457, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43374, %struct.ScmObj* %lst40174, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43374, %struct.ScmObj* %v40173, i64 2)
%stackaddr$makeclosure48508 = alloca %struct.ScmObj*, align 8
%fptrToInt48509 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae43375 to i64
%ae43375 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48509)
store volatile %struct.ScmObj* %ae43375, %struct.ScmObj** %stackaddr$makeclosure48508, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae43375, %struct.ScmObj* %k40457, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae43375, %struct.ScmObj* %lst40174, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae43375, %struct.ScmObj* %v40173, i64 2)
%args47230$anf_45bind40323$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48510 = alloca %struct.ScmObj*, align 8
%args47230$anf_45bind40323$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43375, %struct.ScmObj* %args47230$anf_45bind40323$0)
store volatile %struct.ScmObj* %args47230$anf_45bind40323$1, %struct.ScmObj** %stackaddr$prim48510, align 8
%stackaddr$prim48511 = alloca %struct.ScmObj*, align 8
%args47230$anf_45bind40323$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43374, %struct.ScmObj* %args47230$anf_45bind40323$1)
store volatile %struct.ScmObj* %args47230$anf_45bind40323$2, %struct.ScmObj** %stackaddr$prim48511, align 8
%clofunc48512 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40323)
musttail call tailcc void %clofunc48512(%struct.ScmObj* %anf_45bind40323, %struct.ScmObj* %args47230$anf_45bind40323$2)
ret void
}

define tailcc void @proc_clo$ae43374(%struct.ScmObj* %env$ae43374,%struct.ScmObj* %current_45args47218) {
%stackaddr$env-ref48513 = alloca %struct.ScmObj*, align 8
%k40457 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43374, i64 0)
store %struct.ScmObj* %k40457, %struct.ScmObj** %stackaddr$env-ref48513
%stackaddr$env-ref48514 = alloca %struct.ScmObj*, align 8
%lst40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43374, i64 1)
store %struct.ScmObj* %lst40174, %struct.ScmObj** %stackaddr$env-ref48514
%stackaddr$env-ref48515 = alloca %struct.ScmObj*, align 8
%v40173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43374, i64 2)
store %struct.ScmObj* %v40173, %struct.ScmObj** %stackaddr$env-ref48515
%stackaddr$prim48516 = alloca %struct.ScmObj*, align 8
%_95k40459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47218)
store volatile %struct.ScmObj* %_95k40459, %struct.ScmObj** %stackaddr$prim48516, align 8
%stackaddr$prim48517 = alloca %struct.ScmObj*, align 8
%current_45args47219 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47218)
store volatile %struct.ScmObj* %current_45args47219, %struct.ScmObj** %stackaddr$prim48517, align 8
%stackaddr$prim48518 = alloca %struct.ScmObj*, align 8
%cc40175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47219)
store volatile %struct.ScmObj* %cc40175, %struct.ScmObj** %stackaddr$prim48518, align 8
%ae43483 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48519 = alloca %struct.ScmObj*, align 8
%anf_45bind40324 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43483)
store volatile %struct.ScmObj* %anf_45bind40324, %struct.ScmObj** %stackaddr$prim48519, align 8
%stackaddr$prim48520 = alloca %struct.ScmObj*, align 8
%anf_45bind40325 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40324)
store volatile %struct.ScmObj* %anf_45bind40325, %struct.ScmObj** %stackaddr$prim48520, align 8
%truthy$cmp48521 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40325)
%cmp$cmp48521 = icmp eq i64 %truthy$cmp48521, 1
br i1 %cmp$cmp48521, label %truebranch$cmp48521, label %falsebranch$cmp48521
truebranch$cmp48521:
%ae43487 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43488 = call %struct.ScmObj* @const_init_false()
%args47221$k40457$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48522 = alloca %struct.ScmObj*, align 8
%args47221$k40457$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43488, %struct.ScmObj* %args47221$k40457$0)
store volatile %struct.ScmObj* %args47221$k40457$1, %struct.ScmObj** %stackaddr$prim48522, align 8
%stackaddr$prim48523 = alloca %struct.ScmObj*, align 8
%args47221$k40457$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43487, %struct.ScmObj* %args47221$k40457$1)
store volatile %struct.ScmObj* %args47221$k40457$2, %struct.ScmObj** %stackaddr$prim48523, align 8
%clofunc48524 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40457)
musttail call tailcc void %clofunc48524(%struct.ScmObj* %k40457, %struct.ScmObj* %args47221$k40457$2)
ret void
falsebranch$cmp48521:
%ae43496 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48525 = alloca %struct.ScmObj*, align 8
%anf_45bind40326 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43496)
store volatile %struct.ScmObj* %anf_45bind40326, %struct.ScmObj** %stackaddr$prim48525, align 8
%stackaddr$prim48526 = alloca %struct.ScmObj*, align 8
%anf_45bind40327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40326)
store volatile %struct.ScmObj* %anf_45bind40327, %struct.ScmObj** %stackaddr$prim48526, align 8
%stackaddr$prim48527 = alloca %struct.ScmObj*, align 8
%anf_45bind40328 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40327, %struct.ScmObj* %v40173)
store volatile %struct.ScmObj* %anf_45bind40328, %struct.ScmObj** %stackaddr$prim48527, align 8
%truthy$cmp48528 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40328)
%cmp$cmp48528 = icmp eq i64 %truthy$cmp48528, 1
br i1 %cmp$cmp48528, label %truebranch$cmp48528, label %falsebranch$cmp48528
truebranch$cmp48528:
%ae43502 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48529 = alloca %struct.ScmObj*, align 8
%cpsprim40460 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43502)
store volatile %struct.ScmObj* %cpsprim40460, %struct.ScmObj** %stackaddr$prim48529, align 8
%ae43504 = call %struct.ScmObj* @const_init_int(i64 0)
%args47222$k40457$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48530 = alloca %struct.ScmObj*, align 8
%args47222$k40457$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40460, %struct.ScmObj* %args47222$k40457$0)
store volatile %struct.ScmObj* %args47222$k40457$1, %struct.ScmObj** %stackaddr$prim48530, align 8
%stackaddr$prim48531 = alloca %struct.ScmObj*, align 8
%args47222$k40457$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43504, %struct.ScmObj* %args47222$k40457$1)
store volatile %struct.ScmObj* %args47222$k40457$2, %struct.ScmObj** %stackaddr$prim48531, align 8
%clofunc48532 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40457)
musttail call tailcc void %clofunc48532(%struct.ScmObj* %k40457, %struct.ScmObj* %args47222$k40457$2)
ret void
falsebranch$cmp48528:
%ae43515 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48533 = alloca %struct.ScmObj*, align 8
%anf_45bind40329 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43515)
store volatile %struct.ScmObj* %anf_45bind40329, %struct.ScmObj** %stackaddr$prim48533, align 8
%stackaddr$prim48534 = alloca %struct.ScmObj*, align 8
%anf_45bind40330 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40329)
store volatile %struct.ScmObj* %anf_45bind40330, %struct.ScmObj** %stackaddr$prim48534, align 8
%ae43518 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48535 = alloca %struct.ScmObj*, align 8
%_95040177 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43518, %struct.ScmObj* %anf_45bind40330)
store volatile %struct.ScmObj* %_95040177, %struct.ScmObj** %stackaddr$prim48535, align 8
%args47223$cc40175$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48536 = alloca %struct.ScmObj*, align 8
%args47223$cc40175$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40175, %struct.ScmObj* %args47223$cc40175$0)
store volatile %struct.ScmObj* %args47223$cc40175$1, %struct.ScmObj** %stackaddr$prim48536, align 8
%stackaddr$prim48537 = alloca %struct.ScmObj*, align 8
%args47223$cc40175$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40457, %struct.ScmObj* %args47223$cc40175$1)
store volatile %struct.ScmObj* %args47223$cc40175$2, %struct.ScmObj** %stackaddr$prim48537, align 8
%clofunc48538 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40175)
musttail call tailcc void %clofunc48538(%struct.ScmObj* %cc40175, %struct.ScmObj* %args47223$cc40175$2)
ret void
}

define tailcc void @proc_clo$ae43375(%struct.ScmObj* %env$ae43375,%struct.ScmObj* %current_45args47224) {
%stackaddr$env-ref48539 = alloca %struct.ScmObj*, align 8
%k40457 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43375, i64 0)
store %struct.ScmObj* %k40457, %struct.ScmObj** %stackaddr$env-ref48539
%stackaddr$env-ref48540 = alloca %struct.ScmObj*, align 8
%lst40174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43375, i64 1)
store %struct.ScmObj* %lst40174, %struct.ScmObj** %stackaddr$env-ref48540
%stackaddr$env-ref48541 = alloca %struct.ScmObj*, align 8
%v40173 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae43375, i64 2)
store %struct.ScmObj* %v40173, %struct.ScmObj** %stackaddr$env-ref48541
%stackaddr$prim48542 = alloca %struct.ScmObj*, align 8
%_95k40459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47224)
store volatile %struct.ScmObj* %_95k40459, %struct.ScmObj** %stackaddr$prim48542, align 8
%stackaddr$prim48543 = alloca %struct.ScmObj*, align 8
%current_45args47225 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47224)
store volatile %struct.ScmObj* %current_45args47225, %struct.ScmObj** %stackaddr$prim48543, align 8
%stackaddr$prim48544 = alloca %struct.ScmObj*, align 8
%cc40175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47225)
store volatile %struct.ScmObj* %cc40175, %struct.ScmObj** %stackaddr$prim48544, align 8
%ae43377 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48545 = alloca %struct.ScmObj*, align 8
%anf_45bind40324 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43377)
store volatile %struct.ScmObj* %anf_45bind40324, %struct.ScmObj** %stackaddr$prim48545, align 8
%stackaddr$prim48546 = alloca %struct.ScmObj*, align 8
%anf_45bind40325 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40324)
store volatile %struct.ScmObj* %anf_45bind40325, %struct.ScmObj** %stackaddr$prim48546, align 8
%truthy$cmp48547 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40325)
%cmp$cmp48547 = icmp eq i64 %truthy$cmp48547, 1
br i1 %cmp$cmp48547, label %truebranch$cmp48547, label %falsebranch$cmp48547
truebranch$cmp48547:
%ae43381 = call %struct.ScmObj* @const_init_int(i64 0)
%ae43382 = call %struct.ScmObj* @const_init_false()
%args47227$k40457$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48548 = alloca %struct.ScmObj*, align 8
%args47227$k40457$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43382, %struct.ScmObj* %args47227$k40457$0)
store volatile %struct.ScmObj* %args47227$k40457$1, %struct.ScmObj** %stackaddr$prim48548, align 8
%stackaddr$prim48549 = alloca %struct.ScmObj*, align 8
%args47227$k40457$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43381, %struct.ScmObj* %args47227$k40457$1)
store volatile %struct.ScmObj* %args47227$k40457$2, %struct.ScmObj** %stackaddr$prim48549, align 8
%clofunc48550 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40457)
musttail call tailcc void %clofunc48550(%struct.ScmObj* %k40457, %struct.ScmObj* %args47227$k40457$2)
ret void
falsebranch$cmp48547:
%ae43390 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48551 = alloca %struct.ScmObj*, align 8
%anf_45bind40326 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43390)
store volatile %struct.ScmObj* %anf_45bind40326, %struct.ScmObj** %stackaddr$prim48551, align 8
%stackaddr$prim48552 = alloca %struct.ScmObj*, align 8
%anf_45bind40327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40326)
store volatile %struct.ScmObj* %anf_45bind40327, %struct.ScmObj** %stackaddr$prim48552, align 8
%stackaddr$prim48553 = alloca %struct.ScmObj*, align 8
%anf_45bind40328 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind40327, %struct.ScmObj* %v40173)
store volatile %struct.ScmObj* %anf_45bind40328, %struct.ScmObj** %stackaddr$prim48553, align 8
%truthy$cmp48554 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40328)
%cmp$cmp48554 = icmp eq i64 %truthy$cmp48554, 1
br i1 %cmp$cmp48554, label %truebranch$cmp48554, label %falsebranch$cmp48554
truebranch$cmp48554:
%ae43396 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48555 = alloca %struct.ScmObj*, align 8
%cpsprim40460 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43396)
store volatile %struct.ScmObj* %cpsprim40460, %struct.ScmObj** %stackaddr$prim48555, align 8
%ae43398 = call %struct.ScmObj* @const_init_int(i64 0)
%args47228$k40457$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48556 = alloca %struct.ScmObj*, align 8
%args47228$k40457$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40460, %struct.ScmObj* %args47228$k40457$0)
store volatile %struct.ScmObj* %args47228$k40457$1, %struct.ScmObj** %stackaddr$prim48556, align 8
%stackaddr$prim48557 = alloca %struct.ScmObj*, align 8
%args47228$k40457$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae43398, %struct.ScmObj* %args47228$k40457$1)
store volatile %struct.ScmObj* %args47228$k40457$2, %struct.ScmObj** %stackaddr$prim48557, align 8
%clofunc48558 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40457)
musttail call tailcc void %clofunc48558(%struct.ScmObj* %k40457, %struct.ScmObj* %args47228$k40457$2)
ret void
falsebranch$cmp48554:
%ae43409 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48559 = alloca %struct.ScmObj*, align 8
%anf_45bind40329 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43409)
store volatile %struct.ScmObj* %anf_45bind40329, %struct.ScmObj** %stackaddr$prim48559, align 8
%stackaddr$prim48560 = alloca %struct.ScmObj*, align 8
%anf_45bind40330 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40329)
store volatile %struct.ScmObj* %anf_45bind40330, %struct.ScmObj** %stackaddr$prim48560, align 8
%ae43412 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48561 = alloca %struct.ScmObj*, align 8
%_95040177 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40174, %struct.ScmObj* %ae43412, %struct.ScmObj* %anf_45bind40330)
store volatile %struct.ScmObj* %_95040177, %struct.ScmObj** %stackaddr$prim48561, align 8
%args47229$cc40175$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48562 = alloca %struct.ScmObj*, align 8
%args47229$cc40175$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40175, %struct.ScmObj* %args47229$cc40175$0)
store volatile %struct.ScmObj* %args47229$cc40175$1, %struct.ScmObj** %stackaddr$prim48562, align 8
%stackaddr$prim48563 = alloca %struct.ScmObj*, align 8
%args47229$cc40175$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40457, %struct.ScmObj* %args47229$cc40175$1)
store volatile %struct.ScmObj* %args47229$cc40175$2, %struct.ScmObj** %stackaddr$prim48563, align 8
%clofunc48564 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40175)
musttail call tailcc void %clofunc48564(%struct.ScmObj* %cc40175, %struct.ScmObj* %args47229$cc40175$2)
ret void
}

define tailcc void @proc_clo$ae43360(%struct.ScmObj* %env$ae43360,%struct.ScmObj* %current_45args47231) {
%stackaddr$prim48565 = alloca %struct.ScmObj*, align 8
%k40461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47231)
store volatile %struct.ScmObj* %k40461, %struct.ScmObj** %stackaddr$prim48565, align 8
%stackaddr$prim48566 = alloca %struct.ScmObj*, align 8
%current_45args47232 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47231)
store volatile %struct.ScmObj* %current_45args47232, %struct.ScmObj** %stackaddr$prim48566, align 8
%stackaddr$prim48567 = alloca %struct.ScmObj*, align 8
%u40176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47232)
store volatile %struct.ScmObj* %u40176, %struct.ScmObj** %stackaddr$prim48567, align 8
%args47234$u40176$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48568 = alloca %struct.ScmObj*, align 8
%args47234$u40176$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40176, %struct.ScmObj* %args47234$u40176$0)
store volatile %struct.ScmObj* %args47234$u40176$1, %struct.ScmObj** %stackaddr$prim48568, align 8
%stackaddr$prim48569 = alloca %struct.ScmObj*, align 8
%args47234$u40176$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40461, %struct.ScmObj* %args47234$u40176$1)
store volatile %struct.ScmObj* %args47234$u40176$2, %struct.ScmObj** %stackaddr$prim48569, align 8
%clofunc48570 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40176)
musttail call tailcc void %clofunc48570(%struct.ScmObj* %u40176, %struct.ScmObj* %args47234$u40176$2)
ret void
}

define tailcc void @proc_clo$ae42819(%struct.ScmObj* %env$ae42819,%struct.ScmObj* %current_45args47237) {
%stackaddr$prim48571 = alloca %struct.ScmObj*, align 8
%k40462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47237)
store volatile %struct.ScmObj* %k40462, %struct.ScmObj** %stackaddr$prim48571, align 8
%stackaddr$prim48572 = alloca %struct.ScmObj*, align 8
%current_45args47238 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47237)
store volatile %struct.ScmObj* %current_45args47238, %struct.ScmObj** %stackaddr$prim48572, align 8
%stackaddr$prim48573 = alloca %struct.ScmObj*, align 8
%lst40180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47238)
store volatile %struct.ScmObj* %lst40180, %struct.ScmObj** %stackaddr$prim48573, align 8
%stackaddr$prim48574 = alloca %struct.ScmObj*, align 8
%current_45args47239 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47238)
store volatile %struct.ScmObj* %current_45args47239, %struct.ScmObj** %stackaddr$prim48574, align 8
%stackaddr$prim48575 = alloca %struct.ScmObj*, align 8
%n40179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47239)
store volatile %struct.ScmObj* %n40179, %struct.ScmObj** %stackaddr$prim48575, align 8
%ae42820 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48576 = alloca %struct.ScmObj*, align 8
%n40182 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42820, %struct.ScmObj* %n40179)
store volatile %struct.ScmObj* %n40182, %struct.ScmObj** %stackaddr$prim48576, align 8
%ae42822 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48577 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42822, %struct.ScmObj* %lst40180)
store volatile %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$prim48577, align 8
%stackaddr$makeclosure48578 = alloca %struct.ScmObj*, align 8
%fptrToInt48579 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42824 to i64
%ae42824 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48579)
store volatile %struct.ScmObj* %ae42824, %struct.ScmObj** %stackaddr$makeclosure48578, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42824, %struct.ScmObj* %n40182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42824, %struct.ScmObj* %lst40181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42824, %struct.ScmObj* %k40462, i64 2)
%ae42825 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48580 = alloca %struct.ScmObj*, align 8
%fptrToInt48581 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42826 to i64
%ae42826 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48581)
store volatile %struct.ScmObj* %ae42826, %struct.ScmObj** %stackaddr$makeclosure48580, align 8
%args47259$ae42824$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48582 = alloca %struct.ScmObj*, align 8
%args47259$ae42824$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42826, %struct.ScmObj* %args47259$ae42824$0)
store volatile %struct.ScmObj* %args47259$ae42824$1, %struct.ScmObj** %stackaddr$prim48582, align 8
%stackaddr$prim48583 = alloca %struct.ScmObj*, align 8
%args47259$ae42824$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42825, %struct.ScmObj* %args47259$ae42824$1)
store volatile %struct.ScmObj* %args47259$ae42824$2, %struct.ScmObj** %stackaddr$prim48583, align 8
%clofunc48584 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42824)
musttail call tailcc void %clofunc48584(%struct.ScmObj* %ae42824, %struct.ScmObj* %args47259$ae42824$2)
ret void
}

define tailcc void @proc_clo$ae42824(%struct.ScmObj* %env$ae42824,%struct.ScmObj* %current_45args47241) {
%stackaddr$env-ref48585 = alloca %struct.ScmObj*, align 8
%n40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42824, i64 0)
store %struct.ScmObj* %n40182, %struct.ScmObj** %stackaddr$env-ref48585
%stackaddr$env-ref48586 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42824, i64 1)
store %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$env-ref48586
%stackaddr$env-ref48587 = alloca %struct.ScmObj*, align 8
%k40462 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42824, i64 2)
store %struct.ScmObj* %k40462, %struct.ScmObj** %stackaddr$env-ref48587
%stackaddr$prim48588 = alloca %struct.ScmObj*, align 8
%_95k40463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47241)
store volatile %struct.ScmObj* %_95k40463, %struct.ScmObj** %stackaddr$prim48588, align 8
%stackaddr$prim48589 = alloca %struct.ScmObj*, align 8
%current_45args47242 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47241)
store volatile %struct.ScmObj* %current_45args47242, %struct.ScmObj** %stackaddr$prim48589, align 8
%stackaddr$prim48590 = alloca %struct.ScmObj*, align 8
%anf_45bind40316 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47242)
store volatile %struct.ScmObj* %anf_45bind40316, %struct.ScmObj** %stackaddr$prim48590, align 8
%stackaddr$makeclosure48591 = alloca %struct.ScmObj*, align 8
%fptrToInt48592 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42840 to i64
%ae42840 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48592)
store volatile %struct.ScmObj* %ae42840, %struct.ScmObj** %stackaddr$makeclosure48591, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42840, %struct.ScmObj* %n40182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42840, %struct.ScmObj* %lst40181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42840, %struct.ScmObj* %k40462, i64 2)
%stackaddr$makeclosure48593 = alloca %struct.ScmObj*, align 8
%fptrToInt48594 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42841 to i64
%ae42841 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48594)
store volatile %struct.ScmObj* %ae42841, %struct.ScmObj** %stackaddr$makeclosure48593, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42841, %struct.ScmObj* %n40182, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42841, %struct.ScmObj* %lst40181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42841, %struct.ScmObj* %k40462, i64 2)
%args47254$anf_45bind40316$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48595 = alloca %struct.ScmObj*, align 8
%args47254$anf_45bind40316$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42841, %struct.ScmObj* %args47254$anf_45bind40316$0)
store volatile %struct.ScmObj* %args47254$anf_45bind40316$1, %struct.ScmObj** %stackaddr$prim48595, align 8
%stackaddr$prim48596 = alloca %struct.ScmObj*, align 8
%args47254$anf_45bind40316$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42840, %struct.ScmObj* %args47254$anf_45bind40316$1)
store volatile %struct.ScmObj* %args47254$anf_45bind40316$2, %struct.ScmObj** %stackaddr$prim48596, align 8
%clofunc48597 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40316)
musttail call tailcc void %clofunc48597(%struct.ScmObj* %anf_45bind40316, %struct.ScmObj* %args47254$anf_45bind40316$2)
ret void
}

define tailcc void @proc_clo$ae42840(%struct.ScmObj* %env$ae42840,%struct.ScmObj* %current_45args47244) {
%stackaddr$env-ref48598 = alloca %struct.ScmObj*, align 8
%n40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42840, i64 0)
store %struct.ScmObj* %n40182, %struct.ScmObj** %stackaddr$env-ref48598
%stackaddr$env-ref48599 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42840, i64 1)
store %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$env-ref48599
%stackaddr$env-ref48600 = alloca %struct.ScmObj*, align 8
%k40462 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42840, i64 2)
store %struct.ScmObj* %k40462, %struct.ScmObj** %stackaddr$env-ref48600
%stackaddr$prim48601 = alloca %struct.ScmObj*, align 8
%_95k40464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47244)
store volatile %struct.ScmObj* %_95k40464, %struct.ScmObj** %stackaddr$prim48601, align 8
%stackaddr$prim48602 = alloca %struct.ScmObj*, align 8
%current_45args47245 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47244)
store volatile %struct.ScmObj* %current_45args47245, %struct.ScmObj** %stackaddr$prim48602, align 8
%stackaddr$prim48603 = alloca %struct.ScmObj*, align 8
%cc40183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47245)
store volatile %struct.ScmObj* %cc40183, %struct.ScmObj** %stackaddr$prim48603, align 8
%ae42983 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48604 = alloca %struct.ScmObj*, align 8
%anf_45bind40317 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42983)
store volatile %struct.ScmObj* %anf_45bind40317, %struct.ScmObj** %stackaddr$prim48604, align 8
%ae42984 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48605 = alloca %struct.ScmObj*, align 8
%anf_45bind40318 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42984, %struct.ScmObj* %anf_45bind40317)
store volatile %struct.ScmObj* %anf_45bind40318, %struct.ScmObj** %stackaddr$prim48605, align 8
%truthy$cmp48606 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40318)
%cmp$cmp48606 = icmp eq i64 %truthy$cmp48606, 1
br i1 %cmp$cmp48606, label %truebranch$cmp48606, label %falsebranch$cmp48606
truebranch$cmp48606:
%ae42988 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48607 = alloca %struct.ScmObj*, align 8
%cpsprim40465 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42988)
store volatile %struct.ScmObj* %cpsprim40465, %struct.ScmObj** %stackaddr$prim48607, align 8
%ae42990 = call %struct.ScmObj* @const_init_int(i64 0)
%args47247$k40462$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48608 = alloca %struct.ScmObj*, align 8
%args47247$k40462$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40465, %struct.ScmObj* %args47247$k40462$0)
store volatile %struct.ScmObj* %args47247$k40462$1, %struct.ScmObj** %stackaddr$prim48608, align 8
%stackaddr$prim48609 = alloca %struct.ScmObj*, align 8
%args47247$k40462$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42990, %struct.ScmObj* %args47247$k40462$1)
store volatile %struct.ScmObj* %args47247$k40462$2, %struct.ScmObj** %stackaddr$prim48609, align 8
%clofunc48610 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40462)
musttail call tailcc void %clofunc48610(%struct.ScmObj* %k40462, %struct.ScmObj* %args47247$k40462$2)
ret void
falsebranch$cmp48606:
%ae43001 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48611 = alloca %struct.ScmObj*, align 8
%anf_45bind40319 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae43001)
store volatile %struct.ScmObj* %anf_45bind40319, %struct.ScmObj** %stackaddr$prim48611, align 8
%stackaddr$prim48612 = alloca %struct.ScmObj*, align 8
%anf_45bind40320 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40319)
store volatile %struct.ScmObj* %anf_45bind40320, %struct.ScmObj** %stackaddr$prim48612, align 8
%ae43004 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48613 = alloca %struct.ScmObj*, align 8
%_95040186 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae43004, %struct.ScmObj* %anf_45bind40320)
store volatile %struct.ScmObj* %_95040186, %struct.ScmObj** %stackaddr$prim48613, align 8
%ae43007 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48614 = alloca %struct.ScmObj*, align 8
%anf_45bind40321 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40182, %struct.ScmObj* %ae43007)
store volatile %struct.ScmObj* %anf_45bind40321, %struct.ScmObj** %stackaddr$prim48614, align 8
%ae43009 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48615 = alloca %struct.ScmObj*, align 8
%anf_45bind40322 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40321, %struct.ScmObj* %ae43009)
store volatile %struct.ScmObj* %anf_45bind40322, %struct.ScmObj** %stackaddr$prim48615, align 8
%ae43011 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48616 = alloca %struct.ScmObj*, align 8
%_95140185 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40182, %struct.ScmObj* %ae43011, %struct.ScmObj* %anf_45bind40322)
store volatile %struct.ScmObj* %_95140185, %struct.ScmObj** %stackaddr$prim48616, align 8
%args47248$cc40183$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48617 = alloca %struct.ScmObj*, align 8
%args47248$cc40183$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40183, %struct.ScmObj* %args47248$cc40183$0)
store volatile %struct.ScmObj* %args47248$cc40183$1, %struct.ScmObj** %stackaddr$prim48617, align 8
%stackaddr$prim48618 = alloca %struct.ScmObj*, align 8
%args47248$cc40183$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40462, %struct.ScmObj* %args47248$cc40183$1)
store volatile %struct.ScmObj* %args47248$cc40183$2, %struct.ScmObj** %stackaddr$prim48618, align 8
%clofunc48619 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40183)
musttail call tailcc void %clofunc48619(%struct.ScmObj* %cc40183, %struct.ScmObj* %args47248$cc40183$2)
ret void
}

define tailcc void @proc_clo$ae42841(%struct.ScmObj* %env$ae42841,%struct.ScmObj* %current_45args47249) {
%stackaddr$env-ref48620 = alloca %struct.ScmObj*, align 8
%n40182 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42841, i64 0)
store %struct.ScmObj* %n40182, %struct.ScmObj** %stackaddr$env-ref48620
%stackaddr$env-ref48621 = alloca %struct.ScmObj*, align 8
%lst40181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42841, i64 1)
store %struct.ScmObj* %lst40181, %struct.ScmObj** %stackaddr$env-ref48621
%stackaddr$env-ref48622 = alloca %struct.ScmObj*, align 8
%k40462 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42841, i64 2)
store %struct.ScmObj* %k40462, %struct.ScmObj** %stackaddr$env-ref48622
%stackaddr$prim48623 = alloca %struct.ScmObj*, align 8
%_95k40464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47249)
store volatile %struct.ScmObj* %_95k40464, %struct.ScmObj** %stackaddr$prim48623, align 8
%stackaddr$prim48624 = alloca %struct.ScmObj*, align 8
%current_45args47250 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47249)
store volatile %struct.ScmObj* %current_45args47250, %struct.ScmObj** %stackaddr$prim48624, align 8
%stackaddr$prim48625 = alloca %struct.ScmObj*, align 8
%cc40183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47250)
store volatile %struct.ScmObj* %cc40183, %struct.ScmObj** %stackaddr$prim48625, align 8
%ae42843 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48626 = alloca %struct.ScmObj*, align 8
%anf_45bind40317 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42843)
store volatile %struct.ScmObj* %anf_45bind40317, %struct.ScmObj** %stackaddr$prim48626, align 8
%ae42844 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48627 = alloca %struct.ScmObj*, align 8
%anf_45bind40318 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae42844, %struct.ScmObj* %anf_45bind40317)
store volatile %struct.ScmObj* %anf_45bind40318, %struct.ScmObj** %stackaddr$prim48627, align 8
%truthy$cmp48628 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40318)
%cmp$cmp48628 = icmp eq i64 %truthy$cmp48628, 1
br i1 %cmp$cmp48628, label %truebranch$cmp48628, label %falsebranch$cmp48628
truebranch$cmp48628:
%ae42848 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48629 = alloca %struct.ScmObj*, align 8
%cpsprim40465 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42848)
store volatile %struct.ScmObj* %cpsprim40465, %struct.ScmObj** %stackaddr$prim48629, align 8
%ae42850 = call %struct.ScmObj* @const_init_int(i64 0)
%args47252$k40462$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48630 = alloca %struct.ScmObj*, align 8
%args47252$k40462$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40465, %struct.ScmObj* %args47252$k40462$0)
store volatile %struct.ScmObj* %args47252$k40462$1, %struct.ScmObj** %stackaddr$prim48630, align 8
%stackaddr$prim48631 = alloca %struct.ScmObj*, align 8
%args47252$k40462$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42850, %struct.ScmObj* %args47252$k40462$1)
store volatile %struct.ScmObj* %args47252$k40462$2, %struct.ScmObj** %stackaddr$prim48631, align 8
%clofunc48632 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40462)
musttail call tailcc void %clofunc48632(%struct.ScmObj* %k40462, %struct.ScmObj* %args47252$k40462$2)
ret void
falsebranch$cmp48628:
%ae42861 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48633 = alloca %struct.ScmObj*, align 8
%anf_45bind40319 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42861)
store volatile %struct.ScmObj* %anf_45bind40319, %struct.ScmObj** %stackaddr$prim48633, align 8
%stackaddr$prim48634 = alloca %struct.ScmObj*, align 8
%anf_45bind40320 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40319)
store volatile %struct.ScmObj* %anf_45bind40320, %struct.ScmObj** %stackaddr$prim48634, align 8
%ae42864 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48635 = alloca %struct.ScmObj*, align 8
%_95040186 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst40181, %struct.ScmObj* %ae42864, %struct.ScmObj* %anf_45bind40320)
store volatile %struct.ScmObj* %_95040186, %struct.ScmObj** %stackaddr$prim48635, align 8
%ae42867 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48636 = alloca %struct.ScmObj*, align 8
%anf_45bind40321 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42867)
store volatile %struct.ScmObj* %anf_45bind40321, %struct.ScmObj** %stackaddr$prim48636, align 8
%ae42869 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48637 = alloca %struct.ScmObj*, align 8
%anf_45bind40322 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40321, %struct.ScmObj* %ae42869)
store volatile %struct.ScmObj* %anf_45bind40322, %struct.ScmObj** %stackaddr$prim48637, align 8
%ae42871 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48638 = alloca %struct.ScmObj*, align 8
%_95140185 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n40182, %struct.ScmObj* %ae42871, %struct.ScmObj* %anf_45bind40322)
store volatile %struct.ScmObj* %_95140185, %struct.ScmObj** %stackaddr$prim48638, align 8
%args47253$cc40183$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48639 = alloca %struct.ScmObj*, align 8
%args47253$cc40183$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40183, %struct.ScmObj* %args47253$cc40183$0)
store volatile %struct.ScmObj* %args47253$cc40183$1, %struct.ScmObj** %stackaddr$prim48639, align 8
%stackaddr$prim48640 = alloca %struct.ScmObj*, align 8
%args47253$cc40183$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40462, %struct.ScmObj* %args47253$cc40183$1)
store volatile %struct.ScmObj* %args47253$cc40183$2, %struct.ScmObj** %stackaddr$prim48640, align 8
%clofunc48641 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40183)
musttail call tailcc void %clofunc48641(%struct.ScmObj* %cc40183, %struct.ScmObj* %args47253$cc40183$2)
ret void
}

define tailcc void @proc_clo$ae42826(%struct.ScmObj* %env$ae42826,%struct.ScmObj* %current_45args47255) {
%stackaddr$prim48642 = alloca %struct.ScmObj*, align 8
%k40466 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47255)
store volatile %struct.ScmObj* %k40466, %struct.ScmObj** %stackaddr$prim48642, align 8
%stackaddr$prim48643 = alloca %struct.ScmObj*, align 8
%current_45args47256 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47255)
store volatile %struct.ScmObj* %current_45args47256, %struct.ScmObj** %stackaddr$prim48643, align 8
%stackaddr$prim48644 = alloca %struct.ScmObj*, align 8
%u40184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47256)
store volatile %struct.ScmObj* %u40184, %struct.ScmObj** %stackaddr$prim48644, align 8
%args47258$u40184$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48645 = alloca %struct.ScmObj*, align 8
%args47258$u40184$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u40184, %struct.ScmObj* %args47258$u40184$0)
store volatile %struct.ScmObj* %args47258$u40184$1, %struct.ScmObj** %stackaddr$prim48645, align 8
%stackaddr$prim48646 = alloca %struct.ScmObj*, align 8
%args47258$u40184$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40466, %struct.ScmObj* %args47258$u40184$1)
store volatile %struct.ScmObj* %args47258$u40184$2, %struct.ScmObj** %stackaddr$prim48646, align 8
%clofunc48647 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u40184)
musttail call tailcc void %clofunc48647(%struct.ScmObj* %u40184, %struct.ScmObj* %args47258$u40184$2)
ret void
}

define tailcc void @proc_clo$ae42403(%struct.ScmObj* %env$ae42403,%struct.ScmObj* %current_45args47261) {
%stackaddr$prim48648 = alloca %struct.ScmObj*, align 8
%k40467 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47261)
store volatile %struct.ScmObj* %k40467, %struct.ScmObj** %stackaddr$prim48648, align 8
%stackaddr$prim48649 = alloca %struct.ScmObj*, align 8
%current_45args47262 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47261)
store volatile %struct.ScmObj* %current_45args47262, %struct.ScmObj** %stackaddr$prim48649, align 8
%stackaddr$prim48650 = alloca %struct.ScmObj*, align 8
%a40188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47262)
store volatile %struct.ScmObj* %a40188, %struct.ScmObj** %stackaddr$prim48650, align 8
%ae42404 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim48651 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae42404, %struct.ScmObj* %a40188)
store volatile %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$prim48651, align 8
%stackaddr$makeclosure48652 = alloca %struct.ScmObj*, align 8
%fptrToInt48653 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42406 to i64
%ae42406 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48653)
store volatile %struct.ScmObj* %ae42406, %struct.ScmObj** %stackaddr$makeclosure48652, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42406, %struct.ScmObj* %k40467, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42406, %struct.ScmObj* %a40189, i64 1)
%ae42407 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48654 = alloca %struct.ScmObj*, align 8
%fptrToInt48655 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42408 to i64
%ae42408 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48655)
store volatile %struct.ScmObj* %ae42408, %struct.ScmObj** %stackaddr$makeclosure48654, align 8
%args47284$ae42406$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48656 = alloca %struct.ScmObj*, align 8
%args47284$ae42406$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42408, %struct.ScmObj* %args47284$ae42406$0)
store volatile %struct.ScmObj* %args47284$ae42406$1, %struct.ScmObj** %stackaddr$prim48656, align 8
%stackaddr$prim48657 = alloca %struct.ScmObj*, align 8
%args47284$ae42406$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42407, %struct.ScmObj* %args47284$ae42406$1)
store volatile %struct.ScmObj* %args47284$ae42406$2, %struct.ScmObj** %stackaddr$prim48657, align 8
%clofunc48658 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae42406)
musttail call tailcc void %clofunc48658(%struct.ScmObj* %ae42406, %struct.ScmObj* %args47284$ae42406$2)
ret void
}

define tailcc void @proc_clo$ae42406(%struct.ScmObj* %env$ae42406,%struct.ScmObj* %current_45args47264) {
%stackaddr$env-ref48659 = alloca %struct.ScmObj*, align 8
%k40467 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42406, i64 0)
store %struct.ScmObj* %k40467, %struct.ScmObj** %stackaddr$env-ref48659
%stackaddr$env-ref48660 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42406, i64 1)
store %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$env-ref48660
%stackaddr$prim48661 = alloca %struct.ScmObj*, align 8
%_95k40468 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47264)
store volatile %struct.ScmObj* %_95k40468, %struct.ScmObj** %stackaddr$prim48661, align 8
%stackaddr$prim48662 = alloca %struct.ScmObj*, align 8
%current_45args47265 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47264)
store volatile %struct.ScmObj* %current_45args47265, %struct.ScmObj** %stackaddr$prim48662, align 8
%stackaddr$prim48663 = alloca %struct.ScmObj*, align 8
%anf_45bind40308 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47265)
store volatile %struct.ScmObj* %anf_45bind40308, %struct.ScmObj** %stackaddr$prim48663, align 8
%stackaddr$makeclosure48664 = alloca %struct.ScmObj*, align 8
%fptrToInt48665 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42425 to i64
%ae42425 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48665)
store volatile %struct.ScmObj* %ae42425, %struct.ScmObj** %stackaddr$makeclosure48664, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42425, %struct.ScmObj* %k40467, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42425, %struct.ScmObj* %a40189, i64 1)
%stackaddr$makeclosure48666 = alloca %struct.ScmObj*, align 8
%fptrToInt48667 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42426 to i64
%ae42426 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48667)
store volatile %struct.ScmObj* %ae42426, %struct.ScmObj** %stackaddr$makeclosure48666, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42426, %struct.ScmObj* %k40467, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42426, %struct.ScmObj* %a40189, i64 1)
%args47279$anf_45bind40308$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48668 = alloca %struct.ScmObj*, align 8
%args47279$anf_45bind40308$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42426, %struct.ScmObj* %args47279$anf_45bind40308$0)
store volatile %struct.ScmObj* %args47279$anf_45bind40308$1, %struct.ScmObj** %stackaddr$prim48668, align 8
%stackaddr$prim48669 = alloca %struct.ScmObj*, align 8
%args47279$anf_45bind40308$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42425, %struct.ScmObj* %args47279$anf_45bind40308$1)
store volatile %struct.ScmObj* %args47279$anf_45bind40308$2, %struct.ScmObj** %stackaddr$prim48669, align 8
%clofunc48670 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40308)
musttail call tailcc void %clofunc48670(%struct.ScmObj* %anf_45bind40308, %struct.ScmObj* %args47279$anf_45bind40308$2)
ret void
}

define tailcc void @proc_clo$ae42425(%struct.ScmObj* %env$ae42425,%struct.ScmObj* %current_45args47267) {
%stackaddr$env-ref48671 = alloca %struct.ScmObj*, align 8
%k40467 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42425, i64 0)
store %struct.ScmObj* %k40467, %struct.ScmObj** %stackaddr$env-ref48671
%stackaddr$env-ref48672 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42425, i64 1)
store %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$env-ref48672
%stackaddr$prim48673 = alloca %struct.ScmObj*, align 8
%_95k40469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47267)
store volatile %struct.ScmObj* %_95k40469, %struct.ScmObj** %stackaddr$prim48673, align 8
%stackaddr$prim48674 = alloca %struct.ScmObj*, align 8
%current_45args47268 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47267)
store volatile %struct.ScmObj* %current_45args47268, %struct.ScmObj** %stackaddr$prim48674, align 8
%stackaddr$prim48675 = alloca %struct.ScmObj*, align 8
%cc40190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47268)
store volatile %struct.ScmObj* %cc40190, %struct.ScmObj** %stackaddr$prim48675, align 8
%ae42541 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48676 = alloca %struct.ScmObj*, align 8
%anf_45bind40309 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42541)
store volatile %struct.ScmObj* %anf_45bind40309, %struct.ScmObj** %stackaddr$prim48676, align 8
%stackaddr$prim48677 = alloca %struct.ScmObj*, align 8
%anf_45bind40310 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40309)
store volatile %struct.ScmObj* %anf_45bind40310, %struct.ScmObj** %stackaddr$prim48677, align 8
%truthy$cmp48678 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40310)
%cmp$cmp48678 = icmp eq i64 %truthy$cmp48678, 1
br i1 %cmp$cmp48678, label %truebranch$cmp48678, label %falsebranch$cmp48678
truebranch$cmp48678:
%ae42545 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42546 = call %struct.ScmObj* @const_init_true()
%args47270$k40467$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48679 = alloca %struct.ScmObj*, align 8
%args47270$k40467$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42546, %struct.ScmObj* %args47270$k40467$0)
store volatile %struct.ScmObj* %args47270$k40467$1, %struct.ScmObj** %stackaddr$prim48679, align 8
%stackaddr$prim48680 = alloca %struct.ScmObj*, align 8
%args47270$k40467$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42545, %struct.ScmObj* %args47270$k40467$1)
store volatile %struct.ScmObj* %args47270$k40467$2, %struct.ScmObj** %stackaddr$prim48680, align 8
%clofunc48681 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40467)
musttail call tailcc void %clofunc48681(%struct.ScmObj* %k40467, %struct.ScmObj* %args47270$k40467$2)
ret void
falsebranch$cmp48678:
%ae42554 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48682 = alloca %struct.ScmObj*, align 8
%anf_45bind40311 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42554)
store volatile %struct.ScmObj* %anf_45bind40311, %struct.ScmObj** %stackaddr$prim48682, align 8
%stackaddr$prim48683 = alloca %struct.ScmObj*, align 8
%anf_45bind40312 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40311)
store volatile %struct.ScmObj* %anf_45bind40312, %struct.ScmObj** %stackaddr$prim48683, align 8
%truthy$cmp48684 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40312)
%cmp$cmp48684 = icmp eq i64 %truthy$cmp48684, 1
br i1 %cmp$cmp48684, label %truebranch$cmp48684, label %falsebranch$cmp48684
truebranch$cmp48684:
%ae42558 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48685 = alloca %struct.ScmObj*, align 8
%anf_45bind40313 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42558)
store volatile %struct.ScmObj* %anf_45bind40313, %struct.ScmObj** %stackaddr$prim48685, align 8
%stackaddr$prim48686 = alloca %struct.ScmObj*, align 8
%b40192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40313)
store volatile %struct.ScmObj* %b40192, %struct.ScmObj** %stackaddr$prim48686, align 8
%ae42561 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48687 = alloca %struct.ScmObj*, align 8
%anf_45bind40314 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42561)
store volatile %struct.ScmObj* %anf_45bind40314, %struct.ScmObj** %stackaddr$prim48687, align 8
%stackaddr$prim48688 = alloca %struct.ScmObj*, align 8
%anf_45bind40315 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40314)
store volatile %struct.ScmObj* %anf_45bind40315, %struct.ScmObj** %stackaddr$prim48688, align 8
%ae42564 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48689 = alloca %struct.ScmObj*, align 8
%_95040193 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42564, %struct.ScmObj* %anf_45bind40315)
store volatile %struct.ScmObj* %_95040193, %struct.ScmObj** %stackaddr$prim48689, align 8
%args47271$cc40190$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48690 = alloca %struct.ScmObj*, align 8
%args47271$cc40190$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40190, %struct.ScmObj* %args47271$cc40190$0)
store volatile %struct.ScmObj* %args47271$cc40190$1, %struct.ScmObj** %stackaddr$prim48690, align 8
%stackaddr$prim48691 = alloca %struct.ScmObj*, align 8
%args47271$cc40190$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40467, %struct.ScmObj* %args47271$cc40190$1)
store volatile %struct.ScmObj* %args47271$cc40190$2, %struct.ScmObj** %stackaddr$prim48691, align 8
%clofunc48692 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40190)
musttail call tailcc void %clofunc48692(%struct.ScmObj* %cc40190, %struct.ScmObj* %args47271$cc40190$2)
ret void
falsebranch$cmp48684:
%ae42597 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42598 = call %struct.ScmObj* @const_init_false()
%args47272$k40467$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48693 = alloca %struct.ScmObj*, align 8
%args47272$k40467$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42598, %struct.ScmObj* %args47272$k40467$0)
store volatile %struct.ScmObj* %args47272$k40467$1, %struct.ScmObj** %stackaddr$prim48693, align 8
%stackaddr$prim48694 = alloca %struct.ScmObj*, align 8
%args47272$k40467$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42597, %struct.ScmObj* %args47272$k40467$1)
store volatile %struct.ScmObj* %args47272$k40467$2, %struct.ScmObj** %stackaddr$prim48694, align 8
%clofunc48695 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40467)
musttail call tailcc void %clofunc48695(%struct.ScmObj* %k40467, %struct.ScmObj* %args47272$k40467$2)
ret void
}

define tailcc void @proc_clo$ae42426(%struct.ScmObj* %env$ae42426,%struct.ScmObj* %current_45args47273) {
%stackaddr$env-ref48696 = alloca %struct.ScmObj*, align 8
%k40467 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42426, i64 0)
store %struct.ScmObj* %k40467, %struct.ScmObj** %stackaddr$env-ref48696
%stackaddr$env-ref48697 = alloca %struct.ScmObj*, align 8
%a40189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42426, i64 1)
store %struct.ScmObj* %a40189, %struct.ScmObj** %stackaddr$env-ref48697
%stackaddr$prim48698 = alloca %struct.ScmObj*, align 8
%_95k40469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47273)
store volatile %struct.ScmObj* %_95k40469, %struct.ScmObj** %stackaddr$prim48698, align 8
%stackaddr$prim48699 = alloca %struct.ScmObj*, align 8
%current_45args47274 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47273)
store volatile %struct.ScmObj* %current_45args47274, %struct.ScmObj** %stackaddr$prim48699, align 8
%stackaddr$prim48700 = alloca %struct.ScmObj*, align 8
%cc40190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47274)
store volatile %struct.ScmObj* %cc40190, %struct.ScmObj** %stackaddr$prim48700, align 8
%ae42428 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48701 = alloca %struct.ScmObj*, align 8
%anf_45bind40309 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42428)
store volatile %struct.ScmObj* %anf_45bind40309, %struct.ScmObj** %stackaddr$prim48701, align 8
%stackaddr$prim48702 = alloca %struct.ScmObj*, align 8
%anf_45bind40310 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind40309)
store volatile %struct.ScmObj* %anf_45bind40310, %struct.ScmObj** %stackaddr$prim48702, align 8
%truthy$cmp48703 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40310)
%cmp$cmp48703 = icmp eq i64 %truthy$cmp48703, 1
br i1 %cmp$cmp48703, label %truebranch$cmp48703, label %falsebranch$cmp48703
truebranch$cmp48703:
%ae42432 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42433 = call %struct.ScmObj* @const_init_true()
%args47276$k40467$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48704 = alloca %struct.ScmObj*, align 8
%args47276$k40467$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42433, %struct.ScmObj* %args47276$k40467$0)
store volatile %struct.ScmObj* %args47276$k40467$1, %struct.ScmObj** %stackaddr$prim48704, align 8
%stackaddr$prim48705 = alloca %struct.ScmObj*, align 8
%args47276$k40467$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42432, %struct.ScmObj* %args47276$k40467$1)
store volatile %struct.ScmObj* %args47276$k40467$2, %struct.ScmObj** %stackaddr$prim48705, align 8
%clofunc48706 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40467)
musttail call tailcc void %clofunc48706(%struct.ScmObj* %k40467, %struct.ScmObj* %args47276$k40467$2)
ret void
falsebranch$cmp48703:
%ae42441 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48707 = alloca %struct.ScmObj*, align 8
%anf_45bind40311 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42441)
store volatile %struct.ScmObj* %anf_45bind40311, %struct.ScmObj** %stackaddr$prim48707, align 8
%stackaddr$prim48708 = alloca %struct.ScmObj*, align 8
%anf_45bind40312 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind40311)
store volatile %struct.ScmObj* %anf_45bind40312, %struct.ScmObj** %stackaddr$prim48708, align 8
%truthy$cmp48709 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40312)
%cmp$cmp48709 = icmp eq i64 %truthy$cmp48709, 1
br i1 %cmp$cmp48709, label %truebranch$cmp48709, label %falsebranch$cmp48709
truebranch$cmp48709:
%ae42445 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48710 = alloca %struct.ScmObj*, align 8
%anf_45bind40313 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42445)
store volatile %struct.ScmObj* %anf_45bind40313, %struct.ScmObj** %stackaddr$prim48710, align 8
%stackaddr$prim48711 = alloca %struct.ScmObj*, align 8
%b40192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40313)
store volatile %struct.ScmObj* %b40192, %struct.ScmObj** %stackaddr$prim48711, align 8
%ae42448 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48712 = alloca %struct.ScmObj*, align 8
%anf_45bind40314 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42448)
store volatile %struct.ScmObj* %anf_45bind40314, %struct.ScmObj** %stackaddr$prim48712, align 8
%stackaddr$prim48713 = alloca %struct.ScmObj*, align 8
%anf_45bind40315 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40314)
store volatile %struct.ScmObj* %anf_45bind40315, %struct.ScmObj** %stackaddr$prim48713, align 8
%ae42451 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48714 = alloca %struct.ScmObj*, align 8
%_95040193 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a40189, %struct.ScmObj* %ae42451, %struct.ScmObj* %anf_45bind40315)
store volatile %struct.ScmObj* %_95040193, %struct.ScmObj** %stackaddr$prim48714, align 8
%args47277$cc40190$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48715 = alloca %struct.ScmObj*, align 8
%args47277$cc40190$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc40190, %struct.ScmObj* %args47277$cc40190$0)
store volatile %struct.ScmObj* %args47277$cc40190$1, %struct.ScmObj** %stackaddr$prim48715, align 8
%stackaddr$prim48716 = alloca %struct.ScmObj*, align 8
%args47277$cc40190$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40467, %struct.ScmObj* %args47277$cc40190$1)
store volatile %struct.ScmObj* %args47277$cc40190$2, %struct.ScmObj** %stackaddr$prim48716, align 8
%clofunc48717 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc40190)
musttail call tailcc void %clofunc48717(%struct.ScmObj* %cc40190, %struct.ScmObj* %args47277$cc40190$2)
ret void
falsebranch$cmp48709:
%ae42484 = call %struct.ScmObj* @const_init_int(i64 0)
%ae42485 = call %struct.ScmObj* @const_init_false()
%args47278$k40467$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48718 = alloca %struct.ScmObj*, align 8
%args47278$k40467$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42485, %struct.ScmObj* %args47278$k40467$0)
store volatile %struct.ScmObj* %args47278$k40467$1, %struct.ScmObj** %stackaddr$prim48718, align 8
%stackaddr$prim48719 = alloca %struct.ScmObj*, align 8
%args47278$k40467$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42484, %struct.ScmObj* %args47278$k40467$1)
store volatile %struct.ScmObj* %args47278$k40467$2, %struct.ScmObj** %stackaddr$prim48719, align 8
%clofunc48720 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40467)
musttail call tailcc void %clofunc48720(%struct.ScmObj* %k40467, %struct.ScmObj* %args47278$k40467$2)
ret void
}

define tailcc void @proc_clo$ae42408(%struct.ScmObj* %env$ae42408,%struct.ScmObj* %current_45args47280) {
%stackaddr$prim48721 = alloca %struct.ScmObj*, align 8
%k40470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47280)
store volatile %struct.ScmObj* %k40470, %struct.ScmObj** %stackaddr$prim48721, align 8
%stackaddr$prim48722 = alloca %struct.ScmObj*, align 8
%current_45args47281 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47280)
store volatile %struct.ScmObj* %current_45args47281, %struct.ScmObj** %stackaddr$prim48722, align 8
%stackaddr$prim48723 = alloca %struct.ScmObj*, align 8
%k40191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47281)
store volatile %struct.ScmObj* %k40191, %struct.ScmObj** %stackaddr$prim48723, align 8
%ae42410 = call %struct.ScmObj* @const_init_int(i64 0)
%args47283$k40470$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48724 = alloca %struct.ScmObj*, align 8
%args47283$k40470$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40191, %struct.ScmObj* %args47283$k40470$0)
store volatile %struct.ScmObj* %args47283$k40470$1, %struct.ScmObj** %stackaddr$prim48724, align 8
%stackaddr$prim48725 = alloca %struct.ScmObj*, align 8
%args47283$k40470$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42410, %struct.ScmObj* %args47283$k40470$1)
store volatile %struct.ScmObj* %args47283$k40470$2, %struct.ScmObj** %stackaddr$prim48725, align 8
%clofunc48726 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40470)
musttail call tailcc void %clofunc48726(%struct.ScmObj* %k40470, %struct.ScmObj* %args47283$k40470$2)
ret void
}

define tailcc void @proc_clo$ae42331(%struct.ScmObj* %env$ae42331,%struct.ScmObj* %current_45args47286) {
%stackaddr$env-ref48727 = alloca %struct.ScmObj*, align 8
%_37append40195 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42331, i64 0)
store %struct.ScmObj* %_37append40195, %struct.ScmObj** %stackaddr$env-ref48727
%stackaddr$prim48728 = alloca %struct.ScmObj*, align 8
%k40471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47286)
store volatile %struct.ScmObj* %k40471, %struct.ScmObj** %stackaddr$prim48728, align 8
%stackaddr$prim48729 = alloca %struct.ScmObj*, align 8
%current_45args47287 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47286)
store volatile %struct.ScmObj* %current_45args47287, %struct.ScmObj** %stackaddr$prim48729, align 8
%stackaddr$prim48730 = alloca %struct.ScmObj*, align 8
%ls040198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47287)
store volatile %struct.ScmObj* %ls040198, %struct.ScmObj** %stackaddr$prim48730, align 8
%stackaddr$prim48731 = alloca %struct.ScmObj*, align 8
%current_45args47288 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47287)
store volatile %struct.ScmObj* %current_45args47288, %struct.ScmObj** %stackaddr$prim48731, align 8
%stackaddr$prim48732 = alloca %struct.ScmObj*, align 8
%ls140197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47288)
store volatile %struct.ScmObj* %ls140197, %struct.ScmObj** %stackaddr$prim48732, align 8
%stackaddr$prim48733 = alloca %struct.ScmObj*, align 8
%anf_45bind40302 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls040198)
store volatile %struct.ScmObj* %anf_45bind40302, %struct.ScmObj** %stackaddr$prim48733, align 8
%truthy$cmp48734 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40302)
%cmp$cmp48734 = icmp eq i64 %truthy$cmp48734, 1
br i1 %cmp$cmp48734, label %truebranch$cmp48734, label %falsebranch$cmp48734
truebranch$cmp48734:
%ae42335 = call %struct.ScmObj* @const_init_int(i64 0)
%args47290$k40471$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48735 = alloca %struct.ScmObj*, align 8
%args47290$k40471$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140197, %struct.ScmObj* %args47290$k40471$0)
store volatile %struct.ScmObj* %args47290$k40471$1, %struct.ScmObj** %stackaddr$prim48735, align 8
%stackaddr$prim48736 = alloca %struct.ScmObj*, align 8
%args47290$k40471$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42335, %struct.ScmObj* %args47290$k40471$1)
store volatile %struct.ScmObj* %args47290$k40471$2, %struct.ScmObj** %stackaddr$prim48736, align 8
%clofunc48737 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40471)
musttail call tailcc void %clofunc48737(%struct.ScmObj* %k40471, %struct.ScmObj* %args47290$k40471$2)
ret void
falsebranch$cmp48734:
%stackaddr$prim48738 = alloca %struct.ScmObj*, align 8
%anf_45bind40303 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls040198)
store volatile %struct.ScmObj* %anf_45bind40303, %struct.ScmObj** %stackaddr$prim48738, align 8
%ae42342 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim48739 = alloca %struct.ScmObj*, align 8
%anf_45bind40304 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append40195, %struct.ScmObj* %ae42342)
store volatile %struct.ScmObj* %anf_45bind40304, %struct.ScmObj** %stackaddr$prim48739, align 8
%stackaddr$prim48740 = alloca %struct.ScmObj*, align 8
%anf_45bind40305 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls040198)
store volatile %struct.ScmObj* %anf_45bind40305, %struct.ScmObj** %stackaddr$prim48740, align 8
%stackaddr$makeclosure48741 = alloca %struct.ScmObj*, align 8
%fptrToInt48742 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42345 to i64
%ae42345 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt48742)
store volatile %struct.ScmObj* %ae42345, %struct.ScmObj** %stackaddr$makeclosure48741, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42345, %struct.ScmObj* %k40471, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42345, %struct.ScmObj* %anf_45bind40303, i64 1)
%args47295$anf_45bind40304$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48743 = alloca %struct.ScmObj*, align 8
%args47295$anf_45bind40304$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls140197, %struct.ScmObj* %args47295$anf_45bind40304$0)
store volatile %struct.ScmObj* %args47295$anf_45bind40304$1, %struct.ScmObj** %stackaddr$prim48743, align 8
%stackaddr$prim48744 = alloca %struct.ScmObj*, align 8
%args47295$anf_45bind40304$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40305, %struct.ScmObj* %args47295$anf_45bind40304$1)
store volatile %struct.ScmObj* %args47295$anf_45bind40304$2, %struct.ScmObj** %stackaddr$prim48744, align 8
%stackaddr$prim48745 = alloca %struct.ScmObj*, align 8
%args47295$anf_45bind40304$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42345, %struct.ScmObj* %args47295$anf_45bind40304$2)
store volatile %struct.ScmObj* %args47295$anf_45bind40304$3, %struct.ScmObj** %stackaddr$prim48745, align 8
%clofunc48746 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40304)
musttail call tailcc void %clofunc48746(%struct.ScmObj* %anf_45bind40304, %struct.ScmObj* %args47295$anf_45bind40304$3)
ret void
}

define tailcc void @proc_clo$ae42345(%struct.ScmObj* %env$ae42345,%struct.ScmObj* %current_45args47291) {
%stackaddr$env-ref48747 = alloca %struct.ScmObj*, align 8
%k40471 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42345, i64 0)
store %struct.ScmObj* %k40471, %struct.ScmObj** %stackaddr$env-ref48747
%stackaddr$env-ref48748 = alloca %struct.ScmObj*, align 8
%anf_45bind40303 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42345, i64 1)
store %struct.ScmObj* %anf_45bind40303, %struct.ScmObj** %stackaddr$env-ref48748
%stackaddr$prim48749 = alloca %struct.ScmObj*, align 8
%_95k40472 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47291)
store volatile %struct.ScmObj* %_95k40472, %struct.ScmObj** %stackaddr$prim48749, align 8
%stackaddr$prim48750 = alloca %struct.ScmObj*, align 8
%current_45args47292 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47291)
store volatile %struct.ScmObj* %current_45args47292, %struct.ScmObj** %stackaddr$prim48750, align 8
%stackaddr$prim48751 = alloca %struct.ScmObj*, align 8
%anf_45bind40306 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47292)
store volatile %struct.ScmObj* %anf_45bind40306, %struct.ScmObj** %stackaddr$prim48751, align 8
%stackaddr$prim48752 = alloca %struct.ScmObj*, align 8
%cpsprim40473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40303, %struct.ScmObj* %anf_45bind40306)
store volatile %struct.ScmObj* %cpsprim40473, %struct.ScmObj** %stackaddr$prim48752, align 8
%ae42351 = call %struct.ScmObj* @const_init_int(i64 0)
%args47294$k40471$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48753 = alloca %struct.ScmObj*, align 8
%args47294$k40471$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40473, %struct.ScmObj* %args47294$k40471$0)
store volatile %struct.ScmObj* %args47294$k40471$1, %struct.ScmObj** %stackaddr$prim48753, align 8
%stackaddr$prim48754 = alloca %struct.ScmObj*, align 8
%args47294$k40471$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42351, %struct.ScmObj* %args47294$k40471$1)
store volatile %struct.ScmObj* %args47294$k40471$2, %struct.ScmObj** %stackaddr$prim48754, align 8
%clofunc48755 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40471)
musttail call tailcc void %clofunc48755(%struct.ScmObj* %k40471, %struct.ScmObj* %args47294$k40471$2)
ret void
}

define tailcc void @proc_clo$ae42305(%struct.ScmObj* %env$ae42305,%struct.ScmObj* %current_45args47297) {
%stackaddr$prim48756 = alloca %struct.ScmObj*, align 8
%k40474 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47297)
store volatile %struct.ScmObj* %k40474, %struct.ScmObj** %stackaddr$prim48756, align 8
%stackaddr$prim48757 = alloca %struct.ScmObj*, align 8
%current_45args47298 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47297)
store volatile %struct.ScmObj* %current_45args47298, %struct.ScmObj** %stackaddr$prim48757, align 8
%stackaddr$prim48758 = alloca %struct.ScmObj*, align 8
%a40201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47298)
store volatile %struct.ScmObj* %a40201, %struct.ScmObj** %stackaddr$prim48758, align 8
%stackaddr$prim48759 = alloca %struct.ScmObj*, align 8
%current_45args47299 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47298)
store volatile %struct.ScmObj* %current_45args47299, %struct.ScmObj** %stackaddr$prim48759, align 8
%stackaddr$prim48760 = alloca %struct.ScmObj*, align 8
%b40200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47299)
store volatile %struct.ScmObj* %b40200, %struct.ScmObj** %stackaddr$prim48760, align 8
%stackaddr$prim48761 = alloca %struct.ScmObj*, align 8
%anf_45bind40301 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a40201, %struct.ScmObj* %b40200)
store volatile %struct.ScmObj* %anf_45bind40301, %struct.ScmObj** %stackaddr$prim48761, align 8
%stackaddr$prim48762 = alloca %struct.ScmObj*, align 8
%cpsprim40475 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40301)
store volatile %struct.ScmObj* %cpsprim40475, %struct.ScmObj** %stackaddr$prim48762, align 8
%ae42310 = call %struct.ScmObj* @const_init_int(i64 0)
%args47301$k40474$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48763 = alloca %struct.ScmObj*, align 8
%args47301$k40474$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40475, %struct.ScmObj* %args47301$k40474$0)
store volatile %struct.ScmObj* %args47301$k40474$1, %struct.ScmObj** %stackaddr$prim48763, align 8
%stackaddr$prim48764 = alloca %struct.ScmObj*, align 8
%args47301$k40474$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42310, %struct.ScmObj* %args47301$k40474$1)
store volatile %struct.ScmObj* %args47301$k40474$2, %struct.ScmObj** %stackaddr$prim48764, align 8
%clofunc48765 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40474)
musttail call tailcc void %clofunc48765(%struct.ScmObj* %k40474, %struct.ScmObj* %args47301$k40474$2)
ret void
}

define tailcc void @proc_clo$ae42281(%struct.ScmObj* %env$ae42281,%struct.ScmObj* %current_45args47303) {
%stackaddr$prim48766 = alloca %struct.ScmObj*, align 8
%k40476 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47303)
store volatile %struct.ScmObj* %k40476, %struct.ScmObj** %stackaddr$prim48766, align 8
%stackaddr$prim48767 = alloca %struct.ScmObj*, align 8
%current_45args47304 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47303)
store volatile %struct.ScmObj* %current_45args47304, %struct.ScmObj** %stackaddr$prim48767, align 8
%stackaddr$prim48768 = alloca %struct.ScmObj*, align 8
%a40204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47304)
store volatile %struct.ScmObj* %a40204, %struct.ScmObj** %stackaddr$prim48768, align 8
%stackaddr$prim48769 = alloca %struct.ScmObj*, align 8
%current_45args47305 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47304)
store volatile %struct.ScmObj* %current_45args47305, %struct.ScmObj** %stackaddr$prim48769, align 8
%stackaddr$prim48770 = alloca %struct.ScmObj*, align 8
%b40203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47305)
store volatile %struct.ScmObj* %b40203, %struct.ScmObj** %stackaddr$prim48770, align 8
%stackaddr$prim48771 = alloca %struct.ScmObj*, align 8
%anf_45bind40300 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a40204, %struct.ScmObj* %b40203)
store volatile %struct.ScmObj* %anf_45bind40300, %struct.ScmObj** %stackaddr$prim48771, align 8
%stackaddr$prim48772 = alloca %struct.ScmObj*, align 8
%cpsprim40477 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind40300)
store volatile %struct.ScmObj* %cpsprim40477, %struct.ScmObj** %stackaddr$prim48772, align 8
%ae42286 = call %struct.ScmObj* @const_init_int(i64 0)
%args47307$k40476$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48773 = alloca %struct.ScmObj*, align 8
%args47307$k40476$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40477, %struct.ScmObj* %args47307$k40476$0)
store volatile %struct.ScmObj* %args47307$k40476$1, %struct.ScmObj** %stackaddr$prim48773, align 8
%stackaddr$prim48774 = alloca %struct.ScmObj*, align 8
%args47307$k40476$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42286, %struct.ScmObj* %args47307$k40476$1)
store volatile %struct.ScmObj* %args47307$k40476$2, %struct.ScmObj** %stackaddr$prim48774, align 8
%clofunc48775 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40476)
musttail call tailcc void %clofunc48775(%struct.ScmObj* %k40476, %struct.ScmObj* %args47307$k40476$2)
ret void
}

define tailcc void @proc_clo$ae41887(%struct.ScmObj* %env$ae41887,%struct.ScmObj* %current_45args47310) {
%stackaddr$env-ref48776 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41887, i64 0)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48776
%stackaddr$env-ref48777 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41887, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48777
%stackaddr$env-ref48778 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41887, i64 2)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48778
%stackaddr$prim48779 = alloca %struct.ScmObj*, align 8
%k40478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47310)
store volatile %struct.ScmObj* %k40478, %struct.ScmObj** %stackaddr$prim48779, align 8
%stackaddr$prim48780 = alloca %struct.ScmObj*, align 8
%current_45args47311 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47310)
store volatile %struct.ScmObj* %current_45args47311, %struct.ScmObj** %stackaddr$prim48780, align 8
%stackaddr$prim48781 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47311)
store volatile %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$prim48781, align 8
%ae41889 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48782 = alloca %struct.ScmObj*, align 8
%fptrToInt48783 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41890 to i64
%ae41890 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48783)
store volatile %struct.ScmObj* %ae41890, %struct.ScmObj** %stackaddr$makeclosure48782, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41890, %struct.ScmObj* %_37foldr40128, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41890, %struct.ScmObj* %_37foldl40206, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41890, %struct.ScmObj* %_37foldr140123, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41890, %struct.ScmObj* %_37map140154, i64 3)
%args47368$k40478$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48784 = alloca %struct.ScmObj*, align 8
%args47368$k40478$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41890, %struct.ScmObj* %args47368$k40478$0)
store volatile %struct.ScmObj* %args47368$k40478$1, %struct.ScmObj** %stackaddr$prim48784, align 8
%stackaddr$prim48785 = alloca %struct.ScmObj*, align 8
%args47368$k40478$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41889, %struct.ScmObj* %args47368$k40478$1)
store volatile %struct.ScmObj* %args47368$k40478$2, %struct.ScmObj** %stackaddr$prim48785, align 8
%clofunc48786 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40478)
musttail call tailcc void %clofunc48786(%struct.ScmObj* %k40478, %struct.ScmObj* %args47368$k40478$2)
ret void
}

define tailcc void @proc_clo$ae41890(%struct.ScmObj* %env$ae41890,%struct.ScmObj* %args4020740479) {
%stackaddr$env-ref48787 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41890, i64 0)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48787
%stackaddr$env-ref48788 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41890, i64 1)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48788
%stackaddr$env-ref48789 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41890, i64 2)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48789
%stackaddr$env-ref48790 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41890, i64 3)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48790
%stackaddr$prim48791 = alloca %struct.ScmObj*, align 8
%k40480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4020740479)
store volatile %struct.ScmObj* %k40480, %struct.ScmObj** %stackaddr$prim48791, align 8
%stackaddr$prim48792 = alloca %struct.ScmObj*, align 8
%args40207 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4020740479)
store volatile %struct.ScmObj* %args40207, %struct.ScmObj** %stackaddr$prim48792, align 8
%stackaddr$prim48793 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40207)
store volatile %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$prim48793, align 8
%stackaddr$prim48794 = alloca %struct.ScmObj*, align 8
%anf_45bind40288 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40207)
store volatile %struct.ScmObj* %anf_45bind40288, %struct.ScmObj** %stackaddr$prim48794, align 8
%stackaddr$prim48795 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40288)
store volatile %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$prim48795, align 8
%stackaddr$prim48796 = alloca %struct.ScmObj*, align 8
%anf_45bind40289 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40207)
store volatile %struct.ScmObj* %anf_45bind40289, %struct.ScmObj** %stackaddr$prim48796, align 8
%stackaddr$prim48797 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40289)
store volatile %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$prim48797, align 8
%stackaddr$makeclosure48798 = alloca %struct.ScmObj*, align 8
%fptrToInt48799 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41898 to i64
%ae41898 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt48799)
store volatile %struct.ScmObj* %ae41898, %struct.ScmObj** %stackaddr$makeclosure48798, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41898, %struct.ScmObj* %k40480, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41898, %struct.ScmObj* %lsts40208, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41898, %struct.ScmObj* %_37foldr40128, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41898, %struct.ScmObj* %f40210, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41898, %struct.ScmObj* %acc40209, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41898, %struct.ScmObj* %_37foldl40206, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41898, %struct.ScmObj* %_37foldr140123, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41898, %struct.ScmObj* %_37map140154, i64 7)
%ae41899 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48800 = alloca %struct.ScmObj*, align 8
%fptrToInt48801 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41900 to i64
%ae41900 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48801)
store volatile %struct.ScmObj* %ae41900, %struct.ScmObj** %stackaddr$makeclosure48800, align 8
%args47367$ae41898$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48802 = alloca %struct.ScmObj*, align 8
%args47367$ae41898$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41900, %struct.ScmObj* %args47367$ae41898$0)
store volatile %struct.ScmObj* %args47367$ae41898$1, %struct.ScmObj** %stackaddr$prim48802, align 8
%stackaddr$prim48803 = alloca %struct.ScmObj*, align 8
%args47367$ae41898$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41899, %struct.ScmObj* %args47367$ae41898$1)
store volatile %struct.ScmObj* %args47367$ae41898$2, %struct.ScmObj** %stackaddr$prim48803, align 8
%clofunc48804 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41898)
musttail call tailcc void %clofunc48804(%struct.ScmObj* %ae41898, %struct.ScmObj* %args47367$ae41898$2)
ret void
}

define tailcc void @proc_clo$ae41898(%struct.ScmObj* %env$ae41898,%struct.ScmObj* %current_45args47313) {
%stackaddr$env-ref48805 = alloca %struct.ScmObj*, align 8
%k40480 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41898, i64 0)
store %struct.ScmObj* %k40480, %struct.ScmObj** %stackaddr$env-ref48805
%stackaddr$env-ref48806 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41898, i64 1)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref48806
%stackaddr$env-ref48807 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41898, i64 2)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48807
%stackaddr$env-ref48808 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41898, i64 3)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48808
%stackaddr$env-ref48809 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41898, i64 4)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48809
%stackaddr$env-ref48810 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41898, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48810
%stackaddr$env-ref48811 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41898, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref48811
%stackaddr$env-ref48812 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41898, i64 7)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48812
%stackaddr$prim48813 = alloca %struct.ScmObj*, align 8
%_95k40481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47313)
store volatile %struct.ScmObj* %_95k40481, %struct.ScmObj** %stackaddr$prim48813, align 8
%stackaddr$prim48814 = alloca %struct.ScmObj*, align 8
%current_45args47314 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47313)
store volatile %struct.ScmObj* %current_45args47314, %struct.ScmObj** %stackaddr$prim48814, align 8
%stackaddr$prim48815 = alloca %struct.ScmObj*, align 8
%anf_45bind40290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47314)
store volatile %struct.ScmObj* %anf_45bind40290, %struct.ScmObj** %stackaddr$prim48815, align 8
%stackaddr$makeclosure48816 = alloca %struct.ScmObj*, align 8
%fptrToInt48817 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41930 to i64
%ae41930 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48817)
store volatile %struct.ScmObj* %ae41930, %struct.ScmObj** %stackaddr$makeclosure48816, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41930, %struct.ScmObj* %k40480, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41930, %struct.ScmObj* %lsts40208, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41930, %struct.ScmObj* %_37foldr40128, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41930, %struct.ScmObj* %f40210, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41930, %struct.ScmObj* %acc40209, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41930, %struct.ScmObj* %_37foldl40206, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41930, %struct.ScmObj* %_37map140154, i64 6)
%ae41932 = call %struct.ScmObj* @const_init_false()
%args47360$_37foldr140123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48818 = alloca %struct.ScmObj*, align 8
%args47360$_37foldr140123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40208, %struct.ScmObj* %args47360$_37foldr140123$0)
store volatile %struct.ScmObj* %args47360$_37foldr140123$1, %struct.ScmObj** %stackaddr$prim48818, align 8
%stackaddr$prim48819 = alloca %struct.ScmObj*, align 8
%args47360$_37foldr140123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41932, %struct.ScmObj* %args47360$_37foldr140123$1)
store volatile %struct.ScmObj* %args47360$_37foldr140123$2, %struct.ScmObj** %stackaddr$prim48819, align 8
%stackaddr$prim48820 = alloca %struct.ScmObj*, align 8
%args47360$_37foldr140123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40290, %struct.ScmObj* %args47360$_37foldr140123$2)
store volatile %struct.ScmObj* %args47360$_37foldr140123$3, %struct.ScmObj** %stackaddr$prim48820, align 8
%stackaddr$prim48821 = alloca %struct.ScmObj*, align 8
%args47360$_37foldr140123$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41930, %struct.ScmObj* %args47360$_37foldr140123$3)
store volatile %struct.ScmObj* %args47360$_37foldr140123$4, %struct.ScmObj** %stackaddr$prim48821, align 8
%clofunc48822 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140123)
musttail call tailcc void %clofunc48822(%struct.ScmObj* %_37foldr140123, %struct.ScmObj* %args47360$_37foldr140123$4)
ret void
}

define tailcc void @proc_clo$ae41930(%struct.ScmObj* %env$ae41930,%struct.ScmObj* %current_45args47316) {
%stackaddr$env-ref48823 = alloca %struct.ScmObj*, align 8
%k40480 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41930, i64 0)
store %struct.ScmObj* %k40480, %struct.ScmObj** %stackaddr$env-ref48823
%stackaddr$env-ref48824 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41930, i64 1)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref48824
%stackaddr$env-ref48825 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41930, i64 2)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48825
%stackaddr$env-ref48826 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41930, i64 3)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48826
%stackaddr$env-ref48827 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41930, i64 4)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48827
%stackaddr$env-ref48828 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41930, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48828
%stackaddr$env-ref48829 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41930, i64 6)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48829
%stackaddr$prim48830 = alloca %struct.ScmObj*, align 8
%_95k40482 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47316)
store volatile %struct.ScmObj* %_95k40482, %struct.ScmObj** %stackaddr$prim48830, align 8
%stackaddr$prim48831 = alloca %struct.ScmObj*, align 8
%current_45args47317 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47316)
store volatile %struct.ScmObj* %current_45args47317, %struct.ScmObj** %stackaddr$prim48831, align 8
%stackaddr$prim48832 = alloca %struct.ScmObj*, align 8
%anf_45bind40291 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47317)
store volatile %struct.ScmObj* %anf_45bind40291, %struct.ScmObj** %stackaddr$prim48832, align 8
%truthy$cmp48833 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40291)
%cmp$cmp48833 = icmp eq i64 %truthy$cmp48833, 1
br i1 %cmp$cmp48833, label %truebranch$cmp48833, label %falsebranch$cmp48833
truebranch$cmp48833:
%ae41941 = call %struct.ScmObj* @const_init_int(i64 0)
%args47319$k40480$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48834 = alloca %struct.ScmObj*, align 8
%args47319$k40480$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40209, %struct.ScmObj* %args47319$k40480$0)
store volatile %struct.ScmObj* %args47319$k40480$1, %struct.ScmObj** %stackaddr$prim48834, align 8
%stackaddr$prim48835 = alloca %struct.ScmObj*, align 8
%args47319$k40480$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41941, %struct.ScmObj* %args47319$k40480$1)
store volatile %struct.ScmObj* %args47319$k40480$2, %struct.ScmObj** %stackaddr$prim48835, align 8
%clofunc48836 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40480)
musttail call tailcc void %clofunc48836(%struct.ScmObj* %k40480, %struct.ScmObj* %args47319$k40480$2)
ret void
falsebranch$cmp48833:
%stackaddr$makeclosure48837 = alloca %struct.ScmObj*, align 8
%fptrToInt48838 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41946 to i64
%ae41946 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48838)
store volatile %struct.ScmObj* %ae41946, %struct.ScmObj** %stackaddr$makeclosure48837, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41946, %struct.ScmObj* %k40480, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41946, %struct.ScmObj* %lsts40208, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41946, %struct.ScmObj* %_37foldr40128, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41946, %struct.ScmObj* %f40210, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41946, %struct.ScmObj* %acc40209, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41946, %struct.ScmObj* %_37foldl40206, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41946, %struct.ScmObj* %_37map140154, i64 6)
%ae41947 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48839 = alloca %struct.ScmObj*, align 8
%fptrToInt48840 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41948 to i64
%ae41948 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48840)
store volatile %struct.ScmObj* %ae41948, %struct.ScmObj** %stackaddr$makeclosure48839, align 8
%args47359$ae41946$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48841 = alloca %struct.ScmObj*, align 8
%args47359$ae41946$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41948, %struct.ScmObj* %args47359$ae41946$0)
store volatile %struct.ScmObj* %args47359$ae41946$1, %struct.ScmObj** %stackaddr$prim48841, align 8
%stackaddr$prim48842 = alloca %struct.ScmObj*, align 8
%args47359$ae41946$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41947, %struct.ScmObj* %args47359$ae41946$1)
store volatile %struct.ScmObj* %args47359$ae41946$2, %struct.ScmObj** %stackaddr$prim48842, align 8
%clofunc48843 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41946)
musttail call tailcc void %clofunc48843(%struct.ScmObj* %ae41946, %struct.ScmObj* %args47359$ae41946$2)
ret void
}

define tailcc void @proc_clo$ae41946(%struct.ScmObj* %env$ae41946,%struct.ScmObj* %current_45args47320) {
%stackaddr$env-ref48844 = alloca %struct.ScmObj*, align 8
%k40480 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41946, i64 0)
store %struct.ScmObj* %k40480, %struct.ScmObj** %stackaddr$env-ref48844
%stackaddr$env-ref48845 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41946, i64 1)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref48845
%stackaddr$env-ref48846 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41946, i64 2)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48846
%stackaddr$env-ref48847 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41946, i64 3)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48847
%stackaddr$env-ref48848 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41946, i64 4)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48848
%stackaddr$env-ref48849 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41946, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48849
%stackaddr$env-ref48850 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41946, i64 6)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48850
%stackaddr$prim48851 = alloca %struct.ScmObj*, align 8
%_95k40483 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47320)
store volatile %struct.ScmObj* %_95k40483, %struct.ScmObj** %stackaddr$prim48851, align 8
%stackaddr$prim48852 = alloca %struct.ScmObj*, align 8
%current_45args47321 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47320)
store volatile %struct.ScmObj* %current_45args47321, %struct.ScmObj** %stackaddr$prim48852, align 8
%stackaddr$prim48853 = alloca %struct.ScmObj*, align 8
%anf_45bind40292 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47321)
store volatile %struct.ScmObj* %anf_45bind40292, %struct.ScmObj** %stackaddr$prim48853, align 8
%stackaddr$makeclosure48854 = alloca %struct.ScmObj*, align 8
%fptrToInt48855 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41967 to i64
%ae41967 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48855)
store volatile %struct.ScmObj* %ae41967, %struct.ScmObj** %stackaddr$makeclosure48854, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41967, %struct.ScmObj* %k40480, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41967, %struct.ScmObj* %lsts40208, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41967, %struct.ScmObj* %_37foldr40128, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41967, %struct.ScmObj* %f40210, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41967, %struct.ScmObj* %acc40209, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41967, %struct.ScmObj* %_37foldl40206, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41967, %struct.ScmObj* %_37map140154, i64 6)
%args47354$_37map140154$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48856 = alloca %struct.ScmObj*, align 8
%args47354$_37map140154$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40208, %struct.ScmObj* %args47354$_37map140154$0)
store volatile %struct.ScmObj* %args47354$_37map140154$1, %struct.ScmObj** %stackaddr$prim48856, align 8
%stackaddr$prim48857 = alloca %struct.ScmObj*, align 8
%args47354$_37map140154$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40292, %struct.ScmObj* %args47354$_37map140154$1)
store volatile %struct.ScmObj* %args47354$_37map140154$2, %struct.ScmObj** %stackaddr$prim48857, align 8
%stackaddr$prim48858 = alloca %struct.ScmObj*, align 8
%args47354$_37map140154$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41967, %struct.ScmObj* %args47354$_37map140154$2)
store volatile %struct.ScmObj* %args47354$_37map140154$3, %struct.ScmObj** %stackaddr$prim48858, align 8
%clofunc48859 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140154)
musttail call tailcc void %clofunc48859(%struct.ScmObj* %_37map140154, %struct.ScmObj* %args47354$_37map140154$3)
ret void
}

define tailcc void @proc_clo$ae41967(%struct.ScmObj* %env$ae41967,%struct.ScmObj* %current_45args47323) {
%stackaddr$env-ref48860 = alloca %struct.ScmObj*, align 8
%k40480 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41967, i64 0)
store %struct.ScmObj* %k40480, %struct.ScmObj** %stackaddr$env-ref48860
%stackaddr$env-ref48861 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41967, i64 1)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref48861
%stackaddr$env-ref48862 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41967, i64 2)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48862
%stackaddr$env-ref48863 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41967, i64 3)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48863
%stackaddr$env-ref48864 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41967, i64 4)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48864
%stackaddr$env-ref48865 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41967, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48865
%stackaddr$env-ref48866 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41967, i64 6)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48866
%stackaddr$prim48867 = alloca %struct.ScmObj*, align 8
%_95k40484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47323)
store volatile %struct.ScmObj* %_95k40484, %struct.ScmObj** %stackaddr$prim48867, align 8
%stackaddr$prim48868 = alloca %struct.ScmObj*, align 8
%current_45args47324 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47323)
store volatile %struct.ScmObj* %current_45args47324, %struct.ScmObj** %stackaddr$prim48868, align 8
%stackaddr$prim48869 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47324)
store volatile %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$prim48869, align 8
%stackaddr$makeclosure48870 = alloca %struct.ScmObj*, align 8
%fptrToInt48871 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41970 to i64
%ae41970 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt48871)
store volatile %struct.ScmObj* %ae41970, %struct.ScmObj** %stackaddr$makeclosure48870, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41970, %struct.ScmObj* %k40480, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41970, %struct.ScmObj* %lsts40208, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41970, %struct.ScmObj* %_37foldr40128, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41970, %struct.ScmObj* %lsts_4340215, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41970, %struct.ScmObj* %f40210, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41970, %struct.ScmObj* %acc40209, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41970, %struct.ScmObj* %_37foldl40206, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41970, %struct.ScmObj* %_37map140154, i64 7)
%ae41971 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48872 = alloca %struct.ScmObj*, align 8
%fptrToInt48873 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41972 to i64
%ae41972 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48873)
store volatile %struct.ScmObj* %ae41972, %struct.ScmObj** %stackaddr$makeclosure48872, align 8
%args47353$ae41970$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48874 = alloca %struct.ScmObj*, align 8
%args47353$ae41970$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41972, %struct.ScmObj* %args47353$ae41970$0)
store volatile %struct.ScmObj* %args47353$ae41970$1, %struct.ScmObj** %stackaddr$prim48874, align 8
%stackaddr$prim48875 = alloca %struct.ScmObj*, align 8
%args47353$ae41970$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41971, %struct.ScmObj* %args47353$ae41970$1)
store volatile %struct.ScmObj* %args47353$ae41970$2, %struct.ScmObj** %stackaddr$prim48875, align 8
%clofunc48876 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41970)
musttail call tailcc void %clofunc48876(%struct.ScmObj* %ae41970, %struct.ScmObj* %args47353$ae41970$2)
ret void
}

define tailcc void @proc_clo$ae41970(%struct.ScmObj* %env$ae41970,%struct.ScmObj* %current_45args47326) {
%stackaddr$env-ref48877 = alloca %struct.ScmObj*, align 8
%k40480 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41970, i64 0)
store %struct.ScmObj* %k40480, %struct.ScmObj** %stackaddr$env-ref48877
%stackaddr$env-ref48878 = alloca %struct.ScmObj*, align 8
%lsts40208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41970, i64 1)
store %struct.ScmObj* %lsts40208, %struct.ScmObj** %stackaddr$env-ref48878
%stackaddr$env-ref48879 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41970, i64 2)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48879
%stackaddr$env-ref48880 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41970, i64 3)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref48880
%stackaddr$env-ref48881 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41970, i64 4)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48881
%stackaddr$env-ref48882 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41970, i64 5)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48882
%stackaddr$env-ref48883 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41970, i64 6)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48883
%stackaddr$env-ref48884 = alloca %struct.ScmObj*, align 8
%_37map140154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41970, i64 7)
store %struct.ScmObj* %_37map140154, %struct.ScmObj** %stackaddr$env-ref48884
%stackaddr$prim48885 = alloca %struct.ScmObj*, align 8
%_95k40485 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47326)
store volatile %struct.ScmObj* %_95k40485, %struct.ScmObj** %stackaddr$prim48885, align 8
%stackaddr$prim48886 = alloca %struct.ScmObj*, align 8
%current_45args47327 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47326)
store volatile %struct.ScmObj* %current_45args47327, %struct.ScmObj** %stackaddr$prim48886, align 8
%stackaddr$prim48887 = alloca %struct.ScmObj*, align 8
%anf_45bind40293 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47327)
store volatile %struct.ScmObj* %anf_45bind40293, %struct.ScmObj** %stackaddr$prim48887, align 8
%stackaddr$makeclosure48888 = alloca %struct.ScmObj*, align 8
%fptrToInt48889 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41991 to i64
%ae41991 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt48889)
store volatile %struct.ScmObj* %ae41991, %struct.ScmObj** %stackaddr$makeclosure48888, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41991, %struct.ScmObj* %k40480, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41991, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41991, %struct.ScmObj* %lsts_4340215, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41991, %struct.ScmObj* %f40210, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41991, %struct.ScmObj* %acc40209, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41991, %struct.ScmObj* %_37foldl40206, i64 5)
%args47348$_37map140154$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48890 = alloca %struct.ScmObj*, align 8
%args47348$_37map140154$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40208, %struct.ScmObj* %args47348$_37map140154$0)
store volatile %struct.ScmObj* %args47348$_37map140154$1, %struct.ScmObj** %stackaddr$prim48890, align 8
%stackaddr$prim48891 = alloca %struct.ScmObj*, align 8
%args47348$_37map140154$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40293, %struct.ScmObj* %args47348$_37map140154$1)
store volatile %struct.ScmObj* %args47348$_37map140154$2, %struct.ScmObj** %stackaddr$prim48891, align 8
%stackaddr$prim48892 = alloca %struct.ScmObj*, align 8
%args47348$_37map140154$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41991, %struct.ScmObj* %args47348$_37map140154$2)
store volatile %struct.ScmObj* %args47348$_37map140154$3, %struct.ScmObj** %stackaddr$prim48892, align 8
%clofunc48893 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140154)
musttail call tailcc void %clofunc48893(%struct.ScmObj* %_37map140154, %struct.ScmObj* %args47348$_37map140154$3)
ret void
}

define tailcc void @proc_clo$ae41991(%struct.ScmObj* %env$ae41991,%struct.ScmObj* %current_45args47329) {
%stackaddr$env-ref48894 = alloca %struct.ScmObj*, align 8
%k40480 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41991, i64 0)
store %struct.ScmObj* %k40480, %struct.ScmObj** %stackaddr$env-ref48894
%stackaddr$env-ref48895 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41991, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48895
%stackaddr$env-ref48896 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41991, i64 2)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref48896
%stackaddr$env-ref48897 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41991, i64 3)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48897
%stackaddr$env-ref48898 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41991, i64 4)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48898
%stackaddr$env-ref48899 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41991, i64 5)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48899
%stackaddr$prim48900 = alloca %struct.ScmObj*, align 8
%_95k40486 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47329)
store volatile %struct.ScmObj* %_95k40486, %struct.ScmObj** %stackaddr$prim48900, align 8
%stackaddr$prim48901 = alloca %struct.ScmObj*, align 8
%current_45args47330 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47329)
store volatile %struct.ScmObj* %current_45args47330, %struct.ScmObj** %stackaddr$prim48901, align 8
%stackaddr$prim48902 = alloca %struct.ScmObj*, align 8
%vs40213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47330)
store volatile %struct.ScmObj* %vs40213, %struct.ScmObj** %stackaddr$prim48902, align 8
%stackaddr$makeclosure48903 = alloca %struct.ScmObj*, align 8
%fptrToInt48904 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41994 to i64
%ae41994 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt48904)
store volatile %struct.ScmObj* %ae41994, %struct.ScmObj** %stackaddr$makeclosure48903, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41994, %struct.ScmObj* %k40480, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41994, %struct.ScmObj* %_37foldr40128, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41994, %struct.ScmObj* %lsts_4340215, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41994, %struct.ScmObj* %vs40213, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41994, %struct.ScmObj* %f40210, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41994, %struct.ScmObj* %acc40209, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41994, %struct.ScmObj* %_37foldl40206, i64 6)
%ae41995 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48905 = alloca %struct.ScmObj*, align 8
%fptrToInt48906 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41996 to i64
%ae41996 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt48906)
store volatile %struct.ScmObj* %ae41996, %struct.ScmObj** %stackaddr$makeclosure48905, align 8
%args47347$ae41994$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48907 = alloca %struct.ScmObj*, align 8
%args47347$ae41994$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41996, %struct.ScmObj* %args47347$ae41994$0)
store volatile %struct.ScmObj* %args47347$ae41994$1, %struct.ScmObj** %stackaddr$prim48907, align 8
%stackaddr$prim48908 = alloca %struct.ScmObj*, align 8
%args47347$ae41994$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41995, %struct.ScmObj* %args47347$ae41994$1)
store volatile %struct.ScmObj* %args47347$ae41994$2, %struct.ScmObj** %stackaddr$prim48908, align 8
%clofunc48909 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41994)
musttail call tailcc void %clofunc48909(%struct.ScmObj* %ae41994, %struct.ScmObj* %args47347$ae41994$2)
ret void
}

define tailcc void @proc_clo$ae41994(%struct.ScmObj* %env$ae41994,%struct.ScmObj* %current_45args47332) {
%stackaddr$env-ref48910 = alloca %struct.ScmObj*, align 8
%k40480 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41994, i64 0)
store %struct.ScmObj* %k40480, %struct.ScmObj** %stackaddr$env-ref48910
%stackaddr$env-ref48911 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41994, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48911
%stackaddr$env-ref48912 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41994, i64 2)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref48912
%stackaddr$env-ref48913 = alloca %struct.ScmObj*, align 8
%vs40213 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41994, i64 3)
store %struct.ScmObj* %vs40213, %struct.ScmObj** %stackaddr$env-ref48913
%stackaddr$env-ref48914 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41994, i64 4)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48914
%stackaddr$env-ref48915 = alloca %struct.ScmObj*, align 8
%acc40209 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41994, i64 5)
store %struct.ScmObj* %acc40209, %struct.ScmObj** %stackaddr$env-ref48915
%stackaddr$env-ref48916 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41994, i64 6)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48916
%stackaddr$prim48917 = alloca %struct.ScmObj*, align 8
%_95k40487 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47332)
store volatile %struct.ScmObj* %_95k40487, %struct.ScmObj** %stackaddr$prim48917, align 8
%stackaddr$prim48918 = alloca %struct.ScmObj*, align 8
%current_45args47333 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47332)
store volatile %struct.ScmObj* %current_45args47333, %struct.ScmObj** %stackaddr$prim48918, align 8
%stackaddr$prim48919 = alloca %struct.ScmObj*, align 8
%anf_45bind40294 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47333)
store volatile %struct.ScmObj* %anf_45bind40294, %struct.ScmObj** %stackaddr$prim48919, align 8
%ae42017 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48920 = alloca %struct.ScmObj*, align 8
%anf_45bind40295 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40209, %struct.ScmObj* %ae42017)
store volatile %struct.ScmObj* %anf_45bind40295, %struct.ScmObj** %stackaddr$prim48920, align 8
%stackaddr$makeclosure48921 = alloca %struct.ScmObj*, align 8
%fptrToInt48922 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42019 to i64
%ae42019 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48922)
store volatile %struct.ScmObj* %ae42019, %struct.ScmObj** %stackaddr$makeclosure48921, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42019, %struct.ScmObj* %lsts_4340215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42019, %struct.ScmObj* %f40210, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42019, %struct.ScmObj* %k40480, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae42019, %struct.ScmObj* %_37foldl40206, i64 3)
%args47341$_37foldr40128$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48923 = alloca %struct.ScmObj*, align 8
%args47341$_37foldr40128$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40213, %struct.ScmObj* %args47341$_37foldr40128$0)
store volatile %struct.ScmObj* %args47341$_37foldr40128$1, %struct.ScmObj** %stackaddr$prim48923, align 8
%stackaddr$prim48924 = alloca %struct.ScmObj*, align 8
%args47341$_37foldr40128$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40295, %struct.ScmObj* %args47341$_37foldr40128$1)
store volatile %struct.ScmObj* %args47341$_37foldr40128$2, %struct.ScmObj** %stackaddr$prim48924, align 8
%stackaddr$prim48925 = alloca %struct.ScmObj*, align 8
%args47341$_37foldr40128$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40294, %struct.ScmObj* %args47341$_37foldr40128$2)
store volatile %struct.ScmObj* %args47341$_37foldr40128$3, %struct.ScmObj** %stackaddr$prim48925, align 8
%stackaddr$prim48926 = alloca %struct.ScmObj*, align 8
%args47341$_37foldr40128$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42019, %struct.ScmObj* %args47341$_37foldr40128$3)
store volatile %struct.ScmObj* %args47341$_37foldr40128$4, %struct.ScmObj** %stackaddr$prim48926, align 8
%clofunc48927 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40128)
musttail call tailcc void %clofunc48927(%struct.ScmObj* %_37foldr40128, %struct.ScmObj* %args47341$_37foldr40128$4)
ret void
}

define tailcc void @proc_clo$ae42019(%struct.ScmObj* %env$ae42019,%struct.ScmObj* %current_45args47335) {
%stackaddr$env-ref48928 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42019, i64 0)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref48928
%stackaddr$env-ref48929 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42019, i64 1)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48929
%stackaddr$env-ref48930 = alloca %struct.ScmObj*, align 8
%k40480 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42019, i64 2)
store %struct.ScmObj* %k40480, %struct.ScmObj** %stackaddr$env-ref48930
%stackaddr$env-ref48931 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42019, i64 3)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48931
%stackaddr$prim48932 = alloca %struct.ScmObj*, align 8
%_95k40488 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47335)
store volatile %struct.ScmObj* %_95k40488, %struct.ScmObj** %stackaddr$prim48932, align 8
%stackaddr$prim48933 = alloca %struct.ScmObj*, align 8
%current_45args47336 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47335)
store volatile %struct.ScmObj* %current_45args47336, %struct.ScmObj** %stackaddr$prim48933, align 8
%stackaddr$prim48934 = alloca %struct.ScmObj*, align 8
%anf_45bind40296 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47336)
store volatile %struct.ScmObj* %anf_45bind40296, %struct.ScmObj** %stackaddr$prim48934, align 8
%stackaddr$makeclosure48935 = alloca %struct.ScmObj*, align 8
%fptrToInt48936 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae42023 to i64
%ae42023 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt48936)
store volatile %struct.ScmObj* %ae42023, %struct.ScmObj** %stackaddr$makeclosure48935, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae42023, %struct.ScmObj* %lsts_4340215, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae42023, %struct.ScmObj* %f40210, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae42023, %struct.ScmObj* %k40480, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae42023, %struct.ScmObj* %_37foldl40206, i64 3)
%stackaddr$prim48937 = alloca %struct.ScmObj*, align 8
%cpsargs40491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42023, %struct.ScmObj* %anf_45bind40296)
store volatile %struct.ScmObj* %cpsargs40491, %struct.ScmObj** %stackaddr$prim48937, align 8
%clofunc48938 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40210)
musttail call tailcc void %clofunc48938(%struct.ScmObj* %f40210, %struct.ScmObj* %cpsargs40491)
ret void
}

define tailcc void @proc_clo$ae42023(%struct.ScmObj* %env$ae42023,%struct.ScmObj* %current_45args47338) {
%stackaddr$env-ref48939 = alloca %struct.ScmObj*, align 8
%lsts_4340215 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42023, i64 0)
store %struct.ScmObj* %lsts_4340215, %struct.ScmObj** %stackaddr$env-ref48939
%stackaddr$env-ref48940 = alloca %struct.ScmObj*, align 8
%f40210 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42023, i64 1)
store %struct.ScmObj* %f40210, %struct.ScmObj** %stackaddr$env-ref48940
%stackaddr$env-ref48941 = alloca %struct.ScmObj*, align 8
%k40480 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42023, i64 2)
store %struct.ScmObj* %k40480, %struct.ScmObj** %stackaddr$env-ref48941
%stackaddr$env-ref48942 = alloca %struct.ScmObj*, align 8
%_37foldl40206 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae42023, i64 3)
store %struct.ScmObj* %_37foldl40206, %struct.ScmObj** %stackaddr$env-ref48942
%stackaddr$prim48943 = alloca %struct.ScmObj*, align 8
%_95k40489 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47338)
store volatile %struct.ScmObj* %_95k40489, %struct.ScmObj** %stackaddr$prim48943, align 8
%stackaddr$prim48944 = alloca %struct.ScmObj*, align 8
%current_45args47339 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47338)
store volatile %struct.ScmObj* %current_45args47339, %struct.ScmObj** %stackaddr$prim48944, align 8
%stackaddr$prim48945 = alloca %struct.ScmObj*, align 8
%acc_4340217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47339)
store volatile %struct.ScmObj* %acc_4340217, %struct.ScmObj** %stackaddr$prim48945, align 8
%stackaddr$prim48946 = alloca %struct.ScmObj*, align 8
%anf_45bind40297 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4340217, %struct.ScmObj* %lsts_4340215)
store volatile %struct.ScmObj* %anf_45bind40297, %struct.ScmObj** %stackaddr$prim48946, align 8
%stackaddr$prim48947 = alloca %struct.ScmObj*, align 8
%anf_45bind40298 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40210, %struct.ScmObj* %anf_45bind40297)
store volatile %struct.ScmObj* %anf_45bind40298, %struct.ScmObj** %stackaddr$prim48947, align 8
%stackaddr$prim48948 = alloca %struct.ScmObj*, align 8
%cpsargs40490 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40480, %struct.ScmObj* %anf_45bind40298)
store volatile %struct.ScmObj* %cpsargs40490, %struct.ScmObj** %stackaddr$prim48948, align 8
%clofunc48949 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl40206)
musttail call tailcc void %clofunc48949(%struct.ScmObj* %_37foldl40206, %struct.ScmObj* %cpsargs40490)
ret void
}

define tailcc void @proc_clo$ae41996(%struct.ScmObj* %env$ae41996,%struct.ScmObj* %current_45args47342) {
%stackaddr$prim48950 = alloca %struct.ScmObj*, align 8
%k40492 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47342)
store volatile %struct.ScmObj* %k40492, %struct.ScmObj** %stackaddr$prim48950, align 8
%stackaddr$prim48951 = alloca %struct.ScmObj*, align 8
%current_45args47343 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47342)
store volatile %struct.ScmObj* %current_45args47343, %struct.ScmObj** %stackaddr$prim48951, align 8
%stackaddr$prim48952 = alloca %struct.ScmObj*, align 8
%a40219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47343)
store volatile %struct.ScmObj* %a40219, %struct.ScmObj** %stackaddr$prim48952, align 8
%stackaddr$prim48953 = alloca %struct.ScmObj*, align 8
%current_45args47344 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47343)
store volatile %struct.ScmObj* %current_45args47344, %struct.ScmObj** %stackaddr$prim48953, align 8
%stackaddr$prim48954 = alloca %struct.ScmObj*, align 8
%b40218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47344)
store volatile %struct.ScmObj* %b40218, %struct.ScmObj** %stackaddr$prim48954, align 8
%stackaddr$prim48955 = alloca %struct.ScmObj*, align 8
%cpsprim40493 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40219, %struct.ScmObj* %b40218)
store volatile %struct.ScmObj* %cpsprim40493, %struct.ScmObj** %stackaddr$prim48955, align 8
%ae42000 = call %struct.ScmObj* @const_init_int(i64 0)
%args47346$k40492$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48956 = alloca %struct.ScmObj*, align 8
%args47346$k40492$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40493, %struct.ScmObj* %args47346$k40492$0)
store volatile %struct.ScmObj* %args47346$k40492$1, %struct.ScmObj** %stackaddr$prim48956, align 8
%stackaddr$prim48957 = alloca %struct.ScmObj*, align 8
%args47346$k40492$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae42000, %struct.ScmObj* %args47346$k40492$1)
store volatile %struct.ScmObj* %args47346$k40492$2, %struct.ScmObj** %stackaddr$prim48957, align 8
%clofunc48958 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40492)
musttail call tailcc void %clofunc48958(%struct.ScmObj* %k40492, %struct.ScmObj* %args47346$k40492$2)
ret void
}

define tailcc void @proc_clo$ae41972(%struct.ScmObj* %env$ae41972,%struct.ScmObj* %current_45args47349) {
%stackaddr$prim48959 = alloca %struct.ScmObj*, align 8
%k40494 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47349)
store volatile %struct.ScmObj* %k40494, %struct.ScmObj** %stackaddr$prim48959, align 8
%stackaddr$prim48960 = alloca %struct.ScmObj*, align 8
%current_45args47350 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47349)
store volatile %struct.ScmObj* %current_45args47350, %struct.ScmObj** %stackaddr$prim48960, align 8
%stackaddr$prim48961 = alloca %struct.ScmObj*, align 8
%x40214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47350)
store volatile %struct.ScmObj* %x40214, %struct.ScmObj** %stackaddr$prim48961, align 8
%stackaddr$prim48962 = alloca %struct.ScmObj*, align 8
%cpsprim40495 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40214)
store volatile %struct.ScmObj* %cpsprim40495, %struct.ScmObj** %stackaddr$prim48962, align 8
%ae41975 = call %struct.ScmObj* @const_init_int(i64 0)
%args47352$k40494$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48963 = alloca %struct.ScmObj*, align 8
%args47352$k40494$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40495, %struct.ScmObj* %args47352$k40494$0)
store volatile %struct.ScmObj* %args47352$k40494$1, %struct.ScmObj** %stackaddr$prim48963, align 8
%stackaddr$prim48964 = alloca %struct.ScmObj*, align 8
%args47352$k40494$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41975, %struct.ScmObj* %args47352$k40494$1)
store volatile %struct.ScmObj* %args47352$k40494$2, %struct.ScmObj** %stackaddr$prim48964, align 8
%clofunc48965 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40494)
musttail call tailcc void %clofunc48965(%struct.ScmObj* %k40494, %struct.ScmObj* %args47352$k40494$2)
ret void
}

define tailcc void @proc_clo$ae41948(%struct.ScmObj* %env$ae41948,%struct.ScmObj* %current_45args47355) {
%stackaddr$prim48966 = alloca %struct.ScmObj*, align 8
%k40496 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47355)
store volatile %struct.ScmObj* %k40496, %struct.ScmObj** %stackaddr$prim48966, align 8
%stackaddr$prim48967 = alloca %struct.ScmObj*, align 8
%current_45args47356 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47355)
store volatile %struct.ScmObj* %current_45args47356, %struct.ScmObj** %stackaddr$prim48967, align 8
%stackaddr$prim48968 = alloca %struct.ScmObj*, align 8
%x40216 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47356)
store volatile %struct.ScmObj* %x40216, %struct.ScmObj** %stackaddr$prim48968, align 8
%stackaddr$prim48969 = alloca %struct.ScmObj*, align 8
%cpsprim40497 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40216)
store volatile %struct.ScmObj* %cpsprim40497, %struct.ScmObj** %stackaddr$prim48969, align 8
%ae41951 = call %struct.ScmObj* @const_init_int(i64 0)
%args47358$k40496$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48970 = alloca %struct.ScmObj*, align 8
%args47358$k40496$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40497, %struct.ScmObj* %args47358$k40496$0)
store volatile %struct.ScmObj* %args47358$k40496$1, %struct.ScmObj** %stackaddr$prim48970, align 8
%stackaddr$prim48971 = alloca %struct.ScmObj*, align 8
%args47358$k40496$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41951, %struct.ScmObj* %args47358$k40496$1)
store volatile %struct.ScmObj* %args47358$k40496$2, %struct.ScmObj** %stackaddr$prim48971, align 8
%clofunc48972 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40496)
musttail call tailcc void %clofunc48972(%struct.ScmObj* %k40496, %struct.ScmObj* %args47358$k40496$2)
ret void
}

define tailcc void @proc_clo$ae41900(%struct.ScmObj* %env$ae41900,%struct.ScmObj* %current_45args47361) {
%stackaddr$prim48973 = alloca %struct.ScmObj*, align 8
%k40498 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47361)
store volatile %struct.ScmObj* %k40498, %struct.ScmObj** %stackaddr$prim48973, align 8
%stackaddr$prim48974 = alloca %struct.ScmObj*, align 8
%current_45args47362 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47361)
store volatile %struct.ScmObj* %current_45args47362, %struct.ScmObj** %stackaddr$prim48974, align 8
%stackaddr$prim48975 = alloca %struct.ScmObj*, align 8
%lst40212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47362)
store volatile %struct.ScmObj* %lst40212, %struct.ScmObj** %stackaddr$prim48975, align 8
%stackaddr$prim48976 = alloca %struct.ScmObj*, align 8
%current_45args47363 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47362)
store volatile %struct.ScmObj* %current_45args47363, %struct.ScmObj** %stackaddr$prim48976, align 8
%stackaddr$prim48977 = alloca %struct.ScmObj*, align 8
%b40211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47363)
store volatile %struct.ScmObj* %b40211, %struct.ScmObj** %stackaddr$prim48977, align 8
%truthy$cmp48978 = call i64 @is_truthy_value(%struct.ScmObj* %b40211)
%cmp$cmp48978 = icmp eq i64 %truthy$cmp48978, 1
br i1 %cmp$cmp48978, label %truebranch$cmp48978, label %falsebranch$cmp48978
truebranch$cmp48978:
%ae41903 = call %struct.ScmObj* @const_init_int(i64 0)
%args47365$k40498$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48979 = alloca %struct.ScmObj*, align 8
%args47365$k40498$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40211, %struct.ScmObj* %args47365$k40498$0)
store volatile %struct.ScmObj* %args47365$k40498$1, %struct.ScmObj** %stackaddr$prim48979, align 8
%stackaddr$prim48980 = alloca %struct.ScmObj*, align 8
%args47365$k40498$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41903, %struct.ScmObj* %args47365$k40498$1)
store volatile %struct.ScmObj* %args47365$k40498$2, %struct.ScmObj** %stackaddr$prim48980, align 8
%clofunc48981 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40498)
musttail call tailcc void %clofunc48981(%struct.ScmObj* %k40498, %struct.ScmObj* %args47365$k40498$2)
ret void
falsebranch$cmp48978:
%stackaddr$prim48982 = alloca %struct.ScmObj*, align 8
%cpsprim40499 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40212)
store volatile %struct.ScmObj* %cpsprim40499, %struct.ScmObj** %stackaddr$prim48982, align 8
%ae41910 = call %struct.ScmObj* @const_init_int(i64 0)
%args47366$k40498$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48983 = alloca %struct.ScmObj*, align 8
%args47366$k40498$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40499, %struct.ScmObj* %args47366$k40498$0)
store volatile %struct.ScmObj* %args47366$k40498$1, %struct.ScmObj** %stackaddr$prim48983, align 8
%stackaddr$prim48984 = alloca %struct.ScmObj*, align 8
%args47366$k40498$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41910, %struct.ScmObj* %args47366$k40498$1)
store volatile %struct.ScmObj* %args47366$k40498$2, %struct.ScmObj** %stackaddr$prim48984, align 8
%clofunc48985 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40498)
musttail call tailcc void %clofunc48985(%struct.ScmObj* %k40498, %struct.ScmObj* %args47366$k40498$2)
ret void
}

define tailcc void @proc_clo$ae41741(%struct.ScmObj* %env$ae41741,%struct.ScmObj* %args4015040500) {
%stackaddr$env-ref48986 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41741, i64 0)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref48986
%stackaddr$env-ref48987 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41741, i64 1)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref48987
%stackaddr$env-ref48988 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41741, i64 2)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref48988
%stackaddr$prim48989 = alloca %struct.ScmObj*, align 8
%k40501 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4015040500)
store volatile %struct.ScmObj* %k40501, %struct.ScmObj** %stackaddr$prim48989, align 8
%stackaddr$prim48990 = alloca %struct.ScmObj*, align 8
%args40150 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4015040500)
store volatile %struct.ScmObj* %args40150, %struct.ScmObj** %stackaddr$prim48990, align 8
%stackaddr$prim48991 = alloca %struct.ScmObj*, align 8
%f40152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40150)
store volatile %struct.ScmObj* %f40152, %struct.ScmObj** %stackaddr$prim48991, align 8
%stackaddr$prim48992 = alloca %struct.ScmObj*, align 8
%lsts40151 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40150)
store volatile %struct.ScmObj* %lsts40151, %struct.ScmObj** %stackaddr$prim48992, align 8
%stackaddr$makeclosure48993 = alloca %struct.ScmObj*, align 8
%fptrToInt48994 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41746 to i64
%ae41746 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48994)
store volatile %struct.ScmObj* %ae41746, %struct.ScmObj** %stackaddr$makeclosure48993, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41746, %struct.ScmObj* %lsts40151, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41746, %struct.ScmObj* %k40501, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41746, %struct.ScmObj* %_37foldr40128, i64 2)
%ae41747 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure48995 = alloca %struct.ScmObj*, align 8
%fptrToInt48996 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41748 to i64
%ae41748 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt48996)
store volatile %struct.ScmObj* %ae41748, %struct.ScmObj** %stackaddr$makeclosure48995, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41748, %struct.ScmObj* %f40152, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41748, %struct.ScmObj* %_37last40145, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41748, %struct.ScmObj* %_37drop_45right40142, i64 2)
%args47385$ae41746$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim48997 = alloca %struct.ScmObj*, align 8
%args47385$ae41746$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41748, %struct.ScmObj* %args47385$ae41746$0)
store volatile %struct.ScmObj* %args47385$ae41746$1, %struct.ScmObj** %stackaddr$prim48997, align 8
%stackaddr$prim48998 = alloca %struct.ScmObj*, align 8
%args47385$ae41746$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41747, %struct.ScmObj* %args47385$ae41746$1)
store volatile %struct.ScmObj* %args47385$ae41746$2, %struct.ScmObj** %stackaddr$prim48998, align 8
%clofunc48999 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41746)
musttail call tailcc void %clofunc48999(%struct.ScmObj* %ae41746, %struct.ScmObj* %args47385$ae41746$2)
ret void
}

define tailcc void @proc_clo$ae41746(%struct.ScmObj* %env$ae41746,%struct.ScmObj* %current_45args47370) {
%stackaddr$env-ref49000 = alloca %struct.ScmObj*, align 8
%lsts40151 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41746, i64 0)
store %struct.ScmObj* %lsts40151, %struct.ScmObj** %stackaddr$env-ref49000
%stackaddr$env-ref49001 = alloca %struct.ScmObj*, align 8
%k40501 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41746, i64 1)
store %struct.ScmObj* %k40501, %struct.ScmObj** %stackaddr$env-ref49001
%stackaddr$env-ref49002 = alloca %struct.ScmObj*, align 8
%_37foldr40128 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41746, i64 2)
store %struct.ScmObj* %_37foldr40128, %struct.ScmObj** %stackaddr$env-ref49002
%stackaddr$prim49003 = alloca %struct.ScmObj*, align 8
%_95k40502 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47370)
store volatile %struct.ScmObj* %_95k40502, %struct.ScmObj** %stackaddr$prim49003, align 8
%stackaddr$prim49004 = alloca %struct.ScmObj*, align 8
%current_45args47371 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47370)
store volatile %struct.ScmObj* %current_45args47371, %struct.ScmObj** %stackaddr$prim49004, align 8
%stackaddr$prim49005 = alloca %struct.ScmObj*, align 8
%anf_45bind40285 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47371)
store volatile %struct.ScmObj* %anf_45bind40285, %struct.ScmObj** %stackaddr$prim49005, align 8
%ae41809 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49006 = alloca %struct.ScmObj*, align 8
%anf_45bind40286 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41809, %struct.ScmObj* %lsts40151)
store volatile %struct.ScmObj* %anf_45bind40286, %struct.ScmObj** %stackaddr$prim49006, align 8
%stackaddr$prim49007 = alloca %struct.ScmObj*, align 8
%anf_45bind40287 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40285, %struct.ScmObj* %anf_45bind40286)
store volatile %struct.ScmObj* %anf_45bind40287, %struct.ScmObj** %stackaddr$prim49007, align 8
%stackaddr$prim49008 = alloca %struct.ScmObj*, align 8
%cpsargs40503 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40501, %struct.ScmObj* %anf_45bind40287)
store volatile %struct.ScmObj* %cpsargs40503, %struct.ScmObj** %stackaddr$prim49008, align 8
%clofunc49009 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40128)
musttail call tailcc void %clofunc49009(%struct.ScmObj* %_37foldr40128, %struct.ScmObj* %cpsargs40503)
ret void
}

define tailcc void @proc_clo$ae41748(%struct.ScmObj* %env$ae41748,%struct.ScmObj* %fargs4015340504) {
%stackaddr$env-ref49010 = alloca %struct.ScmObj*, align 8
%f40152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41748, i64 0)
store %struct.ScmObj* %f40152, %struct.ScmObj** %stackaddr$env-ref49010
%stackaddr$env-ref49011 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41748, i64 1)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref49011
%stackaddr$env-ref49012 = alloca %struct.ScmObj*, align 8
%_37drop_45right40142 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41748, i64 2)
store %struct.ScmObj* %_37drop_45right40142, %struct.ScmObj** %stackaddr$env-ref49012
%stackaddr$prim49013 = alloca %struct.ScmObj*, align 8
%k40505 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4015340504)
store volatile %struct.ScmObj* %k40505, %struct.ScmObj** %stackaddr$prim49013, align 8
%stackaddr$prim49014 = alloca %struct.ScmObj*, align 8
%fargs40153 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4015340504)
store volatile %struct.ScmObj* %fargs40153, %struct.ScmObj** %stackaddr$prim49014, align 8
%stackaddr$makeclosure49015 = alloca %struct.ScmObj*, align 8
%fptrToInt49016 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41752 to i64
%ae41752 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49016)
store volatile %struct.ScmObj* %ae41752, %struct.ScmObj** %stackaddr$makeclosure49015, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41752, %struct.ScmObj* %k40505, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41752, %struct.ScmObj* %fargs40153, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41752, %struct.ScmObj* %f40152, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41752, %struct.ScmObj* %_37last40145, i64 3)
%ae41754 = call %struct.ScmObj* @const_init_int(i64 1)
%args47384$_37drop_45right40142$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49017 = alloca %struct.ScmObj*, align 8
%args47384$_37drop_45right40142$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41754, %struct.ScmObj* %args47384$_37drop_45right40142$0)
store volatile %struct.ScmObj* %args47384$_37drop_45right40142$1, %struct.ScmObj** %stackaddr$prim49017, align 8
%stackaddr$prim49018 = alloca %struct.ScmObj*, align 8
%args47384$_37drop_45right40142$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40153, %struct.ScmObj* %args47384$_37drop_45right40142$1)
store volatile %struct.ScmObj* %args47384$_37drop_45right40142$2, %struct.ScmObj** %stackaddr$prim49018, align 8
%stackaddr$prim49019 = alloca %struct.ScmObj*, align 8
%args47384$_37drop_45right40142$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41752, %struct.ScmObj* %args47384$_37drop_45right40142$2)
store volatile %struct.ScmObj* %args47384$_37drop_45right40142$3, %struct.ScmObj** %stackaddr$prim49019, align 8
%clofunc49020 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right40142)
musttail call tailcc void %clofunc49020(%struct.ScmObj* %_37drop_45right40142, %struct.ScmObj* %args47384$_37drop_45right40142$3)
ret void
}

define tailcc void @proc_clo$ae41752(%struct.ScmObj* %env$ae41752,%struct.ScmObj* %current_45args47373) {
%stackaddr$env-ref49021 = alloca %struct.ScmObj*, align 8
%k40505 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41752, i64 0)
store %struct.ScmObj* %k40505, %struct.ScmObj** %stackaddr$env-ref49021
%stackaddr$env-ref49022 = alloca %struct.ScmObj*, align 8
%fargs40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41752, i64 1)
store %struct.ScmObj* %fargs40153, %struct.ScmObj** %stackaddr$env-ref49022
%stackaddr$env-ref49023 = alloca %struct.ScmObj*, align 8
%f40152 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41752, i64 2)
store %struct.ScmObj* %f40152, %struct.ScmObj** %stackaddr$env-ref49023
%stackaddr$env-ref49024 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41752, i64 3)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref49024
%stackaddr$prim49025 = alloca %struct.ScmObj*, align 8
%_95k40506 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47373)
store volatile %struct.ScmObj* %_95k40506, %struct.ScmObj** %stackaddr$prim49025, align 8
%stackaddr$prim49026 = alloca %struct.ScmObj*, align 8
%current_45args47374 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47373)
store volatile %struct.ScmObj* %current_45args47374, %struct.ScmObj** %stackaddr$prim49026, align 8
%stackaddr$prim49027 = alloca %struct.ScmObj*, align 8
%anf_45bind40282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47374)
store volatile %struct.ScmObj* %anf_45bind40282, %struct.ScmObj** %stackaddr$prim49027, align 8
%stackaddr$makeclosure49028 = alloca %struct.ScmObj*, align 8
%fptrToInt49029 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41759 to i64
%ae41759 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49029)
store volatile %struct.ScmObj* %ae41759, %struct.ScmObj** %stackaddr$makeclosure49028, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41759, %struct.ScmObj* %k40505, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41759, %struct.ScmObj* %fargs40153, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41759, %struct.ScmObj* %_37last40145, i64 2)
%stackaddr$prim49030 = alloca %struct.ScmObj*, align 8
%cpsargs40510 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41759, %struct.ScmObj* %anf_45bind40282)
store volatile %struct.ScmObj* %cpsargs40510, %struct.ScmObj** %stackaddr$prim49030, align 8
%clofunc49031 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40152)
musttail call tailcc void %clofunc49031(%struct.ScmObj* %f40152, %struct.ScmObj* %cpsargs40510)
ret void
}

define tailcc void @proc_clo$ae41759(%struct.ScmObj* %env$ae41759,%struct.ScmObj* %current_45args47376) {
%stackaddr$env-ref49032 = alloca %struct.ScmObj*, align 8
%k40505 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41759, i64 0)
store %struct.ScmObj* %k40505, %struct.ScmObj** %stackaddr$env-ref49032
%stackaddr$env-ref49033 = alloca %struct.ScmObj*, align 8
%fargs40153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41759, i64 1)
store %struct.ScmObj* %fargs40153, %struct.ScmObj** %stackaddr$env-ref49033
%stackaddr$env-ref49034 = alloca %struct.ScmObj*, align 8
%_37last40145 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41759, i64 2)
store %struct.ScmObj* %_37last40145, %struct.ScmObj** %stackaddr$env-ref49034
%stackaddr$prim49035 = alloca %struct.ScmObj*, align 8
%_95k40507 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47376)
store volatile %struct.ScmObj* %_95k40507, %struct.ScmObj** %stackaddr$prim49035, align 8
%stackaddr$prim49036 = alloca %struct.ScmObj*, align 8
%current_45args47377 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47376)
store volatile %struct.ScmObj* %current_45args47377, %struct.ScmObj** %stackaddr$prim49036, align 8
%stackaddr$prim49037 = alloca %struct.ScmObj*, align 8
%anf_45bind40283 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47377)
store volatile %struct.ScmObj* %anf_45bind40283, %struct.ScmObj** %stackaddr$prim49037, align 8
%stackaddr$makeclosure49038 = alloca %struct.ScmObj*, align 8
%fptrToInt49039 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41764 to i64
%ae41764 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49039)
store volatile %struct.ScmObj* %ae41764, %struct.ScmObj** %stackaddr$makeclosure49038, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41764, %struct.ScmObj* %k40505, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41764, %struct.ScmObj* %anf_45bind40283, i64 1)
%args47383$_37last40145$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49040 = alloca %struct.ScmObj*, align 8
%args47383$_37last40145$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs40153, %struct.ScmObj* %args47383$_37last40145$0)
store volatile %struct.ScmObj* %args47383$_37last40145$1, %struct.ScmObj** %stackaddr$prim49040, align 8
%stackaddr$prim49041 = alloca %struct.ScmObj*, align 8
%args47383$_37last40145$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41764, %struct.ScmObj* %args47383$_37last40145$1)
store volatile %struct.ScmObj* %args47383$_37last40145$2, %struct.ScmObj** %stackaddr$prim49041, align 8
%clofunc49042 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last40145)
musttail call tailcc void %clofunc49042(%struct.ScmObj* %_37last40145, %struct.ScmObj* %args47383$_37last40145$2)
ret void
}

define tailcc void @proc_clo$ae41764(%struct.ScmObj* %env$ae41764,%struct.ScmObj* %current_45args47379) {
%stackaddr$env-ref49043 = alloca %struct.ScmObj*, align 8
%k40505 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41764, i64 0)
store %struct.ScmObj* %k40505, %struct.ScmObj** %stackaddr$env-ref49043
%stackaddr$env-ref49044 = alloca %struct.ScmObj*, align 8
%anf_45bind40283 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41764, i64 1)
store %struct.ScmObj* %anf_45bind40283, %struct.ScmObj** %stackaddr$env-ref49044
%stackaddr$prim49045 = alloca %struct.ScmObj*, align 8
%_95k40508 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47379)
store volatile %struct.ScmObj* %_95k40508, %struct.ScmObj** %stackaddr$prim49045, align 8
%stackaddr$prim49046 = alloca %struct.ScmObj*, align 8
%current_45args47380 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47379)
store volatile %struct.ScmObj* %current_45args47380, %struct.ScmObj** %stackaddr$prim49046, align 8
%stackaddr$prim49047 = alloca %struct.ScmObj*, align 8
%anf_45bind40284 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47380)
store volatile %struct.ScmObj* %anf_45bind40284, %struct.ScmObj** %stackaddr$prim49047, align 8
%stackaddr$prim49048 = alloca %struct.ScmObj*, align 8
%cpsprim40509 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40283, %struct.ScmObj* %anf_45bind40284)
store volatile %struct.ScmObj* %cpsprim40509, %struct.ScmObj** %stackaddr$prim49048, align 8
%ae41769 = call %struct.ScmObj* @const_init_int(i64 0)
%args47382$k40505$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49049 = alloca %struct.ScmObj*, align 8
%args47382$k40505$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40509, %struct.ScmObj* %args47382$k40505$0)
store volatile %struct.ScmObj* %args47382$k40505$1, %struct.ScmObj** %stackaddr$prim49049, align 8
%stackaddr$prim49050 = alloca %struct.ScmObj*, align 8
%args47382$k40505$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41769, %struct.ScmObj* %args47382$k40505$1)
store volatile %struct.ScmObj* %args47382$k40505$2, %struct.ScmObj** %stackaddr$prim49050, align 8
%clofunc49051 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40505)
musttail call tailcc void %clofunc49051(%struct.ScmObj* %k40505, %struct.ScmObj* %args47382$k40505$2)
ret void
}

define tailcc void @proc_clo$ae41664(%struct.ScmObj* %env$ae41664,%struct.ScmObj* %current_45args47387) {
%stackaddr$env-ref49052 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41664, i64 0)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref49052
%stackaddr$prim49053 = alloca %struct.ScmObj*, align 8
%k40511 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47387)
store volatile %struct.ScmObj* %k40511, %struct.ScmObj** %stackaddr$prim49053, align 8
%stackaddr$prim49054 = alloca %struct.ScmObj*, align 8
%current_45args47388 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47387)
store volatile %struct.ScmObj* %current_45args47388, %struct.ScmObj** %stackaddr$prim49054, align 8
%stackaddr$prim49055 = alloca %struct.ScmObj*, align 8
%f40156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47388)
store volatile %struct.ScmObj* %f40156, %struct.ScmObj** %stackaddr$prim49055, align 8
%stackaddr$prim49056 = alloca %struct.ScmObj*, align 8
%current_45args47389 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47388)
store volatile %struct.ScmObj* %current_45args47389, %struct.ScmObj** %stackaddr$prim49056, align 8
%stackaddr$prim49057 = alloca %struct.ScmObj*, align 8
%lst40155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47389)
store volatile %struct.ScmObj* %lst40155, %struct.ScmObj** %stackaddr$prim49057, align 8
%stackaddr$makeclosure49058 = alloca %struct.ScmObj*, align 8
%fptrToInt49059 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41665 to i64
%ae41665 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49059)
store volatile %struct.ScmObj* %ae41665, %struct.ScmObj** %stackaddr$makeclosure49058, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41665, %struct.ScmObj* %lst40155, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41665, %struct.ScmObj* %_37foldr140123, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41665, %struct.ScmObj* %k40511, i64 2)
%ae41666 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49060 = alloca %struct.ScmObj*, align 8
%fptrToInt49061 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41667 to i64
%ae41667 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49061)
store volatile %struct.ScmObj* %ae41667, %struct.ScmObj** %stackaddr$makeclosure49060, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41667, %struct.ScmObj* %f40156, i64 0)
%args47404$ae41665$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49062 = alloca %struct.ScmObj*, align 8
%args47404$ae41665$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41667, %struct.ScmObj* %args47404$ae41665$0)
store volatile %struct.ScmObj* %args47404$ae41665$1, %struct.ScmObj** %stackaddr$prim49062, align 8
%stackaddr$prim49063 = alloca %struct.ScmObj*, align 8
%args47404$ae41665$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41666, %struct.ScmObj* %args47404$ae41665$1)
store volatile %struct.ScmObj* %args47404$ae41665$2, %struct.ScmObj** %stackaddr$prim49063, align 8
%clofunc49064 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41665)
musttail call tailcc void %clofunc49064(%struct.ScmObj* %ae41665, %struct.ScmObj* %args47404$ae41665$2)
ret void
}

define tailcc void @proc_clo$ae41665(%struct.ScmObj* %env$ae41665,%struct.ScmObj* %current_45args47391) {
%stackaddr$env-ref49065 = alloca %struct.ScmObj*, align 8
%lst40155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41665, i64 0)
store %struct.ScmObj* %lst40155, %struct.ScmObj** %stackaddr$env-ref49065
%stackaddr$env-ref49066 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41665, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref49066
%stackaddr$env-ref49067 = alloca %struct.ScmObj*, align 8
%k40511 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41665, i64 2)
store %struct.ScmObj* %k40511, %struct.ScmObj** %stackaddr$env-ref49067
%stackaddr$prim49068 = alloca %struct.ScmObj*, align 8
%_95k40512 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47391)
store volatile %struct.ScmObj* %_95k40512, %struct.ScmObj** %stackaddr$prim49068, align 8
%stackaddr$prim49069 = alloca %struct.ScmObj*, align 8
%current_45args47392 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47391)
store volatile %struct.ScmObj* %current_45args47392, %struct.ScmObj** %stackaddr$prim49069, align 8
%stackaddr$prim49070 = alloca %struct.ScmObj*, align 8
%anf_45bind40281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47392)
store volatile %struct.ScmObj* %anf_45bind40281, %struct.ScmObj** %stackaddr$prim49070, align 8
%ae41699 = call %struct.ScmObj* @const_init_null()
%args47394$_37foldr140123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49071 = alloca %struct.ScmObj*, align 8
%args47394$_37foldr140123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40155, %struct.ScmObj* %args47394$_37foldr140123$0)
store volatile %struct.ScmObj* %args47394$_37foldr140123$1, %struct.ScmObj** %stackaddr$prim49071, align 8
%stackaddr$prim49072 = alloca %struct.ScmObj*, align 8
%args47394$_37foldr140123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41699, %struct.ScmObj* %args47394$_37foldr140123$1)
store volatile %struct.ScmObj* %args47394$_37foldr140123$2, %struct.ScmObj** %stackaddr$prim49072, align 8
%stackaddr$prim49073 = alloca %struct.ScmObj*, align 8
%args47394$_37foldr140123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40281, %struct.ScmObj* %args47394$_37foldr140123$2)
store volatile %struct.ScmObj* %args47394$_37foldr140123$3, %struct.ScmObj** %stackaddr$prim49073, align 8
%stackaddr$prim49074 = alloca %struct.ScmObj*, align 8
%args47394$_37foldr140123$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40511, %struct.ScmObj* %args47394$_37foldr140123$3)
store volatile %struct.ScmObj* %args47394$_37foldr140123$4, %struct.ScmObj** %stackaddr$prim49074, align 8
%clofunc49075 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140123)
musttail call tailcc void %clofunc49075(%struct.ScmObj* %_37foldr140123, %struct.ScmObj* %args47394$_37foldr140123$4)
ret void
}

define tailcc void @proc_clo$ae41667(%struct.ScmObj* %env$ae41667,%struct.ScmObj* %current_45args47395) {
%stackaddr$env-ref49076 = alloca %struct.ScmObj*, align 8
%f40156 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41667, i64 0)
store %struct.ScmObj* %f40156, %struct.ScmObj** %stackaddr$env-ref49076
%stackaddr$prim49077 = alloca %struct.ScmObj*, align 8
%k40513 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47395)
store volatile %struct.ScmObj* %k40513, %struct.ScmObj** %stackaddr$prim49077, align 8
%stackaddr$prim49078 = alloca %struct.ScmObj*, align 8
%current_45args47396 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47395)
store volatile %struct.ScmObj* %current_45args47396, %struct.ScmObj** %stackaddr$prim49078, align 8
%stackaddr$prim49079 = alloca %struct.ScmObj*, align 8
%v40158 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47396)
store volatile %struct.ScmObj* %v40158, %struct.ScmObj** %stackaddr$prim49079, align 8
%stackaddr$prim49080 = alloca %struct.ScmObj*, align 8
%current_45args47397 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47396)
store volatile %struct.ScmObj* %current_45args47397, %struct.ScmObj** %stackaddr$prim49080, align 8
%stackaddr$prim49081 = alloca %struct.ScmObj*, align 8
%r40157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47397)
store volatile %struct.ScmObj* %r40157, %struct.ScmObj** %stackaddr$prim49081, align 8
%stackaddr$makeclosure49082 = alloca %struct.ScmObj*, align 8
%fptrToInt49083 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41669 to i64
%ae41669 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49083)
store volatile %struct.ScmObj* %ae41669, %struct.ScmObj** %stackaddr$makeclosure49082, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41669, %struct.ScmObj* %k40513, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41669, %struct.ScmObj* %r40157, i64 1)
%args47403$f40156$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49084 = alloca %struct.ScmObj*, align 8
%args47403$f40156$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v40158, %struct.ScmObj* %args47403$f40156$0)
store volatile %struct.ScmObj* %args47403$f40156$1, %struct.ScmObj** %stackaddr$prim49084, align 8
%stackaddr$prim49085 = alloca %struct.ScmObj*, align 8
%args47403$f40156$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41669, %struct.ScmObj* %args47403$f40156$1)
store volatile %struct.ScmObj* %args47403$f40156$2, %struct.ScmObj** %stackaddr$prim49085, align 8
%clofunc49086 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40156)
musttail call tailcc void %clofunc49086(%struct.ScmObj* %f40156, %struct.ScmObj* %args47403$f40156$2)
ret void
}

define tailcc void @proc_clo$ae41669(%struct.ScmObj* %env$ae41669,%struct.ScmObj* %current_45args47399) {
%stackaddr$env-ref49087 = alloca %struct.ScmObj*, align 8
%k40513 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41669, i64 0)
store %struct.ScmObj* %k40513, %struct.ScmObj** %stackaddr$env-ref49087
%stackaddr$env-ref49088 = alloca %struct.ScmObj*, align 8
%r40157 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41669, i64 1)
store %struct.ScmObj* %r40157, %struct.ScmObj** %stackaddr$env-ref49088
%stackaddr$prim49089 = alloca %struct.ScmObj*, align 8
%_95k40514 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47399)
store volatile %struct.ScmObj* %_95k40514, %struct.ScmObj** %stackaddr$prim49089, align 8
%stackaddr$prim49090 = alloca %struct.ScmObj*, align 8
%current_45args47400 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47399)
store volatile %struct.ScmObj* %current_45args47400, %struct.ScmObj** %stackaddr$prim49090, align 8
%stackaddr$prim49091 = alloca %struct.ScmObj*, align 8
%anf_45bind40280 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47400)
store volatile %struct.ScmObj* %anf_45bind40280, %struct.ScmObj** %stackaddr$prim49091, align 8
%stackaddr$prim49092 = alloca %struct.ScmObj*, align 8
%cpsprim40515 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40280, %struct.ScmObj* %r40157)
store volatile %struct.ScmObj* %cpsprim40515, %struct.ScmObj** %stackaddr$prim49092, align 8
%ae41674 = call %struct.ScmObj* @const_init_int(i64 0)
%args47402$k40513$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49093 = alloca %struct.ScmObj*, align 8
%args47402$k40513$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40515, %struct.ScmObj* %args47402$k40513$0)
store volatile %struct.ScmObj* %args47402$k40513$1, %struct.ScmObj** %stackaddr$prim49093, align 8
%stackaddr$prim49094 = alloca %struct.ScmObj*, align 8
%args47402$k40513$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41674, %struct.ScmObj* %args47402$k40513$1)
store volatile %struct.ScmObj* %args47402$k40513$2, %struct.ScmObj** %stackaddr$prim49094, align 8
%clofunc49095 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40513)
musttail call tailcc void %clofunc49095(%struct.ScmObj* %k40513, %struct.ScmObj* %args47402$k40513$2)
ret void
}

define tailcc void @proc_clo$ae41278(%struct.ScmObj* %env$ae41278,%struct.ScmObj* %current_45args47407) {
%stackaddr$env-ref49096 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41278, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref49096
%stackaddr$env-ref49097 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41278, i64 1)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref49097
%stackaddr$prim49098 = alloca %struct.ScmObj*, align 8
%k40516 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47407)
store volatile %struct.ScmObj* %k40516, %struct.ScmObj** %stackaddr$prim49098, align 8
%stackaddr$prim49099 = alloca %struct.ScmObj*, align 8
%current_45args47408 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47407)
store volatile %struct.ScmObj* %current_45args47408, %struct.ScmObj** %stackaddr$prim49099, align 8
%stackaddr$prim49100 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47408)
store volatile %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$prim49100, align 8
%ae41280 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49101 = alloca %struct.ScmObj*, align 8
%fptrToInt49102 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41281 to i64
%ae41281 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49102)
store volatile %struct.ScmObj* %ae41281, %struct.ScmObj** %stackaddr$makeclosure49101, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41281, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41281, %struct.ScmObj* %_37foldr40129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41281, %struct.ScmObj* %_37foldr140123, i64 2)
%args47465$k40516$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49103 = alloca %struct.ScmObj*, align 8
%args47465$k40516$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41281, %struct.ScmObj* %args47465$k40516$0)
store volatile %struct.ScmObj* %args47465$k40516$1, %struct.ScmObj** %stackaddr$prim49103, align 8
%stackaddr$prim49104 = alloca %struct.ScmObj*, align 8
%args47465$k40516$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41280, %struct.ScmObj* %args47465$k40516$1)
store volatile %struct.ScmObj* %args47465$k40516$2, %struct.ScmObj** %stackaddr$prim49104, align 8
%clofunc49105 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40516)
musttail call tailcc void %clofunc49105(%struct.ScmObj* %k40516, %struct.ScmObj* %args47465$k40516$2)
ret void
}

define tailcc void @proc_clo$ae41281(%struct.ScmObj* %env$ae41281,%struct.ScmObj* %args4013040517) {
%stackaddr$env-ref49106 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41281, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref49106
%stackaddr$env-ref49107 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41281, i64 1)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref49107
%stackaddr$env-ref49108 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41281, i64 2)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref49108
%stackaddr$prim49109 = alloca %struct.ScmObj*, align 8
%k40518 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4013040517)
store volatile %struct.ScmObj* %k40518, %struct.ScmObj** %stackaddr$prim49109, align 8
%stackaddr$prim49110 = alloca %struct.ScmObj*, align 8
%args40130 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4013040517)
store volatile %struct.ScmObj* %args40130, %struct.ScmObj** %stackaddr$prim49110, align 8
%stackaddr$prim49111 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args40130)
store volatile %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$prim49111, align 8
%stackaddr$prim49112 = alloca %struct.ScmObj*, align 8
%anf_45bind40267 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40130)
store volatile %struct.ScmObj* %anf_45bind40267, %struct.ScmObj** %stackaddr$prim49112, align 8
%stackaddr$prim49113 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind40267)
store volatile %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$prim49113, align 8
%stackaddr$prim49114 = alloca %struct.ScmObj*, align 8
%anf_45bind40268 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args40130)
store volatile %struct.ScmObj* %anf_45bind40268, %struct.ScmObj** %stackaddr$prim49114, align 8
%stackaddr$prim49115 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind40268)
store volatile %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$prim49115, align 8
%stackaddr$makeclosure49116 = alloca %struct.ScmObj*, align 8
%fptrToInt49117 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41289 to i64
%ae41289 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49117)
store volatile %struct.ScmObj* %ae41289, %struct.ScmObj** %stackaddr$makeclosure49116, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41289, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41289, %struct.ScmObj* %k40518, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41289, %struct.ScmObj* %f40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41289, %struct.ScmObj* %acc40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41289, %struct.ScmObj* %lsts40131, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41289, %struct.ScmObj* %_37foldr40129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41289, %struct.ScmObj* %_37foldr140123, i64 6)
%ae41290 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49118 = alloca %struct.ScmObj*, align 8
%fptrToInt49119 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41291 to i64
%ae41291 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49119)
store volatile %struct.ScmObj* %ae41291, %struct.ScmObj** %stackaddr$makeclosure49118, align 8
%args47464$ae41289$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49120 = alloca %struct.ScmObj*, align 8
%args47464$ae41289$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41291, %struct.ScmObj* %args47464$ae41289$0)
store volatile %struct.ScmObj* %args47464$ae41289$1, %struct.ScmObj** %stackaddr$prim49120, align 8
%stackaddr$prim49121 = alloca %struct.ScmObj*, align 8
%args47464$ae41289$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41290, %struct.ScmObj* %args47464$ae41289$1)
store volatile %struct.ScmObj* %args47464$ae41289$2, %struct.ScmObj** %stackaddr$prim49121, align 8
%clofunc49122 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41289)
musttail call tailcc void %clofunc49122(%struct.ScmObj* %ae41289, %struct.ScmObj* %args47464$ae41289$2)
ret void
}

define tailcc void @proc_clo$ae41289(%struct.ScmObj* %env$ae41289,%struct.ScmObj* %current_45args47410) {
%stackaddr$env-ref49123 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41289, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref49123
%stackaddr$env-ref49124 = alloca %struct.ScmObj*, align 8
%k40518 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41289, i64 1)
store %struct.ScmObj* %k40518, %struct.ScmObj** %stackaddr$env-ref49124
%stackaddr$env-ref49125 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41289, i64 2)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref49125
%stackaddr$env-ref49126 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41289, i64 3)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref49126
%stackaddr$env-ref49127 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41289, i64 4)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref49127
%stackaddr$env-ref49128 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41289, i64 5)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref49128
%stackaddr$env-ref49129 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41289, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref49129
%stackaddr$prim49130 = alloca %struct.ScmObj*, align 8
%_95k40519 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47410)
store volatile %struct.ScmObj* %_95k40519, %struct.ScmObj** %stackaddr$prim49130, align 8
%stackaddr$prim49131 = alloca %struct.ScmObj*, align 8
%current_45args47411 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47410)
store volatile %struct.ScmObj* %current_45args47411, %struct.ScmObj** %stackaddr$prim49131, align 8
%stackaddr$prim49132 = alloca %struct.ScmObj*, align 8
%anf_45bind40269 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47411)
store volatile %struct.ScmObj* %anf_45bind40269, %struct.ScmObj** %stackaddr$prim49132, align 8
%stackaddr$makeclosure49133 = alloca %struct.ScmObj*, align 8
%fptrToInt49134 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41321 to i64
%ae41321 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49134)
store volatile %struct.ScmObj* %ae41321, %struct.ScmObj** %stackaddr$makeclosure49133, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41321, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41321, %struct.ScmObj* %k40518, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41321, %struct.ScmObj* %f40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41321, %struct.ScmObj* %acc40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41321, %struct.ScmObj* %lsts40131, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41321, %struct.ScmObj* %_37foldr40129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41321, %struct.ScmObj* %_37foldr140123, i64 6)
%ae41323 = call %struct.ScmObj* @const_init_false()
%args47457$_37foldr140123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49135 = alloca %struct.ScmObj*, align 8
%args47457$_37foldr140123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40131, %struct.ScmObj* %args47457$_37foldr140123$0)
store volatile %struct.ScmObj* %args47457$_37foldr140123$1, %struct.ScmObj** %stackaddr$prim49135, align 8
%stackaddr$prim49136 = alloca %struct.ScmObj*, align 8
%args47457$_37foldr140123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41323, %struct.ScmObj* %args47457$_37foldr140123$1)
store volatile %struct.ScmObj* %args47457$_37foldr140123$2, %struct.ScmObj** %stackaddr$prim49136, align 8
%stackaddr$prim49137 = alloca %struct.ScmObj*, align 8
%args47457$_37foldr140123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40269, %struct.ScmObj* %args47457$_37foldr140123$2)
store volatile %struct.ScmObj* %args47457$_37foldr140123$3, %struct.ScmObj** %stackaddr$prim49137, align 8
%stackaddr$prim49138 = alloca %struct.ScmObj*, align 8
%args47457$_37foldr140123$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41321, %struct.ScmObj* %args47457$_37foldr140123$3)
store volatile %struct.ScmObj* %args47457$_37foldr140123$4, %struct.ScmObj** %stackaddr$prim49138, align 8
%clofunc49139 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140123)
musttail call tailcc void %clofunc49139(%struct.ScmObj* %_37foldr140123, %struct.ScmObj* %args47457$_37foldr140123$4)
ret void
}

define tailcc void @proc_clo$ae41321(%struct.ScmObj* %env$ae41321,%struct.ScmObj* %current_45args47413) {
%stackaddr$env-ref49140 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41321, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref49140
%stackaddr$env-ref49141 = alloca %struct.ScmObj*, align 8
%k40518 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41321, i64 1)
store %struct.ScmObj* %k40518, %struct.ScmObj** %stackaddr$env-ref49141
%stackaddr$env-ref49142 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41321, i64 2)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref49142
%stackaddr$env-ref49143 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41321, i64 3)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref49143
%stackaddr$env-ref49144 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41321, i64 4)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref49144
%stackaddr$env-ref49145 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41321, i64 5)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref49145
%stackaddr$env-ref49146 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41321, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref49146
%stackaddr$prim49147 = alloca %struct.ScmObj*, align 8
%_95k40520 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47413)
store volatile %struct.ScmObj* %_95k40520, %struct.ScmObj** %stackaddr$prim49147, align 8
%stackaddr$prim49148 = alloca %struct.ScmObj*, align 8
%current_45args47414 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47413)
store volatile %struct.ScmObj* %current_45args47414, %struct.ScmObj** %stackaddr$prim49148, align 8
%stackaddr$prim49149 = alloca %struct.ScmObj*, align 8
%anf_45bind40270 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47414)
store volatile %struct.ScmObj* %anf_45bind40270, %struct.ScmObj** %stackaddr$prim49149, align 8
%truthy$cmp49150 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40270)
%cmp$cmp49150 = icmp eq i64 %truthy$cmp49150, 1
br i1 %cmp$cmp49150, label %truebranch$cmp49150, label %falsebranch$cmp49150
truebranch$cmp49150:
%ae41332 = call %struct.ScmObj* @const_init_int(i64 0)
%args47416$k40518$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49151 = alloca %struct.ScmObj*, align 8
%args47416$k40518$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40132, %struct.ScmObj* %args47416$k40518$0)
store volatile %struct.ScmObj* %args47416$k40518$1, %struct.ScmObj** %stackaddr$prim49151, align 8
%stackaddr$prim49152 = alloca %struct.ScmObj*, align 8
%args47416$k40518$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41332, %struct.ScmObj* %args47416$k40518$1)
store volatile %struct.ScmObj* %args47416$k40518$2, %struct.ScmObj** %stackaddr$prim49152, align 8
%clofunc49153 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40518)
musttail call tailcc void %clofunc49153(%struct.ScmObj* %k40518, %struct.ScmObj* %args47416$k40518$2)
ret void
falsebranch$cmp49150:
%stackaddr$makeclosure49154 = alloca %struct.ScmObj*, align 8
%fptrToInt49155 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41337 to i64
%ae41337 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49155)
store volatile %struct.ScmObj* %ae41337, %struct.ScmObj** %stackaddr$makeclosure49154, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41337, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41337, %struct.ScmObj* %k40518, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41337, %struct.ScmObj* %f40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41337, %struct.ScmObj* %acc40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41337, %struct.ScmObj* %lsts40131, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41337, %struct.ScmObj* %_37foldr40129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41337, %struct.ScmObj* %_37foldr140123, i64 6)
%ae41338 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49156 = alloca %struct.ScmObj*, align 8
%fptrToInt49157 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41339 to i64
%ae41339 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49157)
store volatile %struct.ScmObj* %ae41339, %struct.ScmObj** %stackaddr$makeclosure49156, align 8
%args47456$ae41337$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49158 = alloca %struct.ScmObj*, align 8
%args47456$ae41337$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41339, %struct.ScmObj* %args47456$ae41337$0)
store volatile %struct.ScmObj* %args47456$ae41337$1, %struct.ScmObj** %stackaddr$prim49158, align 8
%stackaddr$prim49159 = alloca %struct.ScmObj*, align 8
%args47456$ae41337$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41338, %struct.ScmObj* %args47456$ae41337$1)
store volatile %struct.ScmObj* %args47456$ae41337$2, %struct.ScmObj** %stackaddr$prim49159, align 8
%clofunc49160 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41337)
musttail call tailcc void %clofunc49160(%struct.ScmObj* %ae41337, %struct.ScmObj* %args47456$ae41337$2)
ret void
}

define tailcc void @proc_clo$ae41337(%struct.ScmObj* %env$ae41337,%struct.ScmObj* %current_45args47417) {
%stackaddr$env-ref49161 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41337, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref49161
%stackaddr$env-ref49162 = alloca %struct.ScmObj*, align 8
%k40518 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41337, i64 1)
store %struct.ScmObj* %k40518, %struct.ScmObj** %stackaddr$env-ref49162
%stackaddr$env-ref49163 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41337, i64 2)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref49163
%stackaddr$env-ref49164 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41337, i64 3)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref49164
%stackaddr$env-ref49165 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41337, i64 4)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref49165
%stackaddr$env-ref49166 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41337, i64 5)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref49166
%stackaddr$env-ref49167 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41337, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref49167
%stackaddr$prim49168 = alloca %struct.ScmObj*, align 8
%_95k40521 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47417)
store volatile %struct.ScmObj* %_95k40521, %struct.ScmObj** %stackaddr$prim49168, align 8
%stackaddr$prim49169 = alloca %struct.ScmObj*, align 8
%current_45args47418 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47417)
store volatile %struct.ScmObj* %current_45args47418, %struct.ScmObj** %stackaddr$prim49169, align 8
%stackaddr$prim49170 = alloca %struct.ScmObj*, align 8
%anf_45bind40271 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47418)
store volatile %struct.ScmObj* %anf_45bind40271, %struct.ScmObj** %stackaddr$prim49170, align 8
%stackaddr$makeclosure49171 = alloca %struct.ScmObj*, align 8
%fptrToInt49172 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41358 to i64
%ae41358 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49172)
store volatile %struct.ScmObj* %ae41358, %struct.ScmObj** %stackaddr$makeclosure49171, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41358, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41358, %struct.ScmObj* %k40518, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41358, %struct.ScmObj* %f40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41358, %struct.ScmObj* %acc40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41358, %struct.ScmObj* %lsts40131, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41358, %struct.ScmObj* %_37foldr40129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41358, %struct.ScmObj* %_37foldr140123, i64 6)
%args47451$_37map140119$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49173 = alloca %struct.ScmObj*, align 8
%args47451$_37map140119$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40131, %struct.ScmObj* %args47451$_37map140119$0)
store volatile %struct.ScmObj* %args47451$_37map140119$1, %struct.ScmObj** %stackaddr$prim49173, align 8
%stackaddr$prim49174 = alloca %struct.ScmObj*, align 8
%args47451$_37map140119$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40271, %struct.ScmObj* %args47451$_37map140119$1)
store volatile %struct.ScmObj* %args47451$_37map140119$2, %struct.ScmObj** %stackaddr$prim49174, align 8
%stackaddr$prim49175 = alloca %struct.ScmObj*, align 8
%args47451$_37map140119$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41358, %struct.ScmObj* %args47451$_37map140119$2)
store volatile %struct.ScmObj* %args47451$_37map140119$3, %struct.ScmObj** %stackaddr$prim49175, align 8
%clofunc49176 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140119)
musttail call tailcc void %clofunc49176(%struct.ScmObj* %_37map140119, %struct.ScmObj* %args47451$_37map140119$3)
ret void
}

define tailcc void @proc_clo$ae41358(%struct.ScmObj* %env$ae41358,%struct.ScmObj* %current_45args47420) {
%stackaddr$env-ref49177 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41358, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref49177
%stackaddr$env-ref49178 = alloca %struct.ScmObj*, align 8
%k40518 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41358, i64 1)
store %struct.ScmObj* %k40518, %struct.ScmObj** %stackaddr$env-ref49178
%stackaddr$env-ref49179 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41358, i64 2)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref49179
%stackaddr$env-ref49180 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41358, i64 3)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref49180
%stackaddr$env-ref49181 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41358, i64 4)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref49181
%stackaddr$env-ref49182 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41358, i64 5)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref49182
%stackaddr$env-ref49183 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41358, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref49183
%stackaddr$prim49184 = alloca %struct.ScmObj*, align 8
%_95k40522 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47420)
store volatile %struct.ScmObj* %_95k40522, %struct.ScmObj** %stackaddr$prim49184, align 8
%stackaddr$prim49185 = alloca %struct.ScmObj*, align 8
%current_45args47421 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47420)
store volatile %struct.ScmObj* %current_45args47421, %struct.ScmObj** %stackaddr$prim49185, align 8
%stackaddr$prim49186 = alloca %struct.ScmObj*, align 8
%lsts_4340138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47421)
store volatile %struct.ScmObj* %lsts_4340138, %struct.ScmObj** %stackaddr$prim49186, align 8
%stackaddr$makeclosure49187 = alloca %struct.ScmObj*, align 8
%fptrToInt49188 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41361 to i64
%ae41361 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt49188)
store volatile %struct.ScmObj* %ae41361, %struct.ScmObj** %stackaddr$makeclosure49187, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41361, %struct.ScmObj* %_37map140119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41361, %struct.ScmObj* %k40518, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41361, %struct.ScmObj* %f40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41361, %struct.ScmObj* %acc40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41361, %struct.ScmObj* %lsts40131, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41361, %struct.ScmObj* %_37foldr40129, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41361, %struct.ScmObj* %_37foldr140123, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae41361, %struct.ScmObj* %lsts_4340138, i64 7)
%ae41362 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49189 = alloca %struct.ScmObj*, align 8
%fptrToInt49190 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41363 to i64
%ae41363 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49190)
store volatile %struct.ScmObj* %ae41363, %struct.ScmObj** %stackaddr$makeclosure49189, align 8
%args47450$ae41361$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49191 = alloca %struct.ScmObj*, align 8
%args47450$ae41361$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41363, %struct.ScmObj* %args47450$ae41361$0)
store volatile %struct.ScmObj* %args47450$ae41361$1, %struct.ScmObj** %stackaddr$prim49191, align 8
%stackaddr$prim49192 = alloca %struct.ScmObj*, align 8
%args47450$ae41361$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41362, %struct.ScmObj* %args47450$ae41361$1)
store volatile %struct.ScmObj* %args47450$ae41361$2, %struct.ScmObj** %stackaddr$prim49192, align 8
%clofunc49193 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41361)
musttail call tailcc void %clofunc49193(%struct.ScmObj* %ae41361, %struct.ScmObj* %args47450$ae41361$2)
ret void
}

define tailcc void @proc_clo$ae41361(%struct.ScmObj* %env$ae41361,%struct.ScmObj* %current_45args47423) {
%stackaddr$env-ref49194 = alloca %struct.ScmObj*, align 8
%_37map140119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41361, i64 0)
store %struct.ScmObj* %_37map140119, %struct.ScmObj** %stackaddr$env-ref49194
%stackaddr$env-ref49195 = alloca %struct.ScmObj*, align 8
%k40518 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41361, i64 1)
store %struct.ScmObj* %k40518, %struct.ScmObj** %stackaddr$env-ref49195
%stackaddr$env-ref49196 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41361, i64 2)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref49196
%stackaddr$env-ref49197 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41361, i64 3)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref49197
%stackaddr$env-ref49198 = alloca %struct.ScmObj*, align 8
%lsts40131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41361, i64 4)
store %struct.ScmObj* %lsts40131, %struct.ScmObj** %stackaddr$env-ref49198
%stackaddr$env-ref49199 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41361, i64 5)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref49199
%stackaddr$env-ref49200 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41361, i64 6)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref49200
%stackaddr$env-ref49201 = alloca %struct.ScmObj*, align 8
%lsts_4340138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41361, i64 7)
store %struct.ScmObj* %lsts_4340138, %struct.ScmObj** %stackaddr$env-ref49201
%stackaddr$prim49202 = alloca %struct.ScmObj*, align 8
%_95k40523 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47423)
store volatile %struct.ScmObj* %_95k40523, %struct.ScmObj** %stackaddr$prim49202, align 8
%stackaddr$prim49203 = alloca %struct.ScmObj*, align 8
%current_45args47424 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47423)
store volatile %struct.ScmObj* %current_45args47424, %struct.ScmObj** %stackaddr$prim49203, align 8
%stackaddr$prim49204 = alloca %struct.ScmObj*, align 8
%anf_45bind40272 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47424)
store volatile %struct.ScmObj* %anf_45bind40272, %struct.ScmObj** %stackaddr$prim49204, align 8
%stackaddr$makeclosure49205 = alloca %struct.ScmObj*, align 8
%fptrToInt49206 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41382 to i64
%ae41382 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt49206)
store volatile %struct.ScmObj* %ae41382, %struct.ScmObj** %stackaddr$makeclosure49205, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41382, %struct.ScmObj* %k40518, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41382, %struct.ScmObj* %f40133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41382, %struct.ScmObj* %acc40132, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41382, %struct.ScmObj* %_37foldr40129, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41382, %struct.ScmObj* %_37foldr140123, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41382, %struct.ScmObj* %lsts_4340138, i64 5)
%args47445$_37map140119$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49207 = alloca %struct.ScmObj*, align 8
%args47445$_37map140119$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts40131, %struct.ScmObj* %args47445$_37map140119$0)
store volatile %struct.ScmObj* %args47445$_37map140119$1, %struct.ScmObj** %stackaddr$prim49207, align 8
%stackaddr$prim49208 = alloca %struct.ScmObj*, align 8
%args47445$_37map140119$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40272, %struct.ScmObj* %args47445$_37map140119$1)
store volatile %struct.ScmObj* %args47445$_37map140119$2, %struct.ScmObj** %stackaddr$prim49208, align 8
%stackaddr$prim49209 = alloca %struct.ScmObj*, align 8
%args47445$_37map140119$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41382, %struct.ScmObj* %args47445$_37map140119$2)
store volatile %struct.ScmObj* %args47445$_37map140119$3, %struct.ScmObj** %stackaddr$prim49209, align 8
%clofunc49210 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map140119)
musttail call tailcc void %clofunc49210(%struct.ScmObj* %_37map140119, %struct.ScmObj* %args47445$_37map140119$3)
ret void
}

define tailcc void @proc_clo$ae41382(%struct.ScmObj* %env$ae41382,%struct.ScmObj* %current_45args47426) {
%stackaddr$env-ref49211 = alloca %struct.ScmObj*, align 8
%k40518 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41382, i64 0)
store %struct.ScmObj* %k40518, %struct.ScmObj** %stackaddr$env-ref49211
%stackaddr$env-ref49212 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41382, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref49212
%stackaddr$env-ref49213 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41382, i64 2)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref49213
%stackaddr$env-ref49214 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41382, i64 3)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref49214
%stackaddr$env-ref49215 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41382, i64 4)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref49215
%stackaddr$env-ref49216 = alloca %struct.ScmObj*, align 8
%lsts_4340138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41382, i64 5)
store %struct.ScmObj* %lsts_4340138, %struct.ScmObj** %stackaddr$env-ref49216
%stackaddr$prim49217 = alloca %struct.ScmObj*, align 8
%_95k40524 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47426)
store volatile %struct.ScmObj* %_95k40524, %struct.ScmObj** %stackaddr$prim49217, align 8
%stackaddr$prim49218 = alloca %struct.ScmObj*, align 8
%current_45args47427 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47426)
store volatile %struct.ScmObj* %current_45args47427, %struct.ScmObj** %stackaddr$prim49218, align 8
%stackaddr$prim49219 = alloca %struct.ScmObj*, align 8
%vs40136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47427)
store volatile %struct.ScmObj* %vs40136, %struct.ScmObj** %stackaddr$prim49219, align 8
%stackaddr$makeclosure49220 = alloca %struct.ScmObj*, align 8
%fptrToInt49221 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41385 to i64
%ae41385 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt49221)
store volatile %struct.ScmObj* %ae41385, %struct.ScmObj** %stackaddr$makeclosure49220, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41385, %struct.ScmObj* %vs40136, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41385, %struct.ScmObj* %k40518, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41385, %struct.ScmObj* %f40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41385, %struct.ScmObj* %acc40132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41385, %struct.ScmObj* %_37foldr40129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae41385, %struct.ScmObj* %_37foldr140123, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae41385, %struct.ScmObj* %lsts_4340138, i64 6)
%ae41386 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49222 = alloca %struct.ScmObj*, align 8
%fptrToInt49223 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41387 to i64
%ae41387 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49223)
store volatile %struct.ScmObj* %ae41387, %struct.ScmObj** %stackaddr$makeclosure49222, align 8
%args47444$ae41385$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49224 = alloca %struct.ScmObj*, align 8
%args47444$ae41385$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41387, %struct.ScmObj* %args47444$ae41385$0)
store volatile %struct.ScmObj* %args47444$ae41385$1, %struct.ScmObj** %stackaddr$prim49224, align 8
%stackaddr$prim49225 = alloca %struct.ScmObj*, align 8
%args47444$ae41385$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41386, %struct.ScmObj* %args47444$ae41385$1)
store volatile %struct.ScmObj* %args47444$ae41385$2, %struct.ScmObj** %stackaddr$prim49225, align 8
%clofunc49226 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41385)
musttail call tailcc void %clofunc49226(%struct.ScmObj* %ae41385, %struct.ScmObj* %args47444$ae41385$2)
ret void
}

define tailcc void @proc_clo$ae41385(%struct.ScmObj* %env$ae41385,%struct.ScmObj* %current_45args47429) {
%stackaddr$env-ref49227 = alloca %struct.ScmObj*, align 8
%vs40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41385, i64 0)
store %struct.ScmObj* %vs40136, %struct.ScmObj** %stackaddr$env-ref49227
%stackaddr$env-ref49228 = alloca %struct.ScmObj*, align 8
%k40518 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41385, i64 1)
store %struct.ScmObj* %k40518, %struct.ScmObj** %stackaddr$env-ref49228
%stackaddr$env-ref49229 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41385, i64 2)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref49229
%stackaddr$env-ref49230 = alloca %struct.ScmObj*, align 8
%acc40132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41385, i64 3)
store %struct.ScmObj* %acc40132, %struct.ScmObj** %stackaddr$env-ref49230
%stackaddr$env-ref49231 = alloca %struct.ScmObj*, align 8
%_37foldr40129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41385, i64 4)
store %struct.ScmObj* %_37foldr40129, %struct.ScmObj** %stackaddr$env-ref49231
%stackaddr$env-ref49232 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41385, i64 5)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref49232
%stackaddr$env-ref49233 = alloca %struct.ScmObj*, align 8
%lsts_4340138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41385, i64 6)
store %struct.ScmObj* %lsts_4340138, %struct.ScmObj** %stackaddr$env-ref49233
%stackaddr$prim49234 = alloca %struct.ScmObj*, align 8
%_95k40525 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47429)
store volatile %struct.ScmObj* %_95k40525, %struct.ScmObj** %stackaddr$prim49234, align 8
%stackaddr$prim49235 = alloca %struct.ScmObj*, align 8
%current_45args47430 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47429)
store volatile %struct.ScmObj* %current_45args47430, %struct.ScmObj** %stackaddr$prim49235, align 8
%stackaddr$prim49236 = alloca %struct.ScmObj*, align 8
%anf_45bind40273 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47430)
store volatile %struct.ScmObj* %anf_45bind40273, %struct.ScmObj** %stackaddr$prim49236, align 8
%stackaddr$prim49237 = alloca %struct.ScmObj*, align 8
%anf_45bind40274 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40132, %struct.ScmObj* %lsts_4340138)
store volatile %struct.ScmObj* %anf_45bind40274, %struct.ScmObj** %stackaddr$prim49237, align 8
%stackaddr$prim49238 = alloca %struct.ScmObj*, align 8
%anf_45bind40275 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40133, %struct.ScmObj* %anf_45bind40274)
store volatile %struct.ScmObj* %anf_45bind40275, %struct.ScmObj** %stackaddr$prim49238, align 8
%stackaddr$makeclosure49239 = alloca %struct.ScmObj*, align 8
%fptrToInt49240 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41411 to i64
%ae41411 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt49240)
store volatile %struct.ScmObj* %ae41411, %struct.ScmObj** %stackaddr$makeclosure49239, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41411, %struct.ScmObj* %vs40136, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41411, %struct.ScmObj* %k40518, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41411, %struct.ScmObj* %f40133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41411, %struct.ScmObj* %anf_45bind40273, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae41411, %struct.ScmObj* %_37foldr140123, i64 4)
%stackaddr$prim49241 = alloca %struct.ScmObj*, align 8
%cpsargs40529 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41411, %struct.ScmObj* %anf_45bind40275)
store volatile %struct.ScmObj* %cpsargs40529, %struct.ScmObj** %stackaddr$prim49241, align 8
%clofunc49242 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr40129)
musttail call tailcc void %clofunc49242(%struct.ScmObj* %_37foldr40129, %struct.ScmObj* %cpsargs40529)
ret void
}

define tailcc void @proc_clo$ae41411(%struct.ScmObj* %env$ae41411,%struct.ScmObj* %current_45args47432) {
%stackaddr$env-ref49243 = alloca %struct.ScmObj*, align 8
%vs40136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41411, i64 0)
store %struct.ScmObj* %vs40136, %struct.ScmObj** %stackaddr$env-ref49243
%stackaddr$env-ref49244 = alloca %struct.ScmObj*, align 8
%k40518 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41411, i64 1)
store %struct.ScmObj* %k40518, %struct.ScmObj** %stackaddr$env-ref49244
%stackaddr$env-ref49245 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41411, i64 2)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref49245
%stackaddr$env-ref49246 = alloca %struct.ScmObj*, align 8
%anf_45bind40273 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41411, i64 3)
store %struct.ScmObj* %anf_45bind40273, %struct.ScmObj** %stackaddr$env-ref49246
%stackaddr$env-ref49247 = alloca %struct.ScmObj*, align 8
%_37foldr140123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41411, i64 4)
store %struct.ScmObj* %_37foldr140123, %struct.ScmObj** %stackaddr$env-ref49247
%stackaddr$prim49248 = alloca %struct.ScmObj*, align 8
%_95k40526 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47432)
store volatile %struct.ScmObj* %_95k40526, %struct.ScmObj** %stackaddr$prim49248, align 8
%stackaddr$prim49249 = alloca %struct.ScmObj*, align 8
%current_45args47433 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47432)
store volatile %struct.ScmObj* %current_45args47433, %struct.ScmObj** %stackaddr$prim49249, align 8
%stackaddr$prim49250 = alloca %struct.ScmObj*, align 8
%anf_45bind40276 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47433)
store volatile %struct.ScmObj* %anf_45bind40276, %struct.ScmObj** %stackaddr$prim49250, align 8
%ae41416 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49251 = alloca %struct.ScmObj*, align 8
%anf_45bind40277 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40276, %struct.ScmObj* %ae41416)
store volatile %struct.ScmObj* %anf_45bind40277, %struct.ScmObj** %stackaddr$prim49251, align 8
%stackaddr$makeclosure49252 = alloca %struct.ScmObj*, align 8
%fptrToInt49253 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41418 to i64
%ae41418 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49253)
store volatile %struct.ScmObj* %ae41418, %struct.ScmObj** %stackaddr$makeclosure49252, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41418, %struct.ScmObj* %k40518, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41418, %struct.ScmObj* %f40133, i64 1)
%args47438$_37foldr140123$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49254 = alloca %struct.ScmObj*, align 8
%args47438$_37foldr140123$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs40136, %struct.ScmObj* %args47438$_37foldr140123$0)
store volatile %struct.ScmObj* %args47438$_37foldr140123$1, %struct.ScmObj** %stackaddr$prim49254, align 8
%stackaddr$prim49255 = alloca %struct.ScmObj*, align 8
%args47438$_37foldr140123$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40277, %struct.ScmObj* %args47438$_37foldr140123$1)
store volatile %struct.ScmObj* %args47438$_37foldr140123$2, %struct.ScmObj** %stackaddr$prim49255, align 8
%stackaddr$prim49256 = alloca %struct.ScmObj*, align 8
%args47438$_37foldr140123$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40273, %struct.ScmObj* %args47438$_37foldr140123$2)
store volatile %struct.ScmObj* %args47438$_37foldr140123$3, %struct.ScmObj** %stackaddr$prim49256, align 8
%stackaddr$prim49257 = alloca %struct.ScmObj*, align 8
%args47438$_37foldr140123$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41418, %struct.ScmObj* %args47438$_37foldr140123$3)
store volatile %struct.ScmObj* %args47438$_37foldr140123$4, %struct.ScmObj** %stackaddr$prim49257, align 8
%clofunc49258 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140123)
musttail call tailcc void %clofunc49258(%struct.ScmObj* %_37foldr140123, %struct.ScmObj* %args47438$_37foldr140123$4)
ret void
}

define tailcc void @proc_clo$ae41418(%struct.ScmObj* %env$ae41418,%struct.ScmObj* %current_45args47435) {
%stackaddr$env-ref49259 = alloca %struct.ScmObj*, align 8
%k40518 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41418, i64 0)
store %struct.ScmObj* %k40518, %struct.ScmObj** %stackaddr$env-ref49259
%stackaddr$env-ref49260 = alloca %struct.ScmObj*, align 8
%f40133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41418, i64 1)
store %struct.ScmObj* %f40133, %struct.ScmObj** %stackaddr$env-ref49260
%stackaddr$prim49261 = alloca %struct.ScmObj*, align 8
%_95k40527 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47435)
store volatile %struct.ScmObj* %_95k40527, %struct.ScmObj** %stackaddr$prim49261, align 8
%stackaddr$prim49262 = alloca %struct.ScmObj*, align 8
%current_45args47436 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47435)
store volatile %struct.ScmObj* %current_45args47436, %struct.ScmObj** %stackaddr$prim49262, align 8
%stackaddr$prim49263 = alloca %struct.ScmObj*, align 8
%anf_45bind40278 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47436)
store volatile %struct.ScmObj* %anf_45bind40278, %struct.ScmObj** %stackaddr$prim49263, align 8
%stackaddr$prim49264 = alloca %struct.ScmObj*, align 8
%cpsargs40528 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40518, %struct.ScmObj* %anf_45bind40278)
store volatile %struct.ScmObj* %cpsargs40528, %struct.ScmObj** %stackaddr$prim49264, align 8
%clofunc49265 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40133)
musttail call tailcc void %clofunc49265(%struct.ScmObj* %f40133, %struct.ScmObj* %cpsargs40528)
ret void
}

define tailcc void @proc_clo$ae41387(%struct.ScmObj* %env$ae41387,%struct.ScmObj* %current_45args47439) {
%stackaddr$prim49266 = alloca %struct.ScmObj*, align 8
%k40530 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47439)
store volatile %struct.ScmObj* %k40530, %struct.ScmObj** %stackaddr$prim49266, align 8
%stackaddr$prim49267 = alloca %struct.ScmObj*, align 8
%current_45args47440 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47439)
store volatile %struct.ScmObj* %current_45args47440, %struct.ScmObj** %stackaddr$prim49267, align 8
%stackaddr$prim49268 = alloca %struct.ScmObj*, align 8
%a40141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47440)
store volatile %struct.ScmObj* %a40141, %struct.ScmObj** %stackaddr$prim49268, align 8
%stackaddr$prim49269 = alloca %struct.ScmObj*, align 8
%current_45args47441 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47440)
store volatile %struct.ScmObj* %current_45args47441, %struct.ScmObj** %stackaddr$prim49269, align 8
%stackaddr$prim49270 = alloca %struct.ScmObj*, align 8
%b40140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47441)
store volatile %struct.ScmObj* %b40140, %struct.ScmObj** %stackaddr$prim49270, align 8
%stackaddr$prim49271 = alloca %struct.ScmObj*, align 8
%cpsprim40531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a40141, %struct.ScmObj* %b40140)
store volatile %struct.ScmObj* %cpsprim40531, %struct.ScmObj** %stackaddr$prim49271, align 8
%ae41391 = call %struct.ScmObj* @const_init_int(i64 0)
%args47443$k40530$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49272 = alloca %struct.ScmObj*, align 8
%args47443$k40530$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40531, %struct.ScmObj* %args47443$k40530$0)
store volatile %struct.ScmObj* %args47443$k40530$1, %struct.ScmObj** %stackaddr$prim49272, align 8
%stackaddr$prim49273 = alloca %struct.ScmObj*, align 8
%args47443$k40530$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41391, %struct.ScmObj* %args47443$k40530$1)
store volatile %struct.ScmObj* %args47443$k40530$2, %struct.ScmObj** %stackaddr$prim49273, align 8
%clofunc49274 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40530)
musttail call tailcc void %clofunc49274(%struct.ScmObj* %k40530, %struct.ScmObj* %args47443$k40530$2)
ret void
}

define tailcc void @proc_clo$ae41363(%struct.ScmObj* %env$ae41363,%struct.ScmObj* %current_45args47446) {
%stackaddr$prim49275 = alloca %struct.ScmObj*, align 8
%k40532 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47446)
store volatile %struct.ScmObj* %k40532, %struct.ScmObj** %stackaddr$prim49275, align 8
%stackaddr$prim49276 = alloca %struct.ScmObj*, align 8
%current_45args47447 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47446)
store volatile %struct.ScmObj* %current_45args47447, %struct.ScmObj** %stackaddr$prim49276, align 8
%stackaddr$prim49277 = alloca %struct.ScmObj*, align 8
%x40137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47447)
store volatile %struct.ScmObj* %x40137, %struct.ScmObj** %stackaddr$prim49277, align 8
%stackaddr$prim49278 = alloca %struct.ScmObj*, align 8
%cpsprim40533 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x40137)
store volatile %struct.ScmObj* %cpsprim40533, %struct.ScmObj** %stackaddr$prim49278, align 8
%ae41366 = call %struct.ScmObj* @const_init_int(i64 0)
%args47449$k40532$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49279 = alloca %struct.ScmObj*, align 8
%args47449$k40532$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40533, %struct.ScmObj* %args47449$k40532$0)
store volatile %struct.ScmObj* %args47449$k40532$1, %struct.ScmObj** %stackaddr$prim49279, align 8
%stackaddr$prim49280 = alloca %struct.ScmObj*, align 8
%args47449$k40532$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41366, %struct.ScmObj* %args47449$k40532$1)
store volatile %struct.ScmObj* %args47449$k40532$2, %struct.ScmObj** %stackaddr$prim49280, align 8
%clofunc49281 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40532)
musttail call tailcc void %clofunc49281(%struct.ScmObj* %k40532, %struct.ScmObj* %args47449$k40532$2)
ret void
}

define tailcc void @proc_clo$ae41339(%struct.ScmObj* %env$ae41339,%struct.ScmObj* %current_45args47452) {
%stackaddr$prim49282 = alloca %struct.ScmObj*, align 8
%k40534 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47452)
store volatile %struct.ScmObj* %k40534, %struct.ScmObj** %stackaddr$prim49282, align 8
%stackaddr$prim49283 = alloca %struct.ScmObj*, align 8
%current_45args47453 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47452)
store volatile %struct.ScmObj* %current_45args47453, %struct.ScmObj** %stackaddr$prim49283, align 8
%stackaddr$prim49284 = alloca %struct.ScmObj*, align 8
%x40139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47453)
store volatile %struct.ScmObj* %x40139, %struct.ScmObj** %stackaddr$prim49284, align 8
%stackaddr$prim49285 = alloca %struct.ScmObj*, align 8
%cpsprim40535 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x40139)
store volatile %struct.ScmObj* %cpsprim40535, %struct.ScmObj** %stackaddr$prim49285, align 8
%ae41342 = call %struct.ScmObj* @const_init_int(i64 0)
%args47455$k40534$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49286 = alloca %struct.ScmObj*, align 8
%args47455$k40534$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40535, %struct.ScmObj* %args47455$k40534$0)
store volatile %struct.ScmObj* %args47455$k40534$1, %struct.ScmObj** %stackaddr$prim49286, align 8
%stackaddr$prim49287 = alloca %struct.ScmObj*, align 8
%args47455$k40534$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41342, %struct.ScmObj* %args47455$k40534$1)
store volatile %struct.ScmObj* %args47455$k40534$2, %struct.ScmObj** %stackaddr$prim49287, align 8
%clofunc49288 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40534)
musttail call tailcc void %clofunc49288(%struct.ScmObj* %k40534, %struct.ScmObj* %args47455$k40534$2)
ret void
}

define tailcc void @proc_clo$ae41291(%struct.ScmObj* %env$ae41291,%struct.ScmObj* %current_45args47458) {
%stackaddr$prim49289 = alloca %struct.ScmObj*, align 8
%k40536 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47458)
store volatile %struct.ScmObj* %k40536, %struct.ScmObj** %stackaddr$prim49289, align 8
%stackaddr$prim49290 = alloca %struct.ScmObj*, align 8
%current_45args47459 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47458)
store volatile %struct.ScmObj* %current_45args47459, %struct.ScmObj** %stackaddr$prim49290, align 8
%stackaddr$prim49291 = alloca %struct.ScmObj*, align 8
%lst40135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47459)
store volatile %struct.ScmObj* %lst40135, %struct.ScmObj** %stackaddr$prim49291, align 8
%stackaddr$prim49292 = alloca %struct.ScmObj*, align 8
%current_45args47460 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47459)
store volatile %struct.ScmObj* %current_45args47460, %struct.ScmObj** %stackaddr$prim49292, align 8
%stackaddr$prim49293 = alloca %struct.ScmObj*, align 8
%b40134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47460)
store volatile %struct.ScmObj* %b40134, %struct.ScmObj** %stackaddr$prim49293, align 8
%truthy$cmp49294 = call i64 @is_truthy_value(%struct.ScmObj* %b40134)
%cmp$cmp49294 = icmp eq i64 %truthy$cmp49294, 1
br i1 %cmp$cmp49294, label %truebranch$cmp49294, label %falsebranch$cmp49294
truebranch$cmp49294:
%ae41294 = call %struct.ScmObj* @const_init_int(i64 0)
%args47462$k40536$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49295 = alloca %struct.ScmObj*, align 8
%args47462$k40536$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b40134, %struct.ScmObj* %args47462$k40536$0)
store volatile %struct.ScmObj* %args47462$k40536$1, %struct.ScmObj** %stackaddr$prim49295, align 8
%stackaddr$prim49296 = alloca %struct.ScmObj*, align 8
%args47462$k40536$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41294, %struct.ScmObj* %args47462$k40536$1)
store volatile %struct.ScmObj* %args47462$k40536$2, %struct.ScmObj** %stackaddr$prim49296, align 8
%clofunc49297 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40536)
musttail call tailcc void %clofunc49297(%struct.ScmObj* %k40536, %struct.ScmObj* %args47462$k40536$2)
ret void
falsebranch$cmp49294:
%stackaddr$prim49298 = alloca %struct.ScmObj*, align 8
%cpsprim40537 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40135)
store volatile %struct.ScmObj* %cpsprim40537, %struct.ScmObj** %stackaddr$prim49298, align 8
%ae41301 = call %struct.ScmObj* @const_init_int(i64 0)
%args47463$k40536$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49299 = alloca %struct.ScmObj*, align 8
%args47463$k40536$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40537, %struct.ScmObj* %args47463$k40536$0)
store volatile %struct.ScmObj* %args47463$k40536$1, %struct.ScmObj** %stackaddr$prim49299, align 8
%stackaddr$prim49300 = alloca %struct.ScmObj*, align 8
%args47463$k40536$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41301, %struct.ScmObj* %args47463$k40536$1)
store volatile %struct.ScmObj* %args47463$k40536$2, %struct.ScmObj** %stackaddr$prim49300, align 8
%clofunc49301 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40536)
musttail call tailcc void %clofunc49301(%struct.ScmObj* %k40536, %struct.ScmObj* %args47463$k40536$2)
ret void
}

define tailcc void @proc_clo$ae41248(%struct.ScmObj* %env$ae41248,%struct.ScmObj* %current_45args47467) {
%stackaddr$env-ref49302 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41248, i64 0)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref49302
%stackaddr$env-ref49303 = alloca %struct.ScmObj*, align 8
%_37length40112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41248, i64 1)
store %struct.ScmObj* %_37length40112, %struct.ScmObj** %stackaddr$env-ref49303
%stackaddr$prim49304 = alloca %struct.ScmObj*, align 8
%k40538 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47467)
store volatile %struct.ScmObj* %k40538, %struct.ScmObj** %stackaddr$prim49304, align 8
%stackaddr$prim49305 = alloca %struct.ScmObj*, align 8
%current_45args47468 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47467)
store volatile %struct.ScmObj* %current_45args47468, %struct.ScmObj** %stackaddr$prim49305, align 8
%stackaddr$prim49306 = alloca %struct.ScmObj*, align 8
%lst40144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47468)
store volatile %struct.ScmObj* %lst40144, %struct.ScmObj** %stackaddr$prim49306, align 8
%stackaddr$prim49307 = alloca %struct.ScmObj*, align 8
%current_45args47469 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47468)
store volatile %struct.ScmObj* %current_45args47469, %struct.ScmObj** %stackaddr$prim49307, align 8
%stackaddr$prim49308 = alloca %struct.ScmObj*, align 8
%n40143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47469)
store volatile %struct.ScmObj* %n40143, %struct.ScmObj** %stackaddr$prim49308, align 8
%stackaddr$makeclosure49309 = alloca %struct.ScmObj*, align 8
%fptrToInt49310 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41250 to i64
%ae41250 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49310)
store volatile %struct.ScmObj* %ae41250, %struct.ScmObj** %stackaddr$makeclosure49309, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41250, %struct.ScmObj* %_37take40115, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41250, %struct.ScmObj* %lst40144, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41250, %struct.ScmObj* %n40143, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41250, %struct.ScmObj* %k40538, i64 3)
%args47475$_37length40112$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49311 = alloca %struct.ScmObj*, align 8
%args47475$_37length40112$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40144, %struct.ScmObj* %args47475$_37length40112$0)
store volatile %struct.ScmObj* %args47475$_37length40112$1, %struct.ScmObj** %stackaddr$prim49311, align 8
%stackaddr$prim49312 = alloca %struct.ScmObj*, align 8
%args47475$_37length40112$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41250, %struct.ScmObj* %args47475$_37length40112$1)
store volatile %struct.ScmObj* %args47475$_37length40112$2, %struct.ScmObj** %stackaddr$prim49312, align 8
%clofunc49313 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40112)
musttail call tailcc void %clofunc49313(%struct.ScmObj* %_37length40112, %struct.ScmObj* %args47475$_37length40112$2)
ret void
}

define tailcc void @proc_clo$ae41250(%struct.ScmObj* %env$ae41250,%struct.ScmObj* %current_45args47471) {
%stackaddr$env-ref49314 = alloca %struct.ScmObj*, align 8
%_37take40115 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41250, i64 0)
store %struct.ScmObj* %_37take40115, %struct.ScmObj** %stackaddr$env-ref49314
%stackaddr$env-ref49315 = alloca %struct.ScmObj*, align 8
%lst40144 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41250, i64 1)
store %struct.ScmObj* %lst40144, %struct.ScmObj** %stackaddr$env-ref49315
%stackaddr$env-ref49316 = alloca %struct.ScmObj*, align 8
%n40143 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41250, i64 2)
store %struct.ScmObj* %n40143, %struct.ScmObj** %stackaddr$env-ref49316
%stackaddr$env-ref49317 = alloca %struct.ScmObj*, align 8
%k40538 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41250, i64 3)
store %struct.ScmObj* %k40538, %struct.ScmObj** %stackaddr$env-ref49317
%stackaddr$prim49318 = alloca %struct.ScmObj*, align 8
%_95k40539 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47471)
store volatile %struct.ScmObj* %_95k40539, %struct.ScmObj** %stackaddr$prim49318, align 8
%stackaddr$prim49319 = alloca %struct.ScmObj*, align 8
%current_45args47472 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47471)
store volatile %struct.ScmObj* %current_45args47472, %struct.ScmObj** %stackaddr$prim49319, align 8
%stackaddr$prim49320 = alloca %struct.ScmObj*, align 8
%anf_45bind40265 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47472)
store volatile %struct.ScmObj* %anf_45bind40265, %struct.ScmObj** %stackaddr$prim49320, align 8
%stackaddr$prim49321 = alloca %struct.ScmObj*, align 8
%anf_45bind40266 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind40265, %struct.ScmObj* %n40143)
store volatile %struct.ScmObj* %anf_45bind40266, %struct.ScmObj** %stackaddr$prim49321, align 8
%args47474$_37take40115$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49322 = alloca %struct.ScmObj*, align 8
%args47474$_37take40115$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40266, %struct.ScmObj* %args47474$_37take40115$0)
store volatile %struct.ScmObj* %args47474$_37take40115$1, %struct.ScmObj** %stackaddr$prim49322, align 8
%stackaddr$prim49323 = alloca %struct.ScmObj*, align 8
%args47474$_37take40115$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40144, %struct.ScmObj* %args47474$_37take40115$1)
store volatile %struct.ScmObj* %args47474$_37take40115$2, %struct.ScmObj** %stackaddr$prim49323, align 8
%stackaddr$prim49324 = alloca %struct.ScmObj*, align 8
%args47474$_37take40115$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40538, %struct.ScmObj* %args47474$_37take40115$2)
store volatile %struct.ScmObj* %args47474$_37take40115$3, %struct.ScmObj** %stackaddr$prim49324, align 8
%clofunc49325 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40115)
musttail call tailcc void %clofunc49325(%struct.ScmObj* %_37take40115, %struct.ScmObj* %args47474$_37take40115$3)
ret void
}

define tailcc void @proc_clo$ae41194(%struct.ScmObj* %env$ae41194,%struct.ScmObj* %current_45args47477) {
%stackaddr$env-ref49326 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41194, i64 0)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref49326
%stackaddr$prim49327 = alloca %struct.ScmObj*, align 8
%k40540 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47477)
store volatile %struct.ScmObj* %k40540, %struct.ScmObj** %stackaddr$prim49327, align 8
%stackaddr$prim49328 = alloca %struct.ScmObj*, align 8
%current_45args47478 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47477)
store volatile %struct.ScmObj* %current_45args47478, %struct.ScmObj** %stackaddr$prim49328, align 8
%stackaddr$prim49329 = alloca %struct.ScmObj*, align 8
%lst40146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47478)
store volatile %struct.ScmObj* %lst40146, %struct.ScmObj** %stackaddr$prim49329, align 8
%stackaddr$makeclosure49330 = alloca %struct.ScmObj*, align 8
%fptrToInt49331 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41195 to i64
%ae41195 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49331)
store volatile %struct.ScmObj* %ae41195, %struct.ScmObj** %stackaddr$makeclosure49330, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41195, %struct.ScmObj* %lst40146, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41195, %struct.ScmObj* %k40540, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41195, %struct.ScmObj* %_37foldl140107, i64 2)
%ae41196 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49332 = alloca %struct.ScmObj*, align 8
%fptrToInt49333 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41197 to i64
%ae41197 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt49333)
store volatile %struct.ScmObj* %ae41197, %struct.ScmObj** %stackaddr$makeclosure49332, align 8
%args47489$ae41195$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49334 = alloca %struct.ScmObj*, align 8
%args47489$ae41195$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41197, %struct.ScmObj* %args47489$ae41195$0)
store volatile %struct.ScmObj* %args47489$ae41195$1, %struct.ScmObj** %stackaddr$prim49334, align 8
%stackaddr$prim49335 = alloca %struct.ScmObj*, align 8
%args47489$ae41195$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41196, %struct.ScmObj* %args47489$ae41195$1)
store volatile %struct.ScmObj* %args47489$ae41195$2, %struct.ScmObj** %stackaddr$prim49335, align 8
%clofunc49336 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae41195)
musttail call tailcc void %clofunc49336(%struct.ScmObj* %ae41195, %struct.ScmObj* %args47489$ae41195$2)
ret void
}

define tailcc void @proc_clo$ae41195(%struct.ScmObj* %env$ae41195,%struct.ScmObj* %current_45args47480) {
%stackaddr$env-ref49337 = alloca %struct.ScmObj*, align 8
%lst40146 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41195, i64 0)
store %struct.ScmObj* %lst40146, %struct.ScmObj** %stackaddr$env-ref49337
%stackaddr$env-ref49338 = alloca %struct.ScmObj*, align 8
%k40540 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41195, i64 1)
store %struct.ScmObj* %k40540, %struct.ScmObj** %stackaddr$env-ref49338
%stackaddr$env-ref49339 = alloca %struct.ScmObj*, align 8
%_37foldl140107 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41195, i64 2)
store %struct.ScmObj* %_37foldl140107, %struct.ScmObj** %stackaddr$env-ref49339
%stackaddr$prim49340 = alloca %struct.ScmObj*, align 8
%_95k40541 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47480)
store volatile %struct.ScmObj* %_95k40541, %struct.ScmObj** %stackaddr$prim49340, align 8
%stackaddr$prim49341 = alloca %struct.ScmObj*, align 8
%current_45args47481 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47480)
store volatile %struct.ScmObj* %current_45args47481, %struct.ScmObj** %stackaddr$prim49341, align 8
%stackaddr$prim49342 = alloca %struct.ScmObj*, align 8
%anf_45bind40264 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47481)
store volatile %struct.ScmObj* %anf_45bind40264, %struct.ScmObj** %stackaddr$prim49342, align 8
%ae41216 = call %struct.ScmObj* @const_init_null()
%args47483$_37foldl140107$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49343 = alloca %struct.ScmObj*, align 8
%args47483$_37foldl140107$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst40146, %struct.ScmObj* %args47483$_37foldl140107$0)
store volatile %struct.ScmObj* %args47483$_37foldl140107$1, %struct.ScmObj** %stackaddr$prim49343, align 8
%stackaddr$prim49344 = alloca %struct.ScmObj*, align 8
%args47483$_37foldl140107$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41216, %struct.ScmObj* %args47483$_37foldl140107$1)
store volatile %struct.ScmObj* %args47483$_37foldl140107$2, %struct.ScmObj** %stackaddr$prim49344, align 8
%stackaddr$prim49345 = alloca %struct.ScmObj*, align 8
%args47483$_37foldl140107$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40264, %struct.ScmObj* %args47483$_37foldl140107$2)
store volatile %struct.ScmObj* %args47483$_37foldl140107$3, %struct.ScmObj** %stackaddr$prim49345, align 8
%stackaddr$prim49346 = alloca %struct.ScmObj*, align 8
%args47483$_37foldl140107$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40540, %struct.ScmObj* %args47483$_37foldl140107$3)
store volatile %struct.ScmObj* %args47483$_37foldl140107$4, %struct.ScmObj** %stackaddr$prim49346, align 8
%clofunc49347 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140107)
musttail call tailcc void %clofunc49347(%struct.ScmObj* %_37foldl140107, %struct.ScmObj* %args47483$_37foldl140107$4)
ret void
}

define tailcc void @proc_clo$ae41197(%struct.ScmObj* %env$ae41197,%struct.ScmObj* %current_45args47484) {
%stackaddr$prim49348 = alloca %struct.ScmObj*, align 8
%k40542 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47484)
store volatile %struct.ScmObj* %k40542, %struct.ScmObj** %stackaddr$prim49348, align 8
%stackaddr$prim49349 = alloca %struct.ScmObj*, align 8
%current_45args47485 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47484)
store volatile %struct.ScmObj* %current_45args47485, %struct.ScmObj** %stackaddr$prim49349, align 8
%stackaddr$prim49350 = alloca %struct.ScmObj*, align 8
%x40148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47485)
store volatile %struct.ScmObj* %x40148, %struct.ScmObj** %stackaddr$prim49350, align 8
%stackaddr$prim49351 = alloca %struct.ScmObj*, align 8
%current_45args47486 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47485)
store volatile %struct.ScmObj* %current_45args47486, %struct.ScmObj** %stackaddr$prim49351, align 8
%stackaddr$prim49352 = alloca %struct.ScmObj*, align 8
%y40147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47486)
store volatile %struct.ScmObj* %y40147, %struct.ScmObj** %stackaddr$prim49352, align 8
%ae41199 = call %struct.ScmObj* @const_init_int(i64 0)
%args47488$k40542$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49353 = alloca %struct.ScmObj*, align 8
%args47488$k40542$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x40148, %struct.ScmObj* %args47488$k40542$0)
store volatile %struct.ScmObj* %args47488$k40542$1, %struct.ScmObj** %stackaddr$prim49353, align 8
%stackaddr$prim49354 = alloca %struct.ScmObj*, align 8
%args47488$k40542$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41199, %struct.ScmObj* %args47488$k40542$1)
store volatile %struct.ScmObj* %args47488$k40542$2, %struct.ScmObj** %stackaddr$prim49354, align 8
%clofunc49355 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40542)
musttail call tailcc void %clofunc49355(%struct.ScmObj* %k40542, %struct.ScmObj* %args47488$k40542$2)
ret void
}

define tailcc void @proc_clo$ae41115(%struct.ScmObj* %env$ae41115,%struct.ScmObj* %current_45args47492) {
%stackaddr$prim49356 = alloca %struct.ScmObj*, align 8
%k40543 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47492)
store volatile %struct.ScmObj* %k40543, %struct.ScmObj** %stackaddr$prim49356, align 8
%stackaddr$prim49357 = alloca %struct.ScmObj*, align 8
%current_45args47493 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47492)
store volatile %struct.ScmObj* %current_45args47493, %struct.ScmObj** %stackaddr$prim49357, align 8
%stackaddr$prim49358 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47493)
store volatile %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$prim49358, align 8
%ae41117 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49359 = alloca %struct.ScmObj*, align 8
%fptrToInt49360 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41118 to i64
%ae41118 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49360)
store volatile %struct.ScmObj* %ae41118, %struct.ScmObj** %stackaddr$makeclosure49359, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41118, %struct.ScmObj* %_37foldl140108, i64 0)
%args47506$k40543$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49361 = alloca %struct.ScmObj*, align 8
%args47506$k40543$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41118, %struct.ScmObj* %args47506$k40543$0)
store volatile %struct.ScmObj* %args47506$k40543$1, %struct.ScmObj** %stackaddr$prim49361, align 8
%stackaddr$prim49362 = alloca %struct.ScmObj*, align 8
%args47506$k40543$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41117, %struct.ScmObj* %args47506$k40543$1)
store volatile %struct.ScmObj* %args47506$k40543$2, %struct.ScmObj** %stackaddr$prim49362, align 8
%clofunc49363 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40543)
musttail call tailcc void %clofunc49363(%struct.ScmObj* %k40543, %struct.ScmObj* %args47506$k40543$2)
ret void
}

define tailcc void @proc_clo$ae41118(%struct.ScmObj* %env$ae41118,%struct.ScmObj* %current_45args47495) {
%stackaddr$env-ref49364 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41118, i64 0)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref49364
%stackaddr$prim49365 = alloca %struct.ScmObj*, align 8
%k40544 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47495)
store volatile %struct.ScmObj* %k40544, %struct.ScmObj** %stackaddr$prim49365, align 8
%stackaddr$prim49366 = alloca %struct.ScmObj*, align 8
%current_45args47496 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47495)
store volatile %struct.ScmObj* %current_45args47496, %struct.ScmObj** %stackaddr$prim49366, align 8
%stackaddr$prim49367 = alloca %struct.ScmObj*, align 8
%f40111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47496)
store volatile %struct.ScmObj* %f40111, %struct.ScmObj** %stackaddr$prim49367, align 8
%stackaddr$prim49368 = alloca %struct.ScmObj*, align 8
%current_45args47497 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47496)
store volatile %struct.ScmObj* %current_45args47497, %struct.ScmObj** %stackaddr$prim49368, align 8
%stackaddr$prim49369 = alloca %struct.ScmObj*, align 8
%acc40110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47497)
store volatile %struct.ScmObj* %acc40110, %struct.ScmObj** %stackaddr$prim49369, align 8
%stackaddr$prim49370 = alloca %struct.ScmObj*, align 8
%current_45args47498 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47497)
store volatile %struct.ScmObj* %current_45args47498, %struct.ScmObj** %stackaddr$prim49370, align 8
%stackaddr$prim49371 = alloca %struct.ScmObj*, align 8
%lst40109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47498)
store volatile %struct.ScmObj* %lst40109, %struct.ScmObj** %stackaddr$prim49371, align 8
%stackaddr$prim49372 = alloca %struct.ScmObj*, align 8
%anf_45bind40259 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40109)
store volatile %struct.ScmObj* %anf_45bind40259, %struct.ScmObj** %stackaddr$prim49372, align 8
%truthy$cmp49373 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40259)
%cmp$cmp49373 = icmp eq i64 %truthy$cmp49373, 1
br i1 %cmp$cmp49373, label %truebranch$cmp49373, label %falsebranch$cmp49373
truebranch$cmp49373:
%ae41122 = call %struct.ScmObj* @const_init_int(i64 0)
%args47500$k40544$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49374 = alloca %struct.ScmObj*, align 8
%args47500$k40544$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40110, %struct.ScmObj* %args47500$k40544$0)
store volatile %struct.ScmObj* %args47500$k40544$1, %struct.ScmObj** %stackaddr$prim49374, align 8
%stackaddr$prim49375 = alloca %struct.ScmObj*, align 8
%args47500$k40544$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41122, %struct.ScmObj* %args47500$k40544$1)
store volatile %struct.ScmObj* %args47500$k40544$2, %struct.ScmObj** %stackaddr$prim49375, align 8
%clofunc49376 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40544)
musttail call tailcc void %clofunc49376(%struct.ScmObj* %k40544, %struct.ScmObj* %args47500$k40544$2)
ret void
falsebranch$cmp49373:
%stackaddr$prim49377 = alloca %struct.ScmObj*, align 8
%anf_45bind40260 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40109)
store volatile %struct.ScmObj* %anf_45bind40260, %struct.ScmObj** %stackaddr$prim49377, align 8
%stackaddr$makeclosure49378 = alloca %struct.ScmObj*, align 8
%fptrToInt49379 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41129 to i64
%ae41129 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49379)
store volatile %struct.ScmObj* %ae41129, %struct.ScmObj** %stackaddr$makeclosure49378, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41129, %struct.ScmObj* %k40544, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae41129, %struct.ScmObj* %f40111, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae41129, %struct.ScmObj* %lst40109, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae41129, %struct.ScmObj* %_37foldl140108, i64 3)
%args47505$f40111$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49380 = alloca %struct.ScmObj*, align 8
%args47505$f40111$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40110, %struct.ScmObj* %args47505$f40111$0)
store volatile %struct.ScmObj* %args47505$f40111$1, %struct.ScmObj** %stackaddr$prim49380, align 8
%stackaddr$prim49381 = alloca %struct.ScmObj*, align 8
%args47505$f40111$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40260, %struct.ScmObj* %args47505$f40111$1)
store volatile %struct.ScmObj* %args47505$f40111$2, %struct.ScmObj** %stackaddr$prim49381, align 8
%stackaddr$prim49382 = alloca %struct.ScmObj*, align 8
%args47505$f40111$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41129, %struct.ScmObj* %args47505$f40111$2)
store volatile %struct.ScmObj* %args47505$f40111$3, %struct.ScmObj** %stackaddr$prim49382, align 8
%clofunc49383 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40111)
musttail call tailcc void %clofunc49383(%struct.ScmObj* %f40111, %struct.ScmObj* %args47505$f40111$3)
ret void
}

define tailcc void @proc_clo$ae41129(%struct.ScmObj* %env$ae41129,%struct.ScmObj* %current_45args47501) {
%stackaddr$env-ref49384 = alloca %struct.ScmObj*, align 8
%k40544 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41129, i64 0)
store %struct.ScmObj* %k40544, %struct.ScmObj** %stackaddr$env-ref49384
%stackaddr$env-ref49385 = alloca %struct.ScmObj*, align 8
%f40111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41129, i64 1)
store %struct.ScmObj* %f40111, %struct.ScmObj** %stackaddr$env-ref49385
%stackaddr$env-ref49386 = alloca %struct.ScmObj*, align 8
%lst40109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41129, i64 2)
store %struct.ScmObj* %lst40109, %struct.ScmObj** %stackaddr$env-ref49386
%stackaddr$env-ref49387 = alloca %struct.ScmObj*, align 8
%_37foldl140108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41129, i64 3)
store %struct.ScmObj* %_37foldl140108, %struct.ScmObj** %stackaddr$env-ref49387
%stackaddr$prim49388 = alloca %struct.ScmObj*, align 8
%_95k40545 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47501)
store volatile %struct.ScmObj* %_95k40545, %struct.ScmObj** %stackaddr$prim49388, align 8
%stackaddr$prim49389 = alloca %struct.ScmObj*, align 8
%current_45args47502 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47501)
store volatile %struct.ScmObj* %current_45args47502, %struct.ScmObj** %stackaddr$prim49389, align 8
%stackaddr$prim49390 = alloca %struct.ScmObj*, align 8
%anf_45bind40261 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47502)
store volatile %struct.ScmObj* %anf_45bind40261, %struct.ScmObj** %stackaddr$prim49390, align 8
%stackaddr$prim49391 = alloca %struct.ScmObj*, align 8
%anf_45bind40262 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40109)
store volatile %struct.ScmObj* %anf_45bind40262, %struct.ScmObj** %stackaddr$prim49391, align 8
%args47504$_37foldl140108$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49392 = alloca %struct.ScmObj*, align 8
%args47504$_37foldl140108$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40262, %struct.ScmObj* %args47504$_37foldl140108$0)
store volatile %struct.ScmObj* %args47504$_37foldl140108$1, %struct.ScmObj** %stackaddr$prim49392, align 8
%stackaddr$prim49393 = alloca %struct.ScmObj*, align 8
%args47504$_37foldl140108$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40261, %struct.ScmObj* %args47504$_37foldl140108$1)
store volatile %struct.ScmObj* %args47504$_37foldl140108$2, %struct.ScmObj** %stackaddr$prim49393, align 8
%stackaddr$prim49394 = alloca %struct.ScmObj*, align 8
%args47504$_37foldl140108$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40111, %struct.ScmObj* %args47504$_37foldl140108$2)
store volatile %struct.ScmObj* %args47504$_37foldl140108$3, %struct.ScmObj** %stackaddr$prim49394, align 8
%stackaddr$prim49395 = alloca %struct.ScmObj*, align 8
%args47504$_37foldl140108$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40544, %struct.ScmObj* %args47504$_37foldl140108$3)
store volatile %struct.ScmObj* %args47504$_37foldl140108$4, %struct.ScmObj** %stackaddr$prim49395, align 8
%clofunc49396 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl140108)
musttail call tailcc void %clofunc49396(%struct.ScmObj* %_37foldl140108, %struct.ScmObj* %args47504$_37foldl140108$4)
ret void
}

define tailcc void @proc_clo$ae41032(%struct.ScmObj* %env$ae41032,%struct.ScmObj* %current_45args47509) {
%stackaddr$prim49397 = alloca %struct.ScmObj*, align 8
%k40546 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47509)
store volatile %struct.ScmObj* %k40546, %struct.ScmObj** %stackaddr$prim49397, align 8
%stackaddr$prim49398 = alloca %struct.ScmObj*, align 8
%current_45args47510 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47509)
store volatile %struct.ScmObj* %current_45args47510, %struct.ScmObj** %stackaddr$prim49398, align 8
%stackaddr$prim49399 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47510)
store volatile %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$prim49399, align 8
%ae41034 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49400 = alloca %struct.ScmObj*, align 8
%fptrToInt49401 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41035 to i64
%ae41035 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49401)
store volatile %struct.ScmObj* %ae41035, %struct.ScmObj** %stackaddr$makeclosure49400, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41035, %struct.ScmObj* %_37length40113, i64 0)
%args47521$k40546$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49402 = alloca %struct.ScmObj*, align 8
%args47521$k40546$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41035, %struct.ScmObj* %args47521$k40546$0)
store volatile %struct.ScmObj* %args47521$k40546$1, %struct.ScmObj** %stackaddr$prim49402, align 8
%stackaddr$prim49403 = alloca %struct.ScmObj*, align 8
%args47521$k40546$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41034, %struct.ScmObj* %args47521$k40546$1)
store volatile %struct.ScmObj* %args47521$k40546$2, %struct.ScmObj** %stackaddr$prim49403, align 8
%clofunc49404 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40546)
musttail call tailcc void %clofunc49404(%struct.ScmObj* %k40546, %struct.ScmObj* %args47521$k40546$2)
ret void
}

define tailcc void @proc_clo$ae41035(%struct.ScmObj* %env$ae41035,%struct.ScmObj* %current_45args47512) {
%stackaddr$env-ref49405 = alloca %struct.ScmObj*, align 8
%_37length40113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41035, i64 0)
store %struct.ScmObj* %_37length40113, %struct.ScmObj** %stackaddr$env-ref49405
%stackaddr$prim49406 = alloca %struct.ScmObj*, align 8
%k40547 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47512)
store volatile %struct.ScmObj* %k40547, %struct.ScmObj** %stackaddr$prim49406, align 8
%stackaddr$prim49407 = alloca %struct.ScmObj*, align 8
%current_45args47513 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47512)
store volatile %struct.ScmObj* %current_45args47513, %struct.ScmObj** %stackaddr$prim49407, align 8
%stackaddr$prim49408 = alloca %struct.ScmObj*, align 8
%lst40114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47513)
store volatile %struct.ScmObj* %lst40114, %struct.ScmObj** %stackaddr$prim49408, align 8
%stackaddr$prim49409 = alloca %struct.ScmObj*, align 8
%anf_45bind40255 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40114)
store volatile %struct.ScmObj* %anf_45bind40255, %struct.ScmObj** %stackaddr$prim49409, align 8
%truthy$cmp49410 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40255)
%cmp$cmp49410 = icmp eq i64 %truthy$cmp49410, 1
br i1 %cmp$cmp49410, label %truebranch$cmp49410, label %falsebranch$cmp49410
truebranch$cmp49410:
%ae41039 = call %struct.ScmObj* @const_init_int(i64 0)
%ae41040 = call %struct.ScmObj* @const_init_int(i64 0)
%args47515$k40547$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49411 = alloca %struct.ScmObj*, align 8
%args47515$k40547$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41040, %struct.ScmObj* %args47515$k40547$0)
store volatile %struct.ScmObj* %args47515$k40547$1, %struct.ScmObj** %stackaddr$prim49411, align 8
%stackaddr$prim49412 = alloca %struct.ScmObj*, align 8
%args47515$k40547$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41039, %struct.ScmObj* %args47515$k40547$1)
store volatile %struct.ScmObj* %args47515$k40547$2, %struct.ScmObj** %stackaddr$prim49412, align 8
%clofunc49413 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40547)
musttail call tailcc void %clofunc49413(%struct.ScmObj* %k40547, %struct.ScmObj* %args47515$k40547$2)
ret void
falsebranch$cmp49410:
%stackaddr$prim49414 = alloca %struct.ScmObj*, align 8
%anf_45bind40256 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40114)
store volatile %struct.ScmObj* %anf_45bind40256, %struct.ScmObj** %stackaddr$prim49414, align 8
%stackaddr$makeclosure49415 = alloca %struct.ScmObj*, align 8
%fptrToInt49416 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae41049 to i64
%ae41049 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49416)
store volatile %struct.ScmObj* %ae41049, %struct.ScmObj** %stackaddr$makeclosure49415, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae41049, %struct.ScmObj* %k40547, i64 0)
%args47520$_37length40113$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49417 = alloca %struct.ScmObj*, align 8
%args47520$_37length40113$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40256, %struct.ScmObj* %args47520$_37length40113$0)
store volatile %struct.ScmObj* %args47520$_37length40113$1, %struct.ScmObj** %stackaddr$prim49417, align 8
%stackaddr$prim49418 = alloca %struct.ScmObj*, align 8
%args47520$_37length40113$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41049, %struct.ScmObj* %args47520$_37length40113$1)
store volatile %struct.ScmObj* %args47520$_37length40113$2, %struct.ScmObj** %stackaddr$prim49418, align 8
%clofunc49419 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length40113)
musttail call tailcc void %clofunc49419(%struct.ScmObj* %_37length40113, %struct.ScmObj* %args47520$_37length40113$2)
ret void
}

define tailcc void @proc_clo$ae41049(%struct.ScmObj* %env$ae41049,%struct.ScmObj* %current_45args47516) {
%stackaddr$env-ref49420 = alloca %struct.ScmObj*, align 8
%k40547 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae41049, i64 0)
store %struct.ScmObj* %k40547, %struct.ScmObj** %stackaddr$env-ref49420
%stackaddr$prim49421 = alloca %struct.ScmObj*, align 8
%_95k40548 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47516)
store volatile %struct.ScmObj* %_95k40548, %struct.ScmObj** %stackaddr$prim49421, align 8
%stackaddr$prim49422 = alloca %struct.ScmObj*, align 8
%current_45args47517 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47516)
store volatile %struct.ScmObj* %current_45args47517, %struct.ScmObj** %stackaddr$prim49422, align 8
%stackaddr$prim49423 = alloca %struct.ScmObj*, align 8
%anf_45bind40257 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47517)
store volatile %struct.ScmObj* %anf_45bind40257, %struct.ScmObj** %stackaddr$prim49423, align 8
%ae41051 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49424 = alloca %struct.ScmObj*, align 8
%cpsprim40549 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae41051, %struct.ScmObj* %anf_45bind40257)
store volatile %struct.ScmObj* %cpsprim40549, %struct.ScmObj** %stackaddr$prim49424, align 8
%ae41054 = call %struct.ScmObj* @const_init_int(i64 0)
%args47519$k40547$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49425 = alloca %struct.ScmObj*, align 8
%args47519$k40547$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40549, %struct.ScmObj* %args47519$k40547$0)
store volatile %struct.ScmObj* %args47519$k40547$1, %struct.ScmObj** %stackaddr$prim49425, align 8
%stackaddr$prim49426 = alloca %struct.ScmObj*, align 8
%args47519$k40547$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae41054, %struct.ScmObj* %args47519$k40547$1)
store volatile %struct.ScmObj* %args47519$k40547$2, %struct.ScmObj** %stackaddr$prim49426, align 8
%clofunc49427 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40547)
musttail call tailcc void %clofunc49427(%struct.ScmObj* %k40547, %struct.ScmObj* %args47519$k40547$2)
ret void
}

define tailcc void @proc_clo$ae40882(%struct.ScmObj* %env$ae40882,%struct.ScmObj* %current_45args47524) {
%stackaddr$prim49428 = alloca %struct.ScmObj*, align 8
%k40550 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47524)
store volatile %struct.ScmObj* %k40550, %struct.ScmObj** %stackaddr$prim49428, align 8
%stackaddr$prim49429 = alloca %struct.ScmObj*, align 8
%current_45args47525 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47524)
store volatile %struct.ScmObj* %current_45args47525, %struct.ScmObj** %stackaddr$prim49429, align 8
%stackaddr$prim49430 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47525)
store volatile %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$prim49430, align 8
%ae40884 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49431 = alloca %struct.ScmObj*, align 8
%fptrToInt49432 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40885 to i64
%ae40885 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49432)
store volatile %struct.ScmObj* %ae40885, %struct.ScmObj** %stackaddr$makeclosure49431, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40885, %struct.ScmObj* %_37take40116, i64 0)
%args47538$k40550$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49433 = alloca %struct.ScmObj*, align 8
%args47538$k40550$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40885, %struct.ScmObj* %args47538$k40550$0)
store volatile %struct.ScmObj* %args47538$k40550$1, %struct.ScmObj** %stackaddr$prim49433, align 8
%stackaddr$prim49434 = alloca %struct.ScmObj*, align 8
%args47538$k40550$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40884, %struct.ScmObj* %args47538$k40550$1)
store volatile %struct.ScmObj* %args47538$k40550$2, %struct.ScmObj** %stackaddr$prim49434, align 8
%clofunc49435 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40550)
musttail call tailcc void %clofunc49435(%struct.ScmObj* %k40550, %struct.ScmObj* %args47538$k40550$2)
ret void
}

define tailcc void @proc_clo$ae40885(%struct.ScmObj* %env$ae40885,%struct.ScmObj* %current_45args47527) {
%stackaddr$env-ref49436 = alloca %struct.ScmObj*, align 8
%_37take40116 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40885, i64 0)
store %struct.ScmObj* %_37take40116, %struct.ScmObj** %stackaddr$env-ref49436
%stackaddr$prim49437 = alloca %struct.ScmObj*, align 8
%k40551 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47527)
store volatile %struct.ScmObj* %k40551, %struct.ScmObj** %stackaddr$prim49437, align 8
%stackaddr$prim49438 = alloca %struct.ScmObj*, align 8
%current_45args47528 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47527)
store volatile %struct.ScmObj* %current_45args47528, %struct.ScmObj** %stackaddr$prim49438, align 8
%stackaddr$prim49439 = alloca %struct.ScmObj*, align 8
%lst40118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47528)
store volatile %struct.ScmObj* %lst40118, %struct.ScmObj** %stackaddr$prim49439, align 8
%stackaddr$prim49440 = alloca %struct.ScmObj*, align 8
%current_45args47529 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47528)
store volatile %struct.ScmObj* %current_45args47529, %struct.ScmObj** %stackaddr$prim49440, align 8
%stackaddr$prim49441 = alloca %struct.ScmObj*, align 8
%n40117 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47529)
store volatile %struct.ScmObj* %n40117, %struct.ScmObj** %stackaddr$prim49441, align 8
%ae40887 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim49442 = alloca %struct.ScmObj*, align 8
%anf_45bind40248 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n40117, %struct.ScmObj* %ae40887)
store volatile %struct.ScmObj* %anf_45bind40248, %struct.ScmObj** %stackaddr$prim49442, align 8
%truthy$cmp49443 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40248)
%cmp$cmp49443 = icmp eq i64 %truthy$cmp49443, 1
br i1 %cmp$cmp49443, label %truebranch$cmp49443, label %falsebranch$cmp49443
truebranch$cmp49443:
%ae40890 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40891 = call %struct.ScmObj* @const_init_null()
%args47531$k40551$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49444 = alloca %struct.ScmObj*, align 8
%args47531$k40551$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40891, %struct.ScmObj* %args47531$k40551$0)
store volatile %struct.ScmObj* %args47531$k40551$1, %struct.ScmObj** %stackaddr$prim49444, align 8
%stackaddr$prim49445 = alloca %struct.ScmObj*, align 8
%args47531$k40551$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40890, %struct.ScmObj* %args47531$k40551$1)
store volatile %struct.ScmObj* %args47531$k40551$2, %struct.ScmObj** %stackaddr$prim49445, align 8
%clofunc49446 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40551)
musttail call tailcc void %clofunc49446(%struct.ScmObj* %k40551, %struct.ScmObj* %args47531$k40551$2)
ret void
falsebranch$cmp49443:
%stackaddr$prim49447 = alloca %struct.ScmObj*, align 8
%anf_45bind40249 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40118)
store volatile %struct.ScmObj* %anf_45bind40249, %struct.ScmObj** %stackaddr$prim49447, align 8
%truthy$cmp49448 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40249)
%cmp$cmp49448 = icmp eq i64 %truthy$cmp49448, 1
br i1 %cmp$cmp49448, label %truebranch$cmp49448, label %falsebranch$cmp49448
truebranch$cmp49448:
%ae40901 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40902 = call %struct.ScmObj* @const_init_null()
%args47532$k40551$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49449 = alloca %struct.ScmObj*, align 8
%args47532$k40551$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40902, %struct.ScmObj* %args47532$k40551$0)
store volatile %struct.ScmObj* %args47532$k40551$1, %struct.ScmObj** %stackaddr$prim49449, align 8
%stackaddr$prim49450 = alloca %struct.ScmObj*, align 8
%args47532$k40551$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40901, %struct.ScmObj* %args47532$k40551$1)
store volatile %struct.ScmObj* %args47532$k40551$2, %struct.ScmObj** %stackaddr$prim49450, align 8
%clofunc49451 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40551)
musttail call tailcc void %clofunc49451(%struct.ScmObj* %k40551, %struct.ScmObj* %args47532$k40551$2)
ret void
falsebranch$cmp49448:
%stackaddr$prim49452 = alloca %struct.ScmObj*, align 8
%anf_45bind40250 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40118)
store volatile %struct.ScmObj* %anf_45bind40250, %struct.ScmObj** %stackaddr$prim49452, align 8
%stackaddr$prim49453 = alloca %struct.ScmObj*, align 8
%anf_45bind40251 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40118)
store volatile %struct.ScmObj* %anf_45bind40251, %struct.ScmObj** %stackaddr$prim49453, align 8
%ae40912 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim49454 = alloca %struct.ScmObj*, align 8
%anf_45bind40252 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n40117, %struct.ScmObj* %ae40912)
store volatile %struct.ScmObj* %anf_45bind40252, %struct.ScmObj** %stackaddr$prim49454, align 8
%stackaddr$makeclosure49455 = alloca %struct.ScmObj*, align 8
%fptrToInt49456 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40914 to i64
%ae40914 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49456)
store volatile %struct.ScmObj* %ae40914, %struct.ScmObj** %stackaddr$makeclosure49455, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40914, %struct.ScmObj* %k40551, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40914, %struct.ScmObj* %anf_45bind40250, i64 1)
%args47537$_37take40116$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49457 = alloca %struct.ScmObj*, align 8
%args47537$_37take40116$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40252, %struct.ScmObj* %args47537$_37take40116$0)
store volatile %struct.ScmObj* %args47537$_37take40116$1, %struct.ScmObj** %stackaddr$prim49457, align 8
%stackaddr$prim49458 = alloca %struct.ScmObj*, align 8
%args47537$_37take40116$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40251, %struct.ScmObj* %args47537$_37take40116$1)
store volatile %struct.ScmObj* %args47537$_37take40116$2, %struct.ScmObj** %stackaddr$prim49458, align 8
%stackaddr$prim49459 = alloca %struct.ScmObj*, align 8
%args47537$_37take40116$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40914, %struct.ScmObj* %args47537$_37take40116$2)
store volatile %struct.ScmObj* %args47537$_37take40116$3, %struct.ScmObj** %stackaddr$prim49459, align 8
%clofunc49460 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take40116)
musttail call tailcc void %clofunc49460(%struct.ScmObj* %_37take40116, %struct.ScmObj* %args47537$_37take40116$3)
ret void
}

define tailcc void @proc_clo$ae40914(%struct.ScmObj* %env$ae40914,%struct.ScmObj* %current_45args47533) {
%stackaddr$env-ref49461 = alloca %struct.ScmObj*, align 8
%k40551 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40914, i64 0)
store %struct.ScmObj* %k40551, %struct.ScmObj** %stackaddr$env-ref49461
%stackaddr$env-ref49462 = alloca %struct.ScmObj*, align 8
%anf_45bind40250 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40914, i64 1)
store %struct.ScmObj* %anf_45bind40250, %struct.ScmObj** %stackaddr$env-ref49462
%stackaddr$prim49463 = alloca %struct.ScmObj*, align 8
%_95k40552 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47533)
store volatile %struct.ScmObj* %_95k40552, %struct.ScmObj** %stackaddr$prim49463, align 8
%stackaddr$prim49464 = alloca %struct.ScmObj*, align 8
%current_45args47534 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47533)
store volatile %struct.ScmObj* %current_45args47534, %struct.ScmObj** %stackaddr$prim49464, align 8
%stackaddr$prim49465 = alloca %struct.ScmObj*, align 8
%anf_45bind40253 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47534)
store volatile %struct.ScmObj* %anf_45bind40253, %struct.ScmObj** %stackaddr$prim49465, align 8
%stackaddr$prim49466 = alloca %struct.ScmObj*, align 8
%cpsprim40553 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40250, %struct.ScmObj* %anf_45bind40253)
store volatile %struct.ScmObj* %cpsprim40553, %struct.ScmObj** %stackaddr$prim49466, align 8
%ae40920 = call %struct.ScmObj* @const_init_int(i64 0)
%args47536$k40551$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49467 = alloca %struct.ScmObj*, align 8
%args47536$k40551$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40553, %struct.ScmObj* %args47536$k40551$0)
store volatile %struct.ScmObj* %args47536$k40551$1, %struct.ScmObj** %stackaddr$prim49467, align 8
%stackaddr$prim49468 = alloca %struct.ScmObj*, align 8
%args47536$k40551$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40920, %struct.ScmObj* %args47536$k40551$1)
store volatile %struct.ScmObj* %args47536$k40551$2, %struct.ScmObj** %stackaddr$prim49468, align 8
%clofunc49469 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40551)
musttail call tailcc void %clofunc49469(%struct.ScmObj* %k40551, %struct.ScmObj* %args47536$k40551$2)
ret void
}

define tailcc void @proc_clo$ae40785(%struct.ScmObj* %env$ae40785,%struct.ScmObj* %current_45args47541) {
%stackaddr$prim49470 = alloca %struct.ScmObj*, align 8
%k40554 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47541)
store volatile %struct.ScmObj* %k40554, %struct.ScmObj** %stackaddr$prim49470, align 8
%stackaddr$prim49471 = alloca %struct.ScmObj*, align 8
%current_45args47542 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47541)
store volatile %struct.ScmObj* %current_45args47542, %struct.ScmObj** %stackaddr$prim49471, align 8
%stackaddr$prim49472 = alloca %struct.ScmObj*, align 8
%_37map40120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47542)
store volatile %struct.ScmObj* %_37map40120, %struct.ScmObj** %stackaddr$prim49472, align 8
%ae40787 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49473 = alloca %struct.ScmObj*, align 8
%fptrToInt49474 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40788 to i64
%ae40788 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49474)
store volatile %struct.ScmObj* %ae40788, %struct.ScmObj** %stackaddr$makeclosure49473, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40788, %struct.ScmObj* %_37map40120, i64 0)
%args47558$k40554$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49475 = alloca %struct.ScmObj*, align 8
%args47558$k40554$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40788, %struct.ScmObj* %args47558$k40554$0)
store volatile %struct.ScmObj* %args47558$k40554$1, %struct.ScmObj** %stackaddr$prim49475, align 8
%stackaddr$prim49476 = alloca %struct.ScmObj*, align 8
%args47558$k40554$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40787, %struct.ScmObj* %args47558$k40554$1)
store volatile %struct.ScmObj* %args47558$k40554$2, %struct.ScmObj** %stackaddr$prim49476, align 8
%clofunc49477 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40554)
musttail call tailcc void %clofunc49477(%struct.ScmObj* %k40554, %struct.ScmObj* %args47558$k40554$2)
ret void
}

define tailcc void @proc_clo$ae40788(%struct.ScmObj* %env$ae40788,%struct.ScmObj* %current_45args47544) {
%stackaddr$env-ref49478 = alloca %struct.ScmObj*, align 8
%_37map40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40788, i64 0)
store %struct.ScmObj* %_37map40120, %struct.ScmObj** %stackaddr$env-ref49478
%stackaddr$prim49479 = alloca %struct.ScmObj*, align 8
%k40555 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47544)
store volatile %struct.ScmObj* %k40555, %struct.ScmObj** %stackaddr$prim49479, align 8
%stackaddr$prim49480 = alloca %struct.ScmObj*, align 8
%current_45args47545 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47544)
store volatile %struct.ScmObj* %current_45args47545, %struct.ScmObj** %stackaddr$prim49480, align 8
%stackaddr$prim49481 = alloca %struct.ScmObj*, align 8
%f40122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47545)
store volatile %struct.ScmObj* %f40122, %struct.ScmObj** %stackaddr$prim49481, align 8
%stackaddr$prim49482 = alloca %struct.ScmObj*, align 8
%current_45args47546 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47545)
store volatile %struct.ScmObj* %current_45args47546, %struct.ScmObj** %stackaddr$prim49482, align 8
%stackaddr$prim49483 = alloca %struct.ScmObj*, align 8
%lst40121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47546)
store volatile %struct.ScmObj* %lst40121, %struct.ScmObj** %stackaddr$prim49483, align 8
%stackaddr$prim49484 = alloca %struct.ScmObj*, align 8
%anf_45bind40242 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40121)
store volatile %struct.ScmObj* %anf_45bind40242, %struct.ScmObj** %stackaddr$prim49484, align 8
%truthy$cmp49485 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40242)
%cmp$cmp49485 = icmp eq i64 %truthy$cmp49485, 1
br i1 %cmp$cmp49485, label %truebranch$cmp49485, label %falsebranch$cmp49485
truebranch$cmp49485:
%ae40792 = call %struct.ScmObj* @const_init_int(i64 0)
%ae40793 = call %struct.ScmObj* @const_init_null()
%args47548$k40555$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49486 = alloca %struct.ScmObj*, align 8
%args47548$k40555$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40793, %struct.ScmObj* %args47548$k40555$0)
store volatile %struct.ScmObj* %args47548$k40555$1, %struct.ScmObj** %stackaddr$prim49486, align 8
%stackaddr$prim49487 = alloca %struct.ScmObj*, align 8
%args47548$k40555$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40792, %struct.ScmObj* %args47548$k40555$1)
store volatile %struct.ScmObj* %args47548$k40555$2, %struct.ScmObj** %stackaddr$prim49487, align 8
%clofunc49488 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40555)
musttail call tailcc void %clofunc49488(%struct.ScmObj* %k40555, %struct.ScmObj* %args47548$k40555$2)
ret void
falsebranch$cmp49485:
%stackaddr$prim49489 = alloca %struct.ScmObj*, align 8
%anf_45bind40243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40121)
store volatile %struct.ScmObj* %anf_45bind40243, %struct.ScmObj** %stackaddr$prim49489, align 8
%stackaddr$makeclosure49490 = alloca %struct.ScmObj*, align 8
%fptrToInt49491 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40802 to i64
%ae40802 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt49491)
store volatile %struct.ScmObj* %ae40802, %struct.ScmObj** %stackaddr$makeclosure49490, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40802, %struct.ScmObj* %lst40121, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40802, %struct.ScmObj* %_37map40120, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40802, %struct.ScmObj* %k40555, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae40802, %struct.ScmObj* %f40122, i64 3)
%args47557$f40122$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49492 = alloca %struct.ScmObj*, align 8
%args47557$f40122$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40243, %struct.ScmObj* %args47557$f40122$0)
store volatile %struct.ScmObj* %args47557$f40122$1, %struct.ScmObj** %stackaddr$prim49492, align 8
%stackaddr$prim49493 = alloca %struct.ScmObj*, align 8
%args47557$f40122$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40802, %struct.ScmObj* %args47557$f40122$1)
store volatile %struct.ScmObj* %args47557$f40122$2, %struct.ScmObj** %stackaddr$prim49493, align 8
%clofunc49494 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40122)
musttail call tailcc void %clofunc49494(%struct.ScmObj* %f40122, %struct.ScmObj* %args47557$f40122$2)
ret void
}

define tailcc void @proc_clo$ae40802(%struct.ScmObj* %env$ae40802,%struct.ScmObj* %current_45args47549) {
%stackaddr$env-ref49495 = alloca %struct.ScmObj*, align 8
%lst40121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40802, i64 0)
store %struct.ScmObj* %lst40121, %struct.ScmObj** %stackaddr$env-ref49495
%stackaddr$env-ref49496 = alloca %struct.ScmObj*, align 8
%_37map40120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40802, i64 1)
store %struct.ScmObj* %_37map40120, %struct.ScmObj** %stackaddr$env-ref49496
%stackaddr$env-ref49497 = alloca %struct.ScmObj*, align 8
%k40555 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40802, i64 2)
store %struct.ScmObj* %k40555, %struct.ScmObj** %stackaddr$env-ref49497
%stackaddr$env-ref49498 = alloca %struct.ScmObj*, align 8
%f40122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40802, i64 3)
store %struct.ScmObj* %f40122, %struct.ScmObj** %stackaddr$env-ref49498
%stackaddr$prim49499 = alloca %struct.ScmObj*, align 8
%_95k40556 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47549)
store volatile %struct.ScmObj* %_95k40556, %struct.ScmObj** %stackaddr$prim49499, align 8
%stackaddr$prim49500 = alloca %struct.ScmObj*, align 8
%current_45args47550 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47549)
store volatile %struct.ScmObj* %current_45args47550, %struct.ScmObj** %stackaddr$prim49500, align 8
%stackaddr$prim49501 = alloca %struct.ScmObj*, align 8
%anf_45bind40244 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47550)
store volatile %struct.ScmObj* %anf_45bind40244, %struct.ScmObj** %stackaddr$prim49501, align 8
%stackaddr$prim49502 = alloca %struct.ScmObj*, align 8
%anf_45bind40245 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40121)
store volatile %struct.ScmObj* %anf_45bind40245, %struct.ScmObj** %stackaddr$prim49502, align 8
%stackaddr$makeclosure49503 = alloca %struct.ScmObj*, align 8
%fptrToInt49504 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40806 to i64
%ae40806 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49504)
store volatile %struct.ScmObj* %ae40806, %struct.ScmObj** %stackaddr$makeclosure49503, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40806, %struct.ScmObj* %anf_45bind40244, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40806, %struct.ScmObj* %k40555, i64 1)
%args47556$_37map40120$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49505 = alloca %struct.ScmObj*, align 8
%args47556$_37map40120$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40245, %struct.ScmObj* %args47556$_37map40120$0)
store volatile %struct.ScmObj* %args47556$_37map40120$1, %struct.ScmObj** %stackaddr$prim49505, align 8
%stackaddr$prim49506 = alloca %struct.ScmObj*, align 8
%args47556$_37map40120$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40122, %struct.ScmObj* %args47556$_37map40120$1)
store volatile %struct.ScmObj* %args47556$_37map40120$2, %struct.ScmObj** %stackaddr$prim49506, align 8
%stackaddr$prim49507 = alloca %struct.ScmObj*, align 8
%args47556$_37map40120$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40806, %struct.ScmObj* %args47556$_37map40120$2)
store volatile %struct.ScmObj* %args47556$_37map40120$3, %struct.ScmObj** %stackaddr$prim49507, align 8
%clofunc49508 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map40120)
musttail call tailcc void %clofunc49508(%struct.ScmObj* %_37map40120, %struct.ScmObj* %args47556$_37map40120$3)
ret void
}

define tailcc void @proc_clo$ae40806(%struct.ScmObj* %env$ae40806,%struct.ScmObj* %current_45args47552) {
%stackaddr$env-ref49509 = alloca %struct.ScmObj*, align 8
%anf_45bind40244 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40806, i64 0)
store %struct.ScmObj* %anf_45bind40244, %struct.ScmObj** %stackaddr$env-ref49509
%stackaddr$env-ref49510 = alloca %struct.ScmObj*, align 8
%k40555 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40806, i64 1)
store %struct.ScmObj* %k40555, %struct.ScmObj** %stackaddr$env-ref49510
%stackaddr$prim49511 = alloca %struct.ScmObj*, align 8
%_95k40557 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47552)
store volatile %struct.ScmObj* %_95k40557, %struct.ScmObj** %stackaddr$prim49511, align 8
%stackaddr$prim49512 = alloca %struct.ScmObj*, align 8
%current_45args47553 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47552)
store volatile %struct.ScmObj* %current_45args47553, %struct.ScmObj** %stackaddr$prim49512, align 8
%stackaddr$prim49513 = alloca %struct.ScmObj*, align 8
%anf_45bind40246 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47553)
store volatile %struct.ScmObj* %anf_45bind40246, %struct.ScmObj** %stackaddr$prim49513, align 8
%stackaddr$prim49514 = alloca %struct.ScmObj*, align 8
%cpsprim40558 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40244, %struct.ScmObj* %anf_45bind40246)
store volatile %struct.ScmObj* %cpsprim40558, %struct.ScmObj** %stackaddr$prim49514, align 8
%ae40812 = call %struct.ScmObj* @const_init_int(i64 0)
%args47555$k40555$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49515 = alloca %struct.ScmObj*, align 8
%args47555$k40555$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim40558, %struct.ScmObj* %args47555$k40555$0)
store volatile %struct.ScmObj* %args47555$k40555$1, %struct.ScmObj** %stackaddr$prim49515, align 8
%stackaddr$prim49516 = alloca %struct.ScmObj*, align 8
%args47555$k40555$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40812, %struct.ScmObj* %args47555$k40555$1)
store volatile %struct.ScmObj* %args47555$k40555$2, %struct.ScmObj** %stackaddr$prim49516, align 8
%clofunc49517 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40555)
musttail call tailcc void %clofunc49517(%struct.ScmObj* %k40555, %struct.ScmObj* %args47555$k40555$2)
ret void
}

define tailcc void @proc_clo$ae40705(%struct.ScmObj* %env$ae40705,%struct.ScmObj* %current_45args47561) {
%stackaddr$prim49518 = alloca %struct.ScmObj*, align 8
%k40559 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47561)
store volatile %struct.ScmObj* %k40559, %struct.ScmObj** %stackaddr$prim49518, align 8
%stackaddr$prim49519 = alloca %struct.ScmObj*, align 8
%current_45args47562 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47561)
store volatile %struct.ScmObj* %current_45args47562, %struct.ScmObj** %stackaddr$prim49519, align 8
%stackaddr$prim49520 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47562)
store volatile %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$prim49520, align 8
%ae40707 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49521 = alloca %struct.ScmObj*, align 8
%fptrToInt49522 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40708 to i64
%ae40708 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49522)
store volatile %struct.ScmObj* %ae40708, %struct.ScmObj** %stackaddr$makeclosure49521, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40708, %struct.ScmObj* %_37foldr140124, i64 0)
%args47575$k40559$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49523 = alloca %struct.ScmObj*, align 8
%args47575$k40559$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40708, %struct.ScmObj* %args47575$k40559$0)
store volatile %struct.ScmObj* %args47575$k40559$1, %struct.ScmObj** %stackaddr$prim49523, align 8
%stackaddr$prim49524 = alloca %struct.ScmObj*, align 8
%args47575$k40559$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40707, %struct.ScmObj* %args47575$k40559$1)
store volatile %struct.ScmObj* %args47575$k40559$2, %struct.ScmObj** %stackaddr$prim49524, align 8
%clofunc49525 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40559)
musttail call tailcc void %clofunc49525(%struct.ScmObj* %k40559, %struct.ScmObj* %args47575$k40559$2)
ret void
}

define tailcc void @proc_clo$ae40708(%struct.ScmObj* %env$ae40708,%struct.ScmObj* %current_45args47564) {
%stackaddr$env-ref49526 = alloca %struct.ScmObj*, align 8
%_37foldr140124 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40708, i64 0)
store %struct.ScmObj* %_37foldr140124, %struct.ScmObj** %stackaddr$env-ref49526
%stackaddr$prim49527 = alloca %struct.ScmObj*, align 8
%k40560 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47564)
store volatile %struct.ScmObj* %k40560, %struct.ScmObj** %stackaddr$prim49527, align 8
%stackaddr$prim49528 = alloca %struct.ScmObj*, align 8
%current_45args47565 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47564)
store volatile %struct.ScmObj* %current_45args47565, %struct.ScmObj** %stackaddr$prim49528, align 8
%stackaddr$prim49529 = alloca %struct.ScmObj*, align 8
%f40127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47565)
store volatile %struct.ScmObj* %f40127, %struct.ScmObj** %stackaddr$prim49529, align 8
%stackaddr$prim49530 = alloca %struct.ScmObj*, align 8
%current_45args47566 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47565)
store volatile %struct.ScmObj* %current_45args47566, %struct.ScmObj** %stackaddr$prim49530, align 8
%stackaddr$prim49531 = alloca %struct.ScmObj*, align 8
%acc40126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47566)
store volatile %struct.ScmObj* %acc40126, %struct.ScmObj** %stackaddr$prim49531, align 8
%stackaddr$prim49532 = alloca %struct.ScmObj*, align 8
%current_45args47567 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47566)
store volatile %struct.ScmObj* %current_45args47567, %struct.ScmObj** %stackaddr$prim49532, align 8
%stackaddr$prim49533 = alloca %struct.ScmObj*, align 8
%lst40125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47567)
store volatile %struct.ScmObj* %lst40125, %struct.ScmObj** %stackaddr$prim49533, align 8
%stackaddr$prim49534 = alloca %struct.ScmObj*, align 8
%anf_45bind40237 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst40125)
store volatile %struct.ScmObj* %anf_45bind40237, %struct.ScmObj** %stackaddr$prim49534, align 8
%truthy$cmp49535 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind40237)
%cmp$cmp49535 = icmp eq i64 %truthy$cmp49535, 1
br i1 %cmp$cmp49535, label %truebranch$cmp49535, label %falsebranch$cmp49535
truebranch$cmp49535:
%ae40712 = call %struct.ScmObj* @const_init_int(i64 0)
%args47569$k40560$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49536 = alloca %struct.ScmObj*, align 8
%args47569$k40560$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40126, %struct.ScmObj* %args47569$k40560$0)
store volatile %struct.ScmObj* %args47569$k40560$1, %struct.ScmObj** %stackaddr$prim49536, align 8
%stackaddr$prim49537 = alloca %struct.ScmObj*, align 8
%args47569$k40560$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40712, %struct.ScmObj* %args47569$k40560$1)
store volatile %struct.ScmObj* %args47569$k40560$2, %struct.ScmObj** %stackaddr$prim49537, align 8
%clofunc49538 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40560)
musttail call tailcc void %clofunc49538(%struct.ScmObj* %k40560, %struct.ScmObj* %args47569$k40560$2)
ret void
falsebranch$cmp49535:
%stackaddr$prim49539 = alloca %struct.ScmObj*, align 8
%anf_45bind40238 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst40125)
store volatile %struct.ScmObj* %anf_45bind40238, %struct.ScmObj** %stackaddr$prim49539, align 8
%stackaddr$prim49540 = alloca %struct.ScmObj*, align 8
%anf_45bind40239 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst40125)
store volatile %struct.ScmObj* %anf_45bind40239, %struct.ScmObj** %stackaddr$prim49540, align 8
%stackaddr$makeclosure49541 = alloca %struct.ScmObj*, align 8
%fptrToInt49542 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40720 to i64
%ae40720 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49542)
store volatile %struct.ScmObj* %ae40720, %struct.ScmObj** %stackaddr$makeclosure49541, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40720, %struct.ScmObj* %k40560, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40720, %struct.ScmObj* %f40127, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40720, %struct.ScmObj* %anf_45bind40238, i64 2)
%args47574$_37foldr140124$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49543 = alloca %struct.ScmObj*, align 8
%args47574$_37foldr140124$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40239, %struct.ScmObj* %args47574$_37foldr140124$0)
store volatile %struct.ScmObj* %args47574$_37foldr140124$1, %struct.ScmObj** %stackaddr$prim49543, align 8
%stackaddr$prim49544 = alloca %struct.ScmObj*, align 8
%args47574$_37foldr140124$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc40126, %struct.ScmObj* %args47574$_37foldr140124$1)
store volatile %struct.ScmObj* %args47574$_37foldr140124$2, %struct.ScmObj** %stackaddr$prim49544, align 8
%stackaddr$prim49545 = alloca %struct.ScmObj*, align 8
%args47574$_37foldr140124$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40127, %struct.ScmObj* %args47574$_37foldr140124$2)
store volatile %struct.ScmObj* %args47574$_37foldr140124$3, %struct.ScmObj** %stackaddr$prim49545, align 8
%stackaddr$prim49546 = alloca %struct.ScmObj*, align 8
%args47574$_37foldr140124$4 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40720, %struct.ScmObj* %args47574$_37foldr140124$3)
store volatile %struct.ScmObj* %args47574$_37foldr140124$4, %struct.ScmObj** %stackaddr$prim49546, align 8
%clofunc49547 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr140124)
musttail call tailcc void %clofunc49547(%struct.ScmObj* %_37foldr140124, %struct.ScmObj* %args47574$_37foldr140124$4)
ret void
}

define tailcc void @proc_clo$ae40720(%struct.ScmObj* %env$ae40720,%struct.ScmObj* %current_45args47570) {
%stackaddr$env-ref49548 = alloca %struct.ScmObj*, align 8
%k40560 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40720, i64 0)
store %struct.ScmObj* %k40560, %struct.ScmObj** %stackaddr$env-ref49548
%stackaddr$env-ref49549 = alloca %struct.ScmObj*, align 8
%f40127 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40720, i64 1)
store %struct.ScmObj* %f40127, %struct.ScmObj** %stackaddr$env-ref49549
%stackaddr$env-ref49550 = alloca %struct.ScmObj*, align 8
%anf_45bind40238 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40720, i64 2)
store %struct.ScmObj* %anf_45bind40238, %struct.ScmObj** %stackaddr$env-ref49550
%stackaddr$prim49551 = alloca %struct.ScmObj*, align 8
%_95k40561 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47570)
store volatile %struct.ScmObj* %_95k40561, %struct.ScmObj** %stackaddr$prim49551, align 8
%stackaddr$prim49552 = alloca %struct.ScmObj*, align 8
%current_45args47571 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47570)
store volatile %struct.ScmObj* %current_45args47571, %struct.ScmObj** %stackaddr$prim49552, align 8
%stackaddr$prim49553 = alloca %struct.ScmObj*, align 8
%anf_45bind40240 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47571)
store volatile %struct.ScmObj* %anf_45bind40240, %struct.ScmObj** %stackaddr$prim49553, align 8
%args47573$f40127$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49554 = alloca %struct.ScmObj*, align 8
%args47573$f40127$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40240, %struct.ScmObj* %args47573$f40127$0)
store volatile %struct.ScmObj* %args47573$f40127$1, %struct.ScmObj** %stackaddr$prim49554, align 8
%stackaddr$prim49555 = alloca %struct.ScmObj*, align 8
%args47573$f40127$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40238, %struct.ScmObj* %args47573$f40127$1)
store volatile %struct.ScmObj* %args47573$f40127$2, %struct.ScmObj** %stackaddr$prim49555, align 8
%stackaddr$prim49556 = alloca %struct.ScmObj*, align 8
%args47573$f40127$3 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40560, %struct.ScmObj* %args47573$f40127$2)
store volatile %struct.ScmObj* %args47573$f40127$3, %struct.ScmObj** %stackaddr$prim49556, align 8
%clofunc49557 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40127)
musttail call tailcc void %clofunc49557(%struct.ScmObj* %f40127, %struct.ScmObj* %args47573$f40127$3)
ret void
}

define tailcc void @proc_clo$ae40588(%struct.ScmObj* %env$ae40588,%struct.ScmObj* %current_45args47578) {
%stackaddr$prim49558 = alloca %struct.ScmObj*, align 8
%k40562 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47578)
store volatile %struct.ScmObj* %k40562, %struct.ScmObj** %stackaddr$prim49558, align 8
%stackaddr$prim49559 = alloca %struct.ScmObj*, align 8
%current_45args47579 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47578)
store volatile %struct.ScmObj* %current_45args47579, %struct.ScmObj** %stackaddr$prim49559, align 8
%stackaddr$prim49560 = alloca %struct.ScmObj*, align 8
%y40104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47579)
store volatile %struct.ScmObj* %y40104, %struct.ScmObj** %stackaddr$prim49560, align 8
%ae40590 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49561 = alloca %struct.ScmObj*, align 8
%fptrToInt49562 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40591 to i64
%ae40591 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt49562)
store volatile %struct.ScmObj* %ae40591, %struct.ScmObj** %stackaddr$makeclosure49561, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40591, %struct.ScmObj* %y40104, i64 0)
%args47597$k40562$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49563 = alloca %struct.ScmObj*, align 8
%args47597$k40562$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40591, %struct.ScmObj* %args47597$k40562$0)
store volatile %struct.ScmObj* %args47597$k40562$1, %struct.ScmObj** %stackaddr$prim49563, align 8
%stackaddr$prim49564 = alloca %struct.ScmObj*, align 8
%args47597$k40562$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40590, %struct.ScmObj* %args47597$k40562$1)
store volatile %struct.ScmObj* %args47597$k40562$2, %struct.ScmObj** %stackaddr$prim49564, align 8
%clofunc49565 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k40562)
musttail call tailcc void %clofunc49565(%struct.ScmObj* %k40562, %struct.ScmObj* %args47597$k40562$2)
ret void
}

define tailcc void @proc_clo$ae40591(%struct.ScmObj* %env$ae40591,%struct.ScmObj* %current_45args47581) {
%stackaddr$env-ref49566 = alloca %struct.ScmObj*, align 8
%y40104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40591, i64 0)
store %struct.ScmObj* %y40104, %struct.ScmObj** %stackaddr$env-ref49566
%stackaddr$prim49567 = alloca %struct.ScmObj*, align 8
%k40563 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47581)
store volatile %struct.ScmObj* %k40563, %struct.ScmObj** %stackaddr$prim49567, align 8
%stackaddr$prim49568 = alloca %struct.ScmObj*, align 8
%current_45args47582 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47581)
store volatile %struct.ScmObj* %current_45args47582, %struct.ScmObj** %stackaddr$prim49568, align 8
%stackaddr$prim49569 = alloca %struct.ScmObj*, align 8
%f40105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47582)
store volatile %struct.ScmObj* %f40105, %struct.ScmObj** %stackaddr$prim49569, align 8
%stackaddr$makeclosure49570 = alloca %struct.ScmObj*, align 8
%fptrToInt49571 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40592 to i64
%ae40592 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49571)
store volatile %struct.ScmObj* %ae40592, %struct.ScmObj** %stackaddr$makeclosure49570, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40592, %struct.ScmObj* %f40105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40592, %struct.ScmObj* %k40563, i64 1)
%ae40593 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure49572 = alloca %struct.ScmObj*, align 8
%fptrToInt49573 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40594 to i64
%ae40594 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49573)
store volatile %struct.ScmObj* %ae40594, %struct.ScmObj** %stackaddr$makeclosure49572, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40594, %struct.ScmObj* %f40105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40594, %struct.ScmObj* %y40104, i64 1)
%args47596$ae40592$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49574 = alloca %struct.ScmObj*, align 8
%args47596$ae40592$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40594, %struct.ScmObj* %args47596$ae40592$0)
store volatile %struct.ScmObj* %args47596$ae40592$1, %struct.ScmObj** %stackaddr$prim49574, align 8
%stackaddr$prim49575 = alloca %struct.ScmObj*, align 8
%args47596$ae40592$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40593, %struct.ScmObj* %args47596$ae40592$1)
store volatile %struct.ScmObj* %args47596$ae40592$2, %struct.ScmObj** %stackaddr$prim49575, align 8
%clofunc49576 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae40592)
musttail call tailcc void %clofunc49576(%struct.ScmObj* %ae40592, %struct.ScmObj* %args47596$ae40592$2)
ret void
}

define tailcc void @proc_clo$ae40592(%struct.ScmObj* %env$ae40592,%struct.ScmObj* %current_45args47584) {
%stackaddr$env-ref49577 = alloca %struct.ScmObj*, align 8
%f40105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40592, i64 0)
store %struct.ScmObj* %f40105, %struct.ScmObj** %stackaddr$env-ref49577
%stackaddr$env-ref49578 = alloca %struct.ScmObj*, align 8
%k40563 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40592, i64 1)
store %struct.ScmObj* %k40563, %struct.ScmObj** %stackaddr$env-ref49578
%stackaddr$prim49579 = alloca %struct.ScmObj*, align 8
%_95k40564 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47584)
store volatile %struct.ScmObj* %_95k40564, %struct.ScmObj** %stackaddr$prim49579, align 8
%stackaddr$prim49580 = alloca %struct.ScmObj*, align 8
%current_45args47585 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47584)
store volatile %struct.ScmObj* %current_45args47585, %struct.ScmObj** %stackaddr$prim49580, align 8
%stackaddr$prim49581 = alloca %struct.ScmObj*, align 8
%anf_45bind40235 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47585)
store volatile %struct.ScmObj* %anf_45bind40235, %struct.ScmObj** %stackaddr$prim49581, align 8
%args47587$f40105$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49582 = alloca %struct.ScmObj*, align 8
%args47587$f40105$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind40235, %struct.ScmObj* %args47587$f40105$0)
store volatile %struct.ScmObj* %args47587$f40105$1, %struct.ScmObj** %stackaddr$prim49582, align 8
%stackaddr$prim49583 = alloca %struct.ScmObj*, align 8
%args47587$f40105$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40563, %struct.ScmObj* %args47587$f40105$1)
store volatile %struct.ScmObj* %args47587$f40105$2, %struct.ScmObj** %stackaddr$prim49583, align 8
%clofunc49584 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f40105)
musttail call tailcc void %clofunc49584(%struct.ScmObj* %f40105, %struct.ScmObj* %args47587$f40105$2)
ret void
}

define tailcc void @proc_clo$ae40594(%struct.ScmObj* %env$ae40594,%struct.ScmObj* %args4010640565) {
%stackaddr$env-ref49585 = alloca %struct.ScmObj*, align 8
%f40105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40594, i64 0)
store %struct.ScmObj* %f40105, %struct.ScmObj** %stackaddr$env-ref49585
%stackaddr$env-ref49586 = alloca %struct.ScmObj*, align 8
%y40104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40594, i64 1)
store %struct.ScmObj* %y40104, %struct.ScmObj** %stackaddr$env-ref49586
%stackaddr$prim49587 = alloca %struct.ScmObj*, align 8
%k40566 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4010640565)
store volatile %struct.ScmObj* %k40566, %struct.ScmObj** %stackaddr$prim49587, align 8
%stackaddr$prim49588 = alloca %struct.ScmObj*, align 8
%args40106 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4010640565)
store volatile %struct.ScmObj* %args40106, %struct.ScmObj** %stackaddr$prim49588, align 8
%stackaddr$makeclosure49589 = alloca %struct.ScmObj*, align 8
%fptrToInt49590 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40598 to i64
%ae40598 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt49590)
store volatile %struct.ScmObj* %ae40598, %struct.ScmObj** %stackaddr$makeclosure49589, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40598, %struct.ScmObj* %f40105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40598, %struct.ScmObj* %k40566, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae40598, %struct.ScmObj* %args40106, i64 2)
%args47595$y40104$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49591 = alloca %struct.ScmObj*, align 8
%args47595$y40104$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y40104, %struct.ScmObj* %args47595$y40104$0)
store volatile %struct.ScmObj* %args47595$y40104$1, %struct.ScmObj** %stackaddr$prim49591, align 8
%stackaddr$prim49592 = alloca %struct.ScmObj*, align 8
%args47595$y40104$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40598, %struct.ScmObj* %args47595$y40104$1)
store volatile %struct.ScmObj* %args47595$y40104$2, %struct.ScmObj** %stackaddr$prim49592, align 8
%clofunc49593 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y40104)
musttail call tailcc void %clofunc49593(%struct.ScmObj* %y40104, %struct.ScmObj* %args47595$y40104$2)
ret void
}

define tailcc void @proc_clo$ae40598(%struct.ScmObj* %env$ae40598,%struct.ScmObj* %current_45args47588) {
%stackaddr$env-ref49594 = alloca %struct.ScmObj*, align 8
%f40105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40598, i64 0)
store %struct.ScmObj* %f40105, %struct.ScmObj** %stackaddr$env-ref49594
%stackaddr$env-ref49595 = alloca %struct.ScmObj*, align 8
%k40566 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40598, i64 1)
store %struct.ScmObj* %k40566, %struct.ScmObj** %stackaddr$env-ref49595
%stackaddr$env-ref49596 = alloca %struct.ScmObj*, align 8
%args40106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40598, i64 2)
store %struct.ScmObj* %args40106, %struct.ScmObj** %stackaddr$env-ref49596
%stackaddr$prim49597 = alloca %struct.ScmObj*, align 8
%_95k40567 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47588)
store volatile %struct.ScmObj* %_95k40567, %struct.ScmObj** %stackaddr$prim49597, align 8
%stackaddr$prim49598 = alloca %struct.ScmObj*, align 8
%current_45args47589 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47588)
store volatile %struct.ScmObj* %current_45args47589, %struct.ScmObj** %stackaddr$prim49598, align 8
%stackaddr$prim49599 = alloca %struct.ScmObj*, align 8
%anf_45bind40233 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47589)
store volatile %struct.ScmObj* %anf_45bind40233, %struct.ScmObj** %stackaddr$prim49599, align 8
%stackaddr$makeclosure49600 = alloca %struct.ScmObj*, align 8
%fptrToInt49601 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae40601 to i64
%ae40601 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt49601)
store volatile %struct.ScmObj* %ae40601, %struct.ScmObj** %stackaddr$makeclosure49600, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae40601, %struct.ScmObj* %k40566, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae40601, %struct.ScmObj* %args40106, i64 1)
%args47594$anf_45bind40233$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49602 = alloca %struct.ScmObj*, align 8
%args47594$anf_45bind40233$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f40105, %struct.ScmObj* %args47594$anf_45bind40233$0)
store volatile %struct.ScmObj* %args47594$anf_45bind40233$1, %struct.ScmObj** %stackaddr$prim49602, align 8
%stackaddr$prim49603 = alloca %struct.ScmObj*, align 8
%args47594$anf_45bind40233$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae40601, %struct.ScmObj* %args47594$anf_45bind40233$1)
store volatile %struct.ScmObj* %args47594$anf_45bind40233$2, %struct.ScmObj** %stackaddr$prim49603, align 8
%clofunc49604 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40233)
musttail call tailcc void %clofunc49604(%struct.ScmObj* %anf_45bind40233, %struct.ScmObj* %args47594$anf_45bind40233$2)
ret void
}

define tailcc void @proc_clo$ae40601(%struct.ScmObj* %env$ae40601,%struct.ScmObj* %current_45args47591) {
%stackaddr$env-ref49605 = alloca %struct.ScmObj*, align 8
%k40566 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40601, i64 0)
store %struct.ScmObj* %k40566, %struct.ScmObj** %stackaddr$env-ref49605
%stackaddr$env-ref49606 = alloca %struct.ScmObj*, align 8
%args40106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae40601, i64 1)
store %struct.ScmObj* %args40106, %struct.ScmObj** %stackaddr$env-ref49606
%stackaddr$prim49607 = alloca %struct.ScmObj*, align 8
%_95k40568 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47591)
store volatile %struct.ScmObj* %_95k40568, %struct.ScmObj** %stackaddr$prim49607, align 8
%stackaddr$prim49608 = alloca %struct.ScmObj*, align 8
%current_45args47592 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47591)
store volatile %struct.ScmObj* %current_45args47592, %struct.ScmObj** %stackaddr$prim49608, align 8
%stackaddr$prim49609 = alloca %struct.ScmObj*, align 8
%anf_45bind40234 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47592)
store volatile %struct.ScmObj* %anf_45bind40234, %struct.ScmObj** %stackaddr$prim49609, align 8
%stackaddr$prim49610 = alloca %struct.ScmObj*, align 8
%cpsargs40569 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40566, %struct.ScmObj* %args40106)
store volatile %struct.ScmObj* %cpsargs40569, %struct.ScmObj** %stackaddr$prim49610, align 8
%clofunc49611 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind40234)
musttail call tailcc void %clofunc49611(%struct.ScmObj* %anf_45bind40234, %struct.ScmObj* %cpsargs40569)
ret void
}

define tailcc void @proc_clo$ae40573(%struct.ScmObj* %env$ae40573,%struct.ScmObj* %current_45args47599) {
%stackaddr$prim49612 = alloca %struct.ScmObj*, align 8
%k40570 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47599)
store volatile %struct.ScmObj* %k40570, %struct.ScmObj** %stackaddr$prim49612, align 8
%stackaddr$prim49613 = alloca %struct.ScmObj*, align 8
%current_45args47600 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args47599)
store volatile %struct.ScmObj* %current_45args47600, %struct.ScmObj** %stackaddr$prim49613, align 8
%stackaddr$prim49614 = alloca %struct.ScmObj*, align 8
%yu40103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args47600)
store volatile %struct.ScmObj* %yu40103, %struct.ScmObj** %stackaddr$prim49614, align 8
%args47602$yu40103$0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim49615 = alloca %struct.ScmObj*, align 8
%args47602$yu40103$1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu40103, %struct.ScmObj* %args47602$yu40103$0)
store volatile %struct.ScmObj* %args47602$yu40103$1, %struct.ScmObj** %stackaddr$prim49615, align 8
%stackaddr$prim49616 = alloca %struct.ScmObj*, align 8
%args47602$yu40103$2 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k40570, %struct.ScmObj* %args47602$yu40103$1)
store volatile %struct.ScmObj* %args47602$yu40103$2, %struct.ScmObj** %stackaddr$prim49616, align 8
%clofunc49617 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu40103)
musttail call tailcc void %clofunc49617(%struct.ScmObj* %yu40103, %struct.ScmObj* %args47602$yu40103$2)
ret void
}