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

@global$sym$ae5213555969 = private unnamed_addr constant [4 x i8] c"yes\00", align 8

define ccc i32 @main() {
%mainenv55313 = call %struct.ScmObj* @const_init_null()
%mainargs55314 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv55313, %struct.ScmObj* %mainargs55314)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv55311,%struct.ScmObj* %mainargs55312) {
%stackaddr$makeclosure55315 = alloca %struct.ScmObj*, align 8
%fptrToInt55316 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48485 to i64
%ae48485 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55316)
store volatile %struct.ScmObj* %ae48485, %struct.ScmObj** %stackaddr$makeclosure55315, align 8
%ae48486 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55317 = alloca %struct.ScmObj*, align 8
%fptrToInt55318 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48487 to i64
%ae48487 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55318)
store volatile %struct.ScmObj* %ae48487, %struct.ScmObj** %stackaddr$makeclosure55317, align 8
%argslist55310$ae484850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55319 = alloca %struct.ScmObj*, align 8
%argslist55310$ae484851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48487, %struct.ScmObj* %argslist55310$ae484850)
store volatile %struct.ScmObj* %argslist55310$ae484851, %struct.ScmObj** %stackaddr$prim55319, align 8
%stackaddr$prim55320 = alloca %struct.ScmObj*, align 8
%argslist55310$ae484852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48486, %struct.ScmObj* %argslist55310$ae484851)
store volatile %struct.ScmObj* %argslist55310$ae484852, %struct.ScmObj** %stackaddr$prim55320, align 8
%clofunc55321 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48485)
musttail call tailcc void %clofunc55321(%struct.ScmObj* %ae48485, %struct.ScmObj* %argslist55310$ae484852)
ret void
}

define tailcc void @proc_clo$ae48485(%struct.ScmObj* %env$ae48485,%struct.ScmObj* %current_45args54694) {
%stackaddr$prim55322 = alloca %struct.ScmObj*, align 8
%_95k48283 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54694)
store volatile %struct.ScmObj* %_95k48283, %struct.ScmObj** %stackaddr$prim55322, align 8
%stackaddr$prim55323 = alloca %struct.ScmObj*, align 8
%current_45args54695 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54694)
store volatile %struct.ScmObj* %current_45args54695, %struct.ScmObj** %stackaddr$prim55323, align 8
%stackaddr$prim55324 = alloca %struct.ScmObj*, align 8
%anf_45bind48153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54695)
store volatile %struct.ScmObj* %anf_45bind48153, %struct.ScmObj** %stackaddr$prim55324, align 8
%stackaddr$makeclosure55325 = alloca %struct.ScmObj*, align 8
%fptrToInt55326 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48500 to i64
%ae48500 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55326)
store volatile %struct.ScmObj* %ae48500, %struct.ScmObj** %stackaddr$makeclosure55325, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48500, %struct.ScmObj* %anf_45bind48153, i64 0)
%ae48501 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55327 = alloca %struct.ScmObj*, align 8
%fptrToInt55328 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48502 to i64
%ae48502 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55328)
store volatile %struct.ScmObj* %ae48502, %struct.ScmObj** %stackaddr$makeclosure55327, align 8
%argslist55305$ae485000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55329 = alloca %struct.ScmObj*, align 8
%argslist55305$ae485001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48502, %struct.ScmObj* %argslist55305$ae485000)
store volatile %struct.ScmObj* %argslist55305$ae485001, %struct.ScmObj** %stackaddr$prim55329, align 8
%stackaddr$prim55330 = alloca %struct.ScmObj*, align 8
%argslist55305$ae485002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48501, %struct.ScmObj* %argslist55305$ae485001)
store volatile %struct.ScmObj* %argslist55305$ae485002, %struct.ScmObj** %stackaddr$prim55330, align 8
%clofunc55331 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48500)
musttail call tailcc void %clofunc55331(%struct.ScmObj* %ae48500, %struct.ScmObj* %argslist55305$ae485002)
ret void
}

define tailcc void @proc_clo$ae48500(%struct.ScmObj* %env$ae48500,%struct.ScmObj* %current_45args54697) {
%stackaddr$env-ref55332 = alloca %struct.ScmObj*, align 8
%anf_45bind48153 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48500, i64 0)
store %struct.ScmObj* %anf_45bind48153, %struct.ScmObj** %stackaddr$env-ref55332
%stackaddr$prim55333 = alloca %struct.ScmObj*, align 8
%_95k48284 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54697)
store volatile %struct.ScmObj* %_95k48284, %struct.ScmObj** %stackaddr$prim55333, align 8
%stackaddr$prim55334 = alloca %struct.ScmObj*, align 8
%current_45args54698 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54697)
store volatile %struct.ScmObj* %current_45args54698, %struct.ScmObj** %stackaddr$prim55334, align 8
%stackaddr$prim55335 = alloca %struct.ScmObj*, align 8
%anf_45bind48157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54698)
store volatile %struct.ScmObj* %anf_45bind48157, %struct.ScmObj** %stackaddr$prim55335, align 8
%stackaddr$makeclosure55336 = alloca %struct.ScmObj*, align 8
%fptrToInt55337 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48615 to i64
%ae48615 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55337)
store volatile %struct.ScmObj* %ae48615, %struct.ScmObj** %stackaddr$makeclosure55336, align 8
%argslist55284$anf_45bind481530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55338 = alloca %struct.ScmObj*, align 8
%argslist55284$anf_45bind481531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48157, %struct.ScmObj* %argslist55284$anf_45bind481530)
store volatile %struct.ScmObj* %argslist55284$anf_45bind481531, %struct.ScmObj** %stackaddr$prim55338, align 8
%stackaddr$prim55339 = alloca %struct.ScmObj*, align 8
%argslist55284$anf_45bind481532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48615, %struct.ScmObj* %argslist55284$anf_45bind481531)
store volatile %struct.ScmObj* %argslist55284$anf_45bind481532, %struct.ScmObj** %stackaddr$prim55339, align 8
%clofunc55340 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48153)
musttail call tailcc void %clofunc55340(%struct.ScmObj* %anf_45bind48153, %struct.ScmObj* %argslist55284$anf_45bind481532)
ret void
}

define tailcc void @proc_clo$ae48615(%struct.ScmObj* %env$ae48615,%struct.ScmObj* %current_45args54700) {
%stackaddr$prim55341 = alloca %struct.ScmObj*, align 8
%_95k48285 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54700)
store volatile %struct.ScmObj* %_95k48285, %struct.ScmObj** %stackaddr$prim55341, align 8
%stackaddr$prim55342 = alloca %struct.ScmObj*, align 8
%current_45args54701 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54700)
store volatile %struct.ScmObj* %current_45args54701, %struct.ScmObj** %stackaddr$prim55342, align 8
%stackaddr$prim55343 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54701)
store volatile %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$prim55343, align 8
%stackaddr$makeclosure55344 = alloca %struct.ScmObj*, align 8
%fptrToInt55345 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48617 to i64
%ae48617 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55345)
store volatile %struct.ScmObj* %ae48617, %struct.ScmObj** %stackaddr$makeclosure55344, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48617, %struct.ScmObj* %Ycmb48025, i64 0)
%ae48618 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55346 = alloca %struct.ScmObj*, align 8
%fptrToInt55347 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48619 to i64
%ae48619 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55347)
store volatile %struct.ScmObj* %ae48619, %struct.ScmObj** %stackaddr$makeclosure55346, align 8
%argslist55283$ae486170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55348 = alloca %struct.ScmObj*, align 8
%argslist55283$ae486171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48619, %struct.ScmObj* %argslist55283$ae486170)
store volatile %struct.ScmObj* %argslist55283$ae486171, %struct.ScmObj** %stackaddr$prim55348, align 8
%stackaddr$prim55349 = alloca %struct.ScmObj*, align 8
%argslist55283$ae486172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48618, %struct.ScmObj* %argslist55283$ae486171)
store volatile %struct.ScmObj* %argslist55283$ae486172, %struct.ScmObj** %stackaddr$prim55349, align 8
%clofunc55350 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48617)
musttail call tailcc void %clofunc55350(%struct.ScmObj* %ae48617, %struct.ScmObj* %argslist55283$ae486172)
ret void
}

define tailcc void @proc_clo$ae48617(%struct.ScmObj* %env$ae48617,%struct.ScmObj* %current_45args54703) {
%stackaddr$env-ref55351 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48617, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55351
%stackaddr$prim55352 = alloca %struct.ScmObj*, align 8
%_95k48286 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54703)
store volatile %struct.ScmObj* %_95k48286, %struct.ScmObj** %stackaddr$prim55352, align 8
%stackaddr$prim55353 = alloca %struct.ScmObj*, align 8
%current_45args54704 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54703)
store volatile %struct.ScmObj* %current_45args54704, %struct.ScmObj** %stackaddr$prim55353, align 8
%stackaddr$prim55354 = alloca %struct.ScmObj*, align 8
%anf_45bind48162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54704)
store volatile %struct.ScmObj* %anf_45bind48162, %struct.ScmObj** %stackaddr$prim55354, align 8
%stackaddr$makeclosure55355 = alloca %struct.ScmObj*, align 8
%fptrToInt55356 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48695 to i64
%ae48695 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55356)
store volatile %struct.ScmObj* %ae48695, %struct.ScmObj** %stackaddr$makeclosure55355, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48695, %struct.ScmObj* %Ycmb48025, i64 0)
%argslist55267$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55357 = alloca %struct.ScmObj*, align 8
%argslist55267$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48162, %struct.ScmObj* %argslist55267$Ycmb480250)
store volatile %struct.ScmObj* %argslist55267$Ycmb480251, %struct.ScmObj** %stackaddr$prim55357, align 8
%stackaddr$prim55358 = alloca %struct.ScmObj*, align 8
%argslist55267$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48695, %struct.ScmObj* %argslist55267$Ycmb480251)
store volatile %struct.ScmObj* %argslist55267$Ycmb480252, %struct.ScmObj** %stackaddr$prim55358, align 8
%clofunc55359 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc55359(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist55267$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae48695(%struct.ScmObj* %env$ae48695,%struct.ScmObj* %current_45args54706) {
%stackaddr$env-ref55360 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48695, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55360
%stackaddr$prim55361 = alloca %struct.ScmObj*, align 8
%_95k48287 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54706)
store volatile %struct.ScmObj* %_95k48287, %struct.ScmObj** %stackaddr$prim55361, align 8
%stackaddr$prim55362 = alloca %struct.ScmObj*, align 8
%current_45args54707 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54706)
store volatile %struct.ScmObj* %current_45args54707, %struct.ScmObj** %stackaddr$prim55362, align 8
%stackaddr$prim55363 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54707)
store volatile %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$prim55363, align 8
%stackaddr$makeclosure55364 = alloca %struct.ScmObj*, align 8
%fptrToInt55365 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48697 to i64
%ae48697 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55365)
store volatile %struct.ScmObj* %ae48697, %struct.ScmObj** %stackaddr$makeclosure55364, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48697, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48697, %struct.ScmObj* %_37foldr148046, i64 1)
%ae48698 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55366 = alloca %struct.ScmObj*, align 8
%fptrToInt55367 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48699 to i64
%ae48699 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55367)
store volatile %struct.ScmObj* %ae48699, %struct.ScmObj** %stackaddr$makeclosure55366, align 8
%argslist55266$ae486970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55368 = alloca %struct.ScmObj*, align 8
%argslist55266$ae486971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48699, %struct.ScmObj* %argslist55266$ae486970)
store volatile %struct.ScmObj* %argslist55266$ae486971, %struct.ScmObj** %stackaddr$prim55368, align 8
%stackaddr$prim55369 = alloca %struct.ScmObj*, align 8
%argslist55266$ae486972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48698, %struct.ScmObj* %argslist55266$ae486971)
store volatile %struct.ScmObj* %argslist55266$ae486972, %struct.ScmObj** %stackaddr$prim55369, align 8
%clofunc55370 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48697)
musttail call tailcc void %clofunc55370(%struct.ScmObj* %ae48697, %struct.ScmObj* %argslist55266$ae486972)
ret void
}

define tailcc void @proc_clo$ae48697(%struct.ScmObj* %env$ae48697,%struct.ScmObj* %current_45args54709) {
%stackaddr$env-ref55371 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48697, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55371
%stackaddr$env-ref55372 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48697, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55372
%stackaddr$prim55373 = alloca %struct.ScmObj*, align 8
%_95k48288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54709)
store volatile %struct.ScmObj* %_95k48288, %struct.ScmObj** %stackaddr$prim55373, align 8
%stackaddr$prim55374 = alloca %struct.ScmObj*, align 8
%current_45args54710 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54709)
store volatile %struct.ScmObj* %current_45args54710, %struct.ScmObj** %stackaddr$prim55374, align 8
%stackaddr$prim55375 = alloca %struct.ScmObj*, align 8
%anf_45bind48168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54710)
store volatile %struct.ScmObj* %anf_45bind48168, %struct.ScmObj** %stackaddr$prim55375, align 8
%stackaddr$makeclosure55376 = alloca %struct.ScmObj*, align 8
%fptrToInt55377 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48792 to i64
%ae48792 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55377)
store volatile %struct.ScmObj* %ae48792, %struct.ScmObj** %stackaddr$makeclosure55376, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48792, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48792, %struct.ScmObj* %_37foldr148046, i64 1)
%argslist55247$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55378 = alloca %struct.ScmObj*, align 8
%argslist55247$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48168, %struct.ScmObj* %argslist55247$Ycmb480250)
store volatile %struct.ScmObj* %argslist55247$Ycmb480251, %struct.ScmObj** %stackaddr$prim55378, align 8
%stackaddr$prim55379 = alloca %struct.ScmObj*, align 8
%argslist55247$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48792, %struct.ScmObj* %argslist55247$Ycmb480251)
store volatile %struct.ScmObj* %argslist55247$Ycmb480252, %struct.ScmObj** %stackaddr$prim55379, align 8
%clofunc55380 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc55380(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist55247$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae48792(%struct.ScmObj* %env$ae48792,%struct.ScmObj* %current_45args54712) {
%stackaddr$env-ref55381 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48792, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55381
%stackaddr$env-ref55382 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48792, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55382
%stackaddr$prim55383 = alloca %struct.ScmObj*, align 8
%_95k48289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54712)
store volatile %struct.ScmObj* %_95k48289, %struct.ScmObj** %stackaddr$prim55383, align 8
%stackaddr$prim55384 = alloca %struct.ScmObj*, align 8
%current_45args54713 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54712)
store volatile %struct.ScmObj* %current_45args54713, %struct.ScmObj** %stackaddr$prim55384, align 8
%stackaddr$prim55385 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54713)
store volatile %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$prim55385, align 8
%stackaddr$makeclosure55386 = alloca %struct.ScmObj*, align 8
%fptrToInt55387 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48794 to i64
%ae48794 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55387)
store volatile %struct.ScmObj* %ae48794, %struct.ScmObj** %stackaddr$makeclosure55386, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48794, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48794, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48794, %struct.ScmObj* %_37map148042, i64 2)
%ae48795 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55388 = alloca %struct.ScmObj*, align 8
%fptrToInt55389 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48796 to i64
%ae48796 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55389)
store volatile %struct.ScmObj* %ae48796, %struct.ScmObj** %stackaddr$makeclosure55388, align 8
%argslist55246$ae487940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55390 = alloca %struct.ScmObj*, align 8
%argslist55246$ae487941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48796, %struct.ScmObj* %argslist55246$ae487940)
store volatile %struct.ScmObj* %argslist55246$ae487941, %struct.ScmObj** %stackaddr$prim55390, align 8
%stackaddr$prim55391 = alloca %struct.ScmObj*, align 8
%argslist55246$ae487942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48795, %struct.ScmObj* %argslist55246$ae487941)
store volatile %struct.ScmObj* %argslist55246$ae487942, %struct.ScmObj** %stackaddr$prim55391, align 8
%clofunc55392 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48794)
musttail call tailcc void %clofunc55392(%struct.ScmObj* %ae48794, %struct.ScmObj* %argslist55246$ae487942)
ret void
}

define tailcc void @proc_clo$ae48794(%struct.ScmObj* %env$ae48794,%struct.ScmObj* %current_45args54715) {
%stackaddr$env-ref55393 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48794, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55393
%stackaddr$env-ref55394 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48794, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55394
%stackaddr$env-ref55395 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48794, i64 2)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55395
%stackaddr$prim55396 = alloca %struct.ScmObj*, align 8
%_95k48290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54715)
store volatile %struct.ScmObj* %_95k48290, %struct.ScmObj** %stackaddr$prim55396, align 8
%stackaddr$prim55397 = alloca %struct.ScmObj*, align 8
%current_45args54716 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54715)
store volatile %struct.ScmObj* %current_45args54716, %struct.ScmObj** %stackaddr$prim55397, align 8
%stackaddr$prim55398 = alloca %struct.ScmObj*, align 8
%anf_45bind48175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54716)
store volatile %struct.ScmObj* %anf_45bind48175, %struct.ScmObj** %stackaddr$prim55398, align 8
%stackaddr$makeclosure55399 = alloca %struct.ScmObj*, align 8
%fptrToInt55400 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48942 to i64
%ae48942 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55400)
store volatile %struct.ScmObj* %ae48942, %struct.ScmObj** %stackaddr$makeclosure55399, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48942, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48942, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48942, %struct.ScmObj* %_37map148042, i64 2)
%argslist55230$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55401 = alloca %struct.ScmObj*, align 8
%argslist55230$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48175, %struct.ScmObj* %argslist55230$Ycmb480250)
store volatile %struct.ScmObj* %argslist55230$Ycmb480251, %struct.ScmObj** %stackaddr$prim55401, align 8
%stackaddr$prim55402 = alloca %struct.ScmObj*, align 8
%argslist55230$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48942, %struct.ScmObj* %argslist55230$Ycmb480251)
store volatile %struct.ScmObj* %argslist55230$Ycmb480252, %struct.ScmObj** %stackaddr$prim55402, align 8
%clofunc55403 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc55403(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist55230$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae48942(%struct.ScmObj* %env$ae48942,%struct.ScmObj* %current_45args54718) {
%stackaddr$env-ref55404 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48942, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55404
%stackaddr$env-ref55405 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48942, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55405
%stackaddr$env-ref55406 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48942, i64 2)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55406
%stackaddr$prim55407 = alloca %struct.ScmObj*, align 8
%_95k48291 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54718)
store volatile %struct.ScmObj* %_95k48291, %struct.ScmObj** %stackaddr$prim55407, align 8
%stackaddr$prim55408 = alloca %struct.ScmObj*, align 8
%current_45args54719 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54718)
store volatile %struct.ScmObj* %current_45args54719, %struct.ScmObj** %stackaddr$prim55408, align 8
%stackaddr$prim55409 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54719)
store volatile %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$prim55409, align 8
%stackaddr$makeclosure55410 = alloca %struct.ScmObj*, align 8
%fptrToInt55411 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48944 to i64
%ae48944 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55411)
store volatile %struct.ScmObj* %ae48944, %struct.ScmObj** %stackaddr$makeclosure55410, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48944, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48944, %struct.ScmObj* %_37take48038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48944, %struct.ScmObj* %_37foldr148046, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48944, %struct.ScmObj* %_37map148042, i64 3)
%ae48945 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55412 = alloca %struct.ScmObj*, align 8
%fptrToInt55413 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48946 to i64
%ae48946 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55413)
store volatile %struct.ScmObj* %ae48946, %struct.ScmObj** %stackaddr$makeclosure55412, align 8
%argslist55229$ae489440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55414 = alloca %struct.ScmObj*, align 8
%argslist55229$ae489441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48946, %struct.ScmObj* %argslist55229$ae489440)
store volatile %struct.ScmObj* %argslist55229$ae489441, %struct.ScmObj** %stackaddr$prim55414, align 8
%stackaddr$prim55415 = alloca %struct.ScmObj*, align 8
%argslist55229$ae489442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48945, %struct.ScmObj* %argslist55229$ae489441)
store volatile %struct.ScmObj* %argslist55229$ae489442, %struct.ScmObj** %stackaddr$prim55415, align 8
%clofunc55416 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48944)
musttail call tailcc void %clofunc55416(%struct.ScmObj* %ae48944, %struct.ScmObj* %argslist55229$ae489442)
ret void
}

define tailcc void @proc_clo$ae48944(%struct.ScmObj* %env$ae48944,%struct.ScmObj* %current_45args54721) {
%stackaddr$env-ref55417 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48944, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55417
%stackaddr$env-ref55418 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48944, i64 1)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref55418
%stackaddr$env-ref55419 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48944, i64 2)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55419
%stackaddr$env-ref55420 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48944, i64 3)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55420
%stackaddr$prim55421 = alloca %struct.ScmObj*, align 8
%_95k48292 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54721)
store volatile %struct.ScmObj* %_95k48292, %struct.ScmObj** %stackaddr$prim55421, align 8
%stackaddr$prim55422 = alloca %struct.ScmObj*, align 8
%current_45args54722 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54721)
store volatile %struct.ScmObj* %current_45args54722, %struct.ScmObj** %stackaddr$prim55422, align 8
%stackaddr$prim55423 = alloca %struct.ScmObj*, align 8
%anf_45bind48179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54722)
store volatile %struct.ScmObj* %anf_45bind48179, %struct.ScmObj** %stackaddr$prim55423, align 8
%stackaddr$makeclosure55424 = alloca %struct.ScmObj*, align 8
%fptrToInt55425 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49025 to i64
%ae49025 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55425)
store volatile %struct.ScmObj* %ae49025, %struct.ScmObj** %stackaddr$makeclosure55424, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49025, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49025, %struct.ScmObj* %_37take48038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49025, %struct.ScmObj* %_37foldr148046, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49025, %struct.ScmObj* %_37map148042, i64 3)
%argslist55215$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55426 = alloca %struct.ScmObj*, align 8
%argslist55215$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48179, %struct.ScmObj* %argslist55215$Ycmb480250)
store volatile %struct.ScmObj* %argslist55215$Ycmb480251, %struct.ScmObj** %stackaddr$prim55426, align 8
%stackaddr$prim55427 = alloca %struct.ScmObj*, align 8
%argslist55215$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49025, %struct.ScmObj* %argslist55215$Ycmb480251)
store volatile %struct.ScmObj* %argslist55215$Ycmb480252, %struct.ScmObj** %stackaddr$prim55427, align 8
%clofunc55428 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc55428(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist55215$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae49025(%struct.ScmObj* %env$ae49025,%struct.ScmObj* %current_45args54724) {
%stackaddr$env-ref55429 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49025, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55429
%stackaddr$env-ref55430 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49025, i64 1)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref55430
%stackaddr$env-ref55431 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49025, i64 2)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55431
%stackaddr$env-ref55432 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49025, i64 3)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55432
%stackaddr$prim55433 = alloca %struct.ScmObj*, align 8
%_95k48293 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54724)
store volatile %struct.ScmObj* %_95k48293, %struct.ScmObj** %stackaddr$prim55433, align 8
%stackaddr$prim55434 = alloca %struct.ScmObj*, align 8
%current_45args54725 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54724)
store volatile %struct.ScmObj* %current_45args54725, %struct.ScmObj** %stackaddr$prim55434, align 8
%stackaddr$prim55435 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54725)
store volatile %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$prim55435, align 8
%stackaddr$makeclosure55436 = alloca %struct.ScmObj*, align 8
%fptrToInt55437 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49027 to i64
%ae49027 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55437)
store volatile %struct.ScmObj* %ae49027, %struct.ScmObj** %stackaddr$makeclosure55436, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49027, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49027, %struct.ScmObj* %_37take48038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49027, %struct.ScmObj* %_37length48035, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49027, %struct.ScmObj* %_37foldr148046, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49027, %struct.ScmObj* %_37map148042, i64 4)
%ae49028 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55438 = alloca %struct.ScmObj*, align 8
%fptrToInt55439 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49029 to i64
%ae49029 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55439)
store volatile %struct.ScmObj* %ae49029, %struct.ScmObj** %stackaddr$makeclosure55438, align 8
%argslist55214$ae490270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55440 = alloca %struct.ScmObj*, align 8
%argslist55214$ae490271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49029, %struct.ScmObj* %argslist55214$ae490270)
store volatile %struct.ScmObj* %argslist55214$ae490271, %struct.ScmObj** %stackaddr$prim55440, align 8
%stackaddr$prim55441 = alloca %struct.ScmObj*, align 8
%argslist55214$ae490272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49028, %struct.ScmObj* %argslist55214$ae490271)
store volatile %struct.ScmObj* %argslist55214$ae490272, %struct.ScmObj** %stackaddr$prim55441, align 8
%clofunc55442 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49027)
musttail call tailcc void %clofunc55442(%struct.ScmObj* %ae49027, %struct.ScmObj* %argslist55214$ae490272)
ret void
}

define tailcc void @proc_clo$ae49027(%struct.ScmObj* %env$ae49027,%struct.ScmObj* %current_45args54727) {
%stackaddr$env-ref55443 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49027, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55443
%stackaddr$env-ref55444 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49027, i64 1)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref55444
%stackaddr$env-ref55445 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49027, i64 2)
store %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$env-ref55445
%stackaddr$env-ref55446 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49027, i64 3)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55446
%stackaddr$env-ref55447 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49027, i64 4)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55447
%stackaddr$prim55448 = alloca %struct.ScmObj*, align 8
%_95k48294 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54727)
store volatile %struct.ScmObj* %_95k48294, %struct.ScmObj** %stackaddr$prim55448, align 8
%stackaddr$prim55449 = alloca %struct.ScmObj*, align 8
%current_45args54728 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54727)
store volatile %struct.ScmObj* %current_45args54728, %struct.ScmObj** %stackaddr$prim55449, align 8
%stackaddr$prim55450 = alloca %struct.ScmObj*, align 8
%anf_45bind48184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54728)
store volatile %struct.ScmObj* %anf_45bind48184, %struct.ScmObj** %stackaddr$prim55450, align 8
%stackaddr$makeclosure55451 = alloca %struct.ScmObj*, align 8
%fptrToInt55452 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49104 to i64
%ae49104 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55452)
store volatile %struct.ScmObj* %ae49104, %struct.ScmObj** %stackaddr$makeclosure55451, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49104, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49104, %struct.ScmObj* %_37take48038, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49104, %struct.ScmObj* %_37length48035, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49104, %struct.ScmObj* %_37foldr148046, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49104, %struct.ScmObj* %_37map148042, i64 4)
%argslist55198$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55453 = alloca %struct.ScmObj*, align 8
%argslist55198$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48184, %struct.ScmObj* %argslist55198$Ycmb480250)
store volatile %struct.ScmObj* %argslist55198$Ycmb480251, %struct.ScmObj** %stackaddr$prim55453, align 8
%stackaddr$prim55454 = alloca %struct.ScmObj*, align 8
%argslist55198$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49104, %struct.ScmObj* %argslist55198$Ycmb480251)
store volatile %struct.ScmObj* %argslist55198$Ycmb480252, %struct.ScmObj** %stackaddr$prim55454, align 8
%clofunc55455 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc55455(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist55198$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae49104(%struct.ScmObj* %env$ae49104,%struct.ScmObj* %current_45args54730) {
%stackaddr$env-ref55456 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49104, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55456
%stackaddr$env-ref55457 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49104, i64 1)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref55457
%stackaddr$env-ref55458 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49104, i64 2)
store %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$env-ref55458
%stackaddr$env-ref55459 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49104, i64 3)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55459
%stackaddr$env-ref55460 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49104, i64 4)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55460
%stackaddr$prim55461 = alloca %struct.ScmObj*, align 8
%_95k48295 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54730)
store volatile %struct.ScmObj* %_95k48295, %struct.ScmObj** %stackaddr$prim55461, align 8
%stackaddr$prim55462 = alloca %struct.ScmObj*, align 8
%current_45args54731 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54730)
store volatile %struct.ScmObj* %current_45args54731, %struct.ScmObj** %stackaddr$prim55462, align 8
%stackaddr$prim55463 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54731)
store volatile %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$prim55463, align 8
%stackaddr$makeclosure55464 = alloca %struct.ScmObj*, align 8
%fptrToInt55465 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49106 to i64
%ae49106 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55465)
store volatile %struct.ScmObj* %ae49106, %struct.ScmObj** %stackaddr$makeclosure55464, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49106, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49106, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49106, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49106, %struct.ScmObj* %_37take48038, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49106, %struct.ScmObj* %_37length48035, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49106, %struct.ScmObj* %_37map148042, i64 5)
%ae49107 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55466 = alloca %struct.ScmObj*, align 8
%fptrToInt55467 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49108 to i64
%ae49108 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55467)
store volatile %struct.ScmObj* %ae49108, %struct.ScmObj** %stackaddr$makeclosure55466, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49108, %struct.ScmObj* %_37foldl148030, i64 0)
%argslist55197$ae491060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55468 = alloca %struct.ScmObj*, align 8
%argslist55197$ae491061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49108, %struct.ScmObj* %argslist55197$ae491060)
store volatile %struct.ScmObj* %argslist55197$ae491061, %struct.ScmObj** %stackaddr$prim55468, align 8
%stackaddr$prim55469 = alloca %struct.ScmObj*, align 8
%argslist55197$ae491062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49107, %struct.ScmObj* %argslist55197$ae491061)
store volatile %struct.ScmObj* %argslist55197$ae491062, %struct.ScmObj** %stackaddr$prim55469, align 8
%clofunc55470 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49106)
musttail call tailcc void %clofunc55470(%struct.ScmObj* %ae49106, %struct.ScmObj* %argslist55197$ae491062)
ret void
}

define tailcc void @proc_clo$ae49106(%struct.ScmObj* %env$ae49106,%struct.ScmObj* %current_45args54733) {
%stackaddr$env-ref55471 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49106, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55471
%stackaddr$env-ref55472 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49106, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55472
%stackaddr$env-ref55473 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49106, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55473
%stackaddr$env-ref55474 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49106, i64 3)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref55474
%stackaddr$env-ref55475 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49106, i64 4)
store %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$env-ref55475
%stackaddr$env-ref55476 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49106, i64 5)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55476
%stackaddr$prim55477 = alloca %struct.ScmObj*, align 8
%_95k48296 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54733)
store volatile %struct.ScmObj* %_95k48296, %struct.ScmObj** %stackaddr$prim55477, align 8
%stackaddr$prim55478 = alloca %struct.ScmObj*, align 8
%current_45args54734 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54733)
store volatile %struct.ScmObj* %current_45args54734, %struct.ScmObj** %stackaddr$prim55478, align 8
%stackaddr$prim55479 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54734)
store volatile %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$prim55479, align 8
%stackaddr$makeclosure55480 = alloca %struct.ScmObj*, align 8
%fptrToInt55481 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49160 to i64
%ae49160 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55481)
store volatile %struct.ScmObj* %ae49160, %struct.ScmObj** %stackaddr$makeclosure55480, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49160, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49160, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49160, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49160, %struct.ScmObj* %_37last48068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49160, %struct.ScmObj* %_37map148042, i64 4)
%ae49161 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55482 = alloca %struct.ScmObj*, align 8
%fptrToInt55483 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49162 to i64
%ae49162 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55483)
store volatile %struct.ScmObj* %ae49162, %struct.ScmObj** %stackaddr$makeclosure55482, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49162, %struct.ScmObj* %_37take48038, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49162, %struct.ScmObj* %_37length48035, i64 1)
%argslist55183$ae491600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55484 = alloca %struct.ScmObj*, align 8
%argslist55183$ae491601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49162, %struct.ScmObj* %argslist55183$ae491600)
store volatile %struct.ScmObj* %argslist55183$ae491601, %struct.ScmObj** %stackaddr$prim55484, align 8
%stackaddr$prim55485 = alloca %struct.ScmObj*, align 8
%argslist55183$ae491602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49161, %struct.ScmObj* %argslist55183$ae491601)
store volatile %struct.ScmObj* %argslist55183$ae491602, %struct.ScmObj** %stackaddr$prim55485, align 8
%clofunc55486 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49160)
musttail call tailcc void %clofunc55486(%struct.ScmObj* %ae49160, %struct.ScmObj* %argslist55183$ae491602)
ret void
}

define tailcc void @proc_clo$ae49160(%struct.ScmObj* %env$ae49160,%struct.ScmObj* %current_45args54736) {
%stackaddr$env-ref55487 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49160, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55487
%stackaddr$env-ref55488 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49160, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55488
%stackaddr$env-ref55489 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49160, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55489
%stackaddr$env-ref55490 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49160, i64 3)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref55490
%stackaddr$env-ref55491 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49160, i64 4)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref55491
%stackaddr$prim55492 = alloca %struct.ScmObj*, align 8
%_95k48297 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54736)
store volatile %struct.ScmObj* %_95k48297, %struct.ScmObj** %stackaddr$prim55492, align 8
%stackaddr$prim55493 = alloca %struct.ScmObj*, align 8
%current_45args54737 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54736)
store volatile %struct.ScmObj* %current_45args54737, %struct.ScmObj** %stackaddr$prim55493, align 8
%stackaddr$prim55494 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54737)
store volatile %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$prim55494, align 8
%stackaddr$makeclosure55495 = alloca %struct.ScmObj*, align 8
%fptrToInt55496 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49190 to i64
%ae49190 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55496)
store volatile %struct.ScmObj* %ae49190, %struct.ScmObj** %stackaddr$makeclosure55495, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49190, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49190, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49190, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49190, %struct.ScmObj* %_37last48068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49190, %struct.ScmObj* %_37drop_45right48065, i64 4)
%ae49191 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55497 = alloca %struct.ScmObj*, align 8
%fptrToInt55498 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49192 to i64
%ae49192 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55498)
store volatile %struct.ScmObj* %ae49192, %struct.ScmObj** %stackaddr$makeclosure55497, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49192, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49192, %struct.ScmObj* %_37map148042, i64 1)
%argslist55173$ae491900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55499 = alloca %struct.ScmObj*, align 8
%argslist55173$ae491901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49192, %struct.ScmObj* %argslist55173$ae491900)
store volatile %struct.ScmObj* %argslist55173$ae491901, %struct.ScmObj** %stackaddr$prim55499, align 8
%stackaddr$prim55500 = alloca %struct.ScmObj*, align 8
%argslist55173$ae491902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49191, %struct.ScmObj* %argslist55173$ae491901)
store volatile %struct.ScmObj* %argslist55173$ae491902, %struct.ScmObj** %stackaddr$prim55500, align 8
%clofunc55501 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49190)
musttail call tailcc void %clofunc55501(%struct.ScmObj* %ae49190, %struct.ScmObj* %argslist55173$ae491902)
ret void
}

define tailcc void @proc_clo$ae49190(%struct.ScmObj* %env$ae49190,%struct.ScmObj* %current_45args54739) {
%stackaddr$env-ref55502 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49190, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55502
%stackaddr$env-ref55503 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49190, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55503
%stackaddr$env-ref55504 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49190, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55504
%stackaddr$env-ref55505 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49190, i64 3)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref55505
%stackaddr$env-ref55506 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49190, i64 4)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref55506
%stackaddr$prim55507 = alloca %struct.ScmObj*, align 8
%_95k48298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54739)
store volatile %struct.ScmObj* %_95k48298, %struct.ScmObj** %stackaddr$prim55507, align 8
%stackaddr$prim55508 = alloca %struct.ScmObj*, align 8
%current_45args54740 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54739)
store volatile %struct.ScmObj* %current_45args54740, %struct.ScmObj** %stackaddr$prim55508, align 8
%stackaddr$prim55509 = alloca %struct.ScmObj*, align 8
%anf_45bind48200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54740)
store volatile %struct.ScmObj* %anf_45bind48200, %struct.ScmObj** %stackaddr$prim55509, align 8
%stackaddr$makeclosure55510 = alloca %struct.ScmObj*, align 8
%fptrToInt55511 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49574 to i64
%ae49574 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55511)
store volatile %struct.ScmObj* %ae49574, %struct.ScmObj** %stackaddr$makeclosure55510, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49574, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49574, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49574, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49574, %struct.ScmObj* %_37last48068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49574, %struct.ScmObj* %_37drop_45right48065, i64 4)
%argslist55113$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55512 = alloca %struct.ScmObj*, align 8
%argslist55113$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48200, %struct.ScmObj* %argslist55113$Ycmb480250)
store volatile %struct.ScmObj* %argslist55113$Ycmb480251, %struct.ScmObj** %stackaddr$prim55512, align 8
%stackaddr$prim55513 = alloca %struct.ScmObj*, align 8
%argslist55113$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49574, %struct.ScmObj* %argslist55113$Ycmb480251)
store volatile %struct.ScmObj* %argslist55113$Ycmb480252, %struct.ScmObj** %stackaddr$prim55513, align 8
%clofunc55514 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc55514(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist55113$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae49574(%struct.ScmObj* %env$ae49574,%struct.ScmObj* %current_45args54742) {
%stackaddr$env-ref55515 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49574, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55515
%stackaddr$env-ref55516 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49574, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55516
%stackaddr$env-ref55517 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49574, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55517
%stackaddr$env-ref55518 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49574, i64 3)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref55518
%stackaddr$env-ref55519 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49574, i64 4)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref55519
%stackaddr$prim55520 = alloca %struct.ScmObj*, align 8
%_95k48299 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54742)
store volatile %struct.ScmObj* %_95k48299, %struct.ScmObj** %stackaddr$prim55520, align 8
%stackaddr$prim55521 = alloca %struct.ScmObj*, align 8
%current_45args54743 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54742)
store volatile %struct.ScmObj* %current_45args54743, %struct.ScmObj** %stackaddr$prim55521, align 8
%stackaddr$prim55522 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54743)
store volatile %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$prim55522, align 8
%stackaddr$makeclosure55523 = alloca %struct.ScmObj*, align 8
%fptrToInt55524 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49576 to i64
%ae49576 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55524)
store volatile %struct.ScmObj* %ae49576, %struct.ScmObj** %stackaddr$makeclosure55523, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49576, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49576, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49576, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49576, %struct.ScmObj* %_37last48068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49576, %struct.ScmObj* %_37foldr48051, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49576, %struct.ScmObj* %_37drop_45right48065, i64 5)
%ae49577 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55525 = alloca %struct.ScmObj*, align 8
%fptrToInt55526 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49578 to i64
%ae49578 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55526)
store volatile %struct.ScmObj* %ae49578, %struct.ScmObj** %stackaddr$makeclosure55525, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49578, %struct.ScmObj* %_37foldr148046, i64 0)
%argslist55112$ae495760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55527 = alloca %struct.ScmObj*, align 8
%argslist55112$ae495761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49578, %struct.ScmObj* %argslist55112$ae495760)
store volatile %struct.ScmObj* %argslist55112$ae495761, %struct.ScmObj** %stackaddr$prim55527, align 8
%stackaddr$prim55528 = alloca %struct.ScmObj*, align 8
%argslist55112$ae495762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49577, %struct.ScmObj* %argslist55112$ae495761)
store volatile %struct.ScmObj* %argslist55112$ae495762, %struct.ScmObj** %stackaddr$prim55528, align 8
%clofunc55529 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49576)
musttail call tailcc void %clofunc55529(%struct.ScmObj* %ae49576, %struct.ScmObj* %argslist55112$ae495762)
ret void
}

define tailcc void @proc_clo$ae49576(%struct.ScmObj* %env$ae49576,%struct.ScmObj* %current_45args54745) {
%stackaddr$env-ref55530 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49576, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55530
%stackaddr$env-ref55531 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49576, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55531
%stackaddr$env-ref55532 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49576, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55532
%stackaddr$env-ref55533 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49576, i64 3)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref55533
%stackaddr$env-ref55534 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49576, i64 4)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55534
%stackaddr$env-ref55535 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49576, i64 5)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref55535
%stackaddr$prim55536 = alloca %struct.ScmObj*, align 8
%_95k48300 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54745)
store volatile %struct.ScmObj* %_95k48300, %struct.ScmObj** %stackaddr$prim55536, align 8
%stackaddr$prim55537 = alloca %struct.ScmObj*, align 8
%current_45args54746 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54745)
store volatile %struct.ScmObj* %current_45args54746, %struct.ScmObj** %stackaddr$prim55537, align 8
%stackaddr$prim55538 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54746)
store volatile %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$prim55538, align 8
%stackaddr$makeclosure55539 = alloca %struct.ScmObj*, align 8
%fptrToInt55540 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49653 to i64
%ae49653 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55540)
store volatile %struct.ScmObj* %ae49653, %struct.ScmObj** %stackaddr$makeclosure55539, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49653, %struct.ScmObj* %_37foldr148046, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49653, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49653, %struct.ScmObj* %Ycmb48025, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49653, %struct.ScmObj* %_37foldr48051, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49653, %struct.ScmObj* %_37map148077, i64 4)
%ae49654 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55541 = alloca %struct.ScmObj*, align 8
%fptrToInt55542 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49655 to i64
%ae49655 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55542)
store volatile %struct.ScmObj* %ae49655, %struct.ScmObj** %stackaddr$makeclosure55541, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49655, %struct.ScmObj* %_37last48068, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49655, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49655, %struct.ScmObj* %_37drop_45right48065, i64 2)
%argslist55093$ae496530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55543 = alloca %struct.ScmObj*, align 8
%argslist55093$ae496531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49655, %struct.ScmObj* %argslist55093$ae496530)
store volatile %struct.ScmObj* %argslist55093$ae496531, %struct.ScmObj** %stackaddr$prim55543, align 8
%stackaddr$prim55544 = alloca %struct.ScmObj*, align 8
%argslist55093$ae496532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49654, %struct.ScmObj* %argslist55093$ae496531)
store volatile %struct.ScmObj* %argslist55093$ae496532, %struct.ScmObj** %stackaddr$prim55544, align 8
%clofunc55545 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49653)
musttail call tailcc void %clofunc55545(%struct.ScmObj* %ae49653, %struct.ScmObj* %argslist55093$ae496532)
ret void
}

define tailcc void @proc_clo$ae49653(%struct.ScmObj* %env$ae49653,%struct.ScmObj* %current_45args54748) {
%stackaddr$env-ref55546 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49653, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref55546
%stackaddr$env-ref55547 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49653, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55547
%stackaddr$env-ref55548 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49653, i64 2)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55548
%stackaddr$env-ref55549 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49653, i64 3)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref55549
%stackaddr$env-ref55550 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49653, i64 4)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55550
%stackaddr$prim55551 = alloca %struct.ScmObj*, align 8
%_95k48301 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54748)
store volatile %struct.ScmObj* %_95k48301, %struct.ScmObj** %stackaddr$prim55551, align 8
%stackaddr$prim55552 = alloca %struct.ScmObj*, align 8
%current_45args54749 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54748)
store volatile %struct.ScmObj* %current_45args54749, %struct.ScmObj** %stackaddr$prim55552, align 8
%stackaddr$prim55553 = alloca %struct.ScmObj*, align 8
%_37map48072 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54749)
store volatile %struct.ScmObj* %_37map48072, %struct.ScmObj** %stackaddr$prim55553, align 8
%stackaddr$makeclosure55554 = alloca %struct.ScmObj*, align 8
%fptrToInt55555 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49799 to i64
%ae49799 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55555)
store volatile %struct.ScmObj* %ae49799, %struct.ScmObj** %stackaddr$makeclosure55554, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49799, %struct.ScmObj* %Ycmb48025, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49799, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49799, %struct.ScmObj* %_37map148077, i64 2)
%ae49800 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55556 = alloca %struct.ScmObj*, align 8
%fptrToInt55557 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49801 to i64
%ae49801 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55557)
store volatile %struct.ScmObj* %ae49801, %struct.ScmObj** %stackaddr$makeclosure55556, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49801, %struct.ScmObj* %_37foldr48051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49801, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49801, %struct.ScmObj* %_37map148077, i64 2)
%argslist55076$ae497990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55558 = alloca %struct.ScmObj*, align 8
%argslist55076$ae497991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49801, %struct.ScmObj* %argslist55076$ae497990)
store volatile %struct.ScmObj* %argslist55076$ae497991, %struct.ScmObj** %stackaddr$prim55558, align 8
%stackaddr$prim55559 = alloca %struct.ScmObj*, align 8
%argslist55076$ae497992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49800, %struct.ScmObj* %argslist55076$ae497991)
store volatile %struct.ScmObj* %argslist55076$ae497992, %struct.ScmObj** %stackaddr$prim55559, align 8
%clofunc55560 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49799)
musttail call tailcc void %clofunc55560(%struct.ScmObj* %ae49799, %struct.ScmObj* %argslist55076$ae497992)
ret void
}

define tailcc void @proc_clo$ae49799(%struct.ScmObj* %env$ae49799,%struct.ScmObj* %current_45args54751) {
%stackaddr$env-ref55561 = alloca %struct.ScmObj*, align 8
%Ycmb48025 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49799, i64 0)
store %struct.ScmObj* %Ycmb48025, %struct.ScmObj** %stackaddr$env-ref55561
%stackaddr$env-ref55562 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49799, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55562
%stackaddr$env-ref55563 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49799, i64 2)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55563
%stackaddr$prim55564 = alloca %struct.ScmObj*, align 8
%_95k48302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54751)
store volatile %struct.ScmObj* %_95k48302, %struct.ScmObj** %stackaddr$prim55564, align 8
%stackaddr$prim55565 = alloca %struct.ScmObj*, align 8
%current_45args54752 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54751)
store volatile %struct.ScmObj* %current_45args54752, %struct.ScmObj** %stackaddr$prim55565, align 8
%stackaddr$prim55566 = alloca %struct.ScmObj*, align 8
%anf_45bind48220 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54752)
store volatile %struct.ScmObj* %anf_45bind48220, %struct.ScmObj** %stackaddr$prim55566, align 8
%stackaddr$makeclosure55567 = alloca %struct.ScmObj*, align 8
%fptrToInt55568 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50191 to i64
%ae50191 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55568)
store volatile %struct.ScmObj* %ae50191, %struct.ScmObj** %stackaddr$makeclosure55567, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50191, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50191, %struct.ScmObj* %_37map148077, i64 1)
%argslist55016$Ycmb480250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55569 = alloca %struct.ScmObj*, align 8
%argslist55016$Ycmb480251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48220, %struct.ScmObj* %argslist55016$Ycmb480250)
store volatile %struct.ScmObj* %argslist55016$Ycmb480251, %struct.ScmObj** %stackaddr$prim55569, align 8
%stackaddr$prim55570 = alloca %struct.ScmObj*, align 8
%argslist55016$Ycmb480252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50191, %struct.ScmObj* %argslist55016$Ycmb480251)
store volatile %struct.ScmObj* %argslist55016$Ycmb480252, %struct.ScmObj** %stackaddr$prim55570, align 8
%clofunc55571 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48025)
musttail call tailcc void %clofunc55571(%struct.ScmObj* %Ycmb48025, %struct.ScmObj* %argslist55016$Ycmb480252)
ret void
}

define tailcc void @proc_clo$ae50191(%struct.ScmObj* %env$ae50191,%struct.ScmObj* %current_45args54754) {
%stackaddr$env-ref55572 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50191, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55572
%stackaddr$env-ref55573 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50191, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55573
%stackaddr$prim55574 = alloca %struct.ScmObj*, align 8
%_95k48303 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54754)
store volatile %struct.ScmObj* %_95k48303, %struct.ScmObj** %stackaddr$prim55574, align 8
%stackaddr$prim55575 = alloca %struct.ScmObj*, align 8
%current_45args54755 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54754)
store volatile %struct.ScmObj* %current_45args54755, %struct.ScmObj** %stackaddr$prim55575, align 8
%stackaddr$prim55576 = alloca %struct.ScmObj*, align 8
%_37foldl48128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54755)
store volatile %struct.ScmObj* %_37foldl48128, %struct.ScmObj** %stackaddr$prim55576, align 8
%stackaddr$makeclosure55577 = alloca %struct.ScmObj*, align 8
%fptrToInt55578 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50193 to i64
%ae50193 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55578)
store volatile %struct.ScmObj* %ae50193, %struct.ScmObj** %stackaddr$makeclosure55577, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50193, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50193, %struct.ScmObj* %_37map148077, i64 1)
%ae50194 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55579 = alloca %struct.ScmObj*, align 8
%fptrToInt55580 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50195 to i64
%ae50195 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55580)
store volatile %struct.ScmObj* %ae50195, %struct.ScmObj** %stackaddr$makeclosure55579, align 8
%argslist55015$ae501930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55581 = alloca %struct.ScmObj*, align 8
%argslist55015$ae501931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50195, %struct.ScmObj* %argslist55015$ae501930)
store volatile %struct.ScmObj* %argslist55015$ae501931, %struct.ScmObj** %stackaddr$prim55581, align 8
%stackaddr$prim55582 = alloca %struct.ScmObj*, align 8
%argslist55015$ae501932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50194, %struct.ScmObj* %argslist55015$ae501931)
store volatile %struct.ScmObj* %argslist55015$ae501932, %struct.ScmObj** %stackaddr$prim55582, align 8
%clofunc55583 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50193)
musttail call tailcc void %clofunc55583(%struct.ScmObj* %ae50193, %struct.ScmObj* %argslist55015$ae501932)
ret void
}

define tailcc void @proc_clo$ae50193(%struct.ScmObj* %env$ae50193,%struct.ScmObj* %current_45args54757) {
%stackaddr$env-ref55584 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50193, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55584
%stackaddr$env-ref55585 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50193, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55585
%stackaddr$prim55586 = alloca %struct.ScmObj*, align 8
%_95k48304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54757)
store volatile %struct.ScmObj* %_95k48304, %struct.ScmObj** %stackaddr$prim55586, align 8
%stackaddr$prim55587 = alloca %struct.ScmObj*, align 8
%current_45args54758 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54757)
store volatile %struct.ScmObj* %current_45args54758, %struct.ScmObj** %stackaddr$prim55587, align 8
%stackaddr$prim55588 = alloca %struct.ScmObj*, align 8
%_37_6248125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54758)
store volatile %struct.ScmObj* %_37_6248125, %struct.ScmObj** %stackaddr$prim55588, align 8
%stackaddr$makeclosure55589 = alloca %struct.ScmObj*, align 8
%fptrToInt55590 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50217 to i64
%ae50217 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55590)
store volatile %struct.ScmObj* %ae50217, %struct.ScmObj** %stackaddr$makeclosure55589, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50217, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50217, %struct.ScmObj* %_37map148077, i64 1)
%ae50218 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55591 = alloca %struct.ScmObj*, align 8
%fptrToInt55592 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50219 to i64
%ae50219 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55592)
store volatile %struct.ScmObj* %ae50219, %struct.ScmObj** %stackaddr$makeclosure55591, align 8
%argslist55009$ae502170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55593 = alloca %struct.ScmObj*, align 8
%argslist55009$ae502171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50219, %struct.ScmObj* %argslist55009$ae502170)
store volatile %struct.ScmObj* %argslist55009$ae502171, %struct.ScmObj** %stackaddr$prim55593, align 8
%stackaddr$prim55594 = alloca %struct.ScmObj*, align 8
%argslist55009$ae502172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50218, %struct.ScmObj* %argslist55009$ae502171)
store volatile %struct.ScmObj* %argslist55009$ae502172, %struct.ScmObj** %stackaddr$prim55594, align 8
%clofunc55595 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50217)
musttail call tailcc void %clofunc55595(%struct.ScmObj* %ae50217, %struct.ScmObj* %argslist55009$ae502172)
ret void
}

define tailcc void @proc_clo$ae50217(%struct.ScmObj* %env$ae50217,%struct.ScmObj* %current_45args54760) {
%stackaddr$env-ref55596 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50217, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55596
%stackaddr$env-ref55597 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50217, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55597
%stackaddr$prim55598 = alloca %struct.ScmObj*, align 8
%_95k48305 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54760)
store volatile %struct.ScmObj* %_95k48305, %struct.ScmObj** %stackaddr$prim55598, align 8
%stackaddr$prim55599 = alloca %struct.ScmObj*, align 8
%current_45args54761 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54760)
store volatile %struct.ScmObj* %current_45args54761, %struct.ScmObj** %stackaddr$prim55599, align 8
%stackaddr$prim55600 = alloca %struct.ScmObj*, align 8
%_37_62_6148122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54761)
store volatile %struct.ScmObj* %_37_62_6148122, %struct.ScmObj** %stackaddr$prim55600, align 8
%ae50241 = call %struct.ScmObj* @const_init_int(i64 1)
%ae50242 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55601 = alloca %struct.ScmObj*, align 8
%_37append48118 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50241, %struct.ScmObj* %ae50242)
store volatile %struct.ScmObj* %_37append48118, %struct.ScmObj** %stackaddr$prim55601, align 8
%stackaddr$makeclosure55602 = alloca %struct.ScmObj*, align 8
%fptrToInt55603 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50243 to i64
%ae50243 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55603)
store volatile %struct.ScmObj* %ae50243, %struct.ScmObj** %stackaddr$makeclosure55602, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50243, %struct.ScmObj* %_37append48118, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50243, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50243, %struct.ScmObj* %_37map148077, i64 2)
%ae50244 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55604 = alloca %struct.ScmObj*, align 8
%fptrToInt55605 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50245 to i64
%ae50245 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55605)
store volatile %struct.ScmObj* %ae50245, %struct.ScmObj** %stackaddr$makeclosure55604, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50245, %struct.ScmObj* %_37append48118, i64 0)
%argslist55003$ae502430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55606 = alloca %struct.ScmObj*, align 8
%argslist55003$ae502431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50245, %struct.ScmObj* %argslist55003$ae502430)
store volatile %struct.ScmObj* %argslist55003$ae502431, %struct.ScmObj** %stackaddr$prim55606, align 8
%stackaddr$prim55607 = alloca %struct.ScmObj*, align 8
%argslist55003$ae502432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50244, %struct.ScmObj* %argslist55003$ae502431)
store volatile %struct.ScmObj* %argslist55003$ae502432, %struct.ScmObj** %stackaddr$prim55607, align 8
%clofunc55608 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50243)
musttail call tailcc void %clofunc55608(%struct.ScmObj* %ae50243, %struct.ScmObj* %argslist55003$ae502432)
ret void
}

define tailcc void @proc_clo$ae50243(%struct.ScmObj* %env$ae50243,%struct.ScmObj* %current_45args54763) {
%stackaddr$env-ref55609 = alloca %struct.ScmObj*, align 8
%_37append48118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50243, i64 0)
store %struct.ScmObj* %_37append48118, %struct.ScmObj** %stackaddr$env-ref55609
%stackaddr$env-ref55610 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50243, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55610
%stackaddr$env-ref55611 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50243, i64 2)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55611
%stackaddr$prim55612 = alloca %struct.ScmObj*, align 8
%_95k48306 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54763)
store volatile %struct.ScmObj* %_95k48306, %struct.ScmObj** %stackaddr$prim55612, align 8
%stackaddr$prim55613 = alloca %struct.ScmObj*, align 8
%current_45args54764 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54763)
store volatile %struct.ScmObj* %current_45args54764, %struct.ScmObj** %stackaddr$prim55613, align 8
%stackaddr$prim55614 = alloca %struct.ScmObj*, align 8
%anf_45bind48228 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54764)
store volatile %struct.ScmObj* %anf_45bind48228, %struct.ScmObj** %stackaddr$prim55614, align 8
%ae50311 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55615 = alloca %struct.ScmObj*, align 8
%_95048119 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append48118, %struct.ScmObj* %ae50311, %struct.ScmObj* %anf_45bind48228)
store volatile %struct.ScmObj* %_95048119, %struct.ScmObj** %stackaddr$prim55615, align 8
%ae50314 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55616 = alloca %struct.ScmObj*, align 8
%_37append48117 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48118, %struct.ScmObj* %ae50314)
store volatile %struct.ScmObj* %_37append48117, %struct.ScmObj** %stackaddr$prim55616, align 8
%stackaddr$makeclosure55617 = alloca %struct.ScmObj*, align 8
%fptrToInt55618 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50315 to i64
%ae50315 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55618)
store volatile %struct.ScmObj* %ae50315, %struct.ScmObj** %stackaddr$makeclosure55617, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50315, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50315, %struct.ScmObj* %_37map148077, i64 1)
%ae50316 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55619 = alloca %struct.ScmObj*, align 8
%fptrToInt55620 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50317 to i64
%ae50317 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55620)
store volatile %struct.ScmObj* %ae50317, %struct.ScmObj** %stackaddr$makeclosure55619, align 8
%argslist54992$ae503150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55621 = alloca %struct.ScmObj*, align 8
%argslist54992$ae503151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50317, %struct.ScmObj* %argslist54992$ae503150)
store volatile %struct.ScmObj* %argslist54992$ae503151, %struct.ScmObj** %stackaddr$prim55621, align 8
%stackaddr$prim55622 = alloca %struct.ScmObj*, align 8
%argslist54992$ae503152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50316, %struct.ScmObj* %argslist54992$ae503151)
store volatile %struct.ScmObj* %argslist54992$ae503152, %struct.ScmObj** %stackaddr$prim55622, align 8
%clofunc55623 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50315)
musttail call tailcc void %clofunc55623(%struct.ScmObj* %ae50315, %struct.ScmObj* %argslist54992$ae503152)
ret void
}

define tailcc void @proc_clo$ae50315(%struct.ScmObj* %env$ae50315,%struct.ScmObj* %current_45args54766) {
%stackaddr$env-ref55624 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50315, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55624
%stackaddr$env-ref55625 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50315, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55625
%stackaddr$prim55626 = alloca %struct.ScmObj*, align 8
%_95k48307 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54766)
store volatile %struct.ScmObj* %_95k48307, %struct.ScmObj** %stackaddr$prim55626, align 8
%stackaddr$prim55627 = alloca %struct.ScmObj*, align 8
%current_45args54767 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54766)
store volatile %struct.ScmObj* %current_45args54767, %struct.ScmObj** %stackaddr$prim55627, align 8
%stackaddr$prim55628 = alloca %struct.ScmObj*, align 8
%_37list_6348110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54767)
store volatile %struct.ScmObj* %_37list_6348110, %struct.ScmObj** %stackaddr$prim55628, align 8
%stackaddr$makeclosure55629 = alloca %struct.ScmObj*, align 8
%fptrToInt55630 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50731 to i64
%ae50731 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55630)
store volatile %struct.ScmObj* %ae50731, %struct.ScmObj** %stackaddr$makeclosure55629, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50731, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50731, %struct.ScmObj* %_37map148077, i64 1)
%ae50732 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55631 = alloca %struct.ScmObj*, align 8
%fptrToInt55632 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50733 to i64
%ae50733 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55632)
store volatile %struct.ScmObj* %ae50733, %struct.ScmObj** %stackaddr$makeclosure55631, align 8
%argslist54967$ae507310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55633 = alloca %struct.ScmObj*, align 8
%argslist54967$ae507311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50733, %struct.ScmObj* %argslist54967$ae507310)
store volatile %struct.ScmObj* %argslist54967$ae507311, %struct.ScmObj** %stackaddr$prim55633, align 8
%stackaddr$prim55634 = alloca %struct.ScmObj*, align 8
%argslist54967$ae507312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50732, %struct.ScmObj* %argslist54967$ae507311)
store volatile %struct.ScmObj* %argslist54967$ae507312, %struct.ScmObj** %stackaddr$prim55634, align 8
%clofunc55635 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50731)
musttail call tailcc void %clofunc55635(%struct.ScmObj* %ae50731, %struct.ScmObj* %argslist54967$ae507312)
ret void
}

define tailcc void @proc_clo$ae50731(%struct.ScmObj* %env$ae50731,%struct.ScmObj* %current_45args54769) {
%stackaddr$env-ref55636 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50731, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55636
%stackaddr$env-ref55637 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50731, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55637
%stackaddr$prim55638 = alloca %struct.ScmObj*, align 8
%_95k48308 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54769)
store volatile %struct.ScmObj* %_95k48308, %struct.ScmObj** %stackaddr$prim55638, align 8
%stackaddr$prim55639 = alloca %struct.ScmObj*, align 8
%current_45args54770 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54769)
store volatile %struct.ScmObj* %current_45args54770, %struct.ScmObj** %stackaddr$prim55639, align 8
%stackaddr$prim55640 = alloca %struct.ScmObj*, align 8
%_37drop48101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54770)
store volatile %struct.ScmObj* %_37drop48101, %struct.ScmObj** %stackaddr$prim55640, align 8
%stackaddr$makeclosure55641 = alloca %struct.ScmObj*, align 8
%fptrToInt55642 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51267 to i64
%ae51267 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55642)
store volatile %struct.ScmObj* %ae51267, %struct.ScmObj** %stackaddr$makeclosure55641, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51267, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51267, %struct.ScmObj* %_37map148077, i64 1)
%ae51268 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55643 = alloca %struct.ScmObj*, align 8
%fptrToInt55644 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51269 to i64
%ae51269 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55644)
store volatile %struct.ScmObj* %ae51269, %struct.ScmObj** %stackaddr$makeclosure55643, align 8
%argslist54943$ae512670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55645 = alloca %struct.ScmObj*, align 8
%argslist54943$ae512671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51269, %struct.ScmObj* %argslist54943$ae512670)
store volatile %struct.ScmObj* %argslist54943$ae512671, %struct.ScmObj** %stackaddr$prim55645, align 8
%stackaddr$prim55646 = alloca %struct.ScmObj*, align 8
%argslist54943$ae512672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51268, %struct.ScmObj* %argslist54943$ae512671)
store volatile %struct.ScmObj* %argslist54943$ae512672, %struct.ScmObj** %stackaddr$prim55646, align 8
%clofunc55647 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51267)
musttail call tailcc void %clofunc55647(%struct.ScmObj* %ae51267, %struct.ScmObj* %argslist54943$ae512672)
ret void
}

define tailcc void @proc_clo$ae51267(%struct.ScmObj* %env$ae51267,%struct.ScmObj* %current_45args54772) {
%stackaddr$env-ref55648 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51267, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55648
%stackaddr$env-ref55649 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51267, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55649
%stackaddr$prim55650 = alloca %struct.ScmObj*, align 8
%_95k48309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54772)
store volatile %struct.ScmObj* %_95k48309, %struct.ScmObj** %stackaddr$prim55650, align 8
%stackaddr$prim55651 = alloca %struct.ScmObj*, align 8
%current_45args54773 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54772)
store volatile %struct.ScmObj* %current_45args54773, %struct.ScmObj** %stackaddr$prim55651, align 8
%stackaddr$prim55652 = alloca %struct.ScmObj*, align 8
%_37memv48094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54773)
store volatile %struct.ScmObj* %_37memv48094, %struct.ScmObj** %stackaddr$prim55652, align 8
%stackaddr$makeclosure55653 = alloca %struct.ScmObj*, align 8
%fptrToInt55654 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51671 to i64
%ae51671 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55654)
store volatile %struct.ScmObj* %ae51671, %struct.ScmObj** %stackaddr$makeclosure55653, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51671, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51671, %struct.ScmObj* %_37map148077, i64 1)
%ae51672 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55655 = alloca %struct.ScmObj*, align 8
%fptrToInt55656 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51673 to i64
%ae51673 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55656)
store volatile %struct.ScmObj* %ae51673, %struct.ScmObj** %stackaddr$makeclosure55655, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51673, %struct.ScmObj* %_37foldl148030, i64 0)
%argslist54917$ae516710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55657 = alloca %struct.ScmObj*, align 8
%argslist54917$ae516711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51673, %struct.ScmObj* %argslist54917$ae516710)
store volatile %struct.ScmObj* %argslist54917$ae516711, %struct.ScmObj** %stackaddr$prim55657, align 8
%stackaddr$prim55658 = alloca %struct.ScmObj*, align 8
%argslist54917$ae516712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51672, %struct.ScmObj* %argslist54917$ae516711)
store volatile %struct.ScmObj* %argslist54917$ae516712, %struct.ScmObj** %stackaddr$prim55658, align 8
%clofunc55659 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51671)
musttail call tailcc void %clofunc55659(%struct.ScmObj* %ae51671, %struct.ScmObj* %argslist54917$ae516712)
ret void
}

define tailcc void @proc_clo$ae51671(%struct.ScmObj* %env$ae51671,%struct.ScmObj* %current_45args54775) {
%stackaddr$env-ref55660 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51671, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55660
%stackaddr$env-ref55661 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51671, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55661
%stackaddr$prim55662 = alloca %struct.ScmObj*, align 8
%_95k48310 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54775)
store volatile %struct.ScmObj* %_95k48310, %struct.ScmObj** %stackaddr$prim55662, align 8
%stackaddr$prim55663 = alloca %struct.ScmObj*, align 8
%current_45args54776 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54775)
store volatile %struct.ScmObj* %current_45args54776, %struct.ScmObj** %stackaddr$prim55663, align 8
%stackaddr$prim55664 = alloca %struct.ScmObj*, align 8
%_37_4748090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54776)
store volatile %struct.ScmObj* %_37_4748090, %struct.ScmObj** %stackaddr$prim55664, align 8
%stackaddr$makeclosure55665 = alloca %struct.ScmObj*, align 8
%fptrToInt55666 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51769 to i64
%ae51769 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55666)
store volatile %struct.ScmObj* %ae51769, %struct.ScmObj** %stackaddr$makeclosure55665, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51769, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51769, %struct.ScmObj* %_37map148077, i64 1)
%ae51770 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55667 = alloca %struct.ScmObj*, align 8
%fptrToInt55668 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51771 to i64
%ae51771 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55668)
store volatile %struct.ScmObj* %ae51771, %struct.ScmObj** %stackaddr$makeclosure55667, align 8
%argslist54904$ae517690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55669 = alloca %struct.ScmObj*, align 8
%argslist54904$ae517691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51771, %struct.ScmObj* %argslist54904$ae517690)
store volatile %struct.ScmObj* %argslist54904$ae517691, %struct.ScmObj** %stackaddr$prim55669, align 8
%stackaddr$prim55670 = alloca %struct.ScmObj*, align 8
%argslist54904$ae517692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51770, %struct.ScmObj* %argslist54904$ae517691)
store volatile %struct.ScmObj* %argslist54904$ae517692, %struct.ScmObj** %stackaddr$prim55670, align 8
%clofunc55671 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51769)
musttail call tailcc void %clofunc55671(%struct.ScmObj* %ae51769, %struct.ScmObj* %argslist54904$ae517692)
ret void
}

define tailcc void @proc_clo$ae51769(%struct.ScmObj* %env$ae51769,%struct.ScmObj* %current_45args54778) {
%stackaddr$env-ref55672 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51769, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55672
%stackaddr$env-ref55673 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51769, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55673
%stackaddr$prim55674 = alloca %struct.ScmObj*, align 8
%_95k48311 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54778)
store volatile %struct.ScmObj* %_95k48311, %struct.ScmObj** %stackaddr$prim55674, align 8
%stackaddr$prim55675 = alloca %struct.ScmObj*, align 8
%current_45args54779 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54778)
store volatile %struct.ScmObj* %current_45args54779, %struct.ScmObj** %stackaddr$prim55675, align 8
%stackaddr$prim55676 = alloca %struct.ScmObj*, align 8
%_37first48088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54779)
store volatile %struct.ScmObj* %_37first48088, %struct.ScmObj** %stackaddr$prim55676, align 8
%stackaddr$makeclosure55677 = alloca %struct.ScmObj*, align 8
%fptrToInt55678 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51789 to i64
%ae51789 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55678)
store volatile %struct.ScmObj* %ae51789, %struct.ScmObj** %stackaddr$makeclosure55677, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51789, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51789, %struct.ScmObj* %_37map148077, i64 1)
%ae51790 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55679 = alloca %struct.ScmObj*, align 8
%fptrToInt55680 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51791 to i64
%ae51791 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55680)
store volatile %struct.ScmObj* %ae51791, %struct.ScmObj** %stackaddr$makeclosure55679, align 8
%argslist54899$ae517890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55681 = alloca %struct.ScmObj*, align 8
%argslist54899$ae517891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51791, %struct.ScmObj* %argslist54899$ae517890)
store volatile %struct.ScmObj* %argslist54899$ae517891, %struct.ScmObj** %stackaddr$prim55681, align 8
%stackaddr$prim55682 = alloca %struct.ScmObj*, align 8
%argslist54899$ae517892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51790, %struct.ScmObj* %argslist54899$ae517891)
store volatile %struct.ScmObj* %argslist54899$ae517892, %struct.ScmObj** %stackaddr$prim55682, align 8
%clofunc55683 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51789)
musttail call tailcc void %clofunc55683(%struct.ScmObj* %ae51789, %struct.ScmObj* %argslist54899$ae517892)
ret void
}

define tailcc void @proc_clo$ae51789(%struct.ScmObj* %env$ae51789,%struct.ScmObj* %current_45args54781) {
%stackaddr$env-ref55684 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51789, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55684
%stackaddr$env-ref55685 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51789, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55685
%stackaddr$prim55686 = alloca %struct.ScmObj*, align 8
%_95k48312 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54781)
store volatile %struct.ScmObj* %_95k48312, %struct.ScmObj** %stackaddr$prim55686, align 8
%stackaddr$prim55687 = alloca %struct.ScmObj*, align 8
%current_45args54782 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54781)
store volatile %struct.ScmObj* %current_45args54782, %struct.ScmObj** %stackaddr$prim55687, align 8
%stackaddr$prim55688 = alloca %struct.ScmObj*, align 8
%_37second48086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54782)
store volatile %struct.ScmObj* %_37second48086, %struct.ScmObj** %stackaddr$prim55688, align 8
%stackaddr$makeclosure55689 = alloca %struct.ScmObj*, align 8
%fptrToInt55690 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51811 to i64
%ae51811 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55690)
store volatile %struct.ScmObj* %ae51811, %struct.ScmObj** %stackaddr$makeclosure55689, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51811, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51811, %struct.ScmObj* %_37map148077, i64 1)
%ae51812 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55691 = alloca %struct.ScmObj*, align 8
%fptrToInt55692 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51813 to i64
%ae51813 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55692)
store volatile %struct.ScmObj* %ae51813, %struct.ScmObj** %stackaddr$makeclosure55691, align 8
%argslist54894$ae518110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55693 = alloca %struct.ScmObj*, align 8
%argslist54894$ae518111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51813, %struct.ScmObj* %argslist54894$ae518110)
store volatile %struct.ScmObj* %argslist54894$ae518111, %struct.ScmObj** %stackaddr$prim55693, align 8
%stackaddr$prim55694 = alloca %struct.ScmObj*, align 8
%argslist54894$ae518112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51812, %struct.ScmObj* %argslist54894$ae518111)
store volatile %struct.ScmObj* %argslist54894$ae518112, %struct.ScmObj** %stackaddr$prim55694, align 8
%clofunc55695 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51811)
musttail call tailcc void %clofunc55695(%struct.ScmObj* %ae51811, %struct.ScmObj* %argslist54894$ae518112)
ret void
}

define tailcc void @proc_clo$ae51811(%struct.ScmObj* %env$ae51811,%struct.ScmObj* %current_45args54784) {
%stackaddr$env-ref55696 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51811, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55696
%stackaddr$env-ref55697 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51811, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55697
%stackaddr$prim55698 = alloca %struct.ScmObj*, align 8
%_95k48313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54784)
store volatile %struct.ScmObj* %_95k48313, %struct.ScmObj** %stackaddr$prim55698, align 8
%stackaddr$prim55699 = alloca %struct.ScmObj*, align 8
%current_45args54785 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54784)
store volatile %struct.ScmObj* %current_45args54785, %struct.ScmObj** %stackaddr$prim55699, align 8
%stackaddr$prim55700 = alloca %struct.ScmObj*, align 8
%_37third48084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54785)
store volatile %struct.ScmObj* %_37third48084, %struct.ScmObj** %stackaddr$prim55700, align 8
%stackaddr$makeclosure55701 = alloca %struct.ScmObj*, align 8
%fptrToInt55702 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51835 to i64
%ae51835 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55702)
store volatile %struct.ScmObj* %ae51835, %struct.ScmObj** %stackaddr$makeclosure55701, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51835, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51835, %struct.ScmObj* %_37map148077, i64 1)
%ae51836 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55703 = alloca %struct.ScmObj*, align 8
%fptrToInt55704 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51837 to i64
%ae51837 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55704)
store volatile %struct.ScmObj* %ae51837, %struct.ScmObj** %stackaddr$makeclosure55703, align 8
%argslist54889$ae518350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55705 = alloca %struct.ScmObj*, align 8
%argslist54889$ae518351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51837, %struct.ScmObj* %argslist54889$ae518350)
store volatile %struct.ScmObj* %argslist54889$ae518351, %struct.ScmObj** %stackaddr$prim55705, align 8
%stackaddr$prim55706 = alloca %struct.ScmObj*, align 8
%argslist54889$ae518352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51836, %struct.ScmObj* %argslist54889$ae518351)
store volatile %struct.ScmObj* %argslist54889$ae518352, %struct.ScmObj** %stackaddr$prim55706, align 8
%clofunc55707 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51835)
musttail call tailcc void %clofunc55707(%struct.ScmObj* %ae51835, %struct.ScmObj* %argslist54889$ae518352)
ret void
}

define tailcc void @proc_clo$ae51835(%struct.ScmObj* %env$ae51835,%struct.ScmObj* %current_45args54787) {
%stackaddr$env-ref55708 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51835, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55708
%stackaddr$env-ref55709 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51835, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55709
%stackaddr$prim55710 = alloca %struct.ScmObj*, align 8
%_95k48314 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54787)
store volatile %struct.ScmObj* %_95k48314, %struct.ScmObj** %stackaddr$prim55710, align 8
%stackaddr$prim55711 = alloca %struct.ScmObj*, align 8
%current_45args54788 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54787)
store volatile %struct.ScmObj* %current_45args54788, %struct.ScmObj** %stackaddr$prim55711, align 8
%stackaddr$prim55712 = alloca %struct.ScmObj*, align 8
%_37fourth48082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54788)
store volatile %struct.ScmObj* %_37fourth48082, %struct.ScmObj** %stackaddr$prim55712, align 8
%stackaddr$makeclosure55713 = alloca %struct.ScmObj*, align 8
%fptrToInt55714 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51861 to i64
%ae51861 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55714)
store volatile %struct.ScmObj* %ae51861, %struct.ScmObj** %stackaddr$makeclosure55713, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51861, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51861, %struct.ScmObj* %_37map148077, i64 1)
%ae51862 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55715 = alloca %struct.ScmObj*, align 8
%fptrToInt55716 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51863 to i64
%ae51863 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55716)
store volatile %struct.ScmObj* %ae51863, %struct.ScmObj** %stackaddr$makeclosure55715, align 8
%argslist54884$ae518610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55717 = alloca %struct.ScmObj*, align 8
%argslist54884$ae518611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51863, %struct.ScmObj* %argslist54884$ae518610)
store volatile %struct.ScmObj* %argslist54884$ae518611, %struct.ScmObj** %stackaddr$prim55717, align 8
%stackaddr$prim55718 = alloca %struct.ScmObj*, align 8
%argslist54884$ae518612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51862, %struct.ScmObj* %argslist54884$ae518611)
store volatile %struct.ScmObj* %argslist54884$ae518612, %struct.ScmObj** %stackaddr$prim55718, align 8
%clofunc55719 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51861)
musttail call tailcc void %clofunc55719(%struct.ScmObj* %ae51861, %struct.ScmObj* %argslist54884$ae518612)
ret void
}

define tailcc void @proc_clo$ae51861(%struct.ScmObj* %env$ae51861,%struct.ScmObj* %current_45args54790) {
%stackaddr$env-ref55720 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51861, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55720
%stackaddr$env-ref55721 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51861, i64 1)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55721
%stackaddr$prim55722 = alloca %struct.ScmObj*, align 8
%_95k48315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54790)
store volatile %struct.ScmObj* %_95k48315, %struct.ScmObj** %stackaddr$prim55722, align 8
%stackaddr$prim55723 = alloca %struct.ScmObj*, align 8
%current_45args54791 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54790)
store volatile %struct.ScmObj* %current_45args54791, %struct.ScmObj** %stackaddr$prim55723, align 8
%stackaddr$prim55724 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54791)
store volatile %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$prim55724, align 8
%stackaddr$makeclosure55725 = alloca %struct.ScmObj*, align 8
%fptrToInt55726 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51885 to i64
%ae51885 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55726)
store volatile %struct.ScmObj* %ae51885, %struct.ScmObj** %stackaddr$makeclosure55725, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51885, %struct.ScmObj* %anf_45bind48264, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51885, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51885, %struct.ScmObj* %_37map148077, i64 2)
%ae51886 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55727 = alloca %struct.ScmObj*, align 8
%fptrToInt55728 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51887 to i64
%ae51887 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55728)
store volatile %struct.ScmObj* %ae51887, %struct.ScmObj** %stackaddr$makeclosure55727, align 8
%argslist54882$ae518850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55729 = alloca %struct.ScmObj*, align 8
%argslist54882$ae518851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51887, %struct.ScmObj* %argslist54882$ae518850)
store volatile %struct.ScmObj* %argslist54882$ae518851, %struct.ScmObj** %stackaddr$prim55729, align 8
%stackaddr$prim55730 = alloca %struct.ScmObj*, align 8
%argslist54882$ae518852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51886, %struct.ScmObj* %argslist54882$ae518851)
store volatile %struct.ScmObj* %argslist54882$ae518852, %struct.ScmObj** %stackaddr$prim55730, align 8
%clofunc55731 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51885)
musttail call tailcc void %clofunc55731(%struct.ScmObj* %ae51885, %struct.ScmObj* %argslist54882$ae518852)
ret void
}

define tailcc void @proc_clo$ae51885(%struct.ScmObj* %env$ae51885,%struct.ScmObj* %current_45args54793) {
%stackaddr$env-ref55732 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51885, i64 0)
store %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$env-ref55732
%stackaddr$env-ref55733 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51885, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55733
%stackaddr$env-ref55734 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51885, i64 2)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55734
%stackaddr$prim55735 = alloca %struct.ScmObj*, align 8
%_95k48316 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54793)
store volatile %struct.ScmObj* %_95k48316, %struct.ScmObj** %stackaddr$prim55735, align 8
%stackaddr$prim55736 = alloca %struct.ScmObj*, align 8
%current_45args54794 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54793)
store volatile %struct.ScmObj* %current_45args54794, %struct.ScmObj** %stackaddr$prim55736, align 8
%stackaddr$prim55737 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54794)
store volatile %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$prim55737, align 8
%stackaddr$makeclosure55738 = alloca %struct.ScmObj*, align 8
%fptrToInt55739 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51920 to i64
%ae51920 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55739)
store volatile %struct.ScmObj* %ae51920, %struct.ScmObj** %stackaddr$makeclosure55738, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51920, %struct.ScmObj* %anf_45bind48265, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51920, %struct.ScmObj* %anf_45bind48264, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51920, %struct.ScmObj* %_37foldl148030, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51920, %struct.ScmObj* %_37map148077, i64 3)
%ae51921 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55740 = alloca %struct.ScmObj*, align 8
%fptrToInt55741 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51922 to i64
%ae51922 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55741)
store volatile %struct.ScmObj* %ae51922, %struct.ScmObj** %stackaddr$makeclosure55740, align 8
%argslist54876$ae519200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55742 = alloca %struct.ScmObj*, align 8
%argslist54876$ae519201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51922, %struct.ScmObj* %argslist54876$ae519200)
store volatile %struct.ScmObj* %argslist54876$ae519201, %struct.ScmObj** %stackaddr$prim55742, align 8
%stackaddr$prim55743 = alloca %struct.ScmObj*, align 8
%argslist54876$ae519202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51921, %struct.ScmObj* %argslist54876$ae519201)
store volatile %struct.ScmObj* %argslist54876$ae519202, %struct.ScmObj** %stackaddr$prim55743, align 8
%clofunc55744 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51920)
musttail call tailcc void %clofunc55744(%struct.ScmObj* %ae51920, %struct.ScmObj* %argslist54876$ae519202)
ret void
}

define tailcc void @proc_clo$ae51920(%struct.ScmObj* %env$ae51920,%struct.ScmObj* %current_45args54796) {
%stackaddr$env-ref55745 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51920, i64 0)
store %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$env-ref55745
%stackaddr$env-ref55746 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51920, i64 1)
store %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$env-ref55746
%stackaddr$env-ref55747 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51920, i64 2)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55747
%stackaddr$env-ref55748 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51920, i64 3)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55748
%stackaddr$prim55749 = alloca %struct.ScmObj*, align 8
%_95k48317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54796)
store volatile %struct.ScmObj* %_95k48317, %struct.ScmObj** %stackaddr$prim55749, align 8
%stackaddr$prim55750 = alloca %struct.ScmObj*, align 8
%current_45args54797 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54796)
store volatile %struct.ScmObj* %current_45args54797, %struct.ScmObj** %stackaddr$prim55750, align 8
%stackaddr$prim55751 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54797)
store volatile %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$prim55751, align 8
%stackaddr$makeclosure55752 = alloca %struct.ScmObj*, align 8
%fptrToInt55753 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51944 to i64
%ae51944 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55753)
store volatile %struct.ScmObj* %ae51944, %struct.ScmObj** %stackaddr$makeclosure55752, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51944, %struct.ScmObj* %anf_45bind48265, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51944, %struct.ScmObj* %anf_45bind48264, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51944, %struct.ScmObj* %_37foldl148030, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51944, %struct.ScmObj* %_37map148077, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51944, %struct.ScmObj* %anf_45bind48266, i64 4)
%ae51945 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55754 = alloca %struct.ScmObj*, align 8
%fptrToInt55755 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51946 to i64
%ae51946 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55755)
store volatile %struct.ScmObj* %ae51946, %struct.ScmObj** %stackaddr$makeclosure55754, align 8
%argslist54874$ae519440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55756 = alloca %struct.ScmObj*, align 8
%argslist54874$ae519441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51946, %struct.ScmObj* %argslist54874$ae519440)
store volatile %struct.ScmObj* %argslist54874$ae519441, %struct.ScmObj** %stackaddr$prim55756, align 8
%stackaddr$prim55757 = alloca %struct.ScmObj*, align 8
%argslist54874$ae519442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51945, %struct.ScmObj* %argslist54874$ae519441)
store volatile %struct.ScmObj* %argslist54874$ae519442, %struct.ScmObj** %stackaddr$prim55757, align 8
%clofunc55758 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51944)
musttail call tailcc void %clofunc55758(%struct.ScmObj* %ae51944, %struct.ScmObj* %argslist54874$ae519442)
ret void
}

define tailcc void @proc_clo$ae51944(%struct.ScmObj* %env$ae51944,%struct.ScmObj* %current_45args54799) {
%stackaddr$env-ref55759 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51944, i64 0)
store %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$env-ref55759
%stackaddr$env-ref55760 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51944, i64 1)
store %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$env-ref55760
%stackaddr$env-ref55761 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51944, i64 2)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55761
%stackaddr$env-ref55762 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51944, i64 3)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55762
%stackaddr$env-ref55763 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51944, i64 4)
store %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$env-ref55763
%stackaddr$prim55764 = alloca %struct.ScmObj*, align 8
%_95k48318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54799)
store volatile %struct.ScmObj* %_95k48318, %struct.ScmObj** %stackaddr$prim55764, align 8
%stackaddr$prim55765 = alloca %struct.ScmObj*, align 8
%current_45args54800 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54799)
store volatile %struct.ScmObj* %current_45args54800, %struct.ScmObj** %stackaddr$prim55765, align 8
%stackaddr$prim55766 = alloca %struct.ScmObj*, align 8
%anf_45bind48267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54800)
store volatile %struct.ScmObj* %anf_45bind48267, %struct.ScmObj** %stackaddr$prim55766, align 8
%stackaddr$makeclosure55767 = alloca %struct.ScmObj*, align 8
%fptrToInt55768 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51966 to i64
%ae51966 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55768)
store volatile %struct.ScmObj* %ae51966, %struct.ScmObj** %stackaddr$makeclosure55767, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51966, %struct.ScmObj* %anf_45bind48265, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51966, %struct.ScmObj* %anf_45bind48264, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51966, %struct.ScmObj* %_37foldl148030, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51966, %struct.ScmObj* %_37map148077, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51966, %struct.ScmObj* %anf_45bind48267, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51966, %struct.ScmObj* %anf_45bind48266, i64 5)
%ae51967 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55769 = alloca %struct.ScmObj*, align 8
%fptrToInt55770 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51968 to i64
%ae51968 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55770)
store volatile %struct.ScmObj* %ae51968, %struct.ScmObj** %stackaddr$makeclosure55769, align 8
%argslist54872$ae519660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55771 = alloca %struct.ScmObj*, align 8
%argslist54872$ae519661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51968, %struct.ScmObj* %argslist54872$ae519660)
store volatile %struct.ScmObj* %argslist54872$ae519661, %struct.ScmObj** %stackaddr$prim55771, align 8
%stackaddr$prim55772 = alloca %struct.ScmObj*, align 8
%argslist54872$ae519662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51967, %struct.ScmObj* %argslist54872$ae519661)
store volatile %struct.ScmObj* %argslist54872$ae519662, %struct.ScmObj** %stackaddr$prim55772, align 8
%clofunc55773 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51966)
musttail call tailcc void %clofunc55773(%struct.ScmObj* %ae51966, %struct.ScmObj* %argslist54872$ae519662)
ret void
}

define tailcc void @proc_clo$ae51966(%struct.ScmObj* %env$ae51966,%struct.ScmObj* %current_45args54802) {
%stackaddr$env-ref55774 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51966, i64 0)
store %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$env-ref55774
%stackaddr$env-ref55775 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51966, i64 1)
store %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$env-ref55775
%stackaddr$env-ref55776 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51966, i64 2)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55776
%stackaddr$env-ref55777 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51966, i64 3)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55777
%stackaddr$env-ref55778 = alloca %struct.ScmObj*, align 8
%anf_45bind48267 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51966, i64 4)
store %struct.ScmObj* %anf_45bind48267, %struct.ScmObj** %stackaddr$env-ref55778
%stackaddr$env-ref55779 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51966, i64 5)
store %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$env-ref55779
%stackaddr$prim55780 = alloca %struct.ScmObj*, align 8
%_95k48319 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54802)
store volatile %struct.ScmObj* %_95k48319, %struct.ScmObj** %stackaddr$prim55780, align 8
%stackaddr$prim55781 = alloca %struct.ScmObj*, align 8
%current_45args54803 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54802)
store volatile %struct.ScmObj* %current_45args54803, %struct.ScmObj** %stackaddr$prim55781, align 8
%stackaddr$prim55782 = alloca %struct.ScmObj*, align 8
%anf_45bind48268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54803)
store volatile %struct.ScmObj* %anf_45bind48268, %struct.ScmObj** %stackaddr$prim55782, align 8
%stackaddr$makeclosure55783 = alloca %struct.ScmObj*, align 8
%fptrToInt55784 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51989 to i64
%ae51989 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55784)
store volatile %struct.ScmObj* %ae51989, %struct.ScmObj** %stackaddr$makeclosure55783, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51989, %struct.ScmObj* %anf_45bind48265, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51989, %struct.ScmObj* %anf_45bind48264, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51989, %struct.ScmObj* %_37foldl148030, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51989, %struct.ScmObj* %_37map148077, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51989, %struct.ScmObj* %anf_45bind48267, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51989, %struct.ScmObj* %anf_45bind48266, i64 5)
%argslist54870$anf_45bind482680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55785 = alloca %struct.ScmObj*, align 8
%argslist54870$anf_45bind482681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51989, %struct.ScmObj* %argslist54870$anf_45bind482680)
store volatile %struct.ScmObj* %argslist54870$anf_45bind482681, %struct.ScmObj** %stackaddr$prim55785, align 8
%clofunc55786 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48268)
musttail call tailcc void %clofunc55786(%struct.ScmObj* %anf_45bind48268, %struct.ScmObj* %argslist54870$anf_45bind482681)
ret void
}

define tailcc void @proc_clo$ae51989(%struct.ScmObj* %env$ae51989,%struct.ScmObj* %current_45args54805) {
%stackaddr$env-ref55787 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51989, i64 0)
store %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$env-ref55787
%stackaddr$env-ref55788 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51989, i64 1)
store %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$env-ref55788
%stackaddr$env-ref55789 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51989, i64 2)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55789
%stackaddr$env-ref55790 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51989, i64 3)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55790
%stackaddr$env-ref55791 = alloca %struct.ScmObj*, align 8
%anf_45bind48267 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51989, i64 4)
store %struct.ScmObj* %anf_45bind48267, %struct.ScmObj** %stackaddr$env-ref55791
%stackaddr$env-ref55792 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51989, i64 5)
store %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$env-ref55792
%stackaddr$prim55793 = alloca %struct.ScmObj*, align 8
%_95k48320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54805)
store volatile %struct.ScmObj* %_95k48320, %struct.ScmObj** %stackaddr$prim55793, align 8
%stackaddr$prim55794 = alloca %struct.ScmObj*, align 8
%current_45args54806 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54805)
store volatile %struct.ScmObj* %current_45args54806, %struct.ScmObj** %stackaddr$prim55794, align 8
%stackaddr$prim55795 = alloca %struct.ScmObj*, align 8
%anf_45bind48269 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54806)
store volatile %struct.ScmObj* %anf_45bind48269, %struct.ScmObj** %stackaddr$prim55795, align 8
%stackaddr$makeclosure55796 = alloca %struct.ScmObj*, align 8
%fptrToInt55797 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51990 to i64
%ae51990 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55797)
store volatile %struct.ScmObj* %ae51990, %struct.ScmObj** %stackaddr$makeclosure55796, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51990, %struct.ScmObj* %_37map148077, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51990, %struct.ScmObj* %anf_45bind48269, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51990, %struct.ScmObj* %anf_45bind48265, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51990, %struct.ScmObj* %anf_45bind48264, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51990, %struct.ScmObj* %_37foldl148030, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51990, %struct.ScmObj* %anf_45bind48267, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51990, %struct.ScmObj* %anf_45bind48266, i64 6)
%ae51991 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55798 = alloca %struct.ScmObj*, align 8
%fptrToInt55799 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51992 to i64
%ae51992 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55799)
store volatile %struct.ScmObj* %ae51992, %struct.ScmObj** %stackaddr$makeclosure55798, align 8
%argslist54869$ae519900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55800 = alloca %struct.ScmObj*, align 8
%argslist54869$ae519901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51992, %struct.ScmObj* %argslist54869$ae519900)
store volatile %struct.ScmObj* %argslist54869$ae519901, %struct.ScmObj** %stackaddr$prim55800, align 8
%stackaddr$prim55801 = alloca %struct.ScmObj*, align 8
%argslist54869$ae519902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51991, %struct.ScmObj* %argslist54869$ae519901)
store volatile %struct.ScmObj* %argslist54869$ae519902, %struct.ScmObj** %stackaddr$prim55801, align 8
%clofunc55802 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51990)
musttail call tailcc void %clofunc55802(%struct.ScmObj* %ae51990, %struct.ScmObj* %argslist54869$ae519902)
ret void
}

define tailcc void @proc_clo$ae51990(%struct.ScmObj* %env$ae51990,%struct.ScmObj* %current_45args54808) {
%stackaddr$env-ref55803 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51990, i64 0)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55803
%stackaddr$env-ref55804 = alloca %struct.ScmObj*, align 8
%anf_45bind48269 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51990, i64 1)
store %struct.ScmObj* %anf_45bind48269, %struct.ScmObj** %stackaddr$env-ref55804
%stackaddr$env-ref55805 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51990, i64 2)
store %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$env-ref55805
%stackaddr$env-ref55806 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51990, i64 3)
store %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$env-ref55806
%stackaddr$env-ref55807 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51990, i64 4)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55807
%stackaddr$env-ref55808 = alloca %struct.ScmObj*, align 8
%anf_45bind48267 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51990, i64 5)
store %struct.ScmObj* %anf_45bind48267, %struct.ScmObj** %stackaddr$env-ref55808
%stackaddr$env-ref55809 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51990, i64 6)
store %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$env-ref55809
%stackaddr$prim55810 = alloca %struct.ScmObj*, align 8
%_95k48321 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54808)
store volatile %struct.ScmObj* %_95k48321, %struct.ScmObj** %stackaddr$prim55810, align 8
%stackaddr$prim55811 = alloca %struct.ScmObj*, align 8
%current_45args54809 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54808)
store volatile %struct.ScmObj* %current_45args54809, %struct.ScmObj** %stackaddr$prim55811, align 8
%stackaddr$prim55812 = alloca %struct.ScmObj*, align 8
%anf_45bind48270 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54809)
store volatile %struct.ScmObj* %anf_45bind48270, %struct.ScmObj** %stackaddr$prim55812, align 8
%stackaddr$makeclosure55813 = alloca %struct.ScmObj*, align 8
%fptrToInt55814 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52012 to i64
%ae52012 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55814)
store volatile %struct.ScmObj* %ae52012, %struct.ScmObj** %stackaddr$makeclosure55813, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52012, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52012, %struct.ScmObj* %anf_45bind48270, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52012, %struct.ScmObj* %_37map148077, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52012, %struct.ScmObj* %anf_45bind48269, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52012, %struct.ScmObj* %anf_45bind48265, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52012, %struct.ScmObj* %anf_45bind48264, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52012, %struct.ScmObj* %anf_45bind48267, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae52012, %struct.ScmObj* %anf_45bind48266, i64 7)
%ae52013 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55815 = alloca %struct.ScmObj*, align 8
%fptrToInt55816 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52014 to i64
%ae52014 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55816)
store volatile %struct.ScmObj* %ae52014, %struct.ScmObj** %stackaddr$makeclosure55815, align 8
%argslist54867$ae520120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55817 = alloca %struct.ScmObj*, align 8
%argslist54867$ae520121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52014, %struct.ScmObj* %argslist54867$ae520120)
store volatile %struct.ScmObj* %argslist54867$ae520121, %struct.ScmObj** %stackaddr$prim55817, align 8
%stackaddr$prim55818 = alloca %struct.ScmObj*, align 8
%argslist54867$ae520122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52013, %struct.ScmObj* %argslist54867$ae520121)
store volatile %struct.ScmObj* %argslist54867$ae520122, %struct.ScmObj** %stackaddr$prim55818, align 8
%clofunc55819 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52012)
musttail call tailcc void %clofunc55819(%struct.ScmObj* %ae52012, %struct.ScmObj* %argslist54867$ae520122)
ret void
}

define tailcc void @proc_clo$ae52012(%struct.ScmObj* %env$ae52012,%struct.ScmObj* %current_45args54811) {
%stackaddr$env-ref55820 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52012, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55820
%stackaddr$env-ref55821 = alloca %struct.ScmObj*, align 8
%anf_45bind48270 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52012, i64 1)
store %struct.ScmObj* %anf_45bind48270, %struct.ScmObj** %stackaddr$env-ref55821
%stackaddr$env-ref55822 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52012, i64 2)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55822
%stackaddr$env-ref55823 = alloca %struct.ScmObj*, align 8
%anf_45bind48269 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52012, i64 3)
store %struct.ScmObj* %anf_45bind48269, %struct.ScmObj** %stackaddr$env-ref55823
%stackaddr$env-ref55824 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52012, i64 4)
store %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$env-ref55824
%stackaddr$env-ref55825 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52012, i64 5)
store %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$env-ref55825
%stackaddr$env-ref55826 = alloca %struct.ScmObj*, align 8
%anf_45bind48267 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52012, i64 6)
store %struct.ScmObj* %anf_45bind48267, %struct.ScmObj** %stackaddr$env-ref55826
%stackaddr$env-ref55827 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52012, i64 7)
store %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$env-ref55827
%stackaddr$prim55828 = alloca %struct.ScmObj*, align 8
%_95k48322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54811)
store volatile %struct.ScmObj* %_95k48322, %struct.ScmObj** %stackaddr$prim55828, align 8
%stackaddr$prim55829 = alloca %struct.ScmObj*, align 8
%current_45args54812 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54811)
store volatile %struct.ScmObj* %current_45args54812, %struct.ScmObj** %stackaddr$prim55829, align 8
%stackaddr$prim55830 = alloca %struct.ScmObj*, align 8
%anf_45bind48271 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54812)
store volatile %struct.ScmObj* %anf_45bind48271, %struct.ScmObj** %stackaddr$prim55830, align 8
%stackaddr$makeclosure55831 = alloca %struct.ScmObj*, align 8
%fptrToInt55832 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52035 to i64
%ae52035 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55832)
store volatile %struct.ScmObj* %ae52035, %struct.ScmObj** %stackaddr$makeclosure55831, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52035, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52035, %struct.ScmObj* %anf_45bind48270, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52035, %struct.ScmObj* %_37map148077, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52035, %struct.ScmObj* %anf_45bind48269, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52035, %struct.ScmObj* %anf_45bind48265, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52035, %struct.ScmObj* %anf_45bind48264, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52035, %struct.ScmObj* %anf_45bind48267, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae52035, %struct.ScmObj* %anf_45bind48266, i64 7)
%argslist54865$anf_45bind482710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55833 = alloca %struct.ScmObj*, align 8
%argslist54865$anf_45bind482711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52035, %struct.ScmObj* %argslist54865$anf_45bind482710)
store volatile %struct.ScmObj* %argslist54865$anf_45bind482711, %struct.ScmObj** %stackaddr$prim55833, align 8
%clofunc55834 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48271)
musttail call tailcc void %clofunc55834(%struct.ScmObj* %anf_45bind48271, %struct.ScmObj* %argslist54865$anf_45bind482711)
ret void
}

define tailcc void @proc_clo$ae52035(%struct.ScmObj* %env$ae52035,%struct.ScmObj* %current_45args54814) {
%stackaddr$env-ref55835 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52035, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55835
%stackaddr$env-ref55836 = alloca %struct.ScmObj*, align 8
%anf_45bind48270 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52035, i64 1)
store %struct.ScmObj* %anf_45bind48270, %struct.ScmObj** %stackaddr$env-ref55836
%stackaddr$env-ref55837 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52035, i64 2)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55837
%stackaddr$env-ref55838 = alloca %struct.ScmObj*, align 8
%anf_45bind48269 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52035, i64 3)
store %struct.ScmObj* %anf_45bind48269, %struct.ScmObj** %stackaddr$env-ref55838
%stackaddr$env-ref55839 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52035, i64 4)
store %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$env-ref55839
%stackaddr$env-ref55840 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52035, i64 5)
store %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$env-ref55840
%stackaddr$env-ref55841 = alloca %struct.ScmObj*, align 8
%anf_45bind48267 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52035, i64 6)
store %struct.ScmObj* %anf_45bind48267, %struct.ScmObj** %stackaddr$env-ref55841
%stackaddr$env-ref55842 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52035, i64 7)
store %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$env-ref55842
%stackaddr$prim55843 = alloca %struct.ScmObj*, align 8
%_95k48323 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54814)
store volatile %struct.ScmObj* %_95k48323, %struct.ScmObj** %stackaddr$prim55843, align 8
%stackaddr$prim55844 = alloca %struct.ScmObj*, align 8
%current_45args54815 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54814)
store volatile %struct.ScmObj* %current_45args54815, %struct.ScmObj** %stackaddr$prim55844, align 8
%stackaddr$prim55845 = alloca %struct.ScmObj*, align 8
%anf_45bind48272 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54815)
store volatile %struct.ScmObj* %anf_45bind48272, %struct.ScmObj** %stackaddr$prim55845, align 8
%stackaddr$makeclosure55846 = alloca %struct.ScmObj*, align 8
%fptrToInt55847 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52036 to i64
%ae52036 = call %struct.ScmObj* @closure_alloc(i64 9, i64 %fptrToInt55847)
store volatile %struct.ScmObj* %ae52036, %struct.ScmObj** %stackaddr$makeclosure55846, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52036, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52036, %struct.ScmObj* %anf_45bind48270, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52036, %struct.ScmObj* %_37map148077, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52036, %struct.ScmObj* %anf_45bind48269, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52036, %struct.ScmObj* %anf_45bind48265, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52036, %struct.ScmObj* %anf_45bind48264, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52036, %struct.ScmObj* %anf_45bind48272, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae52036, %struct.ScmObj* %anf_45bind48267, i64 7)
call void @closure_place_freevar(%struct.ScmObj* %ae52036, %struct.ScmObj* %anf_45bind48266, i64 8)
%ae52037 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55848 = alloca %struct.ScmObj*, align 8
%fptrToInt55849 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52038 to i64
%ae52038 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55849)
store volatile %struct.ScmObj* %ae52038, %struct.ScmObj** %stackaddr$makeclosure55848, align 8
%argslist54864$ae520360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55850 = alloca %struct.ScmObj*, align 8
%argslist54864$ae520361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52038, %struct.ScmObj* %argslist54864$ae520360)
store volatile %struct.ScmObj* %argslist54864$ae520361, %struct.ScmObj** %stackaddr$prim55850, align 8
%stackaddr$prim55851 = alloca %struct.ScmObj*, align 8
%argslist54864$ae520362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52037, %struct.ScmObj* %argslist54864$ae520361)
store volatile %struct.ScmObj* %argslist54864$ae520362, %struct.ScmObj** %stackaddr$prim55851, align 8
%clofunc55852 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52036)
musttail call tailcc void %clofunc55852(%struct.ScmObj* %ae52036, %struct.ScmObj* %argslist54864$ae520362)
ret void
}

define tailcc void @proc_clo$ae52036(%struct.ScmObj* %env$ae52036,%struct.ScmObj* %current_45args54817) {
%stackaddr$env-ref55853 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52036, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55853
%stackaddr$env-ref55854 = alloca %struct.ScmObj*, align 8
%anf_45bind48270 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52036, i64 1)
store %struct.ScmObj* %anf_45bind48270, %struct.ScmObj** %stackaddr$env-ref55854
%stackaddr$env-ref55855 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52036, i64 2)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55855
%stackaddr$env-ref55856 = alloca %struct.ScmObj*, align 8
%anf_45bind48269 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52036, i64 3)
store %struct.ScmObj* %anf_45bind48269, %struct.ScmObj** %stackaddr$env-ref55856
%stackaddr$env-ref55857 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52036, i64 4)
store %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$env-ref55857
%stackaddr$env-ref55858 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52036, i64 5)
store %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$env-ref55858
%stackaddr$env-ref55859 = alloca %struct.ScmObj*, align 8
%anf_45bind48272 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52036, i64 6)
store %struct.ScmObj* %anf_45bind48272, %struct.ScmObj** %stackaddr$env-ref55859
%stackaddr$env-ref55860 = alloca %struct.ScmObj*, align 8
%anf_45bind48267 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52036, i64 7)
store %struct.ScmObj* %anf_45bind48267, %struct.ScmObj** %stackaddr$env-ref55860
%stackaddr$env-ref55861 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52036, i64 8)
store %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$env-ref55861
%stackaddr$prim55862 = alloca %struct.ScmObj*, align 8
%_95k48324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54817)
store volatile %struct.ScmObj* %_95k48324, %struct.ScmObj** %stackaddr$prim55862, align 8
%stackaddr$prim55863 = alloca %struct.ScmObj*, align 8
%current_45args54818 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54817)
store volatile %struct.ScmObj* %current_45args54818, %struct.ScmObj** %stackaddr$prim55863, align 8
%stackaddr$prim55864 = alloca %struct.ScmObj*, align 8
%anf_45bind48273 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54818)
store volatile %struct.ScmObj* %anf_45bind48273, %struct.ScmObj** %stackaddr$prim55864, align 8
%stackaddr$makeclosure55865 = alloca %struct.ScmObj*, align 8
%fptrToInt55866 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52059 to i64
%ae52059 = call %struct.ScmObj* @closure_alloc(i64 9, i64 %fptrToInt55866)
store volatile %struct.ScmObj* %ae52059, %struct.ScmObj** %stackaddr$makeclosure55865, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52059, %struct.ScmObj* %_37foldl148030, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52059, %struct.ScmObj* %anf_45bind48270, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52059, %struct.ScmObj* %_37map148077, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52059, %struct.ScmObj* %anf_45bind48269, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52059, %struct.ScmObj* %anf_45bind48265, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52059, %struct.ScmObj* %anf_45bind48264, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52059, %struct.ScmObj* %anf_45bind48272, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae52059, %struct.ScmObj* %anf_45bind48267, i64 7)
call void @closure_place_freevar(%struct.ScmObj* %ae52059, %struct.ScmObj* %anf_45bind48266, i64 8)
%argslist54862$anf_45bind482730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55867 = alloca %struct.ScmObj*, align 8
%argslist54862$anf_45bind482731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52059, %struct.ScmObj* %argslist54862$anf_45bind482730)
store volatile %struct.ScmObj* %argslist54862$anf_45bind482731, %struct.ScmObj** %stackaddr$prim55867, align 8
%clofunc55868 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48273)
musttail call tailcc void %clofunc55868(%struct.ScmObj* %anf_45bind48273, %struct.ScmObj* %argslist54862$anf_45bind482731)
ret void
}

define tailcc void @proc_clo$ae52059(%struct.ScmObj* %env$ae52059,%struct.ScmObj* %current_45args54820) {
%stackaddr$env-ref55869 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52059, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55869
%stackaddr$env-ref55870 = alloca %struct.ScmObj*, align 8
%anf_45bind48270 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52059, i64 1)
store %struct.ScmObj* %anf_45bind48270, %struct.ScmObj** %stackaddr$env-ref55870
%stackaddr$env-ref55871 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52059, i64 2)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55871
%stackaddr$env-ref55872 = alloca %struct.ScmObj*, align 8
%anf_45bind48269 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52059, i64 3)
store %struct.ScmObj* %anf_45bind48269, %struct.ScmObj** %stackaddr$env-ref55872
%stackaddr$env-ref55873 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52059, i64 4)
store %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$env-ref55873
%stackaddr$env-ref55874 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52059, i64 5)
store %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$env-ref55874
%stackaddr$env-ref55875 = alloca %struct.ScmObj*, align 8
%anf_45bind48272 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52059, i64 6)
store %struct.ScmObj* %anf_45bind48272, %struct.ScmObj** %stackaddr$env-ref55875
%stackaddr$env-ref55876 = alloca %struct.ScmObj*, align 8
%anf_45bind48267 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52059, i64 7)
store %struct.ScmObj* %anf_45bind48267, %struct.ScmObj** %stackaddr$env-ref55876
%stackaddr$env-ref55877 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52059, i64 8)
store %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$env-ref55877
%stackaddr$prim55878 = alloca %struct.ScmObj*, align 8
%_95k48325 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54820)
store volatile %struct.ScmObj* %_95k48325, %struct.ScmObj** %stackaddr$prim55878, align 8
%stackaddr$prim55879 = alloca %struct.ScmObj*, align 8
%current_45args54821 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54820)
store volatile %struct.ScmObj* %current_45args54821, %struct.ScmObj** %stackaddr$prim55879, align 8
%stackaddr$prim55880 = alloca %struct.ScmObj*, align 8
%anf_45bind48274 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54821)
store volatile %struct.ScmObj* %anf_45bind48274, %struct.ScmObj** %stackaddr$prim55880, align 8
%stackaddr$makeclosure55881 = alloca %struct.ScmObj*, align 8
%fptrToInt55882 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52061 to i64
%ae52061 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55882)
store volatile %struct.ScmObj* %ae52061, %struct.ScmObj** %stackaddr$makeclosure55881, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52061, %struct.ScmObj* %_37map148077, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52061, %struct.ScmObj* %anf_45bind48269, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52061, %struct.ScmObj* %anf_45bind48265, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52061, %struct.ScmObj* %anf_45bind48264, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52061, %struct.ScmObj* %_37foldl148030, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52061, %struct.ScmObj* %anf_45bind48267, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52061, %struct.ScmObj* %anf_45bind48266, i64 6)
%argslist54861$anf_45bind482700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55883 = alloca %struct.ScmObj*, align 8
%argslist54861$anf_45bind482701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48274, %struct.ScmObj* %argslist54861$anf_45bind482700)
store volatile %struct.ScmObj* %argslist54861$anf_45bind482701, %struct.ScmObj** %stackaddr$prim55883, align 8
%stackaddr$prim55884 = alloca %struct.ScmObj*, align 8
%argslist54861$anf_45bind482702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48272, %struct.ScmObj* %argslist54861$anf_45bind482701)
store volatile %struct.ScmObj* %argslist54861$anf_45bind482702, %struct.ScmObj** %stackaddr$prim55884, align 8
%stackaddr$prim55885 = alloca %struct.ScmObj*, align 8
%argslist54861$anf_45bind482703 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52061, %struct.ScmObj* %argslist54861$anf_45bind482702)
store volatile %struct.ScmObj* %argslist54861$anf_45bind482703, %struct.ScmObj** %stackaddr$prim55885, align 8
%clofunc55886 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48270)
musttail call tailcc void %clofunc55886(%struct.ScmObj* %anf_45bind48270, %struct.ScmObj* %argslist54861$anf_45bind482703)
ret void
}

define tailcc void @proc_clo$ae52061(%struct.ScmObj* %env$ae52061,%struct.ScmObj* %current_45args54823) {
%stackaddr$env-ref55887 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52061, i64 0)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55887
%stackaddr$env-ref55888 = alloca %struct.ScmObj*, align 8
%anf_45bind48269 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52061, i64 1)
store %struct.ScmObj* %anf_45bind48269, %struct.ScmObj** %stackaddr$env-ref55888
%stackaddr$env-ref55889 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52061, i64 2)
store %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$env-ref55889
%stackaddr$env-ref55890 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52061, i64 3)
store %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$env-ref55890
%stackaddr$env-ref55891 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52061, i64 4)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55891
%stackaddr$env-ref55892 = alloca %struct.ScmObj*, align 8
%anf_45bind48267 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52061, i64 5)
store %struct.ScmObj* %anf_45bind48267, %struct.ScmObj** %stackaddr$env-ref55892
%stackaddr$env-ref55893 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52061, i64 6)
store %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$env-ref55893
%stackaddr$prim55894 = alloca %struct.ScmObj*, align 8
%_95k48326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54823)
store volatile %struct.ScmObj* %_95k48326, %struct.ScmObj** %stackaddr$prim55894, align 8
%stackaddr$prim55895 = alloca %struct.ScmObj*, align 8
%current_45args54824 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54823)
store volatile %struct.ScmObj* %current_45args54824, %struct.ScmObj** %stackaddr$prim55895, align 8
%stackaddr$prim55896 = alloca %struct.ScmObj*, align 8
%anf_45bind48275 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54824)
store volatile %struct.ScmObj* %anf_45bind48275, %struct.ScmObj** %stackaddr$prim55896, align 8
%stackaddr$makeclosure55897 = alloca %struct.ScmObj*, align 8
%fptrToInt55898 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52064 to i64
%ae52064 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55898)
store volatile %struct.ScmObj* %ae52064, %struct.ScmObj** %stackaddr$makeclosure55897, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52064, %struct.ScmObj* %_37map148077, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52064, %struct.ScmObj* %anf_45bind48269, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52064, %struct.ScmObj* %anf_45bind48265, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52064, %struct.ScmObj* %anf_45bind48264, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52064, %struct.ScmObj* %anf_45bind48275, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52064, %struct.ScmObj* %_37foldl148030, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52064, %struct.ScmObj* %anf_45bind48267, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae52064, %struct.ScmObj* %anf_45bind48266, i64 7)
%ae52065 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55899 = alloca %struct.ScmObj*, align 8
%fptrToInt55900 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52066 to i64
%ae52066 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55900)
store volatile %struct.ScmObj* %ae52066, %struct.ScmObj** %stackaddr$makeclosure55899, align 8
%argslist54860$ae520640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55901 = alloca %struct.ScmObj*, align 8
%argslist54860$ae520641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52066, %struct.ScmObj* %argslist54860$ae520640)
store volatile %struct.ScmObj* %argslist54860$ae520641, %struct.ScmObj** %stackaddr$prim55901, align 8
%stackaddr$prim55902 = alloca %struct.ScmObj*, align 8
%argslist54860$ae520642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52065, %struct.ScmObj* %argslist54860$ae520641)
store volatile %struct.ScmObj* %argslist54860$ae520642, %struct.ScmObj** %stackaddr$prim55902, align 8
%clofunc55903 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52064)
musttail call tailcc void %clofunc55903(%struct.ScmObj* %ae52064, %struct.ScmObj* %argslist54860$ae520642)
ret void
}

define tailcc void @proc_clo$ae52064(%struct.ScmObj* %env$ae52064,%struct.ScmObj* %current_45args54826) {
%stackaddr$env-ref55904 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52064, i64 0)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55904
%stackaddr$env-ref55905 = alloca %struct.ScmObj*, align 8
%anf_45bind48269 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52064, i64 1)
store %struct.ScmObj* %anf_45bind48269, %struct.ScmObj** %stackaddr$env-ref55905
%stackaddr$env-ref55906 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52064, i64 2)
store %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$env-ref55906
%stackaddr$env-ref55907 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52064, i64 3)
store %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$env-ref55907
%stackaddr$env-ref55908 = alloca %struct.ScmObj*, align 8
%anf_45bind48275 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52064, i64 4)
store %struct.ScmObj* %anf_45bind48275, %struct.ScmObj** %stackaddr$env-ref55908
%stackaddr$env-ref55909 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52064, i64 5)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55909
%stackaddr$env-ref55910 = alloca %struct.ScmObj*, align 8
%anf_45bind48267 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52064, i64 6)
store %struct.ScmObj* %anf_45bind48267, %struct.ScmObj** %stackaddr$env-ref55910
%stackaddr$env-ref55911 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52064, i64 7)
store %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$env-ref55911
%stackaddr$prim55912 = alloca %struct.ScmObj*, align 8
%_95k48327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54826)
store volatile %struct.ScmObj* %_95k48327, %struct.ScmObj** %stackaddr$prim55912, align 8
%stackaddr$prim55913 = alloca %struct.ScmObj*, align 8
%current_45args54827 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54826)
store volatile %struct.ScmObj* %current_45args54827, %struct.ScmObj** %stackaddr$prim55913, align 8
%stackaddr$prim55914 = alloca %struct.ScmObj*, align 8
%anf_45bind48276 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54827)
store volatile %struct.ScmObj* %anf_45bind48276, %struct.ScmObj** %stackaddr$prim55914, align 8
%stackaddr$makeclosure55915 = alloca %struct.ScmObj*, align 8
%fptrToInt55916 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52089 to i64
%ae52089 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55916)
store volatile %struct.ScmObj* %ae52089, %struct.ScmObj** %stackaddr$makeclosure55915, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52089, %struct.ScmObj* %_37map148077, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52089, %struct.ScmObj* %anf_45bind48269, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52089, %struct.ScmObj* %anf_45bind48265, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52089, %struct.ScmObj* %anf_45bind48264, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52089, %struct.ScmObj* %anf_45bind48275, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52089, %struct.ScmObj* %_37foldl148030, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52089, %struct.ScmObj* %anf_45bind48267, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae52089, %struct.ScmObj* %anf_45bind48266, i64 7)
%argslist54858$anf_45bind482760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55917 = alloca %struct.ScmObj*, align 8
%argslist54858$anf_45bind482761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52089, %struct.ScmObj* %argslist54858$anf_45bind482760)
store volatile %struct.ScmObj* %argslist54858$anf_45bind482761, %struct.ScmObj** %stackaddr$prim55917, align 8
%clofunc55918 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48276)
musttail call tailcc void %clofunc55918(%struct.ScmObj* %anf_45bind48276, %struct.ScmObj* %argslist54858$anf_45bind482761)
ret void
}

define tailcc void @proc_clo$ae52089(%struct.ScmObj* %env$ae52089,%struct.ScmObj* %current_45args54829) {
%stackaddr$env-ref55919 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52089, i64 0)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55919
%stackaddr$env-ref55920 = alloca %struct.ScmObj*, align 8
%anf_45bind48269 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52089, i64 1)
store %struct.ScmObj* %anf_45bind48269, %struct.ScmObj** %stackaddr$env-ref55920
%stackaddr$env-ref55921 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52089, i64 2)
store %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$env-ref55921
%stackaddr$env-ref55922 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52089, i64 3)
store %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$env-ref55922
%stackaddr$env-ref55923 = alloca %struct.ScmObj*, align 8
%anf_45bind48275 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52089, i64 4)
store %struct.ScmObj* %anf_45bind48275, %struct.ScmObj** %stackaddr$env-ref55923
%stackaddr$env-ref55924 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52089, i64 5)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55924
%stackaddr$env-ref55925 = alloca %struct.ScmObj*, align 8
%anf_45bind48267 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52089, i64 6)
store %struct.ScmObj* %anf_45bind48267, %struct.ScmObj** %stackaddr$env-ref55925
%stackaddr$env-ref55926 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52089, i64 7)
store %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$env-ref55926
%stackaddr$prim55927 = alloca %struct.ScmObj*, align 8
%_95k48328 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54829)
store volatile %struct.ScmObj* %_95k48328, %struct.ScmObj** %stackaddr$prim55927, align 8
%stackaddr$prim55928 = alloca %struct.ScmObj*, align 8
%current_45args54830 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54829)
store volatile %struct.ScmObj* %current_45args54830, %struct.ScmObj** %stackaddr$prim55928, align 8
%stackaddr$prim55929 = alloca %struct.ScmObj*, align 8
%anf_45bind48277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54830)
store volatile %struct.ScmObj* %anf_45bind48277, %struct.ScmObj** %stackaddr$prim55929, align 8
%stackaddr$makeclosure55930 = alloca %struct.ScmObj*, align 8
%fptrToInt55931 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52090 to i64
%ae52090 = call %struct.ScmObj* @closure_alloc(i64 9, i64 %fptrToInt55931)
store volatile %struct.ScmObj* %ae52090, %struct.ScmObj** %stackaddr$makeclosure55930, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52090, %struct.ScmObj* %_37map148077, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52090, %struct.ScmObj* %anf_45bind48269, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52090, %struct.ScmObj* %anf_45bind48265, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52090, %struct.ScmObj* %anf_45bind48264, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52090, %struct.ScmObj* %anf_45bind48277, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52090, %struct.ScmObj* %anf_45bind48275, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52090, %struct.ScmObj* %_37foldl148030, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae52090, %struct.ScmObj* %anf_45bind48267, i64 7)
call void @closure_place_freevar(%struct.ScmObj* %ae52090, %struct.ScmObj* %anf_45bind48266, i64 8)
%ae52091 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55932 = alloca %struct.ScmObj*, align 8
%fptrToInt55933 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52092 to i64
%ae52092 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55933)
store volatile %struct.ScmObj* %ae52092, %struct.ScmObj** %stackaddr$makeclosure55932, align 8
%argslist54857$ae520900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55934 = alloca %struct.ScmObj*, align 8
%argslist54857$ae520901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52092, %struct.ScmObj* %argslist54857$ae520900)
store volatile %struct.ScmObj* %argslist54857$ae520901, %struct.ScmObj** %stackaddr$prim55934, align 8
%stackaddr$prim55935 = alloca %struct.ScmObj*, align 8
%argslist54857$ae520902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52091, %struct.ScmObj* %argslist54857$ae520901)
store volatile %struct.ScmObj* %argslist54857$ae520902, %struct.ScmObj** %stackaddr$prim55935, align 8
%clofunc55936 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae52090)
musttail call tailcc void %clofunc55936(%struct.ScmObj* %ae52090, %struct.ScmObj* %argslist54857$ae520902)
ret void
}

define tailcc void @proc_clo$ae52090(%struct.ScmObj* %env$ae52090,%struct.ScmObj* %current_45args54832) {
%stackaddr$env-ref55937 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52090, i64 0)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55937
%stackaddr$env-ref55938 = alloca %struct.ScmObj*, align 8
%anf_45bind48269 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52090, i64 1)
store %struct.ScmObj* %anf_45bind48269, %struct.ScmObj** %stackaddr$env-ref55938
%stackaddr$env-ref55939 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52090, i64 2)
store %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$env-ref55939
%stackaddr$env-ref55940 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52090, i64 3)
store %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$env-ref55940
%stackaddr$env-ref55941 = alloca %struct.ScmObj*, align 8
%anf_45bind48277 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52090, i64 4)
store %struct.ScmObj* %anf_45bind48277, %struct.ScmObj** %stackaddr$env-ref55941
%stackaddr$env-ref55942 = alloca %struct.ScmObj*, align 8
%anf_45bind48275 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52090, i64 5)
store %struct.ScmObj* %anf_45bind48275, %struct.ScmObj** %stackaddr$env-ref55942
%stackaddr$env-ref55943 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52090, i64 6)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55943
%stackaddr$env-ref55944 = alloca %struct.ScmObj*, align 8
%anf_45bind48267 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52090, i64 7)
store %struct.ScmObj* %anf_45bind48267, %struct.ScmObj** %stackaddr$env-ref55944
%stackaddr$env-ref55945 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52090, i64 8)
store %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$env-ref55945
%stackaddr$prim55946 = alloca %struct.ScmObj*, align 8
%_95k48329 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54832)
store volatile %struct.ScmObj* %_95k48329, %struct.ScmObj** %stackaddr$prim55946, align 8
%stackaddr$prim55947 = alloca %struct.ScmObj*, align 8
%current_45args54833 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54832)
store volatile %struct.ScmObj* %current_45args54833, %struct.ScmObj** %stackaddr$prim55947, align 8
%stackaddr$prim55948 = alloca %struct.ScmObj*, align 8
%anf_45bind48278 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54833)
store volatile %struct.ScmObj* %anf_45bind48278, %struct.ScmObj** %stackaddr$prim55948, align 8
%stackaddr$makeclosure55949 = alloca %struct.ScmObj*, align 8
%fptrToInt55950 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52115 to i64
%ae52115 = call %struct.ScmObj* @closure_alloc(i64 9, i64 %fptrToInt55950)
store volatile %struct.ScmObj* %ae52115, %struct.ScmObj** %stackaddr$makeclosure55949, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52115, %struct.ScmObj* %_37map148077, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52115, %struct.ScmObj* %anf_45bind48269, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52115, %struct.ScmObj* %anf_45bind48265, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52115, %struct.ScmObj* %anf_45bind48264, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52115, %struct.ScmObj* %anf_45bind48277, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae52115, %struct.ScmObj* %anf_45bind48275, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae52115, %struct.ScmObj* %_37foldl148030, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae52115, %struct.ScmObj* %anf_45bind48267, i64 7)
call void @closure_place_freevar(%struct.ScmObj* %ae52115, %struct.ScmObj* %anf_45bind48266, i64 8)
%ae52116 = call %struct.ScmObj* @const_init_int(i64 0)
%ae52117 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist54855$anf_45bind482780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55951 = alloca %struct.ScmObj*, align 8
%argslist54855$anf_45bind482781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52117, %struct.ScmObj* %argslist54855$anf_45bind482780)
store volatile %struct.ScmObj* %argslist54855$anf_45bind482781, %struct.ScmObj** %stackaddr$prim55951, align 8
%stackaddr$prim55952 = alloca %struct.ScmObj*, align 8
%argslist54855$anf_45bind482782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52116, %struct.ScmObj* %argslist54855$anf_45bind482781)
store volatile %struct.ScmObj* %argslist54855$anf_45bind482782, %struct.ScmObj** %stackaddr$prim55952, align 8
%stackaddr$prim55953 = alloca %struct.ScmObj*, align 8
%argslist54855$anf_45bind482783 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52115, %struct.ScmObj* %argslist54855$anf_45bind482782)
store volatile %struct.ScmObj* %argslist54855$anf_45bind482783, %struct.ScmObj** %stackaddr$prim55953, align 8
%clofunc55954 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48278)
musttail call tailcc void %clofunc55954(%struct.ScmObj* %anf_45bind48278, %struct.ScmObj* %argslist54855$anf_45bind482783)
ret void
}

define tailcc void @proc_clo$ae52115(%struct.ScmObj* %env$ae52115,%struct.ScmObj* %current_45args54835) {
%stackaddr$env-ref55955 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52115, i64 0)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55955
%stackaddr$env-ref55956 = alloca %struct.ScmObj*, align 8
%anf_45bind48269 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52115, i64 1)
store %struct.ScmObj* %anf_45bind48269, %struct.ScmObj** %stackaddr$env-ref55956
%stackaddr$env-ref55957 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52115, i64 2)
store %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$env-ref55957
%stackaddr$env-ref55958 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52115, i64 3)
store %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$env-ref55958
%stackaddr$env-ref55959 = alloca %struct.ScmObj*, align 8
%anf_45bind48277 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52115, i64 4)
store %struct.ScmObj* %anf_45bind48277, %struct.ScmObj** %stackaddr$env-ref55959
%stackaddr$env-ref55960 = alloca %struct.ScmObj*, align 8
%anf_45bind48275 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52115, i64 5)
store %struct.ScmObj* %anf_45bind48275, %struct.ScmObj** %stackaddr$env-ref55960
%stackaddr$env-ref55961 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52115, i64 6)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55961
%stackaddr$env-ref55962 = alloca %struct.ScmObj*, align 8
%anf_45bind48267 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52115, i64 7)
store %struct.ScmObj* %anf_45bind48267, %struct.ScmObj** %stackaddr$env-ref55962
%stackaddr$env-ref55963 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52115, i64 8)
store %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$env-ref55963
%stackaddr$prim55964 = alloca %struct.ScmObj*, align 8
%_95k48330 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54835)
store volatile %struct.ScmObj* %_95k48330, %struct.ScmObj** %stackaddr$prim55964, align 8
%stackaddr$prim55965 = alloca %struct.ScmObj*, align 8
%current_45args54836 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54835)
store volatile %struct.ScmObj* %current_45args54836, %struct.ScmObj** %stackaddr$prim55965, align 8
%stackaddr$prim55966 = alloca %struct.ScmObj*, align 8
%anf_45bind48279 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54836)
store volatile %struct.ScmObj* %anf_45bind48279, %struct.ScmObj** %stackaddr$prim55966, align 8
%stackaddr$makeclosure55967 = alloca %struct.ScmObj*, align 8
%fptrToInt55968 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52127 to i64
%ae52127 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55968)
store volatile %struct.ScmObj* %ae52127, %struct.ScmObj** %stackaddr$makeclosure55967, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52127, %struct.ScmObj* %anf_45bind48265, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52127, %struct.ScmObj* %anf_45bind48264, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52127, %struct.ScmObj* %_37foldl148030, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52127, %struct.ScmObj* %_37map148077, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae52127, %struct.ScmObj* %anf_45bind48266, i64 4)
%ae52128 = call %struct.ScmObj* @const_init_false()
%ae52129 = call %struct.ScmObj* @const_init_true()
%ae52130 = call %struct.ScmObj* @const_init_int(i64 7)
%ae52135 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @global$sym$ae5213555969, i32 0, i32 0))
%argslist54854$anf_45bind482670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55970 = alloca %struct.ScmObj*, align 8
%argslist54854$anf_45bind482671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52135, %struct.ScmObj* %argslist54854$anf_45bind482670)
store volatile %struct.ScmObj* %argslist54854$anf_45bind482671, %struct.ScmObj** %stackaddr$prim55970, align 8
%stackaddr$prim55971 = alloca %struct.ScmObj*, align 8
%argslist54854$anf_45bind482672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48279, %struct.ScmObj* %argslist54854$anf_45bind482671)
store volatile %struct.ScmObj* %argslist54854$anf_45bind482672, %struct.ScmObj** %stackaddr$prim55971, align 8
%stackaddr$prim55972 = alloca %struct.ScmObj*, align 8
%argslist54854$anf_45bind482673 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48277, %struct.ScmObj* %argslist54854$anf_45bind482672)
store volatile %struct.ScmObj* %argslist54854$anf_45bind482673, %struct.ScmObj** %stackaddr$prim55972, align 8
%stackaddr$prim55973 = alloca %struct.ScmObj*, align 8
%argslist54854$anf_45bind482674 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48275, %struct.ScmObj* %argslist54854$anf_45bind482673)
store volatile %struct.ScmObj* %argslist54854$anf_45bind482674, %struct.ScmObj** %stackaddr$prim55973, align 8
%stackaddr$prim55974 = alloca %struct.ScmObj*, align 8
%argslist54854$anf_45bind482675 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48269, %struct.ScmObj* %argslist54854$anf_45bind482674)
store volatile %struct.ScmObj* %argslist54854$anf_45bind482675, %struct.ScmObj** %stackaddr$prim55974, align 8
%stackaddr$prim55975 = alloca %struct.ScmObj*, align 8
%argslist54854$anf_45bind482676 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52130, %struct.ScmObj* %argslist54854$anf_45bind482675)
store volatile %struct.ScmObj* %argslist54854$anf_45bind482676, %struct.ScmObj** %stackaddr$prim55975, align 8
%stackaddr$prim55976 = alloca %struct.ScmObj*, align 8
%argslist54854$anf_45bind482677 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52129, %struct.ScmObj* %argslist54854$anf_45bind482676)
store volatile %struct.ScmObj* %argslist54854$anf_45bind482677, %struct.ScmObj** %stackaddr$prim55976, align 8
%stackaddr$prim55977 = alloca %struct.ScmObj*, align 8
%argslist54854$anf_45bind482678 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52128, %struct.ScmObj* %argslist54854$anf_45bind482677)
store volatile %struct.ScmObj* %argslist54854$anf_45bind482678, %struct.ScmObj** %stackaddr$prim55977, align 8
%stackaddr$prim55978 = alloca %struct.ScmObj*, align 8
%argslist54854$anf_45bind482679 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52127, %struct.ScmObj* %argslist54854$anf_45bind482678)
store volatile %struct.ScmObj* %argslist54854$anf_45bind482679, %struct.ScmObj** %stackaddr$prim55978, align 8
%clofunc55979 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48267)
musttail call tailcc void %clofunc55979(%struct.ScmObj* %anf_45bind48267, %struct.ScmObj* %argslist54854$anf_45bind482679)
ret void
}

define tailcc void @proc_clo$ae52127(%struct.ScmObj* %env$ae52127,%struct.ScmObj* %current_45args54838) {
%stackaddr$env-ref55980 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52127, i64 0)
store %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$env-ref55980
%stackaddr$env-ref55981 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52127, i64 1)
store %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$env-ref55981
%stackaddr$env-ref55982 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52127, i64 2)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55982
%stackaddr$env-ref55983 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52127, i64 3)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55983
%stackaddr$env-ref55984 = alloca %struct.ScmObj*, align 8
%anf_45bind48266 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52127, i64 4)
store %struct.ScmObj* %anf_45bind48266, %struct.ScmObj** %stackaddr$env-ref55984
%stackaddr$prim55985 = alloca %struct.ScmObj*, align 8
%_95k48331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54838)
store volatile %struct.ScmObj* %_95k48331, %struct.ScmObj** %stackaddr$prim55985, align 8
%stackaddr$prim55986 = alloca %struct.ScmObj*, align 8
%current_45args54839 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54838)
store volatile %struct.ScmObj* %current_45args54839, %struct.ScmObj** %stackaddr$prim55986, align 8
%stackaddr$prim55987 = alloca %struct.ScmObj*, align 8
%anf_45bind48280 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54839)
store volatile %struct.ScmObj* %anf_45bind48280, %struct.ScmObj** %stackaddr$prim55987, align 8
%stackaddr$makeclosure55988 = alloca %struct.ScmObj*, align 8
%fptrToInt55989 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52177 to i64
%ae52177 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55989)
store volatile %struct.ScmObj* %ae52177, %struct.ScmObj** %stackaddr$makeclosure55988, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52177, %struct.ScmObj* %anf_45bind48265, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52177, %struct.ScmObj* %anf_45bind48264, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae52177, %struct.ScmObj* %_37foldl148030, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae52177, %struct.ScmObj* %_37map148077, i64 3)
%argslist54853$_37map1480770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55990 = alloca %struct.ScmObj*, align 8
%argslist54853$_37map1480771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48280, %struct.ScmObj* %argslist54853$_37map1480770)
store volatile %struct.ScmObj* %argslist54853$_37map1480771, %struct.ScmObj** %stackaddr$prim55990, align 8
%stackaddr$prim55991 = alloca %struct.ScmObj*, align 8
%argslist54853$_37map1480772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48266, %struct.ScmObj* %argslist54853$_37map1480771)
store volatile %struct.ScmObj* %argslist54853$_37map1480772, %struct.ScmObj** %stackaddr$prim55991, align 8
%stackaddr$prim55992 = alloca %struct.ScmObj*, align 8
%argslist54853$_37map1480773 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52177, %struct.ScmObj* %argslist54853$_37map1480772)
store volatile %struct.ScmObj* %argslist54853$_37map1480773, %struct.ScmObj** %stackaddr$prim55992, align 8
%clofunc55993 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148077)
musttail call tailcc void %clofunc55993(%struct.ScmObj* %_37map148077, %struct.ScmObj* %argslist54853$_37map1480773)
ret void
}

define tailcc void @proc_clo$ae52177(%struct.ScmObj* %env$ae52177,%struct.ScmObj* %current_45args54841) {
%stackaddr$env-ref55994 = alloca %struct.ScmObj*, align 8
%anf_45bind48265 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52177, i64 0)
store %struct.ScmObj* %anf_45bind48265, %struct.ScmObj** %stackaddr$env-ref55994
%stackaddr$env-ref55995 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52177, i64 1)
store %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$env-ref55995
%stackaddr$env-ref55996 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52177, i64 2)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref55996
%stackaddr$env-ref55997 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52177, i64 3)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref55997
%stackaddr$prim55998 = alloca %struct.ScmObj*, align 8
%_95k48332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54841)
store volatile %struct.ScmObj* %_95k48332, %struct.ScmObj** %stackaddr$prim55998, align 8
%stackaddr$prim55999 = alloca %struct.ScmObj*, align 8
%current_45args54842 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54841)
store volatile %struct.ScmObj* %current_45args54842, %struct.ScmObj** %stackaddr$prim55999, align 8
%stackaddr$prim56000 = alloca %struct.ScmObj*, align 8
%anf_45bind48281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54842)
store volatile %struct.ScmObj* %anf_45bind48281, %struct.ScmObj** %stackaddr$prim56000, align 8
%stackaddr$makeclosure56001 = alloca %struct.ScmObj*, align 8
%fptrToInt56002 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52181 to i64
%ae52181 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56002)
store volatile %struct.ScmObj* %ae52181, %struct.ScmObj** %stackaddr$makeclosure56001, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae52181, %struct.ScmObj* %anf_45bind48264, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae52181, %struct.ScmObj* %_37foldl148030, i64 1)
%argslist54852$_37map1480770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56003 = alloca %struct.ScmObj*, align 8
%argslist54852$_37map1480771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48281, %struct.ScmObj* %argslist54852$_37map1480770)
store volatile %struct.ScmObj* %argslist54852$_37map1480771, %struct.ScmObj** %stackaddr$prim56003, align 8
%stackaddr$prim56004 = alloca %struct.ScmObj*, align 8
%argslist54852$_37map1480772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48265, %struct.ScmObj* %argslist54852$_37map1480771)
store volatile %struct.ScmObj* %argslist54852$_37map1480772, %struct.ScmObj** %stackaddr$prim56004, align 8
%stackaddr$prim56005 = alloca %struct.ScmObj*, align 8
%argslist54852$_37map1480773 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52181, %struct.ScmObj* %argslist54852$_37map1480772)
store volatile %struct.ScmObj* %argslist54852$_37map1480773, %struct.ScmObj** %stackaddr$prim56005, align 8
%clofunc56006 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148077)
musttail call tailcc void %clofunc56006(%struct.ScmObj* %_37map148077, %struct.ScmObj* %argslist54852$_37map1480773)
ret void
}

define tailcc void @proc_clo$ae52181(%struct.ScmObj* %env$ae52181,%struct.ScmObj* %current_45args54844) {
%stackaddr$env-ref56007 = alloca %struct.ScmObj*, align 8
%anf_45bind48264 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52181, i64 0)
store %struct.ScmObj* %anf_45bind48264, %struct.ScmObj** %stackaddr$env-ref56007
%stackaddr$env-ref56008 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae52181, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref56008
%stackaddr$prim56009 = alloca %struct.ScmObj*, align 8
%_95k48333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54844)
store volatile %struct.ScmObj* %_95k48333, %struct.ScmObj** %stackaddr$prim56009, align 8
%stackaddr$prim56010 = alloca %struct.ScmObj*, align 8
%current_45args54845 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54844)
store volatile %struct.ScmObj* %current_45args54845, %struct.ScmObj** %stackaddr$prim56010, align 8
%stackaddr$prim56011 = alloca %struct.ScmObj*, align 8
%anf_45bind48282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54845)
store volatile %struct.ScmObj* %anf_45bind48282, %struct.ScmObj** %stackaddr$prim56011, align 8
%stackaddr$makeclosure56012 = alloca %struct.ScmObj*, align 8
%fptrToInt56013 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae52185 to i64
%ae52185 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56013)
store volatile %struct.ScmObj* %ae52185, %struct.ScmObj** %stackaddr$makeclosure56012, align 8
%ae52187 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54851$_37foldl1480300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56014 = alloca %struct.ScmObj*, align 8
%argslist54851$_37foldl1480301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48282, %struct.ScmObj* %argslist54851$_37foldl1480300)
store volatile %struct.ScmObj* %argslist54851$_37foldl1480301, %struct.ScmObj** %stackaddr$prim56014, align 8
%stackaddr$prim56015 = alloca %struct.ScmObj*, align 8
%argslist54851$_37foldl1480302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52187, %struct.ScmObj* %argslist54851$_37foldl1480301)
store volatile %struct.ScmObj* %argslist54851$_37foldl1480302, %struct.ScmObj** %stackaddr$prim56015, align 8
%stackaddr$prim56016 = alloca %struct.ScmObj*, align 8
%argslist54851$_37foldl1480303 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48264, %struct.ScmObj* %argslist54851$_37foldl1480302)
store volatile %struct.ScmObj* %argslist54851$_37foldl1480303, %struct.ScmObj** %stackaddr$prim56016, align 8
%stackaddr$prim56017 = alloca %struct.ScmObj*, align 8
%argslist54851$_37foldl1480304 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52185, %struct.ScmObj* %argslist54851$_37foldl1480303)
store volatile %struct.ScmObj* %argslist54851$_37foldl1480304, %struct.ScmObj** %stackaddr$prim56017, align 8
%clofunc56018 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148030)
musttail call tailcc void %clofunc56018(%struct.ScmObj* %_37foldl148030, %struct.ScmObj* %argslist54851$_37foldl1480304)
ret void
}

define tailcc void @proc_clo$ae52185(%struct.ScmObj* %env$ae52185,%struct.ScmObj* %current_45args54847) {
%stackaddr$prim56019 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54847)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim56019, align 8
%stackaddr$prim56020 = alloca %struct.ScmObj*, align 8
%current_45args54848 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54847)
store volatile %struct.ScmObj* %current_45args54848, %struct.ScmObj** %stackaddr$prim56020, align 8
%stackaddr$prim56021 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54848)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim56021, align 8
%stackaddr$prim56022 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim56022, align 8
%argslist54850$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56023 = alloca %struct.ScmObj*, align 8
%argslist54850$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist54850$k0)
store volatile %struct.ScmObj* %argslist54850$k1, %struct.ScmObj** %stackaddr$prim56023, align 8
%clofunc56024 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc56024(%struct.ScmObj* %k, %struct.ScmObj* %argslist54850$k1)
ret void
}

define tailcc void @proc_clo$ae52092(%struct.ScmObj* %env$ae52092,%struct.ScmObj* %el4815248334) {
%stackaddr$prim56025 = alloca %struct.ScmObj*, align 8
%k48335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %el4815248334)
store volatile %struct.ScmObj* %k48335, %struct.ScmObj** %stackaddr$prim56025, align 8
%stackaddr$prim56026 = alloca %struct.ScmObj*, align 8
%el48152 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %el4815248334)
store volatile %struct.ScmObj* %el48152, %struct.ScmObj** %stackaddr$prim56026, align 8
%stackaddr$applyprim56027 = alloca %struct.ScmObj*, align 8
%cpsaprim48336 = call %struct.ScmObj* @applyprim_vector(%struct.ScmObj* %el48152)
store volatile %struct.ScmObj* %cpsaprim48336, %struct.ScmObj** %stackaddr$applyprim56027, align 8
%ae52097 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54856$k483350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56028 = alloca %struct.ScmObj*, align 8
%argslist54856$k483351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim48336, %struct.ScmObj* %argslist54856$k483350)
store volatile %struct.ScmObj* %argslist54856$k483351, %struct.ScmObj** %stackaddr$prim56028, align 8
%stackaddr$prim56029 = alloca %struct.ScmObj*, align 8
%argslist54856$k483352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52097, %struct.ScmObj* %argslist54856$k483351)
store volatile %struct.ScmObj* %argslist54856$k483352, %struct.ScmObj** %stackaddr$prim56029, align 8
%clofunc56030 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48335)
musttail call tailcc void %clofunc56030(%struct.ScmObj* %k48335, %struct.ScmObj* %argslist54856$k483352)
ret void
}

define tailcc void @proc_clo$ae52066(%struct.ScmObj* %env$ae52066,%struct.ScmObj* %el4815148337) {
%stackaddr$prim56031 = alloca %struct.ScmObj*, align 8
%k48338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %el4815148337)
store volatile %struct.ScmObj* %k48338, %struct.ScmObj** %stackaddr$prim56031, align 8
%stackaddr$prim56032 = alloca %struct.ScmObj*, align 8
%el48151 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %el4815148337)
store volatile %struct.ScmObj* %el48151, %struct.ScmObj** %stackaddr$prim56032, align 8
%stackaddr$applyprim56033 = alloca %struct.ScmObj*, align 8
%cpsaprim48339 = call %struct.ScmObj* @applyprim_vector(%struct.ScmObj* %el48151)
store volatile %struct.ScmObj* %cpsaprim48339, %struct.ScmObj** %stackaddr$applyprim56033, align 8
%ae52071 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54859$k483380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56034 = alloca %struct.ScmObj*, align 8
%argslist54859$k483381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim48339, %struct.ScmObj* %argslist54859$k483380)
store volatile %struct.ScmObj* %argslist54859$k483381, %struct.ScmObj** %stackaddr$prim56034, align 8
%stackaddr$prim56035 = alloca %struct.ScmObj*, align 8
%argslist54859$k483382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52071, %struct.ScmObj* %argslist54859$k483381)
store volatile %struct.ScmObj* %argslist54859$k483382, %struct.ScmObj** %stackaddr$prim56035, align 8
%clofunc56036 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48338)
musttail call tailcc void %clofunc56036(%struct.ScmObj* %k48338, %struct.ScmObj* %argslist54859$k483382)
ret void
}

define tailcc void @proc_clo$ae52038(%struct.ScmObj* %env$ae52038,%struct.ScmObj* %lst4815048340) {
%stackaddr$prim56037 = alloca %struct.ScmObj*, align 8
%k48341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4815048340)
store volatile %struct.ScmObj* %k48341, %struct.ScmObj** %stackaddr$prim56037, align 8
%stackaddr$prim56038 = alloca %struct.ScmObj*, align 8
%lst48150 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4815048340)
store volatile %struct.ScmObj* %lst48150, %struct.ScmObj** %stackaddr$prim56038, align 8
%ae52042 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54863$k483410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56039 = alloca %struct.ScmObj*, align 8
%argslist54863$k483411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48150, %struct.ScmObj* %argslist54863$k483410)
store volatile %struct.ScmObj* %argslist54863$k483411, %struct.ScmObj** %stackaddr$prim56039, align 8
%stackaddr$prim56040 = alloca %struct.ScmObj*, align 8
%argslist54863$k483412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52042, %struct.ScmObj* %argslist54863$k483411)
store volatile %struct.ScmObj* %argslist54863$k483412, %struct.ScmObj** %stackaddr$prim56040, align 8
%clofunc56041 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48341)
musttail call tailcc void %clofunc56041(%struct.ScmObj* %k48341, %struct.ScmObj* %argslist54863$k483412)
ret void
}

define tailcc void @proc_clo$ae52014(%struct.ScmObj* %env$ae52014,%struct.ScmObj* %lst4814948342) {
%stackaddr$prim56042 = alloca %struct.ScmObj*, align 8
%k48343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4814948342)
store volatile %struct.ScmObj* %k48343, %struct.ScmObj** %stackaddr$prim56042, align 8
%stackaddr$prim56043 = alloca %struct.ScmObj*, align 8
%lst48149 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4814948342)
store volatile %struct.ScmObj* %lst48149, %struct.ScmObj** %stackaddr$prim56043, align 8
%ae52018 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54866$k483430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56044 = alloca %struct.ScmObj*, align 8
%argslist54866$k483431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48149, %struct.ScmObj* %argslist54866$k483430)
store volatile %struct.ScmObj* %argslist54866$k483431, %struct.ScmObj** %stackaddr$prim56044, align 8
%stackaddr$prim56045 = alloca %struct.ScmObj*, align 8
%argslist54866$k483432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae52018, %struct.ScmObj* %argslist54866$k483431)
store volatile %struct.ScmObj* %argslist54866$k483432, %struct.ScmObj** %stackaddr$prim56045, align 8
%clofunc56046 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48343)
musttail call tailcc void %clofunc56046(%struct.ScmObj* %k48343, %struct.ScmObj* %argslist54866$k483432)
ret void
}

define tailcc void @proc_clo$ae51992(%struct.ScmObj* %env$ae51992,%struct.ScmObj* %lst4814848344) {
%stackaddr$prim56047 = alloca %struct.ScmObj*, align 8
%k48345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4814848344)
store volatile %struct.ScmObj* %k48345, %struct.ScmObj** %stackaddr$prim56047, align 8
%stackaddr$prim56048 = alloca %struct.ScmObj*, align 8
%lst48148 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4814848344)
store volatile %struct.ScmObj* %lst48148, %struct.ScmObj** %stackaddr$prim56048, align 8
%ae51996 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54868$k483450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56049 = alloca %struct.ScmObj*, align 8
%argslist54868$k483451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48148, %struct.ScmObj* %argslist54868$k483450)
store volatile %struct.ScmObj* %argslist54868$k483451, %struct.ScmObj** %stackaddr$prim56049, align 8
%stackaddr$prim56050 = alloca %struct.ScmObj*, align 8
%argslist54868$k483452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51996, %struct.ScmObj* %argslist54868$k483451)
store volatile %struct.ScmObj* %argslist54868$k483452, %struct.ScmObj** %stackaddr$prim56050, align 8
%clofunc56051 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48345)
musttail call tailcc void %clofunc56051(%struct.ScmObj* %k48345, %struct.ScmObj* %argslist54868$k483452)
ret void
}

define tailcc void @proc_clo$ae51968(%struct.ScmObj* %env$ae51968,%struct.ScmObj* %lst4814748346) {
%stackaddr$prim56052 = alloca %struct.ScmObj*, align 8
%k48347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4814748346)
store volatile %struct.ScmObj* %k48347, %struct.ScmObj** %stackaddr$prim56052, align 8
%stackaddr$prim56053 = alloca %struct.ScmObj*, align 8
%lst48147 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4814748346)
store volatile %struct.ScmObj* %lst48147, %struct.ScmObj** %stackaddr$prim56053, align 8
%ae51972 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54871$k483470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56054 = alloca %struct.ScmObj*, align 8
%argslist54871$k483471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48147, %struct.ScmObj* %argslist54871$k483470)
store volatile %struct.ScmObj* %argslist54871$k483471, %struct.ScmObj** %stackaddr$prim56054, align 8
%stackaddr$prim56055 = alloca %struct.ScmObj*, align 8
%argslist54871$k483472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51972, %struct.ScmObj* %argslist54871$k483471)
store volatile %struct.ScmObj* %argslist54871$k483472, %struct.ScmObj** %stackaddr$prim56055, align 8
%clofunc56056 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48347)
musttail call tailcc void %clofunc56056(%struct.ScmObj* %k48347, %struct.ScmObj* %argslist54871$k483472)
ret void
}

define tailcc void @proc_clo$ae51946(%struct.ScmObj* %env$ae51946,%struct.ScmObj* %lst4814648348) {
%stackaddr$prim56057 = alloca %struct.ScmObj*, align 8
%k48349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4814648348)
store volatile %struct.ScmObj* %k48349, %struct.ScmObj** %stackaddr$prim56057, align 8
%stackaddr$prim56058 = alloca %struct.ScmObj*, align 8
%lst48146 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4814648348)
store volatile %struct.ScmObj* %lst48146, %struct.ScmObj** %stackaddr$prim56058, align 8
%ae51950 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54873$k483490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56059 = alloca %struct.ScmObj*, align 8
%argslist54873$k483491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48146, %struct.ScmObj* %argslist54873$k483490)
store volatile %struct.ScmObj* %argslist54873$k483491, %struct.ScmObj** %stackaddr$prim56059, align 8
%stackaddr$prim56060 = alloca %struct.ScmObj*, align 8
%argslist54873$k483492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51950, %struct.ScmObj* %argslist54873$k483491)
store volatile %struct.ScmObj* %argslist54873$k483492, %struct.ScmObj** %stackaddr$prim56060, align 8
%clofunc56061 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48349)
musttail call tailcc void %clofunc56061(%struct.ScmObj* %k48349, %struct.ScmObj* %argslist54873$k483492)
ret void
}

define tailcc void @proc_clo$ae51922(%struct.ScmObj* %env$ae51922,%struct.ScmObj* %arg4814548350) {
%stackaddr$prim56062 = alloca %struct.ScmObj*, align 8
%k48351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %arg4814548350)
store volatile %struct.ScmObj* %k48351, %struct.ScmObj** %stackaddr$prim56062, align 8
%stackaddr$prim56063 = alloca %struct.ScmObj*, align 8
%arg48145 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %arg4814548350)
store volatile %struct.ScmObj* %arg48145, %struct.ScmObj** %stackaddr$prim56063, align 8
%stackaddr$applyprim56064 = alloca %struct.ScmObj*, align 8
%cpsaprim48352 = call %struct.ScmObj* @applyprim_number_63(%struct.ScmObj* %arg48145)
store volatile %struct.ScmObj* %cpsaprim48352, %struct.ScmObj** %stackaddr$applyprim56064, align 8
%ae51927 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54875$k483510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56065 = alloca %struct.ScmObj*, align 8
%argslist54875$k483511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim48352, %struct.ScmObj* %argslist54875$k483510)
store volatile %struct.ScmObj* %argslist54875$k483511, %struct.ScmObj** %stackaddr$prim56065, align 8
%stackaddr$prim56066 = alloca %struct.ScmObj*, align 8
%argslist54875$k483512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51927, %struct.ScmObj* %argslist54875$k483511)
store volatile %struct.ScmObj* %argslist54875$k483512, %struct.ScmObj** %stackaddr$prim56066, align 8
%clofunc56067 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48351)
musttail call tailcc void %clofunc56067(%struct.ScmObj* %k48351, %struct.ScmObj* %argslist54875$k483512)
ret void
}

define tailcc void @proc_clo$ae51887(%struct.ScmObj* %env$ae51887,%struct.ScmObj* %current_45args54877) {
%stackaddr$prim56068 = alloca %struct.ScmObj*, align 8
%k48353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54877)
store volatile %struct.ScmObj* %k48353, %struct.ScmObj** %stackaddr$prim56068, align 8
%stackaddr$prim56069 = alloca %struct.ScmObj*, align 8
%current_45args54878 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54877)
store volatile %struct.ScmObj* %current_45args54878, %struct.ScmObj** %stackaddr$prim56069, align 8
%stackaddr$prim56070 = alloca %struct.ScmObj*, align 8
%b48144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54878)
store volatile %struct.ScmObj* %b48144, %struct.ScmObj** %stackaddr$prim56070, align 8
%truthy$cmp56071 = call i64 @is_truthy_value(%struct.ScmObj* %b48144)
%cmp$cmp56071 = icmp eq i64 %truthy$cmp56071, 1
br i1 %cmp$cmp56071, label %truebranch$cmp56071, label %falsebranch$cmp56071
truebranch$cmp56071:
%ae51890 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51891 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist54880$k483530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56072 = alloca %struct.ScmObj*, align 8
%argslist54880$k483531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51891, %struct.ScmObj* %argslist54880$k483530)
store volatile %struct.ScmObj* %argslist54880$k483531, %struct.ScmObj** %stackaddr$prim56072, align 8
%stackaddr$prim56073 = alloca %struct.ScmObj*, align 8
%argslist54880$k483532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51890, %struct.ScmObj* %argslist54880$k483531)
store volatile %struct.ScmObj* %argslist54880$k483532, %struct.ScmObj** %stackaddr$prim56073, align 8
%clofunc56074 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48353)
musttail call tailcc void %clofunc56074(%struct.ScmObj* %k48353, %struct.ScmObj* %argslist54880$k483532)
ret void
falsebranch$cmp56071:
%ae51899 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51900 = call %struct.ScmObj* @const_init_int(i64 2)
%argslist54881$k483530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56075 = alloca %struct.ScmObj*, align 8
%argslist54881$k483531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51900, %struct.ScmObj* %argslist54881$k483530)
store volatile %struct.ScmObj* %argslist54881$k483531, %struct.ScmObj** %stackaddr$prim56075, align 8
%stackaddr$prim56076 = alloca %struct.ScmObj*, align 8
%argslist54881$k483532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51899, %struct.ScmObj* %argslist54881$k483531)
store volatile %struct.ScmObj* %argslist54881$k483532, %struct.ScmObj** %stackaddr$prim56076, align 8
%clofunc56077 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48353)
musttail call tailcc void %clofunc56077(%struct.ScmObj* %k48353, %struct.ScmObj* %argslist54881$k483532)
ret void
}

define tailcc void @proc_clo$ae51863(%struct.ScmObj* %env$ae51863,%struct.ScmObj* %args4814348354) {
%stackaddr$prim56078 = alloca %struct.ScmObj*, align 8
%k48355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4814348354)
store volatile %struct.ScmObj* %k48355, %struct.ScmObj** %stackaddr$prim56078, align 8
%stackaddr$prim56079 = alloca %struct.ScmObj*, align 8
%args48143 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4814348354)
store volatile %struct.ScmObj* %args48143, %struct.ScmObj** %stackaddr$prim56079, align 8
%stackaddr$applyprim56080 = alloca %struct.ScmObj*, align 8
%cpsaprim48356 = call %struct.ScmObj* @applyprim__43(%struct.ScmObj* %args48143)
store volatile %struct.ScmObj* %cpsaprim48356, %struct.ScmObj** %stackaddr$applyprim56080, align 8
%ae51868 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54883$k483550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56081 = alloca %struct.ScmObj*, align 8
%argslist54883$k483551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim48356, %struct.ScmObj* %argslist54883$k483550)
store volatile %struct.ScmObj* %argslist54883$k483551, %struct.ScmObj** %stackaddr$prim56081, align 8
%stackaddr$prim56082 = alloca %struct.ScmObj*, align 8
%argslist54883$k483552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51868, %struct.ScmObj* %argslist54883$k483551)
store volatile %struct.ScmObj* %argslist54883$k483552, %struct.ScmObj** %stackaddr$prim56082, align 8
%clofunc56083 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48355)
musttail call tailcc void %clofunc56083(%struct.ScmObj* %k48355, %struct.ScmObj* %argslist54883$k483552)
ret void
}

define tailcc void @proc_clo$ae51837(%struct.ScmObj* %env$ae51837,%struct.ScmObj* %current_45args54885) {
%stackaddr$prim56084 = alloca %struct.ScmObj*, align 8
%k48357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54885)
store volatile %struct.ScmObj* %k48357, %struct.ScmObj** %stackaddr$prim56084, align 8
%stackaddr$prim56085 = alloca %struct.ScmObj*, align 8
%current_45args54886 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54885)
store volatile %struct.ScmObj* %current_45args54886, %struct.ScmObj** %stackaddr$prim56085, align 8
%stackaddr$prim56086 = alloca %struct.ScmObj*, align 8
%x48083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54886)
store volatile %struct.ScmObj* %x48083, %struct.ScmObj** %stackaddr$prim56086, align 8
%stackaddr$prim56087 = alloca %struct.ScmObj*, align 8
%anf_45bind48261 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48083)
store volatile %struct.ScmObj* %anf_45bind48261, %struct.ScmObj** %stackaddr$prim56087, align 8
%stackaddr$prim56088 = alloca %struct.ScmObj*, align 8
%anf_45bind48262 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48261)
store volatile %struct.ScmObj* %anf_45bind48262, %struct.ScmObj** %stackaddr$prim56088, align 8
%stackaddr$prim56089 = alloca %struct.ScmObj*, align 8
%anf_45bind48263 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48262)
store volatile %struct.ScmObj* %anf_45bind48263, %struct.ScmObj** %stackaddr$prim56089, align 8
%stackaddr$prim56090 = alloca %struct.ScmObj*, align 8
%cpsprim48358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48263)
store volatile %struct.ScmObj* %cpsprim48358, %struct.ScmObj** %stackaddr$prim56090, align 8
%ae51843 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54888$k483570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56091 = alloca %struct.ScmObj*, align 8
%argslist54888$k483571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48358, %struct.ScmObj* %argslist54888$k483570)
store volatile %struct.ScmObj* %argslist54888$k483571, %struct.ScmObj** %stackaddr$prim56091, align 8
%stackaddr$prim56092 = alloca %struct.ScmObj*, align 8
%argslist54888$k483572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51843, %struct.ScmObj* %argslist54888$k483571)
store volatile %struct.ScmObj* %argslist54888$k483572, %struct.ScmObj** %stackaddr$prim56092, align 8
%clofunc56093 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48357)
musttail call tailcc void %clofunc56093(%struct.ScmObj* %k48357, %struct.ScmObj* %argslist54888$k483572)
ret void
}

define tailcc void @proc_clo$ae51813(%struct.ScmObj* %env$ae51813,%struct.ScmObj* %current_45args54890) {
%stackaddr$prim56094 = alloca %struct.ScmObj*, align 8
%k48359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54890)
store volatile %struct.ScmObj* %k48359, %struct.ScmObj** %stackaddr$prim56094, align 8
%stackaddr$prim56095 = alloca %struct.ScmObj*, align 8
%current_45args54891 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54890)
store volatile %struct.ScmObj* %current_45args54891, %struct.ScmObj** %stackaddr$prim56095, align 8
%stackaddr$prim56096 = alloca %struct.ScmObj*, align 8
%x48085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54891)
store volatile %struct.ScmObj* %x48085, %struct.ScmObj** %stackaddr$prim56096, align 8
%stackaddr$prim56097 = alloca %struct.ScmObj*, align 8
%anf_45bind48259 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48085)
store volatile %struct.ScmObj* %anf_45bind48259, %struct.ScmObj** %stackaddr$prim56097, align 8
%stackaddr$prim56098 = alloca %struct.ScmObj*, align 8
%anf_45bind48260 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48259)
store volatile %struct.ScmObj* %anf_45bind48260, %struct.ScmObj** %stackaddr$prim56098, align 8
%stackaddr$prim56099 = alloca %struct.ScmObj*, align 8
%cpsprim48360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48260)
store volatile %struct.ScmObj* %cpsprim48360, %struct.ScmObj** %stackaddr$prim56099, align 8
%ae51818 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54893$k483590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56100 = alloca %struct.ScmObj*, align 8
%argslist54893$k483591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48360, %struct.ScmObj* %argslist54893$k483590)
store volatile %struct.ScmObj* %argslist54893$k483591, %struct.ScmObj** %stackaddr$prim56100, align 8
%stackaddr$prim56101 = alloca %struct.ScmObj*, align 8
%argslist54893$k483592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51818, %struct.ScmObj* %argslist54893$k483591)
store volatile %struct.ScmObj* %argslist54893$k483592, %struct.ScmObj** %stackaddr$prim56101, align 8
%clofunc56102 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48359)
musttail call tailcc void %clofunc56102(%struct.ScmObj* %k48359, %struct.ScmObj* %argslist54893$k483592)
ret void
}

define tailcc void @proc_clo$ae51791(%struct.ScmObj* %env$ae51791,%struct.ScmObj* %current_45args54895) {
%stackaddr$prim56103 = alloca %struct.ScmObj*, align 8
%k48361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54895)
store volatile %struct.ScmObj* %k48361, %struct.ScmObj** %stackaddr$prim56103, align 8
%stackaddr$prim56104 = alloca %struct.ScmObj*, align 8
%current_45args54896 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54895)
store volatile %struct.ScmObj* %current_45args54896, %struct.ScmObj** %stackaddr$prim56104, align 8
%stackaddr$prim56105 = alloca %struct.ScmObj*, align 8
%x48087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54896)
store volatile %struct.ScmObj* %x48087, %struct.ScmObj** %stackaddr$prim56105, align 8
%stackaddr$prim56106 = alloca %struct.ScmObj*, align 8
%anf_45bind48258 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48087)
store volatile %struct.ScmObj* %anf_45bind48258, %struct.ScmObj** %stackaddr$prim56106, align 8
%stackaddr$prim56107 = alloca %struct.ScmObj*, align 8
%cpsprim48362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48258)
store volatile %struct.ScmObj* %cpsprim48362, %struct.ScmObj** %stackaddr$prim56107, align 8
%ae51795 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54898$k483610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56108 = alloca %struct.ScmObj*, align 8
%argslist54898$k483611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48362, %struct.ScmObj* %argslist54898$k483610)
store volatile %struct.ScmObj* %argslist54898$k483611, %struct.ScmObj** %stackaddr$prim56108, align 8
%stackaddr$prim56109 = alloca %struct.ScmObj*, align 8
%argslist54898$k483612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51795, %struct.ScmObj* %argslist54898$k483611)
store volatile %struct.ScmObj* %argslist54898$k483612, %struct.ScmObj** %stackaddr$prim56109, align 8
%clofunc56110 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48361)
musttail call tailcc void %clofunc56110(%struct.ScmObj* %k48361, %struct.ScmObj* %argslist54898$k483612)
ret void
}

define tailcc void @proc_clo$ae51771(%struct.ScmObj* %env$ae51771,%struct.ScmObj* %current_45args54900) {
%stackaddr$prim56111 = alloca %struct.ScmObj*, align 8
%k48363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54900)
store volatile %struct.ScmObj* %k48363, %struct.ScmObj** %stackaddr$prim56111, align 8
%stackaddr$prim56112 = alloca %struct.ScmObj*, align 8
%current_45args54901 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54900)
store volatile %struct.ScmObj* %current_45args54901, %struct.ScmObj** %stackaddr$prim56112, align 8
%stackaddr$prim56113 = alloca %struct.ScmObj*, align 8
%x48089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54901)
store volatile %struct.ScmObj* %x48089, %struct.ScmObj** %stackaddr$prim56113, align 8
%stackaddr$prim56114 = alloca %struct.ScmObj*, align 8
%cpsprim48364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48089)
store volatile %struct.ScmObj* %cpsprim48364, %struct.ScmObj** %stackaddr$prim56114, align 8
%ae51774 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54903$k483630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56115 = alloca %struct.ScmObj*, align 8
%argslist54903$k483631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48364, %struct.ScmObj* %argslist54903$k483630)
store volatile %struct.ScmObj* %argslist54903$k483631, %struct.ScmObj** %stackaddr$prim56115, align 8
%stackaddr$prim56116 = alloca %struct.ScmObj*, align 8
%argslist54903$k483632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51774, %struct.ScmObj* %argslist54903$k483631)
store volatile %struct.ScmObj* %argslist54903$k483632, %struct.ScmObj** %stackaddr$prim56116, align 8
%clofunc56117 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48363)
musttail call tailcc void %clofunc56117(%struct.ScmObj* %k48363, %struct.ScmObj* %argslist54903$k483632)
ret void
}

define tailcc void @proc_clo$ae51673(%struct.ScmObj* %env$ae51673,%struct.ScmObj* %args4809148365) {
%stackaddr$env-ref56118 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51673, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref56118
%stackaddr$prim56119 = alloca %struct.ScmObj*, align 8
%k48366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4809148365)
store volatile %struct.ScmObj* %k48366, %struct.ScmObj** %stackaddr$prim56119, align 8
%stackaddr$prim56120 = alloca %struct.ScmObj*, align 8
%args48091 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4809148365)
store volatile %struct.ScmObj* %args48091, %struct.ScmObj** %stackaddr$prim56120, align 8
%stackaddr$prim56121 = alloca %struct.ScmObj*, align 8
%anf_45bind48252 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %anf_45bind48252, %struct.ScmObj** %stackaddr$prim56121, align 8
%truthy$cmp56122 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48252)
%cmp$cmp56122 = icmp eq i64 %truthy$cmp56122, 1
br i1 %cmp$cmp56122, label %truebranch$cmp56122, label %falsebranch$cmp56122
truebranch$cmp56122:
%ae51679 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51680 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist54905$k483660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56123 = alloca %struct.ScmObj*, align 8
%argslist54905$k483661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51680, %struct.ScmObj* %argslist54905$k483660)
store volatile %struct.ScmObj* %argslist54905$k483661, %struct.ScmObj** %stackaddr$prim56123, align 8
%stackaddr$prim56124 = alloca %struct.ScmObj*, align 8
%argslist54905$k483662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51679, %struct.ScmObj* %argslist54905$k483661)
store volatile %struct.ScmObj* %argslist54905$k483662, %struct.ScmObj** %stackaddr$prim56124, align 8
%clofunc56125 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48366)
musttail call tailcc void %clofunc56125(%struct.ScmObj* %k48366, %struct.ScmObj* %argslist54905$k483662)
ret void
falsebranch$cmp56122:
%stackaddr$prim56126 = alloca %struct.ScmObj*, align 8
%anf_45bind48253 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %anf_45bind48253, %struct.ScmObj** %stackaddr$prim56126, align 8
%stackaddr$prim56127 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48253)
store volatile %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$prim56127, align 8
%truthy$cmp56128 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48254)
%cmp$cmp56128 = icmp eq i64 %truthy$cmp56128, 1
br i1 %cmp$cmp56128, label %truebranch$cmp56128, label %falsebranch$cmp56128
truebranch$cmp56128:
%stackaddr$prim56129 = alloca %struct.ScmObj*, align 8
%cpsprim48367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %cpsprim48367, %struct.ScmObj** %stackaddr$prim56129, align 8
%ae51692 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54906$k483660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56130 = alloca %struct.ScmObj*, align 8
%argslist54906$k483661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48367, %struct.ScmObj* %argslist54906$k483660)
store volatile %struct.ScmObj* %argslist54906$k483661, %struct.ScmObj** %stackaddr$prim56130, align 8
%stackaddr$prim56131 = alloca %struct.ScmObj*, align 8
%argslist54906$k483662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51692, %struct.ScmObj* %argslist54906$k483661)
store volatile %struct.ScmObj* %argslist54906$k483662, %struct.ScmObj** %stackaddr$prim56131, align 8
%clofunc56132 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48366)
musttail call tailcc void %clofunc56132(%struct.ScmObj* %k48366, %struct.ScmObj* %argslist54906$k483662)
ret void
falsebranch$cmp56128:
%stackaddr$makeclosure56133 = alloca %struct.ScmObj*, align 8
%fptrToInt56134 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51697 to i64
%ae51697 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56134)
store volatile %struct.ScmObj* %ae51697, %struct.ScmObj** %stackaddr$makeclosure56133, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51697, %struct.ScmObj* %k48366, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51697, %struct.ScmObj* %_37foldl148030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51697, %struct.ScmObj* %args48091, i64 2)
%ae51698 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56135 = alloca %struct.ScmObj*, align 8
%fptrToInt56136 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51699 to i64
%ae51699 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56136)
store volatile %struct.ScmObj* %ae51699, %struct.ScmObj** %stackaddr$makeclosure56135, align 8
%argslist54916$ae516970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56137 = alloca %struct.ScmObj*, align 8
%argslist54916$ae516971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51699, %struct.ScmObj* %argslist54916$ae516970)
store volatile %struct.ScmObj* %argslist54916$ae516971, %struct.ScmObj** %stackaddr$prim56137, align 8
%stackaddr$prim56138 = alloca %struct.ScmObj*, align 8
%argslist54916$ae516972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51698, %struct.ScmObj* %argslist54916$ae516971)
store volatile %struct.ScmObj* %argslist54916$ae516972, %struct.ScmObj** %stackaddr$prim56138, align 8
%clofunc56139 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51697)
musttail call tailcc void %clofunc56139(%struct.ScmObj* %ae51697, %struct.ScmObj* %argslist54916$ae516972)
ret void
}

define tailcc void @proc_clo$ae51697(%struct.ScmObj* %env$ae51697,%struct.ScmObj* %current_45args54907) {
%stackaddr$env-ref56140 = alloca %struct.ScmObj*, align 8
%k48366 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51697, i64 0)
store %struct.ScmObj* %k48366, %struct.ScmObj** %stackaddr$env-ref56140
%stackaddr$env-ref56141 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51697, i64 1)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref56141
%stackaddr$env-ref56142 = alloca %struct.ScmObj*, align 8
%args48091 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51697, i64 2)
store %struct.ScmObj* %args48091, %struct.ScmObj** %stackaddr$env-ref56142
%stackaddr$prim56143 = alloca %struct.ScmObj*, align 8
%_95k48368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54907)
store volatile %struct.ScmObj* %_95k48368, %struct.ScmObj** %stackaddr$prim56143, align 8
%stackaddr$prim56144 = alloca %struct.ScmObj*, align 8
%current_45args54908 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54907)
store volatile %struct.ScmObj* %current_45args54908, %struct.ScmObj** %stackaddr$prim56144, align 8
%stackaddr$prim56145 = alloca %struct.ScmObj*, align 8
%anf_45bind48255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54908)
store volatile %struct.ScmObj* %anf_45bind48255, %struct.ScmObj** %stackaddr$prim56145, align 8
%stackaddr$prim56146 = alloca %struct.ScmObj*, align 8
%anf_45bind48256 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %anf_45bind48256, %struct.ScmObj** %stackaddr$prim56146, align 8
%stackaddr$prim56147 = alloca %struct.ScmObj*, align 8
%anf_45bind48257 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48091)
store volatile %struct.ScmObj* %anf_45bind48257, %struct.ScmObj** %stackaddr$prim56147, align 8
%argslist54910$_37foldl1480300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56148 = alloca %struct.ScmObj*, align 8
%argslist54910$_37foldl1480301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48257, %struct.ScmObj* %argslist54910$_37foldl1480300)
store volatile %struct.ScmObj* %argslist54910$_37foldl1480301, %struct.ScmObj** %stackaddr$prim56148, align 8
%stackaddr$prim56149 = alloca %struct.ScmObj*, align 8
%argslist54910$_37foldl1480302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48256, %struct.ScmObj* %argslist54910$_37foldl1480301)
store volatile %struct.ScmObj* %argslist54910$_37foldl1480302, %struct.ScmObj** %stackaddr$prim56149, align 8
%stackaddr$prim56150 = alloca %struct.ScmObj*, align 8
%argslist54910$_37foldl1480303 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48255, %struct.ScmObj* %argslist54910$_37foldl1480302)
store volatile %struct.ScmObj* %argslist54910$_37foldl1480303, %struct.ScmObj** %stackaddr$prim56150, align 8
%stackaddr$prim56151 = alloca %struct.ScmObj*, align 8
%argslist54910$_37foldl1480304 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48366, %struct.ScmObj* %argslist54910$_37foldl1480303)
store volatile %struct.ScmObj* %argslist54910$_37foldl1480304, %struct.ScmObj** %stackaddr$prim56151, align 8
%clofunc56152 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148030)
musttail call tailcc void %clofunc56152(%struct.ScmObj* %_37foldl148030, %struct.ScmObj* %argslist54910$_37foldl1480304)
ret void
}

define tailcc void @proc_clo$ae51699(%struct.ScmObj* %env$ae51699,%struct.ScmObj* %current_45args54911) {
%stackaddr$prim56153 = alloca %struct.ScmObj*, align 8
%k48369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54911)
store volatile %struct.ScmObj* %k48369, %struct.ScmObj** %stackaddr$prim56153, align 8
%stackaddr$prim56154 = alloca %struct.ScmObj*, align 8
%current_45args54912 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54911)
store volatile %struct.ScmObj* %current_45args54912, %struct.ScmObj** %stackaddr$prim56154, align 8
%stackaddr$prim56155 = alloca %struct.ScmObj*, align 8
%n48093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54912)
store volatile %struct.ScmObj* %n48093, %struct.ScmObj** %stackaddr$prim56155, align 8
%stackaddr$prim56156 = alloca %struct.ScmObj*, align 8
%current_45args54913 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54912)
store volatile %struct.ScmObj* %current_45args54913, %struct.ScmObj** %stackaddr$prim56156, align 8
%stackaddr$prim56157 = alloca %struct.ScmObj*, align 8
%v48092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54913)
store volatile %struct.ScmObj* %v48092, %struct.ScmObj** %stackaddr$prim56157, align 8
%stackaddr$prim56158 = alloca %struct.ScmObj*, align 8
%cpsprim48370 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v48092, %struct.ScmObj* %n48093)
store volatile %struct.ScmObj* %cpsprim48370, %struct.ScmObj** %stackaddr$prim56158, align 8
%ae51703 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54915$k483690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56159 = alloca %struct.ScmObj*, align 8
%argslist54915$k483691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48370, %struct.ScmObj* %argslist54915$k483690)
store volatile %struct.ScmObj* %argslist54915$k483691, %struct.ScmObj** %stackaddr$prim56159, align 8
%stackaddr$prim56160 = alloca %struct.ScmObj*, align 8
%argslist54915$k483692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51703, %struct.ScmObj* %argslist54915$k483691)
store volatile %struct.ScmObj* %argslist54915$k483692, %struct.ScmObj** %stackaddr$prim56160, align 8
%clofunc56161 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48369)
musttail call tailcc void %clofunc56161(%struct.ScmObj* %k48369, %struct.ScmObj* %argslist54915$k483692)
ret void
}

define tailcc void @proc_clo$ae51269(%struct.ScmObj* %env$ae51269,%struct.ScmObj* %current_45args54918) {
%stackaddr$prim56162 = alloca %struct.ScmObj*, align 8
%k48371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54918)
store volatile %struct.ScmObj* %k48371, %struct.ScmObj** %stackaddr$prim56162, align 8
%stackaddr$prim56163 = alloca %struct.ScmObj*, align 8
%current_45args54919 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54918)
store volatile %struct.ScmObj* %current_45args54919, %struct.ScmObj** %stackaddr$prim56163, align 8
%stackaddr$prim56164 = alloca %struct.ScmObj*, align 8
%v48096 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54919)
store volatile %struct.ScmObj* %v48096, %struct.ScmObj** %stackaddr$prim56164, align 8
%stackaddr$prim56165 = alloca %struct.ScmObj*, align 8
%current_45args54920 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54919)
store volatile %struct.ScmObj* %current_45args54920, %struct.ScmObj** %stackaddr$prim56165, align 8
%stackaddr$prim56166 = alloca %struct.ScmObj*, align 8
%lst48095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54920)
store volatile %struct.ScmObj* %lst48095, %struct.ScmObj** %stackaddr$prim56166, align 8
%ae51270 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56167 = alloca %struct.ScmObj*, align 8
%lst48097 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51270, %struct.ScmObj* %lst48095)
store volatile %struct.ScmObj* %lst48097, %struct.ScmObj** %stackaddr$prim56167, align 8
%stackaddr$makeclosure56168 = alloca %struct.ScmObj*, align 8
%fptrToInt56169 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51272 to i64
%ae51272 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56169)
store volatile %struct.ScmObj* %ae51272, %struct.ScmObj** %stackaddr$makeclosure56168, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51272, %struct.ScmObj* %k48371, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51272, %struct.ScmObj* %lst48097, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51272, %struct.ScmObj* %v48096, i64 2)
%ae51273 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56170 = alloca %struct.ScmObj*, align 8
%fptrToInt56171 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51274 to i64
%ae51274 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56171)
store volatile %struct.ScmObj* %ae51274, %struct.ScmObj** %stackaddr$makeclosure56170, align 8
%argslist54942$ae512720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56172 = alloca %struct.ScmObj*, align 8
%argslist54942$ae512721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51274, %struct.ScmObj* %argslist54942$ae512720)
store volatile %struct.ScmObj* %argslist54942$ae512721, %struct.ScmObj** %stackaddr$prim56172, align 8
%stackaddr$prim56173 = alloca %struct.ScmObj*, align 8
%argslist54942$ae512722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51273, %struct.ScmObj* %argslist54942$ae512721)
store volatile %struct.ScmObj* %argslist54942$ae512722, %struct.ScmObj** %stackaddr$prim56173, align 8
%clofunc56174 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51272)
musttail call tailcc void %clofunc56174(%struct.ScmObj* %ae51272, %struct.ScmObj* %argslist54942$ae512722)
ret void
}

define tailcc void @proc_clo$ae51272(%struct.ScmObj* %env$ae51272,%struct.ScmObj* %current_45args54922) {
%stackaddr$env-ref56175 = alloca %struct.ScmObj*, align 8
%k48371 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51272, i64 0)
store %struct.ScmObj* %k48371, %struct.ScmObj** %stackaddr$env-ref56175
%stackaddr$env-ref56176 = alloca %struct.ScmObj*, align 8
%lst48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51272, i64 1)
store %struct.ScmObj* %lst48097, %struct.ScmObj** %stackaddr$env-ref56176
%stackaddr$env-ref56177 = alloca %struct.ScmObj*, align 8
%v48096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51272, i64 2)
store %struct.ScmObj* %v48096, %struct.ScmObj** %stackaddr$env-ref56177
%stackaddr$prim56178 = alloca %struct.ScmObj*, align 8
%_95k48372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54922)
store volatile %struct.ScmObj* %_95k48372, %struct.ScmObj** %stackaddr$prim56178, align 8
%stackaddr$prim56179 = alloca %struct.ScmObj*, align 8
%current_45args54923 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54922)
store volatile %struct.ScmObj* %current_45args54923, %struct.ScmObj** %stackaddr$prim56179, align 8
%stackaddr$prim56180 = alloca %struct.ScmObj*, align 8
%anf_45bind48244 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54923)
store volatile %struct.ScmObj* %anf_45bind48244, %struct.ScmObj** %stackaddr$prim56180, align 8
%stackaddr$makeclosure56181 = alloca %struct.ScmObj*, align 8
%fptrToInt56182 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51288 to i64
%ae51288 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56182)
store volatile %struct.ScmObj* %ae51288, %struct.ScmObj** %stackaddr$makeclosure56181, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51288, %struct.ScmObj* %k48371, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51288, %struct.ScmObj* %lst48097, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51288, %struct.ScmObj* %v48096, i64 2)
%stackaddr$makeclosure56183 = alloca %struct.ScmObj*, align 8
%fptrToInt56184 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51289 to i64
%ae51289 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56184)
store volatile %struct.ScmObj* %ae51289, %struct.ScmObj** %stackaddr$makeclosure56183, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51289, %struct.ScmObj* %k48371, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51289, %struct.ScmObj* %lst48097, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51289, %struct.ScmObj* %v48096, i64 2)
%argslist54937$anf_45bind482440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56185 = alloca %struct.ScmObj*, align 8
%argslist54937$anf_45bind482441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51289, %struct.ScmObj* %argslist54937$anf_45bind482440)
store volatile %struct.ScmObj* %argslist54937$anf_45bind482441, %struct.ScmObj** %stackaddr$prim56185, align 8
%stackaddr$prim56186 = alloca %struct.ScmObj*, align 8
%argslist54937$anf_45bind482442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51288, %struct.ScmObj* %argslist54937$anf_45bind482441)
store volatile %struct.ScmObj* %argslist54937$anf_45bind482442, %struct.ScmObj** %stackaddr$prim56186, align 8
%clofunc56187 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48244)
musttail call tailcc void %clofunc56187(%struct.ScmObj* %anf_45bind48244, %struct.ScmObj* %argslist54937$anf_45bind482442)
ret void
}

define tailcc void @proc_clo$ae51288(%struct.ScmObj* %env$ae51288,%struct.ScmObj* %current_45args54925) {
%stackaddr$env-ref56188 = alloca %struct.ScmObj*, align 8
%k48371 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51288, i64 0)
store %struct.ScmObj* %k48371, %struct.ScmObj** %stackaddr$env-ref56188
%stackaddr$env-ref56189 = alloca %struct.ScmObj*, align 8
%lst48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51288, i64 1)
store %struct.ScmObj* %lst48097, %struct.ScmObj** %stackaddr$env-ref56189
%stackaddr$env-ref56190 = alloca %struct.ScmObj*, align 8
%v48096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51288, i64 2)
store %struct.ScmObj* %v48096, %struct.ScmObj** %stackaddr$env-ref56190
%stackaddr$prim56191 = alloca %struct.ScmObj*, align 8
%_95k48373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54925)
store volatile %struct.ScmObj* %_95k48373, %struct.ScmObj** %stackaddr$prim56191, align 8
%stackaddr$prim56192 = alloca %struct.ScmObj*, align 8
%current_45args54926 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54925)
store volatile %struct.ScmObj* %current_45args54926, %struct.ScmObj** %stackaddr$prim56192, align 8
%stackaddr$prim56193 = alloca %struct.ScmObj*, align 8
%cc48098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54926)
store volatile %struct.ScmObj* %cc48098, %struct.ScmObj** %stackaddr$prim56193, align 8
%ae51397 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56194 = alloca %struct.ScmObj*, align 8
%anf_45bind48245 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51397)
store volatile %struct.ScmObj* %anf_45bind48245, %struct.ScmObj** %stackaddr$prim56194, align 8
%stackaddr$prim56195 = alloca %struct.ScmObj*, align 8
%anf_45bind48246 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48245)
store volatile %struct.ScmObj* %anf_45bind48246, %struct.ScmObj** %stackaddr$prim56195, align 8
%truthy$cmp56196 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48246)
%cmp$cmp56196 = icmp eq i64 %truthy$cmp56196, 1
br i1 %cmp$cmp56196, label %truebranch$cmp56196, label %falsebranch$cmp56196
truebranch$cmp56196:
%ae51401 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51402 = call %struct.ScmObj* @const_init_false()
%argslist54928$k483710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56197 = alloca %struct.ScmObj*, align 8
%argslist54928$k483711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51402, %struct.ScmObj* %argslist54928$k483710)
store volatile %struct.ScmObj* %argslist54928$k483711, %struct.ScmObj** %stackaddr$prim56197, align 8
%stackaddr$prim56198 = alloca %struct.ScmObj*, align 8
%argslist54928$k483712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51401, %struct.ScmObj* %argslist54928$k483711)
store volatile %struct.ScmObj* %argslist54928$k483712, %struct.ScmObj** %stackaddr$prim56198, align 8
%clofunc56199 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48371)
musttail call tailcc void %clofunc56199(%struct.ScmObj* %k48371, %struct.ScmObj* %argslist54928$k483712)
ret void
falsebranch$cmp56196:
%ae51410 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56200 = alloca %struct.ScmObj*, align 8
%anf_45bind48247 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51410)
store volatile %struct.ScmObj* %anf_45bind48247, %struct.ScmObj** %stackaddr$prim56200, align 8
%stackaddr$prim56201 = alloca %struct.ScmObj*, align 8
%anf_45bind48248 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48247)
store volatile %struct.ScmObj* %anf_45bind48248, %struct.ScmObj** %stackaddr$prim56201, align 8
%stackaddr$prim56202 = alloca %struct.ScmObj*, align 8
%anf_45bind48249 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48248, %struct.ScmObj* %v48096)
store volatile %struct.ScmObj* %anf_45bind48249, %struct.ScmObj** %stackaddr$prim56202, align 8
%truthy$cmp56203 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48249)
%cmp$cmp56203 = icmp eq i64 %truthy$cmp56203, 1
br i1 %cmp$cmp56203, label %truebranch$cmp56203, label %falsebranch$cmp56203
truebranch$cmp56203:
%ae51416 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56204 = alloca %struct.ScmObj*, align 8
%cpsprim48374 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51416)
store volatile %struct.ScmObj* %cpsprim48374, %struct.ScmObj** %stackaddr$prim56204, align 8
%ae51418 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54929$k483710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56205 = alloca %struct.ScmObj*, align 8
%argslist54929$k483711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48374, %struct.ScmObj* %argslist54929$k483710)
store volatile %struct.ScmObj* %argslist54929$k483711, %struct.ScmObj** %stackaddr$prim56205, align 8
%stackaddr$prim56206 = alloca %struct.ScmObj*, align 8
%argslist54929$k483712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51418, %struct.ScmObj* %argslist54929$k483711)
store volatile %struct.ScmObj* %argslist54929$k483712, %struct.ScmObj** %stackaddr$prim56206, align 8
%clofunc56207 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48371)
musttail call tailcc void %clofunc56207(%struct.ScmObj* %k48371, %struct.ScmObj* %argslist54929$k483712)
ret void
falsebranch$cmp56203:
%ae51429 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56208 = alloca %struct.ScmObj*, align 8
%anf_45bind48250 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51429)
store volatile %struct.ScmObj* %anf_45bind48250, %struct.ScmObj** %stackaddr$prim56208, align 8
%stackaddr$prim56209 = alloca %struct.ScmObj*, align 8
%anf_45bind48251 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48250)
store volatile %struct.ScmObj* %anf_45bind48251, %struct.ScmObj** %stackaddr$prim56209, align 8
%ae51432 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56210 = alloca %struct.ScmObj*, align 8
%_95048100 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51432, %struct.ScmObj* %anf_45bind48251)
store volatile %struct.ScmObj* %_95048100, %struct.ScmObj** %stackaddr$prim56210, align 8
%argslist54930$cc480980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56211 = alloca %struct.ScmObj*, align 8
%argslist54930$cc480981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48098, %struct.ScmObj* %argslist54930$cc480980)
store volatile %struct.ScmObj* %argslist54930$cc480981, %struct.ScmObj** %stackaddr$prim56211, align 8
%stackaddr$prim56212 = alloca %struct.ScmObj*, align 8
%argslist54930$cc480982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48371, %struct.ScmObj* %argslist54930$cc480981)
store volatile %struct.ScmObj* %argslist54930$cc480982, %struct.ScmObj** %stackaddr$prim56212, align 8
%clofunc56213 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48098)
musttail call tailcc void %clofunc56213(%struct.ScmObj* %cc48098, %struct.ScmObj* %argslist54930$cc480982)
ret void
}

define tailcc void @proc_clo$ae51289(%struct.ScmObj* %env$ae51289,%struct.ScmObj* %current_45args54931) {
%stackaddr$env-ref56214 = alloca %struct.ScmObj*, align 8
%k48371 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51289, i64 0)
store %struct.ScmObj* %k48371, %struct.ScmObj** %stackaddr$env-ref56214
%stackaddr$env-ref56215 = alloca %struct.ScmObj*, align 8
%lst48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51289, i64 1)
store %struct.ScmObj* %lst48097, %struct.ScmObj** %stackaddr$env-ref56215
%stackaddr$env-ref56216 = alloca %struct.ScmObj*, align 8
%v48096 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51289, i64 2)
store %struct.ScmObj* %v48096, %struct.ScmObj** %stackaddr$env-ref56216
%stackaddr$prim56217 = alloca %struct.ScmObj*, align 8
%_95k48373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54931)
store volatile %struct.ScmObj* %_95k48373, %struct.ScmObj** %stackaddr$prim56217, align 8
%stackaddr$prim56218 = alloca %struct.ScmObj*, align 8
%current_45args54932 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54931)
store volatile %struct.ScmObj* %current_45args54932, %struct.ScmObj** %stackaddr$prim56218, align 8
%stackaddr$prim56219 = alloca %struct.ScmObj*, align 8
%cc48098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54932)
store volatile %struct.ScmObj* %cc48098, %struct.ScmObj** %stackaddr$prim56219, align 8
%ae51291 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56220 = alloca %struct.ScmObj*, align 8
%anf_45bind48245 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51291)
store volatile %struct.ScmObj* %anf_45bind48245, %struct.ScmObj** %stackaddr$prim56220, align 8
%stackaddr$prim56221 = alloca %struct.ScmObj*, align 8
%anf_45bind48246 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48245)
store volatile %struct.ScmObj* %anf_45bind48246, %struct.ScmObj** %stackaddr$prim56221, align 8
%truthy$cmp56222 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48246)
%cmp$cmp56222 = icmp eq i64 %truthy$cmp56222, 1
br i1 %cmp$cmp56222, label %truebranch$cmp56222, label %falsebranch$cmp56222
truebranch$cmp56222:
%ae51295 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51296 = call %struct.ScmObj* @const_init_false()
%argslist54934$k483710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56223 = alloca %struct.ScmObj*, align 8
%argslist54934$k483711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51296, %struct.ScmObj* %argslist54934$k483710)
store volatile %struct.ScmObj* %argslist54934$k483711, %struct.ScmObj** %stackaddr$prim56223, align 8
%stackaddr$prim56224 = alloca %struct.ScmObj*, align 8
%argslist54934$k483712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51295, %struct.ScmObj* %argslist54934$k483711)
store volatile %struct.ScmObj* %argslist54934$k483712, %struct.ScmObj** %stackaddr$prim56224, align 8
%clofunc56225 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48371)
musttail call tailcc void %clofunc56225(%struct.ScmObj* %k48371, %struct.ScmObj* %argslist54934$k483712)
ret void
falsebranch$cmp56222:
%ae51304 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56226 = alloca %struct.ScmObj*, align 8
%anf_45bind48247 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51304)
store volatile %struct.ScmObj* %anf_45bind48247, %struct.ScmObj** %stackaddr$prim56226, align 8
%stackaddr$prim56227 = alloca %struct.ScmObj*, align 8
%anf_45bind48248 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48247)
store volatile %struct.ScmObj* %anf_45bind48248, %struct.ScmObj** %stackaddr$prim56227, align 8
%stackaddr$prim56228 = alloca %struct.ScmObj*, align 8
%anf_45bind48249 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48248, %struct.ScmObj* %v48096)
store volatile %struct.ScmObj* %anf_45bind48249, %struct.ScmObj** %stackaddr$prim56228, align 8
%truthy$cmp56229 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48249)
%cmp$cmp56229 = icmp eq i64 %truthy$cmp56229, 1
br i1 %cmp$cmp56229, label %truebranch$cmp56229, label %falsebranch$cmp56229
truebranch$cmp56229:
%ae51310 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56230 = alloca %struct.ScmObj*, align 8
%cpsprim48374 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51310)
store volatile %struct.ScmObj* %cpsprim48374, %struct.ScmObj** %stackaddr$prim56230, align 8
%ae51312 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54935$k483710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56231 = alloca %struct.ScmObj*, align 8
%argslist54935$k483711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48374, %struct.ScmObj* %argslist54935$k483710)
store volatile %struct.ScmObj* %argslist54935$k483711, %struct.ScmObj** %stackaddr$prim56231, align 8
%stackaddr$prim56232 = alloca %struct.ScmObj*, align 8
%argslist54935$k483712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51312, %struct.ScmObj* %argslist54935$k483711)
store volatile %struct.ScmObj* %argslist54935$k483712, %struct.ScmObj** %stackaddr$prim56232, align 8
%clofunc56233 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48371)
musttail call tailcc void %clofunc56233(%struct.ScmObj* %k48371, %struct.ScmObj* %argslist54935$k483712)
ret void
falsebranch$cmp56229:
%ae51323 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56234 = alloca %struct.ScmObj*, align 8
%anf_45bind48250 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51323)
store volatile %struct.ScmObj* %anf_45bind48250, %struct.ScmObj** %stackaddr$prim56234, align 8
%stackaddr$prim56235 = alloca %struct.ScmObj*, align 8
%anf_45bind48251 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48250)
store volatile %struct.ScmObj* %anf_45bind48251, %struct.ScmObj** %stackaddr$prim56235, align 8
%ae51326 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56236 = alloca %struct.ScmObj*, align 8
%_95048100 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48097, %struct.ScmObj* %ae51326, %struct.ScmObj* %anf_45bind48251)
store volatile %struct.ScmObj* %_95048100, %struct.ScmObj** %stackaddr$prim56236, align 8
%argslist54936$cc480980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56237 = alloca %struct.ScmObj*, align 8
%argslist54936$cc480981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48098, %struct.ScmObj* %argslist54936$cc480980)
store volatile %struct.ScmObj* %argslist54936$cc480981, %struct.ScmObj** %stackaddr$prim56237, align 8
%stackaddr$prim56238 = alloca %struct.ScmObj*, align 8
%argslist54936$cc480982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48371, %struct.ScmObj* %argslist54936$cc480981)
store volatile %struct.ScmObj* %argslist54936$cc480982, %struct.ScmObj** %stackaddr$prim56238, align 8
%clofunc56239 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48098)
musttail call tailcc void %clofunc56239(%struct.ScmObj* %cc48098, %struct.ScmObj* %argslist54936$cc480982)
ret void
}

define tailcc void @proc_clo$ae51274(%struct.ScmObj* %env$ae51274,%struct.ScmObj* %current_45args54938) {
%stackaddr$prim56240 = alloca %struct.ScmObj*, align 8
%k48375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54938)
store volatile %struct.ScmObj* %k48375, %struct.ScmObj** %stackaddr$prim56240, align 8
%stackaddr$prim56241 = alloca %struct.ScmObj*, align 8
%current_45args54939 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54938)
store volatile %struct.ScmObj* %current_45args54939, %struct.ScmObj** %stackaddr$prim56241, align 8
%stackaddr$prim56242 = alloca %struct.ScmObj*, align 8
%u48099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54939)
store volatile %struct.ScmObj* %u48099, %struct.ScmObj** %stackaddr$prim56242, align 8
%argslist54941$u480990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56243 = alloca %struct.ScmObj*, align 8
%argslist54941$u480991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48099, %struct.ScmObj* %argslist54941$u480990)
store volatile %struct.ScmObj* %argslist54941$u480991, %struct.ScmObj** %stackaddr$prim56243, align 8
%stackaddr$prim56244 = alloca %struct.ScmObj*, align 8
%argslist54941$u480992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48375, %struct.ScmObj* %argslist54941$u480991)
store volatile %struct.ScmObj* %argslist54941$u480992, %struct.ScmObj** %stackaddr$prim56244, align 8
%clofunc56245 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48099)
musttail call tailcc void %clofunc56245(%struct.ScmObj* %u48099, %struct.ScmObj* %argslist54941$u480992)
ret void
}

define tailcc void @proc_clo$ae50733(%struct.ScmObj* %env$ae50733,%struct.ScmObj* %current_45args54944) {
%stackaddr$prim56246 = alloca %struct.ScmObj*, align 8
%k48376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54944)
store volatile %struct.ScmObj* %k48376, %struct.ScmObj** %stackaddr$prim56246, align 8
%stackaddr$prim56247 = alloca %struct.ScmObj*, align 8
%current_45args54945 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54944)
store volatile %struct.ScmObj* %current_45args54945, %struct.ScmObj** %stackaddr$prim56247, align 8
%stackaddr$prim56248 = alloca %struct.ScmObj*, align 8
%lst48103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54945)
store volatile %struct.ScmObj* %lst48103, %struct.ScmObj** %stackaddr$prim56248, align 8
%stackaddr$prim56249 = alloca %struct.ScmObj*, align 8
%current_45args54946 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54945)
store volatile %struct.ScmObj* %current_45args54946, %struct.ScmObj** %stackaddr$prim56249, align 8
%stackaddr$prim56250 = alloca %struct.ScmObj*, align 8
%n48102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54946)
store volatile %struct.ScmObj* %n48102, %struct.ScmObj** %stackaddr$prim56250, align 8
%ae50734 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56251 = alloca %struct.ScmObj*, align 8
%n48105 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50734, %struct.ScmObj* %n48102)
store volatile %struct.ScmObj* %n48105, %struct.ScmObj** %stackaddr$prim56251, align 8
%ae50736 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56252 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50736, %struct.ScmObj* %lst48103)
store volatile %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$prim56252, align 8
%stackaddr$makeclosure56253 = alloca %struct.ScmObj*, align 8
%fptrToInt56254 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50738 to i64
%ae50738 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56254)
store volatile %struct.ScmObj* %ae50738, %struct.ScmObj** %stackaddr$makeclosure56253, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50738, %struct.ScmObj* %k48376, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50738, %struct.ScmObj* %lst48104, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50738, %struct.ScmObj* %n48105, i64 2)
%ae50739 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56255 = alloca %struct.ScmObj*, align 8
%fptrToInt56256 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50740 to i64
%ae50740 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56256)
store volatile %struct.ScmObj* %ae50740, %struct.ScmObj** %stackaddr$makeclosure56255, align 8
%argslist54966$ae507380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56257 = alloca %struct.ScmObj*, align 8
%argslist54966$ae507381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50740, %struct.ScmObj* %argslist54966$ae507380)
store volatile %struct.ScmObj* %argslist54966$ae507381, %struct.ScmObj** %stackaddr$prim56257, align 8
%stackaddr$prim56258 = alloca %struct.ScmObj*, align 8
%argslist54966$ae507382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50739, %struct.ScmObj* %argslist54966$ae507381)
store volatile %struct.ScmObj* %argslist54966$ae507382, %struct.ScmObj** %stackaddr$prim56258, align 8
%clofunc56259 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50738)
musttail call tailcc void %clofunc56259(%struct.ScmObj* %ae50738, %struct.ScmObj* %argslist54966$ae507382)
ret void
}

define tailcc void @proc_clo$ae50738(%struct.ScmObj* %env$ae50738,%struct.ScmObj* %current_45args54948) {
%stackaddr$env-ref56260 = alloca %struct.ScmObj*, align 8
%k48376 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50738, i64 0)
store %struct.ScmObj* %k48376, %struct.ScmObj** %stackaddr$env-ref56260
%stackaddr$env-ref56261 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50738, i64 1)
store %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$env-ref56261
%stackaddr$env-ref56262 = alloca %struct.ScmObj*, align 8
%n48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50738, i64 2)
store %struct.ScmObj* %n48105, %struct.ScmObj** %stackaddr$env-ref56262
%stackaddr$prim56263 = alloca %struct.ScmObj*, align 8
%_95k48377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54948)
store volatile %struct.ScmObj* %_95k48377, %struct.ScmObj** %stackaddr$prim56263, align 8
%stackaddr$prim56264 = alloca %struct.ScmObj*, align 8
%current_45args54949 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54948)
store volatile %struct.ScmObj* %current_45args54949, %struct.ScmObj** %stackaddr$prim56264, align 8
%stackaddr$prim56265 = alloca %struct.ScmObj*, align 8
%anf_45bind48237 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54949)
store volatile %struct.ScmObj* %anf_45bind48237, %struct.ScmObj** %stackaddr$prim56265, align 8
%stackaddr$makeclosure56266 = alloca %struct.ScmObj*, align 8
%fptrToInt56267 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50754 to i64
%ae50754 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56267)
store volatile %struct.ScmObj* %ae50754, %struct.ScmObj** %stackaddr$makeclosure56266, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50754, %struct.ScmObj* %k48376, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50754, %struct.ScmObj* %lst48104, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50754, %struct.ScmObj* %n48105, i64 2)
%stackaddr$makeclosure56268 = alloca %struct.ScmObj*, align 8
%fptrToInt56269 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50755 to i64
%ae50755 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56269)
store volatile %struct.ScmObj* %ae50755, %struct.ScmObj** %stackaddr$makeclosure56268, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50755, %struct.ScmObj* %k48376, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50755, %struct.ScmObj* %lst48104, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50755, %struct.ScmObj* %n48105, i64 2)
%argslist54961$anf_45bind482370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56270 = alloca %struct.ScmObj*, align 8
%argslist54961$anf_45bind482371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50755, %struct.ScmObj* %argslist54961$anf_45bind482370)
store volatile %struct.ScmObj* %argslist54961$anf_45bind482371, %struct.ScmObj** %stackaddr$prim56270, align 8
%stackaddr$prim56271 = alloca %struct.ScmObj*, align 8
%argslist54961$anf_45bind482372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50754, %struct.ScmObj* %argslist54961$anf_45bind482371)
store volatile %struct.ScmObj* %argslist54961$anf_45bind482372, %struct.ScmObj** %stackaddr$prim56271, align 8
%clofunc56272 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48237)
musttail call tailcc void %clofunc56272(%struct.ScmObj* %anf_45bind48237, %struct.ScmObj* %argslist54961$anf_45bind482372)
ret void
}

define tailcc void @proc_clo$ae50754(%struct.ScmObj* %env$ae50754,%struct.ScmObj* %current_45args54951) {
%stackaddr$env-ref56273 = alloca %struct.ScmObj*, align 8
%k48376 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50754, i64 0)
store %struct.ScmObj* %k48376, %struct.ScmObj** %stackaddr$env-ref56273
%stackaddr$env-ref56274 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50754, i64 1)
store %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$env-ref56274
%stackaddr$env-ref56275 = alloca %struct.ScmObj*, align 8
%n48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50754, i64 2)
store %struct.ScmObj* %n48105, %struct.ScmObj** %stackaddr$env-ref56275
%stackaddr$prim56276 = alloca %struct.ScmObj*, align 8
%_95k48378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54951)
store volatile %struct.ScmObj* %_95k48378, %struct.ScmObj** %stackaddr$prim56276, align 8
%stackaddr$prim56277 = alloca %struct.ScmObj*, align 8
%current_45args54952 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54951)
store volatile %struct.ScmObj* %current_45args54952, %struct.ScmObj** %stackaddr$prim56277, align 8
%stackaddr$prim56278 = alloca %struct.ScmObj*, align 8
%cc48106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54952)
store volatile %struct.ScmObj* %cc48106, %struct.ScmObj** %stackaddr$prim56278, align 8
%ae50897 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56279 = alloca %struct.ScmObj*, align 8
%anf_45bind48238 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50897)
store volatile %struct.ScmObj* %anf_45bind48238, %struct.ScmObj** %stackaddr$prim56279, align 8
%ae50898 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56280 = alloca %struct.ScmObj*, align 8
%anf_45bind48239 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50898, %struct.ScmObj* %anf_45bind48238)
store volatile %struct.ScmObj* %anf_45bind48239, %struct.ScmObj** %stackaddr$prim56280, align 8
%truthy$cmp56281 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48239)
%cmp$cmp56281 = icmp eq i64 %truthy$cmp56281, 1
br i1 %cmp$cmp56281, label %truebranch$cmp56281, label %falsebranch$cmp56281
truebranch$cmp56281:
%ae50902 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56282 = alloca %struct.ScmObj*, align 8
%cpsprim48379 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50902)
store volatile %struct.ScmObj* %cpsprim48379, %struct.ScmObj** %stackaddr$prim56282, align 8
%ae50904 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54954$k483760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56283 = alloca %struct.ScmObj*, align 8
%argslist54954$k483761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48379, %struct.ScmObj* %argslist54954$k483760)
store volatile %struct.ScmObj* %argslist54954$k483761, %struct.ScmObj** %stackaddr$prim56283, align 8
%stackaddr$prim56284 = alloca %struct.ScmObj*, align 8
%argslist54954$k483762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50904, %struct.ScmObj* %argslist54954$k483761)
store volatile %struct.ScmObj* %argslist54954$k483762, %struct.ScmObj** %stackaddr$prim56284, align 8
%clofunc56285 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48376)
musttail call tailcc void %clofunc56285(%struct.ScmObj* %k48376, %struct.ScmObj* %argslist54954$k483762)
ret void
falsebranch$cmp56281:
%ae50915 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56286 = alloca %struct.ScmObj*, align 8
%anf_45bind48240 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50915)
store volatile %struct.ScmObj* %anf_45bind48240, %struct.ScmObj** %stackaddr$prim56286, align 8
%stackaddr$prim56287 = alloca %struct.ScmObj*, align 8
%anf_45bind48241 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48240)
store volatile %struct.ScmObj* %anf_45bind48241, %struct.ScmObj** %stackaddr$prim56287, align 8
%ae50918 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56288 = alloca %struct.ScmObj*, align 8
%_95048109 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50918, %struct.ScmObj* %anf_45bind48241)
store volatile %struct.ScmObj* %_95048109, %struct.ScmObj** %stackaddr$prim56288, align 8
%ae50921 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56289 = alloca %struct.ScmObj*, align 8
%anf_45bind48242 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50921)
store volatile %struct.ScmObj* %anf_45bind48242, %struct.ScmObj** %stackaddr$prim56289, align 8
%ae50923 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56290 = alloca %struct.ScmObj*, align 8
%anf_45bind48243 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48242, %struct.ScmObj* %ae50923)
store volatile %struct.ScmObj* %anf_45bind48243, %struct.ScmObj** %stackaddr$prim56290, align 8
%ae50925 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56291 = alloca %struct.ScmObj*, align 8
%_95148108 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50925, %struct.ScmObj* %anf_45bind48243)
store volatile %struct.ScmObj* %_95148108, %struct.ScmObj** %stackaddr$prim56291, align 8
%argslist54955$cc481060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56292 = alloca %struct.ScmObj*, align 8
%argslist54955$cc481061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist54955$cc481060)
store volatile %struct.ScmObj* %argslist54955$cc481061, %struct.ScmObj** %stackaddr$prim56292, align 8
%stackaddr$prim56293 = alloca %struct.ScmObj*, align 8
%argslist54955$cc481062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48376, %struct.ScmObj* %argslist54955$cc481061)
store volatile %struct.ScmObj* %argslist54955$cc481062, %struct.ScmObj** %stackaddr$prim56293, align 8
%clofunc56294 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48106)
musttail call tailcc void %clofunc56294(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist54955$cc481062)
ret void
}

define tailcc void @proc_clo$ae50755(%struct.ScmObj* %env$ae50755,%struct.ScmObj* %current_45args54956) {
%stackaddr$env-ref56295 = alloca %struct.ScmObj*, align 8
%k48376 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50755, i64 0)
store %struct.ScmObj* %k48376, %struct.ScmObj** %stackaddr$env-ref56295
%stackaddr$env-ref56296 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50755, i64 1)
store %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$env-ref56296
%stackaddr$env-ref56297 = alloca %struct.ScmObj*, align 8
%n48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50755, i64 2)
store %struct.ScmObj* %n48105, %struct.ScmObj** %stackaddr$env-ref56297
%stackaddr$prim56298 = alloca %struct.ScmObj*, align 8
%_95k48378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54956)
store volatile %struct.ScmObj* %_95k48378, %struct.ScmObj** %stackaddr$prim56298, align 8
%stackaddr$prim56299 = alloca %struct.ScmObj*, align 8
%current_45args54957 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54956)
store volatile %struct.ScmObj* %current_45args54957, %struct.ScmObj** %stackaddr$prim56299, align 8
%stackaddr$prim56300 = alloca %struct.ScmObj*, align 8
%cc48106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54957)
store volatile %struct.ScmObj* %cc48106, %struct.ScmObj** %stackaddr$prim56300, align 8
%ae50757 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56301 = alloca %struct.ScmObj*, align 8
%anf_45bind48238 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50757)
store volatile %struct.ScmObj* %anf_45bind48238, %struct.ScmObj** %stackaddr$prim56301, align 8
%ae50758 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56302 = alloca %struct.ScmObj*, align 8
%anf_45bind48239 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50758, %struct.ScmObj* %anf_45bind48238)
store volatile %struct.ScmObj* %anf_45bind48239, %struct.ScmObj** %stackaddr$prim56302, align 8
%truthy$cmp56303 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48239)
%cmp$cmp56303 = icmp eq i64 %truthy$cmp56303, 1
br i1 %cmp$cmp56303, label %truebranch$cmp56303, label %falsebranch$cmp56303
truebranch$cmp56303:
%ae50762 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56304 = alloca %struct.ScmObj*, align 8
%cpsprim48379 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50762)
store volatile %struct.ScmObj* %cpsprim48379, %struct.ScmObj** %stackaddr$prim56304, align 8
%ae50764 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54959$k483760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56305 = alloca %struct.ScmObj*, align 8
%argslist54959$k483761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48379, %struct.ScmObj* %argslist54959$k483760)
store volatile %struct.ScmObj* %argslist54959$k483761, %struct.ScmObj** %stackaddr$prim56305, align 8
%stackaddr$prim56306 = alloca %struct.ScmObj*, align 8
%argslist54959$k483762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50764, %struct.ScmObj* %argslist54959$k483761)
store volatile %struct.ScmObj* %argslist54959$k483762, %struct.ScmObj** %stackaddr$prim56306, align 8
%clofunc56307 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48376)
musttail call tailcc void %clofunc56307(%struct.ScmObj* %k48376, %struct.ScmObj* %argslist54959$k483762)
ret void
falsebranch$cmp56303:
%ae50775 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56308 = alloca %struct.ScmObj*, align 8
%anf_45bind48240 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50775)
store volatile %struct.ScmObj* %anf_45bind48240, %struct.ScmObj** %stackaddr$prim56308, align 8
%stackaddr$prim56309 = alloca %struct.ScmObj*, align 8
%anf_45bind48241 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48240)
store volatile %struct.ScmObj* %anf_45bind48241, %struct.ScmObj** %stackaddr$prim56309, align 8
%ae50778 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56310 = alloca %struct.ScmObj*, align 8
%_95048109 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48104, %struct.ScmObj* %ae50778, %struct.ScmObj* %anf_45bind48241)
store volatile %struct.ScmObj* %_95048109, %struct.ScmObj** %stackaddr$prim56310, align 8
%ae50781 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56311 = alloca %struct.ScmObj*, align 8
%anf_45bind48242 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50781)
store volatile %struct.ScmObj* %anf_45bind48242, %struct.ScmObj** %stackaddr$prim56311, align 8
%ae50783 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56312 = alloca %struct.ScmObj*, align 8
%anf_45bind48243 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48242, %struct.ScmObj* %ae50783)
store volatile %struct.ScmObj* %anf_45bind48243, %struct.ScmObj** %stackaddr$prim56312, align 8
%ae50785 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56313 = alloca %struct.ScmObj*, align 8
%_95148108 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48105, %struct.ScmObj* %ae50785, %struct.ScmObj* %anf_45bind48243)
store volatile %struct.ScmObj* %_95148108, %struct.ScmObj** %stackaddr$prim56313, align 8
%argslist54960$cc481060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56314 = alloca %struct.ScmObj*, align 8
%argslist54960$cc481061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist54960$cc481060)
store volatile %struct.ScmObj* %argslist54960$cc481061, %struct.ScmObj** %stackaddr$prim56314, align 8
%stackaddr$prim56315 = alloca %struct.ScmObj*, align 8
%argslist54960$cc481062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48376, %struct.ScmObj* %argslist54960$cc481061)
store volatile %struct.ScmObj* %argslist54960$cc481062, %struct.ScmObj** %stackaddr$prim56315, align 8
%clofunc56316 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48106)
musttail call tailcc void %clofunc56316(%struct.ScmObj* %cc48106, %struct.ScmObj* %argslist54960$cc481062)
ret void
}

define tailcc void @proc_clo$ae50740(%struct.ScmObj* %env$ae50740,%struct.ScmObj* %current_45args54962) {
%stackaddr$prim56317 = alloca %struct.ScmObj*, align 8
%k48380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54962)
store volatile %struct.ScmObj* %k48380, %struct.ScmObj** %stackaddr$prim56317, align 8
%stackaddr$prim56318 = alloca %struct.ScmObj*, align 8
%current_45args54963 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54962)
store volatile %struct.ScmObj* %current_45args54963, %struct.ScmObj** %stackaddr$prim56318, align 8
%stackaddr$prim56319 = alloca %struct.ScmObj*, align 8
%u48107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54963)
store volatile %struct.ScmObj* %u48107, %struct.ScmObj** %stackaddr$prim56319, align 8
%argslist54965$u481070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56320 = alloca %struct.ScmObj*, align 8
%argslist54965$u481071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48107, %struct.ScmObj* %argslist54965$u481070)
store volatile %struct.ScmObj* %argslist54965$u481071, %struct.ScmObj** %stackaddr$prim56320, align 8
%stackaddr$prim56321 = alloca %struct.ScmObj*, align 8
%argslist54965$u481072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48380, %struct.ScmObj* %argslist54965$u481071)
store volatile %struct.ScmObj* %argslist54965$u481072, %struct.ScmObj** %stackaddr$prim56321, align 8
%clofunc56322 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48107)
musttail call tailcc void %clofunc56322(%struct.ScmObj* %u48107, %struct.ScmObj* %argslist54965$u481072)
ret void
}

define tailcc void @proc_clo$ae50317(%struct.ScmObj* %env$ae50317,%struct.ScmObj* %current_45args54968) {
%stackaddr$prim56323 = alloca %struct.ScmObj*, align 8
%k48381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54968)
store volatile %struct.ScmObj* %k48381, %struct.ScmObj** %stackaddr$prim56323, align 8
%stackaddr$prim56324 = alloca %struct.ScmObj*, align 8
%current_45args54969 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54968)
store volatile %struct.ScmObj* %current_45args54969, %struct.ScmObj** %stackaddr$prim56324, align 8
%stackaddr$prim56325 = alloca %struct.ScmObj*, align 8
%a48111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54969)
store volatile %struct.ScmObj* %a48111, %struct.ScmObj** %stackaddr$prim56325, align 8
%ae50318 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56326 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50318, %struct.ScmObj* %a48111)
store volatile %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$prim56326, align 8
%stackaddr$makeclosure56327 = alloca %struct.ScmObj*, align 8
%fptrToInt56328 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50320 to i64
%ae50320 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56328)
store volatile %struct.ScmObj* %ae50320, %struct.ScmObj** %stackaddr$makeclosure56327, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50320, %struct.ScmObj* %a48112, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50320, %struct.ScmObj* %k48381, i64 1)
%ae50321 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56329 = alloca %struct.ScmObj*, align 8
%fptrToInt56330 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50322 to i64
%ae50322 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56330)
store volatile %struct.ScmObj* %ae50322, %struct.ScmObj** %stackaddr$makeclosure56329, align 8
%argslist54991$ae503200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56331 = alloca %struct.ScmObj*, align 8
%argslist54991$ae503201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50322, %struct.ScmObj* %argslist54991$ae503200)
store volatile %struct.ScmObj* %argslist54991$ae503201, %struct.ScmObj** %stackaddr$prim56331, align 8
%stackaddr$prim56332 = alloca %struct.ScmObj*, align 8
%argslist54991$ae503202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50321, %struct.ScmObj* %argslist54991$ae503201)
store volatile %struct.ScmObj* %argslist54991$ae503202, %struct.ScmObj** %stackaddr$prim56332, align 8
%clofunc56333 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50320)
musttail call tailcc void %clofunc56333(%struct.ScmObj* %ae50320, %struct.ScmObj* %argslist54991$ae503202)
ret void
}

define tailcc void @proc_clo$ae50320(%struct.ScmObj* %env$ae50320,%struct.ScmObj* %current_45args54971) {
%stackaddr$env-ref56334 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50320, i64 0)
store %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$env-ref56334
%stackaddr$env-ref56335 = alloca %struct.ScmObj*, align 8
%k48381 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50320, i64 1)
store %struct.ScmObj* %k48381, %struct.ScmObj** %stackaddr$env-ref56335
%stackaddr$prim56336 = alloca %struct.ScmObj*, align 8
%_95k48382 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54971)
store volatile %struct.ScmObj* %_95k48382, %struct.ScmObj** %stackaddr$prim56336, align 8
%stackaddr$prim56337 = alloca %struct.ScmObj*, align 8
%current_45args54972 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54971)
store volatile %struct.ScmObj* %current_45args54972, %struct.ScmObj** %stackaddr$prim56337, align 8
%stackaddr$prim56338 = alloca %struct.ScmObj*, align 8
%anf_45bind48229 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54972)
store volatile %struct.ScmObj* %anf_45bind48229, %struct.ScmObj** %stackaddr$prim56338, align 8
%stackaddr$makeclosure56339 = alloca %struct.ScmObj*, align 8
%fptrToInt56340 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50339 to i64
%ae50339 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56340)
store volatile %struct.ScmObj* %ae50339, %struct.ScmObj** %stackaddr$makeclosure56339, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50339, %struct.ScmObj* %a48112, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50339, %struct.ScmObj* %k48381, i64 1)
%stackaddr$makeclosure56341 = alloca %struct.ScmObj*, align 8
%fptrToInt56342 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50340 to i64
%ae50340 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56342)
store volatile %struct.ScmObj* %ae50340, %struct.ScmObj** %stackaddr$makeclosure56341, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50340, %struct.ScmObj* %a48112, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50340, %struct.ScmObj* %k48381, i64 1)
%argslist54986$anf_45bind482290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56343 = alloca %struct.ScmObj*, align 8
%argslist54986$anf_45bind482291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50340, %struct.ScmObj* %argslist54986$anf_45bind482290)
store volatile %struct.ScmObj* %argslist54986$anf_45bind482291, %struct.ScmObj** %stackaddr$prim56343, align 8
%stackaddr$prim56344 = alloca %struct.ScmObj*, align 8
%argslist54986$anf_45bind482292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50339, %struct.ScmObj* %argslist54986$anf_45bind482291)
store volatile %struct.ScmObj* %argslist54986$anf_45bind482292, %struct.ScmObj** %stackaddr$prim56344, align 8
%clofunc56345 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48229)
musttail call tailcc void %clofunc56345(%struct.ScmObj* %anf_45bind48229, %struct.ScmObj* %argslist54986$anf_45bind482292)
ret void
}

define tailcc void @proc_clo$ae50339(%struct.ScmObj* %env$ae50339,%struct.ScmObj* %current_45args54974) {
%stackaddr$env-ref56346 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50339, i64 0)
store %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$env-ref56346
%stackaddr$env-ref56347 = alloca %struct.ScmObj*, align 8
%k48381 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50339, i64 1)
store %struct.ScmObj* %k48381, %struct.ScmObj** %stackaddr$env-ref56347
%stackaddr$prim56348 = alloca %struct.ScmObj*, align 8
%_95k48383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54974)
store volatile %struct.ScmObj* %_95k48383, %struct.ScmObj** %stackaddr$prim56348, align 8
%stackaddr$prim56349 = alloca %struct.ScmObj*, align 8
%current_45args54975 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54974)
store volatile %struct.ScmObj* %current_45args54975, %struct.ScmObj** %stackaddr$prim56349, align 8
%stackaddr$prim56350 = alloca %struct.ScmObj*, align 8
%cc48113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54975)
store volatile %struct.ScmObj* %cc48113, %struct.ScmObj** %stackaddr$prim56350, align 8
%ae50455 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56351 = alloca %struct.ScmObj*, align 8
%anf_45bind48230 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50455)
store volatile %struct.ScmObj* %anf_45bind48230, %struct.ScmObj** %stackaddr$prim56351, align 8
%stackaddr$prim56352 = alloca %struct.ScmObj*, align 8
%anf_45bind48231 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48230)
store volatile %struct.ScmObj* %anf_45bind48231, %struct.ScmObj** %stackaddr$prim56352, align 8
%truthy$cmp56353 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48231)
%cmp$cmp56353 = icmp eq i64 %truthy$cmp56353, 1
br i1 %cmp$cmp56353, label %truebranch$cmp56353, label %falsebranch$cmp56353
truebranch$cmp56353:
%ae50459 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50460 = call %struct.ScmObj* @const_init_true()
%argslist54977$k483810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56354 = alloca %struct.ScmObj*, align 8
%argslist54977$k483811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50460, %struct.ScmObj* %argslist54977$k483810)
store volatile %struct.ScmObj* %argslist54977$k483811, %struct.ScmObj** %stackaddr$prim56354, align 8
%stackaddr$prim56355 = alloca %struct.ScmObj*, align 8
%argslist54977$k483812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50459, %struct.ScmObj* %argslist54977$k483811)
store volatile %struct.ScmObj* %argslist54977$k483812, %struct.ScmObj** %stackaddr$prim56355, align 8
%clofunc56356 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48381)
musttail call tailcc void %clofunc56356(%struct.ScmObj* %k48381, %struct.ScmObj* %argslist54977$k483812)
ret void
falsebranch$cmp56353:
%ae50468 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56357 = alloca %struct.ScmObj*, align 8
%anf_45bind48232 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50468)
store volatile %struct.ScmObj* %anf_45bind48232, %struct.ScmObj** %stackaddr$prim56357, align 8
%stackaddr$prim56358 = alloca %struct.ScmObj*, align 8
%anf_45bind48233 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48232)
store volatile %struct.ScmObj* %anf_45bind48233, %struct.ScmObj** %stackaddr$prim56358, align 8
%truthy$cmp56359 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48233)
%cmp$cmp56359 = icmp eq i64 %truthy$cmp56359, 1
br i1 %cmp$cmp56359, label %truebranch$cmp56359, label %falsebranch$cmp56359
truebranch$cmp56359:
%ae50472 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56360 = alloca %struct.ScmObj*, align 8
%anf_45bind48234 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50472)
store volatile %struct.ScmObj* %anf_45bind48234, %struct.ScmObj** %stackaddr$prim56360, align 8
%stackaddr$prim56361 = alloca %struct.ScmObj*, align 8
%b48115 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48234)
store volatile %struct.ScmObj* %b48115, %struct.ScmObj** %stackaddr$prim56361, align 8
%ae50475 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56362 = alloca %struct.ScmObj*, align 8
%anf_45bind48235 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50475)
store volatile %struct.ScmObj* %anf_45bind48235, %struct.ScmObj** %stackaddr$prim56362, align 8
%stackaddr$prim56363 = alloca %struct.ScmObj*, align 8
%anf_45bind48236 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48235)
store volatile %struct.ScmObj* %anf_45bind48236, %struct.ScmObj** %stackaddr$prim56363, align 8
%ae50478 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56364 = alloca %struct.ScmObj*, align 8
%_95048116 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50478, %struct.ScmObj* %anf_45bind48236)
store volatile %struct.ScmObj* %_95048116, %struct.ScmObj** %stackaddr$prim56364, align 8
%argslist54978$cc481130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56365 = alloca %struct.ScmObj*, align 8
%argslist54978$cc481131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist54978$cc481130)
store volatile %struct.ScmObj* %argslist54978$cc481131, %struct.ScmObj** %stackaddr$prim56365, align 8
%stackaddr$prim56366 = alloca %struct.ScmObj*, align 8
%argslist54978$cc481132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48381, %struct.ScmObj* %argslist54978$cc481131)
store volatile %struct.ScmObj* %argslist54978$cc481132, %struct.ScmObj** %stackaddr$prim56366, align 8
%clofunc56367 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48113)
musttail call tailcc void %clofunc56367(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist54978$cc481132)
ret void
falsebranch$cmp56359:
%ae50511 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50512 = call %struct.ScmObj* @const_init_false()
%argslist54979$k483810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56368 = alloca %struct.ScmObj*, align 8
%argslist54979$k483811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50512, %struct.ScmObj* %argslist54979$k483810)
store volatile %struct.ScmObj* %argslist54979$k483811, %struct.ScmObj** %stackaddr$prim56368, align 8
%stackaddr$prim56369 = alloca %struct.ScmObj*, align 8
%argslist54979$k483812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50511, %struct.ScmObj* %argslist54979$k483811)
store volatile %struct.ScmObj* %argslist54979$k483812, %struct.ScmObj** %stackaddr$prim56369, align 8
%clofunc56370 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48381)
musttail call tailcc void %clofunc56370(%struct.ScmObj* %k48381, %struct.ScmObj* %argslist54979$k483812)
ret void
}

define tailcc void @proc_clo$ae50340(%struct.ScmObj* %env$ae50340,%struct.ScmObj* %current_45args54980) {
%stackaddr$env-ref56371 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50340, i64 0)
store %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$env-ref56371
%stackaddr$env-ref56372 = alloca %struct.ScmObj*, align 8
%k48381 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50340, i64 1)
store %struct.ScmObj* %k48381, %struct.ScmObj** %stackaddr$env-ref56372
%stackaddr$prim56373 = alloca %struct.ScmObj*, align 8
%_95k48383 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54980)
store volatile %struct.ScmObj* %_95k48383, %struct.ScmObj** %stackaddr$prim56373, align 8
%stackaddr$prim56374 = alloca %struct.ScmObj*, align 8
%current_45args54981 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54980)
store volatile %struct.ScmObj* %current_45args54981, %struct.ScmObj** %stackaddr$prim56374, align 8
%stackaddr$prim56375 = alloca %struct.ScmObj*, align 8
%cc48113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54981)
store volatile %struct.ScmObj* %cc48113, %struct.ScmObj** %stackaddr$prim56375, align 8
%ae50342 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56376 = alloca %struct.ScmObj*, align 8
%anf_45bind48230 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50342)
store volatile %struct.ScmObj* %anf_45bind48230, %struct.ScmObj** %stackaddr$prim56376, align 8
%stackaddr$prim56377 = alloca %struct.ScmObj*, align 8
%anf_45bind48231 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48230)
store volatile %struct.ScmObj* %anf_45bind48231, %struct.ScmObj** %stackaddr$prim56377, align 8
%truthy$cmp56378 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48231)
%cmp$cmp56378 = icmp eq i64 %truthy$cmp56378, 1
br i1 %cmp$cmp56378, label %truebranch$cmp56378, label %falsebranch$cmp56378
truebranch$cmp56378:
%ae50346 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50347 = call %struct.ScmObj* @const_init_true()
%argslist54983$k483810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56379 = alloca %struct.ScmObj*, align 8
%argslist54983$k483811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50347, %struct.ScmObj* %argslist54983$k483810)
store volatile %struct.ScmObj* %argslist54983$k483811, %struct.ScmObj** %stackaddr$prim56379, align 8
%stackaddr$prim56380 = alloca %struct.ScmObj*, align 8
%argslist54983$k483812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50346, %struct.ScmObj* %argslist54983$k483811)
store volatile %struct.ScmObj* %argslist54983$k483812, %struct.ScmObj** %stackaddr$prim56380, align 8
%clofunc56381 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48381)
musttail call tailcc void %clofunc56381(%struct.ScmObj* %k48381, %struct.ScmObj* %argslist54983$k483812)
ret void
falsebranch$cmp56378:
%ae50355 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56382 = alloca %struct.ScmObj*, align 8
%anf_45bind48232 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50355)
store volatile %struct.ScmObj* %anf_45bind48232, %struct.ScmObj** %stackaddr$prim56382, align 8
%stackaddr$prim56383 = alloca %struct.ScmObj*, align 8
%anf_45bind48233 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48232)
store volatile %struct.ScmObj* %anf_45bind48233, %struct.ScmObj** %stackaddr$prim56383, align 8
%truthy$cmp56384 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48233)
%cmp$cmp56384 = icmp eq i64 %truthy$cmp56384, 1
br i1 %cmp$cmp56384, label %truebranch$cmp56384, label %falsebranch$cmp56384
truebranch$cmp56384:
%ae50359 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56385 = alloca %struct.ScmObj*, align 8
%anf_45bind48234 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50359)
store volatile %struct.ScmObj* %anf_45bind48234, %struct.ScmObj** %stackaddr$prim56385, align 8
%stackaddr$prim56386 = alloca %struct.ScmObj*, align 8
%b48115 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48234)
store volatile %struct.ScmObj* %b48115, %struct.ScmObj** %stackaddr$prim56386, align 8
%ae50362 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56387 = alloca %struct.ScmObj*, align 8
%anf_45bind48235 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50362)
store volatile %struct.ScmObj* %anf_45bind48235, %struct.ScmObj** %stackaddr$prim56387, align 8
%stackaddr$prim56388 = alloca %struct.ScmObj*, align 8
%anf_45bind48236 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48235)
store volatile %struct.ScmObj* %anf_45bind48236, %struct.ScmObj** %stackaddr$prim56388, align 8
%ae50365 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56389 = alloca %struct.ScmObj*, align 8
%_95048116 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48112, %struct.ScmObj* %ae50365, %struct.ScmObj* %anf_45bind48236)
store volatile %struct.ScmObj* %_95048116, %struct.ScmObj** %stackaddr$prim56389, align 8
%argslist54984$cc481130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56390 = alloca %struct.ScmObj*, align 8
%argslist54984$cc481131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist54984$cc481130)
store volatile %struct.ScmObj* %argslist54984$cc481131, %struct.ScmObj** %stackaddr$prim56390, align 8
%stackaddr$prim56391 = alloca %struct.ScmObj*, align 8
%argslist54984$cc481132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48381, %struct.ScmObj* %argslist54984$cc481131)
store volatile %struct.ScmObj* %argslist54984$cc481132, %struct.ScmObj** %stackaddr$prim56391, align 8
%clofunc56392 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48113)
musttail call tailcc void %clofunc56392(%struct.ScmObj* %cc48113, %struct.ScmObj* %argslist54984$cc481132)
ret void
falsebranch$cmp56384:
%ae50398 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50399 = call %struct.ScmObj* @const_init_false()
%argslist54985$k483810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56393 = alloca %struct.ScmObj*, align 8
%argslist54985$k483811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50399, %struct.ScmObj* %argslist54985$k483810)
store volatile %struct.ScmObj* %argslist54985$k483811, %struct.ScmObj** %stackaddr$prim56393, align 8
%stackaddr$prim56394 = alloca %struct.ScmObj*, align 8
%argslist54985$k483812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50398, %struct.ScmObj* %argslist54985$k483811)
store volatile %struct.ScmObj* %argslist54985$k483812, %struct.ScmObj** %stackaddr$prim56394, align 8
%clofunc56395 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48381)
musttail call tailcc void %clofunc56395(%struct.ScmObj* %k48381, %struct.ScmObj* %argslist54985$k483812)
ret void
}

define tailcc void @proc_clo$ae50322(%struct.ScmObj* %env$ae50322,%struct.ScmObj* %current_45args54987) {
%stackaddr$prim56396 = alloca %struct.ScmObj*, align 8
%k48384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54987)
store volatile %struct.ScmObj* %k48384, %struct.ScmObj** %stackaddr$prim56396, align 8
%stackaddr$prim56397 = alloca %struct.ScmObj*, align 8
%current_45args54988 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54987)
store volatile %struct.ScmObj* %current_45args54988, %struct.ScmObj** %stackaddr$prim56397, align 8
%stackaddr$prim56398 = alloca %struct.ScmObj*, align 8
%k48114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54988)
store volatile %struct.ScmObj* %k48114, %struct.ScmObj** %stackaddr$prim56398, align 8
%ae50324 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54990$k483840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56399 = alloca %struct.ScmObj*, align 8
%argslist54990$k483841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48114, %struct.ScmObj* %argslist54990$k483840)
store volatile %struct.ScmObj* %argslist54990$k483841, %struct.ScmObj** %stackaddr$prim56399, align 8
%stackaddr$prim56400 = alloca %struct.ScmObj*, align 8
%argslist54990$k483842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50324, %struct.ScmObj* %argslist54990$k483841)
store volatile %struct.ScmObj* %argslist54990$k483842, %struct.ScmObj** %stackaddr$prim56400, align 8
%clofunc56401 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48384)
musttail call tailcc void %clofunc56401(%struct.ScmObj* %k48384, %struct.ScmObj* %argslist54990$k483842)
ret void
}

define tailcc void @proc_clo$ae50245(%struct.ScmObj* %env$ae50245,%struct.ScmObj* %current_45args54993) {
%stackaddr$env-ref56402 = alloca %struct.ScmObj*, align 8
%_37append48118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50245, i64 0)
store %struct.ScmObj* %_37append48118, %struct.ScmObj** %stackaddr$env-ref56402
%stackaddr$prim56403 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54993)
store volatile %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$prim56403, align 8
%stackaddr$prim56404 = alloca %struct.ScmObj*, align 8
%current_45args54994 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54993)
store volatile %struct.ScmObj* %current_45args54994, %struct.ScmObj** %stackaddr$prim56404, align 8
%stackaddr$prim56405 = alloca %struct.ScmObj*, align 8
%ls048121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54994)
store volatile %struct.ScmObj* %ls048121, %struct.ScmObj** %stackaddr$prim56405, align 8
%stackaddr$prim56406 = alloca %struct.ScmObj*, align 8
%current_45args54995 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54994)
store volatile %struct.ScmObj* %current_45args54995, %struct.ScmObj** %stackaddr$prim56406, align 8
%stackaddr$prim56407 = alloca %struct.ScmObj*, align 8
%ls148120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54995)
store volatile %struct.ScmObj* %ls148120, %struct.ScmObj** %stackaddr$prim56407, align 8
%stackaddr$prim56408 = alloca %struct.ScmObj*, align 8
%anf_45bind48223 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls048121)
store volatile %struct.ScmObj* %anf_45bind48223, %struct.ScmObj** %stackaddr$prim56408, align 8
%truthy$cmp56409 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48223)
%cmp$cmp56409 = icmp eq i64 %truthy$cmp56409, 1
br i1 %cmp$cmp56409, label %truebranch$cmp56409, label %falsebranch$cmp56409
truebranch$cmp56409:
%ae50249 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54997$k483850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56410 = alloca %struct.ScmObj*, align 8
%argslist54997$k483851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148120, %struct.ScmObj* %argslist54997$k483850)
store volatile %struct.ScmObj* %argslist54997$k483851, %struct.ScmObj** %stackaddr$prim56410, align 8
%stackaddr$prim56411 = alloca %struct.ScmObj*, align 8
%argslist54997$k483852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50249, %struct.ScmObj* %argslist54997$k483851)
store volatile %struct.ScmObj* %argslist54997$k483852, %struct.ScmObj** %stackaddr$prim56411, align 8
%clofunc56412 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48385)
musttail call tailcc void %clofunc56412(%struct.ScmObj* %k48385, %struct.ScmObj* %argslist54997$k483852)
ret void
falsebranch$cmp56409:
%stackaddr$prim56413 = alloca %struct.ScmObj*, align 8
%anf_45bind48224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls048121)
store volatile %struct.ScmObj* %anf_45bind48224, %struct.ScmObj** %stackaddr$prim56413, align 8
%ae50256 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56414 = alloca %struct.ScmObj*, align 8
%anf_45bind48225 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48118, %struct.ScmObj* %ae50256)
store volatile %struct.ScmObj* %anf_45bind48225, %struct.ScmObj** %stackaddr$prim56414, align 8
%stackaddr$prim56415 = alloca %struct.ScmObj*, align 8
%anf_45bind48226 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls048121)
store volatile %struct.ScmObj* %anf_45bind48226, %struct.ScmObj** %stackaddr$prim56415, align 8
%stackaddr$makeclosure56416 = alloca %struct.ScmObj*, align 8
%fptrToInt56417 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50259 to i64
%ae50259 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56417)
store volatile %struct.ScmObj* %ae50259, %struct.ScmObj** %stackaddr$makeclosure56416, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50259, %struct.ScmObj* %k48385, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50259, %struct.ScmObj* %anf_45bind48224, i64 1)
%argslist55002$anf_45bind482250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56418 = alloca %struct.ScmObj*, align 8
%argslist55002$anf_45bind482251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148120, %struct.ScmObj* %argslist55002$anf_45bind482250)
store volatile %struct.ScmObj* %argslist55002$anf_45bind482251, %struct.ScmObj** %stackaddr$prim56418, align 8
%stackaddr$prim56419 = alloca %struct.ScmObj*, align 8
%argslist55002$anf_45bind482252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48226, %struct.ScmObj* %argslist55002$anf_45bind482251)
store volatile %struct.ScmObj* %argslist55002$anf_45bind482252, %struct.ScmObj** %stackaddr$prim56419, align 8
%stackaddr$prim56420 = alloca %struct.ScmObj*, align 8
%argslist55002$anf_45bind482253 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50259, %struct.ScmObj* %argslist55002$anf_45bind482252)
store volatile %struct.ScmObj* %argslist55002$anf_45bind482253, %struct.ScmObj** %stackaddr$prim56420, align 8
%clofunc56421 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48225)
musttail call tailcc void %clofunc56421(%struct.ScmObj* %anf_45bind48225, %struct.ScmObj* %argslist55002$anf_45bind482253)
ret void
}

define tailcc void @proc_clo$ae50259(%struct.ScmObj* %env$ae50259,%struct.ScmObj* %current_45args54998) {
%stackaddr$env-ref56422 = alloca %struct.ScmObj*, align 8
%k48385 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50259, i64 0)
store %struct.ScmObj* %k48385, %struct.ScmObj** %stackaddr$env-ref56422
%stackaddr$env-ref56423 = alloca %struct.ScmObj*, align 8
%anf_45bind48224 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50259, i64 1)
store %struct.ScmObj* %anf_45bind48224, %struct.ScmObj** %stackaddr$env-ref56423
%stackaddr$prim56424 = alloca %struct.ScmObj*, align 8
%_95k48386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54998)
store volatile %struct.ScmObj* %_95k48386, %struct.ScmObj** %stackaddr$prim56424, align 8
%stackaddr$prim56425 = alloca %struct.ScmObj*, align 8
%current_45args54999 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54998)
store volatile %struct.ScmObj* %current_45args54999, %struct.ScmObj** %stackaddr$prim56425, align 8
%stackaddr$prim56426 = alloca %struct.ScmObj*, align 8
%anf_45bind48227 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54999)
store volatile %struct.ScmObj* %anf_45bind48227, %struct.ScmObj** %stackaddr$prim56426, align 8
%stackaddr$prim56427 = alloca %struct.ScmObj*, align 8
%cpsprim48387 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48224, %struct.ScmObj* %anf_45bind48227)
store volatile %struct.ScmObj* %cpsprim48387, %struct.ScmObj** %stackaddr$prim56427, align 8
%ae50265 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55001$k483850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56428 = alloca %struct.ScmObj*, align 8
%argslist55001$k483851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48387, %struct.ScmObj* %argslist55001$k483850)
store volatile %struct.ScmObj* %argslist55001$k483851, %struct.ScmObj** %stackaddr$prim56428, align 8
%stackaddr$prim56429 = alloca %struct.ScmObj*, align 8
%argslist55001$k483852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50265, %struct.ScmObj* %argslist55001$k483851)
store volatile %struct.ScmObj* %argslist55001$k483852, %struct.ScmObj** %stackaddr$prim56429, align 8
%clofunc56430 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48385)
musttail call tailcc void %clofunc56430(%struct.ScmObj* %k48385, %struct.ScmObj* %argslist55001$k483852)
ret void
}

define tailcc void @proc_clo$ae50219(%struct.ScmObj* %env$ae50219,%struct.ScmObj* %current_45args55004) {
%stackaddr$prim56431 = alloca %struct.ScmObj*, align 8
%k48388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55004)
store volatile %struct.ScmObj* %k48388, %struct.ScmObj** %stackaddr$prim56431, align 8
%stackaddr$prim56432 = alloca %struct.ScmObj*, align 8
%current_45args55005 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55004)
store volatile %struct.ScmObj* %current_45args55005, %struct.ScmObj** %stackaddr$prim56432, align 8
%stackaddr$prim56433 = alloca %struct.ScmObj*, align 8
%a48124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55005)
store volatile %struct.ScmObj* %a48124, %struct.ScmObj** %stackaddr$prim56433, align 8
%stackaddr$prim56434 = alloca %struct.ScmObj*, align 8
%current_45args55006 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55005)
store volatile %struct.ScmObj* %current_45args55006, %struct.ScmObj** %stackaddr$prim56434, align 8
%stackaddr$prim56435 = alloca %struct.ScmObj*, align 8
%b48123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55006)
store volatile %struct.ScmObj* %b48123, %struct.ScmObj** %stackaddr$prim56435, align 8
%stackaddr$prim56436 = alloca %struct.ScmObj*, align 8
%anf_45bind48222 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a48124, %struct.ScmObj* %b48123)
store volatile %struct.ScmObj* %anf_45bind48222, %struct.ScmObj** %stackaddr$prim56436, align 8
%stackaddr$prim56437 = alloca %struct.ScmObj*, align 8
%cpsprim48389 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48222)
store volatile %struct.ScmObj* %cpsprim48389, %struct.ScmObj** %stackaddr$prim56437, align 8
%ae50224 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55008$k483880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56438 = alloca %struct.ScmObj*, align 8
%argslist55008$k483881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48389, %struct.ScmObj* %argslist55008$k483880)
store volatile %struct.ScmObj* %argslist55008$k483881, %struct.ScmObj** %stackaddr$prim56438, align 8
%stackaddr$prim56439 = alloca %struct.ScmObj*, align 8
%argslist55008$k483882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50224, %struct.ScmObj* %argslist55008$k483881)
store volatile %struct.ScmObj* %argslist55008$k483882, %struct.ScmObj** %stackaddr$prim56439, align 8
%clofunc56440 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48388)
musttail call tailcc void %clofunc56440(%struct.ScmObj* %k48388, %struct.ScmObj* %argslist55008$k483882)
ret void
}

define tailcc void @proc_clo$ae50195(%struct.ScmObj* %env$ae50195,%struct.ScmObj* %current_45args55010) {
%stackaddr$prim56441 = alloca %struct.ScmObj*, align 8
%k48390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55010)
store volatile %struct.ScmObj* %k48390, %struct.ScmObj** %stackaddr$prim56441, align 8
%stackaddr$prim56442 = alloca %struct.ScmObj*, align 8
%current_45args55011 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55010)
store volatile %struct.ScmObj* %current_45args55011, %struct.ScmObj** %stackaddr$prim56442, align 8
%stackaddr$prim56443 = alloca %struct.ScmObj*, align 8
%a48127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55011)
store volatile %struct.ScmObj* %a48127, %struct.ScmObj** %stackaddr$prim56443, align 8
%stackaddr$prim56444 = alloca %struct.ScmObj*, align 8
%current_45args55012 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55011)
store volatile %struct.ScmObj* %current_45args55012, %struct.ScmObj** %stackaddr$prim56444, align 8
%stackaddr$prim56445 = alloca %struct.ScmObj*, align 8
%b48126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55012)
store volatile %struct.ScmObj* %b48126, %struct.ScmObj** %stackaddr$prim56445, align 8
%stackaddr$prim56446 = alloca %struct.ScmObj*, align 8
%anf_45bind48221 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a48127, %struct.ScmObj* %b48126)
store volatile %struct.ScmObj* %anf_45bind48221, %struct.ScmObj** %stackaddr$prim56446, align 8
%stackaddr$prim56447 = alloca %struct.ScmObj*, align 8
%cpsprim48391 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48221)
store volatile %struct.ScmObj* %cpsprim48391, %struct.ScmObj** %stackaddr$prim56447, align 8
%ae50200 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55014$k483900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56448 = alloca %struct.ScmObj*, align 8
%argslist55014$k483901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48391, %struct.ScmObj* %argslist55014$k483900)
store volatile %struct.ScmObj* %argslist55014$k483901, %struct.ScmObj** %stackaddr$prim56448, align 8
%stackaddr$prim56449 = alloca %struct.ScmObj*, align 8
%argslist55014$k483902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50200, %struct.ScmObj* %argslist55014$k483901)
store volatile %struct.ScmObj* %argslist55014$k483902, %struct.ScmObj** %stackaddr$prim56449, align 8
%clofunc56450 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48390)
musttail call tailcc void %clofunc56450(%struct.ScmObj* %k48390, %struct.ScmObj* %argslist55014$k483902)
ret void
}

define tailcc void @proc_clo$ae49801(%struct.ScmObj* %env$ae49801,%struct.ScmObj* %current_45args55017) {
%stackaddr$env-ref56451 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49801, i64 0)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref56451
%stackaddr$env-ref56452 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49801, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56452
%stackaddr$env-ref56453 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49801, i64 2)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref56453
%stackaddr$prim56454 = alloca %struct.ScmObj*, align 8
%k48392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55017)
store volatile %struct.ScmObj* %k48392, %struct.ScmObj** %stackaddr$prim56454, align 8
%stackaddr$prim56455 = alloca %struct.ScmObj*, align 8
%current_45args55018 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55017)
store volatile %struct.ScmObj* %current_45args55018, %struct.ScmObj** %stackaddr$prim56455, align 8
%stackaddr$prim56456 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55018)
store volatile %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$prim56456, align 8
%ae49803 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56457 = alloca %struct.ScmObj*, align 8
%fptrToInt56458 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49804 to i64
%ae49804 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56458)
store volatile %struct.ScmObj* %ae49804, %struct.ScmObj** %stackaddr$makeclosure56457, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49804, %struct.ScmObj* %_37foldr48051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49804, %struct.ScmObj* %_37foldl48129, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49804, %struct.ScmObj* %_37foldr148046, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49804, %struct.ScmObj* %_37map148077, i64 3)
%argslist55075$k483920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56459 = alloca %struct.ScmObj*, align 8
%argslist55075$k483921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49804, %struct.ScmObj* %argslist55075$k483920)
store volatile %struct.ScmObj* %argslist55075$k483921, %struct.ScmObj** %stackaddr$prim56459, align 8
%stackaddr$prim56460 = alloca %struct.ScmObj*, align 8
%argslist55075$k483922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49803, %struct.ScmObj* %argslist55075$k483921)
store volatile %struct.ScmObj* %argslist55075$k483922, %struct.ScmObj** %stackaddr$prim56460, align 8
%clofunc56461 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48392)
musttail call tailcc void %clofunc56461(%struct.ScmObj* %k48392, %struct.ScmObj* %argslist55075$k483922)
ret void
}

define tailcc void @proc_clo$ae49804(%struct.ScmObj* %env$ae49804,%struct.ScmObj* %args4813048393) {
%stackaddr$env-ref56462 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49804, i64 0)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref56462
%stackaddr$env-ref56463 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49804, i64 1)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56463
%stackaddr$env-ref56464 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49804, i64 2)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56464
%stackaddr$env-ref56465 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49804, i64 3)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref56465
%stackaddr$prim56466 = alloca %struct.ScmObj*, align 8
%k48394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4813048393)
store volatile %struct.ScmObj* %k48394, %struct.ScmObj** %stackaddr$prim56466, align 8
%stackaddr$prim56467 = alloca %struct.ScmObj*, align 8
%args48130 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4813048393)
store volatile %struct.ScmObj* %args48130, %struct.ScmObj** %stackaddr$prim56467, align 8
%stackaddr$prim56468 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48130)
store volatile %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$prim56468, align 8
%stackaddr$prim56469 = alloca %struct.ScmObj*, align 8
%anf_45bind48209 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48130)
store volatile %struct.ScmObj* %anf_45bind48209, %struct.ScmObj** %stackaddr$prim56469, align 8
%stackaddr$prim56470 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48209)
store volatile %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$prim56470, align 8
%stackaddr$prim56471 = alloca %struct.ScmObj*, align 8
%anf_45bind48210 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48130)
store volatile %struct.ScmObj* %anf_45bind48210, %struct.ScmObj** %stackaddr$prim56471, align 8
%stackaddr$prim56472 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48210)
store volatile %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$prim56472, align 8
%stackaddr$makeclosure56473 = alloca %struct.ScmObj*, align 8
%fptrToInt56474 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49812 to i64
%ae49812 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt56474)
store volatile %struct.ScmObj* %ae49812, %struct.ScmObj** %stackaddr$makeclosure56473, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49812, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49812, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49812, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49812, %struct.ScmObj* %acc48132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49812, %struct.ScmObj* %_37foldl48129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49812, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49812, %struct.ScmObj* %_37map148077, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49812, %struct.ScmObj* %k48394, i64 7)
%ae49813 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56475 = alloca %struct.ScmObj*, align 8
%fptrToInt56476 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49814 to i64
%ae49814 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56476)
store volatile %struct.ScmObj* %ae49814, %struct.ScmObj** %stackaddr$makeclosure56475, align 8
%argslist55074$ae498120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56477 = alloca %struct.ScmObj*, align 8
%argslist55074$ae498121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49814, %struct.ScmObj* %argslist55074$ae498120)
store volatile %struct.ScmObj* %argslist55074$ae498121, %struct.ScmObj** %stackaddr$prim56477, align 8
%stackaddr$prim56478 = alloca %struct.ScmObj*, align 8
%argslist55074$ae498122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49813, %struct.ScmObj* %argslist55074$ae498121)
store volatile %struct.ScmObj* %argslist55074$ae498122, %struct.ScmObj** %stackaddr$prim56478, align 8
%clofunc56479 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49812)
musttail call tailcc void %clofunc56479(%struct.ScmObj* %ae49812, %struct.ScmObj* %argslist55074$ae498122)
ret void
}

define tailcc void @proc_clo$ae49812(%struct.ScmObj* %env$ae49812,%struct.ScmObj* %current_45args55020) {
%stackaddr$env-ref56480 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49812, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref56480
%stackaddr$env-ref56481 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49812, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref56481
%stackaddr$env-ref56482 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49812, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref56482
%stackaddr$env-ref56483 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49812, i64 3)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref56483
%stackaddr$env-ref56484 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49812, i64 4)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56484
%stackaddr$env-ref56485 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49812, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56485
%stackaddr$env-ref56486 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49812, i64 6)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref56486
%stackaddr$env-ref56487 = alloca %struct.ScmObj*, align 8
%k48394 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49812, i64 7)
store %struct.ScmObj* %k48394, %struct.ScmObj** %stackaddr$env-ref56487
%stackaddr$prim56488 = alloca %struct.ScmObj*, align 8
%_95k48395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55020)
store volatile %struct.ScmObj* %_95k48395, %struct.ScmObj** %stackaddr$prim56488, align 8
%stackaddr$prim56489 = alloca %struct.ScmObj*, align 8
%current_45args55021 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55020)
store volatile %struct.ScmObj* %current_45args55021, %struct.ScmObj** %stackaddr$prim56489, align 8
%stackaddr$prim56490 = alloca %struct.ScmObj*, align 8
%anf_45bind48211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55021)
store volatile %struct.ScmObj* %anf_45bind48211, %struct.ScmObj** %stackaddr$prim56490, align 8
%stackaddr$makeclosure56491 = alloca %struct.ScmObj*, align 8
%fptrToInt56492 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49844 to i64
%ae49844 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56492)
store volatile %struct.ScmObj* %ae49844, %struct.ScmObj** %stackaddr$makeclosure56491, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49844, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49844, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49844, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49844, %struct.ScmObj* %acc48132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49844, %struct.ScmObj* %_37foldl48129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49844, %struct.ScmObj* %_37map148077, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49844, %struct.ScmObj* %k48394, i64 6)
%ae49846 = call %struct.ScmObj* @const_init_false()
%argslist55067$_37foldr1480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56493 = alloca %struct.ScmObj*, align 8
%argslist55067$_37foldr1480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48131, %struct.ScmObj* %argslist55067$_37foldr1480460)
store volatile %struct.ScmObj* %argslist55067$_37foldr1480461, %struct.ScmObj** %stackaddr$prim56493, align 8
%stackaddr$prim56494 = alloca %struct.ScmObj*, align 8
%argslist55067$_37foldr1480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49846, %struct.ScmObj* %argslist55067$_37foldr1480461)
store volatile %struct.ScmObj* %argslist55067$_37foldr1480462, %struct.ScmObj** %stackaddr$prim56494, align 8
%stackaddr$prim56495 = alloca %struct.ScmObj*, align 8
%argslist55067$_37foldr1480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48211, %struct.ScmObj* %argslist55067$_37foldr1480462)
store volatile %struct.ScmObj* %argslist55067$_37foldr1480463, %struct.ScmObj** %stackaddr$prim56495, align 8
%stackaddr$prim56496 = alloca %struct.ScmObj*, align 8
%argslist55067$_37foldr1480464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49844, %struct.ScmObj* %argslist55067$_37foldr1480463)
store volatile %struct.ScmObj* %argslist55067$_37foldr1480464, %struct.ScmObj** %stackaddr$prim56496, align 8
%clofunc56497 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148046)
musttail call tailcc void %clofunc56497(%struct.ScmObj* %_37foldr148046, %struct.ScmObj* %argslist55067$_37foldr1480464)
ret void
}

define tailcc void @proc_clo$ae49844(%struct.ScmObj* %env$ae49844,%struct.ScmObj* %current_45args55023) {
%stackaddr$env-ref56498 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49844, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref56498
%stackaddr$env-ref56499 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49844, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref56499
%stackaddr$env-ref56500 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49844, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref56500
%stackaddr$env-ref56501 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49844, i64 3)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref56501
%stackaddr$env-ref56502 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49844, i64 4)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56502
%stackaddr$env-ref56503 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49844, i64 5)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref56503
%stackaddr$env-ref56504 = alloca %struct.ScmObj*, align 8
%k48394 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49844, i64 6)
store %struct.ScmObj* %k48394, %struct.ScmObj** %stackaddr$env-ref56504
%stackaddr$prim56505 = alloca %struct.ScmObj*, align 8
%_95k48396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55023)
store volatile %struct.ScmObj* %_95k48396, %struct.ScmObj** %stackaddr$prim56505, align 8
%stackaddr$prim56506 = alloca %struct.ScmObj*, align 8
%current_45args55024 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55023)
store volatile %struct.ScmObj* %current_45args55024, %struct.ScmObj** %stackaddr$prim56506, align 8
%stackaddr$prim56507 = alloca %struct.ScmObj*, align 8
%anf_45bind48212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55024)
store volatile %struct.ScmObj* %anf_45bind48212, %struct.ScmObj** %stackaddr$prim56507, align 8
%truthy$cmp56508 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48212)
%cmp$cmp56508 = icmp eq i64 %truthy$cmp56508, 1
br i1 %cmp$cmp56508, label %truebranch$cmp56508, label %falsebranch$cmp56508
truebranch$cmp56508:
%ae49855 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55026$k483940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56509 = alloca %struct.ScmObj*, align 8
%argslist55026$k483941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48132, %struct.ScmObj* %argslist55026$k483940)
store volatile %struct.ScmObj* %argslist55026$k483941, %struct.ScmObj** %stackaddr$prim56509, align 8
%stackaddr$prim56510 = alloca %struct.ScmObj*, align 8
%argslist55026$k483942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49855, %struct.ScmObj* %argslist55026$k483941)
store volatile %struct.ScmObj* %argslist55026$k483942, %struct.ScmObj** %stackaddr$prim56510, align 8
%clofunc56511 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48394)
musttail call tailcc void %clofunc56511(%struct.ScmObj* %k48394, %struct.ScmObj* %argslist55026$k483942)
ret void
falsebranch$cmp56508:
%stackaddr$makeclosure56512 = alloca %struct.ScmObj*, align 8
%fptrToInt56513 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49860 to i64
%ae49860 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56513)
store volatile %struct.ScmObj* %ae49860, %struct.ScmObj** %stackaddr$makeclosure56512, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49860, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49860, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49860, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49860, %struct.ScmObj* %acc48132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49860, %struct.ScmObj* %_37foldl48129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49860, %struct.ScmObj* %_37map148077, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49860, %struct.ScmObj* %k48394, i64 6)
%ae49861 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56514 = alloca %struct.ScmObj*, align 8
%fptrToInt56515 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49862 to i64
%ae49862 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56515)
store volatile %struct.ScmObj* %ae49862, %struct.ScmObj** %stackaddr$makeclosure56514, align 8
%argslist55066$ae498600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56516 = alloca %struct.ScmObj*, align 8
%argslist55066$ae498601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49862, %struct.ScmObj* %argslist55066$ae498600)
store volatile %struct.ScmObj* %argslist55066$ae498601, %struct.ScmObj** %stackaddr$prim56516, align 8
%stackaddr$prim56517 = alloca %struct.ScmObj*, align 8
%argslist55066$ae498602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49861, %struct.ScmObj* %argslist55066$ae498601)
store volatile %struct.ScmObj* %argslist55066$ae498602, %struct.ScmObj** %stackaddr$prim56517, align 8
%clofunc56518 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49860)
musttail call tailcc void %clofunc56518(%struct.ScmObj* %ae49860, %struct.ScmObj* %argslist55066$ae498602)
ret void
}

define tailcc void @proc_clo$ae49860(%struct.ScmObj* %env$ae49860,%struct.ScmObj* %current_45args55027) {
%stackaddr$env-ref56519 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49860, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref56519
%stackaddr$env-ref56520 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49860, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref56520
%stackaddr$env-ref56521 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49860, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref56521
%stackaddr$env-ref56522 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49860, i64 3)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref56522
%stackaddr$env-ref56523 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49860, i64 4)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56523
%stackaddr$env-ref56524 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49860, i64 5)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref56524
%stackaddr$env-ref56525 = alloca %struct.ScmObj*, align 8
%k48394 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49860, i64 6)
store %struct.ScmObj* %k48394, %struct.ScmObj** %stackaddr$env-ref56525
%stackaddr$prim56526 = alloca %struct.ScmObj*, align 8
%_95k48397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55027)
store volatile %struct.ScmObj* %_95k48397, %struct.ScmObj** %stackaddr$prim56526, align 8
%stackaddr$prim56527 = alloca %struct.ScmObj*, align 8
%current_45args55028 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55027)
store volatile %struct.ScmObj* %current_45args55028, %struct.ScmObj** %stackaddr$prim56527, align 8
%stackaddr$prim56528 = alloca %struct.ScmObj*, align 8
%anf_45bind48213 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55028)
store volatile %struct.ScmObj* %anf_45bind48213, %struct.ScmObj** %stackaddr$prim56528, align 8
%stackaddr$makeclosure56529 = alloca %struct.ScmObj*, align 8
%fptrToInt56530 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49881 to i64
%ae49881 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56530)
store volatile %struct.ScmObj* %ae49881, %struct.ScmObj** %stackaddr$makeclosure56529, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49881, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49881, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49881, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49881, %struct.ScmObj* %acc48132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49881, %struct.ScmObj* %_37foldl48129, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49881, %struct.ScmObj* %_37map148077, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49881, %struct.ScmObj* %k48394, i64 6)
%argslist55061$_37map1480770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56531 = alloca %struct.ScmObj*, align 8
%argslist55061$_37map1480771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48131, %struct.ScmObj* %argslist55061$_37map1480770)
store volatile %struct.ScmObj* %argslist55061$_37map1480771, %struct.ScmObj** %stackaddr$prim56531, align 8
%stackaddr$prim56532 = alloca %struct.ScmObj*, align 8
%argslist55061$_37map1480772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48213, %struct.ScmObj* %argslist55061$_37map1480771)
store volatile %struct.ScmObj* %argslist55061$_37map1480772, %struct.ScmObj** %stackaddr$prim56532, align 8
%stackaddr$prim56533 = alloca %struct.ScmObj*, align 8
%argslist55061$_37map1480773 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49881, %struct.ScmObj* %argslist55061$_37map1480772)
store volatile %struct.ScmObj* %argslist55061$_37map1480773, %struct.ScmObj** %stackaddr$prim56533, align 8
%clofunc56534 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148077)
musttail call tailcc void %clofunc56534(%struct.ScmObj* %_37map148077, %struct.ScmObj* %argslist55061$_37map1480773)
ret void
}

define tailcc void @proc_clo$ae49881(%struct.ScmObj* %env$ae49881,%struct.ScmObj* %current_45args55030) {
%stackaddr$env-ref56535 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49881, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref56535
%stackaddr$env-ref56536 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49881, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref56536
%stackaddr$env-ref56537 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49881, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref56537
%stackaddr$env-ref56538 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49881, i64 3)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref56538
%stackaddr$env-ref56539 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49881, i64 4)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56539
%stackaddr$env-ref56540 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49881, i64 5)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref56540
%stackaddr$env-ref56541 = alloca %struct.ScmObj*, align 8
%k48394 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49881, i64 6)
store %struct.ScmObj* %k48394, %struct.ScmObj** %stackaddr$env-ref56541
%stackaddr$prim56542 = alloca %struct.ScmObj*, align 8
%_95k48398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55030)
store volatile %struct.ScmObj* %_95k48398, %struct.ScmObj** %stackaddr$prim56542, align 8
%stackaddr$prim56543 = alloca %struct.ScmObj*, align 8
%current_45args55031 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55030)
store volatile %struct.ScmObj* %current_45args55031, %struct.ScmObj** %stackaddr$prim56543, align 8
%stackaddr$prim56544 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55031)
store volatile %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$prim56544, align 8
%stackaddr$makeclosure56545 = alloca %struct.ScmObj*, align 8
%fptrToInt56546 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49884 to i64
%ae49884 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt56546)
store volatile %struct.ScmObj* %ae49884, %struct.ScmObj** %stackaddr$makeclosure56545, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49884, %struct.ScmObj* %lsts48131, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49884, %struct.ScmObj* %_37foldr48051, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49884, %struct.ScmObj* %k48394, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49884, %struct.ScmObj* %lsts_4348138, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49884, %struct.ScmObj* %f48133, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49884, %struct.ScmObj* %acc48132, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49884, %struct.ScmObj* %_37foldl48129, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49884, %struct.ScmObj* %_37map148077, i64 7)
%ae49885 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56547 = alloca %struct.ScmObj*, align 8
%fptrToInt56548 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49886 to i64
%ae49886 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56548)
store volatile %struct.ScmObj* %ae49886, %struct.ScmObj** %stackaddr$makeclosure56547, align 8
%argslist55060$ae498840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56549 = alloca %struct.ScmObj*, align 8
%argslist55060$ae498841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49886, %struct.ScmObj* %argslist55060$ae498840)
store volatile %struct.ScmObj* %argslist55060$ae498841, %struct.ScmObj** %stackaddr$prim56549, align 8
%stackaddr$prim56550 = alloca %struct.ScmObj*, align 8
%argslist55060$ae498842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49885, %struct.ScmObj* %argslist55060$ae498841)
store volatile %struct.ScmObj* %argslist55060$ae498842, %struct.ScmObj** %stackaddr$prim56550, align 8
%clofunc56551 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49884)
musttail call tailcc void %clofunc56551(%struct.ScmObj* %ae49884, %struct.ScmObj* %argslist55060$ae498842)
ret void
}

define tailcc void @proc_clo$ae49884(%struct.ScmObj* %env$ae49884,%struct.ScmObj* %current_45args55033) {
%stackaddr$env-ref56552 = alloca %struct.ScmObj*, align 8
%lsts48131 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49884, i64 0)
store %struct.ScmObj* %lsts48131, %struct.ScmObj** %stackaddr$env-ref56552
%stackaddr$env-ref56553 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49884, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref56553
%stackaddr$env-ref56554 = alloca %struct.ScmObj*, align 8
%k48394 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49884, i64 2)
store %struct.ScmObj* %k48394, %struct.ScmObj** %stackaddr$env-ref56554
%stackaddr$env-ref56555 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49884, i64 3)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref56555
%stackaddr$env-ref56556 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49884, i64 4)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref56556
%stackaddr$env-ref56557 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49884, i64 5)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref56557
%stackaddr$env-ref56558 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49884, i64 6)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56558
%stackaddr$env-ref56559 = alloca %struct.ScmObj*, align 8
%_37map148077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49884, i64 7)
store %struct.ScmObj* %_37map148077, %struct.ScmObj** %stackaddr$env-ref56559
%stackaddr$prim56560 = alloca %struct.ScmObj*, align 8
%_95k48399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55033)
store volatile %struct.ScmObj* %_95k48399, %struct.ScmObj** %stackaddr$prim56560, align 8
%stackaddr$prim56561 = alloca %struct.ScmObj*, align 8
%current_45args55034 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55033)
store volatile %struct.ScmObj* %current_45args55034, %struct.ScmObj** %stackaddr$prim56561, align 8
%stackaddr$prim56562 = alloca %struct.ScmObj*, align 8
%anf_45bind48214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55034)
store volatile %struct.ScmObj* %anf_45bind48214, %struct.ScmObj** %stackaddr$prim56562, align 8
%stackaddr$makeclosure56563 = alloca %struct.ScmObj*, align 8
%fptrToInt56564 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49905 to i64
%ae49905 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt56564)
store volatile %struct.ScmObj* %ae49905, %struct.ScmObj** %stackaddr$makeclosure56563, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49905, %struct.ScmObj* %k48394, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49905, %struct.ScmObj* %lsts_4348138, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49905, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49905, %struct.ScmObj* %acc48132, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49905, %struct.ScmObj* %_37foldr48051, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49905, %struct.ScmObj* %_37foldl48129, i64 5)
%argslist55055$_37map1480770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56565 = alloca %struct.ScmObj*, align 8
%argslist55055$_37map1480771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48131, %struct.ScmObj* %argslist55055$_37map1480770)
store volatile %struct.ScmObj* %argslist55055$_37map1480771, %struct.ScmObj** %stackaddr$prim56565, align 8
%stackaddr$prim56566 = alloca %struct.ScmObj*, align 8
%argslist55055$_37map1480772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48214, %struct.ScmObj* %argslist55055$_37map1480771)
store volatile %struct.ScmObj* %argslist55055$_37map1480772, %struct.ScmObj** %stackaddr$prim56566, align 8
%stackaddr$prim56567 = alloca %struct.ScmObj*, align 8
%argslist55055$_37map1480773 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49905, %struct.ScmObj* %argslist55055$_37map1480772)
store volatile %struct.ScmObj* %argslist55055$_37map1480773, %struct.ScmObj** %stackaddr$prim56567, align 8
%clofunc56568 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148077)
musttail call tailcc void %clofunc56568(%struct.ScmObj* %_37map148077, %struct.ScmObj* %argslist55055$_37map1480773)
ret void
}

define tailcc void @proc_clo$ae49905(%struct.ScmObj* %env$ae49905,%struct.ScmObj* %current_45args55036) {
%stackaddr$env-ref56569 = alloca %struct.ScmObj*, align 8
%k48394 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49905, i64 0)
store %struct.ScmObj* %k48394, %struct.ScmObj** %stackaddr$env-ref56569
%stackaddr$env-ref56570 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49905, i64 1)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref56570
%stackaddr$env-ref56571 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49905, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref56571
%stackaddr$env-ref56572 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49905, i64 3)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref56572
%stackaddr$env-ref56573 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49905, i64 4)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref56573
%stackaddr$env-ref56574 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49905, i64 5)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56574
%stackaddr$prim56575 = alloca %struct.ScmObj*, align 8
%_95k48400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55036)
store volatile %struct.ScmObj* %_95k48400, %struct.ScmObj** %stackaddr$prim56575, align 8
%stackaddr$prim56576 = alloca %struct.ScmObj*, align 8
%current_45args55037 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55036)
store volatile %struct.ScmObj* %current_45args55037, %struct.ScmObj** %stackaddr$prim56576, align 8
%stackaddr$prim56577 = alloca %struct.ScmObj*, align 8
%vs48136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55037)
store volatile %struct.ScmObj* %vs48136, %struct.ScmObj** %stackaddr$prim56577, align 8
%stackaddr$makeclosure56578 = alloca %struct.ScmObj*, align 8
%fptrToInt56579 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49908 to i64
%ae49908 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56579)
store volatile %struct.ScmObj* %ae49908, %struct.ScmObj** %stackaddr$makeclosure56578, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49908, %struct.ScmObj* %k48394, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49908, %struct.ScmObj* %lsts_4348138, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49908, %struct.ScmObj* %vs48136, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49908, %struct.ScmObj* %f48133, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49908, %struct.ScmObj* %acc48132, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49908, %struct.ScmObj* %_37foldr48051, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49908, %struct.ScmObj* %_37foldl48129, i64 6)
%ae49909 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56580 = alloca %struct.ScmObj*, align 8
%fptrToInt56581 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49910 to i64
%ae49910 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56581)
store volatile %struct.ScmObj* %ae49910, %struct.ScmObj** %stackaddr$makeclosure56580, align 8
%argslist55054$ae499080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56582 = alloca %struct.ScmObj*, align 8
%argslist55054$ae499081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49910, %struct.ScmObj* %argslist55054$ae499080)
store volatile %struct.ScmObj* %argslist55054$ae499081, %struct.ScmObj** %stackaddr$prim56582, align 8
%stackaddr$prim56583 = alloca %struct.ScmObj*, align 8
%argslist55054$ae499082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49909, %struct.ScmObj* %argslist55054$ae499081)
store volatile %struct.ScmObj* %argslist55054$ae499082, %struct.ScmObj** %stackaddr$prim56583, align 8
%clofunc56584 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49908)
musttail call tailcc void %clofunc56584(%struct.ScmObj* %ae49908, %struct.ScmObj* %argslist55054$ae499082)
ret void
}

define tailcc void @proc_clo$ae49908(%struct.ScmObj* %env$ae49908,%struct.ScmObj* %current_45args55039) {
%stackaddr$env-ref56585 = alloca %struct.ScmObj*, align 8
%k48394 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49908, i64 0)
store %struct.ScmObj* %k48394, %struct.ScmObj** %stackaddr$env-ref56585
%stackaddr$env-ref56586 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49908, i64 1)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref56586
%stackaddr$env-ref56587 = alloca %struct.ScmObj*, align 8
%vs48136 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49908, i64 2)
store %struct.ScmObj* %vs48136, %struct.ScmObj** %stackaddr$env-ref56587
%stackaddr$env-ref56588 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49908, i64 3)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref56588
%stackaddr$env-ref56589 = alloca %struct.ScmObj*, align 8
%acc48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49908, i64 4)
store %struct.ScmObj* %acc48132, %struct.ScmObj** %stackaddr$env-ref56589
%stackaddr$env-ref56590 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49908, i64 5)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref56590
%stackaddr$env-ref56591 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49908, i64 6)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56591
%stackaddr$prim56592 = alloca %struct.ScmObj*, align 8
%_95k48401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55039)
store volatile %struct.ScmObj* %_95k48401, %struct.ScmObj** %stackaddr$prim56592, align 8
%stackaddr$prim56593 = alloca %struct.ScmObj*, align 8
%current_45args55040 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55039)
store volatile %struct.ScmObj* %current_45args55040, %struct.ScmObj** %stackaddr$prim56593, align 8
%stackaddr$prim56594 = alloca %struct.ScmObj*, align 8
%anf_45bind48215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55040)
store volatile %struct.ScmObj* %anf_45bind48215, %struct.ScmObj** %stackaddr$prim56594, align 8
%ae49931 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56595 = alloca %struct.ScmObj*, align 8
%anf_45bind48216 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48132, %struct.ScmObj* %ae49931)
store volatile %struct.ScmObj* %anf_45bind48216, %struct.ScmObj** %stackaddr$prim56595, align 8
%stackaddr$makeclosure56596 = alloca %struct.ScmObj*, align 8
%fptrToInt56597 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49933 to i64
%ae49933 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56597)
store volatile %struct.ScmObj* %ae49933, %struct.ScmObj** %stackaddr$makeclosure56596, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49933, %struct.ScmObj* %k48394, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49933, %struct.ScmObj* %lsts_4348138, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49933, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49933, %struct.ScmObj* %_37foldl48129, i64 3)
%argslist55048$_37foldr480510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56598 = alloca %struct.ScmObj*, align 8
%argslist55048$_37foldr480511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48136, %struct.ScmObj* %argslist55048$_37foldr480510)
store volatile %struct.ScmObj* %argslist55048$_37foldr480511, %struct.ScmObj** %stackaddr$prim56598, align 8
%stackaddr$prim56599 = alloca %struct.ScmObj*, align 8
%argslist55048$_37foldr480512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48216, %struct.ScmObj* %argslist55048$_37foldr480511)
store volatile %struct.ScmObj* %argslist55048$_37foldr480512, %struct.ScmObj** %stackaddr$prim56599, align 8
%stackaddr$prim56600 = alloca %struct.ScmObj*, align 8
%argslist55048$_37foldr480513 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48215, %struct.ScmObj* %argslist55048$_37foldr480512)
store volatile %struct.ScmObj* %argslist55048$_37foldr480513, %struct.ScmObj** %stackaddr$prim56600, align 8
%stackaddr$prim56601 = alloca %struct.ScmObj*, align 8
%argslist55048$_37foldr480514 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49933, %struct.ScmObj* %argslist55048$_37foldr480513)
store volatile %struct.ScmObj* %argslist55048$_37foldr480514, %struct.ScmObj** %stackaddr$prim56601, align 8
%clofunc56602 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48051)
musttail call tailcc void %clofunc56602(%struct.ScmObj* %_37foldr48051, %struct.ScmObj* %argslist55048$_37foldr480514)
ret void
}

define tailcc void @proc_clo$ae49933(%struct.ScmObj* %env$ae49933,%struct.ScmObj* %current_45args55042) {
%stackaddr$env-ref56603 = alloca %struct.ScmObj*, align 8
%k48394 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49933, i64 0)
store %struct.ScmObj* %k48394, %struct.ScmObj** %stackaddr$env-ref56603
%stackaddr$env-ref56604 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49933, i64 1)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref56604
%stackaddr$env-ref56605 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49933, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref56605
%stackaddr$env-ref56606 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49933, i64 3)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56606
%stackaddr$prim56607 = alloca %struct.ScmObj*, align 8
%_95k48402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55042)
store volatile %struct.ScmObj* %_95k48402, %struct.ScmObj** %stackaddr$prim56607, align 8
%stackaddr$prim56608 = alloca %struct.ScmObj*, align 8
%current_45args55043 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55042)
store volatile %struct.ScmObj* %current_45args55043, %struct.ScmObj** %stackaddr$prim56608, align 8
%stackaddr$prim56609 = alloca %struct.ScmObj*, align 8
%anf_45bind48217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55043)
store volatile %struct.ScmObj* %anf_45bind48217, %struct.ScmObj** %stackaddr$prim56609, align 8
%stackaddr$makeclosure56610 = alloca %struct.ScmObj*, align 8
%fptrToInt56611 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49937 to i64
%ae49937 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56611)
store volatile %struct.ScmObj* %ae49937, %struct.ScmObj** %stackaddr$makeclosure56610, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49937, %struct.ScmObj* %k48394, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49937, %struct.ScmObj* %lsts_4348138, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49937, %struct.ScmObj* %f48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49937, %struct.ScmObj* %_37foldl48129, i64 3)
%stackaddr$prim56612 = alloca %struct.ScmObj*, align 8
%cpsargs48405 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49937, %struct.ScmObj* %anf_45bind48217)
store volatile %struct.ScmObj* %cpsargs48405, %struct.ScmObj** %stackaddr$prim56612, align 8
%clofunc56613 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48133)
musttail call tailcc void %clofunc56613(%struct.ScmObj* %f48133, %struct.ScmObj* %cpsargs48405)
ret void
}

define tailcc void @proc_clo$ae49937(%struct.ScmObj* %env$ae49937,%struct.ScmObj* %current_45args55045) {
%stackaddr$env-ref56614 = alloca %struct.ScmObj*, align 8
%k48394 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49937, i64 0)
store %struct.ScmObj* %k48394, %struct.ScmObj** %stackaddr$env-ref56614
%stackaddr$env-ref56615 = alloca %struct.ScmObj*, align 8
%lsts_4348138 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49937, i64 1)
store %struct.ScmObj* %lsts_4348138, %struct.ScmObj** %stackaddr$env-ref56615
%stackaddr$env-ref56616 = alloca %struct.ScmObj*, align 8
%f48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49937, i64 2)
store %struct.ScmObj* %f48133, %struct.ScmObj** %stackaddr$env-ref56616
%stackaddr$env-ref56617 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49937, i64 3)
store %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$env-ref56617
%stackaddr$prim56618 = alloca %struct.ScmObj*, align 8
%_95k48403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55045)
store volatile %struct.ScmObj* %_95k48403, %struct.ScmObj** %stackaddr$prim56618, align 8
%stackaddr$prim56619 = alloca %struct.ScmObj*, align 8
%current_45args55046 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55045)
store volatile %struct.ScmObj* %current_45args55046, %struct.ScmObj** %stackaddr$prim56619, align 8
%stackaddr$prim56620 = alloca %struct.ScmObj*, align 8
%acc_4348140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55046)
store volatile %struct.ScmObj* %acc_4348140, %struct.ScmObj** %stackaddr$prim56620, align 8
%stackaddr$prim56621 = alloca %struct.ScmObj*, align 8
%anf_45bind48218 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4348140, %struct.ScmObj* %lsts_4348138)
store volatile %struct.ScmObj* %anf_45bind48218, %struct.ScmObj** %stackaddr$prim56621, align 8
%stackaddr$prim56622 = alloca %struct.ScmObj*, align 8
%anf_45bind48219 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48133, %struct.ScmObj* %anf_45bind48218)
store volatile %struct.ScmObj* %anf_45bind48219, %struct.ScmObj** %stackaddr$prim56622, align 8
%stackaddr$prim56623 = alloca %struct.ScmObj*, align 8
%cpsargs48404 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48394, %struct.ScmObj* %anf_45bind48219)
store volatile %struct.ScmObj* %cpsargs48404, %struct.ScmObj** %stackaddr$prim56623, align 8
%clofunc56624 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl48129)
musttail call tailcc void %clofunc56624(%struct.ScmObj* %_37foldl48129, %struct.ScmObj* %cpsargs48404)
ret void
}

define tailcc void @proc_clo$ae49910(%struct.ScmObj* %env$ae49910,%struct.ScmObj* %current_45args55049) {
%stackaddr$prim56625 = alloca %struct.ScmObj*, align 8
%k48406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55049)
store volatile %struct.ScmObj* %k48406, %struct.ScmObj** %stackaddr$prim56625, align 8
%stackaddr$prim56626 = alloca %struct.ScmObj*, align 8
%current_45args55050 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55049)
store volatile %struct.ScmObj* %current_45args55050, %struct.ScmObj** %stackaddr$prim56626, align 8
%stackaddr$prim56627 = alloca %struct.ScmObj*, align 8
%a48142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55050)
store volatile %struct.ScmObj* %a48142, %struct.ScmObj** %stackaddr$prim56627, align 8
%stackaddr$prim56628 = alloca %struct.ScmObj*, align 8
%current_45args55051 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55050)
store volatile %struct.ScmObj* %current_45args55051, %struct.ScmObj** %stackaddr$prim56628, align 8
%stackaddr$prim56629 = alloca %struct.ScmObj*, align 8
%b48141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55051)
store volatile %struct.ScmObj* %b48141, %struct.ScmObj** %stackaddr$prim56629, align 8
%stackaddr$prim56630 = alloca %struct.ScmObj*, align 8
%cpsprim48407 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48142, %struct.ScmObj* %b48141)
store volatile %struct.ScmObj* %cpsprim48407, %struct.ScmObj** %stackaddr$prim56630, align 8
%ae49914 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55053$k484060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56631 = alloca %struct.ScmObj*, align 8
%argslist55053$k484061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48407, %struct.ScmObj* %argslist55053$k484060)
store volatile %struct.ScmObj* %argslist55053$k484061, %struct.ScmObj** %stackaddr$prim56631, align 8
%stackaddr$prim56632 = alloca %struct.ScmObj*, align 8
%argslist55053$k484062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49914, %struct.ScmObj* %argslist55053$k484061)
store volatile %struct.ScmObj* %argslist55053$k484062, %struct.ScmObj** %stackaddr$prim56632, align 8
%clofunc56633 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48406)
musttail call tailcc void %clofunc56633(%struct.ScmObj* %k48406, %struct.ScmObj* %argslist55053$k484062)
ret void
}

define tailcc void @proc_clo$ae49886(%struct.ScmObj* %env$ae49886,%struct.ScmObj* %current_45args55056) {
%stackaddr$prim56634 = alloca %struct.ScmObj*, align 8
%k48408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55056)
store volatile %struct.ScmObj* %k48408, %struct.ScmObj** %stackaddr$prim56634, align 8
%stackaddr$prim56635 = alloca %struct.ScmObj*, align 8
%current_45args55057 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55056)
store volatile %struct.ScmObj* %current_45args55057, %struct.ScmObj** %stackaddr$prim56635, align 8
%stackaddr$prim56636 = alloca %struct.ScmObj*, align 8
%x48137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55057)
store volatile %struct.ScmObj* %x48137, %struct.ScmObj** %stackaddr$prim56636, align 8
%stackaddr$prim56637 = alloca %struct.ScmObj*, align 8
%cpsprim48409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48137)
store volatile %struct.ScmObj* %cpsprim48409, %struct.ScmObj** %stackaddr$prim56637, align 8
%ae49889 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55059$k484080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56638 = alloca %struct.ScmObj*, align 8
%argslist55059$k484081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48409, %struct.ScmObj* %argslist55059$k484080)
store volatile %struct.ScmObj* %argslist55059$k484081, %struct.ScmObj** %stackaddr$prim56638, align 8
%stackaddr$prim56639 = alloca %struct.ScmObj*, align 8
%argslist55059$k484082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49889, %struct.ScmObj* %argslist55059$k484081)
store volatile %struct.ScmObj* %argslist55059$k484082, %struct.ScmObj** %stackaddr$prim56639, align 8
%clofunc56640 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48408)
musttail call tailcc void %clofunc56640(%struct.ScmObj* %k48408, %struct.ScmObj* %argslist55059$k484082)
ret void
}

define tailcc void @proc_clo$ae49862(%struct.ScmObj* %env$ae49862,%struct.ScmObj* %current_45args55062) {
%stackaddr$prim56641 = alloca %struct.ScmObj*, align 8
%k48410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55062)
store volatile %struct.ScmObj* %k48410, %struct.ScmObj** %stackaddr$prim56641, align 8
%stackaddr$prim56642 = alloca %struct.ScmObj*, align 8
%current_45args55063 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55062)
store volatile %struct.ScmObj* %current_45args55063, %struct.ScmObj** %stackaddr$prim56642, align 8
%stackaddr$prim56643 = alloca %struct.ScmObj*, align 8
%x48139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55063)
store volatile %struct.ScmObj* %x48139, %struct.ScmObj** %stackaddr$prim56643, align 8
%stackaddr$prim56644 = alloca %struct.ScmObj*, align 8
%cpsprim48411 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48139)
store volatile %struct.ScmObj* %cpsprim48411, %struct.ScmObj** %stackaddr$prim56644, align 8
%ae49865 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55065$k484100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56645 = alloca %struct.ScmObj*, align 8
%argslist55065$k484101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48411, %struct.ScmObj* %argslist55065$k484100)
store volatile %struct.ScmObj* %argslist55065$k484101, %struct.ScmObj** %stackaddr$prim56645, align 8
%stackaddr$prim56646 = alloca %struct.ScmObj*, align 8
%argslist55065$k484102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49865, %struct.ScmObj* %argslist55065$k484101)
store volatile %struct.ScmObj* %argslist55065$k484102, %struct.ScmObj** %stackaddr$prim56646, align 8
%clofunc56647 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48410)
musttail call tailcc void %clofunc56647(%struct.ScmObj* %k48410, %struct.ScmObj* %argslist55065$k484102)
ret void
}

define tailcc void @proc_clo$ae49814(%struct.ScmObj* %env$ae49814,%struct.ScmObj* %current_45args55068) {
%stackaddr$prim56648 = alloca %struct.ScmObj*, align 8
%k48412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55068)
store volatile %struct.ScmObj* %k48412, %struct.ScmObj** %stackaddr$prim56648, align 8
%stackaddr$prim56649 = alloca %struct.ScmObj*, align 8
%current_45args55069 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55068)
store volatile %struct.ScmObj* %current_45args55069, %struct.ScmObj** %stackaddr$prim56649, align 8
%stackaddr$prim56650 = alloca %struct.ScmObj*, align 8
%lst48135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55069)
store volatile %struct.ScmObj* %lst48135, %struct.ScmObj** %stackaddr$prim56650, align 8
%stackaddr$prim56651 = alloca %struct.ScmObj*, align 8
%current_45args55070 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55069)
store volatile %struct.ScmObj* %current_45args55070, %struct.ScmObj** %stackaddr$prim56651, align 8
%stackaddr$prim56652 = alloca %struct.ScmObj*, align 8
%b48134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55070)
store volatile %struct.ScmObj* %b48134, %struct.ScmObj** %stackaddr$prim56652, align 8
%truthy$cmp56653 = call i64 @is_truthy_value(%struct.ScmObj* %b48134)
%cmp$cmp56653 = icmp eq i64 %truthy$cmp56653, 1
br i1 %cmp$cmp56653, label %truebranch$cmp56653, label %falsebranch$cmp56653
truebranch$cmp56653:
%ae49817 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55072$k484120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56654 = alloca %struct.ScmObj*, align 8
%argslist55072$k484121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48134, %struct.ScmObj* %argslist55072$k484120)
store volatile %struct.ScmObj* %argslist55072$k484121, %struct.ScmObj** %stackaddr$prim56654, align 8
%stackaddr$prim56655 = alloca %struct.ScmObj*, align 8
%argslist55072$k484122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49817, %struct.ScmObj* %argslist55072$k484121)
store volatile %struct.ScmObj* %argslist55072$k484122, %struct.ScmObj** %stackaddr$prim56655, align 8
%clofunc56656 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48412)
musttail call tailcc void %clofunc56656(%struct.ScmObj* %k48412, %struct.ScmObj* %argslist55072$k484122)
ret void
falsebranch$cmp56653:
%stackaddr$prim56657 = alloca %struct.ScmObj*, align 8
%cpsprim48413 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48135)
store volatile %struct.ScmObj* %cpsprim48413, %struct.ScmObj** %stackaddr$prim56657, align 8
%ae49824 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55073$k484120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56658 = alloca %struct.ScmObj*, align 8
%argslist55073$k484121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48413, %struct.ScmObj* %argslist55073$k484120)
store volatile %struct.ScmObj* %argslist55073$k484121, %struct.ScmObj** %stackaddr$prim56658, align 8
%stackaddr$prim56659 = alloca %struct.ScmObj*, align 8
%argslist55073$k484122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49824, %struct.ScmObj* %argslist55073$k484121)
store volatile %struct.ScmObj* %argslist55073$k484122, %struct.ScmObj** %stackaddr$prim56659, align 8
%clofunc56660 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48412)
musttail call tailcc void %clofunc56660(%struct.ScmObj* %k48412, %struct.ScmObj* %argslist55073$k484122)
ret void
}

define tailcc void @proc_clo$ae49655(%struct.ScmObj* %env$ae49655,%struct.ScmObj* %args4807348414) {
%stackaddr$env-ref56661 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49655, i64 0)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref56661
%stackaddr$env-ref56662 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49655, i64 1)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref56662
%stackaddr$env-ref56663 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49655, i64 2)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref56663
%stackaddr$prim56664 = alloca %struct.ScmObj*, align 8
%k48415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4807348414)
store volatile %struct.ScmObj* %k48415, %struct.ScmObj** %stackaddr$prim56664, align 8
%stackaddr$prim56665 = alloca %struct.ScmObj*, align 8
%args48073 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4807348414)
store volatile %struct.ScmObj* %args48073, %struct.ScmObj** %stackaddr$prim56665, align 8
%stackaddr$prim56666 = alloca %struct.ScmObj*, align 8
%f48075 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48073)
store volatile %struct.ScmObj* %f48075, %struct.ScmObj** %stackaddr$prim56666, align 8
%stackaddr$prim56667 = alloca %struct.ScmObj*, align 8
%lsts48074 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48073)
store volatile %struct.ScmObj* %lsts48074, %struct.ScmObj** %stackaddr$prim56667, align 8
%stackaddr$makeclosure56668 = alloca %struct.ScmObj*, align 8
%fptrToInt56669 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49660 to i64
%ae49660 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56669)
store volatile %struct.ScmObj* %ae49660, %struct.ScmObj** %stackaddr$makeclosure56668, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49660, %struct.ScmObj* %_37foldr48051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49660, %struct.ScmObj* %k48415, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49660, %struct.ScmObj* %lsts48074, i64 2)
%ae49661 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56670 = alloca %struct.ScmObj*, align 8
%fptrToInt56671 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49662 to i64
%ae49662 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56671)
store volatile %struct.ScmObj* %ae49662, %struct.ScmObj** %stackaddr$makeclosure56670, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49662, %struct.ScmObj* %_37last48068, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49662, %struct.ScmObj* %_37drop_45right48065, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49662, %struct.ScmObj* %f48075, i64 2)
%argslist55092$ae496600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56672 = alloca %struct.ScmObj*, align 8
%argslist55092$ae496601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49662, %struct.ScmObj* %argslist55092$ae496600)
store volatile %struct.ScmObj* %argslist55092$ae496601, %struct.ScmObj** %stackaddr$prim56672, align 8
%stackaddr$prim56673 = alloca %struct.ScmObj*, align 8
%argslist55092$ae496602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49661, %struct.ScmObj* %argslist55092$ae496601)
store volatile %struct.ScmObj* %argslist55092$ae496602, %struct.ScmObj** %stackaddr$prim56673, align 8
%clofunc56674 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49660)
musttail call tailcc void %clofunc56674(%struct.ScmObj* %ae49660, %struct.ScmObj* %argslist55092$ae496602)
ret void
}

define tailcc void @proc_clo$ae49660(%struct.ScmObj* %env$ae49660,%struct.ScmObj* %current_45args55077) {
%stackaddr$env-ref56675 = alloca %struct.ScmObj*, align 8
%_37foldr48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49660, i64 0)
store %struct.ScmObj* %_37foldr48051, %struct.ScmObj** %stackaddr$env-ref56675
%stackaddr$env-ref56676 = alloca %struct.ScmObj*, align 8
%k48415 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49660, i64 1)
store %struct.ScmObj* %k48415, %struct.ScmObj** %stackaddr$env-ref56676
%stackaddr$env-ref56677 = alloca %struct.ScmObj*, align 8
%lsts48074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49660, i64 2)
store %struct.ScmObj* %lsts48074, %struct.ScmObj** %stackaddr$env-ref56677
%stackaddr$prim56678 = alloca %struct.ScmObj*, align 8
%_95k48416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55077)
store volatile %struct.ScmObj* %_95k48416, %struct.ScmObj** %stackaddr$prim56678, align 8
%stackaddr$prim56679 = alloca %struct.ScmObj*, align 8
%current_45args55078 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55077)
store volatile %struct.ScmObj* %current_45args55078, %struct.ScmObj** %stackaddr$prim56679, align 8
%stackaddr$prim56680 = alloca %struct.ScmObj*, align 8
%anf_45bind48206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55078)
store volatile %struct.ScmObj* %anf_45bind48206, %struct.ScmObj** %stackaddr$prim56680, align 8
%ae49723 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56681 = alloca %struct.ScmObj*, align 8
%anf_45bind48207 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49723, %struct.ScmObj* %lsts48074)
store volatile %struct.ScmObj* %anf_45bind48207, %struct.ScmObj** %stackaddr$prim56681, align 8
%stackaddr$prim56682 = alloca %struct.ScmObj*, align 8
%anf_45bind48208 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48206, %struct.ScmObj* %anf_45bind48207)
store volatile %struct.ScmObj* %anf_45bind48208, %struct.ScmObj** %stackaddr$prim56682, align 8
%stackaddr$prim56683 = alloca %struct.ScmObj*, align 8
%cpsargs48417 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48415, %struct.ScmObj* %anf_45bind48208)
store volatile %struct.ScmObj* %cpsargs48417, %struct.ScmObj** %stackaddr$prim56683, align 8
%clofunc56684 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48051)
musttail call tailcc void %clofunc56684(%struct.ScmObj* %_37foldr48051, %struct.ScmObj* %cpsargs48417)
ret void
}

define tailcc void @proc_clo$ae49662(%struct.ScmObj* %env$ae49662,%struct.ScmObj* %fargs4807648418) {
%stackaddr$env-ref56685 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49662, i64 0)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref56685
%stackaddr$env-ref56686 = alloca %struct.ScmObj*, align 8
%_37drop_45right48065 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49662, i64 1)
store %struct.ScmObj* %_37drop_45right48065, %struct.ScmObj** %stackaddr$env-ref56686
%stackaddr$env-ref56687 = alloca %struct.ScmObj*, align 8
%f48075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49662, i64 2)
store %struct.ScmObj* %f48075, %struct.ScmObj** %stackaddr$env-ref56687
%stackaddr$prim56688 = alloca %struct.ScmObj*, align 8
%k48419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4807648418)
store volatile %struct.ScmObj* %k48419, %struct.ScmObj** %stackaddr$prim56688, align 8
%stackaddr$prim56689 = alloca %struct.ScmObj*, align 8
%fargs48076 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4807648418)
store volatile %struct.ScmObj* %fargs48076, %struct.ScmObj** %stackaddr$prim56689, align 8
%stackaddr$makeclosure56690 = alloca %struct.ScmObj*, align 8
%fptrToInt56691 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49666 to i64
%ae49666 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56691)
store volatile %struct.ScmObj* %ae49666, %struct.ScmObj** %stackaddr$makeclosure56690, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49666, %struct.ScmObj* %_37last48068, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49666, %struct.ScmObj* %k48419, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49666, %struct.ScmObj* %fargs48076, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49666, %struct.ScmObj* %f48075, i64 3)
%ae49668 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist55091$_37drop_45right480650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56692 = alloca %struct.ScmObj*, align 8
%argslist55091$_37drop_45right480651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49668, %struct.ScmObj* %argslist55091$_37drop_45right480650)
store volatile %struct.ScmObj* %argslist55091$_37drop_45right480651, %struct.ScmObj** %stackaddr$prim56692, align 8
%stackaddr$prim56693 = alloca %struct.ScmObj*, align 8
%argslist55091$_37drop_45right480652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48076, %struct.ScmObj* %argslist55091$_37drop_45right480651)
store volatile %struct.ScmObj* %argslist55091$_37drop_45right480652, %struct.ScmObj** %stackaddr$prim56693, align 8
%stackaddr$prim56694 = alloca %struct.ScmObj*, align 8
%argslist55091$_37drop_45right480653 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49666, %struct.ScmObj* %argslist55091$_37drop_45right480652)
store volatile %struct.ScmObj* %argslist55091$_37drop_45right480653, %struct.ScmObj** %stackaddr$prim56694, align 8
%clofunc56695 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right48065)
musttail call tailcc void %clofunc56695(%struct.ScmObj* %_37drop_45right48065, %struct.ScmObj* %argslist55091$_37drop_45right480653)
ret void
}

define tailcc void @proc_clo$ae49666(%struct.ScmObj* %env$ae49666,%struct.ScmObj* %current_45args55080) {
%stackaddr$env-ref56696 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49666, i64 0)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref56696
%stackaddr$env-ref56697 = alloca %struct.ScmObj*, align 8
%k48419 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49666, i64 1)
store %struct.ScmObj* %k48419, %struct.ScmObj** %stackaddr$env-ref56697
%stackaddr$env-ref56698 = alloca %struct.ScmObj*, align 8
%fargs48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49666, i64 2)
store %struct.ScmObj* %fargs48076, %struct.ScmObj** %stackaddr$env-ref56698
%stackaddr$env-ref56699 = alloca %struct.ScmObj*, align 8
%f48075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49666, i64 3)
store %struct.ScmObj* %f48075, %struct.ScmObj** %stackaddr$env-ref56699
%stackaddr$prim56700 = alloca %struct.ScmObj*, align 8
%_95k48420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55080)
store volatile %struct.ScmObj* %_95k48420, %struct.ScmObj** %stackaddr$prim56700, align 8
%stackaddr$prim56701 = alloca %struct.ScmObj*, align 8
%current_45args55081 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55080)
store volatile %struct.ScmObj* %current_45args55081, %struct.ScmObj** %stackaddr$prim56701, align 8
%stackaddr$prim56702 = alloca %struct.ScmObj*, align 8
%anf_45bind48203 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55081)
store volatile %struct.ScmObj* %anf_45bind48203, %struct.ScmObj** %stackaddr$prim56702, align 8
%stackaddr$makeclosure56703 = alloca %struct.ScmObj*, align 8
%fptrToInt56704 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49673 to i64
%ae49673 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56704)
store volatile %struct.ScmObj* %ae49673, %struct.ScmObj** %stackaddr$makeclosure56703, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49673, %struct.ScmObj* %_37last48068, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49673, %struct.ScmObj* %k48419, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49673, %struct.ScmObj* %fargs48076, i64 2)
%stackaddr$prim56705 = alloca %struct.ScmObj*, align 8
%cpsargs48424 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49673, %struct.ScmObj* %anf_45bind48203)
store volatile %struct.ScmObj* %cpsargs48424, %struct.ScmObj** %stackaddr$prim56705, align 8
%clofunc56706 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48075)
musttail call tailcc void %clofunc56706(%struct.ScmObj* %f48075, %struct.ScmObj* %cpsargs48424)
ret void
}

define tailcc void @proc_clo$ae49673(%struct.ScmObj* %env$ae49673,%struct.ScmObj* %current_45args55083) {
%stackaddr$env-ref56707 = alloca %struct.ScmObj*, align 8
%_37last48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49673, i64 0)
store %struct.ScmObj* %_37last48068, %struct.ScmObj** %stackaddr$env-ref56707
%stackaddr$env-ref56708 = alloca %struct.ScmObj*, align 8
%k48419 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49673, i64 1)
store %struct.ScmObj* %k48419, %struct.ScmObj** %stackaddr$env-ref56708
%stackaddr$env-ref56709 = alloca %struct.ScmObj*, align 8
%fargs48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49673, i64 2)
store %struct.ScmObj* %fargs48076, %struct.ScmObj** %stackaddr$env-ref56709
%stackaddr$prim56710 = alloca %struct.ScmObj*, align 8
%_95k48421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55083)
store volatile %struct.ScmObj* %_95k48421, %struct.ScmObj** %stackaddr$prim56710, align 8
%stackaddr$prim56711 = alloca %struct.ScmObj*, align 8
%current_45args55084 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55083)
store volatile %struct.ScmObj* %current_45args55084, %struct.ScmObj** %stackaddr$prim56711, align 8
%stackaddr$prim56712 = alloca %struct.ScmObj*, align 8
%anf_45bind48204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55084)
store volatile %struct.ScmObj* %anf_45bind48204, %struct.ScmObj** %stackaddr$prim56712, align 8
%stackaddr$makeclosure56713 = alloca %struct.ScmObj*, align 8
%fptrToInt56714 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49678 to i64
%ae49678 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56714)
store volatile %struct.ScmObj* %ae49678, %struct.ScmObj** %stackaddr$makeclosure56713, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49678, %struct.ScmObj* %k48419, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49678, %struct.ScmObj* %anf_45bind48204, i64 1)
%argslist55090$_37last480680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56715 = alloca %struct.ScmObj*, align 8
%argslist55090$_37last480681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48076, %struct.ScmObj* %argslist55090$_37last480680)
store volatile %struct.ScmObj* %argslist55090$_37last480681, %struct.ScmObj** %stackaddr$prim56715, align 8
%stackaddr$prim56716 = alloca %struct.ScmObj*, align 8
%argslist55090$_37last480682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49678, %struct.ScmObj* %argslist55090$_37last480681)
store volatile %struct.ScmObj* %argslist55090$_37last480682, %struct.ScmObj** %stackaddr$prim56716, align 8
%clofunc56717 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last48068)
musttail call tailcc void %clofunc56717(%struct.ScmObj* %_37last48068, %struct.ScmObj* %argslist55090$_37last480682)
ret void
}

define tailcc void @proc_clo$ae49678(%struct.ScmObj* %env$ae49678,%struct.ScmObj* %current_45args55086) {
%stackaddr$env-ref56718 = alloca %struct.ScmObj*, align 8
%k48419 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49678, i64 0)
store %struct.ScmObj* %k48419, %struct.ScmObj** %stackaddr$env-ref56718
%stackaddr$env-ref56719 = alloca %struct.ScmObj*, align 8
%anf_45bind48204 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49678, i64 1)
store %struct.ScmObj* %anf_45bind48204, %struct.ScmObj** %stackaddr$env-ref56719
%stackaddr$prim56720 = alloca %struct.ScmObj*, align 8
%_95k48422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55086)
store volatile %struct.ScmObj* %_95k48422, %struct.ScmObj** %stackaddr$prim56720, align 8
%stackaddr$prim56721 = alloca %struct.ScmObj*, align 8
%current_45args55087 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55086)
store volatile %struct.ScmObj* %current_45args55087, %struct.ScmObj** %stackaddr$prim56721, align 8
%stackaddr$prim56722 = alloca %struct.ScmObj*, align 8
%anf_45bind48205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55087)
store volatile %struct.ScmObj* %anf_45bind48205, %struct.ScmObj** %stackaddr$prim56722, align 8
%stackaddr$prim56723 = alloca %struct.ScmObj*, align 8
%cpsprim48423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48204, %struct.ScmObj* %anf_45bind48205)
store volatile %struct.ScmObj* %cpsprim48423, %struct.ScmObj** %stackaddr$prim56723, align 8
%ae49683 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55089$k484190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56724 = alloca %struct.ScmObj*, align 8
%argslist55089$k484191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48423, %struct.ScmObj* %argslist55089$k484190)
store volatile %struct.ScmObj* %argslist55089$k484191, %struct.ScmObj** %stackaddr$prim56724, align 8
%stackaddr$prim56725 = alloca %struct.ScmObj*, align 8
%argslist55089$k484192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49683, %struct.ScmObj* %argslist55089$k484191)
store volatile %struct.ScmObj* %argslist55089$k484192, %struct.ScmObj** %stackaddr$prim56725, align 8
%clofunc56726 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48419)
musttail call tailcc void %clofunc56726(%struct.ScmObj* %k48419, %struct.ScmObj* %argslist55089$k484192)
ret void
}

define tailcc void @proc_clo$ae49578(%struct.ScmObj* %env$ae49578,%struct.ScmObj* %current_45args55094) {
%stackaddr$env-ref56727 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49578, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56727
%stackaddr$prim56728 = alloca %struct.ScmObj*, align 8
%k48425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55094)
store volatile %struct.ScmObj* %k48425, %struct.ScmObj** %stackaddr$prim56728, align 8
%stackaddr$prim56729 = alloca %struct.ScmObj*, align 8
%current_45args55095 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55094)
store volatile %struct.ScmObj* %current_45args55095, %struct.ScmObj** %stackaddr$prim56729, align 8
%stackaddr$prim56730 = alloca %struct.ScmObj*, align 8
%f48079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55095)
store volatile %struct.ScmObj* %f48079, %struct.ScmObj** %stackaddr$prim56730, align 8
%stackaddr$prim56731 = alloca %struct.ScmObj*, align 8
%current_45args55096 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55095)
store volatile %struct.ScmObj* %current_45args55096, %struct.ScmObj** %stackaddr$prim56731, align 8
%stackaddr$prim56732 = alloca %struct.ScmObj*, align 8
%lst48078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55096)
store volatile %struct.ScmObj* %lst48078, %struct.ScmObj** %stackaddr$prim56732, align 8
%stackaddr$makeclosure56733 = alloca %struct.ScmObj*, align 8
%fptrToInt56734 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49579 to i64
%ae49579 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56734)
store volatile %struct.ScmObj* %ae49579, %struct.ScmObj** %stackaddr$makeclosure56733, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49579, %struct.ScmObj* %lst48078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49579, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49579, %struct.ScmObj* %k48425, i64 2)
%ae49580 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56735 = alloca %struct.ScmObj*, align 8
%fptrToInt56736 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49581 to i64
%ae49581 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56736)
store volatile %struct.ScmObj* %ae49581, %struct.ScmObj** %stackaddr$makeclosure56735, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49581, %struct.ScmObj* %f48079, i64 0)
%argslist55111$ae495790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56737 = alloca %struct.ScmObj*, align 8
%argslist55111$ae495791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49581, %struct.ScmObj* %argslist55111$ae495790)
store volatile %struct.ScmObj* %argslist55111$ae495791, %struct.ScmObj** %stackaddr$prim56737, align 8
%stackaddr$prim56738 = alloca %struct.ScmObj*, align 8
%argslist55111$ae495792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49580, %struct.ScmObj* %argslist55111$ae495791)
store volatile %struct.ScmObj* %argslist55111$ae495792, %struct.ScmObj** %stackaddr$prim56738, align 8
%clofunc56739 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49579)
musttail call tailcc void %clofunc56739(%struct.ScmObj* %ae49579, %struct.ScmObj* %argslist55111$ae495792)
ret void
}

define tailcc void @proc_clo$ae49579(%struct.ScmObj* %env$ae49579,%struct.ScmObj* %current_45args55098) {
%stackaddr$env-ref56740 = alloca %struct.ScmObj*, align 8
%lst48078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49579, i64 0)
store %struct.ScmObj* %lst48078, %struct.ScmObj** %stackaddr$env-ref56740
%stackaddr$env-ref56741 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49579, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56741
%stackaddr$env-ref56742 = alloca %struct.ScmObj*, align 8
%k48425 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49579, i64 2)
store %struct.ScmObj* %k48425, %struct.ScmObj** %stackaddr$env-ref56742
%stackaddr$prim56743 = alloca %struct.ScmObj*, align 8
%_95k48426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55098)
store volatile %struct.ScmObj* %_95k48426, %struct.ScmObj** %stackaddr$prim56743, align 8
%stackaddr$prim56744 = alloca %struct.ScmObj*, align 8
%current_45args55099 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55098)
store volatile %struct.ScmObj* %current_45args55099, %struct.ScmObj** %stackaddr$prim56744, align 8
%stackaddr$prim56745 = alloca %struct.ScmObj*, align 8
%anf_45bind48202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55099)
store volatile %struct.ScmObj* %anf_45bind48202, %struct.ScmObj** %stackaddr$prim56745, align 8
%ae49613 = call %struct.ScmObj* @const_init_null()
%argslist55101$_37foldr1480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56746 = alloca %struct.ScmObj*, align 8
%argslist55101$_37foldr1480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48078, %struct.ScmObj* %argslist55101$_37foldr1480460)
store volatile %struct.ScmObj* %argslist55101$_37foldr1480461, %struct.ScmObj** %stackaddr$prim56746, align 8
%stackaddr$prim56747 = alloca %struct.ScmObj*, align 8
%argslist55101$_37foldr1480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49613, %struct.ScmObj* %argslist55101$_37foldr1480461)
store volatile %struct.ScmObj* %argslist55101$_37foldr1480462, %struct.ScmObj** %stackaddr$prim56747, align 8
%stackaddr$prim56748 = alloca %struct.ScmObj*, align 8
%argslist55101$_37foldr1480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48202, %struct.ScmObj* %argslist55101$_37foldr1480462)
store volatile %struct.ScmObj* %argslist55101$_37foldr1480463, %struct.ScmObj** %stackaddr$prim56748, align 8
%stackaddr$prim56749 = alloca %struct.ScmObj*, align 8
%argslist55101$_37foldr1480464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48425, %struct.ScmObj* %argslist55101$_37foldr1480463)
store volatile %struct.ScmObj* %argslist55101$_37foldr1480464, %struct.ScmObj** %stackaddr$prim56749, align 8
%clofunc56750 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148046)
musttail call tailcc void %clofunc56750(%struct.ScmObj* %_37foldr148046, %struct.ScmObj* %argslist55101$_37foldr1480464)
ret void
}

define tailcc void @proc_clo$ae49581(%struct.ScmObj* %env$ae49581,%struct.ScmObj* %current_45args55102) {
%stackaddr$env-ref56751 = alloca %struct.ScmObj*, align 8
%f48079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49581, i64 0)
store %struct.ScmObj* %f48079, %struct.ScmObj** %stackaddr$env-ref56751
%stackaddr$prim56752 = alloca %struct.ScmObj*, align 8
%k48427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55102)
store volatile %struct.ScmObj* %k48427, %struct.ScmObj** %stackaddr$prim56752, align 8
%stackaddr$prim56753 = alloca %struct.ScmObj*, align 8
%current_45args55103 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55102)
store volatile %struct.ScmObj* %current_45args55103, %struct.ScmObj** %stackaddr$prim56753, align 8
%stackaddr$prim56754 = alloca %struct.ScmObj*, align 8
%v48081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55103)
store volatile %struct.ScmObj* %v48081, %struct.ScmObj** %stackaddr$prim56754, align 8
%stackaddr$prim56755 = alloca %struct.ScmObj*, align 8
%current_45args55104 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55103)
store volatile %struct.ScmObj* %current_45args55104, %struct.ScmObj** %stackaddr$prim56755, align 8
%stackaddr$prim56756 = alloca %struct.ScmObj*, align 8
%r48080 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55104)
store volatile %struct.ScmObj* %r48080, %struct.ScmObj** %stackaddr$prim56756, align 8
%stackaddr$makeclosure56757 = alloca %struct.ScmObj*, align 8
%fptrToInt56758 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49583 to i64
%ae49583 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56758)
store volatile %struct.ScmObj* %ae49583, %struct.ScmObj** %stackaddr$makeclosure56757, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49583, %struct.ScmObj* %r48080, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49583, %struct.ScmObj* %k48427, i64 1)
%argslist55110$f480790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56759 = alloca %struct.ScmObj*, align 8
%argslist55110$f480791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v48081, %struct.ScmObj* %argslist55110$f480790)
store volatile %struct.ScmObj* %argslist55110$f480791, %struct.ScmObj** %stackaddr$prim56759, align 8
%stackaddr$prim56760 = alloca %struct.ScmObj*, align 8
%argslist55110$f480792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49583, %struct.ScmObj* %argslist55110$f480791)
store volatile %struct.ScmObj* %argslist55110$f480792, %struct.ScmObj** %stackaddr$prim56760, align 8
%clofunc56761 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48079)
musttail call tailcc void %clofunc56761(%struct.ScmObj* %f48079, %struct.ScmObj* %argslist55110$f480792)
ret void
}

define tailcc void @proc_clo$ae49583(%struct.ScmObj* %env$ae49583,%struct.ScmObj* %current_45args55106) {
%stackaddr$env-ref56762 = alloca %struct.ScmObj*, align 8
%r48080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49583, i64 0)
store %struct.ScmObj* %r48080, %struct.ScmObj** %stackaddr$env-ref56762
%stackaddr$env-ref56763 = alloca %struct.ScmObj*, align 8
%k48427 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49583, i64 1)
store %struct.ScmObj* %k48427, %struct.ScmObj** %stackaddr$env-ref56763
%stackaddr$prim56764 = alloca %struct.ScmObj*, align 8
%_95k48428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55106)
store volatile %struct.ScmObj* %_95k48428, %struct.ScmObj** %stackaddr$prim56764, align 8
%stackaddr$prim56765 = alloca %struct.ScmObj*, align 8
%current_45args55107 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55106)
store volatile %struct.ScmObj* %current_45args55107, %struct.ScmObj** %stackaddr$prim56765, align 8
%stackaddr$prim56766 = alloca %struct.ScmObj*, align 8
%anf_45bind48201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55107)
store volatile %struct.ScmObj* %anf_45bind48201, %struct.ScmObj** %stackaddr$prim56766, align 8
%stackaddr$prim56767 = alloca %struct.ScmObj*, align 8
%cpsprim48429 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48201, %struct.ScmObj* %r48080)
store volatile %struct.ScmObj* %cpsprim48429, %struct.ScmObj** %stackaddr$prim56767, align 8
%ae49588 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55109$k484270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56768 = alloca %struct.ScmObj*, align 8
%argslist55109$k484271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48429, %struct.ScmObj* %argslist55109$k484270)
store volatile %struct.ScmObj* %argslist55109$k484271, %struct.ScmObj** %stackaddr$prim56768, align 8
%stackaddr$prim56769 = alloca %struct.ScmObj*, align 8
%argslist55109$k484272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49588, %struct.ScmObj* %argslist55109$k484271)
store volatile %struct.ScmObj* %argslist55109$k484272, %struct.ScmObj** %stackaddr$prim56769, align 8
%clofunc56770 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48427)
musttail call tailcc void %clofunc56770(%struct.ScmObj* %k48427, %struct.ScmObj* %argslist55109$k484272)
ret void
}

define tailcc void @proc_clo$ae49192(%struct.ScmObj* %env$ae49192,%struct.ScmObj* %current_45args55114) {
%stackaddr$env-ref56771 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49192, i64 0)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56771
%stackaddr$env-ref56772 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49192, i64 1)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref56772
%stackaddr$prim56773 = alloca %struct.ScmObj*, align 8
%k48430 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55114)
store volatile %struct.ScmObj* %k48430, %struct.ScmObj** %stackaddr$prim56773, align 8
%stackaddr$prim56774 = alloca %struct.ScmObj*, align 8
%current_45args55115 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55114)
store volatile %struct.ScmObj* %current_45args55115, %struct.ScmObj** %stackaddr$prim56774, align 8
%stackaddr$prim56775 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55115)
store volatile %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$prim56775, align 8
%ae49194 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56776 = alloca %struct.ScmObj*, align 8
%fptrToInt56777 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49195 to i64
%ae49195 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56777)
store volatile %struct.ScmObj* %ae49195, %struct.ScmObj** %stackaddr$makeclosure56776, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49195, %struct.ScmObj* %_37foldr48052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49195, %struct.ScmObj* %_37foldr148046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49195, %struct.ScmObj* %_37map148042, i64 2)
%argslist55172$k484300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56778 = alloca %struct.ScmObj*, align 8
%argslist55172$k484301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49195, %struct.ScmObj* %argslist55172$k484300)
store volatile %struct.ScmObj* %argslist55172$k484301, %struct.ScmObj** %stackaddr$prim56778, align 8
%stackaddr$prim56779 = alloca %struct.ScmObj*, align 8
%argslist55172$k484302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49194, %struct.ScmObj* %argslist55172$k484301)
store volatile %struct.ScmObj* %argslist55172$k484302, %struct.ScmObj** %stackaddr$prim56779, align 8
%clofunc56780 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48430)
musttail call tailcc void %clofunc56780(%struct.ScmObj* %k48430, %struct.ScmObj* %argslist55172$k484302)
ret void
}

define tailcc void @proc_clo$ae49195(%struct.ScmObj* %env$ae49195,%struct.ScmObj* %args4805348431) {
%stackaddr$env-ref56781 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49195, i64 0)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56781
%stackaddr$env-ref56782 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49195, i64 1)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56782
%stackaddr$env-ref56783 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49195, i64 2)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref56783
%stackaddr$prim56784 = alloca %struct.ScmObj*, align 8
%k48432 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4805348431)
store volatile %struct.ScmObj* %k48432, %struct.ScmObj** %stackaddr$prim56784, align 8
%stackaddr$prim56785 = alloca %struct.ScmObj*, align 8
%args48053 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4805348431)
store volatile %struct.ScmObj* %args48053, %struct.ScmObj** %stackaddr$prim56785, align 8
%stackaddr$prim56786 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48053)
store volatile %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$prim56786, align 8
%stackaddr$prim56787 = alloca %struct.ScmObj*, align 8
%anf_45bind48188 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48053)
store volatile %struct.ScmObj* %anf_45bind48188, %struct.ScmObj** %stackaddr$prim56787, align 8
%stackaddr$prim56788 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48188)
store volatile %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$prim56788, align 8
%stackaddr$prim56789 = alloca %struct.ScmObj*, align 8
%anf_45bind48189 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48053)
store volatile %struct.ScmObj* %anf_45bind48189, %struct.ScmObj** %stackaddr$prim56789, align 8
%stackaddr$prim56790 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48189)
store volatile %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$prim56790, align 8
%stackaddr$makeclosure56791 = alloca %struct.ScmObj*, align 8
%fptrToInt56792 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49203 to i64
%ae49203 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56792)
store volatile %struct.ScmObj* %ae49203, %struct.ScmObj** %stackaddr$makeclosure56791, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49203, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49203, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49203, %struct.ScmObj* %lsts48054, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49203, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49203, %struct.ScmObj* %k48432, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49203, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49203, %struct.ScmObj* %_37map148042, i64 6)
%ae49204 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56793 = alloca %struct.ScmObj*, align 8
%fptrToInt56794 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49205 to i64
%ae49205 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56794)
store volatile %struct.ScmObj* %ae49205, %struct.ScmObj** %stackaddr$makeclosure56793, align 8
%argslist55171$ae492030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56795 = alloca %struct.ScmObj*, align 8
%argslist55171$ae492031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49205, %struct.ScmObj* %argslist55171$ae492030)
store volatile %struct.ScmObj* %argslist55171$ae492031, %struct.ScmObj** %stackaddr$prim56795, align 8
%stackaddr$prim56796 = alloca %struct.ScmObj*, align 8
%argslist55171$ae492032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49204, %struct.ScmObj* %argslist55171$ae492031)
store volatile %struct.ScmObj* %argslist55171$ae492032, %struct.ScmObj** %stackaddr$prim56796, align 8
%clofunc56797 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49203)
musttail call tailcc void %clofunc56797(%struct.ScmObj* %ae49203, %struct.ScmObj* %argslist55171$ae492032)
ret void
}

define tailcc void @proc_clo$ae49203(%struct.ScmObj* %env$ae49203,%struct.ScmObj* %current_45args55117) {
%stackaddr$env-ref56798 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49203, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56798
%stackaddr$env-ref56799 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49203, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56799
%stackaddr$env-ref56800 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49203, i64 2)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref56800
%stackaddr$env-ref56801 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49203, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56801
%stackaddr$env-ref56802 = alloca %struct.ScmObj*, align 8
%k48432 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49203, i64 4)
store %struct.ScmObj* %k48432, %struct.ScmObj** %stackaddr$env-ref56802
%stackaddr$env-ref56803 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49203, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56803
%stackaddr$env-ref56804 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49203, i64 6)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref56804
%stackaddr$prim56805 = alloca %struct.ScmObj*, align 8
%_95k48433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55117)
store volatile %struct.ScmObj* %_95k48433, %struct.ScmObj** %stackaddr$prim56805, align 8
%stackaddr$prim56806 = alloca %struct.ScmObj*, align 8
%current_45args55118 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55117)
store volatile %struct.ScmObj* %current_45args55118, %struct.ScmObj** %stackaddr$prim56806, align 8
%stackaddr$prim56807 = alloca %struct.ScmObj*, align 8
%anf_45bind48190 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55118)
store volatile %struct.ScmObj* %anf_45bind48190, %struct.ScmObj** %stackaddr$prim56807, align 8
%stackaddr$makeclosure56808 = alloca %struct.ScmObj*, align 8
%fptrToInt56809 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49235 to i64
%ae49235 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56809)
store volatile %struct.ScmObj* %ae49235, %struct.ScmObj** %stackaddr$makeclosure56808, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49235, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49235, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49235, %struct.ScmObj* %lsts48054, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49235, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49235, %struct.ScmObj* %k48432, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49235, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49235, %struct.ScmObj* %_37map148042, i64 6)
%ae49237 = call %struct.ScmObj* @const_init_false()
%argslist55164$_37foldr1480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56810 = alloca %struct.ScmObj*, align 8
%argslist55164$_37foldr1480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48054, %struct.ScmObj* %argslist55164$_37foldr1480460)
store volatile %struct.ScmObj* %argslist55164$_37foldr1480461, %struct.ScmObj** %stackaddr$prim56810, align 8
%stackaddr$prim56811 = alloca %struct.ScmObj*, align 8
%argslist55164$_37foldr1480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49237, %struct.ScmObj* %argslist55164$_37foldr1480461)
store volatile %struct.ScmObj* %argslist55164$_37foldr1480462, %struct.ScmObj** %stackaddr$prim56811, align 8
%stackaddr$prim56812 = alloca %struct.ScmObj*, align 8
%argslist55164$_37foldr1480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48190, %struct.ScmObj* %argslist55164$_37foldr1480462)
store volatile %struct.ScmObj* %argslist55164$_37foldr1480463, %struct.ScmObj** %stackaddr$prim56812, align 8
%stackaddr$prim56813 = alloca %struct.ScmObj*, align 8
%argslist55164$_37foldr1480464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49235, %struct.ScmObj* %argslist55164$_37foldr1480463)
store volatile %struct.ScmObj* %argslist55164$_37foldr1480464, %struct.ScmObj** %stackaddr$prim56813, align 8
%clofunc56814 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148046)
musttail call tailcc void %clofunc56814(%struct.ScmObj* %_37foldr148046, %struct.ScmObj* %argslist55164$_37foldr1480464)
ret void
}

define tailcc void @proc_clo$ae49235(%struct.ScmObj* %env$ae49235,%struct.ScmObj* %current_45args55120) {
%stackaddr$env-ref56815 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49235, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56815
%stackaddr$env-ref56816 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49235, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56816
%stackaddr$env-ref56817 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49235, i64 2)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref56817
%stackaddr$env-ref56818 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49235, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56818
%stackaddr$env-ref56819 = alloca %struct.ScmObj*, align 8
%k48432 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49235, i64 4)
store %struct.ScmObj* %k48432, %struct.ScmObj** %stackaddr$env-ref56819
%stackaddr$env-ref56820 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49235, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56820
%stackaddr$env-ref56821 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49235, i64 6)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref56821
%stackaddr$prim56822 = alloca %struct.ScmObj*, align 8
%_95k48434 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55120)
store volatile %struct.ScmObj* %_95k48434, %struct.ScmObj** %stackaddr$prim56822, align 8
%stackaddr$prim56823 = alloca %struct.ScmObj*, align 8
%current_45args55121 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55120)
store volatile %struct.ScmObj* %current_45args55121, %struct.ScmObj** %stackaddr$prim56823, align 8
%stackaddr$prim56824 = alloca %struct.ScmObj*, align 8
%anf_45bind48191 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55121)
store volatile %struct.ScmObj* %anf_45bind48191, %struct.ScmObj** %stackaddr$prim56824, align 8
%truthy$cmp56825 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48191)
%cmp$cmp56825 = icmp eq i64 %truthy$cmp56825, 1
br i1 %cmp$cmp56825, label %truebranch$cmp56825, label %falsebranch$cmp56825
truebranch$cmp56825:
%ae49246 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55123$k484320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56826 = alloca %struct.ScmObj*, align 8
%argslist55123$k484321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48055, %struct.ScmObj* %argslist55123$k484320)
store volatile %struct.ScmObj* %argslist55123$k484321, %struct.ScmObj** %stackaddr$prim56826, align 8
%stackaddr$prim56827 = alloca %struct.ScmObj*, align 8
%argslist55123$k484322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49246, %struct.ScmObj* %argslist55123$k484321)
store volatile %struct.ScmObj* %argslist55123$k484322, %struct.ScmObj** %stackaddr$prim56827, align 8
%clofunc56828 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48432)
musttail call tailcc void %clofunc56828(%struct.ScmObj* %k48432, %struct.ScmObj* %argslist55123$k484322)
ret void
falsebranch$cmp56825:
%stackaddr$makeclosure56829 = alloca %struct.ScmObj*, align 8
%fptrToInt56830 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49251 to i64
%ae49251 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56830)
store volatile %struct.ScmObj* %ae49251, %struct.ScmObj** %stackaddr$makeclosure56829, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49251, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49251, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49251, %struct.ScmObj* %lsts48054, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49251, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49251, %struct.ScmObj* %k48432, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49251, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49251, %struct.ScmObj* %_37map148042, i64 6)
%ae49252 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56831 = alloca %struct.ScmObj*, align 8
%fptrToInt56832 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49253 to i64
%ae49253 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56832)
store volatile %struct.ScmObj* %ae49253, %struct.ScmObj** %stackaddr$makeclosure56831, align 8
%argslist55163$ae492510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56833 = alloca %struct.ScmObj*, align 8
%argslist55163$ae492511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49253, %struct.ScmObj* %argslist55163$ae492510)
store volatile %struct.ScmObj* %argslist55163$ae492511, %struct.ScmObj** %stackaddr$prim56833, align 8
%stackaddr$prim56834 = alloca %struct.ScmObj*, align 8
%argslist55163$ae492512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49252, %struct.ScmObj* %argslist55163$ae492511)
store volatile %struct.ScmObj* %argslist55163$ae492512, %struct.ScmObj** %stackaddr$prim56834, align 8
%clofunc56835 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49251)
musttail call tailcc void %clofunc56835(%struct.ScmObj* %ae49251, %struct.ScmObj* %argslist55163$ae492512)
ret void
}

define tailcc void @proc_clo$ae49251(%struct.ScmObj* %env$ae49251,%struct.ScmObj* %current_45args55124) {
%stackaddr$env-ref56836 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49251, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56836
%stackaddr$env-ref56837 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49251, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56837
%stackaddr$env-ref56838 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49251, i64 2)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref56838
%stackaddr$env-ref56839 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49251, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56839
%stackaddr$env-ref56840 = alloca %struct.ScmObj*, align 8
%k48432 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49251, i64 4)
store %struct.ScmObj* %k48432, %struct.ScmObj** %stackaddr$env-ref56840
%stackaddr$env-ref56841 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49251, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56841
%stackaddr$env-ref56842 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49251, i64 6)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref56842
%stackaddr$prim56843 = alloca %struct.ScmObj*, align 8
%_95k48435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55124)
store volatile %struct.ScmObj* %_95k48435, %struct.ScmObj** %stackaddr$prim56843, align 8
%stackaddr$prim56844 = alloca %struct.ScmObj*, align 8
%current_45args55125 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55124)
store volatile %struct.ScmObj* %current_45args55125, %struct.ScmObj** %stackaddr$prim56844, align 8
%stackaddr$prim56845 = alloca %struct.ScmObj*, align 8
%anf_45bind48192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55125)
store volatile %struct.ScmObj* %anf_45bind48192, %struct.ScmObj** %stackaddr$prim56845, align 8
%stackaddr$makeclosure56846 = alloca %struct.ScmObj*, align 8
%fptrToInt56847 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49272 to i64
%ae49272 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56847)
store volatile %struct.ScmObj* %ae49272, %struct.ScmObj** %stackaddr$makeclosure56846, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49272, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49272, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49272, %struct.ScmObj* %lsts48054, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49272, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49272, %struct.ScmObj* %k48432, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49272, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49272, %struct.ScmObj* %_37map148042, i64 6)
%argslist55158$_37map1480420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56848 = alloca %struct.ScmObj*, align 8
%argslist55158$_37map1480421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48054, %struct.ScmObj* %argslist55158$_37map1480420)
store volatile %struct.ScmObj* %argslist55158$_37map1480421, %struct.ScmObj** %stackaddr$prim56848, align 8
%stackaddr$prim56849 = alloca %struct.ScmObj*, align 8
%argslist55158$_37map1480422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48192, %struct.ScmObj* %argslist55158$_37map1480421)
store volatile %struct.ScmObj* %argslist55158$_37map1480422, %struct.ScmObj** %stackaddr$prim56849, align 8
%stackaddr$prim56850 = alloca %struct.ScmObj*, align 8
%argslist55158$_37map1480423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49272, %struct.ScmObj* %argslist55158$_37map1480422)
store volatile %struct.ScmObj* %argslist55158$_37map1480423, %struct.ScmObj** %stackaddr$prim56850, align 8
%clofunc56851 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148042)
musttail call tailcc void %clofunc56851(%struct.ScmObj* %_37map148042, %struct.ScmObj* %argslist55158$_37map1480423)
ret void
}

define tailcc void @proc_clo$ae49272(%struct.ScmObj* %env$ae49272,%struct.ScmObj* %current_45args55127) {
%stackaddr$env-ref56852 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49272, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56852
%stackaddr$env-ref56853 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49272, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56853
%stackaddr$env-ref56854 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49272, i64 2)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref56854
%stackaddr$env-ref56855 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49272, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56855
%stackaddr$env-ref56856 = alloca %struct.ScmObj*, align 8
%k48432 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49272, i64 4)
store %struct.ScmObj* %k48432, %struct.ScmObj** %stackaddr$env-ref56856
%stackaddr$env-ref56857 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49272, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56857
%stackaddr$env-ref56858 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49272, i64 6)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref56858
%stackaddr$prim56859 = alloca %struct.ScmObj*, align 8
%_95k48436 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55127)
store volatile %struct.ScmObj* %_95k48436, %struct.ScmObj** %stackaddr$prim56859, align 8
%stackaddr$prim56860 = alloca %struct.ScmObj*, align 8
%current_45args55128 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55127)
store volatile %struct.ScmObj* %current_45args55128, %struct.ScmObj** %stackaddr$prim56860, align 8
%stackaddr$prim56861 = alloca %struct.ScmObj*, align 8
%lsts_4348061 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55128)
store volatile %struct.ScmObj* %lsts_4348061, %struct.ScmObj** %stackaddr$prim56861, align 8
%stackaddr$makeclosure56862 = alloca %struct.ScmObj*, align 8
%fptrToInt56863 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49275 to i64
%ae49275 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt56863)
store volatile %struct.ScmObj* %ae49275, %struct.ScmObj** %stackaddr$makeclosure56862, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49275, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49275, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49275, %struct.ScmObj* %lsts48054, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49275, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49275, %struct.ScmObj* %k48432, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49275, %struct.ScmObj* %_37foldr148046, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49275, %struct.ScmObj* %lsts_4348061, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49275, %struct.ScmObj* %_37map148042, i64 7)
%ae49276 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56864 = alloca %struct.ScmObj*, align 8
%fptrToInt56865 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49277 to i64
%ae49277 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56865)
store volatile %struct.ScmObj* %ae49277, %struct.ScmObj** %stackaddr$makeclosure56864, align 8
%argslist55157$ae492750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56866 = alloca %struct.ScmObj*, align 8
%argslist55157$ae492751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49277, %struct.ScmObj* %argslist55157$ae492750)
store volatile %struct.ScmObj* %argslist55157$ae492751, %struct.ScmObj** %stackaddr$prim56866, align 8
%stackaddr$prim56867 = alloca %struct.ScmObj*, align 8
%argslist55157$ae492752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49276, %struct.ScmObj* %argslist55157$ae492751)
store volatile %struct.ScmObj* %argslist55157$ae492752, %struct.ScmObj** %stackaddr$prim56867, align 8
%clofunc56868 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49275)
musttail call tailcc void %clofunc56868(%struct.ScmObj* %ae49275, %struct.ScmObj* %argslist55157$ae492752)
ret void
}

define tailcc void @proc_clo$ae49275(%struct.ScmObj* %env$ae49275,%struct.ScmObj* %current_45args55130) {
%stackaddr$env-ref56869 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49275, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56869
%stackaddr$env-ref56870 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49275, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56870
%stackaddr$env-ref56871 = alloca %struct.ScmObj*, align 8
%lsts48054 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49275, i64 2)
store %struct.ScmObj* %lsts48054, %struct.ScmObj** %stackaddr$env-ref56871
%stackaddr$env-ref56872 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49275, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56872
%stackaddr$env-ref56873 = alloca %struct.ScmObj*, align 8
%k48432 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49275, i64 4)
store %struct.ScmObj* %k48432, %struct.ScmObj** %stackaddr$env-ref56873
%stackaddr$env-ref56874 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49275, i64 5)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56874
%stackaddr$env-ref56875 = alloca %struct.ScmObj*, align 8
%lsts_4348061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49275, i64 6)
store %struct.ScmObj* %lsts_4348061, %struct.ScmObj** %stackaddr$env-ref56875
%stackaddr$env-ref56876 = alloca %struct.ScmObj*, align 8
%_37map148042 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49275, i64 7)
store %struct.ScmObj* %_37map148042, %struct.ScmObj** %stackaddr$env-ref56876
%stackaddr$prim56877 = alloca %struct.ScmObj*, align 8
%_95k48437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55130)
store volatile %struct.ScmObj* %_95k48437, %struct.ScmObj** %stackaddr$prim56877, align 8
%stackaddr$prim56878 = alloca %struct.ScmObj*, align 8
%current_45args55131 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55130)
store volatile %struct.ScmObj* %current_45args55131, %struct.ScmObj** %stackaddr$prim56878, align 8
%stackaddr$prim56879 = alloca %struct.ScmObj*, align 8
%anf_45bind48193 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55131)
store volatile %struct.ScmObj* %anf_45bind48193, %struct.ScmObj** %stackaddr$prim56879, align 8
%stackaddr$makeclosure56880 = alloca %struct.ScmObj*, align 8
%fptrToInt56881 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49296 to i64
%ae49296 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt56881)
store volatile %struct.ScmObj* %ae49296, %struct.ScmObj** %stackaddr$makeclosure56880, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49296, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49296, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49296, %struct.ScmObj* %_37foldr48052, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49296, %struct.ScmObj* %k48432, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49296, %struct.ScmObj* %_37foldr148046, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49296, %struct.ScmObj* %lsts_4348061, i64 5)
%argslist55152$_37map1480420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56882 = alloca %struct.ScmObj*, align 8
%argslist55152$_37map1480421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48054, %struct.ScmObj* %argslist55152$_37map1480420)
store volatile %struct.ScmObj* %argslist55152$_37map1480421, %struct.ScmObj** %stackaddr$prim56882, align 8
%stackaddr$prim56883 = alloca %struct.ScmObj*, align 8
%argslist55152$_37map1480422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48193, %struct.ScmObj* %argslist55152$_37map1480421)
store volatile %struct.ScmObj* %argslist55152$_37map1480422, %struct.ScmObj** %stackaddr$prim56883, align 8
%stackaddr$prim56884 = alloca %struct.ScmObj*, align 8
%argslist55152$_37map1480423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49296, %struct.ScmObj* %argslist55152$_37map1480422)
store volatile %struct.ScmObj* %argslist55152$_37map1480423, %struct.ScmObj** %stackaddr$prim56884, align 8
%clofunc56885 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148042)
musttail call tailcc void %clofunc56885(%struct.ScmObj* %_37map148042, %struct.ScmObj* %argslist55152$_37map1480423)
ret void
}

define tailcc void @proc_clo$ae49296(%struct.ScmObj* %env$ae49296,%struct.ScmObj* %current_45args55133) {
%stackaddr$env-ref56886 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49296, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56886
%stackaddr$env-ref56887 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49296, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56887
%stackaddr$env-ref56888 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49296, i64 2)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56888
%stackaddr$env-ref56889 = alloca %struct.ScmObj*, align 8
%k48432 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49296, i64 3)
store %struct.ScmObj* %k48432, %struct.ScmObj** %stackaddr$env-ref56889
%stackaddr$env-ref56890 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49296, i64 4)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56890
%stackaddr$env-ref56891 = alloca %struct.ScmObj*, align 8
%lsts_4348061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49296, i64 5)
store %struct.ScmObj* %lsts_4348061, %struct.ScmObj** %stackaddr$env-ref56891
%stackaddr$prim56892 = alloca %struct.ScmObj*, align 8
%_95k48438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55133)
store volatile %struct.ScmObj* %_95k48438, %struct.ScmObj** %stackaddr$prim56892, align 8
%stackaddr$prim56893 = alloca %struct.ScmObj*, align 8
%current_45args55134 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55133)
store volatile %struct.ScmObj* %current_45args55134, %struct.ScmObj** %stackaddr$prim56893, align 8
%stackaddr$prim56894 = alloca %struct.ScmObj*, align 8
%vs48059 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55134)
store volatile %struct.ScmObj* %vs48059, %struct.ScmObj** %stackaddr$prim56894, align 8
%stackaddr$makeclosure56895 = alloca %struct.ScmObj*, align 8
%fptrToInt56896 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49299 to i64
%ae49299 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt56896)
store volatile %struct.ScmObj* %ae49299, %struct.ScmObj** %stackaddr$makeclosure56895, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49299, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49299, %struct.ScmObj* %acc48055, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49299, %struct.ScmObj* %_37foldr48052, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49299, %struct.ScmObj* %k48432, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49299, %struct.ScmObj* %_37foldr148046, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49299, %struct.ScmObj* %lsts_4348061, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49299, %struct.ScmObj* %vs48059, i64 6)
%ae49300 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56897 = alloca %struct.ScmObj*, align 8
%fptrToInt56898 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49301 to i64
%ae49301 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56898)
store volatile %struct.ScmObj* %ae49301, %struct.ScmObj** %stackaddr$makeclosure56897, align 8
%argslist55151$ae492990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56899 = alloca %struct.ScmObj*, align 8
%argslist55151$ae492991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49301, %struct.ScmObj* %argslist55151$ae492990)
store volatile %struct.ScmObj* %argslist55151$ae492991, %struct.ScmObj** %stackaddr$prim56899, align 8
%stackaddr$prim56900 = alloca %struct.ScmObj*, align 8
%argslist55151$ae492992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49300, %struct.ScmObj* %argslist55151$ae492991)
store volatile %struct.ScmObj* %argslist55151$ae492992, %struct.ScmObj** %stackaddr$prim56900, align 8
%clofunc56901 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49299)
musttail call tailcc void %clofunc56901(%struct.ScmObj* %ae49299, %struct.ScmObj* %argslist55151$ae492992)
ret void
}

define tailcc void @proc_clo$ae49299(%struct.ScmObj* %env$ae49299,%struct.ScmObj* %current_45args55136) {
%stackaddr$env-ref56902 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49299, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56902
%stackaddr$env-ref56903 = alloca %struct.ScmObj*, align 8
%acc48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49299, i64 1)
store %struct.ScmObj* %acc48055, %struct.ScmObj** %stackaddr$env-ref56903
%stackaddr$env-ref56904 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49299, i64 2)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref56904
%stackaddr$env-ref56905 = alloca %struct.ScmObj*, align 8
%k48432 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49299, i64 3)
store %struct.ScmObj* %k48432, %struct.ScmObj** %stackaddr$env-ref56905
%stackaddr$env-ref56906 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49299, i64 4)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56906
%stackaddr$env-ref56907 = alloca %struct.ScmObj*, align 8
%lsts_4348061 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49299, i64 5)
store %struct.ScmObj* %lsts_4348061, %struct.ScmObj** %stackaddr$env-ref56907
%stackaddr$env-ref56908 = alloca %struct.ScmObj*, align 8
%vs48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49299, i64 6)
store %struct.ScmObj* %vs48059, %struct.ScmObj** %stackaddr$env-ref56908
%stackaddr$prim56909 = alloca %struct.ScmObj*, align 8
%_95k48439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55136)
store volatile %struct.ScmObj* %_95k48439, %struct.ScmObj** %stackaddr$prim56909, align 8
%stackaddr$prim56910 = alloca %struct.ScmObj*, align 8
%current_45args55137 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55136)
store volatile %struct.ScmObj* %current_45args55137, %struct.ScmObj** %stackaddr$prim56910, align 8
%stackaddr$prim56911 = alloca %struct.ScmObj*, align 8
%anf_45bind48194 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55137)
store volatile %struct.ScmObj* %anf_45bind48194, %struct.ScmObj** %stackaddr$prim56911, align 8
%stackaddr$prim56912 = alloca %struct.ScmObj*, align 8
%anf_45bind48195 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48055, %struct.ScmObj* %lsts_4348061)
store volatile %struct.ScmObj* %anf_45bind48195, %struct.ScmObj** %stackaddr$prim56912, align 8
%stackaddr$prim56913 = alloca %struct.ScmObj*, align 8
%anf_45bind48196 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48056, %struct.ScmObj* %anf_45bind48195)
store volatile %struct.ScmObj* %anf_45bind48196, %struct.ScmObj** %stackaddr$prim56913, align 8
%stackaddr$makeclosure56914 = alloca %struct.ScmObj*, align 8
%fptrToInt56915 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49325 to i64
%ae49325 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt56915)
store volatile %struct.ScmObj* %ae49325, %struct.ScmObj** %stackaddr$makeclosure56914, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49325, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49325, %struct.ScmObj* %anf_45bind48194, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49325, %struct.ScmObj* %k48432, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49325, %struct.ScmObj* %_37foldr148046, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49325, %struct.ScmObj* %vs48059, i64 4)
%stackaddr$prim56916 = alloca %struct.ScmObj*, align 8
%cpsargs48443 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49325, %struct.ScmObj* %anf_45bind48196)
store volatile %struct.ScmObj* %cpsargs48443, %struct.ScmObj** %stackaddr$prim56916, align 8
%clofunc56917 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48052)
musttail call tailcc void %clofunc56917(%struct.ScmObj* %_37foldr48052, %struct.ScmObj* %cpsargs48443)
ret void
}

define tailcc void @proc_clo$ae49325(%struct.ScmObj* %env$ae49325,%struct.ScmObj* %current_45args55139) {
%stackaddr$env-ref56918 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49325, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56918
%stackaddr$env-ref56919 = alloca %struct.ScmObj*, align 8
%anf_45bind48194 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49325, i64 1)
store %struct.ScmObj* %anf_45bind48194, %struct.ScmObj** %stackaddr$env-ref56919
%stackaddr$env-ref56920 = alloca %struct.ScmObj*, align 8
%k48432 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49325, i64 2)
store %struct.ScmObj* %k48432, %struct.ScmObj** %stackaddr$env-ref56920
%stackaddr$env-ref56921 = alloca %struct.ScmObj*, align 8
%_37foldr148046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49325, i64 3)
store %struct.ScmObj* %_37foldr148046, %struct.ScmObj** %stackaddr$env-ref56921
%stackaddr$env-ref56922 = alloca %struct.ScmObj*, align 8
%vs48059 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49325, i64 4)
store %struct.ScmObj* %vs48059, %struct.ScmObj** %stackaddr$env-ref56922
%stackaddr$prim56923 = alloca %struct.ScmObj*, align 8
%_95k48440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55139)
store volatile %struct.ScmObj* %_95k48440, %struct.ScmObj** %stackaddr$prim56923, align 8
%stackaddr$prim56924 = alloca %struct.ScmObj*, align 8
%current_45args55140 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55139)
store volatile %struct.ScmObj* %current_45args55140, %struct.ScmObj** %stackaddr$prim56924, align 8
%stackaddr$prim56925 = alloca %struct.ScmObj*, align 8
%anf_45bind48197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55140)
store volatile %struct.ScmObj* %anf_45bind48197, %struct.ScmObj** %stackaddr$prim56925, align 8
%ae49330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56926 = alloca %struct.ScmObj*, align 8
%anf_45bind48198 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48197, %struct.ScmObj* %ae49330)
store volatile %struct.ScmObj* %anf_45bind48198, %struct.ScmObj** %stackaddr$prim56926, align 8
%stackaddr$makeclosure56927 = alloca %struct.ScmObj*, align 8
%fptrToInt56928 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49332 to i64
%ae49332 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56928)
store volatile %struct.ScmObj* %ae49332, %struct.ScmObj** %stackaddr$makeclosure56927, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49332, %struct.ScmObj* %f48056, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49332, %struct.ScmObj* %k48432, i64 1)
%argslist55145$_37foldr1480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56929 = alloca %struct.ScmObj*, align 8
%argslist55145$_37foldr1480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48059, %struct.ScmObj* %argslist55145$_37foldr1480460)
store volatile %struct.ScmObj* %argslist55145$_37foldr1480461, %struct.ScmObj** %stackaddr$prim56929, align 8
%stackaddr$prim56930 = alloca %struct.ScmObj*, align 8
%argslist55145$_37foldr1480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48198, %struct.ScmObj* %argslist55145$_37foldr1480461)
store volatile %struct.ScmObj* %argslist55145$_37foldr1480462, %struct.ScmObj** %stackaddr$prim56930, align 8
%stackaddr$prim56931 = alloca %struct.ScmObj*, align 8
%argslist55145$_37foldr1480463 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48194, %struct.ScmObj* %argslist55145$_37foldr1480462)
store volatile %struct.ScmObj* %argslist55145$_37foldr1480463, %struct.ScmObj** %stackaddr$prim56931, align 8
%stackaddr$prim56932 = alloca %struct.ScmObj*, align 8
%argslist55145$_37foldr1480464 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49332, %struct.ScmObj* %argslist55145$_37foldr1480463)
store volatile %struct.ScmObj* %argslist55145$_37foldr1480464, %struct.ScmObj** %stackaddr$prim56932, align 8
%clofunc56933 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148046)
musttail call tailcc void %clofunc56933(%struct.ScmObj* %_37foldr148046, %struct.ScmObj* %argslist55145$_37foldr1480464)
ret void
}

define tailcc void @proc_clo$ae49332(%struct.ScmObj* %env$ae49332,%struct.ScmObj* %current_45args55142) {
%stackaddr$env-ref56934 = alloca %struct.ScmObj*, align 8
%f48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49332, i64 0)
store %struct.ScmObj* %f48056, %struct.ScmObj** %stackaddr$env-ref56934
%stackaddr$env-ref56935 = alloca %struct.ScmObj*, align 8
%k48432 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49332, i64 1)
store %struct.ScmObj* %k48432, %struct.ScmObj** %stackaddr$env-ref56935
%stackaddr$prim56936 = alloca %struct.ScmObj*, align 8
%_95k48441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55142)
store volatile %struct.ScmObj* %_95k48441, %struct.ScmObj** %stackaddr$prim56936, align 8
%stackaddr$prim56937 = alloca %struct.ScmObj*, align 8
%current_45args55143 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55142)
store volatile %struct.ScmObj* %current_45args55143, %struct.ScmObj** %stackaddr$prim56937, align 8
%stackaddr$prim56938 = alloca %struct.ScmObj*, align 8
%anf_45bind48199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55143)
store volatile %struct.ScmObj* %anf_45bind48199, %struct.ScmObj** %stackaddr$prim56938, align 8
%stackaddr$prim56939 = alloca %struct.ScmObj*, align 8
%cpsargs48442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48432, %struct.ScmObj* %anf_45bind48199)
store volatile %struct.ScmObj* %cpsargs48442, %struct.ScmObj** %stackaddr$prim56939, align 8
%clofunc56940 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48056)
musttail call tailcc void %clofunc56940(%struct.ScmObj* %f48056, %struct.ScmObj* %cpsargs48442)
ret void
}

define tailcc void @proc_clo$ae49301(%struct.ScmObj* %env$ae49301,%struct.ScmObj* %current_45args55146) {
%stackaddr$prim56941 = alloca %struct.ScmObj*, align 8
%k48444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55146)
store volatile %struct.ScmObj* %k48444, %struct.ScmObj** %stackaddr$prim56941, align 8
%stackaddr$prim56942 = alloca %struct.ScmObj*, align 8
%current_45args55147 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55146)
store volatile %struct.ScmObj* %current_45args55147, %struct.ScmObj** %stackaddr$prim56942, align 8
%stackaddr$prim56943 = alloca %struct.ScmObj*, align 8
%a48064 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55147)
store volatile %struct.ScmObj* %a48064, %struct.ScmObj** %stackaddr$prim56943, align 8
%stackaddr$prim56944 = alloca %struct.ScmObj*, align 8
%current_45args55148 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55147)
store volatile %struct.ScmObj* %current_45args55148, %struct.ScmObj** %stackaddr$prim56944, align 8
%stackaddr$prim56945 = alloca %struct.ScmObj*, align 8
%b48063 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55148)
store volatile %struct.ScmObj* %b48063, %struct.ScmObj** %stackaddr$prim56945, align 8
%stackaddr$prim56946 = alloca %struct.ScmObj*, align 8
%cpsprim48445 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48064, %struct.ScmObj* %b48063)
store volatile %struct.ScmObj* %cpsprim48445, %struct.ScmObj** %stackaddr$prim56946, align 8
%ae49305 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55150$k484440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56947 = alloca %struct.ScmObj*, align 8
%argslist55150$k484441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48445, %struct.ScmObj* %argslist55150$k484440)
store volatile %struct.ScmObj* %argslist55150$k484441, %struct.ScmObj** %stackaddr$prim56947, align 8
%stackaddr$prim56948 = alloca %struct.ScmObj*, align 8
%argslist55150$k484442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49305, %struct.ScmObj* %argslist55150$k484441)
store volatile %struct.ScmObj* %argslist55150$k484442, %struct.ScmObj** %stackaddr$prim56948, align 8
%clofunc56949 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48444)
musttail call tailcc void %clofunc56949(%struct.ScmObj* %k48444, %struct.ScmObj* %argslist55150$k484442)
ret void
}

define tailcc void @proc_clo$ae49277(%struct.ScmObj* %env$ae49277,%struct.ScmObj* %current_45args55153) {
%stackaddr$prim56950 = alloca %struct.ScmObj*, align 8
%k48446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55153)
store volatile %struct.ScmObj* %k48446, %struct.ScmObj** %stackaddr$prim56950, align 8
%stackaddr$prim56951 = alloca %struct.ScmObj*, align 8
%current_45args55154 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55153)
store volatile %struct.ScmObj* %current_45args55154, %struct.ScmObj** %stackaddr$prim56951, align 8
%stackaddr$prim56952 = alloca %struct.ScmObj*, align 8
%x48060 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55154)
store volatile %struct.ScmObj* %x48060, %struct.ScmObj** %stackaddr$prim56952, align 8
%stackaddr$prim56953 = alloca %struct.ScmObj*, align 8
%cpsprim48447 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48060)
store volatile %struct.ScmObj* %cpsprim48447, %struct.ScmObj** %stackaddr$prim56953, align 8
%ae49280 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55156$k484460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56954 = alloca %struct.ScmObj*, align 8
%argslist55156$k484461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48447, %struct.ScmObj* %argslist55156$k484460)
store volatile %struct.ScmObj* %argslist55156$k484461, %struct.ScmObj** %stackaddr$prim56954, align 8
%stackaddr$prim56955 = alloca %struct.ScmObj*, align 8
%argslist55156$k484462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49280, %struct.ScmObj* %argslist55156$k484461)
store volatile %struct.ScmObj* %argslist55156$k484462, %struct.ScmObj** %stackaddr$prim56955, align 8
%clofunc56956 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48446)
musttail call tailcc void %clofunc56956(%struct.ScmObj* %k48446, %struct.ScmObj* %argslist55156$k484462)
ret void
}

define tailcc void @proc_clo$ae49253(%struct.ScmObj* %env$ae49253,%struct.ScmObj* %current_45args55159) {
%stackaddr$prim56957 = alloca %struct.ScmObj*, align 8
%k48448 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55159)
store volatile %struct.ScmObj* %k48448, %struct.ScmObj** %stackaddr$prim56957, align 8
%stackaddr$prim56958 = alloca %struct.ScmObj*, align 8
%current_45args55160 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55159)
store volatile %struct.ScmObj* %current_45args55160, %struct.ScmObj** %stackaddr$prim56958, align 8
%stackaddr$prim56959 = alloca %struct.ScmObj*, align 8
%x48062 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55160)
store volatile %struct.ScmObj* %x48062, %struct.ScmObj** %stackaddr$prim56959, align 8
%stackaddr$prim56960 = alloca %struct.ScmObj*, align 8
%cpsprim48449 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48062)
store volatile %struct.ScmObj* %cpsprim48449, %struct.ScmObj** %stackaddr$prim56960, align 8
%ae49256 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55162$k484480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56961 = alloca %struct.ScmObj*, align 8
%argslist55162$k484481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48449, %struct.ScmObj* %argslist55162$k484480)
store volatile %struct.ScmObj* %argslist55162$k484481, %struct.ScmObj** %stackaddr$prim56961, align 8
%stackaddr$prim56962 = alloca %struct.ScmObj*, align 8
%argslist55162$k484482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49256, %struct.ScmObj* %argslist55162$k484481)
store volatile %struct.ScmObj* %argslist55162$k484482, %struct.ScmObj** %stackaddr$prim56962, align 8
%clofunc56963 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48448)
musttail call tailcc void %clofunc56963(%struct.ScmObj* %k48448, %struct.ScmObj* %argslist55162$k484482)
ret void
}

define tailcc void @proc_clo$ae49205(%struct.ScmObj* %env$ae49205,%struct.ScmObj* %current_45args55165) {
%stackaddr$prim56964 = alloca %struct.ScmObj*, align 8
%k48450 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55165)
store volatile %struct.ScmObj* %k48450, %struct.ScmObj** %stackaddr$prim56964, align 8
%stackaddr$prim56965 = alloca %struct.ScmObj*, align 8
%current_45args55166 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55165)
store volatile %struct.ScmObj* %current_45args55166, %struct.ScmObj** %stackaddr$prim56965, align 8
%stackaddr$prim56966 = alloca %struct.ScmObj*, align 8
%lst48058 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55166)
store volatile %struct.ScmObj* %lst48058, %struct.ScmObj** %stackaddr$prim56966, align 8
%stackaddr$prim56967 = alloca %struct.ScmObj*, align 8
%current_45args55167 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55166)
store volatile %struct.ScmObj* %current_45args55167, %struct.ScmObj** %stackaddr$prim56967, align 8
%stackaddr$prim56968 = alloca %struct.ScmObj*, align 8
%b48057 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55167)
store volatile %struct.ScmObj* %b48057, %struct.ScmObj** %stackaddr$prim56968, align 8
%truthy$cmp56969 = call i64 @is_truthy_value(%struct.ScmObj* %b48057)
%cmp$cmp56969 = icmp eq i64 %truthy$cmp56969, 1
br i1 %cmp$cmp56969, label %truebranch$cmp56969, label %falsebranch$cmp56969
truebranch$cmp56969:
%ae49208 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55169$k484500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56970 = alloca %struct.ScmObj*, align 8
%argslist55169$k484501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48057, %struct.ScmObj* %argslist55169$k484500)
store volatile %struct.ScmObj* %argslist55169$k484501, %struct.ScmObj** %stackaddr$prim56970, align 8
%stackaddr$prim56971 = alloca %struct.ScmObj*, align 8
%argslist55169$k484502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49208, %struct.ScmObj* %argslist55169$k484501)
store volatile %struct.ScmObj* %argslist55169$k484502, %struct.ScmObj** %stackaddr$prim56971, align 8
%clofunc56972 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48450)
musttail call tailcc void %clofunc56972(%struct.ScmObj* %k48450, %struct.ScmObj* %argslist55169$k484502)
ret void
falsebranch$cmp56969:
%stackaddr$prim56973 = alloca %struct.ScmObj*, align 8
%cpsprim48451 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48058)
store volatile %struct.ScmObj* %cpsprim48451, %struct.ScmObj** %stackaddr$prim56973, align 8
%ae49215 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55170$k484500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56974 = alloca %struct.ScmObj*, align 8
%argslist55170$k484501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48451, %struct.ScmObj* %argslist55170$k484500)
store volatile %struct.ScmObj* %argslist55170$k484501, %struct.ScmObj** %stackaddr$prim56974, align 8
%stackaddr$prim56975 = alloca %struct.ScmObj*, align 8
%argslist55170$k484502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49215, %struct.ScmObj* %argslist55170$k484501)
store volatile %struct.ScmObj* %argslist55170$k484502, %struct.ScmObj** %stackaddr$prim56975, align 8
%clofunc56976 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48450)
musttail call tailcc void %clofunc56976(%struct.ScmObj* %k48450, %struct.ScmObj* %argslist55170$k484502)
ret void
}

define tailcc void @proc_clo$ae49162(%struct.ScmObj* %env$ae49162,%struct.ScmObj* %current_45args55174) {
%stackaddr$env-ref56977 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49162, i64 0)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref56977
%stackaddr$env-ref56978 = alloca %struct.ScmObj*, align 8
%_37length48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49162, i64 1)
store %struct.ScmObj* %_37length48035, %struct.ScmObj** %stackaddr$env-ref56978
%stackaddr$prim56979 = alloca %struct.ScmObj*, align 8
%k48452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55174)
store volatile %struct.ScmObj* %k48452, %struct.ScmObj** %stackaddr$prim56979, align 8
%stackaddr$prim56980 = alloca %struct.ScmObj*, align 8
%current_45args55175 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55174)
store volatile %struct.ScmObj* %current_45args55175, %struct.ScmObj** %stackaddr$prim56980, align 8
%stackaddr$prim56981 = alloca %struct.ScmObj*, align 8
%lst48067 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55175)
store volatile %struct.ScmObj* %lst48067, %struct.ScmObj** %stackaddr$prim56981, align 8
%stackaddr$prim56982 = alloca %struct.ScmObj*, align 8
%current_45args55176 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55175)
store volatile %struct.ScmObj* %current_45args55176, %struct.ScmObj** %stackaddr$prim56982, align 8
%stackaddr$prim56983 = alloca %struct.ScmObj*, align 8
%n48066 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55176)
store volatile %struct.ScmObj* %n48066, %struct.ScmObj** %stackaddr$prim56983, align 8
%stackaddr$makeclosure56984 = alloca %struct.ScmObj*, align 8
%fptrToInt56985 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49164 to i64
%ae49164 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56985)
store volatile %struct.ScmObj* %ae49164, %struct.ScmObj** %stackaddr$makeclosure56984, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49164, %struct.ScmObj* %_37take48038, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49164, %struct.ScmObj* %k48452, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49164, %struct.ScmObj* %lst48067, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49164, %struct.ScmObj* %n48066, i64 3)
%argslist55182$_37length480350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56986 = alloca %struct.ScmObj*, align 8
%argslist55182$_37length480351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48067, %struct.ScmObj* %argslist55182$_37length480350)
store volatile %struct.ScmObj* %argslist55182$_37length480351, %struct.ScmObj** %stackaddr$prim56986, align 8
%stackaddr$prim56987 = alloca %struct.ScmObj*, align 8
%argslist55182$_37length480352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49164, %struct.ScmObj* %argslist55182$_37length480351)
store volatile %struct.ScmObj* %argslist55182$_37length480352, %struct.ScmObj** %stackaddr$prim56987, align 8
%clofunc56988 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48035)
musttail call tailcc void %clofunc56988(%struct.ScmObj* %_37length48035, %struct.ScmObj* %argslist55182$_37length480352)
ret void
}

define tailcc void @proc_clo$ae49164(%struct.ScmObj* %env$ae49164,%struct.ScmObj* %current_45args55178) {
%stackaddr$env-ref56989 = alloca %struct.ScmObj*, align 8
%_37take48038 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49164, i64 0)
store %struct.ScmObj* %_37take48038, %struct.ScmObj** %stackaddr$env-ref56989
%stackaddr$env-ref56990 = alloca %struct.ScmObj*, align 8
%k48452 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49164, i64 1)
store %struct.ScmObj* %k48452, %struct.ScmObj** %stackaddr$env-ref56990
%stackaddr$env-ref56991 = alloca %struct.ScmObj*, align 8
%lst48067 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49164, i64 2)
store %struct.ScmObj* %lst48067, %struct.ScmObj** %stackaddr$env-ref56991
%stackaddr$env-ref56992 = alloca %struct.ScmObj*, align 8
%n48066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49164, i64 3)
store %struct.ScmObj* %n48066, %struct.ScmObj** %stackaddr$env-ref56992
%stackaddr$prim56993 = alloca %struct.ScmObj*, align 8
%_95k48453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55178)
store volatile %struct.ScmObj* %_95k48453, %struct.ScmObj** %stackaddr$prim56993, align 8
%stackaddr$prim56994 = alloca %struct.ScmObj*, align 8
%current_45args55179 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55178)
store volatile %struct.ScmObj* %current_45args55179, %struct.ScmObj** %stackaddr$prim56994, align 8
%stackaddr$prim56995 = alloca %struct.ScmObj*, align 8
%anf_45bind48186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55179)
store volatile %struct.ScmObj* %anf_45bind48186, %struct.ScmObj** %stackaddr$prim56995, align 8
%stackaddr$prim56996 = alloca %struct.ScmObj*, align 8
%anf_45bind48187 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48186, %struct.ScmObj* %n48066)
store volatile %struct.ScmObj* %anf_45bind48187, %struct.ScmObj** %stackaddr$prim56996, align 8
%argslist55181$_37take480380 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56997 = alloca %struct.ScmObj*, align 8
%argslist55181$_37take480381 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48187, %struct.ScmObj* %argslist55181$_37take480380)
store volatile %struct.ScmObj* %argslist55181$_37take480381, %struct.ScmObj** %stackaddr$prim56997, align 8
%stackaddr$prim56998 = alloca %struct.ScmObj*, align 8
%argslist55181$_37take480382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48067, %struct.ScmObj* %argslist55181$_37take480381)
store volatile %struct.ScmObj* %argslist55181$_37take480382, %struct.ScmObj** %stackaddr$prim56998, align 8
%stackaddr$prim56999 = alloca %struct.ScmObj*, align 8
%argslist55181$_37take480383 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48452, %struct.ScmObj* %argslist55181$_37take480382)
store volatile %struct.ScmObj* %argslist55181$_37take480383, %struct.ScmObj** %stackaddr$prim56999, align 8
%clofunc57000 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48038)
musttail call tailcc void %clofunc57000(%struct.ScmObj* %_37take48038, %struct.ScmObj* %argslist55181$_37take480383)
ret void
}

define tailcc void @proc_clo$ae49108(%struct.ScmObj* %env$ae49108,%struct.ScmObj* %current_45args55184) {
%stackaddr$env-ref57001 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49108, i64 0)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref57001
%stackaddr$prim57002 = alloca %struct.ScmObj*, align 8
%k48454 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55184)
store volatile %struct.ScmObj* %k48454, %struct.ScmObj** %stackaddr$prim57002, align 8
%stackaddr$prim57003 = alloca %struct.ScmObj*, align 8
%current_45args55185 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55184)
store volatile %struct.ScmObj* %current_45args55185, %struct.ScmObj** %stackaddr$prim57003, align 8
%stackaddr$prim57004 = alloca %struct.ScmObj*, align 8
%lst48069 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55185)
store volatile %struct.ScmObj* %lst48069, %struct.ScmObj** %stackaddr$prim57004, align 8
%stackaddr$makeclosure57005 = alloca %struct.ScmObj*, align 8
%fptrToInt57006 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49109 to i64
%ae49109 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57006)
store volatile %struct.ScmObj* %ae49109, %struct.ScmObj** %stackaddr$makeclosure57005, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49109, %struct.ScmObj* %k48454, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49109, %struct.ScmObj* %lst48069, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49109, %struct.ScmObj* %_37foldl148030, i64 2)
%ae49110 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57007 = alloca %struct.ScmObj*, align 8
%fptrToInt57008 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49111 to i64
%ae49111 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt57008)
store volatile %struct.ScmObj* %ae49111, %struct.ScmObj** %stackaddr$makeclosure57007, align 8
%argslist55196$ae491090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57009 = alloca %struct.ScmObj*, align 8
%argslist55196$ae491091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49111, %struct.ScmObj* %argslist55196$ae491090)
store volatile %struct.ScmObj* %argslist55196$ae491091, %struct.ScmObj** %stackaddr$prim57009, align 8
%stackaddr$prim57010 = alloca %struct.ScmObj*, align 8
%argslist55196$ae491092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49110, %struct.ScmObj* %argslist55196$ae491091)
store volatile %struct.ScmObj* %argslist55196$ae491092, %struct.ScmObj** %stackaddr$prim57010, align 8
%clofunc57011 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49109)
musttail call tailcc void %clofunc57011(%struct.ScmObj* %ae49109, %struct.ScmObj* %argslist55196$ae491092)
ret void
}

define tailcc void @proc_clo$ae49109(%struct.ScmObj* %env$ae49109,%struct.ScmObj* %current_45args55187) {
%stackaddr$env-ref57012 = alloca %struct.ScmObj*, align 8
%k48454 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49109, i64 0)
store %struct.ScmObj* %k48454, %struct.ScmObj** %stackaddr$env-ref57012
%stackaddr$env-ref57013 = alloca %struct.ScmObj*, align 8
%lst48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49109, i64 1)
store %struct.ScmObj* %lst48069, %struct.ScmObj** %stackaddr$env-ref57013
%stackaddr$env-ref57014 = alloca %struct.ScmObj*, align 8
%_37foldl148030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49109, i64 2)
store %struct.ScmObj* %_37foldl148030, %struct.ScmObj** %stackaddr$env-ref57014
%stackaddr$prim57015 = alloca %struct.ScmObj*, align 8
%_95k48455 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55187)
store volatile %struct.ScmObj* %_95k48455, %struct.ScmObj** %stackaddr$prim57015, align 8
%stackaddr$prim57016 = alloca %struct.ScmObj*, align 8
%current_45args55188 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55187)
store volatile %struct.ScmObj* %current_45args55188, %struct.ScmObj** %stackaddr$prim57016, align 8
%stackaddr$prim57017 = alloca %struct.ScmObj*, align 8
%anf_45bind48185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55188)
store volatile %struct.ScmObj* %anf_45bind48185, %struct.ScmObj** %stackaddr$prim57017, align 8
%ae49130 = call %struct.ScmObj* @const_init_null()
%argslist55190$_37foldl1480300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57018 = alloca %struct.ScmObj*, align 8
%argslist55190$_37foldl1480301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48069, %struct.ScmObj* %argslist55190$_37foldl1480300)
store volatile %struct.ScmObj* %argslist55190$_37foldl1480301, %struct.ScmObj** %stackaddr$prim57018, align 8
%stackaddr$prim57019 = alloca %struct.ScmObj*, align 8
%argslist55190$_37foldl1480302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49130, %struct.ScmObj* %argslist55190$_37foldl1480301)
store volatile %struct.ScmObj* %argslist55190$_37foldl1480302, %struct.ScmObj** %stackaddr$prim57019, align 8
%stackaddr$prim57020 = alloca %struct.ScmObj*, align 8
%argslist55190$_37foldl1480303 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48185, %struct.ScmObj* %argslist55190$_37foldl1480302)
store volatile %struct.ScmObj* %argslist55190$_37foldl1480303, %struct.ScmObj** %stackaddr$prim57020, align 8
%stackaddr$prim57021 = alloca %struct.ScmObj*, align 8
%argslist55190$_37foldl1480304 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48454, %struct.ScmObj* %argslist55190$_37foldl1480303)
store volatile %struct.ScmObj* %argslist55190$_37foldl1480304, %struct.ScmObj** %stackaddr$prim57021, align 8
%clofunc57022 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148030)
musttail call tailcc void %clofunc57022(%struct.ScmObj* %_37foldl148030, %struct.ScmObj* %argslist55190$_37foldl1480304)
ret void
}

define tailcc void @proc_clo$ae49111(%struct.ScmObj* %env$ae49111,%struct.ScmObj* %current_45args55191) {
%stackaddr$prim57023 = alloca %struct.ScmObj*, align 8
%k48456 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55191)
store volatile %struct.ScmObj* %k48456, %struct.ScmObj** %stackaddr$prim57023, align 8
%stackaddr$prim57024 = alloca %struct.ScmObj*, align 8
%current_45args55192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55191)
store volatile %struct.ScmObj* %current_45args55192, %struct.ScmObj** %stackaddr$prim57024, align 8
%stackaddr$prim57025 = alloca %struct.ScmObj*, align 8
%x48071 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55192)
store volatile %struct.ScmObj* %x48071, %struct.ScmObj** %stackaddr$prim57025, align 8
%stackaddr$prim57026 = alloca %struct.ScmObj*, align 8
%current_45args55193 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55192)
store volatile %struct.ScmObj* %current_45args55193, %struct.ScmObj** %stackaddr$prim57026, align 8
%stackaddr$prim57027 = alloca %struct.ScmObj*, align 8
%y48070 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55193)
store volatile %struct.ScmObj* %y48070, %struct.ScmObj** %stackaddr$prim57027, align 8
%ae49113 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55195$k484560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57028 = alloca %struct.ScmObj*, align 8
%argslist55195$k484561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x48071, %struct.ScmObj* %argslist55195$k484560)
store volatile %struct.ScmObj* %argslist55195$k484561, %struct.ScmObj** %stackaddr$prim57028, align 8
%stackaddr$prim57029 = alloca %struct.ScmObj*, align 8
%argslist55195$k484562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49113, %struct.ScmObj* %argslist55195$k484561)
store volatile %struct.ScmObj* %argslist55195$k484562, %struct.ScmObj** %stackaddr$prim57029, align 8
%clofunc57030 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48456)
musttail call tailcc void %clofunc57030(%struct.ScmObj* %k48456, %struct.ScmObj* %argslist55195$k484562)
ret void
}

define tailcc void @proc_clo$ae49029(%struct.ScmObj* %env$ae49029,%struct.ScmObj* %current_45args55199) {
%stackaddr$prim57031 = alloca %struct.ScmObj*, align 8
%k48457 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55199)
store volatile %struct.ScmObj* %k48457, %struct.ScmObj** %stackaddr$prim57031, align 8
%stackaddr$prim57032 = alloca %struct.ScmObj*, align 8
%current_45args55200 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55199)
store volatile %struct.ScmObj* %current_45args55200, %struct.ScmObj** %stackaddr$prim57032, align 8
%stackaddr$prim57033 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55200)
store volatile %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$prim57033, align 8
%ae49031 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57034 = alloca %struct.ScmObj*, align 8
%fptrToInt57035 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49032 to i64
%ae49032 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57035)
store volatile %struct.ScmObj* %ae49032, %struct.ScmObj** %stackaddr$makeclosure57034, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49032, %struct.ScmObj* %_37foldl148031, i64 0)
%argslist55213$k484570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57036 = alloca %struct.ScmObj*, align 8
%argslist55213$k484571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49032, %struct.ScmObj* %argslist55213$k484570)
store volatile %struct.ScmObj* %argslist55213$k484571, %struct.ScmObj** %stackaddr$prim57036, align 8
%stackaddr$prim57037 = alloca %struct.ScmObj*, align 8
%argslist55213$k484572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49031, %struct.ScmObj* %argslist55213$k484571)
store volatile %struct.ScmObj* %argslist55213$k484572, %struct.ScmObj** %stackaddr$prim57037, align 8
%clofunc57038 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48457)
musttail call tailcc void %clofunc57038(%struct.ScmObj* %k48457, %struct.ScmObj* %argslist55213$k484572)
ret void
}

define tailcc void @proc_clo$ae49032(%struct.ScmObj* %env$ae49032,%struct.ScmObj* %current_45args55202) {
%stackaddr$env-ref57039 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49032, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref57039
%stackaddr$prim57040 = alloca %struct.ScmObj*, align 8
%k48458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55202)
store volatile %struct.ScmObj* %k48458, %struct.ScmObj** %stackaddr$prim57040, align 8
%stackaddr$prim57041 = alloca %struct.ScmObj*, align 8
%current_45args55203 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55202)
store volatile %struct.ScmObj* %current_45args55203, %struct.ScmObj** %stackaddr$prim57041, align 8
%stackaddr$prim57042 = alloca %struct.ScmObj*, align 8
%f48034 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55203)
store volatile %struct.ScmObj* %f48034, %struct.ScmObj** %stackaddr$prim57042, align 8
%stackaddr$prim57043 = alloca %struct.ScmObj*, align 8
%current_45args55204 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55203)
store volatile %struct.ScmObj* %current_45args55204, %struct.ScmObj** %stackaddr$prim57043, align 8
%stackaddr$prim57044 = alloca %struct.ScmObj*, align 8
%acc48033 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55204)
store volatile %struct.ScmObj* %acc48033, %struct.ScmObj** %stackaddr$prim57044, align 8
%stackaddr$prim57045 = alloca %struct.ScmObj*, align 8
%current_45args55205 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55204)
store volatile %struct.ScmObj* %current_45args55205, %struct.ScmObj** %stackaddr$prim57045, align 8
%stackaddr$prim57046 = alloca %struct.ScmObj*, align 8
%lst48032 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55205)
store volatile %struct.ScmObj* %lst48032, %struct.ScmObj** %stackaddr$prim57046, align 8
%stackaddr$prim57047 = alloca %struct.ScmObj*, align 8
%anf_45bind48180 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48032)
store volatile %struct.ScmObj* %anf_45bind48180, %struct.ScmObj** %stackaddr$prim57047, align 8
%truthy$cmp57048 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48180)
%cmp$cmp57048 = icmp eq i64 %truthy$cmp57048, 1
br i1 %cmp$cmp57048, label %truebranch$cmp57048, label %falsebranch$cmp57048
truebranch$cmp57048:
%ae49036 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55207$k484580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57049 = alloca %struct.ScmObj*, align 8
%argslist55207$k484581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48033, %struct.ScmObj* %argslist55207$k484580)
store volatile %struct.ScmObj* %argslist55207$k484581, %struct.ScmObj** %stackaddr$prim57049, align 8
%stackaddr$prim57050 = alloca %struct.ScmObj*, align 8
%argslist55207$k484582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49036, %struct.ScmObj* %argslist55207$k484581)
store volatile %struct.ScmObj* %argslist55207$k484582, %struct.ScmObj** %stackaddr$prim57050, align 8
%clofunc57051 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48458)
musttail call tailcc void %clofunc57051(%struct.ScmObj* %k48458, %struct.ScmObj* %argslist55207$k484582)
ret void
falsebranch$cmp57048:
%stackaddr$prim57052 = alloca %struct.ScmObj*, align 8
%anf_45bind48181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48032)
store volatile %struct.ScmObj* %anf_45bind48181, %struct.ScmObj** %stackaddr$prim57052, align 8
%stackaddr$makeclosure57053 = alloca %struct.ScmObj*, align 8
%fptrToInt57054 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49043 to i64
%ae49043 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57054)
store volatile %struct.ScmObj* %ae49043, %struct.ScmObj** %stackaddr$makeclosure57053, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49043, %struct.ScmObj* %f48034, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49043, %struct.ScmObj* %lst48032, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49043, %struct.ScmObj* %_37foldl148031, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49043, %struct.ScmObj* %k48458, i64 3)
%argslist55212$f480340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57055 = alloca %struct.ScmObj*, align 8
%argslist55212$f480341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48033, %struct.ScmObj* %argslist55212$f480340)
store volatile %struct.ScmObj* %argslist55212$f480341, %struct.ScmObj** %stackaddr$prim57055, align 8
%stackaddr$prim57056 = alloca %struct.ScmObj*, align 8
%argslist55212$f480342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48181, %struct.ScmObj* %argslist55212$f480341)
store volatile %struct.ScmObj* %argslist55212$f480342, %struct.ScmObj** %stackaddr$prim57056, align 8
%stackaddr$prim57057 = alloca %struct.ScmObj*, align 8
%argslist55212$f480343 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49043, %struct.ScmObj* %argslist55212$f480342)
store volatile %struct.ScmObj* %argslist55212$f480343, %struct.ScmObj** %stackaddr$prim57057, align 8
%clofunc57058 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48034)
musttail call tailcc void %clofunc57058(%struct.ScmObj* %f48034, %struct.ScmObj* %argslist55212$f480343)
ret void
}

define tailcc void @proc_clo$ae49043(%struct.ScmObj* %env$ae49043,%struct.ScmObj* %current_45args55208) {
%stackaddr$env-ref57059 = alloca %struct.ScmObj*, align 8
%f48034 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49043, i64 0)
store %struct.ScmObj* %f48034, %struct.ScmObj** %stackaddr$env-ref57059
%stackaddr$env-ref57060 = alloca %struct.ScmObj*, align 8
%lst48032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49043, i64 1)
store %struct.ScmObj* %lst48032, %struct.ScmObj** %stackaddr$env-ref57060
%stackaddr$env-ref57061 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49043, i64 2)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref57061
%stackaddr$env-ref57062 = alloca %struct.ScmObj*, align 8
%k48458 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49043, i64 3)
store %struct.ScmObj* %k48458, %struct.ScmObj** %stackaddr$env-ref57062
%stackaddr$prim57063 = alloca %struct.ScmObj*, align 8
%_95k48459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55208)
store volatile %struct.ScmObj* %_95k48459, %struct.ScmObj** %stackaddr$prim57063, align 8
%stackaddr$prim57064 = alloca %struct.ScmObj*, align 8
%current_45args55209 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55208)
store volatile %struct.ScmObj* %current_45args55209, %struct.ScmObj** %stackaddr$prim57064, align 8
%stackaddr$prim57065 = alloca %struct.ScmObj*, align 8
%anf_45bind48182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55209)
store volatile %struct.ScmObj* %anf_45bind48182, %struct.ScmObj** %stackaddr$prim57065, align 8
%stackaddr$prim57066 = alloca %struct.ScmObj*, align 8
%anf_45bind48183 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48032)
store volatile %struct.ScmObj* %anf_45bind48183, %struct.ScmObj** %stackaddr$prim57066, align 8
%argslist55211$_37foldl1480310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57067 = alloca %struct.ScmObj*, align 8
%argslist55211$_37foldl1480311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48183, %struct.ScmObj* %argslist55211$_37foldl1480310)
store volatile %struct.ScmObj* %argslist55211$_37foldl1480311, %struct.ScmObj** %stackaddr$prim57067, align 8
%stackaddr$prim57068 = alloca %struct.ScmObj*, align 8
%argslist55211$_37foldl1480312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48182, %struct.ScmObj* %argslist55211$_37foldl1480311)
store volatile %struct.ScmObj* %argslist55211$_37foldl1480312, %struct.ScmObj** %stackaddr$prim57068, align 8
%stackaddr$prim57069 = alloca %struct.ScmObj*, align 8
%argslist55211$_37foldl1480313 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48034, %struct.ScmObj* %argslist55211$_37foldl1480312)
store volatile %struct.ScmObj* %argslist55211$_37foldl1480313, %struct.ScmObj** %stackaddr$prim57069, align 8
%stackaddr$prim57070 = alloca %struct.ScmObj*, align 8
%argslist55211$_37foldl1480314 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48458, %struct.ScmObj* %argslist55211$_37foldl1480313)
store volatile %struct.ScmObj* %argslist55211$_37foldl1480314, %struct.ScmObj** %stackaddr$prim57070, align 8
%clofunc57071 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148031)
musttail call tailcc void %clofunc57071(%struct.ScmObj* %_37foldl148031, %struct.ScmObj* %argslist55211$_37foldl1480314)
ret void
}

define tailcc void @proc_clo$ae48946(%struct.ScmObj* %env$ae48946,%struct.ScmObj* %current_45args55216) {
%stackaddr$prim57072 = alloca %struct.ScmObj*, align 8
%k48460 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55216)
store volatile %struct.ScmObj* %k48460, %struct.ScmObj** %stackaddr$prim57072, align 8
%stackaddr$prim57073 = alloca %struct.ScmObj*, align 8
%current_45args55217 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55216)
store volatile %struct.ScmObj* %current_45args55217, %struct.ScmObj** %stackaddr$prim57073, align 8
%stackaddr$prim57074 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55217)
store volatile %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$prim57074, align 8
%ae48948 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57075 = alloca %struct.ScmObj*, align 8
%fptrToInt57076 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48949 to i64
%ae48949 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57076)
store volatile %struct.ScmObj* %ae48949, %struct.ScmObj** %stackaddr$makeclosure57075, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48949, %struct.ScmObj* %_37length48036, i64 0)
%argslist55228$k484600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57077 = alloca %struct.ScmObj*, align 8
%argslist55228$k484601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48949, %struct.ScmObj* %argslist55228$k484600)
store volatile %struct.ScmObj* %argslist55228$k484601, %struct.ScmObj** %stackaddr$prim57077, align 8
%stackaddr$prim57078 = alloca %struct.ScmObj*, align 8
%argslist55228$k484602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48948, %struct.ScmObj* %argslist55228$k484601)
store volatile %struct.ScmObj* %argslist55228$k484602, %struct.ScmObj** %stackaddr$prim57078, align 8
%clofunc57079 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48460)
musttail call tailcc void %clofunc57079(%struct.ScmObj* %k48460, %struct.ScmObj* %argslist55228$k484602)
ret void
}

define tailcc void @proc_clo$ae48949(%struct.ScmObj* %env$ae48949,%struct.ScmObj* %current_45args55219) {
%stackaddr$env-ref57080 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48949, i64 0)
store %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$env-ref57080
%stackaddr$prim57081 = alloca %struct.ScmObj*, align 8
%k48461 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55219)
store volatile %struct.ScmObj* %k48461, %struct.ScmObj** %stackaddr$prim57081, align 8
%stackaddr$prim57082 = alloca %struct.ScmObj*, align 8
%current_45args55220 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55219)
store volatile %struct.ScmObj* %current_45args55220, %struct.ScmObj** %stackaddr$prim57082, align 8
%stackaddr$prim57083 = alloca %struct.ScmObj*, align 8
%lst48037 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55220)
store volatile %struct.ScmObj* %lst48037, %struct.ScmObj** %stackaddr$prim57083, align 8
%stackaddr$prim57084 = alloca %struct.ScmObj*, align 8
%anf_45bind48176 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48037)
store volatile %struct.ScmObj* %anf_45bind48176, %struct.ScmObj** %stackaddr$prim57084, align 8
%truthy$cmp57085 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48176)
%cmp$cmp57085 = icmp eq i64 %truthy$cmp57085, 1
br i1 %cmp$cmp57085, label %truebranch$cmp57085, label %falsebranch$cmp57085
truebranch$cmp57085:
%ae48953 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48954 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55222$k484610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57086 = alloca %struct.ScmObj*, align 8
%argslist55222$k484611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48954, %struct.ScmObj* %argslist55222$k484610)
store volatile %struct.ScmObj* %argslist55222$k484611, %struct.ScmObj** %stackaddr$prim57086, align 8
%stackaddr$prim57087 = alloca %struct.ScmObj*, align 8
%argslist55222$k484612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48953, %struct.ScmObj* %argslist55222$k484611)
store volatile %struct.ScmObj* %argslist55222$k484612, %struct.ScmObj** %stackaddr$prim57087, align 8
%clofunc57088 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48461)
musttail call tailcc void %clofunc57088(%struct.ScmObj* %k48461, %struct.ScmObj* %argslist55222$k484612)
ret void
falsebranch$cmp57085:
%stackaddr$prim57089 = alloca %struct.ScmObj*, align 8
%anf_45bind48177 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48037)
store volatile %struct.ScmObj* %anf_45bind48177, %struct.ScmObj** %stackaddr$prim57089, align 8
%stackaddr$makeclosure57090 = alloca %struct.ScmObj*, align 8
%fptrToInt57091 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48963 to i64
%ae48963 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57091)
store volatile %struct.ScmObj* %ae48963, %struct.ScmObj** %stackaddr$makeclosure57090, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48963, %struct.ScmObj* %k48461, i64 0)
%argslist55227$_37length480360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57092 = alloca %struct.ScmObj*, align 8
%argslist55227$_37length480361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48177, %struct.ScmObj* %argslist55227$_37length480360)
store volatile %struct.ScmObj* %argslist55227$_37length480361, %struct.ScmObj** %stackaddr$prim57092, align 8
%stackaddr$prim57093 = alloca %struct.ScmObj*, align 8
%argslist55227$_37length480362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48963, %struct.ScmObj* %argslist55227$_37length480361)
store volatile %struct.ScmObj* %argslist55227$_37length480362, %struct.ScmObj** %stackaddr$prim57093, align 8
%clofunc57094 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48036)
musttail call tailcc void %clofunc57094(%struct.ScmObj* %_37length48036, %struct.ScmObj* %argslist55227$_37length480362)
ret void
}

define tailcc void @proc_clo$ae48963(%struct.ScmObj* %env$ae48963,%struct.ScmObj* %current_45args55223) {
%stackaddr$env-ref57095 = alloca %struct.ScmObj*, align 8
%k48461 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48963, i64 0)
store %struct.ScmObj* %k48461, %struct.ScmObj** %stackaddr$env-ref57095
%stackaddr$prim57096 = alloca %struct.ScmObj*, align 8
%_95k48462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55223)
store volatile %struct.ScmObj* %_95k48462, %struct.ScmObj** %stackaddr$prim57096, align 8
%stackaddr$prim57097 = alloca %struct.ScmObj*, align 8
%current_45args55224 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55223)
store volatile %struct.ScmObj* %current_45args55224, %struct.ScmObj** %stackaddr$prim57097, align 8
%stackaddr$prim57098 = alloca %struct.ScmObj*, align 8
%anf_45bind48178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55224)
store volatile %struct.ScmObj* %anf_45bind48178, %struct.ScmObj** %stackaddr$prim57098, align 8
%ae48965 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57099 = alloca %struct.ScmObj*, align 8
%cpsprim48463 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae48965, %struct.ScmObj* %anf_45bind48178)
store volatile %struct.ScmObj* %cpsprim48463, %struct.ScmObj** %stackaddr$prim57099, align 8
%ae48968 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55226$k484610 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57100 = alloca %struct.ScmObj*, align 8
%argslist55226$k484611 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48463, %struct.ScmObj* %argslist55226$k484610)
store volatile %struct.ScmObj* %argslist55226$k484611, %struct.ScmObj** %stackaddr$prim57100, align 8
%stackaddr$prim57101 = alloca %struct.ScmObj*, align 8
%argslist55226$k484612 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48968, %struct.ScmObj* %argslist55226$k484611)
store volatile %struct.ScmObj* %argslist55226$k484612, %struct.ScmObj** %stackaddr$prim57101, align 8
%clofunc57102 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48461)
musttail call tailcc void %clofunc57102(%struct.ScmObj* %k48461, %struct.ScmObj* %argslist55226$k484612)
ret void
}

define tailcc void @proc_clo$ae48796(%struct.ScmObj* %env$ae48796,%struct.ScmObj* %current_45args55231) {
%stackaddr$prim57103 = alloca %struct.ScmObj*, align 8
%k48464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55231)
store volatile %struct.ScmObj* %k48464, %struct.ScmObj** %stackaddr$prim57103, align 8
%stackaddr$prim57104 = alloca %struct.ScmObj*, align 8
%current_45args55232 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55231)
store volatile %struct.ScmObj* %current_45args55232, %struct.ScmObj** %stackaddr$prim57104, align 8
%stackaddr$prim57105 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55232)
store volatile %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$prim57105, align 8
%ae48798 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57106 = alloca %struct.ScmObj*, align 8
%fptrToInt57107 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48799 to i64
%ae48799 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57107)
store volatile %struct.ScmObj* %ae48799, %struct.ScmObj** %stackaddr$makeclosure57106, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48799, %struct.ScmObj* %_37take48039, i64 0)
%argslist55245$k484640 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57108 = alloca %struct.ScmObj*, align 8
%argslist55245$k484641 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48799, %struct.ScmObj* %argslist55245$k484640)
store volatile %struct.ScmObj* %argslist55245$k484641, %struct.ScmObj** %stackaddr$prim57108, align 8
%stackaddr$prim57109 = alloca %struct.ScmObj*, align 8
%argslist55245$k484642 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48798, %struct.ScmObj* %argslist55245$k484641)
store volatile %struct.ScmObj* %argslist55245$k484642, %struct.ScmObj** %stackaddr$prim57109, align 8
%clofunc57110 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48464)
musttail call tailcc void %clofunc57110(%struct.ScmObj* %k48464, %struct.ScmObj* %argslist55245$k484642)
ret void
}

define tailcc void @proc_clo$ae48799(%struct.ScmObj* %env$ae48799,%struct.ScmObj* %current_45args55234) {
%stackaddr$env-ref57111 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48799, i64 0)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref57111
%stackaddr$prim57112 = alloca %struct.ScmObj*, align 8
%k48465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55234)
store volatile %struct.ScmObj* %k48465, %struct.ScmObj** %stackaddr$prim57112, align 8
%stackaddr$prim57113 = alloca %struct.ScmObj*, align 8
%current_45args55235 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55234)
store volatile %struct.ScmObj* %current_45args55235, %struct.ScmObj** %stackaddr$prim57113, align 8
%stackaddr$prim57114 = alloca %struct.ScmObj*, align 8
%lst48041 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55235)
store volatile %struct.ScmObj* %lst48041, %struct.ScmObj** %stackaddr$prim57114, align 8
%stackaddr$prim57115 = alloca %struct.ScmObj*, align 8
%current_45args55236 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55235)
store volatile %struct.ScmObj* %current_45args55236, %struct.ScmObj** %stackaddr$prim57115, align 8
%stackaddr$prim57116 = alloca %struct.ScmObj*, align 8
%n48040 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55236)
store volatile %struct.ScmObj* %n48040, %struct.ScmObj** %stackaddr$prim57116, align 8
%ae48801 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim57117 = alloca %struct.ScmObj*, align 8
%anf_45bind48169 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n48040, %struct.ScmObj* %ae48801)
store volatile %struct.ScmObj* %anf_45bind48169, %struct.ScmObj** %stackaddr$prim57117, align 8
%truthy$cmp57118 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48169)
%cmp$cmp57118 = icmp eq i64 %truthy$cmp57118, 1
br i1 %cmp$cmp57118, label %truebranch$cmp57118, label %falsebranch$cmp57118
truebranch$cmp57118:
%ae48804 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48805 = call %struct.ScmObj* @const_init_null()
%argslist55238$k484650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57119 = alloca %struct.ScmObj*, align 8
%argslist55238$k484651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48805, %struct.ScmObj* %argslist55238$k484650)
store volatile %struct.ScmObj* %argslist55238$k484651, %struct.ScmObj** %stackaddr$prim57119, align 8
%stackaddr$prim57120 = alloca %struct.ScmObj*, align 8
%argslist55238$k484652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48804, %struct.ScmObj* %argslist55238$k484651)
store volatile %struct.ScmObj* %argslist55238$k484652, %struct.ScmObj** %stackaddr$prim57120, align 8
%clofunc57121 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48465)
musttail call tailcc void %clofunc57121(%struct.ScmObj* %k48465, %struct.ScmObj* %argslist55238$k484652)
ret void
falsebranch$cmp57118:
%stackaddr$prim57122 = alloca %struct.ScmObj*, align 8
%anf_45bind48170 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48041)
store volatile %struct.ScmObj* %anf_45bind48170, %struct.ScmObj** %stackaddr$prim57122, align 8
%truthy$cmp57123 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48170)
%cmp$cmp57123 = icmp eq i64 %truthy$cmp57123, 1
br i1 %cmp$cmp57123, label %truebranch$cmp57123, label %falsebranch$cmp57123
truebranch$cmp57123:
%ae48815 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48816 = call %struct.ScmObj* @const_init_null()
%argslist55239$k484650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57124 = alloca %struct.ScmObj*, align 8
%argslist55239$k484651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48816, %struct.ScmObj* %argslist55239$k484650)
store volatile %struct.ScmObj* %argslist55239$k484651, %struct.ScmObj** %stackaddr$prim57124, align 8
%stackaddr$prim57125 = alloca %struct.ScmObj*, align 8
%argslist55239$k484652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48815, %struct.ScmObj* %argslist55239$k484651)
store volatile %struct.ScmObj* %argslist55239$k484652, %struct.ScmObj** %stackaddr$prim57125, align 8
%clofunc57126 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48465)
musttail call tailcc void %clofunc57126(%struct.ScmObj* %k48465, %struct.ScmObj* %argslist55239$k484652)
ret void
falsebranch$cmp57123:
%stackaddr$prim57127 = alloca %struct.ScmObj*, align 8
%anf_45bind48171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48041)
store volatile %struct.ScmObj* %anf_45bind48171, %struct.ScmObj** %stackaddr$prim57127, align 8
%stackaddr$prim57128 = alloca %struct.ScmObj*, align 8
%anf_45bind48172 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48041)
store volatile %struct.ScmObj* %anf_45bind48172, %struct.ScmObj** %stackaddr$prim57128, align 8
%ae48826 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim57129 = alloca %struct.ScmObj*, align 8
%anf_45bind48173 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n48040, %struct.ScmObj* %ae48826)
store volatile %struct.ScmObj* %anf_45bind48173, %struct.ScmObj** %stackaddr$prim57129, align 8
%stackaddr$makeclosure57130 = alloca %struct.ScmObj*, align 8
%fptrToInt57131 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48828 to i64
%ae48828 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57131)
store volatile %struct.ScmObj* %ae48828, %struct.ScmObj** %stackaddr$makeclosure57130, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48828, %struct.ScmObj* %k48465, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48828, %struct.ScmObj* %anf_45bind48171, i64 1)
%argslist55244$_37take480390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57132 = alloca %struct.ScmObj*, align 8
%argslist55244$_37take480391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48173, %struct.ScmObj* %argslist55244$_37take480390)
store volatile %struct.ScmObj* %argslist55244$_37take480391, %struct.ScmObj** %stackaddr$prim57132, align 8
%stackaddr$prim57133 = alloca %struct.ScmObj*, align 8
%argslist55244$_37take480392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48172, %struct.ScmObj* %argslist55244$_37take480391)
store volatile %struct.ScmObj* %argslist55244$_37take480392, %struct.ScmObj** %stackaddr$prim57133, align 8
%stackaddr$prim57134 = alloca %struct.ScmObj*, align 8
%argslist55244$_37take480393 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48828, %struct.ScmObj* %argslist55244$_37take480392)
store volatile %struct.ScmObj* %argslist55244$_37take480393, %struct.ScmObj** %stackaddr$prim57134, align 8
%clofunc57135 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48039)
musttail call tailcc void %clofunc57135(%struct.ScmObj* %_37take48039, %struct.ScmObj* %argslist55244$_37take480393)
ret void
}

define tailcc void @proc_clo$ae48828(%struct.ScmObj* %env$ae48828,%struct.ScmObj* %current_45args55240) {
%stackaddr$env-ref57136 = alloca %struct.ScmObj*, align 8
%k48465 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48828, i64 0)
store %struct.ScmObj* %k48465, %struct.ScmObj** %stackaddr$env-ref57136
%stackaddr$env-ref57137 = alloca %struct.ScmObj*, align 8
%anf_45bind48171 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48828, i64 1)
store %struct.ScmObj* %anf_45bind48171, %struct.ScmObj** %stackaddr$env-ref57137
%stackaddr$prim57138 = alloca %struct.ScmObj*, align 8
%_95k48466 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55240)
store volatile %struct.ScmObj* %_95k48466, %struct.ScmObj** %stackaddr$prim57138, align 8
%stackaddr$prim57139 = alloca %struct.ScmObj*, align 8
%current_45args55241 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55240)
store volatile %struct.ScmObj* %current_45args55241, %struct.ScmObj** %stackaddr$prim57139, align 8
%stackaddr$prim57140 = alloca %struct.ScmObj*, align 8
%anf_45bind48174 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55241)
store volatile %struct.ScmObj* %anf_45bind48174, %struct.ScmObj** %stackaddr$prim57140, align 8
%stackaddr$prim57141 = alloca %struct.ScmObj*, align 8
%cpsprim48467 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48171, %struct.ScmObj* %anf_45bind48174)
store volatile %struct.ScmObj* %cpsprim48467, %struct.ScmObj** %stackaddr$prim57141, align 8
%ae48834 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55243$k484650 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57142 = alloca %struct.ScmObj*, align 8
%argslist55243$k484651 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48467, %struct.ScmObj* %argslist55243$k484650)
store volatile %struct.ScmObj* %argslist55243$k484651, %struct.ScmObj** %stackaddr$prim57142, align 8
%stackaddr$prim57143 = alloca %struct.ScmObj*, align 8
%argslist55243$k484652 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48834, %struct.ScmObj* %argslist55243$k484651)
store volatile %struct.ScmObj* %argslist55243$k484652, %struct.ScmObj** %stackaddr$prim57143, align 8
%clofunc57144 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48465)
musttail call tailcc void %clofunc57144(%struct.ScmObj* %k48465, %struct.ScmObj* %argslist55243$k484652)
ret void
}

define tailcc void @proc_clo$ae48699(%struct.ScmObj* %env$ae48699,%struct.ScmObj* %current_45args55248) {
%stackaddr$prim57145 = alloca %struct.ScmObj*, align 8
%k48468 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55248)
store volatile %struct.ScmObj* %k48468, %struct.ScmObj** %stackaddr$prim57145, align 8
%stackaddr$prim57146 = alloca %struct.ScmObj*, align 8
%current_45args55249 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55248)
store volatile %struct.ScmObj* %current_45args55249, %struct.ScmObj** %stackaddr$prim57146, align 8
%stackaddr$prim57147 = alloca %struct.ScmObj*, align 8
%_37map48043 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55249)
store volatile %struct.ScmObj* %_37map48043, %struct.ScmObj** %stackaddr$prim57147, align 8
%ae48701 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57148 = alloca %struct.ScmObj*, align 8
%fptrToInt57149 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48702 to i64
%ae48702 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57149)
store volatile %struct.ScmObj* %ae48702, %struct.ScmObj** %stackaddr$makeclosure57148, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48702, %struct.ScmObj* %_37map48043, i64 0)
%argslist55265$k484680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57150 = alloca %struct.ScmObj*, align 8
%argslist55265$k484681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48702, %struct.ScmObj* %argslist55265$k484680)
store volatile %struct.ScmObj* %argslist55265$k484681, %struct.ScmObj** %stackaddr$prim57150, align 8
%stackaddr$prim57151 = alloca %struct.ScmObj*, align 8
%argslist55265$k484682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48701, %struct.ScmObj* %argslist55265$k484681)
store volatile %struct.ScmObj* %argslist55265$k484682, %struct.ScmObj** %stackaddr$prim57151, align 8
%clofunc57152 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48468)
musttail call tailcc void %clofunc57152(%struct.ScmObj* %k48468, %struct.ScmObj* %argslist55265$k484682)
ret void
}

define tailcc void @proc_clo$ae48702(%struct.ScmObj* %env$ae48702,%struct.ScmObj* %current_45args55251) {
%stackaddr$env-ref57153 = alloca %struct.ScmObj*, align 8
%_37map48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48702, i64 0)
store %struct.ScmObj* %_37map48043, %struct.ScmObj** %stackaddr$env-ref57153
%stackaddr$prim57154 = alloca %struct.ScmObj*, align 8
%k48469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55251)
store volatile %struct.ScmObj* %k48469, %struct.ScmObj** %stackaddr$prim57154, align 8
%stackaddr$prim57155 = alloca %struct.ScmObj*, align 8
%current_45args55252 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55251)
store volatile %struct.ScmObj* %current_45args55252, %struct.ScmObj** %stackaddr$prim57155, align 8
%stackaddr$prim57156 = alloca %struct.ScmObj*, align 8
%f48045 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55252)
store volatile %struct.ScmObj* %f48045, %struct.ScmObj** %stackaddr$prim57156, align 8
%stackaddr$prim57157 = alloca %struct.ScmObj*, align 8
%current_45args55253 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55252)
store volatile %struct.ScmObj* %current_45args55253, %struct.ScmObj** %stackaddr$prim57157, align 8
%stackaddr$prim57158 = alloca %struct.ScmObj*, align 8
%lst48044 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55253)
store volatile %struct.ScmObj* %lst48044, %struct.ScmObj** %stackaddr$prim57158, align 8
%stackaddr$prim57159 = alloca %struct.ScmObj*, align 8
%anf_45bind48163 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48044)
store volatile %struct.ScmObj* %anf_45bind48163, %struct.ScmObj** %stackaddr$prim57159, align 8
%truthy$cmp57160 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48163)
%cmp$cmp57160 = icmp eq i64 %truthy$cmp57160, 1
br i1 %cmp$cmp57160, label %truebranch$cmp57160, label %falsebranch$cmp57160
truebranch$cmp57160:
%ae48706 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48707 = call %struct.ScmObj* @const_init_null()
%argslist55255$k484690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57161 = alloca %struct.ScmObj*, align 8
%argslist55255$k484691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48707, %struct.ScmObj* %argslist55255$k484690)
store volatile %struct.ScmObj* %argslist55255$k484691, %struct.ScmObj** %stackaddr$prim57161, align 8
%stackaddr$prim57162 = alloca %struct.ScmObj*, align 8
%argslist55255$k484692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48706, %struct.ScmObj* %argslist55255$k484691)
store volatile %struct.ScmObj* %argslist55255$k484692, %struct.ScmObj** %stackaddr$prim57162, align 8
%clofunc57163 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48469)
musttail call tailcc void %clofunc57163(%struct.ScmObj* %k48469, %struct.ScmObj* %argslist55255$k484692)
ret void
falsebranch$cmp57160:
%stackaddr$prim57164 = alloca %struct.ScmObj*, align 8
%anf_45bind48164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48044)
store volatile %struct.ScmObj* %anf_45bind48164, %struct.ScmObj** %stackaddr$prim57164, align 8
%stackaddr$makeclosure57165 = alloca %struct.ScmObj*, align 8
%fptrToInt57166 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48716 to i64
%ae48716 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt57166)
store volatile %struct.ScmObj* %ae48716, %struct.ScmObj** %stackaddr$makeclosure57165, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48716, %struct.ScmObj* %k48469, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48716, %struct.ScmObj* %f48045, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48716, %struct.ScmObj* %lst48044, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48716, %struct.ScmObj* %_37map48043, i64 3)
%argslist55264$f480450 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57167 = alloca %struct.ScmObj*, align 8
%argslist55264$f480451 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48164, %struct.ScmObj* %argslist55264$f480450)
store volatile %struct.ScmObj* %argslist55264$f480451, %struct.ScmObj** %stackaddr$prim57167, align 8
%stackaddr$prim57168 = alloca %struct.ScmObj*, align 8
%argslist55264$f480452 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48716, %struct.ScmObj* %argslist55264$f480451)
store volatile %struct.ScmObj* %argslist55264$f480452, %struct.ScmObj** %stackaddr$prim57168, align 8
%clofunc57169 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48045)
musttail call tailcc void %clofunc57169(%struct.ScmObj* %f48045, %struct.ScmObj* %argslist55264$f480452)
ret void
}

define tailcc void @proc_clo$ae48716(%struct.ScmObj* %env$ae48716,%struct.ScmObj* %current_45args55256) {
%stackaddr$env-ref57170 = alloca %struct.ScmObj*, align 8
%k48469 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48716, i64 0)
store %struct.ScmObj* %k48469, %struct.ScmObj** %stackaddr$env-ref57170
%stackaddr$env-ref57171 = alloca %struct.ScmObj*, align 8
%f48045 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48716, i64 1)
store %struct.ScmObj* %f48045, %struct.ScmObj** %stackaddr$env-ref57171
%stackaddr$env-ref57172 = alloca %struct.ScmObj*, align 8
%lst48044 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48716, i64 2)
store %struct.ScmObj* %lst48044, %struct.ScmObj** %stackaddr$env-ref57172
%stackaddr$env-ref57173 = alloca %struct.ScmObj*, align 8
%_37map48043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48716, i64 3)
store %struct.ScmObj* %_37map48043, %struct.ScmObj** %stackaddr$env-ref57173
%stackaddr$prim57174 = alloca %struct.ScmObj*, align 8
%_95k48470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55256)
store volatile %struct.ScmObj* %_95k48470, %struct.ScmObj** %stackaddr$prim57174, align 8
%stackaddr$prim57175 = alloca %struct.ScmObj*, align 8
%current_45args55257 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55256)
store volatile %struct.ScmObj* %current_45args55257, %struct.ScmObj** %stackaddr$prim57175, align 8
%stackaddr$prim57176 = alloca %struct.ScmObj*, align 8
%anf_45bind48165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55257)
store volatile %struct.ScmObj* %anf_45bind48165, %struct.ScmObj** %stackaddr$prim57176, align 8
%stackaddr$prim57177 = alloca %struct.ScmObj*, align 8
%anf_45bind48166 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48044)
store volatile %struct.ScmObj* %anf_45bind48166, %struct.ScmObj** %stackaddr$prim57177, align 8
%stackaddr$makeclosure57178 = alloca %struct.ScmObj*, align 8
%fptrToInt57179 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48720 to i64
%ae48720 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57179)
store volatile %struct.ScmObj* %ae48720, %struct.ScmObj** %stackaddr$makeclosure57178, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48720, %struct.ScmObj* %k48469, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48720, %struct.ScmObj* %anf_45bind48165, i64 1)
%argslist55263$_37map480430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57180 = alloca %struct.ScmObj*, align 8
%argslist55263$_37map480431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48166, %struct.ScmObj* %argslist55263$_37map480430)
store volatile %struct.ScmObj* %argslist55263$_37map480431, %struct.ScmObj** %stackaddr$prim57180, align 8
%stackaddr$prim57181 = alloca %struct.ScmObj*, align 8
%argslist55263$_37map480432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48045, %struct.ScmObj* %argslist55263$_37map480431)
store volatile %struct.ScmObj* %argslist55263$_37map480432, %struct.ScmObj** %stackaddr$prim57181, align 8
%stackaddr$prim57182 = alloca %struct.ScmObj*, align 8
%argslist55263$_37map480433 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48720, %struct.ScmObj* %argslist55263$_37map480432)
store volatile %struct.ScmObj* %argslist55263$_37map480433, %struct.ScmObj** %stackaddr$prim57182, align 8
%clofunc57183 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map48043)
musttail call tailcc void %clofunc57183(%struct.ScmObj* %_37map48043, %struct.ScmObj* %argslist55263$_37map480433)
ret void
}

define tailcc void @proc_clo$ae48720(%struct.ScmObj* %env$ae48720,%struct.ScmObj* %current_45args55259) {
%stackaddr$env-ref57184 = alloca %struct.ScmObj*, align 8
%k48469 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48720, i64 0)
store %struct.ScmObj* %k48469, %struct.ScmObj** %stackaddr$env-ref57184
%stackaddr$env-ref57185 = alloca %struct.ScmObj*, align 8
%anf_45bind48165 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48720, i64 1)
store %struct.ScmObj* %anf_45bind48165, %struct.ScmObj** %stackaddr$env-ref57185
%stackaddr$prim57186 = alloca %struct.ScmObj*, align 8
%_95k48471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55259)
store volatile %struct.ScmObj* %_95k48471, %struct.ScmObj** %stackaddr$prim57186, align 8
%stackaddr$prim57187 = alloca %struct.ScmObj*, align 8
%current_45args55260 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55259)
store volatile %struct.ScmObj* %current_45args55260, %struct.ScmObj** %stackaddr$prim57187, align 8
%stackaddr$prim57188 = alloca %struct.ScmObj*, align 8
%anf_45bind48167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55260)
store volatile %struct.ScmObj* %anf_45bind48167, %struct.ScmObj** %stackaddr$prim57188, align 8
%stackaddr$prim57189 = alloca %struct.ScmObj*, align 8
%cpsprim48472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48165, %struct.ScmObj* %anf_45bind48167)
store volatile %struct.ScmObj* %cpsprim48472, %struct.ScmObj** %stackaddr$prim57189, align 8
%ae48726 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55262$k484690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57190 = alloca %struct.ScmObj*, align 8
%argslist55262$k484691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48472, %struct.ScmObj* %argslist55262$k484690)
store volatile %struct.ScmObj* %argslist55262$k484691, %struct.ScmObj** %stackaddr$prim57190, align 8
%stackaddr$prim57191 = alloca %struct.ScmObj*, align 8
%argslist55262$k484692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48726, %struct.ScmObj* %argslist55262$k484691)
store volatile %struct.ScmObj* %argslist55262$k484692, %struct.ScmObj** %stackaddr$prim57191, align 8
%clofunc57192 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48469)
musttail call tailcc void %clofunc57192(%struct.ScmObj* %k48469, %struct.ScmObj* %argslist55262$k484692)
ret void
}

define tailcc void @proc_clo$ae48619(%struct.ScmObj* %env$ae48619,%struct.ScmObj* %current_45args55268) {
%stackaddr$prim57193 = alloca %struct.ScmObj*, align 8
%k48473 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55268)
store volatile %struct.ScmObj* %k48473, %struct.ScmObj** %stackaddr$prim57193, align 8
%stackaddr$prim57194 = alloca %struct.ScmObj*, align 8
%current_45args55269 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55268)
store volatile %struct.ScmObj* %current_45args55269, %struct.ScmObj** %stackaddr$prim57194, align 8
%stackaddr$prim57195 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55269)
store volatile %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$prim57195, align 8
%ae48621 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57196 = alloca %struct.ScmObj*, align 8
%fptrToInt57197 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48622 to i64
%ae48622 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57197)
store volatile %struct.ScmObj* %ae48622, %struct.ScmObj** %stackaddr$makeclosure57196, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48622, %struct.ScmObj* %_37foldr148047, i64 0)
%argslist55282$k484730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57198 = alloca %struct.ScmObj*, align 8
%argslist55282$k484731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48622, %struct.ScmObj* %argslist55282$k484730)
store volatile %struct.ScmObj* %argslist55282$k484731, %struct.ScmObj** %stackaddr$prim57198, align 8
%stackaddr$prim57199 = alloca %struct.ScmObj*, align 8
%argslist55282$k484732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48621, %struct.ScmObj* %argslist55282$k484731)
store volatile %struct.ScmObj* %argslist55282$k484732, %struct.ScmObj** %stackaddr$prim57199, align 8
%clofunc57200 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48473)
musttail call tailcc void %clofunc57200(%struct.ScmObj* %k48473, %struct.ScmObj* %argslist55282$k484732)
ret void
}

define tailcc void @proc_clo$ae48622(%struct.ScmObj* %env$ae48622,%struct.ScmObj* %current_45args55271) {
%stackaddr$env-ref57201 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48622, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref57201
%stackaddr$prim57202 = alloca %struct.ScmObj*, align 8
%k48474 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55271)
store volatile %struct.ScmObj* %k48474, %struct.ScmObj** %stackaddr$prim57202, align 8
%stackaddr$prim57203 = alloca %struct.ScmObj*, align 8
%current_45args55272 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55271)
store volatile %struct.ScmObj* %current_45args55272, %struct.ScmObj** %stackaddr$prim57203, align 8
%stackaddr$prim57204 = alloca %struct.ScmObj*, align 8
%f48050 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55272)
store volatile %struct.ScmObj* %f48050, %struct.ScmObj** %stackaddr$prim57204, align 8
%stackaddr$prim57205 = alloca %struct.ScmObj*, align 8
%current_45args55273 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55272)
store volatile %struct.ScmObj* %current_45args55273, %struct.ScmObj** %stackaddr$prim57205, align 8
%stackaddr$prim57206 = alloca %struct.ScmObj*, align 8
%acc48049 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55273)
store volatile %struct.ScmObj* %acc48049, %struct.ScmObj** %stackaddr$prim57206, align 8
%stackaddr$prim57207 = alloca %struct.ScmObj*, align 8
%current_45args55274 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55273)
store volatile %struct.ScmObj* %current_45args55274, %struct.ScmObj** %stackaddr$prim57207, align 8
%stackaddr$prim57208 = alloca %struct.ScmObj*, align 8
%lst48048 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55274)
store volatile %struct.ScmObj* %lst48048, %struct.ScmObj** %stackaddr$prim57208, align 8
%stackaddr$prim57209 = alloca %struct.ScmObj*, align 8
%anf_45bind48158 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48048)
store volatile %struct.ScmObj* %anf_45bind48158, %struct.ScmObj** %stackaddr$prim57209, align 8
%truthy$cmp57210 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48158)
%cmp$cmp57210 = icmp eq i64 %truthy$cmp57210, 1
br i1 %cmp$cmp57210, label %truebranch$cmp57210, label %falsebranch$cmp57210
truebranch$cmp57210:
%ae48626 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist55276$k484740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57211 = alloca %struct.ScmObj*, align 8
%argslist55276$k484741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48049, %struct.ScmObj* %argslist55276$k484740)
store volatile %struct.ScmObj* %argslist55276$k484741, %struct.ScmObj** %stackaddr$prim57211, align 8
%stackaddr$prim57212 = alloca %struct.ScmObj*, align 8
%argslist55276$k484742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48626, %struct.ScmObj* %argslist55276$k484741)
store volatile %struct.ScmObj* %argslist55276$k484742, %struct.ScmObj** %stackaddr$prim57212, align 8
%clofunc57213 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48474)
musttail call tailcc void %clofunc57213(%struct.ScmObj* %k48474, %struct.ScmObj* %argslist55276$k484742)
ret void
falsebranch$cmp57210:
%stackaddr$prim57214 = alloca %struct.ScmObj*, align 8
%anf_45bind48159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48048)
store volatile %struct.ScmObj* %anf_45bind48159, %struct.ScmObj** %stackaddr$prim57214, align 8
%stackaddr$prim57215 = alloca %struct.ScmObj*, align 8
%anf_45bind48160 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48048)
store volatile %struct.ScmObj* %anf_45bind48160, %struct.ScmObj** %stackaddr$prim57215, align 8
%stackaddr$makeclosure57216 = alloca %struct.ScmObj*, align 8
%fptrToInt57217 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48634 to i64
%ae48634 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57217)
store volatile %struct.ScmObj* %ae48634, %struct.ScmObj** %stackaddr$makeclosure57216, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48634, %struct.ScmObj* %f48050, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48634, %struct.ScmObj* %anf_45bind48159, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48634, %struct.ScmObj* %k48474, i64 2)
%argslist55281$_37foldr1480470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57218 = alloca %struct.ScmObj*, align 8
%argslist55281$_37foldr1480471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48160, %struct.ScmObj* %argslist55281$_37foldr1480470)
store volatile %struct.ScmObj* %argslist55281$_37foldr1480471, %struct.ScmObj** %stackaddr$prim57218, align 8
%stackaddr$prim57219 = alloca %struct.ScmObj*, align 8
%argslist55281$_37foldr1480472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48049, %struct.ScmObj* %argslist55281$_37foldr1480471)
store volatile %struct.ScmObj* %argslist55281$_37foldr1480472, %struct.ScmObj** %stackaddr$prim57219, align 8
%stackaddr$prim57220 = alloca %struct.ScmObj*, align 8
%argslist55281$_37foldr1480473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48050, %struct.ScmObj* %argslist55281$_37foldr1480472)
store volatile %struct.ScmObj* %argslist55281$_37foldr1480473, %struct.ScmObj** %stackaddr$prim57220, align 8
%stackaddr$prim57221 = alloca %struct.ScmObj*, align 8
%argslist55281$_37foldr1480474 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48634, %struct.ScmObj* %argslist55281$_37foldr1480473)
store volatile %struct.ScmObj* %argslist55281$_37foldr1480474, %struct.ScmObj** %stackaddr$prim57221, align 8
%clofunc57222 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148047)
musttail call tailcc void %clofunc57222(%struct.ScmObj* %_37foldr148047, %struct.ScmObj* %argslist55281$_37foldr1480474)
ret void
}

define tailcc void @proc_clo$ae48634(%struct.ScmObj* %env$ae48634,%struct.ScmObj* %current_45args55277) {
%stackaddr$env-ref57223 = alloca %struct.ScmObj*, align 8
%f48050 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48634, i64 0)
store %struct.ScmObj* %f48050, %struct.ScmObj** %stackaddr$env-ref57223
%stackaddr$env-ref57224 = alloca %struct.ScmObj*, align 8
%anf_45bind48159 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48634, i64 1)
store %struct.ScmObj* %anf_45bind48159, %struct.ScmObj** %stackaddr$env-ref57224
%stackaddr$env-ref57225 = alloca %struct.ScmObj*, align 8
%k48474 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48634, i64 2)
store %struct.ScmObj* %k48474, %struct.ScmObj** %stackaddr$env-ref57225
%stackaddr$prim57226 = alloca %struct.ScmObj*, align 8
%_95k48475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55277)
store volatile %struct.ScmObj* %_95k48475, %struct.ScmObj** %stackaddr$prim57226, align 8
%stackaddr$prim57227 = alloca %struct.ScmObj*, align 8
%current_45args55278 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55277)
store volatile %struct.ScmObj* %current_45args55278, %struct.ScmObj** %stackaddr$prim57227, align 8
%stackaddr$prim57228 = alloca %struct.ScmObj*, align 8
%anf_45bind48161 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55278)
store volatile %struct.ScmObj* %anf_45bind48161, %struct.ScmObj** %stackaddr$prim57228, align 8
%argslist55280$f480500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57229 = alloca %struct.ScmObj*, align 8
%argslist55280$f480501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48161, %struct.ScmObj* %argslist55280$f480500)
store volatile %struct.ScmObj* %argslist55280$f480501, %struct.ScmObj** %stackaddr$prim57229, align 8
%stackaddr$prim57230 = alloca %struct.ScmObj*, align 8
%argslist55280$f480502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48159, %struct.ScmObj* %argslist55280$f480501)
store volatile %struct.ScmObj* %argslist55280$f480502, %struct.ScmObj** %stackaddr$prim57230, align 8
%stackaddr$prim57231 = alloca %struct.ScmObj*, align 8
%argslist55280$f480503 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48474, %struct.ScmObj* %argslist55280$f480502)
store volatile %struct.ScmObj* %argslist55280$f480503, %struct.ScmObj** %stackaddr$prim57231, align 8
%clofunc57232 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48050)
musttail call tailcc void %clofunc57232(%struct.ScmObj* %f48050, %struct.ScmObj* %argslist55280$f480503)
ret void
}

define tailcc void @proc_clo$ae48502(%struct.ScmObj* %env$ae48502,%struct.ScmObj* %current_45args55285) {
%stackaddr$prim57233 = alloca %struct.ScmObj*, align 8
%k48476 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55285)
store volatile %struct.ScmObj* %k48476, %struct.ScmObj** %stackaddr$prim57233, align 8
%stackaddr$prim57234 = alloca %struct.ScmObj*, align 8
%current_45args55286 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55285)
store volatile %struct.ScmObj* %current_45args55286, %struct.ScmObj** %stackaddr$prim57234, align 8
%stackaddr$prim57235 = alloca %struct.ScmObj*, align 8
%y48027 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55286)
store volatile %struct.ScmObj* %y48027, %struct.ScmObj** %stackaddr$prim57235, align 8
%ae48504 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57236 = alloca %struct.ScmObj*, align 8
%fptrToInt57237 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48505 to i64
%ae48505 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt57237)
store volatile %struct.ScmObj* %ae48505, %struct.ScmObj** %stackaddr$makeclosure57236, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48505, %struct.ScmObj* %y48027, i64 0)
%argslist55304$k484760 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57238 = alloca %struct.ScmObj*, align 8
%argslist55304$k484761 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48505, %struct.ScmObj* %argslist55304$k484760)
store volatile %struct.ScmObj* %argslist55304$k484761, %struct.ScmObj** %stackaddr$prim57238, align 8
%stackaddr$prim57239 = alloca %struct.ScmObj*, align 8
%argslist55304$k484762 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48504, %struct.ScmObj* %argslist55304$k484761)
store volatile %struct.ScmObj* %argslist55304$k484762, %struct.ScmObj** %stackaddr$prim57239, align 8
%clofunc57240 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48476)
musttail call tailcc void %clofunc57240(%struct.ScmObj* %k48476, %struct.ScmObj* %argslist55304$k484762)
ret void
}

define tailcc void @proc_clo$ae48505(%struct.ScmObj* %env$ae48505,%struct.ScmObj* %current_45args55288) {
%stackaddr$env-ref57241 = alloca %struct.ScmObj*, align 8
%y48027 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48505, i64 0)
store %struct.ScmObj* %y48027, %struct.ScmObj** %stackaddr$env-ref57241
%stackaddr$prim57242 = alloca %struct.ScmObj*, align 8
%k48477 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55288)
store volatile %struct.ScmObj* %k48477, %struct.ScmObj** %stackaddr$prim57242, align 8
%stackaddr$prim57243 = alloca %struct.ScmObj*, align 8
%current_45args55289 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55288)
store volatile %struct.ScmObj* %current_45args55289, %struct.ScmObj** %stackaddr$prim57243, align 8
%stackaddr$prim57244 = alloca %struct.ScmObj*, align 8
%f48028 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55289)
store volatile %struct.ScmObj* %f48028, %struct.ScmObj** %stackaddr$prim57244, align 8
%stackaddr$makeclosure57245 = alloca %struct.ScmObj*, align 8
%fptrToInt57246 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48506 to i64
%ae48506 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57246)
store volatile %struct.ScmObj* %ae48506, %struct.ScmObj** %stackaddr$makeclosure57245, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48506, %struct.ScmObj* %k48477, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48506, %struct.ScmObj* %f48028, i64 1)
%ae48507 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure57247 = alloca %struct.ScmObj*, align 8
%fptrToInt57248 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48508 to i64
%ae48508 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57248)
store volatile %struct.ScmObj* %ae48508, %struct.ScmObj** %stackaddr$makeclosure57247, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48508, %struct.ScmObj* %f48028, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48508, %struct.ScmObj* %y48027, i64 1)
%argslist55303$ae485060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57249 = alloca %struct.ScmObj*, align 8
%argslist55303$ae485061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48508, %struct.ScmObj* %argslist55303$ae485060)
store volatile %struct.ScmObj* %argslist55303$ae485061, %struct.ScmObj** %stackaddr$prim57249, align 8
%stackaddr$prim57250 = alloca %struct.ScmObj*, align 8
%argslist55303$ae485062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48507, %struct.ScmObj* %argslist55303$ae485061)
store volatile %struct.ScmObj* %argslist55303$ae485062, %struct.ScmObj** %stackaddr$prim57250, align 8
%clofunc57251 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48506)
musttail call tailcc void %clofunc57251(%struct.ScmObj* %ae48506, %struct.ScmObj* %argslist55303$ae485062)
ret void
}

define tailcc void @proc_clo$ae48506(%struct.ScmObj* %env$ae48506,%struct.ScmObj* %current_45args55291) {
%stackaddr$env-ref57252 = alloca %struct.ScmObj*, align 8
%k48477 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48506, i64 0)
store %struct.ScmObj* %k48477, %struct.ScmObj** %stackaddr$env-ref57252
%stackaddr$env-ref57253 = alloca %struct.ScmObj*, align 8
%f48028 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48506, i64 1)
store %struct.ScmObj* %f48028, %struct.ScmObj** %stackaddr$env-ref57253
%stackaddr$prim57254 = alloca %struct.ScmObj*, align 8
%_95k48478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55291)
store volatile %struct.ScmObj* %_95k48478, %struct.ScmObj** %stackaddr$prim57254, align 8
%stackaddr$prim57255 = alloca %struct.ScmObj*, align 8
%current_45args55292 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55291)
store volatile %struct.ScmObj* %current_45args55292, %struct.ScmObj** %stackaddr$prim57255, align 8
%stackaddr$prim57256 = alloca %struct.ScmObj*, align 8
%anf_45bind48156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55292)
store volatile %struct.ScmObj* %anf_45bind48156, %struct.ScmObj** %stackaddr$prim57256, align 8
%argslist55294$f480280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57257 = alloca %struct.ScmObj*, align 8
%argslist55294$f480281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48156, %struct.ScmObj* %argslist55294$f480280)
store volatile %struct.ScmObj* %argslist55294$f480281, %struct.ScmObj** %stackaddr$prim57257, align 8
%stackaddr$prim57258 = alloca %struct.ScmObj*, align 8
%argslist55294$f480282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48477, %struct.ScmObj* %argslist55294$f480281)
store volatile %struct.ScmObj* %argslist55294$f480282, %struct.ScmObj** %stackaddr$prim57258, align 8
%clofunc57259 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48028)
musttail call tailcc void %clofunc57259(%struct.ScmObj* %f48028, %struct.ScmObj* %argslist55294$f480282)
ret void
}

define tailcc void @proc_clo$ae48508(%struct.ScmObj* %env$ae48508,%struct.ScmObj* %args4802948479) {
%stackaddr$env-ref57260 = alloca %struct.ScmObj*, align 8
%f48028 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48508, i64 0)
store %struct.ScmObj* %f48028, %struct.ScmObj** %stackaddr$env-ref57260
%stackaddr$env-ref57261 = alloca %struct.ScmObj*, align 8
%y48027 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48508, i64 1)
store %struct.ScmObj* %y48027, %struct.ScmObj** %stackaddr$env-ref57261
%stackaddr$prim57262 = alloca %struct.ScmObj*, align 8
%k48480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4802948479)
store volatile %struct.ScmObj* %k48480, %struct.ScmObj** %stackaddr$prim57262, align 8
%stackaddr$prim57263 = alloca %struct.ScmObj*, align 8
%args48029 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4802948479)
store volatile %struct.ScmObj* %args48029, %struct.ScmObj** %stackaddr$prim57263, align 8
%stackaddr$makeclosure57264 = alloca %struct.ScmObj*, align 8
%fptrToInt57265 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48512 to i64
%ae48512 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt57265)
store volatile %struct.ScmObj* %ae48512, %struct.ScmObj** %stackaddr$makeclosure57264, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48512, %struct.ScmObj* %k48480, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48512, %struct.ScmObj* %args48029, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48512, %struct.ScmObj* %f48028, i64 2)
%argslist55302$y480270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57266 = alloca %struct.ScmObj*, align 8
%argslist55302$y480271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y48027, %struct.ScmObj* %argslist55302$y480270)
store volatile %struct.ScmObj* %argslist55302$y480271, %struct.ScmObj** %stackaddr$prim57266, align 8
%stackaddr$prim57267 = alloca %struct.ScmObj*, align 8
%argslist55302$y480272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48512, %struct.ScmObj* %argslist55302$y480271)
store volatile %struct.ScmObj* %argslist55302$y480272, %struct.ScmObj** %stackaddr$prim57267, align 8
%clofunc57268 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y48027)
musttail call tailcc void %clofunc57268(%struct.ScmObj* %y48027, %struct.ScmObj* %argslist55302$y480272)
ret void
}

define tailcc void @proc_clo$ae48512(%struct.ScmObj* %env$ae48512,%struct.ScmObj* %current_45args55295) {
%stackaddr$env-ref57269 = alloca %struct.ScmObj*, align 8
%k48480 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48512, i64 0)
store %struct.ScmObj* %k48480, %struct.ScmObj** %stackaddr$env-ref57269
%stackaddr$env-ref57270 = alloca %struct.ScmObj*, align 8
%args48029 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48512, i64 1)
store %struct.ScmObj* %args48029, %struct.ScmObj** %stackaddr$env-ref57270
%stackaddr$env-ref57271 = alloca %struct.ScmObj*, align 8
%f48028 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48512, i64 2)
store %struct.ScmObj* %f48028, %struct.ScmObj** %stackaddr$env-ref57271
%stackaddr$prim57272 = alloca %struct.ScmObj*, align 8
%_95k48481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55295)
store volatile %struct.ScmObj* %_95k48481, %struct.ScmObj** %stackaddr$prim57272, align 8
%stackaddr$prim57273 = alloca %struct.ScmObj*, align 8
%current_45args55296 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55295)
store volatile %struct.ScmObj* %current_45args55296, %struct.ScmObj** %stackaddr$prim57273, align 8
%stackaddr$prim57274 = alloca %struct.ScmObj*, align 8
%anf_45bind48154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55296)
store volatile %struct.ScmObj* %anf_45bind48154, %struct.ScmObj** %stackaddr$prim57274, align 8
%stackaddr$makeclosure57275 = alloca %struct.ScmObj*, align 8
%fptrToInt57276 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48515 to i64
%ae48515 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt57276)
store volatile %struct.ScmObj* %ae48515, %struct.ScmObj** %stackaddr$makeclosure57275, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48515, %struct.ScmObj* %k48480, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48515, %struct.ScmObj* %args48029, i64 1)
%argslist55301$anf_45bind481540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57277 = alloca %struct.ScmObj*, align 8
%argslist55301$anf_45bind481541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48028, %struct.ScmObj* %argslist55301$anf_45bind481540)
store volatile %struct.ScmObj* %argslist55301$anf_45bind481541, %struct.ScmObj** %stackaddr$prim57277, align 8
%stackaddr$prim57278 = alloca %struct.ScmObj*, align 8
%argslist55301$anf_45bind481542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48515, %struct.ScmObj* %argslist55301$anf_45bind481541)
store volatile %struct.ScmObj* %argslist55301$anf_45bind481542, %struct.ScmObj** %stackaddr$prim57278, align 8
%clofunc57279 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48154)
musttail call tailcc void %clofunc57279(%struct.ScmObj* %anf_45bind48154, %struct.ScmObj* %argslist55301$anf_45bind481542)
ret void
}

define tailcc void @proc_clo$ae48515(%struct.ScmObj* %env$ae48515,%struct.ScmObj* %current_45args55298) {
%stackaddr$env-ref57280 = alloca %struct.ScmObj*, align 8
%k48480 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48515, i64 0)
store %struct.ScmObj* %k48480, %struct.ScmObj** %stackaddr$env-ref57280
%stackaddr$env-ref57281 = alloca %struct.ScmObj*, align 8
%args48029 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48515, i64 1)
store %struct.ScmObj* %args48029, %struct.ScmObj** %stackaddr$env-ref57281
%stackaddr$prim57282 = alloca %struct.ScmObj*, align 8
%_95k48482 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55298)
store volatile %struct.ScmObj* %_95k48482, %struct.ScmObj** %stackaddr$prim57282, align 8
%stackaddr$prim57283 = alloca %struct.ScmObj*, align 8
%current_45args55299 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55298)
store volatile %struct.ScmObj* %current_45args55299, %struct.ScmObj** %stackaddr$prim57283, align 8
%stackaddr$prim57284 = alloca %struct.ScmObj*, align 8
%anf_45bind48155 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55299)
store volatile %struct.ScmObj* %anf_45bind48155, %struct.ScmObj** %stackaddr$prim57284, align 8
%stackaddr$prim57285 = alloca %struct.ScmObj*, align 8
%cpsargs48483 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48480, %struct.ScmObj* %args48029)
store volatile %struct.ScmObj* %cpsargs48483, %struct.ScmObj** %stackaddr$prim57285, align 8
%clofunc57286 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48155)
musttail call tailcc void %clofunc57286(%struct.ScmObj* %anf_45bind48155, %struct.ScmObj* %cpsargs48483)
ret void
}

define tailcc void @proc_clo$ae48487(%struct.ScmObj* %env$ae48487,%struct.ScmObj* %current_45args55306) {
%stackaddr$prim57287 = alloca %struct.ScmObj*, align 8
%k48484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55306)
store volatile %struct.ScmObj* %k48484, %struct.ScmObj** %stackaddr$prim57287, align 8
%stackaddr$prim57288 = alloca %struct.ScmObj*, align 8
%current_45args55307 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args55306)
store volatile %struct.ScmObj* %current_45args55307, %struct.ScmObj** %stackaddr$prim57288, align 8
%stackaddr$prim57289 = alloca %struct.ScmObj*, align 8
%yu48026 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args55307)
store volatile %struct.ScmObj* %yu48026, %struct.ScmObj** %stackaddr$prim57289, align 8
%argslist55309$yu480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim57290 = alloca %struct.ScmObj*, align 8
%argslist55309$yu480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu48026, %struct.ScmObj* %argslist55309$yu480260)
store volatile %struct.ScmObj* %argslist55309$yu480261, %struct.ScmObj** %stackaddr$prim57290, align 8
%stackaddr$prim57291 = alloca %struct.ScmObj*, align 8
%argslist55309$yu480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48484, %struct.ScmObj* %argslist55309$yu480261)
store volatile %struct.ScmObj* %argslist55309$yu480262, %struct.ScmObj** %stackaddr$prim57291, align 8
%clofunc57292 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu48026)
musttail call tailcc void %clofunc57292(%struct.ScmObj* %yu48026, %struct.ScmObj* %argslist55309$yu480262)
ret void
}