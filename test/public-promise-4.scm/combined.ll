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

@global$sym$ae5117855012 = private unnamed_addr constant [4 x i8] c"yes\00", align 8

define ccc i32 @main() {
%mainenv54356 = call %struct.ScmObj* @const_init_null()
%mainargs54357 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv54356, %struct.ScmObj* %mainargs54357)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv54354,%struct.ScmObj* %mainargs54355) {
%stackaddr$makeclosure54358 = alloca %struct.ScmObj*, align 8
%fptrToInt54359 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47528 to i64
%ae47528 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54359)
store volatile %struct.ScmObj* %ae47528, %struct.ScmObj** %stackaddr$makeclosure54358, align 8
%ae47529 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54360 = alloca %struct.ScmObj*, align 8
%fptrToInt54361 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47530 to i64
%ae47530 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54361)
store volatile %struct.ScmObj* %ae47530, %struct.ScmObj** %stackaddr$makeclosure54360, align 8
%argslist54353$ae475280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54362 = alloca %struct.ScmObj*, align 8
%argslist54353$ae475281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47530, %struct.ScmObj* %argslist54353$ae475280)
store volatile %struct.ScmObj* %argslist54353$ae475281, %struct.ScmObj** %stackaddr$prim54362, align 8
%stackaddr$prim54363 = alloca %struct.ScmObj*, align 8
%argslist54353$ae475282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47529, %struct.ScmObj* %argslist54353$ae475281)
store volatile %struct.ScmObj* %argslist54353$ae475282, %struct.ScmObj** %stackaddr$prim54363, align 8
%clofunc54364 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47528)
musttail call tailcc void %clofunc54364(%struct.ScmObj* %ae47528, %struct.ScmObj* %argslist54353$ae475282)
ret void
}

define tailcc void @proc_clo$ae47528(%struct.ScmObj* %env$ae47528,%struct.ScmObj* %current_45args53737) {
%stackaddr$prim54365 = alloca %struct.ScmObj*, align 8
%_95k47326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53737)
store volatile %struct.ScmObj* %_95k47326, %struct.ScmObj** %stackaddr$prim54365, align 8
%stackaddr$prim54366 = alloca %struct.ScmObj*, align 8
%current_45args53738 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53737)
store volatile %struct.ScmObj* %current_45args53738, %struct.ScmObj** %stackaddr$prim54366, align 8
%stackaddr$prim54367 = alloca %struct.ScmObj*, align 8
%anf_45bind47196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53738)
store volatile %struct.ScmObj* %anf_45bind47196, %struct.ScmObj** %stackaddr$prim54367, align 8
%stackaddr$makeclosure54368 = alloca %struct.ScmObj*, align 8
%fptrToInt54369 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47543 to i64
%ae47543 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54369)
store volatile %struct.ScmObj* %ae47543, %struct.ScmObj** %stackaddr$makeclosure54368, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47543, %struct.ScmObj* %anf_45bind47196, i64 0)
%ae47544 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54370 = alloca %struct.ScmObj*, align 8
%fptrToInt54371 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47545 to i64
%ae47545 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54371)
store volatile %struct.ScmObj* %ae47545, %struct.ScmObj** %stackaddr$makeclosure54370, align 8
%argslist54348$ae475430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54372 = alloca %struct.ScmObj*, align 8
%argslist54348$ae475431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47545, %struct.ScmObj* %argslist54348$ae475430)
store volatile %struct.ScmObj* %argslist54348$ae475431, %struct.ScmObj** %stackaddr$prim54372, align 8
%stackaddr$prim54373 = alloca %struct.ScmObj*, align 8
%argslist54348$ae475432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47544, %struct.ScmObj* %argslist54348$ae475431)
store volatile %struct.ScmObj* %argslist54348$ae475432, %struct.ScmObj** %stackaddr$prim54373, align 8
%clofunc54374 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47543)
musttail call tailcc void %clofunc54374(%struct.ScmObj* %ae47543, %struct.ScmObj* %argslist54348$ae475432)
ret void
}

define tailcc void @proc_clo$ae47543(%struct.ScmObj* %env$ae47543,%struct.ScmObj* %current_45args53740) {
%stackaddr$env-ref54375 = alloca %struct.ScmObj*, align 8
%anf_45bind47196 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47543, i64 0)
store %struct.ScmObj* %anf_45bind47196, %struct.ScmObj** %stackaddr$env-ref54375
%stackaddr$prim54376 = alloca %struct.ScmObj*, align 8
%_95k47327 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53740)
store volatile %struct.ScmObj* %_95k47327, %struct.ScmObj** %stackaddr$prim54376, align 8
%stackaddr$prim54377 = alloca %struct.ScmObj*, align 8
%current_45args53741 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53740)
store volatile %struct.ScmObj* %current_45args53741, %struct.ScmObj** %stackaddr$prim54377, align 8
%stackaddr$prim54378 = alloca %struct.ScmObj*, align 8
%anf_45bind47200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53741)
store volatile %struct.ScmObj* %anf_45bind47200, %struct.ScmObj** %stackaddr$prim54378, align 8
%stackaddr$makeclosure54379 = alloca %struct.ScmObj*, align 8
%fptrToInt54380 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47658 to i64
%ae47658 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54380)
store volatile %struct.ScmObj* %ae47658, %struct.ScmObj** %stackaddr$makeclosure54379, align 8
%argslist54327$anf_45bind471960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54381 = alloca %struct.ScmObj*, align 8
%argslist54327$anf_45bind471961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47200, %struct.ScmObj* %argslist54327$anf_45bind471960)
store volatile %struct.ScmObj* %argslist54327$anf_45bind471961, %struct.ScmObj** %stackaddr$prim54381, align 8
%stackaddr$prim54382 = alloca %struct.ScmObj*, align 8
%argslist54327$anf_45bind471962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47658, %struct.ScmObj* %argslist54327$anf_45bind471961)
store volatile %struct.ScmObj* %argslist54327$anf_45bind471962, %struct.ScmObj** %stackaddr$prim54382, align 8
%clofunc54383 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47196)
musttail call tailcc void %clofunc54383(%struct.ScmObj* %anf_45bind47196, %struct.ScmObj* %argslist54327$anf_45bind471962)
ret void
}

define tailcc void @proc_clo$ae47658(%struct.ScmObj* %env$ae47658,%struct.ScmObj* %current_45args53743) {
%stackaddr$prim54384 = alloca %struct.ScmObj*, align 8
%_95k47328 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53743)
store volatile %struct.ScmObj* %_95k47328, %struct.ScmObj** %stackaddr$prim54384, align 8
%stackaddr$prim54385 = alloca %struct.ScmObj*, align 8
%current_45args53744 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53743)
store volatile %struct.ScmObj* %current_45args53744, %struct.ScmObj** %stackaddr$prim54385, align 8
%stackaddr$prim54386 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53744)
store volatile %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$prim54386, align 8
%stackaddr$makeclosure54387 = alloca %struct.ScmObj*, align 8
%fptrToInt54388 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47660 to i64
%ae47660 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54388)
store volatile %struct.ScmObj* %ae47660, %struct.ScmObj** %stackaddr$makeclosure54387, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47660, %struct.ScmObj* %Ycmb47068, i64 0)
%ae47661 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54389 = alloca %struct.ScmObj*, align 8
%fptrToInt54390 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47662 to i64
%ae47662 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54390)
store volatile %struct.ScmObj* %ae47662, %struct.ScmObj** %stackaddr$makeclosure54389, align 8
%argslist54326$ae476600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54391 = alloca %struct.ScmObj*, align 8
%argslist54326$ae476601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47662, %struct.ScmObj* %argslist54326$ae476600)
store volatile %struct.ScmObj* %argslist54326$ae476601, %struct.ScmObj** %stackaddr$prim54391, align 8
%stackaddr$prim54392 = alloca %struct.ScmObj*, align 8
%argslist54326$ae476602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47661, %struct.ScmObj* %argslist54326$ae476601)
store volatile %struct.ScmObj* %argslist54326$ae476602, %struct.ScmObj** %stackaddr$prim54392, align 8
%clofunc54393 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47660)
musttail call tailcc void %clofunc54393(%struct.ScmObj* %ae47660, %struct.ScmObj* %argslist54326$ae476602)
ret void
}

define tailcc void @proc_clo$ae47660(%struct.ScmObj* %env$ae47660,%struct.ScmObj* %current_45args53746) {
%stackaddr$env-ref54394 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47660, i64 0)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54394
%stackaddr$prim54395 = alloca %struct.ScmObj*, align 8
%_95k47329 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53746)
store volatile %struct.ScmObj* %_95k47329, %struct.ScmObj** %stackaddr$prim54395, align 8
%stackaddr$prim54396 = alloca %struct.ScmObj*, align 8
%current_45args53747 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53746)
store volatile %struct.ScmObj* %current_45args53747, %struct.ScmObj** %stackaddr$prim54396, align 8
%stackaddr$prim54397 = alloca %struct.ScmObj*, align 8
%anf_45bind47205 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53747)
store volatile %struct.ScmObj* %anf_45bind47205, %struct.ScmObj** %stackaddr$prim54397, align 8
%stackaddr$makeclosure54398 = alloca %struct.ScmObj*, align 8
%fptrToInt54399 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47738 to i64
%ae47738 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54399)
store volatile %struct.ScmObj* %ae47738, %struct.ScmObj** %stackaddr$makeclosure54398, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47738, %struct.ScmObj* %Ycmb47068, i64 0)
%argslist54310$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54400 = alloca %struct.ScmObj*, align 8
%argslist54310$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47205, %struct.ScmObj* %argslist54310$Ycmb470680)
store volatile %struct.ScmObj* %argslist54310$Ycmb470681, %struct.ScmObj** %stackaddr$prim54400, align 8
%stackaddr$prim54401 = alloca %struct.ScmObj*, align 8
%argslist54310$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47738, %struct.ScmObj* %argslist54310$Ycmb470681)
store volatile %struct.ScmObj* %argslist54310$Ycmb470682, %struct.ScmObj** %stackaddr$prim54401, align 8
%clofunc54402 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc54402(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist54310$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae47738(%struct.ScmObj* %env$ae47738,%struct.ScmObj* %current_45args53749) {
%stackaddr$env-ref54403 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47738, i64 0)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54403
%stackaddr$prim54404 = alloca %struct.ScmObj*, align 8
%_95k47330 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53749)
store volatile %struct.ScmObj* %_95k47330, %struct.ScmObj** %stackaddr$prim54404, align 8
%stackaddr$prim54405 = alloca %struct.ScmObj*, align 8
%current_45args53750 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53749)
store volatile %struct.ScmObj* %current_45args53750, %struct.ScmObj** %stackaddr$prim54405, align 8
%stackaddr$prim54406 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53750)
store volatile %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$prim54406, align 8
%stackaddr$makeclosure54407 = alloca %struct.ScmObj*, align 8
%fptrToInt54408 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47740 to i64
%ae47740 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54408)
store volatile %struct.ScmObj* %ae47740, %struct.ScmObj** %stackaddr$makeclosure54407, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47740, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47740, %struct.ScmObj* %Ycmb47068, i64 1)
%ae47741 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54409 = alloca %struct.ScmObj*, align 8
%fptrToInt54410 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47742 to i64
%ae47742 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54410)
store volatile %struct.ScmObj* %ae47742, %struct.ScmObj** %stackaddr$makeclosure54409, align 8
%argslist54309$ae477400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54411 = alloca %struct.ScmObj*, align 8
%argslist54309$ae477401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47742, %struct.ScmObj* %argslist54309$ae477400)
store volatile %struct.ScmObj* %argslist54309$ae477401, %struct.ScmObj** %stackaddr$prim54411, align 8
%stackaddr$prim54412 = alloca %struct.ScmObj*, align 8
%argslist54309$ae477402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47741, %struct.ScmObj* %argslist54309$ae477401)
store volatile %struct.ScmObj* %argslist54309$ae477402, %struct.ScmObj** %stackaddr$prim54412, align 8
%clofunc54413 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47740)
musttail call tailcc void %clofunc54413(%struct.ScmObj* %ae47740, %struct.ScmObj* %argslist54309$ae477402)
ret void
}

define tailcc void @proc_clo$ae47740(%struct.ScmObj* %env$ae47740,%struct.ScmObj* %current_45args53752) {
%stackaddr$env-ref54414 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47740, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54414
%stackaddr$env-ref54415 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47740, i64 1)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54415
%stackaddr$prim54416 = alloca %struct.ScmObj*, align 8
%_95k47331 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53752)
store volatile %struct.ScmObj* %_95k47331, %struct.ScmObj** %stackaddr$prim54416, align 8
%stackaddr$prim54417 = alloca %struct.ScmObj*, align 8
%current_45args53753 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53752)
store volatile %struct.ScmObj* %current_45args53753, %struct.ScmObj** %stackaddr$prim54417, align 8
%stackaddr$prim54418 = alloca %struct.ScmObj*, align 8
%anf_45bind47211 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53753)
store volatile %struct.ScmObj* %anf_45bind47211, %struct.ScmObj** %stackaddr$prim54418, align 8
%stackaddr$makeclosure54419 = alloca %struct.ScmObj*, align 8
%fptrToInt54420 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47835 to i64
%ae47835 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54420)
store volatile %struct.ScmObj* %ae47835, %struct.ScmObj** %stackaddr$makeclosure54419, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47835, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47835, %struct.ScmObj* %Ycmb47068, i64 1)
%argslist54290$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54421 = alloca %struct.ScmObj*, align 8
%argslist54290$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47211, %struct.ScmObj* %argslist54290$Ycmb470680)
store volatile %struct.ScmObj* %argslist54290$Ycmb470681, %struct.ScmObj** %stackaddr$prim54421, align 8
%stackaddr$prim54422 = alloca %struct.ScmObj*, align 8
%argslist54290$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47835, %struct.ScmObj* %argslist54290$Ycmb470681)
store volatile %struct.ScmObj* %argslist54290$Ycmb470682, %struct.ScmObj** %stackaddr$prim54422, align 8
%clofunc54423 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc54423(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist54290$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae47835(%struct.ScmObj* %env$ae47835,%struct.ScmObj* %current_45args53755) {
%stackaddr$env-ref54424 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47835, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54424
%stackaddr$env-ref54425 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47835, i64 1)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54425
%stackaddr$prim54426 = alloca %struct.ScmObj*, align 8
%_95k47332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53755)
store volatile %struct.ScmObj* %_95k47332, %struct.ScmObj** %stackaddr$prim54426, align 8
%stackaddr$prim54427 = alloca %struct.ScmObj*, align 8
%current_45args53756 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53755)
store volatile %struct.ScmObj* %current_45args53756, %struct.ScmObj** %stackaddr$prim54427, align 8
%stackaddr$prim54428 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53756)
store volatile %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$prim54428, align 8
%stackaddr$makeclosure54429 = alloca %struct.ScmObj*, align 8
%fptrToInt54430 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47837 to i64
%ae47837 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54430)
store volatile %struct.ScmObj* %ae47837, %struct.ScmObj** %stackaddr$makeclosure54429, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47837, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47837, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47837, %struct.ScmObj* %Ycmb47068, i64 2)
%ae47838 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54431 = alloca %struct.ScmObj*, align 8
%fptrToInt54432 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47839 to i64
%ae47839 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54432)
store volatile %struct.ScmObj* %ae47839, %struct.ScmObj** %stackaddr$makeclosure54431, align 8
%argslist54289$ae478370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54433 = alloca %struct.ScmObj*, align 8
%argslist54289$ae478371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47839, %struct.ScmObj* %argslist54289$ae478370)
store volatile %struct.ScmObj* %argslist54289$ae478371, %struct.ScmObj** %stackaddr$prim54433, align 8
%stackaddr$prim54434 = alloca %struct.ScmObj*, align 8
%argslist54289$ae478372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47838, %struct.ScmObj* %argslist54289$ae478371)
store volatile %struct.ScmObj* %argslist54289$ae478372, %struct.ScmObj** %stackaddr$prim54434, align 8
%clofunc54435 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47837)
musttail call tailcc void %clofunc54435(%struct.ScmObj* %ae47837, %struct.ScmObj* %argslist54289$ae478372)
ret void
}

define tailcc void @proc_clo$ae47837(%struct.ScmObj* %env$ae47837,%struct.ScmObj* %current_45args53758) {
%stackaddr$env-ref54436 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47837, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54436
%stackaddr$env-ref54437 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47837, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54437
%stackaddr$env-ref54438 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47837, i64 2)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54438
%stackaddr$prim54439 = alloca %struct.ScmObj*, align 8
%_95k47333 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53758)
store volatile %struct.ScmObj* %_95k47333, %struct.ScmObj** %stackaddr$prim54439, align 8
%stackaddr$prim54440 = alloca %struct.ScmObj*, align 8
%current_45args53759 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53758)
store volatile %struct.ScmObj* %current_45args53759, %struct.ScmObj** %stackaddr$prim54440, align 8
%stackaddr$prim54441 = alloca %struct.ScmObj*, align 8
%anf_45bind47218 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53759)
store volatile %struct.ScmObj* %anf_45bind47218, %struct.ScmObj** %stackaddr$prim54441, align 8
%stackaddr$makeclosure54442 = alloca %struct.ScmObj*, align 8
%fptrToInt54443 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47985 to i64
%ae47985 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54443)
store volatile %struct.ScmObj* %ae47985, %struct.ScmObj** %stackaddr$makeclosure54442, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47985, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47985, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47985, %struct.ScmObj* %Ycmb47068, i64 2)
%argslist54273$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54444 = alloca %struct.ScmObj*, align 8
%argslist54273$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47218, %struct.ScmObj* %argslist54273$Ycmb470680)
store volatile %struct.ScmObj* %argslist54273$Ycmb470681, %struct.ScmObj** %stackaddr$prim54444, align 8
%stackaddr$prim54445 = alloca %struct.ScmObj*, align 8
%argslist54273$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47985, %struct.ScmObj* %argslist54273$Ycmb470681)
store volatile %struct.ScmObj* %argslist54273$Ycmb470682, %struct.ScmObj** %stackaddr$prim54445, align 8
%clofunc54446 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc54446(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist54273$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae47985(%struct.ScmObj* %env$ae47985,%struct.ScmObj* %current_45args53761) {
%stackaddr$env-ref54447 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47985, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54447
%stackaddr$env-ref54448 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47985, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54448
%stackaddr$env-ref54449 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47985, i64 2)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54449
%stackaddr$prim54450 = alloca %struct.ScmObj*, align 8
%_95k47334 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53761)
store volatile %struct.ScmObj* %_95k47334, %struct.ScmObj** %stackaddr$prim54450, align 8
%stackaddr$prim54451 = alloca %struct.ScmObj*, align 8
%current_45args53762 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53761)
store volatile %struct.ScmObj* %current_45args53762, %struct.ScmObj** %stackaddr$prim54451, align 8
%stackaddr$prim54452 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53762)
store volatile %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$prim54452, align 8
%stackaddr$makeclosure54453 = alloca %struct.ScmObj*, align 8
%fptrToInt54454 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47987 to i64
%ae47987 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54454)
store volatile %struct.ScmObj* %ae47987, %struct.ScmObj** %stackaddr$makeclosure54453, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47987, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47987, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47987, %struct.ScmObj* %Ycmb47068, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47987, %struct.ScmObj* %_37take47081, i64 3)
%ae47988 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54455 = alloca %struct.ScmObj*, align 8
%fptrToInt54456 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47989 to i64
%ae47989 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54456)
store volatile %struct.ScmObj* %ae47989, %struct.ScmObj** %stackaddr$makeclosure54455, align 8
%argslist54272$ae479870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54457 = alloca %struct.ScmObj*, align 8
%argslist54272$ae479871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47989, %struct.ScmObj* %argslist54272$ae479870)
store volatile %struct.ScmObj* %argslist54272$ae479871, %struct.ScmObj** %stackaddr$prim54457, align 8
%stackaddr$prim54458 = alloca %struct.ScmObj*, align 8
%argslist54272$ae479872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47988, %struct.ScmObj* %argslist54272$ae479871)
store volatile %struct.ScmObj* %argslist54272$ae479872, %struct.ScmObj** %stackaddr$prim54458, align 8
%clofunc54459 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47987)
musttail call tailcc void %clofunc54459(%struct.ScmObj* %ae47987, %struct.ScmObj* %argslist54272$ae479872)
ret void
}

define tailcc void @proc_clo$ae47987(%struct.ScmObj* %env$ae47987,%struct.ScmObj* %current_45args53764) {
%stackaddr$env-ref54460 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47987, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54460
%stackaddr$env-ref54461 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47987, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54461
%stackaddr$env-ref54462 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47987, i64 2)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54462
%stackaddr$env-ref54463 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47987, i64 3)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref54463
%stackaddr$prim54464 = alloca %struct.ScmObj*, align 8
%_95k47335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53764)
store volatile %struct.ScmObj* %_95k47335, %struct.ScmObj** %stackaddr$prim54464, align 8
%stackaddr$prim54465 = alloca %struct.ScmObj*, align 8
%current_45args53765 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53764)
store volatile %struct.ScmObj* %current_45args53765, %struct.ScmObj** %stackaddr$prim54465, align 8
%stackaddr$prim54466 = alloca %struct.ScmObj*, align 8
%anf_45bind47222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53765)
store volatile %struct.ScmObj* %anf_45bind47222, %struct.ScmObj** %stackaddr$prim54466, align 8
%stackaddr$makeclosure54467 = alloca %struct.ScmObj*, align 8
%fptrToInt54468 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48068 to i64
%ae48068 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54468)
store volatile %struct.ScmObj* %ae48068, %struct.ScmObj** %stackaddr$makeclosure54467, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48068, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48068, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48068, %struct.ScmObj* %Ycmb47068, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48068, %struct.ScmObj* %_37take47081, i64 3)
%argslist54258$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54469 = alloca %struct.ScmObj*, align 8
%argslist54258$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47222, %struct.ScmObj* %argslist54258$Ycmb470680)
store volatile %struct.ScmObj* %argslist54258$Ycmb470681, %struct.ScmObj** %stackaddr$prim54469, align 8
%stackaddr$prim54470 = alloca %struct.ScmObj*, align 8
%argslist54258$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48068, %struct.ScmObj* %argslist54258$Ycmb470681)
store volatile %struct.ScmObj* %argslist54258$Ycmb470682, %struct.ScmObj** %stackaddr$prim54470, align 8
%clofunc54471 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc54471(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist54258$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae48068(%struct.ScmObj* %env$ae48068,%struct.ScmObj* %current_45args53767) {
%stackaddr$env-ref54472 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48068, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54472
%stackaddr$env-ref54473 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48068, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54473
%stackaddr$env-ref54474 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48068, i64 2)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54474
%stackaddr$env-ref54475 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48068, i64 3)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref54475
%stackaddr$prim54476 = alloca %struct.ScmObj*, align 8
%_95k47336 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53767)
store volatile %struct.ScmObj* %_95k47336, %struct.ScmObj** %stackaddr$prim54476, align 8
%stackaddr$prim54477 = alloca %struct.ScmObj*, align 8
%current_45args53768 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53767)
store volatile %struct.ScmObj* %current_45args53768, %struct.ScmObj** %stackaddr$prim54477, align 8
%stackaddr$prim54478 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53768)
store volatile %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$prim54478, align 8
%stackaddr$makeclosure54479 = alloca %struct.ScmObj*, align 8
%fptrToInt54480 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48070 to i64
%ae48070 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54480)
store volatile %struct.ScmObj* %ae48070, %struct.ScmObj** %stackaddr$makeclosure54479, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48070, %struct.ScmObj* %_37length47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48070, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48070, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48070, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48070, %struct.ScmObj* %_37take47081, i64 4)
%ae48071 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54481 = alloca %struct.ScmObj*, align 8
%fptrToInt54482 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48072 to i64
%ae48072 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54482)
store volatile %struct.ScmObj* %ae48072, %struct.ScmObj** %stackaddr$makeclosure54481, align 8
%argslist54257$ae480700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54483 = alloca %struct.ScmObj*, align 8
%argslist54257$ae480701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48072, %struct.ScmObj* %argslist54257$ae480700)
store volatile %struct.ScmObj* %argslist54257$ae480701, %struct.ScmObj** %stackaddr$prim54483, align 8
%stackaddr$prim54484 = alloca %struct.ScmObj*, align 8
%argslist54257$ae480702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48071, %struct.ScmObj* %argslist54257$ae480701)
store volatile %struct.ScmObj* %argslist54257$ae480702, %struct.ScmObj** %stackaddr$prim54484, align 8
%clofunc54485 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48070)
musttail call tailcc void %clofunc54485(%struct.ScmObj* %ae48070, %struct.ScmObj* %argslist54257$ae480702)
ret void
}

define tailcc void @proc_clo$ae48070(%struct.ScmObj* %env$ae48070,%struct.ScmObj* %current_45args53770) {
%stackaddr$env-ref54486 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48070, i64 0)
store %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$env-ref54486
%stackaddr$env-ref54487 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48070, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54487
%stackaddr$env-ref54488 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48070, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54488
%stackaddr$env-ref54489 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48070, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54489
%stackaddr$env-ref54490 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48070, i64 4)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref54490
%stackaddr$prim54491 = alloca %struct.ScmObj*, align 8
%_95k47337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53770)
store volatile %struct.ScmObj* %_95k47337, %struct.ScmObj** %stackaddr$prim54491, align 8
%stackaddr$prim54492 = alloca %struct.ScmObj*, align 8
%current_45args53771 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53770)
store volatile %struct.ScmObj* %current_45args53771, %struct.ScmObj** %stackaddr$prim54492, align 8
%stackaddr$prim54493 = alloca %struct.ScmObj*, align 8
%anf_45bind47227 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53771)
store volatile %struct.ScmObj* %anf_45bind47227, %struct.ScmObj** %stackaddr$prim54493, align 8
%stackaddr$makeclosure54494 = alloca %struct.ScmObj*, align 8
%fptrToInt54495 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48147 to i64
%ae48147 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54495)
store volatile %struct.ScmObj* %ae48147, %struct.ScmObj** %stackaddr$makeclosure54494, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48147, %struct.ScmObj* %_37length47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48147, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48147, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48147, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48147, %struct.ScmObj* %_37take47081, i64 4)
%argslist54241$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54496 = alloca %struct.ScmObj*, align 8
%argslist54241$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47227, %struct.ScmObj* %argslist54241$Ycmb470680)
store volatile %struct.ScmObj* %argslist54241$Ycmb470681, %struct.ScmObj** %stackaddr$prim54496, align 8
%stackaddr$prim54497 = alloca %struct.ScmObj*, align 8
%argslist54241$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48147, %struct.ScmObj* %argslist54241$Ycmb470681)
store volatile %struct.ScmObj* %argslist54241$Ycmb470682, %struct.ScmObj** %stackaddr$prim54497, align 8
%clofunc54498 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc54498(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist54241$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae48147(%struct.ScmObj* %env$ae48147,%struct.ScmObj* %current_45args53773) {
%stackaddr$env-ref54499 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48147, i64 0)
store %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$env-ref54499
%stackaddr$env-ref54500 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48147, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54500
%stackaddr$env-ref54501 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48147, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54501
%stackaddr$env-ref54502 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48147, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54502
%stackaddr$env-ref54503 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48147, i64 4)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref54503
%stackaddr$prim54504 = alloca %struct.ScmObj*, align 8
%_95k47338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53773)
store volatile %struct.ScmObj* %_95k47338, %struct.ScmObj** %stackaddr$prim54504, align 8
%stackaddr$prim54505 = alloca %struct.ScmObj*, align 8
%current_45args53774 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53773)
store volatile %struct.ScmObj* %current_45args53774, %struct.ScmObj** %stackaddr$prim54505, align 8
%stackaddr$prim54506 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53774)
store volatile %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$prim54506, align 8
%stackaddr$makeclosure54507 = alloca %struct.ScmObj*, align 8
%fptrToInt54508 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48149 to i64
%ae48149 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54508)
store volatile %struct.ScmObj* %ae48149, %struct.ScmObj** %stackaddr$makeclosure54507, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48149, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48149, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48149, %struct.ScmObj* %_37length47078, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48149, %struct.ScmObj* %_37map147085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48149, %struct.ScmObj* %Ycmb47068, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48149, %struct.ScmObj* %_37take47081, i64 5)
%ae48150 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54509 = alloca %struct.ScmObj*, align 8
%fptrToInt54510 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48151 to i64
%ae48151 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54510)
store volatile %struct.ScmObj* %ae48151, %struct.ScmObj** %stackaddr$makeclosure54509, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48151, %struct.ScmObj* %_37foldl147073, i64 0)
%argslist54240$ae481490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54511 = alloca %struct.ScmObj*, align 8
%argslist54240$ae481491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48151, %struct.ScmObj* %argslist54240$ae481490)
store volatile %struct.ScmObj* %argslist54240$ae481491, %struct.ScmObj** %stackaddr$prim54511, align 8
%stackaddr$prim54512 = alloca %struct.ScmObj*, align 8
%argslist54240$ae481492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48150, %struct.ScmObj* %argslist54240$ae481491)
store volatile %struct.ScmObj* %argslist54240$ae481492, %struct.ScmObj** %stackaddr$prim54512, align 8
%clofunc54513 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48149)
musttail call tailcc void %clofunc54513(%struct.ScmObj* %ae48149, %struct.ScmObj* %argslist54240$ae481492)
ret void
}

define tailcc void @proc_clo$ae48149(%struct.ScmObj* %env$ae48149,%struct.ScmObj* %current_45args53776) {
%stackaddr$env-ref54514 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48149, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54514
%stackaddr$env-ref54515 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48149, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54515
%stackaddr$env-ref54516 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48149, i64 2)
store %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$env-ref54516
%stackaddr$env-ref54517 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48149, i64 3)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54517
%stackaddr$env-ref54518 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48149, i64 4)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54518
%stackaddr$env-ref54519 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48149, i64 5)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref54519
%stackaddr$prim54520 = alloca %struct.ScmObj*, align 8
%_95k47339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53776)
store volatile %struct.ScmObj* %_95k47339, %struct.ScmObj** %stackaddr$prim54520, align 8
%stackaddr$prim54521 = alloca %struct.ScmObj*, align 8
%current_45args53777 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53776)
store volatile %struct.ScmObj* %current_45args53777, %struct.ScmObj** %stackaddr$prim54521, align 8
%stackaddr$prim54522 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53777)
store volatile %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$prim54522, align 8
%stackaddr$makeclosure54523 = alloca %struct.ScmObj*, align 8
%fptrToInt54524 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48203 to i64
%ae48203 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54524)
store volatile %struct.ScmObj* %ae48203, %struct.ScmObj** %stackaddr$makeclosure54523, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48203, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48203, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48203, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48203, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48203, %struct.ScmObj* %_37last47111, i64 4)
%ae48204 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54525 = alloca %struct.ScmObj*, align 8
%fptrToInt54526 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48205 to i64
%ae48205 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54526)
store volatile %struct.ScmObj* %ae48205, %struct.ScmObj** %stackaddr$makeclosure54525, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48205, %struct.ScmObj* %_37length47078, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48205, %struct.ScmObj* %_37take47081, i64 1)
%argslist54226$ae482030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54527 = alloca %struct.ScmObj*, align 8
%argslist54226$ae482031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48205, %struct.ScmObj* %argslist54226$ae482030)
store volatile %struct.ScmObj* %argslist54226$ae482031, %struct.ScmObj** %stackaddr$prim54527, align 8
%stackaddr$prim54528 = alloca %struct.ScmObj*, align 8
%argslist54226$ae482032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48204, %struct.ScmObj* %argslist54226$ae482031)
store volatile %struct.ScmObj* %argslist54226$ae482032, %struct.ScmObj** %stackaddr$prim54528, align 8
%clofunc54529 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48203)
musttail call tailcc void %clofunc54529(%struct.ScmObj* %ae48203, %struct.ScmObj* %argslist54226$ae482032)
ret void
}

define tailcc void @proc_clo$ae48203(%struct.ScmObj* %env$ae48203,%struct.ScmObj* %current_45args53779) {
%stackaddr$env-ref54530 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48203, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54530
%stackaddr$env-ref54531 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48203, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54531
%stackaddr$env-ref54532 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48203, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref54532
%stackaddr$env-ref54533 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48203, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54533
%stackaddr$env-ref54534 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48203, i64 4)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref54534
%stackaddr$prim54535 = alloca %struct.ScmObj*, align 8
%_95k47340 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53779)
store volatile %struct.ScmObj* %_95k47340, %struct.ScmObj** %stackaddr$prim54535, align 8
%stackaddr$prim54536 = alloca %struct.ScmObj*, align 8
%current_45args53780 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53779)
store volatile %struct.ScmObj* %current_45args53780, %struct.ScmObj** %stackaddr$prim54536, align 8
%stackaddr$prim54537 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53780)
store volatile %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$prim54537, align 8
%stackaddr$makeclosure54538 = alloca %struct.ScmObj*, align 8
%fptrToInt54539 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48233 to i64
%ae48233 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54539)
store volatile %struct.ScmObj* %ae48233, %struct.ScmObj** %stackaddr$makeclosure54538, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48233, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48233, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48233, %struct.ScmObj* %_37drop_45right47108, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48233, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48233, %struct.ScmObj* %_37last47111, i64 4)
%ae48234 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54540 = alloca %struct.ScmObj*, align 8
%fptrToInt54541 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48235 to i64
%ae48235 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54541)
store volatile %struct.ScmObj* %ae48235, %struct.ScmObj** %stackaddr$makeclosure54540, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48235, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48235, %struct.ScmObj* %_37map147085, i64 1)
%argslist54216$ae482330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54542 = alloca %struct.ScmObj*, align 8
%argslist54216$ae482331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48235, %struct.ScmObj* %argslist54216$ae482330)
store volatile %struct.ScmObj* %argslist54216$ae482331, %struct.ScmObj** %stackaddr$prim54542, align 8
%stackaddr$prim54543 = alloca %struct.ScmObj*, align 8
%argslist54216$ae482332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48234, %struct.ScmObj* %argslist54216$ae482331)
store volatile %struct.ScmObj* %argslist54216$ae482332, %struct.ScmObj** %stackaddr$prim54543, align 8
%clofunc54544 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48233)
musttail call tailcc void %clofunc54544(%struct.ScmObj* %ae48233, %struct.ScmObj* %argslist54216$ae482332)
ret void
}

define tailcc void @proc_clo$ae48233(%struct.ScmObj* %env$ae48233,%struct.ScmObj* %current_45args53782) {
%stackaddr$env-ref54545 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48233, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54545
%stackaddr$env-ref54546 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48233, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54546
%stackaddr$env-ref54547 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48233, i64 2)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref54547
%stackaddr$env-ref54548 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48233, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54548
%stackaddr$env-ref54549 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48233, i64 4)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref54549
%stackaddr$prim54550 = alloca %struct.ScmObj*, align 8
%_95k47341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53782)
store volatile %struct.ScmObj* %_95k47341, %struct.ScmObj** %stackaddr$prim54550, align 8
%stackaddr$prim54551 = alloca %struct.ScmObj*, align 8
%current_45args53783 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53782)
store volatile %struct.ScmObj* %current_45args53783, %struct.ScmObj** %stackaddr$prim54551, align 8
%stackaddr$prim54552 = alloca %struct.ScmObj*, align 8
%anf_45bind47243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53783)
store volatile %struct.ScmObj* %anf_45bind47243, %struct.ScmObj** %stackaddr$prim54552, align 8
%stackaddr$makeclosure54553 = alloca %struct.ScmObj*, align 8
%fptrToInt54554 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48617 to i64
%ae48617 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54554)
store volatile %struct.ScmObj* %ae48617, %struct.ScmObj** %stackaddr$makeclosure54553, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48617, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48617, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48617, %struct.ScmObj* %_37drop_45right47108, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48617, %struct.ScmObj* %Ycmb47068, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48617, %struct.ScmObj* %_37last47111, i64 4)
%argslist54156$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54555 = alloca %struct.ScmObj*, align 8
%argslist54156$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47243, %struct.ScmObj* %argslist54156$Ycmb470680)
store volatile %struct.ScmObj* %argslist54156$Ycmb470681, %struct.ScmObj** %stackaddr$prim54555, align 8
%stackaddr$prim54556 = alloca %struct.ScmObj*, align 8
%argslist54156$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48617, %struct.ScmObj* %argslist54156$Ycmb470681)
store volatile %struct.ScmObj* %argslist54156$Ycmb470682, %struct.ScmObj** %stackaddr$prim54556, align 8
%clofunc54557 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc54557(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist54156$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae48617(%struct.ScmObj* %env$ae48617,%struct.ScmObj* %current_45args53785) {
%stackaddr$env-ref54558 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48617, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54558
%stackaddr$env-ref54559 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48617, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54559
%stackaddr$env-ref54560 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48617, i64 2)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref54560
%stackaddr$env-ref54561 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48617, i64 3)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54561
%stackaddr$env-ref54562 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48617, i64 4)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref54562
%stackaddr$prim54563 = alloca %struct.ScmObj*, align 8
%_95k47342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53785)
store volatile %struct.ScmObj* %_95k47342, %struct.ScmObj** %stackaddr$prim54563, align 8
%stackaddr$prim54564 = alloca %struct.ScmObj*, align 8
%current_45args53786 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53785)
store volatile %struct.ScmObj* %current_45args53786, %struct.ScmObj** %stackaddr$prim54564, align 8
%stackaddr$prim54565 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53786)
store volatile %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$prim54565, align 8
%stackaddr$makeclosure54566 = alloca %struct.ScmObj*, align 8
%fptrToInt54567 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48619 to i64
%ae48619 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54567)
store volatile %struct.ScmObj* %ae48619, %struct.ScmObj** %stackaddr$makeclosure54566, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48619, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48619, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48619, %struct.ScmObj* %_37foldr47094, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48619, %struct.ScmObj* %_37drop_45right47108, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48619, %struct.ScmObj* %Ycmb47068, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48619, %struct.ScmObj* %_37last47111, i64 5)
%ae48620 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54568 = alloca %struct.ScmObj*, align 8
%fptrToInt54569 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48621 to i64
%ae48621 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54569)
store volatile %struct.ScmObj* %ae48621, %struct.ScmObj** %stackaddr$makeclosure54568, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48621, %struct.ScmObj* %_37foldr147089, i64 0)
%argslist54155$ae486190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54570 = alloca %struct.ScmObj*, align 8
%argslist54155$ae486191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48621, %struct.ScmObj* %argslist54155$ae486190)
store volatile %struct.ScmObj* %argslist54155$ae486191, %struct.ScmObj** %stackaddr$prim54570, align 8
%stackaddr$prim54571 = alloca %struct.ScmObj*, align 8
%argslist54155$ae486192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48620, %struct.ScmObj* %argslist54155$ae486191)
store volatile %struct.ScmObj* %argslist54155$ae486192, %struct.ScmObj** %stackaddr$prim54571, align 8
%clofunc54572 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48619)
musttail call tailcc void %clofunc54572(%struct.ScmObj* %ae48619, %struct.ScmObj* %argslist54155$ae486192)
ret void
}

define tailcc void @proc_clo$ae48619(%struct.ScmObj* %env$ae48619,%struct.ScmObj* %current_45args53788) {
%stackaddr$env-ref54573 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48619, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54573
%stackaddr$env-ref54574 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48619, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54574
%stackaddr$env-ref54575 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48619, i64 2)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54575
%stackaddr$env-ref54576 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48619, i64 3)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref54576
%stackaddr$env-ref54577 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48619, i64 4)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54577
%stackaddr$env-ref54578 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48619, i64 5)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref54578
%stackaddr$prim54579 = alloca %struct.ScmObj*, align 8
%_95k47343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53788)
store volatile %struct.ScmObj* %_95k47343, %struct.ScmObj** %stackaddr$prim54579, align 8
%stackaddr$prim54580 = alloca %struct.ScmObj*, align 8
%current_45args53789 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53788)
store volatile %struct.ScmObj* %current_45args53789, %struct.ScmObj** %stackaddr$prim54580, align 8
%stackaddr$prim54581 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53789)
store volatile %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$prim54581, align 8
%stackaddr$makeclosure54582 = alloca %struct.ScmObj*, align 8
%fptrToInt54583 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48696 to i64
%ae48696 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54583)
store volatile %struct.ScmObj* %ae48696, %struct.ScmObj** %stackaddr$makeclosure54582, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48696, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48696, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48696, %struct.ScmObj* %_37foldr47094, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48696, %struct.ScmObj* %_37map147120, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48696, %struct.ScmObj* %Ycmb47068, i64 4)
%ae48697 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54584 = alloca %struct.ScmObj*, align 8
%fptrToInt54585 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48698 to i64
%ae48698 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54585)
store volatile %struct.ScmObj* %ae48698, %struct.ScmObj** %stackaddr$makeclosure54584, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48698, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48698, %struct.ScmObj* %_37drop_45right47108, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48698, %struct.ScmObj* %_37last47111, i64 2)
%argslist54136$ae486960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54586 = alloca %struct.ScmObj*, align 8
%argslist54136$ae486961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48698, %struct.ScmObj* %argslist54136$ae486960)
store volatile %struct.ScmObj* %argslist54136$ae486961, %struct.ScmObj** %stackaddr$prim54586, align 8
%stackaddr$prim54587 = alloca %struct.ScmObj*, align 8
%argslist54136$ae486962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48697, %struct.ScmObj* %argslist54136$ae486961)
store volatile %struct.ScmObj* %argslist54136$ae486962, %struct.ScmObj** %stackaddr$prim54587, align 8
%clofunc54588 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48696)
musttail call tailcc void %clofunc54588(%struct.ScmObj* %ae48696, %struct.ScmObj* %argslist54136$ae486962)
ret void
}

define tailcc void @proc_clo$ae48696(%struct.ScmObj* %env$ae48696,%struct.ScmObj* %current_45args53791) {
%stackaddr$env-ref54589 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48696, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref54589
%stackaddr$env-ref54590 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48696, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54590
%stackaddr$env-ref54591 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48696, i64 2)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref54591
%stackaddr$env-ref54592 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48696, i64 3)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54592
%stackaddr$env-ref54593 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48696, i64 4)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54593
%stackaddr$prim54594 = alloca %struct.ScmObj*, align 8
%_95k47344 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53791)
store volatile %struct.ScmObj* %_95k47344, %struct.ScmObj** %stackaddr$prim54594, align 8
%stackaddr$prim54595 = alloca %struct.ScmObj*, align 8
%current_45args53792 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53791)
store volatile %struct.ScmObj* %current_45args53792, %struct.ScmObj** %stackaddr$prim54595, align 8
%stackaddr$prim54596 = alloca %struct.ScmObj*, align 8
%_37map47115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53792)
store volatile %struct.ScmObj* %_37map47115, %struct.ScmObj** %stackaddr$prim54596, align 8
%stackaddr$makeclosure54597 = alloca %struct.ScmObj*, align 8
%fptrToInt54598 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48842 to i64
%ae48842 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54598)
store volatile %struct.ScmObj* %ae48842, %struct.ScmObj** %stackaddr$makeclosure54597, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48842, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48842, %struct.ScmObj* %_37map147120, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48842, %struct.ScmObj* %Ycmb47068, i64 2)
%ae48843 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54599 = alloca %struct.ScmObj*, align 8
%fptrToInt54600 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48844 to i64
%ae48844 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54600)
store volatile %struct.ScmObj* %ae48844, %struct.ScmObj** %stackaddr$makeclosure54599, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48844, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48844, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48844, %struct.ScmObj* %_37map147120, i64 2)
%argslist54119$ae488420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54601 = alloca %struct.ScmObj*, align 8
%argslist54119$ae488421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48844, %struct.ScmObj* %argslist54119$ae488420)
store volatile %struct.ScmObj* %argslist54119$ae488421, %struct.ScmObj** %stackaddr$prim54601, align 8
%stackaddr$prim54602 = alloca %struct.ScmObj*, align 8
%argslist54119$ae488422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48843, %struct.ScmObj* %argslist54119$ae488421)
store volatile %struct.ScmObj* %argslist54119$ae488422, %struct.ScmObj** %stackaddr$prim54602, align 8
%clofunc54603 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48842)
musttail call tailcc void %clofunc54603(%struct.ScmObj* %ae48842, %struct.ScmObj* %argslist54119$ae488422)
ret void
}

define tailcc void @proc_clo$ae48842(%struct.ScmObj* %env$ae48842,%struct.ScmObj* %current_45args53794) {
%stackaddr$env-ref54604 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48842, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54604
%stackaddr$env-ref54605 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48842, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54605
%stackaddr$env-ref54606 = alloca %struct.ScmObj*, align 8
%Ycmb47068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48842, i64 2)
store %struct.ScmObj* %Ycmb47068, %struct.ScmObj** %stackaddr$env-ref54606
%stackaddr$prim54607 = alloca %struct.ScmObj*, align 8
%_95k47345 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53794)
store volatile %struct.ScmObj* %_95k47345, %struct.ScmObj** %stackaddr$prim54607, align 8
%stackaddr$prim54608 = alloca %struct.ScmObj*, align 8
%current_45args53795 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53794)
store volatile %struct.ScmObj* %current_45args53795, %struct.ScmObj** %stackaddr$prim54608, align 8
%stackaddr$prim54609 = alloca %struct.ScmObj*, align 8
%anf_45bind47263 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53795)
store volatile %struct.ScmObj* %anf_45bind47263, %struct.ScmObj** %stackaddr$prim54609, align 8
%stackaddr$makeclosure54610 = alloca %struct.ScmObj*, align 8
%fptrToInt54611 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49234 to i64
%ae49234 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54611)
store volatile %struct.ScmObj* %ae49234, %struct.ScmObj** %stackaddr$makeclosure54610, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49234, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49234, %struct.ScmObj* %_37map147120, i64 1)
%argslist54059$Ycmb470680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54612 = alloca %struct.ScmObj*, align 8
%argslist54059$Ycmb470681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47263, %struct.ScmObj* %argslist54059$Ycmb470680)
store volatile %struct.ScmObj* %argslist54059$Ycmb470681, %struct.ScmObj** %stackaddr$prim54612, align 8
%stackaddr$prim54613 = alloca %struct.ScmObj*, align 8
%argslist54059$Ycmb470682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49234, %struct.ScmObj* %argslist54059$Ycmb470681)
store volatile %struct.ScmObj* %argslist54059$Ycmb470682, %struct.ScmObj** %stackaddr$prim54613, align 8
%clofunc54614 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb47068)
musttail call tailcc void %clofunc54614(%struct.ScmObj* %Ycmb47068, %struct.ScmObj* %argslist54059$Ycmb470682)
ret void
}

define tailcc void @proc_clo$ae49234(%struct.ScmObj* %env$ae49234,%struct.ScmObj* %current_45args53797) {
%stackaddr$env-ref54615 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49234, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54615
%stackaddr$env-ref54616 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49234, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54616
%stackaddr$prim54617 = alloca %struct.ScmObj*, align 8
%_95k47346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53797)
store volatile %struct.ScmObj* %_95k47346, %struct.ScmObj** %stackaddr$prim54617, align 8
%stackaddr$prim54618 = alloca %struct.ScmObj*, align 8
%current_45args53798 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53797)
store volatile %struct.ScmObj* %current_45args53798, %struct.ScmObj** %stackaddr$prim54618, align 8
%stackaddr$prim54619 = alloca %struct.ScmObj*, align 8
%_37foldl47171 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53798)
store volatile %struct.ScmObj* %_37foldl47171, %struct.ScmObj** %stackaddr$prim54619, align 8
%stackaddr$makeclosure54620 = alloca %struct.ScmObj*, align 8
%fptrToInt54621 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49236 to i64
%ae49236 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54621)
store volatile %struct.ScmObj* %ae49236, %struct.ScmObj** %stackaddr$makeclosure54620, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49236, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49236, %struct.ScmObj* %_37map147120, i64 1)
%ae49237 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54622 = alloca %struct.ScmObj*, align 8
%fptrToInt54623 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49238 to i64
%ae49238 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54623)
store volatile %struct.ScmObj* %ae49238, %struct.ScmObj** %stackaddr$makeclosure54622, align 8
%argslist54058$ae492360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54624 = alloca %struct.ScmObj*, align 8
%argslist54058$ae492361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49238, %struct.ScmObj* %argslist54058$ae492360)
store volatile %struct.ScmObj* %argslist54058$ae492361, %struct.ScmObj** %stackaddr$prim54624, align 8
%stackaddr$prim54625 = alloca %struct.ScmObj*, align 8
%argslist54058$ae492362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49237, %struct.ScmObj* %argslist54058$ae492361)
store volatile %struct.ScmObj* %argslist54058$ae492362, %struct.ScmObj** %stackaddr$prim54625, align 8
%clofunc54626 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49236)
musttail call tailcc void %clofunc54626(%struct.ScmObj* %ae49236, %struct.ScmObj* %argslist54058$ae492362)
ret void
}

define tailcc void @proc_clo$ae49236(%struct.ScmObj* %env$ae49236,%struct.ScmObj* %current_45args53800) {
%stackaddr$env-ref54627 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49236, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54627
%stackaddr$env-ref54628 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49236, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54628
%stackaddr$prim54629 = alloca %struct.ScmObj*, align 8
%_95k47347 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53800)
store volatile %struct.ScmObj* %_95k47347, %struct.ScmObj** %stackaddr$prim54629, align 8
%stackaddr$prim54630 = alloca %struct.ScmObj*, align 8
%current_45args53801 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53800)
store volatile %struct.ScmObj* %current_45args53801, %struct.ScmObj** %stackaddr$prim54630, align 8
%stackaddr$prim54631 = alloca %struct.ScmObj*, align 8
%_37_6247168 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53801)
store volatile %struct.ScmObj* %_37_6247168, %struct.ScmObj** %stackaddr$prim54631, align 8
%stackaddr$makeclosure54632 = alloca %struct.ScmObj*, align 8
%fptrToInt54633 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49260 to i64
%ae49260 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54633)
store volatile %struct.ScmObj* %ae49260, %struct.ScmObj** %stackaddr$makeclosure54632, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49260, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49260, %struct.ScmObj* %_37map147120, i64 1)
%ae49261 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54634 = alloca %struct.ScmObj*, align 8
%fptrToInt54635 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49262 to i64
%ae49262 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54635)
store volatile %struct.ScmObj* %ae49262, %struct.ScmObj** %stackaddr$makeclosure54634, align 8
%argslist54052$ae492600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54636 = alloca %struct.ScmObj*, align 8
%argslist54052$ae492601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49262, %struct.ScmObj* %argslist54052$ae492600)
store volatile %struct.ScmObj* %argslist54052$ae492601, %struct.ScmObj** %stackaddr$prim54636, align 8
%stackaddr$prim54637 = alloca %struct.ScmObj*, align 8
%argslist54052$ae492602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49261, %struct.ScmObj* %argslist54052$ae492601)
store volatile %struct.ScmObj* %argslist54052$ae492602, %struct.ScmObj** %stackaddr$prim54637, align 8
%clofunc54638 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49260)
musttail call tailcc void %clofunc54638(%struct.ScmObj* %ae49260, %struct.ScmObj* %argslist54052$ae492602)
ret void
}

define tailcc void @proc_clo$ae49260(%struct.ScmObj* %env$ae49260,%struct.ScmObj* %current_45args53803) {
%stackaddr$env-ref54639 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49260, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54639
%stackaddr$env-ref54640 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49260, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54640
%stackaddr$prim54641 = alloca %struct.ScmObj*, align 8
%_95k47348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53803)
store volatile %struct.ScmObj* %_95k47348, %struct.ScmObj** %stackaddr$prim54641, align 8
%stackaddr$prim54642 = alloca %struct.ScmObj*, align 8
%current_45args53804 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53803)
store volatile %struct.ScmObj* %current_45args53804, %struct.ScmObj** %stackaddr$prim54642, align 8
%stackaddr$prim54643 = alloca %struct.ScmObj*, align 8
%_37_62_6147165 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53804)
store volatile %struct.ScmObj* %_37_62_6147165, %struct.ScmObj** %stackaddr$prim54643, align 8
%ae49284 = call %struct.ScmObj* @const_init_int(i64 1)
%ae49285 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54644 = alloca %struct.ScmObj*, align 8
%_37append47161 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49284, %struct.ScmObj* %ae49285)
store volatile %struct.ScmObj* %_37append47161, %struct.ScmObj** %stackaddr$prim54644, align 8
%stackaddr$makeclosure54645 = alloca %struct.ScmObj*, align 8
%fptrToInt54646 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49286 to i64
%ae49286 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54646)
store volatile %struct.ScmObj* %ae49286, %struct.ScmObj** %stackaddr$makeclosure54645, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49286, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49286, %struct.ScmObj* %_37map147120, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49286, %struct.ScmObj* %_37append47161, i64 2)
%ae49287 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54647 = alloca %struct.ScmObj*, align 8
%fptrToInt54648 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49288 to i64
%ae49288 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54648)
store volatile %struct.ScmObj* %ae49288, %struct.ScmObj** %stackaddr$makeclosure54647, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49288, %struct.ScmObj* %_37append47161, i64 0)
%argslist54046$ae492860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54649 = alloca %struct.ScmObj*, align 8
%argslist54046$ae492861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49288, %struct.ScmObj* %argslist54046$ae492860)
store volatile %struct.ScmObj* %argslist54046$ae492861, %struct.ScmObj** %stackaddr$prim54649, align 8
%stackaddr$prim54650 = alloca %struct.ScmObj*, align 8
%argslist54046$ae492862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49287, %struct.ScmObj* %argslist54046$ae492861)
store volatile %struct.ScmObj* %argslist54046$ae492862, %struct.ScmObj** %stackaddr$prim54650, align 8
%clofunc54651 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49286)
musttail call tailcc void %clofunc54651(%struct.ScmObj* %ae49286, %struct.ScmObj* %argslist54046$ae492862)
ret void
}

define tailcc void @proc_clo$ae49286(%struct.ScmObj* %env$ae49286,%struct.ScmObj* %current_45args53806) {
%stackaddr$env-ref54652 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49286, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54652
%stackaddr$env-ref54653 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49286, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54653
%stackaddr$env-ref54654 = alloca %struct.ScmObj*, align 8
%_37append47161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49286, i64 2)
store %struct.ScmObj* %_37append47161, %struct.ScmObj** %stackaddr$env-ref54654
%stackaddr$prim54655 = alloca %struct.ScmObj*, align 8
%_95k47349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53806)
store volatile %struct.ScmObj* %_95k47349, %struct.ScmObj** %stackaddr$prim54655, align 8
%stackaddr$prim54656 = alloca %struct.ScmObj*, align 8
%current_45args53807 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53806)
store volatile %struct.ScmObj* %current_45args53807, %struct.ScmObj** %stackaddr$prim54656, align 8
%stackaddr$prim54657 = alloca %struct.ScmObj*, align 8
%anf_45bind47271 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53807)
store volatile %struct.ScmObj* %anf_45bind47271, %struct.ScmObj** %stackaddr$prim54657, align 8
%ae49354 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54658 = alloca %struct.ScmObj*, align 8
%_95047162 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append47161, %struct.ScmObj* %ae49354, %struct.ScmObj* %anf_45bind47271)
store volatile %struct.ScmObj* %_95047162, %struct.ScmObj** %stackaddr$prim54658, align 8
%ae49357 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54659 = alloca %struct.ScmObj*, align 8
%_37append47160 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47161, %struct.ScmObj* %ae49357)
store volatile %struct.ScmObj* %_37append47160, %struct.ScmObj** %stackaddr$prim54659, align 8
%stackaddr$makeclosure54660 = alloca %struct.ScmObj*, align 8
%fptrToInt54661 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49358 to i64
%ae49358 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54661)
store volatile %struct.ScmObj* %ae49358, %struct.ScmObj** %stackaddr$makeclosure54660, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49358, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49358, %struct.ScmObj* %_37map147120, i64 1)
%ae49359 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54662 = alloca %struct.ScmObj*, align 8
%fptrToInt54663 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49360 to i64
%ae49360 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54663)
store volatile %struct.ScmObj* %ae49360, %struct.ScmObj** %stackaddr$makeclosure54662, align 8
%argslist54035$ae493580 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54664 = alloca %struct.ScmObj*, align 8
%argslist54035$ae493581 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49360, %struct.ScmObj* %argslist54035$ae493580)
store volatile %struct.ScmObj* %argslist54035$ae493581, %struct.ScmObj** %stackaddr$prim54664, align 8
%stackaddr$prim54665 = alloca %struct.ScmObj*, align 8
%argslist54035$ae493582 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49359, %struct.ScmObj* %argslist54035$ae493581)
store volatile %struct.ScmObj* %argslist54035$ae493582, %struct.ScmObj** %stackaddr$prim54665, align 8
%clofunc54666 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49358)
musttail call tailcc void %clofunc54666(%struct.ScmObj* %ae49358, %struct.ScmObj* %argslist54035$ae493582)
ret void
}

define tailcc void @proc_clo$ae49358(%struct.ScmObj* %env$ae49358,%struct.ScmObj* %current_45args53809) {
%stackaddr$env-ref54667 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49358, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54667
%stackaddr$env-ref54668 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49358, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54668
%stackaddr$prim54669 = alloca %struct.ScmObj*, align 8
%_95k47350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53809)
store volatile %struct.ScmObj* %_95k47350, %struct.ScmObj** %stackaddr$prim54669, align 8
%stackaddr$prim54670 = alloca %struct.ScmObj*, align 8
%current_45args53810 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53809)
store volatile %struct.ScmObj* %current_45args53810, %struct.ScmObj** %stackaddr$prim54670, align 8
%stackaddr$prim54671 = alloca %struct.ScmObj*, align 8
%_37list_6347153 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53810)
store volatile %struct.ScmObj* %_37list_6347153, %struct.ScmObj** %stackaddr$prim54671, align 8
%stackaddr$makeclosure54672 = alloca %struct.ScmObj*, align 8
%fptrToInt54673 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49774 to i64
%ae49774 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54673)
store volatile %struct.ScmObj* %ae49774, %struct.ScmObj** %stackaddr$makeclosure54672, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49774, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49774, %struct.ScmObj* %_37map147120, i64 1)
%ae49775 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54674 = alloca %struct.ScmObj*, align 8
%fptrToInt54675 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49776 to i64
%ae49776 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54675)
store volatile %struct.ScmObj* %ae49776, %struct.ScmObj** %stackaddr$makeclosure54674, align 8
%argslist54010$ae497740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54676 = alloca %struct.ScmObj*, align 8
%argslist54010$ae497741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49776, %struct.ScmObj* %argslist54010$ae497740)
store volatile %struct.ScmObj* %argslist54010$ae497741, %struct.ScmObj** %stackaddr$prim54676, align 8
%stackaddr$prim54677 = alloca %struct.ScmObj*, align 8
%argslist54010$ae497742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49775, %struct.ScmObj* %argslist54010$ae497741)
store volatile %struct.ScmObj* %argslist54010$ae497742, %struct.ScmObj** %stackaddr$prim54677, align 8
%clofunc54678 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49774)
musttail call tailcc void %clofunc54678(%struct.ScmObj* %ae49774, %struct.ScmObj* %argslist54010$ae497742)
ret void
}

define tailcc void @proc_clo$ae49774(%struct.ScmObj* %env$ae49774,%struct.ScmObj* %current_45args53812) {
%stackaddr$env-ref54679 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49774, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54679
%stackaddr$env-ref54680 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49774, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54680
%stackaddr$prim54681 = alloca %struct.ScmObj*, align 8
%_95k47351 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53812)
store volatile %struct.ScmObj* %_95k47351, %struct.ScmObj** %stackaddr$prim54681, align 8
%stackaddr$prim54682 = alloca %struct.ScmObj*, align 8
%current_45args53813 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53812)
store volatile %struct.ScmObj* %current_45args53813, %struct.ScmObj** %stackaddr$prim54682, align 8
%stackaddr$prim54683 = alloca %struct.ScmObj*, align 8
%_37drop47144 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53813)
store volatile %struct.ScmObj* %_37drop47144, %struct.ScmObj** %stackaddr$prim54683, align 8
%stackaddr$makeclosure54684 = alloca %struct.ScmObj*, align 8
%fptrToInt54685 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50310 to i64
%ae50310 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54685)
store volatile %struct.ScmObj* %ae50310, %struct.ScmObj** %stackaddr$makeclosure54684, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50310, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50310, %struct.ScmObj* %_37map147120, i64 1)
%ae50311 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54686 = alloca %struct.ScmObj*, align 8
%fptrToInt54687 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50312 to i64
%ae50312 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54687)
store volatile %struct.ScmObj* %ae50312, %struct.ScmObj** %stackaddr$makeclosure54686, align 8
%argslist53986$ae503100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54688 = alloca %struct.ScmObj*, align 8
%argslist53986$ae503101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50312, %struct.ScmObj* %argslist53986$ae503100)
store volatile %struct.ScmObj* %argslist53986$ae503101, %struct.ScmObj** %stackaddr$prim54688, align 8
%stackaddr$prim54689 = alloca %struct.ScmObj*, align 8
%argslist53986$ae503102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50311, %struct.ScmObj* %argslist53986$ae503101)
store volatile %struct.ScmObj* %argslist53986$ae503102, %struct.ScmObj** %stackaddr$prim54689, align 8
%clofunc54690 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50310)
musttail call tailcc void %clofunc54690(%struct.ScmObj* %ae50310, %struct.ScmObj* %argslist53986$ae503102)
ret void
}

define tailcc void @proc_clo$ae50310(%struct.ScmObj* %env$ae50310,%struct.ScmObj* %current_45args53815) {
%stackaddr$env-ref54691 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50310, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54691
%stackaddr$env-ref54692 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50310, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54692
%stackaddr$prim54693 = alloca %struct.ScmObj*, align 8
%_95k47352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53815)
store volatile %struct.ScmObj* %_95k47352, %struct.ScmObj** %stackaddr$prim54693, align 8
%stackaddr$prim54694 = alloca %struct.ScmObj*, align 8
%current_45args53816 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53815)
store volatile %struct.ScmObj* %current_45args53816, %struct.ScmObj** %stackaddr$prim54694, align 8
%stackaddr$prim54695 = alloca %struct.ScmObj*, align 8
%_37memv47137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53816)
store volatile %struct.ScmObj* %_37memv47137, %struct.ScmObj** %stackaddr$prim54695, align 8
%stackaddr$makeclosure54696 = alloca %struct.ScmObj*, align 8
%fptrToInt54697 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50714 to i64
%ae50714 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54697)
store volatile %struct.ScmObj* %ae50714, %struct.ScmObj** %stackaddr$makeclosure54696, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50714, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50714, %struct.ScmObj* %_37map147120, i64 1)
%ae50715 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54698 = alloca %struct.ScmObj*, align 8
%fptrToInt54699 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50716 to i64
%ae50716 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54699)
store volatile %struct.ScmObj* %ae50716, %struct.ScmObj** %stackaddr$makeclosure54698, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50716, %struct.ScmObj* %_37foldl147073, i64 0)
%argslist53960$ae507140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54700 = alloca %struct.ScmObj*, align 8
%argslist53960$ae507141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50716, %struct.ScmObj* %argslist53960$ae507140)
store volatile %struct.ScmObj* %argslist53960$ae507141, %struct.ScmObj** %stackaddr$prim54700, align 8
%stackaddr$prim54701 = alloca %struct.ScmObj*, align 8
%argslist53960$ae507142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50715, %struct.ScmObj* %argslist53960$ae507141)
store volatile %struct.ScmObj* %argslist53960$ae507142, %struct.ScmObj** %stackaddr$prim54701, align 8
%clofunc54702 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50714)
musttail call tailcc void %clofunc54702(%struct.ScmObj* %ae50714, %struct.ScmObj* %argslist53960$ae507142)
ret void
}

define tailcc void @proc_clo$ae50714(%struct.ScmObj* %env$ae50714,%struct.ScmObj* %current_45args53818) {
%stackaddr$env-ref54703 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50714, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54703
%stackaddr$env-ref54704 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50714, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54704
%stackaddr$prim54705 = alloca %struct.ScmObj*, align 8
%_95k47353 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53818)
store volatile %struct.ScmObj* %_95k47353, %struct.ScmObj** %stackaddr$prim54705, align 8
%stackaddr$prim54706 = alloca %struct.ScmObj*, align 8
%current_45args53819 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53818)
store volatile %struct.ScmObj* %current_45args53819, %struct.ScmObj** %stackaddr$prim54706, align 8
%stackaddr$prim54707 = alloca %struct.ScmObj*, align 8
%_37_4747133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53819)
store volatile %struct.ScmObj* %_37_4747133, %struct.ScmObj** %stackaddr$prim54707, align 8
%stackaddr$makeclosure54708 = alloca %struct.ScmObj*, align 8
%fptrToInt54709 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50812 to i64
%ae50812 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54709)
store volatile %struct.ScmObj* %ae50812, %struct.ScmObj** %stackaddr$makeclosure54708, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50812, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50812, %struct.ScmObj* %_37map147120, i64 1)
%ae50813 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54710 = alloca %struct.ScmObj*, align 8
%fptrToInt54711 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50814 to i64
%ae50814 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54711)
store volatile %struct.ScmObj* %ae50814, %struct.ScmObj** %stackaddr$makeclosure54710, align 8
%argslist53947$ae508120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54712 = alloca %struct.ScmObj*, align 8
%argslist53947$ae508121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50814, %struct.ScmObj* %argslist53947$ae508120)
store volatile %struct.ScmObj* %argslist53947$ae508121, %struct.ScmObj** %stackaddr$prim54712, align 8
%stackaddr$prim54713 = alloca %struct.ScmObj*, align 8
%argslist53947$ae508122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50813, %struct.ScmObj* %argslist53947$ae508121)
store volatile %struct.ScmObj* %argslist53947$ae508122, %struct.ScmObj** %stackaddr$prim54713, align 8
%clofunc54714 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50812)
musttail call tailcc void %clofunc54714(%struct.ScmObj* %ae50812, %struct.ScmObj* %argslist53947$ae508122)
ret void
}

define tailcc void @proc_clo$ae50812(%struct.ScmObj* %env$ae50812,%struct.ScmObj* %current_45args53821) {
%stackaddr$env-ref54715 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50812, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54715
%stackaddr$env-ref54716 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50812, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54716
%stackaddr$prim54717 = alloca %struct.ScmObj*, align 8
%_95k47354 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53821)
store volatile %struct.ScmObj* %_95k47354, %struct.ScmObj** %stackaddr$prim54717, align 8
%stackaddr$prim54718 = alloca %struct.ScmObj*, align 8
%current_45args53822 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53821)
store volatile %struct.ScmObj* %current_45args53822, %struct.ScmObj** %stackaddr$prim54718, align 8
%stackaddr$prim54719 = alloca %struct.ScmObj*, align 8
%_37first47131 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53822)
store volatile %struct.ScmObj* %_37first47131, %struct.ScmObj** %stackaddr$prim54719, align 8
%stackaddr$makeclosure54720 = alloca %struct.ScmObj*, align 8
%fptrToInt54721 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50832 to i64
%ae50832 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54721)
store volatile %struct.ScmObj* %ae50832, %struct.ScmObj** %stackaddr$makeclosure54720, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50832, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50832, %struct.ScmObj* %_37map147120, i64 1)
%ae50833 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54722 = alloca %struct.ScmObj*, align 8
%fptrToInt54723 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50834 to i64
%ae50834 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54723)
store volatile %struct.ScmObj* %ae50834, %struct.ScmObj** %stackaddr$makeclosure54722, align 8
%argslist53942$ae508320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54724 = alloca %struct.ScmObj*, align 8
%argslist53942$ae508321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50834, %struct.ScmObj* %argslist53942$ae508320)
store volatile %struct.ScmObj* %argslist53942$ae508321, %struct.ScmObj** %stackaddr$prim54724, align 8
%stackaddr$prim54725 = alloca %struct.ScmObj*, align 8
%argslist53942$ae508322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50833, %struct.ScmObj* %argslist53942$ae508321)
store volatile %struct.ScmObj* %argslist53942$ae508322, %struct.ScmObj** %stackaddr$prim54725, align 8
%clofunc54726 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50832)
musttail call tailcc void %clofunc54726(%struct.ScmObj* %ae50832, %struct.ScmObj* %argslist53942$ae508322)
ret void
}

define tailcc void @proc_clo$ae50832(%struct.ScmObj* %env$ae50832,%struct.ScmObj* %current_45args53824) {
%stackaddr$env-ref54727 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50832, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54727
%stackaddr$env-ref54728 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50832, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54728
%stackaddr$prim54729 = alloca %struct.ScmObj*, align 8
%_95k47355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53824)
store volatile %struct.ScmObj* %_95k47355, %struct.ScmObj** %stackaddr$prim54729, align 8
%stackaddr$prim54730 = alloca %struct.ScmObj*, align 8
%current_45args53825 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53824)
store volatile %struct.ScmObj* %current_45args53825, %struct.ScmObj** %stackaddr$prim54730, align 8
%stackaddr$prim54731 = alloca %struct.ScmObj*, align 8
%_37second47129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53825)
store volatile %struct.ScmObj* %_37second47129, %struct.ScmObj** %stackaddr$prim54731, align 8
%stackaddr$makeclosure54732 = alloca %struct.ScmObj*, align 8
%fptrToInt54733 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50854 to i64
%ae50854 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54733)
store volatile %struct.ScmObj* %ae50854, %struct.ScmObj** %stackaddr$makeclosure54732, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50854, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50854, %struct.ScmObj* %_37map147120, i64 1)
%ae50855 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54734 = alloca %struct.ScmObj*, align 8
%fptrToInt54735 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50856 to i64
%ae50856 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54735)
store volatile %struct.ScmObj* %ae50856, %struct.ScmObj** %stackaddr$makeclosure54734, align 8
%argslist53937$ae508540 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54736 = alloca %struct.ScmObj*, align 8
%argslist53937$ae508541 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50856, %struct.ScmObj* %argslist53937$ae508540)
store volatile %struct.ScmObj* %argslist53937$ae508541, %struct.ScmObj** %stackaddr$prim54736, align 8
%stackaddr$prim54737 = alloca %struct.ScmObj*, align 8
%argslist53937$ae508542 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50855, %struct.ScmObj* %argslist53937$ae508541)
store volatile %struct.ScmObj* %argslist53937$ae508542, %struct.ScmObj** %stackaddr$prim54737, align 8
%clofunc54738 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50854)
musttail call tailcc void %clofunc54738(%struct.ScmObj* %ae50854, %struct.ScmObj* %argslist53937$ae508542)
ret void
}

define tailcc void @proc_clo$ae50854(%struct.ScmObj* %env$ae50854,%struct.ScmObj* %current_45args53827) {
%stackaddr$env-ref54739 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50854, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54739
%stackaddr$env-ref54740 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50854, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54740
%stackaddr$prim54741 = alloca %struct.ScmObj*, align 8
%_95k47356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53827)
store volatile %struct.ScmObj* %_95k47356, %struct.ScmObj** %stackaddr$prim54741, align 8
%stackaddr$prim54742 = alloca %struct.ScmObj*, align 8
%current_45args53828 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53827)
store volatile %struct.ScmObj* %current_45args53828, %struct.ScmObj** %stackaddr$prim54742, align 8
%stackaddr$prim54743 = alloca %struct.ScmObj*, align 8
%_37third47127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53828)
store volatile %struct.ScmObj* %_37third47127, %struct.ScmObj** %stackaddr$prim54743, align 8
%stackaddr$makeclosure54744 = alloca %struct.ScmObj*, align 8
%fptrToInt54745 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50878 to i64
%ae50878 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54745)
store volatile %struct.ScmObj* %ae50878, %struct.ScmObj** %stackaddr$makeclosure54744, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50878, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50878, %struct.ScmObj* %_37map147120, i64 1)
%ae50879 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54746 = alloca %struct.ScmObj*, align 8
%fptrToInt54747 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50880 to i64
%ae50880 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54747)
store volatile %struct.ScmObj* %ae50880, %struct.ScmObj** %stackaddr$makeclosure54746, align 8
%argslist53932$ae508780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54748 = alloca %struct.ScmObj*, align 8
%argslist53932$ae508781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50880, %struct.ScmObj* %argslist53932$ae508780)
store volatile %struct.ScmObj* %argslist53932$ae508781, %struct.ScmObj** %stackaddr$prim54748, align 8
%stackaddr$prim54749 = alloca %struct.ScmObj*, align 8
%argslist53932$ae508782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50879, %struct.ScmObj* %argslist53932$ae508781)
store volatile %struct.ScmObj* %argslist53932$ae508782, %struct.ScmObj** %stackaddr$prim54749, align 8
%clofunc54750 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50878)
musttail call tailcc void %clofunc54750(%struct.ScmObj* %ae50878, %struct.ScmObj* %argslist53932$ae508782)
ret void
}

define tailcc void @proc_clo$ae50878(%struct.ScmObj* %env$ae50878,%struct.ScmObj* %current_45args53830) {
%stackaddr$env-ref54751 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50878, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54751
%stackaddr$env-ref54752 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50878, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54752
%stackaddr$prim54753 = alloca %struct.ScmObj*, align 8
%_95k47357 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53830)
store volatile %struct.ScmObj* %_95k47357, %struct.ScmObj** %stackaddr$prim54753, align 8
%stackaddr$prim54754 = alloca %struct.ScmObj*, align 8
%current_45args53831 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53830)
store volatile %struct.ScmObj* %current_45args53831, %struct.ScmObj** %stackaddr$prim54754, align 8
%stackaddr$prim54755 = alloca %struct.ScmObj*, align 8
%_37fourth47125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53831)
store volatile %struct.ScmObj* %_37fourth47125, %struct.ScmObj** %stackaddr$prim54755, align 8
%stackaddr$makeclosure54756 = alloca %struct.ScmObj*, align 8
%fptrToInt54757 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50904 to i64
%ae50904 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54757)
store volatile %struct.ScmObj* %ae50904, %struct.ScmObj** %stackaddr$makeclosure54756, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50904, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50904, %struct.ScmObj* %_37map147120, i64 1)
%ae50905 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54758 = alloca %struct.ScmObj*, align 8
%fptrToInt54759 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50906 to i64
%ae50906 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54759)
store volatile %struct.ScmObj* %ae50906, %struct.ScmObj** %stackaddr$makeclosure54758, align 8
%argslist53927$ae509040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54760 = alloca %struct.ScmObj*, align 8
%argslist53927$ae509041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50906, %struct.ScmObj* %argslist53927$ae509040)
store volatile %struct.ScmObj* %argslist53927$ae509041, %struct.ScmObj** %stackaddr$prim54760, align 8
%stackaddr$prim54761 = alloca %struct.ScmObj*, align 8
%argslist53927$ae509042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50905, %struct.ScmObj* %argslist53927$ae509041)
store volatile %struct.ScmObj* %argslist53927$ae509042, %struct.ScmObj** %stackaddr$prim54761, align 8
%clofunc54762 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50904)
musttail call tailcc void %clofunc54762(%struct.ScmObj* %ae50904, %struct.ScmObj* %argslist53927$ae509042)
ret void
}

define tailcc void @proc_clo$ae50904(%struct.ScmObj* %env$ae50904,%struct.ScmObj* %current_45args53833) {
%stackaddr$env-ref54763 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50904, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54763
%stackaddr$env-ref54764 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50904, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54764
%stackaddr$prim54765 = alloca %struct.ScmObj*, align 8
%_95k47358 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53833)
store volatile %struct.ScmObj* %_95k47358, %struct.ScmObj** %stackaddr$prim54765, align 8
%stackaddr$prim54766 = alloca %struct.ScmObj*, align 8
%current_45args53834 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53833)
store volatile %struct.ScmObj* %current_45args53834, %struct.ScmObj** %stackaddr$prim54766, align 8
%stackaddr$prim54767 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53834)
store volatile %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$prim54767, align 8
%stackaddr$makeclosure54768 = alloca %struct.ScmObj*, align 8
%fptrToInt54769 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50928 to i64
%ae50928 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54769)
store volatile %struct.ScmObj* %ae50928, %struct.ScmObj** %stackaddr$makeclosure54768, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50928, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50928, %struct.ScmObj* %_37map147120, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50928, %struct.ScmObj* %anf_45bind47307, i64 2)
%ae50929 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54770 = alloca %struct.ScmObj*, align 8
%fptrToInt54771 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50930 to i64
%ae50930 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54771)
store volatile %struct.ScmObj* %ae50930, %struct.ScmObj** %stackaddr$makeclosure54770, align 8
%argslist53925$ae509280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54772 = alloca %struct.ScmObj*, align 8
%argslist53925$ae509281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50930, %struct.ScmObj* %argslist53925$ae509280)
store volatile %struct.ScmObj* %argslist53925$ae509281, %struct.ScmObj** %stackaddr$prim54772, align 8
%stackaddr$prim54773 = alloca %struct.ScmObj*, align 8
%argslist53925$ae509282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50929, %struct.ScmObj* %argslist53925$ae509281)
store volatile %struct.ScmObj* %argslist53925$ae509282, %struct.ScmObj** %stackaddr$prim54773, align 8
%clofunc54774 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50928)
musttail call tailcc void %clofunc54774(%struct.ScmObj* %ae50928, %struct.ScmObj* %argslist53925$ae509282)
ret void
}

define tailcc void @proc_clo$ae50928(%struct.ScmObj* %env$ae50928,%struct.ScmObj* %current_45args53836) {
%stackaddr$env-ref54775 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50928, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54775
%stackaddr$env-ref54776 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50928, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54776
%stackaddr$env-ref54777 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50928, i64 2)
store %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$env-ref54777
%stackaddr$prim54778 = alloca %struct.ScmObj*, align 8
%_95k47359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53836)
store volatile %struct.ScmObj* %_95k47359, %struct.ScmObj** %stackaddr$prim54778, align 8
%stackaddr$prim54779 = alloca %struct.ScmObj*, align 8
%current_45args53837 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53836)
store volatile %struct.ScmObj* %current_45args53837, %struct.ScmObj** %stackaddr$prim54779, align 8
%stackaddr$prim54780 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53837)
store volatile %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$prim54780, align 8
%stackaddr$makeclosure54781 = alloca %struct.ScmObj*, align 8
%fptrToInt54782 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50963 to i64
%ae50963 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54782)
store volatile %struct.ScmObj* %ae50963, %struct.ScmObj** %stackaddr$makeclosure54781, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50963, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50963, %struct.ScmObj* %_37map147120, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50963, %struct.ScmObj* %anf_45bind47308, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50963, %struct.ScmObj* %anf_45bind47307, i64 3)
%ae50964 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54783 = alloca %struct.ScmObj*, align 8
%fptrToInt54784 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50965 to i64
%ae50965 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54784)
store volatile %struct.ScmObj* %ae50965, %struct.ScmObj** %stackaddr$makeclosure54783, align 8
%argslist53919$ae509630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54785 = alloca %struct.ScmObj*, align 8
%argslist53919$ae509631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50965, %struct.ScmObj* %argslist53919$ae509630)
store volatile %struct.ScmObj* %argslist53919$ae509631, %struct.ScmObj** %stackaddr$prim54785, align 8
%stackaddr$prim54786 = alloca %struct.ScmObj*, align 8
%argslist53919$ae509632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50964, %struct.ScmObj* %argslist53919$ae509631)
store volatile %struct.ScmObj* %argslist53919$ae509632, %struct.ScmObj** %stackaddr$prim54786, align 8
%clofunc54787 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50963)
musttail call tailcc void %clofunc54787(%struct.ScmObj* %ae50963, %struct.ScmObj* %argslist53919$ae509632)
ret void
}

define tailcc void @proc_clo$ae50963(%struct.ScmObj* %env$ae50963,%struct.ScmObj* %current_45args53839) {
%stackaddr$env-ref54788 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50963, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54788
%stackaddr$env-ref54789 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50963, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54789
%stackaddr$env-ref54790 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50963, i64 2)
store %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$env-ref54790
%stackaddr$env-ref54791 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50963, i64 3)
store %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$env-ref54791
%stackaddr$prim54792 = alloca %struct.ScmObj*, align 8
%_95k47360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53839)
store volatile %struct.ScmObj* %_95k47360, %struct.ScmObj** %stackaddr$prim54792, align 8
%stackaddr$prim54793 = alloca %struct.ScmObj*, align 8
%current_45args53840 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53839)
store volatile %struct.ScmObj* %current_45args53840, %struct.ScmObj** %stackaddr$prim54793, align 8
%stackaddr$prim54794 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53840)
store volatile %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$prim54794, align 8
%stackaddr$makeclosure54795 = alloca %struct.ScmObj*, align 8
%fptrToInt54796 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50987 to i64
%ae50987 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54796)
store volatile %struct.ScmObj* %ae50987, %struct.ScmObj** %stackaddr$makeclosure54795, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50987, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50987, %struct.ScmObj* %_37map147120, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50987, %struct.ScmObj* %anf_45bind47309, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae50987, %struct.ScmObj* %anf_45bind47308, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae50987, %struct.ScmObj* %anf_45bind47307, i64 4)
%ae50988 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54797 = alloca %struct.ScmObj*, align 8
%fptrToInt54798 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50989 to i64
%ae50989 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54798)
store volatile %struct.ScmObj* %ae50989, %struct.ScmObj** %stackaddr$makeclosure54797, align 8
%argslist53917$ae509870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54799 = alloca %struct.ScmObj*, align 8
%argslist53917$ae509871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50989, %struct.ScmObj* %argslist53917$ae509870)
store volatile %struct.ScmObj* %argslist53917$ae509871, %struct.ScmObj** %stackaddr$prim54799, align 8
%stackaddr$prim54800 = alloca %struct.ScmObj*, align 8
%argslist53917$ae509872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50988, %struct.ScmObj* %argslist53917$ae509871)
store volatile %struct.ScmObj* %argslist53917$ae509872, %struct.ScmObj** %stackaddr$prim54800, align 8
%clofunc54801 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50987)
musttail call tailcc void %clofunc54801(%struct.ScmObj* %ae50987, %struct.ScmObj* %argslist53917$ae509872)
ret void
}

define tailcc void @proc_clo$ae50987(%struct.ScmObj* %env$ae50987,%struct.ScmObj* %current_45args53842) {
%stackaddr$env-ref54802 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50987, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54802
%stackaddr$env-ref54803 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50987, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54803
%stackaddr$env-ref54804 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50987, i64 2)
store %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$env-ref54804
%stackaddr$env-ref54805 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50987, i64 3)
store %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$env-ref54805
%stackaddr$env-ref54806 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50987, i64 4)
store %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$env-ref54806
%stackaddr$prim54807 = alloca %struct.ScmObj*, align 8
%_95k47361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53842)
store volatile %struct.ScmObj* %_95k47361, %struct.ScmObj** %stackaddr$prim54807, align 8
%stackaddr$prim54808 = alloca %struct.ScmObj*, align 8
%current_45args53843 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53842)
store volatile %struct.ScmObj* %current_45args53843, %struct.ScmObj** %stackaddr$prim54808, align 8
%stackaddr$prim54809 = alloca %struct.ScmObj*, align 8
%anf_45bind47310 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53843)
store volatile %struct.ScmObj* %anf_45bind47310, %struct.ScmObj** %stackaddr$prim54809, align 8
%stackaddr$makeclosure54810 = alloca %struct.ScmObj*, align 8
%fptrToInt54811 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51009 to i64
%ae51009 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54811)
store volatile %struct.ScmObj* %ae51009, %struct.ScmObj** %stackaddr$makeclosure54810, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51009, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51009, %struct.ScmObj* %_37map147120, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51009, %struct.ScmObj* %anf_45bind47310, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51009, %struct.ScmObj* %anf_45bind47309, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51009, %struct.ScmObj* %anf_45bind47308, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51009, %struct.ScmObj* %anf_45bind47307, i64 5)
%ae51010 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54812 = alloca %struct.ScmObj*, align 8
%fptrToInt54813 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51011 to i64
%ae51011 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54813)
store volatile %struct.ScmObj* %ae51011, %struct.ScmObj** %stackaddr$makeclosure54812, align 8
%argslist53915$ae510090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54814 = alloca %struct.ScmObj*, align 8
%argslist53915$ae510091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51011, %struct.ScmObj* %argslist53915$ae510090)
store volatile %struct.ScmObj* %argslist53915$ae510091, %struct.ScmObj** %stackaddr$prim54814, align 8
%stackaddr$prim54815 = alloca %struct.ScmObj*, align 8
%argslist53915$ae510092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51010, %struct.ScmObj* %argslist53915$ae510091)
store volatile %struct.ScmObj* %argslist53915$ae510092, %struct.ScmObj** %stackaddr$prim54815, align 8
%clofunc54816 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51009)
musttail call tailcc void %clofunc54816(%struct.ScmObj* %ae51009, %struct.ScmObj* %argslist53915$ae510092)
ret void
}

define tailcc void @proc_clo$ae51009(%struct.ScmObj* %env$ae51009,%struct.ScmObj* %current_45args53845) {
%stackaddr$env-ref54817 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51009, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54817
%stackaddr$env-ref54818 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51009, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54818
%stackaddr$env-ref54819 = alloca %struct.ScmObj*, align 8
%anf_45bind47310 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51009, i64 2)
store %struct.ScmObj* %anf_45bind47310, %struct.ScmObj** %stackaddr$env-ref54819
%stackaddr$env-ref54820 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51009, i64 3)
store %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$env-ref54820
%stackaddr$env-ref54821 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51009, i64 4)
store %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$env-ref54821
%stackaddr$env-ref54822 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51009, i64 5)
store %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$env-ref54822
%stackaddr$prim54823 = alloca %struct.ScmObj*, align 8
%_95k47362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53845)
store volatile %struct.ScmObj* %_95k47362, %struct.ScmObj** %stackaddr$prim54823, align 8
%stackaddr$prim54824 = alloca %struct.ScmObj*, align 8
%current_45args53846 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53845)
store volatile %struct.ScmObj* %current_45args53846, %struct.ScmObj** %stackaddr$prim54824, align 8
%stackaddr$prim54825 = alloca %struct.ScmObj*, align 8
%anf_45bind47311 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53846)
store volatile %struct.ScmObj* %anf_45bind47311, %struct.ScmObj** %stackaddr$prim54825, align 8
%stackaddr$makeclosure54826 = alloca %struct.ScmObj*, align 8
%fptrToInt54827 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51032 to i64
%ae51032 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54827)
store volatile %struct.ScmObj* %ae51032, %struct.ScmObj** %stackaddr$makeclosure54826, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51032, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51032, %struct.ScmObj* %_37map147120, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51032, %struct.ScmObj* %anf_45bind47310, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51032, %struct.ScmObj* %anf_45bind47309, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51032, %struct.ScmObj* %anf_45bind47308, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51032, %struct.ScmObj* %anf_45bind47307, i64 5)
%argslist53913$anf_45bind473110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54828 = alloca %struct.ScmObj*, align 8
%argslist53913$anf_45bind473111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51032, %struct.ScmObj* %argslist53913$anf_45bind473110)
store volatile %struct.ScmObj* %argslist53913$anf_45bind473111, %struct.ScmObj** %stackaddr$prim54828, align 8
%clofunc54829 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47311)
musttail call tailcc void %clofunc54829(%struct.ScmObj* %anf_45bind47311, %struct.ScmObj* %argslist53913$anf_45bind473111)
ret void
}

define tailcc void @proc_clo$ae51032(%struct.ScmObj* %env$ae51032,%struct.ScmObj* %current_45args53848) {
%stackaddr$env-ref54830 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51032, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54830
%stackaddr$env-ref54831 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51032, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54831
%stackaddr$env-ref54832 = alloca %struct.ScmObj*, align 8
%anf_45bind47310 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51032, i64 2)
store %struct.ScmObj* %anf_45bind47310, %struct.ScmObj** %stackaddr$env-ref54832
%stackaddr$env-ref54833 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51032, i64 3)
store %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$env-ref54833
%stackaddr$env-ref54834 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51032, i64 4)
store %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$env-ref54834
%stackaddr$env-ref54835 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51032, i64 5)
store %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$env-ref54835
%stackaddr$prim54836 = alloca %struct.ScmObj*, align 8
%_95k47363 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53848)
store volatile %struct.ScmObj* %_95k47363, %struct.ScmObj** %stackaddr$prim54836, align 8
%stackaddr$prim54837 = alloca %struct.ScmObj*, align 8
%current_45args53849 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53848)
store volatile %struct.ScmObj* %current_45args53849, %struct.ScmObj** %stackaddr$prim54837, align 8
%stackaddr$prim54838 = alloca %struct.ScmObj*, align 8
%anf_45bind47312 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53849)
store volatile %struct.ScmObj* %anf_45bind47312, %struct.ScmObj** %stackaddr$prim54838, align 8
%stackaddr$makeclosure54839 = alloca %struct.ScmObj*, align 8
%fptrToInt54840 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51033 to i64
%ae51033 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54840)
store volatile %struct.ScmObj* %ae51033, %struct.ScmObj** %stackaddr$makeclosure54839, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51033, %struct.ScmObj* %_37map147120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51033, %struct.ScmObj* %anf_45bind47312, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51033, %struct.ScmObj* %_37foldl147073, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51033, %struct.ScmObj* %anf_45bind47310, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51033, %struct.ScmObj* %anf_45bind47309, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51033, %struct.ScmObj* %anf_45bind47308, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51033, %struct.ScmObj* %anf_45bind47307, i64 6)
%ae51034 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54841 = alloca %struct.ScmObj*, align 8
%fptrToInt54842 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51035 to i64
%ae51035 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54842)
store volatile %struct.ScmObj* %ae51035, %struct.ScmObj** %stackaddr$makeclosure54841, align 8
%argslist53912$ae510330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54843 = alloca %struct.ScmObj*, align 8
%argslist53912$ae510331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51035, %struct.ScmObj* %argslist53912$ae510330)
store volatile %struct.ScmObj* %argslist53912$ae510331, %struct.ScmObj** %stackaddr$prim54843, align 8
%stackaddr$prim54844 = alloca %struct.ScmObj*, align 8
%argslist53912$ae510332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51034, %struct.ScmObj* %argslist53912$ae510331)
store volatile %struct.ScmObj* %argslist53912$ae510332, %struct.ScmObj** %stackaddr$prim54844, align 8
%clofunc54845 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51033)
musttail call tailcc void %clofunc54845(%struct.ScmObj* %ae51033, %struct.ScmObj* %argslist53912$ae510332)
ret void
}

define tailcc void @proc_clo$ae51033(%struct.ScmObj* %env$ae51033,%struct.ScmObj* %current_45args53851) {
%stackaddr$env-ref54846 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51033, i64 0)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54846
%stackaddr$env-ref54847 = alloca %struct.ScmObj*, align 8
%anf_45bind47312 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51033, i64 1)
store %struct.ScmObj* %anf_45bind47312, %struct.ScmObj** %stackaddr$env-ref54847
%stackaddr$env-ref54848 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51033, i64 2)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54848
%stackaddr$env-ref54849 = alloca %struct.ScmObj*, align 8
%anf_45bind47310 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51033, i64 3)
store %struct.ScmObj* %anf_45bind47310, %struct.ScmObj** %stackaddr$env-ref54849
%stackaddr$env-ref54850 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51033, i64 4)
store %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$env-ref54850
%stackaddr$env-ref54851 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51033, i64 5)
store %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$env-ref54851
%stackaddr$env-ref54852 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51033, i64 6)
store %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$env-ref54852
%stackaddr$prim54853 = alloca %struct.ScmObj*, align 8
%_95k47364 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53851)
store volatile %struct.ScmObj* %_95k47364, %struct.ScmObj** %stackaddr$prim54853, align 8
%stackaddr$prim54854 = alloca %struct.ScmObj*, align 8
%current_45args53852 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53851)
store volatile %struct.ScmObj* %current_45args53852, %struct.ScmObj** %stackaddr$prim54854, align 8
%stackaddr$prim54855 = alloca %struct.ScmObj*, align 8
%anf_45bind47313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53852)
store volatile %struct.ScmObj* %anf_45bind47313, %struct.ScmObj** %stackaddr$prim54855, align 8
%stackaddr$makeclosure54856 = alloca %struct.ScmObj*, align 8
%fptrToInt54857 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51055 to i64
%ae51055 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt54857)
store volatile %struct.ScmObj* %ae51055, %struct.ScmObj** %stackaddr$makeclosure54856, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51055, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51055, %struct.ScmObj* %anf_45bind47313, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51055, %struct.ScmObj* %_37map147120, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51055, %struct.ScmObj* %anf_45bind47312, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51055, %struct.ScmObj* %anf_45bind47310, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51055, %struct.ScmObj* %anf_45bind47309, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51055, %struct.ScmObj* %anf_45bind47308, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae51055, %struct.ScmObj* %anf_45bind47307, i64 7)
%ae51056 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54858 = alloca %struct.ScmObj*, align 8
%fptrToInt54859 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51057 to i64
%ae51057 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54859)
store volatile %struct.ScmObj* %ae51057, %struct.ScmObj** %stackaddr$makeclosure54858, align 8
%argslist53910$ae510550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54860 = alloca %struct.ScmObj*, align 8
%argslist53910$ae510551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51057, %struct.ScmObj* %argslist53910$ae510550)
store volatile %struct.ScmObj* %argslist53910$ae510551, %struct.ScmObj** %stackaddr$prim54860, align 8
%stackaddr$prim54861 = alloca %struct.ScmObj*, align 8
%argslist53910$ae510552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51056, %struct.ScmObj* %argslist53910$ae510551)
store volatile %struct.ScmObj* %argslist53910$ae510552, %struct.ScmObj** %stackaddr$prim54861, align 8
%clofunc54862 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51055)
musttail call tailcc void %clofunc54862(%struct.ScmObj* %ae51055, %struct.ScmObj* %argslist53910$ae510552)
ret void
}

define tailcc void @proc_clo$ae51055(%struct.ScmObj* %env$ae51055,%struct.ScmObj* %current_45args53854) {
%stackaddr$env-ref54863 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51055, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54863
%stackaddr$env-ref54864 = alloca %struct.ScmObj*, align 8
%anf_45bind47313 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51055, i64 1)
store %struct.ScmObj* %anf_45bind47313, %struct.ScmObj** %stackaddr$env-ref54864
%stackaddr$env-ref54865 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51055, i64 2)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54865
%stackaddr$env-ref54866 = alloca %struct.ScmObj*, align 8
%anf_45bind47312 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51055, i64 3)
store %struct.ScmObj* %anf_45bind47312, %struct.ScmObj** %stackaddr$env-ref54866
%stackaddr$env-ref54867 = alloca %struct.ScmObj*, align 8
%anf_45bind47310 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51055, i64 4)
store %struct.ScmObj* %anf_45bind47310, %struct.ScmObj** %stackaddr$env-ref54867
%stackaddr$env-ref54868 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51055, i64 5)
store %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$env-ref54868
%stackaddr$env-ref54869 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51055, i64 6)
store %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$env-ref54869
%stackaddr$env-ref54870 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51055, i64 7)
store %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$env-ref54870
%stackaddr$prim54871 = alloca %struct.ScmObj*, align 8
%_95k47365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53854)
store volatile %struct.ScmObj* %_95k47365, %struct.ScmObj** %stackaddr$prim54871, align 8
%stackaddr$prim54872 = alloca %struct.ScmObj*, align 8
%current_45args53855 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53854)
store volatile %struct.ScmObj* %current_45args53855, %struct.ScmObj** %stackaddr$prim54872, align 8
%stackaddr$prim54873 = alloca %struct.ScmObj*, align 8
%anf_45bind47314 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53855)
store volatile %struct.ScmObj* %anf_45bind47314, %struct.ScmObj** %stackaddr$prim54873, align 8
%stackaddr$makeclosure54874 = alloca %struct.ScmObj*, align 8
%fptrToInt54875 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51078 to i64
%ae51078 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt54875)
store volatile %struct.ScmObj* %ae51078, %struct.ScmObj** %stackaddr$makeclosure54874, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51078, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51078, %struct.ScmObj* %anf_45bind47313, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51078, %struct.ScmObj* %_37map147120, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51078, %struct.ScmObj* %anf_45bind47312, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51078, %struct.ScmObj* %anf_45bind47310, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51078, %struct.ScmObj* %anf_45bind47309, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51078, %struct.ScmObj* %anf_45bind47308, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae51078, %struct.ScmObj* %anf_45bind47307, i64 7)
%argslist53908$anf_45bind473140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54876 = alloca %struct.ScmObj*, align 8
%argslist53908$anf_45bind473141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51078, %struct.ScmObj* %argslist53908$anf_45bind473140)
store volatile %struct.ScmObj* %argslist53908$anf_45bind473141, %struct.ScmObj** %stackaddr$prim54876, align 8
%clofunc54877 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47314)
musttail call tailcc void %clofunc54877(%struct.ScmObj* %anf_45bind47314, %struct.ScmObj* %argslist53908$anf_45bind473141)
ret void
}

define tailcc void @proc_clo$ae51078(%struct.ScmObj* %env$ae51078,%struct.ScmObj* %current_45args53857) {
%stackaddr$env-ref54878 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51078, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54878
%stackaddr$env-ref54879 = alloca %struct.ScmObj*, align 8
%anf_45bind47313 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51078, i64 1)
store %struct.ScmObj* %anf_45bind47313, %struct.ScmObj** %stackaddr$env-ref54879
%stackaddr$env-ref54880 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51078, i64 2)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54880
%stackaddr$env-ref54881 = alloca %struct.ScmObj*, align 8
%anf_45bind47312 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51078, i64 3)
store %struct.ScmObj* %anf_45bind47312, %struct.ScmObj** %stackaddr$env-ref54881
%stackaddr$env-ref54882 = alloca %struct.ScmObj*, align 8
%anf_45bind47310 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51078, i64 4)
store %struct.ScmObj* %anf_45bind47310, %struct.ScmObj** %stackaddr$env-ref54882
%stackaddr$env-ref54883 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51078, i64 5)
store %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$env-ref54883
%stackaddr$env-ref54884 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51078, i64 6)
store %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$env-ref54884
%stackaddr$env-ref54885 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51078, i64 7)
store %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$env-ref54885
%stackaddr$prim54886 = alloca %struct.ScmObj*, align 8
%_95k47366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53857)
store volatile %struct.ScmObj* %_95k47366, %struct.ScmObj** %stackaddr$prim54886, align 8
%stackaddr$prim54887 = alloca %struct.ScmObj*, align 8
%current_45args53858 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53857)
store volatile %struct.ScmObj* %current_45args53858, %struct.ScmObj** %stackaddr$prim54887, align 8
%stackaddr$prim54888 = alloca %struct.ScmObj*, align 8
%anf_45bind47315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53858)
store volatile %struct.ScmObj* %anf_45bind47315, %struct.ScmObj** %stackaddr$prim54888, align 8
%stackaddr$makeclosure54889 = alloca %struct.ScmObj*, align 8
%fptrToInt54890 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51079 to i64
%ae51079 = call %struct.ScmObj* @closure_alloc(i64 9, i64 %fptrToInt54890)
store volatile %struct.ScmObj* %ae51079, %struct.ScmObj** %stackaddr$makeclosure54889, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51079, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51079, %struct.ScmObj* %anf_45bind47313, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51079, %struct.ScmObj* %_37map147120, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51079, %struct.ScmObj* %anf_45bind47312, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51079, %struct.ScmObj* %anf_45bind47315, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51079, %struct.ScmObj* %anf_45bind47310, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51079, %struct.ScmObj* %anf_45bind47309, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae51079, %struct.ScmObj* %anf_45bind47308, i64 7)
call void @closure_place_freevar(%struct.ScmObj* %ae51079, %struct.ScmObj* %anf_45bind47307, i64 8)
%ae51080 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54891 = alloca %struct.ScmObj*, align 8
%fptrToInt54892 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51081 to i64
%ae51081 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54892)
store volatile %struct.ScmObj* %ae51081, %struct.ScmObj** %stackaddr$makeclosure54891, align 8
%argslist53907$ae510790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54893 = alloca %struct.ScmObj*, align 8
%argslist53907$ae510791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51081, %struct.ScmObj* %argslist53907$ae510790)
store volatile %struct.ScmObj* %argslist53907$ae510791, %struct.ScmObj** %stackaddr$prim54893, align 8
%stackaddr$prim54894 = alloca %struct.ScmObj*, align 8
%argslist53907$ae510792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51080, %struct.ScmObj* %argslist53907$ae510791)
store volatile %struct.ScmObj* %argslist53907$ae510792, %struct.ScmObj** %stackaddr$prim54894, align 8
%clofunc54895 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51079)
musttail call tailcc void %clofunc54895(%struct.ScmObj* %ae51079, %struct.ScmObj* %argslist53907$ae510792)
ret void
}

define tailcc void @proc_clo$ae51079(%struct.ScmObj* %env$ae51079,%struct.ScmObj* %current_45args53860) {
%stackaddr$env-ref54896 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51079, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54896
%stackaddr$env-ref54897 = alloca %struct.ScmObj*, align 8
%anf_45bind47313 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51079, i64 1)
store %struct.ScmObj* %anf_45bind47313, %struct.ScmObj** %stackaddr$env-ref54897
%stackaddr$env-ref54898 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51079, i64 2)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54898
%stackaddr$env-ref54899 = alloca %struct.ScmObj*, align 8
%anf_45bind47312 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51079, i64 3)
store %struct.ScmObj* %anf_45bind47312, %struct.ScmObj** %stackaddr$env-ref54899
%stackaddr$env-ref54900 = alloca %struct.ScmObj*, align 8
%anf_45bind47315 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51079, i64 4)
store %struct.ScmObj* %anf_45bind47315, %struct.ScmObj** %stackaddr$env-ref54900
%stackaddr$env-ref54901 = alloca %struct.ScmObj*, align 8
%anf_45bind47310 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51079, i64 5)
store %struct.ScmObj* %anf_45bind47310, %struct.ScmObj** %stackaddr$env-ref54901
%stackaddr$env-ref54902 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51079, i64 6)
store %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$env-ref54902
%stackaddr$env-ref54903 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51079, i64 7)
store %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$env-ref54903
%stackaddr$env-ref54904 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51079, i64 8)
store %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$env-ref54904
%stackaddr$prim54905 = alloca %struct.ScmObj*, align 8
%_95k47367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53860)
store volatile %struct.ScmObj* %_95k47367, %struct.ScmObj** %stackaddr$prim54905, align 8
%stackaddr$prim54906 = alloca %struct.ScmObj*, align 8
%current_45args53861 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53860)
store volatile %struct.ScmObj* %current_45args53861, %struct.ScmObj** %stackaddr$prim54906, align 8
%stackaddr$prim54907 = alloca %struct.ScmObj*, align 8
%anf_45bind47316 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53861)
store volatile %struct.ScmObj* %anf_45bind47316, %struct.ScmObj** %stackaddr$prim54907, align 8
%stackaddr$makeclosure54908 = alloca %struct.ScmObj*, align 8
%fptrToInt54909 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51102 to i64
%ae51102 = call %struct.ScmObj* @closure_alloc(i64 9, i64 %fptrToInt54909)
store volatile %struct.ScmObj* %ae51102, %struct.ScmObj** %stackaddr$makeclosure54908, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51102, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51102, %struct.ScmObj* %anf_45bind47313, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51102, %struct.ScmObj* %_37map147120, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51102, %struct.ScmObj* %anf_45bind47312, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51102, %struct.ScmObj* %anf_45bind47315, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51102, %struct.ScmObj* %anf_45bind47310, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51102, %struct.ScmObj* %anf_45bind47309, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae51102, %struct.ScmObj* %anf_45bind47308, i64 7)
call void @closure_place_freevar(%struct.ScmObj* %ae51102, %struct.ScmObj* %anf_45bind47307, i64 8)
%argslist53905$anf_45bind473160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54910 = alloca %struct.ScmObj*, align 8
%argslist53905$anf_45bind473161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51102, %struct.ScmObj* %argslist53905$anf_45bind473160)
store volatile %struct.ScmObj* %argslist53905$anf_45bind473161, %struct.ScmObj** %stackaddr$prim54910, align 8
%clofunc54911 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47316)
musttail call tailcc void %clofunc54911(%struct.ScmObj* %anf_45bind47316, %struct.ScmObj* %argslist53905$anf_45bind473161)
ret void
}

define tailcc void @proc_clo$ae51102(%struct.ScmObj* %env$ae51102,%struct.ScmObj* %current_45args53863) {
%stackaddr$env-ref54912 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51102, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54912
%stackaddr$env-ref54913 = alloca %struct.ScmObj*, align 8
%anf_45bind47313 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51102, i64 1)
store %struct.ScmObj* %anf_45bind47313, %struct.ScmObj** %stackaddr$env-ref54913
%stackaddr$env-ref54914 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51102, i64 2)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54914
%stackaddr$env-ref54915 = alloca %struct.ScmObj*, align 8
%anf_45bind47312 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51102, i64 3)
store %struct.ScmObj* %anf_45bind47312, %struct.ScmObj** %stackaddr$env-ref54915
%stackaddr$env-ref54916 = alloca %struct.ScmObj*, align 8
%anf_45bind47315 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51102, i64 4)
store %struct.ScmObj* %anf_45bind47315, %struct.ScmObj** %stackaddr$env-ref54916
%stackaddr$env-ref54917 = alloca %struct.ScmObj*, align 8
%anf_45bind47310 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51102, i64 5)
store %struct.ScmObj* %anf_45bind47310, %struct.ScmObj** %stackaddr$env-ref54917
%stackaddr$env-ref54918 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51102, i64 6)
store %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$env-ref54918
%stackaddr$env-ref54919 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51102, i64 7)
store %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$env-ref54919
%stackaddr$env-ref54920 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51102, i64 8)
store %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$env-ref54920
%stackaddr$prim54921 = alloca %struct.ScmObj*, align 8
%_95k47368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53863)
store volatile %struct.ScmObj* %_95k47368, %struct.ScmObj** %stackaddr$prim54921, align 8
%stackaddr$prim54922 = alloca %struct.ScmObj*, align 8
%current_45args53864 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53863)
store volatile %struct.ScmObj* %current_45args53864, %struct.ScmObj** %stackaddr$prim54922, align 8
%stackaddr$prim54923 = alloca %struct.ScmObj*, align 8
%anf_45bind47317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53864)
store volatile %struct.ScmObj* %anf_45bind47317, %struct.ScmObj** %stackaddr$prim54923, align 8
%stackaddr$makeclosure54924 = alloca %struct.ScmObj*, align 8
%fptrToInt54925 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51104 to i64
%ae51104 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt54925)
store volatile %struct.ScmObj* %ae51104, %struct.ScmObj** %stackaddr$makeclosure54924, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51104, %struct.ScmObj* %_37map147120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51104, %struct.ScmObj* %anf_45bind47312, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51104, %struct.ScmObj* %_37foldl147073, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51104, %struct.ScmObj* %anf_45bind47310, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51104, %struct.ScmObj* %anf_45bind47309, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51104, %struct.ScmObj* %anf_45bind47308, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51104, %struct.ScmObj* %anf_45bind47307, i64 6)
%argslist53904$anf_45bind473130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54926 = alloca %struct.ScmObj*, align 8
%argslist53904$anf_45bind473131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47317, %struct.ScmObj* %argslist53904$anf_45bind473130)
store volatile %struct.ScmObj* %argslist53904$anf_45bind473131, %struct.ScmObj** %stackaddr$prim54926, align 8
%stackaddr$prim54927 = alloca %struct.ScmObj*, align 8
%argslist53904$anf_45bind473132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47315, %struct.ScmObj* %argslist53904$anf_45bind473131)
store volatile %struct.ScmObj* %argslist53904$anf_45bind473132, %struct.ScmObj** %stackaddr$prim54927, align 8
%stackaddr$prim54928 = alloca %struct.ScmObj*, align 8
%argslist53904$anf_45bind473133 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51104, %struct.ScmObj* %argslist53904$anf_45bind473132)
store volatile %struct.ScmObj* %argslist53904$anf_45bind473133, %struct.ScmObj** %stackaddr$prim54928, align 8
%clofunc54929 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47313)
musttail call tailcc void %clofunc54929(%struct.ScmObj* %anf_45bind47313, %struct.ScmObj* %argslist53904$anf_45bind473133)
ret void
}

define tailcc void @proc_clo$ae51104(%struct.ScmObj* %env$ae51104,%struct.ScmObj* %current_45args53866) {
%stackaddr$env-ref54930 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51104, i64 0)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54930
%stackaddr$env-ref54931 = alloca %struct.ScmObj*, align 8
%anf_45bind47312 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51104, i64 1)
store %struct.ScmObj* %anf_45bind47312, %struct.ScmObj** %stackaddr$env-ref54931
%stackaddr$env-ref54932 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51104, i64 2)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54932
%stackaddr$env-ref54933 = alloca %struct.ScmObj*, align 8
%anf_45bind47310 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51104, i64 3)
store %struct.ScmObj* %anf_45bind47310, %struct.ScmObj** %stackaddr$env-ref54933
%stackaddr$env-ref54934 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51104, i64 4)
store %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$env-ref54934
%stackaddr$env-ref54935 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51104, i64 5)
store %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$env-ref54935
%stackaddr$env-ref54936 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51104, i64 6)
store %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$env-ref54936
%stackaddr$prim54937 = alloca %struct.ScmObj*, align 8
%_95k47369 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53866)
store volatile %struct.ScmObj* %_95k47369, %struct.ScmObj** %stackaddr$prim54937, align 8
%stackaddr$prim54938 = alloca %struct.ScmObj*, align 8
%current_45args53867 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53866)
store volatile %struct.ScmObj* %current_45args53867, %struct.ScmObj** %stackaddr$prim54938, align 8
%stackaddr$prim54939 = alloca %struct.ScmObj*, align 8
%anf_45bind47318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53867)
store volatile %struct.ScmObj* %anf_45bind47318, %struct.ScmObj** %stackaddr$prim54939, align 8
%stackaddr$makeclosure54940 = alloca %struct.ScmObj*, align 8
%fptrToInt54941 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51107 to i64
%ae51107 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt54941)
store volatile %struct.ScmObj* %ae51107, %struct.ScmObj** %stackaddr$makeclosure54940, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51107, %struct.ScmObj* %_37map147120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51107, %struct.ScmObj* %anf_45bind47312, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51107, %struct.ScmObj* %anf_45bind47318, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51107, %struct.ScmObj* %_37foldl147073, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51107, %struct.ScmObj* %anf_45bind47310, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51107, %struct.ScmObj* %anf_45bind47309, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51107, %struct.ScmObj* %anf_45bind47308, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae51107, %struct.ScmObj* %anf_45bind47307, i64 7)
%ae51108 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54942 = alloca %struct.ScmObj*, align 8
%fptrToInt54943 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51109 to i64
%ae51109 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54943)
store volatile %struct.ScmObj* %ae51109, %struct.ScmObj** %stackaddr$makeclosure54942, align 8
%argslist53903$ae511070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54944 = alloca %struct.ScmObj*, align 8
%argslist53903$ae511071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51109, %struct.ScmObj* %argslist53903$ae511070)
store volatile %struct.ScmObj* %argslist53903$ae511071, %struct.ScmObj** %stackaddr$prim54944, align 8
%stackaddr$prim54945 = alloca %struct.ScmObj*, align 8
%argslist53903$ae511072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51108, %struct.ScmObj* %argslist53903$ae511071)
store volatile %struct.ScmObj* %argslist53903$ae511072, %struct.ScmObj** %stackaddr$prim54945, align 8
%clofunc54946 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51107)
musttail call tailcc void %clofunc54946(%struct.ScmObj* %ae51107, %struct.ScmObj* %argslist53903$ae511072)
ret void
}

define tailcc void @proc_clo$ae51107(%struct.ScmObj* %env$ae51107,%struct.ScmObj* %current_45args53869) {
%stackaddr$env-ref54947 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51107, i64 0)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54947
%stackaddr$env-ref54948 = alloca %struct.ScmObj*, align 8
%anf_45bind47312 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51107, i64 1)
store %struct.ScmObj* %anf_45bind47312, %struct.ScmObj** %stackaddr$env-ref54948
%stackaddr$env-ref54949 = alloca %struct.ScmObj*, align 8
%anf_45bind47318 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51107, i64 2)
store %struct.ScmObj* %anf_45bind47318, %struct.ScmObj** %stackaddr$env-ref54949
%stackaddr$env-ref54950 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51107, i64 3)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54950
%stackaddr$env-ref54951 = alloca %struct.ScmObj*, align 8
%anf_45bind47310 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51107, i64 4)
store %struct.ScmObj* %anf_45bind47310, %struct.ScmObj** %stackaddr$env-ref54951
%stackaddr$env-ref54952 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51107, i64 5)
store %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$env-ref54952
%stackaddr$env-ref54953 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51107, i64 6)
store %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$env-ref54953
%stackaddr$env-ref54954 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51107, i64 7)
store %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$env-ref54954
%stackaddr$prim54955 = alloca %struct.ScmObj*, align 8
%_95k47370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53869)
store volatile %struct.ScmObj* %_95k47370, %struct.ScmObj** %stackaddr$prim54955, align 8
%stackaddr$prim54956 = alloca %struct.ScmObj*, align 8
%current_45args53870 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53869)
store volatile %struct.ScmObj* %current_45args53870, %struct.ScmObj** %stackaddr$prim54956, align 8
%stackaddr$prim54957 = alloca %struct.ScmObj*, align 8
%anf_45bind47319 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53870)
store volatile %struct.ScmObj* %anf_45bind47319, %struct.ScmObj** %stackaddr$prim54957, align 8
%stackaddr$makeclosure54958 = alloca %struct.ScmObj*, align 8
%fptrToInt54959 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51132 to i64
%ae51132 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt54959)
store volatile %struct.ScmObj* %ae51132, %struct.ScmObj** %stackaddr$makeclosure54958, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51132, %struct.ScmObj* %_37map147120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51132, %struct.ScmObj* %anf_45bind47312, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51132, %struct.ScmObj* %anf_45bind47318, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51132, %struct.ScmObj* %_37foldl147073, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51132, %struct.ScmObj* %anf_45bind47310, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51132, %struct.ScmObj* %anf_45bind47309, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51132, %struct.ScmObj* %anf_45bind47308, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae51132, %struct.ScmObj* %anf_45bind47307, i64 7)
%argslist53901$anf_45bind473190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54960 = alloca %struct.ScmObj*, align 8
%argslist53901$anf_45bind473191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51132, %struct.ScmObj* %argslist53901$anf_45bind473190)
store volatile %struct.ScmObj* %argslist53901$anf_45bind473191, %struct.ScmObj** %stackaddr$prim54960, align 8
%clofunc54961 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47319)
musttail call tailcc void %clofunc54961(%struct.ScmObj* %anf_45bind47319, %struct.ScmObj* %argslist53901$anf_45bind473191)
ret void
}

define tailcc void @proc_clo$ae51132(%struct.ScmObj* %env$ae51132,%struct.ScmObj* %current_45args53872) {
%stackaddr$env-ref54962 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51132, i64 0)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54962
%stackaddr$env-ref54963 = alloca %struct.ScmObj*, align 8
%anf_45bind47312 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51132, i64 1)
store %struct.ScmObj* %anf_45bind47312, %struct.ScmObj** %stackaddr$env-ref54963
%stackaddr$env-ref54964 = alloca %struct.ScmObj*, align 8
%anf_45bind47318 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51132, i64 2)
store %struct.ScmObj* %anf_45bind47318, %struct.ScmObj** %stackaddr$env-ref54964
%stackaddr$env-ref54965 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51132, i64 3)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54965
%stackaddr$env-ref54966 = alloca %struct.ScmObj*, align 8
%anf_45bind47310 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51132, i64 4)
store %struct.ScmObj* %anf_45bind47310, %struct.ScmObj** %stackaddr$env-ref54966
%stackaddr$env-ref54967 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51132, i64 5)
store %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$env-ref54967
%stackaddr$env-ref54968 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51132, i64 6)
store %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$env-ref54968
%stackaddr$env-ref54969 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51132, i64 7)
store %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$env-ref54969
%stackaddr$prim54970 = alloca %struct.ScmObj*, align 8
%_95k47371 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53872)
store volatile %struct.ScmObj* %_95k47371, %struct.ScmObj** %stackaddr$prim54970, align 8
%stackaddr$prim54971 = alloca %struct.ScmObj*, align 8
%current_45args53873 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53872)
store volatile %struct.ScmObj* %current_45args53873, %struct.ScmObj** %stackaddr$prim54971, align 8
%stackaddr$prim54972 = alloca %struct.ScmObj*, align 8
%anf_45bind47320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53873)
store volatile %struct.ScmObj* %anf_45bind47320, %struct.ScmObj** %stackaddr$prim54972, align 8
%stackaddr$makeclosure54973 = alloca %struct.ScmObj*, align 8
%fptrToInt54974 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51133 to i64
%ae51133 = call %struct.ScmObj* @closure_alloc(i64 9, i64 %fptrToInt54974)
store volatile %struct.ScmObj* %ae51133, %struct.ScmObj** %stackaddr$makeclosure54973, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51133, %struct.ScmObj* %_37map147120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51133, %struct.ScmObj* %anf_45bind47312, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51133, %struct.ScmObj* %anf_45bind47318, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51133, %struct.ScmObj* %_37foldl147073, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51133, %struct.ScmObj* %anf_45bind47310, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51133, %struct.ScmObj* %anf_45bind47309, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51133, %struct.ScmObj* %anf_45bind47308, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae51133, %struct.ScmObj* %anf_45bind47307, i64 7)
call void @closure_place_freevar(%struct.ScmObj* %ae51133, %struct.ScmObj* %anf_45bind47320, i64 8)
%ae51134 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54975 = alloca %struct.ScmObj*, align 8
%fptrToInt54976 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51135 to i64
%ae51135 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54976)
store volatile %struct.ScmObj* %ae51135, %struct.ScmObj** %stackaddr$makeclosure54975, align 8
%argslist53900$ae511330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54977 = alloca %struct.ScmObj*, align 8
%argslist53900$ae511331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51135, %struct.ScmObj* %argslist53900$ae511330)
store volatile %struct.ScmObj* %argslist53900$ae511331, %struct.ScmObj** %stackaddr$prim54977, align 8
%stackaddr$prim54978 = alloca %struct.ScmObj*, align 8
%argslist53900$ae511332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51134, %struct.ScmObj* %argslist53900$ae511331)
store volatile %struct.ScmObj* %argslist53900$ae511332, %struct.ScmObj** %stackaddr$prim54978, align 8
%clofunc54979 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51133)
musttail call tailcc void %clofunc54979(%struct.ScmObj* %ae51133, %struct.ScmObj* %argslist53900$ae511332)
ret void
}

define tailcc void @proc_clo$ae51133(%struct.ScmObj* %env$ae51133,%struct.ScmObj* %current_45args53875) {
%stackaddr$env-ref54980 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51133, i64 0)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54980
%stackaddr$env-ref54981 = alloca %struct.ScmObj*, align 8
%anf_45bind47312 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51133, i64 1)
store %struct.ScmObj* %anf_45bind47312, %struct.ScmObj** %stackaddr$env-ref54981
%stackaddr$env-ref54982 = alloca %struct.ScmObj*, align 8
%anf_45bind47318 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51133, i64 2)
store %struct.ScmObj* %anf_45bind47318, %struct.ScmObj** %stackaddr$env-ref54982
%stackaddr$env-ref54983 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51133, i64 3)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref54983
%stackaddr$env-ref54984 = alloca %struct.ScmObj*, align 8
%anf_45bind47310 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51133, i64 4)
store %struct.ScmObj* %anf_45bind47310, %struct.ScmObj** %stackaddr$env-ref54984
%stackaddr$env-ref54985 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51133, i64 5)
store %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$env-ref54985
%stackaddr$env-ref54986 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51133, i64 6)
store %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$env-ref54986
%stackaddr$env-ref54987 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51133, i64 7)
store %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$env-ref54987
%stackaddr$env-ref54988 = alloca %struct.ScmObj*, align 8
%anf_45bind47320 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51133, i64 8)
store %struct.ScmObj* %anf_45bind47320, %struct.ScmObj** %stackaddr$env-ref54988
%stackaddr$prim54989 = alloca %struct.ScmObj*, align 8
%_95k47372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53875)
store volatile %struct.ScmObj* %_95k47372, %struct.ScmObj** %stackaddr$prim54989, align 8
%stackaddr$prim54990 = alloca %struct.ScmObj*, align 8
%current_45args53876 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53875)
store volatile %struct.ScmObj* %current_45args53876, %struct.ScmObj** %stackaddr$prim54990, align 8
%stackaddr$prim54991 = alloca %struct.ScmObj*, align 8
%anf_45bind47321 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53876)
store volatile %struct.ScmObj* %anf_45bind47321, %struct.ScmObj** %stackaddr$prim54991, align 8
%stackaddr$makeclosure54992 = alloca %struct.ScmObj*, align 8
%fptrToInt54993 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51158 to i64
%ae51158 = call %struct.ScmObj* @closure_alloc(i64 9, i64 %fptrToInt54993)
store volatile %struct.ScmObj* %ae51158, %struct.ScmObj** %stackaddr$makeclosure54992, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51158, %struct.ScmObj* %_37map147120, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51158, %struct.ScmObj* %anf_45bind47312, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51158, %struct.ScmObj* %anf_45bind47318, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51158, %struct.ScmObj* %_37foldl147073, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51158, %struct.ScmObj* %anf_45bind47310, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae51158, %struct.ScmObj* %anf_45bind47309, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae51158, %struct.ScmObj* %anf_45bind47308, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae51158, %struct.ScmObj* %anf_45bind47307, i64 7)
call void @closure_place_freevar(%struct.ScmObj* %ae51158, %struct.ScmObj* %anf_45bind47320, i64 8)
%ae51159 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51160 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist53898$anf_45bind473210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54994 = alloca %struct.ScmObj*, align 8
%argslist53898$anf_45bind473211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51160, %struct.ScmObj* %argslist53898$anf_45bind473210)
store volatile %struct.ScmObj* %argslist53898$anf_45bind473211, %struct.ScmObj** %stackaddr$prim54994, align 8
%stackaddr$prim54995 = alloca %struct.ScmObj*, align 8
%argslist53898$anf_45bind473212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51159, %struct.ScmObj* %argslist53898$anf_45bind473211)
store volatile %struct.ScmObj* %argslist53898$anf_45bind473212, %struct.ScmObj** %stackaddr$prim54995, align 8
%stackaddr$prim54996 = alloca %struct.ScmObj*, align 8
%argslist53898$anf_45bind473213 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51158, %struct.ScmObj* %argslist53898$anf_45bind473212)
store volatile %struct.ScmObj* %argslist53898$anf_45bind473213, %struct.ScmObj** %stackaddr$prim54996, align 8
%clofunc54997 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47321)
musttail call tailcc void %clofunc54997(%struct.ScmObj* %anf_45bind47321, %struct.ScmObj* %argslist53898$anf_45bind473213)
ret void
}

define tailcc void @proc_clo$ae51158(%struct.ScmObj* %env$ae51158,%struct.ScmObj* %current_45args53878) {
%stackaddr$env-ref54998 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51158, i64 0)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref54998
%stackaddr$env-ref54999 = alloca %struct.ScmObj*, align 8
%anf_45bind47312 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51158, i64 1)
store %struct.ScmObj* %anf_45bind47312, %struct.ScmObj** %stackaddr$env-ref54999
%stackaddr$env-ref55000 = alloca %struct.ScmObj*, align 8
%anf_45bind47318 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51158, i64 2)
store %struct.ScmObj* %anf_45bind47318, %struct.ScmObj** %stackaddr$env-ref55000
%stackaddr$env-ref55001 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51158, i64 3)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref55001
%stackaddr$env-ref55002 = alloca %struct.ScmObj*, align 8
%anf_45bind47310 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51158, i64 4)
store %struct.ScmObj* %anf_45bind47310, %struct.ScmObj** %stackaddr$env-ref55002
%stackaddr$env-ref55003 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51158, i64 5)
store %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$env-ref55003
%stackaddr$env-ref55004 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51158, i64 6)
store %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$env-ref55004
%stackaddr$env-ref55005 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51158, i64 7)
store %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$env-ref55005
%stackaddr$env-ref55006 = alloca %struct.ScmObj*, align 8
%anf_45bind47320 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51158, i64 8)
store %struct.ScmObj* %anf_45bind47320, %struct.ScmObj** %stackaddr$env-ref55006
%stackaddr$prim55007 = alloca %struct.ScmObj*, align 8
%_95k47373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53878)
store volatile %struct.ScmObj* %_95k47373, %struct.ScmObj** %stackaddr$prim55007, align 8
%stackaddr$prim55008 = alloca %struct.ScmObj*, align 8
%current_45args53879 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53878)
store volatile %struct.ScmObj* %current_45args53879, %struct.ScmObj** %stackaddr$prim55008, align 8
%stackaddr$prim55009 = alloca %struct.ScmObj*, align 8
%anf_45bind47322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53879)
store volatile %struct.ScmObj* %anf_45bind47322, %struct.ScmObj** %stackaddr$prim55009, align 8
%stackaddr$makeclosure55010 = alloca %struct.ScmObj*, align 8
%fptrToInt55011 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51170 to i64
%ae51170 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55011)
store volatile %struct.ScmObj* %ae51170, %struct.ScmObj** %stackaddr$makeclosure55010, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51170, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51170, %struct.ScmObj* %_37map147120, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51170, %struct.ScmObj* %anf_45bind47309, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51170, %struct.ScmObj* %anf_45bind47308, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae51170, %struct.ScmObj* %anf_45bind47307, i64 4)
%ae51171 = call %struct.ScmObj* @const_init_false()
%ae51172 = call %struct.ScmObj* @const_init_true()
%ae51173 = call %struct.ScmObj* @const_init_int(i64 7)
%ae51178 = call %struct.ScmObj* @const_init_symbol(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @global$sym$ae5117855012, i32 0, i32 0))
%argslist53897$anf_45bind473100 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55013 = alloca %struct.ScmObj*, align 8
%argslist53897$anf_45bind473101 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51178, %struct.ScmObj* %argslist53897$anf_45bind473100)
store volatile %struct.ScmObj* %argslist53897$anf_45bind473101, %struct.ScmObj** %stackaddr$prim55013, align 8
%stackaddr$prim55014 = alloca %struct.ScmObj*, align 8
%argslist53897$anf_45bind473102 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47322, %struct.ScmObj* %argslist53897$anf_45bind473101)
store volatile %struct.ScmObj* %argslist53897$anf_45bind473102, %struct.ScmObj** %stackaddr$prim55014, align 8
%stackaddr$prim55015 = alloca %struct.ScmObj*, align 8
%argslist53897$anf_45bind473103 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47320, %struct.ScmObj* %argslist53897$anf_45bind473102)
store volatile %struct.ScmObj* %argslist53897$anf_45bind473103, %struct.ScmObj** %stackaddr$prim55015, align 8
%stackaddr$prim55016 = alloca %struct.ScmObj*, align 8
%argslist53897$anf_45bind473104 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47318, %struct.ScmObj* %argslist53897$anf_45bind473103)
store volatile %struct.ScmObj* %argslist53897$anf_45bind473104, %struct.ScmObj** %stackaddr$prim55016, align 8
%stackaddr$prim55017 = alloca %struct.ScmObj*, align 8
%argslist53897$anf_45bind473105 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47312, %struct.ScmObj* %argslist53897$anf_45bind473104)
store volatile %struct.ScmObj* %argslist53897$anf_45bind473105, %struct.ScmObj** %stackaddr$prim55017, align 8
%stackaddr$prim55018 = alloca %struct.ScmObj*, align 8
%argslist53897$anf_45bind473106 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51173, %struct.ScmObj* %argslist53897$anf_45bind473105)
store volatile %struct.ScmObj* %argslist53897$anf_45bind473106, %struct.ScmObj** %stackaddr$prim55018, align 8
%stackaddr$prim55019 = alloca %struct.ScmObj*, align 8
%argslist53897$anf_45bind473107 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51172, %struct.ScmObj* %argslist53897$anf_45bind473106)
store volatile %struct.ScmObj* %argslist53897$anf_45bind473107, %struct.ScmObj** %stackaddr$prim55019, align 8
%stackaddr$prim55020 = alloca %struct.ScmObj*, align 8
%argslist53897$anf_45bind473108 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51171, %struct.ScmObj* %argslist53897$anf_45bind473107)
store volatile %struct.ScmObj* %argslist53897$anf_45bind473108, %struct.ScmObj** %stackaddr$prim55020, align 8
%stackaddr$prim55021 = alloca %struct.ScmObj*, align 8
%argslist53897$anf_45bind473109 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51170, %struct.ScmObj* %argslist53897$anf_45bind473108)
store volatile %struct.ScmObj* %argslist53897$anf_45bind473109, %struct.ScmObj** %stackaddr$prim55021, align 8
%clofunc55022 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47310)
musttail call tailcc void %clofunc55022(%struct.ScmObj* %anf_45bind47310, %struct.ScmObj* %argslist53897$anf_45bind473109)
ret void
}

define tailcc void @proc_clo$ae51170(%struct.ScmObj* %env$ae51170,%struct.ScmObj* %current_45args53881) {
%stackaddr$env-ref55023 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51170, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref55023
%stackaddr$env-ref55024 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51170, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref55024
%stackaddr$env-ref55025 = alloca %struct.ScmObj*, align 8
%anf_45bind47309 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51170, i64 2)
store %struct.ScmObj* %anf_45bind47309, %struct.ScmObj** %stackaddr$env-ref55025
%stackaddr$env-ref55026 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51170, i64 3)
store %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$env-ref55026
%stackaddr$env-ref55027 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51170, i64 4)
store %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$env-ref55027
%stackaddr$prim55028 = alloca %struct.ScmObj*, align 8
%_95k47374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53881)
store volatile %struct.ScmObj* %_95k47374, %struct.ScmObj** %stackaddr$prim55028, align 8
%stackaddr$prim55029 = alloca %struct.ScmObj*, align 8
%current_45args53882 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53881)
store volatile %struct.ScmObj* %current_45args53882, %struct.ScmObj** %stackaddr$prim55029, align 8
%stackaddr$prim55030 = alloca %struct.ScmObj*, align 8
%anf_45bind47323 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53882)
store volatile %struct.ScmObj* %anf_45bind47323, %struct.ScmObj** %stackaddr$prim55030, align 8
%stackaddr$makeclosure55031 = alloca %struct.ScmObj*, align 8
%fptrToInt55032 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51220 to i64
%ae51220 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55032)
store volatile %struct.ScmObj* %ae51220, %struct.ScmObj** %stackaddr$makeclosure55031, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51220, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51220, %struct.ScmObj* %_37map147120, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51220, %struct.ScmObj* %anf_45bind47308, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae51220, %struct.ScmObj* %anf_45bind47307, i64 3)
%argslist53896$_37map1471200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55033 = alloca %struct.ScmObj*, align 8
%argslist53896$_37map1471201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47323, %struct.ScmObj* %argslist53896$_37map1471200)
store volatile %struct.ScmObj* %argslist53896$_37map1471201, %struct.ScmObj** %stackaddr$prim55033, align 8
%stackaddr$prim55034 = alloca %struct.ScmObj*, align 8
%argslist53896$_37map1471202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47309, %struct.ScmObj* %argslist53896$_37map1471201)
store volatile %struct.ScmObj* %argslist53896$_37map1471202, %struct.ScmObj** %stackaddr$prim55034, align 8
%stackaddr$prim55035 = alloca %struct.ScmObj*, align 8
%argslist53896$_37map1471203 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51220, %struct.ScmObj* %argslist53896$_37map1471202)
store volatile %struct.ScmObj* %argslist53896$_37map1471203, %struct.ScmObj** %stackaddr$prim55035, align 8
%clofunc55036 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147120)
musttail call tailcc void %clofunc55036(%struct.ScmObj* %_37map147120, %struct.ScmObj* %argslist53896$_37map1471203)
ret void
}

define tailcc void @proc_clo$ae51220(%struct.ScmObj* %env$ae51220,%struct.ScmObj* %current_45args53884) {
%stackaddr$env-ref55037 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51220, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref55037
%stackaddr$env-ref55038 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51220, i64 1)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref55038
%stackaddr$env-ref55039 = alloca %struct.ScmObj*, align 8
%anf_45bind47308 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51220, i64 2)
store %struct.ScmObj* %anf_45bind47308, %struct.ScmObj** %stackaddr$env-ref55039
%stackaddr$env-ref55040 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51220, i64 3)
store %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$env-ref55040
%stackaddr$prim55041 = alloca %struct.ScmObj*, align 8
%_95k47375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53884)
store volatile %struct.ScmObj* %_95k47375, %struct.ScmObj** %stackaddr$prim55041, align 8
%stackaddr$prim55042 = alloca %struct.ScmObj*, align 8
%current_45args53885 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53884)
store volatile %struct.ScmObj* %current_45args53885, %struct.ScmObj** %stackaddr$prim55042, align 8
%stackaddr$prim55043 = alloca %struct.ScmObj*, align 8
%anf_45bind47324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53885)
store volatile %struct.ScmObj* %anf_45bind47324, %struct.ScmObj** %stackaddr$prim55043, align 8
%stackaddr$makeclosure55044 = alloca %struct.ScmObj*, align 8
%fptrToInt55045 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51224 to i64
%ae51224 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55045)
store volatile %struct.ScmObj* %ae51224, %struct.ScmObj** %stackaddr$makeclosure55044, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51224, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51224, %struct.ScmObj* %anf_45bind47307, i64 1)
%argslist53895$_37map1471200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55046 = alloca %struct.ScmObj*, align 8
%argslist53895$_37map1471201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47324, %struct.ScmObj* %argslist53895$_37map1471200)
store volatile %struct.ScmObj* %argslist53895$_37map1471201, %struct.ScmObj** %stackaddr$prim55046, align 8
%stackaddr$prim55047 = alloca %struct.ScmObj*, align 8
%argslist53895$_37map1471202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47308, %struct.ScmObj* %argslist53895$_37map1471201)
store volatile %struct.ScmObj* %argslist53895$_37map1471202, %struct.ScmObj** %stackaddr$prim55047, align 8
%stackaddr$prim55048 = alloca %struct.ScmObj*, align 8
%argslist53895$_37map1471203 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51224, %struct.ScmObj* %argslist53895$_37map1471202)
store volatile %struct.ScmObj* %argslist53895$_37map1471203, %struct.ScmObj** %stackaddr$prim55048, align 8
%clofunc55049 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147120)
musttail call tailcc void %clofunc55049(%struct.ScmObj* %_37map147120, %struct.ScmObj* %argslist53895$_37map1471203)
ret void
}

define tailcc void @proc_clo$ae51224(%struct.ScmObj* %env$ae51224,%struct.ScmObj* %current_45args53887) {
%stackaddr$env-ref55050 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51224, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref55050
%stackaddr$env-ref55051 = alloca %struct.ScmObj*, align 8
%anf_45bind47307 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51224, i64 1)
store %struct.ScmObj* %anf_45bind47307, %struct.ScmObj** %stackaddr$env-ref55051
%stackaddr$prim55052 = alloca %struct.ScmObj*, align 8
%_95k47376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53887)
store volatile %struct.ScmObj* %_95k47376, %struct.ScmObj** %stackaddr$prim55052, align 8
%stackaddr$prim55053 = alloca %struct.ScmObj*, align 8
%current_45args53888 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53887)
store volatile %struct.ScmObj* %current_45args53888, %struct.ScmObj** %stackaddr$prim55053, align 8
%stackaddr$prim55054 = alloca %struct.ScmObj*, align 8
%anf_45bind47325 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53888)
store volatile %struct.ScmObj* %anf_45bind47325, %struct.ScmObj** %stackaddr$prim55054, align 8
%stackaddr$makeclosure55055 = alloca %struct.ScmObj*, align 8
%fptrToInt55056 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51228 to i64
%ae51228 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55056)
store volatile %struct.ScmObj* %ae51228, %struct.ScmObj** %stackaddr$makeclosure55055, align 8
%ae51230 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53894$_37foldl1470730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55057 = alloca %struct.ScmObj*, align 8
%argslist53894$_37foldl1470731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47325, %struct.ScmObj* %argslist53894$_37foldl1470730)
store volatile %struct.ScmObj* %argslist53894$_37foldl1470731, %struct.ScmObj** %stackaddr$prim55057, align 8
%stackaddr$prim55058 = alloca %struct.ScmObj*, align 8
%argslist53894$_37foldl1470732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51230, %struct.ScmObj* %argslist53894$_37foldl1470731)
store volatile %struct.ScmObj* %argslist53894$_37foldl1470732, %struct.ScmObj** %stackaddr$prim55058, align 8
%stackaddr$prim55059 = alloca %struct.ScmObj*, align 8
%argslist53894$_37foldl1470733 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47307, %struct.ScmObj* %argslist53894$_37foldl1470732)
store volatile %struct.ScmObj* %argslist53894$_37foldl1470733, %struct.ScmObj** %stackaddr$prim55059, align 8
%stackaddr$prim55060 = alloca %struct.ScmObj*, align 8
%argslist53894$_37foldl1470734 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51228, %struct.ScmObj* %argslist53894$_37foldl1470733)
store volatile %struct.ScmObj* %argslist53894$_37foldl1470734, %struct.ScmObj** %stackaddr$prim55060, align 8
%clofunc55061 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147073)
musttail call tailcc void %clofunc55061(%struct.ScmObj* %_37foldl147073, %struct.ScmObj* %argslist53894$_37foldl1470734)
ret void
}

define tailcc void @proc_clo$ae51228(%struct.ScmObj* %env$ae51228,%struct.ScmObj* %current_45args53890) {
%stackaddr$prim55062 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53890)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim55062, align 8
%stackaddr$prim55063 = alloca %struct.ScmObj*, align 8
%current_45args53891 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53890)
store volatile %struct.ScmObj* %current_45args53891, %struct.ScmObj** %stackaddr$prim55063, align 8
%stackaddr$prim55064 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53891)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim55064, align 8
%stackaddr$prim55065 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim55065, align 8
%argslist53893$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55066 = alloca %struct.ScmObj*, align 8
%argslist53893$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist53893$k0)
store volatile %struct.ScmObj* %argslist53893$k1, %struct.ScmObj** %stackaddr$prim55066, align 8
%clofunc55067 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc55067(%struct.ScmObj* %k, %struct.ScmObj* %argslist53893$k1)
ret void
}

define tailcc void @proc_clo$ae51135(%struct.ScmObj* %env$ae51135,%struct.ScmObj* %el4719547377) {
%stackaddr$prim55068 = alloca %struct.ScmObj*, align 8
%k47378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %el4719547377)
store volatile %struct.ScmObj* %k47378, %struct.ScmObj** %stackaddr$prim55068, align 8
%stackaddr$prim55069 = alloca %struct.ScmObj*, align 8
%el47195 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %el4719547377)
store volatile %struct.ScmObj* %el47195, %struct.ScmObj** %stackaddr$prim55069, align 8
%stackaddr$applyprim55070 = alloca %struct.ScmObj*, align 8
%cpsaprim47379 = call %struct.ScmObj* @applyprim_vector(%struct.ScmObj* %el47195)
store volatile %struct.ScmObj* %cpsaprim47379, %struct.ScmObj** %stackaddr$applyprim55070, align 8
%ae51140 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53899$k473780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55071 = alloca %struct.ScmObj*, align 8
%argslist53899$k473781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim47379, %struct.ScmObj* %argslist53899$k473780)
store volatile %struct.ScmObj* %argslist53899$k473781, %struct.ScmObj** %stackaddr$prim55071, align 8
%stackaddr$prim55072 = alloca %struct.ScmObj*, align 8
%argslist53899$k473782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51140, %struct.ScmObj* %argslist53899$k473781)
store volatile %struct.ScmObj* %argslist53899$k473782, %struct.ScmObj** %stackaddr$prim55072, align 8
%clofunc55073 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47378)
musttail call tailcc void %clofunc55073(%struct.ScmObj* %k47378, %struct.ScmObj* %argslist53899$k473782)
ret void
}

define tailcc void @proc_clo$ae51109(%struct.ScmObj* %env$ae51109,%struct.ScmObj* %el4719447380) {
%stackaddr$prim55074 = alloca %struct.ScmObj*, align 8
%k47381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %el4719447380)
store volatile %struct.ScmObj* %k47381, %struct.ScmObj** %stackaddr$prim55074, align 8
%stackaddr$prim55075 = alloca %struct.ScmObj*, align 8
%el47194 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %el4719447380)
store volatile %struct.ScmObj* %el47194, %struct.ScmObj** %stackaddr$prim55075, align 8
%stackaddr$applyprim55076 = alloca %struct.ScmObj*, align 8
%cpsaprim47382 = call %struct.ScmObj* @applyprim_vector(%struct.ScmObj* %el47194)
store volatile %struct.ScmObj* %cpsaprim47382, %struct.ScmObj** %stackaddr$applyprim55076, align 8
%ae51114 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53902$k473810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55077 = alloca %struct.ScmObj*, align 8
%argslist53902$k473811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim47382, %struct.ScmObj* %argslist53902$k473810)
store volatile %struct.ScmObj* %argslist53902$k473811, %struct.ScmObj** %stackaddr$prim55077, align 8
%stackaddr$prim55078 = alloca %struct.ScmObj*, align 8
%argslist53902$k473812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51114, %struct.ScmObj* %argslist53902$k473811)
store volatile %struct.ScmObj* %argslist53902$k473812, %struct.ScmObj** %stackaddr$prim55078, align 8
%clofunc55079 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47381)
musttail call tailcc void %clofunc55079(%struct.ScmObj* %k47381, %struct.ScmObj* %argslist53902$k473812)
ret void
}

define tailcc void @proc_clo$ae51081(%struct.ScmObj* %env$ae51081,%struct.ScmObj* %lst4719347383) {
%stackaddr$prim55080 = alloca %struct.ScmObj*, align 8
%k47384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4719347383)
store volatile %struct.ScmObj* %k47384, %struct.ScmObj** %stackaddr$prim55080, align 8
%stackaddr$prim55081 = alloca %struct.ScmObj*, align 8
%lst47193 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4719347383)
store volatile %struct.ScmObj* %lst47193, %struct.ScmObj** %stackaddr$prim55081, align 8
%ae51085 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53906$k473840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55082 = alloca %struct.ScmObj*, align 8
%argslist53906$k473841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47193, %struct.ScmObj* %argslist53906$k473840)
store volatile %struct.ScmObj* %argslist53906$k473841, %struct.ScmObj** %stackaddr$prim55082, align 8
%stackaddr$prim55083 = alloca %struct.ScmObj*, align 8
%argslist53906$k473842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51085, %struct.ScmObj* %argslist53906$k473841)
store volatile %struct.ScmObj* %argslist53906$k473842, %struct.ScmObj** %stackaddr$prim55083, align 8
%clofunc55084 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47384)
musttail call tailcc void %clofunc55084(%struct.ScmObj* %k47384, %struct.ScmObj* %argslist53906$k473842)
ret void
}

define tailcc void @proc_clo$ae51057(%struct.ScmObj* %env$ae51057,%struct.ScmObj* %lst4719247385) {
%stackaddr$prim55085 = alloca %struct.ScmObj*, align 8
%k47386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4719247385)
store volatile %struct.ScmObj* %k47386, %struct.ScmObj** %stackaddr$prim55085, align 8
%stackaddr$prim55086 = alloca %struct.ScmObj*, align 8
%lst47192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4719247385)
store volatile %struct.ScmObj* %lst47192, %struct.ScmObj** %stackaddr$prim55086, align 8
%ae51061 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53909$k473860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55087 = alloca %struct.ScmObj*, align 8
%argslist53909$k473861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47192, %struct.ScmObj* %argslist53909$k473860)
store volatile %struct.ScmObj* %argslist53909$k473861, %struct.ScmObj** %stackaddr$prim55087, align 8
%stackaddr$prim55088 = alloca %struct.ScmObj*, align 8
%argslist53909$k473862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51061, %struct.ScmObj* %argslist53909$k473861)
store volatile %struct.ScmObj* %argslist53909$k473862, %struct.ScmObj** %stackaddr$prim55088, align 8
%clofunc55089 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47386)
musttail call tailcc void %clofunc55089(%struct.ScmObj* %k47386, %struct.ScmObj* %argslist53909$k473862)
ret void
}

define tailcc void @proc_clo$ae51035(%struct.ScmObj* %env$ae51035,%struct.ScmObj* %lst4719147387) {
%stackaddr$prim55090 = alloca %struct.ScmObj*, align 8
%k47388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4719147387)
store volatile %struct.ScmObj* %k47388, %struct.ScmObj** %stackaddr$prim55090, align 8
%stackaddr$prim55091 = alloca %struct.ScmObj*, align 8
%lst47191 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4719147387)
store volatile %struct.ScmObj* %lst47191, %struct.ScmObj** %stackaddr$prim55091, align 8
%ae51039 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53911$k473880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55092 = alloca %struct.ScmObj*, align 8
%argslist53911$k473881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47191, %struct.ScmObj* %argslist53911$k473880)
store volatile %struct.ScmObj* %argslist53911$k473881, %struct.ScmObj** %stackaddr$prim55092, align 8
%stackaddr$prim55093 = alloca %struct.ScmObj*, align 8
%argslist53911$k473882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51039, %struct.ScmObj* %argslist53911$k473881)
store volatile %struct.ScmObj* %argslist53911$k473882, %struct.ScmObj** %stackaddr$prim55093, align 8
%clofunc55094 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47388)
musttail call tailcc void %clofunc55094(%struct.ScmObj* %k47388, %struct.ScmObj* %argslist53911$k473882)
ret void
}

define tailcc void @proc_clo$ae51011(%struct.ScmObj* %env$ae51011,%struct.ScmObj* %lst4719047389) {
%stackaddr$prim55095 = alloca %struct.ScmObj*, align 8
%k47390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4719047389)
store volatile %struct.ScmObj* %k47390, %struct.ScmObj** %stackaddr$prim55095, align 8
%stackaddr$prim55096 = alloca %struct.ScmObj*, align 8
%lst47190 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4719047389)
store volatile %struct.ScmObj* %lst47190, %struct.ScmObj** %stackaddr$prim55096, align 8
%ae51015 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53914$k473900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55097 = alloca %struct.ScmObj*, align 8
%argslist53914$k473901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47190, %struct.ScmObj* %argslist53914$k473900)
store volatile %struct.ScmObj* %argslist53914$k473901, %struct.ScmObj** %stackaddr$prim55097, align 8
%stackaddr$prim55098 = alloca %struct.ScmObj*, align 8
%argslist53914$k473902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51015, %struct.ScmObj* %argslist53914$k473901)
store volatile %struct.ScmObj* %argslist53914$k473902, %struct.ScmObj** %stackaddr$prim55098, align 8
%clofunc55099 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47390)
musttail call tailcc void %clofunc55099(%struct.ScmObj* %k47390, %struct.ScmObj* %argslist53914$k473902)
ret void
}

define tailcc void @proc_clo$ae50989(%struct.ScmObj* %env$ae50989,%struct.ScmObj* %lst4718947391) {
%stackaddr$prim55100 = alloca %struct.ScmObj*, align 8
%k47392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst4718947391)
store volatile %struct.ScmObj* %k47392, %struct.ScmObj** %stackaddr$prim55100, align 8
%stackaddr$prim55101 = alloca %struct.ScmObj*, align 8
%lst47189 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst4718947391)
store volatile %struct.ScmObj* %lst47189, %struct.ScmObj** %stackaddr$prim55101, align 8
%ae50993 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53916$k473920 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55102 = alloca %struct.ScmObj*, align 8
%argslist53916$k473921 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47189, %struct.ScmObj* %argslist53916$k473920)
store volatile %struct.ScmObj* %argslist53916$k473921, %struct.ScmObj** %stackaddr$prim55102, align 8
%stackaddr$prim55103 = alloca %struct.ScmObj*, align 8
%argslist53916$k473922 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50993, %struct.ScmObj* %argslist53916$k473921)
store volatile %struct.ScmObj* %argslist53916$k473922, %struct.ScmObj** %stackaddr$prim55103, align 8
%clofunc55104 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47392)
musttail call tailcc void %clofunc55104(%struct.ScmObj* %k47392, %struct.ScmObj* %argslist53916$k473922)
ret void
}

define tailcc void @proc_clo$ae50965(%struct.ScmObj* %env$ae50965,%struct.ScmObj* %arg4718847393) {
%stackaddr$prim55105 = alloca %struct.ScmObj*, align 8
%k47394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %arg4718847393)
store volatile %struct.ScmObj* %k47394, %struct.ScmObj** %stackaddr$prim55105, align 8
%stackaddr$prim55106 = alloca %struct.ScmObj*, align 8
%arg47188 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %arg4718847393)
store volatile %struct.ScmObj* %arg47188, %struct.ScmObj** %stackaddr$prim55106, align 8
%stackaddr$applyprim55107 = alloca %struct.ScmObj*, align 8
%cpsaprim47395 = call %struct.ScmObj* @applyprim_number_63(%struct.ScmObj* %arg47188)
store volatile %struct.ScmObj* %cpsaprim47395, %struct.ScmObj** %stackaddr$applyprim55107, align 8
%ae50970 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53918$k473940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55108 = alloca %struct.ScmObj*, align 8
%argslist53918$k473941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim47395, %struct.ScmObj* %argslist53918$k473940)
store volatile %struct.ScmObj* %argslist53918$k473941, %struct.ScmObj** %stackaddr$prim55108, align 8
%stackaddr$prim55109 = alloca %struct.ScmObj*, align 8
%argslist53918$k473942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50970, %struct.ScmObj* %argslist53918$k473941)
store volatile %struct.ScmObj* %argslist53918$k473942, %struct.ScmObj** %stackaddr$prim55109, align 8
%clofunc55110 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47394)
musttail call tailcc void %clofunc55110(%struct.ScmObj* %k47394, %struct.ScmObj* %argslist53918$k473942)
ret void
}

define tailcc void @proc_clo$ae50930(%struct.ScmObj* %env$ae50930,%struct.ScmObj* %current_45args53920) {
%stackaddr$prim55111 = alloca %struct.ScmObj*, align 8
%k47396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53920)
store volatile %struct.ScmObj* %k47396, %struct.ScmObj** %stackaddr$prim55111, align 8
%stackaddr$prim55112 = alloca %struct.ScmObj*, align 8
%current_45args53921 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53920)
store volatile %struct.ScmObj* %current_45args53921, %struct.ScmObj** %stackaddr$prim55112, align 8
%stackaddr$prim55113 = alloca %struct.ScmObj*, align 8
%b47187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53921)
store volatile %struct.ScmObj* %b47187, %struct.ScmObj** %stackaddr$prim55113, align 8
%truthy$cmp55114 = call i64 @is_truthy_value(%struct.ScmObj* %b47187)
%cmp$cmp55114 = icmp eq i64 %truthy$cmp55114, 1
br i1 %cmp$cmp55114, label %truebranch$cmp55114, label %falsebranch$cmp55114
truebranch$cmp55114:
%ae50933 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50934 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist53923$k473960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55115 = alloca %struct.ScmObj*, align 8
%argslist53923$k473961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50934, %struct.ScmObj* %argslist53923$k473960)
store volatile %struct.ScmObj* %argslist53923$k473961, %struct.ScmObj** %stackaddr$prim55115, align 8
%stackaddr$prim55116 = alloca %struct.ScmObj*, align 8
%argslist53923$k473962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50933, %struct.ScmObj* %argslist53923$k473961)
store volatile %struct.ScmObj* %argslist53923$k473962, %struct.ScmObj** %stackaddr$prim55116, align 8
%clofunc55117 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47396)
musttail call tailcc void %clofunc55117(%struct.ScmObj* %k47396, %struct.ScmObj* %argslist53923$k473962)
ret void
falsebranch$cmp55114:
%ae50942 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50943 = call %struct.ScmObj* @const_init_int(i64 2)
%argslist53924$k473960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55118 = alloca %struct.ScmObj*, align 8
%argslist53924$k473961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50943, %struct.ScmObj* %argslist53924$k473960)
store volatile %struct.ScmObj* %argslist53924$k473961, %struct.ScmObj** %stackaddr$prim55118, align 8
%stackaddr$prim55119 = alloca %struct.ScmObj*, align 8
%argslist53924$k473962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50942, %struct.ScmObj* %argslist53924$k473961)
store volatile %struct.ScmObj* %argslist53924$k473962, %struct.ScmObj** %stackaddr$prim55119, align 8
%clofunc55120 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47396)
musttail call tailcc void %clofunc55120(%struct.ScmObj* %k47396, %struct.ScmObj* %argslist53924$k473962)
ret void
}

define tailcc void @proc_clo$ae50906(%struct.ScmObj* %env$ae50906,%struct.ScmObj* %args4718647397) {
%stackaddr$prim55121 = alloca %struct.ScmObj*, align 8
%k47398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4718647397)
store volatile %struct.ScmObj* %k47398, %struct.ScmObj** %stackaddr$prim55121, align 8
%stackaddr$prim55122 = alloca %struct.ScmObj*, align 8
%args47186 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4718647397)
store volatile %struct.ScmObj* %args47186, %struct.ScmObj** %stackaddr$prim55122, align 8
%stackaddr$applyprim55123 = alloca %struct.ScmObj*, align 8
%cpsaprim47399 = call %struct.ScmObj* @applyprim__43(%struct.ScmObj* %args47186)
store volatile %struct.ScmObj* %cpsaprim47399, %struct.ScmObj** %stackaddr$applyprim55123, align 8
%ae50911 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53926$k473980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55124 = alloca %struct.ScmObj*, align 8
%argslist53926$k473981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsaprim47399, %struct.ScmObj* %argslist53926$k473980)
store volatile %struct.ScmObj* %argslist53926$k473981, %struct.ScmObj** %stackaddr$prim55124, align 8
%stackaddr$prim55125 = alloca %struct.ScmObj*, align 8
%argslist53926$k473982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50911, %struct.ScmObj* %argslist53926$k473981)
store volatile %struct.ScmObj* %argslist53926$k473982, %struct.ScmObj** %stackaddr$prim55125, align 8
%clofunc55126 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47398)
musttail call tailcc void %clofunc55126(%struct.ScmObj* %k47398, %struct.ScmObj* %argslist53926$k473982)
ret void
}

define tailcc void @proc_clo$ae50880(%struct.ScmObj* %env$ae50880,%struct.ScmObj* %current_45args53928) {
%stackaddr$prim55127 = alloca %struct.ScmObj*, align 8
%k47400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53928)
store volatile %struct.ScmObj* %k47400, %struct.ScmObj** %stackaddr$prim55127, align 8
%stackaddr$prim55128 = alloca %struct.ScmObj*, align 8
%current_45args53929 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53928)
store volatile %struct.ScmObj* %current_45args53929, %struct.ScmObj** %stackaddr$prim55128, align 8
%stackaddr$prim55129 = alloca %struct.ScmObj*, align 8
%x47126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53929)
store volatile %struct.ScmObj* %x47126, %struct.ScmObj** %stackaddr$prim55129, align 8
%stackaddr$prim55130 = alloca %struct.ScmObj*, align 8
%anf_45bind47304 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47126)
store volatile %struct.ScmObj* %anf_45bind47304, %struct.ScmObj** %stackaddr$prim55130, align 8
%stackaddr$prim55131 = alloca %struct.ScmObj*, align 8
%anf_45bind47305 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47304)
store volatile %struct.ScmObj* %anf_45bind47305, %struct.ScmObj** %stackaddr$prim55131, align 8
%stackaddr$prim55132 = alloca %struct.ScmObj*, align 8
%anf_45bind47306 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47305)
store volatile %struct.ScmObj* %anf_45bind47306, %struct.ScmObj** %stackaddr$prim55132, align 8
%stackaddr$prim55133 = alloca %struct.ScmObj*, align 8
%cpsprim47401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47306)
store volatile %struct.ScmObj* %cpsprim47401, %struct.ScmObj** %stackaddr$prim55133, align 8
%ae50886 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53931$k474000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55134 = alloca %struct.ScmObj*, align 8
%argslist53931$k474001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47401, %struct.ScmObj* %argslist53931$k474000)
store volatile %struct.ScmObj* %argslist53931$k474001, %struct.ScmObj** %stackaddr$prim55134, align 8
%stackaddr$prim55135 = alloca %struct.ScmObj*, align 8
%argslist53931$k474002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50886, %struct.ScmObj* %argslist53931$k474001)
store volatile %struct.ScmObj* %argslist53931$k474002, %struct.ScmObj** %stackaddr$prim55135, align 8
%clofunc55136 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47400)
musttail call tailcc void %clofunc55136(%struct.ScmObj* %k47400, %struct.ScmObj* %argslist53931$k474002)
ret void
}

define tailcc void @proc_clo$ae50856(%struct.ScmObj* %env$ae50856,%struct.ScmObj* %current_45args53933) {
%stackaddr$prim55137 = alloca %struct.ScmObj*, align 8
%k47402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53933)
store volatile %struct.ScmObj* %k47402, %struct.ScmObj** %stackaddr$prim55137, align 8
%stackaddr$prim55138 = alloca %struct.ScmObj*, align 8
%current_45args53934 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53933)
store volatile %struct.ScmObj* %current_45args53934, %struct.ScmObj** %stackaddr$prim55138, align 8
%stackaddr$prim55139 = alloca %struct.ScmObj*, align 8
%x47128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53934)
store volatile %struct.ScmObj* %x47128, %struct.ScmObj** %stackaddr$prim55139, align 8
%stackaddr$prim55140 = alloca %struct.ScmObj*, align 8
%anf_45bind47302 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47128)
store volatile %struct.ScmObj* %anf_45bind47302, %struct.ScmObj** %stackaddr$prim55140, align 8
%stackaddr$prim55141 = alloca %struct.ScmObj*, align 8
%anf_45bind47303 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47302)
store volatile %struct.ScmObj* %anf_45bind47303, %struct.ScmObj** %stackaddr$prim55141, align 8
%stackaddr$prim55142 = alloca %struct.ScmObj*, align 8
%cpsprim47403 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47303)
store volatile %struct.ScmObj* %cpsprim47403, %struct.ScmObj** %stackaddr$prim55142, align 8
%ae50861 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53936$k474020 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55143 = alloca %struct.ScmObj*, align 8
%argslist53936$k474021 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47403, %struct.ScmObj* %argslist53936$k474020)
store volatile %struct.ScmObj* %argslist53936$k474021, %struct.ScmObj** %stackaddr$prim55143, align 8
%stackaddr$prim55144 = alloca %struct.ScmObj*, align 8
%argslist53936$k474022 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50861, %struct.ScmObj* %argslist53936$k474021)
store volatile %struct.ScmObj* %argslist53936$k474022, %struct.ScmObj** %stackaddr$prim55144, align 8
%clofunc55145 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47402)
musttail call tailcc void %clofunc55145(%struct.ScmObj* %k47402, %struct.ScmObj* %argslist53936$k474022)
ret void
}

define tailcc void @proc_clo$ae50834(%struct.ScmObj* %env$ae50834,%struct.ScmObj* %current_45args53938) {
%stackaddr$prim55146 = alloca %struct.ScmObj*, align 8
%k47404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53938)
store volatile %struct.ScmObj* %k47404, %struct.ScmObj** %stackaddr$prim55146, align 8
%stackaddr$prim55147 = alloca %struct.ScmObj*, align 8
%current_45args53939 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53938)
store volatile %struct.ScmObj* %current_45args53939, %struct.ScmObj** %stackaddr$prim55147, align 8
%stackaddr$prim55148 = alloca %struct.ScmObj*, align 8
%x47130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53939)
store volatile %struct.ScmObj* %x47130, %struct.ScmObj** %stackaddr$prim55148, align 8
%stackaddr$prim55149 = alloca %struct.ScmObj*, align 8
%anf_45bind47301 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47130)
store volatile %struct.ScmObj* %anf_45bind47301, %struct.ScmObj** %stackaddr$prim55149, align 8
%stackaddr$prim55150 = alloca %struct.ScmObj*, align 8
%cpsprim47405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47301)
store volatile %struct.ScmObj* %cpsprim47405, %struct.ScmObj** %stackaddr$prim55150, align 8
%ae50838 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53941$k474040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55151 = alloca %struct.ScmObj*, align 8
%argslist53941$k474041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47405, %struct.ScmObj* %argslist53941$k474040)
store volatile %struct.ScmObj* %argslist53941$k474041, %struct.ScmObj** %stackaddr$prim55151, align 8
%stackaddr$prim55152 = alloca %struct.ScmObj*, align 8
%argslist53941$k474042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50838, %struct.ScmObj* %argslist53941$k474041)
store volatile %struct.ScmObj* %argslist53941$k474042, %struct.ScmObj** %stackaddr$prim55152, align 8
%clofunc55153 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47404)
musttail call tailcc void %clofunc55153(%struct.ScmObj* %k47404, %struct.ScmObj* %argslist53941$k474042)
ret void
}

define tailcc void @proc_clo$ae50814(%struct.ScmObj* %env$ae50814,%struct.ScmObj* %current_45args53943) {
%stackaddr$prim55154 = alloca %struct.ScmObj*, align 8
%k47406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53943)
store volatile %struct.ScmObj* %k47406, %struct.ScmObj** %stackaddr$prim55154, align 8
%stackaddr$prim55155 = alloca %struct.ScmObj*, align 8
%current_45args53944 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53943)
store volatile %struct.ScmObj* %current_45args53944, %struct.ScmObj** %stackaddr$prim55155, align 8
%stackaddr$prim55156 = alloca %struct.ScmObj*, align 8
%x47132 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53944)
store volatile %struct.ScmObj* %x47132, %struct.ScmObj** %stackaddr$prim55156, align 8
%stackaddr$prim55157 = alloca %struct.ScmObj*, align 8
%cpsprim47407 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47132)
store volatile %struct.ScmObj* %cpsprim47407, %struct.ScmObj** %stackaddr$prim55157, align 8
%ae50817 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53946$k474060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55158 = alloca %struct.ScmObj*, align 8
%argslist53946$k474061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47407, %struct.ScmObj* %argslist53946$k474060)
store volatile %struct.ScmObj* %argslist53946$k474061, %struct.ScmObj** %stackaddr$prim55158, align 8
%stackaddr$prim55159 = alloca %struct.ScmObj*, align 8
%argslist53946$k474062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50817, %struct.ScmObj* %argslist53946$k474061)
store volatile %struct.ScmObj* %argslist53946$k474062, %struct.ScmObj** %stackaddr$prim55159, align 8
%clofunc55160 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47406)
musttail call tailcc void %clofunc55160(%struct.ScmObj* %k47406, %struct.ScmObj* %argslist53946$k474062)
ret void
}

define tailcc void @proc_clo$ae50716(%struct.ScmObj* %env$ae50716,%struct.ScmObj* %args4713447408) {
%stackaddr$env-ref55161 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50716, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref55161
%stackaddr$prim55162 = alloca %struct.ScmObj*, align 8
%k47409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4713447408)
store volatile %struct.ScmObj* %k47409, %struct.ScmObj** %stackaddr$prim55162, align 8
%stackaddr$prim55163 = alloca %struct.ScmObj*, align 8
%args47134 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4713447408)
store volatile %struct.ScmObj* %args47134, %struct.ScmObj** %stackaddr$prim55163, align 8
%stackaddr$prim55164 = alloca %struct.ScmObj*, align 8
%anf_45bind47295 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %anf_45bind47295, %struct.ScmObj** %stackaddr$prim55164, align 8
%truthy$cmp55165 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47295)
%cmp$cmp55165 = icmp eq i64 %truthy$cmp55165, 1
br i1 %cmp$cmp55165, label %truebranch$cmp55165, label %falsebranch$cmp55165
truebranch$cmp55165:
%ae50722 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50723 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist53948$k474090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55166 = alloca %struct.ScmObj*, align 8
%argslist53948$k474091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50723, %struct.ScmObj* %argslist53948$k474090)
store volatile %struct.ScmObj* %argslist53948$k474091, %struct.ScmObj** %stackaddr$prim55166, align 8
%stackaddr$prim55167 = alloca %struct.ScmObj*, align 8
%argslist53948$k474092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50722, %struct.ScmObj* %argslist53948$k474091)
store volatile %struct.ScmObj* %argslist53948$k474092, %struct.ScmObj** %stackaddr$prim55167, align 8
%clofunc55168 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47409)
musttail call tailcc void %clofunc55168(%struct.ScmObj* %k47409, %struct.ScmObj* %argslist53948$k474092)
ret void
falsebranch$cmp55165:
%stackaddr$prim55169 = alloca %struct.ScmObj*, align 8
%anf_45bind47296 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %anf_45bind47296, %struct.ScmObj** %stackaddr$prim55169, align 8
%stackaddr$prim55170 = alloca %struct.ScmObj*, align 8
%anf_45bind47297 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47296)
store volatile %struct.ScmObj* %anf_45bind47297, %struct.ScmObj** %stackaddr$prim55170, align 8
%truthy$cmp55171 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47297)
%cmp$cmp55171 = icmp eq i64 %truthy$cmp55171, 1
br i1 %cmp$cmp55171, label %truebranch$cmp55171, label %falsebranch$cmp55171
truebranch$cmp55171:
%stackaddr$prim55172 = alloca %struct.ScmObj*, align 8
%cpsprim47410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %cpsprim47410, %struct.ScmObj** %stackaddr$prim55172, align 8
%ae50735 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53949$k474090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55173 = alloca %struct.ScmObj*, align 8
%argslist53949$k474091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47410, %struct.ScmObj* %argslist53949$k474090)
store volatile %struct.ScmObj* %argslist53949$k474091, %struct.ScmObj** %stackaddr$prim55173, align 8
%stackaddr$prim55174 = alloca %struct.ScmObj*, align 8
%argslist53949$k474092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50735, %struct.ScmObj* %argslist53949$k474091)
store volatile %struct.ScmObj* %argslist53949$k474092, %struct.ScmObj** %stackaddr$prim55174, align 8
%clofunc55175 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47409)
musttail call tailcc void %clofunc55175(%struct.ScmObj* %k47409, %struct.ScmObj* %argslist53949$k474092)
ret void
falsebranch$cmp55171:
%stackaddr$makeclosure55176 = alloca %struct.ScmObj*, align 8
%fptrToInt55177 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50740 to i64
%ae50740 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55177)
store volatile %struct.ScmObj* %ae50740, %struct.ScmObj** %stackaddr$makeclosure55176, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50740, %struct.ScmObj* %k47409, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50740, %struct.ScmObj* %_37foldl147073, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50740, %struct.ScmObj* %args47134, i64 2)
%ae50741 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55178 = alloca %struct.ScmObj*, align 8
%fptrToInt55179 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50742 to i64
%ae50742 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55179)
store volatile %struct.ScmObj* %ae50742, %struct.ScmObj** %stackaddr$makeclosure55178, align 8
%argslist53959$ae507400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55180 = alloca %struct.ScmObj*, align 8
%argslist53959$ae507401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50742, %struct.ScmObj* %argslist53959$ae507400)
store volatile %struct.ScmObj* %argslist53959$ae507401, %struct.ScmObj** %stackaddr$prim55180, align 8
%stackaddr$prim55181 = alloca %struct.ScmObj*, align 8
%argslist53959$ae507402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50741, %struct.ScmObj* %argslist53959$ae507401)
store volatile %struct.ScmObj* %argslist53959$ae507402, %struct.ScmObj** %stackaddr$prim55181, align 8
%clofunc55182 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50740)
musttail call tailcc void %clofunc55182(%struct.ScmObj* %ae50740, %struct.ScmObj* %argslist53959$ae507402)
ret void
}

define tailcc void @proc_clo$ae50740(%struct.ScmObj* %env$ae50740,%struct.ScmObj* %current_45args53950) {
%stackaddr$env-ref55183 = alloca %struct.ScmObj*, align 8
%k47409 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50740, i64 0)
store %struct.ScmObj* %k47409, %struct.ScmObj** %stackaddr$env-ref55183
%stackaddr$env-ref55184 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50740, i64 1)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref55184
%stackaddr$env-ref55185 = alloca %struct.ScmObj*, align 8
%args47134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50740, i64 2)
store %struct.ScmObj* %args47134, %struct.ScmObj** %stackaddr$env-ref55185
%stackaddr$prim55186 = alloca %struct.ScmObj*, align 8
%_95k47411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53950)
store volatile %struct.ScmObj* %_95k47411, %struct.ScmObj** %stackaddr$prim55186, align 8
%stackaddr$prim55187 = alloca %struct.ScmObj*, align 8
%current_45args53951 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53950)
store volatile %struct.ScmObj* %current_45args53951, %struct.ScmObj** %stackaddr$prim55187, align 8
%stackaddr$prim55188 = alloca %struct.ScmObj*, align 8
%anf_45bind47298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53951)
store volatile %struct.ScmObj* %anf_45bind47298, %struct.ScmObj** %stackaddr$prim55188, align 8
%stackaddr$prim55189 = alloca %struct.ScmObj*, align 8
%anf_45bind47299 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %anf_45bind47299, %struct.ScmObj** %stackaddr$prim55189, align 8
%stackaddr$prim55190 = alloca %struct.ScmObj*, align 8
%anf_45bind47300 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47134)
store volatile %struct.ScmObj* %anf_45bind47300, %struct.ScmObj** %stackaddr$prim55190, align 8
%argslist53953$_37foldl1470730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55191 = alloca %struct.ScmObj*, align 8
%argslist53953$_37foldl1470731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47300, %struct.ScmObj* %argslist53953$_37foldl1470730)
store volatile %struct.ScmObj* %argslist53953$_37foldl1470731, %struct.ScmObj** %stackaddr$prim55191, align 8
%stackaddr$prim55192 = alloca %struct.ScmObj*, align 8
%argslist53953$_37foldl1470732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47299, %struct.ScmObj* %argslist53953$_37foldl1470731)
store volatile %struct.ScmObj* %argslist53953$_37foldl1470732, %struct.ScmObj** %stackaddr$prim55192, align 8
%stackaddr$prim55193 = alloca %struct.ScmObj*, align 8
%argslist53953$_37foldl1470733 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47298, %struct.ScmObj* %argslist53953$_37foldl1470732)
store volatile %struct.ScmObj* %argslist53953$_37foldl1470733, %struct.ScmObj** %stackaddr$prim55193, align 8
%stackaddr$prim55194 = alloca %struct.ScmObj*, align 8
%argslist53953$_37foldl1470734 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47409, %struct.ScmObj* %argslist53953$_37foldl1470733)
store volatile %struct.ScmObj* %argslist53953$_37foldl1470734, %struct.ScmObj** %stackaddr$prim55194, align 8
%clofunc55195 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147073)
musttail call tailcc void %clofunc55195(%struct.ScmObj* %_37foldl147073, %struct.ScmObj* %argslist53953$_37foldl1470734)
ret void
}

define tailcc void @proc_clo$ae50742(%struct.ScmObj* %env$ae50742,%struct.ScmObj* %current_45args53954) {
%stackaddr$prim55196 = alloca %struct.ScmObj*, align 8
%k47412 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53954)
store volatile %struct.ScmObj* %k47412, %struct.ScmObj** %stackaddr$prim55196, align 8
%stackaddr$prim55197 = alloca %struct.ScmObj*, align 8
%current_45args53955 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53954)
store volatile %struct.ScmObj* %current_45args53955, %struct.ScmObj** %stackaddr$prim55197, align 8
%stackaddr$prim55198 = alloca %struct.ScmObj*, align 8
%n47136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53955)
store volatile %struct.ScmObj* %n47136, %struct.ScmObj** %stackaddr$prim55198, align 8
%stackaddr$prim55199 = alloca %struct.ScmObj*, align 8
%current_45args53956 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53955)
store volatile %struct.ScmObj* %current_45args53956, %struct.ScmObj** %stackaddr$prim55199, align 8
%stackaddr$prim55200 = alloca %struct.ScmObj*, align 8
%v47135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53956)
store volatile %struct.ScmObj* %v47135, %struct.ScmObj** %stackaddr$prim55200, align 8
%stackaddr$prim55201 = alloca %struct.ScmObj*, align 8
%cpsprim47413 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v47135, %struct.ScmObj* %n47136)
store volatile %struct.ScmObj* %cpsprim47413, %struct.ScmObj** %stackaddr$prim55201, align 8
%ae50746 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53958$k474120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55202 = alloca %struct.ScmObj*, align 8
%argslist53958$k474121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47413, %struct.ScmObj* %argslist53958$k474120)
store volatile %struct.ScmObj* %argslist53958$k474121, %struct.ScmObj** %stackaddr$prim55202, align 8
%stackaddr$prim55203 = alloca %struct.ScmObj*, align 8
%argslist53958$k474122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50746, %struct.ScmObj* %argslist53958$k474121)
store volatile %struct.ScmObj* %argslist53958$k474122, %struct.ScmObj** %stackaddr$prim55203, align 8
%clofunc55204 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47412)
musttail call tailcc void %clofunc55204(%struct.ScmObj* %k47412, %struct.ScmObj* %argslist53958$k474122)
ret void
}

define tailcc void @proc_clo$ae50312(%struct.ScmObj* %env$ae50312,%struct.ScmObj* %current_45args53961) {
%stackaddr$prim55205 = alloca %struct.ScmObj*, align 8
%k47414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53961)
store volatile %struct.ScmObj* %k47414, %struct.ScmObj** %stackaddr$prim55205, align 8
%stackaddr$prim55206 = alloca %struct.ScmObj*, align 8
%current_45args53962 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53961)
store volatile %struct.ScmObj* %current_45args53962, %struct.ScmObj** %stackaddr$prim55206, align 8
%stackaddr$prim55207 = alloca %struct.ScmObj*, align 8
%v47139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53962)
store volatile %struct.ScmObj* %v47139, %struct.ScmObj** %stackaddr$prim55207, align 8
%stackaddr$prim55208 = alloca %struct.ScmObj*, align 8
%current_45args53963 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53962)
store volatile %struct.ScmObj* %current_45args53963, %struct.ScmObj** %stackaddr$prim55208, align 8
%stackaddr$prim55209 = alloca %struct.ScmObj*, align 8
%lst47138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53963)
store volatile %struct.ScmObj* %lst47138, %struct.ScmObj** %stackaddr$prim55209, align 8
%ae50313 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55210 = alloca %struct.ScmObj*, align 8
%lst47140 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50313, %struct.ScmObj* %lst47138)
store volatile %struct.ScmObj* %lst47140, %struct.ScmObj** %stackaddr$prim55210, align 8
%stackaddr$makeclosure55211 = alloca %struct.ScmObj*, align 8
%fptrToInt55212 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50315 to i64
%ae50315 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55212)
store volatile %struct.ScmObj* %ae50315, %struct.ScmObj** %stackaddr$makeclosure55211, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50315, %struct.ScmObj* %k47414, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50315, %struct.ScmObj* %lst47140, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50315, %struct.ScmObj* %v47139, i64 2)
%ae50316 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55213 = alloca %struct.ScmObj*, align 8
%fptrToInt55214 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50317 to i64
%ae50317 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55214)
store volatile %struct.ScmObj* %ae50317, %struct.ScmObj** %stackaddr$makeclosure55213, align 8
%argslist53985$ae503150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55215 = alloca %struct.ScmObj*, align 8
%argslist53985$ae503151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50317, %struct.ScmObj* %argslist53985$ae503150)
store volatile %struct.ScmObj* %argslist53985$ae503151, %struct.ScmObj** %stackaddr$prim55215, align 8
%stackaddr$prim55216 = alloca %struct.ScmObj*, align 8
%argslist53985$ae503152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50316, %struct.ScmObj* %argslist53985$ae503151)
store volatile %struct.ScmObj* %argslist53985$ae503152, %struct.ScmObj** %stackaddr$prim55216, align 8
%clofunc55217 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50315)
musttail call tailcc void %clofunc55217(%struct.ScmObj* %ae50315, %struct.ScmObj* %argslist53985$ae503152)
ret void
}

define tailcc void @proc_clo$ae50315(%struct.ScmObj* %env$ae50315,%struct.ScmObj* %current_45args53965) {
%stackaddr$env-ref55218 = alloca %struct.ScmObj*, align 8
%k47414 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50315, i64 0)
store %struct.ScmObj* %k47414, %struct.ScmObj** %stackaddr$env-ref55218
%stackaddr$env-ref55219 = alloca %struct.ScmObj*, align 8
%lst47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50315, i64 1)
store %struct.ScmObj* %lst47140, %struct.ScmObj** %stackaddr$env-ref55219
%stackaddr$env-ref55220 = alloca %struct.ScmObj*, align 8
%v47139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50315, i64 2)
store %struct.ScmObj* %v47139, %struct.ScmObj** %stackaddr$env-ref55220
%stackaddr$prim55221 = alloca %struct.ScmObj*, align 8
%_95k47415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53965)
store volatile %struct.ScmObj* %_95k47415, %struct.ScmObj** %stackaddr$prim55221, align 8
%stackaddr$prim55222 = alloca %struct.ScmObj*, align 8
%current_45args53966 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53965)
store volatile %struct.ScmObj* %current_45args53966, %struct.ScmObj** %stackaddr$prim55222, align 8
%stackaddr$prim55223 = alloca %struct.ScmObj*, align 8
%anf_45bind47287 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53966)
store volatile %struct.ScmObj* %anf_45bind47287, %struct.ScmObj** %stackaddr$prim55223, align 8
%stackaddr$makeclosure55224 = alloca %struct.ScmObj*, align 8
%fptrToInt55225 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50331 to i64
%ae50331 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55225)
store volatile %struct.ScmObj* %ae50331, %struct.ScmObj** %stackaddr$makeclosure55224, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50331, %struct.ScmObj* %k47414, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50331, %struct.ScmObj* %lst47140, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50331, %struct.ScmObj* %v47139, i64 2)
%stackaddr$makeclosure55226 = alloca %struct.ScmObj*, align 8
%fptrToInt55227 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50332 to i64
%ae50332 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55227)
store volatile %struct.ScmObj* %ae50332, %struct.ScmObj** %stackaddr$makeclosure55226, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50332, %struct.ScmObj* %k47414, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50332, %struct.ScmObj* %lst47140, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50332, %struct.ScmObj* %v47139, i64 2)
%argslist53980$anf_45bind472870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55228 = alloca %struct.ScmObj*, align 8
%argslist53980$anf_45bind472871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50332, %struct.ScmObj* %argslist53980$anf_45bind472870)
store volatile %struct.ScmObj* %argslist53980$anf_45bind472871, %struct.ScmObj** %stackaddr$prim55228, align 8
%stackaddr$prim55229 = alloca %struct.ScmObj*, align 8
%argslist53980$anf_45bind472872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50331, %struct.ScmObj* %argslist53980$anf_45bind472871)
store volatile %struct.ScmObj* %argslist53980$anf_45bind472872, %struct.ScmObj** %stackaddr$prim55229, align 8
%clofunc55230 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47287)
musttail call tailcc void %clofunc55230(%struct.ScmObj* %anf_45bind47287, %struct.ScmObj* %argslist53980$anf_45bind472872)
ret void
}

define tailcc void @proc_clo$ae50331(%struct.ScmObj* %env$ae50331,%struct.ScmObj* %current_45args53968) {
%stackaddr$env-ref55231 = alloca %struct.ScmObj*, align 8
%k47414 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50331, i64 0)
store %struct.ScmObj* %k47414, %struct.ScmObj** %stackaddr$env-ref55231
%stackaddr$env-ref55232 = alloca %struct.ScmObj*, align 8
%lst47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50331, i64 1)
store %struct.ScmObj* %lst47140, %struct.ScmObj** %stackaddr$env-ref55232
%stackaddr$env-ref55233 = alloca %struct.ScmObj*, align 8
%v47139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50331, i64 2)
store %struct.ScmObj* %v47139, %struct.ScmObj** %stackaddr$env-ref55233
%stackaddr$prim55234 = alloca %struct.ScmObj*, align 8
%_95k47416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53968)
store volatile %struct.ScmObj* %_95k47416, %struct.ScmObj** %stackaddr$prim55234, align 8
%stackaddr$prim55235 = alloca %struct.ScmObj*, align 8
%current_45args53969 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53968)
store volatile %struct.ScmObj* %current_45args53969, %struct.ScmObj** %stackaddr$prim55235, align 8
%stackaddr$prim55236 = alloca %struct.ScmObj*, align 8
%cc47141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53969)
store volatile %struct.ScmObj* %cc47141, %struct.ScmObj** %stackaddr$prim55236, align 8
%ae50440 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55237 = alloca %struct.ScmObj*, align 8
%anf_45bind47288 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50440)
store volatile %struct.ScmObj* %anf_45bind47288, %struct.ScmObj** %stackaddr$prim55237, align 8
%stackaddr$prim55238 = alloca %struct.ScmObj*, align 8
%anf_45bind47289 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47288)
store volatile %struct.ScmObj* %anf_45bind47289, %struct.ScmObj** %stackaddr$prim55238, align 8
%truthy$cmp55239 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47289)
%cmp$cmp55239 = icmp eq i64 %truthy$cmp55239, 1
br i1 %cmp$cmp55239, label %truebranch$cmp55239, label %falsebranch$cmp55239
truebranch$cmp55239:
%ae50444 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50445 = call %struct.ScmObj* @const_init_false()
%argslist53971$k474140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55240 = alloca %struct.ScmObj*, align 8
%argslist53971$k474141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50445, %struct.ScmObj* %argslist53971$k474140)
store volatile %struct.ScmObj* %argslist53971$k474141, %struct.ScmObj** %stackaddr$prim55240, align 8
%stackaddr$prim55241 = alloca %struct.ScmObj*, align 8
%argslist53971$k474142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50444, %struct.ScmObj* %argslist53971$k474141)
store volatile %struct.ScmObj* %argslist53971$k474142, %struct.ScmObj** %stackaddr$prim55241, align 8
%clofunc55242 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47414)
musttail call tailcc void %clofunc55242(%struct.ScmObj* %k47414, %struct.ScmObj* %argslist53971$k474142)
ret void
falsebranch$cmp55239:
%ae50453 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55243 = alloca %struct.ScmObj*, align 8
%anf_45bind47290 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50453)
store volatile %struct.ScmObj* %anf_45bind47290, %struct.ScmObj** %stackaddr$prim55243, align 8
%stackaddr$prim55244 = alloca %struct.ScmObj*, align 8
%anf_45bind47291 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47290)
store volatile %struct.ScmObj* %anf_45bind47291, %struct.ScmObj** %stackaddr$prim55244, align 8
%stackaddr$prim55245 = alloca %struct.ScmObj*, align 8
%anf_45bind47292 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47291, %struct.ScmObj* %v47139)
store volatile %struct.ScmObj* %anf_45bind47292, %struct.ScmObj** %stackaddr$prim55245, align 8
%truthy$cmp55246 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47292)
%cmp$cmp55246 = icmp eq i64 %truthy$cmp55246, 1
br i1 %cmp$cmp55246, label %truebranch$cmp55246, label %falsebranch$cmp55246
truebranch$cmp55246:
%ae50459 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55247 = alloca %struct.ScmObj*, align 8
%cpsprim47417 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50459)
store volatile %struct.ScmObj* %cpsprim47417, %struct.ScmObj** %stackaddr$prim55247, align 8
%ae50461 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53972$k474140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55248 = alloca %struct.ScmObj*, align 8
%argslist53972$k474141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47417, %struct.ScmObj* %argslist53972$k474140)
store volatile %struct.ScmObj* %argslist53972$k474141, %struct.ScmObj** %stackaddr$prim55248, align 8
%stackaddr$prim55249 = alloca %struct.ScmObj*, align 8
%argslist53972$k474142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50461, %struct.ScmObj* %argslist53972$k474141)
store volatile %struct.ScmObj* %argslist53972$k474142, %struct.ScmObj** %stackaddr$prim55249, align 8
%clofunc55250 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47414)
musttail call tailcc void %clofunc55250(%struct.ScmObj* %k47414, %struct.ScmObj* %argslist53972$k474142)
ret void
falsebranch$cmp55246:
%ae50472 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55251 = alloca %struct.ScmObj*, align 8
%anf_45bind47293 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50472)
store volatile %struct.ScmObj* %anf_45bind47293, %struct.ScmObj** %stackaddr$prim55251, align 8
%stackaddr$prim55252 = alloca %struct.ScmObj*, align 8
%anf_45bind47294 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47293)
store volatile %struct.ScmObj* %anf_45bind47294, %struct.ScmObj** %stackaddr$prim55252, align 8
%ae50475 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55253 = alloca %struct.ScmObj*, align 8
%_95047143 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50475, %struct.ScmObj* %anf_45bind47294)
store volatile %struct.ScmObj* %_95047143, %struct.ScmObj** %stackaddr$prim55253, align 8
%argslist53973$cc471410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55254 = alloca %struct.ScmObj*, align 8
%argslist53973$cc471411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47141, %struct.ScmObj* %argslist53973$cc471410)
store volatile %struct.ScmObj* %argslist53973$cc471411, %struct.ScmObj** %stackaddr$prim55254, align 8
%stackaddr$prim55255 = alloca %struct.ScmObj*, align 8
%argslist53973$cc471412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47414, %struct.ScmObj* %argslist53973$cc471411)
store volatile %struct.ScmObj* %argslist53973$cc471412, %struct.ScmObj** %stackaddr$prim55255, align 8
%clofunc55256 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47141)
musttail call tailcc void %clofunc55256(%struct.ScmObj* %cc47141, %struct.ScmObj* %argslist53973$cc471412)
ret void
}

define tailcc void @proc_clo$ae50332(%struct.ScmObj* %env$ae50332,%struct.ScmObj* %current_45args53974) {
%stackaddr$env-ref55257 = alloca %struct.ScmObj*, align 8
%k47414 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50332, i64 0)
store %struct.ScmObj* %k47414, %struct.ScmObj** %stackaddr$env-ref55257
%stackaddr$env-ref55258 = alloca %struct.ScmObj*, align 8
%lst47140 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50332, i64 1)
store %struct.ScmObj* %lst47140, %struct.ScmObj** %stackaddr$env-ref55258
%stackaddr$env-ref55259 = alloca %struct.ScmObj*, align 8
%v47139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50332, i64 2)
store %struct.ScmObj* %v47139, %struct.ScmObj** %stackaddr$env-ref55259
%stackaddr$prim55260 = alloca %struct.ScmObj*, align 8
%_95k47416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53974)
store volatile %struct.ScmObj* %_95k47416, %struct.ScmObj** %stackaddr$prim55260, align 8
%stackaddr$prim55261 = alloca %struct.ScmObj*, align 8
%current_45args53975 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53974)
store volatile %struct.ScmObj* %current_45args53975, %struct.ScmObj** %stackaddr$prim55261, align 8
%stackaddr$prim55262 = alloca %struct.ScmObj*, align 8
%cc47141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53975)
store volatile %struct.ScmObj* %cc47141, %struct.ScmObj** %stackaddr$prim55262, align 8
%ae50334 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55263 = alloca %struct.ScmObj*, align 8
%anf_45bind47288 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50334)
store volatile %struct.ScmObj* %anf_45bind47288, %struct.ScmObj** %stackaddr$prim55263, align 8
%stackaddr$prim55264 = alloca %struct.ScmObj*, align 8
%anf_45bind47289 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47288)
store volatile %struct.ScmObj* %anf_45bind47289, %struct.ScmObj** %stackaddr$prim55264, align 8
%truthy$cmp55265 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47289)
%cmp$cmp55265 = icmp eq i64 %truthy$cmp55265, 1
br i1 %cmp$cmp55265, label %truebranch$cmp55265, label %falsebranch$cmp55265
truebranch$cmp55265:
%ae50338 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50339 = call %struct.ScmObj* @const_init_false()
%argslist53977$k474140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55266 = alloca %struct.ScmObj*, align 8
%argslist53977$k474141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50339, %struct.ScmObj* %argslist53977$k474140)
store volatile %struct.ScmObj* %argslist53977$k474141, %struct.ScmObj** %stackaddr$prim55266, align 8
%stackaddr$prim55267 = alloca %struct.ScmObj*, align 8
%argslist53977$k474142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50338, %struct.ScmObj* %argslist53977$k474141)
store volatile %struct.ScmObj* %argslist53977$k474142, %struct.ScmObj** %stackaddr$prim55267, align 8
%clofunc55268 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47414)
musttail call tailcc void %clofunc55268(%struct.ScmObj* %k47414, %struct.ScmObj* %argslist53977$k474142)
ret void
falsebranch$cmp55265:
%ae50347 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55269 = alloca %struct.ScmObj*, align 8
%anf_45bind47290 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50347)
store volatile %struct.ScmObj* %anf_45bind47290, %struct.ScmObj** %stackaddr$prim55269, align 8
%stackaddr$prim55270 = alloca %struct.ScmObj*, align 8
%anf_45bind47291 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47290)
store volatile %struct.ScmObj* %anf_45bind47291, %struct.ScmObj** %stackaddr$prim55270, align 8
%stackaddr$prim55271 = alloca %struct.ScmObj*, align 8
%anf_45bind47292 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind47291, %struct.ScmObj* %v47139)
store volatile %struct.ScmObj* %anf_45bind47292, %struct.ScmObj** %stackaddr$prim55271, align 8
%truthy$cmp55272 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47292)
%cmp$cmp55272 = icmp eq i64 %truthy$cmp55272, 1
br i1 %cmp$cmp55272, label %truebranch$cmp55272, label %falsebranch$cmp55272
truebranch$cmp55272:
%ae50353 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55273 = alloca %struct.ScmObj*, align 8
%cpsprim47417 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50353)
store volatile %struct.ScmObj* %cpsprim47417, %struct.ScmObj** %stackaddr$prim55273, align 8
%ae50355 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53978$k474140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55274 = alloca %struct.ScmObj*, align 8
%argslist53978$k474141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47417, %struct.ScmObj* %argslist53978$k474140)
store volatile %struct.ScmObj* %argslist53978$k474141, %struct.ScmObj** %stackaddr$prim55274, align 8
%stackaddr$prim55275 = alloca %struct.ScmObj*, align 8
%argslist53978$k474142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50355, %struct.ScmObj* %argslist53978$k474141)
store volatile %struct.ScmObj* %argslist53978$k474142, %struct.ScmObj** %stackaddr$prim55275, align 8
%clofunc55276 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47414)
musttail call tailcc void %clofunc55276(%struct.ScmObj* %k47414, %struct.ScmObj* %argslist53978$k474142)
ret void
falsebranch$cmp55272:
%ae50366 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55277 = alloca %struct.ScmObj*, align 8
%anf_45bind47293 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50366)
store volatile %struct.ScmObj* %anf_45bind47293, %struct.ScmObj** %stackaddr$prim55277, align 8
%stackaddr$prim55278 = alloca %struct.ScmObj*, align 8
%anf_45bind47294 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47293)
store volatile %struct.ScmObj* %anf_45bind47294, %struct.ScmObj** %stackaddr$prim55278, align 8
%ae50369 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55279 = alloca %struct.ScmObj*, align 8
%_95047143 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47140, %struct.ScmObj* %ae50369, %struct.ScmObj* %anf_45bind47294)
store volatile %struct.ScmObj* %_95047143, %struct.ScmObj** %stackaddr$prim55279, align 8
%argslist53979$cc471410 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55280 = alloca %struct.ScmObj*, align 8
%argslist53979$cc471411 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47141, %struct.ScmObj* %argslist53979$cc471410)
store volatile %struct.ScmObj* %argslist53979$cc471411, %struct.ScmObj** %stackaddr$prim55280, align 8
%stackaddr$prim55281 = alloca %struct.ScmObj*, align 8
%argslist53979$cc471412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47414, %struct.ScmObj* %argslist53979$cc471411)
store volatile %struct.ScmObj* %argslist53979$cc471412, %struct.ScmObj** %stackaddr$prim55281, align 8
%clofunc55282 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47141)
musttail call tailcc void %clofunc55282(%struct.ScmObj* %cc47141, %struct.ScmObj* %argslist53979$cc471412)
ret void
}

define tailcc void @proc_clo$ae50317(%struct.ScmObj* %env$ae50317,%struct.ScmObj* %current_45args53981) {
%stackaddr$prim55283 = alloca %struct.ScmObj*, align 8
%k47418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53981)
store volatile %struct.ScmObj* %k47418, %struct.ScmObj** %stackaddr$prim55283, align 8
%stackaddr$prim55284 = alloca %struct.ScmObj*, align 8
%current_45args53982 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53981)
store volatile %struct.ScmObj* %current_45args53982, %struct.ScmObj** %stackaddr$prim55284, align 8
%stackaddr$prim55285 = alloca %struct.ScmObj*, align 8
%u47142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53982)
store volatile %struct.ScmObj* %u47142, %struct.ScmObj** %stackaddr$prim55285, align 8
%argslist53984$u471420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55286 = alloca %struct.ScmObj*, align 8
%argslist53984$u471421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47142, %struct.ScmObj* %argslist53984$u471420)
store volatile %struct.ScmObj* %argslist53984$u471421, %struct.ScmObj** %stackaddr$prim55286, align 8
%stackaddr$prim55287 = alloca %struct.ScmObj*, align 8
%argslist53984$u471422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47418, %struct.ScmObj* %argslist53984$u471421)
store volatile %struct.ScmObj* %argslist53984$u471422, %struct.ScmObj** %stackaddr$prim55287, align 8
%clofunc55288 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47142)
musttail call tailcc void %clofunc55288(%struct.ScmObj* %u47142, %struct.ScmObj* %argslist53984$u471422)
ret void
}

define tailcc void @proc_clo$ae49776(%struct.ScmObj* %env$ae49776,%struct.ScmObj* %current_45args53987) {
%stackaddr$prim55289 = alloca %struct.ScmObj*, align 8
%k47419 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53987)
store volatile %struct.ScmObj* %k47419, %struct.ScmObj** %stackaddr$prim55289, align 8
%stackaddr$prim55290 = alloca %struct.ScmObj*, align 8
%current_45args53988 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53987)
store volatile %struct.ScmObj* %current_45args53988, %struct.ScmObj** %stackaddr$prim55290, align 8
%stackaddr$prim55291 = alloca %struct.ScmObj*, align 8
%lst47146 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53988)
store volatile %struct.ScmObj* %lst47146, %struct.ScmObj** %stackaddr$prim55291, align 8
%stackaddr$prim55292 = alloca %struct.ScmObj*, align 8
%current_45args53989 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53988)
store volatile %struct.ScmObj* %current_45args53989, %struct.ScmObj** %stackaddr$prim55292, align 8
%stackaddr$prim55293 = alloca %struct.ScmObj*, align 8
%n47145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53989)
store volatile %struct.ScmObj* %n47145, %struct.ScmObj** %stackaddr$prim55293, align 8
%ae49777 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55294 = alloca %struct.ScmObj*, align 8
%n47148 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49777, %struct.ScmObj* %n47145)
store volatile %struct.ScmObj* %n47148, %struct.ScmObj** %stackaddr$prim55294, align 8
%ae49779 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55295 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49779, %struct.ScmObj* %lst47146)
store volatile %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$prim55295, align 8
%stackaddr$makeclosure55296 = alloca %struct.ScmObj*, align 8
%fptrToInt55297 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49781 to i64
%ae49781 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55297)
store volatile %struct.ScmObj* %ae49781, %struct.ScmObj** %stackaddr$makeclosure55296, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49781, %struct.ScmObj* %k47419, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49781, %struct.ScmObj* %lst47147, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49781, %struct.ScmObj* %n47148, i64 2)
%ae49782 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55298 = alloca %struct.ScmObj*, align 8
%fptrToInt55299 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49783 to i64
%ae49783 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55299)
store volatile %struct.ScmObj* %ae49783, %struct.ScmObj** %stackaddr$makeclosure55298, align 8
%argslist54009$ae497810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55300 = alloca %struct.ScmObj*, align 8
%argslist54009$ae497811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49783, %struct.ScmObj* %argslist54009$ae497810)
store volatile %struct.ScmObj* %argslist54009$ae497811, %struct.ScmObj** %stackaddr$prim55300, align 8
%stackaddr$prim55301 = alloca %struct.ScmObj*, align 8
%argslist54009$ae497812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49782, %struct.ScmObj* %argslist54009$ae497811)
store volatile %struct.ScmObj* %argslist54009$ae497812, %struct.ScmObj** %stackaddr$prim55301, align 8
%clofunc55302 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49781)
musttail call tailcc void %clofunc55302(%struct.ScmObj* %ae49781, %struct.ScmObj* %argslist54009$ae497812)
ret void
}

define tailcc void @proc_clo$ae49781(%struct.ScmObj* %env$ae49781,%struct.ScmObj* %current_45args53991) {
%stackaddr$env-ref55303 = alloca %struct.ScmObj*, align 8
%k47419 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49781, i64 0)
store %struct.ScmObj* %k47419, %struct.ScmObj** %stackaddr$env-ref55303
%stackaddr$env-ref55304 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49781, i64 1)
store %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$env-ref55304
%stackaddr$env-ref55305 = alloca %struct.ScmObj*, align 8
%n47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49781, i64 2)
store %struct.ScmObj* %n47148, %struct.ScmObj** %stackaddr$env-ref55305
%stackaddr$prim55306 = alloca %struct.ScmObj*, align 8
%_95k47420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53991)
store volatile %struct.ScmObj* %_95k47420, %struct.ScmObj** %stackaddr$prim55306, align 8
%stackaddr$prim55307 = alloca %struct.ScmObj*, align 8
%current_45args53992 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53991)
store volatile %struct.ScmObj* %current_45args53992, %struct.ScmObj** %stackaddr$prim55307, align 8
%stackaddr$prim55308 = alloca %struct.ScmObj*, align 8
%anf_45bind47280 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53992)
store volatile %struct.ScmObj* %anf_45bind47280, %struct.ScmObj** %stackaddr$prim55308, align 8
%stackaddr$makeclosure55309 = alloca %struct.ScmObj*, align 8
%fptrToInt55310 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49797 to i64
%ae49797 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55310)
store volatile %struct.ScmObj* %ae49797, %struct.ScmObj** %stackaddr$makeclosure55309, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49797, %struct.ScmObj* %k47419, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49797, %struct.ScmObj* %lst47147, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49797, %struct.ScmObj* %n47148, i64 2)
%stackaddr$makeclosure55311 = alloca %struct.ScmObj*, align 8
%fptrToInt55312 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49798 to i64
%ae49798 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55312)
store volatile %struct.ScmObj* %ae49798, %struct.ScmObj** %stackaddr$makeclosure55311, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49798, %struct.ScmObj* %k47419, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49798, %struct.ScmObj* %lst47147, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49798, %struct.ScmObj* %n47148, i64 2)
%argslist54004$anf_45bind472800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55313 = alloca %struct.ScmObj*, align 8
%argslist54004$anf_45bind472801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49798, %struct.ScmObj* %argslist54004$anf_45bind472800)
store volatile %struct.ScmObj* %argslist54004$anf_45bind472801, %struct.ScmObj** %stackaddr$prim55313, align 8
%stackaddr$prim55314 = alloca %struct.ScmObj*, align 8
%argslist54004$anf_45bind472802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49797, %struct.ScmObj* %argslist54004$anf_45bind472801)
store volatile %struct.ScmObj* %argslist54004$anf_45bind472802, %struct.ScmObj** %stackaddr$prim55314, align 8
%clofunc55315 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47280)
musttail call tailcc void %clofunc55315(%struct.ScmObj* %anf_45bind47280, %struct.ScmObj* %argslist54004$anf_45bind472802)
ret void
}

define tailcc void @proc_clo$ae49797(%struct.ScmObj* %env$ae49797,%struct.ScmObj* %current_45args53994) {
%stackaddr$env-ref55316 = alloca %struct.ScmObj*, align 8
%k47419 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49797, i64 0)
store %struct.ScmObj* %k47419, %struct.ScmObj** %stackaddr$env-ref55316
%stackaddr$env-ref55317 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49797, i64 1)
store %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$env-ref55317
%stackaddr$env-ref55318 = alloca %struct.ScmObj*, align 8
%n47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49797, i64 2)
store %struct.ScmObj* %n47148, %struct.ScmObj** %stackaddr$env-ref55318
%stackaddr$prim55319 = alloca %struct.ScmObj*, align 8
%_95k47421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53994)
store volatile %struct.ScmObj* %_95k47421, %struct.ScmObj** %stackaddr$prim55319, align 8
%stackaddr$prim55320 = alloca %struct.ScmObj*, align 8
%current_45args53995 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53994)
store volatile %struct.ScmObj* %current_45args53995, %struct.ScmObj** %stackaddr$prim55320, align 8
%stackaddr$prim55321 = alloca %struct.ScmObj*, align 8
%cc47149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53995)
store volatile %struct.ScmObj* %cc47149, %struct.ScmObj** %stackaddr$prim55321, align 8
%ae49940 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55322 = alloca %struct.ScmObj*, align 8
%anf_45bind47281 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49940)
store volatile %struct.ScmObj* %anf_45bind47281, %struct.ScmObj** %stackaddr$prim55322, align 8
%ae49941 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55323 = alloca %struct.ScmObj*, align 8
%anf_45bind47282 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49941, %struct.ScmObj* %anf_45bind47281)
store volatile %struct.ScmObj* %anf_45bind47282, %struct.ScmObj** %stackaddr$prim55323, align 8
%truthy$cmp55324 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47282)
%cmp$cmp55324 = icmp eq i64 %truthy$cmp55324, 1
br i1 %cmp$cmp55324, label %truebranch$cmp55324, label %falsebranch$cmp55324
truebranch$cmp55324:
%ae49945 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55325 = alloca %struct.ScmObj*, align 8
%cpsprim47422 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49945)
store volatile %struct.ScmObj* %cpsprim47422, %struct.ScmObj** %stackaddr$prim55325, align 8
%ae49947 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53997$k474190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55326 = alloca %struct.ScmObj*, align 8
%argslist53997$k474191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47422, %struct.ScmObj* %argslist53997$k474190)
store volatile %struct.ScmObj* %argslist53997$k474191, %struct.ScmObj** %stackaddr$prim55326, align 8
%stackaddr$prim55327 = alloca %struct.ScmObj*, align 8
%argslist53997$k474192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49947, %struct.ScmObj* %argslist53997$k474191)
store volatile %struct.ScmObj* %argslist53997$k474192, %struct.ScmObj** %stackaddr$prim55327, align 8
%clofunc55328 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47419)
musttail call tailcc void %clofunc55328(%struct.ScmObj* %k47419, %struct.ScmObj* %argslist53997$k474192)
ret void
falsebranch$cmp55324:
%ae49958 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55329 = alloca %struct.ScmObj*, align 8
%anf_45bind47283 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49958)
store volatile %struct.ScmObj* %anf_45bind47283, %struct.ScmObj** %stackaddr$prim55329, align 8
%stackaddr$prim55330 = alloca %struct.ScmObj*, align 8
%anf_45bind47284 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47283)
store volatile %struct.ScmObj* %anf_45bind47284, %struct.ScmObj** %stackaddr$prim55330, align 8
%ae49961 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55331 = alloca %struct.ScmObj*, align 8
%_95047152 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49961, %struct.ScmObj* %anf_45bind47284)
store volatile %struct.ScmObj* %_95047152, %struct.ScmObj** %stackaddr$prim55331, align 8
%ae49964 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55332 = alloca %struct.ScmObj*, align 8
%anf_45bind47285 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49964)
store volatile %struct.ScmObj* %anf_45bind47285, %struct.ScmObj** %stackaddr$prim55332, align 8
%ae49966 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55333 = alloca %struct.ScmObj*, align 8
%anf_45bind47286 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47285, %struct.ScmObj* %ae49966)
store volatile %struct.ScmObj* %anf_45bind47286, %struct.ScmObj** %stackaddr$prim55333, align 8
%ae49968 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55334 = alloca %struct.ScmObj*, align 8
%_95147151 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49968, %struct.ScmObj* %anf_45bind47286)
store volatile %struct.ScmObj* %_95147151, %struct.ScmObj** %stackaddr$prim55334, align 8
%argslist53998$cc471490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55335 = alloca %struct.ScmObj*, align 8
%argslist53998$cc471491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist53998$cc471490)
store volatile %struct.ScmObj* %argslist53998$cc471491, %struct.ScmObj** %stackaddr$prim55335, align 8
%stackaddr$prim55336 = alloca %struct.ScmObj*, align 8
%argslist53998$cc471492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47419, %struct.ScmObj* %argslist53998$cc471491)
store volatile %struct.ScmObj* %argslist53998$cc471492, %struct.ScmObj** %stackaddr$prim55336, align 8
%clofunc55337 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47149)
musttail call tailcc void %clofunc55337(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist53998$cc471492)
ret void
}

define tailcc void @proc_clo$ae49798(%struct.ScmObj* %env$ae49798,%struct.ScmObj* %current_45args53999) {
%stackaddr$env-ref55338 = alloca %struct.ScmObj*, align 8
%k47419 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49798, i64 0)
store %struct.ScmObj* %k47419, %struct.ScmObj** %stackaddr$env-ref55338
%stackaddr$env-ref55339 = alloca %struct.ScmObj*, align 8
%lst47147 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49798, i64 1)
store %struct.ScmObj* %lst47147, %struct.ScmObj** %stackaddr$env-ref55339
%stackaddr$env-ref55340 = alloca %struct.ScmObj*, align 8
%n47148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49798, i64 2)
store %struct.ScmObj* %n47148, %struct.ScmObj** %stackaddr$env-ref55340
%stackaddr$prim55341 = alloca %struct.ScmObj*, align 8
%_95k47421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53999)
store volatile %struct.ScmObj* %_95k47421, %struct.ScmObj** %stackaddr$prim55341, align 8
%stackaddr$prim55342 = alloca %struct.ScmObj*, align 8
%current_45args54000 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53999)
store volatile %struct.ScmObj* %current_45args54000, %struct.ScmObj** %stackaddr$prim55342, align 8
%stackaddr$prim55343 = alloca %struct.ScmObj*, align 8
%cc47149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54000)
store volatile %struct.ScmObj* %cc47149, %struct.ScmObj** %stackaddr$prim55343, align 8
%ae49800 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55344 = alloca %struct.ScmObj*, align 8
%anf_45bind47281 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49800)
store volatile %struct.ScmObj* %anf_45bind47281, %struct.ScmObj** %stackaddr$prim55344, align 8
%ae49801 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55345 = alloca %struct.ScmObj*, align 8
%anf_45bind47282 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae49801, %struct.ScmObj* %anf_45bind47281)
store volatile %struct.ScmObj* %anf_45bind47282, %struct.ScmObj** %stackaddr$prim55345, align 8
%truthy$cmp55346 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47282)
%cmp$cmp55346 = icmp eq i64 %truthy$cmp55346, 1
br i1 %cmp$cmp55346, label %truebranch$cmp55346, label %falsebranch$cmp55346
truebranch$cmp55346:
%ae49805 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55347 = alloca %struct.ScmObj*, align 8
%cpsprim47422 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49805)
store volatile %struct.ScmObj* %cpsprim47422, %struct.ScmObj** %stackaddr$prim55347, align 8
%ae49807 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54002$k474190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55348 = alloca %struct.ScmObj*, align 8
%argslist54002$k474191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47422, %struct.ScmObj* %argslist54002$k474190)
store volatile %struct.ScmObj* %argslist54002$k474191, %struct.ScmObj** %stackaddr$prim55348, align 8
%stackaddr$prim55349 = alloca %struct.ScmObj*, align 8
%argslist54002$k474192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49807, %struct.ScmObj* %argslist54002$k474191)
store volatile %struct.ScmObj* %argslist54002$k474192, %struct.ScmObj** %stackaddr$prim55349, align 8
%clofunc55350 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47419)
musttail call tailcc void %clofunc55350(%struct.ScmObj* %k47419, %struct.ScmObj* %argslist54002$k474192)
ret void
falsebranch$cmp55346:
%ae49818 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55351 = alloca %struct.ScmObj*, align 8
%anf_45bind47283 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49818)
store volatile %struct.ScmObj* %anf_45bind47283, %struct.ScmObj** %stackaddr$prim55351, align 8
%stackaddr$prim55352 = alloca %struct.ScmObj*, align 8
%anf_45bind47284 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47283)
store volatile %struct.ScmObj* %anf_45bind47284, %struct.ScmObj** %stackaddr$prim55352, align 8
%ae49821 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55353 = alloca %struct.ScmObj*, align 8
%_95047152 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst47147, %struct.ScmObj* %ae49821, %struct.ScmObj* %anf_45bind47284)
store volatile %struct.ScmObj* %_95047152, %struct.ScmObj** %stackaddr$prim55353, align 8
%ae49824 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55354 = alloca %struct.ScmObj*, align 8
%anf_45bind47285 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49824)
store volatile %struct.ScmObj* %anf_45bind47285, %struct.ScmObj** %stackaddr$prim55354, align 8
%ae49826 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55355 = alloca %struct.ScmObj*, align 8
%anf_45bind47286 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47285, %struct.ScmObj* %ae49826)
store volatile %struct.ScmObj* %anf_45bind47286, %struct.ScmObj** %stackaddr$prim55355, align 8
%ae49828 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55356 = alloca %struct.ScmObj*, align 8
%_95147151 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n47148, %struct.ScmObj* %ae49828, %struct.ScmObj* %anf_45bind47286)
store volatile %struct.ScmObj* %_95147151, %struct.ScmObj** %stackaddr$prim55356, align 8
%argslist54003$cc471490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55357 = alloca %struct.ScmObj*, align 8
%argslist54003$cc471491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist54003$cc471490)
store volatile %struct.ScmObj* %argslist54003$cc471491, %struct.ScmObj** %stackaddr$prim55357, align 8
%stackaddr$prim55358 = alloca %struct.ScmObj*, align 8
%argslist54003$cc471492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47419, %struct.ScmObj* %argslist54003$cc471491)
store volatile %struct.ScmObj* %argslist54003$cc471492, %struct.ScmObj** %stackaddr$prim55358, align 8
%clofunc55359 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47149)
musttail call tailcc void %clofunc55359(%struct.ScmObj* %cc47149, %struct.ScmObj* %argslist54003$cc471492)
ret void
}

define tailcc void @proc_clo$ae49783(%struct.ScmObj* %env$ae49783,%struct.ScmObj* %current_45args54005) {
%stackaddr$prim55360 = alloca %struct.ScmObj*, align 8
%k47423 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54005)
store volatile %struct.ScmObj* %k47423, %struct.ScmObj** %stackaddr$prim55360, align 8
%stackaddr$prim55361 = alloca %struct.ScmObj*, align 8
%current_45args54006 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54005)
store volatile %struct.ScmObj* %current_45args54006, %struct.ScmObj** %stackaddr$prim55361, align 8
%stackaddr$prim55362 = alloca %struct.ScmObj*, align 8
%u47150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54006)
store volatile %struct.ScmObj* %u47150, %struct.ScmObj** %stackaddr$prim55362, align 8
%argslist54008$u471500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55363 = alloca %struct.ScmObj*, align 8
%argslist54008$u471501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u47150, %struct.ScmObj* %argslist54008$u471500)
store volatile %struct.ScmObj* %argslist54008$u471501, %struct.ScmObj** %stackaddr$prim55363, align 8
%stackaddr$prim55364 = alloca %struct.ScmObj*, align 8
%argslist54008$u471502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47423, %struct.ScmObj* %argslist54008$u471501)
store volatile %struct.ScmObj* %argslist54008$u471502, %struct.ScmObj** %stackaddr$prim55364, align 8
%clofunc55365 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u47150)
musttail call tailcc void %clofunc55365(%struct.ScmObj* %u47150, %struct.ScmObj* %argslist54008$u471502)
ret void
}

define tailcc void @proc_clo$ae49360(%struct.ScmObj* %env$ae49360,%struct.ScmObj* %current_45args54011) {
%stackaddr$prim55366 = alloca %struct.ScmObj*, align 8
%k47424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54011)
store volatile %struct.ScmObj* %k47424, %struct.ScmObj** %stackaddr$prim55366, align 8
%stackaddr$prim55367 = alloca %struct.ScmObj*, align 8
%current_45args54012 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54011)
store volatile %struct.ScmObj* %current_45args54012, %struct.ScmObj** %stackaddr$prim55367, align 8
%stackaddr$prim55368 = alloca %struct.ScmObj*, align 8
%a47154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54012)
store volatile %struct.ScmObj* %a47154, %struct.ScmObj** %stackaddr$prim55368, align 8
%ae49361 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55369 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae49361, %struct.ScmObj* %a47154)
store volatile %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$prim55369, align 8
%stackaddr$makeclosure55370 = alloca %struct.ScmObj*, align 8
%fptrToInt55371 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49363 to i64
%ae49363 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55371)
store volatile %struct.ScmObj* %ae49363, %struct.ScmObj** %stackaddr$makeclosure55370, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49363, %struct.ScmObj* %a47155, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49363, %struct.ScmObj* %k47424, i64 1)
%ae49364 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55372 = alloca %struct.ScmObj*, align 8
%fptrToInt55373 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49365 to i64
%ae49365 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55373)
store volatile %struct.ScmObj* %ae49365, %struct.ScmObj** %stackaddr$makeclosure55372, align 8
%argslist54034$ae493630 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55374 = alloca %struct.ScmObj*, align 8
%argslist54034$ae493631 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49365, %struct.ScmObj* %argslist54034$ae493630)
store volatile %struct.ScmObj* %argslist54034$ae493631, %struct.ScmObj** %stackaddr$prim55374, align 8
%stackaddr$prim55375 = alloca %struct.ScmObj*, align 8
%argslist54034$ae493632 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49364, %struct.ScmObj* %argslist54034$ae493631)
store volatile %struct.ScmObj* %argslist54034$ae493632, %struct.ScmObj** %stackaddr$prim55375, align 8
%clofunc55376 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49363)
musttail call tailcc void %clofunc55376(%struct.ScmObj* %ae49363, %struct.ScmObj* %argslist54034$ae493632)
ret void
}

define tailcc void @proc_clo$ae49363(%struct.ScmObj* %env$ae49363,%struct.ScmObj* %current_45args54014) {
%stackaddr$env-ref55377 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49363, i64 0)
store %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$env-ref55377
%stackaddr$env-ref55378 = alloca %struct.ScmObj*, align 8
%k47424 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49363, i64 1)
store %struct.ScmObj* %k47424, %struct.ScmObj** %stackaddr$env-ref55378
%stackaddr$prim55379 = alloca %struct.ScmObj*, align 8
%_95k47425 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54014)
store volatile %struct.ScmObj* %_95k47425, %struct.ScmObj** %stackaddr$prim55379, align 8
%stackaddr$prim55380 = alloca %struct.ScmObj*, align 8
%current_45args54015 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54014)
store volatile %struct.ScmObj* %current_45args54015, %struct.ScmObj** %stackaddr$prim55380, align 8
%stackaddr$prim55381 = alloca %struct.ScmObj*, align 8
%anf_45bind47272 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54015)
store volatile %struct.ScmObj* %anf_45bind47272, %struct.ScmObj** %stackaddr$prim55381, align 8
%stackaddr$makeclosure55382 = alloca %struct.ScmObj*, align 8
%fptrToInt55383 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49382 to i64
%ae49382 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55383)
store volatile %struct.ScmObj* %ae49382, %struct.ScmObj** %stackaddr$makeclosure55382, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49382, %struct.ScmObj* %a47155, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49382, %struct.ScmObj* %k47424, i64 1)
%stackaddr$makeclosure55384 = alloca %struct.ScmObj*, align 8
%fptrToInt55385 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49383 to i64
%ae49383 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55385)
store volatile %struct.ScmObj* %ae49383, %struct.ScmObj** %stackaddr$makeclosure55384, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49383, %struct.ScmObj* %a47155, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49383, %struct.ScmObj* %k47424, i64 1)
%argslist54029$anf_45bind472720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55386 = alloca %struct.ScmObj*, align 8
%argslist54029$anf_45bind472721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49383, %struct.ScmObj* %argslist54029$anf_45bind472720)
store volatile %struct.ScmObj* %argslist54029$anf_45bind472721, %struct.ScmObj** %stackaddr$prim55386, align 8
%stackaddr$prim55387 = alloca %struct.ScmObj*, align 8
%argslist54029$anf_45bind472722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49382, %struct.ScmObj* %argslist54029$anf_45bind472721)
store volatile %struct.ScmObj* %argslist54029$anf_45bind472722, %struct.ScmObj** %stackaddr$prim55387, align 8
%clofunc55388 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47272)
musttail call tailcc void %clofunc55388(%struct.ScmObj* %anf_45bind47272, %struct.ScmObj* %argslist54029$anf_45bind472722)
ret void
}

define tailcc void @proc_clo$ae49382(%struct.ScmObj* %env$ae49382,%struct.ScmObj* %current_45args54017) {
%stackaddr$env-ref55389 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49382, i64 0)
store %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$env-ref55389
%stackaddr$env-ref55390 = alloca %struct.ScmObj*, align 8
%k47424 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49382, i64 1)
store %struct.ScmObj* %k47424, %struct.ScmObj** %stackaddr$env-ref55390
%stackaddr$prim55391 = alloca %struct.ScmObj*, align 8
%_95k47426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54017)
store volatile %struct.ScmObj* %_95k47426, %struct.ScmObj** %stackaddr$prim55391, align 8
%stackaddr$prim55392 = alloca %struct.ScmObj*, align 8
%current_45args54018 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54017)
store volatile %struct.ScmObj* %current_45args54018, %struct.ScmObj** %stackaddr$prim55392, align 8
%stackaddr$prim55393 = alloca %struct.ScmObj*, align 8
%cc47156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54018)
store volatile %struct.ScmObj* %cc47156, %struct.ScmObj** %stackaddr$prim55393, align 8
%ae49498 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55394 = alloca %struct.ScmObj*, align 8
%anf_45bind47273 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49498)
store volatile %struct.ScmObj* %anf_45bind47273, %struct.ScmObj** %stackaddr$prim55394, align 8
%stackaddr$prim55395 = alloca %struct.ScmObj*, align 8
%anf_45bind47274 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47273)
store volatile %struct.ScmObj* %anf_45bind47274, %struct.ScmObj** %stackaddr$prim55395, align 8
%truthy$cmp55396 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47274)
%cmp$cmp55396 = icmp eq i64 %truthy$cmp55396, 1
br i1 %cmp$cmp55396, label %truebranch$cmp55396, label %falsebranch$cmp55396
truebranch$cmp55396:
%ae49502 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49503 = call %struct.ScmObj* @const_init_true()
%argslist54020$k474240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55397 = alloca %struct.ScmObj*, align 8
%argslist54020$k474241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49503, %struct.ScmObj* %argslist54020$k474240)
store volatile %struct.ScmObj* %argslist54020$k474241, %struct.ScmObj** %stackaddr$prim55397, align 8
%stackaddr$prim55398 = alloca %struct.ScmObj*, align 8
%argslist54020$k474242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49502, %struct.ScmObj* %argslist54020$k474241)
store volatile %struct.ScmObj* %argslist54020$k474242, %struct.ScmObj** %stackaddr$prim55398, align 8
%clofunc55399 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47424)
musttail call tailcc void %clofunc55399(%struct.ScmObj* %k47424, %struct.ScmObj* %argslist54020$k474242)
ret void
falsebranch$cmp55396:
%ae49511 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55400 = alloca %struct.ScmObj*, align 8
%anf_45bind47275 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49511)
store volatile %struct.ScmObj* %anf_45bind47275, %struct.ScmObj** %stackaddr$prim55400, align 8
%stackaddr$prim55401 = alloca %struct.ScmObj*, align 8
%anf_45bind47276 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47275)
store volatile %struct.ScmObj* %anf_45bind47276, %struct.ScmObj** %stackaddr$prim55401, align 8
%truthy$cmp55402 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47276)
%cmp$cmp55402 = icmp eq i64 %truthy$cmp55402, 1
br i1 %cmp$cmp55402, label %truebranch$cmp55402, label %falsebranch$cmp55402
truebranch$cmp55402:
%ae49515 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55403 = alloca %struct.ScmObj*, align 8
%anf_45bind47277 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49515)
store volatile %struct.ScmObj* %anf_45bind47277, %struct.ScmObj** %stackaddr$prim55403, align 8
%stackaddr$prim55404 = alloca %struct.ScmObj*, align 8
%b47158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47277)
store volatile %struct.ScmObj* %b47158, %struct.ScmObj** %stackaddr$prim55404, align 8
%ae49518 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55405 = alloca %struct.ScmObj*, align 8
%anf_45bind47278 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49518)
store volatile %struct.ScmObj* %anf_45bind47278, %struct.ScmObj** %stackaddr$prim55405, align 8
%stackaddr$prim55406 = alloca %struct.ScmObj*, align 8
%anf_45bind47279 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47278)
store volatile %struct.ScmObj* %anf_45bind47279, %struct.ScmObj** %stackaddr$prim55406, align 8
%ae49521 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55407 = alloca %struct.ScmObj*, align 8
%_95047159 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49521, %struct.ScmObj* %anf_45bind47279)
store volatile %struct.ScmObj* %_95047159, %struct.ScmObj** %stackaddr$prim55407, align 8
%argslist54021$cc471560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55408 = alloca %struct.ScmObj*, align 8
%argslist54021$cc471561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist54021$cc471560)
store volatile %struct.ScmObj* %argslist54021$cc471561, %struct.ScmObj** %stackaddr$prim55408, align 8
%stackaddr$prim55409 = alloca %struct.ScmObj*, align 8
%argslist54021$cc471562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47424, %struct.ScmObj* %argslist54021$cc471561)
store volatile %struct.ScmObj* %argslist54021$cc471562, %struct.ScmObj** %stackaddr$prim55409, align 8
%clofunc55410 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47156)
musttail call tailcc void %clofunc55410(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist54021$cc471562)
ret void
falsebranch$cmp55402:
%ae49554 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49555 = call %struct.ScmObj* @const_init_false()
%argslist54022$k474240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55411 = alloca %struct.ScmObj*, align 8
%argslist54022$k474241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49555, %struct.ScmObj* %argslist54022$k474240)
store volatile %struct.ScmObj* %argslist54022$k474241, %struct.ScmObj** %stackaddr$prim55411, align 8
%stackaddr$prim55412 = alloca %struct.ScmObj*, align 8
%argslist54022$k474242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49554, %struct.ScmObj* %argslist54022$k474241)
store volatile %struct.ScmObj* %argslist54022$k474242, %struct.ScmObj** %stackaddr$prim55412, align 8
%clofunc55413 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47424)
musttail call tailcc void %clofunc55413(%struct.ScmObj* %k47424, %struct.ScmObj* %argslist54022$k474242)
ret void
}

define tailcc void @proc_clo$ae49383(%struct.ScmObj* %env$ae49383,%struct.ScmObj* %current_45args54023) {
%stackaddr$env-ref55414 = alloca %struct.ScmObj*, align 8
%a47155 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49383, i64 0)
store %struct.ScmObj* %a47155, %struct.ScmObj** %stackaddr$env-ref55414
%stackaddr$env-ref55415 = alloca %struct.ScmObj*, align 8
%k47424 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49383, i64 1)
store %struct.ScmObj* %k47424, %struct.ScmObj** %stackaddr$env-ref55415
%stackaddr$prim55416 = alloca %struct.ScmObj*, align 8
%_95k47426 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54023)
store volatile %struct.ScmObj* %_95k47426, %struct.ScmObj** %stackaddr$prim55416, align 8
%stackaddr$prim55417 = alloca %struct.ScmObj*, align 8
%current_45args54024 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54023)
store volatile %struct.ScmObj* %current_45args54024, %struct.ScmObj** %stackaddr$prim55417, align 8
%stackaddr$prim55418 = alloca %struct.ScmObj*, align 8
%cc47156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54024)
store volatile %struct.ScmObj* %cc47156, %struct.ScmObj** %stackaddr$prim55418, align 8
%ae49385 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55419 = alloca %struct.ScmObj*, align 8
%anf_45bind47273 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49385)
store volatile %struct.ScmObj* %anf_45bind47273, %struct.ScmObj** %stackaddr$prim55419, align 8
%stackaddr$prim55420 = alloca %struct.ScmObj*, align 8
%anf_45bind47274 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind47273)
store volatile %struct.ScmObj* %anf_45bind47274, %struct.ScmObj** %stackaddr$prim55420, align 8
%truthy$cmp55421 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47274)
%cmp$cmp55421 = icmp eq i64 %truthy$cmp55421, 1
br i1 %cmp$cmp55421, label %truebranch$cmp55421, label %falsebranch$cmp55421
truebranch$cmp55421:
%ae49389 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49390 = call %struct.ScmObj* @const_init_true()
%argslist54026$k474240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55422 = alloca %struct.ScmObj*, align 8
%argslist54026$k474241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49390, %struct.ScmObj* %argslist54026$k474240)
store volatile %struct.ScmObj* %argslist54026$k474241, %struct.ScmObj** %stackaddr$prim55422, align 8
%stackaddr$prim55423 = alloca %struct.ScmObj*, align 8
%argslist54026$k474242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49389, %struct.ScmObj* %argslist54026$k474241)
store volatile %struct.ScmObj* %argslist54026$k474242, %struct.ScmObj** %stackaddr$prim55423, align 8
%clofunc55424 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47424)
musttail call tailcc void %clofunc55424(%struct.ScmObj* %k47424, %struct.ScmObj* %argslist54026$k474242)
ret void
falsebranch$cmp55421:
%ae49398 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55425 = alloca %struct.ScmObj*, align 8
%anf_45bind47275 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49398)
store volatile %struct.ScmObj* %anf_45bind47275, %struct.ScmObj** %stackaddr$prim55425, align 8
%stackaddr$prim55426 = alloca %struct.ScmObj*, align 8
%anf_45bind47276 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind47275)
store volatile %struct.ScmObj* %anf_45bind47276, %struct.ScmObj** %stackaddr$prim55426, align 8
%truthy$cmp55427 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47276)
%cmp$cmp55427 = icmp eq i64 %truthy$cmp55427, 1
br i1 %cmp$cmp55427, label %truebranch$cmp55427, label %falsebranch$cmp55427
truebranch$cmp55427:
%ae49402 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55428 = alloca %struct.ScmObj*, align 8
%anf_45bind47277 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49402)
store volatile %struct.ScmObj* %anf_45bind47277, %struct.ScmObj** %stackaddr$prim55428, align 8
%stackaddr$prim55429 = alloca %struct.ScmObj*, align 8
%b47158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47277)
store volatile %struct.ScmObj* %b47158, %struct.ScmObj** %stackaddr$prim55429, align 8
%ae49405 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55430 = alloca %struct.ScmObj*, align 8
%anf_45bind47278 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49405)
store volatile %struct.ScmObj* %anf_45bind47278, %struct.ScmObj** %stackaddr$prim55430, align 8
%stackaddr$prim55431 = alloca %struct.ScmObj*, align 8
%anf_45bind47279 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47278)
store volatile %struct.ScmObj* %anf_45bind47279, %struct.ScmObj** %stackaddr$prim55431, align 8
%ae49408 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55432 = alloca %struct.ScmObj*, align 8
%_95047159 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a47155, %struct.ScmObj* %ae49408, %struct.ScmObj* %anf_45bind47279)
store volatile %struct.ScmObj* %_95047159, %struct.ScmObj** %stackaddr$prim55432, align 8
%argslist54027$cc471560 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55433 = alloca %struct.ScmObj*, align 8
%argslist54027$cc471561 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist54027$cc471560)
store volatile %struct.ScmObj* %argslist54027$cc471561, %struct.ScmObj** %stackaddr$prim55433, align 8
%stackaddr$prim55434 = alloca %struct.ScmObj*, align 8
%argslist54027$cc471562 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47424, %struct.ScmObj* %argslist54027$cc471561)
store volatile %struct.ScmObj* %argslist54027$cc471562, %struct.ScmObj** %stackaddr$prim55434, align 8
%clofunc55435 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc47156)
musttail call tailcc void %clofunc55435(%struct.ScmObj* %cc47156, %struct.ScmObj* %argslist54027$cc471562)
ret void
falsebranch$cmp55427:
%ae49441 = call %struct.ScmObj* @const_init_int(i64 0)
%ae49442 = call %struct.ScmObj* @const_init_false()
%argslist54028$k474240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55436 = alloca %struct.ScmObj*, align 8
%argslist54028$k474241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49442, %struct.ScmObj* %argslist54028$k474240)
store volatile %struct.ScmObj* %argslist54028$k474241, %struct.ScmObj** %stackaddr$prim55436, align 8
%stackaddr$prim55437 = alloca %struct.ScmObj*, align 8
%argslist54028$k474242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49441, %struct.ScmObj* %argslist54028$k474241)
store volatile %struct.ScmObj* %argslist54028$k474242, %struct.ScmObj** %stackaddr$prim55437, align 8
%clofunc55438 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47424)
musttail call tailcc void %clofunc55438(%struct.ScmObj* %k47424, %struct.ScmObj* %argslist54028$k474242)
ret void
}

define tailcc void @proc_clo$ae49365(%struct.ScmObj* %env$ae49365,%struct.ScmObj* %current_45args54030) {
%stackaddr$prim55439 = alloca %struct.ScmObj*, align 8
%k47427 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54030)
store volatile %struct.ScmObj* %k47427, %struct.ScmObj** %stackaddr$prim55439, align 8
%stackaddr$prim55440 = alloca %struct.ScmObj*, align 8
%current_45args54031 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54030)
store volatile %struct.ScmObj* %current_45args54031, %struct.ScmObj** %stackaddr$prim55440, align 8
%stackaddr$prim55441 = alloca %struct.ScmObj*, align 8
%k47157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54031)
store volatile %struct.ScmObj* %k47157, %struct.ScmObj** %stackaddr$prim55441, align 8
%ae49367 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54033$k474270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55442 = alloca %struct.ScmObj*, align 8
%argslist54033$k474271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47157, %struct.ScmObj* %argslist54033$k474270)
store volatile %struct.ScmObj* %argslist54033$k474271, %struct.ScmObj** %stackaddr$prim55442, align 8
%stackaddr$prim55443 = alloca %struct.ScmObj*, align 8
%argslist54033$k474272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49367, %struct.ScmObj* %argslist54033$k474271)
store volatile %struct.ScmObj* %argslist54033$k474272, %struct.ScmObj** %stackaddr$prim55443, align 8
%clofunc55444 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47427)
musttail call tailcc void %clofunc55444(%struct.ScmObj* %k47427, %struct.ScmObj* %argslist54033$k474272)
ret void
}

define tailcc void @proc_clo$ae49288(%struct.ScmObj* %env$ae49288,%struct.ScmObj* %current_45args54036) {
%stackaddr$env-ref55445 = alloca %struct.ScmObj*, align 8
%_37append47161 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49288, i64 0)
store %struct.ScmObj* %_37append47161, %struct.ScmObj** %stackaddr$env-ref55445
%stackaddr$prim55446 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54036)
store volatile %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$prim55446, align 8
%stackaddr$prim55447 = alloca %struct.ScmObj*, align 8
%current_45args54037 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54036)
store volatile %struct.ScmObj* %current_45args54037, %struct.ScmObj** %stackaddr$prim55447, align 8
%stackaddr$prim55448 = alloca %struct.ScmObj*, align 8
%ls047164 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54037)
store volatile %struct.ScmObj* %ls047164, %struct.ScmObj** %stackaddr$prim55448, align 8
%stackaddr$prim55449 = alloca %struct.ScmObj*, align 8
%current_45args54038 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54037)
store volatile %struct.ScmObj* %current_45args54038, %struct.ScmObj** %stackaddr$prim55449, align 8
%stackaddr$prim55450 = alloca %struct.ScmObj*, align 8
%ls147163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54038)
store volatile %struct.ScmObj* %ls147163, %struct.ScmObj** %stackaddr$prim55450, align 8
%stackaddr$prim55451 = alloca %struct.ScmObj*, align 8
%anf_45bind47266 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls047164)
store volatile %struct.ScmObj* %anf_45bind47266, %struct.ScmObj** %stackaddr$prim55451, align 8
%truthy$cmp55452 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47266)
%cmp$cmp55452 = icmp eq i64 %truthy$cmp55452, 1
br i1 %cmp$cmp55452, label %truebranch$cmp55452, label %falsebranch$cmp55452
truebranch$cmp55452:
%ae49292 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54040$k474280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55453 = alloca %struct.ScmObj*, align 8
%argslist54040$k474281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147163, %struct.ScmObj* %argslist54040$k474280)
store volatile %struct.ScmObj* %argslist54040$k474281, %struct.ScmObj** %stackaddr$prim55453, align 8
%stackaddr$prim55454 = alloca %struct.ScmObj*, align 8
%argslist54040$k474282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49292, %struct.ScmObj* %argslist54040$k474281)
store volatile %struct.ScmObj* %argslist54040$k474282, %struct.ScmObj** %stackaddr$prim55454, align 8
%clofunc55455 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47428)
musttail call tailcc void %clofunc55455(%struct.ScmObj* %k47428, %struct.ScmObj* %argslist54040$k474282)
ret void
falsebranch$cmp55452:
%stackaddr$prim55456 = alloca %struct.ScmObj*, align 8
%anf_45bind47267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls047164)
store volatile %struct.ScmObj* %anf_45bind47267, %struct.ScmObj** %stackaddr$prim55456, align 8
%ae49299 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55457 = alloca %struct.ScmObj*, align 8
%anf_45bind47268 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append47161, %struct.ScmObj* %ae49299)
store volatile %struct.ScmObj* %anf_45bind47268, %struct.ScmObj** %stackaddr$prim55457, align 8
%stackaddr$prim55458 = alloca %struct.ScmObj*, align 8
%anf_45bind47269 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls047164)
store volatile %struct.ScmObj* %anf_45bind47269, %struct.ScmObj** %stackaddr$prim55458, align 8
%stackaddr$makeclosure55459 = alloca %struct.ScmObj*, align 8
%fptrToInt55460 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49302 to i64
%ae49302 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55460)
store volatile %struct.ScmObj* %ae49302, %struct.ScmObj** %stackaddr$makeclosure55459, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49302, %struct.ScmObj* %k47428, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49302, %struct.ScmObj* %anf_45bind47267, i64 1)
%argslist54045$anf_45bind472680 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55461 = alloca %struct.ScmObj*, align 8
%argslist54045$anf_45bind472681 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls147163, %struct.ScmObj* %argslist54045$anf_45bind472680)
store volatile %struct.ScmObj* %argslist54045$anf_45bind472681, %struct.ScmObj** %stackaddr$prim55461, align 8
%stackaddr$prim55462 = alloca %struct.ScmObj*, align 8
%argslist54045$anf_45bind472682 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47269, %struct.ScmObj* %argslist54045$anf_45bind472681)
store volatile %struct.ScmObj* %argslist54045$anf_45bind472682, %struct.ScmObj** %stackaddr$prim55462, align 8
%stackaddr$prim55463 = alloca %struct.ScmObj*, align 8
%argslist54045$anf_45bind472683 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49302, %struct.ScmObj* %argslist54045$anf_45bind472682)
store volatile %struct.ScmObj* %argslist54045$anf_45bind472683, %struct.ScmObj** %stackaddr$prim55463, align 8
%clofunc55464 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47268)
musttail call tailcc void %clofunc55464(%struct.ScmObj* %anf_45bind47268, %struct.ScmObj* %argslist54045$anf_45bind472683)
ret void
}

define tailcc void @proc_clo$ae49302(%struct.ScmObj* %env$ae49302,%struct.ScmObj* %current_45args54041) {
%stackaddr$env-ref55465 = alloca %struct.ScmObj*, align 8
%k47428 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49302, i64 0)
store %struct.ScmObj* %k47428, %struct.ScmObj** %stackaddr$env-ref55465
%stackaddr$env-ref55466 = alloca %struct.ScmObj*, align 8
%anf_45bind47267 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49302, i64 1)
store %struct.ScmObj* %anf_45bind47267, %struct.ScmObj** %stackaddr$env-ref55466
%stackaddr$prim55467 = alloca %struct.ScmObj*, align 8
%_95k47429 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54041)
store volatile %struct.ScmObj* %_95k47429, %struct.ScmObj** %stackaddr$prim55467, align 8
%stackaddr$prim55468 = alloca %struct.ScmObj*, align 8
%current_45args54042 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54041)
store volatile %struct.ScmObj* %current_45args54042, %struct.ScmObj** %stackaddr$prim55468, align 8
%stackaddr$prim55469 = alloca %struct.ScmObj*, align 8
%anf_45bind47270 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54042)
store volatile %struct.ScmObj* %anf_45bind47270, %struct.ScmObj** %stackaddr$prim55469, align 8
%stackaddr$prim55470 = alloca %struct.ScmObj*, align 8
%cpsprim47430 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47267, %struct.ScmObj* %anf_45bind47270)
store volatile %struct.ScmObj* %cpsprim47430, %struct.ScmObj** %stackaddr$prim55470, align 8
%ae49308 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54044$k474280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55471 = alloca %struct.ScmObj*, align 8
%argslist54044$k474281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47430, %struct.ScmObj* %argslist54044$k474280)
store volatile %struct.ScmObj* %argslist54044$k474281, %struct.ScmObj** %stackaddr$prim55471, align 8
%stackaddr$prim55472 = alloca %struct.ScmObj*, align 8
%argslist54044$k474282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49308, %struct.ScmObj* %argslist54044$k474281)
store volatile %struct.ScmObj* %argslist54044$k474282, %struct.ScmObj** %stackaddr$prim55472, align 8
%clofunc55473 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47428)
musttail call tailcc void %clofunc55473(%struct.ScmObj* %k47428, %struct.ScmObj* %argslist54044$k474282)
ret void
}

define tailcc void @proc_clo$ae49262(%struct.ScmObj* %env$ae49262,%struct.ScmObj* %current_45args54047) {
%stackaddr$prim55474 = alloca %struct.ScmObj*, align 8
%k47431 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54047)
store volatile %struct.ScmObj* %k47431, %struct.ScmObj** %stackaddr$prim55474, align 8
%stackaddr$prim55475 = alloca %struct.ScmObj*, align 8
%current_45args54048 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54047)
store volatile %struct.ScmObj* %current_45args54048, %struct.ScmObj** %stackaddr$prim55475, align 8
%stackaddr$prim55476 = alloca %struct.ScmObj*, align 8
%a47167 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54048)
store volatile %struct.ScmObj* %a47167, %struct.ScmObj** %stackaddr$prim55476, align 8
%stackaddr$prim55477 = alloca %struct.ScmObj*, align 8
%current_45args54049 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54048)
store volatile %struct.ScmObj* %current_45args54049, %struct.ScmObj** %stackaddr$prim55477, align 8
%stackaddr$prim55478 = alloca %struct.ScmObj*, align 8
%b47166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54049)
store volatile %struct.ScmObj* %b47166, %struct.ScmObj** %stackaddr$prim55478, align 8
%stackaddr$prim55479 = alloca %struct.ScmObj*, align 8
%anf_45bind47265 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a47167, %struct.ScmObj* %b47166)
store volatile %struct.ScmObj* %anf_45bind47265, %struct.ScmObj** %stackaddr$prim55479, align 8
%stackaddr$prim55480 = alloca %struct.ScmObj*, align 8
%cpsprim47432 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47265)
store volatile %struct.ScmObj* %cpsprim47432, %struct.ScmObj** %stackaddr$prim55480, align 8
%ae49267 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54051$k474310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55481 = alloca %struct.ScmObj*, align 8
%argslist54051$k474311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47432, %struct.ScmObj* %argslist54051$k474310)
store volatile %struct.ScmObj* %argslist54051$k474311, %struct.ScmObj** %stackaddr$prim55481, align 8
%stackaddr$prim55482 = alloca %struct.ScmObj*, align 8
%argslist54051$k474312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49267, %struct.ScmObj* %argslist54051$k474311)
store volatile %struct.ScmObj* %argslist54051$k474312, %struct.ScmObj** %stackaddr$prim55482, align 8
%clofunc55483 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47431)
musttail call tailcc void %clofunc55483(%struct.ScmObj* %k47431, %struct.ScmObj* %argslist54051$k474312)
ret void
}

define tailcc void @proc_clo$ae49238(%struct.ScmObj* %env$ae49238,%struct.ScmObj* %current_45args54053) {
%stackaddr$prim55484 = alloca %struct.ScmObj*, align 8
%k47433 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54053)
store volatile %struct.ScmObj* %k47433, %struct.ScmObj** %stackaddr$prim55484, align 8
%stackaddr$prim55485 = alloca %struct.ScmObj*, align 8
%current_45args54054 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54053)
store volatile %struct.ScmObj* %current_45args54054, %struct.ScmObj** %stackaddr$prim55485, align 8
%stackaddr$prim55486 = alloca %struct.ScmObj*, align 8
%a47170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54054)
store volatile %struct.ScmObj* %a47170, %struct.ScmObj** %stackaddr$prim55486, align 8
%stackaddr$prim55487 = alloca %struct.ScmObj*, align 8
%current_45args54055 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54054)
store volatile %struct.ScmObj* %current_45args54055, %struct.ScmObj** %stackaddr$prim55487, align 8
%stackaddr$prim55488 = alloca %struct.ScmObj*, align 8
%b47169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54055)
store volatile %struct.ScmObj* %b47169, %struct.ScmObj** %stackaddr$prim55488, align 8
%stackaddr$prim55489 = alloca %struct.ScmObj*, align 8
%anf_45bind47264 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a47170, %struct.ScmObj* %b47169)
store volatile %struct.ScmObj* %anf_45bind47264, %struct.ScmObj** %stackaddr$prim55489, align 8
%stackaddr$prim55490 = alloca %struct.ScmObj*, align 8
%cpsprim47434 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind47264)
store volatile %struct.ScmObj* %cpsprim47434, %struct.ScmObj** %stackaddr$prim55490, align 8
%ae49243 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54057$k474330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55491 = alloca %struct.ScmObj*, align 8
%argslist54057$k474331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47434, %struct.ScmObj* %argslist54057$k474330)
store volatile %struct.ScmObj* %argslist54057$k474331, %struct.ScmObj** %stackaddr$prim55491, align 8
%stackaddr$prim55492 = alloca %struct.ScmObj*, align 8
%argslist54057$k474332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49243, %struct.ScmObj* %argslist54057$k474331)
store volatile %struct.ScmObj* %argslist54057$k474332, %struct.ScmObj** %stackaddr$prim55492, align 8
%clofunc55493 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47433)
musttail call tailcc void %clofunc55493(%struct.ScmObj* %k47433, %struct.ScmObj* %argslist54057$k474332)
ret void
}

define tailcc void @proc_clo$ae48844(%struct.ScmObj* %env$ae48844,%struct.ScmObj* %current_45args54060) {
%stackaddr$env-ref55494 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48844, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55494
%stackaddr$env-ref55495 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48844, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55495
%stackaddr$env-ref55496 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48844, i64 2)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref55496
%stackaddr$prim55497 = alloca %struct.ScmObj*, align 8
%k47435 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54060)
store volatile %struct.ScmObj* %k47435, %struct.ScmObj** %stackaddr$prim55497, align 8
%stackaddr$prim55498 = alloca %struct.ScmObj*, align 8
%current_45args54061 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54060)
store volatile %struct.ScmObj* %current_45args54061, %struct.ScmObj** %stackaddr$prim55498, align 8
%stackaddr$prim55499 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54061)
store volatile %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$prim55499, align 8
%ae48846 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55500 = alloca %struct.ScmObj*, align 8
%fptrToInt55501 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48847 to i64
%ae48847 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55501)
store volatile %struct.ScmObj* %ae48847, %struct.ScmObj** %stackaddr$makeclosure55500, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48847, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48847, %struct.ScmObj* %_37foldl47172, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48847, %struct.ScmObj* %_37foldr147089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48847, %struct.ScmObj* %_37map147120, i64 3)
%argslist54118$k474350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55502 = alloca %struct.ScmObj*, align 8
%argslist54118$k474351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48847, %struct.ScmObj* %argslist54118$k474350)
store volatile %struct.ScmObj* %argslist54118$k474351, %struct.ScmObj** %stackaddr$prim55502, align 8
%stackaddr$prim55503 = alloca %struct.ScmObj*, align 8
%argslist54118$k474352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48846, %struct.ScmObj* %argslist54118$k474351)
store volatile %struct.ScmObj* %argslist54118$k474352, %struct.ScmObj** %stackaddr$prim55503, align 8
%clofunc55504 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47435)
musttail call tailcc void %clofunc55504(%struct.ScmObj* %k47435, %struct.ScmObj* %argslist54118$k474352)
ret void
}

define tailcc void @proc_clo$ae48847(%struct.ScmObj* %env$ae48847,%struct.ScmObj* %args4717347436) {
%stackaddr$env-ref55505 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48847, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55505
%stackaddr$env-ref55506 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48847, i64 1)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55506
%stackaddr$env-ref55507 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48847, i64 2)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55507
%stackaddr$env-ref55508 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48847, i64 3)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref55508
%stackaddr$prim55509 = alloca %struct.ScmObj*, align 8
%k47437 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4717347436)
store volatile %struct.ScmObj* %k47437, %struct.ScmObj** %stackaddr$prim55509, align 8
%stackaddr$prim55510 = alloca %struct.ScmObj*, align 8
%args47173 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4717347436)
store volatile %struct.ScmObj* %args47173, %struct.ScmObj** %stackaddr$prim55510, align 8
%stackaddr$prim55511 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47173)
store volatile %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$prim55511, align 8
%stackaddr$prim55512 = alloca %struct.ScmObj*, align 8
%anf_45bind47252 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47173)
store volatile %struct.ScmObj* %anf_45bind47252, %struct.ScmObj** %stackaddr$prim55512, align 8
%stackaddr$prim55513 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47252)
store volatile %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$prim55513, align 8
%stackaddr$prim55514 = alloca %struct.ScmObj*, align 8
%anf_45bind47253 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47173)
store volatile %struct.ScmObj* %anf_45bind47253, %struct.ScmObj** %stackaddr$prim55514, align 8
%stackaddr$prim55515 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47253)
store volatile %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$prim55515, align 8
%stackaddr$makeclosure55516 = alloca %struct.ScmObj*, align 8
%fptrToInt55517 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48855 to i64
%ae48855 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55517)
store volatile %struct.ScmObj* %ae48855, %struct.ScmObj** %stackaddr$makeclosure55516, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48855, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48855, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48855, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48855, %struct.ScmObj* %_37foldr147089, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48855, %struct.ScmObj* %_37map147120, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48855, %struct.ScmObj* %k47437, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48855, %struct.ScmObj* %f47176, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48855, %struct.ScmObj* %acc47175, i64 7)
%ae48856 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55518 = alloca %struct.ScmObj*, align 8
%fptrToInt55519 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48857 to i64
%ae48857 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55519)
store volatile %struct.ScmObj* %ae48857, %struct.ScmObj** %stackaddr$makeclosure55518, align 8
%argslist54117$ae488550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55520 = alloca %struct.ScmObj*, align 8
%argslist54117$ae488551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48857, %struct.ScmObj* %argslist54117$ae488550)
store volatile %struct.ScmObj* %argslist54117$ae488551, %struct.ScmObj** %stackaddr$prim55520, align 8
%stackaddr$prim55521 = alloca %struct.ScmObj*, align 8
%argslist54117$ae488552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48856, %struct.ScmObj* %argslist54117$ae488551)
store volatile %struct.ScmObj* %argslist54117$ae488552, %struct.ScmObj** %stackaddr$prim55521, align 8
%clofunc55522 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48855)
musttail call tailcc void %clofunc55522(%struct.ScmObj* %ae48855, %struct.ScmObj* %argslist54117$ae488552)
ret void
}

define tailcc void @proc_clo$ae48855(%struct.ScmObj* %env$ae48855,%struct.ScmObj* %current_45args54063) {
%stackaddr$env-ref55523 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48855, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref55523
%stackaddr$env-ref55524 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48855, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55524
%stackaddr$env-ref55525 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48855, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55525
%stackaddr$env-ref55526 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48855, i64 3)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55526
%stackaddr$env-ref55527 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48855, i64 4)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref55527
%stackaddr$env-ref55528 = alloca %struct.ScmObj*, align 8
%k47437 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48855, i64 5)
store %struct.ScmObj* %k47437, %struct.ScmObj** %stackaddr$env-ref55528
%stackaddr$env-ref55529 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48855, i64 6)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref55529
%stackaddr$env-ref55530 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48855, i64 7)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref55530
%stackaddr$prim55531 = alloca %struct.ScmObj*, align 8
%_95k47438 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54063)
store volatile %struct.ScmObj* %_95k47438, %struct.ScmObj** %stackaddr$prim55531, align 8
%stackaddr$prim55532 = alloca %struct.ScmObj*, align 8
%current_45args54064 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54063)
store volatile %struct.ScmObj* %current_45args54064, %struct.ScmObj** %stackaddr$prim55532, align 8
%stackaddr$prim55533 = alloca %struct.ScmObj*, align 8
%anf_45bind47254 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54064)
store volatile %struct.ScmObj* %anf_45bind47254, %struct.ScmObj** %stackaddr$prim55533, align 8
%stackaddr$makeclosure55534 = alloca %struct.ScmObj*, align 8
%fptrToInt55535 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48887 to i64
%ae48887 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55535)
store volatile %struct.ScmObj* %ae48887, %struct.ScmObj** %stackaddr$makeclosure55534, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48887, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48887, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48887, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48887, %struct.ScmObj* %_37map147120, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48887, %struct.ScmObj* %k47437, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48887, %struct.ScmObj* %f47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48887, %struct.ScmObj* %acc47175, i64 6)
%ae48889 = call %struct.ScmObj* @const_init_false()
%argslist54110$_37foldr1470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55536 = alloca %struct.ScmObj*, align 8
%argslist54110$_37foldr1470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47174, %struct.ScmObj* %argslist54110$_37foldr1470890)
store volatile %struct.ScmObj* %argslist54110$_37foldr1470891, %struct.ScmObj** %stackaddr$prim55536, align 8
%stackaddr$prim55537 = alloca %struct.ScmObj*, align 8
%argslist54110$_37foldr1470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48889, %struct.ScmObj* %argslist54110$_37foldr1470891)
store volatile %struct.ScmObj* %argslist54110$_37foldr1470892, %struct.ScmObj** %stackaddr$prim55537, align 8
%stackaddr$prim55538 = alloca %struct.ScmObj*, align 8
%argslist54110$_37foldr1470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47254, %struct.ScmObj* %argslist54110$_37foldr1470892)
store volatile %struct.ScmObj* %argslist54110$_37foldr1470893, %struct.ScmObj** %stackaddr$prim55538, align 8
%stackaddr$prim55539 = alloca %struct.ScmObj*, align 8
%argslist54110$_37foldr1470894 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48887, %struct.ScmObj* %argslist54110$_37foldr1470893)
store volatile %struct.ScmObj* %argslist54110$_37foldr1470894, %struct.ScmObj** %stackaddr$prim55539, align 8
%clofunc55540 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147089)
musttail call tailcc void %clofunc55540(%struct.ScmObj* %_37foldr147089, %struct.ScmObj* %argslist54110$_37foldr1470894)
ret void
}

define tailcc void @proc_clo$ae48887(%struct.ScmObj* %env$ae48887,%struct.ScmObj* %current_45args54066) {
%stackaddr$env-ref55541 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48887, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref55541
%stackaddr$env-ref55542 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48887, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55542
%stackaddr$env-ref55543 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48887, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55543
%stackaddr$env-ref55544 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48887, i64 3)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref55544
%stackaddr$env-ref55545 = alloca %struct.ScmObj*, align 8
%k47437 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48887, i64 4)
store %struct.ScmObj* %k47437, %struct.ScmObj** %stackaddr$env-ref55545
%stackaddr$env-ref55546 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48887, i64 5)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref55546
%stackaddr$env-ref55547 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48887, i64 6)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref55547
%stackaddr$prim55548 = alloca %struct.ScmObj*, align 8
%_95k47439 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54066)
store volatile %struct.ScmObj* %_95k47439, %struct.ScmObj** %stackaddr$prim55548, align 8
%stackaddr$prim55549 = alloca %struct.ScmObj*, align 8
%current_45args54067 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54066)
store volatile %struct.ScmObj* %current_45args54067, %struct.ScmObj** %stackaddr$prim55549, align 8
%stackaddr$prim55550 = alloca %struct.ScmObj*, align 8
%anf_45bind47255 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54067)
store volatile %struct.ScmObj* %anf_45bind47255, %struct.ScmObj** %stackaddr$prim55550, align 8
%truthy$cmp55551 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47255)
%cmp$cmp55551 = icmp eq i64 %truthy$cmp55551, 1
br i1 %cmp$cmp55551, label %truebranch$cmp55551, label %falsebranch$cmp55551
truebranch$cmp55551:
%ae48898 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54069$k474370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55552 = alloca %struct.ScmObj*, align 8
%argslist54069$k474371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47175, %struct.ScmObj* %argslist54069$k474370)
store volatile %struct.ScmObj* %argslist54069$k474371, %struct.ScmObj** %stackaddr$prim55552, align 8
%stackaddr$prim55553 = alloca %struct.ScmObj*, align 8
%argslist54069$k474372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48898, %struct.ScmObj* %argslist54069$k474371)
store volatile %struct.ScmObj* %argslist54069$k474372, %struct.ScmObj** %stackaddr$prim55553, align 8
%clofunc55554 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47437)
musttail call tailcc void %clofunc55554(%struct.ScmObj* %k47437, %struct.ScmObj* %argslist54069$k474372)
ret void
falsebranch$cmp55551:
%stackaddr$makeclosure55555 = alloca %struct.ScmObj*, align 8
%fptrToInt55556 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48903 to i64
%ae48903 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55556)
store volatile %struct.ScmObj* %ae48903, %struct.ScmObj** %stackaddr$makeclosure55555, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48903, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48903, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48903, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48903, %struct.ScmObj* %_37map147120, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48903, %struct.ScmObj* %k47437, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48903, %struct.ScmObj* %f47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48903, %struct.ScmObj* %acc47175, i64 6)
%ae48904 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55557 = alloca %struct.ScmObj*, align 8
%fptrToInt55558 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48905 to i64
%ae48905 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55558)
store volatile %struct.ScmObj* %ae48905, %struct.ScmObj** %stackaddr$makeclosure55557, align 8
%argslist54109$ae489030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55559 = alloca %struct.ScmObj*, align 8
%argslist54109$ae489031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48905, %struct.ScmObj* %argslist54109$ae489030)
store volatile %struct.ScmObj* %argslist54109$ae489031, %struct.ScmObj** %stackaddr$prim55559, align 8
%stackaddr$prim55560 = alloca %struct.ScmObj*, align 8
%argslist54109$ae489032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48904, %struct.ScmObj* %argslist54109$ae489031)
store volatile %struct.ScmObj* %argslist54109$ae489032, %struct.ScmObj** %stackaddr$prim55560, align 8
%clofunc55561 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48903)
musttail call tailcc void %clofunc55561(%struct.ScmObj* %ae48903, %struct.ScmObj* %argslist54109$ae489032)
ret void
}

define tailcc void @proc_clo$ae48903(%struct.ScmObj* %env$ae48903,%struct.ScmObj* %current_45args54070) {
%stackaddr$env-ref55562 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48903, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref55562
%stackaddr$env-ref55563 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48903, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55563
%stackaddr$env-ref55564 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48903, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55564
%stackaddr$env-ref55565 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48903, i64 3)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref55565
%stackaddr$env-ref55566 = alloca %struct.ScmObj*, align 8
%k47437 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48903, i64 4)
store %struct.ScmObj* %k47437, %struct.ScmObj** %stackaddr$env-ref55566
%stackaddr$env-ref55567 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48903, i64 5)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref55567
%stackaddr$env-ref55568 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48903, i64 6)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref55568
%stackaddr$prim55569 = alloca %struct.ScmObj*, align 8
%_95k47440 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54070)
store volatile %struct.ScmObj* %_95k47440, %struct.ScmObj** %stackaddr$prim55569, align 8
%stackaddr$prim55570 = alloca %struct.ScmObj*, align 8
%current_45args54071 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54070)
store volatile %struct.ScmObj* %current_45args54071, %struct.ScmObj** %stackaddr$prim55570, align 8
%stackaddr$prim55571 = alloca %struct.ScmObj*, align 8
%anf_45bind47256 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54071)
store volatile %struct.ScmObj* %anf_45bind47256, %struct.ScmObj** %stackaddr$prim55571, align 8
%stackaddr$makeclosure55572 = alloca %struct.ScmObj*, align 8
%fptrToInt55573 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48924 to i64
%ae48924 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55573)
store volatile %struct.ScmObj* %ae48924, %struct.ScmObj** %stackaddr$makeclosure55572, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48924, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48924, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48924, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48924, %struct.ScmObj* %_37map147120, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48924, %struct.ScmObj* %k47437, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48924, %struct.ScmObj* %f47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48924, %struct.ScmObj* %acc47175, i64 6)
%argslist54104$_37map1471200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55574 = alloca %struct.ScmObj*, align 8
%argslist54104$_37map1471201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47174, %struct.ScmObj* %argslist54104$_37map1471200)
store volatile %struct.ScmObj* %argslist54104$_37map1471201, %struct.ScmObj** %stackaddr$prim55574, align 8
%stackaddr$prim55575 = alloca %struct.ScmObj*, align 8
%argslist54104$_37map1471202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47256, %struct.ScmObj* %argslist54104$_37map1471201)
store volatile %struct.ScmObj* %argslist54104$_37map1471202, %struct.ScmObj** %stackaddr$prim55575, align 8
%stackaddr$prim55576 = alloca %struct.ScmObj*, align 8
%argslist54104$_37map1471203 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48924, %struct.ScmObj* %argslist54104$_37map1471202)
store volatile %struct.ScmObj* %argslist54104$_37map1471203, %struct.ScmObj** %stackaddr$prim55576, align 8
%clofunc55577 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147120)
musttail call tailcc void %clofunc55577(%struct.ScmObj* %_37map147120, %struct.ScmObj* %argslist54104$_37map1471203)
ret void
}

define tailcc void @proc_clo$ae48924(%struct.ScmObj* %env$ae48924,%struct.ScmObj* %current_45args54073) {
%stackaddr$env-ref55578 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48924, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref55578
%stackaddr$env-ref55579 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48924, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55579
%stackaddr$env-ref55580 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48924, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55580
%stackaddr$env-ref55581 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48924, i64 3)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref55581
%stackaddr$env-ref55582 = alloca %struct.ScmObj*, align 8
%k47437 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48924, i64 4)
store %struct.ScmObj* %k47437, %struct.ScmObj** %stackaddr$env-ref55582
%stackaddr$env-ref55583 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48924, i64 5)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref55583
%stackaddr$env-ref55584 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48924, i64 6)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref55584
%stackaddr$prim55585 = alloca %struct.ScmObj*, align 8
%_95k47441 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54073)
store volatile %struct.ScmObj* %_95k47441, %struct.ScmObj** %stackaddr$prim55585, align 8
%stackaddr$prim55586 = alloca %struct.ScmObj*, align 8
%current_45args54074 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54073)
store volatile %struct.ScmObj* %current_45args54074, %struct.ScmObj** %stackaddr$prim55586, align 8
%stackaddr$prim55587 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54074)
store volatile %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$prim55587, align 8
%stackaddr$makeclosure55588 = alloca %struct.ScmObj*, align 8
%fptrToInt55589 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48927 to i64
%ae48927 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55589)
store volatile %struct.ScmObj* %ae48927, %struct.ScmObj** %stackaddr$makeclosure55588, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48927, %struct.ScmObj* %lsts47174, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48927, %struct.ScmObj* %_37foldr47094, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48927, %struct.ScmObj* %k47437, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48927, %struct.ScmObj* %lsts_4347181, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48927, %struct.ScmObj* %_37foldl47172, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48927, %struct.ScmObj* %_37map147120, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48927, %struct.ScmObj* %f47176, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48927, %struct.ScmObj* %acc47175, i64 7)
%ae48928 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55590 = alloca %struct.ScmObj*, align 8
%fptrToInt55591 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48929 to i64
%ae48929 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55591)
store volatile %struct.ScmObj* %ae48929, %struct.ScmObj** %stackaddr$makeclosure55590, align 8
%argslist54103$ae489270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55592 = alloca %struct.ScmObj*, align 8
%argslist54103$ae489271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48929, %struct.ScmObj* %argslist54103$ae489270)
store volatile %struct.ScmObj* %argslist54103$ae489271, %struct.ScmObj** %stackaddr$prim55592, align 8
%stackaddr$prim55593 = alloca %struct.ScmObj*, align 8
%argslist54103$ae489272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48928, %struct.ScmObj* %argslist54103$ae489271)
store volatile %struct.ScmObj* %argslist54103$ae489272, %struct.ScmObj** %stackaddr$prim55593, align 8
%clofunc55594 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48927)
musttail call tailcc void %clofunc55594(%struct.ScmObj* %ae48927, %struct.ScmObj* %argslist54103$ae489272)
ret void
}

define tailcc void @proc_clo$ae48927(%struct.ScmObj* %env$ae48927,%struct.ScmObj* %current_45args54076) {
%stackaddr$env-ref55595 = alloca %struct.ScmObj*, align 8
%lsts47174 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48927, i64 0)
store %struct.ScmObj* %lsts47174, %struct.ScmObj** %stackaddr$env-ref55595
%stackaddr$env-ref55596 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48927, i64 1)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55596
%stackaddr$env-ref55597 = alloca %struct.ScmObj*, align 8
%k47437 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48927, i64 2)
store %struct.ScmObj* %k47437, %struct.ScmObj** %stackaddr$env-ref55597
%stackaddr$env-ref55598 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48927, i64 3)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref55598
%stackaddr$env-ref55599 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48927, i64 4)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55599
%stackaddr$env-ref55600 = alloca %struct.ScmObj*, align 8
%_37map147120 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48927, i64 5)
store %struct.ScmObj* %_37map147120, %struct.ScmObj** %stackaddr$env-ref55600
%stackaddr$env-ref55601 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48927, i64 6)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref55601
%stackaddr$env-ref55602 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48927, i64 7)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref55602
%stackaddr$prim55603 = alloca %struct.ScmObj*, align 8
%_95k47442 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54076)
store volatile %struct.ScmObj* %_95k47442, %struct.ScmObj** %stackaddr$prim55603, align 8
%stackaddr$prim55604 = alloca %struct.ScmObj*, align 8
%current_45args54077 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54076)
store volatile %struct.ScmObj* %current_45args54077, %struct.ScmObj** %stackaddr$prim55604, align 8
%stackaddr$prim55605 = alloca %struct.ScmObj*, align 8
%anf_45bind47257 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54077)
store volatile %struct.ScmObj* %anf_45bind47257, %struct.ScmObj** %stackaddr$prim55605, align 8
%stackaddr$makeclosure55606 = alloca %struct.ScmObj*, align 8
%fptrToInt55607 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48948 to i64
%ae48948 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55607)
store volatile %struct.ScmObj* %ae48948, %struct.ScmObj** %stackaddr$makeclosure55606, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48948, %struct.ScmObj* %k47437, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48948, %struct.ScmObj* %lsts_4347181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48948, %struct.ScmObj* %_37foldr47094, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48948, %struct.ScmObj* %_37foldl47172, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48948, %struct.ScmObj* %f47176, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48948, %struct.ScmObj* %acc47175, i64 5)
%argslist54098$_37map1471200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55608 = alloca %struct.ScmObj*, align 8
%argslist54098$_37map1471201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47174, %struct.ScmObj* %argslist54098$_37map1471200)
store volatile %struct.ScmObj* %argslist54098$_37map1471201, %struct.ScmObj** %stackaddr$prim55608, align 8
%stackaddr$prim55609 = alloca %struct.ScmObj*, align 8
%argslist54098$_37map1471202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47257, %struct.ScmObj* %argslist54098$_37map1471201)
store volatile %struct.ScmObj* %argslist54098$_37map1471202, %struct.ScmObj** %stackaddr$prim55609, align 8
%stackaddr$prim55610 = alloca %struct.ScmObj*, align 8
%argslist54098$_37map1471203 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48948, %struct.ScmObj* %argslist54098$_37map1471202)
store volatile %struct.ScmObj* %argslist54098$_37map1471203, %struct.ScmObj** %stackaddr$prim55610, align 8
%clofunc55611 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147120)
musttail call tailcc void %clofunc55611(%struct.ScmObj* %_37map147120, %struct.ScmObj* %argslist54098$_37map1471203)
ret void
}

define tailcc void @proc_clo$ae48948(%struct.ScmObj* %env$ae48948,%struct.ScmObj* %current_45args54079) {
%stackaddr$env-ref55612 = alloca %struct.ScmObj*, align 8
%k47437 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48948, i64 0)
store %struct.ScmObj* %k47437, %struct.ScmObj** %stackaddr$env-ref55612
%stackaddr$env-ref55613 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48948, i64 1)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref55613
%stackaddr$env-ref55614 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48948, i64 2)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55614
%stackaddr$env-ref55615 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48948, i64 3)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55615
%stackaddr$env-ref55616 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48948, i64 4)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref55616
%stackaddr$env-ref55617 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48948, i64 5)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref55617
%stackaddr$prim55618 = alloca %struct.ScmObj*, align 8
%_95k47443 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54079)
store volatile %struct.ScmObj* %_95k47443, %struct.ScmObj** %stackaddr$prim55618, align 8
%stackaddr$prim55619 = alloca %struct.ScmObj*, align 8
%current_45args54080 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54079)
store volatile %struct.ScmObj* %current_45args54080, %struct.ScmObj** %stackaddr$prim55619, align 8
%stackaddr$prim55620 = alloca %struct.ScmObj*, align 8
%vs47179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54080)
store volatile %struct.ScmObj* %vs47179, %struct.ScmObj** %stackaddr$prim55620, align 8
%stackaddr$makeclosure55621 = alloca %struct.ScmObj*, align 8
%fptrToInt55622 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48951 to i64
%ae48951 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55622)
store volatile %struct.ScmObj* %ae48951, %struct.ScmObj** %stackaddr$makeclosure55621, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48951, %struct.ScmObj* %k47437, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48951, %struct.ScmObj* %lsts_4347181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48951, %struct.ScmObj* %_37foldr47094, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48951, %struct.ScmObj* %_37foldl47172, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48951, %struct.ScmObj* %vs47179, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48951, %struct.ScmObj* %f47176, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48951, %struct.ScmObj* %acc47175, i64 6)
%ae48952 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55623 = alloca %struct.ScmObj*, align 8
%fptrToInt55624 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48953 to i64
%ae48953 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55624)
store volatile %struct.ScmObj* %ae48953, %struct.ScmObj** %stackaddr$makeclosure55623, align 8
%argslist54097$ae489510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55625 = alloca %struct.ScmObj*, align 8
%argslist54097$ae489511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48953, %struct.ScmObj* %argslist54097$ae489510)
store volatile %struct.ScmObj* %argslist54097$ae489511, %struct.ScmObj** %stackaddr$prim55625, align 8
%stackaddr$prim55626 = alloca %struct.ScmObj*, align 8
%argslist54097$ae489512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48952, %struct.ScmObj* %argslist54097$ae489511)
store volatile %struct.ScmObj* %argslist54097$ae489512, %struct.ScmObj** %stackaddr$prim55626, align 8
%clofunc55627 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48951)
musttail call tailcc void %clofunc55627(%struct.ScmObj* %ae48951, %struct.ScmObj* %argslist54097$ae489512)
ret void
}

define tailcc void @proc_clo$ae48951(%struct.ScmObj* %env$ae48951,%struct.ScmObj* %current_45args54082) {
%stackaddr$env-ref55628 = alloca %struct.ScmObj*, align 8
%k47437 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48951, i64 0)
store %struct.ScmObj* %k47437, %struct.ScmObj** %stackaddr$env-ref55628
%stackaddr$env-ref55629 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48951, i64 1)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref55629
%stackaddr$env-ref55630 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48951, i64 2)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55630
%stackaddr$env-ref55631 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48951, i64 3)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55631
%stackaddr$env-ref55632 = alloca %struct.ScmObj*, align 8
%vs47179 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48951, i64 4)
store %struct.ScmObj* %vs47179, %struct.ScmObj** %stackaddr$env-ref55632
%stackaddr$env-ref55633 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48951, i64 5)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref55633
%stackaddr$env-ref55634 = alloca %struct.ScmObj*, align 8
%acc47175 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48951, i64 6)
store %struct.ScmObj* %acc47175, %struct.ScmObj** %stackaddr$env-ref55634
%stackaddr$prim55635 = alloca %struct.ScmObj*, align 8
%_95k47444 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54082)
store volatile %struct.ScmObj* %_95k47444, %struct.ScmObj** %stackaddr$prim55635, align 8
%stackaddr$prim55636 = alloca %struct.ScmObj*, align 8
%current_45args54083 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54082)
store volatile %struct.ScmObj* %current_45args54083, %struct.ScmObj** %stackaddr$prim55636, align 8
%stackaddr$prim55637 = alloca %struct.ScmObj*, align 8
%anf_45bind47258 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54083)
store volatile %struct.ScmObj* %anf_45bind47258, %struct.ScmObj** %stackaddr$prim55637, align 8
%ae48974 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55638 = alloca %struct.ScmObj*, align 8
%anf_45bind47259 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47175, %struct.ScmObj* %ae48974)
store volatile %struct.ScmObj* %anf_45bind47259, %struct.ScmObj** %stackaddr$prim55638, align 8
%stackaddr$makeclosure55639 = alloca %struct.ScmObj*, align 8
%fptrToInt55640 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48976 to i64
%ae48976 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55640)
store volatile %struct.ScmObj* %ae48976, %struct.ScmObj** %stackaddr$makeclosure55639, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48976, %struct.ScmObj* %k47437, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48976, %struct.ScmObj* %lsts_4347181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48976, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48976, %struct.ScmObj* %f47176, i64 3)
%argslist54091$_37foldr470940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55641 = alloca %struct.ScmObj*, align 8
%argslist54091$_37foldr470941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47179, %struct.ScmObj* %argslist54091$_37foldr470940)
store volatile %struct.ScmObj* %argslist54091$_37foldr470941, %struct.ScmObj** %stackaddr$prim55641, align 8
%stackaddr$prim55642 = alloca %struct.ScmObj*, align 8
%argslist54091$_37foldr470942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47259, %struct.ScmObj* %argslist54091$_37foldr470941)
store volatile %struct.ScmObj* %argslist54091$_37foldr470942, %struct.ScmObj** %stackaddr$prim55642, align 8
%stackaddr$prim55643 = alloca %struct.ScmObj*, align 8
%argslist54091$_37foldr470943 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47258, %struct.ScmObj* %argslist54091$_37foldr470942)
store volatile %struct.ScmObj* %argslist54091$_37foldr470943, %struct.ScmObj** %stackaddr$prim55643, align 8
%stackaddr$prim55644 = alloca %struct.ScmObj*, align 8
%argslist54091$_37foldr470944 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48976, %struct.ScmObj* %argslist54091$_37foldr470943)
store volatile %struct.ScmObj* %argslist54091$_37foldr470944, %struct.ScmObj** %stackaddr$prim55644, align 8
%clofunc55645 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47094)
musttail call tailcc void %clofunc55645(%struct.ScmObj* %_37foldr47094, %struct.ScmObj* %argslist54091$_37foldr470944)
ret void
}

define tailcc void @proc_clo$ae48976(%struct.ScmObj* %env$ae48976,%struct.ScmObj* %current_45args54085) {
%stackaddr$env-ref55646 = alloca %struct.ScmObj*, align 8
%k47437 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48976, i64 0)
store %struct.ScmObj* %k47437, %struct.ScmObj** %stackaddr$env-ref55646
%stackaddr$env-ref55647 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48976, i64 1)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref55647
%stackaddr$env-ref55648 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48976, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55648
%stackaddr$env-ref55649 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48976, i64 3)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref55649
%stackaddr$prim55650 = alloca %struct.ScmObj*, align 8
%_95k47445 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54085)
store volatile %struct.ScmObj* %_95k47445, %struct.ScmObj** %stackaddr$prim55650, align 8
%stackaddr$prim55651 = alloca %struct.ScmObj*, align 8
%current_45args54086 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54085)
store volatile %struct.ScmObj* %current_45args54086, %struct.ScmObj** %stackaddr$prim55651, align 8
%stackaddr$prim55652 = alloca %struct.ScmObj*, align 8
%anf_45bind47260 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54086)
store volatile %struct.ScmObj* %anf_45bind47260, %struct.ScmObj** %stackaddr$prim55652, align 8
%stackaddr$makeclosure55653 = alloca %struct.ScmObj*, align 8
%fptrToInt55654 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48980 to i64
%ae48980 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55654)
store volatile %struct.ScmObj* %ae48980, %struct.ScmObj** %stackaddr$makeclosure55653, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48980, %struct.ScmObj* %k47437, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48980, %struct.ScmObj* %lsts_4347181, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48980, %struct.ScmObj* %_37foldl47172, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48980, %struct.ScmObj* %f47176, i64 3)
%stackaddr$prim55655 = alloca %struct.ScmObj*, align 8
%cpsargs47448 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48980, %struct.ScmObj* %anf_45bind47260)
store volatile %struct.ScmObj* %cpsargs47448, %struct.ScmObj** %stackaddr$prim55655, align 8
%clofunc55656 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47176)
musttail call tailcc void %clofunc55656(%struct.ScmObj* %f47176, %struct.ScmObj* %cpsargs47448)
ret void
}

define tailcc void @proc_clo$ae48980(%struct.ScmObj* %env$ae48980,%struct.ScmObj* %current_45args54088) {
%stackaddr$env-ref55657 = alloca %struct.ScmObj*, align 8
%k47437 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48980, i64 0)
store %struct.ScmObj* %k47437, %struct.ScmObj** %stackaddr$env-ref55657
%stackaddr$env-ref55658 = alloca %struct.ScmObj*, align 8
%lsts_4347181 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48980, i64 1)
store %struct.ScmObj* %lsts_4347181, %struct.ScmObj** %stackaddr$env-ref55658
%stackaddr$env-ref55659 = alloca %struct.ScmObj*, align 8
%_37foldl47172 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48980, i64 2)
store %struct.ScmObj* %_37foldl47172, %struct.ScmObj** %stackaddr$env-ref55659
%stackaddr$env-ref55660 = alloca %struct.ScmObj*, align 8
%f47176 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48980, i64 3)
store %struct.ScmObj* %f47176, %struct.ScmObj** %stackaddr$env-ref55660
%stackaddr$prim55661 = alloca %struct.ScmObj*, align 8
%_95k47446 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54088)
store volatile %struct.ScmObj* %_95k47446, %struct.ScmObj** %stackaddr$prim55661, align 8
%stackaddr$prim55662 = alloca %struct.ScmObj*, align 8
%current_45args54089 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54088)
store volatile %struct.ScmObj* %current_45args54089, %struct.ScmObj** %stackaddr$prim55662, align 8
%stackaddr$prim55663 = alloca %struct.ScmObj*, align 8
%acc_4347183 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54089)
store volatile %struct.ScmObj* %acc_4347183, %struct.ScmObj** %stackaddr$prim55663, align 8
%stackaddr$prim55664 = alloca %struct.ScmObj*, align 8
%anf_45bind47261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4347183, %struct.ScmObj* %lsts_4347181)
store volatile %struct.ScmObj* %anf_45bind47261, %struct.ScmObj** %stackaddr$prim55664, align 8
%stackaddr$prim55665 = alloca %struct.ScmObj*, align 8
%anf_45bind47262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47176, %struct.ScmObj* %anf_45bind47261)
store volatile %struct.ScmObj* %anf_45bind47262, %struct.ScmObj** %stackaddr$prim55665, align 8
%stackaddr$prim55666 = alloca %struct.ScmObj*, align 8
%cpsargs47447 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47437, %struct.ScmObj* %anf_45bind47262)
store volatile %struct.ScmObj* %cpsargs47447, %struct.ScmObj** %stackaddr$prim55666, align 8
%clofunc55667 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl47172)
musttail call tailcc void %clofunc55667(%struct.ScmObj* %_37foldl47172, %struct.ScmObj* %cpsargs47447)
ret void
}

define tailcc void @proc_clo$ae48953(%struct.ScmObj* %env$ae48953,%struct.ScmObj* %current_45args54092) {
%stackaddr$prim55668 = alloca %struct.ScmObj*, align 8
%k47449 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54092)
store volatile %struct.ScmObj* %k47449, %struct.ScmObj** %stackaddr$prim55668, align 8
%stackaddr$prim55669 = alloca %struct.ScmObj*, align 8
%current_45args54093 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54092)
store volatile %struct.ScmObj* %current_45args54093, %struct.ScmObj** %stackaddr$prim55669, align 8
%stackaddr$prim55670 = alloca %struct.ScmObj*, align 8
%a47185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54093)
store volatile %struct.ScmObj* %a47185, %struct.ScmObj** %stackaddr$prim55670, align 8
%stackaddr$prim55671 = alloca %struct.ScmObj*, align 8
%current_45args54094 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54093)
store volatile %struct.ScmObj* %current_45args54094, %struct.ScmObj** %stackaddr$prim55671, align 8
%stackaddr$prim55672 = alloca %struct.ScmObj*, align 8
%b47184 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54094)
store volatile %struct.ScmObj* %b47184, %struct.ScmObj** %stackaddr$prim55672, align 8
%stackaddr$prim55673 = alloca %struct.ScmObj*, align 8
%cpsprim47450 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47185, %struct.ScmObj* %b47184)
store volatile %struct.ScmObj* %cpsprim47450, %struct.ScmObj** %stackaddr$prim55673, align 8
%ae48957 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54096$k474490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55674 = alloca %struct.ScmObj*, align 8
%argslist54096$k474491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47450, %struct.ScmObj* %argslist54096$k474490)
store volatile %struct.ScmObj* %argslist54096$k474491, %struct.ScmObj** %stackaddr$prim55674, align 8
%stackaddr$prim55675 = alloca %struct.ScmObj*, align 8
%argslist54096$k474492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48957, %struct.ScmObj* %argslist54096$k474491)
store volatile %struct.ScmObj* %argslist54096$k474492, %struct.ScmObj** %stackaddr$prim55675, align 8
%clofunc55676 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47449)
musttail call tailcc void %clofunc55676(%struct.ScmObj* %k47449, %struct.ScmObj* %argslist54096$k474492)
ret void
}

define tailcc void @proc_clo$ae48929(%struct.ScmObj* %env$ae48929,%struct.ScmObj* %current_45args54099) {
%stackaddr$prim55677 = alloca %struct.ScmObj*, align 8
%k47451 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54099)
store volatile %struct.ScmObj* %k47451, %struct.ScmObj** %stackaddr$prim55677, align 8
%stackaddr$prim55678 = alloca %struct.ScmObj*, align 8
%current_45args54100 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54099)
store volatile %struct.ScmObj* %current_45args54100, %struct.ScmObj** %stackaddr$prim55678, align 8
%stackaddr$prim55679 = alloca %struct.ScmObj*, align 8
%x47180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54100)
store volatile %struct.ScmObj* %x47180, %struct.ScmObj** %stackaddr$prim55679, align 8
%stackaddr$prim55680 = alloca %struct.ScmObj*, align 8
%cpsprim47452 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47180)
store volatile %struct.ScmObj* %cpsprim47452, %struct.ScmObj** %stackaddr$prim55680, align 8
%ae48932 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54102$k474510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55681 = alloca %struct.ScmObj*, align 8
%argslist54102$k474511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47452, %struct.ScmObj* %argslist54102$k474510)
store volatile %struct.ScmObj* %argslist54102$k474511, %struct.ScmObj** %stackaddr$prim55681, align 8
%stackaddr$prim55682 = alloca %struct.ScmObj*, align 8
%argslist54102$k474512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48932, %struct.ScmObj* %argslist54102$k474511)
store volatile %struct.ScmObj* %argslist54102$k474512, %struct.ScmObj** %stackaddr$prim55682, align 8
%clofunc55683 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47451)
musttail call tailcc void %clofunc55683(%struct.ScmObj* %k47451, %struct.ScmObj* %argslist54102$k474512)
ret void
}

define tailcc void @proc_clo$ae48905(%struct.ScmObj* %env$ae48905,%struct.ScmObj* %current_45args54105) {
%stackaddr$prim55684 = alloca %struct.ScmObj*, align 8
%k47453 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54105)
store volatile %struct.ScmObj* %k47453, %struct.ScmObj** %stackaddr$prim55684, align 8
%stackaddr$prim55685 = alloca %struct.ScmObj*, align 8
%current_45args54106 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54105)
store volatile %struct.ScmObj* %current_45args54106, %struct.ScmObj** %stackaddr$prim55685, align 8
%stackaddr$prim55686 = alloca %struct.ScmObj*, align 8
%x47182 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54106)
store volatile %struct.ScmObj* %x47182, %struct.ScmObj** %stackaddr$prim55686, align 8
%stackaddr$prim55687 = alloca %struct.ScmObj*, align 8
%cpsprim47454 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47182)
store volatile %struct.ScmObj* %cpsprim47454, %struct.ScmObj** %stackaddr$prim55687, align 8
%ae48908 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54108$k474530 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55688 = alloca %struct.ScmObj*, align 8
%argslist54108$k474531 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47454, %struct.ScmObj* %argslist54108$k474530)
store volatile %struct.ScmObj* %argslist54108$k474531, %struct.ScmObj** %stackaddr$prim55688, align 8
%stackaddr$prim55689 = alloca %struct.ScmObj*, align 8
%argslist54108$k474532 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48908, %struct.ScmObj* %argslist54108$k474531)
store volatile %struct.ScmObj* %argslist54108$k474532, %struct.ScmObj** %stackaddr$prim55689, align 8
%clofunc55690 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47453)
musttail call tailcc void %clofunc55690(%struct.ScmObj* %k47453, %struct.ScmObj* %argslist54108$k474532)
ret void
}

define tailcc void @proc_clo$ae48857(%struct.ScmObj* %env$ae48857,%struct.ScmObj* %current_45args54111) {
%stackaddr$prim55691 = alloca %struct.ScmObj*, align 8
%k47455 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54111)
store volatile %struct.ScmObj* %k47455, %struct.ScmObj** %stackaddr$prim55691, align 8
%stackaddr$prim55692 = alloca %struct.ScmObj*, align 8
%current_45args54112 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54111)
store volatile %struct.ScmObj* %current_45args54112, %struct.ScmObj** %stackaddr$prim55692, align 8
%stackaddr$prim55693 = alloca %struct.ScmObj*, align 8
%lst47178 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54112)
store volatile %struct.ScmObj* %lst47178, %struct.ScmObj** %stackaddr$prim55693, align 8
%stackaddr$prim55694 = alloca %struct.ScmObj*, align 8
%current_45args54113 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54112)
store volatile %struct.ScmObj* %current_45args54113, %struct.ScmObj** %stackaddr$prim55694, align 8
%stackaddr$prim55695 = alloca %struct.ScmObj*, align 8
%b47177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54113)
store volatile %struct.ScmObj* %b47177, %struct.ScmObj** %stackaddr$prim55695, align 8
%truthy$cmp55696 = call i64 @is_truthy_value(%struct.ScmObj* %b47177)
%cmp$cmp55696 = icmp eq i64 %truthy$cmp55696, 1
br i1 %cmp$cmp55696, label %truebranch$cmp55696, label %falsebranch$cmp55696
truebranch$cmp55696:
%ae48860 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54115$k474550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55697 = alloca %struct.ScmObj*, align 8
%argslist54115$k474551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47177, %struct.ScmObj* %argslist54115$k474550)
store volatile %struct.ScmObj* %argslist54115$k474551, %struct.ScmObj** %stackaddr$prim55697, align 8
%stackaddr$prim55698 = alloca %struct.ScmObj*, align 8
%argslist54115$k474552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48860, %struct.ScmObj* %argslist54115$k474551)
store volatile %struct.ScmObj* %argslist54115$k474552, %struct.ScmObj** %stackaddr$prim55698, align 8
%clofunc55699 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47455)
musttail call tailcc void %clofunc55699(%struct.ScmObj* %k47455, %struct.ScmObj* %argslist54115$k474552)
ret void
falsebranch$cmp55696:
%stackaddr$prim55700 = alloca %struct.ScmObj*, align 8
%cpsprim47456 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47178)
store volatile %struct.ScmObj* %cpsprim47456, %struct.ScmObj** %stackaddr$prim55700, align 8
%ae48867 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54116$k474550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55701 = alloca %struct.ScmObj*, align 8
%argslist54116$k474551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47456, %struct.ScmObj* %argslist54116$k474550)
store volatile %struct.ScmObj* %argslist54116$k474551, %struct.ScmObj** %stackaddr$prim55701, align 8
%stackaddr$prim55702 = alloca %struct.ScmObj*, align 8
%argslist54116$k474552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48867, %struct.ScmObj* %argslist54116$k474551)
store volatile %struct.ScmObj* %argslist54116$k474552, %struct.ScmObj** %stackaddr$prim55702, align 8
%clofunc55703 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47455)
musttail call tailcc void %clofunc55703(%struct.ScmObj* %k47455, %struct.ScmObj* %argslist54116$k474552)
ret void
}

define tailcc void @proc_clo$ae48698(%struct.ScmObj* %env$ae48698,%struct.ScmObj* %args4711647457) {
%stackaddr$env-ref55704 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48698, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55704
%stackaddr$env-ref55705 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48698, i64 1)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref55705
%stackaddr$env-ref55706 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48698, i64 2)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref55706
%stackaddr$prim55707 = alloca %struct.ScmObj*, align 8
%k47458 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4711647457)
store volatile %struct.ScmObj* %k47458, %struct.ScmObj** %stackaddr$prim55707, align 8
%stackaddr$prim55708 = alloca %struct.ScmObj*, align 8
%args47116 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4711647457)
store volatile %struct.ScmObj* %args47116, %struct.ScmObj** %stackaddr$prim55708, align 8
%stackaddr$prim55709 = alloca %struct.ScmObj*, align 8
%f47118 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47116)
store volatile %struct.ScmObj* %f47118, %struct.ScmObj** %stackaddr$prim55709, align 8
%stackaddr$prim55710 = alloca %struct.ScmObj*, align 8
%lsts47117 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47116)
store volatile %struct.ScmObj* %lsts47117, %struct.ScmObj** %stackaddr$prim55710, align 8
%stackaddr$makeclosure55711 = alloca %struct.ScmObj*, align 8
%fptrToInt55712 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48703 to i64
%ae48703 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55712)
store volatile %struct.ScmObj* %ae48703, %struct.ScmObj** %stackaddr$makeclosure55711, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48703, %struct.ScmObj* %_37foldr47094, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48703, %struct.ScmObj* %k47458, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48703, %struct.ScmObj* %lsts47117, i64 2)
%ae48704 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55713 = alloca %struct.ScmObj*, align 8
%fptrToInt55714 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48705 to i64
%ae48705 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55714)
store volatile %struct.ScmObj* %ae48705, %struct.ScmObj** %stackaddr$makeclosure55713, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48705, %struct.ScmObj* %_37drop_45right47108, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48705, %struct.ScmObj* %f47118, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48705, %struct.ScmObj* %_37last47111, i64 2)
%argslist54135$ae487030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55715 = alloca %struct.ScmObj*, align 8
%argslist54135$ae487031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48705, %struct.ScmObj* %argslist54135$ae487030)
store volatile %struct.ScmObj* %argslist54135$ae487031, %struct.ScmObj** %stackaddr$prim55715, align 8
%stackaddr$prim55716 = alloca %struct.ScmObj*, align 8
%argslist54135$ae487032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48704, %struct.ScmObj* %argslist54135$ae487031)
store volatile %struct.ScmObj* %argslist54135$ae487032, %struct.ScmObj** %stackaddr$prim55716, align 8
%clofunc55717 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48703)
musttail call tailcc void %clofunc55717(%struct.ScmObj* %ae48703, %struct.ScmObj* %argslist54135$ae487032)
ret void
}

define tailcc void @proc_clo$ae48703(%struct.ScmObj* %env$ae48703,%struct.ScmObj* %current_45args54120) {
%stackaddr$env-ref55718 = alloca %struct.ScmObj*, align 8
%_37foldr47094 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48703, i64 0)
store %struct.ScmObj* %_37foldr47094, %struct.ScmObj** %stackaddr$env-ref55718
%stackaddr$env-ref55719 = alloca %struct.ScmObj*, align 8
%k47458 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48703, i64 1)
store %struct.ScmObj* %k47458, %struct.ScmObj** %stackaddr$env-ref55719
%stackaddr$env-ref55720 = alloca %struct.ScmObj*, align 8
%lsts47117 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48703, i64 2)
store %struct.ScmObj* %lsts47117, %struct.ScmObj** %stackaddr$env-ref55720
%stackaddr$prim55721 = alloca %struct.ScmObj*, align 8
%_95k47459 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54120)
store volatile %struct.ScmObj* %_95k47459, %struct.ScmObj** %stackaddr$prim55721, align 8
%stackaddr$prim55722 = alloca %struct.ScmObj*, align 8
%current_45args54121 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54120)
store volatile %struct.ScmObj* %current_45args54121, %struct.ScmObj** %stackaddr$prim55722, align 8
%stackaddr$prim55723 = alloca %struct.ScmObj*, align 8
%anf_45bind47249 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54121)
store volatile %struct.ScmObj* %anf_45bind47249, %struct.ScmObj** %stackaddr$prim55723, align 8
%ae48766 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55724 = alloca %struct.ScmObj*, align 8
%anf_45bind47250 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48766, %struct.ScmObj* %lsts47117)
store volatile %struct.ScmObj* %anf_45bind47250, %struct.ScmObj** %stackaddr$prim55724, align 8
%stackaddr$prim55725 = alloca %struct.ScmObj*, align 8
%anf_45bind47251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47249, %struct.ScmObj* %anf_45bind47250)
store volatile %struct.ScmObj* %anf_45bind47251, %struct.ScmObj** %stackaddr$prim55725, align 8
%stackaddr$prim55726 = alloca %struct.ScmObj*, align 8
%cpsargs47460 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47458, %struct.ScmObj* %anf_45bind47251)
store volatile %struct.ScmObj* %cpsargs47460, %struct.ScmObj** %stackaddr$prim55726, align 8
%clofunc55727 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47094)
musttail call tailcc void %clofunc55727(%struct.ScmObj* %_37foldr47094, %struct.ScmObj* %cpsargs47460)
ret void
}

define tailcc void @proc_clo$ae48705(%struct.ScmObj* %env$ae48705,%struct.ScmObj* %fargs4711947461) {
%stackaddr$env-ref55728 = alloca %struct.ScmObj*, align 8
%_37drop_45right47108 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48705, i64 0)
store %struct.ScmObj* %_37drop_45right47108, %struct.ScmObj** %stackaddr$env-ref55728
%stackaddr$env-ref55729 = alloca %struct.ScmObj*, align 8
%f47118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48705, i64 1)
store %struct.ScmObj* %f47118, %struct.ScmObj** %stackaddr$env-ref55729
%stackaddr$env-ref55730 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48705, i64 2)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref55730
%stackaddr$prim55731 = alloca %struct.ScmObj*, align 8
%k47462 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4711947461)
store volatile %struct.ScmObj* %k47462, %struct.ScmObj** %stackaddr$prim55731, align 8
%stackaddr$prim55732 = alloca %struct.ScmObj*, align 8
%fargs47119 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4711947461)
store volatile %struct.ScmObj* %fargs47119, %struct.ScmObj** %stackaddr$prim55732, align 8
%stackaddr$makeclosure55733 = alloca %struct.ScmObj*, align 8
%fptrToInt55734 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48709 to i64
%ae48709 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55734)
store volatile %struct.ScmObj* %ae48709, %struct.ScmObj** %stackaddr$makeclosure55733, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48709, %struct.ScmObj* %k47462, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48709, %struct.ScmObj* %fargs47119, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48709, %struct.ScmObj* %f47118, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48709, %struct.ScmObj* %_37last47111, i64 3)
%ae48711 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist54134$_37drop_45right471080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55735 = alloca %struct.ScmObj*, align 8
%argslist54134$_37drop_45right471081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48711, %struct.ScmObj* %argslist54134$_37drop_45right471080)
store volatile %struct.ScmObj* %argslist54134$_37drop_45right471081, %struct.ScmObj** %stackaddr$prim55735, align 8
%stackaddr$prim55736 = alloca %struct.ScmObj*, align 8
%argslist54134$_37drop_45right471082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47119, %struct.ScmObj* %argslist54134$_37drop_45right471081)
store volatile %struct.ScmObj* %argslist54134$_37drop_45right471082, %struct.ScmObj** %stackaddr$prim55736, align 8
%stackaddr$prim55737 = alloca %struct.ScmObj*, align 8
%argslist54134$_37drop_45right471083 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48709, %struct.ScmObj* %argslist54134$_37drop_45right471082)
store volatile %struct.ScmObj* %argslist54134$_37drop_45right471083, %struct.ScmObj** %stackaddr$prim55737, align 8
%clofunc55738 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right47108)
musttail call tailcc void %clofunc55738(%struct.ScmObj* %_37drop_45right47108, %struct.ScmObj* %argslist54134$_37drop_45right471083)
ret void
}

define tailcc void @proc_clo$ae48709(%struct.ScmObj* %env$ae48709,%struct.ScmObj* %current_45args54123) {
%stackaddr$env-ref55739 = alloca %struct.ScmObj*, align 8
%k47462 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48709, i64 0)
store %struct.ScmObj* %k47462, %struct.ScmObj** %stackaddr$env-ref55739
%stackaddr$env-ref55740 = alloca %struct.ScmObj*, align 8
%fargs47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48709, i64 1)
store %struct.ScmObj* %fargs47119, %struct.ScmObj** %stackaddr$env-ref55740
%stackaddr$env-ref55741 = alloca %struct.ScmObj*, align 8
%f47118 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48709, i64 2)
store %struct.ScmObj* %f47118, %struct.ScmObj** %stackaddr$env-ref55741
%stackaddr$env-ref55742 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48709, i64 3)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref55742
%stackaddr$prim55743 = alloca %struct.ScmObj*, align 8
%_95k47463 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54123)
store volatile %struct.ScmObj* %_95k47463, %struct.ScmObj** %stackaddr$prim55743, align 8
%stackaddr$prim55744 = alloca %struct.ScmObj*, align 8
%current_45args54124 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54123)
store volatile %struct.ScmObj* %current_45args54124, %struct.ScmObj** %stackaddr$prim55744, align 8
%stackaddr$prim55745 = alloca %struct.ScmObj*, align 8
%anf_45bind47246 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54124)
store volatile %struct.ScmObj* %anf_45bind47246, %struct.ScmObj** %stackaddr$prim55745, align 8
%stackaddr$makeclosure55746 = alloca %struct.ScmObj*, align 8
%fptrToInt55747 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48716 to i64
%ae48716 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55747)
store volatile %struct.ScmObj* %ae48716, %struct.ScmObj** %stackaddr$makeclosure55746, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48716, %struct.ScmObj* %k47462, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48716, %struct.ScmObj* %fargs47119, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48716, %struct.ScmObj* %_37last47111, i64 2)
%stackaddr$prim55748 = alloca %struct.ScmObj*, align 8
%cpsargs47467 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48716, %struct.ScmObj* %anf_45bind47246)
store volatile %struct.ScmObj* %cpsargs47467, %struct.ScmObj** %stackaddr$prim55748, align 8
%clofunc55749 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47118)
musttail call tailcc void %clofunc55749(%struct.ScmObj* %f47118, %struct.ScmObj* %cpsargs47467)
ret void
}

define tailcc void @proc_clo$ae48716(%struct.ScmObj* %env$ae48716,%struct.ScmObj* %current_45args54126) {
%stackaddr$env-ref55750 = alloca %struct.ScmObj*, align 8
%k47462 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48716, i64 0)
store %struct.ScmObj* %k47462, %struct.ScmObj** %stackaddr$env-ref55750
%stackaddr$env-ref55751 = alloca %struct.ScmObj*, align 8
%fargs47119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48716, i64 1)
store %struct.ScmObj* %fargs47119, %struct.ScmObj** %stackaddr$env-ref55751
%stackaddr$env-ref55752 = alloca %struct.ScmObj*, align 8
%_37last47111 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48716, i64 2)
store %struct.ScmObj* %_37last47111, %struct.ScmObj** %stackaddr$env-ref55752
%stackaddr$prim55753 = alloca %struct.ScmObj*, align 8
%_95k47464 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54126)
store volatile %struct.ScmObj* %_95k47464, %struct.ScmObj** %stackaddr$prim55753, align 8
%stackaddr$prim55754 = alloca %struct.ScmObj*, align 8
%current_45args54127 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54126)
store volatile %struct.ScmObj* %current_45args54127, %struct.ScmObj** %stackaddr$prim55754, align 8
%stackaddr$prim55755 = alloca %struct.ScmObj*, align 8
%anf_45bind47247 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54127)
store volatile %struct.ScmObj* %anf_45bind47247, %struct.ScmObj** %stackaddr$prim55755, align 8
%stackaddr$makeclosure55756 = alloca %struct.ScmObj*, align 8
%fptrToInt55757 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48721 to i64
%ae48721 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55757)
store volatile %struct.ScmObj* %ae48721, %struct.ScmObj** %stackaddr$makeclosure55756, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48721, %struct.ScmObj* %k47462, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48721, %struct.ScmObj* %anf_45bind47247, i64 1)
%argslist54133$_37last471110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55758 = alloca %struct.ScmObj*, align 8
%argslist54133$_37last471111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs47119, %struct.ScmObj* %argslist54133$_37last471110)
store volatile %struct.ScmObj* %argslist54133$_37last471111, %struct.ScmObj** %stackaddr$prim55758, align 8
%stackaddr$prim55759 = alloca %struct.ScmObj*, align 8
%argslist54133$_37last471112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48721, %struct.ScmObj* %argslist54133$_37last471111)
store volatile %struct.ScmObj* %argslist54133$_37last471112, %struct.ScmObj** %stackaddr$prim55759, align 8
%clofunc55760 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last47111)
musttail call tailcc void %clofunc55760(%struct.ScmObj* %_37last47111, %struct.ScmObj* %argslist54133$_37last471112)
ret void
}

define tailcc void @proc_clo$ae48721(%struct.ScmObj* %env$ae48721,%struct.ScmObj* %current_45args54129) {
%stackaddr$env-ref55761 = alloca %struct.ScmObj*, align 8
%k47462 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48721, i64 0)
store %struct.ScmObj* %k47462, %struct.ScmObj** %stackaddr$env-ref55761
%stackaddr$env-ref55762 = alloca %struct.ScmObj*, align 8
%anf_45bind47247 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48721, i64 1)
store %struct.ScmObj* %anf_45bind47247, %struct.ScmObj** %stackaddr$env-ref55762
%stackaddr$prim55763 = alloca %struct.ScmObj*, align 8
%_95k47465 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54129)
store volatile %struct.ScmObj* %_95k47465, %struct.ScmObj** %stackaddr$prim55763, align 8
%stackaddr$prim55764 = alloca %struct.ScmObj*, align 8
%current_45args54130 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54129)
store volatile %struct.ScmObj* %current_45args54130, %struct.ScmObj** %stackaddr$prim55764, align 8
%stackaddr$prim55765 = alloca %struct.ScmObj*, align 8
%anf_45bind47248 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54130)
store volatile %struct.ScmObj* %anf_45bind47248, %struct.ScmObj** %stackaddr$prim55765, align 8
%stackaddr$prim55766 = alloca %struct.ScmObj*, align 8
%cpsprim47466 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47247, %struct.ScmObj* %anf_45bind47248)
store volatile %struct.ScmObj* %cpsprim47466, %struct.ScmObj** %stackaddr$prim55766, align 8
%ae48726 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54132$k474620 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55767 = alloca %struct.ScmObj*, align 8
%argslist54132$k474621 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47466, %struct.ScmObj* %argslist54132$k474620)
store volatile %struct.ScmObj* %argslist54132$k474621, %struct.ScmObj** %stackaddr$prim55767, align 8
%stackaddr$prim55768 = alloca %struct.ScmObj*, align 8
%argslist54132$k474622 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48726, %struct.ScmObj* %argslist54132$k474621)
store volatile %struct.ScmObj* %argslist54132$k474622, %struct.ScmObj** %stackaddr$prim55768, align 8
%clofunc55769 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47462)
musttail call tailcc void %clofunc55769(%struct.ScmObj* %k47462, %struct.ScmObj* %argslist54132$k474622)
ret void
}

define tailcc void @proc_clo$ae48621(%struct.ScmObj* %env$ae48621,%struct.ScmObj* %current_45args54137) {
%stackaddr$env-ref55770 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48621, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55770
%stackaddr$prim55771 = alloca %struct.ScmObj*, align 8
%k47468 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54137)
store volatile %struct.ScmObj* %k47468, %struct.ScmObj** %stackaddr$prim55771, align 8
%stackaddr$prim55772 = alloca %struct.ScmObj*, align 8
%current_45args54138 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54137)
store volatile %struct.ScmObj* %current_45args54138, %struct.ScmObj** %stackaddr$prim55772, align 8
%stackaddr$prim55773 = alloca %struct.ScmObj*, align 8
%f47122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54138)
store volatile %struct.ScmObj* %f47122, %struct.ScmObj** %stackaddr$prim55773, align 8
%stackaddr$prim55774 = alloca %struct.ScmObj*, align 8
%current_45args54139 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54138)
store volatile %struct.ScmObj* %current_45args54139, %struct.ScmObj** %stackaddr$prim55774, align 8
%stackaddr$prim55775 = alloca %struct.ScmObj*, align 8
%lst47121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54139)
store volatile %struct.ScmObj* %lst47121, %struct.ScmObj** %stackaddr$prim55775, align 8
%stackaddr$makeclosure55776 = alloca %struct.ScmObj*, align 8
%fptrToInt55777 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48622 to i64
%ae48622 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55777)
store volatile %struct.ScmObj* %ae48622, %struct.ScmObj** %stackaddr$makeclosure55776, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48622, %struct.ScmObj* %lst47121, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48622, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48622, %struct.ScmObj* %k47468, i64 2)
%ae48623 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55778 = alloca %struct.ScmObj*, align 8
%fptrToInt55779 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48624 to i64
%ae48624 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55779)
store volatile %struct.ScmObj* %ae48624, %struct.ScmObj** %stackaddr$makeclosure55778, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48624, %struct.ScmObj* %f47122, i64 0)
%argslist54154$ae486220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55780 = alloca %struct.ScmObj*, align 8
%argslist54154$ae486221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48624, %struct.ScmObj* %argslist54154$ae486220)
store volatile %struct.ScmObj* %argslist54154$ae486221, %struct.ScmObj** %stackaddr$prim55780, align 8
%stackaddr$prim55781 = alloca %struct.ScmObj*, align 8
%argslist54154$ae486222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48623, %struct.ScmObj* %argslist54154$ae486221)
store volatile %struct.ScmObj* %argslist54154$ae486222, %struct.ScmObj** %stackaddr$prim55781, align 8
%clofunc55782 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48622)
musttail call tailcc void %clofunc55782(%struct.ScmObj* %ae48622, %struct.ScmObj* %argslist54154$ae486222)
ret void
}

define tailcc void @proc_clo$ae48622(%struct.ScmObj* %env$ae48622,%struct.ScmObj* %current_45args54141) {
%stackaddr$env-ref55783 = alloca %struct.ScmObj*, align 8
%lst47121 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48622, i64 0)
store %struct.ScmObj* %lst47121, %struct.ScmObj** %stackaddr$env-ref55783
%stackaddr$env-ref55784 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48622, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55784
%stackaddr$env-ref55785 = alloca %struct.ScmObj*, align 8
%k47468 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48622, i64 2)
store %struct.ScmObj* %k47468, %struct.ScmObj** %stackaddr$env-ref55785
%stackaddr$prim55786 = alloca %struct.ScmObj*, align 8
%_95k47469 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54141)
store volatile %struct.ScmObj* %_95k47469, %struct.ScmObj** %stackaddr$prim55786, align 8
%stackaddr$prim55787 = alloca %struct.ScmObj*, align 8
%current_45args54142 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54141)
store volatile %struct.ScmObj* %current_45args54142, %struct.ScmObj** %stackaddr$prim55787, align 8
%stackaddr$prim55788 = alloca %struct.ScmObj*, align 8
%anf_45bind47245 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54142)
store volatile %struct.ScmObj* %anf_45bind47245, %struct.ScmObj** %stackaddr$prim55788, align 8
%ae48656 = call %struct.ScmObj* @const_init_null()
%argslist54144$_37foldr1470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55789 = alloca %struct.ScmObj*, align 8
%argslist54144$_37foldr1470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47121, %struct.ScmObj* %argslist54144$_37foldr1470890)
store volatile %struct.ScmObj* %argslist54144$_37foldr1470891, %struct.ScmObj** %stackaddr$prim55789, align 8
%stackaddr$prim55790 = alloca %struct.ScmObj*, align 8
%argslist54144$_37foldr1470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48656, %struct.ScmObj* %argslist54144$_37foldr1470891)
store volatile %struct.ScmObj* %argslist54144$_37foldr1470892, %struct.ScmObj** %stackaddr$prim55790, align 8
%stackaddr$prim55791 = alloca %struct.ScmObj*, align 8
%argslist54144$_37foldr1470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47245, %struct.ScmObj* %argslist54144$_37foldr1470892)
store volatile %struct.ScmObj* %argslist54144$_37foldr1470893, %struct.ScmObj** %stackaddr$prim55791, align 8
%stackaddr$prim55792 = alloca %struct.ScmObj*, align 8
%argslist54144$_37foldr1470894 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47468, %struct.ScmObj* %argslist54144$_37foldr1470893)
store volatile %struct.ScmObj* %argslist54144$_37foldr1470894, %struct.ScmObj** %stackaddr$prim55792, align 8
%clofunc55793 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147089)
musttail call tailcc void %clofunc55793(%struct.ScmObj* %_37foldr147089, %struct.ScmObj* %argslist54144$_37foldr1470894)
ret void
}

define tailcc void @proc_clo$ae48624(%struct.ScmObj* %env$ae48624,%struct.ScmObj* %current_45args54145) {
%stackaddr$env-ref55794 = alloca %struct.ScmObj*, align 8
%f47122 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48624, i64 0)
store %struct.ScmObj* %f47122, %struct.ScmObj** %stackaddr$env-ref55794
%stackaddr$prim55795 = alloca %struct.ScmObj*, align 8
%k47470 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54145)
store volatile %struct.ScmObj* %k47470, %struct.ScmObj** %stackaddr$prim55795, align 8
%stackaddr$prim55796 = alloca %struct.ScmObj*, align 8
%current_45args54146 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54145)
store volatile %struct.ScmObj* %current_45args54146, %struct.ScmObj** %stackaddr$prim55796, align 8
%stackaddr$prim55797 = alloca %struct.ScmObj*, align 8
%v47124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54146)
store volatile %struct.ScmObj* %v47124, %struct.ScmObj** %stackaddr$prim55797, align 8
%stackaddr$prim55798 = alloca %struct.ScmObj*, align 8
%current_45args54147 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54146)
store volatile %struct.ScmObj* %current_45args54147, %struct.ScmObj** %stackaddr$prim55798, align 8
%stackaddr$prim55799 = alloca %struct.ScmObj*, align 8
%r47123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54147)
store volatile %struct.ScmObj* %r47123, %struct.ScmObj** %stackaddr$prim55799, align 8
%stackaddr$makeclosure55800 = alloca %struct.ScmObj*, align 8
%fptrToInt55801 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48626 to i64
%ae48626 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55801)
store volatile %struct.ScmObj* %ae48626, %struct.ScmObj** %stackaddr$makeclosure55800, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48626, %struct.ScmObj* %r47123, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48626, %struct.ScmObj* %k47470, i64 1)
%argslist54153$f471220 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55802 = alloca %struct.ScmObj*, align 8
%argslist54153$f471221 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v47124, %struct.ScmObj* %argslist54153$f471220)
store volatile %struct.ScmObj* %argslist54153$f471221, %struct.ScmObj** %stackaddr$prim55802, align 8
%stackaddr$prim55803 = alloca %struct.ScmObj*, align 8
%argslist54153$f471222 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48626, %struct.ScmObj* %argslist54153$f471221)
store volatile %struct.ScmObj* %argslist54153$f471222, %struct.ScmObj** %stackaddr$prim55803, align 8
%clofunc55804 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47122)
musttail call tailcc void %clofunc55804(%struct.ScmObj* %f47122, %struct.ScmObj* %argslist54153$f471222)
ret void
}

define tailcc void @proc_clo$ae48626(%struct.ScmObj* %env$ae48626,%struct.ScmObj* %current_45args54149) {
%stackaddr$env-ref55805 = alloca %struct.ScmObj*, align 8
%r47123 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48626, i64 0)
store %struct.ScmObj* %r47123, %struct.ScmObj** %stackaddr$env-ref55805
%stackaddr$env-ref55806 = alloca %struct.ScmObj*, align 8
%k47470 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48626, i64 1)
store %struct.ScmObj* %k47470, %struct.ScmObj** %stackaddr$env-ref55806
%stackaddr$prim55807 = alloca %struct.ScmObj*, align 8
%_95k47471 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54149)
store volatile %struct.ScmObj* %_95k47471, %struct.ScmObj** %stackaddr$prim55807, align 8
%stackaddr$prim55808 = alloca %struct.ScmObj*, align 8
%current_45args54150 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54149)
store volatile %struct.ScmObj* %current_45args54150, %struct.ScmObj** %stackaddr$prim55808, align 8
%stackaddr$prim55809 = alloca %struct.ScmObj*, align 8
%anf_45bind47244 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54150)
store volatile %struct.ScmObj* %anf_45bind47244, %struct.ScmObj** %stackaddr$prim55809, align 8
%stackaddr$prim55810 = alloca %struct.ScmObj*, align 8
%cpsprim47472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47244, %struct.ScmObj* %r47123)
store volatile %struct.ScmObj* %cpsprim47472, %struct.ScmObj** %stackaddr$prim55810, align 8
%ae48631 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54152$k474700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55811 = alloca %struct.ScmObj*, align 8
%argslist54152$k474701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47472, %struct.ScmObj* %argslist54152$k474700)
store volatile %struct.ScmObj* %argslist54152$k474701, %struct.ScmObj** %stackaddr$prim55811, align 8
%stackaddr$prim55812 = alloca %struct.ScmObj*, align 8
%argslist54152$k474702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48631, %struct.ScmObj* %argslist54152$k474701)
store volatile %struct.ScmObj* %argslist54152$k474702, %struct.ScmObj** %stackaddr$prim55812, align 8
%clofunc55813 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47470)
musttail call tailcc void %clofunc55813(%struct.ScmObj* %k47470, %struct.ScmObj* %argslist54152$k474702)
ret void
}

define tailcc void @proc_clo$ae48235(%struct.ScmObj* %env$ae48235,%struct.ScmObj* %current_45args54157) {
%stackaddr$env-ref55814 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48235, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55814
%stackaddr$env-ref55815 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48235, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55815
%stackaddr$prim55816 = alloca %struct.ScmObj*, align 8
%k47473 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54157)
store volatile %struct.ScmObj* %k47473, %struct.ScmObj** %stackaddr$prim55816, align 8
%stackaddr$prim55817 = alloca %struct.ScmObj*, align 8
%current_45args54158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54157)
store volatile %struct.ScmObj* %current_45args54158, %struct.ScmObj** %stackaddr$prim55817, align 8
%stackaddr$prim55818 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54158)
store volatile %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$prim55818, align 8
%ae48237 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55819 = alloca %struct.ScmObj*, align 8
%fptrToInt55820 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48238 to i64
%ae48238 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55820)
store volatile %struct.ScmObj* %ae48238, %struct.ScmObj** %stackaddr$makeclosure55819, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48238, %struct.ScmObj* %_37foldr147089, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48238, %struct.ScmObj* %_37map147085, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48238, %struct.ScmObj* %_37foldr47095, i64 2)
%argslist54215$k474730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55821 = alloca %struct.ScmObj*, align 8
%argslist54215$k474731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48238, %struct.ScmObj* %argslist54215$k474730)
store volatile %struct.ScmObj* %argslist54215$k474731, %struct.ScmObj** %stackaddr$prim55821, align 8
%stackaddr$prim55822 = alloca %struct.ScmObj*, align 8
%argslist54215$k474732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48237, %struct.ScmObj* %argslist54215$k474731)
store volatile %struct.ScmObj* %argslist54215$k474732, %struct.ScmObj** %stackaddr$prim55822, align 8
%clofunc55823 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47473)
musttail call tailcc void %clofunc55823(%struct.ScmObj* %k47473, %struct.ScmObj* %argslist54215$k474732)
ret void
}

define tailcc void @proc_clo$ae48238(%struct.ScmObj* %env$ae48238,%struct.ScmObj* %args4709647474) {
%stackaddr$env-ref55824 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48238, i64 0)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55824
%stackaddr$env-ref55825 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48238, i64 1)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55825
%stackaddr$env-ref55826 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48238, i64 2)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55826
%stackaddr$prim55827 = alloca %struct.ScmObj*, align 8
%k47475 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4709647474)
store volatile %struct.ScmObj* %k47475, %struct.ScmObj** %stackaddr$prim55827, align 8
%stackaddr$prim55828 = alloca %struct.ScmObj*, align 8
%args47096 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4709647474)
store volatile %struct.ScmObj* %args47096, %struct.ScmObj** %stackaddr$prim55828, align 8
%stackaddr$prim55829 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args47096)
store volatile %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$prim55829, align 8
%stackaddr$prim55830 = alloca %struct.ScmObj*, align 8
%anf_45bind47231 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47096)
store volatile %struct.ScmObj* %anf_45bind47231, %struct.ScmObj** %stackaddr$prim55830, align 8
%stackaddr$prim55831 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind47231)
store volatile %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$prim55831, align 8
%stackaddr$prim55832 = alloca %struct.ScmObj*, align 8
%anf_45bind47232 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args47096)
store volatile %struct.ScmObj* %anf_45bind47232, %struct.ScmObj** %stackaddr$prim55832, align 8
%stackaddr$prim55833 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind47232)
store volatile %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$prim55833, align 8
%stackaddr$makeclosure55834 = alloca %struct.ScmObj*, align 8
%fptrToInt55835 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48246 to i64
%ae48246 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55835)
store volatile %struct.ScmObj* %ae48246, %struct.ScmObj** %stackaddr$makeclosure55834, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48246, %struct.ScmObj* %k47475, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48246, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48246, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48246, %struct.ScmObj* %f47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48246, %struct.ScmObj* %acc47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48246, %struct.ScmObj* %lsts47097, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48246, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48247 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55836 = alloca %struct.ScmObj*, align 8
%fptrToInt55837 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48248 to i64
%ae48248 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55837)
store volatile %struct.ScmObj* %ae48248, %struct.ScmObj** %stackaddr$makeclosure55836, align 8
%argslist54214$ae482460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55838 = alloca %struct.ScmObj*, align 8
%argslist54214$ae482461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48248, %struct.ScmObj* %argslist54214$ae482460)
store volatile %struct.ScmObj* %argslist54214$ae482461, %struct.ScmObj** %stackaddr$prim55838, align 8
%stackaddr$prim55839 = alloca %struct.ScmObj*, align 8
%argslist54214$ae482462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48247, %struct.ScmObj* %argslist54214$ae482461)
store volatile %struct.ScmObj* %argslist54214$ae482462, %struct.ScmObj** %stackaddr$prim55839, align 8
%clofunc55840 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48246)
musttail call tailcc void %clofunc55840(%struct.ScmObj* %ae48246, %struct.ScmObj* %argslist54214$ae482462)
ret void
}

define tailcc void @proc_clo$ae48246(%struct.ScmObj* %env$ae48246,%struct.ScmObj* %current_45args54160) {
%stackaddr$env-ref55841 = alloca %struct.ScmObj*, align 8
%k47475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48246, i64 0)
store %struct.ScmObj* %k47475, %struct.ScmObj** %stackaddr$env-ref55841
%stackaddr$env-ref55842 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48246, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55842
%stackaddr$env-ref55843 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48246, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55843
%stackaddr$env-ref55844 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48246, i64 3)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55844
%stackaddr$env-ref55845 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48246, i64 4)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55845
%stackaddr$env-ref55846 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48246, i64 5)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref55846
%stackaddr$env-ref55847 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48246, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55847
%stackaddr$prim55848 = alloca %struct.ScmObj*, align 8
%_95k47476 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54160)
store volatile %struct.ScmObj* %_95k47476, %struct.ScmObj** %stackaddr$prim55848, align 8
%stackaddr$prim55849 = alloca %struct.ScmObj*, align 8
%current_45args54161 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54160)
store volatile %struct.ScmObj* %current_45args54161, %struct.ScmObj** %stackaddr$prim55849, align 8
%stackaddr$prim55850 = alloca %struct.ScmObj*, align 8
%anf_45bind47233 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54161)
store volatile %struct.ScmObj* %anf_45bind47233, %struct.ScmObj** %stackaddr$prim55850, align 8
%stackaddr$makeclosure55851 = alloca %struct.ScmObj*, align 8
%fptrToInt55852 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48278 to i64
%ae48278 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55852)
store volatile %struct.ScmObj* %ae48278, %struct.ScmObj** %stackaddr$makeclosure55851, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48278, %struct.ScmObj* %k47475, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48278, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48278, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48278, %struct.ScmObj* %f47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48278, %struct.ScmObj* %acc47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48278, %struct.ScmObj* %lsts47097, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48278, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48280 = call %struct.ScmObj* @const_init_false()
%argslist54207$_37foldr1470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55853 = alloca %struct.ScmObj*, align 8
%argslist54207$_37foldr1470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47097, %struct.ScmObj* %argslist54207$_37foldr1470890)
store volatile %struct.ScmObj* %argslist54207$_37foldr1470891, %struct.ScmObj** %stackaddr$prim55853, align 8
%stackaddr$prim55854 = alloca %struct.ScmObj*, align 8
%argslist54207$_37foldr1470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48280, %struct.ScmObj* %argslist54207$_37foldr1470891)
store volatile %struct.ScmObj* %argslist54207$_37foldr1470892, %struct.ScmObj** %stackaddr$prim55854, align 8
%stackaddr$prim55855 = alloca %struct.ScmObj*, align 8
%argslist54207$_37foldr1470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47233, %struct.ScmObj* %argslist54207$_37foldr1470892)
store volatile %struct.ScmObj* %argslist54207$_37foldr1470893, %struct.ScmObj** %stackaddr$prim55855, align 8
%stackaddr$prim55856 = alloca %struct.ScmObj*, align 8
%argslist54207$_37foldr1470894 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48278, %struct.ScmObj* %argslist54207$_37foldr1470893)
store volatile %struct.ScmObj* %argslist54207$_37foldr1470894, %struct.ScmObj** %stackaddr$prim55856, align 8
%clofunc55857 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147089)
musttail call tailcc void %clofunc55857(%struct.ScmObj* %_37foldr147089, %struct.ScmObj* %argslist54207$_37foldr1470894)
ret void
}

define tailcc void @proc_clo$ae48278(%struct.ScmObj* %env$ae48278,%struct.ScmObj* %current_45args54163) {
%stackaddr$env-ref55858 = alloca %struct.ScmObj*, align 8
%k47475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48278, i64 0)
store %struct.ScmObj* %k47475, %struct.ScmObj** %stackaddr$env-ref55858
%stackaddr$env-ref55859 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48278, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55859
%stackaddr$env-ref55860 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48278, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55860
%stackaddr$env-ref55861 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48278, i64 3)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55861
%stackaddr$env-ref55862 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48278, i64 4)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55862
%stackaddr$env-ref55863 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48278, i64 5)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref55863
%stackaddr$env-ref55864 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48278, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55864
%stackaddr$prim55865 = alloca %struct.ScmObj*, align 8
%_95k47477 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54163)
store volatile %struct.ScmObj* %_95k47477, %struct.ScmObj** %stackaddr$prim55865, align 8
%stackaddr$prim55866 = alloca %struct.ScmObj*, align 8
%current_45args54164 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54163)
store volatile %struct.ScmObj* %current_45args54164, %struct.ScmObj** %stackaddr$prim55866, align 8
%stackaddr$prim55867 = alloca %struct.ScmObj*, align 8
%anf_45bind47234 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54164)
store volatile %struct.ScmObj* %anf_45bind47234, %struct.ScmObj** %stackaddr$prim55867, align 8
%truthy$cmp55868 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47234)
%cmp$cmp55868 = icmp eq i64 %truthy$cmp55868, 1
br i1 %cmp$cmp55868, label %truebranch$cmp55868, label %falsebranch$cmp55868
truebranch$cmp55868:
%ae48289 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54166$k474750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55869 = alloca %struct.ScmObj*, align 8
%argslist54166$k474751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47098, %struct.ScmObj* %argslist54166$k474750)
store volatile %struct.ScmObj* %argslist54166$k474751, %struct.ScmObj** %stackaddr$prim55869, align 8
%stackaddr$prim55870 = alloca %struct.ScmObj*, align 8
%argslist54166$k474752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48289, %struct.ScmObj* %argslist54166$k474751)
store volatile %struct.ScmObj* %argslist54166$k474752, %struct.ScmObj** %stackaddr$prim55870, align 8
%clofunc55871 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47475)
musttail call tailcc void %clofunc55871(%struct.ScmObj* %k47475, %struct.ScmObj* %argslist54166$k474752)
ret void
falsebranch$cmp55868:
%stackaddr$makeclosure55872 = alloca %struct.ScmObj*, align 8
%fptrToInt55873 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48294 to i64
%ae48294 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55873)
store volatile %struct.ScmObj* %ae48294, %struct.ScmObj** %stackaddr$makeclosure55872, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48294, %struct.ScmObj* %k47475, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48294, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48294, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48294, %struct.ScmObj* %f47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48294, %struct.ScmObj* %acc47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48294, %struct.ScmObj* %lsts47097, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48294, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48295 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55874 = alloca %struct.ScmObj*, align 8
%fptrToInt55875 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48296 to i64
%ae48296 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55875)
store volatile %struct.ScmObj* %ae48296, %struct.ScmObj** %stackaddr$makeclosure55874, align 8
%argslist54206$ae482940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55876 = alloca %struct.ScmObj*, align 8
%argslist54206$ae482941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48296, %struct.ScmObj* %argslist54206$ae482940)
store volatile %struct.ScmObj* %argslist54206$ae482941, %struct.ScmObj** %stackaddr$prim55876, align 8
%stackaddr$prim55877 = alloca %struct.ScmObj*, align 8
%argslist54206$ae482942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48295, %struct.ScmObj* %argslist54206$ae482941)
store volatile %struct.ScmObj* %argslist54206$ae482942, %struct.ScmObj** %stackaddr$prim55877, align 8
%clofunc55878 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48294)
musttail call tailcc void %clofunc55878(%struct.ScmObj* %ae48294, %struct.ScmObj* %argslist54206$ae482942)
ret void
}

define tailcc void @proc_clo$ae48294(%struct.ScmObj* %env$ae48294,%struct.ScmObj* %current_45args54167) {
%stackaddr$env-ref55879 = alloca %struct.ScmObj*, align 8
%k47475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48294, i64 0)
store %struct.ScmObj* %k47475, %struct.ScmObj** %stackaddr$env-ref55879
%stackaddr$env-ref55880 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48294, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55880
%stackaddr$env-ref55881 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48294, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55881
%stackaddr$env-ref55882 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48294, i64 3)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55882
%stackaddr$env-ref55883 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48294, i64 4)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55883
%stackaddr$env-ref55884 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48294, i64 5)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref55884
%stackaddr$env-ref55885 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48294, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55885
%stackaddr$prim55886 = alloca %struct.ScmObj*, align 8
%_95k47478 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54167)
store volatile %struct.ScmObj* %_95k47478, %struct.ScmObj** %stackaddr$prim55886, align 8
%stackaddr$prim55887 = alloca %struct.ScmObj*, align 8
%current_45args54168 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54167)
store volatile %struct.ScmObj* %current_45args54168, %struct.ScmObj** %stackaddr$prim55887, align 8
%stackaddr$prim55888 = alloca %struct.ScmObj*, align 8
%anf_45bind47235 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54168)
store volatile %struct.ScmObj* %anf_45bind47235, %struct.ScmObj** %stackaddr$prim55888, align 8
%stackaddr$makeclosure55889 = alloca %struct.ScmObj*, align 8
%fptrToInt55890 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48315 to i64
%ae48315 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55890)
store volatile %struct.ScmObj* %ae48315, %struct.ScmObj** %stackaddr$makeclosure55889, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48315, %struct.ScmObj* %k47475, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48315, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48315, %struct.ScmObj* %_37map147085, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48315, %struct.ScmObj* %f47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48315, %struct.ScmObj* %acc47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48315, %struct.ScmObj* %lsts47097, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48315, %struct.ScmObj* %_37foldr47095, i64 6)
%argslist54201$_37map1470850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55891 = alloca %struct.ScmObj*, align 8
%argslist54201$_37map1470851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47097, %struct.ScmObj* %argslist54201$_37map1470850)
store volatile %struct.ScmObj* %argslist54201$_37map1470851, %struct.ScmObj** %stackaddr$prim55891, align 8
%stackaddr$prim55892 = alloca %struct.ScmObj*, align 8
%argslist54201$_37map1470852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47235, %struct.ScmObj* %argslist54201$_37map1470851)
store volatile %struct.ScmObj* %argslist54201$_37map1470852, %struct.ScmObj** %stackaddr$prim55892, align 8
%stackaddr$prim55893 = alloca %struct.ScmObj*, align 8
%argslist54201$_37map1470853 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48315, %struct.ScmObj* %argslist54201$_37map1470852)
store volatile %struct.ScmObj* %argslist54201$_37map1470853, %struct.ScmObj** %stackaddr$prim55893, align 8
%clofunc55894 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147085)
musttail call tailcc void %clofunc55894(%struct.ScmObj* %_37map147085, %struct.ScmObj* %argslist54201$_37map1470853)
ret void
}

define tailcc void @proc_clo$ae48315(%struct.ScmObj* %env$ae48315,%struct.ScmObj* %current_45args54170) {
%stackaddr$env-ref55895 = alloca %struct.ScmObj*, align 8
%k47475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48315, i64 0)
store %struct.ScmObj* %k47475, %struct.ScmObj** %stackaddr$env-ref55895
%stackaddr$env-ref55896 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48315, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55896
%stackaddr$env-ref55897 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48315, i64 2)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55897
%stackaddr$env-ref55898 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48315, i64 3)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55898
%stackaddr$env-ref55899 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48315, i64 4)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55899
%stackaddr$env-ref55900 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48315, i64 5)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref55900
%stackaddr$env-ref55901 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48315, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55901
%stackaddr$prim55902 = alloca %struct.ScmObj*, align 8
%_95k47479 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54170)
store volatile %struct.ScmObj* %_95k47479, %struct.ScmObj** %stackaddr$prim55902, align 8
%stackaddr$prim55903 = alloca %struct.ScmObj*, align 8
%current_45args54171 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54170)
store volatile %struct.ScmObj* %current_45args54171, %struct.ScmObj** %stackaddr$prim55903, align 8
%stackaddr$prim55904 = alloca %struct.ScmObj*, align 8
%lsts_4347104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54171)
store volatile %struct.ScmObj* %lsts_4347104, %struct.ScmObj** %stackaddr$prim55904, align 8
%stackaddr$makeclosure55905 = alloca %struct.ScmObj*, align 8
%fptrToInt55906 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48318 to i64
%ae48318 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55906)
store volatile %struct.ScmObj* %ae48318, %struct.ScmObj** %stackaddr$makeclosure55905, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48318, %struct.ScmObj* %k47475, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48318, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48318, %struct.ScmObj* %lsts_4347104, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48318, %struct.ScmObj* %_37map147085, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48318, %struct.ScmObj* %f47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48318, %struct.ScmObj* %acc47098, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48318, %struct.ScmObj* %lsts47097, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae48318, %struct.ScmObj* %_37foldr47095, i64 7)
%ae48319 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55907 = alloca %struct.ScmObj*, align 8
%fptrToInt55908 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48320 to i64
%ae48320 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55908)
store volatile %struct.ScmObj* %ae48320, %struct.ScmObj** %stackaddr$makeclosure55907, align 8
%argslist54200$ae483180 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55909 = alloca %struct.ScmObj*, align 8
%argslist54200$ae483181 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48320, %struct.ScmObj* %argslist54200$ae483180)
store volatile %struct.ScmObj* %argslist54200$ae483181, %struct.ScmObj** %stackaddr$prim55909, align 8
%stackaddr$prim55910 = alloca %struct.ScmObj*, align 8
%argslist54200$ae483182 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48319, %struct.ScmObj* %argslist54200$ae483181)
store volatile %struct.ScmObj* %argslist54200$ae483182, %struct.ScmObj** %stackaddr$prim55910, align 8
%clofunc55911 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48318)
musttail call tailcc void %clofunc55911(%struct.ScmObj* %ae48318, %struct.ScmObj* %argslist54200$ae483182)
ret void
}

define tailcc void @proc_clo$ae48318(%struct.ScmObj* %env$ae48318,%struct.ScmObj* %current_45args54173) {
%stackaddr$env-ref55912 = alloca %struct.ScmObj*, align 8
%k47475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48318, i64 0)
store %struct.ScmObj* %k47475, %struct.ScmObj** %stackaddr$env-ref55912
%stackaddr$env-ref55913 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48318, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55913
%stackaddr$env-ref55914 = alloca %struct.ScmObj*, align 8
%lsts_4347104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48318, i64 2)
store %struct.ScmObj* %lsts_4347104, %struct.ScmObj** %stackaddr$env-ref55914
%stackaddr$env-ref55915 = alloca %struct.ScmObj*, align 8
%_37map147085 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48318, i64 3)
store %struct.ScmObj* %_37map147085, %struct.ScmObj** %stackaddr$env-ref55915
%stackaddr$env-ref55916 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48318, i64 4)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55916
%stackaddr$env-ref55917 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48318, i64 5)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55917
%stackaddr$env-ref55918 = alloca %struct.ScmObj*, align 8
%lsts47097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48318, i64 6)
store %struct.ScmObj* %lsts47097, %struct.ScmObj** %stackaddr$env-ref55918
%stackaddr$env-ref55919 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48318, i64 7)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55919
%stackaddr$prim55920 = alloca %struct.ScmObj*, align 8
%_95k47480 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54173)
store volatile %struct.ScmObj* %_95k47480, %struct.ScmObj** %stackaddr$prim55920, align 8
%stackaddr$prim55921 = alloca %struct.ScmObj*, align 8
%current_45args54174 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54173)
store volatile %struct.ScmObj* %current_45args54174, %struct.ScmObj** %stackaddr$prim55921, align 8
%stackaddr$prim55922 = alloca %struct.ScmObj*, align 8
%anf_45bind47236 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54174)
store volatile %struct.ScmObj* %anf_45bind47236, %struct.ScmObj** %stackaddr$prim55922, align 8
%stackaddr$makeclosure55923 = alloca %struct.ScmObj*, align 8
%fptrToInt55924 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48339 to i64
%ae48339 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55924)
store volatile %struct.ScmObj* %ae48339, %struct.ScmObj** %stackaddr$makeclosure55923, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48339, %struct.ScmObj* %k47475, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48339, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48339, %struct.ScmObj* %lsts_4347104, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48339, %struct.ScmObj* %f47099, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48339, %struct.ScmObj* %acc47098, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48339, %struct.ScmObj* %_37foldr47095, i64 5)
%argslist54195$_37map1470850 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55925 = alloca %struct.ScmObj*, align 8
%argslist54195$_37map1470851 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts47097, %struct.ScmObj* %argslist54195$_37map1470850)
store volatile %struct.ScmObj* %argslist54195$_37map1470851, %struct.ScmObj** %stackaddr$prim55925, align 8
%stackaddr$prim55926 = alloca %struct.ScmObj*, align 8
%argslist54195$_37map1470852 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47236, %struct.ScmObj* %argslist54195$_37map1470851)
store volatile %struct.ScmObj* %argslist54195$_37map1470852, %struct.ScmObj** %stackaddr$prim55926, align 8
%stackaddr$prim55927 = alloca %struct.ScmObj*, align 8
%argslist54195$_37map1470853 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48339, %struct.ScmObj* %argslist54195$_37map1470852)
store volatile %struct.ScmObj* %argslist54195$_37map1470853, %struct.ScmObj** %stackaddr$prim55927, align 8
%clofunc55928 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map147085)
musttail call tailcc void %clofunc55928(%struct.ScmObj* %_37map147085, %struct.ScmObj* %argslist54195$_37map1470853)
ret void
}

define tailcc void @proc_clo$ae48339(%struct.ScmObj* %env$ae48339,%struct.ScmObj* %current_45args54176) {
%stackaddr$env-ref55929 = alloca %struct.ScmObj*, align 8
%k47475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48339, i64 0)
store %struct.ScmObj* %k47475, %struct.ScmObj** %stackaddr$env-ref55929
%stackaddr$env-ref55930 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48339, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55930
%stackaddr$env-ref55931 = alloca %struct.ScmObj*, align 8
%lsts_4347104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48339, i64 2)
store %struct.ScmObj* %lsts_4347104, %struct.ScmObj** %stackaddr$env-ref55931
%stackaddr$env-ref55932 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48339, i64 3)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55932
%stackaddr$env-ref55933 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48339, i64 4)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55933
%stackaddr$env-ref55934 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48339, i64 5)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55934
%stackaddr$prim55935 = alloca %struct.ScmObj*, align 8
%_95k47481 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54176)
store volatile %struct.ScmObj* %_95k47481, %struct.ScmObj** %stackaddr$prim55935, align 8
%stackaddr$prim55936 = alloca %struct.ScmObj*, align 8
%current_45args54177 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54176)
store volatile %struct.ScmObj* %current_45args54177, %struct.ScmObj** %stackaddr$prim55936, align 8
%stackaddr$prim55937 = alloca %struct.ScmObj*, align 8
%vs47102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54177)
store volatile %struct.ScmObj* %vs47102, %struct.ScmObj** %stackaddr$prim55937, align 8
%stackaddr$makeclosure55938 = alloca %struct.ScmObj*, align 8
%fptrToInt55939 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48342 to i64
%ae48342 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55939)
store volatile %struct.ScmObj* %ae48342, %struct.ScmObj** %stackaddr$makeclosure55938, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48342, %struct.ScmObj* %k47475, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48342, %struct.ScmObj* %_37foldr147089, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48342, %struct.ScmObj* %lsts_4347104, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48342, %struct.ScmObj* %vs47102, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48342, %struct.ScmObj* %f47099, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae48342, %struct.ScmObj* %acc47098, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae48342, %struct.ScmObj* %_37foldr47095, i64 6)
%ae48343 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55940 = alloca %struct.ScmObj*, align 8
%fptrToInt55941 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48344 to i64
%ae48344 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55941)
store volatile %struct.ScmObj* %ae48344, %struct.ScmObj** %stackaddr$makeclosure55940, align 8
%argslist54194$ae483420 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55942 = alloca %struct.ScmObj*, align 8
%argslist54194$ae483421 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48344, %struct.ScmObj* %argslist54194$ae483420)
store volatile %struct.ScmObj* %argslist54194$ae483421, %struct.ScmObj** %stackaddr$prim55942, align 8
%stackaddr$prim55943 = alloca %struct.ScmObj*, align 8
%argslist54194$ae483422 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48343, %struct.ScmObj* %argslist54194$ae483421)
store volatile %struct.ScmObj* %argslist54194$ae483422, %struct.ScmObj** %stackaddr$prim55943, align 8
%clofunc55944 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48342)
musttail call tailcc void %clofunc55944(%struct.ScmObj* %ae48342, %struct.ScmObj* %argslist54194$ae483422)
ret void
}

define tailcc void @proc_clo$ae48342(%struct.ScmObj* %env$ae48342,%struct.ScmObj* %current_45args54179) {
%stackaddr$env-ref55945 = alloca %struct.ScmObj*, align 8
%k47475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48342, i64 0)
store %struct.ScmObj* %k47475, %struct.ScmObj** %stackaddr$env-ref55945
%stackaddr$env-ref55946 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48342, i64 1)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55946
%stackaddr$env-ref55947 = alloca %struct.ScmObj*, align 8
%lsts_4347104 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48342, i64 2)
store %struct.ScmObj* %lsts_4347104, %struct.ScmObj** %stackaddr$env-ref55947
%stackaddr$env-ref55948 = alloca %struct.ScmObj*, align 8
%vs47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48342, i64 3)
store %struct.ScmObj* %vs47102, %struct.ScmObj** %stackaddr$env-ref55948
%stackaddr$env-ref55949 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48342, i64 4)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55949
%stackaddr$env-ref55950 = alloca %struct.ScmObj*, align 8
%acc47098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48342, i64 5)
store %struct.ScmObj* %acc47098, %struct.ScmObj** %stackaddr$env-ref55950
%stackaddr$env-ref55951 = alloca %struct.ScmObj*, align 8
%_37foldr47095 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48342, i64 6)
store %struct.ScmObj* %_37foldr47095, %struct.ScmObj** %stackaddr$env-ref55951
%stackaddr$prim55952 = alloca %struct.ScmObj*, align 8
%_95k47482 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54179)
store volatile %struct.ScmObj* %_95k47482, %struct.ScmObj** %stackaddr$prim55952, align 8
%stackaddr$prim55953 = alloca %struct.ScmObj*, align 8
%current_45args54180 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54179)
store volatile %struct.ScmObj* %current_45args54180, %struct.ScmObj** %stackaddr$prim55953, align 8
%stackaddr$prim55954 = alloca %struct.ScmObj*, align 8
%anf_45bind47237 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54180)
store volatile %struct.ScmObj* %anf_45bind47237, %struct.ScmObj** %stackaddr$prim55954, align 8
%stackaddr$prim55955 = alloca %struct.ScmObj*, align 8
%anf_45bind47238 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47098, %struct.ScmObj* %lsts_4347104)
store volatile %struct.ScmObj* %anf_45bind47238, %struct.ScmObj** %stackaddr$prim55955, align 8
%stackaddr$prim55956 = alloca %struct.ScmObj*, align 8
%anf_45bind47239 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47099, %struct.ScmObj* %anf_45bind47238)
store volatile %struct.ScmObj* %anf_45bind47239, %struct.ScmObj** %stackaddr$prim55956, align 8
%stackaddr$makeclosure55957 = alloca %struct.ScmObj*, align 8
%fptrToInt55958 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48368 to i64
%ae48368 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55958)
store volatile %struct.ScmObj* %ae48368, %struct.ScmObj** %stackaddr$makeclosure55957, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48368, %struct.ScmObj* %anf_45bind47237, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48368, %struct.ScmObj* %k47475, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48368, %struct.ScmObj* %_37foldr147089, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48368, %struct.ScmObj* %vs47102, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48368, %struct.ScmObj* %f47099, i64 4)
%stackaddr$prim55959 = alloca %struct.ScmObj*, align 8
%cpsargs47486 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48368, %struct.ScmObj* %anf_45bind47239)
store volatile %struct.ScmObj* %cpsargs47486, %struct.ScmObj** %stackaddr$prim55959, align 8
%clofunc55960 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr47095)
musttail call tailcc void %clofunc55960(%struct.ScmObj* %_37foldr47095, %struct.ScmObj* %cpsargs47486)
ret void
}

define tailcc void @proc_clo$ae48368(%struct.ScmObj* %env$ae48368,%struct.ScmObj* %current_45args54182) {
%stackaddr$env-ref55961 = alloca %struct.ScmObj*, align 8
%anf_45bind47237 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48368, i64 0)
store %struct.ScmObj* %anf_45bind47237, %struct.ScmObj** %stackaddr$env-ref55961
%stackaddr$env-ref55962 = alloca %struct.ScmObj*, align 8
%k47475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48368, i64 1)
store %struct.ScmObj* %k47475, %struct.ScmObj** %stackaddr$env-ref55962
%stackaddr$env-ref55963 = alloca %struct.ScmObj*, align 8
%_37foldr147089 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48368, i64 2)
store %struct.ScmObj* %_37foldr147089, %struct.ScmObj** %stackaddr$env-ref55963
%stackaddr$env-ref55964 = alloca %struct.ScmObj*, align 8
%vs47102 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48368, i64 3)
store %struct.ScmObj* %vs47102, %struct.ScmObj** %stackaddr$env-ref55964
%stackaddr$env-ref55965 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48368, i64 4)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55965
%stackaddr$prim55966 = alloca %struct.ScmObj*, align 8
%_95k47483 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54182)
store volatile %struct.ScmObj* %_95k47483, %struct.ScmObj** %stackaddr$prim55966, align 8
%stackaddr$prim55967 = alloca %struct.ScmObj*, align 8
%current_45args54183 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54182)
store volatile %struct.ScmObj* %current_45args54183, %struct.ScmObj** %stackaddr$prim55967, align 8
%stackaddr$prim55968 = alloca %struct.ScmObj*, align 8
%anf_45bind47240 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54183)
store volatile %struct.ScmObj* %anf_45bind47240, %struct.ScmObj** %stackaddr$prim55968, align 8
%ae48373 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55969 = alloca %struct.ScmObj*, align 8
%anf_45bind47241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47240, %struct.ScmObj* %ae48373)
store volatile %struct.ScmObj* %anf_45bind47241, %struct.ScmObj** %stackaddr$prim55969, align 8
%stackaddr$makeclosure55970 = alloca %struct.ScmObj*, align 8
%fptrToInt55971 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48375 to i64
%ae48375 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55971)
store volatile %struct.ScmObj* %ae48375, %struct.ScmObj** %stackaddr$makeclosure55970, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48375, %struct.ScmObj* %k47475, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48375, %struct.ScmObj* %f47099, i64 1)
%argslist54188$_37foldr1470890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55972 = alloca %struct.ScmObj*, align 8
%argslist54188$_37foldr1470891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs47102, %struct.ScmObj* %argslist54188$_37foldr1470890)
store volatile %struct.ScmObj* %argslist54188$_37foldr1470891, %struct.ScmObj** %stackaddr$prim55972, align 8
%stackaddr$prim55973 = alloca %struct.ScmObj*, align 8
%argslist54188$_37foldr1470892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47241, %struct.ScmObj* %argslist54188$_37foldr1470891)
store volatile %struct.ScmObj* %argslist54188$_37foldr1470892, %struct.ScmObj** %stackaddr$prim55973, align 8
%stackaddr$prim55974 = alloca %struct.ScmObj*, align 8
%argslist54188$_37foldr1470893 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47237, %struct.ScmObj* %argslist54188$_37foldr1470892)
store volatile %struct.ScmObj* %argslist54188$_37foldr1470893, %struct.ScmObj** %stackaddr$prim55974, align 8
%stackaddr$prim55975 = alloca %struct.ScmObj*, align 8
%argslist54188$_37foldr1470894 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48375, %struct.ScmObj* %argslist54188$_37foldr1470893)
store volatile %struct.ScmObj* %argslist54188$_37foldr1470894, %struct.ScmObj** %stackaddr$prim55975, align 8
%clofunc55976 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147089)
musttail call tailcc void %clofunc55976(%struct.ScmObj* %_37foldr147089, %struct.ScmObj* %argslist54188$_37foldr1470894)
ret void
}

define tailcc void @proc_clo$ae48375(%struct.ScmObj* %env$ae48375,%struct.ScmObj* %current_45args54185) {
%stackaddr$env-ref55977 = alloca %struct.ScmObj*, align 8
%k47475 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48375, i64 0)
store %struct.ScmObj* %k47475, %struct.ScmObj** %stackaddr$env-ref55977
%stackaddr$env-ref55978 = alloca %struct.ScmObj*, align 8
%f47099 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48375, i64 1)
store %struct.ScmObj* %f47099, %struct.ScmObj** %stackaddr$env-ref55978
%stackaddr$prim55979 = alloca %struct.ScmObj*, align 8
%_95k47484 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54185)
store volatile %struct.ScmObj* %_95k47484, %struct.ScmObj** %stackaddr$prim55979, align 8
%stackaddr$prim55980 = alloca %struct.ScmObj*, align 8
%current_45args54186 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54185)
store volatile %struct.ScmObj* %current_45args54186, %struct.ScmObj** %stackaddr$prim55980, align 8
%stackaddr$prim55981 = alloca %struct.ScmObj*, align 8
%anf_45bind47242 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54186)
store volatile %struct.ScmObj* %anf_45bind47242, %struct.ScmObj** %stackaddr$prim55981, align 8
%stackaddr$prim55982 = alloca %struct.ScmObj*, align 8
%cpsargs47485 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47475, %struct.ScmObj* %anf_45bind47242)
store volatile %struct.ScmObj* %cpsargs47485, %struct.ScmObj** %stackaddr$prim55982, align 8
%clofunc55983 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47099)
musttail call tailcc void %clofunc55983(%struct.ScmObj* %f47099, %struct.ScmObj* %cpsargs47485)
ret void
}

define tailcc void @proc_clo$ae48344(%struct.ScmObj* %env$ae48344,%struct.ScmObj* %current_45args54189) {
%stackaddr$prim55984 = alloca %struct.ScmObj*, align 8
%k47487 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54189)
store volatile %struct.ScmObj* %k47487, %struct.ScmObj** %stackaddr$prim55984, align 8
%stackaddr$prim55985 = alloca %struct.ScmObj*, align 8
%current_45args54190 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54189)
store volatile %struct.ScmObj* %current_45args54190, %struct.ScmObj** %stackaddr$prim55985, align 8
%stackaddr$prim55986 = alloca %struct.ScmObj*, align 8
%a47107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54190)
store volatile %struct.ScmObj* %a47107, %struct.ScmObj** %stackaddr$prim55986, align 8
%stackaddr$prim55987 = alloca %struct.ScmObj*, align 8
%current_45args54191 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54190)
store volatile %struct.ScmObj* %current_45args54191, %struct.ScmObj** %stackaddr$prim55987, align 8
%stackaddr$prim55988 = alloca %struct.ScmObj*, align 8
%b47106 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54191)
store volatile %struct.ScmObj* %b47106, %struct.ScmObj** %stackaddr$prim55988, align 8
%stackaddr$prim55989 = alloca %struct.ScmObj*, align 8
%cpsprim47488 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a47107, %struct.ScmObj* %b47106)
store volatile %struct.ScmObj* %cpsprim47488, %struct.ScmObj** %stackaddr$prim55989, align 8
%ae48348 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54193$k474870 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55990 = alloca %struct.ScmObj*, align 8
%argslist54193$k474871 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47488, %struct.ScmObj* %argslist54193$k474870)
store volatile %struct.ScmObj* %argslist54193$k474871, %struct.ScmObj** %stackaddr$prim55990, align 8
%stackaddr$prim55991 = alloca %struct.ScmObj*, align 8
%argslist54193$k474872 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48348, %struct.ScmObj* %argslist54193$k474871)
store volatile %struct.ScmObj* %argslist54193$k474872, %struct.ScmObj** %stackaddr$prim55991, align 8
%clofunc55992 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47487)
musttail call tailcc void %clofunc55992(%struct.ScmObj* %k47487, %struct.ScmObj* %argslist54193$k474872)
ret void
}

define tailcc void @proc_clo$ae48320(%struct.ScmObj* %env$ae48320,%struct.ScmObj* %current_45args54196) {
%stackaddr$prim55993 = alloca %struct.ScmObj*, align 8
%k47489 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54196)
store volatile %struct.ScmObj* %k47489, %struct.ScmObj** %stackaddr$prim55993, align 8
%stackaddr$prim55994 = alloca %struct.ScmObj*, align 8
%current_45args54197 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54196)
store volatile %struct.ScmObj* %current_45args54197, %struct.ScmObj** %stackaddr$prim55994, align 8
%stackaddr$prim55995 = alloca %struct.ScmObj*, align 8
%x47103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54197)
store volatile %struct.ScmObj* %x47103, %struct.ScmObj** %stackaddr$prim55995, align 8
%stackaddr$prim55996 = alloca %struct.ScmObj*, align 8
%cpsprim47490 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x47103)
store volatile %struct.ScmObj* %cpsprim47490, %struct.ScmObj** %stackaddr$prim55996, align 8
%ae48323 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54199$k474890 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55997 = alloca %struct.ScmObj*, align 8
%argslist54199$k474891 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47490, %struct.ScmObj* %argslist54199$k474890)
store volatile %struct.ScmObj* %argslist54199$k474891, %struct.ScmObj** %stackaddr$prim55997, align 8
%stackaddr$prim55998 = alloca %struct.ScmObj*, align 8
%argslist54199$k474892 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48323, %struct.ScmObj* %argslist54199$k474891)
store volatile %struct.ScmObj* %argslist54199$k474892, %struct.ScmObj** %stackaddr$prim55998, align 8
%clofunc55999 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47489)
musttail call tailcc void %clofunc55999(%struct.ScmObj* %k47489, %struct.ScmObj* %argslist54199$k474892)
ret void
}

define tailcc void @proc_clo$ae48296(%struct.ScmObj* %env$ae48296,%struct.ScmObj* %current_45args54202) {
%stackaddr$prim56000 = alloca %struct.ScmObj*, align 8
%k47491 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54202)
store volatile %struct.ScmObj* %k47491, %struct.ScmObj** %stackaddr$prim56000, align 8
%stackaddr$prim56001 = alloca %struct.ScmObj*, align 8
%current_45args54203 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54202)
store volatile %struct.ScmObj* %current_45args54203, %struct.ScmObj** %stackaddr$prim56001, align 8
%stackaddr$prim56002 = alloca %struct.ScmObj*, align 8
%x47105 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54203)
store volatile %struct.ScmObj* %x47105, %struct.ScmObj** %stackaddr$prim56002, align 8
%stackaddr$prim56003 = alloca %struct.ScmObj*, align 8
%cpsprim47492 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x47105)
store volatile %struct.ScmObj* %cpsprim47492, %struct.ScmObj** %stackaddr$prim56003, align 8
%ae48299 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54205$k474910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56004 = alloca %struct.ScmObj*, align 8
%argslist54205$k474911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47492, %struct.ScmObj* %argslist54205$k474910)
store volatile %struct.ScmObj* %argslist54205$k474911, %struct.ScmObj** %stackaddr$prim56004, align 8
%stackaddr$prim56005 = alloca %struct.ScmObj*, align 8
%argslist54205$k474912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48299, %struct.ScmObj* %argslist54205$k474911)
store volatile %struct.ScmObj* %argslist54205$k474912, %struct.ScmObj** %stackaddr$prim56005, align 8
%clofunc56006 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47491)
musttail call tailcc void %clofunc56006(%struct.ScmObj* %k47491, %struct.ScmObj* %argslist54205$k474912)
ret void
}

define tailcc void @proc_clo$ae48248(%struct.ScmObj* %env$ae48248,%struct.ScmObj* %current_45args54208) {
%stackaddr$prim56007 = alloca %struct.ScmObj*, align 8
%k47493 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54208)
store volatile %struct.ScmObj* %k47493, %struct.ScmObj** %stackaddr$prim56007, align 8
%stackaddr$prim56008 = alloca %struct.ScmObj*, align 8
%current_45args54209 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54208)
store volatile %struct.ScmObj* %current_45args54209, %struct.ScmObj** %stackaddr$prim56008, align 8
%stackaddr$prim56009 = alloca %struct.ScmObj*, align 8
%lst47101 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54209)
store volatile %struct.ScmObj* %lst47101, %struct.ScmObj** %stackaddr$prim56009, align 8
%stackaddr$prim56010 = alloca %struct.ScmObj*, align 8
%current_45args54210 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54209)
store volatile %struct.ScmObj* %current_45args54210, %struct.ScmObj** %stackaddr$prim56010, align 8
%stackaddr$prim56011 = alloca %struct.ScmObj*, align 8
%b47100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54210)
store volatile %struct.ScmObj* %b47100, %struct.ScmObj** %stackaddr$prim56011, align 8
%truthy$cmp56012 = call i64 @is_truthy_value(%struct.ScmObj* %b47100)
%cmp$cmp56012 = icmp eq i64 %truthy$cmp56012, 1
br i1 %cmp$cmp56012, label %truebranch$cmp56012, label %falsebranch$cmp56012
truebranch$cmp56012:
%ae48251 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54212$k474930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56013 = alloca %struct.ScmObj*, align 8
%argslist54212$k474931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b47100, %struct.ScmObj* %argslist54212$k474930)
store volatile %struct.ScmObj* %argslist54212$k474931, %struct.ScmObj** %stackaddr$prim56013, align 8
%stackaddr$prim56014 = alloca %struct.ScmObj*, align 8
%argslist54212$k474932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48251, %struct.ScmObj* %argslist54212$k474931)
store volatile %struct.ScmObj* %argslist54212$k474932, %struct.ScmObj** %stackaddr$prim56014, align 8
%clofunc56015 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47493)
musttail call tailcc void %clofunc56015(%struct.ScmObj* %k47493, %struct.ScmObj* %argslist54212$k474932)
ret void
falsebranch$cmp56012:
%stackaddr$prim56016 = alloca %struct.ScmObj*, align 8
%cpsprim47494 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47101)
store volatile %struct.ScmObj* %cpsprim47494, %struct.ScmObj** %stackaddr$prim56016, align 8
%ae48258 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54213$k474930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56017 = alloca %struct.ScmObj*, align 8
%argslist54213$k474931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47494, %struct.ScmObj* %argslist54213$k474930)
store volatile %struct.ScmObj* %argslist54213$k474931, %struct.ScmObj** %stackaddr$prim56017, align 8
%stackaddr$prim56018 = alloca %struct.ScmObj*, align 8
%argslist54213$k474932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48258, %struct.ScmObj* %argslist54213$k474931)
store volatile %struct.ScmObj* %argslist54213$k474932, %struct.ScmObj** %stackaddr$prim56018, align 8
%clofunc56019 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47493)
musttail call tailcc void %clofunc56019(%struct.ScmObj* %k47493, %struct.ScmObj* %argslist54213$k474932)
ret void
}

define tailcc void @proc_clo$ae48205(%struct.ScmObj* %env$ae48205,%struct.ScmObj* %current_45args54217) {
%stackaddr$env-ref56020 = alloca %struct.ScmObj*, align 8
%_37length47078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48205, i64 0)
store %struct.ScmObj* %_37length47078, %struct.ScmObj** %stackaddr$env-ref56020
%stackaddr$env-ref56021 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48205, i64 1)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref56021
%stackaddr$prim56022 = alloca %struct.ScmObj*, align 8
%k47495 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54217)
store volatile %struct.ScmObj* %k47495, %struct.ScmObj** %stackaddr$prim56022, align 8
%stackaddr$prim56023 = alloca %struct.ScmObj*, align 8
%current_45args54218 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54217)
store volatile %struct.ScmObj* %current_45args54218, %struct.ScmObj** %stackaddr$prim56023, align 8
%stackaddr$prim56024 = alloca %struct.ScmObj*, align 8
%lst47110 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54218)
store volatile %struct.ScmObj* %lst47110, %struct.ScmObj** %stackaddr$prim56024, align 8
%stackaddr$prim56025 = alloca %struct.ScmObj*, align 8
%current_45args54219 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54218)
store volatile %struct.ScmObj* %current_45args54219, %struct.ScmObj** %stackaddr$prim56025, align 8
%stackaddr$prim56026 = alloca %struct.ScmObj*, align 8
%n47109 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54219)
store volatile %struct.ScmObj* %n47109, %struct.ScmObj** %stackaddr$prim56026, align 8
%stackaddr$makeclosure56027 = alloca %struct.ScmObj*, align 8
%fptrToInt56028 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48207 to i64
%ae48207 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56028)
store volatile %struct.ScmObj* %ae48207, %struct.ScmObj** %stackaddr$makeclosure56027, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48207, %struct.ScmObj* %lst47110, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48207, %struct.ScmObj* %n47109, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48207, %struct.ScmObj* %_37take47081, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48207, %struct.ScmObj* %k47495, i64 3)
%argslist54225$_37length470780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56029 = alloca %struct.ScmObj*, align 8
%argslist54225$_37length470781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47110, %struct.ScmObj* %argslist54225$_37length470780)
store volatile %struct.ScmObj* %argslist54225$_37length470781, %struct.ScmObj** %stackaddr$prim56029, align 8
%stackaddr$prim56030 = alloca %struct.ScmObj*, align 8
%argslist54225$_37length470782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48207, %struct.ScmObj* %argslist54225$_37length470781)
store volatile %struct.ScmObj* %argslist54225$_37length470782, %struct.ScmObj** %stackaddr$prim56030, align 8
%clofunc56031 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47078)
musttail call tailcc void %clofunc56031(%struct.ScmObj* %_37length47078, %struct.ScmObj* %argslist54225$_37length470782)
ret void
}

define tailcc void @proc_clo$ae48207(%struct.ScmObj* %env$ae48207,%struct.ScmObj* %current_45args54221) {
%stackaddr$env-ref56032 = alloca %struct.ScmObj*, align 8
%lst47110 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48207, i64 0)
store %struct.ScmObj* %lst47110, %struct.ScmObj** %stackaddr$env-ref56032
%stackaddr$env-ref56033 = alloca %struct.ScmObj*, align 8
%n47109 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48207, i64 1)
store %struct.ScmObj* %n47109, %struct.ScmObj** %stackaddr$env-ref56033
%stackaddr$env-ref56034 = alloca %struct.ScmObj*, align 8
%_37take47081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48207, i64 2)
store %struct.ScmObj* %_37take47081, %struct.ScmObj** %stackaddr$env-ref56034
%stackaddr$env-ref56035 = alloca %struct.ScmObj*, align 8
%k47495 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48207, i64 3)
store %struct.ScmObj* %k47495, %struct.ScmObj** %stackaddr$env-ref56035
%stackaddr$prim56036 = alloca %struct.ScmObj*, align 8
%_95k47496 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54221)
store volatile %struct.ScmObj* %_95k47496, %struct.ScmObj** %stackaddr$prim56036, align 8
%stackaddr$prim56037 = alloca %struct.ScmObj*, align 8
%current_45args54222 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54221)
store volatile %struct.ScmObj* %current_45args54222, %struct.ScmObj** %stackaddr$prim56037, align 8
%stackaddr$prim56038 = alloca %struct.ScmObj*, align 8
%anf_45bind47229 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54222)
store volatile %struct.ScmObj* %anf_45bind47229, %struct.ScmObj** %stackaddr$prim56038, align 8
%stackaddr$prim56039 = alloca %struct.ScmObj*, align 8
%anf_45bind47230 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind47229, %struct.ScmObj* %n47109)
store volatile %struct.ScmObj* %anf_45bind47230, %struct.ScmObj** %stackaddr$prim56039, align 8
%argslist54224$_37take470810 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56040 = alloca %struct.ScmObj*, align 8
%argslist54224$_37take470811 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47230, %struct.ScmObj* %argslist54224$_37take470810)
store volatile %struct.ScmObj* %argslist54224$_37take470811, %struct.ScmObj** %stackaddr$prim56040, align 8
%stackaddr$prim56041 = alloca %struct.ScmObj*, align 8
%argslist54224$_37take470812 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47110, %struct.ScmObj* %argslist54224$_37take470811)
store volatile %struct.ScmObj* %argslist54224$_37take470812, %struct.ScmObj** %stackaddr$prim56041, align 8
%stackaddr$prim56042 = alloca %struct.ScmObj*, align 8
%argslist54224$_37take470813 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47495, %struct.ScmObj* %argslist54224$_37take470812)
store volatile %struct.ScmObj* %argslist54224$_37take470813, %struct.ScmObj** %stackaddr$prim56042, align 8
%clofunc56043 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47081)
musttail call tailcc void %clofunc56043(%struct.ScmObj* %_37take47081, %struct.ScmObj* %argslist54224$_37take470813)
ret void
}

define tailcc void @proc_clo$ae48151(%struct.ScmObj* %env$ae48151,%struct.ScmObj* %current_45args54227) {
%stackaddr$env-ref56044 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48151, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref56044
%stackaddr$prim56045 = alloca %struct.ScmObj*, align 8
%k47497 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54227)
store volatile %struct.ScmObj* %k47497, %struct.ScmObj** %stackaddr$prim56045, align 8
%stackaddr$prim56046 = alloca %struct.ScmObj*, align 8
%current_45args54228 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54227)
store volatile %struct.ScmObj* %current_45args54228, %struct.ScmObj** %stackaddr$prim56046, align 8
%stackaddr$prim56047 = alloca %struct.ScmObj*, align 8
%lst47112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54228)
store volatile %struct.ScmObj* %lst47112, %struct.ScmObj** %stackaddr$prim56047, align 8
%stackaddr$makeclosure56048 = alloca %struct.ScmObj*, align 8
%fptrToInt56049 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48152 to i64
%ae48152 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56049)
store volatile %struct.ScmObj* %ae48152, %struct.ScmObj** %stackaddr$makeclosure56048, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48152, %struct.ScmObj* %_37foldl147073, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48152, %struct.ScmObj* %k47497, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48152, %struct.ScmObj* %lst47112, i64 2)
%ae48153 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56050 = alloca %struct.ScmObj*, align 8
%fptrToInt56051 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48154 to i64
%ae48154 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt56051)
store volatile %struct.ScmObj* %ae48154, %struct.ScmObj** %stackaddr$makeclosure56050, align 8
%argslist54239$ae481520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56052 = alloca %struct.ScmObj*, align 8
%argslist54239$ae481521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48154, %struct.ScmObj* %argslist54239$ae481520)
store volatile %struct.ScmObj* %argslist54239$ae481521, %struct.ScmObj** %stackaddr$prim56052, align 8
%stackaddr$prim56053 = alloca %struct.ScmObj*, align 8
%argslist54239$ae481522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48153, %struct.ScmObj* %argslist54239$ae481521)
store volatile %struct.ScmObj* %argslist54239$ae481522, %struct.ScmObj** %stackaddr$prim56053, align 8
%clofunc56054 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48152)
musttail call tailcc void %clofunc56054(%struct.ScmObj* %ae48152, %struct.ScmObj* %argslist54239$ae481522)
ret void
}

define tailcc void @proc_clo$ae48152(%struct.ScmObj* %env$ae48152,%struct.ScmObj* %current_45args54230) {
%stackaddr$env-ref56055 = alloca %struct.ScmObj*, align 8
%_37foldl147073 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48152, i64 0)
store %struct.ScmObj* %_37foldl147073, %struct.ScmObj** %stackaddr$env-ref56055
%stackaddr$env-ref56056 = alloca %struct.ScmObj*, align 8
%k47497 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48152, i64 1)
store %struct.ScmObj* %k47497, %struct.ScmObj** %stackaddr$env-ref56056
%stackaddr$env-ref56057 = alloca %struct.ScmObj*, align 8
%lst47112 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48152, i64 2)
store %struct.ScmObj* %lst47112, %struct.ScmObj** %stackaddr$env-ref56057
%stackaddr$prim56058 = alloca %struct.ScmObj*, align 8
%_95k47498 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54230)
store volatile %struct.ScmObj* %_95k47498, %struct.ScmObj** %stackaddr$prim56058, align 8
%stackaddr$prim56059 = alloca %struct.ScmObj*, align 8
%current_45args54231 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54230)
store volatile %struct.ScmObj* %current_45args54231, %struct.ScmObj** %stackaddr$prim56059, align 8
%stackaddr$prim56060 = alloca %struct.ScmObj*, align 8
%anf_45bind47228 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54231)
store volatile %struct.ScmObj* %anf_45bind47228, %struct.ScmObj** %stackaddr$prim56060, align 8
%ae48173 = call %struct.ScmObj* @const_init_null()
%argslist54233$_37foldl1470730 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56061 = alloca %struct.ScmObj*, align 8
%argslist54233$_37foldl1470731 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst47112, %struct.ScmObj* %argslist54233$_37foldl1470730)
store volatile %struct.ScmObj* %argslist54233$_37foldl1470731, %struct.ScmObj** %stackaddr$prim56061, align 8
%stackaddr$prim56062 = alloca %struct.ScmObj*, align 8
%argslist54233$_37foldl1470732 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48173, %struct.ScmObj* %argslist54233$_37foldl1470731)
store volatile %struct.ScmObj* %argslist54233$_37foldl1470732, %struct.ScmObj** %stackaddr$prim56062, align 8
%stackaddr$prim56063 = alloca %struct.ScmObj*, align 8
%argslist54233$_37foldl1470733 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47228, %struct.ScmObj* %argslist54233$_37foldl1470732)
store volatile %struct.ScmObj* %argslist54233$_37foldl1470733, %struct.ScmObj** %stackaddr$prim56063, align 8
%stackaddr$prim56064 = alloca %struct.ScmObj*, align 8
%argslist54233$_37foldl1470734 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47497, %struct.ScmObj* %argslist54233$_37foldl1470733)
store volatile %struct.ScmObj* %argslist54233$_37foldl1470734, %struct.ScmObj** %stackaddr$prim56064, align 8
%clofunc56065 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147073)
musttail call tailcc void %clofunc56065(%struct.ScmObj* %_37foldl147073, %struct.ScmObj* %argslist54233$_37foldl1470734)
ret void
}

define tailcc void @proc_clo$ae48154(%struct.ScmObj* %env$ae48154,%struct.ScmObj* %current_45args54234) {
%stackaddr$prim56066 = alloca %struct.ScmObj*, align 8
%k47499 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54234)
store volatile %struct.ScmObj* %k47499, %struct.ScmObj** %stackaddr$prim56066, align 8
%stackaddr$prim56067 = alloca %struct.ScmObj*, align 8
%current_45args54235 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54234)
store volatile %struct.ScmObj* %current_45args54235, %struct.ScmObj** %stackaddr$prim56067, align 8
%stackaddr$prim56068 = alloca %struct.ScmObj*, align 8
%x47114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54235)
store volatile %struct.ScmObj* %x47114, %struct.ScmObj** %stackaddr$prim56068, align 8
%stackaddr$prim56069 = alloca %struct.ScmObj*, align 8
%current_45args54236 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54235)
store volatile %struct.ScmObj* %current_45args54236, %struct.ScmObj** %stackaddr$prim56069, align 8
%stackaddr$prim56070 = alloca %struct.ScmObj*, align 8
%y47113 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54236)
store volatile %struct.ScmObj* %y47113, %struct.ScmObj** %stackaddr$prim56070, align 8
%ae48156 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54238$k474990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56071 = alloca %struct.ScmObj*, align 8
%argslist54238$k474991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x47114, %struct.ScmObj* %argslist54238$k474990)
store volatile %struct.ScmObj* %argslist54238$k474991, %struct.ScmObj** %stackaddr$prim56071, align 8
%stackaddr$prim56072 = alloca %struct.ScmObj*, align 8
%argslist54238$k474992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48156, %struct.ScmObj* %argslist54238$k474991)
store volatile %struct.ScmObj* %argslist54238$k474992, %struct.ScmObj** %stackaddr$prim56072, align 8
%clofunc56073 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47499)
musttail call tailcc void %clofunc56073(%struct.ScmObj* %k47499, %struct.ScmObj* %argslist54238$k474992)
ret void
}

define tailcc void @proc_clo$ae48072(%struct.ScmObj* %env$ae48072,%struct.ScmObj* %current_45args54242) {
%stackaddr$prim56074 = alloca %struct.ScmObj*, align 8
%k47500 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54242)
store volatile %struct.ScmObj* %k47500, %struct.ScmObj** %stackaddr$prim56074, align 8
%stackaddr$prim56075 = alloca %struct.ScmObj*, align 8
%current_45args54243 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54242)
store volatile %struct.ScmObj* %current_45args54243, %struct.ScmObj** %stackaddr$prim56075, align 8
%stackaddr$prim56076 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54243)
store volatile %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$prim56076, align 8
%ae48074 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56077 = alloca %struct.ScmObj*, align 8
%fptrToInt56078 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48075 to i64
%ae48075 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56078)
store volatile %struct.ScmObj* %ae48075, %struct.ScmObj** %stackaddr$makeclosure56077, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48075, %struct.ScmObj* %_37foldl147074, i64 0)
%argslist54256$k475000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56079 = alloca %struct.ScmObj*, align 8
%argslist54256$k475001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48075, %struct.ScmObj* %argslist54256$k475000)
store volatile %struct.ScmObj* %argslist54256$k475001, %struct.ScmObj** %stackaddr$prim56079, align 8
%stackaddr$prim56080 = alloca %struct.ScmObj*, align 8
%argslist54256$k475002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48074, %struct.ScmObj* %argslist54256$k475001)
store volatile %struct.ScmObj* %argslist54256$k475002, %struct.ScmObj** %stackaddr$prim56080, align 8
%clofunc56081 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47500)
musttail call tailcc void %clofunc56081(%struct.ScmObj* %k47500, %struct.ScmObj* %argslist54256$k475002)
ret void
}

define tailcc void @proc_clo$ae48075(%struct.ScmObj* %env$ae48075,%struct.ScmObj* %current_45args54245) {
%stackaddr$env-ref56082 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48075, i64 0)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref56082
%stackaddr$prim56083 = alloca %struct.ScmObj*, align 8
%k47501 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54245)
store volatile %struct.ScmObj* %k47501, %struct.ScmObj** %stackaddr$prim56083, align 8
%stackaddr$prim56084 = alloca %struct.ScmObj*, align 8
%current_45args54246 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54245)
store volatile %struct.ScmObj* %current_45args54246, %struct.ScmObj** %stackaddr$prim56084, align 8
%stackaddr$prim56085 = alloca %struct.ScmObj*, align 8
%f47077 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54246)
store volatile %struct.ScmObj* %f47077, %struct.ScmObj** %stackaddr$prim56085, align 8
%stackaddr$prim56086 = alloca %struct.ScmObj*, align 8
%current_45args54247 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54246)
store volatile %struct.ScmObj* %current_45args54247, %struct.ScmObj** %stackaddr$prim56086, align 8
%stackaddr$prim56087 = alloca %struct.ScmObj*, align 8
%acc47076 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54247)
store volatile %struct.ScmObj* %acc47076, %struct.ScmObj** %stackaddr$prim56087, align 8
%stackaddr$prim56088 = alloca %struct.ScmObj*, align 8
%current_45args54248 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54247)
store volatile %struct.ScmObj* %current_45args54248, %struct.ScmObj** %stackaddr$prim56088, align 8
%stackaddr$prim56089 = alloca %struct.ScmObj*, align 8
%lst47075 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54248)
store volatile %struct.ScmObj* %lst47075, %struct.ScmObj** %stackaddr$prim56089, align 8
%stackaddr$prim56090 = alloca %struct.ScmObj*, align 8
%anf_45bind47223 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47075)
store volatile %struct.ScmObj* %anf_45bind47223, %struct.ScmObj** %stackaddr$prim56090, align 8
%truthy$cmp56091 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47223)
%cmp$cmp56091 = icmp eq i64 %truthy$cmp56091, 1
br i1 %cmp$cmp56091, label %truebranch$cmp56091, label %falsebranch$cmp56091
truebranch$cmp56091:
%ae48079 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54250$k475010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56092 = alloca %struct.ScmObj*, align 8
%argslist54250$k475011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47076, %struct.ScmObj* %argslist54250$k475010)
store volatile %struct.ScmObj* %argslist54250$k475011, %struct.ScmObj** %stackaddr$prim56092, align 8
%stackaddr$prim56093 = alloca %struct.ScmObj*, align 8
%argslist54250$k475012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48079, %struct.ScmObj* %argslist54250$k475011)
store volatile %struct.ScmObj* %argslist54250$k475012, %struct.ScmObj** %stackaddr$prim56093, align 8
%clofunc56094 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47501)
musttail call tailcc void %clofunc56094(%struct.ScmObj* %k47501, %struct.ScmObj* %argslist54250$k475012)
ret void
falsebranch$cmp56091:
%stackaddr$prim56095 = alloca %struct.ScmObj*, align 8
%anf_45bind47224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47075)
store volatile %struct.ScmObj* %anf_45bind47224, %struct.ScmObj** %stackaddr$prim56095, align 8
%stackaddr$makeclosure56096 = alloca %struct.ScmObj*, align 8
%fptrToInt56097 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48086 to i64
%ae48086 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56097)
store volatile %struct.ScmObj* %ae48086, %struct.ScmObj** %stackaddr$makeclosure56096, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48086, %struct.ScmObj* %f47077, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48086, %struct.ScmObj* %lst47075, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48086, %struct.ScmObj* %_37foldl147074, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48086, %struct.ScmObj* %k47501, i64 3)
%argslist54255$f470770 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56098 = alloca %struct.ScmObj*, align 8
%argslist54255$f470771 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47076, %struct.ScmObj* %argslist54255$f470770)
store volatile %struct.ScmObj* %argslist54255$f470771, %struct.ScmObj** %stackaddr$prim56098, align 8
%stackaddr$prim56099 = alloca %struct.ScmObj*, align 8
%argslist54255$f470772 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47224, %struct.ScmObj* %argslist54255$f470771)
store volatile %struct.ScmObj* %argslist54255$f470772, %struct.ScmObj** %stackaddr$prim56099, align 8
%stackaddr$prim56100 = alloca %struct.ScmObj*, align 8
%argslist54255$f470773 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48086, %struct.ScmObj* %argslist54255$f470772)
store volatile %struct.ScmObj* %argslist54255$f470773, %struct.ScmObj** %stackaddr$prim56100, align 8
%clofunc56101 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47077)
musttail call tailcc void %clofunc56101(%struct.ScmObj* %f47077, %struct.ScmObj* %argslist54255$f470773)
ret void
}

define tailcc void @proc_clo$ae48086(%struct.ScmObj* %env$ae48086,%struct.ScmObj* %current_45args54251) {
%stackaddr$env-ref56102 = alloca %struct.ScmObj*, align 8
%f47077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48086, i64 0)
store %struct.ScmObj* %f47077, %struct.ScmObj** %stackaddr$env-ref56102
%stackaddr$env-ref56103 = alloca %struct.ScmObj*, align 8
%lst47075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48086, i64 1)
store %struct.ScmObj* %lst47075, %struct.ScmObj** %stackaddr$env-ref56103
%stackaddr$env-ref56104 = alloca %struct.ScmObj*, align 8
%_37foldl147074 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48086, i64 2)
store %struct.ScmObj* %_37foldl147074, %struct.ScmObj** %stackaddr$env-ref56104
%stackaddr$env-ref56105 = alloca %struct.ScmObj*, align 8
%k47501 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48086, i64 3)
store %struct.ScmObj* %k47501, %struct.ScmObj** %stackaddr$env-ref56105
%stackaddr$prim56106 = alloca %struct.ScmObj*, align 8
%_95k47502 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54251)
store volatile %struct.ScmObj* %_95k47502, %struct.ScmObj** %stackaddr$prim56106, align 8
%stackaddr$prim56107 = alloca %struct.ScmObj*, align 8
%current_45args54252 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54251)
store volatile %struct.ScmObj* %current_45args54252, %struct.ScmObj** %stackaddr$prim56107, align 8
%stackaddr$prim56108 = alloca %struct.ScmObj*, align 8
%anf_45bind47225 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54252)
store volatile %struct.ScmObj* %anf_45bind47225, %struct.ScmObj** %stackaddr$prim56108, align 8
%stackaddr$prim56109 = alloca %struct.ScmObj*, align 8
%anf_45bind47226 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47075)
store volatile %struct.ScmObj* %anf_45bind47226, %struct.ScmObj** %stackaddr$prim56109, align 8
%argslist54254$_37foldl1470740 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56110 = alloca %struct.ScmObj*, align 8
%argslist54254$_37foldl1470741 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47226, %struct.ScmObj* %argslist54254$_37foldl1470740)
store volatile %struct.ScmObj* %argslist54254$_37foldl1470741, %struct.ScmObj** %stackaddr$prim56110, align 8
%stackaddr$prim56111 = alloca %struct.ScmObj*, align 8
%argslist54254$_37foldl1470742 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47225, %struct.ScmObj* %argslist54254$_37foldl1470741)
store volatile %struct.ScmObj* %argslist54254$_37foldl1470742, %struct.ScmObj** %stackaddr$prim56111, align 8
%stackaddr$prim56112 = alloca %struct.ScmObj*, align 8
%argslist54254$_37foldl1470743 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47077, %struct.ScmObj* %argslist54254$_37foldl1470742)
store volatile %struct.ScmObj* %argslist54254$_37foldl1470743, %struct.ScmObj** %stackaddr$prim56112, align 8
%stackaddr$prim56113 = alloca %struct.ScmObj*, align 8
%argslist54254$_37foldl1470744 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47501, %struct.ScmObj* %argslist54254$_37foldl1470743)
store volatile %struct.ScmObj* %argslist54254$_37foldl1470744, %struct.ScmObj** %stackaddr$prim56113, align 8
%clofunc56114 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl147074)
musttail call tailcc void %clofunc56114(%struct.ScmObj* %_37foldl147074, %struct.ScmObj* %argslist54254$_37foldl1470744)
ret void
}

define tailcc void @proc_clo$ae47989(%struct.ScmObj* %env$ae47989,%struct.ScmObj* %current_45args54259) {
%stackaddr$prim56115 = alloca %struct.ScmObj*, align 8
%k47503 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54259)
store volatile %struct.ScmObj* %k47503, %struct.ScmObj** %stackaddr$prim56115, align 8
%stackaddr$prim56116 = alloca %struct.ScmObj*, align 8
%current_45args54260 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54259)
store volatile %struct.ScmObj* %current_45args54260, %struct.ScmObj** %stackaddr$prim56116, align 8
%stackaddr$prim56117 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54260)
store volatile %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$prim56117, align 8
%ae47991 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56118 = alloca %struct.ScmObj*, align 8
%fptrToInt56119 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47992 to i64
%ae47992 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56119)
store volatile %struct.ScmObj* %ae47992, %struct.ScmObj** %stackaddr$makeclosure56118, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47992, %struct.ScmObj* %_37length47079, i64 0)
%argslist54271$k475030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56120 = alloca %struct.ScmObj*, align 8
%argslist54271$k475031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47992, %struct.ScmObj* %argslist54271$k475030)
store volatile %struct.ScmObj* %argslist54271$k475031, %struct.ScmObj** %stackaddr$prim56120, align 8
%stackaddr$prim56121 = alloca %struct.ScmObj*, align 8
%argslist54271$k475032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47991, %struct.ScmObj* %argslist54271$k475031)
store volatile %struct.ScmObj* %argslist54271$k475032, %struct.ScmObj** %stackaddr$prim56121, align 8
%clofunc56122 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47503)
musttail call tailcc void %clofunc56122(%struct.ScmObj* %k47503, %struct.ScmObj* %argslist54271$k475032)
ret void
}

define tailcc void @proc_clo$ae47992(%struct.ScmObj* %env$ae47992,%struct.ScmObj* %current_45args54262) {
%stackaddr$env-ref56123 = alloca %struct.ScmObj*, align 8
%_37length47079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47992, i64 0)
store %struct.ScmObj* %_37length47079, %struct.ScmObj** %stackaddr$env-ref56123
%stackaddr$prim56124 = alloca %struct.ScmObj*, align 8
%k47504 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54262)
store volatile %struct.ScmObj* %k47504, %struct.ScmObj** %stackaddr$prim56124, align 8
%stackaddr$prim56125 = alloca %struct.ScmObj*, align 8
%current_45args54263 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54262)
store volatile %struct.ScmObj* %current_45args54263, %struct.ScmObj** %stackaddr$prim56125, align 8
%stackaddr$prim56126 = alloca %struct.ScmObj*, align 8
%lst47080 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54263)
store volatile %struct.ScmObj* %lst47080, %struct.ScmObj** %stackaddr$prim56126, align 8
%stackaddr$prim56127 = alloca %struct.ScmObj*, align 8
%anf_45bind47219 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47080)
store volatile %struct.ScmObj* %anf_45bind47219, %struct.ScmObj** %stackaddr$prim56127, align 8
%truthy$cmp56128 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47219)
%cmp$cmp56128 = icmp eq i64 %truthy$cmp56128, 1
br i1 %cmp$cmp56128, label %truebranch$cmp56128, label %falsebranch$cmp56128
truebranch$cmp56128:
%ae47996 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47997 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54265$k475040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56129 = alloca %struct.ScmObj*, align 8
%argslist54265$k475041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47997, %struct.ScmObj* %argslist54265$k475040)
store volatile %struct.ScmObj* %argslist54265$k475041, %struct.ScmObj** %stackaddr$prim56129, align 8
%stackaddr$prim56130 = alloca %struct.ScmObj*, align 8
%argslist54265$k475042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47996, %struct.ScmObj* %argslist54265$k475041)
store volatile %struct.ScmObj* %argslist54265$k475042, %struct.ScmObj** %stackaddr$prim56130, align 8
%clofunc56131 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47504)
musttail call tailcc void %clofunc56131(%struct.ScmObj* %k47504, %struct.ScmObj* %argslist54265$k475042)
ret void
falsebranch$cmp56128:
%stackaddr$prim56132 = alloca %struct.ScmObj*, align 8
%anf_45bind47220 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47080)
store volatile %struct.ScmObj* %anf_45bind47220, %struct.ScmObj** %stackaddr$prim56132, align 8
%stackaddr$makeclosure56133 = alloca %struct.ScmObj*, align 8
%fptrToInt56134 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48006 to i64
%ae48006 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56134)
store volatile %struct.ScmObj* %ae48006, %struct.ScmObj** %stackaddr$makeclosure56133, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48006, %struct.ScmObj* %k47504, i64 0)
%argslist54270$_37length470790 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56135 = alloca %struct.ScmObj*, align 8
%argslist54270$_37length470791 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47220, %struct.ScmObj* %argslist54270$_37length470790)
store volatile %struct.ScmObj* %argslist54270$_37length470791, %struct.ScmObj** %stackaddr$prim56135, align 8
%stackaddr$prim56136 = alloca %struct.ScmObj*, align 8
%argslist54270$_37length470792 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48006, %struct.ScmObj* %argslist54270$_37length470791)
store volatile %struct.ScmObj* %argslist54270$_37length470792, %struct.ScmObj** %stackaddr$prim56136, align 8
%clofunc56137 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length47079)
musttail call tailcc void %clofunc56137(%struct.ScmObj* %_37length47079, %struct.ScmObj* %argslist54270$_37length470792)
ret void
}

define tailcc void @proc_clo$ae48006(%struct.ScmObj* %env$ae48006,%struct.ScmObj* %current_45args54266) {
%stackaddr$env-ref56138 = alloca %struct.ScmObj*, align 8
%k47504 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48006, i64 0)
store %struct.ScmObj* %k47504, %struct.ScmObj** %stackaddr$env-ref56138
%stackaddr$prim56139 = alloca %struct.ScmObj*, align 8
%_95k47505 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54266)
store volatile %struct.ScmObj* %_95k47505, %struct.ScmObj** %stackaddr$prim56139, align 8
%stackaddr$prim56140 = alloca %struct.ScmObj*, align 8
%current_45args54267 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54266)
store volatile %struct.ScmObj* %current_45args54267, %struct.ScmObj** %stackaddr$prim56140, align 8
%stackaddr$prim56141 = alloca %struct.ScmObj*, align 8
%anf_45bind47221 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54267)
store volatile %struct.ScmObj* %anf_45bind47221, %struct.ScmObj** %stackaddr$prim56141, align 8
%ae48008 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56142 = alloca %struct.ScmObj*, align 8
%cpsprim47506 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae48008, %struct.ScmObj* %anf_45bind47221)
store volatile %struct.ScmObj* %cpsprim47506, %struct.ScmObj** %stackaddr$prim56142, align 8
%ae48011 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54269$k475040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56143 = alloca %struct.ScmObj*, align 8
%argslist54269$k475041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47506, %struct.ScmObj* %argslist54269$k475040)
store volatile %struct.ScmObj* %argslist54269$k475041, %struct.ScmObj** %stackaddr$prim56143, align 8
%stackaddr$prim56144 = alloca %struct.ScmObj*, align 8
%argslist54269$k475042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48011, %struct.ScmObj* %argslist54269$k475041)
store volatile %struct.ScmObj* %argslist54269$k475042, %struct.ScmObj** %stackaddr$prim56144, align 8
%clofunc56145 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47504)
musttail call tailcc void %clofunc56145(%struct.ScmObj* %k47504, %struct.ScmObj* %argslist54269$k475042)
ret void
}

define tailcc void @proc_clo$ae47839(%struct.ScmObj* %env$ae47839,%struct.ScmObj* %current_45args54274) {
%stackaddr$prim56146 = alloca %struct.ScmObj*, align 8
%k47507 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54274)
store volatile %struct.ScmObj* %k47507, %struct.ScmObj** %stackaddr$prim56146, align 8
%stackaddr$prim56147 = alloca %struct.ScmObj*, align 8
%current_45args54275 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54274)
store volatile %struct.ScmObj* %current_45args54275, %struct.ScmObj** %stackaddr$prim56147, align 8
%stackaddr$prim56148 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54275)
store volatile %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$prim56148, align 8
%ae47841 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56149 = alloca %struct.ScmObj*, align 8
%fptrToInt56150 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47842 to i64
%ae47842 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56150)
store volatile %struct.ScmObj* %ae47842, %struct.ScmObj** %stackaddr$makeclosure56149, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47842, %struct.ScmObj* %_37take47082, i64 0)
%argslist54288$k475070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56151 = alloca %struct.ScmObj*, align 8
%argslist54288$k475071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47842, %struct.ScmObj* %argslist54288$k475070)
store volatile %struct.ScmObj* %argslist54288$k475071, %struct.ScmObj** %stackaddr$prim56151, align 8
%stackaddr$prim56152 = alloca %struct.ScmObj*, align 8
%argslist54288$k475072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47841, %struct.ScmObj* %argslist54288$k475071)
store volatile %struct.ScmObj* %argslist54288$k475072, %struct.ScmObj** %stackaddr$prim56152, align 8
%clofunc56153 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47507)
musttail call tailcc void %clofunc56153(%struct.ScmObj* %k47507, %struct.ScmObj* %argslist54288$k475072)
ret void
}

define tailcc void @proc_clo$ae47842(%struct.ScmObj* %env$ae47842,%struct.ScmObj* %current_45args54277) {
%stackaddr$env-ref56154 = alloca %struct.ScmObj*, align 8
%_37take47082 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47842, i64 0)
store %struct.ScmObj* %_37take47082, %struct.ScmObj** %stackaddr$env-ref56154
%stackaddr$prim56155 = alloca %struct.ScmObj*, align 8
%k47508 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54277)
store volatile %struct.ScmObj* %k47508, %struct.ScmObj** %stackaddr$prim56155, align 8
%stackaddr$prim56156 = alloca %struct.ScmObj*, align 8
%current_45args54278 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54277)
store volatile %struct.ScmObj* %current_45args54278, %struct.ScmObj** %stackaddr$prim56156, align 8
%stackaddr$prim56157 = alloca %struct.ScmObj*, align 8
%lst47084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54278)
store volatile %struct.ScmObj* %lst47084, %struct.ScmObj** %stackaddr$prim56157, align 8
%stackaddr$prim56158 = alloca %struct.ScmObj*, align 8
%current_45args54279 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54278)
store volatile %struct.ScmObj* %current_45args54279, %struct.ScmObj** %stackaddr$prim56158, align 8
%stackaddr$prim56159 = alloca %struct.ScmObj*, align 8
%n47083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54279)
store volatile %struct.ScmObj* %n47083, %struct.ScmObj** %stackaddr$prim56159, align 8
%ae47844 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim56160 = alloca %struct.ScmObj*, align 8
%anf_45bind47212 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n47083, %struct.ScmObj* %ae47844)
store volatile %struct.ScmObj* %anf_45bind47212, %struct.ScmObj** %stackaddr$prim56160, align 8
%truthy$cmp56161 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47212)
%cmp$cmp56161 = icmp eq i64 %truthy$cmp56161, 1
br i1 %cmp$cmp56161, label %truebranch$cmp56161, label %falsebranch$cmp56161
truebranch$cmp56161:
%ae47847 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47848 = call %struct.ScmObj* @const_init_null()
%argslist54281$k475080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56162 = alloca %struct.ScmObj*, align 8
%argslist54281$k475081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47848, %struct.ScmObj* %argslist54281$k475080)
store volatile %struct.ScmObj* %argslist54281$k475081, %struct.ScmObj** %stackaddr$prim56162, align 8
%stackaddr$prim56163 = alloca %struct.ScmObj*, align 8
%argslist54281$k475082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47847, %struct.ScmObj* %argslist54281$k475081)
store volatile %struct.ScmObj* %argslist54281$k475082, %struct.ScmObj** %stackaddr$prim56163, align 8
%clofunc56164 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47508)
musttail call tailcc void %clofunc56164(%struct.ScmObj* %k47508, %struct.ScmObj* %argslist54281$k475082)
ret void
falsebranch$cmp56161:
%stackaddr$prim56165 = alloca %struct.ScmObj*, align 8
%anf_45bind47213 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47084)
store volatile %struct.ScmObj* %anf_45bind47213, %struct.ScmObj** %stackaddr$prim56165, align 8
%truthy$cmp56166 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47213)
%cmp$cmp56166 = icmp eq i64 %truthy$cmp56166, 1
br i1 %cmp$cmp56166, label %truebranch$cmp56166, label %falsebranch$cmp56166
truebranch$cmp56166:
%ae47858 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47859 = call %struct.ScmObj* @const_init_null()
%argslist54282$k475080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56167 = alloca %struct.ScmObj*, align 8
%argslist54282$k475081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47859, %struct.ScmObj* %argslist54282$k475080)
store volatile %struct.ScmObj* %argslist54282$k475081, %struct.ScmObj** %stackaddr$prim56167, align 8
%stackaddr$prim56168 = alloca %struct.ScmObj*, align 8
%argslist54282$k475082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47858, %struct.ScmObj* %argslist54282$k475081)
store volatile %struct.ScmObj* %argslist54282$k475082, %struct.ScmObj** %stackaddr$prim56168, align 8
%clofunc56169 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47508)
musttail call tailcc void %clofunc56169(%struct.ScmObj* %k47508, %struct.ScmObj* %argslist54282$k475082)
ret void
falsebranch$cmp56166:
%stackaddr$prim56170 = alloca %struct.ScmObj*, align 8
%anf_45bind47214 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47084)
store volatile %struct.ScmObj* %anf_45bind47214, %struct.ScmObj** %stackaddr$prim56170, align 8
%stackaddr$prim56171 = alloca %struct.ScmObj*, align 8
%anf_45bind47215 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47084)
store volatile %struct.ScmObj* %anf_45bind47215, %struct.ScmObj** %stackaddr$prim56171, align 8
%ae47869 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim56172 = alloca %struct.ScmObj*, align 8
%anf_45bind47216 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n47083, %struct.ScmObj* %ae47869)
store volatile %struct.ScmObj* %anf_45bind47216, %struct.ScmObj** %stackaddr$prim56172, align 8
%stackaddr$makeclosure56173 = alloca %struct.ScmObj*, align 8
%fptrToInt56174 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47871 to i64
%ae47871 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56174)
store volatile %struct.ScmObj* %ae47871, %struct.ScmObj** %stackaddr$makeclosure56173, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47871, %struct.ScmObj* %k47508, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47871, %struct.ScmObj* %anf_45bind47214, i64 1)
%argslist54287$_37take470820 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56175 = alloca %struct.ScmObj*, align 8
%argslist54287$_37take470821 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47216, %struct.ScmObj* %argslist54287$_37take470820)
store volatile %struct.ScmObj* %argslist54287$_37take470821, %struct.ScmObj** %stackaddr$prim56175, align 8
%stackaddr$prim56176 = alloca %struct.ScmObj*, align 8
%argslist54287$_37take470822 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47215, %struct.ScmObj* %argslist54287$_37take470821)
store volatile %struct.ScmObj* %argslist54287$_37take470822, %struct.ScmObj** %stackaddr$prim56176, align 8
%stackaddr$prim56177 = alloca %struct.ScmObj*, align 8
%argslist54287$_37take470823 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47871, %struct.ScmObj* %argslist54287$_37take470822)
store volatile %struct.ScmObj* %argslist54287$_37take470823, %struct.ScmObj** %stackaddr$prim56177, align 8
%clofunc56178 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take47082)
musttail call tailcc void %clofunc56178(%struct.ScmObj* %_37take47082, %struct.ScmObj* %argslist54287$_37take470823)
ret void
}

define tailcc void @proc_clo$ae47871(%struct.ScmObj* %env$ae47871,%struct.ScmObj* %current_45args54283) {
%stackaddr$env-ref56179 = alloca %struct.ScmObj*, align 8
%k47508 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47871, i64 0)
store %struct.ScmObj* %k47508, %struct.ScmObj** %stackaddr$env-ref56179
%stackaddr$env-ref56180 = alloca %struct.ScmObj*, align 8
%anf_45bind47214 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47871, i64 1)
store %struct.ScmObj* %anf_45bind47214, %struct.ScmObj** %stackaddr$env-ref56180
%stackaddr$prim56181 = alloca %struct.ScmObj*, align 8
%_95k47509 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54283)
store volatile %struct.ScmObj* %_95k47509, %struct.ScmObj** %stackaddr$prim56181, align 8
%stackaddr$prim56182 = alloca %struct.ScmObj*, align 8
%current_45args54284 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54283)
store volatile %struct.ScmObj* %current_45args54284, %struct.ScmObj** %stackaddr$prim56182, align 8
%stackaddr$prim56183 = alloca %struct.ScmObj*, align 8
%anf_45bind47217 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54284)
store volatile %struct.ScmObj* %anf_45bind47217, %struct.ScmObj** %stackaddr$prim56183, align 8
%stackaddr$prim56184 = alloca %struct.ScmObj*, align 8
%cpsprim47510 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47214, %struct.ScmObj* %anf_45bind47217)
store volatile %struct.ScmObj* %cpsprim47510, %struct.ScmObj** %stackaddr$prim56184, align 8
%ae47877 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54286$k475080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56185 = alloca %struct.ScmObj*, align 8
%argslist54286$k475081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47510, %struct.ScmObj* %argslist54286$k475080)
store volatile %struct.ScmObj* %argslist54286$k475081, %struct.ScmObj** %stackaddr$prim56185, align 8
%stackaddr$prim56186 = alloca %struct.ScmObj*, align 8
%argslist54286$k475082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47877, %struct.ScmObj* %argslist54286$k475081)
store volatile %struct.ScmObj* %argslist54286$k475082, %struct.ScmObj** %stackaddr$prim56186, align 8
%clofunc56187 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47508)
musttail call tailcc void %clofunc56187(%struct.ScmObj* %k47508, %struct.ScmObj* %argslist54286$k475082)
ret void
}

define tailcc void @proc_clo$ae47742(%struct.ScmObj* %env$ae47742,%struct.ScmObj* %current_45args54291) {
%stackaddr$prim56188 = alloca %struct.ScmObj*, align 8
%k47511 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54291)
store volatile %struct.ScmObj* %k47511, %struct.ScmObj** %stackaddr$prim56188, align 8
%stackaddr$prim56189 = alloca %struct.ScmObj*, align 8
%current_45args54292 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54291)
store volatile %struct.ScmObj* %current_45args54292, %struct.ScmObj** %stackaddr$prim56189, align 8
%stackaddr$prim56190 = alloca %struct.ScmObj*, align 8
%_37map47086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54292)
store volatile %struct.ScmObj* %_37map47086, %struct.ScmObj** %stackaddr$prim56190, align 8
%ae47744 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56191 = alloca %struct.ScmObj*, align 8
%fptrToInt56192 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47745 to i64
%ae47745 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56192)
store volatile %struct.ScmObj* %ae47745, %struct.ScmObj** %stackaddr$makeclosure56191, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47745, %struct.ScmObj* %_37map47086, i64 0)
%argslist54308$k475110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56193 = alloca %struct.ScmObj*, align 8
%argslist54308$k475111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47745, %struct.ScmObj* %argslist54308$k475110)
store volatile %struct.ScmObj* %argslist54308$k475111, %struct.ScmObj** %stackaddr$prim56193, align 8
%stackaddr$prim56194 = alloca %struct.ScmObj*, align 8
%argslist54308$k475112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47744, %struct.ScmObj* %argslist54308$k475111)
store volatile %struct.ScmObj* %argslist54308$k475112, %struct.ScmObj** %stackaddr$prim56194, align 8
%clofunc56195 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47511)
musttail call tailcc void %clofunc56195(%struct.ScmObj* %k47511, %struct.ScmObj* %argslist54308$k475112)
ret void
}

define tailcc void @proc_clo$ae47745(%struct.ScmObj* %env$ae47745,%struct.ScmObj* %current_45args54294) {
%stackaddr$env-ref56196 = alloca %struct.ScmObj*, align 8
%_37map47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47745, i64 0)
store %struct.ScmObj* %_37map47086, %struct.ScmObj** %stackaddr$env-ref56196
%stackaddr$prim56197 = alloca %struct.ScmObj*, align 8
%k47512 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54294)
store volatile %struct.ScmObj* %k47512, %struct.ScmObj** %stackaddr$prim56197, align 8
%stackaddr$prim56198 = alloca %struct.ScmObj*, align 8
%current_45args54295 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54294)
store volatile %struct.ScmObj* %current_45args54295, %struct.ScmObj** %stackaddr$prim56198, align 8
%stackaddr$prim56199 = alloca %struct.ScmObj*, align 8
%f47088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54295)
store volatile %struct.ScmObj* %f47088, %struct.ScmObj** %stackaddr$prim56199, align 8
%stackaddr$prim56200 = alloca %struct.ScmObj*, align 8
%current_45args54296 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54295)
store volatile %struct.ScmObj* %current_45args54296, %struct.ScmObj** %stackaddr$prim56200, align 8
%stackaddr$prim56201 = alloca %struct.ScmObj*, align 8
%lst47087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54296)
store volatile %struct.ScmObj* %lst47087, %struct.ScmObj** %stackaddr$prim56201, align 8
%stackaddr$prim56202 = alloca %struct.ScmObj*, align 8
%anf_45bind47206 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47087)
store volatile %struct.ScmObj* %anf_45bind47206, %struct.ScmObj** %stackaddr$prim56202, align 8
%truthy$cmp56203 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47206)
%cmp$cmp56203 = icmp eq i64 %truthy$cmp56203, 1
br i1 %cmp$cmp56203, label %truebranch$cmp56203, label %falsebranch$cmp56203
truebranch$cmp56203:
%ae47749 = call %struct.ScmObj* @const_init_int(i64 0)
%ae47750 = call %struct.ScmObj* @const_init_null()
%argslist54298$k475120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56204 = alloca %struct.ScmObj*, align 8
%argslist54298$k475121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47750, %struct.ScmObj* %argslist54298$k475120)
store volatile %struct.ScmObj* %argslist54298$k475121, %struct.ScmObj** %stackaddr$prim56204, align 8
%stackaddr$prim56205 = alloca %struct.ScmObj*, align 8
%argslist54298$k475122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47749, %struct.ScmObj* %argslist54298$k475121)
store volatile %struct.ScmObj* %argslist54298$k475122, %struct.ScmObj** %stackaddr$prim56205, align 8
%clofunc56206 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47512)
musttail call tailcc void %clofunc56206(%struct.ScmObj* %k47512, %struct.ScmObj* %argslist54298$k475122)
ret void
falsebranch$cmp56203:
%stackaddr$prim56207 = alloca %struct.ScmObj*, align 8
%anf_45bind47207 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47087)
store volatile %struct.ScmObj* %anf_45bind47207, %struct.ScmObj** %stackaddr$prim56207, align 8
%stackaddr$makeclosure56208 = alloca %struct.ScmObj*, align 8
%fptrToInt56209 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47759 to i64
%ae47759 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt56209)
store volatile %struct.ScmObj* %ae47759, %struct.ScmObj** %stackaddr$makeclosure56208, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47759, %struct.ScmObj* %f47088, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47759, %struct.ScmObj* %lst47087, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47759, %struct.ScmObj* %_37map47086, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae47759, %struct.ScmObj* %k47512, i64 3)
%argslist54307$f470880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56210 = alloca %struct.ScmObj*, align 8
%argslist54307$f470881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47207, %struct.ScmObj* %argslist54307$f470880)
store volatile %struct.ScmObj* %argslist54307$f470881, %struct.ScmObj** %stackaddr$prim56210, align 8
%stackaddr$prim56211 = alloca %struct.ScmObj*, align 8
%argslist54307$f470882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47759, %struct.ScmObj* %argslist54307$f470881)
store volatile %struct.ScmObj* %argslist54307$f470882, %struct.ScmObj** %stackaddr$prim56211, align 8
%clofunc56212 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47088)
musttail call tailcc void %clofunc56212(%struct.ScmObj* %f47088, %struct.ScmObj* %argslist54307$f470882)
ret void
}

define tailcc void @proc_clo$ae47759(%struct.ScmObj* %env$ae47759,%struct.ScmObj* %current_45args54299) {
%stackaddr$env-ref56213 = alloca %struct.ScmObj*, align 8
%f47088 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47759, i64 0)
store %struct.ScmObj* %f47088, %struct.ScmObj** %stackaddr$env-ref56213
%stackaddr$env-ref56214 = alloca %struct.ScmObj*, align 8
%lst47087 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47759, i64 1)
store %struct.ScmObj* %lst47087, %struct.ScmObj** %stackaddr$env-ref56214
%stackaddr$env-ref56215 = alloca %struct.ScmObj*, align 8
%_37map47086 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47759, i64 2)
store %struct.ScmObj* %_37map47086, %struct.ScmObj** %stackaddr$env-ref56215
%stackaddr$env-ref56216 = alloca %struct.ScmObj*, align 8
%k47512 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47759, i64 3)
store %struct.ScmObj* %k47512, %struct.ScmObj** %stackaddr$env-ref56216
%stackaddr$prim56217 = alloca %struct.ScmObj*, align 8
%_95k47513 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54299)
store volatile %struct.ScmObj* %_95k47513, %struct.ScmObj** %stackaddr$prim56217, align 8
%stackaddr$prim56218 = alloca %struct.ScmObj*, align 8
%current_45args54300 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54299)
store volatile %struct.ScmObj* %current_45args54300, %struct.ScmObj** %stackaddr$prim56218, align 8
%stackaddr$prim56219 = alloca %struct.ScmObj*, align 8
%anf_45bind47208 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54300)
store volatile %struct.ScmObj* %anf_45bind47208, %struct.ScmObj** %stackaddr$prim56219, align 8
%stackaddr$prim56220 = alloca %struct.ScmObj*, align 8
%anf_45bind47209 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47087)
store volatile %struct.ScmObj* %anf_45bind47209, %struct.ScmObj** %stackaddr$prim56220, align 8
%stackaddr$makeclosure56221 = alloca %struct.ScmObj*, align 8
%fptrToInt56222 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47763 to i64
%ae47763 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56222)
store volatile %struct.ScmObj* %ae47763, %struct.ScmObj** %stackaddr$makeclosure56221, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47763, %struct.ScmObj* %anf_45bind47208, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47763, %struct.ScmObj* %k47512, i64 1)
%argslist54306$_37map470860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56223 = alloca %struct.ScmObj*, align 8
%argslist54306$_37map470861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47209, %struct.ScmObj* %argslist54306$_37map470860)
store volatile %struct.ScmObj* %argslist54306$_37map470861, %struct.ScmObj** %stackaddr$prim56223, align 8
%stackaddr$prim56224 = alloca %struct.ScmObj*, align 8
%argslist54306$_37map470862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47088, %struct.ScmObj* %argslist54306$_37map470861)
store volatile %struct.ScmObj* %argslist54306$_37map470862, %struct.ScmObj** %stackaddr$prim56224, align 8
%stackaddr$prim56225 = alloca %struct.ScmObj*, align 8
%argslist54306$_37map470863 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47763, %struct.ScmObj* %argslist54306$_37map470862)
store volatile %struct.ScmObj* %argslist54306$_37map470863, %struct.ScmObj** %stackaddr$prim56225, align 8
%clofunc56226 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map47086)
musttail call tailcc void %clofunc56226(%struct.ScmObj* %_37map47086, %struct.ScmObj* %argslist54306$_37map470863)
ret void
}

define tailcc void @proc_clo$ae47763(%struct.ScmObj* %env$ae47763,%struct.ScmObj* %current_45args54302) {
%stackaddr$env-ref56227 = alloca %struct.ScmObj*, align 8
%anf_45bind47208 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47763, i64 0)
store %struct.ScmObj* %anf_45bind47208, %struct.ScmObj** %stackaddr$env-ref56227
%stackaddr$env-ref56228 = alloca %struct.ScmObj*, align 8
%k47512 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47763, i64 1)
store %struct.ScmObj* %k47512, %struct.ScmObj** %stackaddr$env-ref56228
%stackaddr$prim56229 = alloca %struct.ScmObj*, align 8
%_95k47514 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54302)
store volatile %struct.ScmObj* %_95k47514, %struct.ScmObj** %stackaddr$prim56229, align 8
%stackaddr$prim56230 = alloca %struct.ScmObj*, align 8
%current_45args54303 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54302)
store volatile %struct.ScmObj* %current_45args54303, %struct.ScmObj** %stackaddr$prim56230, align 8
%stackaddr$prim56231 = alloca %struct.ScmObj*, align 8
%anf_45bind47210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54303)
store volatile %struct.ScmObj* %anf_45bind47210, %struct.ScmObj** %stackaddr$prim56231, align 8
%stackaddr$prim56232 = alloca %struct.ScmObj*, align 8
%cpsprim47515 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47208, %struct.ScmObj* %anf_45bind47210)
store volatile %struct.ScmObj* %cpsprim47515, %struct.ScmObj** %stackaddr$prim56232, align 8
%ae47769 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54305$k475120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56233 = alloca %struct.ScmObj*, align 8
%argslist54305$k475121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim47515, %struct.ScmObj* %argslist54305$k475120)
store volatile %struct.ScmObj* %argslist54305$k475121, %struct.ScmObj** %stackaddr$prim56233, align 8
%stackaddr$prim56234 = alloca %struct.ScmObj*, align 8
%argslist54305$k475122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47769, %struct.ScmObj* %argslist54305$k475121)
store volatile %struct.ScmObj* %argslist54305$k475122, %struct.ScmObj** %stackaddr$prim56234, align 8
%clofunc56235 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47512)
musttail call tailcc void %clofunc56235(%struct.ScmObj* %k47512, %struct.ScmObj* %argslist54305$k475122)
ret void
}

define tailcc void @proc_clo$ae47662(%struct.ScmObj* %env$ae47662,%struct.ScmObj* %current_45args54311) {
%stackaddr$prim56236 = alloca %struct.ScmObj*, align 8
%k47516 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54311)
store volatile %struct.ScmObj* %k47516, %struct.ScmObj** %stackaddr$prim56236, align 8
%stackaddr$prim56237 = alloca %struct.ScmObj*, align 8
%current_45args54312 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54311)
store volatile %struct.ScmObj* %current_45args54312, %struct.ScmObj** %stackaddr$prim56237, align 8
%stackaddr$prim56238 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54312)
store volatile %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$prim56238, align 8
%ae47664 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56239 = alloca %struct.ScmObj*, align 8
%fptrToInt56240 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47665 to i64
%ae47665 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56240)
store volatile %struct.ScmObj* %ae47665, %struct.ScmObj** %stackaddr$makeclosure56239, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47665, %struct.ScmObj* %_37foldr147090, i64 0)
%argslist54325$k475160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56241 = alloca %struct.ScmObj*, align 8
%argslist54325$k475161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47665, %struct.ScmObj* %argslist54325$k475160)
store volatile %struct.ScmObj* %argslist54325$k475161, %struct.ScmObj** %stackaddr$prim56241, align 8
%stackaddr$prim56242 = alloca %struct.ScmObj*, align 8
%argslist54325$k475162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47664, %struct.ScmObj* %argslist54325$k475161)
store volatile %struct.ScmObj* %argslist54325$k475162, %struct.ScmObj** %stackaddr$prim56242, align 8
%clofunc56243 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47516)
musttail call tailcc void %clofunc56243(%struct.ScmObj* %k47516, %struct.ScmObj* %argslist54325$k475162)
ret void
}

define tailcc void @proc_clo$ae47665(%struct.ScmObj* %env$ae47665,%struct.ScmObj* %current_45args54314) {
%stackaddr$env-ref56244 = alloca %struct.ScmObj*, align 8
%_37foldr147090 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47665, i64 0)
store %struct.ScmObj* %_37foldr147090, %struct.ScmObj** %stackaddr$env-ref56244
%stackaddr$prim56245 = alloca %struct.ScmObj*, align 8
%k47517 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54314)
store volatile %struct.ScmObj* %k47517, %struct.ScmObj** %stackaddr$prim56245, align 8
%stackaddr$prim56246 = alloca %struct.ScmObj*, align 8
%current_45args54315 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54314)
store volatile %struct.ScmObj* %current_45args54315, %struct.ScmObj** %stackaddr$prim56246, align 8
%stackaddr$prim56247 = alloca %struct.ScmObj*, align 8
%f47093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54315)
store volatile %struct.ScmObj* %f47093, %struct.ScmObj** %stackaddr$prim56247, align 8
%stackaddr$prim56248 = alloca %struct.ScmObj*, align 8
%current_45args54316 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54315)
store volatile %struct.ScmObj* %current_45args54316, %struct.ScmObj** %stackaddr$prim56248, align 8
%stackaddr$prim56249 = alloca %struct.ScmObj*, align 8
%acc47092 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54316)
store volatile %struct.ScmObj* %acc47092, %struct.ScmObj** %stackaddr$prim56249, align 8
%stackaddr$prim56250 = alloca %struct.ScmObj*, align 8
%current_45args54317 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54316)
store volatile %struct.ScmObj* %current_45args54317, %struct.ScmObj** %stackaddr$prim56250, align 8
%stackaddr$prim56251 = alloca %struct.ScmObj*, align 8
%lst47091 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54317)
store volatile %struct.ScmObj* %lst47091, %struct.ScmObj** %stackaddr$prim56251, align 8
%stackaddr$prim56252 = alloca %struct.ScmObj*, align 8
%anf_45bind47201 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst47091)
store volatile %struct.ScmObj* %anf_45bind47201, %struct.ScmObj** %stackaddr$prim56252, align 8
%truthy$cmp56253 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind47201)
%cmp$cmp56253 = icmp eq i64 %truthy$cmp56253, 1
br i1 %cmp$cmp56253, label %truebranch$cmp56253, label %falsebranch$cmp56253
truebranch$cmp56253:
%ae47669 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54319$k475170 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56254 = alloca %struct.ScmObj*, align 8
%argslist54319$k475171 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47092, %struct.ScmObj* %argslist54319$k475170)
store volatile %struct.ScmObj* %argslist54319$k475171, %struct.ScmObj** %stackaddr$prim56254, align 8
%stackaddr$prim56255 = alloca %struct.ScmObj*, align 8
%argslist54319$k475172 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47669, %struct.ScmObj* %argslist54319$k475171)
store volatile %struct.ScmObj* %argslist54319$k475172, %struct.ScmObj** %stackaddr$prim56255, align 8
%clofunc56256 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47517)
musttail call tailcc void %clofunc56256(%struct.ScmObj* %k47517, %struct.ScmObj* %argslist54319$k475172)
ret void
falsebranch$cmp56253:
%stackaddr$prim56257 = alloca %struct.ScmObj*, align 8
%anf_45bind47202 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst47091)
store volatile %struct.ScmObj* %anf_45bind47202, %struct.ScmObj** %stackaddr$prim56257, align 8
%stackaddr$prim56258 = alloca %struct.ScmObj*, align 8
%anf_45bind47203 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst47091)
store volatile %struct.ScmObj* %anf_45bind47203, %struct.ScmObj** %stackaddr$prim56258, align 8
%stackaddr$makeclosure56259 = alloca %struct.ScmObj*, align 8
%fptrToInt56260 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47677 to i64
%ae47677 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56260)
store volatile %struct.ScmObj* %ae47677, %struct.ScmObj** %stackaddr$makeclosure56259, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47677, %struct.ScmObj* %f47093, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47677, %struct.ScmObj* %anf_45bind47202, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47677, %struct.ScmObj* %k47517, i64 2)
%argslist54324$_37foldr1470900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56261 = alloca %struct.ScmObj*, align 8
%argslist54324$_37foldr1470901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47203, %struct.ScmObj* %argslist54324$_37foldr1470900)
store volatile %struct.ScmObj* %argslist54324$_37foldr1470901, %struct.ScmObj** %stackaddr$prim56261, align 8
%stackaddr$prim56262 = alloca %struct.ScmObj*, align 8
%argslist54324$_37foldr1470902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc47092, %struct.ScmObj* %argslist54324$_37foldr1470901)
store volatile %struct.ScmObj* %argslist54324$_37foldr1470902, %struct.ScmObj** %stackaddr$prim56262, align 8
%stackaddr$prim56263 = alloca %struct.ScmObj*, align 8
%argslist54324$_37foldr1470903 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47093, %struct.ScmObj* %argslist54324$_37foldr1470902)
store volatile %struct.ScmObj* %argslist54324$_37foldr1470903, %struct.ScmObj** %stackaddr$prim56263, align 8
%stackaddr$prim56264 = alloca %struct.ScmObj*, align 8
%argslist54324$_37foldr1470904 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47677, %struct.ScmObj* %argslist54324$_37foldr1470903)
store volatile %struct.ScmObj* %argslist54324$_37foldr1470904, %struct.ScmObj** %stackaddr$prim56264, align 8
%clofunc56265 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr147090)
musttail call tailcc void %clofunc56265(%struct.ScmObj* %_37foldr147090, %struct.ScmObj* %argslist54324$_37foldr1470904)
ret void
}

define tailcc void @proc_clo$ae47677(%struct.ScmObj* %env$ae47677,%struct.ScmObj* %current_45args54320) {
%stackaddr$env-ref56266 = alloca %struct.ScmObj*, align 8
%f47093 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47677, i64 0)
store %struct.ScmObj* %f47093, %struct.ScmObj** %stackaddr$env-ref56266
%stackaddr$env-ref56267 = alloca %struct.ScmObj*, align 8
%anf_45bind47202 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47677, i64 1)
store %struct.ScmObj* %anf_45bind47202, %struct.ScmObj** %stackaddr$env-ref56267
%stackaddr$env-ref56268 = alloca %struct.ScmObj*, align 8
%k47517 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47677, i64 2)
store %struct.ScmObj* %k47517, %struct.ScmObj** %stackaddr$env-ref56268
%stackaddr$prim56269 = alloca %struct.ScmObj*, align 8
%_95k47518 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54320)
store volatile %struct.ScmObj* %_95k47518, %struct.ScmObj** %stackaddr$prim56269, align 8
%stackaddr$prim56270 = alloca %struct.ScmObj*, align 8
%current_45args54321 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54320)
store volatile %struct.ScmObj* %current_45args54321, %struct.ScmObj** %stackaddr$prim56270, align 8
%stackaddr$prim56271 = alloca %struct.ScmObj*, align 8
%anf_45bind47204 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54321)
store volatile %struct.ScmObj* %anf_45bind47204, %struct.ScmObj** %stackaddr$prim56271, align 8
%argslist54323$f470930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56272 = alloca %struct.ScmObj*, align 8
%argslist54323$f470931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47204, %struct.ScmObj* %argslist54323$f470930)
store volatile %struct.ScmObj* %argslist54323$f470931, %struct.ScmObj** %stackaddr$prim56272, align 8
%stackaddr$prim56273 = alloca %struct.ScmObj*, align 8
%argslist54323$f470932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47202, %struct.ScmObj* %argslist54323$f470931)
store volatile %struct.ScmObj* %argslist54323$f470932, %struct.ScmObj** %stackaddr$prim56273, align 8
%stackaddr$prim56274 = alloca %struct.ScmObj*, align 8
%argslist54323$f470933 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47517, %struct.ScmObj* %argslist54323$f470932)
store volatile %struct.ScmObj* %argslist54323$f470933, %struct.ScmObj** %stackaddr$prim56274, align 8
%clofunc56275 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47093)
musttail call tailcc void %clofunc56275(%struct.ScmObj* %f47093, %struct.ScmObj* %argslist54323$f470933)
ret void
}

define tailcc void @proc_clo$ae47545(%struct.ScmObj* %env$ae47545,%struct.ScmObj* %current_45args54328) {
%stackaddr$prim56276 = alloca %struct.ScmObj*, align 8
%k47519 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54328)
store volatile %struct.ScmObj* %k47519, %struct.ScmObj** %stackaddr$prim56276, align 8
%stackaddr$prim56277 = alloca %struct.ScmObj*, align 8
%current_45args54329 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54328)
store volatile %struct.ScmObj* %current_45args54329, %struct.ScmObj** %stackaddr$prim56277, align 8
%stackaddr$prim56278 = alloca %struct.ScmObj*, align 8
%y47070 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54329)
store volatile %struct.ScmObj* %y47070, %struct.ScmObj** %stackaddr$prim56278, align 8
%ae47547 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56279 = alloca %struct.ScmObj*, align 8
%fptrToInt56280 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47548 to i64
%ae47548 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt56280)
store volatile %struct.ScmObj* %ae47548, %struct.ScmObj** %stackaddr$makeclosure56279, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47548, %struct.ScmObj* %y47070, i64 0)
%argslist54347$k475190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56281 = alloca %struct.ScmObj*, align 8
%argslist54347$k475191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47548, %struct.ScmObj* %argslist54347$k475190)
store volatile %struct.ScmObj* %argslist54347$k475191, %struct.ScmObj** %stackaddr$prim56281, align 8
%stackaddr$prim56282 = alloca %struct.ScmObj*, align 8
%argslist54347$k475192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47547, %struct.ScmObj* %argslist54347$k475191)
store volatile %struct.ScmObj* %argslist54347$k475192, %struct.ScmObj** %stackaddr$prim56282, align 8
%clofunc56283 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k47519)
musttail call tailcc void %clofunc56283(%struct.ScmObj* %k47519, %struct.ScmObj* %argslist54347$k475192)
ret void
}

define tailcc void @proc_clo$ae47548(%struct.ScmObj* %env$ae47548,%struct.ScmObj* %current_45args54331) {
%stackaddr$env-ref56284 = alloca %struct.ScmObj*, align 8
%y47070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47548, i64 0)
store %struct.ScmObj* %y47070, %struct.ScmObj** %stackaddr$env-ref56284
%stackaddr$prim56285 = alloca %struct.ScmObj*, align 8
%k47520 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54331)
store volatile %struct.ScmObj* %k47520, %struct.ScmObj** %stackaddr$prim56285, align 8
%stackaddr$prim56286 = alloca %struct.ScmObj*, align 8
%current_45args54332 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54331)
store volatile %struct.ScmObj* %current_45args54332, %struct.ScmObj** %stackaddr$prim56286, align 8
%stackaddr$prim56287 = alloca %struct.ScmObj*, align 8
%f47071 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54332)
store volatile %struct.ScmObj* %f47071, %struct.ScmObj** %stackaddr$prim56287, align 8
%stackaddr$makeclosure56288 = alloca %struct.ScmObj*, align 8
%fptrToInt56289 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47549 to i64
%ae47549 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56289)
store volatile %struct.ScmObj* %ae47549, %struct.ScmObj** %stackaddr$makeclosure56288, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47549, %struct.ScmObj* %k47520, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47549, %struct.ScmObj* %f47071, i64 1)
%ae47550 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure56290 = alloca %struct.ScmObj*, align 8
%fptrToInt56291 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47551 to i64
%ae47551 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56291)
store volatile %struct.ScmObj* %ae47551, %struct.ScmObj** %stackaddr$makeclosure56290, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47551, %struct.ScmObj* %f47071, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47551, %struct.ScmObj* %y47070, i64 1)
%argslist54346$ae475490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56292 = alloca %struct.ScmObj*, align 8
%argslist54346$ae475491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47551, %struct.ScmObj* %argslist54346$ae475490)
store volatile %struct.ScmObj* %argslist54346$ae475491, %struct.ScmObj** %stackaddr$prim56292, align 8
%stackaddr$prim56293 = alloca %struct.ScmObj*, align 8
%argslist54346$ae475492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47550, %struct.ScmObj* %argslist54346$ae475491)
store volatile %struct.ScmObj* %argslist54346$ae475492, %struct.ScmObj** %stackaddr$prim56293, align 8
%clofunc56294 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae47549)
musttail call tailcc void %clofunc56294(%struct.ScmObj* %ae47549, %struct.ScmObj* %argslist54346$ae475492)
ret void
}

define tailcc void @proc_clo$ae47549(%struct.ScmObj* %env$ae47549,%struct.ScmObj* %current_45args54334) {
%stackaddr$env-ref56295 = alloca %struct.ScmObj*, align 8
%k47520 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47549, i64 0)
store %struct.ScmObj* %k47520, %struct.ScmObj** %stackaddr$env-ref56295
%stackaddr$env-ref56296 = alloca %struct.ScmObj*, align 8
%f47071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47549, i64 1)
store %struct.ScmObj* %f47071, %struct.ScmObj** %stackaddr$env-ref56296
%stackaddr$prim56297 = alloca %struct.ScmObj*, align 8
%_95k47521 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54334)
store volatile %struct.ScmObj* %_95k47521, %struct.ScmObj** %stackaddr$prim56297, align 8
%stackaddr$prim56298 = alloca %struct.ScmObj*, align 8
%current_45args54335 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54334)
store volatile %struct.ScmObj* %current_45args54335, %struct.ScmObj** %stackaddr$prim56298, align 8
%stackaddr$prim56299 = alloca %struct.ScmObj*, align 8
%anf_45bind47199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54335)
store volatile %struct.ScmObj* %anf_45bind47199, %struct.ScmObj** %stackaddr$prim56299, align 8
%argslist54337$f470710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56300 = alloca %struct.ScmObj*, align 8
%argslist54337$f470711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind47199, %struct.ScmObj* %argslist54337$f470710)
store volatile %struct.ScmObj* %argslist54337$f470711, %struct.ScmObj** %stackaddr$prim56300, align 8
%stackaddr$prim56301 = alloca %struct.ScmObj*, align 8
%argslist54337$f470712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47520, %struct.ScmObj* %argslist54337$f470711)
store volatile %struct.ScmObj* %argslist54337$f470712, %struct.ScmObj** %stackaddr$prim56301, align 8
%clofunc56302 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f47071)
musttail call tailcc void %clofunc56302(%struct.ScmObj* %f47071, %struct.ScmObj* %argslist54337$f470712)
ret void
}

define tailcc void @proc_clo$ae47551(%struct.ScmObj* %env$ae47551,%struct.ScmObj* %args4707247522) {
%stackaddr$env-ref56303 = alloca %struct.ScmObj*, align 8
%f47071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47551, i64 0)
store %struct.ScmObj* %f47071, %struct.ScmObj** %stackaddr$env-ref56303
%stackaddr$env-ref56304 = alloca %struct.ScmObj*, align 8
%y47070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47551, i64 1)
store %struct.ScmObj* %y47070, %struct.ScmObj** %stackaddr$env-ref56304
%stackaddr$prim56305 = alloca %struct.ScmObj*, align 8
%k47523 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4707247522)
store volatile %struct.ScmObj* %k47523, %struct.ScmObj** %stackaddr$prim56305, align 8
%stackaddr$prim56306 = alloca %struct.ScmObj*, align 8
%args47072 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4707247522)
store volatile %struct.ScmObj* %args47072, %struct.ScmObj** %stackaddr$prim56306, align 8
%stackaddr$makeclosure56307 = alloca %struct.ScmObj*, align 8
%fptrToInt56308 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47555 to i64
%ae47555 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt56308)
store volatile %struct.ScmObj* %ae47555, %struct.ScmObj** %stackaddr$makeclosure56307, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47555, %struct.ScmObj* %k47523, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47555, %struct.ScmObj* %args47072, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae47555, %struct.ScmObj* %f47071, i64 2)
%argslist54345$y470700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56309 = alloca %struct.ScmObj*, align 8
%argslist54345$y470701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y47070, %struct.ScmObj* %argslist54345$y470700)
store volatile %struct.ScmObj* %argslist54345$y470701, %struct.ScmObj** %stackaddr$prim56309, align 8
%stackaddr$prim56310 = alloca %struct.ScmObj*, align 8
%argslist54345$y470702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47555, %struct.ScmObj* %argslist54345$y470701)
store volatile %struct.ScmObj* %argslist54345$y470702, %struct.ScmObj** %stackaddr$prim56310, align 8
%clofunc56311 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y47070)
musttail call tailcc void %clofunc56311(%struct.ScmObj* %y47070, %struct.ScmObj* %argslist54345$y470702)
ret void
}

define tailcc void @proc_clo$ae47555(%struct.ScmObj* %env$ae47555,%struct.ScmObj* %current_45args54338) {
%stackaddr$env-ref56312 = alloca %struct.ScmObj*, align 8
%k47523 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47555, i64 0)
store %struct.ScmObj* %k47523, %struct.ScmObj** %stackaddr$env-ref56312
%stackaddr$env-ref56313 = alloca %struct.ScmObj*, align 8
%args47072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47555, i64 1)
store %struct.ScmObj* %args47072, %struct.ScmObj** %stackaddr$env-ref56313
%stackaddr$env-ref56314 = alloca %struct.ScmObj*, align 8
%f47071 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47555, i64 2)
store %struct.ScmObj* %f47071, %struct.ScmObj** %stackaddr$env-ref56314
%stackaddr$prim56315 = alloca %struct.ScmObj*, align 8
%_95k47524 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54338)
store volatile %struct.ScmObj* %_95k47524, %struct.ScmObj** %stackaddr$prim56315, align 8
%stackaddr$prim56316 = alloca %struct.ScmObj*, align 8
%current_45args54339 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54338)
store volatile %struct.ScmObj* %current_45args54339, %struct.ScmObj** %stackaddr$prim56316, align 8
%stackaddr$prim56317 = alloca %struct.ScmObj*, align 8
%anf_45bind47197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54339)
store volatile %struct.ScmObj* %anf_45bind47197, %struct.ScmObj** %stackaddr$prim56317, align 8
%stackaddr$makeclosure56318 = alloca %struct.ScmObj*, align 8
%fptrToInt56319 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae47558 to i64
%ae47558 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt56319)
store volatile %struct.ScmObj* %ae47558, %struct.ScmObj** %stackaddr$makeclosure56318, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae47558, %struct.ScmObj* %k47523, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae47558, %struct.ScmObj* %args47072, i64 1)
%argslist54344$anf_45bind471970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56320 = alloca %struct.ScmObj*, align 8
%argslist54344$anf_45bind471971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f47071, %struct.ScmObj* %argslist54344$anf_45bind471970)
store volatile %struct.ScmObj* %argslist54344$anf_45bind471971, %struct.ScmObj** %stackaddr$prim56320, align 8
%stackaddr$prim56321 = alloca %struct.ScmObj*, align 8
%argslist54344$anf_45bind471972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae47558, %struct.ScmObj* %argslist54344$anf_45bind471971)
store volatile %struct.ScmObj* %argslist54344$anf_45bind471972, %struct.ScmObj** %stackaddr$prim56321, align 8
%clofunc56322 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47197)
musttail call tailcc void %clofunc56322(%struct.ScmObj* %anf_45bind47197, %struct.ScmObj* %argslist54344$anf_45bind471972)
ret void
}

define tailcc void @proc_clo$ae47558(%struct.ScmObj* %env$ae47558,%struct.ScmObj* %current_45args54341) {
%stackaddr$env-ref56323 = alloca %struct.ScmObj*, align 8
%k47523 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47558, i64 0)
store %struct.ScmObj* %k47523, %struct.ScmObj** %stackaddr$env-ref56323
%stackaddr$env-ref56324 = alloca %struct.ScmObj*, align 8
%args47072 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae47558, i64 1)
store %struct.ScmObj* %args47072, %struct.ScmObj** %stackaddr$env-ref56324
%stackaddr$prim56325 = alloca %struct.ScmObj*, align 8
%_95k47525 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54341)
store volatile %struct.ScmObj* %_95k47525, %struct.ScmObj** %stackaddr$prim56325, align 8
%stackaddr$prim56326 = alloca %struct.ScmObj*, align 8
%current_45args54342 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54341)
store volatile %struct.ScmObj* %current_45args54342, %struct.ScmObj** %stackaddr$prim56326, align 8
%stackaddr$prim56327 = alloca %struct.ScmObj*, align 8
%anf_45bind47198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54342)
store volatile %struct.ScmObj* %anf_45bind47198, %struct.ScmObj** %stackaddr$prim56327, align 8
%stackaddr$prim56328 = alloca %struct.ScmObj*, align 8
%cpsargs47526 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47523, %struct.ScmObj* %args47072)
store volatile %struct.ScmObj* %cpsargs47526, %struct.ScmObj** %stackaddr$prim56328, align 8
%clofunc56329 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind47198)
musttail call tailcc void %clofunc56329(%struct.ScmObj* %anf_45bind47198, %struct.ScmObj* %cpsargs47526)
ret void
}

define tailcc void @proc_clo$ae47530(%struct.ScmObj* %env$ae47530,%struct.ScmObj* %current_45args54349) {
%stackaddr$prim56330 = alloca %struct.ScmObj*, align 8
%k47527 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54349)
store volatile %struct.ScmObj* %k47527, %struct.ScmObj** %stackaddr$prim56330, align 8
%stackaddr$prim56331 = alloca %struct.ScmObj*, align 8
%current_45args54350 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54349)
store volatile %struct.ScmObj* %current_45args54350, %struct.ScmObj** %stackaddr$prim56331, align 8
%stackaddr$prim56332 = alloca %struct.ScmObj*, align 8
%yu47069 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54350)
store volatile %struct.ScmObj* %yu47069, %struct.ScmObj** %stackaddr$prim56332, align 8
%argslist54352$yu470690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56333 = alloca %struct.ScmObj*, align 8
%argslist54352$yu470691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu47069, %struct.ScmObj* %argslist54352$yu470690)
store volatile %struct.ScmObj* %argslist54352$yu470691, %struct.ScmObj** %stackaddr$prim56333, align 8
%stackaddr$prim56334 = alloca %struct.ScmObj*, align 8
%argslist54352$yu470692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k47527, %struct.ScmObj* %argslist54352$yu470691)
store volatile %struct.ScmObj* %argslist54352$yu470692, %struct.ScmObj** %stackaddr$prim56334, align 8
%clofunc56335 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu47069)
musttail call tailcc void %clofunc56335(%struct.ScmObj* %yu47069, %struct.ScmObj* %argslist54352$yu470692)
ret void
}