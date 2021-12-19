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



define ccc i32 @main() {
%mainenv54384 = call %struct.ScmObj* @const_init_null()
%mainargs54385 = call %struct.ScmObj* @const_init_null()
call tailcc void @proc_main(%struct.ScmObj* %mainenv54384, %struct.ScmObj* %mainargs54385)
ret i32 0
}

define tailcc void @proc_main(%struct.ScmObj* %mainenv54382,%struct.ScmObj* %mainargs54383) {
%stackaddr$makeclosure54386 = alloca %struct.ScmObj*, align 8
%fptrToInt54387 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48425 to i64
%ae48425 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54387)
store volatile %struct.ScmObj* %ae48425, %struct.ScmObj** %stackaddr$makeclosure54386, align 8
%ae48426 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54388 = alloca %struct.ScmObj*, align 8
%fptrToInt54389 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48427 to i64
%ae48427 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54389)
store volatile %struct.ScmObj* %ae48427, %struct.ScmObj** %stackaddr$makeclosure54388, align 8
%argslist54381$ae484250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54390 = alloca %struct.ScmObj*, align 8
%argslist54381$ae484251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48427, %struct.ScmObj* %argslist54381$ae484250)
store volatile %struct.ScmObj* %argslist54381$ae484251, %struct.ScmObj** %stackaddr$prim54390, align 8
%stackaddr$prim54391 = alloca %struct.ScmObj*, align 8
%argslist54381$ae484252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48426, %struct.ScmObj* %argslist54381$ae484251)
store volatile %struct.ScmObj* %argslist54381$ae484252, %struct.ScmObj** %stackaddr$prim54391, align 8
%clofunc54392 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48425)
musttail call tailcc void %clofunc54392(%struct.ScmObj* %ae48425, %struct.ScmObj* %argslist54381$ae484252)
ret void
}

define tailcc void @proc_clo$ae48425(%struct.ScmObj* %env$ae48425,%struct.ScmObj* %current_45args53846) {
%stackaddr$prim54393 = alloca %struct.ScmObj*, align 8
%_95k48260 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53846)
store volatile %struct.ScmObj* %_95k48260, %struct.ScmObj** %stackaddr$prim54393, align 8
%stackaddr$prim54394 = alloca %struct.ScmObj*, align 8
%current_45args53847 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53846)
store volatile %struct.ScmObj* %current_45args53847, %struct.ScmObj** %stackaddr$prim54394, align 8
%stackaddr$prim54395 = alloca %struct.ScmObj*, align 8
%anf_45bind48148 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53847)
store volatile %struct.ScmObj* %anf_45bind48148, %struct.ScmObj** %stackaddr$prim54395, align 8
%stackaddr$makeclosure54396 = alloca %struct.ScmObj*, align 8
%fptrToInt54397 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48440 to i64
%ae48440 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54397)
store volatile %struct.ScmObj* %ae48440, %struct.ScmObj** %stackaddr$makeclosure54396, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48440, %struct.ScmObj* %anf_45bind48148, i64 0)
%ae48441 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54398 = alloca %struct.ScmObj*, align 8
%fptrToInt54399 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48442 to i64
%ae48442 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54399)
store volatile %struct.ScmObj* %ae48442, %struct.ScmObj** %stackaddr$makeclosure54398, align 8
%argslist54376$ae484400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54400 = alloca %struct.ScmObj*, align 8
%argslist54376$ae484401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48442, %struct.ScmObj* %argslist54376$ae484400)
store volatile %struct.ScmObj* %argslist54376$ae484401, %struct.ScmObj** %stackaddr$prim54400, align 8
%stackaddr$prim54401 = alloca %struct.ScmObj*, align 8
%argslist54376$ae484402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48441, %struct.ScmObj* %argslist54376$ae484401)
store volatile %struct.ScmObj* %argslist54376$ae484402, %struct.ScmObj** %stackaddr$prim54401, align 8
%clofunc54402 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48440)
musttail call tailcc void %clofunc54402(%struct.ScmObj* %ae48440, %struct.ScmObj* %argslist54376$ae484402)
ret void
}

define tailcc void @proc_clo$ae48440(%struct.ScmObj* %env$ae48440,%struct.ScmObj* %current_45args53849) {
%stackaddr$env-ref54403 = alloca %struct.ScmObj*, align 8
%anf_45bind48148 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48440, i64 0)
store %struct.ScmObj* %anf_45bind48148, %struct.ScmObj** %stackaddr$env-ref54403
%stackaddr$prim54404 = alloca %struct.ScmObj*, align 8
%_95k48261 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53849)
store volatile %struct.ScmObj* %_95k48261, %struct.ScmObj** %stackaddr$prim54404, align 8
%stackaddr$prim54405 = alloca %struct.ScmObj*, align 8
%current_45args53850 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53849)
store volatile %struct.ScmObj* %current_45args53850, %struct.ScmObj** %stackaddr$prim54405, align 8
%stackaddr$prim54406 = alloca %struct.ScmObj*, align 8
%anf_45bind48152 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53850)
store volatile %struct.ScmObj* %anf_45bind48152, %struct.ScmObj** %stackaddr$prim54406, align 8
%stackaddr$makeclosure54407 = alloca %struct.ScmObj*, align 8
%fptrToInt54408 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48555 to i64
%ae48555 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54408)
store volatile %struct.ScmObj* %ae48555, %struct.ScmObj** %stackaddr$makeclosure54407, align 8
%argslist54355$anf_45bind481480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54409 = alloca %struct.ScmObj*, align 8
%argslist54355$anf_45bind481481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48152, %struct.ScmObj* %argslist54355$anf_45bind481480)
store volatile %struct.ScmObj* %argslist54355$anf_45bind481481, %struct.ScmObj** %stackaddr$prim54409, align 8
%stackaddr$prim54410 = alloca %struct.ScmObj*, align 8
%argslist54355$anf_45bind481482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48555, %struct.ScmObj* %argslist54355$anf_45bind481481)
store volatile %struct.ScmObj* %argslist54355$anf_45bind481482, %struct.ScmObj** %stackaddr$prim54410, align 8
%clofunc54411 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48148)
musttail call tailcc void %clofunc54411(%struct.ScmObj* %anf_45bind48148, %struct.ScmObj* %argslist54355$anf_45bind481482)
ret void
}

define tailcc void @proc_clo$ae48555(%struct.ScmObj* %env$ae48555,%struct.ScmObj* %current_45args53852) {
%stackaddr$prim54412 = alloca %struct.ScmObj*, align 8
%_95k48262 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53852)
store volatile %struct.ScmObj* %_95k48262, %struct.ScmObj** %stackaddr$prim54412, align 8
%stackaddr$prim54413 = alloca %struct.ScmObj*, align 8
%current_45args53853 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53852)
store volatile %struct.ScmObj* %current_45args53853, %struct.ScmObj** %stackaddr$prim54413, align 8
%stackaddr$prim54414 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53853)
store volatile %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$prim54414, align 8
%stackaddr$makeclosure54415 = alloca %struct.ScmObj*, align 8
%fptrToInt54416 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48557 to i64
%ae48557 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54416)
store volatile %struct.ScmObj* %ae48557, %struct.ScmObj** %stackaddr$makeclosure54415, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48557, %struct.ScmObj* %Ycmb48026, i64 0)
%ae48558 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54417 = alloca %struct.ScmObj*, align 8
%fptrToInt54418 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48559 to i64
%ae48559 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54418)
store volatile %struct.ScmObj* %ae48559, %struct.ScmObj** %stackaddr$makeclosure54417, align 8
%argslist54354$ae485570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54419 = alloca %struct.ScmObj*, align 8
%argslist54354$ae485571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48559, %struct.ScmObj* %argslist54354$ae485570)
store volatile %struct.ScmObj* %argslist54354$ae485571, %struct.ScmObj** %stackaddr$prim54419, align 8
%stackaddr$prim54420 = alloca %struct.ScmObj*, align 8
%argslist54354$ae485572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48558, %struct.ScmObj* %argslist54354$ae485571)
store volatile %struct.ScmObj* %argslist54354$ae485572, %struct.ScmObj** %stackaddr$prim54420, align 8
%clofunc54421 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48557)
musttail call tailcc void %clofunc54421(%struct.ScmObj* %ae48557, %struct.ScmObj* %argslist54354$ae485572)
ret void
}

define tailcc void @proc_clo$ae48557(%struct.ScmObj* %env$ae48557,%struct.ScmObj* %current_45args53855) {
%stackaddr$env-ref54422 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48557, i64 0)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54422
%stackaddr$prim54423 = alloca %struct.ScmObj*, align 8
%_95k48263 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53855)
store volatile %struct.ScmObj* %_95k48263, %struct.ScmObj** %stackaddr$prim54423, align 8
%stackaddr$prim54424 = alloca %struct.ScmObj*, align 8
%current_45args53856 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53855)
store volatile %struct.ScmObj* %current_45args53856, %struct.ScmObj** %stackaddr$prim54424, align 8
%stackaddr$prim54425 = alloca %struct.ScmObj*, align 8
%anf_45bind48157 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53856)
store volatile %struct.ScmObj* %anf_45bind48157, %struct.ScmObj** %stackaddr$prim54425, align 8
%stackaddr$makeclosure54426 = alloca %struct.ScmObj*, align 8
%fptrToInt54427 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48635 to i64
%ae48635 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54427)
store volatile %struct.ScmObj* %ae48635, %struct.ScmObj** %stackaddr$makeclosure54426, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48635, %struct.ScmObj* %Ycmb48026, i64 0)
%argslist54338$Ycmb480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54428 = alloca %struct.ScmObj*, align 8
%argslist54338$Ycmb480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48157, %struct.ScmObj* %argslist54338$Ycmb480260)
store volatile %struct.ScmObj* %argslist54338$Ycmb480261, %struct.ScmObj** %stackaddr$prim54428, align 8
%stackaddr$prim54429 = alloca %struct.ScmObj*, align 8
%argslist54338$Ycmb480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48635, %struct.ScmObj* %argslist54338$Ycmb480261)
store volatile %struct.ScmObj* %argslist54338$Ycmb480262, %struct.ScmObj** %stackaddr$prim54429, align 8
%clofunc54430 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48026)
musttail call tailcc void %clofunc54430(%struct.ScmObj* %Ycmb48026, %struct.ScmObj* %argslist54338$Ycmb480262)
ret void
}

define tailcc void @proc_clo$ae48635(%struct.ScmObj* %env$ae48635,%struct.ScmObj* %current_45args53858) {
%stackaddr$env-ref54431 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48635, i64 0)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54431
%stackaddr$prim54432 = alloca %struct.ScmObj*, align 8
%_95k48264 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53858)
store volatile %struct.ScmObj* %_95k48264, %struct.ScmObj** %stackaddr$prim54432, align 8
%stackaddr$prim54433 = alloca %struct.ScmObj*, align 8
%current_45args53859 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53858)
store volatile %struct.ScmObj* %current_45args53859, %struct.ScmObj** %stackaddr$prim54433, align 8
%stackaddr$prim54434 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53859)
store volatile %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$prim54434, align 8
%stackaddr$makeclosure54435 = alloca %struct.ScmObj*, align 8
%fptrToInt54436 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48637 to i64
%ae48637 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54436)
store volatile %struct.ScmObj* %ae48637, %struct.ScmObj** %stackaddr$makeclosure54435, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48637, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48637, %struct.ScmObj* %Ycmb48026, i64 1)
%ae48638 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54437 = alloca %struct.ScmObj*, align 8
%fptrToInt54438 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48639 to i64
%ae48639 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54438)
store volatile %struct.ScmObj* %ae48639, %struct.ScmObj** %stackaddr$makeclosure54437, align 8
%argslist54337$ae486370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54439 = alloca %struct.ScmObj*, align 8
%argslist54337$ae486371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48639, %struct.ScmObj* %argslist54337$ae486370)
store volatile %struct.ScmObj* %argslist54337$ae486371, %struct.ScmObj** %stackaddr$prim54439, align 8
%stackaddr$prim54440 = alloca %struct.ScmObj*, align 8
%argslist54337$ae486372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48638, %struct.ScmObj* %argslist54337$ae486371)
store volatile %struct.ScmObj* %argslist54337$ae486372, %struct.ScmObj** %stackaddr$prim54440, align 8
%clofunc54441 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48637)
musttail call tailcc void %clofunc54441(%struct.ScmObj* %ae48637, %struct.ScmObj* %argslist54337$ae486372)
ret void
}

define tailcc void @proc_clo$ae48637(%struct.ScmObj* %env$ae48637,%struct.ScmObj* %current_45args53861) {
%stackaddr$env-ref54442 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48637, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54442
%stackaddr$env-ref54443 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48637, i64 1)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54443
%stackaddr$prim54444 = alloca %struct.ScmObj*, align 8
%_95k48265 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53861)
store volatile %struct.ScmObj* %_95k48265, %struct.ScmObj** %stackaddr$prim54444, align 8
%stackaddr$prim54445 = alloca %struct.ScmObj*, align 8
%current_45args53862 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53861)
store volatile %struct.ScmObj* %current_45args53862, %struct.ScmObj** %stackaddr$prim54445, align 8
%stackaddr$prim54446 = alloca %struct.ScmObj*, align 8
%anf_45bind48163 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53862)
store volatile %struct.ScmObj* %anf_45bind48163, %struct.ScmObj** %stackaddr$prim54446, align 8
%stackaddr$makeclosure54447 = alloca %struct.ScmObj*, align 8
%fptrToInt54448 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48732 to i64
%ae48732 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54448)
store volatile %struct.ScmObj* %ae48732, %struct.ScmObj** %stackaddr$makeclosure54447, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48732, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48732, %struct.ScmObj* %Ycmb48026, i64 1)
%argslist54318$Ycmb480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54449 = alloca %struct.ScmObj*, align 8
%argslist54318$Ycmb480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48163, %struct.ScmObj* %argslist54318$Ycmb480260)
store volatile %struct.ScmObj* %argslist54318$Ycmb480261, %struct.ScmObj** %stackaddr$prim54449, align 8
%stackaddr$prim54450 = alloca %struct.ScmObj*, align 8
%argslist54318$Ycmb480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48732, %struct.ScmObj* %argslist54318$Ycmb480261)
store volatile %struct.ScmObj* %argslist54318$Ycmb480262, %struct.ScmObj** %stackaddr$prim54450, align 8
%clofunc54451 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48026)
musttail call tailcc void %clofunc54451(%struct.ScmObj* %Ycmb48026, %struct.ScmObj* %argslist54318$Ycmb480262)
ret void
}

define tailcc void @proc_clo$ae48732(%struct.ScmObj* %env$ae48732,%struct.ScmObj* %current_45args53864) {
%stackaddr$env-ref54452 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48732, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54452
%stackaddr$env-ref54453 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48732, i64 1)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54453
%stackaddr$prim54454 = alloca %struct.ScmObj*, align 8
%_95k48266 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53864)
store volatile %struct.ScmObj* %_95k48266, %struct.ScmObj** %stackaddr$prim54454, align 8
%stackaddr$prim54455 = alloca %struct.ScmObj*, align 8
%current_45args53865 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53864)
store volatile %struct.ScmObj* %current_45args53865, %struct.ScmObj** %stackaddr$prim54455, align 8
%stackaddr$prim54456 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53865)
store volatile %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$prim54456, align 8
%stackaddr$makeclosure54457 = alloca %struct.ScmObj*, align 8
%fptrToInt54458 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48734 to i64
%ae48734 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54458)
store volatile %struct.ScmObj* %ae48734, %struct.ScmObj** %stackaddr$makeclosure54457, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48734, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48734, %struct.ScmObj* %_37map148043, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48734, %struct.ScmObj* %Ycmb48026, i64 2)
%ae48735 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54459 = alloca %struct.ScmObj*, align 8
%fptrToInt54460 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48736 to i64
%ae48736 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54460)
store volatile %struct.ScmObj* %ae48736, %struct.ScmObj** %stackaddr$makeclosure54459, align 8
%argslist54317$ae487340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54461 = alloca %struct.ScmObj*, align 8
%argslist54317$ae487341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48736, %struct.ScmObj* %argslist54317$ae487340)
store volatile %struct.ScmObj* %argslist54317$ae487341, %struct.ScmObj** %stackaddr$prim54461, align 8
%stackaddr$prim54462 = alloca %struct.ScmObj*, align 8
%argslist54317$ae487342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48735, %struct.ScmObj* %argslist54317$ae487341)
store volatile %struct.ScmObj* %argslist54317$ae487342, %struct.ScmObj** %stackaddr$prim54462, align 8
%clofunc54463 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48734)
musttail call tailcc void %clofunc54463(%struct.ScmObj* %ae48734, %struct.ScmObj* %argslist54317$ae487342)
ret void
}

define tailcc void @proc_clo$ae48734(%struct.ScmObj* %env$ae48734,%struct.ScmObj* %current_45args53867) {
%stackaddr$env-ref54464 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48734, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54464
%stackaddr$env-ref54465 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48734, i64 1)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref54465
%stackaddr$env-ref54466 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48734, i64 2)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54466
%stackaddr$prim54467 = alloca %struct.ScmObj*, align 8
%_95k48267 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53867)
store volatile %struct.ScmObj* %_95k48267, %struct.ScmObj** %stackaddr$prim54467, align 8
%stackaddr$prim54468 = alloca %struct.ScmObj*, align 8
%current_45args53868 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53867)
store volatile %struct.ScmObj* %current_45args53868, %struct.ScmObj** %stackaddr$prim54468, align 8
%stackaddr$prim54469 = alloca %struct.ScmObj*, align 8
%anf_45bind48170 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53868)
store volatile %struct.ScmObj* %anf_45bind48170, %struct.ScmObj** %stackaddr$prim54469, align 8
%stackaddr$makeclosure54470 = alloca %struct.ScmObj*, align 8
%fptrToInt54471 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48882 to i64
%ae48882 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54471)
store volatile %struct.ScmObj* %ae48882, %struct.ScmObj** %stackaddr$makeclosure54470, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48882, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48882, %struct.ScmObj* %_37map148043, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48882, %struct.ScmObj* %Ycmb48026, i64 2)
%argslist54301$Ycmb480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54472 = alloca %struct.ScmObj*, align 8
%argslist54301$Ycmb480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48170, %struct.ScmObj* %argslist54301$Ycmb480260)
store volatile %struct.ScmObj* %argslist54301$Ycmb480261, %struct.ScmObj** %stackaddr$prim54472, align 8
%stackaddr$prim54473 = alloca %struct.ScmObj*, align 8
%argslist54301$Ycmb480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48882, %struct.ScmObj* %argslist54301$Ycmb480261)
store volatile %struct.ScmObj* %argslist54301$Ycmb480262, %struct.ScmObj** %stackaddr$prim54473, align 8
%clofunc54474 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48026)
musttail call tailcc void %clofunc54474(%struct.ScmObj* %Ycmb48026, %struct.ScmObj* %argslist54301$Ycmb480262)
ret void
}

define tailcc void @proc_clo$ae48882(%struct.ScmObj* %env$ae48882,%struct.ScmObj* %current_45args53870) {
%stackaddr$env-ref54475 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48882, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54475
%stackaddr$env-ref54476 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48882, i64 1)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref54476
%stackaddr$env-ref54477 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48882, i64 2)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54477
%stackaddr$prim54478 = alloca %struct.ScmObj*, align 8
%_95k48268 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53870)
store volatile %struct.ScmObj* %_95k48268, %struct.ScmObj** %stackaddr$prim54478, align 8
%stackaddr$prim54479 = alloca %struct.ScmObj*, align 8
%current_45args53871 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53870)
store volatile %struct.ScmObj* %current_45args53871, %struct.ScmObj** %stackaddr$prim54479, align 8
%stackaddr$prim54480 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53871)
store volatile %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$prim54480, align 8
%stackaddr$makeclosure54481 = alloca %struct.ScmObj*, align 8
%fptrToInt54482 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48884 to i64
%ae48884 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54482)
store volatile %struct.ScmObj* %ae48884, %struct.ScmObj** %stackaddr$makeclosure54481, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48884, %struct.ScmObj* %_37take48039, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48884, %struct.ScmObj* %_37foldr148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48884, %struct.ScmObj* %_37map148043, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48884, %struct.ScmObj* %Ycmb48026, i64 3)
%ae48885 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54483 = alloca %struct.ScmObj*, align 8
%fptrToInt54484 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48886 to i64
%ae48886 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54484)
store volatile %struct.ScmObj* %ae48886, %struct.ScmObj** %stackaddr$makeclosure54483, align 8
%argslist54300$ae488840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54485 = alloca %struct.ScmObj*, align 8
%argslist54300$ae488841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48886, %struct.ScmObj* %argslist54300$ae488840)
store volatile %struct.ScmObj* %argslist54300$ae488841, %struct.ScmObj** %stackaddr$prim54485, align 8
%stackaddr$prim54486 = alloca %struct.ScmObj*, align 8
%argslist54300$ae488842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48885, %struct.ScmObj* %argslist54300$ae488841)
store volatile %struct.ScmObj* %argslist54300$ae488842, %struct.ScmObj** %stackaddr$prim54486, align 8
%clofunc54487 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48884)
musttail call tailcc void %clofunc54487(%struct.ScmObj* %ae48884, %struct.ScmObj* %argslist54300$ae488842)
ret void
}

define tailcc void @proc_clo$ae48884(%struct.ScmObj* %env$ae48884,%struct.ScmObj* %current_45args53873) {
%stackaddr$env-ref54488 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48884, i64 0)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref54488
%stackaddr$env-ref54489 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48884, i64 1)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54489
%stackaddr$env-ref54490 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48884, i64 2)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref54490
%stackaddr$env-ref54491 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48884, i64 3)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54491
%stackaddr$prim54492 = alloca %struct.ScmObj*, align 8
%_95k48269 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53873)
store volatile %struct.ScmObj* %_95k48269, %struct.ScmObj** %stackaddr$prim54492, align 8
%stackaddr$prim54493 = alloca %struct.ScmObj*, align 8
%current_45args53874 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53873)
store volatile %struct.ScmObj* %current_45args53874, %struct.ScmObj** %stackaddr$prim54493, align 8
%stackaddr$prim54494 = alloca %struct.ScmObj*, align 8
%anf_45bind48174 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53874)
store volatile %struct.ScmObj* %anf_45bind48174, %struct.ScmObj** %stackaddr$prim54494, align 8
%stackaddr$makeclosure54495 = alloca %struct.ScmObj*, align 8
%fptrToInt54496 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48965 to i64
%ae48965 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt54496)
store volatile %struct.ScmObj* %ae48965, %struct.ScmObj** %stackaddr$makeclosure54495, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48965, %struct.ScmObj* %_37take48039, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48965, %struct.ScmObj* %_37foldr148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48965, %struct.ScmObj* %_37map148043, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48965, %struct.ScmObj* %Ycmb48026, i64 3)
%argslist54286$Ycmb480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54497 = alloca %struct.ScmObj*, align 8
%argslist54286$Ycmb480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48174, %struct.ScmObj* %argslist54286$Ycmb480260)
store volatile %struct.ScmObj* %argslist54286$Ycmb480261, %struct.ScmObj** %stackaddr$prim54497, align 8
%stackaddr$prim54498 = alloca %struct.ScmObj*, align 8
%argslist54286$Ycmb480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48965, %struct.ScmObj* %argslist54286$Ycmb480261)
store volatile %struct.ScmObj* %argslist54286$Ycmb480262, %struct.ScmObj** %stackaddr$prim54498, align 8
%clofunc54499 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48026)
musttail call tailcc void %clofunc54499(%struct.ScmObj* %Ycmb48026, %struct.ScmObj* %argslist54286$Ycmb480262)
ret void
}

define tailcc void @proc_clo$ae48965(%struct.ScmObj* %env$ae48965,%struct.ScmObj* %current_45args53876) {
%stackaddr$env-ref54500 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48965, i64 0)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref54500
%stackaddr$env-ref54501 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48965, i64 1)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54501
%stackaddr$env-ref54502 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48965, i64 2)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref54502
%stackaddr$env-ref54503 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48965, i64 3)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54503
%stackaddr$prim54504 = alloca %struct.ScmObj*, align 8
%_95k48270 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53876)
store volatile %struct.ScmObj* %_95k48270, %struct.ScmObj** %stackaddr$prim54504, align 8
%stackaddr$prim54505 = alloca %struct.ScmObj*, align 8
%current_45args53877 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53876)
store volatile %struct.ScmObj* %current_45args53877, %struct.ScmObj** %stackaddr$prim54505, align 8
%stackaddr$prim54506 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53877)
store volatile %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$prim54506, align 8
%stackaddr$makeclosure54507 = alloca %struct.ScmObj*, align 8
%fptrToInt54508 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48967 to i64
%ae48967 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54508)
store volatile %struct.ScmObj* %ae48967, %struct.ScmObj** %stackaddr$makeclosure54507, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48967, %struct.ScmObj* %_37take48039, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48967, %struct.ScmObj* %_37length48036, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48967, %struct.ScmObj* %_37foldr148047, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48967, %struct.ScmObj* %_37map148043, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae48967, %struct.ScmObj* %Ycmb48026, i64 4)
%ae48968 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54509 = alloca %struct.ScmObj*, align 8
%fptrToInt54510 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48969 to i64
%ae48969 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54510)
store volatile %struct.ScmObj* %ae48969, %struct.ScmObj** %stackaddr$makeclosure54509, align 8
%argslist54285$ae489670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54511 = alloca %struct.ScmObj*, align 8
%argslist54285$ae489671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48969, %struct.ScmObj* %argslist54285$ae489670)
store volatile %struct.ScmObj* %argslist54285$ae489671, %struct.ScmObj** %stackaddr$prim54511, align 8
%stackaddr$prim54512 = alloca %struct.ScmObj*, align 8
%argslist54285$ae489672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48968, %struct.ScmObj* %argslist54285$ae489671)
store volatile %struct.ScmObj* %argslist54285$ae489672, %struct.ScmObj** %stackaddr$prim54512, align 8
%clofunc54513 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48967)
musttail call tailcc void %clofunc54513(%struct.ScmObj* %ae48967, %struct.ScmObj* %argslist54285$ae489672)
ret void
}

define tailcc void @proc_clo$ae48967(%struct.ScmObj* %env$ae48967,%struct.ScmObj* %current_45args53879) {
%stackaddr$env-ref54514 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48967, i64 0)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref54514
%stackaddr$env-ref54515 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48967, i64 1)
store %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$env-ref54515
%stackaddr$env-ref54516 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48967, i64 2)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54516
%stackaddr$env-ref54517 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48967, i64 3)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref54517
%stackaddr$env-ref54518 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48967, i64 4)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54518
%stackaddr$prim54519 = alloca %struct.ScmObj*, align 8
%_95k48271 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53879)
store volatile %struct.ScmObj* %_95k48271, %struct.ScmObj** %stackaddr$prim54519, align 8
%stackaddr$prim54520 = alloca %struct.ScmObj*, align 8
%current_45args53880 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53879)
store volatile %struct.ScmObj* %current_45args53880, %struct.ScmObj** %stackaddr$prim54520, align 8
%stackaddr$prim54521 = alloca %struct.ScmObj*, align 8
%anf_45bind48179 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53880)
store volatile %struct.ScmObj* %anf_45bind48179, %struct.ScmObj** %stackaddr$prim54521, align 8
%stackaddr$makeclosure54522 = alloca %struct.ScmObj*, align 8
%fptrToInt54523 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49044 to i64
%ae49044 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54523)
store volatile %struct.ScmObj* %ae49044, %struct.ScmObj** %stackaddr$makeclosure54522, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49044, %struct.ScmObj* %_37take48039, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49044, %struct.ScmObj* %_37length48036, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49044, %struct.ScmObj* %_37foldr148047, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49044, %struct.ScmObj* %_37map148043, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49044, %struct.ScmObj* %Ycmb48026, i64 4)
%argslist54269$Ycmb480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54524 = alloca %struct.ScmObj*, align 8
%argslist54269$Ycmb480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48179, %struct.ScmObj* %argslist54269$Ycmb480260)
store volatile %struct.ScmObj* %argslist54269$Ycmb480261, %struct.ScmObj** %stackaddr$prim54524, align 8
%stackaddr$prim54525 = alloca %struct.ScmObj*, align 8
%argslist54269$Ycmb480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49044, %struct.ScmObj* %argslist54269$Ycmb480261)
store volatile %struct.ScmObj* %argslist54269$Ycmb480262, %struct.ScmObj** %stackaddr$prim54525, align 8
%clofunc54526 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48026)
musttail call tailcc void %clofunc54526(%struct.ScmObj* %Ycmb48026, %struct.ScmObj* %argslist54269$Ycmb480262)
ret void
}

define tailcc void @proc_clo$ae49044(%struct.ScmObj* %env$ae49044,%struct.ScmObj* %current_45args53882) {
%stackaddr$env-ref54527 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49044, i64 0)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref54527
%stackaddr$env-ref54528 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49044, i64 1)
store %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$env-ref54528
%stackaddr$env-ref54529 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49044, i64 2)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54529
%stackaddr$env-ref54530 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49044, i64 3)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref54530
%stackaddr$env-ref54531 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49044, i64 4)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54531
%stackaddr$prim54532 = alloca %struct.ScmObj*, align 8
%_95k48272 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53882)
store volatile %struct.ScmObj* %_95k48272, %struct.ScmObj** %stackaddr$prim54532, align 8
%stackaddr$prim54533 = alloca %struct.ScmObj*, align 8
%current_45args53883 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53882)
store volatile %struct.ScmObj* %current_45args53883, %struct.ScmObj** %stackaddr$prim54533, align 8
%stackaddr$prim54534 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53883)
store volatile %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$prim54534, align 8
%stackaddr$makeclosure54535 = alloca %struct.ScmObj*, align 8
%fptrToInt54536 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49046 to i64
%ae49046 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54536)
store volatile %struct.ScmObj* %ae49046, %struct.ScmObj** %stackaddr$makeclosure54535, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49046, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49046, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49046, %struct.ScmObj* %_37take48039, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49046, %struct.ScmObj* %_37length48036, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49046, %struct.ScmObj* %_37map148043, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49046, %struct.ScmObj* %Ycmb48026, i64 5)
%ae49047 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54537 = alloca %struct.ScmObj*, align 8
%fptrToInt54538 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49048 to i64
%ae49048 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54538)
store volatile %struct.ScmObj* %ae49048, %struct.ScmObj** %stackaddr$makeclosure54537, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49048, %struct.ScmObj* %_37foldl148031, i64 0)
%argslist54268$ae490460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54539 = alloca %struct.ScmObj*, align 8
%argslist54268$ae490461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49048, %struct.ScmObj* %argslist54268$ae490460)
store volatile %struct.ScmObj* %argslist54268$ae490461, %struct.ScmObj** %stackaddr$prim54539, align 8
%stackaddr$prim54540 = alloca %struct.ScmObj*, align 8
%argslist54268$ae490462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49047, %struct.ScmObj* %argslist54268$ae490461)
store volatile %struct.ScmObj* %argslist54268$ae490462, %struct.ScmObj** %stackaddr$prim54540, align 8
%clofunc54541 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49046)
musttail call tailcc void %clofunc54541(%struct.ScmObj* %ae49046, %struct.ScmObj* %argslist54268$ae490462)
ret void
}

define tailcc void @proc_clo$ae49046(%struct.ScmObj* %env$ae49046,%struct.ScmObj* %current_45args53885) {
%stackaddr$env-ref54542 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49046, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54542
%stackaddr$env-ref54543 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49046, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54543
%stackaddr$env-ref54544 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49046, i64 2)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref54544
%stackaddr$env-ref54545 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49046, i64 3)
store %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$env-ref54545
%stackaddr$env-ref54546 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49046, i64 4)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref54546
%stackaddr$env-ref54547 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49046, i64 5)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54547
%stackaddr$prim54548 = alloca %struct.ScmObj*, align 8
%_95k48273 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53885)
store volatile %struct.ScmObj* %_95k48273, %struct.ScmObj** %stackaddr$prim54548, align 8
%stackaddr$prim54549 = alloca %struct.ScmObj*, align 8
%current_45args53886 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53885)
store volatile %struct.ScmObj* %current_45args53886, %struct.ScmObj** %stackaddr$prim54549, align 8
%stackaddr$prim54550 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53886)
store volatile %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$prim54550, align 8
%stackaddr$makeclosure54551 = alloca %struct.ScmObj*, align 8
%fptrToInt54552 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49100 to i64
%ae49100 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54552)
store volatile %struct.ScmObj* %ae49100, %struct.ScmObj** %stackaddr$makeclosure54551, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49100, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49100, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49100, %struct.ScmObj* %_37last48069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49100, %struct.ScmObj* %_37map148043, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49100, %struct.ScmObj* %Ycmb48026, i64 4)
%ae49101 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54553 = alloca %struct.ScmObj*, align 8
%fptrToInt54554 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49102 to i64
%ae49102 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54554)
store volatile %struct.ScmObj* %ae49102, %struct.ScmObj** %stackaddr$makeclosure54553, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49102, %struct.ScmObj* %_37take48039, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49102, %struct.ScmObj* %_37length48036, i64 1)
%argslist54254$ae491000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54555 = alloca %struct.ScmObj*, align 8
%argslist54254$ae491001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49102, %struct.ScmObj* %argslist54254$ae491000)
store volatile %struct.ScmObj* %argslist54254$ae491001, %struct.ScmObj** %stackaddr$prim54555, align 8
%stackaddr$prim54556 = alloca %struct.ScmObj*, align 8
%argslist54254$ae491002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49101, %struct.ScmObj* %argslist54254$ae491001)
store volatile %struct.ScmObj* %argslist54254$ae491002, %struct.ScmObj** %stackaddr$prim54556, align 8
%clofunc54557 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49100)
musttail call tailcc void %clofunc54557(%struct.ScmObj* %ae49100, %struct.ScmObj* %argslist54254$ae491002)
ret void
}

define tailcc void @proc_clo$ae49100(%struct.ScmObj* %env$ae49100,%struct.ScmObj* %current_45args53888) {
%stackaddr$env-ref54558 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49100, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54558
%stackaddr$env-ref54559 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49100, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54559
%stackaddr$env-ref54560 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49100, i64 2)
store %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$env-ref54560
%stackaddr$env-ref54561 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49100, i64 3)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref54561
%stackaddr$env-ref54562 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49100, i64 4)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54562
%stackaddr$prim54563 = alloca %struct.ScmObj*, align 8
%_95k48274 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53888)
store volatile %struct.ScmObj* %_95k48274, %struct.ScmObj** %stackaddr$prim54563, align 8
%stackaddr$prim54564 = alloca %struct.ScmObj*, align 8
%current_45args53889 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53888)
store volatile %struct.ScmObj* %current_45args53889, %struct.ScmObj** %stackaddr$prim54564, align 8
%stackaddr$prim54565 = alloca %struct.ScmObj*, align 8
%_37drop_45right48066 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53889)
store volatile %struct.ScmObj* %_37drop_45right48066, %struct.ScmObj** %stackaddr$prim54565, align 8
%stackaddr$makeclosure54566 = alloca %struct.ScmObj*, align 8
%fptrToInt54567 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49130 to i64
%ae49130 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54567)
store volatile %struct.ScmObj* %ae49130, %struct.ScmObj** %stackaddr$makeclosure54566, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49130, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49130, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49130, %struct.ScmObj* %_37last48069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49130, %struct.ScmObj* %_37drop_45right48066, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49130, %struct.ScmObj* %Ycmb48026, i64 4)
%ae49131 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54568 = alloca %struct.ScmObj*, align 8
%fptrToInt54569 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49132 to i64
%ae49132 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54569)
store volatile %struct.ScmObj* %ae49132, %struct.ScmObj** %stackaddr$makeclosure54568, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49132, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49132, %struct.ScmObj* %_37map148043, i64 1)
%argslist54244$ae491300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54570 = alloca %struct.ScmObj*, align 8
%argslist54244$ae491301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49132, %struct.ScmObj* %argslist54244$ae491300)
store volatile %struct.ScmObj* %argslist54244$ae491301, %struct.ScmObj** %stackaddr$prim54570, align 8
%stackaddr$prim54571 = alloca %struct.ScmObj*, align 8
%argslist54244$ae491302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49131, %struct.ScmObj* %argslist54244$ae491301)
store volatile %struct.ScmObj* %argslist54244$ae491302, %struct.ScmObj** %stackaddr$prim54571, align 8
%clofunc54572 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49130)
musttail call tailcc void %clofunc54572(%struct.ScmObj* %ae49130, %struct.ScmObj* %argslist54244$ae491302)
ret void
}

define tailcc void @proc_clo$ae49130(%struct.ScmObj* %env$ae49130,%struct.ScmObj* %current_45args53891) {
%stackaddr$env-ref54573 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49130, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54573
%stackaddr$env-ref54574 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49130, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54574
%stackaddr$env-ref54575 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49130, i64 2)
store %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$env-ref54575
%stackaddr$env-ref54576 = alloca %struct.ScmObj*, align 8
%_37drop_45right48066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49130, i64 3)
store %struct.ScmObj* %_37drop_45right48066, %struct.ScmObj** %stackaddr$env-ref54576
%stackaddr$env-ref54577 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49130, i64 4)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54577
%stackaddr$prim54578 = alloca %struct.ScmObj*, align 8
%_95k48275 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53891)
store volatile %struct.ScmObj* %_95k48275, %struct.ScmObj** %stackaddr$prim54578, align 8
%stackaddr$prim54579 = alloca %struct.ScmObj*, align 8
%current_45args53892 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53891)
store volatile %struct.ScmObj* %current_45args53892, %struct.ScmObj** %stackaddr$prim54579, align 8
%stackaddr$prim54580 = alloca %struct.ScmObj*, align 8
%anf_45bind48195 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53892)
store volatile %struct.ScmObj* %anf_45bind48195, %struct.ScmObj** %stackaddr$prim54580, align 8
%stackaddr$makeclosure54581 = alloca %struct.ScmObj*, align 8
%fptrToInt54582 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49514 to i64
%ae49514 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54582)
store volatile %struct.ScmObj* %ae49514, %struct.ScmObj** %stackaddr$makeclosure54581, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49514, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49514, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49514, %struct.ScmObj* %_37last48069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49514, %struct.ScmObj* %_37drop_45right48066, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49514, %struct.ScmObj* %Ycmb48026, i64 4)
%argslist54184$Ycmb480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54583 = alloca %struct.ScmObj*, align 8
%argslist54184$Ycmb480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48195, %struct.ScmObj* %argslist54184$Ycmb480260)
store volatile %struct.ScmObj* %argslist54184$Ycmb480261, %struct.ScmObj** %stackaddr$prim54583, align 8
%stackaddr$prim54584 = alloca %struct.ScmObj*, align 8
%argslist54184$Ycmb480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49514, %struct.ScmObj* %argslist54184$Ycmb480261)
store volatile %struct.ScmObj* %argslist54184$Ycmb480262, %struct.ScmObj** %stackaddr$prim54584, align 8
%clofunc54585 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48026)
musttail call tailcc void %clofunc54585(%struct.ScmObj* %Ycmb48026, %struct.ScmObj* %argslist54184$Ycmb480262)
ret void
}

define tailcc void @proc_clo$ae49514(%struct.ScmObj* %env$ae49514,%struct.ScmObj* %current_45args53894) {
%stackaddr$env-ref54586 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49514, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54586
%stackaddr$env-ref54587 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49514, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54587
%stackaddr$env-ref54588 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49514, i64 2)
store %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$env-ref54588
%stackaddr$env-ref54589 = alloca %struct.ScmObj*, align 8
%_37drop_45right48066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49514, i64 3)
store %struct.ScmObj* %_37drop_45right48066, %struct.ScmObj** %stackaddr$env-ref54589
%stackaddr$env-ref54590 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49514, i64 4)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54590
%stackaddr$prim54591 = alloca %struct.ScmObj*, align 8
%_95k48276 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53894)
store volatile %struct.ScmObj* %_95k48276, %struct.ScmObj** %stackaddr$prim54591, align 8
%stackaddr$prim54592 = alloca %struct.ScmObj*, align 8
%current_45args53895 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53894)
store volatile %struct.ScmObj* %current_45args53895, %struct.ScmObj** %stackaddr$prim54592, align 8
%stackaddr$prim54593 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53895)
store volatile %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$prim54593, align 8
%stackaddr$makeclosure54594 = alloca %struct.ScmObj*, align 8
%fptrToInt54595 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49516 to i64
%ae49516 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt54595)
store volatile %struct.ScmObj* %ae49516, %struct.ScmObj** %stackaddr$makeclosure54594, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49516, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49516, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49516, %struct.ScmObj* %_37last48069, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49516, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49516, %struct.ScmObj* %_37drop_45right48066, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49516, %struct.ScmObj* %Ycmb48026, i64 5)
%ae49517 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54596 = alloca %struct.ScmObj*, align 8
%fptrToInt54597 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49518 to i64
%ae49518 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54597)
store volatile %struct.ScmObj* %ae49518, %struct.ScmObj** %stackaddr$makeclosure54596, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49518, %struct.ScmObj* %_37foldr148047, i64 0)
%argslist54183$ae495160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54598 = alloca %struct.ScmObj*, align 8
%argslist54183$ae495161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49518, %struct.ScmObj* %argslist54183$ae495160)
store volatile %struct.ScmObj* %argslist54183$ae495161, %struct.ScmObj** %stackaddr$prim54598, align 8
%stackaddr$prim54599 = alloca %struct.ScmObj*, align 8
%argslist54183$ae495162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49517, %struct.ScmObj* %argslist54183$ae495161)
store volatile %struct.ScmObj* %argslist54183$ae495162, %struct.ScmObj** %stackaddr$prim54599, align 8
%clofunc54600 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49516)
musttail call tailcc void %clofunc54600(%struct.ScmObj* %ae49516, %struct.ScmObj* %argslist54183$ae495162)
ret void
}

define tailcc void @proc_clo$ae49516(%struct.ScmObj* %env$ae49516,%struct.ScmObj* %current_45args53897) {
%stackaddr$env-ref54601 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49516, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54601
%stackaddr$env-ref54602 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49516, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54602
%stackaddr$env-ref54603 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49516, i64 2)
store %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$env-ref54603
%stackaddr$env-ref54604 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49516, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref54604
%stackaddr$env-ref54605 = alloca %struct.ScmObj*, align 8
%_37drop_45right48066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49516, i64 4)
store %struct.ScmObj* %_37drop_45right48066, %struct.ScmObj** %stackaddr$env-ref54605
%stackaddr$env-ref54606 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49516, i64 5)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54606
%stackaddr$prim54607 = alloca %struct.ScmObj*, align 8
%_95k48277 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53897)
store volatile %struct.ScmObj* %_95k48277, %struct.ScmObj** %stackaddr$prim54607, align 8
%stackaddr$prim54608 = alloca %struct.ScmObj*, align 8
%current_45args53898 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53897)
store volatile %struct.ScmObj* %current_45args53898, %struct.ScmObj** %stackaddr$prim54608, align 8
%stackaddr$prim54609 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53898)
store volatile %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$prim54609, align 8
%stackaddr$makeclosure54610 = alloca %struct.ScmObj*, align 8
%fptrToInt54611 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49593 to i64
%ae49593 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt54611)
store volatile %struct.ScmObj* %ae49593, %struct.ScmObj** %stackaddr$makeclosure54610, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49593, %struct.ScmObj* %_37foldr148047, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49593, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49593, %struct.ScmObj* %_37foldr48052, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49593, %struct.ScmObj* %_37map148078, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49593, %struct.ScmObj* %Ycmb48026, i64 4)
%ae49594 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54612 = alloca %struct.ScmObj*, align 8
%fptrToInt54613 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49595 to i64
%ae49595 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54613)
store volatile %struct.ScmObj* %ae49595, %struct.ScmObj** %stackaddr$makeclosure54612, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49595, %struct.ScmObj* %_37last48069, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49595, %struct.ScmObj* %_37foldr48052, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49595, %struct.ScmObj* %_37drop_45right48066, i64 2)
%argslist54164$ae495930 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54614 = alloca %struct.ScmObj*, align 8
%argslist54164$ae495931 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49595, %struct.ScmObj* %argslist54164$ae495930)
store volatile %struct.ScmObj* %argslist54164$ae495931, %struct.ScmObj** %stackaddr$prim54614, align 8
%stackaddr$prim54615 = alloca %struct.ScmObj*, align 8
%argslist54164$ae495932 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49594, %struct.ScmObj* %argslist54164$ae495931)
store volatile %struct.ScmObj* %argslist54164$ae495932, %struct.ScmObj** %stackaddr$prim54615, align 8
%clofunc54616 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49593)
musttail call tailcc void %clofunc54616(%struct.ScmObj* %ae49593, %struct.ScmObj* %argslist54164$ae495932)
ret void
}

define tailcc void @proc_clo$ae49593(%struct.ScmObj* %env$ae49593,%struct.ScmObj* %current_45args53900) {
%stackaddr$env-ref54617 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49593, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref54617
%stackaddr$env-ref54618 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49593, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54618
%stackaddr$env-ref54619 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49593, i64 2)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref54619
%stackaddr$env-ref54620 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49593, i64 3)
store %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$env-ref54620
%stackaddr$env-ref54621 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49593, i64 4)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54621
%stackaddr$prim54622 = alloca %struct.ScmObj*, align 8
%_95k48278 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53900)
store volatile %struct.ScmObj* %_95k48278, %struct.ScmObj** %stackaddr$prim54622, align 8
%stackaddr$prim54623 = alloca %struct.ScmObj*, align 8
%current_45args53901 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53900)
store volatile %struct.ScmObj* %current_45args53901, %struct.ScmObj** %stackaddr$prim54623, align 8
%stackaddr$prim54624 = alloca %struct.ScmObj*, align 8
%_37map48073 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53901)
store volatile %struct.ScmObj* %_37map48073, %struct.ScmObj** %stackaddr$prim54624, align 8
%stackaddr$makeclosure54625 = alloca %struct.ScmObj*, align 8
%fptrToInt54626 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49739 to i64
%ae49739 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54626)
store volatile %struct.ScmObj* %ae49739, %struct.ScmObj** %stackaddr$makeclosure54625, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49739, %struct.ScmObj* %_37foldl148031, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49739, %struct.ScmObj* %Ycmb48026, i64 1)
%ae49740 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54627 = alloca %struct.ScmObj*, align 8
%fptrToInt54628 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49741 to i64
%ae49741 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54628)
store volatile %struct.ScmObj* %ae49741, %struct.ScmObj** %stackaddr$makeclosure54627, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49741, %struct.ScmObj* %_37foldr48052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49741, %struct.ScmObj* %_37foldr148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49741, %struct.ScmObj* %_37map148078, i64 2)
%argslist54147$ae497390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54629 = alloca %struct.ScmObj*, align 8
%argslist54147$ae497391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49741, %struct.ScmObj* %argslist54147$ae497390)
store volatile %struct.ScmObj* %argslist54147$ae497391, %struct.ScmObj** %stackaddr$prim54629, align 8
%stackaddr$prim54630 = alloca %struct.ScmObj*, align 8
%argslist54147$ae497392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49740, %struct.ScmObj* %argslist54147$ae497391)
store volatile %struct.ScmObj* %argslist54147$ae497392, %struct.ScmObj** %stackaddr$prim54630, align 8
%clofunc54631 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49739)
musttail call tailcc void %clofunc54631(%struct.ScmObj* %ae49739, %struct.ScmObj* %argslist54147$ae497392)
ret void
}

define tailcc void @proc_clo$ae49739(%struct.ScmObj* %env$ae49739,%struct.ScmObj* %current_45args53903) {
%stackaddr$env-ref54632 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49739, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54632
%stackaddr$env-ref54633 = alloca %struct.ScmObj*, align 8
%Ycmb48026 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49739, i64 1)
store %struct.ScmObj* %Ycmb48026, %struct.ScmObj** %stackaddr$env-ref54633
%stackaddr$prim54634 = alloca %struct.ScmObj*, align 8
%_95k48279 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53903)
store volatile %struct.ScmObj* %_95k48279, %struct.ScmObj** %stackaddr$prim54634, align 8
%stackaddr$prim54635 = alloca %struct.ScmObj*, align 8
%current_45args53904 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53903)
store volatile %struct.ScmObj* %current_45args53904, %struct.ScmObj** %stackaddr$prim54635, align 8
%stackaddr$prim54636 = alloca %struct.ScmObj*, align 8
%anf_45bind48215 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53904)
store volatile %struct.ScmObj* %anf_45bind48215, %struct.ScmObj** %stackaddr$prim54636, align 8
%stackaddr$makeclosure54637 = alloca %struct.ScmObj*, align 8
%fptrToInt54638 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50131 to i64
%ae50131 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54638)
store volatile %struct.ScmObj* %ae50131, %struct.ScmObj** %stackaddr$makeclosure54637, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50131, %struct.ScmObj* %_37foldl148031, i64 0)
%argslist54087$Ycmb480260 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54639 = alloca %struct.ScmObj*, align 8
%argslist54087$Ycmb480261 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48215, %struct.ScmObj* %argslist54087$Ycmb480260)
store volatile %struct.ScmObj* %argslist54087$Ycmb480261, %struct.ScmObj** %stackaddr$prim54639, align 8
%stackaddr$prim54640 = alloca %struct.ScmObj*, align 8
%argslist54087$Ycmb480262 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50131, %struct.ScmObj* %argslist54087$Ycmb480261)
store volatile %struct.ScmObj* %argslist54087$Ycmb480262, %struct.ScmObj** %stackaddr$prim54640, align 8
%clofunc54641 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %Ycmb48026)
musttail call tailcc void %clofunc54641(%struct.ScmObj* %Ycmb48026, %struct.ScmObj* %argslist54087$Ycmb480262)
ret void
}

define tailcc void @proc_clo$ae50131(%struct.ScmObj* %env$ae50131,%struct.ScmObj* %current_45args53906) {
%stackaddr$env-ref54642 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50131, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54642
%stackaddr$prim54643 = alloca %struct.ScmObj*, align 8
%_95k48280 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53906)
store volatile %struct.ScmObj* %_95k48280, %struct.ScmObj** %stackaddr$prim54643, align 8
%stackaddr$prim54644 = alloca %struct.ScmObj*, align 8
%current_45args53907 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53906)
store volatile %struct.ScmObj* %current_45args53907, %struct.ScmObj** %stackaddr$prim54644, align 8
%stackaddr$prim54645 = alloca %struct.ScmObj*, align 8
%_37foldl48129 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53907)
store volatile %struct.ScmObj* %_37foldl48129, %struct.ScmObj** %stackaddr$prim54645, align 8
%stackaddr$makeclosure54646 = alloca %struct.ScmObj*, align 8
%fptrToInt54647 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50133 to i64
%ae50133 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54647)
store volatile %struct.ScmObj* %ae50133, %struct.ScmObj** %stackaddr$makeclosure54646, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50133, %struct.ScmObj* %_37foldl148031, i64 0)
%ae50134 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54648 = alloca %struct.ScmObj*, align 8
%fptrToInt54649 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50135 to i64
%ae50135 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54649)
store volatile %struct.ScmObj* %ae50135, %struct.ScmObj** %stackaddr$makeclosure54648, align 8
%argslist54086$ae501330 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54650 = alloca %struct.ScmObj*, align 8
%argslist54086$ae501331 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50135, %struct.ScmObj* %argslist54086$ae501330)
store volatile %struct.ScmObj* %argslist54086$ae501331, %struct.ScmObj** %stackaddr$prim54650, align 8
%stackaddr$prim54651 = alloca %struct.ScmObj*, align 8
%argslist54086$ae501332 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50134, %struct.ScmObj* %argslist54086$ae501331)
store volatile %struct.ScmObj* %argslist54086$ae501332, %struct.ScmObj** %stackaddr$prim54651, align 8
%clofunc54652 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50133)
musttail call tailcc void %clofunc54652(%struct.ScmObj* %ae50133, %struct.ScmObj* %argslist54086$ae501332)
ret void
}

define tailcc void @proc_clo$ae50133(%struct.ScmObj* %env$ae50133,%struct.ScmObj* %current_45args53909) {
%stackaddr$env-ref54653 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50133, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54653
%stackaddr$prim54654 = alloca %struct.ScmObj*, align 8
%_95k48281 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53909)
store volatile %struct.ScmObj* %_95k48281, %struct.ScmObj** %stackaddr$prim54654, align 8
%stackaddr$prim54655 = alloca %struct.ScmObj*, align 8
%current_45args53910 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53909)
store volatile %struct.ScmObj* %current_45args53910, %struct.ScmObj** %stackaddr$prim54655, align 8
%stackaddr$prim54656 = alloca %struct.ScmObj*, align 8
%_37_6248126 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53910)
store volatile %struct.ScmObj* %_37_6248126, %struct.ScmObj** %stackaddr$prim54656, align 8
%stackaddr$makeclosure54657 = alloca %struct.ScmObj*, align 8
%fptrToInt54658 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50157 to i64
%ae50157 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54658)
store volatile %struct.ScmObj* %ae50157, %struct.ScmObj** %stackaddr$makeclosure54657, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50157, %struct.ScmObj* %_37foldl148031, i64 0)
%ae50158 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54659 = alloca %struct.ScmObj*, align 8
%fptrToInt54660 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50159 to i64
%ae50159 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54660)
store volatile %struct.ScmObj* %ae50159, %struct.ScmObj** %stackaddr$makeclosure54659, align 8
%argslist54080$ae501570 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54661 = alloca %struct.ScmObj*, align 8
%argslist54080$ae501571 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50159, %struct.ScmObj* %argslist54080$ae501570)
store volatile %struct.ScmObj* %argslist54080$ae501571, %struct.ScmObj** %stackaddr$prim54661, align 8
%stackaddr$prim54662 = alloca %struct.ScmObj*, align 8
%argslist54080$ae501572 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50158, %struct.ScmObj* %argslist54080$ae501571)
store volatile %struct.ScmObj* %argslist54080$ae501572, %struct.ScmObj** %stackaddr$prim54662, align 8
%clofunc54663 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50157)
musttail call tailcc void %clofunc54663(%struct.ScmObj* %ae50157, %struct.ScmObj* %argslist54080$ae501572)
ret void
}

define tailcc void @proc_clo$ae50157(%struct.ScmObj* %env$ae50157,%struct.ScmObj* %current_45args53912) {
%stackaddr$env-ref54664 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50157, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54664
%stackaddr$prim54665 = alloca %struct.ScmObj*, align 8
%_95k48282 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53912)
store volatile %struct.ScmObj* %_95k48282, %struct.ScmObj** %stackaddr$prim54665, align 8
%stackaddr$prim54666 = alloca %struct.ScmObj*, align 8
%current_45args53913 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53912)
store volatile %struct.ScmObj* %current_45args53913, %struct.ScmObj** %stackaddr$prim54666, align 8
%stackaddr$prim54667 = alloca %struct.ScmObj*, align 8
%_37_62_6148123 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53913)
store volatile %struct.ScmObj* %_37_62_6148123, %struct.ScmObj** %stackaddr$prim54667, align 8
%ae50181 = call %struct.ScmObj* @const_init_int(i64 1)
%ae50182 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54668 = alloca %struct.ScmObj*, align 8
%_37append48119 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50181, %struct.ScmObj* %ae50182)
store volatile %struct.ScmObj* %_37append48119, %struct.ScmObj** %stackaddr$prim54668, align 8
%stackaddr$makeclosure54669 = alloca %struct.ScmObj*, align 8
%fptrToInt54670 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50183 to i64
%ae50183 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt54670)
store volatile %struct.ScmObj* %ae50183, %struct.ScmObj** %stackaddr$makeclosure54669, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50183, %struct.ScmObj* %_37append48119, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50183, %struct.ScmObj* %_37foldl148031, i64 1)
%ae50184 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54671 = alloca %struct.ScmObj*, align 8
%fptrToInt54672 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50185 to i64
%ae50185 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54672)
store volatile %struct.ScmObj* %ae50185, %struct.ScmObj** %stackaddr$makeclosure54671, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50185, %struct.ScmObj* %_37append48119, i64 0)
%argslist54074$ae501830 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54673 = alloca %struct.ScmObj*, align 8
%argslist54074$ae501831 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50185, %struct.ScmObj* %argslist54074$ae501830)
store volatile %struct.ScmObj* %argslist54074$ae501831, %struct.ScmObj** %stackaddr$prim54673, align 8
%stackaddr$prim54674 = alloca %struct.ScmObj*, align 8
%argslist54074$ae501832 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50184, %struct.ScmObj* %argslist54074$ae501831)
store volatile %struct.ScmObj* %argslist54074$ae501832, %struct.ScmObj** %stackaddr$prim54674, align 8
%clofunc54675 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50183)
musttail call tailcc void %clofunc54675(%struct.ScmObj* %ae50183, %struct.ScmObj* %argslist54074$ae501832)
ret void
}

define tailcc void @proc_clo$ae50183(%struct.ScmObj* %env$ae50183,%struct.ScmObj* %current_45args53915) {
%stackaddr$env-ref54676 = alloca %struct.ScmObj*, align 8
%_37append48119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50183, i64 0)
store %struct.ScmObj* %_37append48119, %struct.ScmObj** %stackaddr$env-ref54676
%stackaddr$env-ref54677 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50183, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54677
%stackaddr$prim54678 = alloca %struct.ScmObj*, align 8
%_95k48283 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53915)
store volatile %struct.ScmObj* %_95k48283, %struct.ScmObj** %stackaddr$prim54678, align 8
%stackaddr$prim54679 = alloca %struct.ScmObj*, align 8
%current_45args53916 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53915)
store volatile %struct.ScmObj* %current_45args53916, %struct.ScmObj** %stackaddr$prim54679, align 8
%stackaddr$prim54680 = alloca %struct.ScmObj*, align 8
%anf_45bind48223 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53916)
store volatile %struct.ScmObj* %anf_45bind48223, %struct.ScmObj** %stackaddr$prim54680, align 8
%ae50251 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54681 = alloca %struct.ScmObj*, align 8
%_95048120 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %_37append48119, %struct.ScmObj* %ae50251, %struct.ScmObj* %anf_45bind48223)
store volatile %struct.ScmObj* %_95048120, %struct.ScmObj** %stackaddr$prim54681, align 8
%ae50254 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54682 = alloca %struct.ScmObj*, align 8
%_37append48118 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48119, %struct.ScmObj* %ae50254)
store volatile %struct.ScmObj* %_37append48118, %struct.ScmObj** %stackaddr$prim54682, align 8
%stackaddr$makeclosure54683 = alloca %struct.ScmObj*, align 8
%fptrToInt54684 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50255 to i64
%ae50255 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54684)
store volatile %struct.ScmObj* %ae50255, %struct.ScmObj** %stackaddr$makeclosure54683, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50255, %struct.ScmObj* %_37foldl148031, i64 0)
%ae50256 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54685 = alloca %struct.ScmObj*, align 8
%fptrToInt54686 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50257 to i64
%ae50257 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54686)
store volatile %struct.ScmObj* %ae50257, %struct.ScmObj** %stackaddr$makeclosure54685, align 8
%argslist54063$ae502550 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54687 = alloca %struct.ScmObj*, align 8
%argslist54063$ae502551 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50257, %struct.ScmObj* %argslist54063$ae502550)
store volatile %struct.ScmObj* %argslist54063$ae502551, %struct.ScmObj** %stackaddr$prim54687, align 8
%stackaddr$prim54688 = alloca %struct.ScmObj*, align 8
%argslist54063$ae502552 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50256, %struct.ScmObj* %argslist54063$ae502551)
store volatile %struct.ScmObj* %argslist54063$ae502552, %struct.ScmObj** %stackaddr$prim54688, align 8
%clofunc54689 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50255)
musttail call tailcc void %clofunc54689(%struct.ScmObj* %ae50255, %struct.ScmObj* %argslist54063$ae502552)
ret void
}

define tailcc void @proc_clo$ae50255(%struct.ScmObj* %env$ae50255,%struct.ScmObj* %current_45args53918) {
%stackaddr$env-ref54690 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50255, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54690
%stackaddr$prim54691 = alloca %struct.ScmObj*, align 8
%_95k48284 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53918)
store volatile %struct.ScmObj* %_95k48284, %struct.ScmObj** %stackaddr$prim54691, align 8
%stackaddr$prim54692 = alloca %struct.ScmObj*, align 8
%current_45args53919 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53918)
store volatile %struct.ScmObj* %current_45args53919, %struct.ScmObj** %stackaddr$prim54692, align 8
%stackaddr$prim54693 = alloca %struct.ScmObj*, align 8
%_37list_6348111 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53919)
store volatile %struct.ScmObj* %_37list_6348111, %struct.ScmObj** %stackaddr$prim54693, align 8
%stackaddr$makeclosure54694 = alloca %struct.ScmObj*, align 8
%fptrToInt54695 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50671 to i64
%ae50671 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54695)
store volatile %struct.ScmObj* %ae50671, %struct.ScmObj** %stackaddr$makeclosure54694, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50671, %struct.ScmObj* %_37foldl148031, i64 0)
%ae50672 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54696 = alloca %struct.ScmObj*, align 8
%fptrToInt54697 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50673 to i64
%ae50673 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54697)
store volatile %struct.ScmObj* %ae50673, %struct.ScmObj** %stackaddr$makeclosure54696, align 8
%argslist54038$ae506710 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54698 = alloca %struct.ScmObj*, align 8
%argslist54038$ae506711 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50673, %struct.ScmObj* %argslist54038$ae506710)
store volatile %struct.ScmObj* %argslist54038$ae506711, %struct.ScmObj** %stackaddr$prim54698, align 8
%stackaddr$prim54699 = alloca %struct.ScmObj*, align 8
%argslist54038$ae506712 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50672, %struct.ScmObj* %argslist54038$ae506711)
store volatile %struct.ScmObj* %argslist54038$ae506712, %struct.ScmObj** %stackaddr$prim54699, align 8
%clofunc54700 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50671)
musttail call tailcc void %clofunc54700(%struct.ScmObj* %ae50671, %struct.ScmObj* %argslist54038$ae506712)
ret void
}

define tailcc void @proc_clo$ae50671(%struct.ScmObj* %env$ae50671,%struct.ScmObj* %current_45args53921) {
%stackaddr$env-ref54701 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50671, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54701
%stackaddr$prim54702 = alloca %struct.ScmObj*, align 8
%_95k48285 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53921)
store volatile %struct.ScmObj* %_95k48285, %struct.ScmObj** %stackaddr$prim54702, align 8
%stackaddr$prim54703 = alloca %struct.ScmObj*, align 8
%current_45args53922 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53921)
store volatile %struct.ScmObj* %current_45args53922, %struct.ScmObj** %stackaddr$prim54703, align 8
%stackaddr$prim54704 = alloca %struct.ScmObj*, align 8
%_37drop48102 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53922)
store volatile %struct.ScmObj* %_37drop48102, %struct.ScmObj** %stackaddr$prim54704, align 8
%stackaddr$makeclosure54705 = alloca %struct.ScmObj*, align 8
%fptrToInt54706 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51207 to i64
%ae51207 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54706)
store volatile %struct.ScmObj* %ae51207, %struct.ScmObj** %stackaddr$makeclosure54705, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51207, %struct.ScmObj* %_37foldl148031, i64 0)
%ae51208 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54707 = alloca %struct.ScmObj*, align 8
%fptrToInt54708 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51209 to i64
%ae51209 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54708)
store volatile %struct.ScmObj* %ae51209, %struct.ScmObj** %stackaddr$makeclosure54707, align 8
%argslist54014$ae512070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54709 = alloca %struct.ScmObj*, align 8
%argslist54014$ae512071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51209, %struct.ScmObj* %argslist54014$ae512070)
store volatile %struct.ScmObj* %argslist54014$ae512071, %struct.ScmObj** %stackaddr$prim54709, align 8
%stackaddr$prim54710 = alloca %struct.ScmObj*, align 8
%argslist54014$ae512072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51208, %struct.ScmObj* %argslist54014$ae512071)
store volatile %struct.ScmObj* %argslist54014$ae512072, %struct.ScmObj** %stackaddr$prim54710, align 8
%clofunc54711 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51207)
musttail call tailcc void %clofunc54711(%struct.ScmObj* %ae51207, %struct.ScmObj* %argslist54014$ae512072)
ret void
}

define tailcc void @proc_clo$ae51207(%struct.ScmObj* %env$ae51207,%struct.ScmObj* %current_45args53924) {
%stackaddr$env-ref54712 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51207, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54712
%stackaddr$prim54713 = alloca %struct.ScmObj*, align 8
%_95k48286 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53924)
store volatile %struct.ScmObj* %_95k48286, %struct.ScmObj** %stackaddr$prim54713, align 8
%stackaddr$prim54714 = alloca %struct.ScmObj*, align 8
%current_45args53925 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53924)
store volatile %struct.ScmObj* %current_45args53925, %struct.ScmObj** %stackaddr$prim54714, align 8
%stackaddr$prim54715 = alloca %struct.ScmObj*, align 8
%_37memv48095 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53925)
store volatile %struct.ScmObj* %_37memv48095, %struct.ScmObj** %stackaddr$prim54715, align 8
%stackaddr$makeclosure54716 = alloca %struct.ScmObj*, align 8
%fptrToInt54717 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51611 to i64
%ae51611 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54717)
store volatile %struct.ScmObj* %ae51611, %struct.ScmObj** %stackaddr$makeclosure54716, align 8
%ae51612 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54718 = alloca %struct.ScmObj*, align 8
%fptrToInt54719 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51613 to i64
%ae51613 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54719)
store volatile %struct.ScmObj* %ae51613, %struct.ScmObj** %stackaddr$makeclosure54718, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51613, %struct.ScmObj* %_37foldl148031, i64 0)
%argslist53988$ae516110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54720 = alloca %struct.ScmObj*, align 8
%argslist53988$ae516111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51613, %struct.ScmObj* %argslist53988$ae516110)
store volatile %struct.ScmObj* %argslist53988$ae516111, %struct.ScmObj** %stackaddr$prim54720, align 8
%stackaddr$prim54721 = alloca %struct.ScmObj*, align 8
%argslist53988$ae516112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51612, %struct.ScmObj* %argslist53988$ae516111)
store volatile %struct.ScmObj* %argslist53988$ae516112, %struct.ScmObj** %stackaddr$prim54721, align 8
%clofunc54722 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51611)
musttail call tailcc void %clofunc54722(%struct.ScmObj* %ae51611, %struct.ScmObj* %argslist53988$ae516112)
ret void
}

define tailcc void @proc_clo$ae51611(%struct.ScmObj* %env$ae51611,%struct.ScmObj* %current_45args53927) {
%stackaddr$prim54723 = alloca %struct.ScmObj*, align 8
%_95k48287 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53927)
store volatile %struct.ScmObj* %_95k48287, %struct.ScmObj** %stackaddr$prim54723, align 8
%stackaddr$prim54724 = alloca %struct.ScmObj*, align 8
%current_45args53928 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53927)
store volatile %struct.ScmObj* %current_45args53928, %struct.ScmObj** %stackaddr$prim54724, align 8
%stackaddr$prim54725 = alloca %struct.ScmObj*, align 8
%_37_4748091 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53928)
store volatile %struct.ScmObj* %_37_4748091, %struct.ScmObj** %stackaddr$prim54725, align 8
%stackaddr$makeclosure54726 = alloca %struct.ScmObj*, align 8
%fptrToInt54727 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51709 to i64
%ae51709 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54727)
store volatile %struct.ScmObj* %ae51709, %struct.ScmObj** %stackaddr$makeclosure54726, align 8
%ae51710 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54728 = alloca %struct.ScmObj*, align 8
%fptrToInt54729 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51711 to i64
%ae51711 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54729)
store volatile %struct.ScmObj* %ae51711, %struct.ScmObj** %stackaddr$makeclosure54728, align 8
%argslist53975$ae517090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54730 = alloca %struct.ScmObj*, align 8
%argslist53975$ae517091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51711, %struct.ScmObj* %argslist53975$ae517090)
store volatile %struct.ScmObj* %argslist53975$ae517091, %struct.ScmObj** %stackaddr$prim54730, align 8
%stackaddr$prim54731 = alloca %struct.ScmObj*, align 8
%argslist53975$ae517092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51710, %struct.ScmObj* %argslist53975$ae517091)
store volatile %struct.ScmObj* %argslist53975$ae517092, %struct.ScmObj** %stackaddr$prim54731, align 8
%clofunc54732 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51709)
musttail call tailcc void %clofunc54732(%struct.ScmObj* %ae51709, %struct.ScmObj* %argslist53975$ae517092)
ret void
}

define tailcc void @proc_clo$ae51709(%struct.ScmObj* %env$ae51709,%struct.ScmObj* %current_45args53930) {
%stackaddr$prim54733 = alloca %struct.ScmObj*, align 8
%_95k48288 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53930)
store volatile %struct.ScmObj* %_95k48288, %struct.ScmObj** %stackaddr$prim54733, align 8
%stackaddr$prim54734 = alloca %struct.ScmObj*, align 8
%current_45args53931 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53930)
store volatile %struct.ScmObj* %current_45args53931, %struct.ScmObj** %stackaddr$prim54734, align 8
%stackaddr$prim54735 = alloca %struct.ScmObj*, align 8
%_37first48089 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53931)
store volatile %struct.ScmObj* %_37first48089, %struct.ScmObj** %stackaddr$prim54735, align 8
%stackaddr$makeclosure54736 = alloca %struct.ScmObj*, align 8
%fptrToInt54737 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51729 to i64
%ae51729 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54737)
store volatile %struct.ScmObj* %ae51729, %struct.ScmObj** %stackaddr$makeclosure54736, align 8
%ae51730 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54738 = alloca %struct.ScmObj*, align 8
%fptrToInt54739 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51731 to i64
%ae51731 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54739)
store volatile %struct.ScmObj* %ae51731, %struct.ScmObj** %stackaddr$makeclosure54738, align 8
%argslist53970$ae517290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54740 = alloca %struct.ScmObj*, align 8
%argslist53970$ae517291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51731, %struct.ScmObj* %argslist53970$ae517290)
store volatile %struct.ScmObj* %argslist53970$ae517291, %struct.ScmObj** %stackaddr$prim54740, align 8
%stackaddr$prim54741 = alloca %struct.ScmObj*, align 8
%argslist53970$ae517292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51730, %struct.ScmObj* %argslist53970$ae517291)
store volatile %struct.ScmObj* %argslist53970$ae517292, %struct.ScmObj** %stackaddr$prim54741, align 8
%clofunc54742 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51729)
musttail call tailcc void %clofunc54742(%struct.ScmObj* %ae51729, %struct.ScmObj* %argslist53970$ae517292)
ret void
}

define tailcc void @proc_clo$ae51729(%struct.ScmObj* %env$ae51729,%struct.ScmObj* %current_45args53933) {
%stackaddr$prim54743 = alloca %struct.ScmObj*, align 8
%_95k48289 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53933)
store volatile %struct.ScmObj* %_95k48289, %struct.ScmObj** %stackaddr$prim54743, align 8
%stackaddr$prim54744 = alloca %struct.ScmObj*, align 8
%current_45args53934 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53933)
store volatile %struct.ScmObj* %current_45args53934, %struct.ScmObj** %stackaddr$prim54744, align 8
%stackaddr$prim54745 = alloca %struct.ScmObj*, align 8
%_37second48087 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53934)
store volatile %struct.ScmObj* %_37second48087, %struct.ScmObj** %stackaddr$prim54745, align 8
%stackaddr$makeclosure54746 = alloca %struct.ScmObj*, align 8
%fptrToInt54747 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51751 to i64
%ae51751 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54747)
store volatile %struct.ScmObj* %ae51751, %struct.ScmObj** %stackaddr$makeclosure54746, align 8
%ae51752 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54748 = alloca %struct.ScmObj*, align 8
%fptrToInt54749 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51753 to i64
%ae51753 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54749)
store volatile %struct.ScmObj* %ae51753, %struct.ScmObj** %stackaddr$makeclosure54748, align 8
%argslist53965$ae517510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54750 = alloca %struct.ScmObj*, align 8
%argslist53965$ae517511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51753, %struct.ScmObj* %argslist53965$ae517510)
store volatile %struct.ScmObj* %argslist53965$ae517511, %struct.ScmObj** %stackaddr$prim54750, align 8
%stackaddr$prim54751 = alloca %struct.ScmObj*, align 8
%argslist53965$ae517512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51752, %struct.ScmObj* %argslist53965$ae517511)
store volatile %struct.ScmObj* %argslist53965$ae517512, %struct.ScmObj** %stackaddr$prim54751, align 8
%clofunc54752 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51751)
musttail call tailcc void %clofunc54752(%struct.ScmObj* %ae51751, %struct.ScmObj* %argslist53965$ae517512)
ret void
}

define tailcc void @proc_clo$ae51751(%struct.ScmObj* %env$ae51751,%struct.ScmObj* %current_45args53936) {
%stackaddr$prim54753 = alloca %struct.ScmObj*, align 8
%_95k48290 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53936)
store volatile %struct.ScmObj* %_95k48290, %struct.ScmObj** %stackaddr$prim54753, align 8
%stackaddr$prim54754 = alloca %struct.ScmObj*, align 8
%current_45args53937 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53936)
store volatile %struct.ScmObj* %current_45args53937, %struct.ScmObj** %stackaddr$prim54754, align 8
%stackaddr$prim54755 = alloca %struct.ScmObj*, align 8
%_37third48085 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53937)
store volatile %struct.ScmObj* %_37third48085, %struct.ScmObj** %stackaddr$prim54755, align 8
%stackaddr$makeclosure54756 = alloca %struct.ScmObj*, align 8
%fptrToInt54757 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51775 to i64
%ae51775 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54757)
store volatile %struct.ScmObj* %ae51775, %struct.ScmObj** %stackaddr$makeclosure54756, align 8
%ae51776 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54758 = alloca %struct.ScmObj*, align 8
%fptrToInt54759 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51777 to i64
%ae51777 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54759)
store volatile %struct.ScmObj* %ae51777, %struct.ScmObj** %stackaddr$makeclosure54758, align 8
%argslist53960$ae517750 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54760 = alloca %struct.ScmObj*, align 8
%argslist53960$ae517751 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51777, %struct.ScmObj* %argslist53960$ae517750)
store volatile %struct.ScmObj* %argslist53960$ae517751, %struct.ScmObj** %stackaddr$prim54760, align 8
%stackaddr$prim54761 = alloca %struct.ScmObj*, align 8
%argslist53960$ae517752 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51776, %struct.ScmObj* %argslist53960$ae517751)
store volatile %struct.ScmObj* %argslist53960$ae517752, %struct.ScmObj** %stackaddr$prim54761, align 8
%clofunc54762 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51775)
musttail call tailcc void %clofunc54762(%struct.ScmObj* %ae51775, %struct.ScmObj* %argslist53960$ae517752)
ret void
}

define tailcc void @proc_clo$ae51775(%struct.ScmObj* %env$ae51775,%struct.ScmObj* %current_45args53939) {
%stackaddr$prim54763 = alloca %struct.ScmObj*, align 8
%_95k48291 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53939)
store volatile %struct.ScmObj* %_95k48291, %struct.ScmObj** %stackaddr$prim54763, align 8
%stackaddr$prim54764 = alloca %struct.ScmObj*, align 8
%current_45args53940 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53939)
store volatile %struct.ScmObj* %current_45args53940, %struct.ScmObj** %stackaddr$prim54764, align 8
%stackaddr$prim54765 = alloca %struct.ScmObj*, align 8
%_37fourth48083 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53940)
store volatile %struct.ScmObj* %_37fourth48083, %struct.ScmObj** %stackaddr$prim54765, align 8
%stackaddr$makeclosure54766 = alloca %struct.ScmObj*, align 8
%fptrToInt54767 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51801 to i64
%ae51801 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54767)
store volatile %struct.ScmObj* %ae51801, %struct.ScmObj** %stackaddr$makeclosure54766, align 8
%ae51802 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54768 = alloca %struct.ScmObj*, align 8
%fptrToInt54769 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51803 to i64
%ae51803 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54769)
store volatile %struct.ScmObj* %ae51803, %struct.ScmObj** %stackaddr$makeclosure54768, align 8
%argslist53955$ae518010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54770 = alloca %struct.ScmObj*, align 8
%argslist53955$ae518011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51803, %struct.ScmObj* %argslist53955$ae518010)
store volatile %struct.ScmObj* %argslist53955$ae518011, %struct.ScmObj** %stackaddr$prim54770, align 8
%stackaddr$prim54771 = alloca %struct.ScmObj*, align 8
%argslist53955$ae518012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51802, %struct.ScmObj* %argslist53955$ae518011)
store volatile %struct.ScmObj* %argslist53955$ae518012, %struct.ScmObj** %stackaddr$prim54771, align 8
%clofunc54772 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51801)
musttail call tailcc void %clofunc54772(%struct.ScmObj* %ae51801, %struct.ScmObj* %argslist53955$ae518012)
ret void
}

define tailcc void @proc_clo$ae51801(%struct.ScmObj* %env$ae51801,%struct.ScmObj* %current_45args53942) {
%stackaddr$prim54773 = alloca %struct.ScmObj*, align 8
%_95k48292 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53942)
store volatile %struct.ScmObj* %_95k48292, %struct.ScmObj** %stackaddr$prim54773, align 8
%stackaddr$prim54774 = alloca %struct.ScmObj*, align 8
%current_45args53943 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53942)
store volatile %struct.ScmObj* %current_45args53943, %struct.ScmObj** %stackaddr$prim54774, align 8
%stackaddr$prim54775 = alloca %struct.ScmObj*, align 8
%anf_45bind48259 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53943)
store volatile %struct.ScmObj* %anf_45bind48259, %struct.ScmObj** %stackaddr$prim54775, align 8
%stackaddr$makeclosure54776 = alloca %struct.ScmObj*, align 8
%fptrToInt54777 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51842 to i64
%ae51842 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54777)
store volatile %struct.ScmObj* %ae51842, %struct.ScmObj** %stackaddr$makeclosure54776, align 8
%ae51843 = call %struct.ScmObj* @const_init_int(i64 2)
%ae51844 = call %struct.ScmObj* @const_init_int(i64 3)
%ae51845 = call %struct.ScmObj* @const_init_int(i64 4)
%ae51846 = call %struct.ScmObj* @const_init_int(i64 5)
%argslist53949$anf_45bind482590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54778 = alloca %struct.ScmObj*, align 8
%argslist53949$anf_45bind482591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51846, %struct.ScmObj* %argslist53949$anf_45bind482590)
store volatile %struct.ScmObj* %argslist53949$anf_45bind482591, %struct.ScmObj** %stackaddr$prim54778, align 8
%stackaddr$prim54779 = alloca %struct.ScmObj*, align 8
%argslist53949$anf_45bind482592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51845, %struct.ScmObj* %argslist53949$anf_45bind482591)
store volatile %struct.ScmObj* %argslist53949$anf_45bind482592, %struct.ScmObj** %stackaddr$prim54779, align 8
%stackaddr$prim54780 = alloca %struct.ScmObj*, align 8
%argslist53949$anf_45bind482593 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51844, %struct.ScmObj* %argslist53949$anf_45bind482592)
store volatile %struct.ScmObj* %argslist53949$anf_45bind482593, %struct.ScmObj** %stackaddr$prim54780, align 8
%stackaddr$prim54781 = alloca %struct.ScmObj*, align 8
%argslist53949$anf_45bind482594 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51843, %struct.ScmObj* %argslist53949$anf_45bind482593)
store volatile %struct.ScmObj* %argslist53949$anf_45bind482594, %struct.ScmObj** %stackaddr$prim54781, align 8
%stackaddr$prim54782 = alloca %struct.ScmObj*, align 8
%argslist53949$anf_45bind482595 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51842, %struct.ScmObj* %argslist53949$anf_45bind482594)
store volatile %struct.ScmObj* %argslist53949$anf_45bind482595, %struct.ScmObj** %stackaddr$prim54782, align 8
%clofunc54783 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48259)
musttail call tailcc void %clofunc54783(%struct.ScmObj* %anf_45bind48259, %struct.ScmObj* %argslist53949$anf_45bind482595)
ret void
}

define tailcc void @proc_clo$ae51842(%struct.ScmObj* %env$ae51842,%struct.ScmObj* %current_45args53945) {
%stackaddr$prim54784 = alloca %struct.ScmObj*, align 8
%k = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53945)
store volatile %struct.ScmObj* %k, %struct.ScmObj** %stackaddr$prim54784, align 8
%stackaddr$prim54785 = alloca %struct.ScmObj*, align 8
%current_45args53946 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53945)
store volatile %struct.ScmObj* %current_45args53946, %struct.ScmObj** %stackaddr$prim54785, align 8
%stackaddr$prim54786 = alloca %struct.ScmObj*, align 8
%x = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53946)
store volatile %struct.ScmObj* %x, %struct.ScmObj** %stackaddr$prim54786, align 8
%stackaddr$prim54787 = alloca %struct.ScmObj*, align 8
%_95die = call %struct.ScmObj* @prim_halt(%struct.ScmObj* %x)
store volatile %struct.ScmObj* %_95die, %struct.ScmObj** %stackaddr$prim54787, align 8
%argslist53948$k0 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54788 = alloca %struct.ScmObj*, align 8
%argslist53948$k1 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x, %struct.ScmObj* %argslist53948$k0)
store volatile %struct.ScmObj* %argslist53948$k1, %struct.ScmObj** %stackaddr$prim54788, align 8
%clofunc54789 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k)
musttail call tailcc void %clofunc54789(%struct.ScmObj* %k, %struct.ScmObj* %argslist53948$k1)
ret void
}

define tailcc void @proc_clo$ae51803(%struct.ScmObj* %env$ae51803,%struct.ScmObj* %t480254814448293) {
%stackaddr$prim54790 = alloca %struct.ScmObj*, align 8
%k48294 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %t480254814448293)
store volatile %struct.ScmObj* %k48294, %struct.ScmObj** %stackaddr$prim54790, align 8
%stackaddr$prim54791 = alloca %struct.ScmObj*, align 8
%t4802548144 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %t480254814448293)
store volatile %struct.ScmObj* %t4802548144, %struct.ScmObj** %stackaddr$prim54791, align 8
%stackaddr$prim54792 = alloca %struct.ScmObj*, align 8
%a48145 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %t4802548144)
store volatile %struct.ScmObj* %a48145, %struct.ScmObj** %stackaddr$prim54792, align 8
%stackaddr$prim54793 = alloca %struct.ScmObj*, align 8
%t4802548146 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %t4802548144)
store volatile %struct.ScmObj* %t4802548146, %struct.ScmObj** %stackaddr$prim54793, align 8
%stackaddr$makeclosure54794 = alloca %struct.ScmObj*, align 8
%fptrToInt54795 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51808 to i64
%ae51808 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt54795)
store volatile %struct.ScmObj* %ae51808, %struct.ScmObj** %stackaddr$makeclosure54794, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51808, %struct.ScmObj* %k48294, i64 0)
%ae51809 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53954$ae518080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54796 = alloca %struct.ScmObj*, align 8
%argslist53954$ae518081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %t4802548146, %struct.ScmObj* %argslist53954$ae518080)
store volatile %struct.ScmObj* %argslist53954$ae518081, %struct.ScmObj** %stackaddr$prim54796, align 8
%stackaddr$prim54797 = alloca %struct.ScmObj*, align 8
%argslist53954$ae518082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51809, %struct.ScmObj* %argslist53954$ae518081)
store volatile %struct.ScmObj* %argslist53954$ae518082, %struct.ScmObj** %stackaddr$prim54797, align 8
%clofunc54798 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51808)
musttail call tailcc void %clofunc54798(%struct.ScmObj* %ae51808, %struct.ScmObj* %argslist53954$ae518082)
ret void
}

define tailcc void @proc_clo$ae51808(%struct.ScmObj* %env$ae51808,%struct.ScmObj* %current_45args53950) {
%stackaddr$env-ref54799 = alloca %struct.ScmObj*, align 8
%k48294 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51808, i64 0)
store %struct.ScmObj* %k48294, %struct.ScmObj** %stackaddr$env-ref54799
%stackaddr$prim54800 = alloca %struct.ScmObj*, align 8
%_95k48295 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53950)
store volatile %struct.ScmObj* %_95k48295, %struct.ScmObj** %stackaddr$prim54800, align 8
%stackaddr$prim54801 = alloca %struct.ScmObj*, align 8
%current_45args53951 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53950)
store volatile %struct.ScmObj* %current_45args53951, %struct.ScmObj** %stackaddr$prim54801, align 8
%stackaddr$prim54802 = alloca %struct.ScmObj*, align 8
%b48147 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53951)
store volatile %struct.ScmObj* %b48147, %struct.ScmObj** %stackaddr$prim54802, align 8
%stackaddr$prim54803 = alloca %struct.ScmObj*, align 8
%cpsprim48296 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %b48147)
store volatile %struct.ScmObj* %cpsprim48296, %struct.ScmObj** %stackaddr$prim54803, align 8
%ae51816 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53953$k482940 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54804 = alloca %struct.ScmObj*, align 8
%argslist53953$k482941 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48296, %struct.ScmObj* %argslist53953$k482940)
store volatile %struct.ScmObj* %argslist53953$k482941, %struct.ScmObj** %stackaddr$prim54804, align 8
%stackaddr$prim54805 = alloca %struct.ScmObj*, align 8
%argslist53953$k482942 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51816, %struct.ScmObj* %argslist53953$k482941)
store volatile %struct.ScmObj* %argslist53953$k482942, %struct.ScmObj** %stackaddr$prim54805, align 8
%clofunc54806 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48294)
musttail call tailcc void %clofunc54806(%struct.ScmObj* %k48294, %struct.ScmObj* %argslist53953$k482942)
ret void
}

define tailcc void @proc_clo$ae51777(%struct.ScmObj* %env$ae51777,%struct.ScmObj* %current_45args53956) {
%stackaddr$prim54807 = alloca %struct.ScmObj*, align 8
%k48297 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53956)
store volatile %struct.ScmObj* %k48297, %struct.ScmObj** %stackaddr$prim54807, align 8
%stackaddr$prim54808 = alloca %struct.ScmObj*, align 8
%current_45args53957 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53956)
store volatile %struct.ScmObj* %current_45args53957, %struct.ScmObj** %stackaddr$prim54808, align 8
%stackaddr$prim54809 = alloca %struct.ScmObj*, align 8
%x48084 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53957)
store volatile %struct.ScmObj* %x48084, %struct.ScmObj** %stackaddr$prim54809, align 8
%stackaddr$prim54810 = alloca %struct.ScmObj*, align 8
%anf_45bind48256 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48084)
store volatile %struct.ScmObj* %anf_45bind48256, %struct.ScmObj** %stackaddr$prim54810, align 8
%stackaddr$prim54811 = alloca %struct.ScmObj*, align 8
%anf_45bind48257 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48256)
store volatile %struct.ScmObj* %anf_45bind48257, %struct.ScmObj** %stackaddr$prim54811, align 8
%stackaddr$prim54812 = alloca %struct.ScmObj*, align 8
%anf_45bind48258 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48257)
store volatile %struct.ScmObj* %anf_45bind48258, %struct.ScmObj** %stackaddr$prim54812, align 8
%stackaddr$prim54813 = alloca %struct.ScmObj*, align 8
%cpsprim48298 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48258)
store volatile %struct.ScmObj* %cpsprim48298, %struct.ScmObj** %stackaddr$prim54813, align 8
%ae51783 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53959$k482970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54814 = alloca %struct.ScmObj*, align 8
%argslist53959$k482971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48298, %struct.ScmObj* %argslist53959$k482970)
store volatile %struct.ScmObj* %argslist53959$k482971, %struct.ScmObj** %stackaddr$prim54814, align 8
%stackaddr$prim54815 = alloca %struct.ScmObj*, align 8
%argslist53959$k482972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51783, %struct.ScmObj* %argslist53959$k482971)
store volatile %struct.ScmObj* %argslist53959$k482972, %struct.ScmObj** %stackaddr$prim54815, align 8
%clofunc54816 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48297)
musttail call tailcc void %clofunc54816(%struct.ScmObj* %k48297, %struct.ScmObj* %argslist53959$k482972)
ret void
}

define tailcc void @proc_clo$ae51753(%struct.ScmObj* %env$ae51753,%struct.ScmObj* %current_45args53961) {
%stackaddr$prim54817 = alloca %struct.ScmObj*, align 8
%k48299 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53961)
store volatile %struct.ScmObj* %k48299, %struct.ScmObj** %stackaddr$prim54817, align 8
%stackaddr$prim54818 = alloca %struct.ScmObj*, align 8
%current_45args53962 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53961)
store volatile %struct.ScmObj* %current_45args53962, %struct.ScmObj** %stackaddr$prim54818, align 8
%stackaddr$prim54819 = alloca %struct.ScmObj*, align 8
%x48086 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53962)
store volatile %struct.ScmObj* %x48086, %struct.ScmObj** %stackaddr$prim54819, align 8
%stackaddr$prim54820 = alloca %struct.ScmObj*, align 8
%anf_45bind48254 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48086)
store volatile %struct.ScmObj* %anf_45bind48254, %struct.ScmObj** %stackaddr$prim54820, align 8
%stackaddr$prim54821 = alloca %struct.ScmObj*, align 8
%anf_45bind48255 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48254)
store volatile %struct.ScmObj* %anf_45bind48255, %struct.ScmObj** %stackaddr$prim54821, align 8
%stackaddr$prim54822 = alloca %struct.ScmObj*, align 8
%cpsprim48300 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48255)
store volatile %struct.ScmObj* %cpsprim48300, %struct.ScmObj** %stackaddr$prim54822, align 8
%ae51758 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53964$k482990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54823 = alloca %struct.ScmObj*, align 8
%argslist53964$k482991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48300, %struct.ScmObj* %argslist53964$k482990)
store volatile %struct.ScmObj* %argslist53964$k482991, %struct.ScmObj** %stackaddr$prim54823, align 8
%stackaddr$prim54824 = alloca %struct.ScmObj*, align 8
%argslist53964$k482992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51758, %struct.ScmObj* %argslist53964$k482991)
store volatile %struct.ScmObj* %argslist53964$k482992, %struct.ScmObj** %stackaddr$prim54824, align 8
%clofunc54825 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48299)
musttail call tailcc void %clofunc54825(%struct.ScmObj* %k48299, %struct.ScmObj* %argslist53964$k482992)
ret void
}

define tailcc void @proc_clo$ae51731(%struct.ScmObj* %env$ae51731,%struct.ScmObj* %current_45args53966) {
%stackaddr$prim54826 = alloca %struct.ScmObj*, align 8
%k48301 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53966)
store volatile %struct.ScmObj* %k48301, %struct.ScmObj** %stackaddr$prim54826, align 8
%stackaddr$prim54827 = alloca %struct.ScmObj*, align 8
%current_45args53967 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53966)
store volatile %struct.ScmObj* %current_45args53967, %struct.ScmObj** %stackaddr$prim54827, align 8
%stackaddr$prim54828 = alloca %struct.ScmObj*, align 8
%x48088 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53967)
store volatile %struct.ScmObj* %x48088, %struct.ScmObj** %stackaddr$prim54828, align 8
%stackaddr$prim54829 = alloca %struct.ScmObj*, align 8
%anf_45bind48253 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48088)
store volatile %struct.ScmObj* %anf_45bind48253, %struct.ScmObj** %stackaddr$prim54829, align 8
%stackaddr$prim54830 = alloca %struct.ScmObj*, align 8
%cpsprim48302 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48253)
store volatile %struct.ScmObj* %cpsprim48302, %struct.ScmObj** %stackaddr$prim54830, align 8
%ae51735 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53969$k483010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54831 = alloca %struct.ScmObj*, align 8
%argslist53969$k483011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48302, %struct.ScmObj* %argslist53969$k483010)
store volatile %struct.ScmObj* %argslist53969$k483011, %struct.ScmObj** %stackaddr$prim54831, align 8
%stackaddr$prim54832 = alloca %struct.ScmObj*, align 8
%argslist53969$k483012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51735, %struct.ScmObj* %argslist53969$k483011)
store volatile %struct.ScmObj* %argslist53969$k483012, %struct.ScmObj** %stackaddr$prim54832, align 8
%clofunc54833 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48301)
musttail call tailcc void %clofunc54833(%struct.ScmObj* %k48301, %struct.ScmObj* %argslist53969$k483012)
ret void
}

define tailcc void @proc_clo$ae51711(%struct.ScmObj* %env$ae51711,%struct.ScmObj* %current_45args53971) {
%stackaddr$prim54834 = alloca %struct.ScmObj*, align 8
%k48303 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53971)
store volatile %struct.ScmObj* %k48303, %struct.ScmObj** %stackaddr$prim54834, align 8
%stackaddr$prim54835 = alloca %struct.ScmObj*, align 8
%current_45args53972 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53971)
store volatile %struct.ScmObj* %current_45args53972, %struct.ScmObj** %stackaddr$prim54835, align 8
%stackaddr$prim54836 = alloca %struct.ScmObj*, align 8
%x48090 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53972)
store volatile %struct.ScmObj* %x48090, %struct.ScmObj** %stackaddr$prim54836, align 8
%stackaddr$prim54837 = alloca %struct.ScmObj*, align 8
%cpsprim48304 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48090)
store volatile %struct.ScmObj* %cpsprim48304, %struct.ScmObj** %stackaddr$prim54837, align 8
%ae51714 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53974$k483030 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54838 = alloca %struct.ScmObj*, align 8
%argslist53974$k483031 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48304, %struct.ScmObj* %argslist53974$k483030)
store volatile %struct.ScmObj* %argslist53974$k483031, %struct.ScmObj** %stackaddr$prim54838, align 8
%stackaddr$prim54839 = alloca %struct.ScmObj*, align 8
%argslist53974$k483032 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51714, %struct.ScmObj* %argslist53974$k483031)
store volatile %struct.ScmObj* %argslist53974$k483032, %struct.ScmObj** %stackaddr$prim54839, align 8
%clofunc54840 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48303)
musttail call tailcc void %clofunc54840(%struct.ScmObj* %k48303, %struct.ScmObj* %argslist53974$k483032)
ret void
}

define tailcc void @proc_clo$ae51613(%struct.ScmObj* %env$ae51613,%struct.ScmObj* %args4809248305) {
%stackaddr$env-ref54841 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51613, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54841
%stackaddr$prim54842 = alloca %struct.ScmObj*, align 8
%k48306 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4809248305)
store volatile %struct.ScmObj* %k48306, %struct.ScmObj** %stackaddr$prim54842, align 8
%stackaddr$prim54843 = alloca %struct.ScmObj*, align 8
%args48092 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4809248305)
store volatile %struct.ScmObj* %args48092, %struct.ScmObj** %stackaddr$prim54843, align 8
%stackaddr$prim54844 = alloca %struct.ScmObj*, align 8
%anf_45bind48247 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %args48092)
store volatile %struct.ScmObj* %anf_45bind48247, %struct.ScmObj** %stackaddr$prim54844, align 8
%truthy$cmp54845 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48247)
%cmp$cmp54845 = icmp eq i64 %truthy$cmp54845, 1
br i1 %cmp$cmp54845, label %truebranch$cmp54845, label %falsebranch$cmp54845
truebranch$cmp54845:
%ae51619 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51620 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist53976$k483060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54846 = alloca %struct.ScmObj*, align 8
%argslist53976$k483061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51620, %struct.ScmObj* %argslist53976$k483060)
store volatile %struct.ScmObj* %argslist53976$k483061, %struct.ScmObj** %stackaddr$prim54846, align 8
%stackaddr$prim54847 = alloca %struct.ScmObj*, align 8
%argslist53976$k483062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51619, %struct.ScmObj* %argslist53976$k483061)
store volatile %struct.ScmObj* %argslist53976$k483062, %struct.ScmObj** %stackaddr$prim54847, align 8
%clofunc54848 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48306)
musttail call tailcc void %clofunc54848(%struct.ScmObj* %k48306, %struct.ScmObj* %argslist53976$k483062)
ret void
falsebranch$cmp54845:
%stackaddr$prim54849 = alloca %struct.ScmObj*, align 8
%anf_45bind48248 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48092)
store volatile %struct.ScmObj* %anf_45bind48248, %struct.ScmObj** %stackaddr$prim54849, align 8
%stackaddr$prim54850 = alloca %struct.ScmObj*, align 8
%anf_45bind48249 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48248)
store volatile %struct.ScmObj* %anf_45bind48249, %struct.ScmObj** %stackaddr$prim54850, align 8
%truthy$cmp54851 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48249)
%cmp$cmp54851 = icmp eq i64 %truthy$cmp54851, 1
br i1 %cmp$cmp54851, label %truebranch$cmp54851, label %falsebranch$cmp54851
truebranch$cmp54851:
%stackaddr$prim54852 = alloca %struct.ScmObj*, align 8
%cpsprim48307 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48092)
store volatile %struct.ScmObj* %cpsprim48307, %struct.ScmObj** %stackaddr$prim54852, align 8
%ae51632 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53977$k483060 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54853 = alloca %struct.ScmObj*, align 8
%argslist53977$k483061 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48307, %struct.ScmObj* %argslist53977$k483060)
store volatile %struct.ScmObj* %argslist53977$k483061, %struct.ScmObj** %stackaddr$prim54853, align 8
%stackaddr$prim54854 = alloca %struct.ScmObj*, align 8
%argslist53977$k483062 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51632, %struct.ScmObj* %argslist53977$k483061)
store volatile %struct.ScmObj* %argslist53977$k483062, %struct.ScmObj** %stackaddr$prim54854, align 8
%clofunc54855 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48306)
musttail call tailcc void %clofunc54855(%struct.ScmObj* %k48306, %struct.ScmObj* %argslist53977$k483062)
ret void
falsebranch$cmp54851:
%stackaddr$makeclosure54856 = alloca %struct.ScmObj*, align 8
%fptrToInt54857 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51637 to i64
%ae51637 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54857)
store volatile %struct.ScmObj* %ae51637, %struct.ScmObj** %stackaddr$makeclosure54856, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51637, %struct.ScmObj* %k48306, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51637, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51637, %struct.ScmObj* %args48092, i64 2)
%ae51638 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54858 = alloca %struct.ScmObj*, align 8
%fptrToInt54859 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51639 to i64
%ae51639 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54859)
store volatile %struct.ScmObj* %ae51639, %struct.ScmObj** %stackaddr$makeclosure54858, align 8
%argslist53987$ae516370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54860 = alloca %struct.ScmObj*, align 8
%argslist53987$ae516371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51639, %struct.ScmObj* %argslist53987$ae516370)
store volatile %struct.ScmObj* %argslist53987$ae516371, %struct.ScmObj** %stackaddr$prim54860, align 8
%stackaddr$prim54861 = alloca %struct.ScmObj*, align 8
%argslist53987$ae516372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51638, %struct.ScmObj* %argslist53987$ae516371)
store volatile %struct.ScmObj* %argslist53987$ae516372, %struct.ScmObj** %stackaddr$prim54861, align 8
%clofunc54862 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51637)
musttail call tailcc void %clofunc54862(%struct.ScmObj* %ae51637, %struct.ScmObj* %argslist53987$ae516372)
ret void
}

define tailcc void @proc_clo$ae51637(%struct.ScmObj* %env$ae51637,%struct.ScmObj* %current_45args53978) {
%stackaddr$env-ref54863 = alloca %struct.ScmObj*, align 8
%k48306 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51637, i64 0)
store %struct.ScmObj* %k48306, %struct.ScmObj** %stackaddr$env-ref54863
%stackaddr$env-ref54864 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51637, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref54864
%stackaddr$env-ref54865 = alloca %struct.ScmObj*, align 8
%args48092 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51637, i64 2)
store %struct.ScmObj* %args48092, %struct.ScmObj** %stackaddr$env-ref54865
%stackaddr$prim54866 = alloca %struct.ScmObj*, align 8
%_95k48308 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53978)
store volatile %struct.ScmObj* %_95k48308, %struct.ScmObj** %stackaddr$prim54866, align 8
%stackaddr$prim54867 = alloca %struct.ScmObj*, align 8
%current_45args53979 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53978)
store volatile %struct.ScmObj* %current_45args53979, %struct.ScmObj** %stackaddr$prim54867, align 8
%stackaddr$prim54868 = alloca %struct.ScmObj*, align 8
%anf_45bind48250 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53979)
store volatile %struct.ScmObj* %anf_45bind48250, %struct.ScmObj** %stackaddr$prim54868, align 8
%stackaddr$prim54869 = alloca %struct.ScmObj*, align 8
%anf_45bind48251 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48092)
store volatile %struct.ScmObj* %anf_45bind48251, %struct.ScmObj** %stackaddr$prim54869, align 8
%stackaddr$prim54870 = alloca %struct.ScmObj*, align 8
%anf_45bind48252 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48092)
store volatile %struct.ScmObj* %anf_45bind48252, %struct.ScmObj** %stackaddr$prim54870, align 8
%argslist53981$_37foldl1480310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54871 = alloca %struct.ScmObj*, align 8
%argslist53981$_37foldl1480311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48252, %struct.ScmObj* %argslist53981$_37foldl1480310)
store volatile %struct.ScmObj* %argslist53981$_37foldl1480311, %struct.ScmObj** %stackaddr$prim54871, align 8
%stackaddr$prim54872 = alloca %struct.ScmObj*, align 8
%argslist53981$_37foldl1480312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48251, %struct.ScmObj* %argslist53981$_37foldl1480311)
store volatile %struct.ScmObj* %argslist53981$_37foldl1480312, %struct.ScmObj** %stackaddr$prim54872, align 8
%stackaddr$prim54873 = alloca %struct.ScmObj*, align 8
%argslist53981$_37foldl1480313 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48250, %struct.ScmObj* %argslist53981$_37foldl1480312)
store volatile %struct.ScmObj* %argslist53981$_37foldl1480313, %struct.ScmObj** %stackaddr$prim54873, align 8
%stackaddr$prim54874 = alloca %struct.ScmObj*, align 8
%argslist53981$_37foldl1480314 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48306, %struct.ScmObj* %argslist53981$_37foldl1480313)
store volatile %struct.ScmObj* %argslist53981$_37foldl1480314, %struct.ScmObj** %stackaddr$prim54874, align 8
%clofunc54875 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148031)
musttail call tailcc void %clofunc54875(%struct.ScmObj* %_37foldl148031, %struct.ScmObj* %argslist53981$_37foldl1480314)
ret void
}

define tailcc void @proc_clo$ae51639(%struct.ScmObj* %env$ae51639,%struct.ScmObj* %current_45args53982) {
%stackaddr$prim54876 = alloca %struct.ScmObj*, align 8
%k48309 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53982)
store volatile %struct.ScmObj* %k48309, %struct.ScmObj** %stackaddr$prim54876, align 8
%stackaddr$prim54877 = alloca %struct.ScmObj*, align 8
%current_45args53983 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53982)
store volatile %struct.ScmObj* %current_45args53983, %struct.ScmObj** %stackaddr$prim54877, align 8
%stackaddr$prim54878 = alloca %struct.ScmObj*, align 8
%n48094 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53983)
store volatile %struct.ScmObj* %n48094, %struct.ScmObj** %stackaddr$prim54878, align 8
%stackaddr$prim54879 = alloca %struct.ScmObj*, align 8
%current_45args53984 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53983)
store volatile %struct.ScmObj* %current_45args53984, %struct.ScmObj** %stackaddr$prim54879, align 8
%stackaddr$prim54880 = alloca %struct.ScmObj*, align 8
%v48093 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53984)
store volatile %struct.ScmObj* %v48093, %struct.ScmObj** %stackaddr$prim54880, align 8
%stackaddr$prim54881 = alloca %struct.ScmObj*, align 8
%cpsprim48310 = call %struct.ScmObj* @prim__47(%struct.ScmObj* %v48093, %struct.ScmObj* %n48094)
store volatile %struct.ScmObj* %cpsprim48310, %struct.ScmObj** %stackaddr$prim54881, align 8
%ae51643 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist53986$k483090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54882 = alloca %struct.ScmObj*, align 8
%argslist53986$k483091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48310, %struct.ScmObj* %argslist53986$k483090)
store volatile %struct.ScmObj* %argslist53986$k483091, %struct.ScmObj** %stackaddr$prim54882, align 8
%stackaddr$prim54883 = alloca %struct.ScmObj*, align 8
%argslist53986$k483092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51643, %struct.ScmObj* %argslist53986$k483091)
store volatile %struct.ScmObj* %argslist53986$k483092, %struct.ScmObj** %stackaddr$prim54883, align 8
%clofunc54884 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48309)
musttail call tailcc void %clofunc54884(%struct.ScmObj* %k48309, %struct.ScmObj* %argslist53986$k483092)
ret void
}

define tailcc void @proc_clo$ae51209(%struct.ScmObj* %env$ae51209,%struct.ScmObj* %current_45args53989) {
%stackaddr$prim54885 = alloca %struct.ScmObj*, align 8
%k48311 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53989)
store volatile %struct.ScmObj* %k48311, %struct.ScmObj** %stackaddr$prim54885, align 8
%stackaddr$prim54886 = alloca %struct.ScmObj*, align 8
%current_45args53990 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53989)
store volatile %struct.ScmObj* %current_45args53990, %struct.ScmObj** %stackaddr$prim54886, align 8
%stackaddr$prim54887 = alloca %struct.ScmObj*, align 8
%v48097 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53990)
store volatile %struct.ScmObj* %v48097, %struct.ScmObj** %stackaddr$prim54887, align 8
%stackaddr$prim54888 = alloca %struct.ScmObj*, align 8
%current_45args53991 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53990)
store volatile %struct.ScmObj* %current_45args53991, %struct.ScmObj** %stackaddr$prim54888, align 8
%stackaddr$prim54889 = alloca %struct.ScmObj*, align 8
%lst48096 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53991)
store volatile %struct.ScmObj* %lst48096, %struct.ScmObj** %stackaddr$prim54889, align 8
%ae51210 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54890 = alloca %struct.ScmObj*, align 8
%lst48098 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae51210, %struct.ScmObj* %lst48096)
store volatile %struct.ScmObj* %lst48098, %struct.ScmObj** %stackaddr$prim54890, align 8
%stackaddr$makeclosure54891 = alloca %struct.ScmObj*, align 8
%fptrToInt54892 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51212 to i64
%ae51212 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54892)
store volatile %struct.ScmObj* %ae51212, %struct.ScmObj** %stackaddr$makeclosure54891, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51212, %struct.ScmObj* %k48311, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51212, %struct.ScmObj* %lst48098, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51212, %struct.ScmObj* %v48097, i64 2)
%ae51213 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54893 = alloca %struct.ScmObj*, align 8
%fptrToInt54894 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51214 to i64
%ae51214 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54894)
store volatile %struct.ScmObj* %ae51214, %struct.ScmObj** %stackaddr$makeclosure54893, align 8
%argslist54013$ae512120 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54895 = alloca %struct.ScmObj*, align 8
%argslist54013$ae512121 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51214, %struct.ScmObj* %argslist54013$ae512120)
store volatile %struct.ScmObj* %argslist54013$ae512121, %struct.ScmObj** %stackaddr$prim54895, align 8
%stackaddr$prim54896 = alloca %struct.ScmObj*, align 8
%argslist54013$ae512122 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51213, %struct.ScmObj* %argslist54013$ae512121)
store volatile %struct.ScmObj* %argslist54013$ae512122, %struct.ScmObj** %stackaddr$prim54896, align 8
%clofunc54897 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae51212)
musttail call tailcc void %clofunc54897(%struct.ScmObj* %ae51212, %struct.ScmObj* %argslist54013$ae512122)
ret void
}

define tailcc void @proc_clo$ae51212(%struct.ScmObj* %env$ae51212,%struct.ScmObj* %current_45args53993) {
%stackaddr$env-ref54898 = alloca %struct.ScmObj*, align 8
%k48311 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51212, i64 0)
store %struct.ScmObj* %k48311, %struct.ScmObj** %stackaddr$env-ref54898
%stackaddr$env-ref54899 = alloca %struct.ScmObj*, align 8
%lst48098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51212, i64 1)
store %struct.ScmObj* %lst48098, %struct.ScmObj** %stackaddr$env-ref54899
%stackaddr$env-ref54900 = alloca %struct.ScmObj*, align 8
%v48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51212, i64 2)
store %struct.ScmObj* %v48097, %struct.ScmObj** %stackaddr$env-ref54900
%stackaddr$prim54901 = alloca %struct.ScmObj*, align 8
%_95k48312 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53993)
store volatile %struct.ScmObj* %_95k48312, %struct.ScmObj** %stackaddr$prim54901, align 8
%stackaddr$prim54902 = alloca %struct.ScmObj*, align 8
%current_45args53994 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53993)
store volatile %struct.ScmObj* %current_45args53994, %struct.ScmObj** %stackaddr$prim54902, align 8
%stackaddr$prim54903 = alloca %struct.ScmObj*, align 8
%anf_45bind48239 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53994)
store volatile %struct.ScmObj* %anf_45bind48239, %struct.ScmObj** %stackaddr$prim54903, align 8
%stackaddr$makeclosure54904 = alloca %struct.ScmObj*, align 8
%fptrToInt54905 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51228 to i64
%ae51228 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54905)
store volatile %struct.ScmObj* %ae51228, %struct.ScmObj** %stackaddr$makeclosure54904, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51228, %struct.ScmObj* %k48311, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51228, %struct.ScmObj* %lst48098, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51228, %struct.ScmObj* %v48097, i64 2)
%stackaddr$makeclosure54906 = alloca %struct.ScmObj*, align 8
%fptrToInt54907 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae51229 to i64
%ae51229 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54907)
store volatile %struct.ScmObj* %ae51229, %struct.ScmObj** %stackaddr$makeclosure54906, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae51229, %struct.ScmObj* %k48311, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae51229, %struct.ScmObj* %lst48098, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae51229, %struct.ScmObj* %v48097, i64 2)
%argslist54008$anf_45bind482390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54908 = alloca %struct.ScmObj*, align 8
%argslist54008$anf_45bind482391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51229, %struct.ScmObj* %argslist54008$anf_45bind482390)
store volatile %struct.ScmObj* %argslist54008$anf_45bind482391, %struct.ScmObj** %stackaddr$prim54908, align 8
%stackaddr$prim54909 = alloca %struct.ScmObj*, align 8
%argslist54008$anf_45bind482392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51228, %struct.ScmObj* %argslist54008$anf_45bind482391)
store volatile %struct.ScmObj* %argslist54008$anf_45bind482392, %struct.ScmObj** %stackaddr$prim54909, align 8
%clofunc54910 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48239)
musttail call tailcc void %clofunc54910(%struct.ScmObj* %anf_45bind48239, %struct.ScmObj* %argslist54008$anf_45bind482392)
ret void
}

define tailcc void @proc_clo$ae51228(%struct.ScmObj* %env$ae51228,%struct.ScmObj* %current_45args53996) {
%stackaddr$env-ref54911 = alloca %struct.ScmObj*, align 8
%k48311 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51228, i64 0)
store %struct.ScmObj* %k48311, %struct.ScmObj** %stackaddr$env-ref54911
%stackaddr$env-ref54912 = alloca %struct.ScmObj*, align 8
%lst48098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51228, i64 1)
store %struct.ScmObj* %lst48098, %struct.ScmObj** %stackaddr$env-ref54912
%stackaddr$env-ref54913 = alloca %struct.ScmObj*, align 8
%v48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51228, i64 2)
store %struct.ScmObj* %v48097, %struct.ScmObj** %stackaddr$env-ref54913
%stackaddr$prim54914 = alloca %struct.ScmObj*, align 8
%_95k48313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53996)
store volatile %struct.ScmObj* %_95k48313, %struct.ScmObj** %stackaddr$prim54914, align 8
%stackaddr$prim54915 = alloca %struct.ScmObj*, align 8
%current_45args53997 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args53996)
store volatile %struct.ScmObj* %current_45args53997, %struct.ScmObj** %stackaddr$prim54915, align 8
%stackaddr$prim54916 = alloca %struct.ScmObj*, align 8
%cc48099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args53997)
store volatile %struct.ScmObj* %cc48099, %struct.ScmObj** %stackaddr$prim54916, align 8
%ae51337 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54917 = alloca %struct.ScmObj*, align 8
%anf_45bind48240 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51337)
store volatile %struct.ScmObj* %anf_45bind48240, %struct.ScmObj** %stackaddr$prim54917, align 8
%stackaddr$prim54918 = alloca %struct.ScmObj*, align 8
%anf_45bind48241 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48240)
store volatile %struct.ScmObj* %anf_45bind48241, %struct.ScmObj** %stackaddr$prim54918, align 8
%truthy$cmp54919 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48241)
%cmp$cmp54919 = icmp eq i64 %truthy$cmp54919, 1
br i1 %cmp$cmp54919, label %truebranch$cmp54919, label %falsebranch$cmp54919
truebranch$cmp54919:
%ae51341 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51342 = call %struct.ScmObj* @const_init_false()
%argslist53999$k483110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54920 = alloca %struct.ScmObj*, align 8
%argslist53999$k483111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51342, %struct.ScmObj* %argslist53999$k483110)
store volatile %struct.ScmObj* %argslist53999$k483111, %struct.ScmObj** %stackaddr$prim54920, align 8
%stackaddr$prim54921 = alloca %struct.ScmObj*, align 8
%argslist53999$k483112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51341, %struct.ScmObj* %argslist53999$k483111)
store volatile %struct.ScmObj* %argslist53999$k483112, %struct.ScmObj** %stackaddr$prim54921, align 8
%clofunc54922 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48311)
musttail call tailcc void %clofunc54922(%struct.ScmObj* %k48311, %struct.ScmObj* %argslist53999$k483112)
ret void
falsebranch$cmp54919:
%ae51350 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54923 = alloca %struct.ScmObj*, align 8
%anf_45bind48242 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51350)
store volatile %struct.ScmObj* %anf_45bind48242, %struct.ScmObj** %stackaddr$prim54923, align 8
%stackaddr$prim54924 = alloca %struct.ScmObj*, align 8
%anf_45bind48243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48242)
store volatile %struct.ScmObj* %anf_45bind48243, %struct.ScmObj** %stackaddr$prim54924, align 8
%stackaddr$prim54925 = alloca %struct.ScmObj*, align 8
%anf_45bind48244 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48243, %struct.ScmObj* %v48097)
store volatile %struct.ScmObj* %anf_45bind48244, %struct.ScmObj** %stackaddr$prim54925, align 8
%truthy$cmp54926 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48244)
%cmp$cmp54926 = icmp eq i64 %truthy$cmp54926, 1
br i1 %cmp$cmp54926, label %truebranch$cmp54926, label %falsebranch$cmp54926
truebranch$cmp54926:
%ae51356 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54927 = alloca %struct.ScmObj*, align 8
%cpsprim48314 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51356)
store volatile %struct.ScmObj* %cpsprim48314, %struct.ScmObj** %stackaddr$prim54927, align 8
%ae51358 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54000$k483110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54928 = alloca %struct.ScmObj*, align 8
%argslist54000$k483111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48314, %struct.ScmObj* %argslist54000$k483110)
store volatile %struct.ScmObj* %argslist54000$k483111, %struct.ScmObj** %stackaddr$prim54928, align 8
%stackaddr$prim54929 = alloca %struct.ScmObj*, align 8
%argslist54000$k483112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51358, %struct.ScmObj* %argslist54000$k483111)
store volatile %struct.ScmObj* %argslist54000$k483112, %struct.ScmObj** %stackaddr$prim54929, align 8
%clofunc54930 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48311)
musttail call tailcc void %clofunc54930(%struct.ScmObj* %k48311, %struct.ScmObj* %argslist54000$k483112)
ret void
falsebranch$cmp54926:
%ae51369 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54931 = alloca %struct.ScmObj*, align 8
%anf_45bind48245 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51369)
store volatile %struct.ScmObj* %anf_45bind48245, %struct.ScmObj** %stackaddr$prim54931, align 8
%stackaddr$prim54932 = alloca %struct.ScmObj*, align 8
%anf_45bind48246 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48245)
store volatile %struct.ScmObj* %anf_45bind48246, %struct.ScmObj** %stackaddr$prim54932, align 8
%ae51372 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54933 = alloca %struct.ScmObj*, align 8
%_95048101 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51372, %struct.ScmObj* %anf_45bind48246)
store volatile %struct.ScmObj* %_95048101, %struct.ScmObj** %stackaddr$prim54933, align 8
%argslist54001$cc480990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54934 = alloca %struct.ScmObj*, align 8
%argslist54001$cc480991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48099, %struct.ScmObj* %argslist54001$cc480990)
store volatile %struct.ScmObj* %argslist54001$cc480991, %struct.ScmObj** %stackaddr$prim54934, align 8
%stackaddr$prim54935 = alloca %struct.ScmObj*, align 8
%argslist54001$cc480992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48311, %struct.ScmObj* %argslist54001$cc480991)
store volatile %struct.ScmObj* %argslist54001$cc480992, %struct.ScmObj** %stackaddr$prim54935, align 8
%clofunc54936 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48099)
musttail call tailcc void %clofunc54936(%struct.ScmObj* %cc48099, %struct.ScmObj* %argslist54001$cc480992)
ret void
}

define tailcc void @proc_clo$ae51229(%struct.ScmObj* %env$ae51229,%struct.ScmObj* %current_45args54002) {
%stackaddr$env-ref54937 = alloca %struct.ScmObj*, align 8
%k48311 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51229, i64 0)
store %struct.ScmObj* %k48311, %struct.ScmObj** %stackaddr$env-ref54937
%stackaddr$env-ref54938 = alloca %struct.ScmObj*, align 8
%lst48098 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51229, i64 1)
store %struct.ScmObj* %lst48098, %struct.ScmObj** %stackaddr$env-ref54938
%stackaddr$env-ref54939 = alloca %struct.ScmObj*, align 8
%v48097 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae51229, i64 2)
store %struct.ScmObj* %v48097, %struct.ScmObj** %stackaddr$env-ref54939
%stackaddr$prim54940 = alloca %struct.ScmObj*, align 8
%_95k48313 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54002)
store volatile %struct.ScmObj* %_95k48313, %struct.ScmObj** %stackaddr$prim54940, align 8
%stackaddr$prim54941 = alloca %struct.ScmObj*, align 8
%current_45args54003 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54002)
store volatile %struct.ScmObj* %current_45args54003, %struct.ScmObj** %stackaddr$prim54941, align 8
%stackaddr$prim54942 = alloca %struct.ScmObj*, align 8
%cc48099 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54003)
store volatile %struct.ScmObj* %cc48099, %struct.ScmObj** %stackaddr$prim54942, align 8
%ae51231 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54943 = alloca %struct.ScmObj*, align 8
%anf_45bind48240 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51231)
store volatile %struct.ScmObj* %anf_45bind48240, %struct.ScmObj** %stackaddr$prim54943, align 8
%stackaddr$prim54944 = alloca %struct.ScmObj*, align 8
%anf_45bind48241 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48240)
store volatile %struct.ScmObj* %anf_45bind48241, %struct.ScmObj** %stackaddr$prim54944, align 8
%truthy$cmp54945 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48241)
%cmp$cmp54945 = icmp eq i64 %truthy$cmp54945, 1
br i1 %cmp$cmp54945, label %truebranch$cmp54945, label %falsebranch$cmp54945
truebranch$cmp54945:
%ae51235 = call %struct.ScmObj* @const_init_int(i64 0)
%ae51236 = call %struct.ScmObj* @const_init_false()
%argslist54005$k483110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54946 = alloca %struct.ScmObj*, align 8
%argslist54005$k483111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51236, %struct.ScmObj* %argslist54005$k483110)
store volatile %struct.ScmObj* %argslist54005$k483111, %struct.ScmObj** %stackaddr$prim54946, align 8
%stackaddr$prim54947 = alloca %struct.ScmObj*, align 8
%argslist54005$k483112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51235, %struct.ScmObj* %argslist54005$k483111)
store volatile %struct.ScmObj* %argslist54005$k483112, %struct.ScmObj** %stackaddr$prim54947, align 8
%clofunc54948 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48311)
musttail call tailcc void %clofunc54948(%struct.ScmObj* %k48311, %struct.ScmObj* %argslist54005$k483112)
ret void
falsebranch$cmp54945:
%ae51244 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54949 = alloca %struct.ScmObj*, align 8
%anf_45bind48242 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51244)
store volatile %struct.ScmObj* %anf_45bind48242, %struct.ScmObj** %stackaddr$prim54949, align 8
%stackaddr$prim54950 = alloca %struct.ScmObj*, align 8
%anf_45bind48243 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48242)
store volatile %struct.ScmObj* %anf_45bind48243, %struct.ScmObj** %stackaddr$prim54950, align 8
%stackaddr$prim54951 = alloca %struct.ScmObj*, align 8
%anf_45bind48244 = call %struct.ScmObj* @prim_eqv_63(%struct.ScmObj* %anf_45bind48243, %struct.ScmObj* %v48097)
store volatile %struct.ScmObj* %anf_45bind48244, %struct.ScmObj** %stackaddr$prim54951, align 8
%truthy$cmp54952 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48244)
%cmp$cmp54952 = icmp eq i64 %truthy$cmp54952, 1
br i1 %cmp$cmp54952, label %truebranch$cmp54952, label %falsebranch$cmp54952
truebranch$cmp54952:
%ae51250 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54953 = alloca %struct.ScmObj*, align 8
%cpsprim48314 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51250)
store volatile %struct.ScmObj* %cpsprim48314, %struct.ScmObj** %stackaddr$prim54953, align 8
%ae51252 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54006$k483110 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54954 = alloca %struct.ScmObj*, align 8
%argslist54006$k483111 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48314, %struct.ScmObj* %argslist54006$k483110)
store volatile %struct.ScmObj* %argslist54006$k483111, %struct.ScmObj** %stackaddr$prim54954, align 8
%stackaddr$prim54955 = alloca %struct.ScmObj*, align 8
%argslist54006$k483112 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae51252, %struct.ScmObj* %argslist54006$k483111)
store volatile %struct.ScmObj* %argslist54006$k483112, %struct.ScmObj** %stackaddr$prim54955, align 8
%clofunc54956 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48311)
musttail call tailcc void %clofunc54956(%struct.ScmObj* %k48311, %struct.ScmObj* %argslist54006$k483112)
ret void
falsebranch$cmp54952:
%ae51263 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54957 = alloca %struct.ScmObj*, align 8
%anf_45bind48245 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51263)
store volatile %struct.ScmObj* %anf_45bind48245, %struct.ScmObj** %stackaddr$prim54957, align 8
%stackaddr$prim54958 = alloca %struct.ScmObj*, align 8
%anf_45bind48246 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48245)
store volatile %struct.ScmObj* %anf_45bind48246, %struct.ScmObj** %stackaddr$prim54958, align 8
%ae51266 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim54959 = alloca %struct.ScmObj*, align 8
%_95048101 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48098, %struct.ScmObj* %ae51266, %struct.ScmObj* %anf_45bind48246)
store volatile %struct.ScmObj* %_95048101, %struct.ScmObj** %stackaddr$prim54959, align 8
%argslist54007$cc480990 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54960 = alloca %struct.ScmObj*, align 8
%argslist54007$cc480991 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48099, %struct.ScmObj* %argslist54007$cc480990)
store volatile %struct.ScmObj* %argslist54007$cc480991, %struct.ScmObj** %stackaddr$prim54960, align 8
%stackaddr$prim54961 = alloca %struct.ScmObj*, align 8
%argslist54007$cc480992 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48311, %struct.ScmObj* %argslist54007$cc480991)
store volatile %struct.ScmObj* %argslist54007$cc480992, %struct.ScmObj** %stackaddr$prim54961, align 8
%clofunc54962 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48099)
musttail call tailcc void %clofunc54962(%struct.ScmObj* %cc48099, %struct.ScmObj* %argslist54007$cc480992)
ret void
}

define tailcc void @proc_clo$ae51214(%struct.ScmObj* %env$ae51214,%struct.ScmObj* %current_45args54009) {
%stackaddr$prim54963 = alloca %struct.ScmObj*, align 8
%k48315 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54009)
store volatile %struct.ScmObj* %k48315, %struct.ScmObj** %stackaddr$prim54963, align 8
%stackaddr$prim54964 = alloca %struct.ScmObj*, align 8
%current_45args54010 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54009)
store volatile %struct.ScmObj* %current_45args54010, %struct.ScmObj** %stackaddr$prim54964, align 8
%stackaddr$prim54965 = alloca %struct.ScmObj*, align 8
%u48100 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54010)
store volatile %struct.ScmObj* %u48100, %struct.ScmObj** %stackaddr$prim54965, align 8
%argslist54012$u481000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54966 = alloca %struct.ScmObj*, align 8
%argslist54012$u481001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48100, %struct.ScmObj* %argslist54012$u481000)
store volatile %struct.ScmObj* %argslist54012$u481001, %struct.ScmObj** %stackaddr$prim54966, align 8
%stackaddr$prim54967 = alloca %struct.ScmObj*, align 8
%argslist54012$u481002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48315, %struct.ScmObj* %argslist54012$u481001)
store volatile %struct.ScmObj* %argslist54012$u481002, %struct.ScmObj** %stackaddr$prim54967, align 8
%clofunc54968 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48100)
musttail call tailcc void %clofunc54968(%struct.ScmObj* %u48100, %struct.ScmObj* %argslist54012$u481002)
ret void
}

define tailcc void @proc_clo$ae50673(%struct.ScmObj* %env$ae50673,%struct.ScmObj* %current_45args54015) {
%stackaddr$prim54969 = alloca %struct.ScmObj*, align 8
%k48316 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54015)
store volatile %struct.ScmObj* %k48316, %struct.ScmObj** %stackaddr$prim54969, align 8
%stackaddr$prim54970 = alloca %struct.ScmObj*, align 8
%current_45args54016 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54015)
store volatile %struct.ScmObj* %current_45args54016, %struct.ScmObj** %stackaddr$prim54970, align 8
%stackaddr$prim54971 = alloca %struct.ScmObj*, align 8
%lst48104 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54016)
store volatile %struct.ScmObj* %lst48104, %struct.ScmObj** %stackaddr$prim54971, align 8
%stackaddr$prim54972 = alloca %struct.ScmObj*, align 8
%current_45args54017 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54016)
store volatile %struct.ScmObj* %current_45args54017, %struct.ScmObj** %stackaddr$prim54972, align 8
%stackaddr$prim54973 = alloca %struct.ScmObj*, align 8
%n48103 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54017)
store volatile %struct.ScmObj* %n48103, %struct.ScmObj** %stackaddr$prim54973, align 8
%ae50674 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54974 = alloca %struct.ScmObj*, align 8
%n48106 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50674, %struct.ScmObj* %n48103)
store volatile %struct.ScmObj* %n48106, %struct.ScmObj** %stackaddr$prim54974, align 8
%ae50676 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim54975 = alloca %struct.ScmObj*, align 8
%lst48105 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50676, %struct.ScmObj* %lst48104)
store volatile %struct.ScmObj* %lst48105, %struct.ScmObj** %stackaddr$prim54975, align 8
%stackaddr$makeclosure54976 = alloca %struct.ScmObj*, align 8
%fptrToInt54977 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50678 to i64
%ae50678 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54977)
store volatile %struct.ScmObj* %ae50678, %struct.ScmObj** %stackaddr$makeclosure54976, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50678, %struct.ScmObj* %lst48105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50678, %struct.ScmObj* %k48316, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50678, %struct.ScmObj* %n48106, i64 2)
%ae50679 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure54978 = alloca %struct.ScmObj*, align 8
%fptrToInt54979 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50680 to i64
%ae50680 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt54979)
store volatile %struct.ScmObj* %ae50680, %struct.ScmObj** %stackaddr$makeclosure54978, align 8
%argslist54037$ae506780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54980 = alloca %struct.ScmObj*, align 8
%argslist54037$ae506781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50680, %struct.ScmObj* %argslist54037$ae506780)
store volatile %struct.ScmObj* %argslist54037$ae506781, %struct.ScmObj** %stackaddr$prim54980, align 8
%stackaddr$prim54981 = alloca %struct.ScmObj*, align 8
%argslist54037$ae506782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50679, %struct.ScmObj* %argslist54037$ae506781)
store volatile %struct.ScmObj* %argslist54037$ae506782, %struct.ScmObj** %stackaddr$prim54981, align 8
%clofunc54982 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50678)
musttail call tailcc void %clofunc54982(%struct.ScmObj* %ae50678, %struct.ScmObj* %argslist54037$ae506782)
ret void
}

define tailcc void @proc_clo$ae50678(%struct.ScmObj* %env$ae50678,%struct.ScmObj* %current_45args54019) {
%stackaddr$env-ref54983 = alloca %struct.ScmObj*, align 8
%lst48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50678, i64 0)
store %struct.ScmObj* %lst48105, %struct.ScmObj** %stackaddr$env-ref54983
%stackaddr$env-ref54984 = alloca %struct.ScmObj*, align 8
%k48316 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50678, i64 1)
store %struct.ScmObj* %k48316, %struct.ScmObj** %stackaddr$env-ref54984
%stackaddr$env-ref54985 = alloca %struct.ScmObj*, align 8
%n48106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50678, i64 2)
store %struct.ScmObj* %n48106, %struct.ScmObj** %stackaddr$env-ref54985
%stackaddr$prim54986 = alloca %struct.ScmObj*, align 8
%_95k48317 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54019)
store volatile %struct.ScmObj* %_95k48317, %struct.ScmObj** %stackaddr$prim54986, align 8
%stackaddr$prim54987 = alloca %struct.ScmObj*, align 8
%current_45args54020 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54019)
store volatile %struct.ScmObj* %current_45args54020, %struct.ScmObj** %stackaddr$prim54987, align 8
%stackaddr$prim54988 = alloca %struct.ScmObj*, align 8
%anf_45bind48232 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54020)
store volatile %struct.ScmObj* %anf_45bind48232, %struct.ScmObj** %stackaddr$prim54988, align 8
%stackaddr$makeclosure54989 = alloca %struct.ScmObj*, align 8
%fptrToInt54990 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50694 to i64
%ae50694 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54990)
store volatile %struct.ScmObj* %ae50694, %struct.ScmObj** %stackaddr$makeclosure54989, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50694, %struct.ScmObj* %lst48105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50694, %struct.ScmObj* %k48316, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50694, %struct.ScmObj* %n48106, i64 2)
%stackaddr$makeclosure54991 = alloca %struct.ScmObj*, align 8
%fptrToInt54992 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50695 to i64
%ae50695 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt54992)
store volatile %struct.ScmObj* %ae50695, %struct.ScmObj** %stackaddr$makeclosure54991, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50695, %struct.ScmObj* %lst48105, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50695, %struct.ScmObj* %k48316, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae50695, %struct.ScmObj* %n48106, i64 2)
%argslist54032$anf_45bind482320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim54993 = alloca %struct.ScmObj*, align 8
%argslist54032$anf_45bind482321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50695, %struct.ScmObj* %argslist54032$anf_45bind482320)
store volatile %struct.ScmObj* %argslist54032$anf_45bind482321, %struct.ScmObj** %stackaddr$prim54993, align 8
%stackaddr$prim54994 = alloca %struct.ScmObj*, align 8
%argslist54032$anf_45bind482322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50694, %struct.ScmObj* %argslist54032$anf_45bind482321)
store volatile %struct.ScmObj* %argslist54032$anf_45bind482322, %struct.ScmObj** %stackaddr$prim54994, align 8
%clofunc54995 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48232)
musttail call tailcc void %clofunc54995(%struct.ScmObj* %anf_45bind48232, %struct.ScmObj* %argslist54032$anf_45bind482322)
ret void
}

define tailcc void @proc_clo$ae50694(%struct.ScmObj* %env$ae50694,%struct.ScmObj* %current_45args54022) {
%stackaddr$env-ref54996 = alloca %struct.ScmObj*, align 8
%lst48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50694, i64 0)
store %struct.ScmObj* %lst48105, %struct.ScmObj** %stackaddr$env-ref54996
%stackaddr$env-ref54997 = alloca %struct.ScmObj*, align 8
%k48316 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50694, i64 1)
store %struct.ScmObj* %k48316, %struct.ScmObj** %stackaddr$env-ref54997
%stackaddr$env-ref54998 = alloca %struct.ScmObj*, align 8
%n48106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50694, i64 2)
store %struct.ScmObj* %n48106, %struct.ScmObj** %stackaddr$env-ref54998
%stackaddr$prim54999 = alloca %struct.ScmObj*, align 8
%_95k48318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54022)
store volatile %struct.ScmObj* %_95k48318, %struct.ScmObj** %stackaddr$prim54999, align 8
%stackaddr$prim55000 = alloca %struct.ScmObj*, align 8
%current_45args54023 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54022)
store volatile %struct.ScmObj* %current_45args54023, %struct.ScmObj** %stackaddr$prim55000, align 8
%stackaddr$prim55001 = alloca %struct.ScmObj*, align 8
%cc48107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54023)
store volatile %struct.ScmObj* %cc48107, %struct.ScmObj** %stackaddr$prim55001, align 8
%ae50837 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55002 = alloca %struct.ScmObj*, align 8
%anf_45bind48233 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48106, %struct.ScmObj* %ae50837)
store volatile %struct.ScmObj* %anf_45bind48233, %struct.ScmObj** %stackaddr$prim55002, align 8
%ae50838 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55003 = alloca %struct.ScmObj*, align 8
%anf_45bind48234 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50838, %struct.ScmObj* %anf_45bind48233)
store volatile %struct.ScmObj* %anf_45bind48234, %struct.ScmObj** %stackaddr$prim55003, align 8
%truthy$cmp55004 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48234)
%cmp$cmp55004 = icmp eq i64 %truthy$cmp55004, 1
br i1 %cmp$cmp55004, label %truebranch$cmp55004, label %falsebranch$cmp55004
truebranch$cmp55004:
%ae50842 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55005 = alloca %struct.ScmObj*, align 8
%cpsprim48319 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae50842)
store volatile %struct.ScmObj* %cpsprim48319, %struct.ScmObj** %stackaddr$prim55005, align 8
%ae50844 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54025$k483160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55006 = alloca %struct.ScmObj*, align 8
%argslist54025$k483161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48319, %struct.ScmObj* %argslist54025$k483160)
store volatile %struct.ScmObj* %argslist54025$k483161, %struct.ScmObj** %stackaddr$prim55006, align 8
%stackaddr$prim55007 = alloca %struct.ScmObj*, align 8
%argslist54025$k483162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50844, %struct.ScmObj* %argslist54025$k483161)
store volatile %struct.ScmObj* %argslist54025$k483162, %struct.ScmObj** %stackaddr$prim55007, align 8
%clofunc55008 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48316)
musttail call tailcc void %clofunc55008(%struct.ScmObj* %k48316, %struct.ScmObj* %argslist54025$k483162)
ret void
falsebranch$cmp55004:
%ae50855 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55009 = alloca %struct.ScmObj*, align 8
%anf_45bind48235 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae50855)
store volatile %struct.ScmObj* %anf_45bind48235, %struct.ScmObj** %stackaddr$prim55009, align 8
%stackaddr$prim55010 = alloca %struct.ScmObj*, align 8
%anf_45bind48236 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48235)
store volatile %struct.ScmObj* %anf_45bind48236, %struct.ScmObj** %stackaddr$prim55010, align 8
%ae50858 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55011 = alloca %struct.ScmObj*, align 8
%_95048110 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae50858, %struct.ScmObj* %anf_45bind48236)
store volatile %struct.ScmObj* %_95048110, %struct.ScmObj** %stackaddr$prim55011, align 8
%ae50861 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55012 = alloca %struct.ScmObj*, align 8
%anf_45bind48237 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48106, %struct.ScmObj* %ae50861)
store volatile %struct.ScmObj* %anf_45bind48237, %struct.ScmObj** %stackaddr$prim55012, align 8
%ae50863 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55013 = alloca %struct.ScmObj*, align 8
%anf_45bind48238 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48237, %struct.ScmObj* %ae50863)
store volatile %struct.ScmObj* %anf_45bind48238, %struct.ScmObj** %stackaddr$prim55013, align 8
%ae50865 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55014 = alloca %struct.ScmObj*, align 8
%_95148109 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48106, %struct.ScmObj* %ae50865, %struct.ScmObj* %anf_45bind48238)
store volatile %struct.ScmObj* %_95148109, %struct.ScmObj** %stackaddr$prim55014, align 8
%argslist54026$cc481070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55015 = alloca %struct.ScmObj*, align 8
%argslist54026$cc481071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48107, %struct.ScmObj* %argslist54026$cc481070)
store volatile %struct.ScmObj* %argslist54026$cc481071, %struct.ScmObj** %stackaddr$prim55015, align 8
%stackaddr$prim55016 = alloca %struct.ScmObj*, align 8
%argslist54026$cc481072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48316, %struct.ScmObj* %argslist54026$cc481071)
store volatile %struct.ScmObj* %argslist54026$cc481072, %struct.ScmObj** %stackaddr$prim55016, align 8
%clofunc55017 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48107)
musttail call tailcc void %clofunc55017(%struct.ScmObj* %cc48107, %struct.ScmObj* %argslist54026$cc481072)
ret void
}

define tailcc void @proc_clo$ae50695(%struct.ScmObj* %env$ae50695,%struct.ScmObj* %current_45args54027) {
%stackaddr$env-ref55018 = alloca %struct.ScmObj*, align 8
%lst48105 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50695, i64 0)
store %struct.ScmObj* %lst48105, %struct.ScmObj** %stackaddr$env-ref55018
%stackaddr$env-ref55019 = alloca %struct.ScmObj*, align 8
%k48316 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50695, i64 1)
store %struct.ScmObj* %k48316, %struct.ScmObj** %stackaddr$env-ref55019
%stackaddr$env-ref55020 = alloca %struct.ScmObj*, align 8
%n48106 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50695, i64 2)
store %struct.ScmObj* %n48106, %struct.ScmObj** %stackaddr$env-ref55020
%stackaddr$prim55021 = alloca %struct.ScmObj*, align 8
%_95k48318 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54027)
store volatile %struct.ScmObj* %_95k48318, %struct.ScmObj** %stackaddr$prim55021, align 8
%stackaddr$prim55022 = alloca %struct.ScmObj*, align 8
%current_45args54028 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54027)
store volatile %struct.ScmObj* %current_45args54028, %struct.ScmObj** %stackaddr$prim55022, align 8
%stackaddr$prim55023 = alloca %struct.ScmObj*, align 8
%cc48107 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54028)
store volatile %struct.ScmObj* %cc48107, %struct.ScmObj** %stackaddr$prim55023, align 8
%ae50697 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55024 = alloca %struct.ScmObj*, align 8
%anf_45bind48233 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48106, %struct.ScmObj* %ae50697)
store volatile %struct.ScmObj* %anf_45bind48233, %struct.ScmObj** %stackaddr$prim55024, align 8
%ae50698 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55025 = alloca %struct.ScmObj*, align 8
%anf_45bind48234 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %ae50698, %struct.ScmObj* %anf_45bind48233)
store volatile %struct.ScmObj* %anf_45bind48234, %struct.ScmObj** %stackaddr$prim55025, align 8
%truthy$cmp55026 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48234)
%cmp$cmp55026 = icmp eq i64 %truthy$cmp55026, 1
br i1 %cmp$cmp55026, label %truebranch$cmp55026, label %falsebranch$cmp55026
truebranch$cmp55026:
%ae50702 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55027 = alloca %struct.ScmObj*, align 8
%cpsprim48319 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae50702)
store volatile %struct.ScmObj* %cpsprim48319, %struct.ScmObj** %stackaddr$prim55027, align 8
%ae50704 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54030$k483160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55028 = alloca %struct.ScmObj*, align 8
%argslist54030$k483161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48319, %struct.ScmObj* %argslist54030$k483160)
store volatile %struct.ScmObj* %argslist54030$k483161, %struct.ScmObj** %stackaddr$prim55028, align 8
%stackaddr$prim55029 = alloca %struct.ScmObj*, align 8
%argslist54030$k483162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50704, %struct.ScmObj* %argslist54030$k483161)
store volatile %struct.ScmObj* %argslist54030$k483162, %struct.ScmObj** %stackaddr$prim55029, align 8
%clofunc55030 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48316)
musttail call tailcc void %clofunc55030(%struct.ScmObj* %k48316, %struct.ScmObj* %argslist54030$k483162)
ret void
falsebranch$cmp55026:
%ae50715 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55031 = alloca %struct.ScmObj*, align 8
%anf_45bind48235 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae50715)
store volatile %struct.ScmObj* %anf_45bind48235, %struct.ScmObj** %stackaddr$prim55031, align 8
%stackaddr$prim55032 = alloca %struct.ScmObj*, align 8
%anf_45bind48236 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48235)
store volatile %struct.ScmObj* %anf_45bind48236, %struct.ScmObj** %stackaddr$prim55032, align 8
%ae50718 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55033 = alloca %struct.ScmObj*, align 8
%_95048110 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %lst48105, %struct.ScmObj* %ae50718, %struct.ScmObj* %anf_45bind48236)
store volatile %struct.ScmObj* %_95048110, %struct.ScmObj** %stackaddr$prim55033, align 8
%ae50721 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55034 = alloca %struct.ScmObj*, align 8
%anf_45bind48237 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %n48106, %struct.ScmObj* %ae50721)
store volatile %struct.ScmObj* %anf_45bind48237, %struct.ScmObj** %stackaddr$prim55034, align 8
%ae50723 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55035 = alloca %struct.ScmObj*, align 8
%anf_45bind48238 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48237, %struct.ScmObj* %ae50723)
store volatile %struct.ScmObj* %anf_45bind48238, %struct.ScmObj** %stackaddr$prim55035, align 8
%ae50725 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55036 = alloca %struct.ScmObj*, align 8
%_95148109 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %n48106, %struct.ScmObj* %ae50725, %struct.ScmObj* %anf_45bind48238)
store volatile %struct.ScmObj* %_95148109, %struct.ScmObj** %stackaddr$prim55036, align 8
%argslist54031$cc481070 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55037 = alloca %struct.ScmObj*, align 8
%argslist54031$cc481071 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48107, %struct.ScmObj* %argslist54031$cc481070)
store volatile %struct.ScmObj* %argslist54031$cc481071, %struct.ScmObj** %stackaddr$prim55037, align 8
%stackaddr$prim55038 = alloca %struct.ScmObj*, align 8
%argslist54031$cc481072 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48316, %struct.ScmObj* %argslist54031$cc481071)
store volatile %struct.ScmObj* %argslist54031$cc481072, %struct.ScmObj** %stackaddr$prim55038, align 8
%clofunc55039 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48107)
musttail call tailcc void %clofunc55039(%struct.ScmObj* %cc48107, %struct.ScmObj* %argslist54031$cc481072)
ret void
}

define tailcc void @proc_clo$ae50680(%struct.ScmObj* %env$ae50680,%struct.ScmObj* %current_45args54033) {
%stackaddr$prim55040 = alloca %struct.ScmObj*, align 8
%k48320 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54033)
store volatile %struct.ScmObj* %k48320, %struct.ScmObj** %stackaddr$prim55040, align 8
%stackaddr$prim55041 = alloca %struct.ScmObj*, align 8
%current_45args54034 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54033)
store volatile %struct.ScmObj* %current_45args54034, %struct.ScmObj** %stackaddr$prim55041, align 8
%stackaddr$prim55042 = alloca %struct.ScmObj*, align 8
%u48108 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54034)
store volatile %struct.ScmObj* %u48108, %struct.ScmObj** %stackaddr$prim55042, align 8
%argslist54036$u481080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55043 = alloca %struct.ScmObj*, align 8
%argslist54036$u481081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %u48108, %struct.ScmObj* %argslist54036$u481080)
store volatile %struct.ScmObj* %argslist54036$u481081, %struct.ScmObj** %stackaddr$prim55043, align 8
%stackaddr$prim55044 = alloca %struct.ScmObj*, align 8
%argslist54036$u481082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48320, %struct.ScmObj* %argslist54036$u481081)
store volatile %struct.ScmObj* %argslist54036$u481082, %struct.ScmObj** %stackaddr$prim55044, align 8
%clofunc55045 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %u48108)
musttail call tailcc void %clofunc55045(%struct.ScmObj* %u48108, %struct.ScmObj* %argslist54036$u481082)
ret void
}

define tailcc void @proc_clo$ae50257(%struct.ScmObj* %env$ae50257,%struct.ScmObj* %current_45args54039) {
%stackaddr$prim55046 = alloca %struct.ScmObj*, align 8
%k48321 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54039)
store volatile %struct.ScmObj* %k48321, %struct.ScmObj** %stackaddr$prim55046, align 8
%stackaddr$prim55047 = alloca %struct.ScmObj*, align 8
%current_45args54040 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54039)
store volatile %struct.ScmObj* %current_45args54040, %struct.ScmObj** %stackaddr$prim55047, align 8
%stackaddr$prim55048 = alloca %struct.ScmObj*, align 8
%a48112 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54040)
store volatile %struct.ScmObj* %a48112, %struct.ScmObj** %stackaddr$prim55048, align 8
%ae50258 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55049 = alloca %struct.ScmObj*, align 8
%a48113 = call %struct.ScmObj* @prim_make_45vector(%struct.ScmObj* %ae50258, %struct.ScmObj* %a48112)
store volatile %struct.ScmObj* %a48113, %struct.ScmObj** %stackaddr$prim55049, align 8
%stackaddr$makeclosure55050 = alloca %struct.ScmObj*, align 8
%fptrToInt55051 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50260 to i64
%ae50260 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55051)
store volatile %struct.ScmObj* %ae50260, %struct.ScmObj** %stackaddr$makeclosure55050, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50260, %struct.ScmObj* %a48113, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50260, %struct.ScmObj* %k48321, i64 1)
%ae50261 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55052 = alloca %struct.ScmObj*, align 8
%fptrToInt55053 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50262 to i64
%ae50262 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55053)
store volatile %struct.ScmObj* %ae50262, %struct.ScmObj** %stackaddr$makeclosure55052, align 8
%argslist54062$ae502600 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55054 = alloca %struct.ScmObj*, align 8
%argslist54062$ae502601 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50262, %struct.ScmObj* %argslist54062$ae502600)
store volatile %struct.ScmObj* %argslist54062$ae502601, %struct.ScmObj** %stackaddr$prim55054, align 8
%stackaddr$prim55055 = alloca %struct.ScmObj*, align 8
%argslist54062$ae502602 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50261, %struct.ScmObj* %argslist54062$ae502601)
store volatile %struct.ScmObj* %argslist54062$ae502602, %struct.ScmObj** %stackaddr$prim55055, align 8
%clofunc55056 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae50260)
musttail call tailcc void %clofunc55056(%struct.ScmObj* %ae50260, %struct.ScmObj* %argslist54062$ae502602)
ret void
}

define tailcc void @proc_clo$ae50260(%struct.ScmObj* %env$ae50260,%struct.ScmObj* %current_45args54042) {
%stackaddr$env-ref55057 = alloca %struct.ScmObj*, align 8
%a48113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50260, i64 0)
store %struct.ScmObj* %a48113, %struct.ScmObj** %stackaddr$env-ref55057
%stackaddr$env-ref55058 = alloca %struct.ScmObj*, align 8
%k48321 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50260, i64 1)
store %struct.ScmObj* %k48321, %struct.ScmObj** %stackaddr$env-ref55058
%stackaddr$prim55059 = alloca %struct.ScmObj*, align 8
%_95k48322 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54042)
store volatile %struct.ScmObj* %_95k48322, %struct.ScmObj** %stackaddr$prim55059, align 8
%stackaddr$prim55060 = alloca %struct.ScmObj*, align 8
%current_45args54043 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54042)
store volatile %struct.ScmObj* %current_45args54043, %struct.ScmObj** %stackaddr$prim55060, align 8
%stackaddr$prim55061 = alloca %struct.ScmObj*, align 8
%anf_45bind48224 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54043)
store volatile %struct.ScmObj* %anf_45bind48224, %struct.ScmObj** %stackaddr$prim55061, align 8
%stackaddr$makeclosure55062 = alloca %struct.ScmObj*, align 8
%fptrToInt55063 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50279 to i64
%ae50279 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55063)
store volatile %struct.ScmObj* %ae50279, %struct.ScmObj** %stackaddr$makeclosure55062, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50279, %struct.ScmObj* %a48113, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50279, %struct.ScmObj* %k48321, i64 1)
%stackaddr$makeclosure55064 = alloca %struct.ScmObj*, align 8
%fptrToInt55065 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50280 to i64
%ae50280 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55065)
store volatile %struct.ScmObj* %ae50280, %struct.ScmObj** %stackaddr$makeclosure55064, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50280, %struct.ScmObj* %a48113, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50280, %struct.ScmObj* %k48321, i64 1)
%argslist54057$anf_45bind482240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55066 = alloca %struct.ScmObj*, align 8
%argslist54057$anf_45bind482241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50280, %struct.ScmObj* %argslist54057$anf_45bind482240)
store volatile %struct.ScmObj* %argslist54057$anf_45bind482241, %struct.ScmObj** %stackaddr$prim55066, align 8
%stackaddr$prim55067 = alloca %struct.ScmObj*, align 8
%argslist54057$anf_45bind482242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50279, %struct.ScmObj* %argslist54057$anf_45bind482241)
store volatile %struct.ScmObj* %argslist54057$anf_45bind482242, %struct.ScmObj** %stackaddr$prim55067, align 8
%clofunc55068 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48224)
musttail call tailcc void %clofunc55068(%struct.ScmObj* %anf_45bind48224, %struct.ScmObj* %argslist54057$anf_45bind482242)
ret void
}

define tailcc void @proc_clo$ae50279(%struct.ScmObj* %env$ae50279,%struct.ScmObj* %current_45args54045) {
%stackaddr$env-ref55069 = alloca %struct.ScmObj*, align 8
%a48113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50279, i64 0)
store %struct.ScmObj* %a48113, %struct.ScmObj** %stackaddr$env-ref55069
%stackaddr$env-ref55070 = alloca %struct.ScmObj*, align 8
%k48321 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50279, i64 1)
store %struct.ScmObj* %k48321, %struct.ScmObj** %stackaddr$env-ref55070
%stackaddr$prim55071 = alloca %struct.ScmObj*, align 8
%_95k48323 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54045)
store volatile %struct.ScmObj* %_95k48323, %struct.ScmObj** %stackaddr$prim55071, align 8
%stackaddr$prim55072 = alloca %struct.ScmObj*, align 8
%current_45args54046 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54045)
store volatile %struct.ScmObj* %current_45args54046, %struct.ScmObj** %stackaddr$prim55072, align 8
%stackaddr$prim55073 = alloca %struct.ScmObj*, align 8
%cc48114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54046)
store volatile %struct.ScmObj* %cc48114, %struct.ScmObj** %stackaddr$prim55073, align 8
%ae50395 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55074 = alloca %struct.ScmObj*, align 8
%anf_45bind48225 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50395)
store volatile %struct.ScmObj* %anf_45bind48225, %struct.ScmObj** %stackaddr$prim55074, align 8
%stackaddr$prim55075 = alloca %struct.ScmObj*, align 8
%anf_45bind48226 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48225)
store volatile %struct.ScmObj* %anf_45bind48226, %struct.ScmObj** %stackaddr$prim55075, align 8
%truthy$cmp55076 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48226)
%cmp$cmp55076 = icmp eq i64 %truthy$cmp55076, 1
br i1 %cmp$cmp55076, label %truebranch$cmp55076, label %falsebranch$cmp55076
truebranch$cmp55076:
%ae50399 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50400 = call %struct.ScmObj* @const_init_true()
%argslist54048$k483210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55077 = alloca %struct.ScmObj*, align 8
%argslist54048$k483211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50400, %struct.ScmObj* %argslist54048$k483210)
store volatile %struct.ScmObj* %argslist54048$k483211, %struct.ScmObj** %stackaddr$prim55077, align 8
%stackaddr$prim55078 = alloca %struct.ScmObj*, align 8
%argslist54048$k483212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50399, %struct.ScmObj* %argslist54048$k483211)
store volatile %struct.ScmObj* %argslist54048$k483212, %struct.ScmObj** %stackaddr$prim55078, align 8
%clofunc55079 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48321)
musttail call tailcc void %clofunc55079(%struct.ScmObj* %k48321, %struct.ScmObj* %argslist54048$k483212)
ret void
falsebranch$cmp55076:
%ae50408 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55080 = alloca %struct.ScmObj*, align 8
%anf_45bind48227 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50408)
store volatile %struct.ScmObj* %anf_45bind48227, %struct.ScmObj** %stackaddr$prim55080, align 8
%stackaddr$prim55081 = alloca %struct.ScmObj*, align 8
%anf_45bind48228 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48227)
store volatile %struct.ScmObj* %anf_45bind48228, %struct.ScmObj** %stackaddr$prim55081, align 8
%truthy$cmp55082 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48228)
%cmp$cmp55082 = icmp eq i64 %truthy$cmp55082, 1
br i1 %cmp$cmp55082, label %truebranch$cmp55082, label %falsebranch$cmp55082
truebranch$cmp55082:
%ae50412 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55083 = alloca %struct.ScmObj*, align 8
%anf_45bind48229 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50412)
store volatile %struct.ScmObj* %anf_45bind48229, %struct.ScmObj** %stackaddr$prim55083, align 8
%stackaddr$prim55084 = alloca %struct.ScmObj*, align 8
%b48116 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48229)
store volatile %struct.ScmObj* %b48116, %struct.ScmObj** %stackaddr$prim55084, align 8
%ae50415 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55085 = alloca %struct.ScmObj*, align 8
%anf_45bind48230 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50415)
store volatile %struct.ScmObj* %anf_45bind48230, %struct.ScmObj** %stackaddr$prim55085, align 8
%stackaddr$prim55086 = alloca %struct.ScmObj*, align 8
%anf_45bind48231 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48230)
store volatile %struct.ScmObj* %anf_45bind48231, %struct.ScmObj** %stackaddr$prim55086, align 8
%ae50418 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55087 = alloca %struct.ScmObj*, align 8
%_95048117 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50418, %struct.ScmObj* %anf_45bind48231)
store volatile %struct.ScmObj* %_95048117, %struct.ScmObj** %stackaddr$prim55087, align 8
%argslist54049$cc481140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55088 = alloca %struct.ScmObj*, align 8
%argslist54049$cc481141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48114, %struct.ScmObj* %argslist54049$cc481140)
store volatile %struct.ScmObj* %argslist54049$cc481141, %struct.ScmObj** %stackaddr$prim55088, align 8
%stackaddr$prim55089 = alloca %struct.ScmObj*, align 8
%argslist54049$cc481142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48321, %struct.ScmObj* %argslist54049$cc481141)
store volatile %struct.ScmObj* %argslist54049$cc481142, %struct.ScmObj** %stackaddr$prim55089, align 8
%clofunc55090 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48114)
musttail call tailcc void %clofunc55090(%struct.ScmObj* %cc48114, %struct.ScmObj* %argslist54049$cc481142)
ret void
falsebranch$cmp55082:
%ae50451 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50452 = call %struct.ScmObj* @const_init_false()
%argslist54050$k483210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55091 = alloca %struct.ScmObj*, align 8
%argslist54050$k483211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50452, %struct.ScmObj* %argslist54050$k483210)
store volatile %struct.ScmObj* %argslist54050$k483211, %struct.ScmObj** %stackaddr$prim55091, align 8
%stackaddr$prim55092 = alloca %struct.ScmObj*, align 8
%argslist54050$k483212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50451, %struct.ScmObj* %argslist54050$k483211)
store volatile %struct.ScmObj* %argslist54050$k483212, %struct.ScmObj** %stackaddr$prim55092, align 8
%clofunc55093 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48321)
musttail call tailcc void %clofunc55093(%struct.ScmObj* %k48321, %struct.ScmObj* %argslist54050$k483212)
ret void
}

define tailcc void @proc_clo$ae50280(%struct.ScmObj* %env$ae50280,%struct.ScmObj* %current_45args54051) {
%stackaddr$env-ref55094 = alloca %struct.ScmObj*, align 8
%a48113 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50280, i64 0)
store %struct.ScmObj* %a48113, %struct.ScmObj** %stackaddr$env-ref55094
%stackaddr$env-ref55095 = alloca %struct.ScmObj*, align 8
%k48321 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50280, i64 1)
store %struct.ScmObj* %k48321, %struct.ScmObj** %stackaddr$env-ref55095
%stackaddr$prim55096 = alloca %struct.ScmObj*, align 8
%_95k48323 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54051)
store volatile %struct.ScmObj* %_95k48323, %struct.ScmObj** %stackaddr$prim55096, align 8
%stackaddr$prim55097 = alloca %struct.ScmObj*, align 8
%current_45args54052 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54051)
store volatile %struct.ScmObj* %current_45args54052, %struct.ScmObj** %stackaddr$prim55097, align 8
%stackaddr$prim55098 = alloca %struct.ScmObj*, align 8
%cc48114 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54052)
store volatile %struct.ScmObj* %cc48114, %struct.ScmObj** %stackaddr$prim55098, align 8
%ae50282 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55099 = alloca %struct.ScmObj*, align 8
%anf_45bind48225 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50282)
store volatile %struct.ScmObj* %anf_45bind48225, %struct.ScmObj** %stackaddr$prim55099, align 8
%stackaddr$prim55100 = alloca %struct.ScmObj*, align 8
%anf_45bind48226 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %anf_45bind48225)
store volatile %struct.ScmObj* %anf_45bind48226, %struct.ScmObj** %stackaddr$prim55100, align 8
%truthy$cmp55101 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48226)
%cmp$cmp55101 = icmp eq i64 %truthy$cmp55101, 1
br i1 %cmp$cmp55101, label %truebranch$cmp55101, label %falsebranch$cmp55101
truebranch$cmp55101:
%ae50286 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50287 = call %struct.ScmObj* @const_init_true()
%argslist54054$k483210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55102 = alloca %struct.ScmObj*, align 8
%argslist54054$k483211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50287, %struct.ScmObj* %argslist54054$k483210)
store volatile %struct.ScmObj* %argslist54054$k483211, %struct.ScmObj** %stackaddr$prim55102, align 8
%stackaddr$prim55103 = alloca %struct.ScmObj*, align 8
%argslist54054$k483212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50286, %struct.ScmObj* %argslist54054$k483211)
store volatile %struct.ScmObj* %argslist54054$k483212, %struct.ScmObj** %stackaddr$prim55103, align 8
%clofunc55104 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48321)
musttail call tailcc void %clofunc55104(%struct.ScmObj* %k48321, %struct.ScmObj* %argslist54054$k483212)
ret void
falsebranch$cmp55101:
%ae50295 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55105 = alloca %struct.ScmObj*, align 8
%anf_45bind48227 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50295)
store volatile %struct.ScmObj* %anf_45bind48227, %struct.ScmObj** %stackaddr$prim55105, align 8
%stackaddr$prim55106 = alloca %struct.ScmObj*, align 8
%anf_45bind48228 = call %struct.ScmObj* @prim_cons_63(%struct.ScmObj* %anf_45bind48227)
store volatile %struct.ScmObj* %anf_45bind48228, %struct.ScmObj** %stackaddr$prim55106, align 8
%truthy$cmp55107 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48228)
%cmp$cmp55107 = icmp eq i64 %truthy$cmp55107, 1
br i1 %cmp$cmp55107, label %truebranch$cmp55107, label %falsebranch$cmp55107
truebranch$cmp55107:
%ae50299 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55108 = alloca %struct.ScmObj*, align 8
%anf_45bind48229 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50299)
store volatile %struct.ScmObj* %anf_45bind48229, %struct.ScmObj** %stackaddr$prim55108, align 8
%stackaddr$prim55109 = alloca %struct.ScmObj*, align 8
%b48116 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48229)
store volatile %struct.ScmObj* %b48116, %struct.ScmObj** %stackaddr$prim55109, align 8
%ae50302 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55110 = alloca %struct.ScmObj*, align 8
%anf_45bind48230 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50302)
store volatile %struct.ScmObj* %anf_45bind48230, %struct.ScmObj** %stackaddr$prim55110, align 8
%stackaddr$prim55111 = alloca %struct.ScmObj*, align 8
%anf_45bind48231 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48230)
store volatile %struct.ScmObj* %anf_45bind48231, %struct.ScmObj** %stackaddr$prim55111, align 8
%ae50305 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55112 = alloca %struct.ScmObj*, align 8
%_95048117 = call %struct.ScmObj* @prim_vector_45set_33(%struct.ScmObj* %a48113, %struct.ScmObj* %ae50305, %struct.ScmObj* %anf_45bind48231)
store volatile %struct.ScmObj* %_95048117, %struct.ScmObj** %stackaddr$prim55112, align 8
%argslist54055$cc481140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55113 = alloca %struct.ScmObj*, align 8
%argslist54055$cc481141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cc48114, %struct.ScmObj* %argslist54055$cc481140)
store volatile %struct.ScmObj* %argslist54055$cc481141, %struct.ScmObj** %stackaddr$prim55113, align 8
%stackaddr$prim55114 = alloca %struct.ScmObj*, align 8
%argslist54055$cc481142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48321, %struct.ScmObj* %argslist54055$cc481141)
store volatile %struct.ScmObj* %argslist54055$cc481142, %struct.ScmObj** %stackaddr$prim55114, align 8
%clofunc55115 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %cc48114)
musttail call tailcc void %clofunc55115(%struct.ScmObj* %cc48114, %struct.ScmObj* %argslist54055$cc481142)
ret void
falsebranch$cmp55107:
%ae50338 = call %struct.ScmObj* @const_init_int(i64 0)
%ae50339 = call %struct.ScmObj* @const_init_false()
%argslist54056$k483210 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55116 = alloca %struct.ScmObj*, align 8
%argslist54056$k483211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50339, %struct.ScmObj* %argslist54056$k483210)
store volatile %struct.ScmObj* %argslist54056$k483211, %struct.ScmObj** %stackaddr$prim55116, align 8
%stackaddr$prim55117 = alloca %struct.ScmObj*, align 8
%argslist54056$k483212 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50338, %struct.ScmObj* %argslist54056$k483211)
store volatile %struct.ScmObj* %argslist54056$k483212, %struct.ScmObj** %stackaddr$prim55117, align 8
%clofunc55118 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48321)
musttail call tailcc void %clofunc55118(%struct.ScmObj* %k48321, %struct.ScmObj* %argslist54056$k483212)
ret void
}

define tailcc void @proc_clo$ae50262(%struct.ScmObj* %env$ae50262,%struct.ScmObj* %current_45args54058) {
%stackaddr$prim55119 = alloca %struct.ScmObj*, align 8
%k48324 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54058)
store volatile %struct.ScmObj* %k48324, %struct.ScmObj** %stackaddr$prim55119, align 8
%stackaddr$prim55120 = alloca %struct.ScmObj*, align 8
%current_45args54059 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54058)
store volatile %struct.ScmObj* %current_45args54059, %struct.ScmObj** %stackaddr$prim55120, align 8
%stackaddr$prim55121 = alloca %struct.ScmObj*, align 8
%k48115 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54059)
store volatile %struct.ScmObj* %k48115, %struct.ScmObj** %stackaddr$prim55121, align 8
%ae50264 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54061$k483240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55122 = alloca %struct.ScmObj*, align 8
%argslist54061$k483241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48115, %struct.ScmObj* %argslist54061$k483240)
store volatile %struct.ScmObj* %argslist54061$k483241, %struct.ScmObj** %stackaddr$prim55122, align 8
%stackaddr$prim55123 = alloca %struct.ScmObj*, align 8
%argslist54061$k483242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50264, %struct.ScmObj* %argslist54061$k483241)
store volatile %struct.ScmObj* %argslist54061$k483242, %struct.ScmObj** %stackaddr$prim55123, align 8
%clofunc55124 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48324)
musttail call tailcc void %clofunc55124(%struct.ScmObj* %k48324, %struct.ScmObj* %argslist54061$k483242)
ret void
}

define tailcc void @proc_clo$ae50185(%struct.ScmObj* %env$ae50185,%struct.ScmObj* %current_45args54064) {
%stackaddr$env-ref55125 = alloca %struct.ScmObj*, align 8
%_37append48119 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50185, i64 0)
store %struct.ScmObj* %_37append48119, %struct.ScmObj** %stackaddr$env-ref55125
%stackaddr$prim55126 = alloca %struct.ScmObj*, align 8
%k48325 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54064)
store volatile %struct.ScmObj* %k48325, %struct.ScmObj** %stackaddr$prim55126, align 8
%stackaddr$prim55127 = alloca %struct.ScmObj*, align 8
%current_45args54065 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54064)
store volatile %struct.ScmObj* %current_45args54065, %struct.ScmObj** %stackaddr$prim55127, align 8
%stackaddr$prim55128 = alloca %struct.ScmObj*, align 8
%ls048122 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54065)
store volatile %struct.ScmObj* %ls048122, %struct.ScmObj** %stackaddr$prim55128, align 8
%stackaddr$prim55129 = alloca %struct.ScmObj*, align 8
%current_45args54066 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54065)
store volatile %struct.ScmObj* %current_45args54066, %struct.ScmObj** %stackaddr$prim55129, align 8
%stackaddr$prim55130 = alloca %struct.ScmObj*, align 8
%ls148121 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54066)
store volatile %struct.ScmObj* %ls148121, %struct.ScmObj** %stackaddr$prim55130, align 8
%stackaddr$prim55131 = alloca %struct.ScmObj*, align 8
%anf_45bind48218 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %ls048122)
store volatile %struct.ScmObj* %anf_45bind48218, %struct.ScmObj** %stackaddr$prim55131, align 8
%truthy$cmp55132 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48218)
%cmp$cmp55132 = icmp eq i64 %truthy$cmp55132, 1
br i1 %cmp$cmp55132, label %truebranch$cmp55132, label %falsebranch$cmp55132
truebranch$cmp55132:
%ae50189 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54068$k483250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55133 = alloca %struct.ScmObj*, align 8
%argslist54068$k483251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148121, %struct.ScmObj* %argslist54068$k483250)
store volatile %struct.ScmObj* %argslist54068$k483251, %struct.ScmObj** %stackaddr$prim55133, align 8
%stackaddr$prim55134 = alloca %struct.ScmObj*, align 8
%argslist54068$k483252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50189, %struct.ScmObj* %argslist54068$k483251)
store volatile %struct.ScmObj* %argslist54068$k483252, %struct.ScmObj** %stackaddr$prim55134, align 8
%clofunc55135 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48325)
musttail call tailcc void %clofunc55135(%struct.ScmObj* %k48325, %struct.ScmObj* %argslist54068$k483252)
ret void
falsebranch$cmp55132:
%stackaddr$prim55136 = alloca %struct.ScmObj*, align 8
%anf_45bind48219 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %ls048122)
store volatile %struct.ScmObj* %anf_45bind48219, %struct.ScmObj** %stackaddr$prim55136, align 8
%ae50196 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55137 = alloca %struct.ScmObj*, align 8
%anf_45bind48220 = call %struct.ScmObj* @prim_vector_45ref(%struct.ScmObj* %_37append48119, %struct.ScmObj* %ae50196)
store volatile %struct.ScmObj* %anf_45bind48220, %struct.ScmObj** %stackaddr$prim55137, align 8
%stackaddr$prim55138 = alloca %struct.ScmObj*, align 8
%anf_45bind48221 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %ls048122)
store volatile %struct.ScmObj* %anf_45bind48221, %struct.ScmObj** %stackaddr$prim55138, align 8
%stackaddr$makeclosure55139 = alloca %struct.ScmObj*, align 8
%fptrToInt55140 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae50199 to i64
%ae50199 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55140)
store volatile %struct.ScmObj* %ae50199, %struct.ScmObj** %stackaddr$makeclosure55139, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae50199, %struct.ScmObj* %k48325, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae50199, %struct.ScmObj* %anf_45bind48219, i64 1)
%argslist54073$anf_45bind482200 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55141 = alloca %struct.ScmObj*, align 8
%argslist54073$anf_45bind482201 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ls148121, %struct.ScmObj* %argslist54073$anf_45bind482200)
store volatile %struct.ScmObj* %argslist54073$anf_45bind482201, %struct.ScmObj** %stackaddr$prim55141, align 8
%stackaddr$prim55142 = alloca %struct.ScmObj*, align 8
%argslist54073$anf_45bind482202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48221, %struct.ScmObj* %argslist54073$anf_45bind482201)
store volatile %struct.ScmObj* %argslist54073$anf_45bind482202, %struct.ScmObj** %stackaddr$prim55142, align 8
%stackaddr$prim55143 = alloca %struct.ScmObj*, align 8
%argslist54073$anf_45bind482203 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50199, %struct.ScmObj* %argslist54073$anf_45bind482202)
store volatile %struct.ScmObj* %argslist54073$anf_45bind482203, %struct.ScmObj** %stackaddr$prim55143, align 8
%clofunc55144 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48220)
musttail call tailcc void %clofunc55144(%struct.ScmObj* %anf_45bind48220, %struct.ScmObj* %argslist54073$anf_45bind482203)
ret void
}

define tailcc void @proc_clo$ae50199(%struct.ScmObj* %env$ae50199,%struct.ScmObj* %current_45args54069) {
%stackaddr$env-ref55145 = alloca %struct.ScmObj*, align 8
%k48325 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50199, i64 0)
store %struct.ScmObj* %k48325, %struct.ScmObj** %stackaddr$env-ref55145
%stackaddr$env-ref55146 = alloca %struct.ScmObj*, align 8
%anf_45bind48219 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae50199, i64 1)
store %struct.ScmObj* %anf_45bind48219, %struct.ScmObj** %stackaddr$env-ref55146
%stackaddr$prim55147 = alloca %struct.ScmObj*, align 8
%_95k48326 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54069)
store volatile %struct.ScmObj* %_95k48326, %struct.ScmObj** %stackaddr$prim55147, align 8
%stackaddr$prim55148 = alloca %struct.ScmObj*, align 8
%current_45args54070 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54069)
store volatile %struct.ScmObj* %current_45args54070, %struct.ScmObj** %stackaddr$prim55148, align 8
%stackaddr$prim55149 = alloca %struct.ScmObj*, align 8
%anf_45bind48222 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54070)
store volatile %struct.ScmObj* %anf_45bind48222, %struct.ScmObj** %stackaddr$prim55149, align 8
%stackaddr$prim55150 = alloca %struct.ScmObj*, align 8
%cpsprim48327 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48219, %struct.ScmObj* %anf_45bind48222)
store volatile %struct.ScmObj* %cpsprim48327, %struct.ScmObj** %stackaddr$prim55150, align 8
%ae50205 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54072$k483250 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55151 = alloca %struct.ScmObj*, align 8
%argslist54072$k483251 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48327, %struct.ScmObj* %argslist54072$k483250)
store volatile %struct.ScmObj* %argslist54072$k483251, %struct.ScmObj** %stackaddr$prim55151, align 8
%stackaddr$prim55152 = alloca %struct.ScmObj*, align 8
%argslist54072$k483252 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50205, %struct.ScmObj* %argslist54072$k483251)
store volatile %struct.ScmObj* %argslist54072$k483252, %struct.ScmObj** %stackaddr$prim55152, align 8
%clofunc55153 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48325)
musttail call tailcc void %clofunc55153(%struct.ScmObj* %k48325, %struct.ScmObj* %argslist54072$k483252)
ret void
}

define tailcc void @proc_clo$ae50159(%struct.ScmObj* %env$ae50159,%struct.ScmObj* %current_45args54075) {
%stackaddr$prim55154 = alloca %struct.ScmObj*, align 8
%k48328 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54075)
store volatile %struct.ScmObj* %k48328, %struct.ScmObj** %stackaddr$prim55154, align 8
%stackaddr$prim55155 = alloca %struct.ScmObj*, align 8
%current_45args54076 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54075)
store volatile %struct.ScmObj* %current_45args54076, %struct.ScmObj** %stackaddr$prim55155, align 8
%stackaddr$prim55156 = alloca %struct.ScmObj*, align 8
%a48125 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54076)
store volatile %struct.ScmObj* %a48125, %struct.ScmObj** %stackaddr$prim55156, align 8
%stackaddr$prim55157 = alloca %struct.ScmObj*, align 8
%current_45args54077 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54076)
store volatile %struct.ScmObj* %current_45args54077, %struct.ScmObj** %stackaddr$prim55157, align 8
%stackaddr$prim55158 = alloca %struct.ScmObj*, align 8
%b48124 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54077)
store volatile %struct.ScmObj* %b48124, %struct.ScmObj** %stackaddr$prim55158, align 8
%stackaddr$prim55159 = alloca %struct.ScmObj*, align 8
%anf_45bind48217 = call %struct.ScmObj* @prim__60(%struct.ScmObj* %a48125, %struct.ScmObj* %b48124)
store volatile %struct.ScmObj* %anf_45bind48217, %struct.ScmObj** %stackaddr$prim55159, align 8
%stackaddr$prim55160 = alloca %struct.ScmObj*, align 8
%cpsprim48329 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48217)
store volatile %struct.ScmObj* %cpsprim48329, %struct.ScmObj** %stackaddr$prim55160, align 8
%ae50164 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54079$k483280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55161 = alloca %struct.ScmObj*, align 8
%argslist54079$k483281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48329, %struct.ScmObj* %argslist54079$k483280)
store volatile %struct.ScmObj* %argslist54079$k483281, %struct.ScmObj** %stackaddr$prim55161, align 8
%stackaddr$prim55162 = alloca %struct.ScmObj*, align 8
%argslist54079$k483282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50164, %struct.ScmObj* %argslist54079$k483281)
store volatile %struct.ScmObj* %argslist54079$k483282, %struct.ScmObj** %stackaddr$prim55162, align 8
%clofunc55163 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48328)
musttail call tailcc void %clofunc55163(%struct.ScmObj* %k48328, %struct.ScmObj* %argslist54079$k483282)
ret void
}

define tailcc void @proc_clo$ae50135(%struct.ScmObj* %env$ae50135,%struct.ScmObj* %current_45args54081) {
%stackaddr$prim55164 = alloca %struct.ScmObj*, align 8
%k48330 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54081)
store volatile %struct.ScmObj* %k48330, %struct.ScmObj** %stackaddr$prim55164, align 8
%stackaddr$prim55165 = alloca %struct.ScmObj*, align 8
%current_45args54082 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54081)
store volatile %struct.ScmObj* %current_45args54082, %struct.ScmObj** %stackaddr$prim55165, align 8
%stackaddr$prim55166 = alloca %struct.ScmObj*, align 8
%a48128 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54082)
store volatile %struct.ScmObj* %a48128, %struct.ScmObj** %stackaddr$prim55166, align 8
%stackaddr$prim55167 = alloca %struct.ScmObj*, align 8
%current_45args54083 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54082)
store volatile %struct.ScmObj* %current_45args54083, %struct.ScmObj** %stackaddr$prim55167, align 8
%stackaddr$prim55168 = alloca %struct.ScmObj*, align 8
%b48127 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54083)
store volatile %struct.ScmObj* %b48127, %struct.ScmObj** %stackaddr$prim55168, align 8
%stackaddr$prim55169 = alloca %struct.ScmObj*, align 8
%anf_45bind48216 = call %struct.ScmObj* @prim__60_61(%struct.ScmObj* %a48128, %struct.ScmObj* %b48127)
store volatile %struct.ScmObj* %anf_45bind48216, %struct.ScmObj** %stackaddr$prim55169, align 8
%stackaddr$prim55170 = alloca %struct.ScmObj*, align 8
%cpsprim48331 = call %struct.ScmObj* @prim_not(%struct.ScmObj* %anf_45bind48216)
store volatile %struct.ScmObj* %cpsprim48331, %struct.ScmObj** %stackaddr$prim55170, align 8
%ae50140 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54085$k483300 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55171 = alloca %struct.ScmObj*, align 8
%argslist54085$k483301 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48331, %struct.ScmObj* %argslist54085$k483300)
store volatile %struct.ScmObj* %argslist54085$k483301, %struct.ScmObj** %stackaddr$prim55171, align 8
%stackaddr$prim55172 = alloca %struct.ScmObj*, align 8
%argslist54085$k483302 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae50140, %struct.ScmObj* %argslist54085$k483301)
store volatile %struct.ScmObj* %argslist54085$k483302, %struct.ScmObj** %stackaddr$prim55172, align 8
%clofunc55173 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48330)
musttail call tailcc void %clofunc55173(%struct.ScmObj* %k48330, %struct.ScmObj* %argslist54085$k483302)
ret void
}

define tailcc void @proc_clo$ae49741(%struct.ScmObj* %env$ae49741,%struct.ScmObj* %current_45args54088) {
%stackaddr$env-ref55174 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49741, i64 0)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55174
%stackaddr$env-ref55175 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49741, i64 1)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55175
%stackaddr$env-ref55176 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49741, i64 2)
store %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$env-ref55176
%stackaddr$prim55177 = alloca %struct.ScmObj*, align 8
%k48332 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54088)
store volatile %struct.ScmObj* %k48332, %struct.ScmObj** %stackaddr$prim55177, align 8
%stackaddr$prim55178 = alloca %struct.ScmObj*, align 8
%current_45args54089 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54088)
store volatile %struct.ScmObj* %current_45args54089, %struct.ScmObj** %stackaddr$prim55178, align 8
%stackaddr$prim55179 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54089)
store volatile %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$prim55179, align 8
%ae49743 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55180 = alloca %struct.ScmObj*, align 8
%fptrToInt55181 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49744 to i64
%ae49744 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55181)
store volatile %struct.ScmObj* %ae49744, %struct.ScmObj** %stackaddr$makeclosure55180, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49744, %struct.ScmObj* %_37foldr48052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49744, %struct.ScmObj* %_37foldl48130, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49744, %struct.ScmObj* %_37foldr148047, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49744, %struct.ScmObj* %_37map148078, i64 3)
%argslist54146$k483320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55182 = alloca %struct.ScmObj*, align 8
%argslist54146$k483321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49744, %struct.ScmObj* %argslist54146$k483320)
store volatile %struct.ScmObj* %argslist54146$k483321, %struct.ScmObj** %stackaddr$prim55182, align 8
%stackaddr$prim55183 = alloca %struct.ScmObj*, align 8
%argslist54146$k483322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49743, %struct.ScmObj* %argslist54146$k483321)
store volatile %struct.ScmObj* %argslist54146$k483322, %struct.ScmObj** %stackaddr$prim55183, align 8
%clofunc55184 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48332)
musttail call tailcc void %clofunc55184(%struct.ScmObj* %k48332, %struct.ScmObj* %argslist54146$k483322)
ret void
}

define tailcc void @proc_clo$ae49744(%struct.ScmObj* %env$ae49744,%struct.ScmObj* %args4813148333) {
%stackaddr$env-ref55185 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49744, i64 0)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55185
%stackaddr$env-ref55186 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49744, i64 1)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref55186
%stackaddr$env-ref55187 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49744, i64 2)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55187
%stackaddr$env-ref55188 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49744, i64 3)
store %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$env-ref55188
%stackaddr$prim55189 = alloca %struct.ScmObj*, align 8
%k48334 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4813148333)
store volatile %struct.ScmObj* %k48334, %struct.ScmObj** %stackaddr$prim55189, align 8
%stackaddr$prim55190 = alloca %struct.ScmObj*, align 8
%args48131 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4813148333)
store volatile %struct.ScmObj* %args48131, %struct.ScmObj** %stackaddr$prim55190, align 8
%stackaddr$prim55191 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48131)
store volatile %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$prim55191, align 8
%stackaddr$prim55192 = alloca %struct.ScmObj*, align 8
%anf_45bind48204 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48131)
store volatile %struct.ScmObj* %anf_45bind48204, %struct.ScmObj** %stackaddr$prim55192, align 8
%stackaddr$prim55193 = alloca %struct.ScmObj*, align 8
%acc48133 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48204)
store volatile %struct.ScmObj* %acc48133, %struct.ScmObj** %stackaddr$prim55193, align 8
%stackaddr$prim55194 = alloca %struct.ScmObj*, align 8
%anf_45bind48205 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48131)
store volatile %struct.ScmObj* %anf_45bind48205, %struct.ScmObj** %stackaddr$prim55194, align 8
%stackaddr$prim55195 = alloca %struct.ScmObj*, align 8
%lsts48132 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48205)
store volatile %struct.ScmObj* %lsts48132, %struct.ScmObj** %stackaddr$prim55195, align 8
%stackaddr$makeclosure55196 = alloca %struct.ScmObj*, align 8
%fptrToInt55197 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49752 to i64
%ae49752 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55197)
store volatile %struct.ScmObj* %ae49752, %struct.ScmObj** %stackaddr$makeclosure55196, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49752, %struct.ScmObj* %lsts48132, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49752, %struct.ScmObj* %_37foldr48052, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49752, %struct.ScmObj* %k48334, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49752, %struct.ScmObj* %_37map148078, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49752, %struct.ScmObj* %f48134, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49752, %struct.ScmObj* %acc48133, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49752, %struct.ScmObj* %_37foldl48130, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49752, %struct.ScmObj* %_37foldr148047, i64 7)
%ae49753 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55198 = alloca %struct.ScmObj*, align 8
%fptrToInt55199 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49754 to i64
%ae49754 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55199)
store volatile %struct.ScmObj* %ae49754, %struct.ScmObj** %stackaddr$makeclosure55198, align 8
%argslist54145$ae497520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55200 = alloca %struct.ScmObj*, align 8
%argslist54145$ae497521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49754, %struct.ScmObj* %argslist54145$ae497520)
store volatile %struct.ScmObj* %argslist54145$ae497521, %struct.ScmObj** %stackaddr$prim55200, align 8
%stackaddr$prim55201 = alloca %struct.ScmObj*, align 8
%argslist54145$ae497522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49753, %struct.ScmObj* %argslist54145$ae497521)
store volatile %struct.ScmObj* %argslist54145$ae497522, %struct.ScmObj** %stackaddr$prim55201, align 8
%clofunc55202 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49752)
musttail call tailcc void %clofunc55202(%struct.ScmObj* %ae49752, %struct.ScmObj* %argslist54145$ae497522)
ret void
}

define tailcc void @proc_clo$ae49752(%struct.ScmObj* %env$ae49752,%struct.ScmObj* %current_45args54091) {
%stackaddr$env-ref55203 = alloca %struct.ScmObj*, align 8
%lsts48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49752, i64 0)
store %struct.ScmObj* %lsts48132, %struct.ScmObj** %stackaddr$env-ref55203
%stackaddr$env-ref55204 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49752, i64 1)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55204
%stackaddr$env-ref55205 = alloca %struct.ScmObj*, align 8
%k48334 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49752, i64 2)
store %struct.ScmObj* %k48334, %struct.ScmObj** %stackaddr$env-ref55205
%stackaddr$env-ref55206 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49752, i64 3)
store %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$env-ref55206
%stackaddr$env-ref55207 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49752, i64 4)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref55207
%stackaddr$env-ref55208 = alloca %struct.ScmObj*, align 8
%acc48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49752, i64 5)
store %struct.ScmObj* %acc48133, %struct.ScmObj** %stackaddr$env-ref55208
%stackaddr$env-ref55209 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49752, i64 6)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref55209
%stackaddr$env-ref55210 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49752, i64 7)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55210
%stackaddr$prim55211 = alloca %struct.ScmObj*, align 8
%_95k48335 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54091)
store volatile %struct.ScmObj* %_95k48335, %struct.ScmObj** %stackaddr$prim55211, align 8
%stackaddr$prim55212 = alloca %struct.ScmObj*, align 8
%current_45args54092 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54091)
store volatile %struct.ScmObj* %current_45args54092, %struct.ScmObj** %stackaddr$prim55212, align 8
%stackaddr$prim55213 = alloca %struct.ScmObj*, align 8
%anf_45bind48206 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54092)
store volatile %struct.ScmObj* %anf_45bind48206, %struct.ScmObj** %stackaddr$prim55213, align 8
%stackaddr$makeclosure55214 = alloca %struct.ScmObj*, align 8
%fptrToInt55215 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49784 to i64
%ae49784 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55215)
store volatile %struct.ScmObj* %ae49784, %struct.ScmObj** %stackaddr$makeclosure55214, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49784, %struct.ScmObj* %lsts48132, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49784, %struct.ScmObj* %_37foldr48052, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49784, %struct.ScmObj* %k48334, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49784, %struct.ScmObj* %_37map148078, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49784, %struct.ScmObj* %f48134, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49784, %struct.ScmObj* %acc48133, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49784, %struct.ScmObj* %_37foldl48130, i64 6)
%ae49786 = call %struct.ScmObj* @const_init_false()
%argslist54138$_37foldr1480470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55216 = alloca %struct.ScmObj*, align 8
%argslist54138$_37foldr1480471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48132, %struct.ScmObj* %argslist54138$_37foldr1480470)
store volatile %struct.ScmObj* %argslist54138$_37foldr1480471, %struct.ScmObj** %stackaddr$prim55216, align 8
%stackaddr$prim55217 = alloca %struct.ScmObj*, align 8
%argslist54138$_37foldr1480472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49786, %struct.ScmObj* %argslist54138$_37foldr1480471)
store volatile %struct.ScmObj* %argslist54138$_37foldr1480472, %struct.ScmObj** %stackaddr$prim55217, align 8
%stackaddr$prim55218 = alloca %struct.ScmObj*, align 8
%argslist54138$_37foldr1480473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48206, %struct.ScmObj* %argslist54138$_37foldr1480472)
store volatile %struct.ScmObj* %argslist54138$_37foldr1480473, %struct.ScmObj** %stackaddr$prim55218, align 8
%stackaddr$prim55219 = alloca %struct.ScmObj*, align 8
%argslist54138$_37foldr1480474 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49784, %struct.ScmObj* %argslist54138$_37foldr1480473)
store volatile %struct.ScmObj* %argslist54138$_37foldr1480474, %struct.ScmObj** %stackaddr$prim55219, align 8
%clofunc55220 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148047)
musttail call tailcc void %clofunc55220(%struct.ScmObj* %_37foldr148047, %struct.ScmObj* %argslist54138$_37foldr1480474)
ret void
}

define tailcc void @proc_clo$ae49784(%struct.ScmObj* %env$ae49784,%struct.ScmObj* %current_45args54094) {
%stackaddr$env-ref55221 = alloca %struct.ScmObj*, align 8
%lsts48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49784, i64 0)
store %struct.ScmObj* %lsts48132, %struct.ScmObj** %stackaddr$env-ref55221
%stackaddr$env-ref55222 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49784, i64 1)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55222
%stackaddr$env-ref55223 = alloca %struct.ScmObj*, align 8
%k48334 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49784, i64 2)
store %struct.ScmObj* %k48334, %struct.ScmObj** %stackaddr$env-ref55223
%stackaddr$env-ref55224 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49784, i64 3)
store %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$env-ref55224
%stackaddr$env-ref55225 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49784, i64 4)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref55225
%stackaddr$env-ref55226 = alloca %struct.ScmObj*, align 8
%acc48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49784, i64 5)
store %struct.ScmObj* %acc48133, %struct.ScmObj** %stackaddr$env-ref55226
%stackaddr$env-ref55227 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49784, i64 6)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref55227
%stackaddr$prim55228 = alloca %struct.ScmObj*, align 8
%_95k48336 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54094)
store volatile %struct.ScmObj* %_95k48336, %struct.ScmObj** %stackaddr$prim55228, align 8
%stackaddr$prim55229 = alloca %struct.ScmObj*, align 8
%current_45args54095 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54094)
store volatile %struct.ScmObj* %current_45args54095, %struct.ScmObj** %stackaddr$prim55229, align 8
%stackaddr$prim55230 = alloca %struct.ScmObj*, align 8
%anf_45bind48207 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54095)
store volatile %struct.ScmObj* %anf_45bind48207, %struct.ScmObj** %stackaddr$prim55230, align 8
%truthy$cmp55231 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48207)
%cmp$cmp55231 = icmp eq i64 %truthy$cmp55231, 1
br i1 %cmp$cmp55231, label %truebranch$cmp55231, label %falsebranch$cmp55231
truebranch$cmp55231:
%ae49795 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54097$k483340 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55232 = alloca %struct.ScmObj*, align 8
%argslist54097$k483341 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48133, %struct.ScmObj* %argslist54097$k483340)
store volatile %struct.ScmObj* %argslist54097$k483341, %struct.ScmObj** %stackaddr$prim55232, align 8
%stackaddr$prim55233 = alloca %struct.ScmObj*, align 8
%argslist54097$k483342 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49795, %struct.ScmObj* %argslist54097$k483341)
store volatile %struct.ScmObj* %argslist54097$k483342, %struct.ScmObj** %stackaddr$prim55233, align 8
%clofunc55234 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48334)
musttail call tailcc void %clofunc55234(%struct.ScmObj* %k48334, %struct.ScmObj* %argslist54097$k483342)
ret void
falsebranch$cmp55231:
%stackaddr$makeclosure55235 = alloca %struct.ScmObj*, align 8
%fptrToInt55236 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49800 to i64
%ae49800 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55236)
store volatile %struct.ScmObj* %ae49800, %struct.ScmObj** %stackaddr$makeclosure55235, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49800, %struct.ScmObj* %lsts48132, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49800, %struct.ScmObj* %_37foldr48052, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49800, %struct.ScmObj* %k48334, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49800, %struct.ScmObj* %_37map148078, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49800, %struct.ScmObj* %f48134, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49800, %struct.ScmObj* %acc48133, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49800, %struct.ScmObj* %_37foldl48130, i64 6)
%ae49801 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55237 = alloca %struct.ScmObj*, align 8
%fptrToInt55238 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49802 to i64
%ae49802 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55238)
store volatile %struct.ScmObj* %ae49802, %struct.ScmObj** %stackaddr$makeclosure55237, align 8
%argslist54137$ae498000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55239 = alloca %struct.ScmObj*, align 8
%argslist54137$ae498001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49802, %struct.ScmObj* %argslist54137$ae498000)
store volatile %struct.ScmObj* %argslist54137$ae498001, %struct.ScmObj** %stackaddr$prim55239, align 8
%stackaddr$prim55240 = alloca %struct.ScmObj*, align 8
%argslist54137$ae498002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49801, %struct.ScmObj* %argslist54137$ae498001)
store volatile %struct.ScmObj* %argslist54137$ae498002, %struct.ScmObj** %stackaddr$prim55240, align 8
%clofunc55241 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49800)
musttail call tailcc void %clofunc55241(%struct.ScmObj* %ae49800, %struct.ScmObj* %argslist54137$ae498002)
ret void
}

define tailcc void @proc_clo$ae49800(%struct.ScmObj* %env$ae49800,%struct.ScmObj* %current_45args54098) {
%stackaddr$env-ref55242 = alloca %struct.ScmObj*, align 8
%lsts48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49800, i64 0)
store %struct.ScmObj* %lsts48132, %struct.ScmObj** %stackaddr$env-ref55242
%stackaddr$env-ref55243 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49800, i64 1)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55243
%stackaddr$env-ref55244 = alloca %struct.ScmObj*, align 8
%k48334 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49800, i64 2)
store %struct.ScmObj* %k48334, %struct.ScmObj** %stackaddr$env-ref55244
%stackaddr$env-ref55245 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49800, i64 3)
store %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$env-ref55245
%stackaddr$env-ref55246 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49800, i64 4)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref55246
%stackaddr$env-ref55247 = alloca %struct.ScmObj*, align 8
%acc48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49800, i64 5)
store %struct.ScmObj* %acc48133, %struct.ScmObj** %stackaddr$env-ref55247
%stackaddr$env-ref55248 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49800, i64 6)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref55248
%stackaddr$prim55249 = alloca %struct.ScmObj*, align 8
%_95k48337 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54098)
store volatile %struct.ScmObj* %_95k48337, %struct.ScmObj** %stackaddr$prim55249, align 8
%stackaddr$prim55250 = alloca %struct.ScmObj*, align 8
%current_45args54099 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54098)
store volatile %struct.ScmObj* %current_45args54099, %struct.ScmObj** %stackaddr$prim55250, align 8
%stackaddr$prim55251 = alloca %struct.ScmObj*, align 8
%anf_45bind48208 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54099)
store volatile %struct.ScmObj* %anf_45bind48208, %struct.ScmObj** %stackaddr$prim55251, align 8
%stackaddr$makeclosure55252 = alloca %struct.ScmObj*, align 8
%fptrToInt55253 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49821 to i64
%ae49821 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55253)
store volatile %struct.ScmObj* %ae49821, %struct.ScmObj** %stackaddr$makeclosure55252, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49821, %struct.ScmObj* %lsts48132, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49821, %struct.ScmObj* %_37foldr48052, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49821, %struct.ScmObj* %k48334, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49821, %struct.ScmObj* %_37map148078, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49821, %struct.ScmObj* %f48134, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49821, %struct.ScmObj* %acc48133, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49821, %struct.ScmObj* %_37foldl48130, i64 6)
%argslist54132$_37map1480780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55254 = alloca %struct.ScmObj*, align 8
%argslist54132$_37map1480781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48132, %struct.ScmObj* %argslist54132$_37map1480780)
store volatile %struct.ScmObj* %argslist54132$_37map1480781, %struct.ScmObj** %stackaddr$prim55254, align 8
%stackaddr$prim55255 = alloca %struct.ScmObj*, align 8
%argslist54132$_37map1480782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48208, %struct.ScmObj* %argslist54132$_37map1480781)
store volatile %struct.ScmObj* %argslist54132$_37map1480782, %struct.ScmObj** %stackaddr$prim55255, align 8
%stackaddr$prim55256 = alloca %struct.ScmObj*, align 8
%argslist54132$_37map1480783 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49821, %struct.ScmObj* %argslist54132$_37map1480782)
store volatile %struct.ScmObj* %argslist54132$_37map1480783, %struct.ScmObj** %stackaddr$prim55256, align 8
%clofunc55257 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148078)
musttail call tailcc void %clofunc55257(%struct.ScmObj* %_37map148078, %struct.ScmObj* %argslist54132$_37map1480783)
ret void
}

define tailcc void @proc_clo$ae49821(%struct.ScmObj* %env$ae49821,%struct.ScmObj* %current_45args54101) {
%stackaddr$env-ref55258 = alloca %struct.ScmObj*, align 8
%lsts48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49821, i64 0)
store %struct.ScmObj* %lsts48132, %struct.ScmObj** %stackaddr$env-ref55258
%stackaddr$env-ref55259 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49821, i64 1)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55259
%stackaddr$env-ref55260 = alloca %struct.ScmObj*, align 8
%k48334 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49821, i64 2)
store %struct.ScmObj* %k48334, %struct.ScmObj** %stackaddr$env-ref55260
%stackaddr$env-ref55261 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49821, i64 3)
store %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$env-ref55261
%stackaddr$env-ref55262 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49821, i64 4)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref55262
%stackaddr$env-ref55263 = alloca %struct.ScmObj*, align 8
%acc48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49821, i64 5)
store %struct.ScmObj* %acc48133, %struct.ScmObj** %stackaddr$env-ref55263
%stackaddr$env-ref55264 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49821, i64 6)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref55264
%stackaddr$prim55265 = alloca %struct.ScmObj*, align 8
%_95k48338 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54101)
store volatile %struct.ScmObj* %_95k48338, %struct.ScmObj** %stackaddr$prim55265, align 8
%stackaddr$prim55266 = alloca %struct.ScmObj*, align 8
%current_45args54102 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54101)
store volatile %struct.ScmObj* %current_45args54102, %struct.ScmObj** %stackaddr$prim55266, align 8
%stackaddr$prim55267 = alloca %struct.ScmObj*, align 8
%lsts_4348139 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54102)
store volatile %struct.ScmObj* %lsts_4348139, %struct.ScmObj** %stackaddr$prim55267, align 8
%stackaddr$makeclosure55268 = alloca %struct.ScmObj*, align 8
%fptrToInt55269 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49824 to i64
%ae49824 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55269)
store volatile %struct.ScmObj* %ae49824, %struct.ScmObj** %stackaddr$makeclosure55268, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49824, %struct.ScmObj* %lsts48132, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49824, %struct.ScmObj* %_37foldr48052, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49824, %struct.ScmObj* %k48334, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49824, %struct.ScmObj* %_37map148078, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49824, %struct.ScmObj* %f48134, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49824, %struct.ScmObj* %acc48133, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49824, %struct.ScmObj* %_37foldl48130, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49824, %struct.ScmObj* %lsts_4348139, i64 7)
%ae49825 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55270 = alloca %struct.ScmObj*, align 8
%fptrToInt55271 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49826 to i64
%ae49826 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55271)
store volatile %struct.ScmObj* %ae49826, %struct.ScmObj** %stackaddr$makeclosure55270, align 8
%argslist54131$ae498240 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55272 = alloca %struct.ScmObj*, align 8
%argslist54131$ae498241 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49826, %struct.ScmObj* %argslist54131$ae498240)
store volatile %struct.ScmObj* %argslist54131$ae498241, %struct.ScmObj** %stackaddr$prim55272, align 8
%stackaddr$prim55273 = alloca %struct.ScmObj*, align 8
%argslist54131$ae498242 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49825, %struct.ScmObj* %argslist54131$ae498241)
store volatile %struct.ScmObj* %argslist54131$ae498242, %struct.ScmObj** %stackaddr$prim55273, align 8
%clofunc55274 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49824)
musttail call tailcc void %clofunc55274(%struct.ScmObj* %ae49824, %struct.ScmObj* %argslist54131$ae498242)
ret void
}

define tailcc void @proc_clo$ae49824(%struct.ScmObj* %env$ae49824,%struct.ScmObj* %current_45args54104) {
%stackaddr$env-ref55275 = alloca %struct.ScmObj*, align 8
%lsts48132 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49824, i64 0)
store %struct.ScmObj* %lsts48132, %struct.ScmObj** %stackaddr$env-ref55275
%stackaddr$env-ref55276 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49824, i64 1)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55276
%stackaddr$env-ref55277 = alloca %struct.ScmObj*, align 8
%k48334 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49824, i64 2)
store %struct.ScmObj* %k48334, %struct.ScmObj** %stackaddr$env-ref55277
%stackaddr$env-ref55278 = alloca %struct.ScmObj*, align 8
%_37map148078 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49824, i64 3)
store %struct.ScmObj* %_37map148078, %struct.ScmObj** %stackaddr$env-ref55278
%stackaddr$env-ref55279 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49824, i64 4)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref55279
%stackaddr$env-ref55280 = alloca %struct.ScmObj*, align 8
%acc48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49824, i64 5)
store %struct.ScmObj* %acc48133, %struct.ScmObj** %stackaddr$env-ref55280
%stackaddr$env-ref55281 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49824, i64 6)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref55281
%stackaddr$env-ref55282 = alloca %struct.ScmObj*, align 8
%lsts_4348139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49824, i64 7)
store %struct.ScmObj* %lsts_4348139, %struct.ScmObj** %stackaddr$env-ref55282
%stackaddr$prim55283 = alloca %struct.ScmObj*, align 8
%_95k48339 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54104)
store volatile %struct.ScmObj* %_95k48339, %struct.ScmObj** %stackaddr$prim55283, align 8
%stackaddr$prim55284 = alloca %struct.ScmObj*, align 8
%current_45args54105 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54104)
store volatile %struct.ScmObj* %current_45args54105, %struct.ScmObj** %stackaddr$prim55284, align 8
%stackaddr$prim55285 = alloca %struct.ScmObj*, align 8
%anf_45bind48209 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54105)
store volatile %struct.ScmObj* %anf_45bind48209, %struct.ScmObj** %stackaddr$prim55285, align 8
%stackaddr$makeclosure55286 = alloca %struct.ScmObj*, align 8
%fptrToInt55287 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49845 to i64
%ae49845 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55287)
store volatile %struct.ScmObj* %ae49845, %struct.ScmObj** %stackaddr$makeclosure55286, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49845, %struct.ScmObj* %f48134, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49845, %struct.ScmObj* %acc48133, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49845, %struct.ScmObj* %_37foldr48052, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49845, %struct.ScmObj* %_37foldl48130, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49845, %struct.ScmObj* %k48334, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49845, %struct.ScmObj* %lsts_4348139, i64 5)
%argslist54126$_37map1480780 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55288 = alloca %struct.ScmObj*, align 8
%argslist54126$_37map1480781 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48132, %struct.ScmObj* %argslist54126$_37map1480780)
store volatile %struct.ScmObj* %argslist54126$_37map1480781, %struct.ScmObj** %stackaddr$prim55288, align 8
%stackaddr$prim55289 = alloca %struct.ScmObj*, align 8
%argslist54126$_37map1480782 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48209, %struct.ScmObj* %argslist54126$_37map1480781)
store volatile %struct.ScmObj* %argslist54126$_37map1480782, %struct.ScmObj** %stackaddr$prim55289, align 8
%stackaddr$prim55290 = alloca %struct.ScmObj*, align 8
%argslist54126$_37map1480783 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49845, %struct.ScmObj* %argslist54126$_37map1480782)
store volatile %struct.ScmObj* %argslist54126$_37map1480783, %struct.ScmObj** %stackaddr$prim55290, align 8
%clofunc55291 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148078)
musttail call tailcc void %clofunc55291(%struct.ScmObj* %_37map148078, %struct.ScmObj* %argslist54126$_37map1480783)
ret void
}

define tailcc void @proc_clo$ae49845(%struct.ScmObj* %env$ae49845,%struct.ScmObj* %current_45args54107) {
%stackaddr$env-ref55292 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49845, i64 0)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref55292
%stackaddr$env-ref55293 = alloca %struct.ScmObj*, align 8
%acc48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49845, i64 1)
store %struct.ScmObj* %acc48133, %struct.ScmObj** %stackaddr$env-ref55293
%stackaddr$env-ref55294 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49845, i64 2)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55294
%stackaddr$env-ref55295 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49845, i64 3)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref55295
%stackaddr$env-ref55296 = alloca %struct.ScmObj*, align 8
%k48334 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49845, i64 4)
store %struct.ScmObj* %k48334, %struct.ScmObj** %stackaddr$env-ref55296
%stackaddr$env-ref55297 = alloca %struct.ScmObj*, align 8
%lsts_4348139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49845, i64 5)
store %struct.ScmObj* %lsts_4348139, %struct.ScmObj** %stackaddr$env-ref55297
%stackaddr$prim55298 = alloca %struct.ScmObj*, align 8
%_95k48340 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54107)
store volatile %struct.ScmObj* %_95k48340, %struct.ScmObj** %stackaddr$prim55298, align 8
%stackaddr$prim55299 = alloca %struct.ScmObj*, align 8
%current_45args54108 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54107)
store volatile %struct.ScmObj* %current_45args54108, %struct.ScmObj** %stackaddr$prim55299, align 8
%stackaddr$prim55300 = alloca %struct.ScmObj*, align 8
%vs48137 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54108)
store volatile %struct.ScmObj* %vs48137, %struct.ScmObj** %stackaddr$prim55300, align 8
%stackaddr$makeclosure55301 = alloca %struct.ScmObj*, align 8
%fptrToInt55302 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49848 to i64
%ae49848 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55302)
store volatile %struct.ScmObj* %ae49848, %struct.ScmObj** %stackaddr$makeclosure55301, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49848, %struct.ScmObj* %vs48137, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49848, %struct.ScmObj* %f48134, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49848, %struct.ScmObj* %acc48133, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49848, %struct.ScmObj* %_37foldr48052, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49848, %struct.ScmObj* %_37foldl48130, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49848, %struct.ScmObj* %k48334, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49848, %struct.ScmObj* %lsts_4348139, i64 6)
%ae49849 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55303 = alloca %struct.ScmObj*, align 8
%fptrToInt55304 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49850 to i64
%ae49850 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55304)
store volatile %struct.ScmObj* %ae49850, %struct.ScmObj** %stackaddr$makeclosure55303, align 8
%argslist54125$ae498480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55305 = alloca %struct.ScmObj*, align 8
%argslist54125$ae498481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49850, %struct.ScmObj* %argslist54125$ae498480)
store volatile %struct.ScmObj* %argslist54125$ae498481, %struct.ScmObj** %stackaddr$prim55305, align 8
%stackaddr$prim55306 = alloca %struct.ScmObj*, align 8
%argslist54125$ae498482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49849, %struct.ScmObj* %argslist54125$ae498481)
store volatile %struct.ScmObj* %argslist54125$ae498482, %struct.ScmObj** %stackaddr$prim55306, align 8
%clofunc55307 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49848)
musttail call tailcc void %clofunc55307(%struct.ScmObj* %ae49848, %struct.ScmObj* %argslist54125$ae498482)
ret void
}

define tailcc void @proc_clo$ae49848(%struct.ScmObj* %env$ae49848,%struct.ScmObj* %current_45args54110) {
%stackaddr$env-ref55308 = alloca %struct.ScmObj*, align 8
%vs48137 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49848, i64 0)
store %struct.ScmObj* %vs48137, %struct.ScmObj** %stackaddr$env-ref55308
%stackaddr$env-ref55309 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49848, i64 1)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref55309
%stackaddr$env-ref55310 = alloca %struct.ScmObj*, align 8
%acc48133 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49848, i64 2)
store %struct.ScmObj* %acc48133, %struct.ScmObj** %stackaddr$env-ref55310
%stackaddr$env-ref55311 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49848, i64 3)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55311
%stackaddr$env-ref55312 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49848, i64 4)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref55312
%stackaddr$env-ref55313 = alloca %struct.ScmObj*, align 8
%k48334 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49848, i64 5)
store %struct.ScmObj* %k48334, %struct.ScmObj** %stackaddr$env-ref55313
%stackaddr$env-ref55314 = alloca %struct.ScmObj*, align 8
%lsts_4348139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49848, i64 6)
store %struct.ScmObj* %lsts_4348139, %struct.ScmObj** %stackaddr$env-ref55314
%stackaddr$prim55315 = alloca %struct.ScmObj*, align 8
%_95k48341 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54110)
store volatile %struct.ScmObj* %_95k48341, %struct.ScmObj** %stackaddr$prim55315, align 8
%stackaddr$prim55316 = alloca %struct.ScmObj*, align 8
%current_45args54111 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54110)
store volatile %struct.ScmObj* %current_45args54111, %struct.ScmObj** %stackaddr$prim55316, align 8
%stackaddr$prim55317 = alloca %struct.ScmObj*, align 8
%anf_45bind48210 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54111)
store volatile %struct.ScmObj* %anf_45bind48210, %struct.ScmObj** %stackaddr$prim55317, align 8
%ae49871 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55318 = alloca %struct.ScmObj*, align 8
%anf_45bind48211 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48133, %struct.ScmObj* %ae49871)
store volatile %struct.ScmObj* %anf_45bind48211, %struct.ScmObj** %stackaddr$prim55318, align 8
%stackaddr$makeclosure55319 = alloca %struct.ScmObj*, align 8
%fptrToInt55320 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49873 to i64
%ae49873 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55320)
store volatile %struct.ScmObj* %ae49873, %struct.ScmObj** %stackaddr$makeclosure55319, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49873, %struct.ScmObj* %f48134, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49873, %struct.ScmObj* %_37foldl48130, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49873, %struct.ScmObj* %k48334, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49873, %struct.ScmObj* %lsts_4348139, i64 3)
%argslist54119$_37foldr480520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55321 = alloca %struct.ScmObj*, align 8
%argslist54119$_37foldr480521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48137, %struct.ScmObj* %argslist54119$_37foldr480520)
store volatile %struct.ScmObj* %argslist54119$_37foldr480521, %struct.ScmObj** %stackaddr$prim55321, align 8
%stackaddr$prim55322 = alloca %struct.ScmObj*, align 8
%argslist54119$_37foldr480522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48211, %struct.ScmObj* %argslist54119$_37foldr480521)
store volatile %struct.ScmObj* %argslist54119$_37foldr480522, %struct.ScmObj** %stackaddr$prim55322, align 8
%stackaddr$prim55323 = alloca %struct.ScmObj*, align 8
%argslist54119$_37foldr480523 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48210, %struct.ScmObj* %argslist54119$_37foldr480522)
store volatile %struct.ScmObj* %argslist54119$_37foldr480523, %struct.ScmObj** %stackaddr$prim55323, align 8
%stackaddr$prim55324 = alloca %struct.ScmObj*, align 8
%argslist54119$_37foldr480524 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49873, %struct.ScmObj* %argslist54119$_37foldr480523)
store volatile %struct.ScmObj* %argslist54119$_37foldr480524, %struct.ScmObj** %stackaddr$prim55324, align 8
%clofunc55325 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48052)
musttail call tailcc void %clofunc55325(%struct.ScmObj* %_37foldr48052, %struct.ScmObj* %argslist54119$_37foldr480524)
ret void
}

define tailcc void @proc_clo$ae49873(%struct.ScmObj* %env$ae49873,%struct.ScmObj* %current_45args54113) {
%stackaddr$env-ref55326 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49873, i64 0)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref55326
%stackaddr$env-ref55327 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49873, i64 1)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref55327
%stackaddr$env-ref55328 = alloca %struct.ScmObj*, align 8
%k48334 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49873, i64 2)
store %struct.ScmObj* %k48334, %struct.ScmObj** %stackaddr$env-ref55328
%stackaddr$env-ref55329 = alloca %struct.ScmObj*, align 8
%lsts_4348139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49873, i64 3)
store %struct.ScmObj* %lsts_4348139, %struct.ScmObj** %stackaddr$env-ref55329
%stackaddr$prim55330 = alloca %struct.ScmObj*, align 8
%_95k48342 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54113)
store volatile %struct.ScmObj* %_95k48342, %struct.ScmObj** %stackaddr$prim55330, align 8
%stackaddr$prim55331 = alloca %struct.ScmObj*, align 8
%current_45args54114 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54113)
store volatile %struct.ScmObj* %current_45args54114, %struct.ScmObj** %stackaddr$prim55331, align 8
%stackaddr$prim55332 = alloca %struct.ScmObj*, align 8
%anf_45bind48212 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54114)
store volatile %struct.ScmObj* %anf_45bind48212, %struct.ScmObj** %stackaddr$prim55332, align 8
%stackaddr$makeclosure55333 = alloca %struct.ScmObj*, align 8
%fptrToInt55334 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49877 to i64
%ae49877 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55334)
store volatile %struct.ScmObj* %ae49877, %struct.ScmObj** %stackaddr$makeclosure55333, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49877, %struct.ScmObj* %f48134, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49877, %struct.ScmObj* %_37foldl48130, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49877, %struct.ScmObj* %k48334, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49877, %struct.ScmObj* %lsts_4348139, i64 3)
%stackaddr$prim55335 = alloca %struct.ScmObj*, align 8
%cpsargs48345 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49877, %struct.ScmObj* %anf_45bind48212)
store volatile %struct.ScmObj* %cpsargs48345, %struct.ScmObj** %stackaddr$prim55335, align 8
%clofunc55336 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48134)
musttail call tailcc void %clofunc55336(%struct.ScmObj* %f48134, %struct.ScmObj* %cpsargs48345)
ret void
}

define tailcc void @proc_clo$ae49877(%struct.ScmObj* %env$ae49877,%struct.ScmObj* %current_45args54116) {
%stackaddr$env-ref55337 = alloca %struct.ScmObj*, align 8
%f48134 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49877, i64 0)
store %struct.ScmObj* %f48134, %struct.ScmObj** %stackaddr$env-ref55337
%stackaddr$env-ref55338 = alloca %struct.ScmObj*, align 8
%_37foldl48130 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49877, i64 1)
store %struct.ScmObj* %_37foldl48130, %struct.ScmObj** %stackaddr$env-ref55338
%stackaddr$env-ref55339 = alloca %struct.ScmObj*, align 8
%k48334 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49877, i64 2)
store %struct.ScmObj* %k48334, %struct.ScmObj** %stackaddr$env-ref55339
%stackaddr$env-ref55340 = alloca %struct.ScmObj*, align 8
%lsts_4348139 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49877, i64 3)
store %struct.ScmObj* %lsts_4348139, %struct.ScmObj** %stackaddr$env-ref55340
%stackaddr$prim55341 = alloca %struct.ScmObj*, align 8
%_95k48343 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54116)
store volatile %struct.ScmObj* %_95k48343, %struct.ScmObj** %stackaddr$prim55341, align 8
%stackaddr$prim55342 = alloca %struct.ScmObj*, align 8
%current_45args54117 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54116)
store volatile %struct.ScmObj* %current_45args54117, %struct.ScmObj** %stackaddr$prim55342, align 8
%stackaddr$prim55343 = alloca %struct.ScmObj*, align 8
%acc_4348141 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54117)
store volatile %struct.ScmObj* %acc_4348141, %struct.ScmObj** %stackaddr$prim55343, align 8
%stackaddr$prim55344 = alloca %struct.ScmObj*, align 8
%anf_45bind48213 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc_4348141, %struct.ScmObj* %lsts_4348139)
store volatile %struct.ScmObj* %anf_45bind48213, %struct.ScmObj** %stackaddr$prim55344, align 8
%stackaddr$prim55345 = alloca %struct.ScmObj*, align 8
%anf_45bind48214 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48134, %struct.ScmObj* %anf_45bind48213)
store volatile %struct.ScmObj* %anf_45bind48214, %struct.ScmObj** %stackaddr$prim55345, align 8
%stackaddr$prim55346 = alloca %struct.ScmObj*, align 8
%cpsargs48344 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48334, %struct.ScmObj* %anf_45bind48214)
store volatile %struct.ScmObj* %cpsargs48344, %struct.ScmObj** %stackaddr$prim55346, align 8
%clofunc55347 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl48130)
musttail call tailcc void %clofunc55347(%struct.ScmObj* %_37foldl48130, %struct.ScmObj* %cpsargs48344)
ret void
}

define tailcc void @proc_clo$ae49850(%struct.ScmObj* %env$ae49850,%struct.ScmObj* %current_45args54120) {
%stackaddr$prim55348 = alloca %struct.ScmObj*, align 8
%k48346 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54120)
store volatile %struct.ScmObj* %k48346, %struct.ScmObj** %stackaddr$prim55348, align 8
%stackaddr$prim55349 = alloca %struct.ScmObj*, align 8
%current_45args54121 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54120)
store volatile %struct.ScmObj* %current_45args54121, %struct.ScmObj** %stackaddr$prim55349, align 8
%stackaddr$prim55350 = alloca %struct.ScmObj*, align 8
%a48143 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54121)
store volatile %struct.ScmObj* %a48143, %struct.ScmObj** %stackaddr$prim55350, align 8
%stackaddr$prim55351 = alloca %struct.ScmObj*, align 8
%current_45args54122 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54121)
store volatile %struct.ScmObj* %current_45args54122, %struct.ScmObj** %stackaddr$prim55351, align 8
%stackaddr$prim55352 = alloca %struct.ScmObj*, align 8
%b48142 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54122)
store volatile %struct.ScmObj* %b48142, %struct.ScmObj** %stackaddr$prim55352, align 8
%stackaddr$prim55353 = alloca %struct.ScmObj*, align 8
%cpsprim48347 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48143, %struct.ScmObj* %b48142)
store volatile %struct.ScmObj* %cpsprim48347, %struct.ScmObj** %stackaddr$prim55353, align 8
%ae49854 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54124$k483460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55354 = alloca %struct.ScmObj*, align 8
%argslist54124$k483461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48347, %struct.ScmObj* %argslist54124$k483460)
store volatile %struct.ScmObj* %argslist54124$k483461, %struct.ScmObj** %stackaddr$prim55354, align 8
%stackaddr$prim55355 = alloca %struct.ScmObj*, align 8
%argslist54124$k483462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49854, %struct.ScmObj* %argslist54124$k483461)
store volatile %struct.ScmObj* %argslist54124$k483462, %struct.ScmObj** %stackaddr$prim55355, align 8
%clofunc55356 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48346)
musttail call tailcc void %clofunc55356(%struct.ScmObj* %k48346, %struct.ScmObj* %argslist54124$k483462)
ret void
}

define tailcc void @proc_clo$ae49826(%struct.ScmObj* %env$ae49826,%struct.ScmObj* %current_45args54127) {
%stackaddr$prim55357 = alloca %struct.ScmObj*, align 8
%k48348 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54127)
store volatile %struct.ScmObj* %k48348, %struct.ScmObj** %stackaddr$prim55357, align 8
%stackaddr$prim55358 = alloca %struct.ScmObj*, align 8
%current_45args54128 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54127)
store volatile %struct.ScmObj* %current_45args54128, %struct.ScmObj** %stackaddr$prim55358, align 8
%stackaddr$prim55359 = alloca %struct.ScmObj*, align 8
%x48138 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54128)
store volatile %struct.ScmObj* %x48138, %struct.ScmObj** %stackaddr$prim55359, align 8
%stackaddr$prim55360 = alloca %struct.ScmObj*, align 8
%cpsprim48349 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48138)
store volatile %struct.ScmObj* %cpsprim48349, %struct.ScmObj** %stackaddr$prim55360, align 8
%ae49829 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54130$k483480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55361 = alloca %struct.ScmObj*, align 8
%argslist54130$k483481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48349, %struct.ScmObj* %argslist54130$k483480)
store volatile %struct.ScmObj* %argslist54130$k483481, %struct.ScmObj** %stackaddr$prim55361, align 8
%stackaddr$prim55362 = alloca %struct.ScmObj*, align 8
%argslist54130$k483482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49829, %struct.ScmObj* %argslist54130$k483481)
store volatile %struct.ScmObj* %argslist54130$k483482, %struct.ScmObj** %stackaddr$prim55362, align 8
%clofunc55363 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48348)
musttail call tailcc void %clofunc55363(%struct.ScmObj* %k48348, %struct.ScmObj* %argslist54130$k483482)
ret void
}

define tailcc void @proc_clo$ae49802(%struct.ScmObj* %env$ae49802,%struct.ScmObj* %current_45args54133) {
%stackaddr$prim55364 = alloca %struct.ScmObj*, align 8
%k48350 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54133)
store volatile %struct.ScmObj* %k48350, %struct.ScmObj** %stackaddr$prim55364, align 8
%stackaddr$prim55365 = alloca %struct.ScmObj*, align 8
%current_45args54134 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54133)
store volatile %struct.ScmObj* %current_45args54134, %struct.ScmObj** %stackaddr$prim55365, align 8
%stackaddr$prim55366 = alloca %struct.ScmObj*, align 8
%x48140 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54134)
store volatile %struct.ScmObj* %x48140, %struct.ScmObj** %stackaddr$prim55366, align 8
%stackaddr$prim55367 = alloca %struct.ScmObj*, align 8
%cpsprim48351 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48140)
store volatile %struct.ScmObj* %cpsprim48351, %struct.ScmObj** %stackaddr$prim55367, align 8
%ae49805 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54136$k483500 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55368 = alloca %struct.ScmObj*, align 8
%argslist54136$k483501 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48351, %struct.ScmObj* %argslist54136$k483500)
store volatile %struct.ScmObj* %argslist54136$k483501, %struct.ScmObj** %stackaddr$prim55368, align 8
%stackaddr$prim55369 = alloca %struct.ScmObj*, align 8
%argslist54136$k483502 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49805, %struct.ScmObj* %argslist54136$k483501)
store volatile %struct.ScmObj* %argslist54136$k483502, %struct.ScmObj** %stackaddr$prim55369, align 8
%clofunc55370 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48350)
musttail call tailcc void %clofunc55370(%struct.ScmObj* %k48350, %struct.ScmObj* %argslist54136$k483502)
ret void
}

define tailcc void @proc_clo$ae49754(%struct.ScmObj* %env$ae49754,%struct.ScmObj* %current_45args54139) {
%stackaddr$prim55371 = alloca %struct.ScmObj*, align 8
%k48352 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54139)
store volatile %struct.ScmObj* %k48352, %struct.ScmObj** %stackaddr$prim55371, align 8
%stackaddr$prim55372 = alloca %struct.ScmObj*, align 8
%current_45args54140 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54139)
store volatile %struct.ScmObj* %current_45args54140, %struct.ScmObj** %stackaddr$prim55372, align 8
%stackaddr$prim55373 = alloca %struct.ScmObj*, align 8
%lst48136 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54140)
store volatile %struct.ScmObj* %lst48136, %struct.ScmObj** %stackaddr$prim55373, align 8
%stackaddr$prim55374 = alloca %struct.ScmObj*, align 8
%current_45args54141 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54140)
store volatile %struct.ScmObj* %current_45args54141, %struct.ScmObj** %stackaddr$prim55374, align 8
%stackaddr$prim55375 = alloca %struct.ScmObj*, align 8
%b48135 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54141)
store volatile %struct.ScmObj* %b48135, %struct.ScmObj** %stackaddr$prim55375, align 8
%truthy$cmp55376 = call i64 @is_truthy_value(%struct.ScmObj* %b48135)
%cmp$cmp55376 = icmp eq i64 %truthy$cmp55376, 1
br i1 %cmp$cmp55376, label %truebranch$cmp55376, label %falsebranch$cmp55376
truebranch$cmp55376:
%ae49757 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54143$k483520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55377 = alloca %struct.ScmObj*, align 8
%argslist54143$k483521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48135, %struct.ScmObj* %argslist54143$k483520)
store volatile %struct.ScmObj* %argslist54143$k483521, %struct.ScmObj** %stackaddr$prim55377, align 8
%stackaddr$prim55378 = alloca %struct.ScmObj*, align 8
%argslist54143$k483522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49757, %struct.ScmObj* %argslist54143$k483521)
store volatile %struct.ScmObj* %argslist54143$k483522, %struct.ScmObj** %stackaddr$prim55378, align 8
%clofunc55379 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48352)
musttail call tailcc void %clofunc55379(%struct.ScmObj* %k48352, %struct.ScmObj* %argslist54143$k483522)
ret void
falsebranch$cmp55376:
%stackaddr$prim55380 = alloca %struct.ScmObj*, align 8
%cpsprim48353 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48136)
store volatile %struct.ScmObj* %cpsprim48353, %struct.ScmObj** %stackaddr$prim55380, align 8
%ae49764 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54144$k483520 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55381 = alloca %struct.ScmObj*, align 8
%argslist54144$k483521 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48353, %struct.ScmObj* %argslist54144$k483520)
store volatile %struct.ScmObj* %argslist54144$k483521, %struct.ScmObj** %stackaddr$prim55381, align 8
%stackaddr$prim55382 = alloca %struct.ScmObj*, align 8
%argslist54144$k483522 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49764, %struct.ScmObj* %argslist54144$k483521)
store volatile %struct.ScmObj* %argslist54144$k483522, %struct.ScmObj** %stackaddr$prim55382, align 8
%clofunc55383 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48352)
musttail call tailcc void %clofunc55383(%struct.ScmObj* %k48352, %struct.ScmObj* %argslist54144$k483522)
ret void
}

define tailcc void @proc_clo$ae49595(%struct.ScmObj* %env$ae49595,%struct.ScmObj* %args4807448354) {
%stackaddr$env-ref55384 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49595, i64 0)
store %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$env-ref55384
%stackaddr$env-ref55385 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49595, i64 1)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55385
%stackaddr$env-ref55386 = alloca %struct.ScmObj*, align 8
%_37drop_45right48066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49595, i64 2)
store %struct.ScmObj* %_37drop_45right48066, %struct.ScmObj** %stackaddr$env-ref55386
%stackaddr$prim55387 = alloca %struct.ScmObj*, align 8
%k48355 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4807448354)
store volatile %struct.ScmObj* %k48355, %struct.ScmObj** %stackaddr$prim55387, align 8
%stackaddr$prim55388 = alloca %struct.ScmObj*, align 8
%args48074 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4807448354)
store volatile %struct.ScmObj* %args48074, %struct.ScmObj** %stackaddr$prim55388, align 8
%stackaddr$prim55389 = alloca %struct.ScmObj*, align 8
%f48076 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48074)
store volatile %struct.ScmObj* %f48076, %struct.ScmObj** %stackaddr$prim55389, align 8
%stackaddr$prim55390 = alloca %struct.ScmObj*, align 8
%lsts48075 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48074)
store volatile %struct.ScmObj* %lsts48075, %struct.ScmObj** %stackaddr$prim55390, align 8
%stackaddr$makeclosure55391 = alloca %struct.ScmObj*, align 8
%fptrToInt55392 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49600 to i64
%ae49600 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55392)
store volatile %struct.ScmObj* %ae49600, %struct.ScmObj** %stackaddr$makeclosure55391, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49600, %struct.ScmObj* %_37foldr48052, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49600, %struct.ScmObj* %k48355, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49600, %struct.ScmObj* %lsts48075, i64 2)
%ae49601 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55393 = alloca %struct.ScmObj*, align 8
%fptrToInt55394 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49602 to i64
%ae49602 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55394)
store volatile %struct.ScmObj* %ae49602, %struct.ScmObj** %stackaddr$makeclosure55393, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49602, %struct.ScmObj* %_37last48069, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49602, %struct.ScmObj* %_37drop_45right48066, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49602, %struct.ScmObj* %f48076, i64 2)
%argslist54163$ae496000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55395 = alloca %struct.ScmObj*, align 8
%argslist54163$ae496001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49602, %struct.ScmObj* %argslist54163$ae496000)
store volatile %struct.ScmObj* %argslist54163$ae496001, %struct.ScmObj** %stackaddr$prim55395, align 8
%stackaddr$prim55396 = alloca %struct.ScmObj*, align 8
%argslist54163$ae496002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49601, %struct.ScmObj* %argslist54163$ae496001)
store volatile %struct.ScmObj* %argslist54163$ae496002, %struct.ScmObj** %stackaddr$prim55396, align 8
%clofunc55397 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49600)
musttail call tailcc void %clofunc55397(%struct.ScmObj* %ae49600, %struct.ScmObj* %argslist54163$ae496002)
ret void
}

define tailcc void @proc_clo$ae49600(%struct.ScmObj* %env$ae49600,%struct.ScmObj* %current_45args54148) {
%stackaddr$env-ref55398 = alloca %struct.ScmObj*, align 8
%_37foldr48052 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49600, i64 0)
store %struct.ScmObj* %_37foldr48052, %struct.ScmObj** %stackaddr$env-ref55398
%stackaddr$env-ref55399 = alloca %struct.ScmObj*, align 8
%k48355 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49600, i64 1)
store %struct.ScmObj* %k48355, %struct.ScmObj** %stackaddr$env-ref55399
%stackaddr$env-ref55400 = alloca %struct.ScmObj*, align 8
%lsts48075 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49600, i64 2)
store %struct.ScmObj* %lsts48075, %struct.ScmObj** %stackaddr$env-ref55400
%stackaddr$prim55401 = alloca %struct.ScmObj*, align 8
%_95k48356 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54148)
store volatile %struct.ScmObj* %_95k48356, %struct.ScmObj** %stackaddr$prim55401, align 8
%stackaddr$prim55402 = alloca %struct.ScmObj*, align 8
%current_45args54149 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54148)
store volatile %struct.ScmObj* %current_45args54149, %struct.ScmObj** %stackaddr$prim55402, align 8
%stackaddr$prim55403 = alloca %struct.ScmObj*, align 8
%anf_45bind48201 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54149)
store volatile %struct.ScmObj* %anf_45bind48201, %struct.ScmObj** %stackaddr$prim55403, align 8
%ae49663 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55404 = alloca %struct.ScmObj*, align 8
%anf_45bind48202 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49663, %struct.ScmObj* %lsts48075)
store volatile %struct.ScmObj* %anf_45bind48202, %struct.ScmObj** %stackaddr$prim55404, align 8
%stackaddr$prim55405 = alloca %struct.ScmObj*, align 8
%anf_45bind48203 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48201, %struct.ScmObj* %anf_45bind48202)
store volatile %struct.ScmObj* %anf_45bind48203, %struct.ScmObj** %stackaddr$prim55405, align 8
%stackaddr$prim55406 = alloca %struct.ScmObj*, align 8
%cpsargs48357 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48355, %struct.ScmObj* %anf_45bind48203)
store volatile %struct.ScmObj* %cpsargs48357, %struct.ScmObj** %stackaddr$prim55406, align 8
%clofunc55407 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48052)
musttail call tailcc void %clofunc55407(%struct.ScmObj* %_37foldr48052, %struct.ScmObj* %cpsargs48357)
ret void
}

define tailcc void @proc_clo$ae49602(%struct.ScmObj* %env$ae49602,%struct.ScmObj* %fargs4807748358) {
%stackaddr$env-ref55408 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49602, i64 0)
store %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$env-ref55408
%stackaddr$env-ref55409 = alloca %struct.ScmObj*, align 8
%_37drop_45right48066 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49602, i64 1)
store %struct.ScmObj* %_37drop_45right48066, %struct.ScmObj** %stackaddr$env-ref55409
%stackaddr$env-ref55410 = alloca %struct.ScmObj*, align 8
%f48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49602, i64 2)
store %struct.ScmObj* %f48076, %struct.ScmObj** %stackaddr$env-ref55410
%stackaddr$prim55411 = alloca %struct.ScmObj*, align 8
%k48359 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %fargs4807748358)
store volatile %struct.ScmObj* %k48359, %struct.ScmObj** %stackaddr$prim55411, align 8
%stackaddr$prim55412 = alloca %struct.ScmObj*, align 8
%fargs48077 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %fargs4807748358)
store volatile %struct.ScmObj* %fargs48077, %struct.ScmObj** %stackaddr$prim55412, align 8
%stackaddr$makeclosure55413 = alloca %struct.ScmObj*, align 8
%fptrToInt55414 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49606 to i64
%ae49606 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55414)
store volatile %struct.ScmObj* %ae49606, %struct.ScmObj** %stackaddr$makeclosure55413, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49606, %struct.ScmObj* %k48359, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49606, %struct.ScmObj* %_37last48069, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49606, %struct.ScmObj* %fargs48077, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49606, %struct.ScmObj* %f48076, i64 3)
%ae49608 = call %struct.ScmObj* @const_init_int(i64 1)
%argslist54162$_37drop_45right480660 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55415 = alloca %struct.ScmObj*, align 8
%argslist54162$_37drop_45right480661 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49608, %struct.ScmObj* %argslist54162$_37drop_45right480660)
store volatile %struct.ScmObj* %argslist54162$_37drop_45right480661, %struct.ScmObj** %stackaddr$prim55415, align 8
%stackaddr$prim55416 = alloca %struct.ScmObj*, align 8
%argslist54162$_37drop_45right480662 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48077, %struct.ScmObj* %argslist54162$_37drop_45right480661)
store volatile %struct.ScmObj* %argslist54162$_37drop_45right480662, %struct.ScmObj** %stackaddr$prim55416, align 8
%stackaddr$prim55417 = alloca %struct.ScmObj*, align 8
%argslist54162$_37drop_45right480663 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49606, %struct.ScmObj* %argslist54162$_37drop_45right480662)
store volatile %struct.ScmObj* %argslist54162$_37drop_45right480663, %struct.ScmObj** %stackaddr$prim55417, align 8
%clofunc55418 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37drop_45right48066)
musttail call tailcc void %clofunc55418(%struct.ScmObj* %_37drop_45right48066, %struct.ScmObj* %argslist54162$_37drop_45right480663)
ret void
}

define tailcc void @proc_clo$ae49606(%struct.ScmObj* %env$ae49606,%struct.ScmObj* %current_45args54151) {
%stackaddr$env-ref55419 = alloca %struct.ScmObj*, align 8
%k48359 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49606, i64 0)
store %struct.ScmObj* %k48359, %struct.ScmObj** %stackaddr$env-ref55419
%stackaddr$env-ref55420 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49606, i64 1)
store %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$env-ref55420
%stackaddr$env-ref55421 = alloca %struct.ScmObj*, align 8
%fargs48077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49606, i64 2)
store %struct.ScmObj* %fargs48077, %struct.ScmObj** %stackaddr$env-ref55421
%stackaddr$env-ref55422 = alloca %struct.ScmObj*, align 8
%f48076 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49606, i64 3)
store %struct.ScmObj* %f48076, %struct.ScmObj** %stackaddr$env-ref55422
%stackaddr$prim55423 = alloca %struct.ScmObj*, align 8
%_95k48360 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54151)
store volatile %struct.ScmObj* %_95k48360, %struct.ScmObj** %stackaddr$prim55423, align 8
%stackaddr$prim55424 = alloca %struct.ScmObj*, align 8
%current_45args54152 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54151)
store volatile %struct.ScmObj* %current_45args54152, %struct.ScmObj** %stackaddr$prim55424, align 8
%stackaddr$prim55425 = alloca %struct.ScmObj*, align 8
%anf_45bind48198 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54152)
store volatile %struct.ScmObj* %anf_45bind48198, %struct.ScmObj** %stackaddr$prim55425, align 8
%stackaddr$makeclosure55426 = alloca %struct.ScmObj*, align 8
%fptrToInt55427 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49613 to i64
%ae49613 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55427)
store volatile %struct.ScmObj* %ae49613, %struct.ScmObj** %stackaddr$makeclosure55426, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49613, %struct.ScmObj* %k48359, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49613, %struct.ScmObj* %_37last48069, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49613, %struct.ScmObj* %fargs48077, i64 2)
%stackaddr$prim55428 = alloca %struct.ScmObj*, align 8
%cpsargs48364 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49613, %struct.ScmObj* %anf_45bind48198)
store volatile %struct.ScmObj* %cpsargs48364, %struct.ScmObj** %stackaddr$prim55428, align 8
%clofunc55429 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48076)
musttail call tailcc void %clofunc55429(%struct.ScmObj* %f48076, %struct.ScmObj* %cpsargs48364)
ret void
}

define tailcc void @proc_clo$ae49613(%struct.ScmObj* %env$ae49613,%struct.ScmObj* %current_45args54154) {
%stackaddr$env-ref55430 = alloca %struct.ScmObj*, align 8
%k48359 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49613, i64 0)
store %struct.ScmObj* %k48359, %struct.ScmObj** %stackaddr$env-ref55430
%stackaddr$env-ref55431 = alloca %struct.ScmObj*, align 8
%_37last48069 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49613, i64 1)
store %struct.ScmObj* %_37last48069, %struct.ScmObj** %stackaddr$env-ref55431
%stackaddr$env-ref55432 = alloca %struct.ScmObj*, align 8
%fargs48077 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49613, i64 2)
store %struct.ScmObj* %fargs48077, %struct.ScmObj** %stackaddr$env-ref55432
%stackaddr$prim55433 = alloca %struct.ScmObj*, align 8
%_95k48361 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54154)
store volatile %struct.ScmObj* %_95k48361, %struct.ScmObj** %stackaddr$prim55433, align 8
%stackaddr$prim55434 = alloca %struct.ScmObj*, align 8
%current_45args54155 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54154)
store volatile %struct.ScmObj* %current_45args54155, %struct.ScmObj** %stackaddr$prim55434, align 8
%stackaddr$prim55435 = alloca %struct.ScmObj*, align 8
%anf_45bind48199 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54155)
store volatile %struct.ScmObj* %anf_45bind48199, %struct.ScmObj** %stackaddr$prim55435, align 8
%stackaddr$makeclosure55436 = alloca %struct.ScmObj*, align 8
%fptrToInt55437 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49618 to i64
%ae49618 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55437)
store volatile %struct.ScmObj* %ae49618, %struct.ScmObj** %stackaddr$makeclosure55436, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49618, %struct.ScmObj* %k48359, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49618, %struct.ScmObj* %anf_45bind48199, i64 1)
%argslist54161$_37last480690 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55438 = alloca %struct.ScmObj*, align 8
%argslist54161$_37last480691 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %fargs48077, %struct.ScmObj* %argslist54161$_37last480690)
store volatile %struct.ScmObj* %argslist54161$_37last480691, %struct.ScmObj** %stackaddr$prim55438, align 8
%stackaddr$prim55439 = alloca %struct.ScmObj*, align 8
%argslist54161$_37last480692 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49618, %struct.ScmObj* %argslist54161$_37last480691)
store volatile %struct.ScmObj* %argslist54161$_37last480692, %struct.ScmObj** %stackaddr$prim55439, align 8
%clofunc55440 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37last48069)
musttail call tailcc void %clofunc55440(%struct.ScmObj* %_37last48069, %struct.ScmObj* %argslist54161$_37last480692)
ret void
}

define tailcc void @proc_clo$ae49618(%struct.ScmObj* %env$ae49618,%struct.ScmObj* %current_45args54157) {
%stackaddr$env-ref55441 = alloca %struct.ScmObj*, align 8
%k48359 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49618, i64 0)
store %struct.ScmObj* %k48359, %struct.ScmObj** %stackaddr$env-ref55441
%stackaddr$env-ref55442 = alloca %struct.ScmObj*, align 8
%anf_45bind48199 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49618, i64 1)
store %struct.ScmObj* %anf_45bind48199, %struct.ScmObj** %stackaddr$env-ref55442
%stackaddr$prim55443 = alloca %struct.ScmObj*, align 8
%_95k48362 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54157)
store volatile %struct.ScmObj* %_95k48362, %struct.ScmObj** %stackaddr$prim55443, align 8
%stackaddr$prim55444 = alloca %struct.ScmObj*, align 8
%current_45args54158 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54157)
store volatile %struct.ScmObj* %current_45args54158, %struct.ScmObj** %stackaddr$prim55444, align 8
%stackaddr$prim55445 = alloca %struct.ScmObj*, align 8
%anf_45bind48200 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54158)
store volatile %struct.ScmObj* %anf_45bind48200, %struct.ScmObj** %stackaddr$prim55445, align 8
%stackaddr$prim55446 = alloca %struct.ScmObj*, align 8
%cpsprim48363 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48199, %struct.ScmObj* %anf_45bind48200)
store volatile %struct.ScmObj* %cpsprim48363, %struct.ScmObj** %stackaddr$prim55446, align 8
%ae49623 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54160$k483590 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55447 = alloca %struct.ScmObj*, align 8
%argslist54160$k483591 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48363, %struct.ScmObj* %argslist54160$k483590)
store volatile %struct.ScmObj* %argslist54160$k483591, %struct.ScmObj** %stackaddr$prim55447, align 8
%stackaddr$prim55448 = alloca %struct.ScmObj*, align 8
%argslist54160$k483592 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49623, %struct.ScmObj* %argslist54160$k483591)
store volatile %struct.ScmObj* %argslist54160$k483592, %struct.ScmObj** %stackaddr$prim55448, align 8
%clofunc55449 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48359)
musttail call tailcc void %clofunc55449(%struct.ScmObj* %k48359, %struct.ScmObj* %argslist54160$k483592)
ret void
}

define tailcc void @proc_clo$ae49518(%struct.ScmObj* %env$ae49518,%struct.ScmObj* %current_45args54165) {
%stackaddr$env-ref55450 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49518, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55450
%stackaddr$prim55451 = alloca %struct.ScmObj*, align 8
%k48365 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54165)
store volatile %struct.ScmObj* %k48365, %struct.ScmObj** %stackaddr$prim55451, align 8
%stackaddr$prim55452 = alloca %struct.ScmObj*, align 8
%current_45args54166 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54165)
store volatile %struct.ScmObj* %current_45args54166, %struct.ScmObj** %stackaddr$prim55452, align 8
%stackaddr$prim55453 = alloca %struct.ScmObj*, align 8
%f48080 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54166)
store volatile %struct.ScmObj* %f48080, %struct.ScmObj** %stackaddr$prim55453, align 8
%stackaddr$prim55454 = alloca %struct.ScmObj*, align 8
%current_45args54167 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54166)
store volatile %struct.ScmObj* %current_45args54167, %struct.ScmObj** %stackaddr$prim55454, align 8
%stackaddr$prim55455 = alloca %struct.ScmObj*, align 8
%lst48079 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54167)
store volatile %struct.ScmObj* %lst48079, %struct.ScmObj** %stackaddr$prim55455, align 8
%stackaddr$makeclosure55456 = alloca %struct.ScmObj*, align 8
%fptrToInt55457 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49519 to i64
%ae49519 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55457)
store volatile %struct.ScmObj* %ae49519, %struct.ScmObj** %stackaddr$makeclosure55456, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49519, %struct.ScmObj* %lst48079, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49519, %struct.ScmObj* %_37foldr148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49519, %struct.ScmObj* %k48365, i64 2)
%ae49520 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55458 = alloca %struct.ScmObj*, align 8
%fptrToInt55459 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49521 to i64
%ae49521 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55459)
store volatile %struct.ScmObj* %ae49521, %struct.ScmObj** %stackaddr$makeclosure55458, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49521, %struct.ScmObj* %f48080, i64 0)
%argslist54182$ae495190 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55460 = alloca %struct.ScmObj*, align 8
%argslist54182$ae495191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49521, %struct.ScmObj* %argslist54182$ae495190)
store volatile %struct.ScmObj* %argslist54182$ae495191, %struct.ScmObj** %stackaddr$prim55460, align 8
%stackaddr$prim55461 = alloca %struct.ScmObj*, align 8
%argslist54182$ae495192 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49520, %struct.ScmObj* %argslist54182$ae495191)
store volatile %struct.ScmObj* %argslist54182$ae495192, %struct.ScmObj** %stackaddr$prim55461, align 8
%clofunc55462 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49519)
musttail call tailcc void %clofunc55462(%struct.ScmObj* %ae49519, %struct.ScmObj* %argslist54182$ae495192)
ret void
}

define tailcc void @proc_clo$ae49519(%struct.ScmObj* %env$ae49519,%struct.ScmObj* %current_45args54169) {
%stackaddr$env-ref55463 = alloca %struct.ScmObj*, align 8
%lst48079 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49519, i64 0)
store %struct.ScmObj* %lst48079, %struct.ScmObj** %stackaddr$env-ref55463
%stackaddr$env-ref55464 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49519, i64 1)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55464
%stackaddr$env-ref55465 = alloca %struct.ScmObj*, align 8
%k48365 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49519, i64 2)
store %struct.ScmObj* %k48365, %struct.ScmObj** %stackaddr$env-ref55465
%stackaddr$prim55466 = alloca %struct.ScmObj*, align 8
%_95k48366 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54169)
store volatile %struct.ScmObj* %_95k48366, %struct.ScmObj** %stackaddr$prim55466, align 8
%stackaddr$prim55467 = alloca %struct.ScmObj*, align 8
%current_45args54170 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54169)
store volatile %struct.ScmObj* %current_45args54170, %struct.ScmObj** %stackaddr$prim55467, align 8
%stackaddr$prim55468 = alloca %struct.ScmObj*, align 8
%anf_45bind48197 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54170)
store volatile %struct.ScmObj* %anf_45bind48197, %struct.ScmObj** %stackaddr$prim55468, align 8
%ae49553 = call %struct.ScmObj* @const_init_null()
%argslist54172$_37foldr1480470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55469 = alloca %struct.ScmObj*, align 8
%argslist54172$_37foldr1480471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48079, %struct.ScmObj* %argslist54172$_37foldr1480470)
store volatile %struct.ScmObj* %argslist54172$_37foldr1480471, %struct.ScmObj** %stackaddr$prim55469, align 8
%stackaddr$prim55470 = alloca %struct.ScmObj*, align 8
%argslist54172$_37foldr1480472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49553, %struct.ScmObj* %argslist54172$_37foldr1480471)
store volatile %struct.ScmObj* %argslist54172$_37foldr1480472, %struct.ScmObj** %stackaddr$prim55470, align 8
%stackaddr$prim55471 = alloca %struct.ScmObj*, align 8
%argslist54172$_37foldr1480473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48197, %struct.ScmObj* %argslist54172$_37foldr1480472)
store volatile %struct.ScmObj* %argslist54172$_37foldr1480473, %struct.ScmObj** %stackaddr$prim55471, align 8
%stackaddr$prim55472 = alloca %struct.ScmObj*, align 8
%argslist54172$_37foldr1480474 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48365, %struct.ScmObj* %argslist54172$_37foldr1480473)
store volatile %struct.ScmObj* %argslist54172$_37foldr1480474, %struct.ScmObj** %stackaddr$prim55472, align 8
%clofunc55473 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148047)
musttail call tailcc void %clofunc55473(%struct.ScmObj* %_37foldr148047, %struct.ScmObj* %argslist54172$_37foldr1480474)
ret void
}

define tailcc void @proc_clo$ae49521(%struct.ScmObj* %env$ae49521,%struct.ScmObj* %current_45args54173) {
%stackaddr$env-ref55474 = alloca %struct.ScmObj*, align 8
%f48080 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49521, i64 0)
store %struct.ScmObj* %f48080, %struct.ScmObj** %stackaddr$env-ref55474
%stackaddr$prim55475 = alloca %struct.ScmObj*, align 8
%k48367 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54173)
store volatile %struct.ScmObj* %k48367, %struct.ScmObj** %stackaddr$prim55475, align 8
%stackaddr$prim55476 = alloca %struct.ScmObj*, align 8
%current_45args54174 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54173)
store volatile %struct.ScmObj* %current_45args54174, %struct.ScmObj** %stackaddr$prim55476, align 8
%stackaddr$prim55477 = alloca %struct.ScmObj*, align 8
%v48082 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54174)
store volatile %struct.ScmObj* %v48082, %struct.ScmObj** %stackaddr$prim55477, align 8
%stackaddr$prim55478 = alloca %struct.ScmObj*, align 8
%current_45args54175 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54174)
store volatile %struct.ScmObj* %current_45args54175, %struct.ScmObj** %stackaddr$prim55478, align 8
%stackaddr$prim55479 = alloca %struct.ScmObj*, align 8
%r48081 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54175)
store volatile %struct.ScmObj* %r48081, %struct.ScmObj** %stackaddr$prim55479, align 8
%stackaddr$makeclosure55480 = alloca %struct.ScmObj*, align 8
%fptrToInt55481 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49523 to i64
%ae49523 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55481)
store volatile %struct.ScmObj* %ae49523, %struct.ScmObj** %stackaddr$makeclosure55480, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49523, %struct.ScmObj* %r48081, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49523, %struct.ScmObj* %k48367, i64 1)
%argslist54181$f480800 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55482 = alloca %struct.ScmObj*, align 8
%argslist54181$f480801 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %v48082, %struct.ScmObj* %argslist54181$f480800)
store volatile %struct.ScmObj* %argslist54181$f480801, %struct.ScmObj** %stackaddr$prim55482, align 8
%stackaddr$prim55483 = alloca %struct.ScmObj*, align 8
%argslist54181$f480802 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49523, %struct.ScmObj* %argslist54181$f480801)
store volatile %struct.ScmObj* %argslist54181$f480802, %struct.ScmObj** %stackaddr$prim55483, align 8
%clofunc55484 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48080)
musttail call tailcc void %clofunc55484(%struct.ScmObj* %f48080, %struct.ScmObj* %argslist54181$f480802)
ret void
}

define tailcc void @proc_clo$ae49523(%struct.ScmObj* %env$ae49523,%struct.ScmObj* %current_45args54177) {
%stackaddr$env-ref55485 = alloca %struct.ScmObj*, align 8
%r48081 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49523, i64 0)
store %struct.ScmObj* %r48081, %struct.ScmObj** %stackaddr$env-ref55485
%stackaddr$env-ref55486 = alloca %struct.ScmObj*, align 8
%k48367 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49523, i64 1)
store %struct.ScmObj* %k48367, %struct.ScmObj** %stackaddr$env-ref55486
%stackaddr$prim55487 = alloca %struct.ScmObj*, align 8
%_95k48368 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54177)
store volatile %struct.ScmObj* %_95k48368, %struct.ScmObj** %stackaddr$prim55487, align 8
%stackaddr$prim55488 = alloca %struct.ScmObj*, align 8
%current_45args54178 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54177)
store volatile %struct.ScmObj* %current_45args54178, %struct.ScmObj** %stackaddr$prim55488, align 8
%stackaddr$prim55489 = alloca %struct.ScmObj*, align 8
%anf_45bind48196 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54178)
store volatile %struct.ScmObj* %anf_45bind48196, %struct.ScmObj** %stackaddr$prim55489, align 8
%stackaddr$prim55490 = alloca %struct.ScmObj*, align 8
%cpsprim48369 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48196, %struct.ScmObj* %r48081)
store volatile %struct.ScmObj* %cpsprim48369, %struct.ScmObj** %stackaddr$prim55490, align 8
%ae49528 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54180$k483670 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55491 = alloca %struct.ScmObj*, align 8
%argslist54180$k483671 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48369, %struct.ScmObj* %argslist54180$k483670)
store volatile %struct.ScmObj* %argslist54180$k483671, %struct.ScmObj** %stackaddr$prim55491, align 8
%stackaddr$prim55492 = alloca %struct.ScmObj*, align 8
%argslist54180$k483672 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49528, %struct.ScmObj* %argslist54180$k483671)
store volatile %struct.ScmObj* %argslist54180$k483672, %struct.ScmObj** %stackaddr$prim55492, align 8
%clofunc55493 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48367)
musttail call tailcc void %clofunc55493(%struct.ScmObj* %k48367, %struct.ScmObj* %argslist54180$k483672)
ret void
}

define tailcc void @proc_clo$ae49132(%struct.ScmObj* %env$ae49132,%struct.ScmObj* %current_45args54185) {
%stackaddr$env-ref55494 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49132, i64 0)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55494
%stackaddr$env-ref55495 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49132, i64 1)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref55495
%stackaddr$prim55496 = alloca %struct.ScmObj*, align 8
%k48370 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54185)
store volatile %struct.ScmObj* %k48370, %struct.ScmObj** %stackaddr$prim55496, align 8
%stackaddr$prim55497 = alloca %struct.ScmObj*, align 8
%current_45args54186 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54185)
store volatile %struct.ScmObj* %current_45args54186, %struct.ScmObj** %stackaddr$prim55497, align 8
%stackaddr$prim55498 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54186)
store volatile %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$prim55498, align 8
%ae49134 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55499 = alloca %struct.ScmObj*, align 8
%fptrToInt55500 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49135 to i64
%ae49135 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55500)
store volatile %struct.ScmObj* %ae49135, %struct.ScmObj** %stackaddr$makeclosure55499, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49135, %struct.ScmObj* %_37foldr48053, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49135, %struct.ScmObj* %_37foldr148047, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49135, %struct.ScmObj* %_37map148043, i64 2)
%argslist54243$k483700 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55501 = alloca %struct.ScmObj*, align 8
%argslist54243$k483701 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49135, %struct.ScmObj* %argslist54243$k483700)
store volatile %struct.ScmObj* %argslist54243$k483701, %struct.ScmObj** %stackaddr$prim55501, align 8
%stackaddr$prim55502 = alloca %struct.ScmObj*, align 8
%argslist54243$k483702 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49134, %struct.ScmObj* %argslist54243$k483701)
store volatile %struct.ScmObj* %argslist54243$k483702, %struct.ScmObj** %stackaddr$prim55502, align 8
%clofunc55503 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48370)
musttail call tailcc void %clofunc55503(%struct.ScmObj* %k48370, %struct.ScmObj* %argslist54243$k483702)
ret void
}

define tailcc void @proc_clo$ae49135(%struct.ScmObj* %env$ae49135,%struct.ScmObj* %args4805448371) {
%stackaddr$env-ref55504 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49135, i64 0)
store %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$env-ref55504
%stackaddr$env-ref55505 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49135, i64 1)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55505
%stackaddr$env-ref55506 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49135, i64 2)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref55506
%stackaddr$prim55507 = alloca %struct.ScmObj*, align 8
%k48372 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4805448371)
store volatile %struct.ScmObj* %k48372, %struct.ScmObj** %stackaddr$prim55507, align 8
%stackaddr$prim55508 = alloca %struct.ScmObj*, align 8
%args48054 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4805448371)
store volatile %struct.ScmObj* %args48054, %struct.ScmObj** %stackaddr$prim55508, align 8
%stackaddr$prim55509 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args48054)
store volatile %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$prim55509, align 8
%stackaddr$prim55510 = alloca %struct.ScmObj*, align 8
%anf_45bind48183 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48054)
store volatile %struct.ScmObj* %anf_45bind48183, %struct.ScmObj** %stackaddr$prim55510, align 8
%stackaddr$prim55511 = alloca %struct.ScmObj*, align 8
%acc48056 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %anf_45bind48183)
store volatile %struct.ScmObj* %acc48056, %struct.ScmObj** %stackaddr$prim55511, align 8
%stackaddr$prim55512 = alloca %struct.ScmObj*, align 8
%anf_45bind48184 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args48054)
store volatile %struct.ScmObj* %anf_45bind48184, %struct.ScmObj** %stackaddr$prim55512, align 8
%stackaddr$prim55513 = alloca %struct.ScmObj*, align 8
%lsts48055 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %anf_45bind48184)
store volatile %struct.ScmObj* %lsts48055, %struct.ScmObj** %stackaddr$prim55513, align 8
%stackaddr$makeclosure55514 = alloca %struct.ScmObj*, align 8
%fptrToInt55515 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49143 to i64
%ae49143 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55515)
store volatile %struct.ScmObj* %ae49143, %struct.ScmObj** %stackaddr$makeclosure55514, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49143, %struct.ScmObj* %f48057, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49143, %struct.ScmObj* %acc48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49143, %struct.ScmObj* %lsts48055, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49143, %struct.ScmObj* %_37foldr48053, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49143, %struct.ScmObj* %k48372, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49143, %struct.ScmObj* %_37foldr148047, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49143, %struct.ScmObj* %_37map148043, i64 6)
%ae49144 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55516 = alloca %struct.ScmObj*, align 8
%fptrToInt55517 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49145 to i64
%ae49145 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55517)
store volatile %struct.ScmObj* %ae49145, %struct.ScmObj** %stackaddr$makeclosure55516, align 8
%argslist54242$ae491430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55518 = alloca %struct.ScmObj*, align 8
%argslist54242$ae491431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49145, %struct.ScmObj* %argslist54242$ae491430)
store volatile %struct.ScmObj* %argslist54242$ae491431, %struct.ScmObj** %stackaddr$prim55518, align 8
%stackaddr$prim55519 = alloca %struct.ScmObj*, align 8
%argslist54242$ae491432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49144, %struct.ScmObj* %argslist54242$ae491431)
store volatile %struct.ScmObj* %argslist54242$ae491432, %struct.ScmObj** %stackaddr$prim55519, align 8
%clofunc55520 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49143)
musttail call tailcc void %clofunc55520(%struct.ScmObj* %ae49143, %struct.ScmObj* %argslist54242$ae491432)
ret void
}

define tailcc void @proc_clo$ae49143(%struct.ScmObj* %env$ae49143,%struct.ScmObj* %current_45args54188) {
%stackaddr$env-ref55521 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49143, i64 0)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref55521
%stackaddr$env-ref55522 = alloca %struct.ScmObj*, align 8
%acc48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49143, i64 1)
store %struct.ScmObj* %acc48056, %struct.ScmObj** %stackaddr$env-ref55522
%stackaddr$env-ref55523 = alloca %struct.ScmObj*, align 8
%lsts48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49143, i64 2)
store %struct.ScmObj* %lsts48055, %struct.ScmObj** %stackaddr$env-ref55523
%stackaddr$env-ref55524 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49143, i64 3)
store %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$env-ref55524
%stackaddr$env-ref55525 = alloca %struct.ScmObj*, align 8
%k48372 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49143, i64 4)
store %struct.ScmObj* %k48372, %struct.ScmObj** %stackaddr$env-ref55525
%stackaddr$env-ref55526 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49143, i64 5)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55526
%stackaddr$env-ref55527 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49143, i64 6)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref55527
%stackaddr$prim55528 = alloca %struct.ScmObj*, align 8
%_95k48373 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54188)
store volatile %struct.ScmObj* %_95k48373, %struct.ScmObj** %stackaddr$prim55528, align 8
%stackaddr$prim55529 = alloca %struct.ScmObj*, align 8
%current_45args54189 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54188)
store volatile %struct.ScmObj* %current_45args54189, %struct.ScmObj** %stackaddr$prim55529, align 8
%stackaddr$prim55530 = alloca %struct.ScmObj*, align 8
%anf_45bind48185 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54189)
store volatile %struct.ScmObj* %anf_45bind48185, %struct.ScmObj** %stackaddr$prim55530, align 8
%stackaddr$makeclosure55531 = alloca %struct.ScmObj*, align 8
%fptrToInt55532 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49175 to i64
%ae49175 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55532)
store volatile %struct.ScmObj* %ae49175, %struct.ScmObj** %stackaddr$makeclosure55531, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49175, %struct.ScmObj* %f48057, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49175, %struct.ScmObj* %acc48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49175, %struct.ScmObj* %lsts48055, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49175, %struct.ScmObj* %_37foldr48053, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49175, %struct.ScmObj* %k48372, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49175, %struct.ScmObj* %_37foldr148047, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49175, %struct.ScmObj* %_37map148043, i64 6)
%ae49177 = call %struct.ScmObj* @const_init_false()
%argslist54235$_37foldr1480470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55533 = alloca %struct.ScmObj*, align 8
%argslist54235$_37foldr1480471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48055, %struct.ScmObj* %argslist54235$_37foldr1480470)
store volatile %struct.ScmObj* %argslist54235$_37foldr1480471, %struct.ScmObj** %stackaddr$prim55533, align 8
%stackaddr$prim55534 = alloca %struct.ScmObj*, align 8
%argslist54235$_37foldr1480472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49177, %struct.ScmObj* %argslist54235$_37foldr1480471)
store volatile %struct.ScmObj* %argslist54235$_37foldr1480472, %struct.ScmObj** %stackaddr$prim55534, align 8
%stackaddr$prim55535 = alloca %struct.ScmObj*, align 8
%argslist54235$_37foldr1480473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48185, %struct.ScmObj* %argslist54235$_37foldr1480472)
store volatile %struct.ScmObj* %argslist54235$_37foldr1480473, %struct.ScmObj** %stackaddr$prim55535, align 8
%stackaddr$prim55536 = alloca %struct.ScmObj*, align 8
%argslist54235$_37foldr1480474 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49175, %struct.ScmObj* %argslist54235$_37foldr1480473)
store volatile %struct.ScmObj* %argslist54235$_37foldr1480474, %struct.ScmObj** %stackaddr$prim55536, align 8
%clofunc55537 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148047)
musttail call tailcc void %clofunc55537(%struct.ScmObj* %_37foldr148047, %struct.ScmObj* %argslist54235$_37foldr1480474)
ret void
}

define tailcc void @proc_clo$ae49175(%struct.ScmObj* %env$ae49175,%struct.ScmObj* %current_45args54191) {
%stackaddr$env-ref55538 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49175, i64 0)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref55538
%stackaddr$env-ref55539 = alloca %struct.ScmObj*, align 8
%acc48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49175, i64 1)
store %struct.ScmObj* %acc48056, %struct.ScmObj** %stackaddr$env-ref55539
%stackaddr$env-ref55540 = alloca %struct.ScmObj*, align 8
%lsts48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49175, i64 2)
store %struct.ScmObj* %lsts48055, %struct.ScmObj** %stackaddr$env-ref55540
%stackaddr$env-ref55541 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49175, i64 3)
store %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$env-ref55541
%stackaddr$env-ref55542 = alloca %struct.ScmObj*, align 8
%k48372 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49175, i64 4)
store %struct.ScmObj* %k48372, %struct.ScmObj** %stackaddr$env-ref55542
%stackaddr$env-ref55543 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49175, i64 5)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55543
%stackaddr$env-ref55544 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49175, i64 6)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref55544
%stackaddr$prim55545 = alloca %struct.ScmObj*, align 8
%_95k48374 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54191)
store volatile %struct.ScmObj* %_95k48374, %struct.ScmObj** %stackaddr$prim55545, align 8
%stackaddr$prim55546 = alloca %struct.ScmObj*, align 8
%current_45args54192 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54191)
store volatile %struct.ScmObj* %current_45args54192, %struct.ScmObj** %stackaddr$prim55546, align 8
%stackaddr$prim55547 = alloca %struct.ScmObj*, align 8
%anf_45bind48186 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54192)
store volatile %struct.ScmObj* %anf_45bind48186, %struct.ScmObj** %stackaddr$prim55547, align 8
%truthy$cmp55548 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48186)
%cmp$cmp55548 = icmp eq i64 %truthy$cmp55548, 1
br i1 %cmp$cmp55548, label %truebranch$cmp55548, label %falsebranch$cmp55548
truebranch$cmp55548:
%ae49186 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54194$k483720 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55549 = alloca %struct.ScmObj*, align 8
%argslist54194$k483721 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48056, %struct.ScmObj* %argslist54194$k483720)
store volatile %struct.ScmObj* %argslist54194$k483721, %struct.ScmObj** %stackaddr$prim55549, align 8
%stackaddr$prim55550 = alloca %struct.ScmObj*, align 8
%argslist54194$k483722 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49186, %struct.ScmObj* %argslist54194$k483721)
store volatile %struct.ScmObj* %argslist54194$k483722, %struct.ScmObj** %stackaddr$prim55550, align 8
%clofunc55551 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48372)
musttail call tailcc void %clofunc55551(%struct.ScmObj* %k48372, %struct.ScmObj* %argslist54194$k483722)
ret void
falsebranch$cmp55548:
%stackaddr$makeclosure55552 = alloca %struct.ScmObj*, align 8
%fptrToInt55553 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49191 to i64
%ae49191 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55553)
store volatile %struct.ScmObj* %ae49191, %struct.ScmObj** %stackaddr$makeclosure55552, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49191, %struct.ScmObj* %f48057, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49191, %struct.ScmObj* %acc48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49191, %struct.ScmObj* %lsts48055, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49191, %struct.ScmObj* %_37foldr48053, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49191, %struct.ScmObj* %k48372, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49191, %struct.ScmObj* %_37foldr148047, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49191, %struct.ScmObj* %_37map148043, i64 6)
%ae49192 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55554 = alloca %struct.ScmObj*, align 8
%fptrToInt55555 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49193 to i64
%ae49193 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55555)
store volatile %struct.ScmObj* %ae49193, %struct.ScmObj** %stackaddr$makeclosure55554, align 8
%argslist54234$ae491910 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55556 = alloca %struct.ScmObj*, align 8
%argslist54234$ae491911 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49193, %struct.ScmObj* %argslist54234$ae491910)
store volatile %struct.ScmObj* %argslist54234$ae491911, %struct.ScmObj** %stackaddr$prim55556, align 8
%stackaddr$prim55557 = alloca %struct.ScmObj*, align 8
%argslist54234$ae491912 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49192, %struct.ScmObj* %argslist54234$ae491911)
store volatile %struct.ScmObj* %argslist54234$ae491912, %struct.ScmObj** %stackaddr$prim55557, align 8
%clofunc55558 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49191)
musttail call tailcc void %clofunc55558(%struct.ScmObj* %ae49191, %struct.ScmObj* %argslist54234$ae491912)
ret void
}

define tailcc void @proc_clo$ae49191(%struct.ScmObj* %env$ae49191,%struct.ScmObj* %current_45args54195) {
%stackaddr$env-ref55559 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49191, i64 0)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref55559
%stackaddr$env-ref55560 = alloca %struct.ScmObj*, align 8
%acc48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49191, i64 1)
store %struct.ScmObj* %acc48056, %struct.ScmObj** %stackaddr$env-ref55560
%stackaddr$env-ref55561 = alloca %struct.ScmObj*, align 8
%lsts48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49191, i64 2)
store %struct.ScmObj* %lsts48055, %struct.ScmObj** %stackaddr$env-ref55561
%stackaddr$env-ref55562 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49191, i64 3)
store %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$env-ref55562
%stackaddr$env-ref55563 = alloca %struct.ScmObj*, align 8
%k48372 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49191, i64 4)
store %struct.ScmObj* %k48372, %struct.ScmObj** %stackaddr$env-ref55563
%stackaddr$env-ref55564 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49191, i64 5)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55564
%stackaddr$env-ref55565 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49191, i64 6)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref55565
%stackaddr$prim55566 = alloca %struct.ScmObj*, align 8
%_95k48375 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54195)
store volatile %struct.ScmObj* %_95k48375, %struct.ScmObj** %stackaddr$prim55566, align 8
%stackaddr$prim55567 = alloca %struct.ScmObj*, align 8
%current_45args54196 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54195)
store volatile %struct.ScmObj* %current_45args54196, %struct.ScmObj** %stackaddr$prim55567, align 8
%stackaddr$prim55568 = alloca %struct.ScmObj*, align 8
%anf_45bind48187 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54196)
store volatile %struct.ScmObj* %anf_45bind48187, %struct.ScmObj** %stackaddr$prim55568, align 8
%stackaddr$makeclosure55569 = alloca %struct.ScmObj*, align 8
%fptrToInt55570 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49212 to i64
%ae49212 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55570)
store volatile %struct.ScmObj* %ae49212, %struct.ScmObj** %stackaddr$makeclosure55569, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49212, %struct.ScmObj* %f48057, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49212, %struct.ScmObj* %acc48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49212, %struct.ScmObj* %lsts48055, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49212, %struct.ScmObj* %_37foldr48053, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49212, %struct.ScmObj* %k48372, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49212, %struct.ScmObj* %_37foldr148047, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49212, %struct.ScmObj* %_37map148043, i64 6)
%argslist54229$_37map1480430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55571 = alloca %struct.ScmObj*, align 8
%argslist54229$_37map1480431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48055, %struct.ScmObj* %argslist54229$_37map1480430)
store volatile %struct.ScmObj* %argslist54229$_37map1480431, %struct.ScmObj** %stackaddr$prim55571, align 8
%stackaddr$prim55572 = alloca %struct.ScmObj*, align 8
%argslist54229$_37map1480432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48187, %struct.ScmObj* %argslist54229$_37map1480431)
store volatile %struct.ScmObj* %argslist54229$_37map1480432, %struct.ScmObj** %stackaddr$prim55572, align 8
%stackaddr$prim55573 = alloca %struct.ScmObj*, align 8
%argslist54229$_37map1480433 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49212, %struct.ScmObj* %argslist54229$_37map1480432)
store volatile %struct.ScmObj* %argslist54229$_37map1480433, %struct.ScmObj** %stackaddr$prim55573, align 8
%clofunc55574 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148043)
musttail call tailcc void %clofunc55574(%struct.ScmObj* %_37map148043, %struct.ScmObj* %argslist54229$_37map1480433)
ret void
}

define tailcc void @proc_clo$ae49212(%struct.ScmObj* %env$ae49212,%struct.ScmObj* %current_45args54198) {
%stackaddr$env-ref55575 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49212, i64 0)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref55575
%stackaddr$env-ref55576 = alloca %struct.ScmObj*, align 8
%acc48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49212, i64 1)
store %struct.ScmObj* %acc48056, %struct.ScmObj** %stackaddr$env-ref55576
%stackaddr$env-ref55577 = alloca %struct.ScmObj*, align 8
%lsts48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49212, i64 2)
store %struct.ScmObj* %lsts48055, %struct.ScmObj** %stackaddr$env-ref55577
%stackaddr$env-ref55578 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49212, i64 3)
store %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$env-ref55578
%stackaddr$env-ref55579 = alloca %struct.ScmObj*, align 8
%k48372 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49212, i64 4)
store %struct.ScmObj* %k48372, %struct.ScmObj** %stackaddr$env-ref55579
%stackaddr$env-ref55580 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49212, i64 5)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55580
%stackaddr$env-ref55581 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49212, i64 6)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref55581
%stackaddr$prim55582 = alloca %struct.ScmObj*, align 8
%_95k48376 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54198)
store volatile %struct.ScmObj* %_95k48376, %struct.ScmObj** %stackaddr$prim55582, align 8
%stackaddr$prim55583 = alloca %struct.ScmObj*, align 8
%current_45args54199 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54198)
store volatile %struct.ScmObj* %current_45args54199, %struct.ScmObj** %stackaddr$prim55583, align 8
%stackaddr$prim55584 = alloca %struct.ScmObj*, align 8
%lsts_4348062 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54199)
store volatile %struct.ScmObj* %lsts_4348062, %struct.ScmObj** %stackaddr$prim55584, align 8
%stackaddr$makeclosure55585 = alloca %struct.ScmObj*, align 8
%fptrToInt55586 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49215 to i64
%ae49215 = call %struct.ScmObj* @closure_alloc(i64 8, i64 %fptrToInt55586)
store volatile %struct.ScmObj* %ae49215, %struct.ScmObj** %stackaddr$makeclosure55585, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49215, %struct.ScmObj* %f48057, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49215, %struct.ScmObj* %acc48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49215, %struct.ScmObj* %lsts48055, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49215, %struct.ScmObj* %_37foldr48053, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49215, %struct.ScmObj* %k48372, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49215, %struct.ScmObj* %_37foldr148047, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49215, %struct.ScmObj* %lsts_4348062, i64 6)
call void @closure_place_freevar(%struct.ScmObj* %ae49215, %struct.ScmObj* %_37map148043, i64 7)
%ae49216 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55587 = alloca %struct.ScmObj*, align 8
%fptrToInt55588 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49217 to i64
%ae49217 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55588)
store volatile %struct.ScmObj* %ae49217, %struct.ScmObj** %stackaddr$makeclosure55587, align 8
%argslist54228$ae492150 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55589 = alloca %struct.ScmObj*, align 8
%argslist54228$ae492151 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49217, %struct.ScmObj* %argslist54228$ae492150)
store volatile %struct.ScmObj* %argslist54228$ae492151, %struct.ScmObj** %stackaddr$prim55589, align 8
%stackaddr$prim55590 = alloca %struct.ScmObj*, align 8
%argslist54228$ae492152 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49216, %struct.ScmObj* %argslist54228$ae492151)
store volatile %struct.ScmObj* %argslist54228$ae492152, %struct.ScmObj** %stackaddr$prim55590, align 8
%clofunc55591 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49215)
musttail call tailcc void %clofunc55591(%struct.ScmObj* %ae49215, %struct.ScmObj* %argslist54228$ae492152)
ret void
}

define tailcc void @proc_clo$ae49215(%struct.ScmObj* %env$ae49215,%struct.ScmObj* %current_45args54201) {
%stackaddr$env-ref55592 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49215, i64 0)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref55592
%stackaddr$env-ref55593 = alloca %struct.ScmObj*, align 8
%acc48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49215, i64 1)
store %struct.ScmObj* %acc48056, %struct.ScmObj** %stackaddr$env-ref55593
%stackaddr$env-ref55594 = alloca %struct.ScmObj*, align 8
%lsts48055 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49215, i64 2)
store %struct.ScmObj* %lsts48055, %struct.ScmObj** %stackaddr$env-ref55594
%stackaddr$env-ref55595 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49215, i64 3)
store %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$env-ref55595
%stackaddr$env-ref55596 = alloca %struct.ScmObj*, align 8
%k48372 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49215, i64 4)
store %struct.ScmObj* %k48372, %struct.ScmObj** %stackaddr$env-ref55596
%stackaddr$env-ref55597 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49215, i64 5)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55597
%stackaddr$env-ref55598 = alloca %struct.ScmObj*, align 8
%lsts_4348062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49215, i64 6)
store %struct.ScmObj* %lsts_4348062, %struct.ScmObj** %stackaddr$env-ref55598
%stackaddr$env-ref55599 = alloca %struct.ScmObj*, align 8
%_37map148043 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49215, i64 7)
store %struct.ScmObj* %_37map148043, %struct.ScmObj** %stackaddr$env-ref55599
%stackaddr$prim55600 = alloca %struct.ScmObj*, align 8
%_95k48377 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54201)
store volatile %struct.ScmObj* %_95k48377, %struct.ScmObj** %stackaddr$prim55600, align 8
%stackaddr$prim55601 = alloca %struct.ScmObj*, align 8
%current_45args54202 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54201)
store volatile %struct.ScmObj* %current_45args54202, %struct.ScmObj** %stackaddr$prim55601, align 8
%stackaddr$prim55602 = alloca %struct.ScmObj*, align 8
%anf_45bind48188 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54202)
store volatile %struct.ScmObj* %anf_45bind48188, %struct.ScmObj** %stackaddr$prim55602, align 8
%stackaddr$makeclosure55603 = alloca %struct.ScmObj*, align 8
%fptrToInt55604 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49236 to i64
%ae49236 = call %struct.ScmObj* @closure_alloc(i64 6, i64 %fptrToInt55604)
store volatile %struct.ScmObj* %ae49236, %struct.ScmObj** %stackaddr$makeclosure55603, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49236, %struct.ScmObj* %f48057, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49236, %struct.ScmObj* %acc48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49236, %struct.ScmObj* %_37foldr48053, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49236, %struct.ScmObj* %k48372, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49236, %struct.ScmObj* %_37foldr148047, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49236, %struct.ScmObj* %lsts_4348062, i64 5)
%argslist54223$_37map1480430 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55605 = alloca %struct.ScmObj*, align 8
%argslist54223$_37map1480431 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lsts48055, %struct.ScmObj* %argslist54223$_37map1480430)
store volatile %struct.ScmObj* %argslist54223$_37map1480431, %struct.ScmObj** %stackaddr$prim55605, align 8
%stackaddr$prim55606 = alloca %struct.ScmObj*, align 8
%argslist54223$_37map1480432 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48188, %struct.ScmObj* %argslist54223$_37map1480431)
store volatile %struct.ScmObj* %argslist54223$_37map1480432, %struct.ScmObj** %stackaddr$prim55606, align 8
%stackaddr$prim55607 = alloca %struct.ScmObj*, align 8
%argslist54223$_37map1480433 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49236, %struct.ScmObj* %argslist54223$_37map1480432)
store volatile %struct.ScmObj* %argslist54223$_37map1480433, %struct.ScmObj** %stackaddr$prim55607, align 8
%clofunc55608 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map148043)
musttail call tailcc void %clofunc55608(%struct.ScmObj* %_37map148043, %struct.ScmObj* %argslist54223$_37map1480433)
ret void
}

define tailcc void @proc_clo$ae49236(%struct.ScmObj* %env$ae49236,%struct.ScmObj* %current_45args54204) {
%stackaddr$env-ref55609 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49236, i64 0)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref55609
%stackaddr$env-ref55610 = alloca %struct.ScmObj*, align 8
%acc48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49236, i64 1)
store %struct.ScmObj* %acc48056, %struct.ScmObj** %stackaddr$env-ref55610
%stackaddr$env-ref55611 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49236, i64 2)
store %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$env-ref55611
%stackaddr$env-ref55612 = alloca %struct.ScmObj*, align 8
%k48372 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49236, i64 3)
store %struct.ScmObj* %k48372, %struct.ScmObj** %stackaddr$env-ref55612
%stackaddr$env-ref55613 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49236, i64 4)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55613
%stackaddr$env-ref55614 = alloca %struct.ScmObj*, align 8
%lsts_4348062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49236, i64 5)
store %struct.ScmObj* %lsts_4348062, %struct.ScmObj** %stackaddr$env-ref55614
%stackaddr$prim55615 = alloca %struct.ScmObj*, align 8
%_95k48378 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54204)
store volatile %struct.ScmObj* %_95k48378, %struct.ScmObj** %stackaddr$prim55615, align 8
%stackaddr$prim55616 = alloca %struct.ScmObj*, align 8
%current_45args54205 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54204)
store volatile %struct.ScmObj* %current_45args54205, %struct.ScmObj** %stackaddr$prim55616, align 8
%stackaddr$prim55617 = alloca %struct.ScmObj*, align 8
%vs48060 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54205)
store volatile %struct.ScmObj* %vs48060, %struct.ScmObj** %stackaddr$prim55617, align 8
%stackaddr$makeclosure55618 = alloca %struct.ScmObj*, align 8
%fptrToInt55619 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49239 to i64
%ae49239 = call %struct.ScmObj* @closure_alloc(i64 7, i64 %fptrToInt55619)
store volatile %struct.ScmObj* %ae49239, %struct.ScmObj** %stackaddr$makeclosure55618, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49239, %struct.ScmObj* %f48057, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49239, %struct.ScmObj* %acc48056, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49239, %struct.ScmObj* %_37foldr48053, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49239, %struct.ScmObj* %k48372, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49239, %struct.ScmObj* %_37foldr148047, i64 4)
call void @closure_place_freevar(%struct.ScmObj* %ae49239, %struct.ScmObj* %lsts_4348062, i64 5)
call void @closure_place_freevar(%struct.ScmObj* %ae49239, %struct.ScmObj* %vs48060, i64 6)
%ae49240 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55620 = alloca %struct.ScmObj*, align 8
%fptrToInt55621 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49241 to i64
%ae49241 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55621)
store volatile %struct.ScmObj* %ae49241, %struct.ScmObj** %stackaddr$makeclosure55620, align 8
%argslist54222$ae492390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55622 = alloca %struct.ScmObj*, align 8
%argslist54222$ae492391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49241, %struct.ScmObj* %argslist54222$ae492390)
store volatile %struct.ScmObj* %argslist54222$ae492391, %struct.ScmObj** %stackaddr$prim55622, align 8
%stackaddr$prim55623 = alloca %struct.ScmObj*, align 8
%argslist54222$ae492392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49240, %struct.ScmObj* %argslist54222$ae492391)
store volatile %struct.ScmObj* %argslist54222$ae492392, %struct.ScmObj** %stackaddr$prim55623, align 8
%clofunc55624 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49239)
musttail call tailcc void %clofunc55624(%struct.ScmObj* %ae49239, %struct.ScmObj* %argslist54222$ae492392)
ret void
}

define tailcc void @proc_clo$ae49239(%struct.ScmObj* %env$ae49239,%struct.ScmObj* %current_45args54207) {
%stackaddr$env-ref55625 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49239, i64 0)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref55625
%stackaddr$env-ref55626 = alloca %struct.ScmObj*, align 8
%acc48056 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49239, i64 1)
store %struct.ScmObj* %acc48056, %struct.ScmObj** %stackaddr$env-ref55626
%stackaddr$env-ref55627 = alloca %struct.ScmObj*, align 8
%_37foldr48053 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49239, i64 2)
store %struct.ScmObj* %_37foldr48053, %struct.ScmObj** %stackaddr$env-ref55627
%stackaddr$env-ref55628 = alloca %struct.ScmObj*, align 8
%k48372 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49239, i64 3)
store %struct.ScmObj* %k48372, %struct.ScmObj** %stackaddr$env-ref55628
%stackaddr$env-ref55629 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49239, i64 4)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55629
%stackaddr$env-ref55630 = alloca %struct.ScmObj*, align 8
%lsts_4348062 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49239, i64 5)
store %struct.ScmObj* %lsts_4348062, %struct.ScmObj** %stackaddr$env-ref55630
%stackaddr$env-ref55631 = alloca %struct.ScmObj*, align 8
%vs48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49239, i64 6)
store %struct.ScmObj* %vs48060, %struct.ScmObj** %stackaddr$env-ref55631
%stackaddr$prim55632 = alloca %struct.ScmObj*, align 8
%_95k48379 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54207)
store volatile %struct.ScmObj* %_95k48379, %struct.ScmObj** %stackaddr$prim55632, align 8
%stackaddr$prim55633 = alloca %struct.ScmObj*, align 8
%current_45args54208 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54207)
store volatile %struct.ScmObj* %current_45args54208, %struct.ScmObj** %stackaddr$prim55633, align 8
%stackaddr$prim55634 = alloca %struct.ScmObj*, align 8
%anf_45bind48189 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54208)
store volatile %struct.ScmObj* %anf_45bind48189, %struct.ScmObj** %stackaddr$prim55634, align 8
%stackaddr$prim55635 = alloca %struct.ScmObj*, align 8
%anf_45bind48190 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48056, %struct.ScmObj* %lsts_4348062)
store volatile %struct.ScmObj* %anf_45bind48190, %struct.ScmObj** %stackaddr$prim55635, align 8
%stackaddr$prim55636 = alloca %struct.ScmObj*, align 8
%anf_45bind48191 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48057, %struct.ScmObj* %anf_45bind48190)
store volatile %struct.ScmObj* %anf_45bind48191, %struct.ScmObj** %stackaddr$prim55636, align 8
%stackaddr$makeclosure55637 = alloca %struct.ScmObj*, align 8
%fptrToInt55638 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49265 to i64
%ae49265 = call %struct.ScmObj* @closure_alloc(i64 5, i64 %fptrToInt55638)
store volatile %struct.ScmObj* %ae49265, %struct.ScmObj** %stackaddr$makeclosure55637, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49265, %struct.ScmObj* %f48057, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49265, %struct.ScmObj* %k48372, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49265, %struct.ScmObj* %_37foldr148047, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49265, %struct.ScmObj* %anf_45bind48189, i64 3)
call void @closure_place_freevar(%struct.ScmObj* %ae49265, %struct.ScmObj* %vs48060, i64 4)
%stackaddr$prim55639 = alloca %struct.ScmObj*, align 8
%cpsargs48383 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49265, %struct.ScmObj* %anf_45bind48191)
store volatile %struct.ScmObj* %cpsargs48383, %struct.ScmObj** %stackaddr$prim55639, align 8
%clofunc55640 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr48053)
musttail call tailcc void %clofunc55640(%struct.ScmObj* %_37foldr48053, %struct.ScmObj* %cpsargs48383)
ret void
}

define tailcc void @proc_clo$ae49265(%struct.ScmObj* %env$ae49265,%struct.ScmObj* %current_45args54210) {
%stackaddr$env-ref55641 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49265, i64 0)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref55641
%stackaddr$env-ref55642 = alloca %struct.ScmObj*, align 8
%k48372 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49265, i64 1)
store %struct.ScmObj* %k48372, %struct.ScmObj** %stackaddr$env-ref55642
%stackaddr$env-ref55643 = alloca %struct.ScmObj*, align 8
%_37foldr148047 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49265, i64 2)
store %struct.ScmObj* %_37foldr148047, %struct.ScmObj** %stackaddr$env-ref55643
%stackaddr$env-ref55644 = alloca %struct.ScmObj*, align 8
%anf_45bind48189 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49265, i64 3)
store %struct.ScmObj* %anf_45bind48189, %struct.ScmObj** %stackaddr$env-ref55644
%stackaddr$env-ref55645 = alloca %struct.ScmObj*, align 8
%vs48060 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49265, i64 4)
store %struct.ScmObj* %vs48060, %struct.ScmObj** %stackaddr$env-ref55645
%stackaddr$prim55646 = alloca %struct.ScmObj*, align 8
%_95k48380 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54210)
store volatile %struct.ScmObj* %_95k48380, %struct.ScmObj** %stackaddr$prim55646, align 8
%stackaddr$prim55647 = alloca %struct.ScmObj*, align 8
%current_45args54211 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54210)
store volatile %struct.ScmObj* %current_45args54211, %struct.ScmObj** %stackaddr$prim55647, align 8
%stackaddr$prim55648 = alloca %struct.ScmObj*, align 8
%anf_45bind48192 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54211)
store volatile %struct.ScmObj* %anf_45bind48192, %struct.ScmObj** %stackaddr$prim55648, align 8
%ae49270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55649 = alloca %struct.ScmObj*, align 8
%anf_45bind48193 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48192, %struct.ScmObj* %ae49270)
store volatile %struct.ScmObj* %anf_45bind48193, %struct.ScmObj** %stackaddr$prim55649, align 8
%stackaddr$makeclosure55650 = alloca %struct.ScmObj*, align 8
%fptrToInt55651 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49272 to i64
%ae49272 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55651)
store volatile %struct.ScmObj* %ae49272, %struct.ScmObj** %stackaddr$makeclosure55650, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49272, %struct.ScmObj* %f48057, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49272, %struct.ScmObj* %k48372, i64 1)
%argslist54216$_37foldr1480470 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55652 = alloca %struct.ScmObj*, align 8
%argslist54216$_37foldr1480471 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %vs48060, %struct.ScmObj* %argslist54216$_37foldr1480470)
store volatile %struct.ScmObj* %argslist54216$_37foldr1480471, %struct.ScmObj** %stackaddr$prim55652, align 8
%stackaddr$prim55653 = alloca %struct.ScmObj*, align 8
%argslist54216$_37foldr1480472 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48193, %struct.ScmObj* %argslist54216$_37foldr1480471)
store volatile %struct.ScmObj* %argslist54216$_37foldr1480472, %struct.ScmObj** %stackaddr$prim55653, align 8
%stackaddr$prim55654 = alloca %struct.ScmObj*, align 8
%argslist54216$_37foldr1480473 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48189, %struct.ScmObj* %argslist54216$_37foldr1480472)
store volatile %struct.ScmObj* %argslist54216$_37foldr1480473, %struct.ScmObj** %stackaddr$prim55654, align 8
%stackaddr$prim55655 = alloca %struct.ScmObj*, align 8
%argslist54216$_37foldr1480474 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49272, %struct.ScmObj* %argslist54216$_37foldr1480473)
store volatile %struct.ScmObj* %argslist54216$_37foldr1480474, %struct.ScmObj** %stackaddr$prim55655, align 8
%clofunc55656 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148047)
musttail call tailcc void %clofunc55656(%struct.ScmObj* %_37foldr148047, %struct.ScmObj* %argslist54216$_37foldr1480474)
ret void
}

define tailcc void @proc_clo$ae49272(%struct.ScmObj* %env$ae49272,%struct.ScmObj* %current_45args54213) {
%stackaddr$env-ref55657 = alloca %struct.ScmObj*, align 8
%f48057 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49272, i64 0)
store %struct.ScmObj* %f48057, %struct.ScmObj** %stackaddr$env-ref55657
%stackaddr$env-ref55658 = alloca %struct.ScmObj*, align 8
%k48372 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49272, i64 1)
store %struct.ScmObj* %k48372, %struct.ScmObj** %stackaddr$env-ref55658
%stackaddr$prim55659 = alloca %struct.ScmObj*, align 8
%_95k48381 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54213)
store volatile %struct.ScmObj* %_95k48381, %struct.ScmObj** %stackaddr$prim55659, align 8
%stackaddr$prim55660 = alloca %struct.ScmObj*, align 8
%current_45args54214 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54213)
store volatile %struct.ScmObj* %current_45args54214, %struct.ScmObj** %stackaddr$prim55660, align 8
%stackaddr$prim55661 = alloca %struct.ScmObj*, align 8
%anf_45bind48194 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54214)
store volatile %struct.ScmObj* %anf_45bind48194, %struct.ScmObj** %stackaddr$prim55661, align 8
%stackaddr$prim55662 = alloca %struct.ScmObj*, align 8
%cpsargs48382 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48372, %struct.ScmObj* %anf_45bind48194)
store volatile %struct.ScmObj* %cpsargs48382, %struct.ScmObj** %stackaddr$prim55662, align 8
%clofunc55663 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48057)
musttail call tailcc void %clofunc55663(%struct.ScmObj* %f48057, %struct.ScmObj* %cpsargs48382)
ret void
}

define tailcc void @proc_clo$ae49241(%struct.ScmObj* %env$ae49241,%struct.ScmObj* %current_45args54217) {
%stackaddr$prim55664 = alloca %struct.ScmObj*, align 8
%k48384 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54217)
store volatile %struct.ScmObj* %k48384, %struct.ScmObj** %stackaddr$prim55664, align 8
%stackaddr$prim55665 = alloca %struct.ScmObj*, align 8
%current_45args54218 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54217)
store volatile %struct.ScmObj* %current_45args54218, %struct.ScmObj** %stackaddr$prim55665, align 8
%stackaddr$prim55666 = alloca %struct.ScmObj*, align 8
%a48065 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54218)
store volatile %struct.ScmObj* %a48065, %struct.ScmObj** %stackaddr$prim55666, align 8
%stackaddr$prim55667 = alloca %struct.ScmObj*, align 8
%current_45args54219 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54218)
store volatile %struct.ScmObj* %current_45args54219, %struct.ScmObj** %stackaddr$prim55667, align 8
%stackaddr$prim55668 = alloca %struct.ScmObj*, align 8
%b48064 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54219)
store volatile %struct.ScmObj* %b48064, %struct.ScmObj** %stackaddr$prim55668, align 8
%stackaddr$prim55669 = alloca %struct.ScmObj*, align 8
%cpsprim48385 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %a48065, %struct.ScmObj* %b48064)
store volatile %struct.ScmObj* %cpsprim48385, %struct.ScmObj** %stackaddr$prim55669, align 8
%ae49245 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54221$k483840 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55670 = alloca %struct.ScmObj*, align 8
%argslist54221$k483841 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48385, %struct.ScmObj* %argslist54221$k483840)
store volatile %struct.ScmObj* %argslist54221$k483841, %struct.ScmObj** %stackaddr$prim55670, align 8
%stackaddr$prim55671 = alloca %struct.ScmObj*, align 8
%argslist54221$k483842 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49245, %struct.ScmObj* %argslist54221$k483841)
store volatile %struct.ScmObj* %argslist54221$k483842, %struct.ScmObj** %stackaddr$prim55671, align 8
%clofunc55672 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48384)
musttail call tailcc void %clofunc55672(%struct.ScmObj* %k48384, %struct.ScmObj* %argslist54221$k483842)
ret void
}

define tailcc void @proc_clo$ae49217(%struct.ScmObj* %env$ae49217,%struct.ScmObj* %current_45args54224) {
%stackaddr$prim55673 = alloca %struct.ScmObj*, align 8
%k48386 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54224)
store volatile %struct.ScmObj* %k48386, %struct.ScmObj** %stackaddr$prim55673, align 8
%stackaddr$prim55674 = alloca %struct.ScmObj*, align 8
%current_45args54225 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54224)
store volatile %struct.ScmObj* %current_45args54225, %struct.ScmObj** %stackaddr$prim55674, align 8
%stackaddr$prim55675 = alloca %struct.ScmObj*, align 8
%x48061 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54225)
store volatile %struct.ScmObj* %x48061, %struct.ScmObj** %stackaddr$prim55675, align 8
%stackaddr$prim55676 = alloca %struct.ScmObj*, align 8
%cpsprim48387 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %x48061)
store volatile %struct.ScmObj* %cpsprim48387, %struct.ScmObj** %stackaddr$prim55676, align 8
%ae49220 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54227$k483860 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55677 = alloca %struct.ScmObj*, align 8
%argslist54227$k483861 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48387, %struct.ScmObj* %argslist54227$k483860)
store volatile %struct.ScmObj* %argslist54227$k483861, %struct.ScmObj** %stackaddr$prim55677, align 8
%stackaddr$prim55678 = alloca %struct.ScmObj*, align 8
%argslist54227$k483862 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49220, %struct.ScmObj* %argslist54227$k483861)
store volatile %struct.ScmObj* %argslist54227$k483862, %struct.ScmObj** %stackaddr$prim55678, align 8
%clofunc55679 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48386)
musttail call tailcc void %clofunc55679(%struct.ScmObj* %k48386, %struct.ScmObj* %argslist54227$k483862)
ret void
}

define tailcc void @proc_clo$ae49193(%struct.ScmObj* %env$ae49193,%struct.ScmObj* %current_45args54230) {
%stackaddr$prim55680 = alloca %struct.ScmObj*, align 8
%k48388 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54230)
store volatile %struct.ScmObj* %k48388, %struct.ScmObj** %stackaddr$prim55680, align 8
%stackaddr$prim55681 = alloca %struct.ScmObj*, align 8
%current_45args54231 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54230)
store volatile %struct.ScmObj* %current_45args54231, %struct.ScmObj** %stackaddr$prim55681, align 8
%stackaddr$prim55682 = alloca %struct.ScmObj*, align 8
%x48063 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54231)
store volatile %struct.ScmObj* %x48063, %struct.ScmObj** %stackaddr$prim55682, align 8
%stackaddr$prim55683 = alloca %struct.ScmObj*, align 8
%cpsprim48389 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %x48063)
store volatile %struct.ScmObj* %cpsprim48389, %struct.ScmObj** %stackaddr$prim55683, align 8
%ae49196 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54233$k483880 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55684 = alloca %struct.ScmObj*, align 8
%argslist54233$k483881 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48389, %struct.ScmObj* %argslist54233$k483880)
store volatile %struct.ScmObj* %argslist54233$k483881, %struct.ScmObj** %stackaddr$prim55684, align 8
%stackaddr$prim55685 = alloca %struct.ScmObj*, align 8
%argslist54233$k483882 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49196, %struct.ScmObj* %argslist54233$k483881)
store volatile %struct.ScmObj* %argslist54233$k483882, %struct.ScmObj** %stackaddr$prim55685, align 8
%clofunc55686 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48388)
musttail call tailcc void %clofunc55686(%struct.ScmObj* %k48388, %struct.ScmObj* %argslist54233$k483882)
ret void
}

define tailcc void @proc_clo$ae49145(%struct.ScmObj* %env$ae49145,%struct.ScmObj* %current_45args54236) {
%stackaddr$prim55687 = alloca %struct.ScmObj*, align 8
%k48390 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54236)
store volatile %struct.ScmObj* %k48390, %struct.ScmObj** %stackaddr$prim55687, align 8
%stackaddr$prim55688 = alloca %struct.ScmObj*, align 8
%current_45args54237 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54236)
store volatile %struct.ScmObj* %current_45args54237, %struct.ScmObj** %stackaddr$prim55688, align 8
%stackaddr$prim55689 = alloca %struct.ScmObj*, align 8
%lst48059 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54237)
store volatile %struct.ScmObj* %lst48059, %struct.ScmObj** %stackaddr$prim55689, align 8
%stackaddr$prim55690 = alloca %struct.ScmObj*, align 8
%current_45args54238 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54237)
store volatile %struct.ScmObj* %current_45args54238, %struct.ScmObj** %stackaddr$prim55690, align 8
%stackaddr$prim55691 = alloca %struct.ScmObj*, align 8
%b48058 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54238)
store volatile %struct.ScmObj* %b48058, %struct.ScmObj** %stackaddr$prim55691, align 8
%truthy$cmp55692 = call i64 @is_truthy_value(%struct.ScmObj* %b48058)
%cmp$cmp55692 = icmp eq i64 %truthy$cmp55692, 1
br i1 %cmp$cmp55692, label %truebranch$cmp55692, label %falsebranch$cmp55692
truebranch$cmp55692:
%ae49148 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54240$k483900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55693 = alloca %struct.ScmObj*, align 8
%argslist54240$k483901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %b48058, %struct.ScmObj* %argslist54240$k483900)
store volatile %struct.ScmObj* %argslist54240$k483901, %struct.ScmObj** %stackaddr$prim55693, align 8
%stackaddr$prim55694 = alloca %struct.ScmObj*, align 8
%argslist54240$k483902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49148, %struct.ScmObj* %argslist54240$k483901)
store volatile %struct.ScmObj* %argslist54240$k483902, %struct.ScmObj** %stackaddr$prim55694, align 8
%clofunc55695 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48390)
musttail call tailcc void %clofunc55695(%struct.ScmObj* %k48390, %struct.ScmObj* %argslist54240$k483902)
ret void
falsebranch$cmp55692:
%stackaddr$prim55696 = alloca %struct.ScmObj*, align 8
%cpsprim48391 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48059)
store volatile %struct.ScmObj* %cpsprim48391, %struct.ScmObj** %stackaddr$prim55696, align 8
%ae49155 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54241$k483900 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55697 = alloca %struct.ScmObj*, align 8
%argslist54241$k483901 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48391, %struct.ScmObj* %argslist54241$k483900)
store volatile %struct.ScmObj* %argslist54241$k483901, %struct.ScmObj** %stackaddr$prim55697, align 8
%stackaddr$prim55698 = alloca %struct.ScmObj*, align 8
%argslist54241$k483902 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49155, %struct.ScmObj* %argslist54241$k483901)
store volatile %struct.ScmObj* %argslist54241$k483902, %struct.ScmObj** %stackaddr$prim55698, align 8
%clofunc55699 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48390)
musttail call tailcc void %clofunc55699(%struct.ScmObj* %k48390, %struct.ScmObj* %argslist54241$k483902)
ret void
}

define tailcc void @proc_clo$ae49102(%struct.ScmObj* %env$ae49102,%struct.ScmObj* %current_45args54245) {
%stackaddr$env-ref55700 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49102, i64 0)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref55700
%stackaddr$env-ref55701 = alloca %struct.ScmObj*, align 8
%_37length48036 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49102, i64 1)
store %struct.ScmObj* %_37length48036, %struct.ScmObj** %stackaddr$env-ref55701
%stackaddr$prim55702 = alloca %struct.ScmObj*, align 8
%k48392 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54245)
store volatile %struct.ScmObj* %k48392, %struct.ScmObj** %stackaddr$prim55702, align 8
%stackaddr$prim55703 = alloca %struct.ScmObj*, align 8
%current_45args54246 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54245)
store volatile %struct.ScmObj* %current_45args54246, %struct.ScmObj** %stackaddr$prim55703, align 8
%stackaddr$prim55704 = alloca %struct.ScmObj*, align 8
%lst48068 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54246)
store volatile %struct.ScmObj* %lst48068, %struct.ScmObj** %stackaddr$prim55704, align 8
%stackaddr$prim55705 = alloca %struct.ScmObj*, align 8
%current_45args54247 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54246)
store volatile %struct.ScmObj* %current_45args54247, %struct.ScmObj** %stackaddr$prim55705, align 8
%stackaddr$prim55706 = alloca %struct.ScmObj*, align 8
%n48067 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54247)
store volatile %struct.ScmObj* %n48067, %struct.ScmObj** %stackaddr$prim55706, align 8
%stackaddr$makeclosure55707 = alloca %struct.ScmObj*, align 8
%fptrToInt55708 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49104 to i64
%ae49104 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55708)
store volatile %struct.ScmObj* %ae49104, %struct.ScmObj** %stackaddr$makeclosure55707, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49104, %struct.ScmObj* %k48392, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49104, %struct.ScmObj* %_37take48039, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49104, %struct.ScmObj* %lst48068, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae49104, %struct.ScmObj* %n48067, i64 3)
%argslist54253$_37length480360 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55709 = alloca %struct.ScmObj*, align 8
%argslist54253$_37length480361 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48068, %struct.ScmObj* %argslist54253$_37length480360)
store volatile %struct.ScmObj* %argslist54253$_37length480361, %struct.ScmObj** %stackaddr$prim55709, align 8
%stackaddr$prim55710 = alloca %struct.ScmObj*, align 8
%argslist54253$_37length480362 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49104, %struct.ScmObj* %argslist54253$_37length480361)
store volatile %struct.ScmObj* %argslist54253$_37length480362, %struct.ScmObj** %stackaddr$prim55710, align 8
%clofunc55711 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48036)
musttail call tailcc void %clofunc55711(%struct.ScmObj* %_37length48036, %struct.ScmObj* %argslist54253$_37length480362)
ret void
}

define tailcc void @proc_clo$ae49104(%struct.ScmObj* %env$ae49104,%struct.ScmObj* %current_45args54249) {
%stackaddr$env-ref55712 = alloca %struct.ScmObj*, align 8
%k48392 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49104, i64 0)
store %struct.ScmObj* %k48392, %struct.ScmObj** %stackaddr$env-ref55712
%stackaddr$env-ref55713 = alloca %struct.ScmObj*, align 8
%_37take48039 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49104, i64 1)
store %struct.ScmObj* %_37take48039, %struct.ScmObj** %stackaddr$env-ref55713
%stackaddr$env-ref55714 = alloca %struct.ScmObj*, align 8
%lst48068 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49104, i64 2)
store %struct.ScmObj* %lst48068, %struct.ScmObj** %stackaddr$env-ref55714
%stackaddr$env-ref55715 = alloca %struct.ScmObj*, align 8
%n48067 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49104, i64 3)
store %struct.ScmObj* %n48067, %struct.ScmObj** %stackaddr$env-ref55715
%stackaddr$prim55716 = alloca %struct.ScmObj*, align 8
%_95k48393 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54249)
store volatile %struct.ScmObj* %_95k48393, %struct.ScmObj** %stackaddr$prim55716, align 8
%stackaddr$prim55717 = alloca %struct.ScmObj*, align 8
%current_45args54250 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54249)
store volatile %struct.ScmObj* %current_45args54250, %struct.ScmObj** %stackaddr$prim55717, align 8
%stackaddr$prim55718 = alloca %struct.ScmObj*, align 8
%anf_45bind48181 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54250)
store volatile %struct.ScmObj* %anf_45bind48181, %struct.ScmObj** %stackaddr$prim55718, align 8
%stackaddr$prim55719 = alloca %struct.ScmObj*, align 8
%anf_45bind48182 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %anf_45bind48181, %struct.ScmObj* %n48067)
store volatile %struct.ScmObj* %anf_45bind48182, %struct.ScmObj** %stackaddr$prim55719, align 8
%argslist54252$_37take480390 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55720 = alloca %struct.ScmObj*, align 8
%argslist54252$_37take480391 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48182, %struct.ScmObj* %argslist54252$_37take480390)
store volatile %struct.ScmObj* %argslist54252$_37take480391, %struct.ScmObj** %stackaddr$prim55720, align 8
%stackaddr$prim55721 = alloca %struct.ScmObj*, align 8
%argslist54252$_37take480392 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48068, %struct.ScmObj* %argslist54252$_37take480391)
store volatile %struct.ScmObj* %argslist54252$_37take480392, %struct.ScmObj** %stackaddr$prim55721, align 8
%stackaddr$prim55722 = alloca %struct.ScmObj*, align 8
%argslist54252$_37take480393 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48392, %struct.ScmObj* %argslist54252$_37take480392)
store volatile %struct.ScmObj* %argslist54252$_37take480393, %struct.ScmObj** %stackaddr$prim55722, align 8
%clofunc55723 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48039)
musttail call tailcc void %clofunc55723(%struct.ScmObj* %_37take48039, %struct.ScmObj* %argslist54252$_37take480393)
ret void
}

define tailcc void @proc_clo$ae49048(%struct.ScmObj* %env$ae49048,%struct.ScmObj* %current_45args54255) {
%stackaddr$env-ref55724 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49048, i64 0)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref55724
%stackaddr$prim55725 = alloca %struct.ScmObj*, align 8
%k48394 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54255)
store volatile %struct.ScmObj* %k48394, %struct.ScmObj** %stackaddr$prim55725, align 8
%stackaddr$prim55726 = alloca %struct.ScmObj*, align 8
%current_45args54256 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54255)
store volatile %struct.ScmObj* %current_45args54256, %struct.ScmObj** %stackaddr$prim55726, align 8
%stackaddr$prim55727 = alloca %struct.ScmObj*, align 8
%lst48070 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54256)
store volatile %struct.ScmObj* %lst48070, %struct.ScmObj** %stackaddr$prim55727, align 8
%stackaddr$makeclosure55728 = alloca %struct.ScmObj*, align 8
%fptrToInt55729 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49049 to i64
%ae49049 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55729)
store volatile %struct.ScmObj* %ae49049, %struct.ScmObj** %stackaddr$makeclosure55728, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae49049, %struct.ScmObj* %lst48070, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae49049, %struct.ScmObj* %_37foldl148031, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae49049, %struct.ScmObj* %k48394, i64 2)
%ae49050 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55730 = alloca %struct.ScmObj*, align 8
%fptrToInt55731 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae49051 to i64
%ae49051 = call %struct.ScmObj* @closure_alloc(i64 0, i64 %fptrToInt55731)
store volatile %struct.ScmObj* %ae49051, %struct.ScmObj** %stackaddr$makeclosure55730, align 8
%argslist54267$ae490490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55732 = alloca %struct.ScmObj*, align 8
%argslist54267$ae490491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49051, %struct.ScmObj* %argslist54267$ae490490)
store volatile %struct.ScmObj* %argslist54267$ae490491, %struct.ScmObj** %stackaddr$prim55732, align 8
%stackaddr$prim55733 = alloca %struct.ScmObj*, align 8
%argslist54267$ae490492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49050, %struct.ScmObj* %argslist54267$ae490491)
store volatile %struct.ScmObj* %argslist54267$ae490492, %struct.ScmObj** %stackaddr$prim55733, align 8
%clofunc55734 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae49049)
musttail call tailcc void %clofunc55734(%struct.ScmObj* %ae49049, %struct.ScmObj* %argslist54267$ae490492)
ret void
}

define tailcc void @proc_clo$ae49049(%struct.ScmObj* %env$ae49049,%struct.ScmObj* %current_45args54258) {
%stackaddr$env-ref55735 = alloca %struct.ScmObj*, align 8
%lst48070 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49049, i64 0)
store %struct.ScmObj* %lst48070, %struct.ScmObj** %stackaddr$env-ref55735
%stackaddr$env-ref55736 = alloca %struct.ScmObj*, align 8
%_37foldl148031 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49049, i64 1)
store %struct.ScmObj* %_37foldl148031, %struct.ScmObj** %stackaddr$env-ref55736
%stackaddr$env-ref55737 = alloca %struct.ScmObj*, align 8
%k48394 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae49049, i64 2)
store %struct.ScmObj* %k48394, %struct.ScmObj** %stackaddr$env-ref55737
%stackaddr$prim55738 = alloca %struct.ScmObj*, align 8
%_95k48395 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54258)
store volatile %struct.ScmObj* %_95k48395, %struct.ScmObj** %stackaddr$prim55738, align 8
%stackaddr$prim55739 = alloca %struct.ScmObj*, align 8
%current_45args54259 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54258)
store volatile %struct.ScmObj* %current_45args54259, %struct.ScmObj** %stackaddr$prim55739, align 8
%stackaddr$prim55740 = alloca %struct.ScmObj*, align 8
%anf_45bind48180 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54259)
store volatile %struct.ScmObj* %anf_45bind48180, %struct.ScmObj** %stackaddr$prim55740, align 8
%ae49070 = call %struct.ScmObj* @const_init_null()
%argslist54261$_37foldl1480310 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55741 = alloca %struct.ScmObj*, align 8
%argslist54261$_37foldl1480311 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %lst48070, %struct.ScmObj* %argslist54261$_37foldl1480310)
store volatile %struct.ScmObj* %argslist54261$_37foldl1480311, %struct.ScmObj** %stackaddr$prim55741, align 8
%stackaddr$prim55742 = alloca %struct.ScmObj*, align 8
%argslist54261$_37foldl1480312 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49070, %struct.ScmObj* %argslist54261$_37foldl1480311)
store volatile %struct.ScmObj* %argslist54261$_37foldl1480312, %struct.ScmObj** %stackaddr$prim55742, align 8
%stackaddr$prim55743 = alloca %struct.ScmObj*, align 8
%argslist54261$_37foldl1480313 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48180, %struct.ScmObj* %argslist54261$_37foldl1480312)
store volatile %struct.ScmObj* %argslist54261$_37foldl1480313, %struct.ScmObj** %stackaddr$prim55743, align 8
%stackaddr$prim55744 = alloca %struct.ScmObj*, align 8
%argslist54261$_37foldl1480314 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48394, %struct.ScmObj* %argslist54261$_37foldl1480313)
store volatile %struct.ScmObj* %argslist54261$_37foldl1480314, %struct.ScmObj** %stackaddr$prim55744, align 8
%clofunc55745 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148031)
musttail call tailcc void %clofunc55745(%struct.ScmObj* %_37foldl148031, %struct.ScmObj* %argslist54261$_37foldl1480314)
ret void
}

define tailcc void @proc_clo$ae49051(%struct.ScmObj* %env$ae49051,%struct.ScmObj* %current_45args54262) {
%stackaddr$prim55746 = alloca %struct.ScmObj*, align 8
%k48396 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54262)
store volatile %struct.ScmObj* %k48396, %struct.ScmObj** %stackaddr$prim55746, align 8
%stackaddr$prim55747 = alloca %struct.ScmObj*, align 8
%current_45args54263 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54262)
store volatile %struct.ScmObj* %current_45args54263, %struct.ScmObj** %stackaddr$prim55747, align 8
%stackaddr$prim55748 = alloca %struct.ScmObj*, align 8
%x48072 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54263)
store volatile %struct.ScmObj* %x48072, %struct.ScmObj** %stackaddr$prim55748, align 8
%stackaddr$prim55749 = alloca %struct.ScmObj*, align 8
%current_45args54264 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54263)
store volatile %struct.ScmObj* %current_45args54264, %struct.ScmObj** %stackaddr$prim55749, align 8
%stackaddr$prim55750 = alloca %struct.ScmObj*, align 8
%y48071 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54264)
store volatile %struct.ScmObj* %y48071, %struct.ScmObj** %stackaddr$prim55750, align 8
%ae49053 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54266$k483960 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55751 = alloca %struct.ScmObj*, align 8
%argslist54266$k483961 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %x48072, %struct.ScmObj* %argslist54266$k483960)
store volatile %struct.ScmObj* %argslist54266$k483961, %struct.ScmObj** %stackaddr$prim55751, align 8
%stackaddr$prim55752 = alloca %struct.ScmObj*, align 8
%argslist54266$k483962 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae49053, %struct.ScmObj* %argslist54266$k483961)
store volatile %struct.ScmObj* %argslist54266$k483962, %struct.ScmObj** %stackaddr$prim55752, align 8
%clofunc55753 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48396)
musttail call tailcc void %clofunc55753(%struct.ScmObj* %k48396, %struct.ScmObj* %argslist54266$k483962)
ret void
}

define tailcc void @proc_clo$ae48969(%struct.ScmObj* %env$ae48969,%struct.ScmObj* %current_45args54270) {
%stackaddr$prim55754 = alloca %struct.ScmObj*, align 8
%k48397 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54270)
store volatile %struct.ScmObj* %k48397, %struct.ScmObj** %stackaddr$prim55754, align 8
%stackaddr$prim55755 = alloca %struct.ScmObj*, align 8
%current_45args54271 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54270)
store volatile %struct.ScmObj* %current_45args54271, %struct.ScmObj** %stackaddr$prim55755, align 8
%stackaddr$prim55756 = alloca %struct.ScmObj*, align 8
%_37foldl148032 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54271)
store volatile %struct.ScmObj* %_37foldl148032, %struct.ScmObj** %stackaddr$prim55756, align 8
%ae48971 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55757 = alloca %struct.ScmObj*, align 8
%fptrToInt55758 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48972 to i64
%ae48972 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55758)
store volatile %struct.ScmObj* %ae48972, %struct.ScmObj** %stackaddr$makeclosure55757, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48972, %struct.ScmObj* %_37foldl148032, i64 0)
%argslist54284$k483970 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55759 = alloca %struct.ScmObj*, align 8
%argslist54284$k483971 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48972, %struct.ScmObj* %argslist54284$k483970)
store volatile %struct.ScmObj* %argslist54284$k483971, %struct.ScmObj** %stackaddr$prim55759, align 8
%stackaddr$prim55760 = alloca %struct.ScmObj*, align 8
%argslist54284$k483972 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48971, %struct.ScmObj* %argslist54284$k483971)
store volatile %struct.ScmObj* %argslist54284$k483972, %struct.ScmObj** %stackaddr$prim55760, align 8
%clofunc55761 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48397)
musttail call tailcc void %clofunc55761(%struct.ScmObj* %k48397, %struct.ScmObj* %argslist54284$k483972)
ret void
}

define tailcc void @proc_clo$ae48972(%struct.ScmObj* %env$ae48972,%struct.ScmObj* %current_45args54273) {
%stackaddr$env-ref55762 = alloca %struct.ScmObj*, align 8
%_37foldl148032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48972, i64 0)
store %struct.ScmObj* %_37foldl148032, %struct.ScmObj** %stackaddr$env-ref55762
%stackaddr$prim55763 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54273)
store volatile %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$prim55763, align 8
%stackaddr$prim55764 = alloca %struct.ScmObj*, align 8
%current_45args54274 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54273)
store volatile %struct.ScmObj* %current_45args54274, %struct.ScmObj** %stackaddr$prim55764, align 8
%stackaddr$prim55765 = alloca %struct.ScmObj*, align 8
%f48035 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54274)
store volatile %struct.ScmObj* %f48035, %struct.ScmObj** %stackaddr$prim55765, align 8
%stackaddr$prim55766 = alloca %struct.ScmObj*, align 8
%current_45args54275 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54274)
store volatile %struct.ScmObj* %current_45args54275, %struct.ScmObj** %stackaddr$prim55766, align 8
%stackaddr$prim55767 = alloca %struct.ScmObj*, align 8
%acc48034 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54275)
store volatile %struct.ScmObj* %acc48034, %struct.ScmObj** %stackaddr$prim55767, align 8
%stackaddr$prim55768 = alloca %struct.ScmObj*, align 8
%current_45args54276 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54275)
store volatile %struct.ScmObj* %current_45args54276, %struct.ScmObj** %stackaddr$prim55768, align 8
%stackaddr$prim55769 = alloca %struct.ScmObj*, align 8
%lst48033 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54276)
store volatile %struct.ScmObj* %lst48033, %struct.ScmObj** %stackaddr$prim55769, align 8
%stackaddr$prim55770 = alloca %struct.ScmObj*, align 8
%anf_45bind48175 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48033)
store volatile %struct.ScmObj* %anf_45bind48175, %struct.ScmObj** %stackaddr$prim55770, align 8
%truthy$cmp55771 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48175)
%cmp$cmp55771 = icmp eq i64 %truthy$cmp55771, 1
br i1 %cmp$cmp55771, label %truebranch$cmp55771, label %falsebranch$cmp55771
truebranch$cmp55771:
%ae48976 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54278$k483980 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55772 = alloca %struct.ScmObj*, align 8
%argslist54278$k483981 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48034, %struct.ScmObj* %argslist54278$k483980)
store volatile %struct.ScmObj* %argslist54278$k483981, %struct.ScmObj** %stackaddr$prim55772, align 8
%stackaddr$prim55773 = alloca %struct.ScmObj*, align 8
%argslist54278$k483982 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48976, %struct.ScmObj* %argslist54278$k483981)
store volatile %struct.ScmObj* %argslist54278$k483982, %struct.ScmObj** %stackaddr$prim55773, align 8
%clofunc55774 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48398)
musttail call tailcc void %clofunc55774(%struct.ScmObj* %k48398, %struct.ScmObj* %argslist54278$k483982)
ret void
falsebranch$cmp55771:
%stackaddr$prim55775 = alloca %struct.ScmObj*, align 8
%anf_45bind48176 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48033)
store volatile %struct.ScmObj* %anf_45bind48176, %struct.ScmObj** %stackaddr$prim55775, align 8
%stackaddr$makeclosure55776 = alloca %struct.ScmObj*, align 8
%fptrToInt55777 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48983 to i64
%ae48983 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55777)
store volatile %struct.ScmObj* %ae48983, %struct.ScmObj** %stackaddr$makeclosure55776, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48983, %struct.ScmObj* %f48035, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48983, %struct.ScmObj* %lst48033, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48983, %struct.ScmObj* %_37foldl148032, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48983, %struct.ScmObj* %k48398, i64 3)
%argslist54283$f480350 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55778 = alloca %struct.ScmObj*, align 8
%argslist54283$f480351 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48034, %struct.ScmObj* %argslist54283$f480350)
store volatile %struct.ScmObj* %argslist54283$f480351, %struct.ScmObj** %stackaddr$prim55778, align 8
%stackaddr$prim55779 = alloca %struct.ScmObj*, align 8
%argslist54283$f480352 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48176, %struct.ScmObj* %argslist54283$f480351)
store volatile %struct.ScmObj* %argslist54283$f480352, %struct.ScmObj** %stackaddr$prim55779, align 8
%stackaddr$prim55780 = alloca %struct.ScmObj*, align 8
%argslist54283$f480353 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48983, %struct.ScmObj* %argslist54283$f480352)
store volatile %struct.ScmObj* %argslist54283$f480353, %struct.ScmObj** %stackaddr$prim55780, align 8
%clofunc55781 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48035)
musttail call tailcc void %clofunc55781(%struct.ScmObj* %f48035, %struct.ScmObj* %argslist54283$f480353)
ret void
}

define tailcc void @proc_clo$ae48983(%struct.ScmObj* %env$ae48983,%struct.ScmObj* %current_45args54279) {
%stackaddr$env-ref55782 = alloca %struct.ScmObj*, align 8
%f48035 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48983, i64 0)
store %struct.ScmObj* %f48035, %struct.ScmObj** %stackaddr$env-ref55782
%stackaddr$env-ref55783 = alloca %struct.ScmObj*, align 8
%lst48033 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48983, i64 1)
store %struct.ScmObj* %lst48033, %struct.ScmObj** %stackaddr$env-ref55783
%stackaddr$env-ref55784 = alloca %struct.ScmObj*, align 8
%_37foldl148032 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48983, i64 2)
store %struct.ScmObj* %_37foldl148032, %struct.ScmObj** %stackaddr$env-ref55784
%stackaddr$env-ref55785 = alloca %struct.ScmObj*, align 8
%k48398 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48983, i64 3)
store %struct.ScmObj* %k48398, %struct.ScmObj** %stackaddr$env-ref55785
%stackaddr$prim55786 = alloca %struct.ScmObj*, align 8
%_95k48399 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54279)
store volatile %struct.ScmObj* %_95k48399, %struct.ScmObj** %stackaddr$prim55786, align 8
%stackaddr$prim55787 = alloca %struct.ScmObj*, align 8
%current_45args54280 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54279)
store volatile %struct.ScmObj* %current_45args54280, %struct.ScmObj** %stackaddr$prim55787, align 8
%stackaddr$prim55788 = alloca %struct.ScmObj*, align 8
%anf_45bind48177 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54280)
store volatile %struct.ScmObj* %anf_45bind48177, %struct.ScmObj** %stackaddr$prim55788, align 8
%stackaddr$prim55789 = alloca %struct.ScmObj*, align 8
%anf_45bind48178 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48033)
store volatile %struct.ScmObj* %anf_45bind48178, %struct.ScmObj** %stackaddr$prim55789, align 8
%argslist54282$_37foldl1480320 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55790 = alloca %struct.ScmObj*, align 8
%argslist54282$_37foldl1480321 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48178, %struct.ScmObj* %argslist54282$_37foldl1480320)
store volatile %struct.ScmObj* %argslist54282$_37foldl1480321, %struct.ScmObj** %stackaddr$prim55790, align 8
%stackaddr$prim55791 = alloca %struct.ScmObj*, align 8
%argslist54282$_37foldl1480322 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48177, %struct.ScmObj* %argslist54282$_37foldl1480321)
store volatile %struct.ScmObj* %argslist54282$_37foldl1480322, %struct.ScmObj** %stackaddr$prim55791, align 8
%stackaddr$prim55792 = alloca %struct.ScmObj*, align 8
%argslist54282$_37foldl1480323 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48035, %struct.ScmObj* %argslist54282$_37foldl1480322)
store volatile %struct.ScmObj* %argslist54282$_37foldl1480323, %struct.ScmObj** %stackaddr$prim55792, align 8
%stackaddr$prim55793 = alloca %struct.ScmObj*, align 8
%argslist54282$_37foldl1480324 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48398, %struct.ScmObj* %argslist54282$_37foldl1480323)
store volatile %struct.ScmObj* %argslist54282$_37foldl1480324, %struct.ScmObj** %stackaddr$prim55793, align 8
%clofunc55794 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldl148032)
musttail call tailcc void %clofunc55794(%struct.ScmObj* %_37foldl148032, %struct.ScmObj* %argslist54282$_37foldl1480324)
ret void
}

define tailcc void @proc_clo$ae48886(%struct.ScmObj* %env$ae48886,%struct.ScmObj* %current_45args54287) {
%stackaddr$prim55795 = alloca %struct.ScmObj*, align 8
%k48400 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54287)
store volatile %struct.ScmObj* %k48400, %struct.ScmObj** %stackaddr$prim55795, align 8
%stackaddr$prim55796 = alloca %struct.ScmObj*, align 8
%current_45args54288 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54287)
store volatile %struct.ScmObj* %current_45args54288, %struct.ScmObj** %stackaddr$prim55796, align 8
%stackaddr$prim55797 = alloca %struct.ScmObj*, align 8
%_37length48037 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54288)
store volatile %struct.ScmObj* %_37length48037, %struct.ScmObj** %stackaddr$prim55797, align 8
%ae48888 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55798 = alloca %struct.ScmObj*, align 8
%fptrToInt55799 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48889 to i64
%ae48889 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55799)
store volatile %struct.ScmObj* %ae48889, %struct.ScmObj** %stackaddr$makeclosure55798, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48889, %struct.ScmObj* %_37length48037, i64 0)
%argslist54299$k484000 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55800 = alloca %struct.ScmObj*, align 8
%argslist54299$k484001 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48889, %struct.ScmObj* %argslist54299$k484000)
store volatile %struct.ScmObj* %argslist54299$k484001, %struct.ScmObj** %stackaddr$prim55800, align 8
%stackaddr$prim55801 = alloca %struct.ScmObj*, align 8
%argslist54299$k484002 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48888, %struct.ScmObj* %argslist54299$k484001)
store volatile %struct.ScmObj* %argslist54299$k484002, %struct.ScmObj** %stackaddr$prim55801, align 8
%clofunc55802 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48400)
musttail call tailcc void %clofunc55802(%struct.ScmObj* %k48400, %struct.ScmObj* %argslist54299$k484002)
ret void
}

define tailcc void @proc_clo$ae48889(%struct.ScmObj* %env$ae48889,%struct.ScmObj* %current_45args54290) {
%stackaddr$env-ref55803 = alloca %struct.ScmObj*, align 8
%_37length48037 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48889, i64 0)
store %struct.ScmObj* %_37length48037, %struct.ScmObj** %stackaddr$env-ref55803
%stackaddr$prim55804 = alloca %struct.ScmObj*, align 8
%k48401 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54290)
store volatile %struct.ScmObj* %k48401, %struct.ScmObj** %stackaddr$prim55804, align 8
%stackaddr$prim55805 = alloca %struct.ScmObj*, align 8
%current_45args54291 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54290)
store volatile %struct.ScmObj* %current_45args54291, %struct.ScmObj** %stackaddr$prim55805, align 8
%stackaddr$prim55806 = alloca %struct.ScmObj*, align 8
%lst48038 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54291)
store volatile %struct.ScmObj* %lst48038, %struct.ScmObj** %stackaddr$prim55806, align 8
%stackaddr$prim55807 = alloca %struct.ScmObj*, align 8
%anf_45bind48171 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48038)
store volatile %struct.ScmObj* %anf_45bind48171, %struct.ScmObj** %stackaddr$prim55807, align 8
%truthy$cmp55808 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48171)
%cmp$cmp55808 = icmp eq i64 %truthy$cmp55808, 1
br i1 %cmp$cmp55808, label %truebranch$cmp55808, label %falsebranch$cmp55808
truebranch$cmp55808:
%ae48893 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48894 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54293$k484010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55809 = alloca %struct.ScmObj*, align 8
%argslist54293$k484011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48894, %struct.ScmObj* %argslist54293$k484010)
store volatile %struct.ScmObj* %argslist54293$k484011, %struct.ScmObj** %stackaddr$prim55809, align 8
%stackaddr$prim55810 = alloca %struct.ScmObj*, align 8
%argslist54293$k484012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48893, %struct.ScmObj* %argslist54293$k484011)
store volatile %struct.ScmObj* %argslist54293$k484012, %struct.ScmObj** %stackaddr$prim55810, align 8
%clofunc55811 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48401)
musttail call tailcc void %clofunc55811(%struct.ScmObj* %k48401, %struct.ScmObj* %argslist54293$k484012)
ret void
falsebranch$cmp55808:
%stackaddr$prim55812 = alloca %struct.ScmObj*, align 8
%anf_45bind48172 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48038)
store volatile %struct.ScmObj* %anf_45bind48172, %struct.ScmObj** %stackaddr$prim55812, align 8
%stackaddr$makeclosure55813 = alloca %struct.ScmObj*, align 8
%fptrToInt55814 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48903 to i64
%ae48903 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55814)
store volatile %struct.ScmObj* %ae48903, %struct.ScmObj** %stackaddr$makeclosure55813, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48903, %struct.ScmObj* %k48401, i64 0)
%argslist54298$_37length480370 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55815 = alloca %struct.ScmObj*, align 8
%argslist54298$_37length480371 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48172, %struct.ScmObj* %argslist54298$_37length480370)
store volatile %struct.ScmObj* %argslist54298$_37length480371, %struct.ScmObj** %stackaddr$prim55815, align 8
%stackaddr$prim55816 = alloca %struct.ScmObj*, align 8
%argslist54298$_37length480372 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48903, %struct.ScmObj* %argslist54298$_37length480371)
store volatile %struct.ScmObj* %argslist54298$_37length480372, %struct.ScmObj** %stackaddr$prim55816, align 8
%clofunc55817 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37length48037)
musttail call tailcc void %clofunc55817(%struct.ScmObj* %_37length48037, %struct.ScmObj* %argslist54298$_37length480372)
ret void
}

define tailcc void @proc_clo$ae48903(%struct.ScmObj* %env$ae48903,%struct.ScmObj* %current_45args54294) {
%stackaddr$env-ref55818 = alloca %struct.ScmObj*, align 8
%k48401 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48903, i64 0)
store %struct.ScmObj* %k48401, %struct.ScmObj** %stackaddr$env-ref55818
%stackaddr$prim55819 = alloca %struct.ScmObj*, align 8
%_95k48402 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54294)
store volatile %struct.ScmObj* %_95k48402, %struct.ScmObj** %stackaddr$prim55819, align 8
%stackaddr$prim55820 = alloca %struct.ScmObj*, align 8
%current_45args54295 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54294)
store volatile %struct.ScmObj* %current_45args54295, %struct.ScmObj** %stackaddr$prim55820, align 8
%stackaddr$prim55821 = alloca %struct.ScmObj*, align 8
%anf_45bind48173 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54295)
store volatile %struct.ScmObj* %anf_45bind48173, %struct.ScmObj** %stackaddr$prim55821, align 8
%ae48905 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55822 = alloca %struct.ScmObj*, align 8
%cpsprim48403 = call %struct.ScmObj* @prim__43(%struct.ScmObj* %ae48905, %struct.ScmObj* %anf_45bind48173)
store volatile %struct.ScmObj* %cpsprim48403, %struct.ScmObj** %stackaddr$prim55822, align 8
%ae48908 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54297$k484010 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55823 = alloca %struct.ScmObj*, align 8
%argslist54297$k484011 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48403, %struct.ScmObj* %argslist54297$k484010)
store volatile %struct.ScmObj* %argslist54297$k484011, %struct.ScmObj** %stackaddr$prim55823, align 8
%stackaddr$prim55824 = alloca %struct.ScmObj*, align 8
%argslist54297$k484012 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48908, %struct.ScmObj* %argslist54297$k484011)
store volatile %struct.ScmObj* %argslist54297$k484012, %struct.ScmObj** %stackaddr$prim55824, align 8
%clofunc55825 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48401)
musttail call tailcc void %clofunc55825(%struct.ScmObj* %k48401, %struct.ScmObj* %argslist54297$k484012)
ret void
}

define tailcc void @proc_clo$ae48736(%struct.ScmObj* %env$ae48736,%struct.ScmObj* %current_45args54302) {
%stackaddr$prim55826 = alloca %struct.ScmObj*, align 8
%k48404 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54302)
store volatile %struct.ScmObj* %k48404, %struct.ScmObj** %stackaddr$prim55826, align 8
%stackaddr$prim55827 = alloca %struct.ScmObj*, align 8
%current_45args54303 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54302)
store volatile %struct.ScmObj* %current_45args54303, %struct.ScmObj** %stackaddr$prim55827, align 8
%stackaddr$prim55828 = alloca %struct.ScmObj*, align 8
%_37take48040 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54303)
store volatile %struct.ScmObj* %_37take48040, %struct.ScmObj** %stackaddr$prim55828, align 8
%ae48738 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55829 = alloca %struct.ScmObj*, align 8
%fptrToInt55830 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48739 to i64
%ae48739 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55830)
store volatile %struct.ScmObj* %ae48739, %struct.ScmObj** %stackaddr$makeclosure55829, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48739, %struct.ScmObj* %_37take48040, i64 0)
%argslist54316$k484040 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55831 = alloca %struct.ScmObj*, align 8
%argslist54316$k484041 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48739, %struct.ScmObj* %argslist54316$k484040)
store volatile %struct.ScmObj* %argslist54316$k484041, %struct.ScmObj** %stackaddr$prim55831, align 8
%stackaddr$prim55832 = alloca %struct.ScmObj*, align 8
%argslist54316$k484042 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48738, %struct.ScmObj* %argslist54316$k484041)
store volatile %struct.ScmObj* %argslist54316$k484042, %struct.ScmObj** %stackaddr$prim55832, align 8
%clofunc55833 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48404)
musttail call tailcc void %clofunc55833(%struct.ScmObj* %k48404, %struct.ScmObj* %argslist54316$k484042)
ret void
}

define tailcc void @proc_clo$ae48739(%struct.ScmObj* %env$ae48739,%struct.ScmObj* %current_45args54305) {
%stackaddr$env-ref55834 = alloca %struct.ScmObj*, align 8
%_37take48040 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48739, i64 0)
store %struct.ScmObj* %_37take48040, %struct.ScmObj** %stackaddr$env-ref55834
%stackaddr$prim55835 = alloca %struct.ScmObj*, align 8
%k48405 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54305)
store volatile %struct.ScmObj* %k48405, %struct.ScmObj** %stackaddr$prim55835, align 8
%stackaddr$prim55836 = alloca %struct.ScmObj*, align 8
%current_45args54306 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54305)
store volatile %struct.ScmObj* %current_45args54306, %struct.ScmObj** %stackaddr$prim55836, align 8
%stackaddr$prim55837 = alloca %struct.ScmObj*, align 8
%lst48042 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54306)
store volatile %struct.ScmObj* %lst48042, %struct.ScmObj** %stackaddr$prim55837, align 8
%stackaddr$prim55838 = alloca %struct.ScmObj*, align 8
%current_45args54307 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54306)
store volatile %struct.ScmObj* %current_45args54307, %struct.ScmObj** %stackaddr$prim55838, align 8
%stackaddr$prim55839 = alloca %struct.ScmObj*, align 8
%n48041 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54307)
store volatile %struct.ScmObj* %n48041, %struct.ScmObj** %stackaddr$prim55839, align 8
%ae48741 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$prim55840 = alloca %struct.ScmObj*, align 8
%anf_45bind48164 = call %struct.ScmObj* @prim__61(%struct.ScmObj* %n48041, %struct.ScmObj* %ae48741)
store volatile %struct.ScmObj* %anf_45bind48164, %struct.ScmObj** %stackaddr$prim55840, align 8
%truthy$cmp55841 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48164)
%cmp$cmp55841 = icmp eq i64 %truthy$cmp55841, 1
br i1 %cmp$cmp55841, label %truebranch$cmp55841, label %falsebranch$cmp55841
truebranch$cmp55841:
%ae48744 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48745 = call %struct.ScmObj* @const_init_null()
%argslist54309$k484050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55842 = alloca %struct.ScmObj*, align 8
%argslist54309$k484051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48745, %struct.ScmObj* %argslist54309$k484050)
store volatile %struct.ScmObj* %argslist54309$k484051, %struct.ScmObj** %stackaddr$prim55842, align 8
%stackaddr$prim55843 = alloca %struct.ScmObj*, align 8
%argslist54309$k484052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48744, %struct.ScmObj* %argslist54309$k484051)
store volatile %struct.ScmObj* %argslist54309$k484052, %struct.ScmObj** %stackaddr$prim55843, align 8
%clofunc55844 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48405)
musttail call tailcc void %clofunc55844(%struct.ScmObj* %k48405, %struct.ScmObj* %argslist54309$k484052)
ret void
falsebranch$cmp55841:
%stackaddr$prim55845 = alloca %struct.ScmObj*, align 8
%anf_45bind48165 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48042)
store volatile %struct.ScmObj* %anf_45bind48165, %struct.ScmObj** %stackaddr$prim55845, align 8
%truthy$cmp55846 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48165)
%cmp$cmp55846 = icmp eq i64 %truthy$cmp55846, 1
br i1 %cmp$cmp55846, label %truebranch$cmp55846, label %falsebranch$cmp55846
truebranch$cmp55846:
%ae48755 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48756 = call %struct.ScmObj* @const_init_null()
%argslist54310$k484050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55847 = alloca %struct.ScmObj*, align 8
%argslist54310$k484051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48756, %struct.ScmObj* %argslist54310$k484050)
store volatile %struct.ScmObj* %argslist54310$k484051, %struct.ScmObj** %stackaddr$prim55847, align 8
%stackaddr$prim55848 = alloca %struct.ScmObj*, align 8
%argslist54310$k484052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48755, %struct.ScmObj* %argslist54310$k484051)
store volatile %struct.ScmObj* %argslist54310$k484052, %struct.ScmObj** %stackaddr$prim55848, align 8
%clofunc55849 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48405)
musttail call tailcc void %clofunc55849(%struct.ScmObj* %k48405, %struct.ScmObj* %argslist54310$k484052)
ret void
falsebranch$cmp55846:
%stackaddr$prim55850 = alloca %struct.ScmObj*, align 8
%anf_45bind48166 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48042)
store volatile %struct.ScmObj* %anf_45bind48166, %struct.ScmObj** %stackaddr$prim55850, align 8
%stackaddr$prim55851 = alloca %struct.ScmObj*, align 8
%anf_45bind48167 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48042)
store volatile %struct.ScmObj* %anf_45bind48167, %struct.ScmObj** %stackaddr$prim55851, align 8
%ae48766 = call %struct.ScmObj* @const_init_int(i64 1)
%stackaddr$prim55852 = alloca %struct.ScmObj*, align 8
%anf_45bind48168 = call %struct.ScmObj* @prim__45(%struct.ScmObj* %n48041, %struct.ScmObj* %ae48766)
store volatile %struct.ScmObj* %anf_45bind48168, %struct.ScmObj** %stackaddr$prim55852, align 8
%stackaddr$makeclosure55853 = alloca %struct.ScmObj*, align 8
%fptrToInt55854 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48768 to i64
%ae48768 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55854)
store volatile %struct.ScmObj* %ae48768, %struct.ScmObj** %stackaddr$makeclosure55853, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48768, %struct.ScmObj* %anf_45bind48166, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48768, %struct.ScmObj* %k48405, i64 1)
%argslist54315$_37take480400 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55855 = alloca %struct.ScmObj*, align 8
%argslist54315$_37take480401 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48168, %struct.ScmObj* %argslist54315$_37take480400)
store volatile %struct.ScmObj* %argslist54315$_37take480401, %struct.ScmObj** %stackaddr$prim55855, align 8
%stackaddr$prim55856 = alloca %struct.ScmObj*, align 8
%argslist54315$_37take480402 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48167, %struct.ScmObj* %argslist54315$_37take480401)
store volatile %struct.ScmObj* %argslist54315$_37take480402, %struct.ScmObj** %stackaddr$prim55856, align 8
%stackaddr$prim55857 = alloca %struct.ScmObj*, align 8
%argslist54315$_37take480403 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48768, %struct.ScmObj* %argslist54315$_37take480402)
store volatile %struct.ScmObj* %argslist54315$_37take480403, %struct.ScmObj** %stackaddr$prim55857, align 8
%clofunc55858 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37take48040)
musttail call tailcc void %clofunc55858(%struct.ScmObj* %_37take48040, %struct.ScmObj* %argslist54315$_37take480403)
ret void
}

define tailcc void @proc_clo$ae48768(%struct.ScmObj* %env$ae48768,%struct.ScmObj* %current_45args54311) {
%stackaddr$env-ref55859 = alloca %struct.ScmObj*, align 8
%anf_45bind48166 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48768, i64 0)
store %struct.ScmObj* %anf_45bind48166, %struct.ScmObj** %stackaddr$env-ref55859
%stackaddr$env-ref55860 = alloca %struct.ScmObj*, align 8
%k48405 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48768, i64 1)
store %struct.ScmObj* %k48405, %struct.ScmObj** %stackaddr$env-ref55860
%stackaddr$prim55861 = alloca %struct.ScmObj*, align 8
%_95k48406 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54311)
store volatile %struct.ScmObj* %_95k48406, %struct.ScmObj** %stackaddr$prim55861, align 8
%stackaddr$prim55862 = alloca %struct.ScmObj*, align 8
%current_45args54312 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54311)
store volatile %struct.ScmObj* %current_45args54312, %struct.ScmObj** %stackaddr$prim55862, align 8
%stackaddr$prim55863 = alloca %struct.ScmObj*, align 8
%anf_45bind48169 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54312)
store volatile %struct.ScmObj* %anf_45bind48169, %struct.ScmObj** %stackaddr$prim55863, align 8
%stackaddr$prim55864 = alloca %struct.ScmObj*, align 8
%cpsprim48407 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48166, %struct.ScmObj* %anf_45bind48169)
store volatile %struct.ScmObj* %cpsprim48407, %struct.ScmObj** %stackaddr$prim55864, align 8
%ae48774 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54314$k484050 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55865 = alloca %struct.ScmObj*, align 8
%argslist54314$k484051 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48407, %struct.ScmObj* %argslist54314$k484050)
store volatile %struct.ScmObj* %argslist54314$k484051, %struct.ScmObj** %stackaddr$prim55865, align 8
%stackaddr$prim55866 = alloca %struct.ScmObj*, align 8
%argslist54314$k484052 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48774, %struct.ScmObj* %argslist54314$k484051)
store volatile %struct.ScmObj* %argslist54314$k484052, %struct.ScmObj** %stackaddr$prim55866, align 8
%clofunc55867 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48405)
musttail call tailcc void %clofunc55867(%struct.ScmObj* %k48405, %struct.ScmObj* %argslist54314$k484052)
ret void
}

define tailcc void @proc_clo$ae48639(%struct.ScmObj* %env$ae48639,%struct.ScmObj* %current_45args54319) {
%stackaddr$prim55868 = alloca %struct.ScmObj*, align 8
%k48408 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54319)
store volatile %struct.ScmObj* %k48408, %struct.ScmObj** %stackaddr$prim55868, align 8
%stackaddr$prim55869 = alloca %struct.ScmObj*, align 8
%current_45args54320 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54319)
store volatile %struct.ScmObj* %current_45args54320, %struct.ScmObj** %stackaddr$prim55869, align 8
%stackaddr$prim55870 = alloca %struct.ScmObj*, align 8
%_37map48044 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54320)
store volatile %struct.ScmObj* %_37map48044, %struct.ScmObj** %stackaddr$prim55870, align 8
%ae48641 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55871 = alloca %struct.ScmObj*, align 8
%fptrToInt55872 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48642 to i64
%ae48642 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55872)
store volatile %struct.ScmObj* %ae48642, %struct.ScmObj** %stackaddr$makeclosure55871, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48642, %struct.ScmObj* %_37map48044, i64 0)
%argslist54336$k484080 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55873 = alloca %struct.ScmObj*, align 8
%argslist54336$k484081 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48642, %struct.ScmObj* %argslist54336$k484080)
store volatile %struct.ScmObj* %argslist54336$k484081, %struct.ScmObj** %stackaddr$prim55873, align 8
%stackaddr$prim55874 = alloca %struct.ScmObj*, align 8
%argslist54336$k484082 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48641, %struct.ScmObj* %argslist54336$k484081)
store volatile %struct.ScmObj* %argslist54336$k484082, %struct.ScmObj** %stackaddr$prim55874, align 8
%clofunc55875 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48408)
musttail call tailcc void %clofunc55875(%struct.ScmObj* %k48408, %struct.ScmObj* %argslist54336$k484082)
ret void
}

define tailcc void @proc_clo$ae48642(%struct.ScmObj* %env$ae48642,%struct.ScmObj* %current_45args54322) {
%stackaddr$env-ref55876 = alloca %struct.ScmObj*, align 8
%_37map48044 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48642, i64 0)
store %struct.ScmObj* %_37map48044, %struct.ScmObj** %stackaddr$env-ref55876
%stackaddr$prim55877 = alloca %struct.ScmObj*, align 8
%k48409 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54322)
store volatile %struct.ScmObj* %k48409, %struct.ScmObj** %stackaddr$prim55877, align 8
%stackaddr$prim55878 = alloca %struct.ScmObj*, align 8
%current_45args54323 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54322)
store volatile %struct.ScmObj* %current_45args54323, %struct.ScmObj** %stackaddr$prim55878, align 8
%stackaddr$prim55879 = alloca %struct.ScmObj*, align 8
%f48046 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54323)
store volatile %struct.ScmObj* %f48046, %struct.ScmObj** %stackaddr$prim55879, align 8
%stackaddr$prim55880 = alloca %struct.ScmObj*, align 8
%current_45args54324 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54323)
store volatile %struct.ScmObj* %current_45args54324, %struct.ScmObj** %stackaddr$prim55880, align 8
%stackaddr$prim55881 = alloca %struct.ScmObj*, align 8
%lst48045 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54324)
store volatile %struct.ScmObj* %lst48045, %struct.ScmObj** %stackaddr$prim55881, align 8
%stackaddr$prim55882 = alloca %struct.ScmObj*, align 8
%anf_45bind48158 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48045)
store volatile %struct.ScmObj* %anf_45bind48158, %struct.ScmObj** %stackaddr$prim55882, align 8
%truthy$cmp55883 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48158)
%cmp$cmp55883 = icmp eq i64 %truthy$cmp55883, 1
br i1 %cmp$cmp55883, label %truebranch$cmp55883, label %falsebranch$cmp55883
truebranch$cmp55883:
%ae48646 = call %struct.ScmObj* @const_init_int(i64 0)
%ae48647 = call %struct.ScmObj* @const_init_null()
%argslist54326$k484090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55884 = alloca %struct.ScmObj*, align 8
%argslist54326$k484091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48647, %struct.ScmObj* %argslist54326$k484090)
store volatile %struct.ScmObj* %argslist54326$k484091, %struct.ScmObj** %stackaddr$prim55884, align 8
%stackaddr$prim55885 = alloca %struct.ScmObj*, align 8
%argslist54326$k484092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48646, %struct.ScmObj* %argslist54326$k484091)
store volatile %struct.ScmObj* %argslist54326$k484092, %struct.ScmObj** %stackaddr$prim55885, align 8
%clofunc55886 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48409)
musttail call tailcc void %clofunc55886(%struct.ScmObj* %k48409, %struct.ScmObj* %argslist54326$k484092)
ret void
falsebranch$cmp55883:
%stackaddr$prim55887 = alloca %struct.ScmObj*, align 8
%anf_45bind48159 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48045)
store volatile %struct.ScmObj* %anf_45bind48159, %struct.ScmObj** %stackaddr$prim55887, align 8
%stackaddr$makeclosure55888 = alloca %struct.ScmObj*, align 8
%fptrToInt55889 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48656 to i64
%ae48656 = call %struct.ScmObj* @closure_alloc(i64 4, i64 %fptrToInt55889)
store volatile %struct.ScmObj* %ae48656, %struct.ScmObj** %stackaddr$makeclosure55888, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48656, %struct.ScmObj* %k48409, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48656, %struct.ScmObj* %f48046, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48656, %struct.ScmObj* %lst48045, i64 2)
call void @closure_place_freevar(%struct.ScmObj* %ae48656, %struct.ScmObj* %_37map48044, i64 3)
%argslist54335$f480460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55890 = alloca %struct.ScmObj*, align 8
%argslist54335$f480461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48159, %struct.ScmObj* %argslist54335$f480460)
store volatile %struct.ScmObj* %argslist54335$f480461, %struct.ScmObj** %stackaddr$prim55890, align 8
%stackaddr$prim55891 = alloca %struct.ScmObj*, align 8
%argslist54335$f480462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48656, %struct.ScmObj* %argslist54335$f480461)
store volatile %struct.ScmObj* %argslist54335$f480462, %struct.ScmObj** %stackaddr$prim55891, align 8
%clofunc55892 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48046)
musttail call tailcc void %clofunc55892(%struct.ScmObj* %f48046, %struct.ScmObj* %argslist54335$f480462)
ret void
}

define tailcc void @proc_clo$ae48656(%struct.ScmObj* %env$ae48656,%struct.ScmObj* %current_45args54327) {
%stackaddr$env-ref55893 = alloca %struct.ScmObj*, align 8
%k48409 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48656, i64 0)
store %struct.ScmObj* %k48409, %struct.ScmObj** %stackaddr$env-ref55893
%stackaddr$env-ref55894 = alloca %struct.ScmObj*, align 8
%f48046 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48656, i64 1)
store %struct.ScmObj* %f48046, %struct.ScmObj** %stackaddr$env-ref55894
%stackaddr$env-ref55895 = alloca %struct.ScmObj*, align 8
%lst48045 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48656, i64 2)
store %struct.ScmObj* %lst48045, %struct.ScmObj** %stackaddr$env-ref55895
%stackaddr$env-ref55896 = alloca %struct.ScmObj*, align 8
%_37map48044 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48656, i64 3)
store %struct.ScmObj* %_37map48044, %struct.ScmObj** %stackaddr$env-ref55896
%stackaddr$prim55897 = alloca %struct.ScmObj*, align 8
%_95k48410 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54327)
store volatile %struct.ScmObj* %_95k48410, %struct.ScmObj** %stackaddr$prim55897, align 8
%stackaddr$prim55898 = alloca %struct.ScmObj*, align 8
%current_45args54328 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54327)
store volatile %struct.ScmObj* %current_45args54328, %struct.ScmObj** %stackaddr$prim55898, align 8
%stackaddr$prim55899 = alloca %struct.ScmObj*, align 8
%anf_45bind48160 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54328)
store volatile %struct.ScmObj* %anf_45bind48160, %struct.ScmObj** %stackaddr$prim55899, align 8
%stackaddr$prim55900 = alloca %struct.ScmObj*, align 8
%anf_45bind48161 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48045)
store volatile %struct.ScmObj* %anf_45bind48161, %struct.ScmObj** %stackaddr$prim55900, align 8
%stackaddr$makeclosure55901 = alloca %struct.ScmObj*, align 8
%fptrToInt55902 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48660 to i64
%ae48660 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55902)
store volatile %struct.ScmObj* %ae48660, %struct.ScmObj** %stackaddr$makeclosure55901, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48660, %struct.ScmObj* %k48409, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48660, %struct.ScmObj* %anf_45bind48160, i64 1)
%argslist54334$_37map480440 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55903 = alloca %struct.ScmObj*, align 8
%argslist54334$_37map480441 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48161, %struct.ScmObj* %argslist54334$_37map480440)
store volatile %struct.ScmObj* %argslist54334$_37map480441, %struct.ScmObj** %stackaddr$prim55903, align 8
%stackaddr$prim55904 = alloca %struct.ScmObj*, align 8
%argslist54334$_37map480442 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48046, %struct.ScmObj* %argslist54334$_37map480441)
store volatile %struct.ScmObj* %argslist54334$_37map480442, %struct.ScmObj** %stackaddr$prim55904, align 8
%stackaddr$prim55905 = alloca %struct.ScmObj*, align 8
%argslist54334$_37map480443 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48660, %struct.ScmObj* %argslist54334$_37map480442)
store volatile %struct.ScmObj* %argslist54334$_37map480443, %struct.ScmObj** %stackaddr$prim55905, align 8
%clofunc55906 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37map48044)
musttail call tailcc void %clofunc55906(%struct.ScmObj* %_37map48044, %struct.ScmObj* %argslist54334$_37map480443)
ret void
}

define tailcc void @proc_clo$ae48660(%struct.ScmObj* %env$ae48660,%struct.ScmObj* %current_45args54330) {
%stackaddr$env-ref55907 = alloca %struct.ScmObj*, align 8
%k48409 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48660, i64 0)
store %struct.ScmObj* %k48409, %struct.ScmObj** %stackaddr$env-ref55907
%stackaddr$env-ref55908 = alloca %struct.ScmObj*, align 8
%anf_45bind48160 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48660, i64 1)
store %struct.ScmObj* %anf_45bind48160, %struct.ScmObj** %stackaddr$env-ref55908
%stackaddr$prim55909 = alloca %struct.ScmObj*, align 8
%_95k48411 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54330)
store volatile %struct.ScmObj* %_95k48411, %struct.ScmObj** %stackaddr$prim55909, align 8
%stackaddr$prim55910 = alloca %struct.ScmObj*, align 8
%current_45args54331 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54330)
store volatile %struct.ScmObj* %current_45args54331, %struct.ScmObj** %stackaddr$prim55910, align 8
%stackaddr$prim55911 = alloca %struct.ScmObj*, align 8
%anf_45bind48162 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54331)
store volatile %struct.ScmObj* %anf_45bind48162, %struct.ScmObj** %stackaddr$prim55911, align 8
%stackaddr$prim55912 = alloca %struct.ScmObj*, align 8
%cpsprim48412 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48160, %struct.ScmObj* %anf_45bind48162)
store volatile %struct.ScmObj* %cpsprim48412, %struct.ScmObj** %stackaddr$prim55912, align 8
%ae48666 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54333$k484090 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55913 = alloca %struct.ScmObj*, align 8
%argslist54333$k484091 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %cpsprim48412, %struct.ScmObj* %argslist54333$k484090)
store volatile %struct.ScmObj* %argslist54333$k484091, %struct.ScmObj** %stackaddr$prim55913, align 8
%stackaddr$prim55914 = alloca %struct.ScmObj*, align 8
%argslist54333$k484092 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48666, %struct.ScmObj* %argslist54333$k484091)
store volatile %struct.ScmObj* %argslist54333$k484092, %struct.ScmObj** %stackaddr$prim55914, align 8
%clofunc55915 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48409)
musttail call tailcc void %clofunc55915(%struct.ScmObj* %k48409, %struct.ScmObj* %argslist54333$k484092)
ret void
}

define tailcc void @proc_clo$ae48559(%struct.ScmObj* %env$ae48559,%struct.ScmObj* %current_45args54339) {
%stackaddr$prim55916 = alloca %struct.ScmObj*, align 8
%k48413 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54339)
store volatile %struct.ScmObj* %k48413, %struct.ScmObj** %stackaddr$prim55916, align 8
%stackaddr$prim55917 = alloca %struct.ScmObj*, align 8
%current_45args54340 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54339)
store volatile %struct.ScmObj* %current_45args54340, %struct.ScmObj** %stackaddr$prim55917, align 8
%stackaddr$prim55918 = alloca %struct.ScmObj*, align 8
%_37foldr148048 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54340)
store volatile %struct.ScmObj* %_37foldr148048, %struct.ScmObj** %stackaddr$prim55918, align 8
%ae48561 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55919 = alloca %struct.ScmObj*, align 8
%fptrToInt55920 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48562 to i64
%ae48562 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55920)
store volatile %struct.ScmObj* %ae48562, %struct.ScmObj** %stackaddr$makeclosure55919, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48562, %struct.ScmObj* %_37foldr148048, i64 0)
%argslist54353$k484130 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55921 = alloca %struct.ScmObj*, align 8
%argslist54353$k484131 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48562, %struct.ScmObj* %argslist54353$k484130)
store volatile %struct.ScmObj* %argslist54353$k484131, %struct.ScmObj** %stackaddr$prim55921, align 8
%stackaddr$prim55922 = alloca %struct.ScmObj*, align 8
%argslist54353$k484132 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48561, %struct.ScmObj* %argslist54353$k484131)
store volatile %struct.ScmObj* %argslist54353$k484132, %struct.ScmObj** %stackaddr$prim55922, align 8
%clofunc55923 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48413)
musttail call tailcc void %clofunc55923(%struct.ScmObj* %k48413, %struct.ScmObj* %argslist54353$k484132)
ret void
}

define tailcc void @proc_clo$ae48562(%struct.ScmObj* %env$ae48562,%struct.ScmObj* %current_45args54342) {
%stackaddr$env-ref55924 = alloca %struct.ScmObj*, align 8
%_37foldr148048 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48562, i64 0)
store %struct.ScmObj* %_37foldr148048, %struct.ScmObj** %stackaddr$env-ref55924
%stackaddr$prim55925 = alloca %struct.ScmObj*, align 8
%k48414 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54342)
store volatile %struct.ScmObj* %k48414, %struct.ScmObj** %stackaddr$prim55925, align 8
%stackaddr$prim55926 = alloca %struct.ScmObj*, align 8
%current_45args54343 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54342)
store volatile %struct.ScmObj* %current_45args54343, %struct.ScmObj** %stackaddr$prim55926, align 8
%stackaddr$prim55927 = alloca %struct.ScmObj*, align 8
%f48051 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54343)
store volatile %struct.ScmObj* %f48051, %struct.ScmObj** %stackaddr$prim55927, align 8
%stackaddr$prim55928 = alloca %struct.ScmObj*, align 8
%current_45args54344 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54343)
store volatile %struct.ScmObj* %current_45args54344, %struct.ScmObj** %stackaddr$prim55928, align 8
%stackaddr$prim55929 = alloca %struct.ScmObj*, align 8
%acc48050 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54344)
store volatile %struct.ScmObj* %acc48050, %struct.ScmObj** %stackaddr$prim55929, align 8
%stackaddr$prim55930 = alloca %struct.ScmObj*, align 8
%current_45args54345 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54344)
store volatile %struct.ScmObj* %current_45args54345, %struct.ScmObj** %stackaddr$prim55930, align 8
%stackaddr$prim55931 = alloca %struct.ScmObj*, align 8
%lst48049 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54345)
store volatile %struct.ScmObj* %lst48049, %struct.ScmObj** %stackaddr$prim55931, align 8
%stackaddr$prim55932 = alloca %struct.ScmObj*, align 8
%anf_45bind48153 = call %struct.ScmObj* @prim_null_63(%struct.ScmObj* %lst48049)
store volatile %struct.ScmObj* %anf_45bind48153, %struct.ScmObj** %stackaddr$prim55932, align 8
%truthy$cmp55933 = call i64 @is_truthy_value(%struct.ScmObj* %anf_45bind48153)
%cmp$cmp55933 = icmp eq i64 %truthy$cmp55933, 1
br i1 %cmp$cmp55933, label %truebranch$cmp55933, label %falsebranch$cmp55933
truebranch$cmp55933:
%ae48566 = call %struct.ScmObj* @const_init_int(i64 0)
%argslist54347$k484140 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55934 = alloca %struct.ScmObj*, align 8
%argslist54347$k484141 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48050, %struct.ScmObj* %argslist54347$k484140)
store volatile %struct.ScmObj* %argslist54347$k484141, %struct.ScmObj** %stackaddr$prim55934, align 8
%stackaddr$prim55935 = alloca %struct.ScmObj*, align 8
%argslist54347$k484142 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48566, %struct.ScmObj* %argslist54347$k484141)
store volatile %struct.ScmObj* %argslist54347$k484142, %struct.ScmObj** %stackaddr$prim55935, align 8
%clofunc55936 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48414)
musttail call tailcc void %clofunc55936(%struct.ScmObj* %k48414, %struct.ScmObj* %argslist54347$k484142)
ret void
falsebranch$cmp55933:
%stackaddr$prim55937 = alloca %struct.ScmObj*, align 8
%anf_45bind48154 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %lst48049)
store volatile %struct.ScmObj* %anf_45bind48154, %struct.ScmObj** %stackaddr$prim55937, align 8
%stackaddr$prim55938 = alloca %struct.ScmObj*, align 8
%anf_45bind48155 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %lst48049)
store volatile %struct.ScmObj* %anf_45bind48155, %struct.ScmObj** %stackaddr$prim55938, align 8
%stackaddr$makeclosure55939 = alloca %struct.ScmObj*, align 8
%fptrToInt55940 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48574 to i64
%ae48574 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55940)
store volatile %struct.ScmObj* %ae48574, %struct.ScmObj** %stackaddr$makeclosure55939, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48574, %struct.ScmObj* %f48051, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48574, %struct.ScmObj* %k48414, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48574, %struct.ScmObj* %anf_45bind48154, i64 2)
%argslist54352$_37foldr1480480 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55941 = alloca %struct.ScmObj*, align 8
%argslist54352$_37foldr1480481 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48155, %struct.ScmObj* %argslist54352$_37foldr1480480)
store volatile %struct.ScmObj* %argslist54352$_37foldr1480481, %struct.ScmObj** %stackaddr$prim55941, align 8
%stackaddr$prim55942 = alloca %struct.ScmObj*, align 8
%argslist54352$_37foldr1480482 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %acc48050, %struct.ScmObj* %argslist54352$_37foldr1480481)
store volatile %struct.ScmObj* %argslist54352$_37foldr1480482, %struct.ScmObj** %stackaddr$prim55942, align 8
%stackaddr$prim55943 = alloca %struct.ScmObj*, align 8
%argslist54352$_37foldr1480483 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48051, %struct.ScmObj* %argslist54352$_37foldr1480482)
store volatile %struct.ScmObj* %argslist54352$_37foldr1480483, %struct.ScmObj** %stackaddr$prim55943, align 8
%stackaddr$prim55944 = alloca %struct.ScmObj*, align 8
%argslist54352$_37foldr1480484 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48574, %struct.ScmObj* %argslist54352$_37foldr1480483)
store volatile %struct.ScmObj* %argslist54352$_37foldr1480484, %struct.ScmObj** %stackaddr$prim55944, align 8
%clofunc55945 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %_37foldr148048)
musttail call tailcc void %clofunc55945(%struct.ScmObj* %_37foldr148048, %struct.ScmObj* %argslist54352$_37foldr1480484)
ret void
}

define tailcc void @proc_clo$ae48574(%struct.ScmObj* %env$ae48574,%struct.ScmObj* %current_45args54348) {
%stackaddr$env-ref55946 = alloca %struct.ScmObj*, align 8
%f48051 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48574, i64 0)
store %struct.ScmObj* %f48051, %struct.ScmObj** %stackaddr$env-ref55946
%stackaddr$env-ref55947 = alloca %struct.ScmObj*, align 8
%k48414 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48574, i64 1)
store %struct.ScmObj* %k48414, %struct.ScmObj** %stackaddr$env-ref55947
%stackaddr$env-ref55948 = alloca %struct.ScmObj*, align 8
%anf_45bind48154 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48574, i64 2)
store %struct.ScmObj* %anf_45bind48154, %struct.ScmObj** %stackaddr$env-ref55948
%stackaddr$prim55949 = alloca %struct.ScmObj*, align 8
%_95k48415 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54348)
store volatile %struct.ScmObj* %_95k48415, %struct.ScmObj** %stackaddr$prim55949, align 8
%stackaddr$prim55950 = alloca %struct.ScmObj*, align 8
%current_45args54349 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54348)
store volatile %struct.ScmObj* %current_45args54349, %struct.ScmObj** %stackaddr$prim55950, align 8
%stackaddr$prim55951 = alloca %struct.ScmObj*, align 8
%anf_45bind48156 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54349)
store volatile %struct.ScmObj* %anf_45bind48156, %struct.ScmObj** %stackaddr$prim55951, align 8
%argslist54351$f480510 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55952 = alloca %struct.ScmObj*, align 8
%argslist54351$f480511 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48156, %struct.ScmObj* %argslist54351$f480510)
store volatile %struct.ScmObj* %argslist54351$f480511, %struct.ScmObj** %stackaddr$prim55952, align 8
%stackaddr$prim55953 = alloca %struct.ScmObj*, align 8
%argslist54351$f480512 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48154, %struct.ScmObj* %argslist54351$f480511)
store volatile %struct.ScmObj* %argslist54351$f480512, %struct.ScmObj** %stackaddr$prim55953, align 8
%stackaddr$prim55954 = alloca %struct.ScmObj*, align 8
%argslist54351$f480513 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48414, %struct.ScmObj* %argslist54351$f480512)
store volatile %struct.ScmObj* %argslist54351$f480513, %struct.ScmObj** %stackaddr$prim55954, align 8
%clofunc55955 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48051)
musttail call tailcc void %clofunc55955(%struct.ScmObj* %f48051, %struct.ScmObj* %argslist54351$f480513)
ret void
}

define tailcc void @proc_clo$ae48442(%struct.ScmObj* %env$ae48442,%struct.ScmObj* %current_45args54356) {
%stackaddr$prim55956 = alloca %struct.ScmObj*, align 8
%k48416 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54356)
store volatile %struct.ScmObj* %k48416, %struct.ScmObj** %stackaddr$prim55956, align 8
%stackaddr$prim55957 = alloca %struct.ScmObj*, align 8
%current_45args54357 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54356)
store volatile %struct.ScmObj* %current_45args54357, %struct.ScmObj** %stackaddr$prim55957, align 8
%stackaddr$prim55958 = alloca %struct.ScmObj*, align 8
%y48028 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54357)
store volatile %struct.ScmObj* %y48028, %struct.ScmObj** %stackaddr$prim55958, align 8
%ae48444 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55959 = alloca %struct.ScmObj*, align 8
%fptrToInt55960 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48445 to i64
%ae48445 = call %struct.ScmObj* @closure_alloc(i64 1, i64 %fptrToInt55960)
store volatile %struct.ScmObj* %ae48445, %struct.ScmObj** %stackaddr$makeclosure55959, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48445, %struct.ScmObj* %y48028, i64 0)
%argslist54375$k484160 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55961 = alloca %struct.ScmObj*, align 8
%argslist54375$k484161 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48445, %struct.ScmObj* %argslist54375$k484160)
store volatile %struct.ScmObj* %argslist54375$k484161, %struct.ScmObj** %stackaddr$prim55961, align 8
%stackaddr$prim55962 = alloca %struct.ScmObj*, align 8
%argslist54375$k484162 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48444, %struct.ScmObj* %argslist54375$k484161)
store volatile %struct.ScmObj* %argslist54375$k484162, %struct.ScmObj** %stackaddr$prim55962, align 8
%clofunc55963 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %k48416)
musttail call tailcc void %clofunc55963(%struct.ScmObj* %k48416, %struct.ScmObj* %argslist54375$k484162)
ret void
}

define tailcc void @proc_clo$ae48445(%struct.ScmObj* %env$ae48445,%struct.ScmObj* %current_45args54359) {
%stackaddr$env-ref55964 = alloca %struct.ScmObj*, align 8
%y48028 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48445, i64 0)
store %struct.ScmObj* %y48028, %struct.ScmObj** %stackaddr$env-ref55964
%stackaddr$prim55965 = alloca %struct.ScmObj*, align 8
%k48417 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54359)
store volatile %struct.ScmObj* %k48417, %struct.ScmObj** %stackaddr$prim55965, align 8
%stackaddr$prim55966 = alloca %struct.ScmObj*, align 8
%current_45args54360 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54359)
store volatile %struct.ScmObj* %current_45args54360, %struct.ScmObj** %stackaddr$prim55966, align 8
%stackaddr$prim55967 = alloca %struct.ScmObj*, align 8
%f48029 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54360)
store volatile %struct.ScmObj* %f48029, %struct.ScmObj** %stackaddr$prim55967, align 8
%stackaddr$makeclosure55968 = alloca %struct.ScmObj*, align 8
%fptrToInt55969 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48446 to i64
%ae48446 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55969)
store volatile %struct.ScmObj* %ae48446, %struct.ScmObj** %stackaddr$makeclosure55968, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48446, %struct.ScmObj* %k48417, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48446, %struct.ScmObj* %f48029, i64 1)
%ae48447 = call %struct.ScmObj* @const_init_int(i64 0)
%stackaddr$makeclosure55970 = alloca %struct.ScmObj*, align 8
%fptrToInt55971 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48448 to i64
%ae48448 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55971)
store volatile %struct.ScmObj* %ae48448, %struct.ScmObj** %stackaddr$makeclosure55970, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48448, %struct.ScmObj* %f48029, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48448, %struct.ScmObj* %y48028, i64 1)
%argslist54374$ae484460 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55972 = alloca %struct.ScmObj*, align 8
%argslist54374$ae484461 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48448, %struct.ScmObj* %argslist54374$ae484460)
store volatile %struct.ScmObj* %argslist54374$ae484461, %struct.ScmObj** %stackaddr$prim55972, align 8
%stackaddr$prim55973 = alloca %struct.ScmObj*, align 8
%argslist54374$ae484462 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48447, %struct.ScmObj* %argslist54374$ae484461)
store volatile %struct.ScmObj* %argslist54374$ae484462, %struct.ScmObj** %stackaddr$prim55973, align 8
%clofunc55974 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %ae48446)
musttail call tailcc void %clofunc55974(%struct.ScmObj* %ae48446, %struct.ScmObj* %argslist54374$ae484462)
ret void
}

define tailcc void @proc_clo$ae48446(%struct.ScmObj* %env$ae48446,%struct.ScmObj* %current_45args54362) {
%stackaddr$env-ref55975 = alloca %struct.ScmObj*, align 8
%k48417 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48446, i64 0)
store %struct.ScmObj* %k48417, %struct.ScmObj** %stackaddr$env-ref55975
%stackaddr$env-ref55976 = alloca %struct.ScmObj*, align 8
%f48029 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48446, i64 1)
store %struct.ScmObj* %f48029, %struct.ScmObj** %stackaddr$env-ref55976
%stackaddr$prim55977 = alloca %struct.ScmObj*, align 8
%_95k48418 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54362)
store volatile %struct.ScmObj* %_95k48418, %struct.ScmObj** %stackaddr$prim55977, align 8
%stackaddr$prim55978 = alloca %struct.ScmObj*, align 8
%current_45args54363 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54362)
store volatile %struct.ScmObj* %current_45args54363, %struct.ScmObj** %stackaddr$prim55978, align 8
%stackaddr$prim55979 = alloca %struct.ScmObj*, align 8
%anf_45bind48151 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54363)
store volatile %struct.ScmObj* %anf_45bind48151, %struct.ScmObj** %stackaddr$prim55979, align 8
%argslist54365$f480290 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55980 = alloca %struct.ScmObj*, align 8
%argslist54365$f480291 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %anf_45bind48151, %struct.ScmObj* %argslist54365$f480290)
store volatile %struct.ScmObj* %argslist54365$f480291, %struct.ScmObj** %stackaddr$prim55980, align 8
%stackaddr$prim55981 = alloca %struct.ScmObj*, align 8
%argslist54365$f480292 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48417, %struct.ScmObj* %argslist54365$f480291)
store volatile %struct.ScmObj* %argslist54365$f480292, %struct.ScmObj** %stackaddr$prim55981, align 8
%clofunc55982 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %f48029)
musttail call tailcc void %clofunc55982(%struct.ScmObj* %f48029, %struct.ScmObj* %argslist54365$f480292)
ret void
}

define tailcc void @proc_clo$ae48448(%struct.ScmObj* %env$ae48448,%struct.ScmObj* %args4803048419) {
%stackaddr$env-ref55983 = alloca %struct.ScmObj*, align 8
%f48029 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48448, i64 0)
store %struct.ScmObj* %f48029, %struct.ScmObj** %stackaddr$env-ref55983
%stackaddr$env-ref55984 = alloca %struct.ScmObj*, align 8
%y48028 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48448, i64 1)
store %struct.ScmObj* %y48028, %struct.ScmObj** %stackaddr$env-ref55984
%stackaddr$prim55985 = alloca %struct.ScmObj*, align 8
%k48420 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %args4803048419)
store volatile %struct.ScmObj* %k48420, %struct.ScmObj** %stackaddr$prim55985, align 8
%stackaddr$prim55986 = alloca %struct.ScmObj*, align 8
%args48030 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %args4803048419)
store volatile %struct.ScmObj* %args48030, %struct.ScmObj** %stackaddr$prim55986, align 8
%stackaddr$makeclosure55987 = alloca %struct.ScmObj*, align 8
%fptrToInt55988 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48452 to i64
%ae48452 = call %struct.ScmObj* @closure_alloc(i64 3, i64 %fptrToInt55988)
store volatile %struct.ScmObj* %ae48452, %struct.ScmObj** %stackaddr$makeclosure55987, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48452, %struct.ScmObj* %k48420, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48452, %struct.ScmObj* %args48030, i64 1)
call void @closure_place_freevar(%struct.ScmObj* %ae48452, %struct.ScmObj* %f48029, i64 2)
%argslist54373$y480280 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim55989 = alloca %struct.ScmObj*, align 8
%argslist54373$y480281 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %y48028, %struct.ScmObj* %argslist54373$y480280)
store volatile %struct.ScmObj* %argslist54373$y480281, %struct.ScmObj** %stackaddr$prim55989, align 8
%stackaddr$prim55990 = alloca %struct.ScmObj*, align 8
%argslist54373$y480282 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48452, %struct.ScmObj* %argslist54373$y480281)
store volatile %struct.ScmObj* %argslist54373$y480282, %struct.ScmObj** %stackaddr$prim55990, align 8
%clofunc55991 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %y48028)
musttail call tailcc void %clofunc55991(%struct.ScmObj* %y48028, %struct.ScmObj* %argslist54373$y480282)
ret void
}

define tailcc void @proc_clo$ae48452(%struct.ScmObj* %env$ae48452,%struct.ScmObj* %current_45args54366) {
%stackaddr$env-ref55992 = alloca %struct.ScmObj*, align 8
%k48420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48452, i64 0)
store %struct.ScmObj* %k48420, %struct.ScmObj** %stackaddr$env-ref55992
%stackaddr$env-ref55993 = alloca %struct.ScmObj*, align 8
%args48030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48452, i64 1)
store %struct.ScmObj* %args48030, %struct.ScmObj** %stackaddr$env-ref55993
%stackaddr$env-ref55994 = alloca %struct.ScmObj*, align 8
%f48029 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48452, i64 2)
store %struct.ScmObj* %f48029, %struct.ScmObj** %stackaddr$env-ref55994
%stackaddr$prim55995 = alloca %struct.ScmObj*, align 8
%_95k48421 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54366)
store volatile %struct.ScmObj* %_95k48421, %struct.ScmObj** %stackaddr$prim55995, align 8
%stackaddr$prim55996 = alloca %struct.ScmObj*, align 8
%current_45args54367 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54366)
store volatile %struct.ScmObj* %current_45args54367, %struct.ScmObj** %stackaddr$prim55996, align 8
%stackaddr$prim55997 = alloca %struct.ScmObj*, align 8
%anf_45bind48149 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54367)
store volatile %struct.ScmObj* %anf_45bind48149, %struct.ScmObj** %stackaddr$prim55997, align 8
%stackaddr$makeclosure55998 = alloca %struct.ScmObj*, align 8
%fptrToInt55999 = ptrtoint void(%struct.ScmObj*,%struct.ScmObj*)* @proc_clo$ae48455 to i64
%ae48455 = call %struct.ScmObj* @closure_alloc(i64 2, i64 %fptrToInt55999)
store volatile %struct.ScmObj* %ae48455, %struct.ScmObj** %stackaddr$makeclosure55998, align 8
call void @closure_place_freevar(%struct.ScmObj* %ae48455, %struct.ScmObj* %k48420, i64 0)
call void @closure_place_freevar(%struct.ScmObj* %ae48455, %struct.ScmObj* %args48030, i64 1)
%argslist54372$anf_45bind481490 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56000 = alloca %struct.ScmObj*, align 8
%argslist54372$anf_45bind481491 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %f48029, %struct.ScmObj* %argslist54372$anf_45bind481490)
store volatile %struct.ScmObj* %argslist54372$anf_45bind481491, %struct.ScmObj** %stackaddr$prim56000, align 8
%stackaddr$prim56001 = alloca %struct.ScmObj*, align 8
%argslist54372$anf_45bind481492 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %ae48455, %struct.ScmObj* %argslist54372$anf_45bind481491)
store volatile %struct.ScmObj* %argslist54372$anf_45bind481492, %struct.ScmObj** %stackaddr$prim56001, align 8
%clofunc56002 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48149)
musttail call tailcc void %clofunc56002(%struct.ScmObj* %anf_45bind48149, %struct.ScmObj* %argslist54372$anf_45bind481492)
ret void
}

define tailcc void @proc_clo$ae48455(%struct.ScmObj* %env$ae48455,%struct.ScmObj* %current_45args54369) {
%stackaddr$env-ref56003 = alloca %struct.ScmObj*, align 8
%k48420 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48455, i64 0)
store %struct.ScmObj* %k48420, %struct.ScmObj** %stackaddr$env-ref56003
%stackaddr$env-ref56004 = alloca %struct.ScmObj*, align 8
%args48030 = call %struct.ScmObj* @closure_env_get(%struct.ScmObj* %env$ae48455, i64 1)
store %struct.ScmObj* %args48030, %struct.ScmObj** %stackaddr$env-ref56004
%stackaddr$prim56005 = alloca %struct.ScmObj*, align 8
%_95k48422 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54369)
store volatile %struct.ScmObj* %_95k48422, %struct.ScmObj** %stackaddr$prim56005, align 8
%stackaddr$prim56006 = alloca %struct.ScmObj*, align 8
%current_45args54370 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54369)
store volatile %struct.ScmObj* %current_45args54370, %struct.ScmObj** %stackaddr$prim56006, align 8
%stackaddr$prim56007 = alloca %struct.ScmObj*, align 8
%anf_45bind48150 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54370)
store volatile %struct.ScmObj* %anf_45bind48150, %struct.ScmObj** %stackaddr$prim56007, align 8
%stackaddr$prim56008 = alloca %struct.ScmObj*, align 8
%cpsargs48423 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48420, %struct.ScmObj* %args48030)
store volatile %struct.ScmObj* %cpsargs48423, %struct.ScmObj** %stackaddr$prim56008, align 8
%clofunc56009 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %anf_45bind48150)
musttail call tailcc void %clofunc56009(%struct.ScmObj* %anf_45bind48150, %struct.ScmObj* %cpsargs48423)
ret void
}

define tailcc void @proc_clo$ae48427(%struct.ScmObj* %env$ae48427,%struct.ScmObj* %current_45args54377) {
%stackaddr$prim56010 = alloca %struct.ScmObj*, align 8
%k48424 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54377)
store volatile %struct.ScmObj* %k48424, %struct.ScmObj** %stackaddr$prim56010, align 8
%stackaddr$prim56011 = alloca %struct.ScmObj*, align 8
%current_45args54378 = call %struct.ScmObj* @prim_cdr(%struct.ScmObj* %current_45args54377)
store volatile %struct.ScmObj* %current_45args54378, %struct.ScmObj** %stackaddr$prim56011, align 8
%stackaddr$prim56012 = alloca %struct.ScmObj*, align 8
%yu48027 = call %struct.ScmObj* @prim_car(%struct.ScmObj* %current_45args54378)
store volatile %struct.ScmObj* %yu48027, %struct.ScmObj** %stackaddr$prim56012, align 8
%argslist54380$yu480270 = call %struct.ScmObj* @const_init_null()
%stackaddr$prim56013 = alloca %struct.ScmObj*, align 8
%argslist54380$yu480271 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %yu48027, %struct.ScmObj* %argslist54380$yu480270)
store volatile %struct.ScmObj* %argslist54380$yu480271, %struct.ScmObj** %stackaddr$prim56013, align 8
%stackaddr$prim56014 = alloca %struct.ScmObj*, align 8
%argslist54380$yu480272 = call %struct.ScmObj* @prim_cons(%struct.ScmObj* %k48424, %struct.ScmObj* %argslist54380$yu480271)
store volatile %struct.ScmObj* %argslist54380$yu480272, %struct.ScmObj** %stackaddr$prim56014, align 8
%clofunc56015 = call void(%struct.ScmObj*,%struct.ScmObj*)* @closure_get_fn_part(%struct.ScmObj* %yu48027)
musttail call tailcc void %clofunc56015(%struct.ScmObj* %yu48027, %struct.ScmObj* %argslist54380$yu480272)
ret void
}